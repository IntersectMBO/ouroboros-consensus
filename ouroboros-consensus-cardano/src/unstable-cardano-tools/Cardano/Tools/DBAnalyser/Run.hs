{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tools.DBAnalyser.Run (analyse) where

import           Cardano.Tools.DBAnalyser.Analysis
import           Cardano.Tools.DBAnalyser.HasAnalysis
import           Cardano.Tools.DBAnalyser.Types
import           Codec.Serialise (Serialise (decode))
import           Control.Monad.Except (runExceptT)
import           Control.Tracer (Tracer (..), nullTracer)
import           Data.Singletons (Sing, SingI (..))
import qualified Debug.Trace as Debug
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
import           Ouroboros.Consensus.Ledger.Extended
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as LedgerSupportsMempool
                     (HasTxs)
import qualified Ouroboros.Consensus.Node as Node
import qualified Ouroboros.Consensus.Node.InitStorage as Node
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args
import           Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB (lgrHasFS)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.LedgerDB (DiskSnapshot (..),
                     readSnapshot)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry
import           System.IO
import           Text.Printf (printf)


{-------------------------------------------------------------------------------
  Analyse
-------------------------------------------------------------------------------}

analyse ::
     forall blk .
     ( Node.RunNode blk
     , Show (Header blk)
     , HasAnalysis blk
     , HasProtocolInfo blk
     , LedgerSupportsMempool.HasTxs blk
     )
  => DBAnalyserConfig
  -> Args blk
  -> IO (Maybe AnalysisResult)
analyse DBAnalyserConfig{analysis, confLimit, dbDir, selectDB, validation, verbose, diskSnapshotChecksum} args =
    withRegistry $ \registry -> do
      lock           <- newMVar ()
      chainDBTracer  <- mkTracer lock verbose
      analysisTracer <- mkTracer lock True
      ProtocolInfo { pInfoInitLedger = genesisLedger, pInfoConfig = cfg } <-
        mkProtocolInfo args
      let chunkInfo   = Node.nodeImmutableDbChunkInfo (configStorage cfg)
          chainDbArgs =
                maybeValidateAll
              $ updateTracer chainDBTracer
              $ completeChainDbArgs
                  registry
                  InFuture.dontCheck
                  cfg
                  genesisLedger
                  chunkInfo
                  (const True)
                  (Node.stdMkChainDbHasFS dbDir)
                  (Node.stdMkChainDbHasFS dbDir)
              $ defaultArgs
          immutableDbArgs = ChainDB.cdbImmDbArgs chainDbArgs
          ledgerDbFS = lgrHasFS $ ChainDB.cdbLgrDbArgs chainDbArgs

      withImmutableDB immutableDbArgs $ \(immutableDB, internal) -> do
        SomeAnalysis (Proxy :: Proxy startFrom) ana <- pure $ runAnalysis analysis
        startFrom <- case sing :: Sing startFrom of
          SStartFromPoint       -> FromPoint <$> case startSlot of
            Origin         -> pure GenesisPoint
            NotOrigin slot -> ImmutableDB.getHashForSlot internal slot >>= \case
              Just hash -> pure $ BlockPoint slot hash
              Nothing   -> fail $ "No block with given slot in the ImmutableDB: " <> show slot
          SStartFromLedgerState -> do
            -- TODO we need to check if the snapshot exists. If not, print an
            -- error and ask the user if she wanted to create a snapshot first and
            -- how to do it.
            initLedgerErr <- runExceptT $ case startSlot of
              Origin                  -> pure genesisLedger
              NotOrigin (SlotNo slot) -> readSnapshot
                ledgerDbFS
                (decodeDiskExtLedgerState $ configCodec cfg)
                decode
                diskSnapshotChecksum
                (DiskSnapshot slot (Just "db-analyser"))
                -- TODO @readSnapshot@ has type @ExceptT ReadIncrementalErr m
                -- (ExtLedgerState blk)@ but it also throws exceptions! This makes
                -- error handling more challenging than it ought to be. Maybe we
                -- can enrich the error that @readSnapthot@ return, so that it can
                -- contain the @HasFS@ errors as well.
            initLedger <- either (error . show) pure initLedgerErr
            -- This marker divides the "loading" phase of the program, where the
            -- system is principally occupied with reading snapshot data from
            -- disk, from the "processing" phase, where we are streaming blocks
            -- and running the ledger processing on them.
            Debug.traceMarkerIO "SNAPSHOT_LOADED"
            pure $ FromLedgerState initLedger

        result <- ana AnalysisEnv {
            cfg
          , startFrom
          , db = immutableDB
          , registry
          , ledgerDbFS = ledgerDbFS
          , limit = confLimit
          , tracer = analysisTracer
          }
        tipPoint <- atomically $ ImmutableDB.getTipPoint immutableDB
        putStrLn $ "ImmutableDB tip: " ++ show tipPoint
        pure result
  where
    SelectImmutableDB startSlot = selectDB

    withImmutableDB immutableDbArgs =
        bracket
          (ImmutableDB.openDBInternal immutableDbArgs runWithTempRegistry)
          (ImmutableDB.closeDB . fst)

    mkTracer _    False = return nullTracer
    mkTracer lock True  = do
      startTime <- getMonotonicTime
      return $ Tracer $ \ev -> withLock $ do
        traceTime <- getMonotonicTime
        let diff = diffTime traceTime startTime
        hPutStrLn stderr $ printf "[%.6fs] %s" (realToFrac diff :: Double) (show ev)
        hFlush stderr
      where
        withLock = bracket_ (takeMVar lock) (putMVar lock ())

    maybeValidateAll = case (analysis, validation) of
      (_, Just ValidateAllBlocks)      -> ensureValidateAll
      (_, Just MinimumBlockValidation) -> id
      (OnlyValidation, _ )             -> ensureValidateAll
      _                                -> id
