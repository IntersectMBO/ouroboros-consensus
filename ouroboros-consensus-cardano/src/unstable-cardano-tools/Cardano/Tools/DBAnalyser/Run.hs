{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tools.DBAnalyser.Run (analyse) where

import           Cardano.Tools.DBAnalyser.Analysis
import           Cardano.Tools.DBAnalyser.HasAnalysis
import           Cardano.Tools.DBAnalyser.Types
import           Control.Tracer (Tracer (..), nullTracer)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as LedgerSupportsMempool
                     (HasTxs)
import qualified Ouroboros.Consensus.Node as Node
import qualified Ouroboros.Consensus.Node.InitStorage as Node
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as ChainDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.Impl.Args as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Args as LedgerDB.V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore as LedgerDB.V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog as LedgerDB.V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Snapshots as LedgerDB.V1
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry
import           System.FilePath
import           System.FS.API
import           System.FS.IO
import           System.IO

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
     , CanStowLedgerTables (LedgerState blk)
     )
  => DBAnalyserConfig
  -> Args blk
  -> IO (Maybe AnalysisResult)
analyse DBAnalyserConfig{analysis, confLimit, ssdDir, stateInSSD, tablesInSSD, dbDir, selectDB, validation, verbose, bsArgs} args =
    withRegistry $ \registry -> do
      lock           <- newMVar ()
      chainDBTracer  <- mkTracer lock verbose
      analysisTracer <- mkTracer lock True
      ProtocolInfo { pInfoInitLedger = genesisLedger, pInfoConfig = cfg } <-
        mkProtocolInfo args
      let chunkInfo  = Node.nodeImmutableDbChunkInfo (configStorage cfg)
          bss = LedgerDB.V1.V1Args LedgerDB.V1.DisableFlushing LedgerDB.V1.DisableQuerySize $ bsArgs
          flavargs = LedgerDB.LedgerDbFlavorArgsV1 bss
          args' =
            ChainDB.completeChainDbArgs
              registry
              InFuture.dontCheck
              cfg
              genesisLedger
              chunkInfo
              (const True)
              (Node.stdMkChainDbHasFS dbDir)
              (Node.stdMkChainDbHasFS ssdDir)
              flavargs $
            ChainDB.defaultArgs
          chainDbArgs = maybeValidateAll $ ChainDB.updateTracer chainDBTracer args'
          immutableDbArgs = ChainDB.cdbImmDbArgs chainDbArgs
          normalFs = SomeHasFS $ ioHasFS $ MountPoint (dbDir  </> "ledger")
          ssdFs    = SomeHasFS $ ioHasFS $ MountPoint (ssdDir </> "ledger")
          ldbArgs = ChainDB.cdbLgrDbArgs args'

      case selectDB of
        SelectImmutableDB initializeFrom -> do
          -- TODO we need to check if the snapshot exists. If not, print an
          -- error and ask the user if she wanted to create a snapshot first and
          -- how to do it.
          (initHeaderState, newStartFrom, replayPoint) <- either (error . show) id <$> case initializeFrom of
            Nothing       -> pure $ Right $ (headerState genesisLedger, Nothing, GenesisPoint)
            Just snapshot -> do
              res <- LedgerDB.V1.loadSnapshot nullTracer bsArgs (configCodec cfg)
                (if stateInSSD then ssdFs else normalFs)
                (if tablesInSSD then ssdFs else normalFs)
                ssdFs
                snapshot
              case res of
                Left err -> pure $ Left err
                Right ((dbch, bs), _) -> do
                  LedgerDB.V1.bsClose bs
                  let st = LedgerDB.V1.current $ LedgerDB.V1.anchorlessChangelog dbch
                  pure $ Right $ ( headerState st
                                 , Just snapshot
                                 , castPoint $ getTip st
                                 )

          ImmutableDB.withDB (ImmutableDB.openDB immutableDbArgs runWithTempRegistry) $ \immutableDB -> do
            -- This marker divides the "loading" phase of the program, where the
            -- system is principally occupied with reading snapshot data from
            -- disk, from the "processing" phase, where we are streaming blocks
            -- and running the ledger processing on them.
            result <- runAnalysis analysis $ AnalysisEnv {
                cfg
              , initHeaderState
              , db = Left immutableDB
              , registry
              , limit = confLimit
              , tracer = analysisTracer
              , ledgerDbArgs = ldbArgs { LedgerDB.lgrStartSnapshot = newStartFrom }
              , replayPoint
              }
            tipPoint <- atomically $ ImmutableDB.getTipPoint immutableDB
            putStrLn $ "ImmutableDB tip: " ++ show tipPoint
            pure result
        SelectChainDB initializeFrom -> do

          (initHeaderState, newStartFrom, replayPoint) <- either (error . show) id <$> case initializeFrom of
            Nothing       -> pure $ Right $ (headerState genesisLedger, Nothing, GenesisPoint)
            Just snapshot -> do
              res <- LedgerDB.V1.loadSnapshot nullTracer bsArgs (configCodec cfg)
                (if stateInSSD then ssdFs else normalFs)
                (if tablesInSSD then ssdFs else normalFs)
                ssdFs
                snapshot
              case res of
                Left err -> pure $ Left err
                Right ((dbch, bs), _) -> do
                  LedgerDB.V1.bsClose bs
                  let st = LedgerDB.V1.current $ LedgerDB.V1.anchorlessChangelog dbch
                  pure $ Right $ ( headerState st
                                 , Just snapshot
                                 , castPoint $ getTip st
                                 )

          ChainDB.withDB chainDbArgs $ \chainDB -> do
            result <- runAnalysis analysis $ AnalysisEnv {
                cfg
              , initHeaderState
              , db = Right chainDB
              , registry
              , limit = confLimit
              , tracer = analysisTracer
              , ledgerDbArgs = ldbArgs { LedgerDB.lgrStartSnapshot = newStartFrom }
              , replayPoint
              }
            tipPoint <- atomically $ ChainDB.getTipPoint chainDB
            putStrLn $ "ChainDB tip: " ++ show tipPoint
            pure result
  where
    mkTracer _    False = return nullTracer
    mkTracer lock True  = do
      startTime <- getMonotonicTime
      return $ Tracer $ \ev -> withLock $ do
        traceTime <- getMonotonicTime
        let diff = diffTime traceTime startTime
        hPutStrLn stderr $ concat ["[", show diff, "] ", show ev]
        hFlush stderr
      where
        withLock = bracket_ (takeMVar lock) (putMVar lock ())

    maybeValidateAll = case (analysis, validation) of
      (_, Just ValidateAllBlocks)      -> ChainDB.ensureValidateAll
      (_, Just MinimumBlockValidation) -> id
      (OnlyValidation, _ )             -> ChainDB.ensureValidateAll
      _                                -> id

