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
import           Control.ResourceRegistry
import           Control.Tracer (Tracer (..), nullTracer)
import           Data.Singletons (Sing, SingI (..))
import qualified Data.SOP.Dict as Dict
import qualified Debug.Trace as Debug
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Inspect
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as LedgerSupportsMempool
                     (HasTxs)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.Node as Node
import qualified Ouroboros.Consensus.Node.InitStorage as Node
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as ChainDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Stream as ImmutableDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.Impl.Args as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.Impl.Init as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Args as LedgerDB.V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API as BS
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB as LMDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Init as LedgerDB.V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Args as LedgerDB.V2
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Network.Block (genesisPoint)
import           System.IO
import           Text.Printf (printf)

{-------------------------------------------------------------------------------
  Analyse
-------------------------------------------------------------------------------}

openLedgerDB ::
     ( LedgerSupportsProtocol blk
     , InspectLedger blk
     , LedgerDB.LedgerDbSerialiseConstraints blk
     , HasHardForkHistory blk
     )
  => Complete LedgerDB.LedgerDbArgs IO blk
  -> IO ( LedgerDB.LedgerDB' IO blk
        , LedgerDB.TestInternals' IO blk
        )
openLedgerDB lgrDbArgs@LedgerDB.LedgerDbArgs{LedgerDB.lgrFlavorArgs=LedgerDB.LedgerDbFlavorArgsV1 bss} = do
  (ledgerDB, _, intLedgerDB) <-
    LedgerDB.openDBInternal
      lgrDbArgs
      (LedgerDB.V1.mkInitDb
        lgrDbArgs
        bss
        (\_ -> error "no replay"))
      emptyStream
      genesisPoint
  pure (ledgerDB, intLedgerDB)
openLedgerDB LedgerDB.LedgerDbArgs{LedgerDB.lgrFlavorArgs=LedgerDB.LedgerDbFlavorArgsV2{}} =
  error "not defined for v2, use v1 instead for now!"

emptyStream :: Applicative m => ImmutableDB.StreamAPI m blk a
emptyStream = ImmutableDB.StreamAPI $ \_ k -> k $ Right $ pure ImmutableDB.NoMoreItems

defaultLMDBLimits :: LMDB.LMDBLimits
defaultLMDBLimits = LMDB.LMDBLimits
   { LMDB.lmdbMapSize = 16 * 1024 * 1024 * 1024
   , LMDB.lmdbMaxDatabases = 10
   , LMDB.lmdbMaxReaders = 16
   }

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
analyse DBAnalyserConfig{analysis, confLimit, dbDir, selectDB, validation, verbose, ldbBackend, diskSnapshotChecksumOnRead} args =
    withRegistry $ \registry -> do
      lock           <- newMVar ()
      chainDBTracer  <- mkTracer lock verbose
      analysisTracer <- mkTracer lock True
      ProtocolInfo { pInfoInitLedger = genesisLedger, pInfoConfig = cfg } <-
        mkProtocolInfo args
      let shfs = Node.stdMkChainDbHasFS dbDir
          chunkInfo  = Node.nodeImmutableDbChunkInfo (configStorage cfg)
          flavargs = case ldbBackend of
            V1InMem -> LedgerDB.LedgerDbFlavorArgsV1
               ( LedgerDB.V1.V1Args
                   LedgerDB.V1.DisableFlushing
                   LedgerDB.V1.DisableQuerySize
                   LedgerDB.V1.InMemoryBackingStoreArgs
               )
            V1LMDB  -> LedgerDB.LedgerDbFlavorArgsV1
               ( LedgerDB.V1.V1Args
                   LedgerDB.V1.DisableFlushing
                   LedgerDB.V1.DisableQuerySize
                   ( LedgerDB.V1.LMDBBackingStoreArgs
                       (BS.LiveLMDBFS (shfs (ChainDB.RelativeMountPoint "lmdb")))
                       defaultLMDBLimits
                       Dict.Dict
                   )
               )
            V2InMem -> LedgerDB.LedgerDbFlavorArgsV2
                         (LedgerDB.V2.V2Args LedgerDB.V2.InMemoryHandleArgs)
          args' =
            ChainDB.completeChainDbArgs
              registry
              cfg
              genesisLedger
              chunkInfo
              (const True)
              shfs
              shfs
              flavargs $
            ChainDB.defaultArgs
          chainDbArgs = maybeValidateAll $ ChainDB.updateTracer chainDBTracer args'
          immutableDbArgs = ChainDB.cdbImmDbArgs chainDbArgs
          ldbArgs = ChainDB.cdbLgrDbArgs args'

      withImmutableDB immutableDbArgs $ \(immutableDB, internal) -> do
        SomeAnalysis (Proxy :: Proxy startFrom) ana <- pure $ runAnalysis analysis
        startFrom <- case sing :: Sing startFrom of
          SStartFromPoint       -> FromPoint <$> case startSlot of
            Origin         -> pure GenesisPoint
            NotOrigin slot -> ImmutableDB.getHashForSlot internal slot >>= \case
              Just hash -> pure $ BlockPoint slot hash
              Nothing   -> fail $ "No block with given slot in the ImmutableDB: " <> show slot
          SStartFromLedgerState -> do
            (ledgerDB, intLedgerDB) <- openLedgerDB ldbArgs
            -- This marker divides the "loading" phase of the program, where the
            -- system is principally occupied with reading snapshot data from
            -- disk, from the "processing" phase, where we are streaming blocks
            -- and running the ledger processing on them.
            Debug.traceMarkerIO "SNAPSHOT_LOADED"
            pure $ FromLedgerState ledgerDB intLedgerDB

        result <- ana AnalysisEnv {
            cfg
          , startFrom
          , db = immutableDB
          , registry
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
      (_, Just ValidateAllBlocks)      -> ChainDB.ensureValidateAll
      (_, Just MinimumBlockValidation) -> id
      (OnlyValidation, _ )             -> ChainDB.ensureValidateAll
      _                                -> id
