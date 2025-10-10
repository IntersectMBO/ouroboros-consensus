{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Tools.DBAnalyser.Run (analyse) where

import Cardano.Ledger.BaseTypes
import Cardano.Tools.DBAnalyser.Analysis
import Cardano.Tools.DBAnalyser.HasAnalysis
import Cardano.Tools.DBAnalyser.Types
import Control.ResourceRegistry
import Control.Tracer (Tracer (..), nullTracer)
import Data.Functor.Contravariant ((>$<))
import qualified Data.SOP.Dict as Dict
import Data.Singletons (Sing, SingI (..))
import qualified Debug.Trace as Debug
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Inspect
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as LedgerSupportsMempool
  ( HasTxs
  )
import Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.Node as Node
import qualified Ouroboros.Consensus.Node.InitStorage as Node
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as ChainDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Stream as ImmutableDB
import Ouroboros.Consensus.Storage.LedgerDB (TraceEvent (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1 as LedgerDB.V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Args as LedgerDB.V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore as LedgerDB.V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB as LMDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Snapshots as LedgerDB.V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2 as LedgerDB.V2
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Backend as LedgerDB.V2
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory as InMemory
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LSM as LSM
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.Orphans ()
import Ouroboros.Network.Block (genesisPoint)
import System.FS.API
import System.IO
import System.Random
import Text.Printf (printf)

{-------------------------------------------------------------------------------
  Analyse
-------------------------------------------------------------------------------}

openLedgerDB ::
  forall blk.
  ( LedgerSupportsProtocol blk
  , InspectLedger blk
  , HasHardForkHistory blk
  , LedgerDB.LedgerSupportsLedgerDB blk
  ) =>
  Complete LedgerDB.LedgerDbArgs IO blk ->
  IO
    ( LedgerDB.LedgerDB' IO blk
    , LedgerDB.TestInternals' IO blk
    )
openLedgerDB args = do
  (ldb, _, od) <- case LedgerDB.lgrBackendArgs args of
    LedgerDB.LedgerDbBackendArgsV1 bss ->
      let snapManager = LedgerDB.V1.snapshotManager args
          initDb =
            LedgerDB.V1.mkInitDb
              args
              bss
              (\_ -> pure (error "no stream"))
              snapManager
              (LedgerDB.praosGetVolatileSuffix $ LedgerDB.ledgerDbCfgSecParam $ LedgerDB.lgrConfig args)
       in LedgerDB.openDBInternal args initDb snapManager emptyStream genesisPoint
    LedgerDB.LedgerDbBackendArgsV2 (LedgerDB.V2.SomeBackendArgs bArgs) -> do
      res <-
        LedgerDB.V2.mkResources
          (Proxy @blk)
          (LedgerDBFlavorImplEvent . LedgerDB.FlavorImplSpecificTraceV2 >$< LedgerDB.lgrTracer args)
          bArgs
          (LedgerDB.lgrRegistry args)
          (LedgerDB.lgrHasFS args)
      let snapManager =
            LedgerDB.V2.snapshotManager
              (Proxy @blk)
              res
              (configCodec . getExtLedgerCfg . LedgerDB.ledgerDbCfg $ LedgerDB.lgrConfig args)
              (LedgerDBSnapshotEvent >$< LedgerDB.lgrTracer args)
              (LedgerDB.lgrHasFS args)
      let initDb =
            LedgerDB.V2.mkInitDb
              args
              (\_ -> pure (error "no stream"))
              snapManager
              (LedgerDB.praosGetVolatileSuffix $ LedgerDB.ledgerDbCfgSecParam $ LedgerDB.lgrConfig args)
              res
      LedgerDB.openDBInternal args initDb snapManager emptyStream genesisPoint
  pure (ldb, od)

emptyStream :: Applicative m => ImmutableDB.StreamAPI m blk a
emptyStream = ImmutableDB.StreamAPI $ \_ k -> k $ Right $ pure ImmutableDB.NoMoreItems

defaultLMDBLimits :: LMDB.LMDBLimits
defaultLMDBLimits =
  LMDB.LMDBLimits
    { LMDB.lmdbMapSize = 16 * 1024 * 1024 * 1024
    , LMDB.lmdbMaxDatabases = 10
    , LMDB.lmdbMaxReaders = 16
    }

analyse ::
  forall blk.
  ( Node.RunNode blk
  , Show (Header blk)
  , HasAnalysis blk
  , HasProtocolInfo blk
  , LedgerSupportsMempool.HasTxs blk
  , CanStowLedgerTables (LedgerState blk)
  ) =>
  DBAnalyserConfig ->
  Args blk ->
  IO (Maybe AnalysisResult)
analyse dbaConfig args =
  withRegistry $ \registry -> do
    lock <- newMVar ()
    chainDBTracer <- mkTracer lock verbose
    analysisTracer <- mkTracer lock True
    lsmSalt <- fst . genWord64 <$> newStdGen
    ProtocolInfo{pInfoInitLedger = genesisLedger, pInfoConfig = cfg} <-
      mkProtocolInfo args
    let shfs = Node.stdMkChainDbHasFS dbDir
        chunkInfo = Node.nodeImmutableDbChunkInfo (configStorage cfg)
        flavargs = case ldbBackend of
          V1LMDB ->
            LedgerDB.LedgerDbBackendArgsV1
              $ LedgerDB.V1.V1Args
                LedgerDB.V1.DisableFlushing
              $ LedgerDB.V1.SomeBackendArgs
              $ LMDB.LMDBBackingStoreArgs
                "lmdb"
                defaultLMDBLimits
                Dict.Dict
          V2InMem ->
            LedgerDB.LedgerDbBackendArgsV2 $
              LedgerDB.V2.SomeBackendArgs InMemory.InMemArgs
          V2LSM ->
            LedgerDB.LedgerDbBackendArgsV2 $
              LedgerDB.V2.SomeBackendArgs $
                LSM.LSMArgs (mkFsPath ["lsm"]) lsmSalt (LSM.stdMkBlockIOFS dbDir)

        args' =
          ChainDB.completeChainDbArgs
            registry
            cfg
            genesisLedger
            chunkInfo
            (const True)
            shfs
            shfs
            flavargs
            $ ChainDB.defaultArgs
        -- Set @k=1@ to reduce the memory usage of the LedgerDB. We only ever
        -- go forward so we don't need to account for rollbacks.
        args'' =
          args'
            { ChainDB.cdbLgrDbArgs =
                ( \x ->
                    x
                      { LedgerDB.lgrConfig =
                          LedgerDB.LedgerDbCfg
                            (SecurityParam (knownNonZeroBounded @1))
                            (LedgerDB.ledgerDbCfg $ LedgerDB.lgrConfig x)
                            OmitLedgerEvents
                      }
                )
                  (ChainDB.cdbLgrDbArgs args')
            }
        chainDbArgs = maybeValidateAll $ ChainDB.updateTracer chainDBTracer args''
        immutableDbArgs = ChainDB.cdbImmDbArgs chainDbArgs
        ldbArgs = ChainDB.cdbLgrDbArgs args''

    withImmutableDB immutableDbArgs $ \(immutableDB, internal) -> do
      SomeAnalysis (Proxy :: Proxy startFrom) ana <- pure $ runAnalysis analysis
      startFrom <- case sing :: Sing startFrom of
        SStartFromPoint ->
          FromPoint <$> case startSlot of
            Origin -> pure GenesisPoint
            NotOrigin slot ->
              ImmutableDB.getHashForSlot internal slot >>= \case
                Just hash -> pure $ BlockPoint slot hash
                Nothing -> fail $ "No block with given slot in the ImmutableDB: " <> show slot
        SStartFromLedgerState -> do
          (ledgerDB, intLedgerDB) <- openLedgerDB ldbArgs
          -- This marker divides the "loading" phase of the program, where the
          -- system is principally occupied with reading snapshot data from
          -- disk, from the "processing" phase, where we are streaming blocks
          -- and running the ledger processing on them.
          Debug.traceMarkerIO "SNAPSHOT_LOADED"
          pure $ FromLedgerState ledgerDB intLedgerDB

      result <-
        ana
          AnalysisEnv
            { cfg
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
  DBAnalyserConfig
    { analysis
    , confLimit
    , dbDir
    , selectDB
    , validation
    , verbose
    , ldbBackend
    } = dbaConfig

  SelectImmutableDB startSlot = selectDB

  withImmutableDB immutableDbArgs =
    bracket
      (ImmutableDB.openDBInternal immutableDbArgs runWithTempRegistry)
      (ImmutableDB.closeDB . fst)

  mkTracer _ False = return nullTracer
  mkTracer lock True = do
    startTime <- getMonotonicTime
    return $ Tracer $ \ev -> withLock $ do
      traceTime <- getMonotonicTime
      let diff = diffTime traceTime startTime
      hPutStrLn stderr $ printf "[%.6fs] %s" (realToFrac diff :: Double) (show ev)
      hFlush stderr
   where
    withLock = bracket_ (takeMVar lock) (putMVar lock ())

  maybeValidateAll = case (analysis, validation) of
    (_, Just ValidateAllBlocks) -> ChainDB.ensureValidateAll
    (_, Just MinimumBlockValidation) -> id
    (OnlyValidation, _) -> ChainDB.ensureValidateAll
    _ -> id
