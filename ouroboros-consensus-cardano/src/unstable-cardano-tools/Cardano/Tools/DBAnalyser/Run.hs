{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Tools.DBAnalyser.Run (analyse) where

import Cardano.Ledger.BaseTypes
import Cardano.Tools.DBAnalyser.Analysis
import Cardano.Tools.DBAnalyser.HasAnalysis
import Cardano.Tools.DBAnalyser.Types
import Control.Monad (unless)
import Control.Monad.Trans.Class
import Control.ResourceRegistry
import Control.Tracer (Tracer (..), nullTracer)
import Data.Functor.Contravariant ((>$<))
import Data.Singletons (Sing, SingI (..))
import qualified Debug.Trace as Debug
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Inspect
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as LedgerSupportsMempool
  ( HasTxs
  )
import Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.Node as Node
import qualified Ouroboros.Consensus.Node.InitStorage as Node
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Consensus.Protocol.Abstract
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as ChainDB
import Ouroboros.Consensus.Storage.Common (BlockComponent (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Stream as ImmutableDB
import Ouroboros.Consensus.Storage.LedgerDB (TraceEvent (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
  ( DiskSnapshot (..)
  , listSnapshots
  )
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
import System.Random (genWord64, newStdGen)
import Text.Printf (printf)

{-------------------------------------------------------------------------------
  Analyse
-------------------------------------------------------------------------------}

openLedgerDB ::
  forall blk.
  ( LedgerSupportsProtocol blk
  , InspectLedger blk
  , HasHardForkHistory blk
  ) =>
  Complete LedgerDB.LedgerDbArgs IO blk ->
  ImmutableDB.ImmutableDB IO blk ->
  -- | The replay goal, i.e. the point up to which blocks from the ImmutableDB
  -- are replayed on top of the chosen snapshot. The chosen snapshot is the
  -- newest one that is not more recent than this point, and blocks are replayed
  -- on top of it until the ledger state is exactly at this point. For
  -- db-analyser this is the @--analyse-from@ point, so that an analysis
  -- starting from a ledger state begins exactly there, regardless of which
  -- snapshots happen to exist on disk.
  Point blk ->
  IO
    ( LedgerDB.LedgerDB' IO blk
    , LedgerDB.TestInternals' IO blk
    )
openLedgerDB args immutableDB replayGoal =
  runWithTempRegistry $
    (,()) <$> do
      (ldb, od) <- case LedgerDB.lgrBackendArgs args of
        LedgerDB.LedgerDbBackendArgsV2 (LedgerDB.V2.SomeBackendArgs bArgs) -> do
          res <-
            LedgerDB.V2.mkResources
              (Proxy @blk)
              (LedgerDBFlavorImplEvent . LedgerDB.FlavorImplSpecificTraceV2 >$< LedgerDB.lgrTracer args)
              bArgs
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
          lift $ do
            warnUnlessSnapshotAtGoal snapManager
            LedgerDB.openDBInternal args initDb snapManager replayStream replayGoal
      pure (ldb, od)
 where
  -- Stream blocks from the ImmutableDB, stopping as soon as we reach a block
  -- that is more recent than the replay goal. As a result, the replay leaves
  -- the ledger state exactly at the replay goal (the last block at or before
  -- it), rather than streaming all the way to the ImmutableDB tip.
  replayStream = ImmutableDB.streamAPI' shouldStop GetBlock immutableDB
   where
    shouldStop blk
      | NotOrigin (blockSlot blk) > pointSlot replayGoal = pure ImmutableDB.NoMoreItems
      | otherwise = pure $ ImmutableDB.NextItem blk

  -- Warn when there is no snapshot exactly at the requested replay goal. In
  -- that case the LedgerDB is initialised from the newest older snapshot and
  -- blocks are replayed up to the goal, which can be considerably slower than
  -- starting from a snapshot that already sits at the requested slot.
  warnUnlessSnapshotAtGoal snapManager =
    case pointSlot replayGoal of
      Origin -> pure ()
      NotOrigin slot -> do
        snapshots <- listSnapshots snapManager
        unless (any ((== unSlotNo slot) . dsNumber) snapshots) $
          hPutStrLn stderr $
            "Warning: no ledger snapshot exists exactly at slot "
              <> show (unSlotNo slot)
              <> "; starting from the newest older snapshot and replaying blocks up to that slot."

analyse ::
  forall blk.
  ( Node.RunNode blk
  , Show (Header blk)
  , Show (ReasonForSwitch (TiebreakerView (BlockProtocol blk)))
  , Show (TxIn blk)
  , Show (TxOut blk)
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
    snapshotDelayRng <- newStdGen
    let shfs = Node.stdMkChainDbHasFS dbDir
        chunkInfo = Node.nodeImmutableDbChunkInfo (configStorage cfg)
        flavargs = case ldbBackend of
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
            snapshotDelayRng
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

      let getPointForSlot :: SlotNo -> IO (Point blk)
          getPointForSlot slot =
            ImmutableDB.getHashForSlot internal slot >>= \case
              Just hash -> pure $ BlockPoint slot hash
              Nothing -> fail $ "No block with given slot in the ImmutableDB: " <> show slot

      startFrom <- case sing :: Sing startFrom of
        SStartFromPoint -> do
          FromPoint <$> case startSlot of
            Origin -> pure GenesisPoint
            NotOrigin slot -> getPointForSlot slot
        SStartFromLedgerState -> do
          (ledgerDB, intLedgerDB) <-
            openLedgerDB ldbArgs immutableDB =<< case startSlot of
              Origin -> pure genesisPoint
              NotOrigin slot -> getPointForSlot slot
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
      (ImmutableDB.openDBInternal immutableDbArgs)
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
