{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Storage.ChainDB.Impl
  ( -- * Initialization
    ChainDbArgs (..)
  , SerialiseDiskConstraints
  , defaultArgs
  , openDB
  , withDB

    -- * Trace types
  , SelectionChangedInfo (..)
  , TraceAddBlockEvent (..)
  , TraceAddPerasCertEvent (..)
  , TraceChainSelStarvationEvent (..)
  , TraceCopyToImmutableDBEvent (..)
  , TraceEvent (..)
  , TraceFollowerEvent (..)
  , TraceGCEvent (..)
  , TraceInitChainSelEvent (..)
  , TraceIteratorEvent (..)
  , TraceOpenEvent (..)
  , TracePipeliningEvent (..)
  , TraceValidationEvent (..)

    -- * Re-exported for convenience
  , Args.RelativeMountPoint (..)
  , ImmutableDB.ImmutableDbSerialiseConstraints
  , LedgerDB.LedgerDbSerialiseConstraints
  , VolatileDB.VolatileDbSerialiseConstraints

    -- * Internals for testing purposes
  , Internal (..)
  , openDBInternal
  ) where

import Control.Monad (void, when)
import Control.Monad.Trans.Class (lift)
import qualified Control.RAWLock as RAW
import Control.ResourceRegistry
  ( allocate
  , cancelThread
  , forkLinkedThread
  , runWithTempRegistry
  )
import Control.Tracer
import Data.Functor ((<&>))
import Data.Functor.Contravariant ((>$<))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import GHC.Stack (HasCallStack)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.HeaderValidation (HeaderWithTime, mkHeaderWithTime)
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Ledger.SupportsPeras (LedgerSupportsPeras)
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as API
import Ouroboros.Consensus.Storage.ChainDB.Impl.Args
  ( ChainDbArgs
  , defaultArgs
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as Args
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Background as Background
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ChainSel as ChainSel
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Follower as Follower
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Iterator as Iterator
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Query as Query
import Ouroboros.Consensus.Storage.ChainDB.Impl.Types
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Stream as ImmutableDB
import Ouroboros.Consensus.Storage.LedgerDB (LedgerSupportsLedgerDB)
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import qualified Ouroboros.Consensus.Storage.PerasCertDB as PerasCertDB
import qualified Ouroboros.Consensus.Storage.PerasVoteDB as PerasVoteDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import Ouroboros.Consensus.Util (newFuse, whenJust)
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM
  ( Fingerprint (..)
  , WithFingerprint (..)
  )
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.BlockFetch.ConsensusInterface
  ( ChainSelStarvation (..)
  )

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

withDB ::
  forall m blk a.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , LedgerSupportsPeras blk
  , BlockSupportsDiffusionPipelining blk
  , InspectLedger blk
  , HasHardForkHistory blk
  , ConvertRawHash blk
  , SerialiseDiskConstraints blk
  , LedgerSupportsLedgerDB blk
  ) =>
  Complete Args.ChainDbArgs m blk ->
  (ChainDB m blk -> m a) ->
  m a
withDB args = bracket (fst <$> openDBInternal args True) API.closeDB

openDB ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , LedgerSupportsPeras blk
  , BlockSupportsDiffusionPipelining blk
  , InspectLedger blk
  , HasHardForkHistory blk
  , ConvertRawHash blk
  , SerialiseDiskConstraints blk
  , LedgerSupportsLedgerDB blk
  ) =>
  Complete Args.ChainDbArgs m blk ->
  m (ChainDB m blk)
openDB args = fst <$> openDBInternal args True

openDBInternal ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , LedgerSupportsPeras blk
  , BlockSupportsDiffusionPipelining blk
  , InspectLedger blk
  , HasHardForkHistory blk
  , ConvertRawHash blk
  , SerialiseDiskConstraints blk
  , HasCallStack
  , LedgerSupportsLedgerDB blk
  ) =>
  Complete Args.ChainDbArgs m blk ->
  -- | 'True' = Launch background tasks
  Bool ->
  m (ChainDB m blk, Internal m blk)
openDBInternal args launchBgTasks = runWithTempRegistry $ do
  ( immutableDB
    , immutableDbTipPoint
    , volatileDB
    , ledgerDbGetVolatileSuffix
    , setGetCurrentChainForLedgerDB
    ) <- lift $ do
    traceWith tracer $ TraceOpenEvent StartedOpeningDB
    traceWith tracer $ TraceOpenEvent StartedOpeningImmutableDB
    immutableDB <- ImmutableDB.openDB argsImmutableDb
    immutableDbTipPoint <- atomically $ ImmutableDB.getTipPoint immutableDB
    let immutableDbTipChunk =
          chunkIndexOfPoint (ImmutableDB.immChunkInfo argsImmutableDb) immutableDbTipPoint
    traceWith tracer $
      TraceOpenEvent $
        OpenedImmutableDB immutableDbTipPoint immutableDbTipChunk

    traceWith tracer $ TraceOpenEvent StartedOpeningVolatileDB
    volatileDB <- VolatileDB.openDB argsVolatileDb
    maxSlot <- atomically $ VolatileDB.getMaxSlotNo volatileDB
    traceWith tracer $ TraceOpenEvent (OpenedVolatileDB maxSlot)
    -- The 'StartedOpeningLgrDB' / 'OpenedLgrDB' trace events fire later,
    -- inside 'runInitLedgerDB', once the background thread starts the
    -- LedgerDB-open work.
    (ledgerDbGetVolatileSuffix, setGetCurrentChainForLedgerDB) <-
      mkLedgerDbGetVolatileSuffix
    pure
      ( immutableDB
      , immutableDbTipPoint
      , volatileDB
      , ledgerDbGetVolatileSuffix
      , setGetCurrentChainForLedgerDB
      )

  -- The LedgerDB is opened later inside 'runInitLedgerDB' so that, when
  -- 'EarlyN2C' is enabled, the (potentially long) initial replay can run
  -- on a background thread. See "Resource management in the LedgerDB" in
  -- "Ouroboros.Consensus.Storage.LedgerDB.API" for the temp-registry
  -- handover details.
  lift $ do
    perasCertDB <- PerasCertDB.createDB argsPerasCertDB
    perasVoteDB <- PerasVoteDB.createDB argsPerasVoteDB

    varInvalid <- newTVarIO (WithFingerprint Map.empty (Fingerprint 0))
    varLdbStatus <- newTVarIO LdbReplaying
    varCancelInit <- newTVarIO (return ())

    -- Empty current chain anchored at the immutable tip; the background
    -- initialiser populates this once initial chain selection runs.
    immutableTipAnchor <- atomically $ ImmutableDB.getTipAnchor immutableDB
    let initialChain :: AnchoredFragment (Header blk)
        initialChain = AF.Empty (AF.castAnchor immutableTipAnchor)
        initialChainWithTime :: AnchoredFragment (HeaderWithTime blk)
        initialChainWithTime = AF.Empty (AF.castAnchor immutableTipAnchor)

    varChain <-
      newTVarWithInvariantIO checkInternalChain $
        InternalChain initialChain initialChainWithTime
    varTentativeState <- newTVarIO $ initialTentativeHeaderState (Proxy @blk)
    varTentativeHeader <- newTVarIO SNothing
    varIterators <- newTVarIO Map.empty
    varFollowers <- newTVarIO Map.empty
    varNextIteratorKey <- newTVarIO (IteratorKey 0)
    varNextFollowerKey <- newTVarIO (FollowerKey 0)
    varKillBgThreads <- newTVarIO $ return ()
    immdbLock <- RAW.new ()
    chainSelFuse <- newFuse "chain selection"
    chainSelQueue <- newChainSelQueue (Args.cdbsBlocksToAddSize cdbSpecificArgs)
    varChainSelStarvation <- newTVarIO ChainSelStarvationOngoing
    varSnapshotDelayRNG <- newTVarIO (Args.cdbsSnapshotDelayRNG cdbSpecificArgs)

    let synthesisedLedger =
          case Args.cdbsSynthesiseLedger cdbSpecificArgs of
            Nothing -> Nothing
            Just f -> f immutableDbTipPoint
        env =
          CDB
            { cdbImmutableDB = immutableDB
            , cdbImmutableDBLock = immdbLock
            , cdbVolatileDB = volatileDB
            , cdbLedgerDBStatus = varLdbStatus
            , cdbCancelInit = varCancelInit
            , cdbEarlyN2C =
                Args.cdbsEarlyN2C cdbSpecificArgs
            , cdbSyntheticLedger = synthesisedLedger
            , cdbChain = varChain
            , cdbTentativeState = varTentativeState
            , cdbTentativeHeader = varTentativeHeader
            , cdbIterators = varIterators
            , cdbFollowers = varFollowers
            , cdbTopLevelConfig = Args.cdbsTopLevelConfig cdbSpecificArgs
            , cdbInvalid = varInvalid
            , cdbNextIteratorKey = varNextIteratorKey
            , cdbNextFollowerKey = varNextFollowerKey
            , cdbChainSelFuse = chainSelFuse
            , cdbTracer = tracer
            , cdbRegistry = Args.cdbsRegistry cdbSpecificArgs
            , cdbGcDelay = Args.cdbsGcDelay cdbSpecificArgs
            , cdbGcInterval = Args.cdbsGcInterval cdbSpecificArgs
            , cdbKillBgThreads = varKillBgThreads
            , cdbChainSelQueue = chainSelQueue
            , cdbLoE = Args.cdbsLoE cdbSpecificArgs
            , cdbChainSelStarvation = varChainSelStarvation
            , cdbPerasCertDB = perasCertDB
            , cdbPerasVoteDB = perasVoteDB
            , cdbSnapshotDelayRNG = varSnapshotDelayRNG
            }

    h <- fmap CDBHandle $ newTVarIO $ ChainDbOpen env
    let chainDB =
          API.ChainDB
            { addBlockAsync = getEnv2 h ChainSel.addBlockAsync
            , chainSelAsync = getEnv h ChainSel.triggerChainSelectionAsync
            , getCurrentChain = getEnvSTM h Query.getCurrentChain
            , getCurrentChainWithTime = getEnvSTM h Query.getCurrentChainWithTime
            , getTipBlock = getEnv h Query.getTipBlock
            , getTipHeader = getEnv h Query.getTipHeader
            , getTipPoint = getEnvSTM h Query.getTipPoint
            , getBlockComponent = getEnv2 h Query.getBlockComponent
            , getIsFetched = getEnvSTM h Query.getIsFetched
            , getIsValid = getEnvSTM h Query.getIsValid
            , getMaxSlotNo = getEnvSTM h Query.getMaxSlotNo
            , stream = Iterator.stream h
            , newFollower = Follower.newFollower h
            , getIsInvalidBlock = getEnvSTM h Query.getIsInvalidBlock
            , getChainSelStarvation = getEnvSTM h Query.getChainSelStarvation
            , closeDB = closeDB h
            , isOpen = isOpen h
            , getCurrentLedger = getEnvSTM h Query.getCurrentLedger
            , getImmutableLedger = getEnvSTM h Query.getImmutableLedger
            , getPastLedger = getEnvSTM1 h Query.getPastLedger
            , getHeaderStateHistory = getEnvSTM h Query.getHeaderStateHistory
            , allocInRegistryReadOnlyForkerAtPoint = getEnv2 h Query.allocInRegistryReadOnlyForkerAtPoint
            , openReadOnlyForkerAtPoint = getEnv1 h Query.openReadOnlyForkerAtPoint
            , withReadOnlyForkerAtPoint = getEnvTrans2 h Query.withReadOnlyForkerAtPoint
            , getStatistics = getEnv h Query.getStatistics
            , addPerasCertAsync = getEnv1 h ChainSel.addPerasCertAsync
            , getPerasWeightSnapshot = getEnvSTM h Query.getPerasWeightSnapshot
            , getLatestPerasCertSeen = getEnvSTM h Query.getLatestPerasCertSeen
            , getPerasCertsAfter = getEnvSTM1 h Query.getPerasCertsAfter
            , getPerasCertIds = getEnvSTM h Query.getPerasCertIds
            , addPerasVoteWithAsyncCertHandling = getEnv1 h ChainSel.addPerasVoteWithAsyncCertHandling
            , getPerasVotesAfter = getEnvSTM1 h Query.getPerasVotesAfter
            , getPerasVoteIds = getEnvSTM h Query.getPerasVoteIds
            , waitForImmutableBlock = getEnv1 h Query.waitForImmutableBlock
            , getLatestPerasCertOnChainRound = getEnvSTM h Query.getLatestPerasCertOnChainRound
            }
    addBlockTestFuse <- newFuse "test chain selection"
    let testing =
          Internal
            { intCopyToImmutableDB = getEnv h Background.copyToImmutableDB
            , intGarbageCollect = \slot -> getEnv h $ \e -> do
                Background.garbageCollectBlocks e slot
                Background.garbageCollectPeras e slot
                ldb <- atomically $ awaitLedgerDB e
                LedgerDB.garbageCollect ldb slot
            , intTryTakeSnapshot = getEnv2 h $ \env' cb delay -> do
                ldb <- atomically $ awaitLedgerDB env'
                LedgerDB.tryTakeSnapshot ldb cb delay
            , intAddBlockRunner = getEnv h (Background.addBlockRunner addBlockTestFuse)
            , intKillBgThreads = varKillBgThreads
            }

    -- When 'EarlyN2C' is enabled, fork the LedgerDB
    -- initialiser so 'openDB' can return immediately; otherwise run it
    -- inline (original synchronous open).
    case Args.cdbsEarlyN2C cdbSpecificArgs of
      API.EarlyN2CEnabled -> do
        traceWith tracer $
          TraceOpenEvent $
            OpenedDBImmutableReady immutableDbTipPoint

        initThread <-
          forkLinkedThread (Args.cdbsRegistry cdbSpecificArgs) "ChainDB.initLedgerDB" $
            runInitLedgerDB
              args
              env
              immutableDB
              volatileDB
              immutableDbTipPoint
              ledgerDbGetVolatileSuffix
              setGetCurrentChainForLedgerDB
              launchBgTasks
        atomically $ writeTVar varCancelInit (cancelThread initThread)
      API.EarlyN2CDisabled ->
        runInitLedgerDB
          args
          env
          immutableDB
          volatileDB
          immutableDbTipPoint
          ledgerDbGetVolatileSuffix
          setGetCurrentChainForLedgerDB
          launchBgTasks

    -- Note we put the ChainDB in the top level registry before exiting the
    -- 'runWithTempRegistry' scope. This way, the critical resources (actually
    -- only the LedgerDB resources, see "Resource management in the LedgerDB"
    -- note in "Ouroboros.Consensus.Storage.LedgerDB.API") are always tracked by
    -- a registry. As the ChainDB is so fundamental to the execution of
    -- Consensus, it is justified for the LedgerDB to temporarily allocate
    -- resources with 'impossibleToNotTransfer'.
    _ <- allocate (Args.cdbsRegistry cdbSpecificArgs) (\_ -> return chainDB) API.closeDB

    return ((chainDB, testing), env)
 where
  tracer = Args.cdbsTracer cdbSpecificArgs
  Args.ChainDbArgs
    argsImmutableDb
    argsVolatileDb
    _argsLgrDb
    argsPerasCertDB
    argsPerasVoteDB
    cdbSpecificArgs = args

  -- The LedgerDB requires a criterion ('LedgerDB.GetVolatileSuffix')
  -- determining which of its states are volatile/immutable. Once initial
  -- chain selection has run we can defer this decision to
  -- 'Query.getCurrentChain'.
  --
  -- The LedgerDB is initialised on the background thread spawned by
  -- 'openDBInternal' (see 'runInitLedgerDB'). 'setVarChain' is only invoked
  -- once that background work has selected an initial chain. While the
  -- LedgerDB is replaying, the TMVar is empty and the suffix defaults to
  -- 'id', i.e. the entire suffix is treated as volatile. This is fine as we
  -- don't perform any rollbacks during this period.
  mkLedgerDbGetVolatileSuffix ::
    m
      ( LedgerDB.GetVolatileSuffix m blk
      , STM m (AnchoredFragment (Header blk)) -> m ()
      )
  mkLedgerDbGetVolatileSuffix = do
    varGetCurrentChain ::
      StrictTMVar m (OnlyCheckWhnf (STM m (AnchoredFragment (Header blk)))) <-
      newEmptyTMVarIO
    let getVolatileSuffix =
          LedgerDB.GetVolatileSuffix $
            tryReadTMVar varGetCurrentChain >>= \case
              -- If @setVarChain@ has not yet been invoked, return the entire
              -- suffix as volatile.
              Nothing -> pure id
              -- Otherwise, return the suffix with the same length as the
              -- current chain.
              Just (OnlyCheckWhnf getCurrentChain) -> do
                curChainLen <- AF.length <$> getCurrentChain
                pure $ AF.anchorNewest (fromIntegral curChainLen)
        setVarChain = atomically . writeTMVar varGetCurrentChain . OnlyCheckWhnf
    pure (getVolatileSuffix, setVarChain)

-- | Background initialisation of the LedgerDB and initial chain selection.
--
-- Runs on a registry-linked thread spawned by 'openDBInternal'. On success
-- the result of initial chain selection is written to 'cdbChain',
-- 'cdbLedgerDBStatus' is transitioned to 'LdbReady', 'cdbCancelInit' is
-- cleared, and (when requested) the ChainDB background tasks are launched.
-- On failure the linked-thread exception propagates to the parent registry.
runInitLedgerDB ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , BlockSupportsDiffusionPipelining blk
  , InspectLedger blk
  , HasHardForkHistory blk
  , LedgerSupportsLedgerDB blk
  ) =>
  Complete Args.ChainDbArgs m blk ->
  ChainDbEnv m blk ->
  ImmutableDB.ImmutableDB m blk ->
  VolatileDB.VolatileDB m blk ->
  Point blk ->
  LedgerDB.GetVolatileSuffix m blk ->
  (STM m (AnchoredFragment (Header blk)) -> m ()) ->
  Bool ->
  m ()
runInitLedgerDB
  args
  env
  immutableDB
  volatileDB
  immutableDbTipPoint
  ledgerDbGetVolatileSuffix
  setGetCurrentChainForLedgerDB
  launchBgTasks = runWithTempRegistry $ do
    lift $ traceWith tracer $ TraceOpenEvent StartedOpeningLgrDB

    lgrDB <-
      LedgerDB.openDB
        argsLgrDb
        (ImmutableDB.streamAPI immutableDB)
        immutableDbTipPoint
        (Query.getAnyKnownBlock immutableDB volatileDB)
        ledgerDbGetVolatileSuffix

    lift $ do
      traceWith tracer $ TraceOpenEvent OpenedLgrDB

      let initChainSelTracer = TraceInitChainSelEvent >$< tracer
      traceWith initChainSelTracer StartedInitChainSelection
      initialLoE <- Args.cdbsLoE cdbSpecificArgs
      initialWeights <-
        atomically $ PerasCertDB.getWeightSnapshot (cdbPerasCertDB env)
      chain <-
        ChainSel.initialChainSelection
          immutableDB
          volatileDB
          lgrDB
          initChainSelTracer
          (cdbTopLevelConfig env)
          (cdbInvalid env)
          (void initialLoE)
          (forgetFingerprint initialWeights)
      traceWith initChainSelTracer InitialChainSelected
      LedgerDB.tryFlush lgrDB

      curLedger <- atomically $ LedgerDB.getVolatileTip lgrDB
      let lcfg = configLedger (cdbTopLevelConfig env)
          chainWithTime =
            AF.mapAnchoredFragment
              (mkHeaderWithTime lcfg (ledgerState curLedger))
              chain

      atomically $ do
        writeTVar (cdbChain env) (InternalChain chain chainWithTime)
        writeTVar (cdbLedgerDBStatus env) (LdbReady lgrDB)
        writeTVar (cdbCancelInit env) (return ())

      setGetCurrentChainForLedgerDB $ Query.getCurrentChain env

      traceWith tracer $
        TraceOpenEvent $
          OpenedDB
            (castPoint (AF.anchorPoint chain) :: Point blk)
            (castPoint (AF.headPoint chain) :: Point blk)

      when launchBgTasks $ Background.launchBgTasks env

      return ((), env)
   where
    Args.ChainDbArgs
      _argsImmutableDb
      _argsVolatileDb
      argsLgrDb
      _argsPerasCertDB
      _argsPerasVoteDB
      cdbSpecificArgs = args
    tracer = Args.cdbsTracer cdbSpecificArgs

isOpen :: IOLike m => ChainDbHandle m blk -> STM m Bool
isOpen (CDBHandle varState) =
  readTVar varState <&> \case
    ChainDbClosed -> False
    ChainDbOpen _env -> True

closeDB ::
  forall m blk.
  ( IOLike m
  , HasHeader (Header blk)
  , HasCallStack
  ) =>
  ChainDbHandle m blk -> m ()
closeDB (CDBHandle varState) = do
  mbOpenEnv <-
    atomically $
      readTVar varState >>= \case
        -- Idempotent
        ChainDbClosed -> return Nothing
        ChainDbOpen env -> do
          writeTVar varState ChainDbClosed
          return $ Just env

  -- Only when the ChainDB was open
  whenJust mbOpenEnv $ \cdb@CDB{..} -> do
    -- If the LedgerDB is still being initialised in the background, cancel
    -- that work first so it cannot race with the cleanup below.
    cancelInit <- atomically $ readTVar cdbCancelInit
    cancelInit

    Follower.closeAllFollowers cdb
    Iterator.closeAllIterators cdb

    killBgThreads <- atomically $ readTVar cdbKillBgThreads
    killBgThreads

    ImmutableDB.closeDB cdbImmutableDB
    VolatileDB.closeDB cdbVolatileDB

    -- The LedgerDB is only opened by the background initialisation; if that
    -- never reached 'LdbReady', there is no LedgerDB handle to close.
    atomically (readTVar cdbLedgerDBStatus) >>= \case
      LdbReplaying -> pure ()
      LdbReady ldb -> LedgerDB.closeDB ldb

    chain <- atomically $ icWithoutTime <$> readTVar cdbChain

    traceWith cdbTracer $
      TraceOpenEvent $
        ClosedDB
          (castPoint $ AF.anchorPoint chain)
          (castPoint $ AF.headPoint chain)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Lift 'chunkIndexOfSlot' to 'Point'
--
-- Returns 'firstChunkNo' in case of 'GenesisPoint'.
chunkIndexOfPoint :: ImmutableDB.ChunkInfo -> Point blk -> ImmutableDB.ChunkNo
chunkIndexOfPoint chunkInfo = \case
  GenesisPoint -> ImmutableDB.firstChunkNo
  BlockPoint slot _ -> ImmutableDB.chunkIndexOfSlot chunkInfo slot
