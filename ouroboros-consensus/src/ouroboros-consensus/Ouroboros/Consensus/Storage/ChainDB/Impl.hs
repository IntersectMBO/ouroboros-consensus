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
import Control.ResourceRegistry
  ( WithTempRegistry
  , allocate
  , runInnerWithTempRegistry
  , runWithTempRegistry
  , withRegistry
  )
import Control.Tracer
import Data.Functor ((<&>))
import Data.Functor.Contravariant ((>$<))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.Fragment.Validated as VF
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.HeaderValidation (mkHeaderWithTime)
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Ledger.Inspect
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
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import Ouroboros.Consensus.Util (newFuse, whenJust, withFuse)
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM
  ( Fingerprint (..)
  , WithFingerprint (..)
  )
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
  lift $ traceWith tracer $ TraceOpenEvent StartedOpeningDB
  lift $ traceWith tracer $ TraceOpenEvent StartedOpeningImmutableDB
  immutableDB <- ImmutableDB.openDB argsImmutableDb $ innerOpenCont ImmutableDB.closeDB
  immutableDbTipPoint <- lift $ atomically $ ImmutableDB.getTipPoint immutableDB
  let immutableDbTipChunk =
        chunkIndexOfPoint (ImmutableDB.immChunkInfo argsImmutableDb) immutableDbTipPoint
  lift $
    traceWith tracer $
      TraceOpenEvent $
        OpenedImmutableDB immutableDbTipPoint immutableDbTipChunk

  lift $ traceWith tracer $ TraceOpenEvent StartedOpeningVolatileDB
  volatileDB <- VolatileDB.openDB argsVolatileDb $ innerOpenCont VolatileDB.closeDB
  maxSlot <- lift $ atomically $ VolatileDB.getMaxSlotNo volatileDB
  (chainDB, testing, env) <- lift $ do
    traceWith tracer $ TraceOpenEvent (OpenedVolatileDB maxSlot)
    traceWith tracer $ TraceOpenEvent StartedOpeningLgrDB
    (lgrDB, replayed) <-
      LedgerDB.openDB
        argsLgrDb
        (ImmutableDB.streamAPI immutableDB)
        immutableDbTipPoint
        (Query.getAnyKnownBlock immutableDB volatileDB)
    traceWith tracer $ TraceOpenEvent OpenedLgrDB

    varInvalid <- newTVarIO (WithFingerprint Map.empty (Fingerprint 0))

    let initChainSelTracer = TraceInitChainSelEvent >$< tracer

    traceWith initChainSelTracer StartedInitChainSelection
    initialLoE <- Args.cdbsLoE cdbSpecificArgs
    chain <- withRegistry $ \rr -> do
      chainAndLedger <-
        ChainSel.initialChainSelection
          immutableDB
          volatileDB
          lgrDB
          rr
          initChainSelTracer
          (Args.cdbsTopLevelConfig cdbSpecificArgs)
          varInvalid
          (void initialLoE)
      traceWith initChainSelTracer InitialChainSelected

      let chain = VF.validatedFragment chainAndLedger
          ledger = VF.validatedLedger chainAndLedger

      atomically $ LedgerDB.forkerCommit ledger
      LedgerDB.forkerClose ledger
      pure chain
    LedgerDB.tryFlush lgrDB

    curLedger <- atomically $ LedgerDB.getVolatileTip lgrDB
    let lcfg = configLedger (Args.cdbsTopLevelConfig cdbSpecificArgs)

        -- the volatile tip ledger state can translate the slots of the volatile
        -- headers
        chainWithTime =
          AF.mapAnchoredFragment
            ( mkHeaderWithTime
                lcfg
                (ledgerState curLedger)
            )
            chain

    varChain <- newTVarWithInvariantIO checkInternalChain $ InternalChain chain chainWithTime
    varTentativeState <- newTVarIO $ initialTentativeHeaderState (Proxy @blk)
    varTentativeHeader <- newTVarIO SNothing
    varIterators <- newTVarIO Map.empty
    varFollowers <- newTVarIO Map.empty
    varNextIteratorKey <- newTVarIO (IteratorKey 0)
    varNextFollowerKey <- newTVarIO (FollowerKey 0)
    varKillBgThreads <- newTVarIO $ return ()
    copyFuse <- newFuse "copy to immutable db"
    chainSelFuse <- newFuse "chain selection"
    chainSelQueue <- newChainSelQueue (Args.cdbsBlocksToAddSize cdbSpecificArgs)
    varChainSelStarvation <- newTVarIO ChainSelStarvationOngoing

    let env =
          CDB
            { cdbImmutableDB = immutableDB
            , cdbVolatileDB = volatileDB
            , cdbLedgerDB = lgrDB
            , cdbChain = varChain
            , cdbTentativeState = varTentativeState
            , cdbTentativeHeader = varTentativeHeader
            , cdbIterators = varIterators
            , cdbFollowers = varFollowers
            , cdbTopLevelConfig = Args.cdbsTopLevelConfig cdbSpecificArgs
            , cdbInvalid = varInvalid
            , cdbNextIteratorKey = varNextIteratorKey
            , cdbNextFollowerKey = varNextFollowerKey
            , cdbCopyFuse = copyFuse
            , cdbChainSelFuse = chainSelFuse
            , cdbTracer = tracer
            , cdbRegistry = Args.cdbsRegistry cdbSpecificArgs
            , cdbGcDelay = Args.cdbsGcDelay cdbSpecificArgs
            , cdbGcInterval = Args.cdbsGcInterval cdbSpecificArgs
            , cdbKillBgThreads = varKillBgThreads
            , cdbChainSelQueue = chainSelQueue
            , cdbLoE = Args.cdbsLoE cdbSpecificArgs
            , cdbChainSelStarvation = varChainSelStarvation
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
            , getReadOnlyForkerAtPoint = getEnv2 h Query.getReadOnlyForkerAtPoint
            , getStatistics = getEnv h Query.getStatistics
            }
    addBlockTestFuse <- newFuse "test chain selection"
    copyTestFuse <- newFuse "test copy to immutable db"
    let testing =
          Internal
            { intCopyToImmutableDB = getEnv h (withFuse copyTestFuse . Background.copyToImmutableDB)
            , intGarbageCollect = \slot -> getEnv h $ \e -> do
                Background.garbageCollectBlocks e slot
                LedgerDB.garbageCollect (cdbLedgerDB e) slot
            , intTryTakeSnapshot = getEnv h $ \env' ->
                void $ LedgerDB.tryTakeSnapshot (cdbLedgerDB env') Nothing maxBound
            , intAddBlockRunner = getEnv h (Background.addBlockRunner addBlockTestFuse)
            , intKillBgThreads = varKillBgThreads
            }

    traceWith tracer $
      TraceOpenEvent $
        OpenedDB
          (castPoint $ AF.anchorPoint chain)
          (castPoint $ AF.headPoint chain)

    when launchBgTasks $ Background.launchBgTasks env replayed

    return (chainDB, testing, env)

  _ <- lift $ allocate (Args.cdbsRegistry cdbSpecificArgs) (\_ -> return chainDB) API.closeDB

  return ((chainDB, testing), env)
 where
  tracer = Args.cdbsTracer cdbSpecificArgs
  Args.ChainDbArgs argsImmutableDb argsVolatileDb argsLgrDb cdbSpecificArgs = args

-- | We use 'runInnerWithTempRegistry' for the component databases.
innerOpenCont ::
  IOLike m =>
  (innerDB -> m ()) ->
  WithTempRegistry st m (innerDB, st) ->
  WithTempRegistry (ChainDbEnv m blk) m innerDB
innerOpenCont closer m =
  runInnerWithTempRegistry
    (fmap (\(innerDB, st) -> (innerDB, st, innerDB)) m)
    ((True <$) . closer)
    (\_env _innerDB -> True)

-- This check is degenerate because handles in @_env@ and the
-- @_innerDB@ handle do not support an equality check; all of the
-- identifying data is only in the handle's closure, not
-- accessible because of our intentional encapsulation choices.

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
    Follower.closeAllFollowers cdb
    Iterator.closeAllIterators cdb

    killBgThreads <- atomically $ readTVar cdbKillBgThreads
    killBgThreads

    ImmutableDB.closeDB cdbImmutableDB
    VolatileDB.closeDB cdbVolatileDB
    LedgerDB.closeDB cdbLedgerDB

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
