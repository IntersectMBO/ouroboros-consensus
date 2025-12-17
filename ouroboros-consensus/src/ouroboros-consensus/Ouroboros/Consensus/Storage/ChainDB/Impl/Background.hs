{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Background tasks:
--
-- * Copying blocks from the VolatileDB to the ImmutableDB
-- * Performing and scheduling garbage collections on the VolatileDB
-- * Writing snapshots of the LedgerDB to disk and deleting old ones
-- * Executing scheduled chain selections
module Ouroboros.Consensus.Storage.ChainDB.Impl.Background
  ( -- * Launch background tasks
    launchBgTasks

    -- * Copying blocks from the VolatileDB to the ImmutableDB
  , copyToImmutableDB

    -- * Executing garbage collection
  , garbageCollectBlocks

    -- * Scheduling garbage collections
  , GcParams (..)
  , GcSchedule
  , computeTimeForGC
  , gcScheduleRunner
  , newGcSchedule
  , scheduleGC

    -- ** Testing
  , ScheduledGc (..)
  , dumpGcSchedule

    -- * Adding blocks to the ChainDB
  , addBlockRunner
  ) where

import Control.Exception (assert)
import Control.Monad (forM_, forever, void, when)
import Control.Monad.Trans.Class (lift)
import Control.ResourceRegistry
import Control.Tracer
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as Seq
import Data.Time.Clock
import Data.Void (Void)
import Data.Word
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.ChainDB.API
  ( AddBlockResult (..)
  , BlockComponent (..)
  )
import Ouroboros.Consensus.Storage.ChainDB.Impl.ChainSel
  ( chainSelSync
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Query as Query
import Ouroboros.Consensus.Storage.ChainDB.Impl.Types
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import qualified Ouroboros.Consensus.Storage.PerasCertDB.API as PerasCertDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM (Watcher (..), forkLinkedWatcher)
import Ouroboros.Network.AnchoredFragment (AnchoredSeq (..))
import qualified Ouroboros.Network.AnchoredFragment as AF

{-------------------------------------------------------------------------------
  Launch background tasks
-------------------------------------------------------------------------------}

launchBgTasks ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , BlockSupportsDiffusionPipelining blk
  , InspectLedger blk
  , HasHardForkHistory blk
  ) =>
  ChainDbEnv m blk ->
  -- | Number of immutable blocks replayed on ledger DB startup
  Word64 ->
  m ()
launchBgTasks cdb@CDB{..} replayed = do
  !addBlockThread <-
    launch "ChainDB.addBlockRunner" $
      addBlockRunner cdbChainSelFuse cdb

  ledgerDbTasksTrigger <- newLedgerDbTasksTrigger replayed
  !ledgerDbMaintenaceThread <-
    forkLinkedWatcher cdbRegistry "ChainDB.ledgerDbTaskWatcher" $
      ledgerDbTaskWatcher cdb ledgerDbTasksTrigger

  gcSchedule <- newGcSchedule
  !gcThread <-
    launch "ChainDB.gcBlocksScheduleRunner" $
      gcScheduleRunner gcSchedule $
        garbageCollectBlocks cdb

  !copyToImmutableDBThread <-
    launch "ChainDB.copyToImmutableDBRunner" $
      copyToImmutableDBRunner cdb ledgerDbTasksTrigger gcSchedule cdbCopyFuse

  atomically $
    writeTVar cdbKillBgThreads $
      sequence_
        [ addBlockThread
        , cancelThread ledgerDbMaintenaceThread
        , gcThread
        , copyToImmutableDBThread
        ]
 where
  launch :: String -> m Void -> m (m ())
  launch = fmap cancelThread .: forkLinkedThread cdbRegistry

{-------------------------------------------------------------------------------
  Copying blocks from the VolatileDB to the ImmutableDB
-------------------------------------------------------------------------------}

-- | Copy the blocks older than the immutable tip from the VolatileDB to the
-- ImmutableDB.
--
-- The headers of these blocks can be retrieved by considering headers in
-- 'cdbChain' that are not also in 'getCurrentChain' (a suffix of 'cdbChain').
--
-- The copied blocks are removed from the fragment stored in 'cdbChain'.
--
-- This function does not remove blocks from the VolatileDB.
--
-- The 'SlotNo' of the tip of the ImmutableDB after copying the blocks is
-- returned. This can be used for a garbage collection on the VolatileDB.
copyToImmutableDB ::
  forall m blk.
  ( IOLike m
  , ConsensusProtocol (BlockProtocol blk)
  , HasHeader blk
  , GetHeader blk
  , HasCallStack
  ) =>
  ChainDbEnv m blk ->
  Electric m (WithOrigin SlotNo)
copyToImmutableDB cdb@CDB{..} = electric $ do
  toCopy <- atomically $ do
    curChain <- icWithoutTime <$> readTVar cdbChain
    curChainVolSuffix <- Query.getCurrentChain cdb
    let nbToCopy = max 0 $ AF.length curChain - AF.length curChainVolSuffix
        toCopy :: [Point blk]
        toCopy =
          map headerPoint $
            AF.toOldestFirst $
              AF.takeOldest nbToCopy curChain
    return toCopy

  if null toCopy
    -- This can't happen in practice, as we're only called when there are new
    -- immutable blocks. However, in the tests, we will be calling this function
    -- manually, which means it might be called when there are no blocks to
    -- copy.
    then trace NoBlocksToCopyToImmutableDB
    else forM_ toCopy $ \pt -> do
      let hash = case pointHash pt of
            BlockHash h -> h
            -- There is no actual genesis block that can occur on a chain
            GenesisHash -> error "genesis block on current chain"
      slotNoAtImmutableDBTip <- atomically $ ImmutableDB.getTipSlot cdbImmutableDB
      assert (pointSlot pt >= slotNoAtImmutableDBTip) $ return ()
      -- When the block is corrupt, the function below will throw an
      -- exception. This exception will make sure that we shut down the node
      -- and that the next time we start, validation will be enabled.
      blk <- VolatileDB.getKnownBlockComponent cdbVolatileDB GetVerifiedBlock hash
      -- We're the only one modifying the ImmutableDB, so the tip cannot
      -- have changed since we last checked it.
      ImmutableDB.appendBlock cdbImmutableDB blk
      -- TODO the invariant of 'cdbChain' is shortly violated between
      -- these two lines: the tip was updated on the line above, but the
      -- anchor point is only updated on the line below.
      atomically $ removeFromChain pt
      trace $ CopiedBlockToImmutableDB pt

  -- Get the /possibly/ updated tip of the ImmutableDB
  atomically $ ImmutableDB.getTipSlot cdbImmutableDB
 where
  trace = traceWith (contramap TraceCopyToImmutableDBEvent cdbTracer)

  -- \| Remove the header corresponding to the given point from the beginning
  -- of the current chain fragment.
  --
  -- PRECONDITION: the header must be the first one (oldest) in the chain
  removeFromChain :: Point blk -> STM m ()
  removeFromChain pt = do
    -- The chain might have been extended in the meantime.
    readTVar cdbChain >>= \case
      InternalChain (hdr :< newChain) (_hwt :< newChainWithTime)
        | headerPoint hdr == pt ->
            writeTVar cdbChain $ InternalChain newChain newChainWithTime
      -- We're the only one removing things from 'cdbChain', so this cannot
      -- happen if the precondition was satisfied.
      _ -> error "header to remove not on the current chain"

{-------------------------------------------------------------------------------
  Copy to ImmutableDB
-------------------------------------------------------------------------------}

-- | Copy blocks from the VolatileDB to ImmutableDB and trigger further tasks in
-- other threads.
--
-- Wait until the current chain ('cdbChain') is longer than its volatile suffix
-- ('getCurrentChain'). When this occurs, it indicates that new blocks have
-- become immutable. These newly immutable blocks are then copied from the
-- VolatileDB to the ImmutableDB (using 'copyToImmutableDB'). Once that is
-- complete,
--
-- * Trigger LedgerDB maintenance tasks, namely flushing, taking snapshots and
--   garbage collection.
--
-- * Schedule GC of the VolatileDB ('scheduleGC') for the 'SlotNo' of the most
--   recent block that was copied.
--
-- It is important that we only take LedgerDB snapshots when are are /sure/ they
-- have been copied to the ImmutableDB, since the LedgerDB assumes that all
-- snapshots correspond to immutable blocks. (Of course, data corruption can
-- occur and we can handle it by reverting to an older LedgerDB snapshot, but we
-- should need this only in exceptional circumstances.)
--
-- We do not store any state of the VolatileDB GC. If the node shuts down before
-- GC can happen, when we restart the node and schedule the /next/ GC, it will
-- /imply/ any previously scheduled GC, since GC is driven by slot number
-- ("garbage collect anything older than @x@").
copyToImmutableDBRunner ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  ) =>
  ChainDbEnv m blk ->
  LedgerDbTasksTrigger m ->
  GcSchedule m ->
  Fuse m ->
  m Void
copyToImmutableDBRunner cdb@CDB{..} ledgerDbTasksTrigger gcSchedule fuse = do
  -- this first flush will persist the differences that come from the initial
  -- chain selection.
  LedgerDB.tryFlush cdbLedgerDB
  forever copyAndTrigger
 where
  copyAndTrigger :: m ()
  copyAndTrigger = do
    -- Wait for 'cdbChain' to become longer than 'getCurrentChain'.
    numToWrite <- atomically $ do
      curChain <- icWithoutTime <$> readTVar cdbChain
      curChainVolSuffix <- Query.getCurrentChain cdb
      let numToWrite = AF.length curChain - AF.length curChainVolSuffix
      check $ numToWrite > 0
      return $ fromIntegral numToWrite

    -- Copy blocks to ImmutableDB
    --
    -- This is a synchronous operation: when it returns, the blocks have been
    -- copied to disk (though not flushed, necessarily).
    gcSlotNo <- withFuse fuse (copyToImmutableDB cdb)

    triggerLedgerDbTasks ledgerDbTasksTrigger gcSlotNo numToWrite
    scheduleGC' gcSlotNo

  scheduleGC' :: WithOrigin SlotNo -> m ()
  scheduleGC' Origin = return ()
  scheduleGC' (NotOrigin slotNo) =
    scheduleGC
      (contramap TraceGCEvent cdbTracer)
      slotNo
      GcParams
        { gcDelay = cdbGcDelay
        , gcInterval = cdbGcInterval
        }
      gcSchedule

{-------------------------------------------------------------------------------
  LedgerDB maintenance tasks
-------------------------------------------------------------------------------}

-- | Trigger for the LedgerDB maintenance tasks, namely whenever the immutable
-- DB tip slot advances when we finish copying blocks to it.
newtype LedgerDbTasksTrigger m
  = LedgerDbTasksTrigger (StrictTVar m LedgerDbTaskState)

data LedgerDbTaskState = LedgerDbTaskState
  { ldbtsImmTip :: !(WithOrigin SlotNo)
  , ldbtsPrevSnapshotTime :: !(Maybe Time)
  , ldbtsBlocksSinceLastSnapshot :: !Word64
  }
  deriving stock Generic
  deriving anyclass NoThunks

newLedgerDbTasksTrigger ::
  IOLike m =>
  -- | Number of blocks replayed.
  Word64 ->
  m (LedgerDbTasksTrigger m)
newLedgerDbTasksTrigger replayed = LedgerDbTasksTrigger <$> newTVarIO st
 where
  st =
    LedgerDbTaskState
      { ldbtsImmTip = Origin
      , ldbtsPrevSnapshotTime = Nothing
      , ldbtsBlocksSinceLastSnapshot = replayed
      }

triggerLedgerDbTasks ::
  forall m.
  IOLike m =>
  LedgerDbTasksTrigger m ->
  -- | New tip of the ImmutableDB.
  WithOrigin SlotNo ->
  -- | Number of blocks written to the ImmutableDB.
  Word64 ->
  m ()
triggerLedgerDbTasks (LedgerDbTasksTrigger varSt) immTip numWritten =
  atomically $ modifyTVar varSt $ \st ->
    st
      { ldbtsImmTip = immTip
      , ldbtsBlocksSinceLastSnapshot = ldbtsBlocksSinceLastSnapshot st + numWritten
      }

-- | Run LedgerDB maintenance tasks when 'LedgerDbTasksTrigger' changes.
--
--  * Flushing of differences.
--  * Taking snapshots.
--  * Garbage collection.
ledgerDbTaskWatcher ::
  forall m blk.
  IOLike m =>
  ChainDbEnv m blk ->
  LedgerDbTasksTrigger m ->
  Watcher m LedgerDbTaskState (WithOrigin SlotNo)
ledgerDbTaskWatcher CDB{..} (LedgerDbTasksTrigger varSt) =
  Watcher
    { wFingerprint = ldbtsImmTip
    , wInitial = Nothing
    , wReader = readTVar varSt
    , wNotify =
        \LedgerDbTaskState
           { ldbtsImmTip
           , ldbtsBlocksSinceLastSnapshot = blocksSinceLast
           , ldbtsPrevSnapshotTime = prevSnapTime
           } ->
            whenJust (withOriginToMaybe ldbtsImmTip) $ \slotNo -> do
              LedgerDB.tryFlush cdbLedgerDB

              now <- getMonotonicTime
              LedgerDB.SnapCounters
                { prevSnapshotTime
                , ntBlocksSinceLastSnap
                } <-
                LedgerDB.tryTakeSnapshot
                  cdbLedgerDB
                  ((,now) <$> prevSnapTime)
                  blocksSinceLast
              when (ntBlocksSinceLastSnap == 0) $ traceMarkerIO "Took snapshot"
              atomically $ modifyTVar varSt $ \st ->
                st
                  { ldbtsBlocksSinceLastSnapshot =
                      ldbtsBlocksSinceLastSnapshot st - blocksSinceLast + ntBlocksSinceLastSnap
                  , ldbtsPrevSnapshotTime = prevSnapshotTime
                  }

              LedgerDB.garbageCollect cdbLedgerDB slotNo
    }

{-------------------------------------------------------------------------------
  Executing garbage collection
-------------------------------------------------------------------------------}

-- | Trigger a garbage collection for blocks older than the given 'SlotNo' on
-- the VolatileDB.
--
-- This is thread-safe as the VolatileDB locks itself while performing a GC.
--
-- When calling this function it is __critical__ that the blocks that will be
-- garbage collected, which are determined by the @slotNo@ parameter, have
-- already been copied to the immutable DB (if they are part of the current
-- selection).
--
-- TODO will a long GC be a bottleneck? It will block any other calls to
-- @putBlock@ and @getBlock@.
garbageCollectBlocks :: forall m blk. IOLike m => ChainDbEnv m blk -> SlotNo -> m ()
garbageCollectBlocks CDB{..} slotNo = do
  VolatileDB.garbageCollect cdbVolatileDB slotNo
  atomically $ do
    modifyTVar cdbInvalid $ fmap $ Map.filter ((>= slotNo) . invalidBlockSlotNo)
  PerasCertDB.garbageCollect cdbPerasCertDB slotNo
  traceWith cdbTracer $ TraceGCEvent $ PerformedGC slotNo

{-------------------------------------------------------------------------------
  Scheduling garbage collections
-------------------------------------------------------------------------------}

-- | Scheduled garbage collections
--
-- When a block has been copied to the ImmutableDB, we schedule a VolatileDB
-- garbage collection for the slot corresponding to the block in the future.
-- How far in the future is determined by the 'gcDelay' parameter. The goal is
-- to allow some overlap so that the write to the ImmutableDB will have been
-- flushed to disk before the block is removed from the VolatileDB.
--
-- We store scheduled garbage collections in a LIFO queue. Since the queue
-- will be very short (see further down for why) and entries are more often
-- added (at the block sync speed by a single thread) than removed (once every
-- 'gcInterval'), we simply use a 'StrictSeq' stored in a 'TVar' to make
-- reasoning and testing easier. Entries are enqueued at the end (right) and
-- dequeued from the head (left).
--
-- The 'Time's in the queue will be monotonically increasing. A fictional
-- example (with hh:mm:ss):
--
-- > [(16:01:12, SlotNo 1012), (16:04:38, SlotNo 1045), ..]
--
-- Scheduling a garbage collection with 'scheduleGC' will add an entry to the
-- end of the queue for the given slot at the time equal to now
-- ('getMonotonicTime') + the @gcDelay@ rounded to @gcInterval@. Unless the
-- last entry in the queue was scheduled for the same rounded time, in that
-- case the new entry replaces the existing entry. The goal of this is to
-- batch garbage collections so that, when possible, at most one garbage
-- collection happens every @gcInterval@.
--
-- For example, starting with an empty queue and @gcDelay = 5min@ and
-- @gcInterval = 10s@:
--
-- At 8:43:22, we schedule a GC for slot 10:
--
-- > [(8:48:30, SlotNo 10)]
--
-- The scheduled time is rounded up to the next interval. Next, at 8:43:24, we
-- schedule a GC for slot 11:
--
-- > [(8:48:30, SlotNo 11)]
--
-- Note that the existing entry is replaced with the new one, as they map to
-- the same @gcInterval@. Instead of two GCs 2 seconds apart, we will only
-- schedule one GC.
--
-- Next, at 8:44:02, we schedule a GC for slot 12:
--
-- > [(8:48:30, SlotNo 11), (8:49:10, SlotNo 12)]
--
-- Now, a new entry was appended to the queue, as it doesn't map to the same
-- @gcInterval@ as the last one.
--
-- In other words, everything scheduled in the first 10s will be done after
-- 20s. The bounds are the open-closed interval:
--
-- > (now + gcDelay, now + gcDelay + gcInterval]
--
-- Whether we're syncing at high speed or downloading blocks as they are
-- produced, the length of the queue will be at most @⌈gcDelay / gcInterval⌉ +
-- 1@, e.g., 5min / 10s = 31 entries. The @+ 1@ is needed because we might be
-- somewhere in the middle of a @gcInterval@.
--
-- The background thread will look at head of the queue and wait until that
-- has 'Time' passed. After the wait, it will pop off the head of the queue
-- and perform a garbage collection for the 'SlotNo' in the head. Note that
-- the 'SlotNo' before the wait can be different from the one after the wait,
-- precisely because of batching.
newtype GcSchedule m = GcSchedule (StrictTVar m (StrictSeq ScheduledGc))

data ScheduledGc = ScheduledGc
  { scheduledGcTime :: !Time
  -- ^ Time at which to run the garbage collection
  , scheduledGcSlot :: !SlotNo
  -- ^ For which slot to run the garbage collection
  }
  deriving (Eq, Show, Generic, NoThunks)

instance Condense ScheduledGc where
  condense (ScheduledGc time slot) = condense (time, slot)

data GcParams = GcParams
  { gcDelay :: !DiffTime
  -- ^ How long to wait until performing the GC. See 'cdbsGcDelay'.
  , gcInterval :: !DiffTime
  -- ^ The GC interval: the minimum time between two GCs. See
  -- 'cdbsGcInterval'.
  }
  deriving Show

newGcSchedule :: IOLike m => m (GcSchedule m)
newGcSchedule = GcSchedule <$> newTVarIO Seq.empty

scheduleGC ::
  forall m blk.
  IOLike m =>
  Tracer m (TraceGCEvent blk) ->
  -- | The slot to use for garbage collection
  SlotNo ->
  GcParams ->
  GcSchedule m ->
  m ()
scheduleGC tracer slotNo gcParams (GcSchedule varQueue) = do
  timeScheduledForGC <- computeTimeForGC gcParams <$> getMonotonicTime
  atomically $ modifyTVar varQueue $ \case
    queue' :|> ScheduledGc{scheduledGcTime = lastTimeScheduledForGC}
      | timeScheduledForGC == lastTimeScheduledForGC ->
          -- Same interval, batch it
          queue' :|> ScheduledGc timeScheduledForGC slotNo
    queue ->
      -- Different interval or empty, so append it
      queue :|> ScheduledGc timeScheduledForGC slotNo
  traceWith tracer $ ScheduledGC slotNo timeScheduledForGC

computeTimeForGC ::
  GcParams ->
  -- | Now
  Time ->
  -- | The time at which to perform the GC
  Time
computeTimeForGC GcParams{gcDelay, gcInterval} (Time now) =
  Time $
    picosecondsToDiffTime $
      -- We're rounding up to the nearest interval, because rounding down
      -- would mean GC'ing too early.
      roundUpToInterval
        (diffTimeToPicoseconds gcInterval)
        (diffTimeToPicoseconds (now + gcDelay))

-- | Round to an interval
--
-- PRECONDITION: interval > 0
--
-- >    [roundUpToInterval 5 n | n <- [1..15]]
-- > == [5,5,5,5,5, 10,10,10,10,10, 15,15,15,15,15]
--
-- >    roundUpToInterval 5 0
-- > == 0
roundUpToInterval :: (Integral a, Integral b) => b -> a -> a
roundUpToInterval interval x
  | m == 0 =
      d * fromIntegral interval
  | otherwise =
      (d + 1) * fromIntegral interval
 where
  (d, m) = x `divMod` fromIntegral interval

gcScheduleRunner ::
  forall m.
  IOLike m =>
  GcSchedule m ->
  -- | GC function
  (SlotNo -> m ()) ->
  m Void
gcScheduleRunner (GcSchedule varQueue) runGc = forever $ do
  -- Peek to know how long to wait
  timeScheduledForGC <-
    atomically $
      readTVar varQueue >>= \case
        Seq.Empty -> retry
        ScheduledGc{scheduledGcTime} :<| _ -> return scheduledGcTime

  currentTime <- getMonotonicTime
  let toWait = max 0 (timeScheduledForGC `diffTime` currentTime)
  threadDelay toWait

  -- After waiting, find the slot for which to GC and remove the entry from
  -- the queue.
  slotNo <-
    atomically $
      readTVar varQueue >>= \case
        ScheduledGc{scheduledGcSlot} :<| queue' -> do
          writeTVar varQueue queue'
          return scheduledGcSlot

        -- Impossible, we peeked at the queue and it contained an entry. We
        -- are the only one removing entries, so it can't have been removed
        -- while we were waiting.
        Seq.Empty -> error "queue empty after waiting"

  -- Garbage collection is called synchronously
  runGc slotNo

-- | Return the current contents of the 'GcSchedule' queue without modifying
-- it.
--
-- For testing purposes.
dumpGcSchedule :: IOLike m => GcSchedule m -> STM m [ScheduledGc]
dumpGcSchedule (GcSchedule varQueue) = toList <$> readTVar varQueue

{-------------------------------------------------------------------------------
  Adding blocks to the ChainDB
-------------------------------------------------------------------------------}

-- | Read blocks from 'cdbChainSelQueue' and add them synchronously to the
-- ChainDB.
addBlockRunner ::
  ( IOLike m
  , LedgerSupportsProtocol blk
  , BlockSupportsDiffusionPipelining blk
  , InspectLedger blk
  , HasHardForkHistory blk
  , HasCallStack
  ) =>
  Fuse m ->
  ChainDbEnv m blk ->
  m Void
addBlockRunner fuse cdb@CDB{..} = forever $ do
  let trace = traceWith cdbTracer . TraceAddBlockEvent
  trace PoppingFromQueue
  -- if the `chainSelSync` does not complete because it was killed by an async
  -- exception (or it errored), notify the blocked thread
  withFuse fuse $
    bracketOnError
      (lift $ getChainSelMessage starvationTracer cdbChainSelStarvation cdbChainSelQueue)
      ( \message -> lift $ atomically $ do
          case message of
            ChainSelReprocessLoEBlocks varProcessed ->
              void $ tryPutTMVar varProcessed ()
            ChainSelAddBlock BlockToAdd{varBlockWrittenToDisk, varBlockProcessed} -> do
              _ <-
                tryPutTMVar
                  varBlockWrittenToDisk
                  False
              _ <-
                tryPutTMVar
                  varBlockProcessed
                  (FailedToAddBlock "Failed to add block synchronously")
              pure ()
            ChainSelAddPerasCert _cert varProcessed ->
              void $ tryPutTMVar varProcessed ()
          closeChainSelQueue cdbChainSelQueue
      )
      ( \message -> do
          lift $ case message of
            ChainSelReprocessLoEBlocks _ ->
              trace PoppedReprocessLoEBlocksFromQueue
            ChainSelAddBlock BlockToAdd{blockToAdd} ->
              trace $ PoppedBlockFromQueue $ blockRealPoint blockToAdd
            ChainSelAddPerasCert cert _varProcessed ->
              traceWith cdbTracer $
                TraceAddPerasCertEvent $
                  PoppedPerasCertFromQueue (getPerasCertRound cert) (getPerasCertBoostedBlock cert)
          chainSelSync cdb message
          lift $ atomically $ processedChainSelMessage cdbChainSelQueue message
      )
 where
  starvationTracer = Tracer $ traceWith cdbTracer . TraceChainSelStarvationEvent
