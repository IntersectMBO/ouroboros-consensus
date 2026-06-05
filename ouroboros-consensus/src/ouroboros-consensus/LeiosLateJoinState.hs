{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | LeiosKernel state and background work for the late-join feature.
--
-- A late-joining node receives a CertRB before it holds the certified EB's
-- transaction closure. ChainSel must hold such a block back until the closure
-- arrives. The bookkeeping that decides which blocks to hold back lives here,
-- in the LeiosKernel area, not in the ChainDB (which stays Leios-free).
--
-- This module provides the state record 'LeiosLateJoinState' (the
-- 'headerAnnouncementsCache', the pending-CertRB 'announcementsMap', and the
-- 'pendingTriggers' queue), the 'gcPruner' that bounds the cache, and the
-- derived ignore-set 'getBlockedCertRBs' that ChainSel reads. The writers that
-- populate the maps and the queue are added by later commits; for now the state
-- is allocated empty and nothing writes it.
module LeiosLateJoinState (module LeiosLateJoinState) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import LeiosDemoTypes (EbHash, LeiosPoint)
import Ouroboros.Consensus.Block (HeaderHash, RealPoint, SlotNo, WithOrigin (..))
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB (..))
import Ouroboros.Consensus.Util.IOLike

-- | LeiosKernel state for the late-join feature.
data LeiosLateJoinState m blk = LeiosLateJoinState
  { headerAnnouncementsCache ::
      !(StrictTVar m (Map (HeaderHash blk) (SlotNo, Maybe LeiosPoint)))
  -- ^ One entry per VolatileDB block: the block's own slot and the EB point
  -- its header announces (if any). The slot lets 'gcPruner' drop entries for
  -- blocks the VolatileDB has already garbage-collected. The announcement
  -- lets the AddBlock listener find a CertRB child's certified EB hash from
  -- its parent's entry.
  , announcementsMap :: !(StrictTVar m (Map (HeaderHash blk) EbHash))
  -- ^ The CertRBs whose certified EB closure was missing when the block
  -- arrived. Key is the certifying RB's header hash; value is the EB hash it
  -- certifies. 'getBlockedCertRBs' turns this into the ignore-set ChainSel
  -- reads. Swept on read, not pruned: a stale entry for a garbage-collected
  -- RB is harmless because reconsidering that RB is a no-op.
  , pendingTriggers :: !(TQueue m (RealPoint blk))
  -- ^ "Re-run chain selection on this block" requests, drained by the trigger
  -- worker. Unbounded on purpose: its only producer is the asynchronous
  -- AcquiredEbTxs subscriber, never the synchronous AddBlock listener, so a
  -- bounded queue could only backpressure the subscriber, and the depth is
  -- already bounded by the size of 'announcementsMap'.
  , readCompletedClosuresSTM :: !(STM m (Set EbHash))
  -- ^ The LeiosDb read for the set of EBs whose closure is local, captured at
  -- construction so 'getBlockedCertRBs' can subtract it from 'announcementsMap'
  -- in one transaction.
  }

-- | Create a 'LeiosLateJoinState' with empty maps and an empty trigger queue.
--
-- Takes the LeiosDb completed-closures read so 'getBlockedCertRBs' can subtract
-- it in one transaction. No writers run here; later commits add the AddBlock
-- listener, the AcquiredEbTxs subscriber, and the startup walker that fill the
-- state.
newLeiosLateJoinState ::
  (IOLike m, NoThunks (HeaderHash blk)) =>
  -- | 'readCompletedClosuresSTM' from the 'LeiosDbHandle'.
  STM m (Set EbHash) ->
  m (LeiosLateJoinState m blk)
newLeiosLateJoinState readCompleted = do
  cache <- newTVarIO Map.empty
  annMap <- newTVarIO Map.empty
  triggers <- newTQueueIO
  pure
    LeiosLateJoinState
      { headerAnnouncementsCache = cache
      , announcementsMap = annMap
      , pendingTriggers = triggers
      , readCompletedClosuresSTM = readCompleted
      }

-- | The CertRBs ChainSel must hold back: 'announcementsMap' keys whose
-- certified EB closure is still missing.
--
-- Reads 'announcementsMap' and the completed-closures set in one transaction,
-- so ChainSel never holds back a CertRB whose closure has in fact already
-- arrived. The subtraction is also the sweep that keeps 'announcementsMap'
-- bounded: an entry whose closure has arrived stops appearing in the result
-- even before the subscriber removes it.
getBlockedCertRBs ::
  IOLike m =>
  LeiosLateJoinState m blk ->
  STM m (Set (HeaderHash blk))
getBlockedCertRBs st = do
  annMap <- readTVar (announcementsMap st)
  completed <- readCompletedClosuresSTM st
  pure $ Map.keysSet (Map.filter (`Set.notMember` completed) annMap)

-- | Bound 'headerAnnouncementsCache' to the VolatileDB's volatile window.
--
-- Blocks on 'getLastGcSlot' and wakes on each new GC slot. On each wake-up it
-- drops every cache entry the VolatileDB GC has already dropped: strictly
-- @blockSlot < gcSlot@, mirroring the VolatileDB's @<@ (not @<=@) rule. With
-- @<@, every block the VolatileDB still holds keeps its cache entry, so a
-- parent lookup never misses because of pruning.
--
-- Loops forever; intended to run as a linked background thread.
gcPruner ::
  forall m blk.
  IOLike m =>
  ChainDB m blk ->
  LeiosLateJoinState m blk ->
  m ()
gcPruner chainDB st = go Origin
 where
  go :: WithOrigin SlotNo -> m ()
  go prev = do
    gcSlot <- atomically $ do
      slot <- getLastGcSlot chainDB
      check (slot > prev)
      pure slot
    atomically $
      modifyTVar (headerAnnouncementsCache st) (dropCollected gcSlot)
    go gcSlot

  -- Keep entries whose block the VolatileDB still holds (slot >= gcSlot).
  dropCollected :: WithOrigin SlotNo -> Map k (SlotNo, a) -> Map k (SlotNo, a)
  dropCollected gcSlot = Map.filter (\(slot, _) -> NotOrigin slot >= gcSlot)
