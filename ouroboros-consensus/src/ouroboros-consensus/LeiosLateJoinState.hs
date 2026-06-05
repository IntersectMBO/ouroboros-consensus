{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | LeiosKernel state and background work for the late-join feature.
--
-- A late-joining node receives a CertRB before it holds the certified EB's
-- transaction closure. ChainSel must hold such a block back until the closure
-- arrives. The bookkeeping that decides which blocks to hold back lives here,
-- in the LeiosKernel area, not in the ChainDB (which stays Leios-free).
--
-- This module currently provides 'headerAnnouncementsCache' and the
-- 'gcPruner' that bounds it. The writers that populate the cache, the
-- pending-CertRB map, and the derived ignore-set are added by later commits.
module LeiosLateJoinState (module LeiosLateJoinState) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import LeiosDemoTypes (LeiosPoint)
import Ouroboros.Consensus.Block (HeaderHash, SlotNo, WithOrigin (..))
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
  }

-- | Create a 'LeiosLateJoinState' with an empty cache.
newLeiosLateJoinState ::
  (IOLike m, NoThunks (HeaderHash blk)) => m (LeiosLateJoinState m blk)
newLeiosLateJoinState = LeiosLateJoinState <$> newTVarIO Map.empty

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
