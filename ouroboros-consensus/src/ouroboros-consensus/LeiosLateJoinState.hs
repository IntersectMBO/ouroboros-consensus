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
-- 'pendingTriggers' queue), the 'gcPruner' that bounds the cache, the derived
-- ignore-set 'getBlockedCertRBs' that ChainSel reads, and the AddBlock listener
-- 'leiosAddBlockListener' that writes the cache and 'announcementsMap'. The
-- listener is not registered with the ChainDB yet (a later commit wires it), so
-- it does not run and behaviour is unchanged. 'pendingTriggers' still has no
-- writer.
module LeiosLateJoinState (module LeiosLateJoinState) where

import Control.Monad (forM_, when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import LeiosDemoTypes (EbHash, IsCertRB (..), LeiosPoint, pointEbHash)
import Ouroboros.Consensus.Block
  ( ChainHash (..)
  , GetPrevHash (..)
  , Header
  , HeaderHash
  , RealPoint
  , SlotNo
  , WithOrigin (..)
  , blockSlot
  , headerHash
  )
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB (..))
import Ouroboros.Consensus.Storage.LedgerDB.Forker (ResolveLeiosBlock (..))
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

-- | The AddBlock listener body: update the late-join bookkeeping for one block.
--
-- ChainDB runs this synchronously, in STM scope, between writing the block to
-- the VolatileDB and running chain selection on it (registered via
-- 'registerHeaderListener'). Sharing the transaction that adds the block closes
-- the window an asynchronous listener would leave: there is no moment when the
-- block is in the VolatileDB but absent from this state, so a closure arriving
-- "in between" cannot strand the block.
--
-- For every block it records the slot and announced EB point in
-- 'headerAnnouncementsCache'. For a CertRB whose certified EB closure is not yet
-- local, it adds the block to 'announcementsMap' so 'getBlockedCertRBs' holds it
-- back. The certified EB hash comes from the parent's cached announcement, not
-- from the block itself: a CertRB certifies the EB its parent announced
-- (CIP-0164).
leiosAddBlockListener ::
  (IOLike m, ResolveLeiosBlock blk, GetPrevHash blk) =>
  LeiosLateJoinState m blk ->
  Header blk ->
  STM m ()
leiosAddBlockListener st hdr = do
  modifyTVar (headerAnnouncementsCache st) $
    Map.insert (headerHash hdr) (blockSlot hdr, headerLeiosAnnouncement hdr)
  case headerIsCertRB hdr of
    NotCertRB -> pure ()
    CertRB -> do
      cache <- readTVar (headerAnnouncementsCache st)
      forM_ (parentAnnouncement hdr cache) $
        blockCertRbIfClosureMissing st (headerHash hdr)

-- | The EB hash announced by this header's parent, read from a cache snapshot.
-- 'Nothing' when the parent is not cached yet, or it announced no EB.
parentAnnouncement ::
  GetPrevHash blk =>
  Header blk ->
  Map (HeaderHash blk) (SlotNo, Maybe LeiosPoint) ->
  Maybe EbHash
parentAnnouncement hdr cache = case headerPrevHash hdr of
  GenesisHash -> Nothing
  BlockHash parentHash -> pointEbHash <$> (snd =<< Map.lookup parentHash cache)

-- | Hold a CertRB back when the EB closure it certifies is not local yet.
--
-- Inserts the CertRB into 'announcementsMap'. 'getBlockedCertRBs' subtracts the
-- completed closures again, so a closure that arrives after this check is still
-- handled there; the only cost of inserting when the closure is in fact already
-- present is one redundant entry, dropped on the next 'getBlockedCertRBs' read.
blockCertRbIfClosureMissing ::
  (IOLike m, Ord (HeaderHash blk)) =>
  LeiosLateJoinState m blk ->
  -- | The CertRB's header hash.
  HeaderHash blk ->
  -- | The EB hash it certifies.
  EbHash ->
  STM m ()
blockCertRbIfClosureMissing st rbHash ebHash = do
  completed <- readCompletedClosuresSTM st
  when (ebHash `Set.notMember` completed) $
    modifyTVar (announcementsMap st) (Map.insert rbHash ebHash)
