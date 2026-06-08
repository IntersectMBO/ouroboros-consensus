{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | LeiosKernel state and background work for the late-join feature.
--
-- A late-joining node receives a CertRB before it holds the certified EB's
-- transaction closure. ChainSel must hold such a block back until the closure
-- arrives. The bookkeeping that decides which blocks to hold back lives here,
-- in the LeiosKernel area, not in the ChainDB (which stays Leios-free).
--
-- This module provides the state record 'LeiosLateJoinState' (the
-- 'headerAnnouncementsCache', the pending-CertRB 'announcementsMap', the
-- 'certRbsByAnnouncer' reverse index, and the 'pendingTriggers' queue), the
-- 'gcPruner' that bounds the cache, the derived ignore-set 'getBlockedCertRBs'
-- that ChainSel reads, the AddBlock listener 'leiosAddBlockListener' that writes
-- the cache and 'announcementsMap', the 'runTriggerWorker' loop that drains
-- 'pendingTriggers' into ChainDB reselect requests, and the
-- 'runAcquiredEbTxsSubscriber' loop that fills 'pendingTriggers' when an EB
-- closure arrives. None of the listener, the worker, or the subscriber is wired
-- to the ChainDB or LeiosDb yet (a later commit registers the listener and forks
-- the two loops), so they do not run and behaviour is unchanged.
module LeiosLateJoinState (module LeiosLateJoinState) where

import Control.Concurrent.Class.MonadSTM.Strict (readTChan)
import Control.Monad (forM_, forever, when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import LeiosDemoDb.Common
  ( LeiosDbHandle
  , LeiosEbNotification (..)
  , subscribeEbNotifications
  )
import LeiosDemoTypes (EbHash, IsCertRB (..), LeiosPoint, pointEbHash)
import Ouroboros.Consensus.Block
  ( ChainHash (..)
  , GetPrevHash (..)
  , HasHeader
  , Header
  , HeaderHash
  , RealPoint (..)
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
  , certRbsByAnnouncer ::
      !(StrictTVar m (Map (HeaderHash blk) (Set (HeaderHash blk))))
  -- ^ Reverse index for the child-before-parent race: announcer (parent)
  -- header hash -> the CertRB children that certify the EB it announced, kept
  -- only for CertRBs whose announcer had not been added when they arrived. Two
  -- ChainSync clients can interleave AddBlock, so a CertRB can enter chain
  -- selection before its parent; its own listener pass then has no cached
  -- announcement to read the EB hash from. 'leiosAddBlockListener' records such
  -- an orphan here and 'sweepOrphanedCertRbs' picks it up when the announcer is
  -- added, dropping the entry then. The map therefore holds only CertRBs whose
  -- announcer has not arrived yet; 'gcPruner' does not touch it.
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
  orphanedCertRbs <- newTVarIO Map.empty
  triggers <- newTQueueIO
  pure
    LeiosLateJoinState
      { headerAnnouncementsCache = cache
      , announcementsMap = annMap
      , pendingTriggers = triggers
      , readCompletedClosuresSTM = readCompleted
      , certRbsByAnnouncer = orphanedCertRbs
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

-- | Drain 'pendingTriggers' and ask ChainDB to re-run chain selection on each
-- block.
--
-- One blocking read per iteration: wait for the next 'RealPoint', then call
-- 'reconsiderBlockAsync' to enqueue a reselection for it. 'reconsiderBlockAsync'
-- writes to ChainDB's bounded chain-selection queue and may block when that
-- queue is full. The worker is allowed to block there: the only producer of
-- 'pendingTriggers' is the asynchronous AcquiredEbTxs subscriber, never the
-- synchronous AddBlock listener, so this backpressure never reaches the
-- transaction that adds a block. During a late-join burst many CertRBs unblock
-- as their closures arrive, all onto one queue with one consumer running chain
-- selection per message, so blocking here is expected, not a fault.
--
-- The constraint is 'IOLike m' alone: the worker only enqueues a reselection
-- request, it does not run chain selection, so it needs none of the ledger
-- constraints the chain-selection thread carries.
--
-- Reconsidering a block the VolatileDB has since garbage-collected is a no-op:
-- ChainDB finds no block for the point and ignores the request, so the worker
-- needs no separate guard for that.
--
-- Loops forever; intended to run as a linked background thread. Nothing forks it
-- yet, so behaviour is unchanged.
runTriggerWorker ::
  IOLike m =>
  ChainDB m blk ->
  LeiosLateJoinState m blk ->
  m Void
runTriggerWorker chainDB st = forever $ do
  rp <- atomically $ readTQueue (pendingTriggers st)
  reconsiderBlockAsync chainDB rp

-- | Drain the LeiosDb EB-notification channel and unblock the CertRBs each
-- arriving closure releases.
--
-- 'subscribeEbNotifications' returns a channel scoped to this call, so a
-- notification emitted after subscribing is delivered exactly once. Each
-- iteration reads one notification and acts on it in the same transaction: for
-- an 'AcquiredEbTxs' (the EB closure is now complete locally) it calls
-- 'unblockCertRbsForClosure'; an 'AcquiredEb' is the body alone, the closure is
-- not yet complete, so it is ignored. Reading the notification and updating the
-- bookkeeping in one transaction is invariant I1: no reader ever sees a CertRB
-- that is neither in 'announcementsMap' nor queued.
--
-- The constraint is 'IOLike m' plus 'Ord (HeaderHash blk)' for the
-- 'announcementsMap' and cache lookups in 'unblockCertRbsForClosure'; the loop
-- only enqueues reselections, so it needs none of the ledger constraints the
-- chain-selection thread carries.
--
-- This is the sole writer of 'pendingTriggers'; the AddBlock listener never
-- writes it. Loops forever; intended to run as a linked background thread.
-- Nothing forks it yet, so behaviour is unchanged.
runAcquiredEbTxsSubscriber ::
  (IOLike m, Ord (HeaderHash blk)) =>
  LeiosDbHandle m ->
  LeiosLateJoinState m blk ->
  m Void
runAcquiredEbTxsSubscriber leiosDb st = do
  notifications <- subscribeEbNotifications leiosDb
  forever $
    atomically $
      readTChan notifications >>= \case
        AcquiredEbTxs point -> unblockCertRbsForClosure st (pointEbHash point)
        AcquiredEb{} -> pure ()

-- | Remove from 'announcementsMap' every CertRB waiting on this EB and enqueue
-- one reselection per removed CertRB.
--
-- The closure for @ebHash@ has just become local. 'getBlockedCertRBs' already
-- stops holding these CertRBs back (it subtracts the completed closures), so the
-- removal here only keeps the map bounded; the enqueue is what drives the
-- reselection. Both happen in one transaction with the channel read, so each
-- CertRB is enqueued exactly once: a re-arriving notification for the same EB
-- finds the entries already gone (invariant I2).
--
-- A removed CertRB whose cache entry the 'gcPruner' has already dropped is
-- skipped: the VolatileDB no longer holds that block, so reconsidering it would
-- be a no-op anyway (invariant I4). 'certRbsByAnnouncer' is not touched: it holds
-- only orphaned CertRBs, disjoint from 'announcementsMap'.
unblockCertRbsForClosure ::
  (IOLike m, Ord (HeaderHash blk)) =>
  LeiosLateJoinState m blk ->
  EbHash ->
  STM m ()
unblockCertRbsForClosure st ebHash = do
  (matched, kept) <- Map.partition (== ebHash) <$> readTVar (announcementsMap st)
  writeTVar (announcementsMap st) kept
  cache <- readTVar (headerAnnouncementsCache st)
  forM_ (Map.keys matched) $ \rbHash ->
    forM_ (Map.lookup rbHash cache) $ \(slot, _ann) ->
      writeTQueue (pendingTriggers st) (RealPoint slot rbHash)

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
      recordIfOrphan st hdr cache
  -- This block may itself be the announcer of CertRB children that arrived
  -- before it. Hold each back if the announced EB's closure is still missing.
  sweepOrphanedCertRbs st hdr

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

-- | Record a CertRB under its announcer when the announcer is not cached yet.
--
-- Handles the child-before-parent race: when two ChainSync clients interleave
-- their AddBlock writes, a CertRB can be added before the RB that announced its
-- EB. Its own listener pass then finds no cached announcement and cannot read
-- the EB hash to hold it back. Recording it under the announcer's hash lets
-- 'sweepOrphanedCertRbs' pick it up when the announcer is added. When the
-- announcer is already cached, the direct path in 'leiosAddBlockListener'
-- handles the CertRB and this records nothing.
recordIfOrphan ::
  (IOLike m, GetPrevHash blk) =>
  LeiosLateJoinState m blk ->
  Header blk ->
  Map (HeaderHash blk) (SlotNo, Maybe LeiosPoint) ->
  STM m ()
recordIfOrphan st hdr cache = case headerPrevHash hdr of
  BlockHash parentHash
    | not (Map.member parentHash cache) ->
        modifyTVar (certRbsByAnnouncer st) $
          Map.insertWith Set.union parentHash (Set.singleton (headerHash hdr))
  _ -> pure ()

-- | Hold back CertRB children that were added before this header, their
-- announcer.
--
-- Reads the orphans 'recordIfOrphan' filed under this header's hash, drops the
-- entry (the announcer is now added, so any later child of it takes the direct
-- path), and holds each orphan back if the EB this header announced is still
-- missing its closure. Does nothing when this header announced no EB: then it
-- certifies nothing for its children to wait on.
sweepOrphanedCertRbs ::
  (IOLike m, ResolveLeiosBlock blk, HasHeader (Header blk)) =>
  LeiosLateJoinState m blk ->
  Header blk ->
  STM m ()
sweepOrphanedCertRbs st hdr =
  forM_ (headerLeiosAnnouncement hdr) $ \leiosPoint -> do
    orphans <-
      Map.findWithDefault Set.empty (headerHash hdr)
        <$> readTVar (certRbsByAnnouncer st)
    modifyTVar (certRbsByAnnouncer st) (Map.delete (headerHash hdr))
    forM_ orphans $ \childHash ->
      blockCertRbIfClosureMissing st childHash (pointEbHash leiosPoint)
