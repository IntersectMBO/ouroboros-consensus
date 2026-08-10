{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- | The reference implementation of the LeiosTxCache: a bounded, in-memory index
-- over the recently-announced Leios EBs, their bodies, and the txs those bodies
-- reference, with reference-counted incremental eviction. Simple and obviously
-- correct; "LeiosTxCache.Optimized" is validated for observational equivalence
-- to it.
--
-- This is deliberately /independent/ of the on-disk LeiosDb: the two are
-- separate caches with different eviction policies. This index retains only
-- what is referenced by the @'maxAnnouncementCount'@ freshest EB announcements
-- it has processed, so it stays small enough to keep resident in memory (a
-- lookup is disk-latency-free); the LeiosDb retains far more (everything
-- referenced by the acquired EBs younger than the immutable tip). Because they
-- are independent stores, sometimes the node will re-fetch and re-validate a tx
-- that has been evicted from the LeiosTxCache even while the LeiosDb still holds
-- it; that is acceptable.
--
-- The type parameters are payloads the index does not interpret:
--
--   * @a@ is what we record for a tx that has been inserted (fetched) but not
--     yet applied; @()@ in a proper node, or the tx's bytes in a
--     test\/prototype.
--
--   * @v@ is what we record for a tx that has been applied. Kept polymorphic so
--     the index is not coupled to any ledger type.
--
--   * @b@ (per body) cannot be trivial: it must at least carry the body's
--     'TxHash'es, via 'ReferencesTxsByHash', so eviction can decrement their
--     refcounts without touching storage. It need not carry the whole body — a
--     minimal @b@ of just the hashes suffices; storing more (e.g. the serialized
--     body, which could ancillarily answer a MsgLeiosBodyRequest on a hit) is an
--     implementation choice.
module LeiosTxCache.Reference
  ( -- * Index
    LeiosTxCacheIndex (..)
  , emptyLeiosTxCacheIndex

    -- * Operations
  , insertAnnouncement
  , evictOlderThan
  , insertBody
  , insertUnappliedTx
  , insertAppliedTx
  , lookupTx
  , lookupBody

    -- * Internal state (exposed for testing)
  , TxState (..)

    -- * Shared types (re-exported from "LeiosTxCache.API")
  , ReferencesTxsByHash (..)
  , RefCount (..)
  , BodyState (..)
  , maxAnnouncementCount
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import LeiosDemoTypes (EbHash, RbHash, TxHash)
import LeiosTxCache.API
  ( BodyState (..)
  , InsertBodySummary
  , RefCount (..)
  , ReferencesTxsByHash (..)
  , maxAnnouncementCount
  , mkInsertBodySummary
  )
import qualified Lens.Micro as L
import qualified Lens.Micro.Extras as L

data TxState a v
  = -- | An inserted body refers to this tx, but the tx itself is not inserted.
    TxNotYetInserted {-# UNPACK #-} !RefCount
  | -- | The tx is inserted (fetched) but not yet applied, neither by the
    -- Mempool nor the LeiosVoting thread.
    TxAlreadyInserted {-# UNPACK #-} !RefCount !a
  | -- | The tx has been applied.
    TxAlreadyValidated {-# UNPACK #-} !RefCount !v

-- | The index. The 'txState' entries are the payload; the rest is maintained for
-- incremental eviction.
data LeiosTxCacheIndex a v b = MkLeiosTxCacheIndex
  { announcementState :: !(Map SlotNo (NEMap RbHash EbHash))
  -- ^ The retained EB announcements, keyed by slot then announcing RB header.
  , announcementCount :: !Int
  -- ^ INVARIANT: @= sum (fmap NEMap.size announcementState)@.
  --
  -- INVARIANT: @0 <= announcementCount <= maxAnnouncementCount@.
  , bodyState :: !(Map EbHash (BodyState b))
  -- ^ INVARIANT: each 'RefCount' equals the number of announcements in
  -- 'announcementState' whose 'EbHash' is this one.
  , txState :: !(Map TxHash (TxState a v))
  -- ^ INVARIANT: each 'RefCount' equals the number of 'BodyAlreadyInserted's in
  -- 'bodyState' that reference this tx.
  , prunedSlot :: !SlotNo
  -- ^ The greatest slot 'evictOlderThan' has pruned to (monotonically
  -- non-decreasing; 'SlotNo' @0@ until the first prune)
  }

emptyLeiosTxCacheIndex :: LeiosTxCacheIndex a v b
emptyLeiosTxCacheIndex =
  MkLeiosTxCacheIndex
    { announcementState = Map.empty
    , announcementCount = 0
    , bodyState = Map.empty
    , txState = Map.empty
    , prunedSlot = SlotNo 0
    }

{-------------------------------------------------------------------------------
  Refcount lenses

  Record syntax handles the 'LeiosTxCacheIndex' fields; these are only for the
  refcount that is common to every constructor of the 'BodyState' \/ 'TxState'
  sums, where record syntax does not suffice.
-------------------------------------------------------------------------------}

-- | The refcount of a body, regardless of whether the body itself is inserted.
bodyRefCountL :: L.Lens' (BodyState b) RefCount
bodyRefCountL = L.lens getIt setIt
 where
  getIt = \case
    BodyNotYetInserted rc -> rc
    BodyAlreadyInserted rc _ -> rc
  setIt s rc = case s of
    BodyNotYetInserted _ -> BodyNotYetInserted rc
    BodyAlreadyInserted _ b -> BodyAlreadyInserted rc b

-- | The refcount of a tx, regardless of its insertion\/application state.
txRefCountL :: L.Lens' (TxState a v) RefCount
txRefCountL = L.lens getIt setIt
 where
  getIt = \case
    TxNotYetInserted rc -> rc
    TxAlreadyInserted rc _ -> rc
    TxAlreadyValidated rc _ -> rc
  setIt s rc = case s of
    TxNotYetInserted _ -> TxNotYetInserted rc
    TxAlreadyInserted _ a -> TxAlreadyInserted rc a
    TxAlreadyValidated _ v -> TxAlreadyValidated rc v

{-------------------------------------------------------------------------------
  RefCount helpers
-------------------------------------------------------------------------------}

incRefCount :: RefCount -> RefCount
incRefCount (MkRefCount n) = MkRefCount (n + 1)

-- | Decrement, or 'SNothing' if it would reach zero (i.e. the entry is now
-- unreferenced and should be evicted).
decRefCount :: RefCount -> StrictMaybe RefCount
decRefCount (MkRefCount n)
  | n <= 1 = SNothing
  | otherwise = SJust $ MkRefCount $ n - 1

{-------------------------------------------------------------------------------
  Operations
-------------------------------------------------------------------------------}

-- | Insert an EB announcement (identified by its slot and announcing RB header),
-- bumping the announced EB's body refcount. Re-inserting the same announcement
-- is a no-op, as is inserting an EB whose slot is strictly older than
-- 'prunedSlot' (one the cache has already been pruned past; see 'evictOlderThan').
--
-- If this pushes 'announcementCount' past 'maxAnnouncementCount', the oldest
-- announcement (least slot, then least RB header) is evicted; that cascades
-- through the body and, if the body was inserted, its txs. Returns the bodies
-- and txs that were evicted.
insertAnnouncement ::
  ReferencesTxsByHash b =>
  SlotNo ->
  RbHash ->
  EbHash ->
  LeiosTxCacheIndex a v b ->
  (LeiosTxCacheIndex a v b, Set EbHash, Set TxHash)
insertAnnouncement slot rbh ebh idx
  | slot < prunedSlot idx = (idx, Set.empty, Set.empty)
  | alreadyPresent = (idx, Set.empty, Set.empty)
  | otherwise = evictIfNeeded inserted
 where
  alreadyPresent = case Map.lookup slot (announcementState idx) of
    Nothing -> False
    Just nem -> NEMap.member rbh nem

  inserted =
    MkLeiosTxCacheIndex
      { announcementState =
          Map.alter
            (Just . maybe (NEMap.singleton rbh ebh) (NEMap.insert rbh ebh))
            slot
            (announcementState idx)
      , announcementCount = announcementCount idx + 1
      , bodyState =
          Map.alter
            (Just . maybe (BodyNotYetInserted (MkRefCount 1)) (L.over bodyRefCountL incRefCount))
            ebh
            (bodyState idx)
      , txState = txState idx
      , prunedSlot = prunedSlot idx
      }

-- | Repeatedly 'evictOldest' while @shouldEvict@ holds of the index: the shared
-- core of the two eviction entrypoints ('evictIfNeeded' and 'evictOlderThan').
-- Strict accumulators avoid building up '<>' thunks.
evictWhile ::
  ReferencesTxsByHash b =>
  (LeiosTxCacheIndex a v b -> Bool) ->
  LeiosTxCacheIndex a v b ->
  (LeiosTxCacheIndex a v b, Set EbHash, Set TxHash)
evictWhile shouldEvict = go Set.empty Set.empty
 where
  go !evEbs !evTxs !idx
    | shouldEvict idx =
        let (idx', evEbs', evTxs') = evictOldest idx
         in go (evEbs <> evEbs') (evTxs <> evTxs') idx'
    | otherwise = (idx, evEbs, evTxs)

-- | Evict oldest announcements until within 'maxAnnouncementCount'. In practice
-- a single 'insertAnnouncement' overshoots by at most one, but the loop is robust
-- regardless.
evictIfNeeded ::
  ReferencesTxsByHash b =>
  LeiosTxCacheIndex a v b ->
  (LeiosTxCacheIndex a v b, Set EbHash, Set TxHash)
evictIfNeeded = evictWhile ((> maxAnnouncementCount) . announcementCount)

-- | Evict every retained announcement whose slot is strictly older than the
-- boundary, cascading through the bodies and txs like 'insertAnnouncement'.
-- Returns the evicted bodies and txs. Advances 'prunedSlot' to the boundary
-- (monotonically), after which 'insertAnnouncement' refuses any EB that old.
evictOlderThan ::
  ReferencesTxsByHash b =>
  SlotNo ->
  LeiosTxCacheIndex a v b ->
  (LeiosTxCacheIndex a v b, Set EbHash, Set TxHash)
evictOlderThan boundary idx =
  evictWhile oldestIsStale idx'
 where
  idx' = idx{prunedSlot = max (prunedSlot idx) boundary}
  oldestIsStale i = case Map.lookupMin (announcementState i) of
    Just (slotMin, _) -> slotMin < prunedSlot idx'
    Nothing -> False

evictOldest ::
  ReferencesTxsByHash b =>
  LeiosTxCacheIndex a v b ->
  (LeiosTxCacheIndex a v b, Set EbHash, Set TxHash)
evictOldest idx =
  ( MkLeiosTxCacheIndex
      { announcementState = announcementState'
      , announcementCount = announcementCount idx - 1
      , bodyState = bodyState'
      , txState = txState'
      , prunedSlot = prunedSlot idx
      }
  , evEbs
  , evTxs
  )
 where
  (slotMin, nem) = Map.findMin (announcementState idx)
  (rbhMin, ebhEvicted) = NEMap.findMin nem

  announcementState' = case NEMap.nonEmptyMap (NEMap.delete rbhMin nem) of
    Nothing -> Map.delete slotMin (announcementState idx)
    Just nem' -> Map.insert slotMin nem' (announcementState idx)

  (bodyState', txState', evEbs, evTxs) =
    decBody ebhEvicted (bodyState idx) (txState idx)

-- | Decrement a body's refcount; if it reaches zero, remove it and (if it had
-- been inserted) decrement each of its referenced txs.
decBody ::
  ReferencesTxsByHash b =>
  EbHash ->
  Map EbHash (BodyState b) ->
  Map TxHash (TxState a v) ->
  (Map EbHash (BodyState b), Map TxHash (TxState a v), Set EbHash, Set TxHash)
decBody ebh bs ts = case Map.lookup ebh bs of
  Nothing -> (bs, ts, Set.empty, Set.empty)
  Just b -> case decRefCount (L.view bodyRefCountL b) of
    SJust rc -> (Map.insert ebh (L.set bodyRefCountL rc b) bs, ts, Set.empty, Set.empty)
    SNothing ->
      let (ts', evTxs) = case b of
            BodyNotYetInserted _ -> (ts, Set.empty)
            BodyAlreadyInserted _ body -> foldTxReferences decTx (ts, Set.empty) body
       in (Map.delete ebh bs, ts', Set.singleton ebh, evTxs)

decTx ::
  (Map TxHash (TxState a v), Set TxHash) ->
  TxHash ->
  (Map TxHash (TxState a v), Set TxHash)
decTx (ts, evTxs) txh =
  let (ev, ts') = Map.alterF upd txh ts
   in (ts', evTxs <> ev)
 where
  upd Nothing = (Set.empty, Nothing)
  upd (Just tx) = case decRefCount (L.view txRefCountL tx) of
    SJust rc -> (Set.empty, Just (L.set txRefCountL rc tx))
    SNothing -> (Set.singleton txh, Nothing)

-- | Record that we now hold the body of this EB, bumping the refcount of each tx
-- it references. Idempotent, and a no-op if no announcement references this EB
-- (its refcount would be zero).
insertBody ::
  ReferencesTxsByHash b =>
  EbHash ->
  b ->
  LeiosTxCacheIndex a v b ->
  (LeiosTxCacheIndex a v b, Maybe InsertBodySummary)
insertBody ebh body idx = case Map.lookup ebh (bodyState idx) of
  Nothing -> (idx, Nothing)
  Just BodyAlreadyInserted{} -> (idx, Nothing)
  Just (BodyNotYetInserted rc) ->
    let ((n, tracked, acquired, validated), txState') =
          foldTxReferences bumpTx ((0, 0, 0, 0), txState idx) body
        idx' =
          MkLeiosTxCacheIndex
            { announcementState = announcementState idx
            , announcementCount = announcementCount idx
            , bodyState = Map.insert ebh (BodyAlreadyInserted rc body) (bodyState idx)
            , txState = txState'
            , prunedSlot = prunedSlot idx
            }
     in (idx', Just (mkInsertBodySummary n tracked acquired validated (Map.size txState')))
 where
  -- Bump each tx's refcount and, in the same pass, classify its /prior/ state so
  -- the summary needs no second traversal.
  bumpTx ((!nn, !tt, !aa, !vv), ts) txh =
    let (dt, da, dv) = case Map.lookup txh ts of
          Nothing -> (0, 0, 0) -- new: not yet tracked
          Just (TxNotYetInserted _) -> (1, 0, 0) -- tracked, not acquired
          Just (TxAlreadyInserted _ _) -> (1, 1, 0) -- acquired, not validated
          Just (TxAlreadyValidated _ _) -> (1, 1, 1) -- acquired and validated
        ts' =
          Map.alter
            (Just . maybe (TxNotYetInserted (MkRefCount 1)) (L.over txRefCountL incRefCount))
            txh
            ts
     in ((nn + 1, tt + dt, aa + da, vv + dv), ts')

-- | Record the payload of a fetched-but-not-yet-applied tx, without changing its
-- refcount. A no-op if no inserted body references this tx.
insertUnappliedTx :: TxHash -> a -> LeiosTxCacheIndex a v b -> LeiosTxCacheIndex a v b
insertUnappliedTx txh a idx =
  MkLeiosTxCacheIndex
    { announcementState = announcementState idx
    , announcementCount = announcementCount idx
    , bodyState = bodyState idx
    , txState = Map.alter upd txh (txState idx)
    , prunedSlot = prunedSlot idx
    }
 where
  upd Nothing = Nothing
  upd (Just tx) = Just (TxAlreadyInserted (L.view txRefCountL tx) a)

-- | Record the payload of an applied tx, without changing its refcount. A no-op
-- if no inserted body references this tx.
insertAppliedTx :: TxHash -> v -> LeiosTxCacheIndex a v b -> LeiosTxCacheIndex a v b
insertAppliedTx txh v idx =
  MkLeiosTxCacheIndex
    { announcementState = announcementState idx
    , announcementCount = announcementCount idx
    , bodyState = bodyState idx
    , txState = Map.alter upd txh (txState idx)
    , prunedSlot = prunedSlot idx
    }
 where
  upd Nothing = Nothing
  upd (Just tx) = Just (TxAlreadyValidated (L.view txRefCountL tx) v)

-- | The tx's recorded payload, if we hold it: @Left@ when inserted but not yet
-- applied, @Right@ when applied. 'Nothing' if the tx is absent or merely
-- referenced-but-not-yet-inserted.
lookupTx :: TxHash -> LeiosTxCacheIndex a v b -> Maybe (Either a v)
lookupTx txh idx = case Map.lookup txh (txState idx) of
  Nothing -> Nothing
  Just (TxNotYetInserted _) -> Nothing
  Just (TxAlreadyInserted _ a) -> Just (Left a)
  Just (TxAlreadyValidated _ v) -> Just (Right v)

lookupBody :: EbHash -> LeiosTxCacheIndex a v b -> Maybe b
lookupBody ebh idx = case Map.lookup ebh (bodyState idx) of
  Just (BodyAlreadyInserted _ b) -> Just b
  _ -> Nothing
