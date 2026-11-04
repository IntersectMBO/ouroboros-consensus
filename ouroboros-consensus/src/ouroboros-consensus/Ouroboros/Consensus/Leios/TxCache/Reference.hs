{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- | The reference implementation of the LeiosTxCache: a bounded, in-memory index
-- over the recently-announced Leios EBs, their bodies, and the txs those bodies
-- reference, with reference-counted incremental eviction. Simple and obviously
-- correct, so that a faster implementation can be tested for observational
-- equivalence to it.
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
--     yet applied; @()@ in the node, or the tx's bytes in a test.
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
module Ouroboros.Consensus.Leios.TxCache.Reference
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
  , setTxLocations
  , lookupTxLocation

    -- * Internal state (exposed for testing)
  , TxState (..)
  , bodyRefCount
  , txRefCount

    -- * Shared types (re-exported from "Ouroboros.Consensus.Leios.TxCache.API")
  , ReferencesTxsByHash (..)
  , RefCount (..)
  , BodyState (..)
  , maxAnnouncementCount
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import Data.List (find)
import qualified Data.List as List
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Ouroboros.Consensus.Leios.TxCache.API
  ( BodyState (..)
  , EbRingIndex (UnsafeEbRingIndex)
  , LeiosTxCacheInsertBodySummary
  , RefCount (..)
  , ReferencesTxsByHash (..)
  , TxArrivalPrior (..)
  , maxAnnouncementCount
  , mkLeiosTxCacheInsertBodySummary
  )
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlockHash
  , LeiosPoint (..)
  , RankingBlockHash
  , TxHash
  , TxLocation (..)
  , TxOffset
  )

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
  { announcementState :: !(Map SlotNo (NEMap RankingBlockHash EndorserBlockHash))
  -- ^ The retained EB announcements, keyed by slot then announcing RB header.
  , announcementCount :: !Int
  -- ^ INVARIANT: @= sum (fmap NEMap.size announcementState)@.
  --
  -- INVARIANT: @0 <= announcementCount <= maxAnnouncementCount@.
  , bodyState :: !(Map EndorserBlockHash (BodyState b))
  -- ^ INVARIANT: each 'RefCount' equals the number of announcements in
  -- 'announcementState' whose 'EndorserBlockHash' is this one.
  , txState :: !(Map TxHash (TxState a v))
  -- ^ INVARIANT: each 'RefCount' equals the number of 'BodyAlreadyInserted's in
  -- 'bodyState' that reference this tx.
  , prunedSlot :: !SlotNo
  -- ^ The greatest slot 'evictOlderThan' has pruned to (monotonically
  -- non-decreasing; 'SlotNo' @0@ until the first prune)
  , txLocState :: !(Map TxHash (EbRingIndex, TxOffset))
  -- ^ Where a tracked tx's durable bytes live: a 'locRing' slot and an offset
  -- into that EB. INVARIANT: keys are a subset of 'txState''s. In practice only
  -- of its 'TxAlreadyInserted' keys: 'setTxLocations' runs just after bytes
  -- become durable, never for a 'TxNotYetInserted' tx, which has no bytes to
  -- point at. Eviction then prunes this map by 'txState' refcount, so the subset
  -- claim is what holds in general.
  , locRing :: !(Map EbRingIndex LeiosPoint)
  -- ^ The points recent tx locations point into, keyed by ring slot
  -- (@'locNext' \`mod\` 'maxAnnouncementCount'@ at claim time). Slot reuse
  -- makes a stored location stale, not wrong: the db fill guards it.
  , locNext :: !Int
  -- ^ Next ring slot to claim (monotone).
  }

emptyLeiosTxCacheIndex :: LeiosTxCacheIndex a v b
emptyLeiosTxCacheIndex =
  MkLeiosTxCacheIndex
    { announcementState = Map.empty
    , announcementCount = 0
    , bodyState = Map.empty
    , txState = Map.empty
    , prunedSlot = SlotNo 0
    , txLocState = Map.empty
    , locRing = Map.empty
    , locNext = 0
    }

{-------------------------------------------------------------------------------
  Refcounts

  Record syntax handles the 'LeiosTxCacheIndex' fields; these are only for the
  refcount that is common to every constructor of the 'BodyState' \/ 'TxState'
  sums, where record syntax does not suffice.
-------------------------------------------------------------------------------}

-- | The refcount of a body, regardless of whether the body itself is inserted.
bodyRefCount :: BodyState b -> RefCount
bodyRefCount = \case
  BodyNotYetInserted rc -> rc
  BodyAlreadyInserted rc _ -> rc

overBodyRefCount :: (RefCount -> RefCount) -> BodyState b -> BodyState b
overBodyRefCount f = \case
  BodyNotYetInserted rc -> BodyNotYetInserted (f rc)
  BodyAlreadyInserted rc b -> BodyAlreadyInserted (f rc) b

-- | The refcount of a tx, regardless of its insertion\/application state.
txRefCount :: TxState a v -> RefCount
txRefCount = \case
  TxNotYetInserted rc -> rc
  TxAlreadyInserted rc _ -> rc
  TxAlreadyValidated rc _ -> rc

overTxRefCount :: (RefCount -> RefCount) -> TxState a v -> TxState a v
overTxRefCount f = \case
  TxNotYetInserted rc -> TxNotYetInserted (f rc)
  TxAlreadyInserted rc a -> TxAlreadyInserted (f rc) a
  TxAlreadyValidated rc v -> TxAlreadyValidated (f rc) v

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
  RankingBlockHash ->
  EndorserBlockHash ->
  LeiosTxCacheIndex a v b ->
  (LeiosTxCacheIndex a v b, Set EndorserBlockHash, Set TxHash)
insertAnnouncement slot rbh ebh idx
  | slot < prunedSlot idx = (idx, Set.empty, Set.empty)
  | alreadyPresent = (idx, Set.empty, Set.empty)
  | otherwise = evictIfNeeded inserted
 where
  alreadyPresent = case Map.lookup slot (announcementState idx) of
    Nothing -> False
    Just nem -> NEMap.member rbh nem

  inserted =
    idx
      { announcementState =
          Map.alter
            (Just . maybe (NEMap.singleton rbh ebh) (NEMap.insert rbh ebh))
            slot
            (announcementState idx)
      , announcementCount = announcementCount idx + 1
      , bodyState =
          Map.alter
            (Just . maybe (BodyNotYetInserted (MkRefCount 1)) (overBodyRefCount incRefCount))
            ebh
            (bodyState idx)
      }

-- | Repeatedly 'evictOldest' while @shouldEvict@ holds of the index: the shared
-- core of the two eviction entrypoints ('evictIfNeeded' and 'evictOlderThan').
-- Strict accumulators avoid building up '<>' thunks.
evictWhile ::
  ReferencesTxsByHash b =>
  (LeiosTxCacheIndex a v b -> Bool) ->
  LeiosTxCacheIndex a v b ->
  (LeiosTxCacheIndex a v b, Set EndorserBlockHash, Set TxHash)
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
  (LeiosTxCacheIndex a v b, Set EndorserBlockHash, Set TxHash)
evictIfNeeded = evictWhile ((> maxAnnouncementCount) . announcementCount)

-- | Evict every retained announcement whose slot is strictly older than the
-- boundary, cascading through the bodies and txs like 'insertAnnouncement'.
-- Returns the evicted bodies and txs. Advances 'prunedSlot' to the boundary
-- (monotonically), after which 'insertAnnouncement' refuses any EB that old.
evictOlderThan ::
  ReferencesTxsByHash b =>
  SlotNo ->
  LeiosTxCacheIndex a v b ->
  (LeiosTxCacheIndex a v b, Set EndorserBlockHash, Set TxHash)
evictOlderThan boundary idx =
  evictWhile oldestIsStale idx'
 where
  idx' = idx{prunedSlot = max (prunedSlot idx) boundary}
  oldestIsStale i = case Map.lookupMin (announcementState i) of
    Just (slotMin, _) -> slotMin < prunedSlot idx'
    Nothing -> False

-- | Evict the oldest announcement. The index must hold at least one.
evictOldest ::
  ReferencesTxsByHash b =>
  LeiosTxCacheIndex a v b ->
  (LeiosTxCacheIndex a v b, Set EndorserBlockHash, Set TxHash)
evictOldest idx =
  ( idx
      { announcementState = announcementState'
      , announcementCount = announcementCount idx - 1
      , bodyState = bodyState'
      , txState = txState'
      , txLocState = txLocState idx `Map.withoutKeys` evTxs
      , locRing = locRing'
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

  -- Evicting a body prunes the LeiosDb rows of its points, so their ring slots
  -- no longer name durable fill sources: free them. A tx still held by a
  -- younger point keeps that point's location (set by 'setTxLocations' when the
  -- younger body landed); a tx that lived only in the evicted points now
  -- resolves to /no/ location and is fetched, rather than to a pruned source
  -- (or, once 'locNext' reuses the slot, a different point). Only a genuine body
  -- eviction frees the slots; dropping one of several announcements of a
  -- still-held EB leaves them in place.
  locRing'
    | Set.member ebhEvicted evEbs = Map.filter ((/= ebhEvicted) . pointEbHash) (locRing idx)
    | otherwise = locRing idx

-- | Decrement a body's refcount; if it reaches zero, remove it and (if it had
-- been inserted) decrement each of its referenced txs.
decBody ::
  ReferencesTxsByHash b =>
  EndorserBlockHash ->
  Map EndorserBlockHash (BodyState b) ->
  Map TxHash (TxState a v) ->
  (Map EndorserBlockHash (BodyState b), Map TxHash (TxState a v), Set EndorserBlockHash, Set TxHash)
decBody ebh bs ts = case Map.lookup ebh bs of
  Nothing -> (bs, ts, Set.empty, Set.empty)
  Just b -> case decRefCount (bodyRefCount b) of
    SJust rc -> (Map.insert ebh (overBodyRefCount (const rc) b) bs, ts, Set.empty, Set.empty)
    SNothing ->
      let (ts', evTxs) = case b of
            BodyNotYetInserted _ -> (ts, Set.empty)
            BodyAlreadyInserted _ body ->
              foldTxReferences (\acc txh _sz -> decTx acc txh) (ts, Set.empty) body
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
  upd (Just tx) = case decRefCount (txRefCount tx) of
    SJust rc -> (Set.empty, Just (overTxRefCount (const rc) tx))
    SNothing -> (Set.singleton txh, Nothing)

-- | Record that we now hold the body of this EB, bumping the refcount of each tx
-- it references. Idempotent, and a no-op if no announcement references this EB
-- (its refcount would be zero). Either way, fold over its txs with their
-- locations.
insertBody ::
  ReferencesTxsByHash b =>
  -- | Capacity 'ibsCacheLoad' is reported against; the pure index has no
  -- allocation of its own, so callers pass the production table's.
  Int ->
  EndorserBlockHash ->
  b ->
  w ->
  (w -> TxOffset -> TxHash -> BytesSize -> Maybe TxLocation -> w) ->
  LeiosTxCacheIndex a v b ->
  (LeiosTxCacheIndex a v b, (Maybe LeiosTxCacheInsertBodySummary, w))
insertBody loadCapacity ebh body nil snoc idx = case Map.lookup ebh (bodyState idx) of
  Nothing -> (idx, (Nothing, locateTxs body nil snoc idx))
  Just BodyAlreadyInserted{} -> (idx, (Nothing, locateTxs body nil snoc idx))
  Just (BodyNotYetInserted rc) ->
    let ((n, tracked, acquired, validated, w), txState') =
          foldTxReferences bumpTx ((0, 0, 0, 0, nil), txState idx) body
        idx' =
          idx
            { bodyState = Map.insert ebh (BodyAlreadyInserted rc body) (bodyState idx)
            , txState = txState'
            }
     in ( idx'
        ,
          ( Just $
              mkLeiosTxCacheInsertBodySummary
                loadCapacity
                n
                tracked
                acquired
                validated
                (Map.size txState')
          , w
          )
        )
 where
  -- Bump each tx's refcount and, in the same pass, classify its /prior/ state
  -- (the counts feed the summary) and read its durable location, snoc'ing every
  -- referenced tx onto the caller's accumulator at its body offset ('nn'). The
  -- location is 'Just' iff a held EB owns the tx's bytes, so the fill lookup
  -- needs no second traversal.
  bumpTx ((!nn, !tt, !aa, !vv, !w), ts) txh sz =
    let (dt, da, dv) = case Map.lookup txh ts of
          Nothing -> (0, 0, 0) -- new: not yet tracked
          Just (TxNotYetInserted _) -> (1, 0, 0) -- tracked, not acquired
          Just (TxAlreadyInserted _ _) -> (1, 1, 0) -- acquired, not validated
          Just (TxAlreadyValidated _ _) -> (1, 1, 1) -- acquired and validated
        ts' =
          Map.alter
            (Just . maybe (TxNotYetInserted (MkRefCount 1)) (overTxRefCount incRefCount))
            txh
            ts
        w' = snoc w nn txh sz (lookupTxLocation txh idx)
     in ((nn + 1, tt + dt, aa + da, vv + dv, w'), ts')

-- | Record the payload of a fetched-but-not-yet-applied tx
insertUnappliedTx ::
  TxHash -> a -> LeiosTxCacheIndex a v b -> (LeiosTxCacheIndex a v b, TxArrivalPrior)
insertUnappliedTx txh a idx = (idx{txState = txState'}, prior)
 where
  (prior, txState') = Map.alterF (\mv -> (classify mv, upd mv)) txh (txState idx)
  classify = \case
    Nothing -> TxWasUntracked
    Just (TxNotYetInserted _) -> TxWasNotYetInserted
    Just TxAlreadyInserted{} -> TxWasAlreadyHeld
    Just TxAlreadyValidated{} -> TxWasAlreadyHeld
  upd Nothing = Nothing
  upd (Just tx) = Just (TxAlreadyInserted (txRefCount tx) a)

-- | Record the payload of an applied tx, without changing its refcount. A no-op
-- if no inserted body references this tx.
insertAppliedTx :: TxHash -> v -> LeiosTxCacheIndex a v b -> LeiosTxCacheIndex a v b
insertAppliedTx txh v idx = idx{txState = Map.alter upd txh (txState idx)}
 where
  upd Nothing = Nothing
  upd (Just tx) = Just (TxAlreadyValidated (txRefCount tx) v)

-- | The tx's recorded payload, if we hold it: @Left@ when inserted but not yet
-- applied, @Right@ when applied. 'Nothing' if the tx is absent or merely
-- referenced-but-not-yet-inserted.
lookupTx :: TxHash -> LeiosTxCacheIndex a v b -> Maybe (Either a v)
lookupTx txh idx = case Map.lookup txh (txState idx) of
  Nothing -> Nothing
  Just (TxNotYetInserted _) -> Nothing
  Just (TxAlreadyInserted _ a) -> Just (Left a)
  Just (TxAlreadyValidated _ v) -> Just (Right v)

lookupBody :: EndorserBlockHash -> LeiosTxCacheIndex a v b -> Maybe b
lookupBody ebh idx = case Map.lookup ebh (bodyState idx) of
  Nothing -> Nothing
  Just BodyNotYetInserted{} -> Nothing
  Just (BodyAlreadyInserted _ b) -> Just b

-- | Record where each tx's durable bytes now live: point each /tracked/ tx at
-- the source point's ring slot. The latest location wins -- the newest source
-- point outlives older ones in both the ring and the db.
--
-- A point's closure arrives over one or more batches, each a separate call
-- here; all of them must land in the single slot the point holds. So a new slot
-- is claimed only the first time a point is seen (while it still occupies the
-- ring); claiming one per batch would wrap the ring every
-- 'maxAnnouncementCount' /batches/ rather than points, overwriting slots still
-- referenced by recent points.
setTxLocations ::
  LeiosPoint -> [(TxOffset, TxHash)] -> LeiosTxCacheIndex a v b -> LeiosTxCacheIndex a v b
setTxLocations point offTxs idx =
  idx
    { locRing = Map.insert slot point (locRing idx)
    , locNext = locNext'
    , txLocState = List.foldl' upd (txLocState idx) offTxs
    }
 where
  (slot, locNext') = case ringSlotOf point idx of
    Just s -> (s, locNext idx)
    Nothing -> (UnsafeEbRingIndex (locNext idx `mod` maxAnnouncementCount), locNext idx + 1)
  upd m (off, txh)
    | Map.member txh (txState idx) = Map.insert txh (slot, off) m
    | otherwise = m

-- | The ring slot a point currently occupies, if it still does. Scans the
-- ring, at most 'maxAnnouncementCount' entries.
ringSlotOf :: LeiosPoint -> LeiosTxCacheIndex a v b -> Maybe EbRingIndex
ringSlotOf point = fmap fst . find ((== point) . snd) . Map.toList . locRing

-- | Where this tx's durable bytes live, if a tracked tx has a recorded location
-- whose ring slot is still populated. Possibly stale (see 'locRing').
lookupTxLocation :: TxHash -> LeiosTxCacheIndex a v b -> Maybe TxLocation
lookupTxLocation txh idx = do
  (ringIdx, off) <- Map.lookup txh (txLocState idx)
  src <- Map.lookup ringIdx (locRing idx)
  Just (MkTxLocation src off)

-- | Fold over the txs this body references, each with its durable location if
-- the index knows one: what 'insertBody' does for a body it does not insert.
locateTxs ::
  ReferencesTxsByHash b =>
  b ->
  w ->
  (w -> TxOffset -> TxHash -> BytesSize -> Maybe TxLocation -> w) ->
  LeiosTxCacheIndex a v b ->
  w
locateTxs b nil snoc idx =
  snd $
    foldTxReferences
      (\(nn, w) txh sz -> (nn + 1, snoc w nn txh sz (lookupTxLocation txh idx)))
      (0, nil)
      b
