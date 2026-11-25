{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- | The reference implementation of the LeiosTxCache: a bounded, in-memory index
-- over the bodies of recent Leios EBs the node took in, and the txs it holds
-- that those bodies reference, with incremental eviction. Simple and obviously
-- correct, so that a faster implementation can be tested for observational
-- equivalence to it.
--
-- This is deliberately /independent/ of the on-disk LeiosDb: the two are
-- separate caches with different eviction policies. This index retains only
-- what is referenced by the bodies taken in at the @'maxLeiosPointCount'@
-- freshest points, so it stays small enough to keep resident in memory (a
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
--   * @b@ is the body taken in at a point. It must at least carry the body's
--     'TxHash'es, via 'ReferencesTxsByHash', so eviction can find its txs
--     without touching storage.
module Ouroboros.Consensus.Leios.TxCache.Reference
  ( -- * Index
    LeiosTxCacheIndex (..)
  , TrackedTx (..)
  , emptyLeiosTxCacheIndex

    -- * Operations
  , evictOlderThanFoldl'
  , insertBodyAndFoldl'
  , insertLandedTx
  , classifyTxs
  , lookupTx
  , lookupTxLocation
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..), maybeToStrictMaybe, strictMaybeToMaybe)
import Data.Set (Set)
import Ouroboros.Consensus.Leios.TxCache.API
  ( LeiosTxCacheInsertBodySummary (..)
  , ReferencesTxsByHash (..)
  , TxArrivalPrior (..)
  , TxState (..)
  , bucketTxArrival
  , maxLeiosPointCount
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace (FetchArrivalBytes)
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , LeiosPoint (..)
  , TxHash
  , TxLocation (..)
  , TxOffset
  )

-- | Classify each payload occurrence from the same immutable index. This
-- reads point retention without updating youngest references or locations.
classifyTxs :: LeiosPoint -> [(TxHash, BytesSize)] -> LeiosTxCacheIndex a v b -> FetchArrivalBytes
classifyTxs point txs idx = foldl' step mempty txs
 where
  retained = Map.member point (pointState idx)
  step !acc (txh, size) = acc <> bucketTxArrival (prior txh) size
  prior txh
    | not retained = TxEvicted
    | Map.member txh (txState idx) = TxWasHeld
    | otherwise = TxWasAbsent

-- | The index.
data LeiosTxCacheIndex a v b = MkLeiosTxCacheIndex
  { prunedSlot :: !SlotNo
  -- ^ The greatest slot 'evictOlderThanFoldl'' has pruned to (monotonically
  -- non-decreasing; 'SlotNo' @0@ until the first prune)
  , pointState :: !(Map LeiosPoint b)
  -- ^ The retained points, each with the body taken in at it. Two points of
  -- one EB are two entries, each with its own copy of the body.
  --
  -- INVARIANT: @'Map.size' pointState <= maxLeiosPointCount@.
  , txState :: !(Map TxHash (TrackedTx a v))
  -- ^ The txs the node holds that landed at a retained point
  -- ('insertLandedTx').
  }

-- | A tx the index holds.
data TrackedTx a v = MkTrackedTx
  { ttYoungestReference :: !LeiosPoint
  -- ^ The youngest retained point that this tx landed at, or whose body was
  -- taken in while the index held this tx and references it. Points are
  -- evicted oldest first, so the index forgets this tx when it evicts this
  -- point.
  , ttState :: !(TxState a v)
  , ttLocation :: !(StrictMaybe TxLocation)
  -- ^ The youngest retained point this tx landed at, with the tx's offset in
  -- that point's body.
  }

emptyLeiosTxCacheIndex :: LeiosTxCacheIndex a v b
emptyLeiosTxCacheIndex =
  MkLeiosTxCacheIndex
    { prunedSlot = SlotNo 0
    , pointState = Map.empty
    , txState = Map.empty
    }

{-------------------------------------------------------------------------------
  Operations
-------------------------------------------------------------------------------}

-- | Evict every retained point whose slot is strictly older than the boundary,
-- oldest first, as 'insertBodyAndFoldl'' evicts the oldest point. Returns the
-- evicted points, and folds over the txs forgotten with them, walking each
-- evicted body from its first tx to its last and visiting each forgotten tx
-- once: at its first offset in the body of its youngest reference. Advances
-- 'prunedSlot' to the boundary (monotonically), after which
-- 'insertBodyAndFoldl'' retains no point that old.
evictOlderThanFoldl' ::
  ReferencesTxsByHash b =>
  SlotNo ->
  w ->
  (w -> TxHash -> w) ->
  LeiosTxCacheIndex a v b ->
  (LeiosTxCacheIndex a v b, Set LeiosPoint, w)
evictOlderThanFoldl' boundary nil snoc idx =
  ( idx{prunedSlot = pruned, pointState = kept, txState = txState'}
  , Map.keysSet old
  , w
  )
 where
  pruned = max (prunedSlot idx) boundary
  (old, kept) = Map.spanAntitone ((< pruned) . pointSlotNo) (pointState idx)
  (txState', w) =
    Map.foldlWithKey'
      (\(!ts, !acc) point body -> releasePoint point body acc snoc ts)
      (txState idx, nil)
      old

-- | Evict the oldest retained point if 'maxLeiosPointCount' are retained.
evictOldestIfFull ::
  ReferencesTxsByHash b =>
  LeiosTxCacheIndex a v b ->
  LeiosTxCacheIndex a v b
evictOldestIfFull idx = case Map.minViewWithKey (pointState idx) of
  Just ((oldest, body), rest)
    | Map.size (pointState idx) >= maxLeiosPointCount ->
        idx{pointState = rest, txState = fst (releasePoint oldest body () (\() _txh -> ()) (txState idx))}
  _ -> idx

-- | Forget what the index holds because of the oldest retained point, which is
-- being evicted: each tx of its body whose youngest reference it is, and each
-- location at it. Folds over the txs it forgets, in body order.
releasePoint ::
  ReferencesTxsByHash b =>
  LeiosPoint ->
  b ->
  w ->
  (w -> TxHash -> w) ->
  Map TxHash (TrackedTx a v) ->
  (Map TxHash (TrackedTx a v), w)
releasePoint point body nil snoc ts0 = foldTxReferences step (ts0, nil) body
 where
  step (!ts, !w) txh _sz = case Map.lookup txh ts of
    Nothing -> (ts, w)
    Just tt
      | ttYoungestReference tt == point -> (Map.delete txh ts, snoc w txh)
      | otherwise -> (Map.insert txh tt{ttLocation = elsewhere (ttLocation tt)} ts, w)
  elsewhere = \case
    SJust (MkTxLocation src _off) | src == point -> SNothing
    loc -> loc

-- | Retain this point with its body, make it the youngest reference of each tx
-- the body references that the index holds, if it is younger, and summarise
-- that. A no-op when the point is already retained, when it is strictly older
-- than 'prunedSlot' (one the cache has already been pruned past; see
-- 'evictOlderThanFoldl''), or when 'maxLeiosPointCount' points are retained and
-- it is older than all of them. Otherwise, when 'maxLeiosPointCount' points are
-- retained, it first evicts the oldest (least slot, then least EB hash).
--
-- Either way, fold over its txs with their locations, as they are after any
-- eviction and before the insertion.
insertBodyAndFoldl' ::
  ReferencesTxsByHash b =>
  -- | The capacity to report as 'ibsCacheCapacity'; the pure index has no
  -- allocation of its own, so callers pass the production table's.
  Int ->
  LeiosPoint ->
  b ->
  w ->
  (w -> TxOffset -> TxHash -> BytesSize -> Maybe TxLocation -> w) ->
  LeiosTxCacheIndex a v b ->
  (LeiosTxCacheIndex a v b, (Maybe LeiosTxCacheInsertBodySummary, w))
insertBodyAndFoldl' capacity point body nil snoc idx
  | pointSlotNo point < prunedSlot idx = ignore
  | Map.member point (pointState idx) = ignore
  | Map.size (pointState idx) >= maxLeiosPointCount
  , any ((point <) . fst) (Map.lookupMin (pointState idx)) =
      ignore
  | otherwise =
      ( idx1
          { pointState = Map.insert point body (pointState idx1)
          , txState = foldTxReferences (\ts txh _sz -> Map.adjust younger txh ts) (txState idx1) body
          }
      , (Just (summarise capacity body idx1), locateTxs body nil snoc idx1)
      )
 where
  ignore = (idx, (Nothing, locateTxs body nil snoc idx))
  idx1 = evictOldestIfFull idx
  younger tt = tt{ttYoungestReference = max point (ttYoungestReference tt)}

-- | Summarise what the index holds of this body's txs.
summarise ::
  ReferencesTxsByHash b =>
  Int ->
  b ->
  LeiosTxCacheIndex a v b ->
  LeiosTxCacheInsertBodySummary
summarise capacity body idx =
  MkLeiosTxCacheInsertBodySummary
    { ibsTxsInEb = n
    , ibsAcquired = acquired
    , ibsValidated = validated
    , ibsCacheTxCount = Map.size (txState idx)
    , ibsCacheCapacity = capacity
    }
 where
  (n, acquired, validated) = foldTxReferences count (0, 0, 0) body
  count (!nn, !aa, !vv) txh _sz = case ttState <$> Map.lookup txh (txState idx) of
    Nothing -> (nn + 1, aa, vv)
    Just TxInserted{} -> (nn + 1, aa + 1, vv)
    Just TxValidated{} -> (nn + 1, aa + 1, vv + 1)

-- | Record that this point now holds this tx, the one its body lists at this
-- offset, with its state. A no-op if the index does not (still) retain the
-- point.
--
-- A tx the index does not hold is recorded with this point as its youngest
-- reference and its location. For one it holds, this point becomes its youngest
-- reference if it is younger than the tx's existing youngest reference, and its
-- location if it is younger than the tx's existing location or the tx has none.
-- Either way, the tx is recorded as validated if it already has been, or if
-- this says so.
insertLandedTx ::
  LeiosPoint ->
  TxOffset ->
  TxHash ->
  TxState a v ->
  LeiosTxCacheIndex a v b ->
  (LeiosTxCacheIndex a v b, TxArrivalPrior)
insertLandedTx point off txh st idx
  | Map.notMember point (pointState idx) = (idx, TxEvicted)
  | otherwise = (idx{txState = txState'}, prior)
 where
  (prior, txState') = Map.alterF upd txh (txState idx)
  here = MkTxLocation point off
  upd = \case
    Nothing -> (TxWasAbsent, Just (MkTrackedTx point st (SJust here)))
    Just tt ->
      ( TxWasHeld
      , Just
          MkTrackedTx
            { ttYoungestReference = max point (ttYoungestReference tt)
            , ttState = merge st (ttState tt)
            , ttLocation = case ttLocation tt of
                SJust loc@(MkTxLocation src _) | src > point -> SJust loc
                _ -> SJust here
            }
      )
  merge new@TxValidated{} _ = new
  merge TxInserted{} held@TxValidated{} = held
  merge new@TxInserted{} TxInserted{} = new

-- | The state of this tx, if the index holds it.
lookupTx :: TxHash -> LeiosTxCacheIndex a v b -> StrictMaybe (TxState a v)
lookupTx txh idx = maybeToStrictMaybe (ttState <$> Map.lookup txh (txState idx))

-- | Where this tx landed, if it landed at a retained point.
lookupTxLocation :: TxHash -> LeiosTxCacheIndex a v b -> Maybe TxLocation
lookupTxLocation txh idx = strictMaybeToMaybe . ttLocation =<< Map.lookup txh (txState idx)

-- | Fold over the txs this body references, each with its location, if the
-- index knows one.
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
      (\(!nn, !w) txh sz -> (nn + 1, snoc w nn txh sz (lookupTxLocation txh idx)))
      (0, nil)
      b
