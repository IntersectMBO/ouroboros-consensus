{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | The LeiosTxCache interface: the handle type and the small set of types
-- shared by every implementation. See "Ouroboros.Consensus.Leios.TxCache" for
-- the overview.
module Ouroboros.Consensus.Leios.TxCache.API
  ( -- * Handle
    LeiosTxCache (..)

    -- * Body payloads
  , ReferencesTxsByHash (..)

    -- * Shared refcount \/ body state
  , RefCount (..)
  , BodyState (..)
  , maxAnnouncementCount
  , EbRingIndex (UnsafeEbRingIndex)
  , mkEbRingIndex
  , unEbRingIndex

    -- * Insert-body observability summary
  , LeiosTxCacheInsertBodySummary (..)
  , defaultLeiosTxCacheShift
  , mkLeiosTxCacheInsertBodySummary
  , worstCaseCacheTxCount

    -- * Arrival classification
  , FetchArrivalBytes (..)
  , fetchArrivalInvalid
  , fetchArrivalEvicted
  , fetchArrivalGood
  , fetchArrivalExtra
  , TxArrivalPrior (..)
  , bucketTxArrival
  ) where

import Cardano.Slotting.Slot (SlotNo)
import Data.Set (Set)
import Data.Word (Word8)
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlockHash
  , RankingBlockHash
  , TxHash
  , TxLocation
  , TxOffset
  , maxTxsPerEndorserBlock
  )

-- | A monadic tx-cache handle: the pure index operations, each performing its
-- state update in @m@.
data LeiosTxCache m a v b = LeiosTxCache
  { insertAnnouncement ::
      SlotNo -> RankingBlockHash -> EndorserBlockHash -> m (Set EndorserBlockHash, Set TxHash)
  -- ^ Insert an announcement; returns the bodies and txs it evicted, if any. A
  -- no-op for an EB whose slot is strictly older than the latest 'evictOlderThan'
  -- boundary — the cache has already been pruned past it.
  , evictOlderThan :: SlotNo -> m (Set EndorserBlockHash, Set TxHash)
  -- ^ Evict every retained announcement whose slot is strictly older than the
  -- given boundary; returns the bodies and txs it evicted, if any. The tip-driven
  -- eviction entrypoint (see "Ouroboros.Consensus.Leios.TxCache"),
  -- complementing the count-driven eviction that 'insertAnnouncement' performs.
  -- Also records the boundary, after which 'insertAnnouncement' ignores any EB
  -- that old.
  --
  -- ORDERING CONTRACT — MUST HOLD: prune this in-memory cache to a slot @X@
  -- /strictly before/ the LeiosDb is pruned to that same @X@ — never after, never
  -- concurrently. This cache is only an index of the LeiosDb, so evicting from the
  -- index first is what guarantees it can never report a hit for a tx the LeiosDb
  -- has already dropped. Reverse the order and you arm the hit-prune hazard: a
  -- false hit ⇒ a skipped fetch ⇒ a silently-incomplete EB closure.
  , insertBody ::
      forall w.
      EndorserBlockHash ->
      b ->
      w ->
      (w -> TxOffset -> TxHash -> BytesSize -> Maybe TxLocation -> w) ->
      m (Maybe (LeiosTxCacheInsertBodySummary, w))
  -- ^ Record that we hold this EB's body, bumping the refcount of each tx it
  -- references. In the same pass, fold a caller-supplied accumulator over /every/
  -- referenced tx: starting from the nil @w@ and extending it with the snoc @w ->
  -- offset -> 'TxHash' -> 'BytesSize' -> 'Maybe' 'TxLocation' -> w@, where @offset@
  -- is the tx's position in the body and the location is 'Just' iff some held EB
  -- durably holds the tx's bytes ('setTxLocations'), i.e. it can serve a cross-EB
  -- fill. The entry is already in hand for the refcount bump, so this location read
  -- rides that pass rather than costing a second one. Returns the summary and the
  -- built @w@, or 'Nothing' when the EB is unannounced or its body is already
  -- inserted (a no-op, so nothing is folded).
  , lookupBody :: EndorserBlockHash -> m (Maybe b)
  -- ^ The EB's body if we hold it (its 'BodyState' is 'BodyAlreadyInserted');
  -- 'Nothing' if the EB is untracked or only announced. Unlike a tx, an EB body
  -- pins itself: a hit means it is in the LeiosDb and stays there until the EB is
  -- pruned, so no cross-object reasoning is needed.
  , withLockedInsertUnappliedTx ::
      (forall w. w -> (w -> TxHash -> BytesSize -> a -> m w) -> m w) ->
      m FetchArrivalBytes
  -- ^ Has exclusive write-access
  --
  -- The 'BytesSize' argument is only used to accumulate the
  -- 'FetchArrivalBytes', which is only used for observability.
  , withLockedInsertAppliedTx :: (forall w. w -> (w -> TxHash -> v -> m w) -> m w) -> m ()
  -- ^ Has exclusive write-access
  , withLookupTx :: forall r. ((TxHash -> m (Maybe (Either a v))) -> m r) -> m r
  -- ^ Also holds the lock
  , setTxLocations :: EndorserBlockHash -> [(TxOffset, TxHash)] -> m ()
  -- ^ Advertise where these txs' DURABLE bytes live -- the given EB's rows at
  -- the given offsets -- for cross-EB fill. Call this on write confirmation
  -- only: a location must never name bytes that could still be lost. A tx the
  -- cache no longer tracks is skipped, never resurrected. Latest location wins:
  -- a tx held by several EBs points at the most recently advertised one (evicted
  -- last).
  --
  -- MAY be called repeatedly for one EB: its closure arrives over many
  -- 'MsgLeiosBlockTxs', and each batch confirms a slice. The EB claims a single
  -- ring slot the first time it is seen and every later call reuses it, so one
  -- EB costs one slot however many batches deliver it -- claiming a slot per call
  -- would wrap the ring every @maxAnnouncementCount@ batches rather than EBs.
  --
  -- The read side is 'insertBody': the next body to reference a tx observes its
  -- location in the same pass it bumps refcounts. A location so observed may be
  -- stale (the source swept, its ring slot reused): the LeiosDb fill guards make
  -- that a no-op, so staleness costs a fetch, never corruption.
  }

-- | A body @b@ from which the referenced txs can be enumerated, each paired with
-- its on-the-wire size, in body order.
--
-- The fold must visit each referenced 'TxHash' at most once per body (a valid EB
-- body references a tx at most once), so that a body contributes exactly one to
-- each of its txs' refcounts. Visiting in body order lets a consumer recover each
-- tx's offset from its position in the fold.
class ReferencesTxsByHash b where
  foldTxReferences :: (r -> TxHash -> BytesSize -> r) -> r -> b -> r

-- | The maximum number of EB announcements retained. Inserting past it evicts
-- the oldest, cascading through the body and tx refcounts.
maxAnnouncementCount :: Int
maxAnnouncementCount = 128 -- TODO magic number

-- | An index into the source-EB ring the tx-location machinery keeps (the last
-- 'maxAnnouncementCount' EBs whose txs became durable). Meant to hold a value in
-- @[0, 'maxAnnouncementCount')@, so it fits the packed location word's ring field
-- and can never key the ring out of bounds. 'mkEbRingIndex' is the checked way in;
-- 'UnsafeEbRingIndex' is the escape hatch for a caller that maintains the bound
-- itself, e.g. after reducing a monotone claim counter mod the ring size.
newtype EbRingIndex = UnsafeEbRingIndex Int
  deriving (Eq, Ord, Show)

-- | Check an 'Int' against the ring's bounds: 'Just' iff it lies in
-- @[0, 'maxAnnouncementCount')@. Use for an index read back from packed or stored
-- data, where out of range means corruption and the caller should decline the
-- location rather than key the ring wrongly.
mkEbRingIndex :: Int -> Maybe EbRingIndex
mkEbRingIndex n
  | n >= 0 && n < maxAnnouncementCount = Just (UnsafeEbRingIndex n)
  | otherwise = Nothing

-- | The raw slot, for packing into the location word.
unEbRingIndex :: EbRingIndex -> Int
unEbRingIndex (UnsafeEbRingIndex i) = i

-- | A reference count.
--
-- INVARIANT: @> 0@ (an entry at zero is removed rather than stored).
newtype RefCount = MkRefCount Word8
  deriving (Eq, Show)

data BodyState b
  = -- | An announcement of this EB has been inserted, but not its body.
    BodyNotYetInserted {-# UNPACK #-} !RefCount
  | BodyAlreadyInserted {-# UNPACK #-} !RefCount !b

-- | Summary of an EB body inserted into the LeiosTxCache, for observability.
-- The counts nest: @ibsTxsInEb >= ibsTracked >= ibsAcquired >= ibsValidated@.
data LeiosTxCacheInsertBodySummary = MkLeiosTxCacheInsertBodySummary
  { ibsTxsInEb :: !Int
  -- ^ txs the EB body references
  , ibsTracked :: !Int
  -- ^ of those, how many the cache already tracked
  , ibsAcquired :: !Int
  -- ^ of those tracked, how many were already acquired (inserted or validated)
  , ibsValidated :: !Int
  -- ^ of those acquired, how many were already validated
  , ibsCacheTxCount :: !Int
  -- ^ total txs the cache tracks after this insert
  , ibsCacheLoad :: !Double
  -- ^ 'ibsCacheTxCount' as a fraction of the worst-case cache capacity
  }
  deriving (Eq, Show)

-- | The bytes of one LeiosFetch arrival ('MsgLeiosBlock' or 'MsgLeiosBlockTxs'),
-- partitioned by the arriving item's /prior/ state in the LeiosTxCache. The four
-- fields sum to the message's total size.
data FetchArrivalBytes = MkFetchArrivalBytes
  { fabInvalid :: !BytesSize
  -- ^ Bytes of an invalid message (the whole message).
  , fabEvicted :: !BytesSize
  -- ^ Bytes whose prior cache state was absent (assumed present once, since evicted).
  , fabGood :: !BytesSize
  -- ^ Bytes in the expected not-yet-inserted state.
  , fabExtra :: !BytesSize
  -- ^ Bytes already inserted (redundant).
  }
  deriving (Eq, Show)

instance Semigroup FetchArrivalBytes where
  MkFetchArrivalBytes a1 b1 c1 d1 <> MkFetchArrivalBytes a2 b2 c2 d2 =
    MkFetchArrivalBytes (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)

instance Monoid FetchArrivalBytes where
  mempty = MkFetchArrivalBytes 0 0 0 0

-- | The message's whole size attributed to a single bucket, the rest zero.
fetchArrivalInvalid
  , fetchArrivalEvicted
  , fetchArrivalGood
  , fetchArrivalExtra ::
    BytesSize -> FetchArrivalBytes
fetchArrivalInvalid n = mempty{fabInvalid = n}
fetchArrivalEvicted n = mempty{fabEvicted = n}
fetchArrivalGood n = mempty{fabGood = n}
fetchArrivalExtra n = mempty{fabExtra = n}

-- | A tx's state in the cache /before/ an unapplied insert, surfaced by the
-- 'withLockedInsertUnappliedTx' step so a caller can classify an arriving tx.
data TxArrivalPrior
  = -- | Untracked: no held body references it (assumed present once, since evicted).
    TxWasUntracked
  | -- | Referenced by a held body but not yet acquired (the expected case).
    TxWasNotYetInserted
  | -- | Already acquired (inserted or validated); a redundant delivery.
    TxWasAlreadyHeld
  deriving (Eq, Show)

-- | Bucket an arriving tx's bytes by its prior state (for 'FetchArrivalBytes').
bucketTxArrival :: TxArrivalPrior -> BytesSize -> FetchArrivalBytes
bucketTxArrival = \case
  TxWasUntracked -> fetchArrivalEvicted
  TxWasNotYetInserted -> fetchArrivalGood
  TxWasAlreadyHeld -> fetchArrivalExtra

-- | The worst-case number of txs the cache can be asked to hold: a full
-- 'maxAnnouncementCount' window of EBs, each referencing the maximum
-- 'maxTxsPerEndorserBlock' distinct txs.
worstCaseCacheTxCount :: Int
worstCaseCacheTxCount = maxAnnouncementCount * maxTxsPerEndorserBlock

-- | The table shift the node allocates its Leios tx cache with: @2 ^@ this
-- many slots. The pure reference handle reports 'ibsCacheLoad' against the
-- same allocation, so the load factor means one thing across both handles.
-- It covers 'worstCaseCacheTxCount' about twice over.
--
-- The latency profile of accessing the hash table is part of the overall
-- diffusion correctness, and that begins to degrade when the load factor
-- exceeds ~50%. So that degradation is already a limiting factor even before it
-- risks a total failure (at >100%). Keep this in mind before lowering this or
-- raising 'maxTxsPerEndorserBlock'.
defaultLeiosTxCacheShift :: Int
defaultLeiosTxCacheShift = 22

-- | Build an 'LeiosTxCacheInsertBodySummary' from the raw counts, computing the
-- load factor ('ibsCacheLoad') against the given capacity — how full this
-- cache is, so 1.0 coincides with it actually filling up.
mkLeiosTxCacheInsertBodySummary ::
  Int -> Int -> Int -> Int -> Int -> Int -> LeiosTxCacheInsertBodySummary
mkLeiosTxCacheInsertBodySummary capacity txsInEb tracked acquired validated cacheTxCount =
  MkLeiosTxCacheInsertBodySummary
    { ibsTxsInEb = txsInEb
    , ibsTracked = tracked
    , ibsAcquired = acquired
    , ibsValidated = validated
    , ibsCacheTxCount = cacheTxCount
    , ibsCacheLoad = fromIntegral cacheTxCount / fromIntegral capacity
    }
