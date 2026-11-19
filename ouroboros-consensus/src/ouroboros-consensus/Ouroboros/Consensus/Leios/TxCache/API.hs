{-# LANGUAGE BangPatterns #-}
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
  , PackedEndorserBlock
  , pebBytes
  , packEndorserBlock

    -- * Shared refcount \/ body state
  , RefCount (..)
  , HeldBody (..)
  , maxLeiosPointCount
  , EbRingIndex (UnsafeEbRingIndex)
  , mkEbRingIndex
  , unEbRingIndex

    -- * Insert-body observability summary
  , LeiosTxCacheInsertBodySummary (..)
  , defaultLeiosTxCacheShift
  , mkLeiosTxCacheInsertBodySummary
  , worstCaseCacheTxCount

    -- * Arrival classification
  , TxArrivalPrior (..)
  , bucketTxArrival
  ) where

import Cardano.Crypto.Hash (Hash (UnsafeHash), hashToBytesShort)
import Cardano.Slotting.Slot (SlotNo)
import Data.Bits (shiftL, shiftR, (.|.))
import qualified Data.ByteString.Short as SBS
import Data.Set (Set)
import Data.Word (Word8)
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , EndorserBlockHash
  , LeiosPoint
  , TxHash
  , TxLocation
  , TxOffset
  , endorserBlockReferences
  , endorserBlockShortBytes
  , maxTxsPerEndorserBlock
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
  ( FetchArrivalBytes
  , LeiosTxCacheInsertBodySummary (..)
  , fetchArrivalEvicted
  , fetchArrivalExtra
  , fetchArrivalGood
  )

-- | A monadic tx-cache handle: the pure index operations, each performing its
-- state update in @m@. Retention is bounded by body-bearing points; see
-- @docs/fetch-policy/leiosfetch-improved.md#cache-retention@.
data LeiosTxCache m a v b = LeiosTxCache
  { evictOlderThan :: SlotNo -> m (Set EndorserBlockHash, Set TxHash)
  -- ^ Evict every retained point whose slot is strictly older than the
  -- given boundary; returns the bodies and txs it evicted, if any. The tip-driven
  -- eviction entrypoint (see "Ouroboros.Consensus.Leios.TxCache"),
  -- complementing the count-driven eviction that 'insertBody' performs. Also
  -- records the boundary, after which 'insertBody' retains no point that old.
  --
  -- ORDERING CONTRACT: prune this metadata before forwarding database
  -- promotion to the same boundary, releasing the cache lock before storage IO.
  -- Promotion changes authoritative read routing; subsequent collection can
  -- remove volatile bytes. The monotonic fence rejects later publication from
  -- older points. Pruning early can lose a reuse opportunity, but deletes no
  -- database bytes. Already-captured locations remain hints: the writer checks
  -- their source and confirms only actual target possession.
  , insertBody ::
      forall w.
      LeiosPoint ->
      b ->
      w ->
      (w -> TxOffset -> TxHash -> BytesSize -> Maybe TxLocation -> w) ->
      m (Maybe LeiosTxCacheInsertBodySummary, w)
  -- ^ Take in the body of the EB at this point: retain the point, and record
  -- that we hold the body, bumping the refcount of each tx it references, and
  -- summarise that. There is no summary when the body is already held, for
  -- another point; then only the point is retained. Nothing changes, and there
  -- is no summary, when the point is already retained, or is strictly older
  -- than the latest 'evictOlderThan' boundary.
  --
  -- Retaining a point past 'maxLeiosPointCount' evicts the oldest (least slot,
  -- then least EB hash), cascading through the body and tx refcounts. A body
  -- shared by several retained points contributes only one TX reference per
  -- distinct hash it lists. No prior announcement is required. This retention
  -- is not confirmation that the target body write landed.
  --
  -- Either way, fold a caller-supplied accumulator over /every/ referenced tx:
  -- starting from the nil @w@ and extending it with the snoc @w -> offset ->
  -- 'TxHash' -> 'BytesSize' -> 'Maybe' 'TxLocation' -> w@, where @offset@ is the
  -- tx's position in the body. A 'Just' location was published from confirmed
  -- bytes ('setTxLocations'); it proposes a cross-EB fill but does not pin its
  -- source against later retirement. The writer checks availability when it
  -- consumes the descriptor. When the body is inserted, this location read
  -- shares the refcount pass rather than requiring a second traversal.
  , lookupBody :: EndorserBlockHash -> m (Maybe b)
  -- ^ The EB's body if we hold it. A hit means only that the body was taken in
  -- for some point of the EB: it says nothing about what the LeiosDb holds at
  -- any point.
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
  , setTxLocations :: LeiosPoint -> [(TxOffset, TxHash)] -> m ()
  -- ^ Advertise where these txs' DURABLE bytes live -- the given point's rows
  -- at the given offsets -- for cross-point fill. Call this from positive write
  -- confirmation, never merely from queue admission. Confirmation proves past
  -- presence, not a pin against subsequent retirement. A tx the
  -- cache no longer tracks is skipped, never resurrected. Publication from a
  -- point below the pruning frontier is rejected under the same cache lock,
  -- even when a newer body still tracks that TX. Among eligible points, the
  -- most recently advertised location wins.
  --
  -- MAY be called repeatedly for one point: its closure arrives over many
  -- 'MsgLeiosBlockTxs', and each batch confirms a slice. The point claims a
  -- single ring entry the first time it is seen and every later call reuses it,
  -- so one point costs one entry however many batches its closure arrives in --
  -- claiming an entry per call would wrap the ring every @maxLeiosPointCount@
  -- batches rather than points.
  --
  -- The read side is 'insertBody': the next body to reference a tx observes its
  -- location in the same pass it bumps refcounts. A location so observed may be
  -- stale (the source swept, its ring entry reused): the LeiosDb fill guards make
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

-- | The node's body payload: the bytes the endorser block was received in, and
-- each referenced tx's hash and size, packed, in body order, at 36 bytes per
-- tx. The cache retains up to 'maxLeiosPointCount' bodies, so a decoded
-- 'EndorserBlock' would cost several times more.
--
-- TODO The received bytes take about as much as the packed references, 35 to
-- 39 bytes per reference against 36, so keeping both doubles what the bodies
-- cost: up to 'maxLeiosPointCount' (128) bodies of up to
-- 'Ouroboros.Consensus.Leios.Types.maxEndorserBlockBytesSize' (600 kB) each is
-- about 77 MB at worst for either half, and so about 154 MB for both. Keeping
-- only the bytes would instead decode them on every fold over the references,
-- ie on every 'insertBody' and every eviction.
data PackedEndorserBlock = MkPackedEndorserBlock
  { pebBytes :: !SBS.ShortByteString
  -- ^ the bytes the endorser block was received in
  , pebReferences :: !SBS.ShortByteString
  -- ^ each tx's hash, then its size in four big-endian bytes
  }

packEndorserBlock :: EndorserBlock -> PackedEndorserBlock
packEndorserBlock eb =
  MkPackedEndorserBlock
    { pebBytes = endorserBlockShortBytes eb
    , pebReferences =
        mconcat
          [ hashToBytesShort txHash <> SBS.pack [fromIntegral (size `shiftR` (8 * i)) | i <- [3, 2, 1, 0]]
          | (txHash, size) <- endorserBlockReferences eb
          ]
    }

instance ReferencesTxsByHash PackedEndorserBlock where
  foldTxReferences f z MkPackedEndorserBlock{pebReferences = sbs} = go z 0
   where
    go !acc off
      | off + entryBytes > SBS.length sbs = acc
      | otherwise =
          go
            (f acc (UnsafeHash (SBS.take 32 (SBS.drop off sbs))) (sizeAt (off + 32)))
            (off + entryBytes)
    sizeAt off =
      foldl (\w i -> w `shiftL` 8 .|. fromIntegral (SBS.index sbs (off + i))) 0 [0 .. 3]
    entryBytes = 36

-- | The maximum number of points the cache retains, each one at which a body was
-- taken in. Retaining one more evicts the oldest, cascading through the body
-- and tx refcounts.
maxLeiosPointCount :: Int
maxLeiosPointCount = 128 -- TODO magic number

-- | An index into the source-EB ring the tx-location machinery keeps (the last
-- 'maxLeiosPointCount' points whose txs became durable). Meant to hold a value in
-- @[0, 'maxLeiosPointCount')@, so it fits the packed location word's ring field
-- and can never key the ring out of bounds. 'mkEbRingIndex' is the checked way in;
-- 'UnsafeEbRingIndex' is the escape hatch for a caller that maintains the bound
-- itself, e.g. after reducing a monotone claim counter mod the ring size.
newtype EbRingIndex = UnsafeEbRingIndex Int
  deriving (Eq, Ord, Show)

-- | Check an 'Int' against the ring's bounds: 'Just' iff it lies in
-- @[0, 'maxLeiosPointCount')@. Use for an index read back from packed or stored
-- data, where out of range means corruption and the caller should decline the
-- location rather than key the ring wrongly.
mkEbRingIndex :: Int -> Maybe EbRingIndex
mkEbRingIndex n
  | n >= 0 && n < maxLeiosPointCount = Just (UnsafeEbRingIndex n)
  | otherwise = Nothing

-- | The raw index, for packing into the location word.
unEbRingIndex :: EbRingIndex -> Int
unEbRingIndex (UnsafeEbRingIndex i) = i

-- | A reference count.
--
-- INVARIANT: @> 0@ (an entry at zero is removed rather than stored).
newtype RefCount = MkRefCount Word8
  deriving (Eq, Show)

-- | A body the cache holds, with the number of retained points it was taken in
-- at.
data HeldBody b = MkHeldBody {-# UNPACK #-} !RefCount !b

-- | A tx's state in the cache /before/ an unapplied insert, surfaced by the
-- 'withLockedInsertUnappliedTx' step so a caller can classify an arriving tx.
data TxArrivalPrior
  = -- | Untracked: no held body references it (assumed present once, since evicted).
    TxWasUntracked
  | -- | Referenced by a held body but not yet acquired (the expected case).
    TxWasNotYetInserted
  | -- | Already acquired (inserted or validated), so this arrival is redundant.
    TxWasAlreadyHeld
  deriving (Eq, Show)

-- | Bucket an arriving tx's bytes by its prior state (for 'FetchArrivalBytes').
bucketTxArrival :: TxArrivalPrior -> BytesSize -> FetchArrivalBytes
bucketTxArrival = \case
  TxWasUntracked -> fetchArrivalEvicted
  TxWasNotYetInserted -> fetchArrivalGood
  TxWasAlreadyHeld -> fetchArrivalExtra

-- | The worst-case number of txs the cache can be asked to hold: a full
-- 'maxLeiosPointCount' window of points, each of its own EB referencing the maximum
-- 'maxTxsPerEndorserBlock' distinct txs.
worstCaseCacheTxCount :: Int
worstCaseCacheTxCount = maxLeiosPointCount * maxTxsPerEndorserBlock

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
