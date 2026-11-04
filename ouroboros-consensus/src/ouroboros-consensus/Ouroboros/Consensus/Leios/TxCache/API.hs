{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | The LeiosTxCache interface: the handle type and the small set of types
-- shared by every implementation. See "Ouroboros.Consensus.Leios.TxCache" for
-- the overview.
module Ouroboros.Consensus.Leios.TxCache.API
  ( -- * Handle
    LeiosTxCache (..)
  , evictOlderThan
  , insertBody

    -- * Body payloads
  , ReferencesTxsByHash (..)
  , PackedEndorserBlock
  , packEndorserBlock

    -- * Tx states
  , TxState (..)
  , LandedTx (..)

    -- * Bounds
  , maxLeiosPointCount

    -- * Insert-body observability summary
  , LeiosTxCacheInsertBodySummary (..)
  , defaultLeiosTxCacheShift
  , worstCaseCacheTxCount

    -- * Arrival classification
  , TxArrivalPrior (..)
  , bucketTxArrival
  ) where

import Cardano.Crypto.Hash (Hash (UnsafeHash), hashToBytesShort)
import Cardano.Slotting.Slot (SlotNo)
import qualified Data.ByteString.Short as SBS
import Data.Maybe.Strict (StrictMaybe)
import Data.Set (Set)
import qualified Data.Vector.Unboxed as VU
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , LeiosPoint
  , TxHash
  , TxLocation
  , TxOffset
  , endorserBlockReferences
  , maxTxsPerEndorserBlock
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
  ( FetchArrivalBytes
  , LeiosTxCacheInsertBodySummary (..)
  , fetchArrivalEvicted
  , fetchArrivalExtra
  , fetchArrivalGood
  )

-- | The maximum number of points the cache retains, each one at which a body was
-- taken in. Retaining one more evicts the oldest.
--
-- It is chosen to be:
--
-- * small enough that the cache's RAM cost stays tolerable even in the worst
--   case, where every retained point's EB references 'maxTxsPerEndorserBlock'
--   (about 17 thousand) distinct txs: that 'worstCaseCacheTxCount' (about 2.2
--   million) fills the capacity the node's cache is sized for
--   ('defaultLeiosTxCacheShift') to a load factor of only about half; and
--
-- * large enough that a "significant" portion of stake is likely to be elected
--   again before its previous EB has been evicted from a recipient node's
--   LeiosTxCache.
maxLeiosPointCount :: Int
maxLeiosPointCount = 128

-- | The worst-case number of txs the cache can be asked to hold: a full
-- 'maxLeiosPointCount' window of points, each of its own EB referencing the maximum
-- 'maxTxsPerEndorserBlock' distinct txs.
worstCaseCacheTxCount :: Int
worstCaseCacheTxCount = maxLeiosPointCount * maxTxsPerEndorserBlock

-- | The table shift the node allocates its Leios tx cache with: @2 ^@ this
-- many slots. The pure reference handle reports the same capacity as
-- 'ibsCacheCapacity', so the load means one thing across both handles. It
-- covers 'worstCaseCacheTxCount' about twice over.
--
-- The latency profile of accessing the hash table is part of the overall
-- diffusion correctness, and that begins to degrade when the load factor
-- exceeds ~50%. So that degradation is already a limiting factor even before it
-- risks a total failure (at >100%). Keep this in mind before lowering this or
-- raising 'maxTxsPerEndorserBlock'.
defaultLeiosTxCacheShift :: Int
defaultLeiosTxCacheShift = 22

-- | A handle on the LeiosTxCache. Its operations are atomic with respect to
-- each other: each holds the handle's lock while it runs, including any
-- continuation it is given.
--
-- The cache holds a tx for as long as it retains a point that the tx landed at
-- ('insertLandedTxs'), or whose body it took in while holding the tx
-- ('insertBodyAndFoldl''). It evicts points oldest first, so it tracks only
-- each tx's youngest such point, its youngest reference, and forgets the tx
-- when it evicts that point. Evicting a point also forgets the locations at it.
data LeiosTxCache m a v b = LeiosTxCache
  { evictOlderThanFoldl' ::
      forall w.
      SlotNo ->
      w ->
      (w -> TxHash -> w) ->
      m (Set LeiosPoint, w)
  -- ^ Evict every retained point whose slot is strictly older than the given
  -- boundary; returns the points it evicted. The tip-driven eviction entrypoint
  -- (see "Ouroboros.Consensus.Leios.TxCache"), complementing the count-driven
  -- eviction that 'insertBodyAndFoldl'' performs. Also records the boundary,
  -- after which 'insertBodyAndFoldl'' retains no point that old.
  --
  -- Pruning this cache before the LeiosDb is pruned to the same slot keeps its
  -- tx locations from naming rows the LeiosDb has deleted, though such a
  -- location would only cost a fetch; see "Ouroboros.Consensus.Leios.TxCache".
  --
  -- Also fold a caller-supplied accumulator over the txs it forgets. It walks
  -- the evicted points oldest first (least slot, then least EB hash), each body
  -- from its first tx to its last, and visits each forgotten tx once: at its
  -- first offset in the body of its youngest reference. It is a strict left
  -- fold: it evaluates each accumulator, the final one included, to WHNF before
  -- it returns.
  , insertBodyAndFoldl' ::
      forall w.
      LeiosPoint ->
      b ->
      w ->
      (w -> TxOffset -> TxHash -> BytesSize -> Maybe TxLocation -> w) ->
      m (Maybe LeiosTxCacheInsertBodySummary, w)
  -- ^ Take in the body of the EB at this point: retain the point with its body,
  -- make it the youngest reference of each tx the body references that the
  -- cache holds, if it is younger, and summarise that. A point of an EB the
  -- cache already holds at another point is taken in like any other. Nothing
  -- changes, and there is no summary, when the point is already retained, when
  -- it is strictly older than the latest 'evictOlderThanFoldl'' boundary, or
  -- when 'maxLeiosPointCount' points are retained and it is older than all of
  -- them. Otherwise, when 'maxLeiosPointCount' points are retained, it first
  -- evicts the oldest (least slot, then least EB hash).
  --
  -- Either way, fold a caller-supplied accumulator over /every/ referenced tx:
  -- starting from the nil @w@ and extending it with the snoc @w -> offset ->
  -- 'TxHash' -> 'BytesSize' -> 'Maybe' 'TxLocation' -> w@, where @offset@ is the
  -- tx's position in the body and the location is 'Just' iff the tx landed at
  -- some retained point ('insertLandedTxs'), i.e. it can serve a cross-EB
  -- fill. The fold lets the caller build what it needs without a list of every
  -- referenced tx in between. It is a strict left fold: it evaluates each
  -- accumulator, the final one included, to WHNF before it returns.
  , insertLandedTxs :: LeiosPoint -> [LandedTx a v] -> m FetchArrivalBytes
  -- ^ Record that the given point now holds these txs, each the one its body
  -- lists at the tx's offset, with its state. Call this once the write lands.
  -- A no-op if the cache does not (still) retain the point.
  --
  -- A tx the cache does not hold is recorded with this point as its youngest
  -- reference and its location. For one it holds, this point becomes its
  -- youngest reference if it is younger than the tx's existing youngest
  -- reference, and its location if it is younger than the tx's existing
  -- location or the tx has none (younger: greater slot, then greater EB hash;
  -- the cache evicts it last). Either way, the tx is recorded as validated if
  -- it already has been, or if this says so.
  --
  -- Has exclusive write-access. Each tx's 'ltSize' is only used to accumulate
  -- the 'FetchArrivalBytes', which is only used for observability.
  --
  -- MAY be called repeatedly for one point: its closure arrives over many
  -- 'MsgLeiosBlockTxs'.
  , withLookupTx ::
      forall r.
      (forall n. Monad n => (TxHash -> n (StrictMaybe (TxState a v))) -> n r) ->
      m r
  -- ^ Run the continuation with a lookup of the state of each tx the cache
  -- holds.
  --
  -- NOTE: The continuation's type is polymorphic in its monad so that it cannot
  -- call any operation of this handle, this one included: this runs it while
  -- holding the lock, so any such call would deadlock.
  }

-- | 'evictOlderThanFoldl'', for a caller that needs nothing back.
evictOlderThan :: Functor m => LeiosTxCache m a v b -> SlotNo -> m ()
evictOlderThan cache boundary =
  () <$ evictOlderThanFoldl' cache boundary () (\() _txh -> ())

-- | 'insertBodyAndFoldl'', for a caller that needs only the summary.
insertBody ::
  Functor m =>
  LeiosTxCache m a v b ->
  LeiosPoint ->
  b ->
  m (Maybe LeiosTxCacheInsertBodySummary)
insertBody cache point b =
  fst <$> insertBodyAndFoldl' cache point b () (\() _off _txh _sz _loc -> ())

-- | A body @b@ from which the referenced txs can be enumerated, each paired with
-- its on-the-wire size, in body order.
--
-- Visiting in body order lets a consumer recover each tx's offset from its
-- position in the fold.
class ReferencesTxsByHash b where
  foldTxReferences :: (r -> TxHash -> BytesSize -> r) -> r -> b -> r

-- | The node's body payload: each referenced tx's hash and size, in body order,
-- the hashes in one byte string and the sizes in an unboxed vector, at 36 bytes
-- per tx, about as much as the body's own encoding takes. The cache retains up
-- to 'maxLeiosPointCount' (128) of them, of up to
-- 'Ouroboros.Consensus.Leios.Types.maxEndorserBlockBytesSize' (600 kB) each, so
-- about 77 MB at worst; a decoded 'EndorserBlock' would cost several times
-- more.
data PackedEndorserBlock
  = MkPackedEndorserBlock !SBS.ShortByteString !(VU.Vector BytesSize)

packEndorserBlock :: EndorserBlock -> PackedEndorserBlock
packEndorserBlock eb =
  MkPackedEndorserBlock
    (mconcat (map (hashToBytesShort . fst) refs))
    (VU.fromList (map snd refs))
 where
  refs = endorserBlockReferences eb

instance ReferencesTxsByHash PackedEndorserBlock where
  foldTxReferences f z (MkPackedEndorserBlock hashes sizes) =
    VU.ifoldl' (\acc i sz -> f acc (hashAt i) sz) z sizes
   where
    hashAt i = UnsafeHash (SBS.take 32 (SBS.drop (32 * i) hashes))

-- | Whether a tx the cache holds was applied, and what it records of the tx.
data TxState a v
  = -- | The tx is inserted (fetched) but not yet applied, neither by the
    -- Mempool nor the LeiosVoting thread.
    TxInserted !a
  | -- | The tx has been applied.
    TxValidated !v
  deriving (Eq, Show)

-- | A tx a write landed at a point.
data LandedTx a v = MkLandedTx
  { ltOffset :: !TxOffset
  -- ^ the offset at which the point's body lists it
  , ltHash :: !TxHash
  -- ^ its hash
  , ltSize :: !BytesSize
  -- ^ its size
  , ltState :: !(TxState a v)
  -- ^ its state
  }

-- | What the cache held of a tx before it landed, by which 'insertLandedTxs'
-- classifies the tx's bytes ('bucketTxArrival').
data TxArrivalPrior
  = -- | The cache does not retain the point the tx landed at: it has evicted
    -- that point, or ignored its body as older than what it retains.
    TxEvicted
  | -- | The cache did not hold the tx (the expected case).
    TxWasAbsent
  | -- | The cache already held the tx, so this arrival is redundant.
    TxWasHeld
  deriving (Eq, Show)

-- | Bucket an arriving tx's bytes by its prior state (for 'FetchArrivalBytes').
bucketTxArrival :: TxArrivalPrior -> BytesSize -> FetchArrivalBytes
bucketTxArrival = \case
  TxEvicted -> fetchArrivalEvicted
  TxWasAbsent -> fetchArrivalGood
  TxWasHeld -> fetchArrivalExtra
