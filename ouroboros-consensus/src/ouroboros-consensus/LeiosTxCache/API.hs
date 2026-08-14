{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

-- | The LeiosTxCache interface: the handle type and the small set of types
-- shared by every implementation. See "LeiosTxCache" for the overview and the
-- backing-store design note.
module LeiosTxCache.API
  ( -- * Handle
    LeiosTxCache (..)

    -- * Body payloads
  , ReferencesTxsByHash (..)

    -- * Shared refcount \/ body state
  , RefCount (..)
  , BodyState (..)
  , maxAnnouncementCount

    -- * Insert-body observability summary
  , InsertBodySummary (..)
  , mkInsertBodySummary
  , worstCaseCacheTxCount

    -- * Arrival classification
  , TxArrivalPrior (..)
  , bucketTxArrival
  ) where

import Cardano.Slotting.Slot (SlotNo)
import Codec.CBOR.Read (deserialiseFromBytes)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Short (fromShort)
import Data.Set (Set)
import qualified Data.Vector.Strict as V
import Data.Word (Word8)
import LeiosDemoTypes
  ( BytesSize
  , EbHash
  , FetchArrivalBytes
  , InsertBodySummary (..)
  , RbHash
  , SerializedEbBody (..)
  , TxHash
  , decodeLeiosEb
  , fetchArrivalEvicted
  , fetchArrivalExtra
  , fetchArrivalGood
  , leiosEbTxs
  , maxTxsPerEb
  )

-- | A monadic tx-cache handle: the pure index operations, each performing its
-- state update in @m@.
data LeiosTxCache m a v b = LeiosTxCache
  { insertAnnouncement :: SlotNo -> RbHash -> EbHash -> m (Set EbHash, Set TxHash)
  -- ^ Insert an announcement; returns the bodies and txs it evicted, if any. A
  -- no-op for an EB whose slot is strictly older than the latest 'evictOlderThan'
  -- boundary — the cache has already been pruned past it.
  , evictOlderThan :: SlotNo -> m (Set EbHash, Set TxHash)
  -- ^ Evict every retained announcement whose slot is strictly older than the
  -- given boundary; returns the bodies and txs it evicted, if any. The tip-driven
  -- eviction entrypoint (see "LeiosTxCache"), complementing the count-driven
  -- eviction that 'insertAnnouncement' performs. Also records the boundary, after
  -- which 'insertAnnouncement' ignores any EB that old.
  --
  -- ORDERING CONTRACT — MUST HOLD: prune this in-memory cache to a slot @X@
  -- /strictly before/ the LeiosDb is pruned to that same @X@ — never after, never
  -- concurrently. This cache is only an index of the LeiosDb, so evicting from the
  -- index first is what guarantees it can never report a hit for a tx the LeiosDb
  -- has already dropped. Reverse the order and you arm the hit-prune hazard: a
  -- false hit ⇒ a skipped fetch ⇒ a silently-incomplete EB closure.
  , insertBody ::
      forall w.
      EbHash ->
      b ->
      w ->
      (w -> Int -> TxHash -> BytesSize -> w) ->
      m (Maybe (InsertBodySummary, w))
  -- ^ Record that we hold this EB's body, bumping the refcount of each tx it
  -- references. In the same pass, fold a caller-supplied accumulator over the
  -- referenced txs that are /not yet acquired/ (the "misses"): starting from the
  -- nil @w@ and extending it with the snoc @w -> offset -> 'TxHash' -> 'BytesSize'
  -- -> w@, where @offset@ is the tx's position in the body. Returns the summary
  -- and the built @w@, or 'Nothing' when the EB is unannounced or its body is
  -- already inserted (a no-op, so nothing is folded).
  , lookupBody :: EbHash -> m (Maybe b)
  -- ^ The EB's body if we hold it (its 'BodyState' is 'BodyAlreadyInserted');
  -- 'Nothing' if the EB is untracked or only announced. Unlike a tx, an EB body
  -- pins itself: a hit means it is in the LeiosDb and stays there until the EB is
  -- pruned, so no cross-object reasoning is needed.
  , withLockedInsertUnappliedTx ::
      (forall w. w -> (w -> TxHash -> BytesSize -> a -> m w) -> m w) -> m FetchArrivalBytes
  -- ^ Has exclusive write-access
  --
  -- The 'BytesSize' argument is only used to accumulate the
  -- 'FetchArrivalBytes', which is only used for observability.
  , withLockedInsertAppliedTx :: (forall w. w -> (w -> TxHash -> v -> m w) -> m w) -> m ()
  -- ^ Has exclusive write-access
  , withLookupTx :: forall r. ((TxHash -> m (Maybe (Either a v))) -> m r) -> m r
  -- ^ Does not not hold the lock
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

-- | The production body type: 'SerializedEbBody' is decoded to enumerate its
-- referenced txs. (The type lives in "LeiosDemoTypes"; the instance lives here,
-- with the class, to keep it non-orphan.)
instance ReferencesTxsByHash SerializedEbBody where
  foldTxReferences f z (MkSerializedEbBody sbs) =
    V.foldl' (\acc (txh, sz) -> f acc txh sz) z (leiosEbTxs eb)
   where
    eb = case deserialiseFromBytes decodeLeiosEb (LBS.fromStrict (fromShort sbs)) of
      Right (_leftover, decoded) -> decoded
      Left err -> error $ "SerializedEbBody: undecodable: " <> show err

-- | The maximum number of EB announcements retained. Inserting past it evicts
-- the oldest, cascading through the body and tx refcounts.
maxAnnouncementCount :: Int
maxAnnouncementCount = 128 -- TODO magic number

-- | A reference count.
--
-- INVARIANT: @> 0@ (an entry at zero is removed rather than stored).
newtype RefCount = MkRefCount Word8
  deriving (Eq, Show)

data BodyState b
  = -- | An announcement of this EB has been inserted, but not its body.
    BodyNotYetInserted {-# UNPACK #-} !RefCount
  | BodyAlreadyInserted {-# UNPACK #-} !RefCount !b

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

-- | The worst-case number of txs the cache can hold: a full 'maxAnnouncementCount'
-- window of EBs, each referencing the maximum 'maxTxsPerEb' distinct txs. The fixed
-- denominator for the cache's load factor.
worstCaseCacheTxCount :: Int
worstCaseCacheTxCount = maxAnnouncementCount * maxTxsPerEb

-- | Build an 'InsertBodySummary' from the raw counts, computing the load factor
-- ('ibsCacheLoad') against 'worstCaseCacheTxCount'.
mkInsertBodySummary :: Int -> Int -> Int -> Int -> Int -> InsertBodySummary
mkInsertBodySummary txsInEb tracked acquired validated cacheTxCount =
  InsertBodySummary
    { ibsTxsInEb = txsInEb
    , ibsTracked = tracked
    , ibsAcquired = acquired
    , ibsValidated = validated
    , ibsCacheTxCount = cacheTxCount
    , ibsCacheLoad = fromIntegral cacheTxCount / fromIntegral worstCaseCacheTxCount
    }
