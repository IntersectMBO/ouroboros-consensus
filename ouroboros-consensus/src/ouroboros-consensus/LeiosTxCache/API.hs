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
  ) where

import Cardano.Slotting.Slot (SlotNo)
import Codec.CBOR.Read (deserialiseFromBytes)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Short (fromShort)
import Data.Set (Set)
import qualified Data.Vector.Strict as V
import Data.Word (Word8)
import LeiosDemoTypes
  ( EbHash
  , InsertBodySummary (..)
  , RbHash
  , SerializedEbBody (..)
  , TxHash
  , decodeLeiosEb
  , leiosEbTxs
  , maxTxsPerEb
  )

-- | A monadic tx-cache handle: the pure index operations, each performing its
-- state update in @m@.
data LeiosTxCache m a v b = LeiosTxCache
  { insertAnnouncement :: SlotNo -> RbHash -> EbHash -> m (Set EbHash, Set TxHash)
  -- ^ Insert an announcement; returns the bodies and txs it evicted, if any.
  , evictOlderThan :: SlotNo -> m (Set EbHash, Set TxHash)
  -- ^ Evict every retained announcement whose slot is strictly older than the
  -- given boundary; returns the bodies and txs it evicted, if any. The tip-driven
  -- eviction entrypoint (see "LeiosTxCache"), complementing the count-driven
  -- eviction that 'insertAnnouncement' performs.
  , insertBody :: EbHash -> b -> m (Maybe InsertBodySummary)
  , withLockedInsertUnappliedTx :: (forall w. w -> (w -> TxHash -> a -> m w) -> m w) -> m ()
  -- ^ Has exclusive write-access
  , withLockedInsertAppliedTx :: (forall w. w -> (w -> TxHash -> v -> m w) -> m w) -> m ()
  -- ^ Has exclusive write-access
  , withLookupTx :: forall r. ((TxHash -> m (Maybe (Either a v))) -> m r) -> m r
  -- ^ Does not not hold the lock
  }

-- | A body @b@ from which the referenced txs can be enumerated by hash.
--
-- The fold must visit each referenced 'TxHash' at most once per body (a valid EB
-- body references a tx at most once), so that a body contributes exactly one to
-- each of its txs' refcounts.
class ReferencesTxsByHash b where
  foldTxReferences :: (r -> TxHash -> r) -> r -> b -> r

-- | The production body type: 'SerializedEbBody' is decoded to enumerate its
-- referenced txs. (The type lives in "LeiosDemoTypes"; the instance lives here,
-- with the class, to keep it non-orphan.)
instance ReferencesTxsByHash SerializedEbBody where
  foldTxReferences f z (MkSerializedEbBody sbs) =
    V.foldl' (\acc (txh, _sz) -> f acc txh) z (leiosEbTxs eb)
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
