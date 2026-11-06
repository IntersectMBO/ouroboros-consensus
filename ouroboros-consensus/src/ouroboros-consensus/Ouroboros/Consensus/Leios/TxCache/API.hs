{-# LANGUAGE BangPatterns #-}

-- | Bounded transaction-location discovery, separate from durable acquisition.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#deduplication-and-durable-acquisition@.
module Ouroboros.Consensus.Leios.TxCache.API
  ( LeiosTxCache (..)
  , CacheConfig (..)
  , defaultCacheConfig
  , validateCacheConfig
  , CacheEpoch
  , CacheStats (..)
  , PackedReferences
  , packReferences
  , foldReferences
  ) where

import Cardano.Crypto.Hash (hashFromBytesShort, hashToBytesShort)
import Cardano.Slotting.Slot (SlotNo)
import Data.Bits (finiteBitSize, shiftL, shiftR, (.|.))
import qualified Data.ByteString.Short as SBS
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.Util.IOLike (STM)

-- | Runtime sizing. Bounds must fit both the source-ring packing and table.
data CacheConfig = CacheConfig
  { cacheAnnouncements :: !Int
  -- ^ Maximum retained accepted announcements; at most 128 ring slots.
  , cacheTableShift :: !Int
  -- ^ Allocate @2^shift@ salted hash-table entries.
  }
  deriving (Eq, Show)

-- | 160.5 MiB of table arrays, excluding compact references and metadata.
defaultCacheConfig :: CacheConfig
defaultCacheConfig = CacheConfig 128 22

-- | Reject arithmetic overflow and configurations unable to hold the trusted
-- maximum references, leaving a free slot for probing/deletion termination.
validateCacheConfig :: CacheConfig -> Either String ()
validateCacheConfig (CacheConfig count shift)
  | count < 1 || count > 128 = Left "cache announcement count must be in [1,128]"
  | shift < 6 || shift >= finiteBitSize (0 :: Int) - 6 = Left "invalid cache allocation shift"
  | toInteger count * toInteger maxTxsPerEndorserBlock >= (2 :: Integer) ^ shift =
      Left "cache table cannot hold the announcement reference bound"
  | maxTxsPerEndorserBlock > 0x1000000 = Left "cache TX offset exceeds 24 bits"
  | otherwise = Right ()

-- | Local source revision; never sent over the wire or used as an acquisition ID.
type CacheEpoch = Natural

-- | Compact hash/size references only, without retaining original body bytes.
newtype PackedReferences = PackedReferences SBS.ShortByteString
  deriving (Eq, Show)

packReferences :: EndorserBlock -> PackedReferences
packReferences body =
  PackedReferences $
    SBS.concat
      [ hashToBytesShort h <> SBS.pack [fromIntegral (n `shiftR` (8 * i)) | i <- [3, 2, 1, 0]]
      | (h, n) <- endorserBlockReferences body
      ]

-- | Strict bounded traversal. Hash reconstruction uses the checked public API.
foldReferences :: (a -> TxOffset -> TxHash -> BytesSize -> a) -> a -> PackedReferences -> a
foldReferences f z (PackedReferences bytes) = go z 0
 where
  go !acc off
    | off >= SBS.length bytes = acc
    | otherwise = case hashFromBytesShort (SBS.pack [SBS.index bytes (off + i) | i <- [0 .. 31]]) of
        Nothing -> error "TxCache: invalid packed hash"
        Just h -> go (f acc (off `div` 36) h (sizeAt (off + 32))) (off + 36)
  sizeAt off = foldl (\w i -> (w `shiftL` 8) .|. fromIntegral (SBS.index bytes (off + i))) 0 [0 .. 3]

-- | Bounded observability, without payloads or attempt history.
data CacheStats = CacheStats
  { cacheAnnouncementCount :: !Int
  , cacheBodyCount :: !Int
  , cacheTransactionCount :: !Int
  , cacheArrayBytes :: !Word64
  }
  deriving (Eq, Show)

-- | Memory-only location service. Hits are hints, never acquisition receipts.
-- Each location snapshot is paired with the epoch observed under the same lock.
data LeiosTxCache m = LeiosTxCache
  { recordAnnouncement :: SlotNo -> RankingBlockHash -> EndorserBlockHash -> m ()
  -- ^ Record an accepted announcement, ignoring duplicates and pruned slots.
  , trackDurableReferences :: EndorserBlockHash -> PackedReferences -> m ()
  -- ^ Retain references only for a tracked EB after body commit.
  , lookupLocations :: [(TxOffset, TxHash)] -> m (CacheEpoch, [(TxOffset, Maybe TxLocation)])
  -- ^ Bounded batch lookup; offsets identify the caller's target positions.
  , publishConfirmed :: LeiosPoint -> [(TxOffset, TxHash)] -> m ()
  -- ^ Publish confirmed source rows only, after the real storage receipt.
  , invalidateLocationIfMatches :: TxHash -> TxLocation -> m ()
  -- ^ Remove an obsolete hint without erasing a newer source.
  , pruneTo :: SlotNo -> m ()
  -- ^ Advance the monotone frontier before forwarding database GC.
  , cacheSourceEpoch :: STM m CacheEpoch
  -- ^ Source-change wake predicate; no cache MVar acquisition in STM.
  , readCacheStats :: m CacheStats
  }
