-- | Salted, packed mutable-table cache. Masked bounded memory updates preserve
-- the donor's interruption fix: restoring the MVar alone cannot roll back arrays.
module Ouroboros.Consensus.Leios.TxCache.Optimized (newOptimizedCache) where

import Cardano.Crypto.Hash (hashToBytesShort)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString.Short as SBS
import Data.Word (Word64)
import Ouroboros.Consensus.Leios.TxCache.API
import Ouroboros.Consensus.Leios.TxCache.Internal
import qualified Ouroboros.Consensus.Leios.TxCache.Optimized.MutableHashTable as HT
import Ouroboros.Consensus.Leios.Types (TxHash)
import Ouroboros.Consensus.Util.IOLike

newOptimizedCache :: IOLike m => CacheConfig -> Word64 -> Word64 -> m (LeiosTxCache m)
newOptimizedCache cfg k0 k1 = do
  either (error . ("TxCache: " ++)) pure (validateCacheConfig cfg)
  ht <- HT.new (cacheTableShift cfg) k0 k1
  newCache
    cfg
    TxTable
      { tableLookup = HT.lookup ht . toKey
      , tableInsert = HT.insert ht . toKey
      , tableDelete = \h -> HT.delete ht (toKey h) >> pure ()
      , tableSize = HT.size ht
      , tableArrayBytes = fromIntegral (HT.capacity ht) * 40 + fromIntegral (HT.capacity ht `div` 8)
      }

-- Checked public hash conversion, independent of cardano-base's internal layout.
toKey :: TxHash -> HT.Key
toKey h = HT.Key (wordAt 0) (wordAt 8) (wordAt 16) (wordAt 24)
 where
  bytes = hashToBytesShort h
  wordAt off = foldl (\acc i -> (acc `shiftL` 8) .|. fromIntegral (SBS.index bytes (off + i))) 0 [0 .. 7]
