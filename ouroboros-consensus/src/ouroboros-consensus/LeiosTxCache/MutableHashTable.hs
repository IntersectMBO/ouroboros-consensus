{-# LANGUAGE BangPatterns #-}

-- | A mutable, salted open-addressing hash table for the Leios tx cache: linear
-- probing with backward-shift (tombstone-free) deletion, a salted SipHash-2-4,
-- and a separate occupancy bitset — a Haskell port of the reference
-- @hash_table.c@, mapping a 32-byte key to a 'Word64' value.
--
-- The backing store is a 'MutableByteArray' (via "Data.Primitive"): GHC-owned,
-- so its lifetime is GC-managed and its footprint shows up in the RTS stats, but
-- as a large byte object the collector never traces into it or copies it —
-- contiguous and off to the side, just accounted for. Entries are five
-- 'Word64's each (four key words + one value); occupancy is one bit per slot.
--
-- The key is four 'Word64's ('Key'), so there is no per-key heap object and both
-- hashing and equality read the words directly.
module LeiosTxCache.MutableHashTable
  ( MutableHashTable
  , Key (..)
  , new
  , insert
  , lookup
  , delete
  , size
  , capacity
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits
import Data.Primitive.ByteArray
  ( MutableByteArray
  , fillByteArray
  , newByteArray
  , readByteArray
  , writeByteArray
  )
import Data.Primitive.MutVar (MutVar, modifyMutVar', newMutVar, readMutVar)
import Data.Word (Word64)
import Prelude hiding (lookup)

-- | A 32-byte key as four 'Word64's.
data Key = Key !Word64 !Word64 !Word64 !Word64
  deriving (Eq, Ord, Show)

data MutableHashTable s = MutableHashTable
  { mhtCap :: !Int
  -- ^ capacity, a power of two
  , mhtMask :: !Int
  -- ^ @mhtCap - 1@
  , mhtEntries :: !(MutableByteArray s)
  -- ^ @mhtCap * 5@ words: entry @i@ is words @[i*5 .. i*5+3]@ (key) then @i*5+4@ (value)
  , mhtOccupied :: !(MutableByteArray s)
  -- ^ @mhtCap \`div\` 64@ words: one bit per slot
  , mhtK0 :: !Word64
  , mhtK1 :: !Word64
  , mhtSize :: !(MutVar s Int)
  }

-- | Allocate a table of @2 ^ nshift@ slots (@nshift >= 6@) with the given 128-bit
-- salt. Feed a securely-random salt: keys are adversarial.
new :: PrimMonad m => Int -> Word64 -> Word64 -> m (MutableHashTable (PrimState m))
new nshift k0 k1
  | nshift < 6 = error "MutableHashTable.new: nshift must be >= 6"
  | otherwise = do
      let cap = 1 `unsafeShiftL` nshift
          occWords = cap `div` 64
      entries <- newByteArray (cap * 5 * 8)
      occupied <- newByteArray (occWords * 8)
      fillByteArray occupied 0 (occWords * 8) 0
      szRef <- newMutVar 0
      pure
        MutableHashTable
          { mhtCap = cap
          , mhtMask = cap - 1
          , mhtEntries = entries
          , mhtOccupied = occupied
          , mhtK0 = k0
          , mhtK1 = k1
          , mhtSize = szRef
          }

capacity :: MutableHashTable s -> Int
capacity = mhtCap

size :: PrimMonad m => MutableHashTable (PrimState m) -> m Int
size = readMutVar . mhtSize

{-------------------------------------------------------------------------------
  Occupancy bitset
-------------------------------------------------------------------------------}

isOccupied :: PrimMonad m => MutableHashTable (PrimState m) -> Int -> m Bool
isOccupied ht i = do
  w <- readByteArray (mhtOccupied ht) (i `unsafeShiftR` 6)
  pure $ testBit (w :: Word64) (i .&. 63)

setOccupied :: PrimMonad m => MutableHashTable (PrimState m) -> Int -> m ()
setOccupied ht i = do
  let j = i `unsafeShiftR` 6
  w <- readByteArray (mhtOccupied ht) j
  writeByteArray (mhtOccupied ht) j (setBit (w :: Word64) (i .&. 63))

clearOccupied :: PrimMonad m => MutableHashTable (PrimState m) -> Int -> m ()
clearOccupied ht i = do
  let j = i `unsafeShiftR` 6
  w <- readByteArray (mhtOccupied ht) j
  writeByteArray (mhtOccupied ht) j (clearBit (w :: Word64) (i .&. 63))

{-------------------------------------------------------------------------------
  Entry access
-------------------------------------------------------------------------------}

readKey :: PrimMonad m => MutableHashTable (PrimState m) -> Int -> m Key
readKey ht i = do
  let b = i * 5
  Key
    <$> readByteArray (mhtEntries ht) b
    <*> readByteArray (mhtEntries ht) (b + 1)
    <*> readByteArray (mhtEntries ht) (b + 2)
    <*> readByteArray (mhtEntries ht) (b + 3)

writeKey :: PrimMonad m => MutableHashTable (PrimState m) -> Int -> Key -> m ()
writeKey ht i (Key a b c d) = do
  let o = i * 5
  writeByteArray (mhtEntries ht) o a
  writeByteArray (mhtEntries ht) (o + 1) b
  writeByteArray (mhtEntries ht) (o + 2) c
  writeByteArray (mhtEntries ht) (o + 3) d

readVal :: PrimMonad m => MutableHashTable (PrimState m) -> Int -> m Word64
readVal ht i = readByteArray (mhtEntries ht) (i * 5 + 4)

writeVal :: PrimMonad m => MutableHashTable (PrimState m) -> Int -> Word64 -> m ()
writeVal ht i = writeByteArray (mhtEntries ht) (i * 5 + 4)

{-------------------------------------------------------------------------------
  SipHash-2-4, unrolled for a 32-byte (4-word) key
-------------------------------------------------------------------------------}

rotl :: Word64 -> Int -> Word64
rotl x b = (x `unsafeShiftL` b) .|. (x `unsafeShiftR` (64 - b))

sipround :: Word64 -> Word64 -> Word64 -> Word64 -> (Word64, Word64, Word64, Word64)
sipround v0 v1 v2 v3 =
  let v0a = v0 + v1
      v1a = rotl v1 13 `xor` v0a
      v0b = rotl v0a 32
      v2a = v2 + v3
      v3a = rotl v3 16 `xor` v2a
      v0c = v0b + v3a
      v3b = rotl v3a 21 `xor` v0c
      v2b = v2a + v1a
      v1b = rotl v1a 17 `xor` v2b
      v2c = rotl v2b 32
   in (v0c, v1b, v2c, v3b)

-- | Absorb one message word: @v3 ^= m; SIPROUND; SIPROUND; v0 ^= m@.
compress :: Word64 -> (Word64, Word64, Word64, Word64) -> (Word64, Word64, Word64, Word64)
compress m (v0, v1, v2, v3) =
  let (a0, a1, a2, a3) = sipround v0 v1 v2 (v3 `xor` m)
      (b0, b1, b2, b3) = sipround a0 a1 a2 a3
   in (b0 `xor` m, b1, b2, b3)

hashKey :: MutableHashTable s -> Key -> Int
hashKey ht (Key m0 m1 m2 m3) =
  fromIntegral (folded .&. fromIntegral (mhtMask ht))
 where
  k0 = mhtK0 ht
  k1 = mhtK1 ht
  s0 =
    ( 0x736f6d6570736575 `xor` k0
    , 0x646f72616e646f6d `xor` k1
    , 0x6c7967656e657261 `xor` k0
    , 0x7465646279746573 `xor` k1
    )
  absorbed = compress m3 (compress m2 (compress m1 (compress m0 s0)))
  (p0, p1, p2, p3) = compress (32 `unsafeShiftL` 56) absorbed
  -- finalization: v2 ^= 0xff; SIPROUND x4
  r1 = sipround p0 p1 (p2 `xor` 0xff) p3
  (a0, a1, a2, a3) = r1
  r2 = sipround a0 a1 a2 a3
  (b0, b1, b2, b3) = r2
  r3 = sipround b0 b1 b2 b3
  (c0, c1, c2, c3) = r3
  (g0, g1, g2, g3) = sipround c0 c1 c2 c3
  h64 = g0 `xor` g1 `xor` g2 `xor` g3
  folded = h64 `xor` (h64 `unsafeShiftR` 32)

{-------------------------------------------------------------------------------
  Operations
-------------------------------------------------------------------------------}

-- | Insert or overwrite. Guarded against the full-table infinite loop: raises.
insert :: PrimMonad m => MutableHashTable (PrimState m) -> Key -> Word64 -> m ()
insert ht key val = go 0 (hashKey ht key)
 where
  cap = mhtCap ht
  mask = mhtMask ht
  go !steps !idx
    | steps >= cap = error "MutableHashTable.insert: table full"
    | otherwise = do
        occ <- isOccupied ht idx
        if occ
          then do
            k <- readKey ht idx
            if k == key
              then writeVal ht idx val
              else go (steps + 1) ((idx + 1) .&. mask)
          else do
            writeKey ht idx key
            writeVal ht idx val
            setOccupied ht idx
            modifyMutVar' (mhtSize ht) (+ 1)

lookup :: PrimMonad m => MutableHashTable (PrimState m) -> Key -> m (Maybe Word64)
lookup ht key = go 0 (hashKey ht key)
 where
  cap = mhtCap ht
  mask = mhtMask ht
  go !steps !idx
    | steps >= cap = pure Nothing
    | otherwise = do
        occ <- isOccupied ht idx
        if not occ
          then pure Nothing
          else do
            k <- readKey ht idx
            if k == key
              then Just <$> readVal ht idx
              else go (steps + 1) ((idx + 1) .&. mask)

-- | Delete with backward-shift: after clearing the slot, pull following entries
-- back toward their ideal index so no tombstone is left behind. Returns whether
-- the key was present.
delete :: PrimMonad m => MutableHashTable (PrimState m) -> Key -> m Bool
delete ht key = do
  mIdx <- findSlot 0 (hashKey ht key)
  case mIdx of
    Nothing -> pure False
    Just idx -> do
      clearOccupied ht idx
      modifyMutVar' (mhtSize ht) (subtract 1)
      goShift 0 idx ((idx + 1) .&. mask)
      pure True
 where
  cap = mhtCap ht
  mask = mhtMask ht

  findSlot !steps !idx
    | steps >= cap = pure Nothing
    | otherwise = do
        occ <- isOccupied ht idx
        if not occ
          then pure Nothing
          else do
            k <- readKey ht idx
            if k == key
              then pure (Just idx)
              else findSlot (steps + 1) ((idx + 1) .&. mask)

  goShift !steps !cur !nxt
    | steps >= cap = error "MutableHashTable.delete: backshift did not terminate"
    | otherwise = do
        occ <- isOccupied ht nxt
        if not occ
          then pure ()
          else do
            k <- readKey ht nxt
            let ideal = hashKey ht k
                distCur = (cur - ideal) .&. mask
                distNxt = (nxt - ideal) .&. mask
            if distCur < distNxt
              then do
                v <- readVal ht nxt
                writeKey ht cur k
                writeVal ht cur v
                setOccupied ht cur
                clearOccupied ht nxt
                goShift (steps + 1) nxt ((nxt + 1) .&. mask)
              else goShift (steps + 1) cur ((nxt + 1) .&. mask)
