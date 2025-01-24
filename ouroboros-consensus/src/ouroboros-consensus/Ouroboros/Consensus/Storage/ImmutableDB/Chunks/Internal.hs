{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal (
    ChunkInfo (..)
  , simpleChunkInfo
  , singleChunkInfo
    -- * Chunk number
  , ChunkNo (..)
  , chunkNoFromInt
  , chunkNoToInt
  , chunksBetween
  , countChunks
  , firstChunkNo
  , nextChunkNo
  , prevChunkNo
    -- * Chunk size
  , ChunkSize (..)
  , getChunkSize
    -- * Layout
  , RelativeSlot (..)
  , assertRelativeSlotInChunk
  , compareRelativeSlot
  , maxRelativeIndex
  , mkRelativeSlot
    -- * Assertions
  , ChunkAssertionFailure
  , assertSameChunk
  , assertWithinBounds
  ) where

import           Control.Exception
import           Control.Monad
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.RedundantConstraints

-- | Size of the chunks of the immutable DB
--
-- This is the key data structure that drives all layout functions.
--
-- TODO: Add support for non-uniform 'ChunkInfo'
-- <https://github.com/IntersectMBO/ouroboros-network/issues/1754>
data ChunkInfo =
    -- | A single, uniform, chunk size
    UniformChunkSize !ChunkSize
  deriving stock    (Show, Generic)
  deriving anyclass (NoThunks)

-- | Simple chunk config with a single chunk size
--
-- This intentionally takes 'EpochSize' (number of slots) rather than
-- 'ChunkSize': the translation from 'EpochSize' to 'ChunkSize' (number of
-- available entries in a chunk) should not be done by client code.
simpleChunkInfo :: EpochSize -> ChunkInfo
simpleChunkInfo (EpochSize sz) = UniformChunkSize (ChunkSize sz)

-- | 'ChunkInfo' for a single 'ChunkSize'
--
-- See also 'simpleChunkInfo'.
singleChunkInfo :: ChunkSize -> ChunkInfo
singleChunkInfo = UniformChunkSize

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Size of a chunk
--
-- The total number of slots available in a chunk is equal to 'numBlocks'
data ChunkSize = ChunkSize {
      -- | The number of blocks in this chunk
      numBlocks :: !Word64
    }
  deriving stock    (Show, Generic)
  deriving anyclass (NoThunks)

-- | Chunk number
newtype ChunkNo = ChunkNo { unChunkNo :: Word64 }
  deriving stock   (Eq, Ord, Generic)
  deriving newtype (Show, NoThunks)

-- | First chunk
firstChunkNo :: ChunkNo
firstChunkNo = ChunkNo 0

-- | Convert 'ChunkNo' to 'Int'
--
-- This is primarily useful for the immutable DB, which uses an 'IntPSQ'.
chunkNoToInt :: ChunkNo -> Int
chunkNoToInt (ChunkNo n) = fromIntegral n

-- | Convert 'Int' to 'ChunkNo'
--
-- See 'chunkNoToInt' for motivation.
chunkNoFromInt :: Int -> ChunkNo
chunkNoFromInt n = ChunkNo (fromIntegral n)

nextChunkNo :: ChunkNo -> ChunkNo
nextChunkNo (ChunkNo n) = ChunkNo (n + 1)

prevChunkNo :: ChunkNo -> Maybe ChunkNo
prevChunkNo (ChunkNo n) = guard (n > 0) >> return (ChunkNo $ n - 1)

-- | Count number of chunks between two indices
--
-- > countChunks x              x  == 0
-- > countChunks x (nextChunkNo x) == 1
countChunks :: ChunkNo -> ChunkNo -> Word64
countChunks (ChunkNo a) (ChunkNo b) = if a >= b then a - b else b - a

-- | Enumerate all chunks
--
-- > chunksBetween x              x  == [x]
-- > chunksBetween x (nextChunkNo x) == [x, nextChunkNo x]
chunksBetween :: ChunkNo -> ChunkNo -> [ChunkNo]
chunksBetween (ChunkNo a) (ChunkNo b) = map ChunkNo $
                                          if a >= b then [a .. b] else [b .. a]

getChunkSize :: ChunkInfo -> ChunkNo -> ChunkSize
getChunkSize chunkInfo _chunk =
    case chunkInfo of
      UniformChunkSize sz -> sz

{-------------------------------------------------------------------------------
  Layout

  These are defined in the @Internal@ module so that most code can safely
  import from "Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Layout" without
  worrying that it's making assumptions that it shouldn't. All bets are off for
  modules that import "Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal".
-------------------------------------------------------------------------------}

-- | A /relative/ slot within a chunk
data RelativeSlot = RelativeSlot {
    -- | The chunk index of the chunk this slot is in
    --
    -- Recorded primarily to be able to define a semi-sensible 'Ord' instance.
    relativeSlotChunkNo   :: !ChunkNo

    -- | The size of the chunk that this slot is in
    --
    -- We record this for bounds checking
  , relativeSlotChunkSize :: !ChunkSize

    -- | The index within the chunk
  , relativeSlotIndex     :: !Word64
  }
  deriving stock    (Show, Generic)
  deriving anyclass (NoThunks)

-- | Maximum relative index within a chunk
maxRelativeIndex :: ChunkSize -> Word64
maxRelativeIndex ChunkSize{..} = numBlocks - 1

-- | Smart constructor for 'RelativeSlot'
mkRelativeSlot :: HasCallStack => ChunkInfo -> ChunkNo -> Word64 -> RelativeSlot
mkRelativeSlot chunkInfo chunk index =
    assertWithinBounds index size $
    RelativeSlot {
        relativeSlotChunkNo   = chunk
      , relativeSlotChunkSize = size
      , relativeSlotIndex     = index
      }
  where
    size = getChunkSize chunkInfo chunk

instance Eq RelativeSlot where
  a == b
    | relativeSlotChunkNo a /= relativeSlotChunkNo b = False
    | otherwise =
        -- If the 'ChunkNo's are the same, then the 'ChunkSize's /must/ also be
        assertSameChunk (relativeSlotChunkNo a) (relativeSlotChunkNo b) $
          relativeSlotIndex a == relativeSlotIndex b

-- | 'RelativeSlot' is partially ordered, not totally ordered
--
-- It makes no sense to compare 'RelativeSlots' from different chunks. Doing so
-- will result in an assertion failure.
compareRelativeSlot :: HasCallStack => RelativeSlot -> RelativeSlot -> Ordering
compareRelativeSlot a b =
    assertSameChunk (relativeSlotChunkNo a) (relativeSlotChunkNo b) $
      compare (relativeSlotIndex a) (relativeSlotIndex b)

assertRelativeSlotInChunk :: HasCallStack => ChunkNo -> RelativeSlot -> Word64
assertRelativeSlotInChunk chunk relSlot =
    assertSameChunk (relativeSlotChunkNo relSlot) chunk $
      relativeSlotIndex relSlot

{-------------------------------------------------------------------------------
  Assert failures

  We insist on keeping the HasCallStack constraint here, because if we make
  that constraint depend on CPP, we will get redundant constraint warnings for
  any functions that (transitively) call these functions.
-------------------------------------------------------------------------------}

data ChunkAssertionFailure =
    NotSameChunk ChunkNo ChunkNo PrettyCallStack
  | NotWithinBounds Word64 ChunkSize PrettyCallStack
  deriving (Show)

instance Exception ChunkAssertionFailure

assertSameChunk :: HasCallStack => ChunkNo -> ChunkNo -> a -> a
#if ENABLE_ASSERTIONS
assertSameChunk a b
  | a == b    = id
  | otherwise = throw $ NotSameChunk a b prettyCallStack
#else
assertSameChunk _ _ = id
#endif
  where
    _ = keepRedundantConstraint (Proxy @HasCallStack)

assertWithinBounds :: HasCallStack => Word64 -> ChunkSize -> a -> a
#if ENABLE_ASSERTIONS
assertWithinBounds ix sz
  | ix <= maxRelativeIndex sz = id
  | otherwise                 = throw $ NotWithinBounds ix sz prettyCallStack
#else
assertWithinBounds _ _ = id
#endif
  where
    _ = keepRedundantConstraint (Proxy @HasCallStack)
