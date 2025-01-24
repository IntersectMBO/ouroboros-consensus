{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- | Layout of individual chunks on disk
--
-- This module is not re-exported from the public Chunks API, since it's only
-- relevant internally in the immutable DB. This module makes the layout
-- decisions.
module Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Layout (
    -- * Relative slots
    NextRelativeSlot (..)
  , firstBlock
  , maxRelativeSlot
  , nextRelativeSlot
  , nthBlock
  , unsafeNextRelativeSlot
    -- ** Opaque
  , RelativeSlot
    -- * Chunks
  , chunkIndexOfSlot
    -- * Slots within a chunk
  , ChunkSlot (..)
  , pattern ChunkSlot
    -- ** Translation /to/ 'ChunkSlot'
  , chunkSlotForSlot
  , chunkSlotForRelativeSlot
  , chunkSlotForTip
  , chunkSlotForUnknownBlock
    -- ** Translation /from/ 'ChunkSlot'
  , chunkSlotToSlot
  ) where

import           GHC.Generics (Generic)
import           GHC.Stack
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Storage.ImmutableDB.API (Tip (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal

{-------------------------------------------------------------------------------
  Relative slots
-------------------------------------------------------------------------------}

-- | The last relative slot within a chunk of the given size
maxRelativeSlot :: ChunkInfo -> ChunkNo -> RelativeSlot
maxRelativeSlot ci chunk =
    mkRelativeSlot ci chunk (maxRelativeIndex size)
  where
    size = getChunkSize ci chunk

-- | The @n@'th relative slot for an arbitrary block
nthBlock :: (HasCallStack, Integral a)
         => ChunkInfo -> ChunkNo -> a -> RelativeSlot
nthBlock ci chunk = mkRelativeSlot ci chunk . fromIntegral

-- | The first relative slot
firstBlock :: ChunkInfo -> ChunkNo -> RelativeSlot
firstBlock ci chunk = mkRelativeSlot ci chunk 0

-- | Result of 'nextRelativeSlot'
data NextRelativeSlot =
    -- | There is a next negative slot
    NextRelativeSlot RelativeSlot

    -- | We reached the end of the chunk
  | NoMoreRelativeSlots

-- | Next relative slot
nextRelativeSlot :: HasCallStack => RelativeSlot -> NextRelativeSlot
nextRelativeSlot s@RelativeSlot{..} =
    -- Assert that the /current/ value is within bounds
    assertWithinBounds relativeSlotIndex relativeSlotChunkSize $
      if relativeSlotIndex == maxRelativeIndex relativeSlotChunkSize
        then NoMoreRelativeSlots
        else NextRelativeSlot $ s { relativeSlotIndex = succ relativeSlotIndex }

-- | Variation on 'nextRelativeSlot' where the caller /knows/ that there must
-- be a next slot
--
-- Throws an assertion failure (if assertions are enabled) if there is no
-- next slot.
unsafeNextRelativeSlot :: HasCallStack => RelativeSlot -> RelativeSlot
unsafeNextRelativeSlot s@RelativeSlot{..} =
    assertWithinBounds (succ relativeSlotIndex) relativeSlotChunkSize $
      s { relativeSlotIndex = succ relativeSlotIndex }

{-------------------------------------------------------------------------------
  Chucks
-------------------------------------------------------------------------------}

chunkIndexOfSlot :: ChunkInfo -> SlotNo -> ChunkNo
chunkIndexOfSlot (UniformChunkSize ChunkSize{..}) (SlotNo slot) = ChunkNo $
    slot `div` numBlocks

{-------------------------------------------------------------------------------
  Slot within an epoch
-------------------------------------------------------------------------------}

-- | Uniquely identify a block within the immutable DB
--
-- Constructor marked as 'Unsafe'; construction should normally happen inside
-- this module only (though see the 'ChunkSlot' pattern synonym).
data ChunkSlot = UnsafeChunkSlot
  { chunkIndex    :: !ChunkNo
  , chunkRelative :: !RelativeSlot
  } deriving (Eq, Generic, NoThunks)

-- | We provide a manual 'Ord' instance because 'RelativeSlot' does not
-- (and cannot) define one. By comparing the 'chunkIndex' before the index here,
-- we establish the precondition to 'compareRelativeSlot'.
instance Ord ChunkSlot where
  compare a b = mconcat [
      compare             (chunkIndex    a) (chunkIndex    b)
    , compareRelativeSlot (chunkRelative a) (chunkRelative b)
    ]

{-# COMPLETE ChunkSlot #-}
pattern ChunkSlot :: ChunkNo -> RelativeSlot -> ChunkSlot
pattern ChunkSlot index relative <- UnsafeChunkSlot index relative

instance Show ChunkSlot where
  show (ChunkSlot e s) = show (unChunkNo e, relativeSlotIndex s)

{-------------------------------------------------------------------------------
  Translation /to/ 'ChunkSlot'
-------------------------------------------------------------------------------}

-- | Chunk slot for an unknown block
chunkSlotForUnknownBlock :: ChunkInfo
                         -> SlotNo
                         -> (ChunkNo, ChunkSlot)
chunkSlotForUnknownBlock ci slot = (
      chunkIndex cslot
    , cslot
    )
  where
    cslot = chunkSlotForSlot ci slot

-- | Chunk slot for a slot
chunkSlotForSlot :: ChunkInfo -> SlotNo -> ChunkSlot
chunkSlotForSlot (UniformChunkSize sz@ChunkSize{..}) (SlotNo slot) =
    UnsafeChunkSlot {
        chunkIndex    = ChunkNo chunk
      , chunkRelative = RelativeSlot (ChunkNo chunk) sz $ withinChunk
      }
  where
    (chunk, withinChunk) = slot `divMod` numBlocks

-- | Chunk slot for 'Tip'
chunkSlotForTip :: ChunkInfo -> Tip blk -> ChunkSlot
chunkSlotForTip ci Tip { tipSlotNo } =
    chunkSlotForSlot ci tipSlotNo

chunkSlotForRelativeSlot :: ChunkNo -> RelativeSlot -> ChunkSlot
chunkSlotForRelativeSlot chunk relSlot =
    assertSameChunk (relativeSlotChunkNo relSlot) chunk $
      UnsafeChunkSlot chunk relSlot

{-------------------------------------------------------------------------------
  Translation /from/ 'ChunkSlot'
-------------------------------------------------------------------------------}

-- | From relative to absolute slot
chunkSlotToSlot :: ChunkInfo -> ChunkSlot -> SlotNo
chunkSlotToSlot (UniformChunkSize ChunkSize{..}) UnsafeChunkSlot{..} = SlotNo $
      chunk * numBlocks + relativeSlotIndex
  where
    ChunkNo chunk    = chunkIndex
    RelativeSlot{..} = chunkRelative
