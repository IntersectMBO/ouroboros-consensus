{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Helpers for printing various objects in a terse way. Terse printing is
-- similar to that provided by the 'Condense' typeclass except it can be
-- sometimes even more compact and it is very specific to tests.
module Test.Util.TersePrinting (
    terseBlock
  , terseFrag
  , terseFragH
  , terseHeader
  , tersePoint
  ) where

import           Cardano.Slotting.Block (BlockNo (BlockNo))
import           Cardano.Slotting.Slot (SlotNo (SlotNo))
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Ouroboros.Consensus.Block (Header,
                     Point (BlockPoint, GenesisPoint), blockHash, blockNo,
                     blockSlot, getHeader)
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Network.AnchoredFragment
                     (Anchor (Anchor, AnchorGenesis), AnchoredFragment,
                     AnchoredSeq (Empty), anchor, mapAnchoredFragment,
                     toOldestFirst)
import           Test.Util.TestBlock (Header (TestHeader), TestBlock,
                     TestHash (TestHash), unTestHash)

terseSlotBlock :: SlotNo -> BlockNo -> String
terseSlotBlock (SlotNo slot) (BlockNo block) =
  show block ++ "-" ++ show slot

terseSlotBlockFork :: SlotNo -> BlockNo -> TestHash -> String
terseSlotBlockFork sno bno (TestHash hash) =
  terseSlotBlock sno bno ++ forkNoSuffix hash
  where
    forkNoSuffix (forkNo :| _) | forkNo == 0 = ""
                               | otherwise = "[" ++ show forkNo ++ "]"

terseBlock :: TestBlock -> String
terseBlock block =
  terseSlotBlockFork (blockSlot block) (blockNo block) (blockHash block)

terseHeader :: Header TestBlock -> String
terseHeader (TestHeader block) = terseBlock block

tersePoint :: Point TestBlock -> String
tersePoint = \case
  BlockPoint slot hash -> terseSlotBlockFork slot (BlockNo (fromIntegral (length (unTestHash hash)))) hash
  GenesisPoint -> "G"

terseFragH :: AnchoredFragment (Header TestBlock) -> String
terseFragH frag =
  renderAnchor ++ renderBlocks
  where
    renderBlocks = case frag of
      Empty _ -> ""
      _       -> " | " ++ unwords (terseHeader <$> toOldestFirst frag)
    renderAnchor = case anchor frag of
      AnchorGenesis -> "G"
      Anchor slot hash block -> terseSlotBlock slot block ++ renderAnchorHash hash
    renderAnchorHash hash
      | all (== 0) (unTestHash hash) = ""
      | otherwise = condense hash

terseFrag :: AnchoredFragment TestBlock -> String
terseFrag =
  terseFragH . mapAnchoredFragment getHeader
