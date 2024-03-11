{-# LANGUAGE TypeFamilies #-}

-- | Helpers for printing various objects in a terse way. Terse printing is
-- similar to that provided by the 'Condense' typeclass except it can be
-- sometimes even more compact and it is very specific to tests.
module Test.Util.TersePrinting (
    terseBlock
  , terseFragment
  , terseHFragment
  , terseHeader
  , tersePoint
  , terseRealPoint
  , terseTip
  , terseWithOrigin
  ) where

import           Cardano.Slotting.Block (BlockNo (BlockNo))
import           Data.List (intercalate)
import           Data.List.NonEmpty (NonEmpty ((:|)), toList)
import qualified Data.List.NonEmpty as NE
import           Ouroboros.Consensus.Block (Header,
                     Point (BlockPoint, GenesisPoint), RealPoint,
                     SlotNo (SlotNo), blockHash, blockNo, blockSlot,
                     realPointToPoint)
import           Ouroboros.Network.AnchoredFragment (Anchor, AnchoredFragment,
                     anchor, anchorToPoint, mapAnchoredFragment, toOldestFirst)
import           Ouroboros.Network.Block (Tip (..))
import           Ouroboros.Network.Point (WithOrigin (..))
import           Test.Util.TestBlock (Header (TestHeader), TestBlock,
                     TestHash (TestHash), unTestHash)

-- | Run-length encoding of a list. This groups consecutive duplicate elements,
-- counting them. Only the first element of the equality is kept. For instance:
--
-- > runLengthEncoding [0, 0, 1, 0, 2, 2, 2] = [(2, 0), (1, 1), (1, 0), (3, 2)]
runLengthEncoding :: Eq a => [a] -> [(Int, a)]
runLengthEncoding xs = [(length ys, NE.head ys) | ys <- NE.group xs]

-- | Print the given 'BlockNo', 'SlotNo' and 'TestHash' in a terse way:
-- @block-slot[hash]@. @hash@ only shows if there is a non-zero element in it.
-- When it shows, it shows in a compact form. For instance, the hash
-- @[0,0,1,0,0,0]@ shows as @[2x0,1,3x0]@. This function is meant as a helper
-- for other functions.
terseBlockSlotHash :: BlockNo -> SlotNo -> TestHash -> String
terseBlockSlotHash (BlockNo bno) (SlotNo sno) (TestHash hash) =
    show bno ++ "-" ++ show sno ++ renderHash
  where
    renderHash = case runLengthEncoding (reverse (toList hash)) of
      [(_, 0)]    -> ""
      hashGrouped -> "[" ++ intercalate "," (map renderGroup hashGrouped) ++ "]"
    renderGroup (1, e) = show e
    renderGroup (n, e) = show n ++ "x" ++ show e

-- | Same as 'terseBlockSlotHash' except only the last element of the hash
-- shows, if it is non-zero. This makes sense when showing a fragment.
terseBlockSlotHash' :: BlockNo -> SlotNo -> TestHash -> String
terseBlockSlotHash' (BlockNo bno) (SlotNo sno) (TestHash hash) =
    show bno ++ "-" ++ show sno ++ renderHashSuffix hash
  where
    renderHashSuffix (forkNo :| _)
      | forkNo == 0 = ""
      | otherwise = "[" ++ show forkNo ++ "]"

-- | Print a 'TestBlock' as @block-slot[hash]@. @hash@ only shows if there is a
-- non-zero element in it. When it shows, it shows in a compact form. For
-- instance, the hash @[0,0,1,0,0,0]@ shows as @[2x0,1,3x0]@.
terseBlock :: TestBlock -> String
terseBlock block = terseBlockSlotHash (blockNo block) (blockSlot block) (blockHash block)

-- | Same as 'terseBlock' except only the last element of the hash shows, if it
-- is non-zero. This makes sense when showing a fragment.
terseBlock' :: TestBlock -> String
terseBlock' block = terseBlockSlotHash' (blockNo block) (blockSlot block) (blockHash block)

-- | Same as 'terseBlock' for headers.
terseHeader :: Header TestBlock -> String
terseHeader (TestHeader block) = terseBlock block

-- | Same as 'terseBlock' for points. Genesis shows as @G@.
tersePoint :: Point TestBlock -> String
tersePoint GenesisPoint = "G"
tersePoint (BlockPoint slot hash) =
  terseBlockSlotHash (BlockNo (fromIntegral (length (unTestHash hash)))) slot hash

terseRealPoint :: RealPoint TestBlock -> String
terseRealPoint = tersePoint . realPointToPoint

-- | Same as 'tersePoint' for anchors.
terseAnchor :: Anchor TestBlock -> String
terseAnchor = tersePoint . anchorToPoint

-- | Same as 'tersePoint' for tips.
terseTip :: Tip TestBlock -> String
terseTip TipGenesis         = "G"
terseTip (Tip sno hash bno) = terseBlockSlotHash bno sno hash

-- | Given a printer for elements of type @a@, prints a @WithOrigin a@ in a
-- terse way. Origin shows as @G@.
terseWithOrigin :: (a -> String) -> WithOrigin a -> String
terseWithOrigin _ Origin      = "G"
terseWithOrigin terseA (At a) = terseA a

-- | Print a fragment of 'TestBlock' in a terse way. This shows as @anchor |
-- block ...@ where @anchor@ is printed with 'terseAnchor' and @block@s are
-- printed with @terseBlock'@; in particular, only the last element of the hash
-- shows and only when it is non-zero.
terseFragment :: AnchoredFragment TestBlock -> String
terseFragment fragment =
    terseAnchor (anchor fragment) ++ renderBlocks
  where
    renderBlocks = case toOldestFirst fragment of
      []     -> ""
      blocks -> " | " ++ unwords (map terseBlock' blocks)

-- | Same as 'terseFragment' for fragments of headers.
terseHFragment :: AnchoredFragment (Header TestBlock) -> String
terseHFragment = terseFragment . mapAnchoredFragment (\(TestHeader block) -> block)
