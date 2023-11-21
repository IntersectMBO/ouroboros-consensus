{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.PointSchedule.Tests (tests) where

import           Data.List (sort)
--import           Data.List.NonEmpty (NonEmpty ((:|)))
--import           Data.Maybe (fromJust)
import           Data.Time.Clock (DiffTime, picosecondsToDiffTime, diffTimeToPicoseconds)
--import           Ouroboros.Consensus.Block (getHeader)
--import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (SlotNo (..))
--import           Test.Consensus.BlockTree (btTrunk)
--import           Test.Consensus.PointSchedule.SinglePeer
import           Test.Consensus.PointSchedule.SinglePeer.Indices
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.QuickCheck.Random
import           Test.Tasty
import           Test.Tasty.QuickCheck
import qualified Test.Util.QuickCheck as QC
import           System.Random.Stateful (runSTGen_)

tests :: TestTree
tests =
    testGroup "PointSchedule"
      [ testProperty "singleJumpTipPoints" prop_singleJumpTipPoints
      , testProperty "tipPointSchedule" prop_tipPointSchedule
      ]

data SingleJumpTipPointsInput = SingleJumpTipPointsInput
  { sjtpMin :: Int
  , sjtpMax  :: Int
  } deriving (Show)

instance QC.Arbitrary SingleJumpTipPointsInput where
  arbitrary = do
    QC.NonNegative a <- QC.arbitrary
    QC.NonNegative b <- QC.arbitrary
    pure $ SingleJumpTipPointsInput (min a b) (max a b)

prop_singleJumpTipPoints :: QCGen -> SingleJumpTipPointsInput -> QC.Property
prop_singleJumpTipPoints seed (SingleJumpTipPointsInput m n) =
    runSTGen_ seed $ \g -> do
      xs <- singleJumpTipPoints g m n
      pure $ isSorted xs
        QC..&&.
         (QC.counterexample ("length xs = " ++ show (length xs)) $
           length xs `QC.le` n - m + 1
         )
        QC..&&.
         (QC.counterexample ("head xs = " ++ show (head xs)) $
             head xs `QC.le` n
           QC..&&.
             m `QC.le` head xs
         )

data TipPointScheduleInput = TipPointScheduleInput
  { tpsSlotLength :: DiffTime
  , tpsMsgInterval :: (DiffTime, DiffTime)
  , tpsSlots :: [SlotNo]
  } deriving (Show)

instance QC.Arbitrary TipPointScheduleInput where
  arbitrary = do
    slotLength <- chooseDiffTime (1, 20)
    a <- (\t -> t - 1) <$> chooseDiffTime (1, slotLength)
    b <- (\t -> t - 1) <$> chooseDiffTime (1, slotLength)
    let msgInterval = (min a b, max a b)
    slots0 <- sort . map SlotNo <$> arbitrary
    slots1 <- sort . map SlotNo <$> arbitrary
    pure $ TipPointScheduleInput slotLength msgInterval (slots0 ++ slots1)

prop_tipPointSchedule :: QCGen -> TipPointScheduleInput -> QC.Property
prop_tipPointSchedule seed (TipPointScheduleInput slotLength msgInterval slots) =
    runSTGen_ seed $ \g -> do
      ts <- tipPointSchedule g slotLength msgInterval slots
      pure $
          (QC.counterexample ("length slots = " ++ show (length slots)) $
           QC.counterexample ("length ts = " ++ show (length ts)) $
             length slots QC.=== length ts
          )
        QC..&&.
          isSorted ts



isSorted :: (Ord a, Show a) => [a] -> QC.Property
isSorted xs =
    QC.counterexample ("isSorted " ++ show xs) $
    foldr (QC..&&.) (QC.property True)
      [ QC.le a b | (a, b) <- zip xs (tail xs) ]

chooseDiffTime :: (DiffTime, DiffTime) -> QC.Gen DiffTime
chooseDiffTime (a, b) = do
    let aInt = diffTimeToPicoseconds a
        bInt = diffTimeToPicoseconds b
    picosecondsToDiffTime <$> QC.chooseInteger (aInt, bInt)
