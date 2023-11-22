{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.PointSchedule.Tests (tests) where

import           Control.Monad (replicateM)
import           Data.List (mapAccumL, sort)
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
      , testProperty "headerPointSchedule" prop_headerPointSchedule
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
      pure $ isSorted QC.le xs
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
          isSorted QC.le ts

data HeaderPointScheduleInput = HeaderPointScheduleInput
  { hpsMsgInterval :: (DiffTime, DiffTime)
  , hpsTipPoints :: [(Maybe Int, [(DiffTime, Int)])]
  } deriving (Show)

instance QC.Arbitrary HeaderPointScheduleInput where
  arbitrary = do
    a <- (\t -> t - 1) <$> chooseDiffTime (1, 20)
    b <- (\t -> t - 1) <$> chooseDiffTime (1, 20)
    let msgInterval = (min a b, max a b)
    branchCount <- QC.choose (1, 5)
    branchTips <- replicateM branchCount (sort . QC.getNonEmpty <$> QC.arbitrary)
    let tpCount = sum $ map length branchTips
    ts <- sort <$> replicateM tpCount (chooseDiffTime (0, fromIntegral tpCount))
    let tpts = zipMany ts branchTips
    intersectionBlocks <- sort <$> replicateM branchCount QC.arbitrary
    maybes <- QC.infiniteList @(Maybe Int)
    let intersections = zipWith (>>) maybes $ map Just intersectionBlocks
    pure $ HeaderPointScheduleInput msgInterval (zip intersections tpts)

prop_headerPointSchedule :: QCGen -> HeaderPointScheduleInput -> QC.Property
prop_headerPointSchedule g (HeaderPointScheduleInput msgDelayInterval xs) =
    runSTGen_ g $ \g' -> do
      hpss <- headerPointSchedule g' msgDelayInterval xs
      pure $
          (QC.counterexample ("length xs = " ++ show (length xs)) $
           QC.counterexample ("length hpss = " ++ show (length hpss)) $
            length xs QC.=== length hpss
          )
        QC..&&.
          (QC.counterexample ("header points are sorted in each branch") $
            foldr (QC..&&.) (QC.property True)
              [ QC.counterexample ("branch = " ++ show hps) $
                isSorted QC.lt (map snd trunk) QC..&&. isSorted QC.lt (map snd branch)
              | hps@(HeaderPointSchedule trunk branch) <- hpss
              ]
          )
         QC..&&.
          (QC.counterexample ("times are sorted accross branches") $
           QC.counterexample ("branches = " ++ show hpss) $
            isSorted QC.le $ concat
              [ map fst trunk ++ map fst branch
              | HeaderPointSchedule trunk branch <- hpss
              ]
          )
        QC..&&.
          (QC.counterexample ("trunk header points are sorted accross branches") $
           QC.counterexample ("branches = " ++ show hpss) $
            isSorted QC.lt $ concat
              [ map snd trunk | HeaderPointSchedule trunk _ <- hpss ]
          )


isSorted :: Show a => (a -> a -> QC.Property) -> [a] -> QC.Property
isSorted cmp xs =
    QC.counterexample ("isSorted " ++ show xs) $
    foldr (QC..&&.) (QC.property True)
      [ cmp a b | (a, b) <- zip xs (drop 1 xs) ]

chooseDiffTime :: (DiffTime, DiffTime) -> QC.Gen DiffTime
chooseDiffTime (a, b) = do
    let aInt = diffTimeToPicoseconds a
        bInt = diffTimeToPicoseconds b
    picosecondsToDiffTime <$> QC.chooseInteger (aInt, bInt)

zipMany :: [a] -> [[b]] -> [[(a, b)]]
zipMany xs0 = snd . mapAccumL (go []) xs0
  where
    go acc xs [] = (xs, reverse acc)
    go _acc [] _ys = error "zipMany: lengths don't match"
    go acc (x:xs) (y:ys) = go ((x, y) : acc) xs ys
