{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.PointSchedule.Tests (tests) where

import           Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..),
                     withOrigin)
import           Control.Monad (forM, replicateM)
import           Control.Monad.Class.MonadTime.SI (Time (Time))
import           Data.Bifunctor (second)
import           Data.List (foldl', group, isSuffixOf, partition, sort)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (isNothing)
import           Data.Time.Clock (DiffTime, diffTimeToPicoseconds,
                     picosecondsToDiffTime)
import           GHC.Stack (HasCallStack)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (blockHash)
import           System.Random.Stateful (runSTGen_)
import           Test.Consensus.PointSchedule.SinglePeer
import           Test.Consensus.PointSchedule.SinglePeer.Indices
import qualified Test.QuickCheck as QC hiding (elements)
import           Test.QuickCheck
import           Test.QuickCheck.Random
import           Test.Tasty
import           Test.Tasty.QuickCheck
import qualified Test.Util.QuickCheck as QC
import           Test.Util.TestBlock (TestBlock, TestHash (unTestHash),
                     firstBlock, modifyFork, successorBlock, tbSlot)
import           Test.Util.TestEnv

tests :: TestTree
tests =
    adjustQuickCheckTests (* 100) $
    adjustOption (\(QuickCheckMaxSize n) -> QuickCheckMaxSize (n `div` 10)) $
    testGroup "PointSchedule"
      [ testProperty "zipMany" prop_zipMany
      , testProperty "singleJumpTipPoints" prop_singleJumpTipPoints
      , testProperty "tipPointSchedule" prop_tipPointSchedule
      , testProperty "headerPointSchedule" prop_headerPointSchedule
      , testProperty "peerScheduleFromTipPoints" prop_peerScheduleFromTipPoints
      ]

prop_zipMany :: [[Int]] -> QC.Property
prop_zipMany xss =
    let xs :: [Int]
        xs = concatMap (map (+1)) xss
        ys :: [[(Int, Int)]]
        ys = zipMany xs xss
     in
          length xss QC.=== length ys
        QC..&&.
          map (map snd) ys QC.=== xss
        QC..&&.
          concatMap (map fst) ys QC.=== xs

data SingleJumpTipPointsInput = SingleJumpTipPointsInput
  { sjtpMin :: Int
  , sjtpMax :: Int
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
         (QC.counterexample ("head xs = " ++ show (headCallStack xs)) $
             headCallStack xs `QC.le` n
           QC..&&.
             m `QC.le` headCallStack xs
         )

data TipPointScheduleInput = TipPointScheduleInput
  { tpsSlotLength  :: DiffTime
  , tpsMsgInterval :: (DiffTime, DiffTime)
  , tpsSlots       :: [SlotNo]
  } deriving (Show)

instance QC.Arbitrary TipPointScheduleInput where
  arbitrary = do
    slotLength <- chooseDiffTime (1, 20)
    msgInterval <- genTimeInterval (slotLength - 0.1)
    slots0 <- dedupSorted . map (SlotNo . QC.getNonNegative) <$> QC.orderedList
    slots1 <- dedupSorted . map (SlotNo . QC.getNonNegative) <$> QC.orderedList
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
  , hpsTipPoints   :: [(Maybe Int, [(Time, Int)])]
  } deriving (Show)

instance QC.Arbitrary HeaderPointScheduleInput where
  arbitrary = do
    msgInterval <- genTimeInterval 10
    branchTips <- genTipPoints
    let branchCount = length branchTips
        tpCount = sum $ map length branchTips
    ts <- map Time <$> scanl1 (+) . sort <$> replicateM tpCount (chooseDiffTime (7, 12))
    let tpts = zipMany ts branchTips
    intersectionBlocks <- genIntersections branchCount
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
        QC..&&.
          (QC.counterexample "branch header points follow tip points" $
           QC.counterexample ("branches = " ++ show hpss) $
             foldr (QC..&&.) (QC.property True) $
               zipWith (\hps x ->
                 case x of
                   (Just _, b) -> headerPointsFollowTipPoints leMaybe (hpsBranch hps) b
                   _ -> QC.property True
                ) hpss xs
          )
  where
    leMaybe :: Ord a => a -> a -> Maybe Ordering
    leMaybe a b = Just $ compare a b

data PeerScheduleFromTipPointsInput = PeerScheduleFromTipPointsInput
       PeerScheduleParams
       [(IsTrunk, [Int])]
       (AF.AnchoredFragment TestBlock)
       [AF.AnchoredFragment TestBlock]

instance Show PeerScheduleFromTipPointsInput where
  show (PeerScheduleFromTipPointsInput psp tps trunk branches) =
    unlines
      [ "PeerScheduleFromTipPointsInput"
      , "  params = "  ++ show psp
      , "  tipPoints = " ++ show tps
      , "  trunk = " ++ show (AF.toOldestFirst trunk)
      , "  branches = " ++ show [ (AF.anchorBlockNo b, AF.toOldestFirst b) | b <- branches ]
      ]

instance QC.Arbitrary PeerScheduleFromTipPointsInput where
  arbitrary = do
    slotLength <- chooseDiffTime (1, 20)
    tipDelayInterval <- genTimeInterval (slotLength - 0.1)
    headerDelayInterval <- genTimeInterval (min 2 (slotLength - 0.1))
    blockDelayInterval <- genTimeInterval (min 2 (slotLength - 0.1))
    tipPoints <- genTipPoints
    isTrunks <- QC.infiniteList
    intersections <- genIntersections (length tipPoints)
    let tstps = zip isTrunks tipPoints
        tsi = zip isTrunks intersections
        -- The maximum block number in the tip points and the intersections.
        maxBlock =
          maximum $ concat [ b | (IsTrunk, b) <- tstps ] ++
                           [ i | (IsBranch, i) <- tsi ]
    trunkSlots <- map SlotNo <$> genSortedVectorWithoutDuplicates (maxBlock + 1)
    let branchesTipPoints = [ b | (IsBranch, b) <- tstps ]
    branchesSlots <- forM branchesTipPoints $ \b -> do
      let maxBranchBlock = maximum b
      map SlotNo <$> genSortedVectorWithoutDuplicates (maxBranchBlock + 1)
    let trunk = mkFragment Origin trunkSlots 0
        branchIntersections = [ i | (IsBranch, i) <- tsi ]
        branches =
          [ genAdversarialFragment trunk fNo i branchSlots
          | (fNo, branchSlots, i)  <- zip3 [1..] branchesSlots branchIntersections
          ]
        psp = PeerScheduleParams
          { pspSlotLength = slotLength
          , pspTipDelayInterval = tipDelayInterval
          , pspHeaderDelayInterval = headerDelayInterval
          , pspBlockDelayInterval = blockDelayInterval
          }

    pure $ PeerScheduleFromTipPointsInput psp tstps trunk branches

instance QC.Arbitrary IsTrunk where
  arbitrary = QC.elements [IsTrunk, IsBranch]

prop_peerScheduleFromTipPoints :: QCGen -> PeerScheduleFromTipPointsInput -> QC.Property
prop_peerScheduleFromTipPoints seed (PeerScheduleFromTipPointsInput psp tps trunk branches) =
    runSTGen_ seed $ \g -> do
      ss <- peerScheduleFromTipPoints g psp tps trunk branches
      let (tps', (hps, _bps)) =
            partition (isHeaderPoint . snd) <$> partition (isTipPoint . snd) ss
      pure $
          (QC.counterexample ("hps = " ++ show (map (second showPoint) hps)) $
           QC.counterexample ("tps' = " ++ show (map (second showPoint) tps')) $
             headerPointsFollowTipPoints isAncestorBlock
               (map (second schedulePointToBlock) hps)
               (map (second schedulePointToBlock) tps')
          )
        QC..&&.
          (QC.counterexample ("schedule = " ++ show (map (second showPoint) ss)) $
            isSorted QC.le (map fst ss))
        QC..&&.
          (QC.counterexample ("schedule = " ++ show (map (second showPoint) ss)) $
           QC.counterexample ("header points don't decrease or repeat") $
            noReturnToAncestors (filter isHeaderPoint $ map snd ss)
          )
        QC..&&.
          (QC.counterexample ("schedule = " ++ show (map (second showPoint) ss)) $
           QC.counterexample ("block points don't decrease or repeat") $
            noReturnToAncestors (filter isBlockPoint $ map snd ss)
          )
  where
    showPoint :: SchedulePoint -> String
    showPoint (ScheduleTipPoint b)    = "TP " ++ show (blockHash b)
    showPoint (ScheduleHeaderPoint b) = "HP " ++ show (blockHash b)
    showPoint (ScheduleBlockPoint b)  = "BP " ++ show (blockHash b)

    isTipPoint :: SchedulePoint -> Bool
    isTipPoint (ScheduleTipPoint _) = True
    isTipPoint _                    = False

    isHeaderPoint :: SchedulePoint -> Bool
    isHeaderPoint (ScheduleHeaderPoint _) = True
    isHeaderPoint _                       = False

    isBlockPoint :: SchedulePoint -> Bool
    isBlockPoint (ScheduleBlockPoint _) = True
    isBlockPoint _                      = False

isAncestorBlock :: TestBlock -> TestBlock -> Maybe Ordering
isAncestorBlock b0 b1 =
    if isSuffixOf
         (NonEmpty.toList (unTestHash (blockHash b0)))
         (NonEmpty.toList (unTestHash (blockHash b1)))
    then if blockHash b0 == blockHash b1
      then Just EQ
      else Just LT
    else Nothing

noReturnToAncestors :: [SchedulePoint] -> QC.Property
noReturnToAncestors = go []
  where
    go _ [] = QC.property True
    go ancestors (p : ss) =
      let b = schedulePointToBlock p
       in   foldr (QC..&&.) (QC.property True)
              (map (isNotAncestorOf b) ancestors)
          QC..&&.
            go (b : ancestors) ss

    isNotAncestorOf :: TestBlock -> TestBlock -> QC.Property
    isNotAncestorOf b0 b1 =
      QC.counterexample ("return to ancestor: " ++ show (blockHash b0) ++ " -> " ++ show (blockHash b1)) $
        QC.property $ isNothing $ isAncestorBlock b0 b1

genTimeInterval :: DiffTime -> QC.Gen (DiffTime, DiffTime)
genTimeInterval trange = do
    a <- chooseDiffTime (1, trange)
    b <- chooseDiffTime (1, trange)
    pure (min a b, max a b)

genTipPoints :: QC.Gen [[Int]]
genTipPoints = do
    branchCount <- QC.choose (1, 5)
    xss <- QC.vector branchCount
    pure $ map (dedupSorted . sort . map QC.getNonNegative . QC.getNonEmpty) xss

-- | @genIntersections n@ generates a list of @n@ intersections as block numbers.
genIntersections :: Int -> QC.Gen [Int]
genIntersections n =
    -- Intersection with the genesis block is represented by @Just (-1)@.
    map (\x -> x - 1) . sort . map QC.getNonNegative <$> QC.vector n

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

dedupSorted :: Eq a => [a] -> [a]
dedupSorted = map headCallStack . group

headCallStack :: HasCallStack => [a] -> a
headCallStack xs = if null xs then error "headCallStack: empty list" else head xs

headerPointsFollowTipPoints :: Show a => (a -> a -> Maybe Ordering) -> [(Time, a)] -> [(Time, a)] -> QC.Property
headerPointsFollowTipPoints _ [] [] = QC.property True
headerPointsFollowTipPoints isAncestor ((t0, i0) : ss) ((t1, i1) : ps) =
      QC.counterexample "schedule times follow tip points" (QC.ge t0 t1)
    QC..&&.
      (case isAncestor i0 i1 of
         Just LT -> headerPointsFollowTipPoints isAncestor ss ((t1, i1) : ps)
         Just EQ -> headerPointsFollowTipPoints isAncestor ss ps
         _       -> headerPointsFollowTipPoints isAncestor ((t0, i0) : ss) ps
      )
headerPointsFollowTipPoints _ [] _ps =
--      There can be unscheduled header points if they would be produced so
--      late that they would come after the tip point has moved to another branch.
--
--      QC.counterexample ("schedule times are sufficient for: " ++ show ps) $
--        QC.property False
      QC.property True
headerPointsFollowTipPoints _ ss [] =
      QC.counterexample ("schedule times finish after last tip point: " ++ show ss) $
        QC.property False

-- | @genAdversarialFragment goodBlocks forkNo prefixCount slotsA@ generates
-- a fragment for a chain that forks off the given chain.
genAdversarialFragment :: AF.AnchoredFragment TestBlock -> Int -> Int -> [SlotNo] -> AF.AnchoredFragment TestBlock
genAdversarialFragment goodBlocks forkNo prefixCount slotsA
      = mkFragment intersectionBlock slotsA forkNo
  where
    -- blocks in the common prefix in reversed order
    intersectionBlock = case AF.head $ AF.takeOldest (prefixCount + 1) goodBlocks of
        Left _  -> Origin
        Right b -> At b

-- | @mkFragment pre active forkNo@ generates a list of blocks at the given slots.
mkFragment :: WithOrigin TestBlock -> [SlotNo] -> Int -> AF.AnchoredFragment TestBlock
mkFragment pre active forkNo = AF.fromNewestFirst anchor $ foldl' issue [] active
  where
    anchor = withOrigin AF.AnchorGenesis AF.anchorFromBlock pre
    issue (h : t) s = (successorBlock h) {tbSlot = s} : h : t
    issue [] s | Origin <- pre = [(firstBlock (fromIntegral forkNo)) {tbSlot = s}]
               | At h <- pre = [(modifyFork (const (fromIntegral forkNo)) (successorBlock h)) {tbSlot = s}]

-- | @genVectorWithoutDuplicates n@ generates a vector of length @n@
-- without duplicates.
genSortedVectorWithoutDuplicates :: (QC.Arbitrary a, Num a, Ord a) => Int -> QC.Gen [a]
genSortedVectorWithoutDuplicates n = do
    x0 <- QC.arbitrary
    scanl (+) x0 . map ((+1) . QC.getNonNegative) <$> QC.vector (n - 1)
