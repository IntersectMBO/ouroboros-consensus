{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test properties of the shrinking functions
module Test.Consensus.PointSchedule.Shrinking.Tests (tests) where

import Data.Foldable (toList)
import Data.Map (keys)
import Data.Maybe (mapMaybe)
import Ouroboros.Consensus.Util (lastMaybe)
import Test.Consensus.Genesis.Setup (genChains)
import Test.Consensus.Genesis.Tests.Uniform (genUniformSchedulePoints)
import Test.Consensus.PointSchedule
  ( PeerSchedule
  , PointSchedule (..)
  , prettyPointSchedule
  )
import Test.Consensus.PointSchedule.Peers (Peers (..))
import Test.Consensus.PointSchedule.Shrinking (shrinkHonestPeers)
import Test.Consensus.PointSchedule.SinglePeer (SchedulePoint (..))
import Test.QuickCheck (Property, conjoin, counterexample)
import Test.Tasty
import Test.Tasty.QuickCheck (choose, forAllBlind, testProperty)
import Test.Util.TestBlock (TestBlock)

tests :: TestTree
tests =
  testGroup
    "shrinking functions"
    [ testGroup
        "honest peer shrinking"
        [ testProperty "actually shortens the schedule" prop_shortens
        , testProperty "preserves the final state all peers" prop_preservesFinalStates
        ]
    ]

prop_shortens :: Property
prop_shortens = checkShrinkProperty isShorterThan

prop_preservesFinalStates :: Property
prop_preservesFinalStates = checkShrinkProperty doesNotChangeFinalState

samePeers :: Peers (PeerSchedule blk) -> Peers (PeerSchedule blk) -> Bool
samePeers sch1 sch2 =
  (keys $ adversarialPeers sch1)
    == (keys $ adversarialPeers sch2)

-- | Checks whether at least one peer schedule in the second given peers schedule
-- is shorter than its corresponding one in the fist given peers schedule. “Shorter”
-- here means that it executes in less time.
isShorterThan :: Peers (PeerSchedule blk) -> Peers (PeerSchedule blk) -> Bool
isShorterThan original shrunk =
  samePeers original shrunk
    && ( or $
           zipWith
             (\oldSch newSch -> (fst <$> lastMaybe newSch) < (fst <$> lastMaybe oldSch))
             (toList original)
             (toList shrunk)
       )

doesNotChangeFinalState :: Eq blk => Peers (PeerSchedule blk) -> Peers (PeerSchedule blk) -> Bool
doesNotChangeFinalState original shrunk =
  samePeers original shrunk
    && ( and $
           zipWith
             ( \oldSch newSch ->
                 lastTP oldSch == lastTP newSch
                   && lastHP oldSch == lastHP newSch
                   && lastBP oldSch == lastBP newSch
             )
             (toList original)
             (toList shrunk)
       )
 where
  lastTP :: PeerSchedule blk -> Maybe (SchedulePoint blk)
  lastTP sch = lastMaybe $ mapMaybe (\case (_, p@(ScheduleTipPoint _)) -> Just p; _ -> Nothing) sch
  lastHP :: PeerSchedule blk -> Maybe (SchedulePoint blk)
  lastHP sch = lastMaybe $ mapMaybe (\case (_, p@(ScheduleHeaderPoint _)) -> Just p; _ -> Nothing) sch
  lastBP :: PeerSchedule blk -> Maybe (SchedulePoint blk)
  lastBP sch = lastMaybe $ mapMaybe (\case (_, p@(ScheduleBlockPoint _)) -> Just p; _ -> Nothing) sch

checkShrinkProperty ::
  (Peers (PeerSchedule TestBlock) -> Peers (PeerSchedule TestBlock) -> Bool) -> Property
checkShrinkProperty prop =
  forAllBlind
    (genChains (choose (1, 4)) >>= genUniformSchedulePoints)
    ( \sch@PointSchedule{psSchedule, psStartOrder, psMinEndTime} ->
        conjoin $
          map
            ( \shrunk ->
                counterexample
                  ( "Original schedule:\n"
                      ++ unlines (map ("    " ++) $ prettyPointSchedule sch)
                      ++ "\nShrunk schedule:\n"
                      ++ unlines
                        ( map ("    " ++) $
                            prettyPointSchedule $
                              PointSchedule{psSchedule = shrunk, psStartOrder, psMinEndTime}
                        )
                  )
                  (prop psSchedule shrunk)
            )
            (shrinkHonestPeers psSchedule)
    )
