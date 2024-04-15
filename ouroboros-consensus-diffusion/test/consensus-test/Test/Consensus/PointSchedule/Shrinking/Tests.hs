{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test properties of the shrinking functions
module Test.Consensus.PointSchedule.Shrinking.Tests (tests) where

import           Data.Foldable (toList)
import           Data.Map (keys)
import           Data.Maybe (mapMaybe)
import           Test.Consensus.Genesis.Setup (genChains)
import           Test.Consensus.Genesis.Tests.Uniform (genUniformSchedulePoints)
import           Test.Consensus.PointSchedule (PeerSchedule, PeersSchedule,
                     prettyPeersSchedule)
import           Test.Consensus.PointSchedule.Peers (Peer (..), Peers (..))
import           Test.Consensus.PointSchedule.Shrinking (shrinkHonestPeer)
import           Test.Consensus.PointSchedule.SinglePeer (SchedulePoint (..))
import           Test.QuickCheck (Property, conjoin, counterexample)
import           Test.Tasty
import           Test.Tasty.QuickCheck (choose, forAllBlind, testProperty)
import           Test.Util.TestBlock (TestBlock)

tests :: TestTree
tests =
  testGroup "shrinking functions"
    [ testGroup "honest peer shrinking"
      [ testProperty "actually shortens the schedule" prop_shortens
      , testProperty "preserves the final state all peers" prop_preservesFinalStates
      , testProperty "doesn't remove points of the adversarial schedule" prop_preserversAdversarial
      ]
    ]

prop_shortens :: Property
prop_shortens = checkShrinkProperty isShorterThan

prop_preservesFinalStates :: Property
prop_preservesFinalStates = checkShrinkProperty doesNotChangeFinalState

prop_preserversAdversarial :: Property
prop_preserversAdversarial = checkShrinkProperty doesNotRemoveAdversarialPoints

-- | Apparently, `unsnoc` hasn't been invented yet, so we'll do this manually
lastM :: [a] -> Maybe a
lastM []     = Nothing
lastM [a]    = Just a
lastM (_:ps) = lastM ps

samePeers :: PeersSchedule blk -> PeersSchedule blk -> Bool
samePeers sch1 sch2 = (keys $ others sch1) == (keys $ others sch2)

-- | Checks whether at least one peer schedule in the second given peers schedule
-- is shorter than its corresponding one in the fist given peers schedule. “Shorter”
-- here means that it executes in less time.
isShorterThan :: PeersSchedule blk -> PeersSchedule blk -> Bool
isShorterThan original shrunk =
  samePeers original shrunk
  && (or $ zipWith
    (\oldSch newSch -> (fst <$> lastM newSch) < (fst <$> lastM oldSch))
    (toList original)
    (toList shrunk)
  )

doesNotChangeFinalState :: Eq blk => PeersSchedule blk -> PeersSchedule blk -> Bool
doesNotChangeFinalState original shrunk =
  samePeers original shrunk
  && (and $ zipWith
    (\oldSch newSch ->
      lastTP oldSch == lastTP newSch &&
      lastHP oldSch == lastHP newSch &&
      lastBP oldSch == lastBP newSch
    )
    (toList original)
    (toList shrunk)
  )
  where
    lastTP :: PeerSchedule blk -> Maybe (SchedulePoint blk)
    lastTP sch = lastM $ mapMaybe (\case (_, p@(ScheduleTipPoint    _)) -> Just p ; _ -> Nothing) sch
    lastHP :: PeerSchedule blk -> Maybe (SchedulePoint blk)
    lastHP sch = lastM $ mapMaybe (\case (_, p@(ScheduleHeaderPoint _)) -> Just p ; _ -> Nothing) sch
    lastBP :: PeerSchedule blk -> Maybe (SchedulePoint blk)
    lastBP sch = lastM $ mapMaybe (\case (_, p@(ScheduleBlockPoint  _)) -> Just p ; _ -> Nothing) sch

doesNotRemoveAdversarialPoints :: Eq blk => PeersSchedule blk -> PeersSchedule blk -> Bool
doesNotRemoveAdversarialPoints original shrunk =
  samePeers original shrunk
  && (and $ zipWith
    (\oldSch newSch -> fmap snd oldSch == fmap snd newSch)
    (toList $ (fmap value) $ others original)
    (toList $ (fmap value) $ others shrunk)
  )

checkShrinkProperty :: (PeersSchedule TestBlock -> PeersSchedule TestBlock -> Bool) -> Property
checkShrinkProperty prop =
  forAllBlind
    (genChains (choose (1, 4)) >>= genUniformSchedulePoints)
    (\schedule ->
      conjoin $ map
      (\shrunk ->
          counterexample
          (  "Original schedule:\n"
          ++ unlines (map ("    " ++) $ prettyPeersSchedule schedule)
          ++ "\nShrunk schedule:\n"
          ++ unlines (map ("    " ++) $ prettyPeersSchedule shrunk)
          )
          (prop schedule shrunk)
      )
      (shrinkHonestPeer schedule)
    )
