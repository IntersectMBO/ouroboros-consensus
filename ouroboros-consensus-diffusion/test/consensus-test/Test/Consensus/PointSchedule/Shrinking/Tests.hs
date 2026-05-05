{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Test properties of the shrinking functions
module Test.Consensus.PointSchedule.Shrinking.Tests (tests) where

import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Monad.Class.MonadTime.SI (Time (..))
import Data.Foldable (toList)
import Data.Map (elems, empty, keys, singleton)
import Data.Maybe (mapMaybe)
import Ouroboros.Consensus.Util (lastMaybe)
import Test.Consensus.Genesis.Setup (genChains)
import Test.Consensus.Genesis.Tests.Uniform (genUniformSchedulePoints)
import Test.Consensus.PointSchedule
  ( PeerSchedule
  , PointSchedule (..)
  , prettyPointSchedule
  )
import Test.Consensus.PointSchedule.Peers (PeerId (..), Peers (..))
import Test.Consensus.PointSchedule.Shrinking (shrinkAdversarialPeer, shrinkHonestPeers)
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
        [ testProperty "actually shortens the schedule" prop_honShortens
        , testProperty "preserves the final state all peers" prop_honPreservesFinalStates
        ]
    , testGroup
        "adversarial peer shrinking"
        [ testProperty "preserves consistency of TP/HP/BP" prop_advPreservesConsistency
        ]
    ]

prop_honShortens :: Property
prop_honShortens = checkHonShrinkProperty isShorterThan

prop_honPreservesFinalStates :: Property
prop_honPreservesFinalStates = checkHonShrinkProperty doesNotChangeFinalState

prop_advPreservesConsistency :: Property
prop_advPreservesConsistency = checkAdvShrinkProperty preservesConsistency

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

-- | Checks that the shrunk schedule still has the properties TP >= HP and HP >= BP at any time
preservesConsistency :: Ord blk => PeerSchedule blk -> PeerSchedule blk -> Bool
preservesConsistency _original shrunk =
  (\(_, _, x) -> x) $
    foldr
      ( \(_t, p) (tp, hp, acc) ->
          case p of
            ScheduleTipPoint newTp -> (newTp, hp, acc)
            ScheduleHeaderPoint newHp -> (tp, newHp, acc && tp >= newHp)
            ScheduleBlockPoint newBp -> (tp, hp, acc && hp >= newBp)
      )
      (Origin, Origin, True)
      shrunk

checkHonShrinkProperty ::
  (Peers (PeerSchedule TestBlock) -> Peers (PeerSchedule TestBlock) -> Bool) -> Property
checkHonShrinkProperty prop =
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

checkAdvShrinkProperty :: (PeerSchedule TestBlock -> PeerSchedule TestBlock -> Bool) -> Property
checkAdvShrinkProperty prop =
  forAllBlind
    ( do
        chains <- genChains (choose (1, 4))
        PointSchedule{psSchedule} <- genUniformSchedulePoints chains
        -- \| We generated at least one honest peer schedule, and it can serve as adversarial
        -- for the intent of this test.
        case elems (honestPeers psSchedule) of
          [] -> error "No honest peers generated"
          hd : _ -> pure hd
    )
    ( \sch ->
        conjoin $
          map
            ( \shrunk ->
                counterexample
                  ( "Original schedule:\n"
                      ++ unlines (map ("    " ++) $ prettyPointSchedule $ mkPointSchedule sch)
                      ++ "\nShrunk schedule:\n"
                      ++ unlines (map ("    " ++) $ prettyPointSchedule $ mkPointSchedule shrunk)
                  )
                  (prop sch shrunk)
            )
            (shrinkAdversarialPeer sch)
    )

mkPointSchedule :: PeerSchedule TestBlock -> PointSchedule TestBlock
mkPointSchedule sch =
  PointSchedule
    { psSchedule = Peers{honestPeers = empty, adversarialPeers = singleton 1 sch}
    , psStartOrder = [AdversarialPeer 1]
    , psMinEndTime = case lastMaybe sch of
        Nothing -> Time 0
        Just (t, _) -> t
    }
