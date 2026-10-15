-- | Tests of which votes the node's 'VoteState' keeps.
module Test.Consensus.Leios.VoteState (tests) where

import Cardano.Crypto.Leios (LeiosSeatId)
import qualified Data.ByteString as BS
import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Types (LeiosVote (..), RbHash (..))
import Ouroboros.Consensus.Leios.VoteState (Votes, maxVoteElections, recordVote, wantsVote)
import Test.Consensus.Leios.LeiosNotify.Codec (signedVote)
import Test.QuickCheck
  ( Gen
  , Property
  , choose
  , conjoin
  , counterexample
  , elements
  , forAll
  , vectorOf
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "VoteState"
    [ testProperty
        "the node keeps the first vote of each seat in the latest election slots"
        prop_keepsLatest
    ]

-- | Recording every wanted vote keeps exactly the latest 'maxVoteElections'
-- slots seen, each with the first vote of each seat, and never wants a vote of
-- one seat in one slot twice, so the node never relays it twice either.
prop_keepsLatest :: Property
prop_keepsLatest = forAll genVotes $ \vs ->
  let (kept, wanted) = foldl' record (Map.empty, []) vs
   in conjoin
        [ counterexample "kept" $
            kept === Map.drop (Map.size (firstVotes vs) - maxVoteElections) (firstVotes vs)
        , counterexample "wanted twice" $ length wanted === Set.size (Set.fromList wanted)
        ]
 where
  record ::
    (Votes, [(SlotNo, LeiosSeatId)]) -> (SlotNo, LeiosVote) -> (Votes, [(SlotNo, LeiosSeatId)])
  record (kept, wanted) (slot, vote)
    | wantsVote slot seat kept = (recordVote slot vote kept, (slot, seat) : wanted)
    | otherwise = (kept, wanted)
   where
    seat = voteSeatId vote

  firstVotes vs = Map.fromListWith (flip Map.union) [(slot, Map.singleton (voteSeatId v) v) | (slot, v) <- vs]

-- | Enough votes, over enough slots, that the node forgets some.
genVotes :: Gen [(SlotNo, LeiosVote)]
genVotes = do
  n <- choose (0, 10 * maxVoteElections)
  vectorOf n $
    (,) <$> (SlotNo <$> choose (0, 3 * fromIntegral maxVoteElections)) <*> elements someVotes

-- | Several votes of each of a few seats, so that the first one is told from
-- the others.
someVotes :: [LeiosVote]
someVotes = [signedVote (MkRbHash (BS.replicate 32 tag)) seat | tag <- [0 .. 3], seat <- [0 .. 3]]
