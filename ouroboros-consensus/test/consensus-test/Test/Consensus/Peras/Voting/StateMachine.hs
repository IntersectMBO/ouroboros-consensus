{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | State machine test for Peras voting rules.
module Test.Consensus.Peras.Voting.StateMachine (tests) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad.State (MonadState (..), State, evalState, modify)
import Data.Bifunctor (Bifunctor (..))
import Data.Either (isRight)
import Data.Foldable (Foldable (..))
import Data.Sequence (Seq, ViewL (..), viewl, (|>))
import Ouroboros.Consensus.Block.Abstract (WithOrigin (..))
import Ouroboros.Consensus.Block.SupportsPeras (PerasRoundNo (..))
import Ouroboros.Consensus.Peras.Params
  ( PerasBlockMinSlots (..)
  , PerasCertArrivalThreshold (..)
  , PerasCooldownRounds (..)
  , PerasIgnoranceRounds (..)
  , PerasParams (..)
  )
import Ouroboros.Consensus.Peras.Voting (NoVoteReason, PerasVotingView (..), VoteReason)
import qualified Ouroboros.Consensus.Peras.Voting as SUT (isPerasVotingAllowed)
import Ouroboros.Consensus.Util.Pred (explainShallow)
import qualified Test.Consensus.Peras.Voting.Model as Model
import Test.Consensus.Peras.Voting.Util (TestCert)
import Test.QuickCheck.Monadic (monadic)
import Test.QuickCheck.StateModel
  ( Actions
  , Annotated (..)
  , Any (..)
  , Generic
  , HasVariables (..)
  , RunModel (..)
  , StateModel (..)
  , counterexamplePost
  , runActions
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, Testable (..), frequency, tabulate, testProperty)
import Test.Util.TestEnv (adjustQuickCheckTests)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests =
  adjustQuickCheckTests (* 100) $
    testGroup
      "Peras.Voting.StateMachine"
      [ testProperty
          "notVotingTwiceImpliesCooldown"
          prop_notVotingTwiceImpliesCooldown
      ]

{-------------------------------------------------------------------------------
  Local voting state properties
-------------------------------------------------------------------------------}

-- | If a node doesn't vote for more than two consecutive voting rounds, then
-- it _must_ enter a cooldown period that lasts for at least R-1 rounds.
prop_notVotingTwiceImpliesCooldown :: Actions VotingState -> Property
prop_notVotingTwiceImpliesCooldown =
  prop_sm_trace $ \trace -> do
    tabulate "Traces" [show trace] $
      property True

{-------------------------------------------------------------------------------
  Generic state machine property
-------------------------------------------------------------------------------}

-- | Lift a trace property into a state machine property.
--
-- The trace is extracted from the model's underlying state at the end of the
-- state machine simulation.
prop_sm_trace :: (VotingTrace -> Property) -> Actions VotingState -> Property
prop_sm_trace prop actions =
  monadic (flip evalState initPerasVotingView) $ do
    finalVotingState <- underlyingState . fst <$> runActions actions
    return (prop (votingTrace finalVotingState))

-- | Initialize a mocked Peras voting view for testing.
initPerasVotingView :: PerasVotingView TestCert
initPerasVotingView =
  PerasVotingView
    { perasParams =
        PerasParams
          { perasIgnoranceRounds = PerasIgnoranceRounds 5
          , perasCooldownRounds = PerasCooldownRounds 5
          , perasBlockMinSlots = PerasBlockMinSlots 3
          , perasCertArrivalThreshold = PerasCertArrivalThreshold 3
          }
    , currRoundNo = PerasRoundNo 0
    , latestCertSeen = Origin
    , latestCertOnChain = Origin
    , certRoundStart = \_ -> SlotNo 0
    , arrivalSlot = \_ -> SlotNo 0
    , candidateExtendsCert = \_ -> True
    }

-- | Mock the voting view to simulate doing nothing, allowing a quorum to be
-- reached if all the normal conditions are met.
doNothing :: PerasVotingView TestCert -> PerasVotingView TestCert
doNothing pvv = pvv

-- | Mock the voting view to simulate not reaching a quorum.
preventQuorum :: PerasVotingView TestCert -> PerasVotingView TestCert
preventQuorum pvv = pvv

-- | Whether a node voted or not, and if so, why.
type VotingDecision = Bool

-- | A trace describing what a node decided to do over time.
type VotingTrace = Seq (SlotNo, PerasRoundNo, VotingDecision)

-- | A simple representation of the _local_ voting state as seen by a _single_
-- node over time. This can be used to define some temporal-style properties
-- over the voting behaviour of that node.
--
-- NOTE: this won't accurately represent the global voting state of a set of
-- nodes, as an attacker can be assumed to be able to distort their local view
-- via e.g. network partitioning and selective vote release. Nonetheless, this
-- can still be useful to test those properties that should hold even locally.
data VotingState = VotingState
  { currSlotNo :: SlotNo
  , votingTrace :: VotingTrace
  , votingView :: PerasVotingView TestCert
  }
  deriving Generic

-- TODO: improve Show instance
instance Show VotingState where
  show vs = show (currSlotNo vs, votingTrace vs)

initVotingState :: VotingState
initVotingState = VotingState 0 mempty initPerasVotingView

instance StateModel VotingState where
  -- External attacker interactions
  data Action VotingState a where
    -- The attacker does nothing, allowing the honest nodes to progress
    -- naturally, i.e., reaching a voting quroum, and thus giving rise to a
    -- new certificate boosting a block (which one is irrelevant here).
    DoNothing :: Action VotingState (Either NoVoteReason VoteReason)
    -- The attacker prevents the honest nodes from reaching a voting quorum
    -- for current round.
    PreventQuorum :: Action VotingState (Either NoVoteReason VoteReason)

  -- Empty initial model
  initialState = initVotingState

  -- TODO: make this more interesting
  arbitraryAction _ _ =
    frequency
      [ (3, pure (Some DoNothing))
      , (1, pure (Some PreventQuorum))
      ]

  -- Transition function for the model state
  nextState votingState action _ =
    votingState
      { currSlotNo = slotNo + 1
      , votingTrace = votingTrace votingState |> (slotNo, roundNo, decision)
      , votingView = pvv'
      }
   where
    slotNo = currSlotNo votingState
    -- TODO: use PerasRoundLength
    roundNo = PerasRoundNo (unSlotNo (slotNo + 1) `quot` 10)
    pvv' =
      case action of
        DoNothing ->
          doNothing (votingView votingState)
        PreventQuorum ->
          preventQuorum (votingView votingState)
    (decision, _) = Model.isPerasVotingAllowed pvv'

deriving stock instance Show (Action VotingState a)
deriving stock instance Eq (Action VotingState a)

instance HasVariables VotingState where
  getAllVariables _ = mempty

instance HasVariables (Action VotingState a) where
  getAllVariables _ = mempty

instance RunModel VotingState (State (PerasVotingView TestCert)) where
  -- Execute the actual action against the real system under test
  perform _ action _ =
    case action of
      DoNothing -> do
        modify doNothing
        SUT.isPerasVotingAllowed <$> get
      PreventQuorum -> do
        modify preventQuorum
        SUT.isPerasVotingAllowed <$> get

  -- Check that both the model and SUT agree on the outcome
  postcondition (_, votingStateAfter) DoNothing _ actual = do
    let (_, _, expected) :< _ = viewl (votingTrace votingStateAfter)
    counterexamplePost $ show expected <> " /= " <> show (isRight actual)
    counterexamplePost $ "Model voting state: " <> show votingStateAfter
    counterexamplePost $ "SUT return value:   " <> show (bimap explainShallow explainShallow actual)
    pure $ expected == isRight actual
  postcondition (_, votingStateAfter) PreventQuorum _ actual = do
    let (_, _, expected) :< _ = viewl (votingTrace votingStateAfter)
    counterexamplePost $ show expected <> " /= " <> show (isRight actual)
    counterexamplePost $ "Model voting state: " <> show votingStateAfter
    counterexamplePost $ "SUT return value:   " <> show (bimap explainShallow explainShallow actual)
    pure $ expected == isRight actual
