{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

-- | Peras vote aggregation and certificate forging
--
-- This module implements the core voting logic for the Peras protocol, which
-- aggregates stake-weighted votes on chain blocks and forges certificates when
-- quorum is reached.
--
-- = Overview
--
-- In Peras, validators vote on specific blocks during designated voting rounds.
-- Each vote carries a stake weight, and votes are aggregated by:
--
--   * __Round__: each vote belongs to a specific 'PerasRoundNo'
--   * __Target__: within a round, votes are cast for different block 'Point's
--
-- As votes arrive, the system tracks the total stake backing each candidate
-- block. When one target accumulates enough stake to exceed the configured
-- quorum threshold, a certificate is automatically forged for that block,
-- making it a winner for that round.
--
-- = State Machine
--
-- For every round being voted for, the aggregation follows a state machine:
--
-- 1. __Quorum not reached__: multiple block targets are candidates, each
--    accumulating votes and stake. All targets compete to reach quorum first.
--
-- 2. __Quorum reached__: once a target reaches quorum, it becomes the winner
--    and a certificate is forged. All other targets become losers and continue
--    tracking votes without affecting the outcome.
--
-- = Quorum Threshold and Multiple Winners
--
-- The quorum threshold is parameterized via 'PerasCfg'. Depending on this
-- configuration and the stake distribution, it may be theoretically possible
-- for multiple targets to exceed the threshold within the same round.
--
-- This module treats multiple winners as an error condition and rejects votes
-- that would cause this, raising instead a 'RoundVoteStateLoserAboveQuorum'
-- exception. This indicates that either:
--   * The quorum threshold is misconfigured, or that
--   * We were extremely unlucky when randomly selecting the voting committee.
--
-- With a correct threshold configuration (e.g., > 3/4 of total stake + a small
-- safety margin to account for an unlucky local sortition when selecting
-- non-persistent voters during committee selection), multiple winners should be
-- impossible given honest stake distribution.
--
-- = Key Types
--
--   * 'PerasRoundVoteState': tracks all voting activity for a single round, and
--      its logically split between separate 'NoQuorum' and 'Quorum' types
--      representing the two states (1) and (2) described above, respectively.
--   * 'PerasTargetVoteState': tracks votes for one specific block target
--   * 'PerasTargetVoteTally': raw vote count and stake accumulation
--   * 'PerasTargetVoteStatus': type-level status (Candidate/Winner/Loser)
--   * 'UpdateRoundVoteStateError': errors from invalid state transitions
--
-- = Usage
--
-- The primary entry point is 'updatePerasRoundVoteStates', which adds a new
-- vote to the aggregate state. Pattern synonyms 'VoteGeneratedNewCert' and
-- 'VoteDidntGenerateNewCert' allow clients to observe when certificates are
-- freshly forged (as opposed to voting on an already-won target).
module Ouroboros.Consensus.Peras.Vote.Aggregation
  ( PerasRoundVoteState
  , ptvsTotalStake
  , pattern VoteGeneratedNewCert
  , pattern VoteDidntGenerateNewCert
  , updatePerasRoundVoteStates
  , getPerasRoundVoteStateCertMaybe
  , UpdateRoundVoteStateError (..)
  ) where

import Cardano.Prelude (fromMaybe)
import Control.Exception (assert)
import Data.Functor.Compose (Compose (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (WithArrivalTime, forgetArrivalTime)

{-------------------------------------------------------------------------------
  Voting state for a given Peras round
-------------------------------------------------------------------------------}

-- | Current vote state for a given round
data PerasRoundVoteState blk = PerasRoundVoteState
  { prvsRoundNo :: !PerasRoundNo
  , prvsState :: !(Either (NoQuorum blk) (Quorum blk))
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass NoThunks

instance HasPerasVoteRound (PerasRoundVoteState blk) where
  getPerasVoteRound = prvsRoundNo

-- | Current vote state when a quorum has not yet been reached
data NoQuorum blk = NoQuorum
  { candidateStates :: !(Map (Point blk) (PerasTargetVoteState blk 'Candidate))
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass NoThunks

-- | Current vote state when a quorum has been reached
data Quorum blk = Quorum
  { excessVotes :: !Word64
  , loserStates :: !(Map (Point blk) (PerasTargetVoteState blk 'Loser))
  , winnerState :: !(PerasTargetVoteState blk 'Winner)
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass NoThunks

-- | Get the certificate if quorum was reached for the given round
getPerasRoundVoteStateCertMaybe ::
  PerasRoundVoteState blk ->
  Maybe (ValidatedPerasCert blk)
getPerasRoundVoteStateCertMaybe = \case
  PerasRoundVoteState
    { prvsState =
      Right
        Quorum
          { winnerState =
            PerasTargetVoteWinner _ cert
          }
    } ->
      Just cert
  _ ->
    Nothing

-- | Create a fresh round vote state for the given round number
freshRoundVoteState ::
  PerasRoundNo ->
  PerasRoundVoteState blk
freshRoundVoteState roundNo =
  PerasRoundVoteState
    { prvsRoundNo = roundNo
    , prvsState =
        Left
          NoQuorum
            { candidateStates =
                Map.empty
            }
    }

-- | Errors that may occur when updating the round vote state with a new vote
data UpdateRoundVoteStateError blk
  = RoundVoteStateLoserAboveQuorum
      (PerasTargetVoteState blk 'Winner)
      (PerasTargetVoteState blk 'Loser)
  | RoundVoteStateForgingCertError
      (PerasForgeErr blk)

-- | Add a vote to an existing round vote aggregate.
--
-- PRECONDITION: the vote's round must match the aggregate's round.
--
-- May fail if the state transition is invalid (e.g., a loser going above
-- quorum) or if forging the certificate fails.
updatePerasRoundVoteState ::
  forall blk.
  StandardHash blk =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasCfg blk ->
  PerasRoundVoteState blk ->
  Either (UpdateRoundVoteStateError blk) (PerasRoundVoteState blk)
updatePerasRoundVoteState vote cfg roundState =
  assert (getPerasVoteRound vote == getPerasVoteRound roundState) $ do
    case roundState of
      -- Quorum not yet reached
      state@PerasRoundVoteState
        { prvsState =
          Left
            NoQuorum
              { candidateStates
              }
        } -> do
          let oldCandidateState =
                Map.findWithDefault
                  (freshCandidateVoteState (getPerasVoteTarget vote))
                  (getPerasVoteBlock vote)
                  candidateStates
          candidateOrWinnerState <-
            updateCandidateVoteState cfg vote oldCandidateState
              `onErr` \err ->
                RoundVoteStateForgingCertError err
          case candidateOrWinnerState of
            RemainedCandidate newCandidateState -> do
              -- Quorum still not reached for this round
              let prvsCandidateStates' =
                    Map.insert
                      (getPerasVoteBlock vote)
                      newCandidateState
                      candidateStates
              pure $
                state
                  { prvsState =
                      Left
                        NoQuorum
                          { candidateStates = prvsCandidateStates'
                          }
                  }
            BecameWinner winnerState -> do
              -- Quorum has been reached for the first time here for this round
              let winnerPoint =
                    pvtBlock (ptvtTarget (ptvsVoteTally winnerState))
                  loserStates =
                    candidateToLoser <$> Map.delete winnerPoint candidateStates
              pure $
                PerasRoundVoteState
                  { prvsRoundNo =
                      prvsRoundNo roundState
                  , prvsState =
                      Right
                        Quorum
                          { excessVotes = 0
                          , loserStates = loserStates
                          , winnerState = winnerState
                          }
                  }

      -- Quorum already reached
      state@PerasRoundVoteState
        { prvsState =
          Right
            Quorum
              { excessVotes
              , winnerState
              , loserStates
              }
        } -> do
          let votePoint =
                getPerasVoteBlock vote
              winnerPoint =
                pvtBlock (ptvtTarget (ptvsVoteTally winnerState))
          if votePoint == winnerPoint
            -- The vote ratifies the winner => update winner state
            then do
              let winnerState' =
                    updateWinnerVoteState vote winnerState
              pure $
                state
                  { prvsState =
                      Right
                        Quorum
                          { excessVotes = excessVotes + 1
                          , winnerState = winnerState'
                          , loserStates = loserStates
                          }
                  }

            -- The vote is for a loser => update loser state
            else do
              let existingOrFreshLoserVoteState =
                    fromMaybe (freshLoserVoteState (getPerasVoteTarget vote))
                  updateMaybeLoserVoteState mState =
                    fmap Just $
                      updateLoserVoteState cfg vote (existingOrFreshLoserVoteState mState)
                        `onErr` \err ->
                          RoundVoteStateLoserAboveQuorum winnerState err
              loserStates' <- Map.alterF updateMaybeLoserVoteState votePoint loserStates
              pure $
                state
                  { prvsState =
                      Right
                        Quorum
                          { excessVotes = excessVotes + 1
                          , winnerState = winnerState
                          , loserStates = loserStates'
                          }
                  }

-- | Updates the round vote states map with the given vote.
--
-- A new entry is created if necessary (i.e., if there is no existing state for
-- the vote's round).
--
-- May fail if the state transition is invalid (e.g., a loser going above
-- quorum) or if forging the certificate fails.
updatePerasRoundVoteStates ::
  forall blk.
  StandardHash blk =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasCfg blk ->
  Map PerasRoundNo (PerasRoundVoteState blk) ->
  Either
    (UpdateRoundVoteStateError blk)
    (PerasRoundVoteState blk, Map PerasRoundNo (PerasRoundVoteState blk))
updatePerasRoundVoteStates vote cfg =
  alterMapAndReturnUpdatedValue
    updateMaybePerasRoundVoteState
    (getPerasVoteRound vote)
 where
  -- We use the Functor instance of `Compose (Either e) ((,) s)` ≅
  -- `λt. Either e (s, t)` in `Map.alterF`. That way, we can return both the
  -- updated map and the updated leaf in one pass, and still handle errors.
  alterMapAndReturnUpdatedValue ::
    Ord k =>
    (Maybe a -> Either e (a, a)) ->
    k ->
    Map k a ->
    Either e (a, Map k a)
  alterMapAndReturnUpdatedValue f k =
    getCompose . Map.alterF (fmap Just . (Compose . f)) k

  -- If there is no existing state for the vote's round, create a fresh one.
  existingOrFreshRoundVoteState ::
    Maybe (PerasRoundVoteState blk) ->
    PerasRoundVoteState blk
  existingOrFreshRoundVoteState =
    fromMaybe (freshRoundVoteState (getPerasVoteRound vote))

  -- Update the round state, creating a fresh one if necessary, and returning
  -- the updated state.
  updateMaybePerasRoundVoteState ::
    Maybe (PerasRoundVoteState blk) ->
    Either
      (UpdateRoundVoteStateError blk)
      (PerasRoundVoteState blk, PerasRoundVoteState blk)
  updateMaybePerasRoundVoteState mRoundState = do
    let roundState = existingOrFreshRoundVoteState mRoundState
    newRoundState <- updatePerasRoundVoteState vote cfg roundState
    pure (newRoundState, newRoundState)

{-------------------------------------------------------------------------------
  Peras round vote state pattern synonyms
-------------------------------------------------------------------------------}

-- These pattern synonyms hide internal details of the round vote state, while
-- allowing the client to observe when a certificate has just been forged.

-- | Matches a round vote state where a certificate has just been forged
pattern VoteGeneratedNewCert ::
  ValidatedPerasCert blk ->
  PerasRoundVoteState blk
pattern VoteGeneratedNewCert cert <-
  (voteGeneratedCert -> Just cert)

-- | Matches a round vote state where a certificate has either not yet been
-- forged, or was forged by a previous vote
pattern VoteDidntGenerateNewCert ::
  PerasRoundVoteState blk
pattern VoteDidntGenerateNewCert <-
  (voteGeneratedCert -> Nothing)

{-# COMPLETE VoteGeneratedNewCert, VoteDidntGenerateNewCert #-}

-- | Helper for the above pattern synonyms
voteGeneratedCert :: PerasRoundVoteState blk -> Maybe (ValidatedPerasCert blk)
voteGeneratedCert = \case
  PerasRoundVoteState
    { prvsState =
      Right
        Quorum
          { excessVotes = 0 -- just reached quorum
          , winnerState = PerasTargetVoteWinner _ cert
          }
    } ->
      Just cert
  _ ->
    Nothing

{-------------------------------------------------------------------------------
  Peras target vote tally
-------------------------------------------------------------------------------}

-- | Tally of votes for a given target (round number and block point)
data PerasTargetVoteTally blk = PerasTargetVoteTally
  { ptvtTarget :: !(PerasVoteTarget blk)
  -- ^ What we are tallying votes for
  , ptvtVotes :: !(Map (PerasVoteId blk) (WithArrivalTime (ValidatedPerasVote blk)))
  -- ^ Votes received for this target, indexed by vote ID
  , ptvtTotalStake :: !PerasVoteStake
  -- ^ Total stake of the votes received for this target
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass NoThunks

freshTargetVoteTally :: PerasVoteTarget blk -> PerasTargetVoteTally blk
freshTargetVoteTally target =
  PerasTargetVoteTally
    { ptvtTarget = target
    , ptvtVotes = Map.empty
    , ptvtTotalStake = PerasVoteStake 0
    }

-- | Add a vote to an existing target tally if it isn't already present,
-- and update the stake accordingly.
--
-- PRECONDITION: the vote's target must match the tally's target.
updateTargetVoteTally ::
  StandardHash blk =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasTargetVoteTally blk ->
  PerasTargetVoteTally blk
updateTargetVoteTally
  vote
  ptvt@PerasTargetVoteTally
    { ptvtVotes
    , ptvtTarget
    , ptvtTotalStake
    } =
    assert (getPerasVoteTarget vote == ptvtTarget) $ do
      ptvt
        { ptvtVotes = pvaVotes'
        , ptvtTotalStake = pvaTotalStake'
        }
   where
    swapVote =
      Map.insertLookupWithKey
        (\_k old _new -> old)
        (getPerasVoteId vote)

    (pvaVotes', pvaTotalStake')
      -- key WAS NOT present → vote inserted and stake updated
      | (Nothing, votes') <- swapVote vote ptvtVotes =
          (votes', ptvtTotalStake + vpvVoteStake (forgetArrivalTime vote))
      -- key WAS already present → votes and stake unchanged
      | otherwise =
          (ptvtVotes, ptvtTotalStake)

{-------------------------------------------------------------------------------
  Peras target vote status
-------------------------------------------------------------------------------}

-- | Indicate the current status of the target w.r.t the voting process
data PerasTargetVoteStatus
  = Candidate
  | Winner
  | Loser
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass NoThunks

-- | Voting state for a given target.
--
-- We indicate at type level the status of the target w.r.t the voting process.
data PerasTargetVoteState blk (status :: PerasTargetVoteStatus) where
  PerasTargetVoteCandidate ::
    !(PerasTargetVoteTally blk) ->
    PerasTargetVoteState blk 'Candidate
  PerasTargetVoteLoser ::
    !(PerasTargetVoteTally blk) ->
    PerasTargetVoteState blk 'Loser
  PerasTargetVoteWinner ::
    !(PerasTargetVoteTally blk) ->
    !(ValidatedPerasCert blk) ->
    PerasTargetVoteState blk 'Winner

deriving stock instance
  ( Eq (PerasTargetVoteTally blk)
  , Eq (ValidatedPerasCert blk)
  ) =>
  Eq (PerasTargetVoteState blk status)

deriving stock instance
  ( Ord (PerasTargetVoteTally blk)
  , Ord (ValidatedPerasCert blk)
  ) =>
  Ord (PerasTargetVoteState blk status)

deriving stock instance
  ( Show (PerasTargetVoteTally blk)
  , Show (ValidatedPerasCert blk)
  ) =>
  Show (PerasTargetVoteState blk status)

instance
  ( NoThunks (PerasTargetVoteTally blk)
  , NoThunks (ValidatedPerasCert blk)
  ) =>
  NoThunks (PerasTargetVoteState blk status)
  where
  -- avoid the Generic-based default
  showTypeOf _ = "PerasTargetVoteState"

  -- we can just delegate wNoThunks to our custom noThunks
  wNoThunks = noThunks

  noThunks ctx (PerasTargetVoteCandidate tally) =
    noThunks ctx tally
  noThunks ctx (PerasTargetVoteLoser tally) =
    noThunks ctx tally
  noThunks ctx (PerasTargetVoteWinner tally cert) =
    noThunks ctx (tally, cert)

instance HasPerasVoteRound (PerasTargetVoteState blk status) where
  getPerasVoteRound = pvtRoundNo . ptvtTarget . ptvsVoteTally

instance HasPerasVoteBlock (PerasTargetVoteState blk status) blk where
  getPerasVoteBlock = pvtBlock . ptvtTarget . ptvsVoteTally

-- | Extract the underlying vote tally from a target vote state
ptvsVoteTally :: PerasTargetVoteState blk status -> PerasTargetVoteTally blk
ptvsVoteTally = \case
  PerasTargetVoteCandidate tally -> tally
  PerasTargetVoteLoser tally -> tally
  PerasTargetVoteWinner tally _ -> tally

-- | Extract the total stake from a target vote state
ptvsTotalStake :: PerasTargetVoteState blk status -> PerasVoteStake
ptvsTotalStake = ptvtTotalStake . ptvsVoteTally

freshCandidateVoteState :: PerasVoteTarget blk -> PerasTargetVoteState blk 'Candidate
freshCandidateVoteState target =
  PerasTargetVoteCandidate (freshTargetVoteTally target)

freshLoserVoteState :: PerasVoteTarget blk -> PerasTargetVoteState blk 'Loser
freshLoserVoteState target =
  PerasTargetVoteLoser (freshTargetVoteTally target)

-- | Convert a 'Candidate' state to a 'Loser' state.
--
-- This function is called on all candidates (except the winner) once a winner
-- is elected.
candidateToLoser ::
  PerasTargetVoteState blk 'Candidate ->
  PerasTargetVoteState blk 'Loser
candidateToLoser (PerasTargetVoteCandidate tally) =
  PerasTargetVoteLoser tally

-- | Subtype of 'PerasTargetVoteState' to indicate whether the target remains a
-- candidate or has been elected winner
data PerasVoteStateCandidateOrWinner blk
  = RemainedCandidate (PerasTargetVoteState blk 'Candidate)
  | BecameWinner (PerasTargetVoteState blk 'Winner)

-- | Add a vote to an existing target vote state if it isn't already present.
--
-- May fail if the candidate is elected winner but forging the certificate fails.
updateCandidateVoteState ::
  StandardHash blk =>
  PerasCfg blk ->
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasTargetVoteState blk 'Candidate ->
  Either
    (PerasForgeErr blk)
    (PerasVoteStateCandidateOrWinner blk)
updateCandidateVoteState cfg vote oldState =
  let
    newVoteTally = updateTargetVoteTally vote (ptvsVoteTally oldState)
    voteList = forgetArrivalTime <$> Map.elems (ptvtVotes newVoteTally)
   in
    case votesReachQuorum cfg voteList of
      Just votesWithQuorum -> do
        cert <- forgePerasCert cfg votesWithQuorum
        pure $ BecameWinner (PerasTargetVoteWinner newVoteTally cert)
      Nothing -> do
        pure $ RemainedCandidate (PerasTargetVoteCandidate newVoteTally)

-- | Add a vote to an existing target vote state if it isn't already present.
--
-- PRECONDITION: the vote's target must match the underlying tally's target.
--
-- May fail if the loser goes above quorum by adding the vote.
updateLoserVoteState ::
  StandardHash blk =>
  PerasCfg blk ->
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasTargetVoteState blk 'Loser ->
  Either (PerasTargetVoteState blk 'Loser) (PerasTargetVoteState blk 'Loser)
updateLoserVoteState cfg vote oldState =
  assert (getPerasVoteTarget vote == ptvtTarget (ptvsVoteTally oldState)) $ do
    let newVoteTally = updateTargetVoteTally vote (ptvsVoteTally oldState)
        aboveQuorum = stakeAboveThreshold cfg (ptvtTotalStake newVoteTally)
     in if aboveQuorum
          then Left $ PerasTargetVoteLoser newVoteTally
          else Right $ PerasTargetVoteLoser newVoteTally

-- | Add a vote to an existing target vote state if it isn't already present.
--
-- PRECONDITION: the vote's target must match the underlying tally's target.
updateWinnerVoteState ::
  StandardHash blk =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasTargetVoteState blk 'Winner ->
  PerasTargetVoteState blk 'Winner
updateWinnerVoteState vote oldState =
  assert (getPerasVoteTarget vote == ptvtTarget (ptvsVoteTally oldState)) $ do
    let newVoteTally = updateTargetVoteTally vote (ptvsVoteTally oldState)
        (PerasTargetVoteWinner _ cert) = oldState
     in PerasTargetVoteWinner newVoteTally cert

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Apply a function to the error part of an Either
onErr :: Either e a -> (e -> e') -> Either e' a
onErr (Left err) f = Left (f err)
onErr (Right val) _ = Right val
