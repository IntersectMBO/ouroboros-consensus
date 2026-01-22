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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- for PerasRoundVoteState
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Ouroboros.Consensus.Peras.Vote.Aggregation
  ( PerasRoundVoteState (prvsRoundNo)
  , pattern VoteGeneratedNewCert
  , pattern VoteDidntGenerateNewCert
  , freshRoundVoteState
  , updatePerasRoundVoteState
  , updatePerasRoundVoteStates
  , prvsMaybeCert
  , UpdateRoundVoteStateError (..)
  ) where

import Cardano.Prelude (fromMaybe)
import Data.Functor.Compose (Compose (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (WithArrivalTime, forgetArrivalTime)
import Ouroboros.Consensus.Peras.Params (PerasParams)
import Ouroboros.Consensus.Peras.Round (PerasRoundNo)
import Ouroboros.Consensus.Peras.Vote
  ( PerasVoteId
  , PerasVoteStake (..)
  , PerasVoteTarget (..)
  , stakeAboveThreshold
  )

{-------------------------------------------------------------------------------
  Voting state for a given Peras round
-------------------------------------------------------------------------------}

-- | Current vote state for a given round
data PerasRoundVoteState blk
  = PerasRoundVoteStateQuorumNotReached
      { prvsRoundNo :: !PerasRoundNo
      , prvsCandidateStates :: !(Map (Point blk) (PerasTargetVoteState blk 'Candidate))
      }
  | PerasRoundVoteStateQuorumReachedAlready
      { prvsRoundNo :: !PerasRoundNo
      , prvsExcessVotes :: !Word64
      , prvsLoserStates :: !(Map (Point blk) (PerasTargetVoteState blk 'Loser))
      , prvsWinnerState :: !(PerasTargetVoteState blk 'Winner)
      }

deriving instance
  ( StandardHash blk
  , Eq (PerasCert blk)
  , Eq (PerasVote blk)
  ) =>
  Eq (PerasRoundVoteState blk)

deriving instance
  ( StandardHash blk
  , Show (PerasCert blk)
  , Show (PerasVote blk)
  ) =>
  Show (PerasRoundVoteState blk)

deriving instance Generic (PerasRoundVoteState blk)

deriving instance
  ( StandardHash blk
  , NoThunks (PerasCert blk)
  , NoThunks (PerasVote blk)
  ) =>
  NoThunks (PerasRoundVoteState blk)

-- | Get the certificate if quorum was reached for the given round
prvsMaybeCert :: PerasRoundVoteState blk -> Maybe (ValidatedPerasCert blk)
prvsMaybeCert = \case
  PerasRoundVoteStateQuorumNotReached{} ->
    Nothing
  PerasRoundVoteStateQuorumReachedAlready
    { prvsWinnerState = PerasTargetVoteWinner _ cert
    } ->
      Just cert

-- | Create a fresh round vote state for the given round number
freshRoundVoteState ::
  PerasRoundNo ->
  PerasRoundVoteState blk
freshRoundVoteState roundNo =
  PerasRoundVoteStateQuorumNotReached
    { prvsRoundNo = roundNo
    , prvsCandidateStates = Map.empty
    }

-- | Errors that may occur when updating the round vote state with a new vote
data UpdateRoundVoteStateError blk
  = RoundVoteStateLoserAboveQuorum
      (Point blk) -- the existing winner
      (Point blk) -- the loser that went above quorum
  | RoundVoteStateForgingCertError
      (PerasCertForgeErr blk)

-- | Add a vote to an existing round aggregate.
--
-- PRECONDITION: the vote's round must match the aggregate's round.
--
-- May fail if the state transition is invalid (e.g., a loser going above
-- quorum) or if forging the certificate fails.
updatePerasRoundVoteState ::
  forall blk.
  ( BlockSupportsPeras blk
  , PerasCfg blk ~ PerasParams
  ) =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasCfg blk ->
  PerasRoundVoteState blk ->
  Either (UpdateRoundVoteStateError blk) (PerasRoundVoteState blk)
updatePerasRoundVoteState vote _ roundState
  | getPerasVoteRound vote /= prvsRoundNo roundState =
      error "updatePerasRoundVoteState: vote round does not match aggregate round"
updatePerasRoundVoteState vote cfg roundState = do
  case roundState of
    -- Quorum not yet reached
    PerasRoundVoteStateQuorumNotReached{prvsCandidateStates} -> do
      let oldCandidateState =
            Map.findWithDefault
              (freshCandidateVoteState (getPerasVoteTarget vote))
              (pvtBlock (getPerasVoteTarget vote))
              prvsCandidateStates
      candidateOrWinnerState <-
        updateCandidateVoteState cfg vote oldCandidateState
          `onErr` \err ->
            RoundVoteStateForgingCertError err
      case candidateOrWinnerState of
        RemainedCandidate newCandidateState -> do
          -- Quorum still not reached for this round
          let prvsCandidateStates' =
                Map.insert
                  (pvtBlock (getPerasVoteTarget vote))
                  newCandidateState
                  prvsCandidateStates
          pure $
            PerasRoundVoteStateQuorumNotReached
              { prvsRoundNo = prvsRoundNo roundState
              , prvsCandidateStates = prvsCandidateStates'
              }
        BecameWinner winnerState -> do
          -- Quorum has been reached for the first time here for this round
          let winnerPoint =
                pvtBlock (ptvtTarget (ptvsVoteTally winnerState))
              loserStates =
                candidateToLoser cfg
                  <$> Map.delete winnerPoint prvsCandidateStates
          pure $
            PerasRoundVoteStateQuorumReachedAlready
              { prvsRoundNo = prvsRoundNo roundState
              , prvsExcessVotes = 0
              , prvsLoserStates = loserStates
              , prvsWinnerState = winnerState
              }

    -- Quorum already reached
    state@PerasRoundVoteStateQuorumReachedAlready{prvsLoserStates, prvsWinnerState} -> do
      let votePoint =
            pvtBlock (getPerasVoteTarget vote)
          winnerPoint =
            pvtBlock (ptvtTarget (ptvsVoteTally prvsWinnerState))

      if votePoint == winnerPoint
        -- The vote ratifies the winner => update winner state
        then do
          let winnerState' = updateWinnerVoteState vote prvsWinnerState
          pure $
            state
              { prvsExcessVotes = prvsExcessVotes state + 1
              , prvsWinnerState = winnerState'
              }

        -- The vote is for a loser => update loser state
        else do
          let existingOrFreshLoserVoteState =
                fromMaybe (freshLoserVoteState (getPerasVoteTarget vote))
              updateMaybeLoserVoteState mState =
                fmap Just $
                  updateLoserVoteState cfg vote (existingOrFreshLoserVoteState mState)
                    `onErr` \prvsLoserState ->
                      RoundVoteStateLoserAboveQuorum
                        (pvtBlock (ptvtTarget (ptvsVoteTally prvsWinnerState)))
                        (pvtBlock (ptvtTarget (ptvsVoteTally prvsLoserState)))
          prvsLoserStates' <- Map.alterF updateMaybeLoserVoteState votePoint prvsLoserStates
          pure $
            state
              { prvsExcessVotes = prvsExcessVotes state + 1
              , prvsLoserStates = prvsLoserStates'
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
  ( BlockSupportsPeras blk
  , PerasCfg blk ~ PerasParams
  ) =>
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
  PerasRoundVoteStateQuorumReachedAlready
    { prvsExcessVotes = 0 -- just reached quorum
    , prvsWinnerState = PerasTargetVoteWinner _ cert
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

deriving instance
  ( StandardHash blk
  , Eq (PerasVote blk)
  ) =>
  Eq (PerasTargetVoteTally blk)

deriving instance
  ( StandardHash blk
  , Show (PerasVote blk)
  ) =>
  Show (PerasTargetVoteTally blk)

deriving instance Generic (PerasTargetVoteTally blk)

deriving instance
  ( StandardHash blk
  , NoThunks (PerasVote blk)
  ) =>
  NoThunks (PerasTargetVoteTally blk)

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
  BlockSupportsPeras blk =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasTargetVoteTally blk ->
  PerasTargetVoteTally blk
updateTargetVoteTally vote tally
  | getPerasVoteTarget vote /= ptvtTarget tally =
      error "updateTargetVoteTally: vote target does not match tally target"
updateTargetVoteTally
  vote
  ptvt@PerasTargetVoteTally
    { ptvtVotes
    , ptvtTotalStake
    } =
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

-- | Extract the underlying vote tally from a target vote state
ptvsVoteTally :: PerasTargetVoteState blk status -> PerasTargetVoteTally blk
ptvsVoteTally = \case
  PerasTargetVoteCandidate tally -> tally
  PerasTargetVoteLoser tally -> tally
  PerasTargetVoteWinner tally _ -> tally

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
  PerasCfg blk ~ PerasParams =>
  PerasCfg blk ->
  PerasTargetVoteState blk 'Candidate ->
  PerasTargetVoteState blk 'Loser
candidateToLoser cfg (PerasTargetVoteCandidate tally) =
  if stakeAboveThreshold cfg (ptvtTotalStake tally)
    then error "candidateToLoser: candidate is above quorum"
    else PerasTargetVoteLoser tally

-- | Subtype of 'PerasTargetVoteState' to indicate whether the target remains a
-- candidate or has been elected winner
data PerasVoteStateCandidateOrWinner blk
  = RemainedCandidate (PerasTargetVoteState blk 'Candidate)
  | BecameWinner (PerasTargetVoteState blk 'Winner)

-- | Add a vote to an existing target vote state if it isn't already present.
--
-- May fail if the candidate is elected winner but forging the certificate fails.
updateCandidateVoteState ::
  ( BlockSupportsPeras blk
  , PerasCfg blk ~ PerasParams
  ) =>
  PerasCfg blk ->
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasTargetVoteState blk 'Candidate ->
  Either
    (PerasCertForgeErr blk)
    (PerasVoteStateCandidateOrWinner blk)
updateCandidateVoteState cfg vote oldState = do
  let newVoteTally = updateTargetVoteTally vote (ptvsVoteTally oldState)
      voteList = forgetArrivalTime <$> Map.elems (ptvtVotes newVoteTally)
   in if stakeAboveThreshold cfg (ptvtTotalStake newVoteTally)
        then do
          let PerasVoteTarget{pvtRoundNo, pvtBlock} = ptvtTarget newVoteTally
          cert <- forgePerasCert cfg pvtRoundNo pvtBlock voteList
          pure $ BecameWinner (PerasTargetVoteWinner newVoteTally cert)
        else
          pure $ RemainedCandidate (PerasTargetVoteCandidate newVoteTally)

-- | Add a vote to an existing target vote state if it isn't already present.
--
-- PRECONDITION: the vote's target must match the underlying tally's target.
--
-- May fail if the loser goes above quorum by adding the vote.
updateLoserVoteState ::
  ( PerasCfg blk ~ PerasParams
  , BlockSupportsPeras blk
  ) =>
  PerasCfg blk ->
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasTargetVoteState blk 'Loser ->
  Either (PerasTargetVoteState blk 'Loser) (PerasTargetVoteState blk 'Loser)
updateLoserVoteState cfg vote oldState =
  let newVoteTally = updateTargetVoteTally vote (ptvsVoteTally oldState)
      aboveQuorum = stakeAboveThreshold cfg (ptvtTotalStake newVoteTally)
   in if aboveQuorum
        then Left $ PerasTargetVoteLoser newVoteTally
        else Right $ PerasTargetVoteLoser newVoteTally

-- | Add a vote to an existing target vote state if it isn't already present.
--
-- PRECONDITION: the vote's target must match the underlying tally's target.
updateWinnerVoteState ::
  BlockSupportsPeras blk =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasTargetVoteState blk 'Winner ->
  PerasTargetVoteState blk 'Winner
updateWinnerVoteState vote oldState =
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
