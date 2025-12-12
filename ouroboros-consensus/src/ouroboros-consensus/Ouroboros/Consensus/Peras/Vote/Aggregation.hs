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
-- for PerasRoundVoteState
{-# OPTIONS_GHC -Wno-partial-fields #-}
-- for voteTallyAboveQuorum specifically
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ouroboros.Consensus.Peras.Vote.Aggregation
  ( UpdateRoundVoteStateError (..)
  , PerasRoundVoteState
  , pattern PerasRoundVoteStateQuorumReachedNowWithCert
  , prvsMaybeCert
  , freshRoundVoteState
  , updatePerasRoundVoteState
  , updatePerasRoundVoteStates
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

-- | Tally of votes for a given target (round number and block point).
data PerasTargetVoteTally blk = PerasTargetVoteTally
  { ptvtTarget :: !(PerasVoteTarget blk)
  , ptvtVotes :: !(Map (PerasVoteId blk) (WithArrivalTime (ValidatedPerasVote blk)))
  , ptvtTotalStake :: !PerasVoteStake
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

instance HasPerasVoteTarget (PerasTargetVoteTally blk) blk where
  getPerasVoteTarget = ptvtTarget

instance HasPerasVoteRound (PerasTargetVoteTally blk) where
  getPerasVoteRound = pvtRoundNo . getPerasVoteTarget

instance HasPerasVoteBlock (PerasTargetVoteTally blk) blk where
  getPerasVoteBlock = pvtBlock . getPerasVoteTarget

freshTargetVoteTally :: PerasVoteTarget blk -> PerasTargetVoteTally blk
freshTargetVoteTally target =
  PerasTargetVoteTally
    { ptvtTarget = target
    , ptvtVotes = Map.empty
    , ptvtTotalStake = PerasVoteStake 0
    }

-- | Check whether the given target vote tally's stake is above quorum.
voteTallyAboveQuorum ::
  StandardHash blk =>
  PerasCfg blk ->
  PerasTargetVoteTally blk ->
  Bool
voteTallyAboveQuorum PerasParams{perasQuorumStakeThreshold} ptvt =
  unPerasVoteStake (ptvtTotalStake ptvt) >= unPerasQuorumStakeThreshold perasQuorumStakeThreshold

-- | Add a vote to an existing target tally if it isn't already present,
-- and update the stake accordingly.
-- PRECONDITION: the vote's target must match the tally's target.
updateTargetVoteTally ::
  StandardHash blk =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasTargetVoteTally blk ->
  PerasTargetVoteTally blk
updateTargetVoteTally
  vote
  ptvt@PerasTargetVoteTally
    { ptvtTarget
    , ptvtVotes
    , ptvtTotalStake
    } =
    if getPerasVoteTarget vote /= ptvtTarget
      then error "updatePerasVoteTally: vote target does not match tally target"
      else
        let (pvaVotes', pvaTotalStake') =
              case Map.insertLookupWithKey
                (\_k old _new -> old)
                (getPerasVoteId vote)
                vote
                ptvtVotes of
                (Nothing, votes') ->
                  -- key was NOT present → inserted and stake updated
                  (votes', ptvtTotalStake + vpvVoteStake (forgetArrivalTime vote))
                (Just _, _) ->
                  -- key WAS already present → votes and stake unchanged
                  (ptvtVotes, ptvtTotalStake)
         in ptvt{ptvtVotes = pvaVotes', ptvtTotalStake = pvaTotalStake'}

-------------------------------------------------------------------------------

-- | Indicate the current status of the target w.r.t the voting process.
data PerasTargetVoteStatus
  = Candidate
  | Winner
  | Loser
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass NoThunks

-- | Voting state for a given target.
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
    -- | Number of extra votes received since the target was elected winner /
    -- the cert was forged.
    !Word64 ->
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
  noThunks ctx (PerasTargetVoteWinner tally w cert) =
    noThunks ctx (tally, w, cert)

instance HasPerasVoteTarget (PerasTargetVoteState blk status) blk where
  getPerasVoteTarget = getPerasVoteTarget . ptvsVoteTally

instance HasPerasVoteRound (PerasTargetVoteState blk status) where
  getPerasVoteRound = getPerasVoteRound . ptvsVoteTally

instance HasPerasVoteBlock (PerasTargetVoteState blk status) blk where
  getPerasVoteBlock = getPerasVoteBlock . ptvsVoteTally

ptvsVoteTally :: PerasTargetVoteState blk status -> PerasTargetVoteTally blk
ptvsVoteTally = \case
  PerasTargetVoteCandidate tally -> tally
  PerasTargetVoteLoser tally -> tally
  PerasTargetVoteWinner tally _ _ -> tally

freshCandidateVoteState :: PerasVoteTarget blk -> PerasTargetVoteState blk 'Candidate
freshCandidateVoteState target =
  PerasTargetVoteCandidate (freshTargetVoteTally target)

freshLoserVoteState :: PerasVoteTarget blk -> PerasTargetVoteState blk 'Loser
freshLoserVoteState target =
  PerasTargetVoteLoser (freshTargetVoteTally target)

-- | Convert a 'Candidate' state to a 'Loser' state. This function is called on
-- all candidates (except the winner) once a winner is elected.
candidateToLoser ::
  StandardHash blk =>
  PerasCfg blk ->
  PerasTargetVoteState blk 'Candidate ->
  PerasTargetVoteState blk 'Loser
candidateToLoser cfg (PerasTargetVoteCandidate tally) =
  if voteTallyAboveQuorum cfg tally
    then error "candidateToLoser: candidate is above quorum"
    else PerasTargetVoteLoser tally

die :: e -> Either e a
die = Left

onErr :: Either e a -> (e -> e') -> Either e' a
onErr (Left err) f = Left (f err)
onErr (Right val) _ = Right val

data PerasVoteStateCandidateOrWinner blk =
  ACandidate (PerasTargetVoteState blk 'Candidate)
  | AWinner (PerasTargetVoteState blk 'Winner)
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
updateCandidateVoteState cfg vote oldState = do
  let newVoteTally = updateTargetVoteTally vote (ptvsVoteTally oldState)
      voteList = forgetArrivalTime <$> Map.elems (ptvtVotes newVoteTally)
   in if voteTallyAboveQuorum cfg newVoteTally
        then do 
          cert <- forgePerasCert cfg (ptvtTarget newVoteTally) voteList
          pure $ AWinner (PerasTargetVoteWinner newVoteTally 0 cert)
        else
          pure $ ACandidate (PerasTargetVoteCandidate newVoteTally)

-- | Add a vote to an existing target vote state if it isn't already present
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
  let newVoteTally = updateTargetVoteTally vote (ptvsVoteTally oldState)
      aboveQuorum = voteTallyAboveQuorum cfg newVoteTally
   in if aboveQuorum
        then die $ PerasTargetVoteLoser newVoteTally
        else pure $ PerasTargetVoteLoser newVoteTally

-- | Add a vote to an existing target vote state if it isn't already present
-- PRECONDITION: the vote's target must match the underlying tally's target.
updateWinnerVoteState ::
  StandardHash blk =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasTargetVoteState blk 'Winner ->
  PerasTargetVoteState blk 'Winner
updateWinnerVoteState vote oldState =
  let newVoteTally = updateTargetVoteTally vote (ptvsVoteTally oldState)
      (PerasTargetVoteWinner _ extraCertCount cert) = oldState
   in PerasTargetVoteWinner newVoteTally (extraCertCount + 1) cert

-------------------------------------------------------------------------------

-- | Current vote state for a given round.
data PerasRoundVoteState blk
  = PerasRoundVoteStateQuorumNotReached
      { prvsRoundNo :: !PerasRoundNo
      , prvsCandidateStates :: !(Map (Point blk) (PerasTargetVoteState blk 'Candidate))
      }
  | PerasRoundVoteStateQuorumReachedAlready
      { prvsRoundNo :: !PerasRoundNo
      , prvsLoserStates :: !(Map (Point blk) (PerasTargetVoteState blk 'Loser))
      , prvsWinnerState :: !(PerasTargetVoteState blk 'Winner)
      }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

pattern PerasRoundVoteStateQuorumReachedNowWithCert ::
  ValidatedPerasCert blk -> PerasRoundVoteState blk
pattern PerasRoundVoteStateQuorumReachedNowWithCert cert <-
  PerasRoundVoteStateQuorumReachedAlready
    { prvsWinnerState = PerasTargetVoteWinner _ 0 cert
    }

instance HasPerasVoteRound (PerasRoundVoteState blk) where
  getPerasVoteRound = prvsRoundNo

-- | Get the certificate if quorum was reached for the given round.
prvsMaybeCert :: PerasRoundVoteState blk -> Maybe (ValidatedPerasCert blk)
prvsMaybeCert = \case
  PerasRoundVoteStateQuorumNotReached{} -> Nothing
  PerasRoundVoteStateQuorumReachedAlready{prvsWinnerState = PerasTargetVoteWinner _ _ cert} ->
    Just cert

freshRoundVoteState ::
  PerasRoundNo ->
  PerasRoundVoteState blk
freshRoundVoteState roundNo =
  PerasRoundVoteStateQuorumNotReached
    { prvsRoundNo = roundNo
    , prvsCandidateStates = Map.empty
    }

data UpdateRoundVoteStateError blk
  = RoundVoteStateLoserAboveQuorum (PerasTargetVoteState blk 'Winner) (PerasTargetVoteState blk 'Loser)
  | RoundVoteStateForgingCertError (PerasForgeErr blk)

-- | Add a vote to an existing round aggregate.
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
  if getPerasVoteRound vote /= getPerasVoteRound roundState
    then error "updatePerasRoundVoteTallys: vote round does not match aggregate round"
    else case roundState of
      PerasRoundVoteStateQuorumNotReached{prvsCandidateStates} ->
        let oldCandidateState =
              Map.findWithDefault
                (freshCandidateVoteState (getPerasVoteTarget vote))
                (getPerasVoteBlock vote)
                prvsCandidateStates
         in do
            candidateOrWinnerState <- updateCandidateVoteState cfg vote oldCandidateState
              `onErr` (\err -> RoundVoteStateForgingCertError err)
            case candidateOrWinnerState of
              ACandidate newCandidateState ->
                -- Quorum still not reached for this round
                let prvsCandidateStates' =
                      Map.insert
                        (getPerasVoteBlock vote)
                        newCandidateState
                        prvsCandidateStates
                 in pure $
                      PerasRoundVoteStateQuorumNotReached
                        { prvsRoundNo = prvsRoundNo roundState
                        , prvsCandidateStates = prvsCandidateStates'
                        }
              AWinner winnerState ->
                -- Quorum has been reached for the first time here for this round
                let winnerPoint = getPerasVoteBlock winnerState
                    loserStates = candidateToLoser cfg <$> Map.delete winnerPoint prvsCandidateStates
                 in pure $
                      PerasRoundVoteStateQuorumReachedAlready
                        { prvsRoundNo = prvsRoundNo roundState
                        , prvsLoserStates = loserStates
                        , prvsWinnerState = winnerState
                        }

      state@PerasRoundVoteStateQuorumReachedAlready{prvsLoserStates, prvsWinnerState} ->
        let votePoint = getPerasVoteBlock vote
            winnerPoint = getPerasVoteBlock prvsWinnerState

            updateMaybeLoser mState =
              updateLoserVoteState cfg vote (fromMaybe (freshLoserVoteState (getPerasVoteTarget vote)) mState)
                `onErr` (\err -> RoundVoteStateLoserAboveQuorum prvsWinnerState err)

         in if votePoint == winnerPoint
              then pure $ state{prvsWinnerState = updateWinnerVoteState vote prvsWinnerState}
              else do
                prvsLoserStates' <- Map.alterF
                  (\mState -> Just <$> updateMaybeLoser mState)
                  votePoint
                  prvsLoserStates
                pure $ state{prvsLoserStates = prvsLoserStates'}

-- | Updates the round vote states map with the given vote.
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
  -- We use the Functor instance of `Compose (Either e) ((,) s)` ≅ `λt. Either e (s, t)` in `Map.alterF`
  -- That way, we can return both the updated map and the updated leaf in one pass,
  -- and still handle errors.
  getCompose
    . Map.alterF
      (\mState -> Just <$> updateMaybeRoundState mState)
      (getPerasVoteRound vote)
 where
  updateMaybeRoundState ::
    Maybe (PerasRoundVoteState blk) ->
    Compose
      (Either (UpdateRoundVoteStateError blk))
      ((,) (PerasRoundVoteState blk))
      (PerasRoundVoteState blk)
  updateMaybeRoundState mRoundState = Compose $
    case updatePerasRoundVoteState
      vote
      cfg
      (fromMaybe (freshRoundVoteState (getPerasVoteRound vote)) mRoundState) of
      Left err -> Left err
      Right newRoundState -> Right (newRoundState, newRoundState)
