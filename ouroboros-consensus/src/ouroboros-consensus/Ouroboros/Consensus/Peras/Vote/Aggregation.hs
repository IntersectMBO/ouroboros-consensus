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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Peras vote aggregation and certificate forging
--
-- This module implements the core voting logic for the Peras protocol, which
-- aggregates weighted votes on chain blocks and forges certificates when
-- quorum is reached.
--
-- = Overview
--
-- In Peras, validators vote on specific blocks during designated voting rounds.
-- Each vote carries a weight, and votes are aggregated by:
--
--   * __Round__: each vote belongs to a specific 'PerasRoundNo'
--   * __Target__: within a round, votes are cast for different block 'Point's
--
-- As votes arrive, the system tracks the total weight backing each candidate
-- block. When one target accumulates enough weight to exceed the configured
-- quorum threshold, a certificate is automatically forged for that block,
-- making it a winner for that round.
--
-- = State Machine
--
-- For every round being voted for, the aggregation follows a state machine:
--
-- 1. __Quorum not reached__: multiple block targets are candidates, each
--    accumulating votes and weight. All targets compete to reach quorum first.
--
-- 2. __Quorum reached__: once a target reaches quorum, it becomes the winner
--    and a certificate is forged. All other targets become losers and continue
--    tracking votes without affecting the outcome.
--
-- = Quorum Threshold and Multiple Winners
--
-- The quorum threshold is parameterized via 'PerasParams'. Depending on this
-- configuration and the weight distribution, it may be theoretically possible
-- for multiple targets to exceed the threshold within the same round.
--
-- This module treats multiple winners as an error condition and rejects votes
-- that would cause this, raising instead a 'RoundVoteStateLoserAboveQuorum'
-- exception. This indicates that either:
--   * The quorum threshold is misconfigured, or that
--   * We were extremely unlucky when randomly selecting the voting committee.
--
-- With a correct threshold configuration (e.g., > 3/4 of total weight + a small
-- safety margin to account for an unlucky local sortition when selecting
-- non-persistent voters during committee selection), multiple winners should be
-- impossible given honest weight distribution.
--
-- = Key Types
--
--   * 'PerasRoundVoteState': tracks all voting activity for a single round, and
--      its logically split between separate 'NoQuorum' and 'Quorum' types
--      representing the two states (1) and (2) described above, respectively.
--   * 'PerasTargetVoteState': tracks votes for one specific block target
--   * 'PerasVoteCollection': raw vote count and weight accumulation
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
  , getPerasRoundVoteStateRound
  , getPerasRoundVoteStateCertMaybe
  , getPerasRoundVoteStateMaxTargetedSlot
  , pattern VoteGeneratedNewCert
  , pattern VoteDidntGenerateNewCert
  , updatePerasRoundVoteStates
  , UpdateRoundVoteStateError (..)
  , PerasTargetVoteState
  , getPerasTargetVoteStateTotalWeight
  , getPerasTargetVoteStateBlock
  , PerasVoteCollectionWithQuorum (..)
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
import Ouroboros.Consensus.BlockchainTime (WithArrivalTime)

{-------------------------------------------------------------------------------
  Voting state for a given Peras round
-------------------------------------------------------------------------------}

-- | Current vote state for a given round
data PerasRoundVoteState blk = PerasRoundVoteState
  { prvsRoundNo :: !PerasRoundNo
  , prvsState :: !(Either (NoQuorum blk) (Quorum blk))
  }

deriving instance
  ( StandardHash blk
  , Show (PerasVote blk)
  , Show (PerasCert blk)
  ) =>
  Show (PerasRoundVoteState blk)
deriving instance
  ( StandardHash blk
  , Eq (PerasVote blk)
  , Eq (PerasCert blk)
  ) =>
  Eq (PerasRoundVoteState blk)
deriving instance
  ( StandardHash blk
  , NoThunks (PerasVote blk)
  , NoThunks (PerasCert blk)
  ) =>
  NoThunks (PerasRoundVoteState blk)
deriving instance
  Generic (PerasRoundVoteState blk)

-- | Current vote state when a quorum has not yet been reached
data NoQuorum blk = NoQuorum
  { candidateStates :: !(Map (Point blk) (PerasTargetVoteState blk 'Candidate))
  }

deriving instance
  ( StandardHash blk
  , Show (PerasVote blk)
  , Show (PerasCert blk)
  ) =>
  Show (NoQuorum blk)
deriving instance
  ( StandardHash blk
  , Eq (PerasVote blk)
  , Eq (PerasCert blk)
  ) =>
  Eq (NoQuorum blk)
deriving instance
  ( StandardHash blk
  , NoThunks (PerasVote blk)
  , NoThunks (PerasCert blk)
  ) =>
  NoThunks (NoQuorum blk)
deriving instance
  Generic (NoQuorum blk)

-- | Current vote state when a quorum has been reached
data Quorum blk = Quorum
  { excessVotes :: !Word64
  , loserStates :: !(Map (Point blk) (PerasTargetVoteState blk 'Loser))
  , winnerState :: !(PerasTargetVoteState blk 'Winner)
  }

deriving instance
  ( StandardHash blk
  , Show (PerasVote blk)
  , Show (PerasCert blk)
  ) =>
  Show (Quorum blk)
deriving instance
  ( StandardHash blk
  , Eq (PerasVote blk)
  , Eq (PerasCert blk)
  ) =>
  Eq (Quorum blk)
deriving instance
  ( StandardHash blk
  , NoThunks (PerasVote blk)
  , NoThunks (PerasCert blk)
  ) =>
  NoThunks (Quorum blk)
deriving instance
  Generic (Quorum blk)

-- | Get the round number of a round vote state
getPerasRoundVoteStateRound :: PerasRoundVoteState blk -> PerasRoundNo
getPerasRoundVoteStateRound = prvsRoundNo

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

-- | Get the youngest (maximum) slot targeted by a vote in this round.
--
-- This is useful for garbage collection: a round voting data can be fully
-- collected only when its youngest targeted slot is strictly older than the
-- GC threshold.
getPerasRoundVoteStateMaxTargetedSlot ::
  PerasRoundVoteState blk ->
  WithOrigin SlotNo
getPerasRoundVoteStateMaxTargetedSlot PerasRoundVoteState{prvsState} =
  case prvsState of
    Left NoQuorum{candidateStates} ->
      maximumOrOrigin $ map pointSlot $ Map.keys candidateStates
    Right Quorum{winnerState, loserStates} ->
      maximumOrOrigin $
        pointSlot (getPerasTargetVoteStateBlock winnerState)
          : (pointSlot <$> Map.keys loserStates)
 where
  maximumOrOrigin [] = Origin
  maximumOrOrigin xs = maximum xs

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
      (PerasError blk)

-- | Add a vote to an existing round vote aggregate.
--
-- PRECONDITION: the vote's round must match the aggregate's round.
--
-- May fail if the state transition is invalid (e.g., a loser going above
-- quorum) or if forging the certificate fails.
updatePerasRoundVoteState ::
  forall blk.
  BlockSupportsPeras blk =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasParams ->
  PerasRoundVoteState blk ->
  Either (UpdateRoundVoteStateError blk) (PerasRoundVoteState blk)
updatePerasRoundVoteState vote params roundState =
  assert (getPerasVoteRound vote == getPerasRoundVoteStateRound roundState) $ do
    case roundState of
      -- Quorum not yet reached
      state@PerasRoundVoteState
        { prvsState =
          Left
            NoQuorum
              { candidateStates
              }
        } -> do
          let updateMaybeCandidateState = \case
                Nothing ->
                  candidateOrWinnerVoteStateSingleton params vote
                Just oldCandidateState ->
                  updateCandidateVoteState params vote oldCandidateState
          candidateOrWinnerState <-
            updateMaybeCandidateState (Map.lookup (getPerasVotePoint vote) candidateStates)
          case candidateOrWinnerState of
            RemainedCandidate newCandidateState -> do
              -- Quorum still not reached for this round
              let prvsCandidateStates' =
                    Map.insert
                      (getPerasVotePoint vote)
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
                    pvtBlock (pvcTarget (ptvsVoteCollection winnerState))
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
                getPerasVotePoint vote
              winnerPoint =
                pvtBlock (pvcTarget (ptvsVoteCollection winnerState))
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
              let updateMaybeLoserVoteState = \case
                    Nothing ->
                      loserVoteStateSingleton params winnerState vote
                    Just oldLoserState ->
                      updateLoserVoteState params winnerState vote oldLoserState

              loserStates' <-
                Map.alterF (\mState -> Just <$> updateMaybeLoserVoteState mState) votePoint loserStates
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
  BlockSupportsPeras blk =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasParams ->
  Map PerasRoundNo (PerasRoundVoteState blk) ->
  Either
    (UpdateRoundVoteStateError blk)
    (PerasRoundVoteState blk, Map PerasRoundNo (PerasRoundVoteState blk))
updatePerasRoundVoteStates vote params =
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
    newRoundState <- updatePerasRoundVoteState vote params roundState
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
    !(PerasVoteCollection blk) ->
    PerasTargetVoteState blk 'Candidate
  PerasTargetVoteLoser ::
    !(PerasVoteCollection blk) ->
    PerasTargetVoteState blk 'Loser
  PerasTargetVoteWinner ::
    !(PerasVoteCollection blk) ->
    !(ValidatedPerasCert blk) ->
    PerasTargetVoteState blk 'Winner

deriving stock instance
  ( Eq (PerasVoteCollection blk)
  , Eq (ValidatedPerasCert blk)
  ) =>
  Eq (PerasTargetVoteState blk status)

deriving stock instance
  ( Ord (PerasVoteCollection blk)
  , Ord (ValidatedPerasCert blk)
  ) =>
  Ord (PerasTargetVoteState blk status)

deriving stock instance
  ( Show (PerasVoteCollection blk)
  , Show (ValidatedPerasCert blk)
  ) =>
  Show (PerasTargetVoteState blk status)

instance
  ( NoThunks (PerasVoteCollection blk)
  , NoThunks (ValidatedPerasCert blk)
  ) =>
  NoThunks (PerasTargetVoteState blk status)
  where
  -- avoid the Generic-based default
  showTypeOf _ = "PerasTargetVoteState"

  -- we can just delegate wNoThunks to our custom noThunks
  wNoThunks = noThunks

  noThunks ctx (PerasTargetVoteCandidate voteCollection) =
    noThunks ctx voteCollection
  noThunks ctx (PerasTargetVoteLoser voteCollection) =
    noThunks ctx voteCollection
  noThunks ctx (PerasTargetVoteWinner voteCollection cert) =
    noThunks ctx (voteCollection, cert)

-- | Extract the total weight from a target vote state
getPerasTargetVoteStateTotalWeight :: PerasTargetVoteState blk status -> VoteWeight
getPerasTargetVoteStateTotalWeight = pvcTotalWeight . ptvsVoteCollection

-- | Extract the block point from a target vote state
getPerasTargetVoteStateBlock :: PerasTargetVoteState blk status -> Point blk
getPerasTargetVoteStateBlock = pvtBlock . pvcTarget . ptvsVoteCollection

-- | Extract the underlying vote voteCollection from a target vote state
ptvsVoteCollection :: PerasTargetVoteState blk status -> PerasVoteCollection blk
ptvsVoteCollection = \case
  PerasTargetVoteCandidate voteCollection -> voteCollection
  PerasTargetVoteLoser voteCollection -> voteCollection
  PerasTargetVoteWinner voteCollection _ -> voteCollection

candidateOrWinnerVoteStateSingleton ::
  BlockSupportsPeras blk =>
  PerasParams ->
  WithArrivalTime (ValidatedPerasVote blk) ->
  Either
    (UpdateRoundVoteStateError blk)
    (PerasVoteStateCandidateOrWinner blk)
candidateOrWinnerVoteStateSingleton params vote =
  let voteCollection = perasVoteCollectionSingleton vote
   in case perasVoteCollectionCheckQuorum params voteCollection of
        Just votesWithQuorum -> do
          cert <- forgePerasCert params votesWithQuorum `onErr` RoundVoteStateForgingCertError
          pure $ BecameWinner $ PerasTargetVoteWinner voteCollection cert
        Nothing ->
          pure $ RemainedCandidate $ PerasTargetVoteCandidate voteCollection

loserVoteStateSingleton ::
  IsPerasVote (PerasVote blk) blk =>
  PerasParams ->
  PerasTargetVoteState blk 'Winner ->
  WithArrivalTime (ValidatedPerasVote blk) ->
  Either (UpdateRoundVoteStateError blk) (PerasTargetVoteState blk 'Loser)
loserVoteStateSingleton params winnerState vote =
  let voteCollection = perasVoteCollectionSingleton vote
   in case perasVoteCollectionCheckQuorum params voteCollection of
        Just _ ->
          Left $ RoundVoteStateLoserAboveQuorum winnerState (PerasTargetVoteLoser voteCollection)
        Nothing ->
          Right $ PerasTargetVoteLoser voteCollection

-- | Convert a 'Candidate' state to a 'Loser' state.
--
-- This function is called on all candidates (except the winner) once a winner
-- is elected.
candidateToLoser ::
  PerasTargetVoteState blk 'Candidate ->
  PerasTargetVoteState blk 'Loser
candidateToLoser (PerasTargetVoteCandidate voteCollection) =
  PerasTargetVoteLoser voteCollection

-- | Subtype of 'PerasTargetVoteState' to indicate whether the target remains a
-- candidate or has been elected winner
data PerasVoteStateCandidateOrWinner blk
  = RemainedCandidate (PerasTargetVoteState blk 'Candidate)
  | BecameWinner (PerasTargetVoteState blk 'Winner)

-- | Add a vote to an existing target vote state if it isn't already present.
--
-- May fail if the candidate is elected winner but forging the certificate fails.
updateCandidateVoteState ::
  BlockSupportsPeras blk =>
  PerasParams ->
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasTargetVoteState blk 'Candidate ->
  Either
    (UpdateRoundVoteStateError blk)
    (PerasVoteStateCandidateOrWinner blk)
updateCandidateVoteState params vote oldState =
  let
    newVoteCollection = perasVoteCollectionAddVote vote (ptvsVoteCollection oldState)
   in
    case perasVoteCollectionCheckQuorum params newVoteCollection of
      Just votesWithQuorum -> do
        cert <- forgePerasCert params votesWithQuorum `onErr` RoundVoteStateForgingCertError
        pure $ BecameWinner (PerasTargetVoteWinner newVoteCollection cert)
      Nothing -> do
        pure $ RemainedCandidate (PerasTargetVoteCandidate newVoteCollection)

-- | Add a vote to an existing target vote state if it isn't already present.
--
-- PRECONDITION: the vote's target must match the underlying voteCollection's target.
--
-- May fail if the loser goes above quorum by adding the vote.
updateLoserVoteState ::
  ( StandardHash blk
  , IsPerasVote (PerasVote blk) blk
  ) =>
  PerasParams ->
  PerasTargetVoteState blk 'Winner ->
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasTargetVoteState blk 'Loser ->
  Either (UpdateRoundVoteStateError blk) (PerasTargetVoteState blk 'Loser)
updateLoserVoteState params winnerState vote oldState =
  assert (getPerasVoteTarget vote == pvcTarget (ptvsVoteCollection oldState)) $ do
    let newVoteCollection = perasVoteCollectionAddVote vote (ptvsVoteCollection oldState)
     in case perasVoteCollectionCheckQuorum params newVoteCollection of
          Just _ ->
            Left $ RoundVoteStateLoserAboveQuorum winnerState (PerasTargetVoteLoser newVoteCollection)
          Nothing -> Right $ PerasTargetVoteLoser newVoteCollection

-- | Add a vote to an existing target vote state if it isn't already present.
--
-- PRECONDITION: the vote's target must match the underlying voteCollection's target.
updateWinnerVoteState ::
  ( StandardHash blk
  , IsPerasVote (PerasVote blk) blk
  ) =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasTargetVoteState blk 'Winner ->
  PerasTargetVoteState blk 'Winner
updateWinnerVoteState vote oldState =
  assert (getPerasVoteTarget vote == pvcTarget (ptvsVoteCollection oldState)) $ do
    let newVoteCollection = perasVoteCollectionAddVote vote (ptvsVoteCollection oldState)
        (PerasTargetVoteWinner _ cert) = oldState
     in PerasTargetVoteWinner newVoteCollection cert

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Apply a function to the error part of an Either
onErr :: Either e a -> (e -> e') -> Either e' a
onErr (Left err) f = Left (f err)
onErr (Right val) _ = Right val
