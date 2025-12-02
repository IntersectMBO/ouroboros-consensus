{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Ouroboros.Consensus.Storage.PerasVoteDB.Impl
  ( -- * Opening
    PerasVoteDbArgs (..)
  , defaultArgs
  , openDB

    -- * Trace types
  , TraceEvent (..)

    -- * Exceptions
  , PerasVoteDbError (..)
  ) where

import Cardano.Prelude (Word64)
import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Data (Typeable)
import Data.Foldable qualified as Foldable
import Data.Functor.Compose (Compose (..))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (WithArrivalTime (forgetArrivalTime))
import Ouroboros.Consensus.Storage.PerasVoteDB.API
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM

-- | Tally of votes for a given target (round number and block point).
data PerasTargetVoteTally blk = PerasTargetVoteTally
  { ptvtTarget :: !(PerasVoteTarget blk)
  , ptvtVotes :: !(Map (IdOf (PerasVote blk)) (WithArrivalTime (ValidatedPerasVote blk)))
  , ptvtTotalStake :: !PerasVoteStake
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

instance HasPerasVoteTarget (PerasTargetVoteTally blk) blk where
  getPerasVoteTarget = ptvtTarget

instance HasPerasVoteRound (PerasTargetVoteTally blk) where
  getPerasVoteRound = fst . getPerasVoteTarget

instance HasPerasVoteVotedBlock (PerasTargetVoteTally blk) blk where
  getPerasVoteVotedBlock = snd . getPerasVoteTarget

freshTargetVoteTally :: PerasVoteTarget blk -> PerasTargetVoteTally blk
freshTargetVoteTally target =
  PerasTargetVoteTally
    { ptvtTarget = target
    , ptvtVotes = Map.empty
    , ptvtTotalStake = PerasVoteStake 0
    }

-- | Check whether the given target vote tally's stake is above quorum.
voteTallyAboveQuorum ::
  PerasCfg blk ->
  PerasTargetVoteTally blk ->
  Bool
voteTallyAboveQuorum PerasCfg{perasCfgQuorumThreshold} ptvt =
  ptvtTotalStake ptvt >= perasCfgQuorumThreshold

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
                (getId vote)
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

instance HasPerasVoteVotedBlock (PerasTargetVoteState blk status) blk where
  getPerasVoteVotedBlock = getPerasVoteVotedBlock . ptvsVoteTally

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
  PerasCfg blk ->
  PerasTargetVoteState blk 'Candidate ->
  PerasTargetVoteState blk 'Loser
candidateToLoser cfg (PerasTargetVoteCandidate tally) =
  if voteTallyAboveQuorum cfg tally
    then error "candidateToLoser: candidate is above quorum"
    else PerasTargetVoteLoser tally

-- | Add a vote to an existing target vote state if it isn't already present
-- PRECONDITION: the vote's target must match the underlying tally's target.
--
-- May fail if the candidate is elected winner but forging the certificate fails.
updateCandidateVoteState ::
  StandardHash blk =>
  PerasCfg blk ->
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasTargetVoteState blk 'Candidate ->
  Either
    (PerasForgeErr blk)
    (Either (PerasTargetVoteState blk 'Candidate) (PerasTargetVoteState blk 'Winner))
updateCandidateVoteState cfg vote oldState =
  let newVoteTally = updateTargetVoteTally vote (ptvsVoteTally oldState)
      voteList = forgetArrivalTime <$> Map.elems (ptvtVotes newVoteTally)
   in if voteTallyAboveQuorum cfg newVoteTally
        then case forgePerasCert cfg (ptvtTarget newVoteTally) voteList of
          Left err -> Left $ err
          Right cert -> Right $ Right $ PerasTargetVoteWinner newVoteTally 0 cert
        else Right $ Left $ PerasTargetVoteCandidate newVoteTally

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
        then Left $ PerasTargetVoteLoser newVoteTally
        else Right $ PerasTargetVoteLoser newVoteTally

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
                (getPerasVoteVotedBlock vote)
                prvsCandidateStates
         in case updateCandidateVoteState cfg vote oldCandidateState of
              Left err -> Left $ RoundVoteStateForgingCertError err
              Right (Left newCandidateState) ->
                let prvsCandidateStates' =
                      Map.insert
                        (getPerasVoteVotedBlock vote)
                        newCandidateState
                        prvsCandidateStates
                 in Right $
                      PerasRoundVoteStateQuorumNotReached
                        { prvsRoundNo = prvsRoundNo roundState
                        , prvsCandidateStates = prvsCandidateStates'
                        }
              Right (Right winnerState) ->
                let winnerPoint = getPerasVoteVotedBlock winnerState
                    loserStates = candidateToLoser cfg <$> (Map.delete winnerPoint prvsCandidateStates)
                 in Right $
                      PerasRoundVoteStateQuorumReachedAlready
                        { prvsRoundNo = prvsRoundNo roundState
                        , prvsLoserStates = loserStates
                        , prvsWinnerState = winnerState
                        }
      state@PerasRoundVoteStateQuorumReachedAlready{prvsLoserStates, prvsWinnerState} ->
        let votePoint = getPerasVoteVotedBlock vote
            winnerPoint = getPerasVoteVotedBlock prvsWinnerState

            updateMaybeLoser ::
              Maybe (PerasTargetVoteState blk 'Loser) ->
              Either (PerasTargetVoteState blk 'Loser) (PerasTargetVoteState blk 'Loser)
            updateMaybeLoser mState =
              updateLoserVoteState cfg vote (fromMaybe (freshLoserVoteState (getPerasVoteTarget vote)) mState)
         in if votePoint == winnerPoint
              then Right $ state{prvsWinnerState = updateWinnerVoteState vote prvsWinnerState}
              else case Map.alterF (\mState -> Just <$> updateMaybeLoser mState) votePoint prvsLoserStates of
                Left newLoserStateAboveQuorum ->
                  Left $ RoundVoteStateLoserAboveQuorum prvsWinnerState newLoserStateAboveQuorum
                Right prvsLoserStates' ->
                  Right $ state{prvsLoserStates = prvsLoserStates'}

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

{------------------------------------------------------------------------------
  Opening the database
------------------------------------------------------------------------------}

type PerasVoteDbArgs :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data PerasVoteDbArgs f m blk = PerasVoteDbArgs
  { pvdbaTracer :: Tracer m (TraceEvent blk)
  , pvdbaPerasCfg :: HKD f (PerasCfg blk)
  }

defaultArgs :: Applicative m => Incomplete PerasVoteDbArgs m blk
defaultArgs =
  PerasVoteDbArgs
    { pvdbaTracer = nullTracer
    , pvdbaPerasCfg = noDefault
    }

openDB ::
  forall m blk.
  ( IOLike m
  , StandardHash blk
  , Typeable blk
  ) =>
  Complete PerasVoteDbArgs m blk ->
  m (PerasVoteDB m blk)
openDB args@PerasVoteDbArgs{pvdbaPerasCfg} = do
  pvdbPerasVoteStateVar <-
    newTVarWithInvariantIO
      (either Just (const Nothing) . invariantForPerasVoteState)
      initialPerasVoteState
  let env =
        PerasVoteDbEnv
          { pvdbTracer
          , pvdbPerasVoteStateVar
          }
  h <- PerasVoteDbHandle <$> newTVarIO (PerasVoteDbOpen env)
  traceWith pvdbTracer OpenedPerasVoteDB
  pure
    PerasVoteDB
      { addVote = getEnv1 h (implAddVote pvdbaPerasCfg)
      , getVoteSnapshot = getEnvSTM h implGetVoteSnapshot
      , getForgedCertForRound = getEnvSTM1 h implForgedCertForRound
      , garbageCollect = getEnv1 h implGarbageCollect
      , closeDB = implCloseDB h
      }
 where
  PerasVoteDbArgs
    { pvdbaTracer = pvdbTracer
    } = args

{-------------------------------------------------------------------------------
  Database state
-------------------------------------------------------------------------------}

newtype PerasVoteDbHandle m blk = PerasVoteDbHandle (StrictTVar m (PerasVoteDbState m blk))

data PerasVoteDbState m blk
  = PerasVoteDbOpen !(PerasVoteDbEnv m blk)
  | PerasVoteDbClosed
  deriving stock Generic
  deriving anyclass NoThunks

data PerasVoteDbEnv m blk = PerasVoteDbEnv
  { pvdbTracer :: !(Tracer m (TraceEvent blk))
  , pvdbPerasVoteStateVar :: !(StrictTVar m (WithFingerprint (PerasVoteState blk)))
  -- ^ The 'RoundNo's of all votes currently in the db.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasVoteDbEnv" (PerasVoteDbEnv m blk)

getEnv ::
  forall m r blk.
  (IOLike m, HasCallStack, StandardHash blk, Typeable blk) =>
  PerasVoteDbHandle m blk ->
  (PerasVoteDbEnv m blk -> m r) ->
  m r
getEnv (PerasVoteDbHandle varState) f =
  readTVarIO varState >>= \case
    PerasVoteDbOpen env -> f env
    PerasVoteDbClosed -> throwIO $ ClosedDBError @blk prettyCallStack

getEnv1 ::
  (IOLike m, HasCallStack, StandardHash blk, Typeable blk) =>
  PerasVoteDbHandle m blk ->
  (PerasVoteDbEnv m blk -> a -> m r) ->
  a ->
  m r
getEnv1 h f a = getEnv h (\env -> f env a)

getEnvSTM ::
  forall m r blk.
  (IOLike m, HasCallStack, StandardHash blk, Typeable blk) =>
  PerasVoteDbHandle m blk ->
  (PerasVoteDbEnv m blk -> STM m r) ->
  STM m r
getEnvSTM (PerasVoteDbHandle varState) f =
  readTVar varState >>= \case
    PerasVoteDbOpen env -> f env
    PerasVoteDbClosed -> throwIO $ ClosedDBError @blk prettyCallStack

getEnvSTM1 ::
  (IOLike m, HasCallStack, StandardHash blk, Typeable blk) =>
  PerasVoteDbHandle m blk ->
  (PerasVoteDbEnv m blk -> a -> STM m r) ->
  a ->
  STM m r
getEnvSTM1 h f a = getEnvSTM h (\env -> f env a)

{-------------------------------------------------------------------------------
  API implementation
-------------------------------------------------------------------------------}

implCloseDB :: IOLike m => PerasVoteDbHandle m blk -> m ()
implCloseDB (PerasVoteDbHandle varState) =
  atomically (swapTVar varState PerasVoteDbClosed) >>= \case
    PerasVoteDbOpen PerasVoteDbEnv{pvdbTracer} -> do
      traceWith pvdbTracer ClosedPerasVoteDB
    -- DB was already closed.
    PerasVoteDbClosed -> pure ()

-- TODO: we will need to update this method with non-trivial validation logic
-- see https://github.com/tweag/cardano-peras/issues/120
implAddVote ::
  ( IOLike m
  , StandardHash blk
  , Typeable blk
  ) =>
  PerasCfg blk ->
  PerasVoteDbEnv m blk ->
  WithArrivalTime (ValidatedPerasVote blk) ->
  m (AddPerasVoteResult blk)
implAddVote
  perasCfg
  PerasVoteDbEnv
    { pvdbTracer
    , pvdbPerasVoteStateVar
    }
  vote = do
    let voteId = getId vote
        voteTarget = getPerasVoteTarget vote
        voteStake = vpvVoteStake (forgetArrivalTime vote)

    traceWith pvdbTracer $ AddingPerasVote voteTarget voteId voteStake

    res <- atomically $ do
      WithFingerprint
        PerasVoteState
          { pvsVoteIds
          , pvsRoundVoteStates
          , pvsVotesByTicket
          , pvsLastTicketNo
          }
        fp <-
        readTVar pvdbPerasVoteStateVar

      if Set.member voteId pvsVoteIds
        then pure PerasVoteAlreadyInDB
        else do
          let pvsVoteIds' = Set.insert voteId pvsVoteIds
              pvsLastTicketNo' = succ pvsLastTicketNo
              pvsVotesByTicket' = Map.insert pvsLastTicketNo' vote pvsVotesByTicket
              fp' = succ fp
          (res, pvsRoundVoteStates') <- case updatePerasRoundVoteStates vote perasCfg pvsRoundVoteStates of
            Left (RoundVoteStateLoserAboveQuorum winnerState loserState) ->
              throwSTM $
                EquivocatingCertError
                  (getPerasVoteRound vote)
                  (getPerasVoteVotedBlock winnerState, vpvVoteStake (forgetArrivalTime vote))
                  (getPerasVoteVotedBlock loserState, vpvVoteStake (forgetArrivalTime vote))
            Left (RoundVoteStateForgingCertError err) ->
              throwSTM $ ForgingCertError err
            Right
              ( PerasRoundVoteStateQuorumReachedAlready{prvsWinnerState = PerasTargetVoteWinner _ 0 cert}
                , pvsRoundVoteStates'
                ) ->
              pure (AddedPerasVoteAndGeneratedNewCert cert, pvsRoundVoteStates')
            Right (_, pvsRoundVoteStates') ->
              pure (AddedPerasVoteButDidntGenerateNewCert, pvsRoundVoteStates')
          writeTVar pvdbPerasVoteStateVar $
            WithFingerprint
              PerasVoteState
                { pvsVoteIds = pvsVoteIds'
                , pvsRoundVoteStates = pvsRoundVoteStates'
                , pvsVotesByTicket = pvsVotesByTicket'
                , pvsLastTicketNo = pvsLastTicketNo'
                }
              fp'
          pure res

    case res of
      PerasVoteAlreadyInDB -> traceWith pvdbTracer $ IgnoredVoteAlreadyInDB voteId
      AddedPerasVoteButDidntGenerateNewCert -> traceWith pvdbTracer $ AddedPerasVote voteId
      AddedPerasVoteAndGeneratedNewCert newCert -> do
        traceWith pvdbTracer $ AddedPerasVote voteId
        traceWith pvdbTracer $ GeneratedPerasCert newCert
    pure res

implGetVoteSnapshot ::
  IOLike m =>
  PerasVoteDbEnv m blk -> STM m (PerasVoteSnapshot blk)
implGetVoteSnapshot PerasVoteDbEnv{pvdbPerasVoteStateVar} = do
  PerasVoteState{pvsVoteIds, pvsVotesByTicket} <- forgetFingerprint <$> readTVar pvdbPerasVoteStateVar
  pure $
    PerasVoteSnapshot
      { containsVote = (`Set.member` pvsVoteIds)
      , getVotesAfter = \ticketNo ->
          snd $ Map.split ticketNo pvsVotesByTicket
      }

implForgedCertForRound ::
  IOLike m =>
  PerasVoteDbEnv m blk ->
  PerasRoundNo ->
  STM m (Maybe (ValidatedPerasCert blk))
implForgedCertForRound PerasVoteDbEnv{pvdbPerasVoteStateVar} roundNo = do
  PerasVoteState{pvsRoundVoteStates} <- forgetFingerprint <$> readTVar pvdbPerasVoteStateVar
  case Map.lookup roundNo pvsRoundVoteStates of
    Nothing -> pure Nothing
    Just aggr -> pure $ prvsMaybeCert aggr

implGarbageCollect ::
  forall m blk.
  IOLike m =>
  PerasVoteDbEnv m blk -> PerasRoundNo -> m ()
implGarbageCollect PerasVoteDbEnv{pvdbPerasVoteStateVar} roundNo =
  -- No need to update the 'Fingerprint' as we only remove votes that do
  -- not matter for comparing interesting chains.
  atomically $ modifyTVar pvdbPerasVoteStateVar (fmap gc)
 where
  gc :: PerasVoteState blk -> PerasVoteState blk
  gc
    PerasVoteState
      { pvsVoteIds
      , pvsRoundVoteStates
      , pvsVotesByTicket
      , pvsLastTicketNo
      } =
      let pvsRoundVoteStates' =
            Map.filterWithKey
              (\rNo _ -> rNo >= roundNo)
              pvsRoundVoteStates
          (pvsVotesByTicket', votesToRemove) = Map.partition (\vote -> getPerasVoteRound vote >= roundNo) pvsVotesByTicket
          pvsVoteIds' =
            Foldable.foldl'
              (\set vote -> Set.delete (getId vote) set)
              pvsVoteIds
              votesToRemove
       in PerasVoteState
            { pvsVoteIds = pvsVoteIds'
            , pvsRoundVoteStates = pvsRoundVoteStates'
            , pvsVotesByTicket = pvsVotesByTicket'
            , pvsLastTicketNo = pvsLastTicketNo
            }

{-------------------------------------------------------------------------------
  Implementation-internal types
-------------------------------------------------------------------------------}

data PerasVoteState blk = PerasVoteState
  { pvsVoteIds :: !(Set (IdOf (PerasVote blk)))
  , pvsRoundVoteStates :: !(Map PerasRoundNo (PerasRoundVoteState blk))
  , pvsVotesByTicket :: !(Map PerasVoteTicketNo (WithArrivalTime (ValidatedPerasVote blk)))
  -- ^ The votes by 'PerasVoteTicketNo'.
  --
  -- INVARIANT: In sync with 'pvsRoundVoteStates'.
  , pvsLastTicketNo :: !PerasVoteTicketNo
  -- ^ The most recent 'PerasVoteTicketNo' (or 'zeroPerasVoteTicketNo'
  -- otherwise).
  }
  deriving stock (Show, Generic)
  deriving anyclass NoThunks

initialPerasVoteState :: WithFingerprint (PerasVoteState blk)
initialPerasVoteState =
  WithFingerprint
    PerasVoteState
      { pvsVoteIds = Set.empty
      , pvsRoundVoteStates = Map.empty
      , pvsVotesByTicket = Map.empty
      , pvsLastTicketNo = zeroPerasVoteTicketNo
      }
    (Fingerprint 0)

-- | Check that the fields of 'PerasVoteState' are in sync.
invariantForPerasVoteState ::
  WithFingerprint (PerasVoteState blk) -> Either String ()
invariantForPerasVoteState _pvs =
  -- TODO
  pure ()

{-------------------------------------------------------------------------------
  Trace types
-------------------------------------------------------------------------------}

data TraceEvent blk
  = OpenedPerasVoteDB
  | ClosedPerasVoteDB
  | AddingPerasVote (PerasVoteTarget blk) (IdOf (PerasVote blk)) PerasVoteStake
  | AddedPerasVote (IdOf (PerasVote blk))
  | IgnoredVoteAlreadyInDB (IdOf (PerasVote blk))
  | GeneratedPerasCert (ValidatedPerasCert blk)
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

data PerasVoteDbError blk
  = ClosedDBError PrettyCallStack
  | EquivocatingCertError PerasRoundNo (Point blk, PerasVoteStake) (Point blk, PerasVoteStake)
  | ForgingCertError (PerasForgeErr blk)
  deriving stock Show
  deriving anyclass Exception
