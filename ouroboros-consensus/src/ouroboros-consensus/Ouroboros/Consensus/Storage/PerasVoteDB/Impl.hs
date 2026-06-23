{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.PerasVoteDB.Impl
  ( -- * Opening
    PerasVoteDbArgs (..)
  , defaultArgs
  , createDB

    -- * Trace types
  , TraceEvent (..)
  ) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Foldable (for_)
import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (WithArrivalTime (..))
import Ouroboros.Consensus.Peras.Context
import Ouroboros.Consensus.Peras.Vote.Aggregation
import Ouroboros.Consensus.Storage.PerasVoteDB.API
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM

{-------------------------------------------------------------------------------
  Database state
-------------------------------------------------------------------------------}

data PerasVoteDbEnv m blk = PerasVoteDbEnv
  { pvdeTracer :: !(Tracer m (TraceEvent blk))
  , pvdeState :: !(StrictTVar m (WithFingerprint (PerasVoteDbState blk)))
  -- ^ The 'RoundNo's of all votes currently in the db.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasVoteDbEnv" (PerasVoteDbEnv m blk)

-- INVARIANT: See 'invariantForPerasVoteDbState'.
data PerasVoteDbState blk = PerasVoteDbState
  { pvdsVoteIds :: !(Set (PerasVoteId blk))
  , pvdsRoundVoteStates :: !(Map PerasRoundNo (PerasRoundVoteState blk))
  , pvdsVotesByTicket :: !(Map PerasVoteTicketNo (WithArrivalTime (ValidatedPerasVote blk)))
  -- ^ The votes by 'PerasVoteTicketNo'.
  --
  -- INVARIANT: In sync with 'pvsRoundVoteStates'.
  , pvdsLastTicketNo :: !PerasVoteTicketNo
  -- ^ The most recent 'PerasVoteTicketNo' (or 'zeroPerasVoteTicketNo' otherwise).
  }

deriving instance
  ( StandardHash blk
  , Show (PerasVote blk)
  , Show (PerasCert blk)
  ) =>
  Show (PerasVoteDbState blk)
deriving instance
  ( StandardHash blk
  , Eq (PerasVote blk)
  , Eq (PerasCert blk)
  ) =>
  Eq (PerasVoteDbState blk)
deriving instance
  ( StandardHash blk
  , NoThunks (PerasVote blk)
  , NoThunks (PerasCert blk)
  ) =>
  NoThunks (PerasVoteDbState blk)
deriving instance
  Generic (PerasVoteDbState blk)

initialPerasVoteDbState :: WithFingerprint (PerasVoteDbState blk)
initialPerasVoteDbState =
  WithFingerprint
    PerasVoteDbState
      { pvdsVoteIds = Set.empty
      , pvdsRoundVoteStates = Map.empty
      , pvdsVotesByTicket = Map.empty
      , pvdsLastTicketNo = zeroPerasVoteTicketNo
      }
    (Fingerprint 0)

-- | Check that the fields of 'PerasVoteState' are in sync.
invariantForPerasVoteDbState ::
  IsPerasVote (PerasVote blk) blk =>
  WithFingerprint (PerasVoteDbState blk) -> Either String ()
invariantForPerasVoteDbState pvs = do
  for_ (Map.toList pvdsRoundVoteStates) $ \(roundNo, prvs) ->
    checkEqual
      "pvcRoundVoteStates rounds"
      roundNo
      (getPerasRoundVoteStateRound prvs)
  checkEqual
    "pvcsVotesByTicket"
    (Set.fromList (getPerasVoteRound <$> Map.elems pvdsVotesByTicket))
    (Set.fromList (pviRoundNo <$> Set.elems pvdsVoteIds))
  for_ (Map.keys pvdsVotesByTicket) $ \ticketNo ->
    when (ticketNo > pvdsLastTicketNo) $
      throwError $
        "Ticket number monotonicity violation: "
          <> show ticketNo
          <> " > "
          <> show pvdsLastTicketNo
 where
  PerasVoteDbState
    { pvdsRoundVoteStates
    , pvdsVotesByTicket
    , pvdsVoteIds
    , pvdsLastTicketNo
    } = forgetFingerprint pvs

  checkEqual :: (Eq a, Show a) => String -> a -> a -> Either String ()
  checkEqual msg a b =
    when (a /= b) $ throwError $ msg <> ": Not equal: " <> show a <> ", " <> show b

{-------------------------------------------------------------------------------
  Trace types
-------------------------------------------------------------------------------}

data TraceEvent blk
  = AddVote
      (PerasVoteId blk)
      (WithArrivalTime (ValidatedPerasVote blk))
      (AddPerasVoteResult blk)
  | GarbageCollected
      SlotNo

deriving instance
  ( Show (PerasVoteId blk)
  , Show (ValidatedPerasVote blk)
  , Show (AddPerasVoteResult blk)
  ) =>
  Show (TraceEvent blk)
deriving instance
  ( Eq (PerasVoteId blk)
  , Eq (ValidatedPerasVote blk)
  , Eq (AddPerasVoteResult blk)
  ) =>
  Eq (TraceEvent blk)
deriving instance
  ( NoThunks (PerasVoteId blk)
  , NoThunks (ValidatedPerasVote blk)
  , NoThunks (AddPerasVoteResult blk)
  ) =>
  NoThunks (TraceEvent blk)
deriving instance
  Generic (TraceEvent blk)

{------------------------------------------------------------------------------
  Creating the database
------------------------------------------------------------------------------}

type PerasVoteDbArgs :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data PerasVoteDbArgs f m blk = PerasVoteDbArgs
  { pvdbaTracer :: Tracer m (TraceEvent blk)
  , pvdbaPerasEpochContextResolverHandle :: HKD f (PerasEpochContextResolverHandle m blk)
  }

defaultArgs :: Monad m => Incomplete PerasVoteDbArgs m blk
defaultArgs =
  PerasVoteDbArgs
    { pvdbaTracer = nullTracer
    , pvdbaPerasEpochContextResolverHandle = noDefault
    }

createDB ::
  forall m blk.
  ( IOLike m
  , BlockSupportsPeras blk
  , StateSupportsPerasEpochContext blk
  ) =>
  Complete PerasVoteDbArgs m blk ->
  m (PerasVoteDB m blk)
createDB args@PerasVoteDbArgs{pvdbaPerasEpochContextResolverHandle} = do
  pvdeState <-
    newTVarWithInvariantIO
      (either Just (const Nothing) . invariantForPerasVoteDbState)
      initialPerasVoteDbState
  let env =
        PerasVoteDbEnv
          { pvdeTracer
          , pvdeState
          }
  pure
    PerasVoteDB
      { addVote = implAddVote pvdbaPerasEpochContextResolverHandle env
      , getVoteIds = implGetVoteIds env
      , getVotesAfter = implGetVotesAfter env
      , getForgedCertForRound = implGetForgedCertForRound env
      , garbageCollect = implGarbageCollect env
      }
 where
  PerasVoteDbArgs
    { pvdbaTracer = pvdeTracer
    } = args

{-------------------------------------------------------------------------------
  API implementation
-------------------------------------------------------------------------------}

-- TODO: we will need to update this method with non-trivial validation logic
-- see https://github.com/tweag/cardano-peras/issues/120
implAddVote ::
  ( IOLike m
  , BlockSupportsPeras blk
  , StateSupportsPerasEpochContext blk
  ) =>
  PerasEpochContextResolverHandle m blk ->
  PerasVoteDbEnv m blk ->
  WithArrivalTime (ValidatedPerasVote blk) ->
  STM m (m (AddPerasVoteResult blk))
implAddVote resolverHandle PerasVoteDbEnv{pvdeTracer, pvdeState} vote = do
  let voteId = getPerasVoteId vote
  addPerasVoteRes <- do
    WithFingerprint pvds fp <- readTVar pvdeState
    (res, pvds') <- addOrIgnoreVote pvds voteId
    writeTVar pvdeState (WithFingerprint pvds' (succ fp))
    pure res
  pure $ do
    traceWith pvdeTracer (AddVote voteId vote addPerasVoteRes)
    return addPerasVoteRes
 where
  addOrIgnoreVote pvds voteId
    -- Vote is already in the DB => ignore it
    | Set.member voteId (pvdsVoteIds pvds) = voteAlreadyInDB pvds
    -- New vote => try to add it to the DB
    | otherwise = tryAddVote pvds voteId

  voteAlreadyInDB pvds = pure (PerasVoteAlreadyInDB, pvds)

  tryAddVote pvds voteId = do
    -- We need to get the 'PerasEpochContext' corresponding to the vote 'PerasRoundNo'
    epochContext <-
      either throwSTM pure =<< resolveRoundNoWithHandle resolverHandle (getPerasVoteRound vote)

    let pvsVoteIds' = Set.insert voteId (pvdsVoteIds pvds)
        pvsLastTicketNo' = succ (pvdsLastTicketNo pvds)
        pvsVotesByTicket' = Map.insert pvsLastTicketNo' vote (pvdsVotesByTicket pvds)

    (addPerasVoteRes, pvsRoundVoteStates') <-
      case updatePerasRoundVoteStates vote epochContext (pvdsRoundVoteStates pvds) of
        -- Added vote and reached a quorum, forging a new certificate
        Right (VoteGeneratedNewCert cert, pvsRoundVoteStates') ->
          pure (AddedPerasVoteAndGeneratedNewCert cert, pvsRoundVoteStates')
        -- Added vote but did not generate a new certificate, either
        -- because quorum was not reached yet, or because this vote was
        -- cast upon a target that had already won so a certificate was
        -- forged in a previous step.
        Right (VoteDidntGenerateNewCert, pvsRoundVoteStates') ->
          pure (AddedPerasVoteButDidntGenerateNewCert, pvsRoundVoteStates')
        -- Adding the vote led to more than one winner => internal error
        Left (RoundVoteStateLoserAboveQuorum winnerState loserState) ->
          throwSTM $
            MultipleWinnersInRound
              (getPerasVoteRound vote)
              ( ExistingPerasRoundWinner
                  ( getPerasTargetVoteStateBlock winnerState
                  , getPerasTargetVoteStateTotalWeight winnerState
                  )
              )
              ( BlockedPerasRoundWinner
                  ( getPerasTargetVoteStateBlock loserState
                  , getPerasTargetVoteStateTotalWeight loserState
                  )
              )
        -- Reached quorum but failed to forge a certificate
        Left (RoundVoteStateForgingCertError forgeErr) ->
          throwSTM $
            ForgingCertError forgeErr

    pure
      ( addPerasVoteRes
      , PerasVoteDbState
          { pvdsVoteIds = pvsVoteIds'
          , pvdsRoundVoteStates = pvsRoundVoteStates'
          , pvdsVotesByTicket = pvsVotesByTicket'
          , pvdsLastTicketNo = pvsLastTicketNo'
          }
      )

implGetVoteIds ::
  IOLike m =>
  PerasVoteDbEnv m blk ->
  STM m (Set (PerasVoteId blk))
implGetVoteIds PerasVoteDbEnv{pvdeState} = do
  PerasVoteDbState{pvdsVoteIds} <-
    forgetFingerprint <$> readTVar pvdeState
  pure pvdsVoteIds

implGetVotesAfter ::
  IOLike m =>
  PerasVoteDbEnv m blk ->
  PerasVoteTicketNo ->
  STM m (Map PerasVoteTicketNo (WithArrivalTime (ValidatedPerasVote blk)))
implGetVotesAfter PerasVoteDbEnv{pvdeState} ticketNo = do
  PerasVoteDbState{pvdsVotesByTicket} <-
    forgetFingerprint <$> readTVar pvdeState
  pure $ snd $ Map.split ticketNo pvdsVotesByTicket

implGetForgedCertForRound ::
  IOLike m =>
  PerasVoteDbEnv m blk ->
  PerasRoundNo ->
  STM m (Maybe (ValidatedPerasCert blk))
implGetForgedCertForRound PerasVoteDbEnv{pvdeState} roundNo = do
  PerasVoteDbState{pvdsRoundVoteStates} <-
    forgetFingerprint <$> readTVar pvdeState
  case Map.lookup roundNo pvdsRoundVoteStates of
    Nothing -> pure Nothing
    Just aggr -> pure (getPerasRoundVoteStateCertMaybe aggr)

implGarbageCollect ::
  forall m blk.
  ( IOLike m
  , IsPerasVote (PerasVote blk) blk
  ) =>
  PerasVoteDbEnv m blk ->
  SlotNo ->
  STM m (m ())
implGarbageCollect PerasVoteDbEnv{pvdeTracer, pvdeState} slotNo = do
  -- No need to update the 'Fingerprint' as we only remove votes that do
  -- not matter for comparing interesting chains.
  modifyTVar pvdeState (fmap gc)
  pure $ do
    traceWith pvdeTracer (GarbageCollected slotNo)
    return ()
 where
  gc :: PerasVoteDbState blk -> PerasVoteDbState blk
  gc
    PerasVoteDbState
      { pvdsVoteIds
      , pvdsRoundVoteStates
      , pvdsVotesByTicket
      , pvdsLastTicketNo
      } =
      let
        -- First, determine which rounds to delete based on the round vote
        -- state: a round is deleted only when the youngest target of all its
        -- votes is strictly older than the GC threshold.
        --
        -- NOTE:
        -- This conservative approach could cause round states to be kept
        -- for a long time if an attacker keeps adding votes for a given
        -- round but with a target far into the future,
        -- see https://github.com/tweag/cardano-peras/issues/218
        (roundsToDelete, pvsRoundVoteStates') =
          Map.partition
            (\rvs -> getPerasRoundVoteStateMaxTargetedSlot rvs < NotOrigin slotNo)
            pvdsRoundVoteStates
        deletedRoundNos =
          Map.keysSet roundsToDelete
        -- Then, remove all votes belonging to deleted rounds from the
        -- by-ticket index
        (pvsVotesByTicket', votesToRemove) =
          Map.partition
            (\vote -> not (Set.member (getPerasVoteRound vote) deletedRoundNos))
            pvdsVotesByTicket
        -- Finally, remove the corresponding ids from the set of vote ids
        pvsVoteIds' =
          Foldable.foldl'
            (\set vote -> Set.delete (getPerasVoteId vote) set)
            pvdsVoteIds
            votesToRemove
       in
        PerasVoteDbState
          { pvdsVoteIds = pvsVoteIds'
          , pvdsRoundVoteStates = pvsRoundVoteStates'
          , pvdsVotesByTicket = pvsVotesByTicket'
          , pvdsLastTicketNo = pvdsLastTicketNo
          }
