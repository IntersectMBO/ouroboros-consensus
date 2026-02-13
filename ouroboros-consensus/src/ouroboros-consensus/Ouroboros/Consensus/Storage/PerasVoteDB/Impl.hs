{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Ouroboros.Consensus.Storage.PerasVoteDB.Impl
  ( -- * Opening
    PerasVoteDbArgs (..)
  , defaultArgs
  , openDB

    -- * Trace types
  , TraceEvent (..)

    -- * Exceptions
  , ExistingPerasRoundWinner (..)
  , BlockedPerasRoundWinner (..)
  , PerasVoteDbError (..)
  ) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Data (Typeable)
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
import Ouroboros.Consensus.Peras.Vote.Aggregation
import Ouroboros.Consensus.Storage.PerasVoteDB.API
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM

{-------------------------------------------------------------------------------
  Utilitites for STMWithTracing type
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Database state
-------------------------------------------------------------------------------}

type PerasVoteDbHandle m blk = StrictTVar m (WithFingerprint (PerasVoteDbState blk))

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
  deriving stock (Show, Generic)
  deriving anyclass NoThunks

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
  WithFingerprint (PerasVoteDbState blk) -> Either String ()
invariantForPerasVoteDbState pvs = do
  for_ (Map.toList pvdsRoundVoteStates) $ \(roundNo, prvs) ->
    checkEqual "pvcRoundVoteStates rounds" roundNo (getPerasVoteRound prvs)
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
  Exceptions
-------------------------------------------------------------------------------}

newtype ExistingPerasRoundWinner blk
  = ExistingPerasRoundWinner (Point blk, PerasVoteStake)
  deriving stock (Show, Eq)

newtype BlockedPerasRoundWinner blk
  = BlockedPerasRoundWinner (Point blk, PerasVoteStake)
  deriving stock (Show, Eq)

data PerasVoteDbError blk
  = -- | Attempted to add a vote that would lead to multiple winners for the
    -- same round
    MultipleWinnersInRound
      PerasRoundNo
      (ExistingPerasRoundWinner blk)
      (BlockedPerasRoundWinner blk)
  | -- | An error occurred while forging a certificate
    ForgingCertError (PerasForgeErr blk)
  deriving stock Show
  deriving anyclass Exception

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
openDB PerasVoteDbArgs{pvdbaPerasCfg, pvdbaTracer} = do
  perasVoteDbHandle <-
    newTVarWithInvariantIO
      (either Just (const Nothing) . invariantForPerasVoteDbState)
      initialPerasVoteDbState
  traceWith pvdbaTracer OpenedPerasVoteDB
  pure
    PerasVoteDB
      { atomicallyWithTracing = \(STMWithTracing action) -> do
          (events, res) <- atomically action
          mapM_ (traceWith pvdbaTracer) events
          pure res
      , addVote = implAddVote pvdbaPerasCfg perasVoteDbHandle
      , getVoteIds = implGetVoteIds perasVoteDbHandle
      , getVotesAfter = implGetVotesAfter perasVoteDbHandle
      , getForgedCertForRound = implGetForgedCertForRound perasVoteDbHandle
      , garbageCollect = implGarbageCollect perasVoteDbHandle
      }

{-------------------------------------------------------------------------------
  API implementation
-------------------------------------------------------------------------------}

-- TODO: we will need to update this method with non-trivial validation logic
-- see https://github.com/tweag/cardano-peras/issues/120
implAddVote ::
  ( IOLike m
  , StandardHash blk
  , Typeable blk
  ) =>
  PerasCfg blk ->
  PerasVoteDbHandle m blk ->
  WithArrivalTime (ValidatedPerasVote blk) ->
  STMWithTracing (TraceEvent blk) m (AddPerasVoteResult blk)
implAddVote perasCfg perasVoteDbHandle vote = STMWithTracing $ do
  let voteId = getPerasVoteId vote
      voteTarget = getPerasVoteTarget vote
      voteStake = vpvVoteStake (forgetArrivalTime vote)

  let traceEvents = [AddingPerasVote voteTarget voteId voteStake]

  addPerasVoteRes <- do
    WithFingerprint pvds fp <- readTVar perasVoteDbHandle
    (res, pvds') <- addOrIgnoreVote pvds voteId
    writeTVar perasVoteDbHandle (WithFingerprint pvds' (succ fp))
    pure res

  let traceEvents' =
        traceEvents ++ case addPerasVoteRes of
          PerasVoteAlreadyInDB -> [IgnoredVoteAlreadyInDB voteId]
          AddedPerasVoteButDidntGenerateNewCert -> [AddedPerasVote voteId]
          AddedPerasVoteAndGeneratedNewCert newCert -> [AddedPerasVote voteId, GeneratedPerasCert newCert]

  pure (traceEvents', addPerasVoteRes)
 where
  addOrIgnoreVote pvds voteId
    -- Vote is already in the DB => ignore it
    | Set.member voteId (pvdsVoteIds pvds) = voteAlreadyInDB pvds
    -- New vote => try to add it to the DB
    | otherwise = tryAddVote pvds voteId

  voteAlreadyInDB pvds = pure (PerasVoteAlreadyInDB, pvds)

  tryAddVote pvds voteId = do
    let pvsVoteIds' = Set.insert voteId (pvdsVoteIds pvds)
        pvsLastTicketNo' = succ (pvdsLastTicketNo pvds)
        pvsVotesByTicket' = Map.insert pvsLastTicketNo' vote (pvdsVotesByTicket pvds)

    (addPerasVoteRes, pvsRoundVoteStates') <-
      case updatePerasRoundVoteStates vote perasCfg (pvdsRoundVoteStates pvds) of
        -- Added vote and reached a quorum, forging a new certificate
        Right (VoteGeneratedNewCert cert, pvsRoundVoteStates') -> pure (AddedPerasVoteAndGeneratedNewCert cert, pvsRoundVoteStates')
        -- Added vote but did not generate a new certificate, either
        -- because quorum was not reached yet, or because this vote was
        -- cast upon a target that had already won so a certificate was
        -- forged in a previous step.
        Right (VoteDidntGenerateNewCert, pvsRoundVoteStates') -> pure (AddedPerasVoteButDidntGenerateNewCert, pvsRoundVoteStates')
        -- Adding the vote led to more than one winner => internal error
        Left (RoundVoteStateLoserAboveQuorum winnerState loserState) ->
          throwSTM $
            MultipleWinnersInRound
              (getPerasVoteRound vote)
              ( ExistingPerasRoundWinner
                  ( getPerasVoteBlock winnerState
                  , ptvsTotalStake winnerState
                  )
              )
              ( BlockedPerasRoundWinner
                  ( getPerasVoteBlock loserState
                  , ptvsTotalStake loserState
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
  PerasVoteDbHandle m blk ->
  STMWithTracing (TraceEvent blk) m (Set (PerasVoteId blk))
implGetVoteIds perasVoteDbHandle = STMWithTracing $ do
  PerasVoteDbState{pvdsVoteIds} <-
    forgetFingerprint <$> readTVar perasVoteDbHandle
  pure ([], pvdsVoteIds)

implGetVotesAfter ::
  IOLike m =>
  PerasVoteDbHandle m blk ->
  PerasVoteTicketNo ->
  STMWithTracing (TraceEvent blk) m (Map PerasVoteTicketNo (WithArrivalTime (ValidatedPerasVote blk)))
implGetVotesAfter perasVoteDbHandle ticketNo = STMWithTracing $ do
  PerasVoteDbState{pvdsVotesByTicket} <-
    forgetFingerprint <$> readTVar perasVoteDbHandle
  pure ([], snd $ Map.split ticketNo pvdsVotesByTicket)

implGetForgedCertForRound ::
  IOLike m =>
  PerasVoteDbHandle m blk ->
  PerasRoundNo ->
  STMWithTracing (TraceEvent blk) m (Maybe (ValidatedPerasCert blk))
implGetForgedCertForRound perasVoteDbHandle roundNo = STMWithTracing $ do
  PerasVoteDbState{pvdsRoundVoteStates} <-
    forgetFingerprint <$> readTVar perasVoteDbHandle
  case Map.lookup roundNo pvdsRoundVoteStates of
    Nothing -> pure ([], Nothing)
    Just aggr -> pure ([], getPerasRoundVoteStateCertMaybe aggr)

implGarbageCollect ::
  forall m blk.
  IOLike m =>
  PerasVoteDbHandle m blk -> PerasRoundNo -> STMWithTracing (TraceEvent blk) m ()
implGarbageCollect perasVoteDbHandle roundNo = STMWithTracing $ do
  -- No need to update the 'Fingerprint' as we only remove votes that do
  -- not matter for comparing interesting chains.
  newState <- modifyTVar perasVoteDbHandle (fmap gc)
  pure ([GarbageCollected roundNo], newState)
 where
  gc :: PerasVoteDbState blk -> PerasVoteDbState blk
  gc
    PerasVoteDbState
      { pvdsVoteIds
      , pvdsRoundVoteStates
      , pvdsVotesByTicket
      , pvdsLastTicketNo
      } =
      let pvsRoundVoteStates' =
            Map.filterWithKey
              (\rNo _ -> rNo >= roundNo)
              pvdsRoundVoteStates
          (pvsVotesByTicket', votesToRemove) =
            Map.partition
              (\vote -> getPerasVoteRound vote >= roundNo)
              pvdsVotesByTicket
          pvsVoteIds' =
            Foldable.foldl'
              (\set vote -> Set.delete (getPerasVoteId vote) set)
              pvdsVoteIds
              votesToRemove
       in PerasVoteDbState
            { pvdsVoteIds = pvsVoteIds'
            , pvdsRoundVoteStates = pvsRoundVoteStates'
            , pvdsVotesByTicket = pvsVotesByTicket'
            , pvdsLastTicketNo = pvdsLastTicketNo
            }
