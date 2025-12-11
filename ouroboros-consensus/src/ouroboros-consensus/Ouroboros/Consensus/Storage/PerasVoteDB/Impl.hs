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
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

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

import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Data (Typeable)
import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (WithArrivalTime (forgetArrivalTime))
import Ouroboros.Consensus.Peras.Vote.Aggregation
import Ouroboros.Consensus.Storage.PerasVoteDB.API
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM

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
    let voteId = getPerasVoteId vote
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
                  (getPerasVoteBlock winnerState, vpvVoteStake (forgetArrivalTime vote))
                  (getPerasVoteBlock loserState, vpvVoteStake (forgetArrivalTime vote))
            Left (RoundVoteStateForgingCertError err) ->
              throwSTM $ ForgingCertError err
            Right
              ( PerasRoundVoteStateQuorumReachedNowWithCert cert
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
              (\set vote -> Set.delete (getPerasVoteId vote) set)
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
  { pvsVoteIds :: !(Set (PerasVoteId blk))
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
  | AddingPerasVote (PerasVoteTarget blk) (PerasVoteId blk) PerasVoteStake
  | AddedPerasVote (PerasVoteId blk)
  | IgnoredVoteAlreadyInDB (PerasVoteId blk)
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
