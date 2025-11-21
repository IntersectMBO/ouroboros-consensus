{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

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

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Foldable (for_)
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block hiding (UpdatePerasVoteAggregateResult (..))
import Ouroboros.Consensus.Block.SupportsPeras qualified as UPVAR
import Ouroboros.Consensus.BlockchainTime (WithArrivalTime (forgetArrivalTime))
import Ouroboros.Consensus.Peras.Weight
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
  }

defaultArgs :: Applicative m => Incomplete PerasVoteDbArgs m blk
defaultArgs =
  PerasVoteDbArgs
    { pvdbaTracer = nullTracer
    }

openDB ::
  forall m blk.
  ( IOLike m
  , StandardHash blk
  ) =>
  Complete PerasVoteDbArgs m blk ->
  m (PerasVoteDB m blk)
openDB args = do
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
      { addVote = getEnv1 h implAddVote
      , getStakeSnapshot = getEnvSTM h implGetStakeSnapshot
      , getVoteSnapshot = getEnvSTM h implGetVoteSnapshot
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
  (IOLike m, HasCallStack) =>
  PerasVoteDbHandle m blk ->
  (PerasVoteDbEnv m blk -> m r) ->
  m r
getEnv (PerasVoteDbHandle varState) f =
  readTVarIO varState >>= \case
    PerasVoteDbOpen env -> f env
    PerasVoteDbClosed -> throwIO $ ClosedDBError prettyCallStack

getEnv1 ::
  (IOLike m, HasCallStack) =>
  PerasVoteDbHandle m blk ->
  (PerasVoteDbEnv m blk -> a -> m r) ->
  a ->
  m r
getEnv1 h f a = getEnv h (\env -> f env a)

getEnvSTM ::
  (IOLike m, HasCallStack) =>
  PerasVoteDbHandle m blk ->
  (PerasVoteDbEnv m blk -> STM m r) ->
  STM m r
getEnvSTM (PerasVoteDbHandle varState) f =
  readTVar varState >>= \case
    PerasVoteDbOpen env -> f env
    PerasVoteDbClosed -> throwIO $ ClosedDBError prettyCallStack

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
  ) =>
  PerasVoteDbEnv m blk ->
  WithArrivalTime (ValidatedPerasVote blk) ->
  m (AddPerasVoteResult blk)
implAddVote env vote = do
  traceWith pvdbTracer $ AddingPerasVote voteTarget voteId voteStake

  res <- atomically $ do
    WithFingerprint
      PerasVoteState
        { pvsVoteIds
        , pvsVotesByTarget
        , pvsVotesByTicket
        , pvsLastTicketNo
        }
      fp <-
      readTVar pvdbPerasVoteStateVar

    if Set.member voteId (pvsVoteIds)
      then pure PerasVoteAlreadyInDB
      else do
        let pvsVoteIds' = Set.insert voteId (pvsVoteIds)
            (res, pvsVotesByTarget') =
              Map.alterF
                ( \mExistingAggr ->
                    let aggr = fromMaybe (emptyPerasVoteAggregate voteTarget) mExistingAggr
                     in case updatePerasVoteAggregate aggr vote of
                          UPVAR.IncorrectPerasVoteTarget -> error "The aggregate should match the vote target."
                          UPVAR.AddedPerasVoteButDidntGenerateNewCert aggr' -> (AddedPerasVoteButDidntGenerateNewCert, Just aggr')
                          UPVAR.AddedPerasVoteAndGeneratedNewCert aggr' cert -> (AddedPerasVoteAndGeneratedNewCert cert, Just aggr')
                )
                voteTarget
                pvsVotesByTarget
            pvsLastTicketNo' = succ pvsLastTicketNo
            pvsVotesByTicket' = Map.insert pvsLastTicketNo' vote pvsVotesByTicket
            fp' = succ fp
        writeTVar pvdbPerasVoteStateVar $
          WithFingerprint
            PerasVoteState
              { pvsVoteIds = pvsVoteIds'
              , pvsVotesByTarget = pvsVotesByTarget'
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
 where
  PerasVoteDbEnv
    { pvdbTracer
    , pvdbPerasVoteStateVar
    } = env

  voteTarget = getPerasVoteTarget vote
  voteId = getId vote
  voteStake = vpvVoteStake . forgetArrivalTime $ vote

implGetStakeSnapshot ::
  IOLike m =>
  PerasVoteDbEnv m blk -> STM m (PerasStakeSnapshot blk)
implGetStakeSnapshot PerasVoteDbEnv{pvdbPerasVoteStateVar} = do
  perasVoteState <- readTVar pvdbPerasVoteStateVar
  let unPerasStakeSnapshot = pvsVotesByTarget <$> perasVoteState
  pure $ PerasStakeSnapshot{unPerasStakeSnapshot}

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
      , pvsVotesByTarget
      , pvsVotesByTicket
      , pvsLastTicketNo
      } =
      let pvsVotesByTarget' =
            Map.filterWithKey
              (\(rNo, _) _ -> rNo >= roundNo)
              pvsVotesByTarget
          (pvsVotesByTicket', votesToRemove) = Map.partition (\vote -> getPerasVoteRound vote >= roundNo) pvsVotesByTicket
          pvsVoteIds' =
            Foldable.foldl'
              (\set vote -> Set.delete (getId vote) set)
              pvsVoteIds
              votesToRemove
       in PerasVoteState
            { pvsVoteIds = pvsVoteIds'
            , pvsVotesByTarget = pvsVotesByTarget'
            , pvsVotesByTicket = pvsVotesByTicket'
            , pvsLastTicketNo = pvsLastTicketNo
            }

{-------------------------------------------------------------------------------
  Implementation-internal types
-------------------------------------------------------------------------------}

data PerasVoteState blk = PerasVoteState
  { pvsVoteIds :: !(Set (IdOf (PerasVote blk)))
  , pvsVotesByTarget :: !(Map (PerasVoteTarget blk) (PerasVoteAggregate blk))
  , pvsVotesByTicket :: !(Map PerasVoteTicketNo (WithArrivalTime (ValidatedPerasVote blk)))
  -- ^ The votes by 'PerasVoteTicketNo'.
  --
  -- INVARIANT: In sync with 'pvsVotesByTarget'.
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
      , pvsVotesByTarget = Map.empty
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

data PerasVoteDbError
  = ClosedDBError PrettyCallStack
  deriving stock Show
  deriving anyclass Exception
