{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
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

import Control.Tracer (Tracer, nullTracer, traceWith)
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
import Ouroboros.Consensus.Storage.PerasVoteDB.API
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM

-- | Update a 'Map' at a given key with a function, using a default value as the
-- "old" value if the key is not present.
-- Both the old and new values are returned, along with the updated map.
updateWithDefault ::
  forall k v.
  Ord k =>
  -- | default value if key not present
  v ->
  -- | update function
  (v -> v) ->
  -- | key
  k ->
  Map k v ->
  -- | (old value, new value, updated map)
  (v, v, Map k v)
updateWithDefault def upd k m =
  case Map.alterF go k m of
    ((old, new), m') -> (old, new, m')
 where
  go :: Maybe v -> ((v, v), Maybe v)
  go Nothing =
    let old = def
        new = upd old
     in ((old, new), Just new)
  go (Just old) =
    let new = upd old
     in ((old, new), Just new)

-- | Aggregate of votes for a given target (round number and block point).
data PerasTargetVoteAggregate blk = PerasTargetVoteAggregate
  { ptvaTarget :: !(PerasVoteTarget blk)
  , ptvaVotes :: !(Map (IdOf (PerasVote blk)) (WithArrivalTime (ValidatedPerasVote blk)))
  , ptvaTotalStake :: !PerasVoteStake
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

-- | View of votes for a given round number. This type is in charge of enforcing
-- that only one certificate can be generated per round.
data PerasRoundVoteAggregates blk
  = PerasRoundVoteAggregatesQuorumNotReached
      { prvaRoundNo :: PerasRoundNo
      , prvaVoteAggregates :: !(Map (Point blk) (PerasTargetVoteAggregate blk))
      }
  | PerasRoundVoteAggregatesQuorumReachedAlready
      { prvaRoundNo :: PerasRoundNo
      , prvaVoteAggregates :: !(Map (Point blk) (PerasTargetVoteAggregate blk))
      , prvaCert :: ValidatedPerasCert blk
      }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

-- | Get the certificate if quorum was reached for the given round.
prvaMaybeCert :: PerasRoundVoteAggregates blk -> Maybe (ValidatedPerasCert blk)
prvaMaybeCert aggr = case aggr of
  PerasRoundVoteAggregatesQuorumNotReached{} -> Nothing
  PerasRoundVoteAggregatesQuorumReachedAlready{prvaCert} -> Just prvaCert

-- | Create a new, target-level vote aggregate for the given target.
newTargetVoteAggregate ::
  PerasVoteTarget blk ->
  PerasTargetVoteAggregate blk
newTargetVoteAggregate target =
  PerasTargetVoteAggregate
    { ptvaTarget = target
    , ptvaVotes = Map.empty
    , ptvaTotalStake = PerasVoteStake 0
    }

-- | Create a new, round-level vote aggregate for the given round number.
newRoundVoteAggregates ::
  PerasRoundNo ->
  PerasRoundVoteAggregates blk
newRoundVoteAggregates roundNo =
  PerasRoundVoteAggregatesQuorumNotReached
    { prvaRoundNo = roundNo
    , prvaVoteAggregates = Map.empty
    }

-- | Add a vote to an existing target aggregate if it isn't already present,
-- and update the stake accordingly.
-- PRECONDITION: the vote's target must match the aggregate's target.
updateTargetVoteAggregate ::
  StandardHash blk =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasTargetVoteAggregate blk ->
  PerasTargetVoteAggregate blk
updateTargetVoteAggregate
  vote
  ptva@PerasTargetVoteAggregate
    { ptvaTarget = (roundNo, point)
    , ptvaVotes = existingVotes
    , ptvaTotalStake = initialStake
    } =
    if getPerasVoteRound vote /= roundNo || getPerasVoteVotedBlock vote /= point
      then error "updatePerasVoteAggregate: vote target does not match aggregate target"
      else
        let (pvaVotes', pvaTotalStake') =
              case Map.insertLookupWithKey
                (\_k old _new -> old)
                (getId vote)
                vote
                existingVotes of
                (Nothing, votes') ->
                  -- key was NOT present → inserted and stake updated
                  (votes', initialStake + vpvVoteStake (forgetArrivalTime vote))
                (Just _, _) ->
                  -- key WAS already present → votes and stake unchanged
                  (existingVotes, initialStake)
         in ptva{ptvaVotes = pvaVotes', ptvaTotalStake = pvaTotalStake'}

-- | Add a vote to an existing round aggregate.
-- PRECONDITION: the vote's round must match the aggregate's round.
updatePerasRoundVoteAggregates ::
  StandardHash blk =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasCfg blk ->
  PerasRoundVoteAggregates blk ->
  PerasRoundVoteAggregates blk
updatePerasRoundVoteAggregates vote cfg@PerasCfg{perasCfgQuorumThreshold} roundAggrs =
  if getPerasVoteRound vote /= prvaRoundNo roundAggrs
    then error "updatePerasRoundVoteAggregates: vote round does not match aggregate round"
    else
      let target = getPerasVoteTarget vote
          point = snd target
          (oldTargetAggr, newTargetAggr, prvaVoteAggregates') =
            updateWithDefault
              (newTargetVoteAggregate target)
              (updateTargetVoteAggregate vote)
              point
              (prvaVoteAggregates roundAggrs)
          mustEmitCert =
            ptvaTotalStake oldTargetAggr < perasCfgQuorumThreshold
              && ptvaTotalStake newTargetAggr >= perasCfgQuorumThreshold
       in case (roundAggrs, mustEmitCert) of
            (PerasRoundVoteAggregatesQuorumNotReached{prvaRoundNo}, True) ->
              PerasRoundVoteAggregatesQuorumReachedAlready
                { prvaRoundNo
                , prvaVoteAggregates = prvaVoteAggregates'
                , prvaCert = case forgePerasCert cfg target (forgetArrivalTime <$> Map.elems (ptvaVotes newTargetAggr)) of
                    -- TODO: the forge error might be made into a PerasVoteDB error variant?
                    Left err ->
                      error
                        ( "updatePerasRoundVoteAggregates: forging the "
                            ++ "certificate should be valid when quorum is "
                            ++ "reached, but got error: "
                            ++ show err
                        )
                    Right cert -> cert
                }
            (PerasRoundVoteAggregatesQuorumNotReached{prvaRoundNo}, False) ->
              PerasRoundVoteAggregatesQuorumNotReached
                { prvaRoundNo
                , prvaVoteAggregates = prvaVoteAggregates'
                }
            (PerasRoundVoteAggregatesQuorumReachedAlready{prvaRoundNo, prvaCert}, True) ->
              -- TODO: the equivocating cert error might be made into a PerasVoteDB error variant?
              error
                ( "updatePerasRoundVoteAggregates: quorum was already reached for round "
                    ++ show prvaRoundNo
                    ++ " on point "
                    ++ show (pcCertBoostedBlock . vpcCert $ prvaCert)
                    ++ ", but we reached it again on point "
                    ++ show point
                    ++ "with "
                    ++ show (ptvaTotalStake newTargetAggr)
                    ++ " stake (quorum threshold = "
                    ++ (show perasCfgQuorumThreshold)
                    ++ ". Equivocating certificates should be impossible per design."
                )
            (PerasRoundVoteAggregatesQuorumReachedAlready{prvaRoundNo, prvaCert}, False) ->
              PerasRoundVoteAggregatesQuorumReachedAlready
                { prvaRoundNo
                , prvaCert
                , prvaVoteAggregates = prvaVoteAggregates'
                }

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

getEnvSTM1 ::
  (IOLike m, HasCallStack) =>
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
          , pvsVoteAggrsByRound
          , pvsVotesByTicket
          , pvsLastTicketNo
          }
        fp <-
        readTVar pvdbPerasVoteStateVar

      if Set.member voteId pvsVoteIds
        then pure PerasVoteAlreadyInDB
        else do
          let pvsVoteIds' = Set.insert voteId pvsVoteIds
              roundNo = getPerasVoteRound vote
              (oldRoundAggrs, newRoundAggrs, pvsVoteAggrsByRound') =
                updateWithDefault
                  (newRoundVoteAggregates roundNo)
                  (updatePerasRoundVoteAggregates vote perasCfg)
                  roundNo
                  pvsVoteAggrsByRound
              res = case (oldRoundAggrs, newRoundAggrs) of
                ( PerasRoundVoteAggregatesQuorumNotReached{}
                  , PerasRoundVoteAggregatesQuorumReachedAlready{prvaCert}
                  ) ->
                  AddedPerasVoteAndGeneratedNewCert prvaCert
                _ -> AddedPerasVoteButDidntGenerateNewCert
              pvsLastTicketNo' = succ pvsLastTicketNo
              pvsVotesByTicket' = Map.insert pvsLastTicketNo' vote pvsVotesByTicket
              fp' = succ fp
          writeTVar pvdbPerasVoteStateVar $
            WithFingerprint
              PerasVoteState
                { pvsVoteIds = pvsVoteIds'
                , pvsVoteAggrsByRound = pvsVoteAggrsByRound'
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
  PerasVoteState{pvsVoteAggrsByRound} <- forgetFingerprint <$> readTVar pvdbPerasVoteStateVar
  case Map.lookup roundNo pvsVoteAggrsByRound of
    Nothing -> pure Nothing
    Just aggr -> pure $ prvaMaybeCert aggr

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
      , pvsVoteAggrsByRound
      , pvsVotesByTicket
      , pvsLastTicketNo
      } =
      let pvsVoteAggrsByRound' =
            Map.filterWithKey
              (\rNo _ -> rNo >= roundNo)
              pvsVoteAggrsByRound
          (pvsVotesByTicket', votesToRemove) = Map.partition (\vote -> getPerasVoteRound vote >= roundNo) pvsVotesByTicket
          pvsVoteIds' =
            Foldable.foldl'
              (\set vote -> Set.delete (getId vote) set)
              pvsVoteIds
              votesToRemove
       in PerasVoteState
            { pvsVoteIds = pvsVoteIds'
            , pvsVoteAggrsByRound = pvsVoteAggrsByRound'
            , pvsVotesByTicket = pvsVotesByTicket'
            , pvsLastTicketNo = pvsLastTicketNo
            }

{-------------------------------------------------------------------------------
  Implementation-internal types
-------------------------------------------------------------------------------}

data PerasVoteState blk = PerasVoteState
  { pvsVoteIds :: !(Set (IdOf (PerasVote blk)))
  , pvsVoteAggrsByRound :: !(Map PerasRoundNo (PerasRoundVoteAggregates blk))
  , pvsVotesByTicket :: !(Map PerasVoteTicketNo (WithArrivalTime (ValidatedPerasVote blk)))
  -- ^ The votes by 'PerasVoteTicketNo'.
  --
  -- INVARIANT: In sync with 'pvsVoteAggrsByRound'.
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
      , pvsVoteAggrsByRound = Map.empty
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
