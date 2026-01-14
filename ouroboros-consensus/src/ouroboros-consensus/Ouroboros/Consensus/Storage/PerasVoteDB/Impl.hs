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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (WithArrivalTime (forgetArrivalTime))
import Ouroboros.Consensus.Peras.Params (PerasParams)
import Ouroboros.Consensus.Peras.Round (PerasRoundNo)
import Ouroboros.Consensus.Peras.Vote
  ( PerasVoteId (..)
  , PerasVoteStake
  , PerasVoteTarget
  )
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
  , BlockSupportsPeras blk
  , PerasCfg blk ~ PerasParams
  ) =>
  Complete PerasVoteDbArgs m blk ->
  m (PerasVoteDB m blk)
openDB args@PerasVoteDbArgs{pvdbaPerasCfg} = do
  pvdbPerasVoteStateVar <-
    newTVarWithInvariantIO
      (either Just (const Nothing) . invariantForPerasVolatileVoteState)
      initialPerasVolatileVoteState
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

newtype PerasVoteDbHandle m blk
  = PerasVoteDbHandle (StrictTVar m (PerasVoteDbState m blk))

data PerasVoteDbState m blk
  = PerasVoteDbOpen !(PerasVoteDbEnv m blk)
  | PerasVoteDbClosed
  deriving stock Generic
  deriving anyclass NoThunks

data PerasVoteDbEnv m blk = PerasVoteDbEnv
  { pvdbTracer :: !(Tracer m (TraceEvent blk))
  , pvdbPerasVoteStateVar :: !(StrictTVar m (WithFingerprint (PerasVolatileVoteState blk)))
  -- ^ The 'RoundNo's of all votes currently in the db.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasVoteDbEnv" (PerasVoteDbEnv m blk)

getEnv ::
  forall m r blk.
  (IOLike m, HasCallStack, BlockSupportsPeras blk) =>
  PerasVoteDbHandle m blk ->
  (PerasVoteDbEnv m blk -> m r) ->
  m r
getEnv (PerasVoteDbHandle varState) f =
  readTVarIO varState >>= \case
    PerasVoteDbOpen env -> f env
    PerasVoteDbClosed -> throwIO $ ClosedDBError @blk prettyCallStack

getEnv1 ::
  (IOLike m, HasCallStack, BlockSupportsPeras blk) =>
  PerasVoteDbHandle m blk ->
  (PerasVoteDbEnv m blk -> a -> m r) ->
  a ->
  m r
getEnv1 h f a = getEnv h (\env -> f env a)

getEnvSTM ::
  forall m r blk.
  (IOLike m, HasCallStack, BlockSupportsPeras blk) =>
  PerasVoteDbHandle m blk ->
  (PerasVoteDbEnv m blk -> STM m r) ->
  STM m r
getEnvSTM (PerasVoteDbHandle varState) f =
  readTVar varState >>= \case
    PerasVoteDbOpen env -> f env
    PerasVoteDbClosed -> throwIO $ ClosedDBError @blk prettyCallStack

getEnvSTM1 ::
  (IOLike m, HasCallStack, BlockSupportsPeras blk) =>
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
  , BlockSupportsPeras blk
  , PerasCfg blk ~ PerasParams
  ) =>
  PerasCfg blk ->
  PerasVoteDbEnv m blk ->
  WithArrivalTime (ValidatedPerasVote blk) ->
  m (AddPerasVoteResult blk)
implAddVote perasCfg PerasVoteDbEnv{pvdbTracer, pvdbPerasVoteStateVar} vote = do
  let voteId = getPerasVoteId vote
      voteTarget = getPerasVoteTarget vote
      voteStake = vpvVoteStake (forgetArrivalTime vote)

  traceWith pvdbTracer $ AddingPerasVote voteTarget voteId voteStake

  addPerasVoteRes <- atomically $ do
    WithFingerprint pvvs fp <- readTVar pvdbPerasVoteStateVar
    (res, pvvs') <- addOrIgnoreVote pvvs voteId
    writeTVar pvdbPerasVoteStateVar (WithFingerprint pvvs' (succ fp))
    pure res

  case addPerasVoteRes of
    PerasVoteAlreadyInDB -> do
      traceWith pvdbTracer $ IgnoredVoteAlreadyInDB voteId
    AddedPerasVoteButDidntGenerateNewCert -> do
      traceWith pvdbTracer $ AddedPerasVote voteId
    AddedPerasVoteAndGeneratedNewCert newCert -> do
      traceWith pvdbTracer $ AddedPerasVote voteId
      traceWith pvdbTracer $ GeneratedPerasCert newCert

  pure addPerasVoteRes
 where
  addOrIgnoreVote pvvs voteId
    -- Vote is already in the DB => ignore it
    | Set.member voteId (pvvsVoteIds pvvs) = voteAlreadyInDB pvvs
    -- New vote => try to add it to the DB
    | otherwise = tryAddVote pvvs voteId

  voteAlreadyInDB pvvs = do
    pure (PerasVoteAlreadyInDB, pvvs)

  tryAddVote pvvs voteId = do
    let pvsVoteIds' = Set.insert voteId (pvvsVoteIds pvvs)
        pvsLastTicketNo' = succ (pvvsLastTicketNo pvvs)
        pvsVotesByTicket' = Map.insert pvsLastTicketNo' vote (pvvsVotesByTicket pvvs)

    (addPerasVoteRes, pvsRoundVoteStates') <-
      case updatePerasRoundVoteStates vote perasCfg (pvvsRoundVoteStates pvvs) of
        -- Added vote and reached a quorum, forging a new certificate
        Right (VoteGeneratedNewCert cert, pvsRoundVoteStates') -> do
          pure (AddedPerasVoteAndGeneratedNewCert cert, pvsRoundVoteStates')
        -- Added vote but did not generate a new certificate, either
        -- because quorum was not reached yet, or because this vote was
        -- cast upon a target that had already won so a certificate was
        -- forged in a previous step.
        Right (VoteDidntGenerateNewCert, pvsRoundVoteStates') -> do
          pure (AddedPerasVoteButDidntGenerateNewCert, pvsRoundVoteStates')
        -- Adding the vote led to more than one winner => internal error
        Left (RoundVoteStateLoserAboveQuorum winner loser) ->
          throwSTM $
            MultipleWinnersInRound
              (getPerasVoteRound vote)
              (vpvVoteStake (forgetArrivalTime vote))
              winner
              loser
        -- Reached quorum but failed to forge a certificate
        Left (RoundVoteStateForgingCertError forgeErr) ->
          throwSTM $
            ForgingCertError forgeErr

    pure
      ( addPerasVoteRes
      , PerasVolatileVoteState
          { pvvsVoteIds = pvsVoteIds'
          , pvvsRoundVoteStates = pvsRoundVoteStates'
          , pvvsVotesByTicket = pvsVotesByTicket'
          , pvvsLastTicketNo = pvsLastTicketNo'
          }
      )

implGetVoteSnapshot ::
  IOLike m =>
  PerasVoteDbEnv m blk -> STM m (PerasVoteSnapshot blk)
implGetVoteSnapshot PerasVoteDbEnv{pvdbPerasVoteStateVar} = do
  PerasVolatileVoteState{pvvsVoteIds, pvvsVotesByTicket} <-
    forgetFingerprint <$> readTVar pvdbPerasVoteStateVar
  pure $
    PerasVoteSnapshot
      { containsVote = (`Set.member` pvvsVoteIds)
      , getVotesAfter = \ticketNo ->
          snd $ Map.split ticketNo pvvsVotesByTicket
      }

implForgedCertForRound ::
  IOLike m =>
  PerasVoteDbEnv m blk ->
  PerasRoundNo ->
  STM m (Maybe (ValidatedPerasCert blk))
implForgedCertForRound PerasVoteDbEnv{pvdbPerasVoteStateVar} roundNo = do
  PerasVolatileVoteState{pvvsRoundVoteStates} <-
    forgetFingerprint <$> readTVar pvdbPerasVoteStateVar
  case Map.lookup roundNo pvvsRoundVoteStates of
    Nothing -> pure Nothing
    Just aggr -> pure $ prvsMaybeCert aggr

implGarbageCollect ::
  forall m blk.
  ( IOLike m
  , BlockSupportsPeras blk
  ) =>
  PerasVoteDbEnv m blk -> PerasRoundNo -> m ()
implGarbageCollect PerasVoteDbEnv{pvdbPerasVoteStateVar} roundNo =
  -- No need to update the 'Fingerprint' as we only remove votes that do
  -- not matter for comparing interesting chains.
  atomically $ modifyTVar pvdbPerasVoteStateVar (fmap gc)
 where
  gc :: PerasVolatileVoteState blk -> PerasVolatileVoteState blk
  gc
    PerasVolatileVoteState
      { pvvsVoteIds
      , pvvsRoundVoteStates
      , pvvsVotesByTicket
      , pvvsLastTicketNo
      } =
      let pvsRoundVoteStates' =
            Map.filterWithKey
              (\rNo _ -> rNo >= roundNo)
              pvvsRoundVoteStates
          (pvsVotesByTicket', votesToRemove) =
            Map.partition
              (\vote -> getPerasVoteRound vote >= roundNo)
              pvvsVotesByTicket
          pvsVoteIds' =
            Foldable.foldl'
              (\set vote -> Set.delete (getPerasVoteId vote) set)
              pvvsVoteIds
              votesToRemove
       in PerasVolatileVoteState
            { pvvsVoteIds = pvsVoteIds'
            , pvvsRoundVoteStates = pvsRoundVoteStates'
            , pvvsVotesByTicket = pvsVotesByTicket'
            , pvvsLastTicketNo = pvvsLastTicketNo
            }

{-------------------------------------------------------------------------------
  Implementation-internal types
-------------------------------------------------------------------------------}

-- | Volatile Peras certificate state, i.e. certificates that could influence
-- chain selection by boosting a volatile block.
--
-- INVARIANT: See 'invariantForPerasVolatileVoteState'.
data PerasVolatileVoteState blk = PerasVolatileVoteState
  { pvvsVoteIds :: !(Set (PerasVoteId blk))
  , pvvsRoundVoteStates :: !(Map PerasRoundNo (PerasRoundVoteState blk))
  , pvvsVotesByTicket :: !(Map PerasVoteTicketNo (WithArrivalTime (ValidatedPerasVote blk)))
  -- ^ The votes by 'PerasVoteTicketNo'.
  --
  -- INVARIANT: In sync with 'pvsRoundVoteStates'.
  , pvvsLastTicketNo :: !PerasVoteTicketNo
  -- ^ The most recent 'PerasVoteTicketNo' (or 'zeroPerasVoteTicketNo' otherwise).
  }

deriving instance
  ( StandardHash blk
  , Show (PerasCert blk)
  , Show (PerasVote blk)
  ) =>
  Show (PerasVolatileVoteState blk)

deriving instance Generic (PerasVolatileVoteState blk)

deriving instance
  ( StandardHash blk
  , NoThunks (PerasCert blk)
  , NoThunks (PerasVote blk)
  ) =>
  NoThunks (PerasVolatileVoteState blk)

initialPerasVolatileVoteState :: WithFingerprint (PerasVolatileVoteState blk)
initialPerasVolatileVoteState =
  WithFingerprint
    PerasVolatileVoteState
      { pvvsVoteIds = Set.empty
      , pvvsRoundVoteStates = Map.empty
      , pvvsVotesByTicket = Map.empty
      , pvvsLastTicketNo = zeroPerasVoteTicketNo
      }
    (Fingerprint 0)

-- | Check that the fields of 'PerasVoteState' are in sync.
invariantForPerasVolatileVoteState ::
  BlockSupportsPeras blk =>
  WithFingerprint (PerasVolatileVoteState blk) ->
  Either String ()
invariantForPerasVolatileVoteState pvs = do
  for_ (Map.toList pvvsRoundVoteStates) $ \(roundNo, prvs) ->
    checkEqual "pvcRoundVoteStates rounds" roundNo (prvsRoundNo prvs)
  checkEqual
    "pvcsVotesByTicket"
    (Set.fromList (getPerasVoteRound <$> Map.elems pvvsVotesByTicket))
    (Set.fromList (pviRoundNo <$> Set.elems pvvsVoteIds))
  for_ (Map.keys pvvsVotesByTicket) $ \ticketNo ->
    when (ticketNo > pvvsLastTicketNo) $
      throwError $
        "Ticket number monotonicity violation: "
          <> show ticketNo
          <> " > "
          <> show pvvsLastTicketNo
 where
  PerasVolatileVoteState
    { pvvsRoundVoteStates
    , pvvsVotesByTicket
    , pvvsVoteIds
    , pvvsLastTicketNo
    } = forgetFingerprint pvs

  checkEqual :: (Eq a, Show a) => String -> a -> a -> Either String ()
  checkEqual msg a b =
    when (a /= b) $ throwError $ msg <> ": Not equal: " <> show a <> ", " <> show b

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

deriving instance BlockSupportsPeras blk => Show (TraceEvent blk)
deriving instance BlockSupportsPeras blk => Eq (TraceEvent blk)
deriving instance Generic (TraceEvent blk)

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

data PerasVoteDbError blk
  = -- | Attempted to use a closed database
    ClosedDBError PrettyCallStack
  | -- | Attempted to add a vote that would lead to multiple winners for the same round
    MultipleWinnersInRound PerasRoundNo PerasVoteStake (Point blk) (Point blk)
  | -- | An error occurred while forging a certificate
    ForgingCertError (PerasForgeErr blk)

deriving instance BlockSupportsPeras blk => Show (PerasVoteDbError blk)

instance BlockSupportsPeras blk => Exception (PerasVoteDbError blk)
