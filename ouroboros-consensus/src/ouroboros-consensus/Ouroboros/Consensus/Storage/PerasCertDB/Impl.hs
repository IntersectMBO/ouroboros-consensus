{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Ouroboros.Consensus.Storage.PerasCertDB.Impl
  ( -- * Opening
    PerasCertDbArgs (..)
  , defaultArgs
  , openDB

    -- * Trace types
  , TraceEvent (..)

    -- * Exceptions
  , PerasCertDbError (..)
  ) where

import Control.Monad (join)
import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Storage.PerasCertDB.API
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.IOLike

{------------------------------------------------------------------------------
  Opening the database
------------------------------------------------------------------------------}

type PerasCertDbArgs :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data PerasCertDbArgs f m blk = PerasCertDbArgs
  { pcdbaTracer :: Tracer m (TraceEvent blk)
  }

defaultArgs :: Applicative m => Incomplete PerasCertDbArgs m blk
defaultArgs =
  PerasCertDbArgs
    { pcdbaTracer = nullTracer
    }

openDB ::
  forall m blk.
  ( IOLike m
  , StandardHash blk
  ) =>
  Complete PerasCertDbArgs m blk ->
  m (PerasCertDB m blk)
openDB args = do
  pcdbRoundNos <- newTVarIO Set.empty
  pcdbWeightByPoint <- newTVarIO Map.empty
  let env =
        PerasCertDbEnv
          { pcdbTracer
          , pcdbRoundNos
          , pcdbWeightByPoint
          }
  h <- PerasCertDbHandle <$> newTVarIO (PerasCertDbOpen env)
  traceWith pcdbTracer OpenedPerasCertDB
  pure
    PerasCertDB
      { addCert = getEnv1 h implAddCert
      , getWeightSnapshot = getEnvSTM h implGetWeightSnapshot
      , closeDB = implCloseDB h
      }
 where
  PerasCertDbArgs
    { pcdbaTracer = pcdbTracer
    } = args

{-------------------------------------------------------------------------------
  Database state
-------------------------------------------------------------------------------}

newtype PerasCertDbHandle m blk = PerasCertDbHandle (StrictTVar m (PerasCertDbState m blk))

data PerasCertDbState m blk
  = PerasCertDbOpen !(PerasCertDbEnv m blk)
  | PerasCertDbClosed
  deriving stock Generic
  deriving anyclass NoThunks

data PerasCertDbEnv m blk = PerasCertDbEnv
  { pcdbTracer :: !(Tracer m (TraceEvent blk))
  , pcdbRoundNos :: !(StrictTVar m (Set PerasRoundNo))
  -- ^ The 'RoundNo's of all certificates currently in the db.
  , pcdbWeightByPoint :: !(StrictTVar m (Map (Point blk) PerasWeight))
  -- ^ The weight of boosted blocks w.r.t. the certificates currently in the
  -- db.
  --
  -- INVARIANT: In sync with 'pcdbRoundNos'.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasCertDbEnv" (PerasCertDbEnv m blk)

getEnv ::
  (IOLike m, HasCallStack) =>
  PerasCertDbHandle m blk ->
  (PerasCertDbEnv m blk -> m r) ->
  m r
getEnv (PerasCertDbHandle varState) f =
  readTVarIO varState >>= \case
    PerasCertDbOpen env -> f env
    PerasCertDbClosed -> throwIO $ ClosedDBError prettyCallStack

getEnv1 ::
  (IOLike m, HasCallStack) =>
  PerasCertDbHandle m blk ->
  (PerasCertDbEnv m blk -> a -> m r) ->
  a ->
  m r
getEnv1 h f a = getEnv h (\env -> f env a)

getEnvSTM ::
  (IOLike m, HasCallStack) =>
  PerasCertDbHandle m blk ->
  (PerasCertDbEnv m blk -> STM m r) ->
  STM m r
getEnvSTM (PerasCertDbHandle varState) f =
  readTVar varState >>= \case
    PerasCertDbOpen env -> f env
    PerasCertDbClosed -> throwIO $ ClosedDBError prettyCallStack

{-------------------------------------------------------------------------------
  API implementation
-------------------------------------------------------------------------------}

implCloseDB :: IOLike m => PerasCertDbHandle m blk -> m ()
implCloseDB (PerasCertDbHandle varState) =
  atomically (swapTVar varState PerasCertDbClosed) >>= \case
    PerasCertDbOpen PerasCertDbEnv{pcdbTracer} -> do
      traceWith pcdbTracer ClosedPerasCertDB
    -- DB was already closed.
    PerasCertDbClosed -> pure ()

-- TODO: validation
implAddCert ::
  ( IOLike m
  , StandardHash blk
  ) =>
  PerasCertDbEnv m blk ->
  PerasCert blk ->
  m ()
implAddCert env cert = do
  traceWith pcdbTracer $ AddingPerasCert roundNo boostedPt
  join $ atomically $ do
    roundNos <- readTVar pcdbRoundNos
    if Set.member roundNo roundNos
      then do
        pure $ traceWith pcdbTracer $ IgnoredCertAlreadyInDB roundNo boostedPt
      else do
        writeTVar pcdbRoundNos $ Set.insert roundNo roundNos
        -- Note that the same block might be boosted by multiple points.
        modifyTVar pcdbWeightByPoint $ Map.insertWith (<>) boostedPt boostPerCert
        pure $ traceWith pcdbTracer $ AddedPerasCert roundNo boostedPt
 where
  PerasCertDbEnv
    { pcdbTracer
    , pcdbRoundNos
    , pcdbWeightByPoint
    } = env

  roundNo = perasCertRound cert
  boostedPt = perasCertBoostedBlock cert

implGetWeightSnapshot ::
  IOLike m =>
  PerasCertDbEnv m blk -> STM m (PerasWeightSnapshot blk)
implGetWeightSnapshot PerasCertDbEnv{pcdbWeightByPoint} =
  PerasWeightSnapshot <$> readTVar pcdbWeightByPoint

{-------------------------------------------------------------------------------
  Trace types
-------------------------------------------------------------------------------}

data TraceEvent blk
  = OpenedPerasCertDB
  | ClosedPerasCertDB
  | AddingPerasCert PerasRoundNo (Point blk)
  | AddedPerasCert PerasRoundNo (Point blk)
  | IgnoredCertAlreadyInDB PerasRoundNo (Point blk)
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

data PerasCertDbError
  = ClosedDBError PrettyCallStack
  deriving stock Show
  deriving anyclass Exception
