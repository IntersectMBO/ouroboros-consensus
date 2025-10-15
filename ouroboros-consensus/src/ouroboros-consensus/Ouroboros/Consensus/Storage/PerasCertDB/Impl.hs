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

import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime)
import Ouroboros.Consensus.Peras.Weight
import Ouroboros.Consensus.Storage.PerasCertDB.API
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM

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
  pcdbVolatileState <- newTVarIO initialPerasVolatileCertState
  let env =
        PerasCertDbEnv
          { pcdbTracer
          , pcdbVolatileState
          }
  h <- PerasCertDbHandle <$> newTVarIO (PerasCertDbOpen env)
  traceWith pcdbTracer OpenedPerasCertDB
  pure
    PerasCertDB
      { addCert = getEnv1 h implAddCert
      , getWeightSnapshot = getEnvSTM h implGetWeightSnapshot
      , getCertSnapshot = getEnvSTM h implGetCertSnapshot
      , garbageCollect = getEnv1 h implGarbageCollect
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
  , pcdbVolatileState :: !(StrictTVar m (WithFingerprint (PerasVolatileCertState blk)))
  -- ^ The 'RoundNo's of all certificates currently in the db.
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
  WithArrivalTime (ValidatedPerasCert blk) ->
  m AddPerasCertResult
implAddCert env cert = do
  traceWith pcdbTracer $ AddingPerasCert roundNo boostedPt
  res <- atomically $ do
    WithFingerprint
      PerasVolatileCertState
        { pvcsCerts
        , pvcsWeightByPoint
        , pvcsCertsByTicket
        , pvcsLastTicketNo
        }
      fp <-
      readTVar pcdbVolatileState
    if Map.member roundNo pvcsCerts
      then pure PerasCertAlreadyInDB
      else do
        let pvcsLastTicketNo' = succ pvcsLastTicketNo
        writeTVar pcdbVolatileState $
          WithFingerprint
            PerasVolatileCertState
              { pvcsCerts =
                  Map.insert roundNo cert pvcsCerts
              , -- Note that the same block might be boosted by multiple points.
                pvcsWeightByPoint =
                  addToPerasWeightSnapshot boostedPt (getPerasCertBoost cert) pvcsWeightByPoint
              , pvcsCertsByTicket =
                  Map.insert pvcsLastTicketNo' cert pvcsCertsByTicket
              , pvcsLastTicketNo = pvcsLastTicketNo'
              }
            (succ fp)
        pure AddedPerasCertToDB
  traceWith pcdbTracer $ case res of
    AddedPerasCertToDB -> AddedPerasCert roundNo boostedPt
    PerasCertAlreadyInDB -> IgnoredCertAlreadyInDB roundNo boostedPt
  pure res
 where
  PerasCertDbEnv
    { pcdbTracer
    , pcdbVolatileState
    } = env

  boostedPt = getPerasCertBoostedBlock cert
  roundNo = getPerasCertRound cert

implGetWeightSnapshot ::
  IOLike m =>
  PerasCertDbEnv m blk -> STM m (WithFingerprint (PerasWeightSnapshot blk))
implGetWeightSnapshot PerasCertDbEnv{pcdbVolatileState} =
  fmap pvcsWeightByPoint <$> readTVar pcdbVolatileState

implGetCertSnapshot ::
  IOLike m =>
  PerasCertDbEnv m blk -> STM m (PerasCertSnapshot blk)
implGetCertSnapshot PerasCertDbEnv{pcdbVolatileState} =
  readTVar pcdbVolatileState
    <&> forgetFingerprint
    <&> \PerasVolatileCertState
           { pvcsCerts
           , pvcsCertsByTicket
           } ->
        PerasCertSnapshot
          { containsCert = \r -> Map.member r pvcsCerts
          , getCertsAfter = \ticketNo ->
              let (_, certs) = Map.split ticketNo pvcsCertsByTicket
               in [(cert, tno) | (tno, cert) <- Map.toAscList certs]
          }

implGarbageCollect ::
  forall m blk.
  (IOLike m, StandardHash blk) =>
  PerasCertDbEnv m blk -> SlotNo -> m ()
implGarbageCollect PerasCertDbEnv{pcdbVolatileState} slot =
  -- No need to update the 'Fingerprint' as we only remove certificates that do
  -- not matter for comparing interesting chains.
  atomically $ modifyTVar pcdbVolatileState (fmap gc)
 where
  gc :: PerasVolatileCertState blk -> PerasVolatileCertState blk
  gc
    PerasVolatileCertState
      { pvcsCerts
      , pvcsWeightByPoint
      , pvcsLastTicketNo
      , pvcsCertsByTicket
      } =
      PerasVolatileCertState
        { pvcsCerts = Map.filter keepCert pvcsCerts
        , pvcsWeightByPoint = prunePerasWeightSnapshot slot pvcsWeightByPoint
        , pvcsCertsByTicket = Map.filter keepCert pvcsCertsByTicket
        , pvcsLastTicketNo = pvcsLastTicketNo
        }
     where
      keepCert cert =
        pointSlot (getPerasCertBoostedBlock cert) >= NotOrigin slot

{-------------------------------------------------------------------------------
  Implementation-internal types
-------------------------------------------------------------------------------}

-- | Volatile Peras certificate state, i.e. certificates that could influence
-- chain selection by boosting a volatile block.
data PerasVolatileCertState blk = PerasVolatileCertState
  { pvcsCerts :: !(Map PerasRoundNo (WithArrivalTime (ValidatedPerasCert blk)))
  -- ^ The boosted blocks by 'RoundNo' of all certificates currently in the db.
  , pvcsWeightByPoint :: !(PerasWeightSnapshot blk)
  -- ^ The weight of boosted blocks w.r.t. the certificates currently in the db.
  --
  -- INVARIANT: In sync with 'pvcsCerts'.
  , pvcsCertsByTicket :: !(Map PerasCertTicketNo (WithArrivalTime (ValidatedPerasCert blk)))
  -- ^ The certificates by 'PerasCertTicketNo'.
  --
  -- INVARIANT: In sync with 'pvcsCerts'.
  , pvcsLastTicketNo :: !PerasCertTicketNo
  -- ^ The most recent 'PerasCertTicketNo' (or 'zeroPerasCertTicketNo'
  -- otherwise).
  }
  deriving stock (Show, Generic)
  deriving anyclass NoThunks

initialPerasVolatileCertState :: WithFingerprint (PerasVolatileCertState blk)
initialPerasVolatileCertState =
  WithFingerprint
    PerasVolatileCertState
      { pvcsCerts = Map.empty
      , pvcsWeightByPoint = emptyPerasWeightSnapshot
      , pvcsCertsByTicket = Map.empty
      , pvcsLastTicketNo = zeroPerasCertTicketNo
      }
    (Fingerprint 0)

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
