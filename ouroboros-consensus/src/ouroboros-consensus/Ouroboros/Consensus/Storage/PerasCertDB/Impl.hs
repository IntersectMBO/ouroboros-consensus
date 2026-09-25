{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.PerasCertDB.Impl
  ( -- * Opening
    PerasCertDbArgs (..)
  , defaultArgs
  , createDB

    -- * Trace types
  , TraceEvent (..)
  ) where

import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..), strictMaybeToMaybe)
import Data.Set (Set)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (WithArrivalTime (..))
import Ouroboros.Consensus.Peras.Weight (PerasWeightSnapshot, mkPerasWeightSnapshot)
import Ouroboros.Consensus.Storage.PerasCertDB.API
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM

{-------------------------------------------------------------------------------
  Database state
-------------------------------------------------------------------------------}

data PerasCertDbEnv m blk = PerasCertDbEnv
  { pcdbTracer :: !(Tracer m (TraceEvent blk))
  , pcdbState :: !(StrictTVar m (WithFingerprint (PerasCertDbState blk)))
  -- ^ The volatile state of the certificate database.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasCertDbEnv" (PerasCertDbEnv m blk)

-- | INVARIANT: See 'invariantForPerasCertDbState'.
data PerasCertDbState blk = PerasCertDbState
  { pcdsCerts :: !(Map PerasRoundNo (WithArrivalTime (ValidatedPerasCert blk)))
  -- ^ The certificates by 'PerasRoundNo'.
  , pcdsLatestCertSeen ::
      !(StrictMaybe (WithBoostedBlockStatus (WithArrivalTime (ValidatedPerasCert blk))))
  -- ^ The certificate with the highest round number that has been added to the
  -- db since it has been opened.
  }

deriving instance
  Show (PerasCert blk) =>
  Show (PerasCertDbState blk)
deriving instance
  NoThunks (PerasCert blk) =>
  NoThunks (PerasCertDbState blk)
deriving instance
  Generic (PerasCertDbState blk)

initialPerasCertDbState :: WithFingerprint (PerasCertDbState blk)
initialPerasCertDbState =
  WithFingerprint
    PerasCertDbState
      { pcdsCerts = Map.empty
      , pcdsLatestCertSeen = SNothing
      }
    (Fingerprint 0)

{-------------------------------------------------------------------------------
  Trace types
-------------------------------------------------------------------------------}

data TraceEvent blk
  = AddCert
      PerasRoundNo
      (WithArrivalTime (ValidatedPerasCert blk))
      AddPerasCertResult
  | GarbageCollected
      SlotNo

deriving instance
  Show (PerasCert blk) =>
  Show (TraceEvent blk)
deriving instance
  Eq (PerasCert blk) =>
  Eq (TraceEvent blk)
deriving instance
  Generic (TraceEvent blk)

{------------------------------------------------------------------------------
  Creating the database
------------------------------------------------------------------------------}

type PerasCertDbArgs :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data PerasCertDbArgs f m blk = PerasCertDbArgs
  { pcdbaTracer :: Tracer m (TraceEvent blk)
  }

defaultArgs :: Monad m => Incomplete PerasCertDbArgs m blk
defaultArgs =
  PerasCertDbArgs
    { pcdbaTracer = nullTracer
    }

createDB ::
  forall m blk.
  ( IOLike m
  , BlockSupportsPeras blk
  ) =>
  Complete PerasCertDbArgs m blk ->
  m (PerasCertDB m blk)
createDB args = do
  pcdbState <-
    newTVarIO
      initialPerasCertDbState
  let env =
        PerasCertDbEnv
          { pcdbTracer
          , pcdbState
          }
  pure
    PerasCertDB
      { addCert = implAddCert env
      , getCertIds = implGetCertIds env
      , getCertsAfter = implGetCertsAfter env
      , getWeightSnapshot = implGetWeightSnapshot env
      , getLatestCertSeen = implGetLatestCertSeen env
      , garbageCollect = implGarbageCollect env
      }
 where
  PerasCertDbArgs
    { pcdbaTracer = pcdbTracer
    } = args

{-------------------------------------------------------------------------------
  API implementation
-------------------------------------------------------------------------------}

-- TODO: we will need to update this method with non-trivial validation logic
-- see https://github.com/tweag/cardano-peras/issues/120
implAddCert ::
  ( IOLike m
  , IsPerasCert (PerasCert blk) blk
  ) =>
  PerasCertDbEnv m blk ->
  WithArrivalTime (ValidatedPerasCert blk) ->
  STM m (m AddPerasCertResult)
implAddCert PerasCertDbEnv{pcdbTracer, pcdbState} cert = do
  let roundNo = getPerasCertRound cert
  addPerasCertRes <- do
    WithFingerprint pcds fp <- readTVar pcdbState
    if Map.member roundNo (pcdsCerts pcds)
      then pure PerasCertAlreadyInDB
      else do
        let pcdsCerts' = Map.insert roundNo cert (pcdsCerts pcds)
            pcdsLatestCertSeen' =
              case pcdsLatestCertSeen pcds of
                SNothing ->
                  SJust (CertBoostingBlockInVolatileDB cert)
                SJust prev
                  | getPerasCertRound cert
                      > getPerasCertRound (forgetBoostedBlockStatus prev) ->
                      SJust (CertBoostingBlockInVolatileDB cert)
                  | otherwise ->
                      SJust prev
        writeTVar pcdbState $
          WithFingerprint
            PerasCertDbState
              { pcdsCerts = pcdsCerts'
              , pcdsLatestCertSeen = pcdsLatestCertSeen'
              }
            (succ fp)
        pure AddedPerasCertToDB
  pure $ do
    traceWith pcdbTracer (AddCert roundNo cert addPerasCertRes)
    pure addPerasCertRes

implGetWeightSnapshot ::
  ( IOLike m
  , StandardHash blk
  , IsPerasCert (PerasCert blk) blk
  ) =>
  PerasCertDbEnv m blk ->
  STM m (WithFingerprint (PerasWeightSnapshot blk))
implGetWeightSnapshot PerasCertDbEnv{pcdbState} = do
  WithFingerprint pcds fp <- readTVar pcdbState
  let weights =
        mkPerasWeightSnapshot
          [ (getPerasCertPoint cert, vpcCertBoost (forgetArrivalTime cert))
          | cert <- Map.elems (pcdsCerts pcds)
          ]
  pure (WithFingerprint weights fp)

implGetCertIds ::
  IOLike m =>
  PerasCertDbEnv m blk ->
  STM m (Set PerasRoundNo)
implGetCertIds PerasCertDbEnv{pcdbState} = do
  PerasCertDbState{pcdsCerts} <-
    forgetFingerprint <$> readTVar pcdbState
  pure $ Map.keysSet pcdsCerts

implGetCertsAfter ::
  IOLike m =>
  PerasCertDbEnv m blk ->
  PerasRoundNo ->
  STM m (Map PerasRoundNo (m (WithArrivalTime (ValidatedPerasCert blk))))
implGetCertsAfter PerasCertDbEnv{pcdbState} roundNo = do
  PerasCertDbState{pcdsCerts} <-
    forgetFingerprint <$> readTVar pcdbState
  let strictlyGreater = snd $ Map.split roundNo pcdsCerts
  pure $ pure <$> strictlyGreater

implGetLatestCertSeen ::
  IOLike m =>
  PerasCertDbEnv m blk ->
  STM m (Maybe (WithBoostedBlockStatus (WithArrivalTime (ValidatedPerasCert blk))))
implGetLatestCertSeen PerasCertDbEnv{pcdbState} = do
  PerasCertDbState{pcdsLatestCertSeen} <-
    forgetFingerprint <$> readTVar pcdbState
  pure $ strictMaybeToMaybe pcdsLatestCertSeen

implGarbageCollect ::
  forall m blk.
  ( IOLike m
  , IsPerasCert (PerasCert blk) blk
  ) =>
  PerasCertDbEnv m blk ->
  SlotNo ->
  STM m (m ())
implGarbageCollect PerasCertDbEnv{pcdbTracer, pcdbState} slotNo = do
  -- No need to update the 'Fingerprint' as we only remove certificates that do
  -- not matter for comparing interesting chains.
  modifyTVar pcdbState (fmap gc)
  pure $ traceWith pcdbTracer (GarbageCollected slotNo)
 where
  gc :: PerasCertDbState blk -> PerasCertDbState blk
  gc
    PerasCertDbState
      { pcdsCerts
      , pcdsLatestCertSeen
      } =
      let pcdsCerts' =
            Map.filter
              (\cert -> pointSlot (getPerasCertPoint cert) >= NotOrigin slotNo)
              pcdsCerts
          pcdsLatestCertSeen' =
            updateIfBoostingGarbageCollectedBlock <$> pcdsLatestCertSeen

          -- Update the latest certificate seen status when its corresponding
          -- boosted block gets garbage collected.
          updateIfBoostingGarbageCollectedBlock cert
            | pointSlot (getPerasCertPoint (forgetBoostedBlockStatus cert))
                < NotOrigin slotNo =
                CertBoostingBlockNoLongerInVolatileDB (forgetBoostedBlockStatus cert)
            | otherwise =
                cert
       in PerasCertDbState
            { pcdsCerts = pcdsCerts'
            , pcdsLatestCertSeen = pcdsLatestCertSeen'
            }
