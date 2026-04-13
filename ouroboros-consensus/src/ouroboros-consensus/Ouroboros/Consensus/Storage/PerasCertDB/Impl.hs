{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Ouroboros.Consensus.Storage.PerasCertDB.Impl
  ( -- * Opening
    PerasCertDbArgs (..)
  , defaultArgs
  , createDB

    -- * Trace types
  , TraceEvent (..)
  ) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
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
  { pcdsCertIds :: !(Set PerasRoundNo)
  -- ^ The round numbers of all certificates currently in the db.
  , pcdsCertsByTicket :: !(Map PerasCertTicketNo (WithArrivalTime (ValidatedPerasCert blk)))
  -- ^ The certificates by 'PerasCertTicketNo'.
  --
  -- INVARIANT: In sync with 'pcdsCertIds'.
  , pcdsLastTicketNo :: !PerasCertTicketNo
  -- ^ The most recent 'PerasCertTicketNo' (or 'zeroPerasCertTicketNo'
  -- otherwise).
  , pcdsLatestCertSeen :: !(Maybe (WithArrivalTime (ValidatedPerasCert blk)))
  -- ^ The certificate with the highest round number that has been added to the
  -- db since it has been opened.
  }
  deriving stock (Show, Generic)
  deriving anyclass NoThunks

initialPerasCertDbState :: WithFingerprint (PerasCertDbState blk)
initialPerasCertDbState =
  WithFingerprint
    PerasCertDbState
      { pcdsCertIds = Set.empty
      , pcdsCertsByTicket = Map.empty
      , pcdsLastTicketNo = zeroPerasCertTicketNo
      , pcdsLatestCertSeen = Nothing
      }
    (Fingerprint 0)

-- | Check that the fields of 'PerasCertDbState' are in sync.
invariantForPerasCertDbState ::
  WithFingerprint (PerasCertDbState blk) -> Either String ()
invariantForPerasCertDbState pcds = do
  checkEqual
    "pcdsCertsByTicket"
    (Set.fromList (getPerasCertRound <$> Map.elems pcdsCertsByTicket))
    pcdsCertIds
  for_ (Map.keys pcdsCertsByTicket) $ \ticketNo ->
    when (ticketNo > pcdsLastTicketNo) $
      throwError $
        "Ticket number monotonicity violation: "
          <> show ticketNo
          <> " > "
          <> show pcdsLastTicketNo
 where
  PerasCertDbState
    { pcdsCertIds
    , pcdsCertsByTicket
    , pcdsLastTicketNo
    } = forgetFingerprint pcds

  checkEqual :: (Eq a, Show a) => String -> a -> a -> Either String ()
  checkEqual msg a b =
    when (a /= b) $ throwError $ msg <> ": Not equal: " <> show a <> ", " <> show b

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
  deriving stock (Show, Eq, Generic)

{------------------------------------------------------------------------------
  Creating the database
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

createDB ::
  forall m blk.
  ( IOLike m
  , StandardHash blk
  ) =>
  Complete PerasCertDbArgs m blk ->
  m (PerasCertDB m blk)
createDB args = do
  pcdbState <-
    newTVarWithInvariantIO
      (either Just (const Nothing) . invariantForPerasCertDbState)
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
  IOLike m =>
  PerasCertDbEnv m blk ->
  WithArrivalTime (ValidatedPerasCert blk) ->
  STM m (m AddPerasCertResult)
implAddCert PerasCertDbEnv{pcdbTracer, pcdbState} cert = do
  let roundNo = getPerasCertRound cert
  addPerasCertRes <- do
    WithFingerprint pcds fp <- readTVar pcdbState
    if Set.member roundNo (pcdsCertIds pcds)
      then pure PerasCertAlreadyInDB
      else do
        let pcdsLastTicketNo' = succ (pcdsLastTicketNo pcds)
            pcdsCertIds' = Set.insert roundNo (pcdsCertIds pcds)
            pcdsCertsByTicket' = Map.insert pcdsLastTicketNo' cert (pcdsCertsByTicket pcds)
            pcdsLatestCertSeen' = case pcdsLatestCertSeen pcds of
              Nothing -> Just cert
              Just prev
                | getPerasCertRound cert > getPerasCertRound prev -> Just cert
                | otherwise -> Just prev
        writeTVar pcdbState $
          WithFingerprint
            PerasCertDbState
              { pcdsCertIds = pcdsCertIds'
              , pcdsCertsByTicket = pcdsCertsByTicket'
              , pcdsLastTicketNo = pcdsLastTicketNo'
              , pcdsLatestCertSeen = pcdsLatestCertSeen'
              }
            (succ fp)
        pure AddedPerasCertToDB
  pure $ do
    traceWith pcdbTracer (AddCert roundNo cert addPerasCertRes)
    pure addPerasCertRes

implGetWeightSnapshot ::
  (IOLike m, StandardHash blk) =>
  PerasCertDbEnv m blk ->
  STM m (WithFingerprint (PerasWeightSnapshot blk))
implGetWeightSnapshot PerasCertDbEnv{pcdbState} = do
  WithFingerprint pcds fp <- readTVar pcdbState
  let weights =
        mkPerasWeightSnapshot
          [ (getPerasCertBoostedBlock cert, getPerasCertBoost cert)
          | cert <- Map.elems (pcdsCertsByTicket pcds)
          ]
  pure (WithFingerprint weights fp)

implGetCertIds ::
  IOLike m =>
  PerasCertDbEnv m blk ->
  STM m (Set PerasRoundNo)
implGetCertIds PerasCertDbEnv{pcdbState} = do
  PerasCertDbState{pcdsCertIds} <-
    forgetFingerprint <$> readTVar pcdbState
  pure pcdsCertIds

implGetCertsAfter ::
  IOLike m =>
  PerasCertDbEnv m blk ->
  PerasCertTicketNo ->
  STM m (Map PerasCertTicketNo (m (WithArrivalTime (ValidatedPerasCert blk))))
implGetCertsAfter PerasCertDbEnv{pcdbState} ticketNo = do
  PerasCertDbState{pcdsCertsByTicket} <-
    forgetFingerprint <$> readTVar pcdbState
  let strictlyGreater = snd $ Map.split ticketNo pcdsCertsByTicket
  pure $ pure <$> strictlyGreater

implGetLatestCertSeen ::
  IOLike m =>
  PerasCertDbEnv m blk ->
  STM m (Maybe (WithArrivalTime (ValidatedPerasCert blk)))
implGetLatestCertSeen PerasCertDbEnv{pcdbState} = do
  PerasCertDbState{pcdsLatestCertSeen} <-
    forgetFingerprint <$> readTVar pcdbState
  pure pcdsLatestCertSeen

implGarbageCollect ::
  forall m blk.
  IOLike m =>
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
      { pcdsCertsByTicket
      , pcdsLastTicketNo
      , pcdsLatestCertSeen
      } =
      let pcdsCertsByTicket' =
            Map.filter
              (\cert -> pointSlot (getPerasCertBoostedBlock cert) >= NotOrigin slotNo)
              pcdsCertsByTicket
          pcdsCertIds' =
            Set.fromList (getPerasCertRound <$> Map.elems pcdsCertsByTicket')
       in PerasCertDbState
            { pcdsCertIds = pcdsCertIds'
            , pcdsCertsByTicket = pcdsCertsByTicket'
            , pcdsLastTicketNo = pcdsLastTicketNo
            , pcdsLatestCertSeen = pcdsLatestCertSeen
            }
