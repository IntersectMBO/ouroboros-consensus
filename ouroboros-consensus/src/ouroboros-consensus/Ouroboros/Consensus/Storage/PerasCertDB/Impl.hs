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

import Control.Monad (unless, when)
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
  , pcdsLatestCertSeen :: !(Maybe (WithBoostedBlockStatus (WithArrivalTime (ValidatedPerasCert blk))))
  -- ^ The certificate with the highest round number that has been added to the
  -- db since it has been opened.
  , pcdsUnsettled ::
      !(Map (WithOrigin SlotNo) (Map PerasRoundNo (WithArrivalTime (ValidatedPerasCert blk))))
  -- ^ The certificates that have not been settled yet (see 'markCertsSettled'),
  -- indexed by the slot of their boosted block and their round number.
  --
  -- INVARIANT: Every certificate in here is also in 'pcdsCertsByTicket', and
  -- is indexed by its own boosted slot and round number. No inner map is
  -- empty.
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
      { pcdsCertIds = Set.empty
      , pcdsCertsByTicket = Map.empty
      , pcdsLastTicketNo = zeroPerasCertTicketNo
      , pcdsLatestCertSeen = Nothing
      , pcdsUnsettled = Map.empty
      }
    (Fingerprint 0)

-- | Check that the fields of 'PerasCertDbState' are in sync.
invariantForPerasCertDbState ::
  IsPerasCert (PerasCert blk) blk =>
  WithFingerprint (PerasCertDbState blk) ->
  Either String ()
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
  for_ (Map.toList pcdsUnsettled) $ \(slot, certs) -> do
    when (Map.null certs) $
      throwError $
        "Empty set of unsettled certificates at slot " <> show slot
    for_ (Map.toList certs) $ \(roundNo, cert) -> do
      checkEqual "unsettled certificate round" (getPerasCertRound cert) roundNo
      checkEqual "unsettled certificate slot" (pointSlot (getPerasCertPoint cert)) slot
      when (roundNo `Set.notMember` pcdsCertIds) $
        throwError $
          "Unsettled certificate not in the db: " <> show roundNo
 where
  PerasCertDbState
    { pcdsCertIds
    , pcdsCertsByTicket
    , pcdsLastTicketNo
    , pcdsUnsettled
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
  | -- | Garbage collection for the given slot kept these certificates, despite
    -- them boosting an older slot, because they have not been settled yet.
    RetainedUnsettledCerts
      SlotNo
      (Set PerasRoundNo)

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
      , getCertsBoosting = implGetCertsBoosting env
      , getCertsAfter = implGetCertsAfter env
      , getWeightSnapshot = implGetWeightSnapshot env
      , getLatestCertSeen = implGetLatestCertSeen env
      , getUnsettledCertsUpTo = implGetUnsettledCertsUpTo env
      , markCertsSettled = implMarkCertsSettled env
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
    if Set.member roundNo (pcdsCertIds pcds)
      then pure PerasCertAlreadyInDB
      else do
        let pcdsLastTicketNo' = succ (pcdsLastTicketNo pcds)
            pcdsCertIds' = Set.insert roundNo (pcdsCertIds pcds)
            pcdsCertsByTicket' = Map.insert pcdsLastTicketNo' cert (pcdsCertsByTicket pcds)
            pcdsLatestCertSeen' =
              case pcdsLatestCertSeen pcds of
                Nothing ->
                  Just (CertBoostingBlockInVolatileDB cert)
                Just prev
                  | getPerasCertRound cert
                      > getPerasCertRound (forgetBoostedBlockStatus prev) ->
                      Just (CertBoostingBlockInVolatileDB cert)
                  | otherwise ->
                      Just prev
            pcdsUnsettled' =
              Map.insertWith
                Map.union
                (pointSlot (getPerasCertPoint cert))
                (Map.singleton roundNo cert)
                (pcdsUnsettled pcds)
        writeTVar pcdbState $
          WithFingerprint
            PerasCertDbState
              { pcdsCertIds = pcdsCertIds'
              , pcdsCertsByTicket = pcdsCertsByTicket'
              , pcdsLastTicketNo = pcdsLastTicketNo'
              , pcdsLatestCertSeen = pcdsLatestCertSeen'
              , pcdsUnsettled = pcdsUnsettled'
              }
            (succ fp)
        pure AddedPerasCertToDB
  pure $ do
    traceWith pcdbTracer (AddCert roundNo cert addPerasCertRes)
    pure addPerasCertRes

implGetCertsBoosting ::
  ( IOLike m
  , StandardHash blk
  , IsPerasCert (PerasCert blk) blk
  ) =>
  PerasCertDbEnv m blk ->
  Point blk ->
  STM m [WithArrivalTime (ValidatedPerasCert blk)]
implGetCertsBoosting PerasCertDbEnv{pcdbState} pt = do
  PerasCertDbState{pcdsCertsByTicket} <-
    forgetFingerprint <$> readTVar pcdbState
  pure
    [ cert
    | cert <- Map.elems pcdsCertsByTicket
    , getPerasCertPoint cert == pt
    ]

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
  STM m (Maybe (WithBoostedBlockStatus (WithArrivalTime (ValidatedPerasCert blk))))
implGetLatestCertSeen PerasCertDbEnv{pcdbState} = do
  PerasCertDbState{pcdsLatestCertSeen} <-
    forgetFingerprint <$> readTVar pcdbState
  pure pcdsLatestCertSeen

implGetUnsettledCertsUpTo ::
  IOLike m =>
  PerasCertDbEnv m blk ->
  WithOrigin SlotNo ->
  STM m [WithArrivalTime (ValidatedPerasCert blk)]
implGetUnsettledCertsUpTo PerasCertDbEnv{pcdbState} slot = do
  PerasCertDbState{pcdsUnsettled} <-
    forgetFingerprint <$> readTVar pcdbState
  pure $
    concatMap Map.elems $
      Map.elems $
        Map.takeWhileAntitone (<= slot) pcdsUnsettled

implMarkCertsSettled ::
  IOLike m =>
  PerasCertDbEnv m blk ->
  Set PerasRoundNo ->
  STM m ()
implMarkCertsSettled PerasCertDbEnv{pcdbState} roundNos =
  -- No need to update the 'Fingerprint': settling a certificate does not change
  -- the weights.
  modifyTVar pcdbState $ fmap $ \pcds ->
    pcds
      { pcdsUnsettled =
          Map.mapMaybe
            ( \certs ->
                let certs' = certs `Map.withoutKeys` roundNos
                 in if Map.null certs' then Nothing else Just certs'
            )
            (pcdsUnsettled pcds)
      }

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
  WithFingerprint pcds fp <- readTVar pcdbState
  writeTVar pcdbState $ WithFingerprint (gc pcds) fp
  let retained =
        Set.unions
          [ Map.keysSet certs
          | certs <-
              Map.elems $
                Map.takeWhileAntitone (< NotOrigin slotNo) (pcdsUnsettled pcds)
          ]
  pure $ do
    traceWith pcdbTracer (GarbageCollected slotNo)
    unless (Set.null retained) $
      traceWith pcdbTracer (RetainedUnsettledCerts slotNo retained)
 where
  gc :: PerasCertDbState blk -> PerasCertDbState blk
  gc
    PerasCertDbState
      { pcdsCertsByTicket
      , pcdsLastTicketNo
      , pcdsLatestCertSeen
      , pcdsUnsettled
      } =
      let isUnsettled cert =
            maybe False (Map.member (getPerasCertRound cert)) $
              Map.lookup (pointSlot (getPerasCertPoint cert)) pcdsUnsettled
          pcdsCertsByTicket' =
            Map.filter
              ( \cert ->
                  pointSlot (getPerasCertPoint cert) >= NotOrigin slotNo
                    || isUnsettled cert
              )
              pcdsCertsByTicket
          pcdsCertIds' =
            Set.fromList (getPerasCertRound <$> Map.elems pcdsCertsByTicket')
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
            { pcdsCertIds = pcdsCertIds'
            , pcdsCertsByTicket = pcdsCertsByTicket'
            , pcdsLastTicketNo = pcdsLastTicketNo
            , pcdsLatestCertSeen = pcdsLatestCertSeen'
            , pcdsUnsettled = pcdsUnsettled
            }
