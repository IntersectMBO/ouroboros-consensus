{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Praos with the Leios overlay.
module Ouroboros.Consensus.Protocol.Leios
  ( LeiosCrypto
  , PraosWithLeios
  , ConsensusConfig (..)
  ) where

import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Chain (ChainChecksPParams (..))
import Cardano.Ledger.Keys (hashKey)
import Cardano.Protocol.Crypto (Crypto, StandardCrypto)
import qualified Cardano.Protocol.Leios.BlockHeader as LeiosCodec
import Cardano.Slotting.EpochInfo (epochInfoSlotLength)
import Cardano.Slotting.Slot (SlotNo)
import Ouroboros.Consensus.Block (WithOrigin (NotOrigin))
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.Except (Except, throwError)
import Data.Functor.Identity (runIdentity)
import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import LeiosDemoTypes (ebAnnouncementSize, minCertificationSlot)
import NoThunks.Class (NoThunks)
import qualified Cardano.Ledger.Dijkstra.Forecast as Dijkstra
import qualified Cardano.Ledger.Shelley.API as SL
import Lens.Micro ((^.))
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.Praos
import Ouroboros.Consensus.Protocol.Praos.Common
  ( EitherLeiosF
  , PraosCanBeLeader
  , PraosTiebreakerView
  , ShelleyProtocolHeader
  , fromCodecEbAnnouncement
  )
import Ouroboros.Consensus.Protocol.Praos.Orphans ()
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Views

{-------------------------------------------------------------------------------
  The protocol
-------------------------------------------------------------------------------}

type PraosWithLeios :: Type -> Type
data PraosWithLeios c

type instance ShelleyProtocolHeader (PraosWithLeios c) = LeiosCodec.Header c

instance BasePraosCrypto (PraosWithLeios StandardCrypto) StandardCrypto

type LeiosCrypto c = BasePraosCrypto (PraosWithLeios c) c

{-------------------------------------------------------------------------------
  The fields only this protocol has
-------------------------------------------------------------------------------}

-- | This protocol is Leios, so it holds the right alternative.
newtype instance EitherLeiosF (PraosWithLeios c) a b = LeiosLeiosRight b
  deriving (Eq, Generic, Show)
  deriving newtype NFData

deriving anyclass instance
  NoThunks b => NoThunks (EitherLeiosF (PraosWithLeios c) a b)

instance Functor (EitherLeiosF (PraosWithLeios c) a) where
  fmap f (LeiosLeiosRight b) = LeiosLeiosRight (f b)

instance Applicative (EitherLeiosF (PraosWithLeios c) a) where
  pure = LeiosLeiosRight
  LeiosLeiosRight f <*> LeiosLeiosRight b = LeiosLeiosRight (f b)

instance Foldable (EitherLeiosF (PraosWithLeios c) a) where
  foldMap f (LeiosLeiosRight b) = f b

instance Traversable (EitherLeiosF (PraosWithLeios c) a) where
  traverse f (LeiosLeiosRight b) = LeiosLeiosRight <$> f b

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Praos's configuration, wrapped only because 'ConsensusConfig' is a data
-- family.
--
-- Leios adds nothing to it: the periods, the committee, the quorum and the size
-- bounds all reach the protocol through the ledger view instead.
newtype instance ConsensusConfig (PraosWithLeios c) = LeiosConfig
  { leiosPraosConfig :: ConsensusConfig (Praos c)
  }
  deriving Generic

deriving newtype instance Crypto c => NoThunks (ConsensusConfig (PraosWithLeios c))

{-------------------------------------------------------------------------------
  Chain-dep state
-------------------------------------------------------------------------------}

-- | Its own format, which merely also starts counting; this is the version the
-- Leios prototype has been writing, so snapshots already on disk still decode.
instance Typeable c => SerialisePraosState (PraosWithLeios c) where
  versionOfPraosState _ = 1
  countOfFieldsInPraosState _ = 9

{-------------------------------------------------------------------------------
  The header checks this protocol adds
-------------------------------------------------------------------------------}

-- | The Leios header checks that read only the header and the ledger view.
--
-- Sound out of context, which is what lets both header paths run them:
-- 'updateChainDepState', from the header's own predecessor, and
-- 'validateAnnouncementChainDepState', from the immutable tip. The bound they
-- check is forecast for the header's own slot, so both paths read the same
-- value: a forecast either yields the view that slot will have or refuses as
-- 'OutsideHorizon'. The staleness the announcement path does have to live with
-- is in the chain-dep state --- the opcert counters --- and none of these
-- checks reads it.
leiosContextFreeHeaderChecks ::
  Views.BasePraosLedgerView (PraosWithLeios c) ->
  Views.BasePraosValidateView (PraosWithLeios c) c ->
  Except (BasePraosValidationErr (PraosWithLeios c) c) ()
leiosContextFreeHeaderChecks lv b = do
  let LeiosLeiosRight (_containsCert, mbAnn) = Views.hvLeios b
      LeiosLeiosRight maxEbBodySize = Views.plvMaxEbBodySize lv
  case mbAnn of
    SNothing -> pure ()
    SJust ann -> do
      let announced = ebAnnouncementSize ann
          -- TEMPORARY KLUDGE -- DO NOT MERGE into main.
          --
          -- The deployed testnet has historical announcements above the
          -- 'maxEndorserBlockReferencesSize' its own Dijkstra genesis sets
          -- (e.g. 102429 against 100000 at slot 709083), so enforcing the
          -- ledger's value stalls the sync there. Exception granted here and
          -- here only: every other use of the limit, and the genesis file
          -- itself, are untouched.
          maximum' = max 200000 maxEbBodySize
      when (announced > maximum') $
        throwError $
          LeiosEbTooBig (LeiosLeiosRight ()) announced maximum'

-- | The Leios-specific checks on a header, called by 'updateChainDepState'.
--
-- 'leiosContextFreeHeaderChecks' plus the one check that needs the header's
-- immediate predecessor: a CertRB may not certify an announcement younger than
-- the certification gap, and only the predecessor's state says which
-- announcement that is.
leiosHeaderChecks ::
  ConsensusConfig (PraosWithLeios c) ->
  Views.BasePraosLedgerView (PraosWithLeios c) ->
  Views.BasePraosValidateView (PraosWithLeios c) c ->
  SlotNo ->
  BasePraosState (PraosWithLeios c) ->
  Except (BasePraosValidationErr (PraosWithLeios c) c) ()
leiosHeaderChecks cfg lv b slot cs = do
  leiosContextFreeHeaderChecks lv b
  let LeiosLeiosRight (containsCert, _mbAnn) = Views.hvLeios b
      LeiosLeiosRight announcementPeriod = Views.plvAnnouncementPeriodLength lv
      LeiosLeiosRight votePeriod = Views.plvVotePeriodLength lv
      LeiosLeiosRight diffusionPeriod = Views.plvDiffusionPeriodLength lv
      LeiosLeiosRight announcedByPredecessor = praosStateLeiosAnnouncement cs
      PraosConfig{praosEpochInfo} = leiosPraosConfig cfg

  -- Note that the genesis state doesn't announce an EB.
  when containsCert $
    case (announcedByPredecessor, praosStateLastSlot cs) of
      (SJust{}, NotOrigin announcingSlot) -> do
        let earliestAllowed =
              minCertificationSlot
                ( runIdentity $
                    epochInfoSlotLength
                      (History.toPureEpochInfo praosEpochInfo)
                      slot
                )
                announcementPeriod
                votePeriod
                diffusionPeriod
                announcingSlot
        when (slot < earliestAllowed) $
          throwError $
            LeiosCertTooYoung (LeiosLeiosRight ()) announcingSlot slot earliestAllowed
      -- A state that announced an EB has necessarily applied a header, so
      -- 'Origin' is the same situation as announcing nothing.
      _ -> throwError $ LeiosCertWithoutAnnouncement (LeiosLeiosRight ())

{-------------------------------------------------------------------------------
  ConsensusProtocol
-------------------------------------------------------------------------------}

instance LeiosCrypto c => ConsensusProtocol (PraosWithLeios c) where
  type ChainDepState (PraosWithLeios c) = BasePraosState (PraosWithLeios c)
  type IsLeader (PraosWithLeios c) = PraosIsLeader c
  type CanBeLeader (PraosWithLeios c) = PraosCanBeLeader c
  type TiebreakerView (PraosWithLeios c) = PraosTiebreakerView c
  type LedgerView (PraosWithLeios c) = Views.BasePraosLedgerView (PraosWithLeios c)
  type ValidationErr (PraosWithLeios c) = BasePraosValidationErr (PraosWithLeios c) c
  type ValidateView (PraosWithLeios c) = Views.BasePraosValidateView (PraosWithLeios c) c

  protocolSecurityParam = praosSecurityParam . praosParams . leiosPraosConfig

  checkIsLeader cfg = checkIsLeaderPraos (praosParams (leiosPraosConfig cfg))

  tickChainDepState cfg = tickPraosState (praosEpochInfo (leiosPraosConfig cfg))

  updateChainDepState cfg b slot tcs = do
    -- The Leios header checks. Cheap, so they run before the signature checks.
    --
    -- NB cert/txs exclusivity is not among these: it is a property of the body,
    -- and 'blockMatchesHeader' already enforces it where the body is in hand.
    -- Nor is the EB closure's size: the announcement carries only one size, and
    -- it is the body's.
    leiosHeaderChecks cfg lv b slot cs
    -- Everything below this line exactly matches the 'Praos' instance, _except_
    -- the 'reupdateChainDepState' continuation is also at type 'Leios'.
    validateKESSignature prms lv (praosStateOCertCounters cs) b
    validateVRFSignature (praosStateEpochNonce cs) lv (praosLeaderF prms) b
    pure $ reupdateChainDepState cfg b slot tcs
   where
    PraosConfig{praosParams = prms} = leiosPraosConfig cfg
    lv = tickedPraosStateLedgerView tcs
    cs = tickedPraosStateChainDepState tcs

  reupdateChainDepState cfg b slot tcs =
    reupdatePraosState prms ei upd b slot (tickedPraosStateChainDepState tcs)
   where
    PraosConfig prms ei = leiosPraosConfig cfg

    -- A header with no announcement clears the field.
    upd _old =
      LeiosLeiosRight $
        MkAnnouncedBy (hashKey (Views.hvVK b)) . fromCodecEbAnnouncement
          <$> LeiosCodec.hbEbAnnouncement (Views.hvSigned b)

instance Dijkstra.DijkstraEraForecast era => Views.ForecastsLeios (PraosWithLeios c) era where
  forecastToBasePraosLedgerView (f :: SL.Forecast t era) =
    Views.PraosLedgerView
      { Views.plvPoolDistr = f ^. SL.poolDistrForecastL @era @t
      , Views.plvMaxHeaderSize = ccMaxBHSize cc
      , Views.plvMaxBodySize = ccMaxBBSize cc
      , Views.plvProtocolVersion = ccProtocolVersion cc
      , Views.plvCommittee =
          LeiosLeiosRight $ f ^. Dijkstra.leiosCommitteeForecastL @era @t
      , Views.plvQuorumStakeThreshold =
          LeiosLeiosRight $ f ^. Dijkstra.leiosQuorumStakeThresholdForecastL @era @t
      , Views.plvAnnouncementPeriodLength =
          LeiosLeiosRight $ f ^. Dijkstra.leiosAnnouncementPeriodLengthForecastL @era @t
      , Views.plvVotePeriodLength =
          LeiosLeiosRight $ f ^. Dijkstra.leiosVotePeriodLengthForecastL @era @t
      , Views.plvDiffusionPeriodLength =
          LeiosLeiosRight $ f ^. Dijkstra.leiosDiffusionPeriodLengthForecastL @era @t
      , Views.plvMaxEbBodySize =
          LeiosLeiosRight $ f ^. Dijkstra.maxEndorserBlockReferencesSizeForecastL @era @t
      , Views.plvMaxEbTxsSize =
          LeiosLeiosRight $ f ^. Dijkstra.maxEndorserBlockTxsSizeForecastL @era @t
      }
   where
    cc = SL.forecastChainChecks @t @era f
