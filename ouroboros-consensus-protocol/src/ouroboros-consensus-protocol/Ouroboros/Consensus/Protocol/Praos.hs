{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Praos with no extensions.
module Ouroboros.Consensus.Protocol.Praos
  ( ConsensusConfig (..)
  , LeiosOnly (..)
  , VoidUnlessLeios
  , WhenLeios
  , Praos
  , PraosCrypto
  , PraosLedgerView
  , PraosState
  , PraosValidateView
  , PraosValidationErr

    -- * Re-exports

    -- | These were defined in this module before
    -- "Ouroboros.Consensus.Protocol.PolyPraos" existed. They are re-exported only
    -- for that historical reason, so that the modules importing them here did not
    -- need to change.
  , AnnouncedBy (..)
  , PolyPraosCrypto
  , PolyPraosState (..)
  , PolyPraosValidationErr (..)
  , PraosCannotForge (..)
  , PraosFields (..)
  , PraosIsLeader (..)
  , PraosParams (..)
  , PraosToSign (..)
  , SerialisePraosState
  , Ticked (..)
  , checkIsLeaderPolyPraos
  , forgePraosFields
  , getOpCertCountersPolyPraos
  , getPraosNoncesPolyPraos
  , leiosContextFreeHeaderChecks
  , praosCheckCanForge
  , reupdateChainDepStatePolyPraos
  , updateChainDepStatePolyPraos
  , tickChainDepStatePolyPraos
  , validateKESSignature
  , validateVRFSignature

    -- ** For testing purposes
  , doValidateKESSignature
  , doValidateVRFSignature
  ) where

import Cardano.Ledger.Chain (ChainChecksPParams (..))
import qualified Cardano.Ledger.Chain as SL
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Protocol.Crypto (Crypto, StandardCrypto)
import qualified Cardano.Protocol.Praos.BlockHeader as PraosCodec
import qualified Cardano.Protocol.TPraos.API as SL
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as SL
import qualified Cardano.Protocol.TPraos.Rules.Tickn as SL
import Cardano.Slotting.EpochInfo (EpochInfo)
import Control.Monad.Except (Except)
import Data.Coerce (coerce)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.PolyPraos
import Ouroboros.Consensus.Protocol.Praos.Common
import Ouroboros.Consensus.Protocol.Praos.Orphans ()
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Views
import Ouroboros.Consensus.Protocol.TPraos
  ( ConsensusConfig (TPraosConfig, tpraosEpochInfo, tpraosParams)
  , TPraos
  , TPraosState (tpraosStateChainDepState, tpraosStateLastSlot)
  )

{-------------------------------------------------------------------------------
  The protocol
-------------------------------------------------------------------------------}

type Praos :: Type -> Type
data Praos c

type instance ShelleyProtocolHeader (Praos c) = PraosCodec.Header c

instance PolyPraosCrypto (Praos StandardCrypto) StandardCrypto

class (Crypto c, PolyPraosCrypto (Praos c) c) => PraosCrypto c

instance PraosCrypto StandardCrypto

type PraosState c = PolyPraosState (Praos c)

type PraosLedgerView c = Views.PolyPraosLedgerView (Praos c)

type PraosValidateView c = Views.PolyPraosValidateView (Praos c) c

type PraosValidationErr c = PolyPraosValidationErr (Praos c) c

{-------------------------------------------------------------------------------
  The fields only this protocol has
-------------------------------------------------------------------------------}

-- | Praos is not Leios, so it holds the left alternative.
newtype instance LeiosOnly (Praos c) a b = PraosLacksLeios a
  deriving (Eq, Generic, Show)

deriving anyclass instance NoThunks a => NoThunks (LeiosOnly (Praos c) a b)

instance Functor (LeiosOnly (Praos c) a) where
  fmap _ (PraosLacksLeios a) = PraosLacksLeios a

instance () ~ a => Applicative (LeiosOnly (Praos c) a) where
  pure _ = PraosLacksLeios ()
  PraosLacksLeios () <*> PraosLacksLeios () = PraosLacksLeios ()

instance Foldable (LeiosOnly (Praos c) a) where
  foldMap _ (PraosLacksLeios _) = mempty

instance Traversable (LeiosOnly (Praos c) a) where
  traverse _ (PraosLacksLeios a) = pure (PraosLacksLeios a)

instance TypeSwitch (LeiosOnly (Praos c)) where
  typeSwitchL = PraosLacksLeios (PraosLacksLeios ())
  typeSwitchR = PraosLacksLeios ()

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Static configuration
data instance ConsensusConfig (Praos c) = PraosConfig
  { praosParams :: !PraosParams
  , praosEpochInfo :: !(EpochInfo (Except History.PastHorizonException))
  -- it's useful for this record to be EpochInfo and one other thing,
  -- because the one other thing can then be used as the
  -- PartialConsensConfig in the HFC instance.
  }
  deriving Generic

instance Crypto c => NoThunks (ConsensusConfig (Praos c))

instance HasMaxMajorProtVer (Praos c) where
  protoMaxMajorPV = praosMaxMajorPV . praosParams

{-------------------------------------------------------------------------------
  ConsensusProtocol
-------------------------------------------------------------------------------}

instance PraosCrypto c => ConsensusProtocol (Praos c) where
  type ChainDepState (Praos c) = PolyPraosState (Praos c)
  type IsLeader (Praos c) = PraosIsLeader c
  type CanBeLeader (Praos c) = PraosCanBeLeader c
  type TiebreakerView (Praos c) = PraosTiebreakerView c
  type LedgerView (Praos c) = Views.PolyPraosLedgerView (Praos c)
  type ValidationErr (Praos c) = PolyPraosValidationErr (Praos c) c
  type ValidateView (Praos c) = Views.PolyPraosValidateView (Praos c) c

  protocolSecurityParam = praosSecurityParam . praosParams

  checkIsLeader = checkIsLeaderPolyPraos . praosParams

  tickChainDepState = tickChainDepStatePolyPraos . praosEpochInfo

  updateChainDepState (PraosConfig prms ei) = updateChainDepStatePolyPraos prms ei

  reupdateChainDepState (PraosConfig prms ei) = reupdateChainDepStatePolyPraos prms ei

instance Views.ForecastToPolyPraosLedgerView (Praos c) era where
  forecastToPolyPraosLedgerView (f :: SL.Forecast t era) =
    Views.PraosLedgerView
      { Views.plvPoolDistr = f ^. SL.poolDistrForecastL @era @t
      , Views.plvMaxHeaderSize = ccMaxBHSize cc
      , Views.plvMaxBodySize = ccMaxBBSize cc
      , Views.plvProtocolVersion = ccProtocolVersion cc
      , Views.plvCommittee = PraosLacksLeios ()
      , Views.plvQuorumStakeThreshold = PraosLacksLeios ()
      , Views.plvAnnouncementPeriodLength = PraosLacksLeios ()
      , Views.plvVotePeriodLength = PraosLacksLeios ()
      , Views.plvDiffusionPeriodLength = PraosLacksLeios ()
      , Views.plvMaxEbBodySize = PraosLacksLeios ()
      , Views.plvMaxEbTxsSize = PraosLacksLeios ()
      }
   where
    cc = SL.forecastChainChecks @t @era f

{-------------------------------------------------------------------------------
  Translation from transitional Praos
-------------------------------------------------------------------------------}

-- | We can translate between TPraos and Praos, provided:
--
-- - They share the same HASH algorithm
-- - They share the same ADDRHASH algorithm
-- - They share the same DSIGN verification keys
-- - They share the same VRF verification keys
instance TranslateProto (TPraos c) (Praos c) where
  translateLedgerView _ SL.TPraosLedgerView{SL.tplvPoolDistr, SL.tplvChainChecks} =
    Views.PraosLedgerView
      { Views.plvPoolDistr = tplvPoolDistr
      , Views.plvMaxHeaderSize = SL.ccMaxBHSize tplvChainChecks
      , Views.plvMaxBodySize = SL.ccMaxBBSize tplvChainChecks
      , Views.plvProtocolVersion = SL.ccProtocolVersion tplvChainChecks
      , Views.plvCommittee = PraosLacksLeios ()
      , Views.plvQuorumStakeThreshold = PraosLacksLeios ()
      , Views.plvAnnouncementPeriodLength = PraosLacksLeios ()
      , Views.plvVotePeriodLength = PraosLacksLeios ()
      , Views.plvDiffusionPeriodLength = PraosLacksLeios ()
      , Views.plvMaxEbBodySize = PraosLacksLeios ()
      , Views.plvMaxEbTxsSize = PraosLacksLeios ()
      }

  translateChainDepState _ tpState =
    PraosState
      { praosStateLastSlot = tpraosStateLastSlot tpState
      , praosStateOCertCounters = Map.mapKeysMonotonic coerce certCounters
      , praosStateEvolvingNonce = evolvingNonce
      , praosStateCandidateNonce = candidateNonce
      , praosStateEpochNonce = epochNonce
      , praosStatePreviousEpochNonce = epochNonce -- same as current epoch nonce
      , praosStateLabNonce = csLabNonce
      , praosStateLastEpochBlockNonce = SL.ticknStatePrevHashNonce csTickn
      , praosStateLeiosAnnouncement = PraosLacksLeios ()
      }
   where
    SL.ChainDepState{SL.csProtocol, SL.csTickn, SL.csLabNonce} =
      tpraosStateChainDepState tpState
    SL.PrtclState certCounters evolvingNonce candidateNonce =
      csProtocol
    epochNonce = SL.ticknStateEpochNonce csTickn

{-------------------------------------------------------------------------------
  PraosProtocolSupportsNode
-------------------------------------------------------------------------------}

instance PraosCrypto c => PraosProtocolSupportsNode (Praos c) where
  type PraosProtocolSupportsNodeCrypto (Praos c) = c

  getPraosNonces _prx = getPraosNoncesPolyPraos

  getOpCertCounters _prx = getOpCertCountersPolyPraos
