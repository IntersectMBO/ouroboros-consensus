{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Praos with the Leios overlay.
module Ouroboros.Consensus.Protocol.Praos2
  ( LeiosOnly (..)
  , LeiosCrypto
  , PerasOnly (..)
  , Praos2
  , Praos2CanBeLeader (..)
  , ConsensusConfig (..)
  ) where

import Cardano.Crypto.Leios (LeiosSigningKey)
import Cardano.Ledger.Chain (ChainChecksPParams (..))
import Cardano.Ledger.Core (TranslationContext)
import qualified Cardano.Ledger.Dijkstra.Forecast as Dijkstra
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Cardano.Ledger.Dijkstra.PParams (UpgradeDijkstraPParams (..))
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.State (emptyLeiosCommittee)
import Cardano.Protocol.Crypto (Crypto, StandardCrypto)
import qualified Cardano.Protocol.Leios.BlockHeader as LeiosCodec
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Maybe.Strict (StrictMaybe)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Strict.Either as Strict
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView (..), LeiosProtocol (..))
import Ouroboros.Consensus.Peras.Protocol (PerasLedgerView (..), PerasProtocol (..))
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.PolyPraos
  ( WhyNotCertifiable (..)
  , getContainsLeiosCertPolyPraos
  , getLeiosAnnouncementPolyPraos
  , getLeiosLedgerViewPolyPraos
  , getPerasLedgerViewPolyPraos
  , markUncertifiable
  , validateLeiosAnnouncementPolyPraos
  )
import Ouroboros.Consensus.Protocol.Praos
import Ouroboros.Consensus.Protocol.Praos.Common
  ( HasMaxMajorProtVer (..)
  , PraosCanBeLeader
  , PraosProtocolSupportsNode (..)
  , PraosTiebreakerView
  , ShelleyProtocolHeader
  , TypeSwitch (..)
  )
import Ouroboros.Consensus.Protocol.Praos.Orphans ()
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Views
import Ouroboros.Consensus.Protocol.TPraos (TPraos)

{-------------------------------------------------------------------------------
  The protocol
-------------------------------------------------------------------------------}

type Praos2 :: Type -> Type
data Praos2 c

type instance ShelleyProtocolHeader (Praos2 c) = LeiosCodec.Header c

instance PolyPraosCrypto (Praos2 StandardCrypto) StandardCrypto

-- | Praos's credentials to lead, and the BLS key to vote in Leios.
data Praos2CanBeLeader c = Praos2CanBeLeader
  { praos2CanBeLeaderPraos :: !(PraosCanBeLeader c)
  , praos2CanBeLeaderBLSKey :: !(StrictMaybe LeiosSigningKey)
  -- ^ None if the node does not vote.
  }
  deriving Generic

instance NoThunks (PraosCanBeLeader c) => NoThunks (Praos2CanBeLeader c)

class (Crypto c, PolyPraosCrypto (Praos2 c) c) => LeiosCrypto c

instance LeiosCrypto StandardCrypto

{-------------------------------------------------------------------------------
  The fields only this protocol has
-------------------------------------------------------------------------------}

-- | This protocol is Leios, so it holds the right alternative.
newtype instance LeiosOnly (Praos2 c) a b = Praos2HasLeios b
  deriving (Eq, Generic, Show)

deriving anyclass instance
  NoThunks b => NoThunks (LeiosOnly (Praos2 c) a b)

instance Functor (LeiosOnly (Praos2 c) a) where
  fmap f (Praos2HasLeios b) = Praos2HasLeios (f b)

instance Applicative (LeiosOnly (Praos2 c) a) where
  pure = Praos2HasLeios
  Praos2HasLeios f <*> Praos2HasLeios b = Praos2HasLeios (f b)

instance Foldable (LeiosOnly (Praos2 c) a) where
  foldMap f (Praos2HasLeios b) = f b

instance Traversable (LeiosOnly (Praos2 c) a) where
  traverse f (Praos2HasLeios b) = Praos2HasLeios <$> f b

instance TypeSwitch (LeiosOnly (Praos2 c)) where
  typeSwitchL = Praos2HasLeios ()
  typeSwitchR = Praos2HasLeios (Praos2HasLeios ())

-- | This protocol is Peras, so it holds the right alternative.
newtype instance PerasOnly (Praos2 c) a b = Praos2HasPeras b
  deriving (Eq, Generic, Show)

deriving anyclass instance
  NoThunks b => NoThunks (PerasOnly (Praos2 c) a b)

instance Functor (PerasOnly (Praos2 c) a) where
  fmap f (Praos2HasPeras b) = Praos2HasPeras (f b)

instance Applicative (PerasOnly (Praos2 c) a) where
  pure = Praos2HasPeras
  Praos2HasPeras f <*> Praos2HasPeras b = Praos2HasPeras (f b)

instance Foldable (PerasOnly (Praos2 c) a) where
  foldMap f (Praos2HasPeras b) = f b

instance Traversable (PerasOnly (Praos2 c) a) where
  traverse f (Praos2HasPeras b) = Praos2HasPeras <$> f b

instance TypeSwitch (PerasOnly (Praos2 c)) where
  typeSwitchL = Praos2HasPeras ()
  typeSwitchR = Praos2HasPeras (Praos2HasPeras ())

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Praos's configuration, wrapped only because 'ConsensusConfig' is a data
-- family.
--
-- Leios adds nothing to it: the periods, the committee, the quorum and the size
-- bounds all reach the protocol through the ledger view instead.
newtype instance ConsensusConfig (Praos2 c) = LeiosConfig
  { leiosPraosConfig :: ConsensusConfig (Praos c)
  }
  deriving Generic

deriving newtype instance Crypto c => NoThunks (ConsensusConfig (Praos2 c))

instance HasMaxMajorProtVer (Praos2 c) where
  protoMaxMajorPV = praosMaxMajorPV . praosParams . leiosPraosConfig

{-------------------------------------------------------------------------------
  ConsensusProtocol
-------------------------------------------------------------------------------}

instance LeiosCrypto c => LeiosProtocol (Praos2 c) where
  getLeiosAnnouncement _proxy = getLeiosAnnouncementPolyPraos
  getContainsLeiosCert _proxy = getContainsLeiosCertPolyPraos
  leiosBLSKeys _proxy = toList . praos2CanBeLeaderBLSKey
  validateLeiosAnnouncement (LeiosConfig (PraosConfig prms _ei)) b _slot =
    validateLeiosAnnouncementPolyPraos prms b
  getLeiosLedgerView _proxy = getLeiosLedgerViewPolyPraos

instance LeiosCrypto c => PerasProtocol (Praos2 c) where
  getPerasLedgerView _proxy = getPerasLedgerViewPolyPraos

instance LeiosCrypto c => ConsensusProtocol (Praos2 c) where
  type ChainDepState (Praos2 c) = PolyPraosState (Praos2 c)
  type IsLeader (Praos2 c) = PraosIsLeader c
  type CanBeLeader (Praos2 c) = Praos2CanBeLeader c
  type TiebreakerView (Praos2 c) = PraosTiebreakerView c
  type LedgerView (Praos2 c) = Views.PolyPraosLedgerView (Praos2 c)
  type ValidationErr (Praos2 c) = PolyPraosValidationErr (Praos2 c) c
  type ValidateView (Praos2 c) = Views.PolyPraosValidateView (Praos2 c) c

  protocolSecurityParam = praosSecurityParam . praosParams . leiosPraosConfig

  checkIsLeader cfg = checkIsLeaderPolyPraos (praosParams (leiosPraosConfig cfg)) . praos2CanBeLeaderPraos

  tickChainDepState = tickChainDepStatePolyPraos . praosEpochInfo . leiosPraosConfig

  -- The Leios header checks are cheap, so they run before the signature checks.
  updateChainDepState (LeiosConfig (PraosConfig prms ei)) = updateChainDepStatePolyPraos prms ei

  reupdateChainDepState (LeiosConfig (PraosConfig prms ei)) = reupdateChainDepStatePolyPraos prms ei

instance Dijkstra.DijkstraEraForecast era => Views.ForecastToPolyPraosLedgerView (Praos2 c) era where
  forecastToPolyPraosLedgerView (f :: SL.Forecast t era) =
    Views.PraosLedgerView
      { Views.plvPoolDistr = f ^. SL.poolDistrForecastL @era @t
      , Views.plvMaxHeaderSize = ccMaxBHSize cc
      , Views.plvMaxBodySize = ccMaxBBSize cc
      , Views.plvProtocolVersion = ccProtocolVersion cc
      , Views.plvLeios =
          Praos2HasLeios
            MkLeiosLedgerView
              { llvCommittee = f ^. Dijkstra.leiosCommitteeForecastL @era @t
              , llvQuorumStakeThreshold = f ^. Dijkstra.leiosQuorumStakeThresholdForecastL @era @t
              , llvAnnouncementPeriod = f ^. Dijkstra.leiosAnnouncementPeriodLengthForecastL @era @t
              , llvVotePeriod = f ^. Dijkstra.leiosVotePeriodLengthForecastL @era @t
              , llvDiffusionPeriod = f ^. Dijkstra.leiosDiffusionPeriodLengthForecastL @era @t
              , llvMaxEbBodySize = f ^. Dijkstra.maxEndorserBlockReferencesSizeForecastL @era @t
              , llvMaxEbTxsSize = f ^. Dijkstra.maxEndorserBlockTxsSizeForecastL @era @t
              }
      , Views.plvPeras =
          Praos2HasPeras
            MkPerasLedgerView
              { plvMinCandidateBlockAge = f ^. Dijkstra.perasMinCandidateBlockAgeForecastL @era @t
              , plvHealingFactor = f ^. Dijkstra.perasHealingFactorForecastL @era @t
              , plvCertBoost = f ^. Dijkstra.perasCertBoostForecastL @era @t
              , plvTargetCommitteeSize = f ^. Dijkstra.perasTargetCommitteeSizeForecastL @era @t
              , plvBootstrapRound = f ^. Dijkstra.perasBootstrapRoundForecastL @era @t
              , plvQuorumThresholdSafetyMargin = f ^. Dijkstra.perasQuorumThresholdSafetyMarginForecastL @era @t
              }
      }
   where
    cc = SL.forecastChainChecks @t @era f

{-------------------------------------------------------------------------------
  Translation from the protocol without Leios
-------------------------------------------------------------------------------}

-- | Crossing from Praos into Praos with Leios.
--
-- Everything carries over unchanged; the Leios fields merely have to be
-- introduced. Nothing has been announced, since no header of the protocol being
-- left could have carried an announcement. The Leios parameters come from the
-- Dijkstra genesis. The committee is empty: none is seated until a stake
-- snapshot rotation within the new era, beyond any forecast from the old one.
instance
  TranslationContext era ~ DijkstraGenesis =>
  TranslateProto (Praos c) (Praos2 c) era
  where
  translateLedgerView _prx DijkstraGenesis{dgUpgradePParams = upgrade} lv =
    Views.PraosLedgerView
      { Views.plvPoolDistr = Views.plvPoolDistr lv
      , Views.plvMaxHeaderSize = Views.plvMaxHeaderSize lv
      , Views.plvMaxBodySize = Views.plvMaxBodySize lv
      , Views.plvProtocolVersion = Views.plvProtocolVersion lv
      , Views.plvLeios =
          Praos2HasLeios
            MkLeiosLedgerView
              { llvCommittee = emptyLeiosCommittee
              , llvQuorumStakeThreshold = udppLeiosQuorumStakeThreshold upgrade
              , llvAnnouncementPeriod = udppLeiosAnnouncementPeriodLength upgrade
              , llvVotePeriod = udppLeiosVotePeriodLength upgrade
              , llvDiffusionPeriod = udppLeiosDiffusionPeriodLength upgrade
              , llvMaxEbBodySize = udppMaxEndorserBlockReferencesSize upgrade
              , llvMaxEbTxsSize = udppMaxEndorserBlockTxsSize upgrade
              }
      , Views.plvPeras =
          Praos2HasPeras
            MkPerasLedgerView
              { plvMinCandidateBlockAge = udppPerasMinCandidateBlockAge upgrade
              , plvHealingFactor = udppPerasHealingFactor upgrade
              , plvCertBoost = udppPerasCertBoost upgrade
              , plvTargetCommitteeSize = udppPerasTargetCommitteeSize upgrade
              , plvBootstrapRound = udppPerasBootstrapRound upgrade
              , plvQuorumThresholdSafetyMargin = udppPerasQuorumThresholdSafetyMargin upgrade
              }
      }

  translateChainDepState _ st =
    PraosState
      { praosStateLastSlot = praosStateLastSlot st
      , praosStateOCertCounters = praosStateOCertCounters st
      , praosStateEvolvingNonce = praosStateEvolvingNonce st
      , praosStateCandidateNonce = praosStateCandidateNonce st
      , praosStateEpochNonce = praosStateEpochNonce st
      , praosStatePreviousEpochNonce = praosStatePreviousEpochNonce st
      , praosStateLabNonce = praosStateLabNonce st
      , praosStateLastEpochBlockNonce = praosStateLastEpochBlockNonce st
      , praosStateLeiosAnnouncement = Praos2HasLeios (Strict.Left NothingAnnounced)
      }

-- | Crossing from one era with Leios into another.
--
-- Everything carries over unchanged except the announcement: it was made in the
-- era being left, so it is no longer certifiable, since an endorser block may
-- not be certified across an era boundary (to keep the design simpler).
--
-- Nothing uses this yet: it's dead code until two adjacent eras use 'Praos2'.
-- Even so, it's important to implement it _now_, because it would be easy to
-- forget, and doing so could introduce blindspots in the rest of the Leios
-- rules.
instance TranslateProto (Praos2 c) (Praos2 c) era where
  translateLedgerView _prx _ctx = id
  translateChainDepState _ st =
    st
      { praosStateLeiosAnnouncement =
          markUncertifiable AnnouncedInPreviousEra <$> praosStateLeiosAnnouncement st
      }

instance
  forall c era.
  TranslationContext era ~ DijkstraGenesis =>
  TranslateProto (TPraos c) (Praos2 c) era
  where
  translateLedgerView _prx ctx =
    translateLedgerView (Proxy @(Praos c, Praos2 c, era)) ctx
      . translateLedgerView (Proxy @(TPraos c, Praos c, era)) ctx

  translateChainDepState _ =
    translateChainDepState (Proxy @(Praos c, Praos2 c, era))
      . translateChainDepState (Proxy @(TPraos c, Praos c, era))

{-------------------------------------------------------------------------------
  PraosProtocolSupportsNode
-------------------------------------------------------------------------------}

instance LeiosCrypto c => PraosProtocolSupportsNode (Praos2 c) where
  type PraosProtocolSupportsNodeCrypto (Praos2 c) = c

  getPraosNonces _prx = getPraosNoncesPolyPraos

  getOpCertCounters _prx = getOpCertCountersPolyPraos
