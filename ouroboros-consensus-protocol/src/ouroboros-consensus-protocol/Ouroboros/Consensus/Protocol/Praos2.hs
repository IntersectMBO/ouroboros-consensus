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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Praos with the Leios overlay.
module Ouroboros.Consensus.Protocol.Praos2
  ( LeiosOnly (..)
  , LeiosCrypto
  , Praos2
  , ConsensusConfig (..)
  ) where

import Cardano.Ledger.BaseTypes (Milliseconds32 (..), StrictMaybe (..))
import Cardano.Ledger.Chain (ChainChecksPParams (..))
import qualified Cardano.Ledger.Dijkstra.Forecast as Dijkstra
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.State (emptyLeiosCommittee)
import Cardano.Protocol.Crypto (Crypto, StandardCrypto)
import qualified Cardano.Protocol.Leios.BlockHeader as LeiosCodec
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Protocol.Abstract
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

instance LeiosCrypto c => ConsensusProtocol (Praos2 c) where
  type ChainDepState (Praos2 c) = PolyPraosState (Praos2 c)
  type IsLeader (Praos2 c) = PraosIsLeader c
  type CanBeLeader (Praos2 c) = PraosCanBeLeader c
  type TiebreakerView (Praos2 c) = PraosTiebreakerView c
  type LedgerView (Praos2 c) = Views.PolyPraosLedgerView (Praos2 c)
  type ValidationErr (Praos2 c) = PolyPraosValidationErr (Praos2 c) c
  type ValidateView (Praos2 c) = Views.PolyPraosValidateView (Praos2 c) c

  protocolSecurityParam = praosSecurityParam . praosParams . leiosPraosConfig

  checkIsLeader = checkIsLeaderPolyPraos . praosParams . leiosPraosConfig

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
      , Views.plvCommittee =
          Praos2HasLeios $ f ^. Dijkstra.leiosCommitteeForecastL @era @t
      , Views.plvQuorumStakeThreshold =
          Praos2HasLeios $ f ^. Dijkstra.leiosQuorumStakeThresholdForecastL @era @t
      , Views.plvAnnouncementPeriodLength =
          Praos2HasLeios $ f ^. Dijkstra.leiosAnnouncementPeriodLengthForecastL @era @t
      , Views.plvVotePeriodLength =
          Praos2HasLeios $ f ^. Dijkstra.leiosVotePeriodLengthForecastL @era @t
      , Views.plvDiffusionPeriodLength =
          Praos2HasLeios $ f ^. Dijkstra.leiosDiffusionPeriodLengthForecastL @era @t
      , Views.plvMaxEbBodySize =
          Praos2HasLeios $ f ^. Dijkstra.maxEndorserBlockReferencesSizeForecastL @era @t
      , Views.plvMaxEbTxsSize =
          Praos2HasLeios $ f ^. Dijkstra.maxEndorserBlockTxsSizeForecastL @era @t
      }
   where
    cc = SL.forecastChainChecks @t @era f

{-------------------------------------------------------------------------------
  Translation from the protocol without Leios
-------------------------------------------------------------------------------}

-- | Crossing from Praos into Praos with Leios.
--
-- Everything carries over unchanged; the Leios fields merely have to be
-- introduced. The announcement starts empty, since no header of the protocol
-- being left could have carried one. The ledger view seats no committee, so
-- nothing can be certified against it: the committee is empty and the quorum is
-- the entire weight. That is the truth, not a placeholder, until a snapshot
-- seated by the new era's rules rotates in, so the other values do not matter.
instance TranslateProto (Praos c) (Praos2 c) where
  translateLedgerView _ lv =
    Views.PraosLedgerView
      { Views.plvPoolDistr = Views.plvPoolDistr lv
      , Views.plvMaxHeaderSize = Views.plvMaxHeaderSize lv
      , Views.plvMaxBodySize = Views.plvMaxBodySize lv
      , Views.plvProtocolVersion = Views.plvProtocolVersion lv
      , Views.plvCommittee = Praos2HasLeios emptyLeiosCommittee
      , Views.plvQuorumStakeThreshold = Praos2HasLeios maxBound
      , Views.plvAnnouncementPeriodLength = Praos2HasLeios (Milliseconds32 0)
      , Views.plvVotePeriodLength = Praos2HasLeios (Milliseconds32 0)
      , Views.plvDiffusionPeriodLength = Praos2HasLeios (Milliseconds32 1000000000)
      , Views.plvMaxEbBodySize = Praos2HasLeios 0
      , Views.plvMaxEbTxsSize = Praos2HasLeios 0
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
      , praosStateLeiosAnnouncement = Praos2HasLeios SNothing
      }

instance forall c. TranslateProto (TPraos c) (Praos2 c) where
  translateLedgerView _ =
    translateLedgerView (Proxy @(Praos c, Praos2 c))
      . translateLedgerView (Proxy @(TPraos c, Praos c))

  translateChainDepState _ =
    translateChainDepState (Proxy @(Praos c, Praos2 c))
      . translateChainDepState (Proxy @(TPraos c, Praos c))

{-------------------------------------------------------------------------------
  PraosProtocolSupportsNode
-------------------------------------------------------------------------------}

instance LeiosCrypto c => PraosProtocolSupportsNode (Praos2 c) where
  type PraosProtocolSupportsNodeCrypto (Praos2 c) = c

  getPraosNonces _prx = getPraosNoncesPolyPraos

  getOpCertCounters _prx = getOpCertCountersPolyPraos
