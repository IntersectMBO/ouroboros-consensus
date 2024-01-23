{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Complete protocol parameters from partial protocol parameters.
module Ouroboros.Consensus.Cardano.Node.HasPartialProtocolParams (
    HasPartialProtocolParams (..)
  , PartialProtocolParams (..)
  ) where

import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import           Data.Kind (Type)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Byron.Node (ByronLeaderCredentials,
                     PBftSignatureThreshold, ProtocolParams (..))
import           Ouroboros.Consensus.Cardano.Node.ProtocolVersions
                     (ProtocolVersion)
import qualified Ouroboros.Consensus.Mempool.Capacity as Mempool
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.Eras (AllegraEra, AlonzoEra,
                     BabbageEra, ConwayEra, MaryEra, ShelleyEra)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Node (ProtocolParams (..))

{-------------------------------------------------------------------------------
  Class
-------------------------------------------------------------------------------}

class HasPartialProtocolParams blk where
  data PartialProtocolParams blk :: Type

  completeProtocolParams ::
       ProtocolVersion blk
    -> PartialProtocolParams blk
    -> ProtocolParams blk

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance HasPartialProtocolParams ByronBlock where
  data instance PartialProtocolParams ByronBlock =
    PartialProtocolParamsByron {
        pbyronGenesis                :: Genesis.Config
      , pbyronPbftSignatureThreshold :: Maybe PBftSignatureThreshold
      , pbyronSoftwareVersion        :: Update.SoftwareVersion
      , pbyronLeaderCredentials      :: Maybe ByronLeaderCredentials
      , pbyronMaxTxCapacityOverrides :: Mempool.TxOverrides ByronBlock
      }

  completeProtocolParams byronProtVer partialParamsByron =
      ProtocolParamsByron {
          byronGenesis = pbyronGenesis partialParamsByron
        , byronPbftSignatureThreshold = pbyronPbftSignatureThreshold partialParamsByron
        , byronProtocolVersion = byronProtVer
        , byronSoftwareVersion = pbyronSoftwareVersion partialParamsByron
        , byronLeaderCredentials = pbyronLeaderCredentials partialParamsByron
        , byronMaxTxCapacityOverrides = pbyronMaxTxCapacityOverrides partialParamsByron
        }

instance HasPartialProtocolParams (ShelleyBlock (TPraos c) (ShelleyEra c)) where
  newtype instance PartialProtocolParams (ShelleyBlock (TPraos c) (ShelleyEra c)) =
    PartialProtocolParamsShelley {
        pshelleyMaxTxCapacityOverrides :: Mempool.TxOverrides (ShelleyBlock (TPraos c) (ShelleyEra c))
      }

  completeProtocolParams protVer (PartialProtocolParamsShelley txOverrides) =
    ProtocolParamsShelley protVer txOverrides

instance HasPartialProtocolParams (ShelleyBlock (TPraos c) (AllegraEra c)) where
  newtype instance PartialProtocolParams (ShelleyBlock (TPraos c) (AllegraEra c)) =
    PartialProtocolParamsAllegra {
        pallegraMaxTxCapacityOverrides :: Mempool.TxOverrides (ShelleyBlock (TPraos c) (AllegraEra c))
      }

  completeProtocolParams protVer (PartialProtocolParamsAllegra txOverrides) =
    ProtocolParamsAllegra protVer txOverrides

instance HasPartialProtocolParams (ShelleyBlock (TPraos c) (MaryEra c)) where
  newtype instance PartialProtocolParams (ShelleyBlock (TPraos c) (MaryEra c)) =
    PartialProtocolParamsMary {
        pmaryMaxTxCapacityOverrides :: Mempool.TxOverrides (ShelleyBlock (TPraos c) (MaryEra c))
      }

  completeProtocolParams protVer (PartialProtocolParamsMary txOverrides) =
    ProtocolParamsMary protVer txOverrides

instance HasPartialProtocolParams (ShelleyBlock (TPraos c) (AlonzoEra c)) where
  newtype instance PartialProtocolParams (ShelleyBlock (TPraos c) (AlonzoEra c)) =
    PartialProtocolParamsAlonzo {
        palonzoMaxTxCapacityOverrides :: Mempool.TxOverrides (ShelleyBlock (TPraos c) (AlonzoEra c))
      }

  completeProtocolParams protVer (PartialProtocolParamsAlonzo txOverrides) =
    ProtocolParamsAlonzo protVer txOverrides

instance HasPartialProtocolParams (ShelleyBlock (Praos c) (BabbageEra c)) where
  newtype instance PartialProtocolParams (ShelleyBlock (Praos c) (BabbageEra c)) =
    PartialProtocolParamsBabbage {
        pbabbageMaxTxCapacityOverrides :: Mempool.TxOverrides (ShelleyBlock (Praos c) (BabbageEra c))
      }

  completeProtocolParams protVer (PartialProtocolParamsBabbage txOverrides) =
    ProtocolParamsBabbage protVer txOverrides

instance HasPartialProtocolParams (ShelleyBlock (Praos c) (ConwayEra c)) where
  newtype instance PartialProtocolParams (ShelleyBlock (Praos c) (ConwayEra c)) =
    PartialProtocolParamsConway {
        pconwayMaxTxCapacityOverrides :: Mempool.TxOverrides (ShelleyBlock (Praos c) (ConwayEra c))
      }

  completeProtocolParams protVer (PartialProtocolParamsConway txOverrides) =
    ProtocolParamsConway protVer txOverrides
