{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators            #-}

module Legacy.Cardano.Block (
    LegacyCardanoBlock
  , LegacyCardanoEras
  , LegacyCardanoShelleyEras
  ) where

import           Data.Kind
import           Ouroboros.Consensus.Byron.Ledger.Block
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Legacy.Block
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Shelley.Ledger

type LegacyCardanoEras :: Type -> [Type]
type LegacyCardanoEras c =  LegacyBlock ByronBlock
                         ': LegacyCardanoShelleyEras c

type LegacyCardanoShelleyEras :: Type -> [Type]
type LegacyCardanoShelleyEras c =
  '[ LegacyBlock (ShelleyBlock (TPraos c) (ShelleyEra c))
   , LegacyBlock (ShelleyBlock (TPraos c) (AllegraEra c))
   , LegacyBlock (ShelleyBlock (TPraos c) (MaryEra c))
   , LegacyBlock (ShelleyBlock (TPraos c) (AlonzoEra c))
   , LegacyBlock (ShelleyBlock (Praos c)  (BabbageEra c))
   , LegacyBlock (ShelleyBlock (Praos c)  (ConwayEra c))
   ]

type LegacyCardanoBlock c = LegacyBlock (HardForkBlock (LegacyCardanoEras c))
