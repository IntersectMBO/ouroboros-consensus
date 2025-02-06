{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.Config (
    BlockConfig (..)
  , CodecConfig (..)
  , StorageConfig (..)
  , compactGenesis
  , getCompactGenesis
  , mkShelleyBlockConfig
    -- * opaque
  , CompactGenesis
  ) where

import           Cardano.Ledger.Binary (FromCBOR, ToCBOR)
import qualified Cardano.Ledger.Shelley.API as SL
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Protocol.Praos.Common
                     (VRFTiebreakerFlavor (..))
import           Ouroboros.Consensus.Shelley.Eras (isBeforeConway)
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Network.Magic (NetworkMagic (..))

{-------------------------------------------------------------------------------
  Additional node configuration
-------------------------------------------------------------------------------}

data instance BlockConfig (ShelleyBlock proto era) = ShelleyConfig {
      -- | The highest protocol version this node supports. It will be stored
      -- the headers of produced blocks.
      shelleyProtocolVersion     :: !SL.ProtVer
    , shelleySystemStart         :: !SystemStart
    , shelleyNetworkMagic        :: !NetworkMagic
      -- | For nodes that can produce blocks, this should be set to the
      -- verification key(s) corresponding to the node's signing key(s). For non
      -- block producing nodes, this can be set to the empty map.
    , shelleyBlockIssuerVKeys    :: !(Map (SL.KeyHash 'SL.BlockIssuer)
                                          (SL.VKey 'SL.BlockIssuer))
    , shelleyVRFTiebreakerFlavor :: !VRFTiebreakerFlavor
    }
  deriving stock (Generic)

deriving instance ShelleyBasedEra era => Show     (BlockConfig (ShelleyBlock proto era))
deriving instance ShelleyBasedEra era => NoThunks (BlockConfig (ShelleyBlock proto era))

mkShelleyBlockConfig ::
     forall proto era. ShelleyBasedEra era
  => SL.ProtVer
  -> SL.ShelleyGenesis
  -> [SL.VKey 'SL.BlockIssuer]
  -> BlockConfig (ShelleyBlock proto era)
mkShelleyBlockConfig protVer genesis blockIssuerVKeys = ShelleyConfig {
      shelleyProtocolVersion     = protVer
    , shelleySystemStart         = SystemStart  $ SL.sgSystemStart  genesis
    , shelleyNetworkMagic        = NetworkMagic $ SL.sgNetworkMagic genesis
    , shelleyBlockIssuerVKeys    = Map.fromList
        [ (SL.hashKey k, k)
        | k <- blockIssuerVKeys
        ]
    , shelleyVRFTiebreakerFlavor
    }
  where
    shelleyVRFTiebreakerFlavor
      | isBeforeConway (Proxy @era)
      = UnrestrictedVRFTiebreaker
      | otherwise
      -- See 'RestrictedVRFTiebreaker' for context. 5 slots is the "usual" value
      -- we consider when talking about the maximum propagation delay.
      = RestrictedVRFTiebreaker 5

{-------------------------------------------------------------------------------
  Codec config
-------------------------------------------------------------------------------}

-- | No particular codec configuration is needed for Shelley
data instance CodecConfig (ShelleyBlock proto era) = ShelleyCodecConfig
  deriving (Generic, NoThunks)

{-------------------------------------------------------------------------------
  Storage config
-------------------------------------------------------------------------------}

data instance StorageConfig (ShelleyBlock proto era) = ShelleyStorageConfig {
      -- | Needed for 'nodeCheckIntegrity'
      shelleyStorageConfigSlotsPerKESPeriod :: !Word64
      -- | Needed for 'nodeImmutableDbChunkInfo'
    , shelleyStorageConfigSecurityParam     :: !SecurityParam
    }
  deriving (Generic, NoThunks)

{-------------------------------------------------------------------------------
  Compact genesis
-------------------------------------------------------------------------------}

-- | Compact variant of 'SL.ShelleyGenesis' with some fields erased that are
-- only used on start-up and that should not be kept in memory forever.
--
-- Concretely:
--
-- * The 'sgInitialFunds' field is erased. It is only used to set up the initial
--   UTxO in tests and testnets.
--
-- * The 'sgStaking' field is erased. It is only used to register initial stake
--   pools in tests and benchmarks.
newtype CompactGenesis = CompactGenesis { getCompactGenesis :: SL.ShelleyGenesis }
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToCBOR, FromCBOR)

deriving anyclass instance NoThunks CompactGenesis

-- | Compacts the given 'SL.ShelleyGenesis'.
compactGenesis :: SL.ShelleyGenesis -> CompactGenesis
compactGenesis genesis = CompactGenesis $
    genesis {
        SL.sgInitialFunds = mempty
      , SL.sgStaking      = SL.emptyGenesisStaking
      }
