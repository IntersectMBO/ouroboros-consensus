{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- DUPLICATE -- adapted from: cardano-node/src/Cardano/Node/Types.hs

module Cardano.Node.Types
  ( -- * Configuration
    AdjustFilePaths (..)
  , ConfigError (..)
  , ConfigYamlFilePath (..)
  , DbFile (..)
  , GenesisFile (..)
  , GenesisHash (..)
  , MaxConcurrencyBulkSync (..)
  , MaxConcurrencyDeadline (..)
  , ProtocolFilepaths (..)

    -- * Consensus protocol configuration
  , NodeAlonzoProtocolConfiguration (..)
  , NodeByronProtocolConfiguration (..)
  , NodeConwayProtocolConfiguration (..)
  , NodeDijkstraProtocolConfiguration (..)
  , NodeHardForkProtocolConfiguration (..)
  , NodeProtocolConfiguration (..)
  , NodeShelleyProtocolConfiguration (..)
  , VRFPrivateKeyFilePermissionError (..)
  , renderVRFPrivateKeyFilePermissionError
  ) where

import qualified Cardano.Chain.Update as Byron
import Cardano.Crypto (RequiresNetworkMagic)
import qualified Cardano.Crypto.Hash as Crypto
import Data.Aeson
import Data.String (IsString)
import Data.Text as Text (Text, pack, unpack)
import Data.Word (Word16, Word8)
import Ouroboros.Consensus.Block.Abstract (EpochNo)

-- | Errors for the cardano-config module.
data ConfigError
  = ConfigErrorFileNotFound FilePath
  | ConfigErrorNoEKG
  deriving Show

-- | Filepath of the configuration yaml file. This file determines
-- all the configuration settings required for the cardano node
-- (logging, tracing, protocol, slot length etc)
newtype ConfigYamlFilePath = ConfigYamlFilePath
  {unConfigPath :: FilePath}
  deriving newtype (Eq, Show)

newtype DbFile = DbFile
  {unDB :: FilePath}
  deriving newtype (Eq, Show)

newtype GenesisFile = GenesisFile
  {unGenesisFile :: FilePath}
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

instance FromJSON GenesisFile where
  parseJSON (String genFp) = pure . GenesisFile $ Text.unpack genFp
  parseJSON invalid =
    fail $
      "Parsing of GenesisFile failed due to type mismatch. "
        <> "Encountered: "
        <> show invalid

newtype MaxConcurrencyBulkSync = MaxConcurrencyBulkSync
  {unMaxConcurrencyBulkSync :: Word}
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, Show)

newtype MaxConcurrencyDeadline = MaxConcurrencyDeadline
  {unMaxConcurrencyDeadline :: Word}
  deriving stock (Eq, Ord)
  deriving newtype (FromJSON, Show)

{-
-- | Newtype wrapper which provides 'FromJSON' instance for 'DiffusionMode'.
--
newtype NodeDiffusionMode
  = NodeDiffusionMode { getDiffusionMode :: DiffusionMode }
  deriving newtype Show

instance FromJSON NodeDiffusionMode where
    parseJSON (String str) =
      case str of
        "InitiatorOnly"
          -> pure $ NodeDiffusionMode InitiatorOnlyDiffusionMode
        "InitiatorAndResponder"
          -> pure $ NodeDiffusionMode InitiatorAndResponderDiffusionMode
        _ -> fail "Parsing NodeDiffusionMode failed: can be either 'InitiatorOnly' or 'InitiatorAndResponder'"
    parseJSON _ = fail "Parsing NodeDiffusionMode failed"
-}

class AdjustFilePaths a where
  adjustFilePaths :: (FilePath -> FilePath) -> a -> a

data ProtocolFilepaths
  = ProtocolFilepaths
  { byronCertFile :: !(Maybe FilePath)
  , byronKeyFile :: !(Maybe FilePath)
  , shelleyKESFile :: !(Maybe FilePath)
  , shelleyVRFFile :: !(Maybe FilePath)
  , shelleyCertFile :: !(Maybe FilePath)
  , shelleyBulkCredsFile :: !(Maybe FilePath)
  }
  deriving (Eq, Show)

newtype GenesisHash = GenesisHash (Crypto.Hash Crypto.Blake2b_256 Crypto.ByteString)
  deriving newtype (Eq, Show, ToJSON, FromJSON)

data NodeProtocolConfiguration
  = NodeProtocolConfigurationByron NodeByronProtocolConfiguration
  | NodeProtocolConfigurationShelley NodeShelleyProtocolConfiguration
  | NodeProtocolConfigurationCardano
      NodeByronProtocolConfiguration
      NodeShelleyProtocolConfiguration
      NodeAlonzoProtocolConfiguration
      NodeConwayProtocolConfiguration
      NodeDijkstraProtocolConfiguration
      NodeHardForkProtocolConfiguration
  deriving (Eq, Show)

data NodeShelleyProtocolConfiguration
  = NodeShelleyProtocolConfiguration
  { npcShelleyGenesisFile :: !GenesisFile
  , npcShelleyGenesisFileHash :: !(Maybe GenesisHash)
  }
  deriving (Eq, Show)

data NodeAlonzoProtocolConfiguration
  = NodeAlonzoProtocolConfiguration
  { npcAlonzoGenesisFile :: !GenesisFile
  , npcAlonzoGenesisFileHash :: !(Maybe GenesisHash)
  }
  deriving (Eq, Show)

data NodeByronProtocolConfiguration
  = NodeByronProtocolConfiguration
  { npcByronGenesisFile :: !GenesisFile
  , npcByronGenesisFileHash :: !(Maybe GenesisHash)
  , npcByronReqNetworkMagic :: !RequiresNetworkMagic
  , npcByronPbftSignatureThresh :: !(Maybe Double)
  , -- TODO: eliminate these two: it can be hard-coded

    npcByronApplicationName :: !Byron.ApplicationName
  -- ^ Update application name.
  , npcByronApplicationVersion :: !Byron.NumSoftwareVersion
  -- ^ Application (ie software) version.
  , -- TODO: eliminate these: it can be done automatically in consensus

    npcByronSupportedProtocolVersionMajor :: !Word16
  -- ^ These declare the version of the protocol that the node is prepared
  -- to run. This is usually the version of the protocol in use on the
  -- chain now, but during protocol updates this version will be the one
  -- that we declare that we are ready to move to. This is the endorsement
  -- mechanism for determining when enough block producers are ready to
  -- move to the next version.
  , npcByronSupportedProtocolVersionMinor :: !Word16
  , npcByronSupportedProtocolVersionAlt :: !Word8
  }
  deriving (Eq, Show)

data NodeConwayProtocolConfiguration
  = NodeConwayProtocolConfiguration
  { npcConwayGenesisFile :: !GenesisFile
  , npcConwayGenesisFileHash :: !(Maybe GenesisHash)
  }
  deriving (Eq, Show)

data NodeDijkstraProtocolConfiguration
  = NodeDijkstraProtocolConfiguration
  { npcDijkstraGenesisFile :: !GenesisFile
  , npcDijkstraGenesisFileHash :: !(Maybe GenesisHash)
  }
  deriving (Eq, Show)

-- | Configuration relating to a hard forks themselves, not the specific eras.
data NodeHardForkProtocolConfiguration
  = NodeHardForkProtocolConfiguration
  { npcTestEnableDevelopmentHardForkEras :: Bool
  -- ^ During the development and integration of new eras we wish to be
  -- able to test the hard fork transition into the new era, but we do not
  -- wish to generally have the node advertise that it understands the new
  -- era. Avoiding advertising new development eras until they are ready
  -- makes it practical to include new not-yet-ready eras into the main
  -- release version of the node without the danger that operators on the
  -- mainnet will prematurely advertise that their nodes are capable of
  -- crossing the next hard fork.
  --
  -- It should /always/ remain at the default of false for nodes running
  -- on the mainnet.
  --
  -- This flag should be set to true for nodes taking part in testnets for
  -- testing the new era.
  , npcTestShelleyHardForkAtEpoch :: Maybe EpochNo
  -- ^ For testing purposes we support specifying that the hard fork
  -- happens at an exact epoch number (ie the first epoch of the new era).
  --
  -- Obviously if this is used, all the nodes in the test cluster must be
  -- configured the same, or they will disagree.
  , npcTestAllegraHardForkAtEpoch :: Maybe EpochNo
  -- ^ For testing purposes we support specifying that the hard fork
  -- happens at an exact epoch number (ie the first epoch of the new era).
  --
  -- Obviously if this is used, all the nodes in the test cluster must be
  -- configured the same, or they will disagree.
  , npcTestMaryHardForkAtEpoch :: Maybe EpochNo
  -- ^ For testing purposes we support specifying that the hard fork
  -- happens at an exact epoch number (ie the first epoch of the new era).
  --
  -- Obviously if this is used, all the nodes in the test cluster must be
  -- configured the same, or they will disagree.
  , npcTestAlonzoHardForkAtEpoch :: Maybe EpochNo
  -- ^ For testing purposes we support specifying that the hard fork
  -- happens at an exact epoch number (ie the first epoch of the new era).
  --
  -- Obviously if this is used, all the nodes in the test cluster must be
  -- configured the same, or they will disagree.
  , npcTestBabbageHardForkAtEpoch :: Maybe EpochNo
  , npcTestConwayHardForkAtEpoch :: Maybe EpochNo
  , npcTestDijkstraHardForkAtEpoch :: Maybe EpochNo
  }
  deriving (Eq, Show)

instance AdjustFilePaths NodeProtocolConfiguration where
  adjustFilePaths f (NodeProtocolConfigurationByron pc) =
    NodeProtocolConfigurationByron (adjustFilePaths f pc)
  adjustFilePaths f (NodeProtocolConfigurationShelley pc) =
    NodeProtocolConfigurationShelley (adjustFilePaths f pc)
  adjustFilePaths f (NodeProtocolConfigurationCardano pcb pcs pca pcc pcd pch) =
    NodeProtocolConfigurationCardano
      (adjustFilePaths f pcb)
      (adjustFilePaths f pcs)
      (adjustFilePaths f pca)
      (adjustFilePaths f pcc)
      (adjustFilePaths f pcd)
      pch

instance AdjustFilePaths NodeByronProtocolConfiguration where
  adjustFilePaths
    f
    x@NodeByronProtocolConfiguration
      { npcByronGenesisFile
      } =
      x{npcByronGenesisFile = adjustFilePaths f npcByronGenesisFile}

instance AdjustFilePaths NodeShelleyProtocolConfiguration where
  adjustFilePaths
    f
    x@NodeShelleyProtocolConfiguration
      { npcShelleyGenesisFile
      } =
      x{npcShelleyGenesisFile = adjustFilePaths f npcShelleyGenesisFile}

instance AdjustFilePaths NodeAlonzoProtocolConfiguration where
  adjustFilePaths
    f
    x@NodeAlonzoProtocolConfiguration
      { npcAlonzoGenesisFile
      } =
      x{npcAlonzoGenesisFile = adjustFilePaths f npcAlonzoGenesisFile}

instance AdjustFilePaths NodeConwayProtocolConfiguration where
  adjustFilePaths
    f
    x@NodeConwayProtocolConfiguration
      { npcConwayGenesisFile
      } =
      x{npcConwayGenesisFile = adjustFilePaths f npcConwayGenesisFile}

instance AdjustFilePaths NodeDijkstraProtocolConfiguration where
  adjustFilePaths
    f
    x@NodeDijkstraProtocolConfiguration
      { npcDijkstraGenesisFile
      } =
      x{npcDijkstraGenesisFile = adjustFilePaths f npcDijkstraGenesisFile}

instance AdjustFilePaths GenesisFile where
  adjustFilePaths f (GenesisFile p) = GenesisFile (f p)

instance AdjustFilePaths a => AdjustFilePaths (Maybe a) where
  adjustFilePaths f = fmap (adjustFilePaths f)

data VRFPrivateKeyFilePermissionError
  = OtherPermissionsExist FilePath
  | GroupPermissionsExist FilePath
  | GenericPermissionsExist FilePath
  deriving Show

renderVRFPrivateKeyFilePermissionError :: VRFPrivateKeyFilePermissionError -> Text
renderVRFPrivateKeyFilePermissionError err =
  case err of
    OtherPermissionsExist fp ->
      "VRF private key file at: "
        <> Text.pack fp
        <> " has \"other\" file permissions. Please remove all \"other\" file permissions."
    GroupPermissionsExist fp ->
      "VRF private key file at: "
        <> Text.pack fp
        <> "has \"group\" file permissions. Please remove all \"group\" file permissions."
    GenericPermissionsExist fp ->
      "VRF private key file at: "
        <> Text.pack fp
        <> "has \"generic\" file permissions. Please remove all \"generic\" file permissions."
