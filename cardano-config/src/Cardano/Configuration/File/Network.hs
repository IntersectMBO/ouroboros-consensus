-- | Configuration options related to networking
module Cardano.Configuration.File.Network
  ( DiffusionMode (..)
  , NetworkConfiguration (..)
  , AcceptedConnectionsLimit (..)
  , PeerSharing (..)
  , ResponderCoreAffinityPolicy (..)
  , LocalConnectionsConfig (..)
  ) where

import Cardano.Configuration.Basics
import Data.Aeson
import qualified Data.Text as T
import Data.Time.Clock (DiffTime)
import Data.Word
import GHC.Generics (Generic)
import Prelude hiding (FilePath)

-- | TODO
data DiffusionMode = InitiatorOnlyDiffusionMode | InitiatorAndResponderDiffusionMode
  deriving (Generic, Show)

instance FromJSON DiffusionMode where
  parseJSON =
    withText "DiffusionMode" $ \case
      "InitiatorOnly" -> pure InitiatorOnlyDiffusionMode
      "InitiatorAndResponder" -> pure InitiatorAndResponderDiffusionMode
      x -> fail $ "Unknown diffusion mode: " <> T.unpack x

-- | TODO
data AcceptedConnectionsLimit = AcceptedConnectionsLimit Word32 Word32 DiffTime
  deriving (Generic, Show)

instance FromJSON AcceptedConnectionsLimit where
  parseJSON = withObject "AcceptedConnectionsLimit" $ \v ->
    AcceptedConnectionsLimit
      <$> v .: "hardLimit"
      <*> v .: "softLimit"
      <*> v .: "delay"

-- | Whether to enable peer sharing
data PeerSharing = PeerSharingEnabled | PeerSharingDisabled deriving (Generic, Show)

instance FromJSON PeerSharing where
  parseJSON =
    withBool "PeerSharing" $ \b ->
      pure $
        if b
          then PeerSharingEnabled
          else PeerSharingDisabled

-- | TODO
data ResponderCoreAffinityPolicy = NoResponderCoreAffinity | ResponderCoreAffinity
  deriving (Generic, Show)

instance FromJSON ResponderCoreAffinityPolicy where
  parseJSON =
    withText "ResponderCoreAffinityPolicy" $ \case
      "NoResponderCoreAffinity" -> pure NoResponderCoreAffinity
      "ResponderCoreAffinity" -> pure ResponderCoreAffinity
      x -> fail $ "Unknown responder core affinity policy: " <> T.unpack x

-- | Options related to Networking configuration. Most of the fields are
-- @Maybe@ such that the networking layer can then set the appropriate
-- defaults.
data NetworkConfiguration = NetworkConfiguration
  { pncDiffusionMode :: DiffusionMode
  , pncMaxConcurrencyBulkSync :: Maybe Word
  , pncMaxConcurrencyDeadline :: Maybe Word
  , pncProtocolIdleTimeout :: Maybe DiffTime
  , pncTimeWaitTimeout :: Maybe DiffTime
  , pncEgressPollInterval :: Maybe DiffTime
  , pncChainSyncIdleTimeout :: Maybe DiffTime
  , pncAcceptedConnectionsLimit :: Maybe AcceptedConnectionsLimit
  , pncDeadlineTargetOfRootPeers :: Maybe Int
  , pncDeadlineTargetOfKnownPeers :: Maybe Int
  , pncDeadlineTargetOfEstablishedPeers :: Maybe Int
  , pncDeadlineTargetOfActivePeers :: Maybe Int
  , pncDeadlineTargetOfKnownBigLedgerPeers :: Maybe Int
  , pncDeadlineTargetOfEstablishedBigLedgerPeers :: Maybe Int
  , pncDeadlineTargetOfActiveBigLedgerPeers :: Maybe Int
  , pncSyncTargetOfRootPeers :: Maybe Int
  , pncSyncTargetOfKnownPeers :: Maybe Int
  , pncSyncTargetOfEstablishedPeers :: Maybe Int
  , pncSyncTargetOfActivePeers :: Maybe Int
  , pncSyncTargetOfKnownBigLedgerPeers :: Maybe Int
  , pncSyncTargetOfEstablishedBigLedgerPeers :: Maybe Int
  , pncSyncTargetOfActiveBigLedgerPeers :: Maybe Int
  , pncMinBigLedgerPeersForTrustedState :: Maybe Int
  , pncPeerSharing :: Maybe PeerSharing
  , pncResponderCoreAffinityPolicy :: Maybe ResponderCoreAffinityPolicy
  }
  deriving (Generic, Show)

instance FromJSON NetworkConfiguration where
  parseJSON =
    withObject "Configuration" $ \v ->
      NetworkConfiguration
        <$> v .:? "DiffusionMode" .!= InitiatorAndResponderDiffusionMode
        <*> v .:= "MaxConcurrencyBulkSync"
        <*> v .:= "MaxConcurrencyDeadline"
        <*> v .:= "ProtocolIdleTimeout"
        <*> v .:= "TimeWaitTimeout"
        <*> v .:= "EgressPollInterval"
        <*> v .:= "ChainSyncIdleTimeout"
        <*> v .:= "AcceptedConnectionsLimit"
        <*> v .:= "DeadlineTargetOfRootPeers"
        <*> v .:= "DeadlineTargetOfKnownPeers"
        <*> v .:= "DeadlineTargetOfEstablishedPeers"
        <*> v .:= "DeadlineTargetOfActivePeers"
        <*> v .:= "DeadlineTargetOfKnownBigLedgerPeers"
        <*> v .:= "DeadlineTargetOfEstablishedBigLedgerPeers"
        <*> v .:= "DeadlineTargetOfActiveBigLedgerPeers"
        <*> v .:= "SyncTargetOfRootPeers"
        <*> v .:= "SyncTargetOfKnownPeers"
        <*> v .:= "SyncTargetOfEstablishedPeers"
        <*> v .:= "SyncTargetOfActivePeers"
        <*> v .:= "SyncTargetOfKnownBigLedgerPeers"
        <*> v .:= "SyncTargetOfEstablishedBigLedgerPeers"
        <*> v .:= "SyncTargetOfActiveBigLedgerPeers"
        <*> v .:= "MinBigLedgerPeersForTrustedState"
        <*> v .:= "PeerSharing"
        <*> v .:= "ResponderCoreAffinityPolicy"

-- | Connections for local clients
data LocalConnectionsConfig = LocalConnectionsConfig
  { pncSocketPath :: Maybe (File "Socket")
  }
  deriving (Generic, Show)

instance FromJSON LocalConnectionsConfig where
  parseJSON =
    withObject "Configuration" $ \v ->
      LocalConnectionsConfig <$> v .:? "SocketPath"
