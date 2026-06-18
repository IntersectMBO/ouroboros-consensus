-- | Configuration options related to networking
module Cardano.Configuration.File.Network
  ( NetworkConfiguration (..)
  , AcceptedConnectionsLimit (..)
  , LocalConnectionsConfig (..)
  ) where

import Cardano.Configuration.Basics
import Data.Aeson
import Data.Time.Clock (DiffTime)
import Data.Word
import GHC.Generics (Generic)

-- | TODO
data AcceptedConnectionsLimit = AcceptedConnectionsLimit Word32 Word32 DiffTime
  deriving (Generic, Show)

instance FromJSON AcceptedConnectionsLimit where
  parseJSON = withObject "AcceptedConnectionsLimit" $ \v ->
    AcceptedConnectionsLimit
      <$> v .: "hardLimit"
      <*> v .: "softLimit"
      <*> v .: "delay"

-- | Options related to Networking configuration. Most of the fields are
-- @Maybe@ such that the networking layer can then set the appropriate
-- defaults.
data NetworkConfiguration = NetworkConfiguration
  { pncDiffusionMode :: String
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
  , pncPeerSharing :: Maybe Bool
  , pncResponderCoreAffinityPolicy :: Maybe String
  , pncExperimentalProtocolsEnabled :: Maybe Bool
  , pncTxSubmissionLogicVersion :: Maybe String
  , pncTxSubmissionInitDelay :: Maybe DiffTime
  }
  deriving (Generic, Show)

instance FromJSON NetworkConfiguration where
  parseJSON =
    withObject "Configuration" $ \v ->
      NetworkConfiguration
        <$> v .:? "DiffusionMode" .!= "InitiatorAndResponder"
        <*> v .:= "MaxConcurrencyBulkSync"
        <*> v .:= "MaxConcurrencyDeadline"
        <*> v .:= "ProtocolIdleTimeout"
        <*> v .:= "TimeWaitTimeout"
        <*> v .:= "EgressPollInterval"
        <*> v .:= "ChainSyncIdleTimeout"
        <*> v .:= "AcceptedConnectionsLimit"
        <*> v .:= "TargetNumberOfRootPeers"
        <*> v .:= "TargetNumberOfKnownPeers"
        <*> v .:= "TargetNumberOfEstablishedPeers"
        <*> v .:= "TargetNumberOfActivePeers"
        <*> v .:= "TargetNumberOfKnownBigLedgerPeers"
        <*> v .:= "TargetNumberOfEstablishedBigLedgerPeers"
        <*> v .:= "TargetNumberOfActiveBigLedgerPeers"
        <*> v .:= "SyncTargetNumberOfRootPeers"
        <*> v .:= "SyncTargetNumberOfKnownPeers"
        <*> v .:= "SyncTargetNumberOfEstablishedPeers"
        <*> v .:= "SyncTargetNumberOfActivePeers"
        <*> v .:= "SyncTargetNumberOfKnownBigLedgerPeers"
        <*> v .:= "SyncTargetNumberOfEstablishedBigLedgerPeers"
        <*> v .:= "SyncTargetNumberOfActiveBigLedgerPeers"
        <*> v .:= "MinBigLedgerPeersForTrustedState"
        <*> v .:= "PeerSharing"
        <*> v .:= "ResponderCoreAffinityPolicy"
        <*> v .:= "ExperimentalProtocolsEnabled"
        <*> v .:= "TxSubmissionLogicVersion"
        <*> v .:= "TxSubmissionInitDelay"

-- | Connections for local clients
data LocalConnectionsConfig = LocalConnectionsConfig
  { pncSocketPath :: Maybe FilePath
  , pncEnableRpc :: Maybe Bool
  , pncRpcSocketPath :: Maybe FilePath
  }
  deriving (Generic, Show)

instance FromJSON LocalConnectionsConfig where
  parseJSON =
    withObject "Configuration" $ \v ->
      LocalConnectionsConfig
        <$> v .:? "SocketPath"
        <*> v .:? "EnableRpc"
        <*> v .:? "RpcSocketPath"
