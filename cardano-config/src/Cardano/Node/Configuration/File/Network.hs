{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Node.Configuration.File.Network where

import Cardano.Node.Configuration.Basics
import Data.Aeson
import qualified Data.Text as T
import Data.Time.Clock (DiffTime)
import Data.Word
import GHC.Generics (Generic)
import Prelude hiding (FilePath)

data DiffusionMode = InitiatorOnlyDiffusionMode | InitiatorAndResponderDiffusionMode
  deriving (Generic, Show)

instance FromJSON DiffusionMode where
  parseJSON =
    withText "DiffusionMode" $ \case
      "InitiatorOnly" -> pure InitiatorOnlyDiffusionMode
      "InitiatorAndResponder" -> pure InitiatorAndResponderDiffusionMode
      x -> fail $ "Unknown diffusion mode: " <> T.unpack x

data AcceptedConnectionsLimit = AcceptedConnectionsLimit Word32 Word32 DiffTime
  deriving (Generic, Show)

instance FromJSON AcceptedConnectionsLimit where
  parseJSON = withObject "AcceptedConnectionsLimit" $ \v ->
    AcceptedConnectionsLimit
      <$> v .: "hardLimit"
      <*> v .: "softLimit"
      <*> v .: "delay"

data PeerSharing = PeerSharingEnabled | PeerSharingDisabled deriving (Generic, Show)

instance FromJSON PeerSharing where
  parseJSON =
    withBool "PeerSharing" $ \b ->
      pure $
        if b
          then PeerSharingEnabled
          else PeerSharingDisabled

data ResponderCoreAffinityPolicy = NoResponderCoreAffinity | ResponderCoreAffinity
  deriving (Generic, Show)

instance FromJSON ResponderCoreAffinityPolicy where
  parseJSON =
    withText "ResponderCoreAffinityPolicy" $ \case
      "NoResponderCoreAffinity" -> pure NoResponderCoreAffinity
      "ResponderCoreAffinity" -> pure ResponderCoreAffinity
      x -> fail $ "Unknown responder core affinity policy: " <> T.unpack x

data NetworkConfiguration = NetworkConfiguration
  { pncDiffusionMode :: !DiffusionMode
  , pncMaxConcurrencyBulkSync :: !(Override Word)
  , pncMaxConcurrencyDeadline :: !(Override Word)
  , pncProtocolIdleTimeout :: !(Override DiffTime)
  , pncTimeWaitTimeout :: !(Override DiffTime)
  , pncEgressPollInterval :: !(Override DiffTime)
  , pncChainSyncIdleTimeout :: !(Override DiffTime)
  , pncAcceptedConnectionsLimit :: !(Override AcceptedConnectionsLimit)
  , pncDeadlineTargetOfRootPeers :: !(Override Int)
  , pncDeadlineTargetOfKnownPeers :: !(Override Int)
  , pncDeadlineTargetOfEstablishedPeers :: !(Override Int)
  , pncDeadlineTargetOfActivePeers :: !(Override Int)
  , pncDeadlineTargetOfKnownBigLedgerPeers :: !(Override Int)
  , pncDeadlineTargetOfEstablishedBigLedgerPeers :: !(Override Int)
  , pncDeadlineTargetOfActiveBigLedgerPeers :: !(Override Int)
  , pncSyncTargetOfRootPeers :: !(Override Int)
  , pncSyncTargetOfKnownPeers :: !(Override Int)
  , pncSyncTargetOfEstablishedPeers :: !(Override Int)
  , pncSyncTargetOfActivePeers :: !(Override Int)
  , pncSyncTargetOfKnownBigLedgerPeers :: !(Override Int)
  , pncSyncTargetOfEstablishedBigLedgerPeers :: !(Override Int)
  , pncSyncTargetOfActiveBigLedgerPeers :: !(Override Int)
  , pncMinBigLedgerPeersForTrustedState :: !(Override Int)
  , pncPeerSharing :: !(Override PeerSharing)
  , pncResponderCoreAffinityPolicy :: !(Override ResponderCoreAffinityPolicy)
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

data LocalConnectionsConfig = LocalConnectionsConfig
  { pncSocketPath :: !(Maybe (FilePath "Socket"))
  }
  deriving (Generic, Show)

instance FromJSON LocalConnectionsConfig where
  parseJSON =
    withObject "Configuration" $ \v ->
      LocalConnectionsConfig <$> v .:? "SocketPath"
