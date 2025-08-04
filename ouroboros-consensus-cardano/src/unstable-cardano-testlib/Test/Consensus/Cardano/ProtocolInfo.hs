{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Utility functions to elaborate a Cardano 'ProtocolInfo' from certain parameters.
module Test.Consensus.Cardano.ProtocolInfo
  ( -- * ProtocolInfo elaboration parameter types
    ByronSlotLengthInSeconds (..)
  , NumCoreNodes (..)
  , ShelleySlotLengthInSeconds (..)

    -- ** Hard-fork specification
  , Era (..)
  , hardForkInto
  , hardForkOnDefaultProtocolVersions

    -- * ProtocolInfo elaboration
  , mkSimpleTestProtocolInfo
  , mkTestProtocolInfo
  , protocolVersionZero
  ) where

import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Update as CC.Update
import qualified Cardano.Ledger.Api.Transition as L
import qualified Cardano.Ledger.BaseTypes as SL
import Cardano.Protocol.Crypto (StandardCrypto)
import qualified Cardano.Protocol.TPraos.OCert as SL
import qualified Cardano.Slotting.Time as Time
import Data.Proxy (Proxy (..))
import Data.SOP.Strict
import Data.Word (Word64)
import Ouroboros.Consensus.Block.Forging (BlockForging)
import Ouroboros.Consensus.BlockchainTime (SlotLength)
import Ouroboros.Consensus.Byron.Node
  ( ByronLeaderCredentials
  , ProtocolParamsByron (..)
  , byronGenesis
  , byronPbftSignatureThreshold
  , byronSoftwareVersion
  )
import Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import Ouroboros.Consensus.Cardano.Node
  ( CardanoHardForkConstraints
  , CardanoHardForkTrigger (..)
  , CardanoHardForkTriggers (..)
  , CardanoProtocolParams (..)
  , protocolInfoCardano
  )
import Ouroboros.Consensus.Config (emptyCheckpointsMap)
import Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import Ouroboros.Consensus.Node.ProtocolInfo
  ( NumCoreNodes (..)
  , ProtocolInfo
  )
import Ouroboros.Consensus.NodeId (CoreNodeId (..))
import Ouroboros.Consensus.Protocol.PBFT
  ( PBftParams
  , PBftSignatureThreshold (..)
  )
import Ouroboros.Consensus.Shelley.Node
  ( ProtocolParamsShelleyBased (..)
  , ShelleyGenesis
  , ShelleyLeaderCredentials
  )
import Ouroboros.Consensus.Util.IOLike (IOLike)
import qualified Test.Cardano.Ledger.Alonzo.Examples as Alonzo
import qualified Test.Cardano.Ledger.Conway.Examples as Conway
import qualified Test.Cardano.Ledger.Dijkstra.Examples as Dijkstra
import qualified Test.Cardano.Ledger.Shelley.Examples as Shelley
import qualified Test.ThreadNet.Infra.Byron as Byron
import qualified Test.ThreadNet.Infra.Shelley as Shelley
import Test.ThreadNet.Util.Seed (Seed (Seed), runGen)
import Test.Util.Slots (NumSlots (..))

{-------------------------------------------------------------------------------
  ProtocolInfo elaboration parameter types
-------------------------------------------------------------------------------}

newtype ByronSlotLengthInSeconds = ByronSlotLengthInSeconds Word64

newtype ShelleySlotLengthInSeconds = ShelleySlotLengthInSeconds Word64

class ToSlotLength a where
  toSlotLength :: a -> SlotLength

instance ToSlotLength ByronSlotLengthInSeconds where
  toSlotLength (ByronSlotLengthInSeconds n) = Time.slotLengthFromSec $ fromIntegral n

instance ToSlotLength ShelleySlotLengthInSeconds where
  toSlotLength (ShelleySlotLengthInSeconds n) = Time.slotLengthFromSec $ fromIntegral n

data Era
  = Byron
  | Shelley
  | Allegra
  | Mary
  | Alonzo
  | Babbage
  | Conway
  deriving (Show, Eq, Ord, Enum)

protocolVersionZero :: SL.ProtVer
protocolVersionZero = SL.ProtVer versionZero 0
 where
  versionZero :: SL.Version
  versionZero = SL.natVersion @0

hardForkOnDefaultProtocolVersions :: CardanoHardForkTriggers
hardForkOnDefaultProtocolVersions =
  CardanoHardForkTriggers $
    hpure CardanoTriggerHardForkAtDefaultVersion

hardForkInto :: Era -> CardanoHardForkTriggers
hardForkInto Byron = hardForkOnDefaultProtocolVersions
hardForkInto Shelley =
  hardForkOnDefaultProtocolVersions
    { triggerHardForkShelley = CardanoTriggerHardForkAtEpoch 0
    }
hardForkInto Allegra =
  (hardForkInto Shelley)
    { triggerHardForkAllegra = CardanoTriggerHardForkAtEpoch 0
    }
hardForkInto Mary =
  (hardForkInto Allegra)
    { triggerHardForkMary = CardanoTriggerHardForkAtEpoch 0
    }
hardForkInto Alonzo =
  (hardForkInto Mary)
    { triggerHardForkAlonzo = CardanoTriggerHardForkAtEpoch 0
    }
hardForkInto Babbage =
  (hardForkInto Alonzo)
    { triggerHardForkBabbage = CardanoTriggerHardForkAtEpoch 0
    }
hardForkInto Conway =
  (hardForkInto Babbage)
    { triggerHardForkConway = CardanoTriggerHardForkAtEpoch 0
    }

{-------------------------------------------------------------------------------
 ProtocolInfo elaboration
-------------------------------------------------------------------------------}

-- | Create a Cardano protocol info for testing purposes, using some predifined
-- settings (see below). For a more general version see 'mkTestProtocolInfo'.
--
-- The resulting 'ProtocolInfo' will use a randomly generated core node. This
-- generation will use a fixed seed. See 'mkTestProtocolInfo' for a function
-- that takes a core node as parameter.
--
-- The resulting 'ProtocolInfo' will:
--
-- - Use a 'NeutralNonce'.
-- - Use a fixed number of slots per KES evolution.
-- - Have version 0 0 0 as Byron protocol version.
-- - Use 1 as 'PbftSignatureThreshold'
--
-- If you want to tweak the resulting protocol info further see
-- 'mkTestProtocolInfo'.
--
-- The resulting 'ProtocolInfo' contains a ledger state. The
-- 'CardanoHardForkTriggers' parameter will determine to which era this ledger
-- state belongs. See 'hardForkInto' and 'hardForkOnDefaultProtocolVersions' for
-- more details on how to specify a value of this type.
mkSimpleTestProtocolInfo ::
  forall c.
  CardanoHardForkConstraints c =>
  -- | Network decentralization parameter.
  Shelley.DecentralizationParam ->
  SecurityParam ->
  ByronSlotLengthInSeconds ->
  ShelleySlotLengthInSeconds ->
  SL.ProtVer ->
  CardanoHardForkTriggers ->
  ProtocolInfo (CardanoBlock c)
mkSimpleTestProtocolInfo
  decentralizationParam
  securityParam
  byronSlotLenghtInSeconds
  shelleySlotLengthInSeconds
  protocolVersion
  hardForkTriggers =
    fst $
      mkTestProtocolInfo @IO
        (CoreNodeId 0, coreNodeShelley)
        shelleyGenesis
        aByronProtocolVersion
        SL.NeutralNonce
        genesisByron
        generatedSecretsByron
        (Just $ PBftSignatureThreshold 1)
        protocolVersion
        hardForkTriggers
   where
    aByronProtocolVersion =
      CC.Update.ProtocolVersion 0 0 0

    coreNodeShelley :: Shelley.CoreNode c
    coreNodeShelley = runGen initSeed $ Shelley.genCoreNode initialKESPeriod
     where
      initSeed :: Seed
      initSeed = Seed 0

      initialKESPeriod :: SL.KESPeriod
      initialKESPeriod = SL.KESPeriod 0

    pbftParams :: PBftParams
    pbftParams = Byron.byronPBftParams securityParam (NumCoreNodes 1)

    generatedSecretsByron :: CC.Genesis.GeneratedSecrets
    (genesisByron, generatedSecretsByron) =
      Byron.generateGenesisConfig (toSlotLength byronSlotLenghtInSeconds) pbftParams

    shelleyGenesis :: ShelleyGenesis
    shelleyGenesis =
      Shelley.mkGenesisConfig
        protocolVersion
        securityParam
        activeSlotCoeff
        decentralizationParam
        maxLovelaceSupply
        (toSlotLength shelleySlotLengthInSeconds)
        (Shelley.mkKesConfig (Proxy @StandardCrypto) numSlots)
        [coreNodeShelley]
     where
      maxLovelaceSupply :: Word64
      maxLovelaceSupply = 45000000000000000

      activeSlotCoeff :: Rational
      activeSlotCoeff = 0.2 -- c.f. mainnet is more conservative, using 0.05
      numSlots = NumSlots 100

-- | A more generalized version of 'mkSimpleTestProtocolInfo'.
mkTestProtocolInfo ::
  forall m c.
  (CardanoHardForkConstraints c, IOLike m) =>
  -- | Id of the node for which the protocol info will be elaborated.
  (CoreNodeId, Shelley.CoreNode c) ->
  -- | These nodes will be part of the initial delegation mapping, and funds
  --   will be allocated to these nodes.
  ShelleyGenesis ->
  -- | Protocol version of the Byron era proposal.
  CC.Update.ProtocolVersion ->
  SL.Nonce ->
  CC.Genesis.Config ->
  CC.Genesis.GeneratedSecrets ->
  Maybe PBftSignatureThreshold ->
  -- | See 'protocolInfoCardano' for the details of what is the
  -- relation between this version and any 'TriggerHardForkAtVersion'
  -- that __might__ appear in the 'CardanoHardForkTriggers' parameter.
  SL.ProtVer ->
  -- | Specification of the era to which the initial state should hard-fork to.
  CardanoHardForkTriggers ->
  (ProtocolInfo (CardanoBlock c), m [BlockForging m (CardanoBlock c)])
mkTestProtocolInfo
  (coreNodeId, coreNode)
  shelleyGenesis
  aByronProtocolVersion
  initialNonce
  genesisByron
  generatedSecretsByron
  aByronPbftSignatureThreshold
  protocolVersion
  hardForkTriggers =
    protocolInfoCardano
      ( CardanoProtocolParams
          ProtocolParamsByron
            { byronGenesis = genesisByron
            , byronPbftSignatureThreshold = aByronPbftSignatureThreshold
            , byronProtocolVersion = aByronProtocolVersion
            , byronSoftwareVersion = softVerByron
            , byronLeaderCredentials = Just leaderCredentialsByron
            }
          ProtocolParamsShelleyBased
            { shelleyBasedInitialNonce = initialNonce
            , shelleyBasedLeaderCredentials = [leaderCredentialsShelley]
            }
          hardForkTriggers
          ( L.mkLatestTransitionConfig
              shelleyGenesis
              -- These example genesis objects might need to become more
              -- realistic in the future, but they are fine for now:
              --
              --  * The current ThreadNet tests only involve pre-Alonzo eras.
              --
              --  * The LocalTxSubmissionServer does not (yet) rely on any
              --    details of these.
              exampleAlonzoGenesis
              exampleConwayGenesis
              exampleDijkstraGenesis
          )
          emptyCheckpointsMap
          protocolVersion
      )
   where
    exampleAlonzoGenesis = Shelley.leTranslationContext Alonzo.ledgerExamples
    exampleConwayGenesis = Shelley.leTranslationContext Conway.ledgerExamples
    exampleDijkstraGenesis = Shelley.leTranslationContext Dijkstra.ledgerExamples

    leaderCredentialsByron :: ByronLeaderCredentials
    leaderCredentialsByron =
      Byron.mkLeaderCredentials
        genesisByron
        generatedSecretsByron
        coreNodeId

    -- This sets a vestigial header field which is not actually used for anything.
    softVerByron :: CC.Update.SoftwareVersion
    softVerByron = Byron.theProposedSoftwareVersion

    leaderCredentialsShelley :: ShelleyLeaderCredentials c
    leaderCredentialsShelley = Shelley.mkLeaderCredentials coreNode
