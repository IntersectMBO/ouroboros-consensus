{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Utility functions to elaborate a Cardano 'ProtocolInfo' from certain parameters.
module Test.Consensus.Cardano.ProtocolInfo (
    -- * ProtocolInfo elaboration
    mkSimpleTestProtocolInfo
  , mkTestProtocolInfo
    -- * ProtocolInfo elaboration parameter types
  , ByronSlotLengthInSeconds (..)
  , NumCoreNodes (..)
  , ShelleySlotLengthInSeconds (..)
    -- ** Hard-fork specification
  , Era (..)
  , HardForkSpec (..)
  , hardForkInto
  , stayInByron
  ) where

import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Update as CC.Update
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Conway.Genesis as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.Translation as SL
import qualified Cardano.Protocol.TPraos.OCert as SL
import qualified Cardano.Slotting.Time as Time
import           Data.Proxy (Proxy (..))
import           Data.Word (Word64)
import           Ouroboros.Consensus.BlockchainTime (SlotLength)
import           Ouroboros.Consensus.Byron.Node (ByronLeaderCredentials,
                     ProtocolParamsByron (..), byronGenesis,
                     byronMaxTxCapacityOverrides, byronPbftSignatureThreshold,
                     byronSoftwareVersion)
import           Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import           Ouroboros.Consensus.Cardano.Node (CardanoHardForkConstraints,
                     ProtocolTransitionParamsShelleyBased (..),
                     TriggerHardFork (TriggerHardForkAtEpoch, TriggerHardForkNever),
                     protocolInfoCardano, transitionTranslationContext,
                     transitionTrigger)
import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..),
                     ProtocolInfo)
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.PBFT (PBftParams,
                     PBftSignatureThreshold (..))
import           Ouroboros.Consensus.Shelley.Node (ProtocolParamsAllegra (..),
                     ProtocolParamsAlonzo (..), ProtocolParamsMary (..),
                     ProtocolParamsShelley (..),
                     ProtocolParamsShelleyBased (..), ShelleyGenesis,
                     ShelleyLeaderCredentials, allegraMaxTxCapacityOverrides,
                     allegraProtVer, alonzoMaxTxCapacityOverrides,
                     alonzoProtVer, maryMaxTxCapacityOverrides, maryProtVer,
                     sgGenDelegs, shelleyBasedGenesis, shelleyBasedInitialNonce,
                     shelleyBasedLeaderCredentials,
                     shelleyMaxTxCapacityOverrides, shelleyProtVer)
import           Ouroboros.Consensus.Shelley.Node.Praos
                     (ProtocolParamsBabbage (..), ProtocolParamsConway (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import qualified Test.ThreadNet.Infra.Alonzo as Alonzo
import qualified Test.ThreadNet.Infra.Byron as Byron
import qualified Test.ThreadNet.Infra.Shelley as Shelley
import           Test.ThreadNet.Util.Seed (Seed (Seed), runGen)
import           Test.Util.Slots (NumSlots (..))

--------------------------------------------------------------------------------
-- ProtocolInfo elaboration parameter types
--------------------------------------------------------------------------------

newtype ByronSlotLengthInSeconds = ByronSlotLengthInSeconds Word64

newtype ShelleySlotLengthInSeconds = ShelleySlotLengthInSeconds Word64

class ToSlotLength a where
  toSlotLength :: a -> SlotLength

instance ToSlotLength ByronSlotLengthInSeconds where
  toSlotLength (ByronSlotLengthInSeconds n) = Time.slotLengthFromSec $ fromIntegral n

instance ToSlotLength ShelleySlotLengthInSeconds where
  toSlotLength (ShelleySlotLengthInSeconds n) = Time.slotLengthFromSec $ fromIntegral n

-- | This data structure is used to specify if and when hardforks should take
-- place, and the version used at each era. See 'stayInByron' and 'hardForkInto'
-- for examples.
data HardForkSpec =
    HardForkSpec {
      shelleyHardForkSpec :: (SL.ProtVer, TriggerHardFork)
    , allegraHardForkSpec :: (SL.ProtVer, TriggerHardFork)
    , maryHardForkSpec    :: (SL.ProtVer, TriggerHardFork)
    , alonzoHardForkSpec  :: (SL.ProtVer, TriggerHardFork)
    , babbageHardForkSpec :: (SL.ProtVer, TriggerHardFork)
    , conwayHardForkSpec  :: (SL.ProtVer, TriggerHardFork)
    }

data Era = Shelley
         | Allegra
         | Mary
         | Alonzo
         | Babbage
         | Conway

selectEra :: Era -> HardForkSpec -> (SL.ProtVer, TriggerHardFork)
selectEra Shelley HardForkSpec { shelleyHardForkSpec } = shelleyHardForkSpec
selectEra Allegra HardForkSpec { allegraHardForkSpec } = allegraHardForkSpec
selectEra Mary    HardForkSpec { maryHardForkSpec    } = maryHardForkSpec
selectEra Alonzo  HardForkSpec { alonzoHardForkSpec  } = alonzoHardForkSpec
selectEra Babbage HardForkSpec { babbageHardForkSpec } = babbageHardForkSpec
selectEra Conway  HardForkSpec { conwayHardForkSpec  } = conwayHardForkSpec

hfSpecProtVer :: Era -> HardForkSpec -> SL.ProtVer
hfSpecProtVer era = fst . selectEra era

hfSpecTransitionTrigger :: Era -> HardForkSpec -> TriggerHardFork
hfSpecTransitionTrigger era = snd . selectEra era

stayInByron :: HardForkSpec
stayInByron =
    HardForkSpec {
      shelleyHardForkSpec = (SL.ProtVer versionZero 0, TriggerHardForkNever)
    , allegraHardForkSpec = (SL.ProtVer versionZero 0, TriggerHardForkNever)
    , maryHardForkSpec    = (SL.ProtVer versionZero 0, TriggerHardForkNever)
    , alonzoHardForkSpec  = (SL.ProtVer versionZero 0, TriggerHardForkNever)
    , babbageHardForkSpec = (SL.ProtVer versionZero 0, TriggerHardForkNever)
    , conwayHardForkSpec  = (SL.ProtVer versionZero 0, TriggerHardForkNever)
    }

versionZero :: SL.Version
versionZero = SL.natVersion @0

hardForkInto :: Era -> HardForkSpec
hardForkInto Shelley =
    stayInByron
      { shelleyHardForkSpec = (SL.ProtVer versionZero 0, TriggerHardForkAtEpoch 0) }
hardForkInto Allegra =
    (hardForkInto Shelley)
      { allegraHardForkSpec = (SL.ProtVer versionZero 0, TriggerHardForkAtEpoch 0) }
hardForkInto Mary    =
    (hardForkInto Allegra)
      { maryHardForkSpec    = (SL.ProtVer versionZero 0, TriggerHardForkAtEpoch 0) }
hardForkInto Alonzo  =
    (hardForkInto Mary)
      { alonzoHardForkSpec  = (SL.ProtVer versionZero 0, TriggerHardForkAtEpoch 0) }
hardForkInto Babbage =
    (hardForkInto Alonzo)
      { babbageHardForkSpec  = (SL.ProtVer versionZero 0, TriggerHardForkAtEpoch 0) }
hardForkInto Conway =
    (hardForkInto Babbage)
      { conwayHardForkSpec  = (SL.ProtVer versionZero 0, TriggerHardForkAtEpoch 0) }

--------------------------------------------------------------------------------
-- ProtocolInfo elaboration
--------------------------------------------------------------------------------

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
-- The resulting 'ProtocolInfo' contains a ledger state. The 'HardForkSpec'
-- parameter will determine to which era this ledger state belongs. See
-- 'HardForkSpec' for more details on how to specify a value of this type.
--
mkSimpleTestProtocolInfo ::
     forall m c
   . (IOLike m,  CardanoHardForkConstraints c)
  => Shelley.DecentralizationParam
  -- ^ Network decentralization parameter.
  -> SecurityParam
  -> ByronSlotLengthInSeconds
  -> ShelleySlotLengthInSeconds
  -> HardForkSpec
  -> ProtocolInfo m (CardanoBlock c)
mkSimpleTestProtocolInfo
    decentralizationParam
    securityParam
    byronSlotLenghtInSeconds
    shelleySlotLengthInSeconds
    hardForkSpec
  =
    mkTestProtocolInfo
      (CoreNodeId 0, coreNodeShelley)
      shelleyGenesis
      byronProtocolVersion
      SL.NeutralNonce
      genesisByron
      generatedSecretsByron
      (Just $ PBftSignatureThreshold 1)
      hardForkSpec
  where
    byronProtocolVersion =
        CC.Update.ProtocolVersion 0 0 0

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

    shelleyGenesis :: ShelleyGenesis c
    shelleyGenesis =
        Shelley.mkGenesisConfig
          (hfSpecProtVer Shelley hardForkSpec)
          securityParam
          activeSlotCoeff
          decentralizationParam
          maxLovelaceSupply
          (toSlotLength shelleySlotLengthInSeconds)
          (Shelley.mkKesConfig (Proxy @c) numSlots)
          [coreNodeShelley]
      where
        maxLovelaceSupply :: Word64
        maxLovelaceSupply = 45000000000000000

        activeSlotCoeff :: Rational
        activeSlotCoeff = 0.2   -- c.f. mainnet is more conservative, using 0.05

        numSlots = NumSlots 100

-- | A more generalized version of 'mkSimpleTestProtocolInfo'.
--
mkTestProtocolInfo ::
     forall m c
   . (IOLike m,  CardanoHardForkConstraints c)
  => (CoreNodeId, Shelley.CoreNode c)
  -- ^ Id of the node for which the protocol info will be elaborated.
  -> ShelleyGenesis c
  -- ^ These nodes will be part of the initial delegation mapping, and funds
  --   will be allocated to these nodes.
  -> CC.Update.ProtocolVersion
  -- ^ Protocol version of the Byron era proposal.
  -> SL.Nonce
  -> CC.Genesis.Config
  -> CC.Genesis.GeneratedSecrets
  -> Maybe PBftSignatureThreshold
  -> HardForkSpec
  -- ^ Specification of the era to which the initial state should hard-fork to.
  -> ProtocolInfo m (CardanoBlock c)
mkTestProtocolInfo
    (coreNodeId, coreNode)
    shelleyGenesis
    aByronProtocolVersion
    initialNonce
    genesisByron
    generatedSecretsByron
    aByronPbftSignatureThreshold
    hardForkSpec
  =
    protocolInfoCardano
        ProtocolParamsByron {
            byronGenesis                = genesisByron
            -- Trivialize the PBFT signature window so that the forks induced by
            -- the network partition are as deep as possible.
          , byronPbftSignatureThreshold = aByronPbftSignatureThreshold
          , byronProtocolVersion        = aByronProtocolVersion
          , byronSoftwareVersion        = softVerByron
          , byronLeaderCredentials      = Just leaderCredentialsByron
          , byronMaxTxCapacityOverrides = Mempool.mkOverrides Mempool.noOverridesMeasure
          }
        ProtocolParamsShelleyBased {
            shelleyBasedGenesis           = shelleyGenesis
          , shelleyBasedInitialNonce      = initialNonce
          , shelleyBasedLeaderCredentials = [leaderCredentialsShelley]
          }
        ProtocolParamsShelley {
            shelleyProtVer                = hfSpecProtVer Shelley hardForkSpec
          , shelleyMaxTxCapacityOverrides = Mempool.mkOverrides Mempool.noOverridesMeasure
          }
        ProtocolParamsAllegra {
            allegraProtVer                = hfSpecProtVer Allegra hardForkSpec
          , allegraMaxTxCapacityOverrides = Mempool.mkOverrides Mempool.noOverridesMeasure
          }
        ProtocolParamsMary {
            maryProtVer                   = hfSpecProtVer Mary hardForkSpec
          , maryMaxTxCapacityOverrides    = Mempool.mkOverrides Mempool.noOverridesMeasure
          }
        ProtocolParamsAlonzo {
            alonzoProtVer                 = hfSpecProtVer Alonzo hardForkSpec
          , alonzoMaxTxCapacityOverrides  = Mempool.mkOverrides Mempool.noOverridesMeasure
          }
        ProtocolParamsBabbage {
            babbageProtVer                = hfSpecProtVer Babbage hardForkSpec
          , babbageMaxTxCapacityOverrides = Mempool.mkOverrides Mempool.noOverridesMeasure
          }
        ProtocolParamsConway {
            conwayProtVer                 = hfSpecProtVer Conway hardForkSpec
          , conwayMaxTxCapacityOverrides  = Mempool.mkOverrides Mempool.noOverridesMeasure
          }
        ProtocolTransitionParamsShelleyBased {
          transitionTranslationContext = SL.toFromByronTranslationContext shelleyGenesis
        , transitionTrigger            = hfSpecTransitionTrigger Shelley hardForkSpec
        }
        ProtocolTransitionParamsShelleyBased {
            transitionTranslationContext = ()
          , transitionTrigger            = hfSpecTransitionTrigger Allegra hardForkSpec
          }
        ProtocolTransitionParamsShelleyBased {
            transitionTranslationContext = ()
          , transitionTrigger            = hfSpecTransitionTrigger Mary hardForkSpec
          }
        ProtocolTransitionParamsShelleyBased {
            transitionTranslationContext = Alonzo.degenerateAlonzoGenesis
          , transitionTrigger            = hfSpecTransitionTrigger Alonzo hardForkSpec
          }
        ProtocolTransitionParamsShelleyBased {
            transitionTranslationContext = ()
          , transitionTrigger            = hfSpecTransitionTrigger Babbage hardForkSpec
          }
        ProtocolTransitionParamsShelleyBased {
            transitionTranslationContext =
              -- Note that this is effectively a no-op, which is fine for
              -- testing, at least for now.
              SL.ConwayGenesis $ SL.GenDelegs $ sgGenDelegs shelleyGenesis
          , transitionTrigger            = hfSpecTransitionTrigger Conway hardForkSpec
          }

  where
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
