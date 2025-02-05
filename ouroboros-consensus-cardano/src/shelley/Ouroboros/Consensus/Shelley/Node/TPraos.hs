{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Node.TPraos (
    MaxMajorProtVer (..)
  , ProtocolParamsShelleyBased (..)
  , SL.Nonce (..)
  , SL.ProtVer (..)
  , SL.ShelleyGenesis (..)
  , SL.ShelleyGenesisStaking (..)
  , SL.emptyGenesisStaking
  , ShelleyLeaderCredentials (..)
  , protocolInfoShelley
  , protocolInfoTPraosShelleyBased
  , shelleyBlockForging
  , shelleySharedBlockForging
  , validateGenesis
  ) where

import           Cardano.Crypto.Hash (Hash)
import qualified Cardano.Crypto.VRF as VRF
import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Ledger.Api.Transition as L
import           Cardano.Ledger.Hashes (HASH)
import qualified Cardano.Ledger.Shelley.API as SL
import           Cardano.Protocol.Crypto (VRF)
import qualified Cardano.Protocol.TPraos.API as SL
import qualified Cardano.Protocol.TPraos.OCert as Absolute (KESPeriod (..))
import qualified Cardano.Protocol.TPraos.OCert as SL
import           Cardano.Slotting.EpochInfo
import           Cardano.Slotting.Time (mkSlotLength)
import           Control.Monad.Except (Except)
import           Data.Bifunctor (first)
import qualified Data.Text as T
import qualified Data.Text as Text
import           Lens.Micro ((^.))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool (TxLimits)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Ledger.HotKey (HotKey)
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import           Ouroboros.Consensus.Protocol.Praos.Common
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.Inspect ()
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import           Ouroboros.Consensus.Shelley.Node.Common
                     (ProtocolParamsShelleyBased (..), ShelleyEraWithCrypto,
                     ShelleyLeaderCredentials (..), shelleyBlockIssuerVKey)
import           Ouroboros.Consensus.Shelley.Node.Serialisation ()
import           Ouroboros.Consensus.Shelley.Protocol.TPraos ()
import           Ouroboros.Consensus.Util.Assert
import           Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

-- | Create a 'BlockForging' record for a single era.
--
-- In case the same credentials should be shared across multiple Shelley-based
-- eras, use 'shelleySharedBlockForging'.
shelleyBlockForging ::
     forall m era c.
      ( ShelleyCompatible (TPraos c) era
      , PraosCrypto c
      -- , c ~ StandardCrypto
      , TxLimits (ShelleyBlock (TPraos c) era)
      , IOLike m
      )
  => TPraosParams
  -> ShelleyLeaderCredentials c
  -> m (BlockForging m (ShelleyBlock (TPraos c) era))
shelleyBlockForging tpraosParams credentials = do
    hotKey <- HotKey.mkHotKey @m @c initSignKey startPeriod tpraosMaxKESEvo
    pure $ shelleySharedBlockForging hotKey slotToPeriod credentials
  where
    TPraosParams {tpraosMaxKESEvo, tpraosSlotsPerKESPeriod} = tpraosParams

    ShelleyLeaderCredentials {
        shelleyLeaderCredentialsInitSignKey = initSignKey
      , shelleyLeaderCredentialsCanBeLeader = canBeLeader
      } = credentials

    startPeriod :: Absolute.KESPeriod
    startPeriod = SL.ocertKESPeriod $ praosCanBeLeaderOpCert canBeLeader

    slotToPeriod :: SlotNo -> Absolute.KESPeriod
    slotToPeriod (SlotNo slot) =
      SL.KESPeriod $ fromIntegral $ slot `div` tpraosSlotsPerKESPeriod

-- | Create a 'BlockForging' record safely using a given 'Hotkey'.
--
-- The name of the era (separated by a @_@) will be appended to each
-- 'forgeLabel'.
shelleySharedBlockForging ::
     forall m c era.
     ( ShelleyEraWithCrypto c (TPraos c) era
     , PraosCrypto c
     , IOLike m
     )
  => HotKey c m
  -> (SlotNo -> Absolute.KESPeriod)
  -> ShelleyLeaderCredentials c
  -> BlockForging m (ShelleyBlock (TPraos c) era)
shelleySharedBlockForging hotKey slotToPeriod credentials =
    BlockForging {
        forgeLabel       = label <> "_" <> T.pack (L.eraName @era)
      , canBeLeader      = canBeLeader
      , updateForgeState = \_ curSlot _ ->
                               forgeStateUpdateInfoFromUpdateInfo <$>
                                 HotKey.evolve hotKey (slotToPeriod curSlot)
      , checkCanForge    = \cfg curSlot _tickedChainDepState ->
                               tpraosCheckCanForge
                                 (configConsensus cfg)
                                 forgingVRFHash
                                 curSlot
      , forgeBlock       = \cfg ->
          forgeShelleyBlock
            hotKey
            canBeLeader
            cfg
      }
  where
    ShelleyLeaderCredentials {
        shelleyLeaderCredentialsCanBeLeader = canBeLeader
      , shelleyLeaderCredentialsLabel       = label
      } = credentials

    forgingVRFHash :: Hash HASH (VRF.VerKeyVRF (VRF c))
    forgingVRFHash =
          VRF.hashVerKeyVRF
        . VRF.deriveVerKeyVRF
        . praosCanBeLeaderSignKeyVRF
        $ canBeLeader

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

-- | Check the validity of the genesis config. To be used in conjunction with
-- 'assertWithMsg'.
validateGenesis :: SL.ShelleyGenesis -> Either String ()
validateGenesis = first errsToString . SL.validateGenesis
  where
    errsToString :: [SL.ValidationErr] -> String
    errsToString errs =
        Text.unpack $ Text.unlines
          ("Invalid genesis config:" : map SL.describeValidationErr errs)

protocolInfoShelley ::
     forall m c.
      ( IOLike m
      , PraosCrypto c
      , ShelleyCompatible (TPraos c) ShelleyEra
      , TxLimits (ShelleyBlock (TPraos c) ShelleyEra)
      )
  => SL.ShelleyGenesis
  -> ProtocolParamsShelleyBased c
  -> SL.ProtVer
  -> ( ProtocolInfo (ShelleyBlock (TPraos c) ShelleyEra)
     , m [BlockForging m (ShelleyBlock (TPraos c) ShelleyEra)]
     )
protocolInfoShelley shelleyGenesis
                    protocolParamsShelleyBased
                    protVer =
    protocolInfoTPraosShelleyBased
      protocolParamsShelleyBased
      (L.mkShelleyTransitionConfig shelleyGenesis)
      protVer

protocolInfoTPraosShelleyBased ::
     forall m era c.
      ( IOLike m
      , PraosCrypto c
      , ShelleyCompatible (TPraos c) era
      , TxLimits (ShelleyBlock (TPraos c) era)
      -- , c ~ StandardCrypto
      )
  => ProtocolParamsShelleyBased c
  -> L.TransitionConfig era
  -> SL.ProtVer
     -- ^ see 'shelleyProtVer', mutatis mutandi
  -> ( ProtocolInfo (ShelleyBlock (TPraos c) era)
     , m [BlockForging m (ShelleyBlock (TPraos c) era)]
     )
protocolInfoTPraosShelleyBased ProtocolParamsShelleyBased {
                             shelleyBasedInitialNonce      = initialNonce
                           , shelleyBasedLeaderCredentials = credentialss
                           }
                         transitionCfg
                         protVer =
    assertWithMsg (validateGenesis genesis) $
    ( ProtocolInfo {
        pInfoConfig       = topLevelConfig
      , pInfoInitLedger   = initExtLedgerState
      }
    , traverse
        (shelleyBlockForging tpraosParams)
        credentialss
    )
  where
    genesis :: SL.ShelleyGenesis
    genesis = transitionCfg ^. L.tcShelleyGenesisL

    maxMajorProtVer :: MaxMajorProtVer
    maxMajorProtVer = MaxMajorProtVer $ SL.pvMajor protVer

    topLevelConfig :: TopLevelConfig (ShelleyBlock (TPraos c) era)
    topLevelConfig = TopLevelConfig {
        topLevelConfigProtocol    = consensusConfig
      , topLevelConfigLedger      = ledgerConfig
      , topLevelConfigBlock       = blockConfig
      , topLevelConfigCodec       = ShelleyCodecConfig
      , topLevelConfigStorage     = storageConfig
      , topLevelConfigCheckpoints = emptyCheckpointsMap
      }

    consensusConfig :: ConsensusConfig (BlockProtocol (ShelleyBlock (TPraos c) era))
    consensusConfig = TPraosConfig {
        tpraosParams
      , tpraosEpochInfo = epochInfo
      }

    ledgerConfig :: LedgerConfig (ShelleyBlock (TPraos c) era)
    ledgerConfig =
        mkShelleyLedgerConfig
         genesis
         (transitionCfg ^. L.tcTranslationContextL)
         epochInfo

    epochInfo :: EpochInfo (Except History.PastHorizonException)
    epochInfo =
        fixedEpochInfo
          (SL.sgEpochLength genesis)
          (mkSlotLength $ SL.fromNominalDiffTimeMicro $ SL.sgSlotLength genesis)

    tpraosParams :: TPraosParams
    tpraosParams = mkTPraosParams maxMajorProtVer initialNonce genesis

    blockConfig :: BlockConfig (ShelleyBlock (TPraos c) era)
    blockConfig =
        mkShelleyBlockConfig
          protVer
          genesis
          (shelleyBlockIssuerVKey <$> credentialss)

    storageConfig :: StorageConfig (ShelleyBlock (TPraos c) era)
    storageConfig = ShelleyStorageConfig {
          shelleyStorageConfigSlotsPerKESPeriod = tpraosSlotsPerKESPeriod tpraosParams
        , shelleyStorageConfigSecurityParam     = tpraosSecurityParam     tpraosParams
        }

    initLedgerState :: LedgerState (ShelleyBlock (TPraos c) era)
    initLedgerState = ShelleyLedgerState {
        shelleyLedgerTip        = Origin
      , shelleyLedgerState      =
            L.injectIntoTestState transitionCfg
          $ L.createInitialState  transitionCfg
      , shelleyLedgerTransition = ShelleyTransitionInfo {shelleyAfterVoting = 0}
      }

    initChainDepState :: TPraosState c
    initChainDepState = TPraosState Origin $
      SL.initialChainDepState initialNonce (SL.sgGenDelegs genesis)

    initExtLedgerState :: ExtLedgerState (ShelleyBlock (TPraos c) era)
    initExtLedgerState = ExtLedgerState {
        ledgerState = initLedgerState
      , headerState = genesisHeaderState initChainDepState
      }
