{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test infrastructure to test hard-forking from one Shelley-based era to
-- another, e.g., Shelley to Allegra.
module Test.ThreadNet.Infra.ShelleyBasedHardFork (
    -- * Blocks
    ShelleyBasedHardForkBlock
  , ShelleyBasedHardForkEras
    -- * Transactions
  , pattern GenTxShelley1
  , pattern GenTxShelley2
    -- * Node
  , ShelleyBasedHardForkConstraints
  , protocolInfoShelleyBasedHardFork
  ) where

import qualified Cardano.Ledger.Api.Transition as L
import qualified Cardano.Ledger.Era as SL
import qualified Cardano.Ledger.Shelley.API as SL
import           Control.Monad.Except (runExcept)
import qualified Data.Map.Strict as Map
import           Data.SOP.BasicFunctors
import qualified Data.SOP.InPairs as InPairs
import           Data.SOP.Strict
import qualified Data.SOP.Tails as Tails
import           Data.Void (Void)
import           Lens.Micro ((^.))
import           Ouroboros.Consensus.Block.Forging (BlockForging)
import           Ouroboros.Consensus.Cardano.CanHardFork
                     (ShelleyPartialLedgerConfig (..), forecastAcrossShelley,
                     translateChainDepStateAcrossShelley)
import           Ouroboros.Consensus.Cardano.Node (TriggerHardFork (..))
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Binary
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation
import qualified Ouroboros.Consensus.HardFork.Combinator.State.Types as HFC
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Basics (LedgerConfig)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Mempool (TxLimits)
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (eitherToMaybe)
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import           Test.ThreadNet.TxGen
import           Test.ThreadNet.TxGen.Shelley ()

{-------------------------------------------------------------------------------
  Block type
-------------------------------------------------------------------------------}

-- | Two eras, both Shelley-based.
type ShelleyBasedHardForkEras proto1 era1 proto2 era2 =
    '[ShelleyBlock proto1 era1, ShelleyBlock proto2 era2]

type ShelleyBasedHardForkBlock proto1 era1 proto2 era2 =
  HardForkBlock (ShelleyBasedHardForkEras proto1 era1 proto2 era2)

{-------------------------------------------------------------------------------
  Pattern synonyms, for encapsulation and legibility
-------------------------------------------------------------------------------}

type ShelleyBasedHardForkGenTx proto1 era1 proto2 era2 =
  GenTx (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)

pattern GenTxShelley1 ::
     GenTx (ShelleyBlock proto1 era1)
  -> ShelleyBasedHardForkGenTx proto1 era1 proto2 era2
pattern GenTxShelley1 tx = HardForkGenTx (OneEraGenTx (Z tx))

pattern GenTxShelley2 ::
     GenTx (ShelleyBlock proto2 era2)
  -> ShelleyBasedHardForkGenTx proto1 era1 proto2 era2
pattern GenTxShelley2 tx = HardForkGenTx (OneEraGenTx (S (Z tx)))

{-# COMPLETE GenTxShelley1, GenTxShelley2 #-}

pattern ShelleyBasedHardForkNodeToNodeVersion1 ::
     BlockNodeToNodeVersion (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)
pattern ShelleyBasedHardForkNodeToNodeVersion1 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  WrapNodeToNodeVersion ShelleyNodeToNodeVersion1
      :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1
      :* Nil
      )

pattern ShelleyBasedHardForkNodeToClientVersion1 ::
     BlockNodeToClientVersion (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)
pattern ShelleyBasedHardForkNodeToClientVersion1 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* Nil
      )

{-------------------------------------------------------------------------------
  Consensus instances
-------------------------------------------------------------------------------}

type ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 =
  ( ShelleyCompatible proto1 era1
  , ShelleyCompatible proto2 era2
  , LedgerSupportsProtocol (ShelleyBlock proto1 era1)
  , LedgerSupportsProtocol (ShelleyBlock proto2 era2)
  , TxLimits (ShelleyBlock proto1 era1)
  , TxLimits (ShelleyBlock proto2 era2)
  , SL.PreviousEra era2 ~ era1

  , SL.TranslateEra       era2 SL.NewEpochState
  , SL.TranslateEra       era2 WrapTx

  , SL.TranslationError   era2 SL.NewEpochState ~ Void

    -- At the moment, fix the protocols together
  , EraCrypto era1 ~ EraCrypto era2
  , PraosCrypto (EraCrypto era1)
  , proto1 ~ TPraos (EraCrypto era1)
  , proto1 ~ proto2
  )

instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
      => SerialiseHFC (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
   -- use defaults

instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
      => CanHardFork (ShelleyBasedHardForkEras proto1 era1 proto2 era2) where
  hardForkEraTranslation = EraTranslation {
        translateLedgerState   = PCons translateLedgerState                PNil
      , translateChainDepState = PCons translateChainDepStateAcrossShelley PNil
      , crossEraForecast       = PCons forecastAcrossShelleyWrapper        PNil
      }
    where
      translateLedgerState ::
           InPairs.RequiringBoth
             WrapLedgerConfig
             (HFC.Translate LedgerState)
             (ShelleyBlock proto1 era1)
             (ShelleyBlock proto2 era2)
      translateLedgerState =
          InPairs.RequireBoth
        $ \_cfg1 cfg2 -> HFC.Translate
        $ \_epochNo ->
              unComp
            . SL.translateEra'
                (shelleyLedgerTranslationContext (unwrapLedgerConfig cfg2))
            . Comp

      forecastAcrossShelleyWrapper ::
           InPairs.RequiringBoth
              WrapLedgerConfig
              (HFC.CrossEraForecaster LedgerState WrapLedgerView)
              (ShelleyBlock proto1 era1)
              (ShelleyBlock proto2 era2)
      forecastAcrossShelleyWrapper =
          InPairs.RequireBoth $ \(WrapLedgerConfig cfg1) (WrapLedgerConfig cfg2) ->
            HFC.CrossEraForecaster $ forecastAcrossShelley cfg1 cfg2

  hardForkChainSel = Tails.mk2 CompareSameSelectView

  hardForkInjectTxs =
        InPairs.mk2
      $ InPairs.RequireBoth $ \_cfg1 cfg2 ->
        let ctxt = shelleyLedgerTranslationContext (unwrapLedgerConfig cfg2)
        in
          Pair2
            (InjectTx          (translateTx          ctxt))
            (InjectValidatedTx (translateValidatedTx ctxt))
    where
      translateTx ::
           SL.TranslationContext era2
        ->        GenTx (ShelleyBlock proto era1)
        -> Maybe (GenTx (ShelleyBlock proto era2))
      translateTx transCtxt =
          fmap unComp
        . eitherToMaybe . runExcept . SL.translateEra transCtxt
        . Comp

      translateValidatedTx ::
           SL.TranslationContext era2
        ->        WrapValidatedGenTx (ShelleyBlock proto era1)
        -> Maybe (WrapValidatedGenTx (ShelleyBlock proto era2))
      translateValidatedTx transCtxt =
            fmap unComp
          . eitherToMaybe . runExcept . SL.translateEra transCtxt
          . Comp

instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
      => SupportedNetworkProtocolVersion (ShelleyBasedHardForkBlock proto1 era1 proto2 era2) where
  supportedNodeToNodeVersions _ = Map.fromList $
      [ (maxBound, ShelleyBasedHardForkNodeToNodeVersion1)
      ]

  supportedNodeToClientVersions _ = Map.fromList $
      [ (maxBound, ShelleyBasedHardForkNodeToClientVersion1)
      ]

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

{-------------------------------------------------------------------------------
  Protocol info
-------------------------------------------------------------------------------}

protocolInfoShelleyBasedHardFork ::
     forall m proto1 era1 proto2 era2.
     (IOLike m, ShelleyBasedHardForkConstraints proto1 era1 proto2 era2)
  => ProtocolParamsShelleyBased (EraCrypto era1)
  -> SL.ProtVer
  -> SL.ProtVer
  -> L.TransitionConfig era2
  -> TriggerHardFork
  -> ( ProtocolInfo      (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)
     , m [BlockForging m (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)]
     )
protocolInfoShelleyBasedHardFork protocolParamsShelleyBased
                                 protVer1
                                 protVer2
                                 transCfg2
                                 hardForkTrigger =
    protocolInfoBinary
      -- Era 1
      protocolInfo1
      blockForging1
      eraParams1
      tpraosParams
      toPartialLedgerConfig1
      -- Era 2
      protocolInfo2
      blockForging2
      eraParams2
      tpraosParams
      toPartialLedgerConfig2
  where
    ProtocolParamsShelleyBased {
        shelleyBasedInitialNonce
      , shelleyBasedLeaderCredentials
      } = protocolParamsShelleyBased

    -- Era 1

    genesis :: SL.ShelleyGenesis (EraCrypto era1)
    genesis = transCfg2 ^. L.tcShelleyGenesisL

    protocolInfo1 :: ProtocolInfo (ShelleyBlock proto1 era1)
    blockForging1 :: m [BlockForging m (ShelleyBlock proto1 era1)]
    (protocolInfo1, blockForging1) =
        protocolInfoTPraosShelleyBased
          protocolParamsShelleyBased
          (transCfg2 ^. L.tcPreviousEraConfigL)
          protVer1
          (Mempool.mkOverrides Mempool.noOverridesMeasure)

    eraParams1 :: History.EraParams
    eraParams1 = shelleyEraParams genesis

    toPartialLedgerConfig1 ::
         LedgerConfig (ShelleyBlock proto1 era1)
      -> PartialLedgerConfig (ShelleyBlock proto1 era1)
    toPartialLedgerConfig1 cfg = ShelleyPartialLedgerConfig {
          shelleyLedgerConfig    = cfg
        , shelleyTriggerHardFork = hardForkTrigger
        }

    -- Era 2

    protocolInfo2 :: ProtocolInfo (ShelleyBlock proto2 era2)
    blockForging2 :: m [BlockForging m (ShelleyBlock proto2 era2)]
    (protocolInfo2, blockForging2) =
        protocolInfoTPraosShelleyBased
          ProtocolParamsShelleyBased {
              shelleyBasedInitialNonce
            , shelleyBasedLeaderCredentials
            }
          transCfg2
          protVer2
          (Mempool.mkOverrides Mempool.noOverridesMeasure)

    eraParams2 :: History.EraParams
    eraParams2 = shelleyEraParams genesis

    toPartialLedgerConfig2 ::
         LedgerConfig (ShelleyBlock proto2 era2)
      -> PartialLedgerConfig (ShelleyBlock proto2 era2)
    toPartialLedgerConfig2 cfg = ShelleyPartialLedgerConfig {
          shelleyLedgerConfig    = cfg
        , shelleyTriggerHardFork = TriggerHardForkNotDuringThisExecution
        }

{-------------------------------------------------------------------------------
  TxGen instance
-------------------------------------------------------------------------------}

-- | Use a generic implementation for 'TxGen'
instance ( TxGen (ShelleyBlock proto1 era1)
         , TxGen (ShelleyBlock proto2 era2)
         , ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
         ) => TxGen (ShelleyBasedHardForkBlock proto1 era1 proto2 era2) where
  type TxGenExtra (ShelleyBasedHardForkBlock proto1 era1 proto2 era2) =
    NP WrapTxGenExtra (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
  testGenTxs = testGenTxsHfc
