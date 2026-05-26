{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test infrastructure to test hard-forking from one Shelley-based era to
-- another, e.g., Shelley to Allegra.
module Test.ThreadNet.Infra.ShelleyBasedHardFork
  ( -- * Blocks
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
import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Shelley.API as SL
import Control.Monad.Except (runExcept)
import qualified Control.Tracer as Tracer
import qualified Data.Map.Strict as Map
import Data.SOP.BasicFunctors
import qualified Data.SOP.InPairs as InPairs
import Data.SOP.Strict
import qualified Data.SOP.Tails as Tails
import Data.Void (Void)
import Lens.Micro ((^.))
import Ouroboros.Consensus.Block.Forging (MkBlockForging)
import Ouroboros.Consensus.Cardano.CanHardFork
  ( crossEraForecastAcrossShelley
  , translateChainDepStateAcrossShelley
  )
import Ouroboros.Consensus.Cardano.Node (TriggerHardFork (..))
import Ouroboros.Consensus.Config (TopLevelConfig (..))
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.Embed.Binary
import Ouroboros.Consensus.HardFork.Combinator.Serialisation
import Ouroboros.Consensus.HardFork.Combinator.State.Types as HFC
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsPeras (LedgerSupportsPeras)
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol
  )
import Ouroboros.Consensus.Node
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Protocol.Praos.AgentClient
  ( KESAgentClientTrace
  , KESAgentContext
  )
import Ouroboros.Consensus.Protocol.TPraos
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Node
import Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto)
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util (eitherToMaybe)
import Ouroboros.Consensus.Util.IOLike (MonadThrow)
import Test.ThreadNet.TxGen
import Test.ThreadNet.TxGen.Shelley ()

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
  GenTx (ShelleyBlock proto1 era1) ->
  ShelleyBasedHardForkGenTx proto1 era1 proto2 era2
pattern GenTxShelley1 tx = HardForkGenTx (OneEraGenTx (Z tx))

pattern GenTxShelley2 ::
  GenTx (ShelleyBlock proto2 era2) ->
  ShelleyBasedHardForkGenTx proto1 era1 proto2 era2
pattern GenTxShelley2 tx = HardForkGenTx (OneEraGenTx (S (Z tx)))

{-# COMPLETE GenTxShelley1, GenTxShelley2 #-}

pattern ShelleyBasedHardForkNodeToNodeVersionMax ::
  BlockNodeToNodeVersion (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)
pattern ShelleyBasedHardForkNodeToNodeVersionMax =
  HardForkNodeToNodeEnabled
    HardForkSpecificNodeToNodeVersionMax
    ( WrapNodeToNodeVersion ShelleyNodeToNodeVersionMax
        :* WrapNodeToNodeVersion ShelleyNodeToNodeVersionMax
        :* Nil
      )

pattern HardForkSpecificNodeToNodeVersionMax :: HardForkSpecificNodeToNodeVersion
pattern HardForkSpecificNodeToNodeVersionMax <- ((== maxBound) -> True)
  where
    HardForkSpecificNodeToNodeVersionMax = maxBound

pattern ShelleyNodeToNodeVersionMax :: ShelleyNodeToNodeVersion
pattern ShelleyNodeToNodeVersionMax <- ((== maxBound) -> True)
  where
    ShelleyNodeToNodeVersionMax = maxBound

pattern ShelleyBasedHardForkNodeToClientVersionMax ::
  BlockNodeToClientVersion (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)
pattern ShelleyBasedHardForkNodeToClientVersionMax =
  HardForkNodeToClientEnabled
    HardForkSpecificNodeToClientVersionMax
    ( EraNodeToClientEnabled ShelleyNodeToClientVersionMax
        :* EraNodeToClientEnabled ShelleyNodeToClientVersionMax
        :* Nil
      )

pattern HardForkSpecificNodeToClientVersionMax :: HardForkSpecificNodeToClientVersion
pattern HardForkSpecificNodeToClientVersionMax <- ((== maxBound) -> True)
  where
    HardForkSpecificNodeToClientVersionMax = maxBound

pattern ShelleyNodeToClientVersionMax :: ShelleyNodeToClientVersion
pattern ShelleyNodeToClientVersionMax <- ((== maxBound) -> True)
  where
    ShelleyNodeToClientVersionMax = maxBound

{-------------------------------------------------------------------------------
  Consensus instances
-------------------------------------------------------------------------------}

type ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 =
  ( ShelleyCompatible proto1 era1
  , ShelleyCompatible proto2 era2
  , LedgerSupportsProtocol (ShelleyBlock proto1 era1)
  , LedgerSupportsProtocol (ShelleyBlock proto2 era2)
  , LedgerSupportsPeras (ShelleyBlock proto1 era1)
  , LedgerSupportsPeras (ShelleyBlock proto2 era2)
  , TxLimits (ShelleyBlock proto1 era1)
  , TxLimits (ShelleyBlock proto2 era2)
  , TranslateTxMeasure (TxMeasure (ShelleyBlock proto1 era1)) (TxMeasure (ShelleyBlock proto2 era2))
  , SL.PreviousEra era2 ~ era1
  , SL.TranslateEra era2 SL.NewEpochState
  , SL.TranslateEra era2 (SL.Tx SL.TopTx)
  , SL.TranslationError era2 SL.NewEpochState ~ Void
  , -- At the moment, fix the protocols together
    ProtoCrypto proto1 ~ ProtoCrypto proto2
  , PraosCrypto (ProtoCrypto proto1)
  , proto1 ~ TPraos (ProtoCrypto proto1)
  , proto1 ~ proto2
  )

class TranslateTxMeasure a b where
  translateTxMeasure :: a -> b

instance TranslateTxMeasure (IgnoringOverflow ByteSize32) (IgnoringOverflow ByteSize32) where
  translateTxMeasure = id

instance TranslateTxMeasure (IgnoringOverflow ByteSize32) AlonzoMeasure where
  translateTxMeasure x = AlonzoMeasure x mempty

instance TranslateTxMeasure (IgnoringOverflow ByteSize32) ConwayMeasure where
  translateTxMeasure =
    translateTxMeasure . (\x -> x :: AlonzoMeasure) . translateTxMeasure

instance TranslateTxMeasure AlonzoMeasure AlonzoMeasure where
  translateTxMeasure = id

instance TranslateTxMeasure AlonzoMeasure ConwayMeasure where
  translateTxMeasure x = ConwayMeasure x mempty

instance TranslateTxMeasure ConwayMeasure ConwayMeasure where
  translateTxMeasure = id

instance
  ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 =>
  SerialiseHFC (ShelleyBasedHardForkEras proto1 era1 proto2 era2)

-- use defaults

instance
  ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 =>
  CanHardFork (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
  where
  type
    HardForkTxMeasure (ShelleyBasedHardForkEras proto1 era1 proto2 era2) =
      TxMeasure (ShelleyBlock proto2 era2)

  type
    HFLedgerTablesFactory m (ShelleyBasedHardForkEras proto1 era1 proto2 era2) =
      MkHandle m

  hardForkStateHandleTranslation = \_tctx ->
    StateHandleTranslation
      { translateLedgerState = PCons translateLedgerState PNil
      }
   where
    translateLedgerState ::
      MonadThrow m =>
      InPairs.RequiringBoth
        WrapLedgerConfig
        (TranslateLedgerState m)
        (ShelleyBlock proto1 era1)
        (ShelleyBlock proto2 era2)
    translateLedgerState =
      InPairs.RequireBoth $
        \_cfg1 cfg2 ->
          HFC.TranslateLedgerState
            { translateLedgerStateWith = \_epochNo (ShelleyStateHandle st h) ->
                let st' =
                      unComp
                        . SL.translateEra'
                          (shelleyLedgerTranslationContext (unwrapLedgerConfig cfg2))
                        $ Comp st
                 in ShelleyStateHandle st' <$> castHandle h (shelleyLedgerState st')
            }

  hardForkEraTranslation =
    EraTranslation
      { translateChainDepState = PCons translateChainDepStateAcrossShelley PNil
      , crossEraForecast = PCons crossEraForecastAcrossShelley PNil
      }

  hardForkChainSel = Tails.mk2 SameTiebreakerAcrossEras

  hardForkInjectTxs =
    InPairs.mk2 $
      InPairs.RequireBoth $ \_cfg1 cfg2 ->
        let ctxt = shelleyLedgerTranslationContext (unwrapLedgerConfig cfg2)
         in Pair2
              (InjectTx (translateTx ctxt))
              (InjectValidatedTx (translateValidatedTx ctxt))
   where
    translateTx ::
      SL.TranslationContext era2 ->
      GenTx (ShelleyBlock proto era1) ->
      Maybe (GenTx (ShelleyBlock proto era2))
    translateTx transCtxt =
      fmap unComp
        . eitherToMaybe
        . runExcept
        . SL.translateEra transCtxt
        . Comp

    translateValidatedTx ::
      SL.TranslationContext era2 ->
      WrapValidatedGenTx (ShelleyBlock proto era1) ->
      Maybe (WrapValidatedGenTx (ShelleyBlock proto era2))
    translateValidatedTx transCtxt =
      fmap unComp
        . eitherToMaybe
        . runExcept
        . SL.translateEra transCtxt
        . Comp

  hardForkInjTxMeasure = \case
    (Z (WrapTxMeasure x)) -> translateTxMeasure x
    S (Z (WrapTxMeasure x)) -> x

instance
  ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 =>
  SupportedNetworkProtocolVersion (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)
  where
  supportedNodeToNodeVersions _ =
    Map.fromList $
      [ (maxBound, ShelleyBasedHardForkNodeToNodeVersionMax)
      ]

  supportedNodeToClientVersions _ =
    Map.fromList $
      [ (maxBound, ShelleyBasedHardForkNodeToClientVersionMax)
      ]

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

{-------------------------------------------------------------------------------
  Protocol info
-------------------------------------------------------------------------------}

protocolInfoShelleyBasedHardFork ::
  forall m proto1 era1 proto2 era2.
  ( KESAgentContext (ProtoCrypto proto2) m
  , ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
  ) =>
  ProtocolParamsShelleyBased (ProtoCrypto proto1) ->
  SL.ProtVer ->
  SL.ProtVer ->
  L.TransitionConfig era2 ->
  TriggerHardFork ->
  -- | Backend factory shared by both eras. For an in-memory backend, pass
  -- the 'MkHandle' produced by 'mkInMemoryFactory'.
  MkHandle m ->
  ( ProtocolInfo m (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)
  , Tracer.Tracer m KESAgentClientTrace ->
    m [MkBlockForging m (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)]
  )
protocolInfoShelleyBasedHardFork
  protocolParamsShelleyBased
  protVer1
  protVer2
  transCfg2
  hardForkTrigger
  mkH =
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
      -- Project the HFC table factory down to era 1's per-era factory.
      -- Shelley's per-era 'LedgerTablesFactory' is @()@: the per-era
      -- 'protocolInfoTPraosShelleyBased' already captures the 'MkHandle'
      -- via its closure (passed in as 'mkH'), so the per-era factory
      -- doesn't need to carry anything.
      (const ())
   where
    ProtocolParamsShelleyBased
      { shelleyBasedInitialNonce
      , shelleyBasedLeaderCredentials
      } = protocolParamsShelleyBased

    -- Era 1

    genesis :: SL.ShelleyGenesis
    genesis = transCfg2 ^. L.tcShelleyGenesisL

    protocolInfo1 :: ProtocolInfo m (ShelleyBlock proto1 era1)
    blockForging1 ::
      Tracer.Tracer m KESAgentClientTrace -> m [MkBlockForging m (ShelleyBlock proto1 era1)]
    (protocolInfo1, blockForging1) =
      -- 'protocolInfoTPraosShelleyBased' derives the @maxMajorProtVer@
      -- (used by Shelley's CHAIN rule to reject blocks whose advertised
      -- protocol version exceeds the node's max) from the supplied
      -- 'protVer'. For an era-1 protocolInfo built that way, the max
      -- ends up at @pvMajor protVer1@. But protocol-update transactions
      -- inside era 1 can bump the chain's current protocol version up
      -- to @protVer2@ (the trigger value) /before/ the era transition
      -- fires at the next epoch boundary: in that window era-1
      -- successor blocks carry protocol version 2 in their header. We
      -- override era 1's @tpraosMaxMajorPV@ to @protVer2@ so the CHAIN
      -- rule still accepts them.
      --
      -- This mirrors what mainline @protocolInfoCardano@ does: it sets
      -- a single shared @maxMajorProtVer@ (the FINAL era's version) for
      -- every era's protocolInfo, rather than deriving it per era.
      let (raw, bf) =
            protocolInfoTPraosShelleyBased
              protocolParamsShelleyBased
              (transCfg2 ^. L.tcPreviousEraConfigL)
              protVer1
              mkH
       in (bumpMaxProtVer raw, bf)

    bumpMaxProtVer ::
      ProtocolInfo m (ShelleyBlock proto1 era1) ->
      ProtocolInfo m (ShelleyBlock proto1 era1)
    bumpMaxProtVer pInfo =
      pInfo
        { pInfoConfig =
            let tlc = pInfoConfig pInfo
                tpcfg = topLevelConfigProtocol tlc
                params = tpraosParams tpcfg
             in tlc
                  { topLevelConfigProtocol =
                      tpcfg
                        { tpraosParams =
                            params
                              { tpraosMaxMajorPV = MaxMajorProtVer (SL.pvMajor protVer2)
                              }
                        }
                  }
        }

    eraParams1 :: History.EraParams
    eraParams1 = shelleyEraParams genesis

    toPartialLedgerConfig1 ::
      LedgerConfig (ShelleyBlock proto1 era1) ->
      PartialLedgerConfig (ShelleyBlock proto1 era1)
    toPartialLedgerConfig1 cfg =
      ShelleyPartialLedgerConfig
        { shelleyLedgerConfig = cfg
        , shelleyTriggerHardFork = hardForkTrigger
        }

    -- Era 2

    protocolInfo2 :: ProtocolInfo m (ShelleyBlock proto2 era2)
    blockForging2 ::
      Tracer.Tracer m KESAgentClientTrace -> m [MkBlockForging m (ShelleyBlock proto2 era2)]
    (protocolInfo2, blockForging2) =
      protocolInfoTPraosShelleyBased
        ProtocolParamsShelleyBased
          { shelleyBasedInitialNonce
          , shelleyBasedLeaderCredentials
          }
        transCfg2
        protVer2
        mkH

    eraParams2 :: History.EraParams
    eraParams2 = shelleyEraParams genesis

    toPartialLedgerConfig2 ::
      LedgerConfig (ShelleyBlock proto2 era2) ->
      PartialLedgerConfig (ShelleyBlock proto2 era2)
    toPartialLedgerConfig2 cfg =
      ShelleyPartialLedgerConfig
        { shelleyLedgerConfig = cfg
        , shelleyTriggerHardFork = TriggerHardForkNotDuringThisExecution
        }

{-------------------------------------------------------------------------------
  TxGen instance
-------------------------------------------------------------------------------}

-- | Use a generic implementation for 'TxGen'
instance
  ( TxGen (ShelleyBlock proto1 era1)
  , TxGen (ShelleyBlock proto2 era2)
  , ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
  ) =>
  TxGen (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)
  where
  type
    TxGenExtra (ShelleyBasedHardForkBlock proto1 era1 proto2 era2) =
      NP WrapTxGenExtra (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
  testGenTxs = testGenTxsHfc
