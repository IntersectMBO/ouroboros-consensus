{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Coerce
import qualified Data.Map.Strict as Map
import Data.MemPack (MemPack)
import Data.SOP.BasicFunctors
import qualified Data.SOP.InPairs as InPairs
import Data.SOP.Index (Index (..), injectNS)
import Data.SOP.Strict
import qualified Data.SOP.Tails as Tails
import Data.Void (Void)
import Lens.Micro ((%~), (&), (.~), (^.))
import Ouroboros.Consensus.Block.Forging (MkBlockForging)
import Ouroboros.Consensus.Cardano.CanHardFork
  ( crossEraForecastAcrossShelley
  , translateChainDepStateAcrossShelley
  )
import Ouroboros.Consensus.Cardano.Node (TriggerHardFork (..))
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.Embed.Binary
import Ouroboros.Consensus.HardFork.Combinator.Serialisation
import Ouroboros.Consensus.HardFork.Combinator.State.Types as HFC
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
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
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util (eitherToMaybe)
import System.FS.API (SomeHasFS)
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
  , TranslateTxMeasure
      (TxMeasurePhase1 (ShelleyBlock proto1 era1))
      (TxMeasurePhase1 (ShelleyBlock proto2 era2))
  , TranslateTxMeasure
      (TxMeasurePhase2 (ShelleyBlock proto1 era1))
      (TxMeasurePhase2 (ShelleyBlock proto2 era2))
  , SL.PreviousEra era2 ~ era1
  , SL.TranslateEra era2 SL.NewEpochState
  , SL.TranslateEra era2 (SL.Tx SL.TopTx)
  , SL.TranslationError era2 SL.NewEpochState ~ Void
  , -- At the moment, fix the protocols together
    ProtoCrypto proto1 ~ ProtoCrypto proto2
  , PraosCrypto (ProtoCrypto proto1)
  , proto1 ~ TPraos (ProtoCrypto proto1)
  , proto1 ~ proto2
  , MemPack (TxOut (ShelleyBlock proto1 era1))
  , MemPack (TxOut (ShelleyBlock proto2 era2))
  )

class TranslateTxMeasure a b where
  translateTxMeasure :: a -> b

-- Phase 1 measures

instance TranslateTxMeasure (IgnoringOverflow ByteSize32) (IgnoringOverflow ByteSize32) where
  translateTxMeasure = id

instance TranslateTxMeasure (IgnoringOverflow ByteSize32) AlonzoMeasure where
  translateTxMeasure x = AlonzoMeasure x mempty

instance TranslateTxMeasure AlonzoMeasure AlonzoMeasure where
  translateTxMeasure = id

-- Phase 2 measures

instance TranslateTxMeasure TrivialTxMeasurePhase2 TrivialTxMeasurePhase2 where
  translateTxMeasure = id

instance TranslateTxMeasure TrivialTxMeasurePhase2 RefScriptSize where
  translateTxMeasure TrivialTxMeasurePhase2 = mempty

instance TranslateTxMeasure RefScriptSize RefScriptSize where
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
    HardForkTxMeasurePhase1 (ShelleyBasedHardForkEras proto1 era1 proto2 era2) =
      TxMeasurePhase1 (ShelleyBlock proto2 era2)
  type
    HardForkTxMeasurePhase2 (ShelleyBasedHardForkEras proto1 era1 proto2 era2) =
      TxMeasurePhase2 (ShelleyBlock proto2 era2)

  hardForkEraTranslation =
    EraTranslation
      { translateLedgerState = PCons translateLedgerState PNil
      , translateDiff =
          PCons (TranslateDiff (translateLedgerTablesWith translateLedgerTables)) PNil
      , -- A pure-upgrade boundary: the on-disk values upgrade their @TxOut@s when
        -- the first block's values are read (the @TxIn@ is era-stable).
        translateValues = PCons (TranslateValues (Map.map SL.upgradeTxOut)) PNil
      , -- The @TxIn@ key type is era-stable, so the keys are unchanged.
        translateKeys = PCons (TranslateKeys id) PNil
      , translateChainDepState = PCons translateChainDepStateAcrossShelley PNil
      , crossEraForecast = PCons crossEraForecastAcrossShelley PNil
      }
   where
    translateLedgerState ::
      InPairs.RequiringBoth
        WrapLedgerConfig
        TranslateLedgerState
        (ShelleyBlock proto1 era1)
        (ShelleyBlock proto2 era2)
    translateLedgerState =
      InPairs.RequireBoth $
        \_cfg1 cfg2 ->
          HFC.TranslateLedgerState
            { translateLedgerStateWith = \_epochNo ls ->
                -- Pure-upgrade boundary: the state translates with no new diffs.
                ( unComp
                    . SL.translateEra'
                      (shelleyLedgerTranslationContext (unwrapLedgerConfig cfg2))
                    . Comp
                    $ ls
                , mempty
                )
            }

    translateLedgerTables ::
      TranslateLedgerTables
        (ShelleyBlock proto1 era1)
        (ShelleyBlock proto2 era2)
    translateLedgerTables =
      HFC.TranslateLedgerTables
        { translateTxInWith = coerce
        , translateTxOutWith = SL.upgradeTxOut
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

  hardForkInjTxMeasurePhase1 = \case
    (Z (WrapTxMeasurePhase1 x)) -> translateTxMeasure x
    S (Z (WrapTxMeasurePhase1 x)) -> x

  hardForkInjTxMeasurePhase2 = \case
    (Z (WrapTxMeasurePhase2 x)) -> translateTxMeasure x
    S (Z (WrapTxMeasurePhase2 x)) -> x

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
  Query HF
-------------------------------------------------------------------------------}

answerShelleyBasedQueryHF ::
  ( xs ~ '[ShelleyBlock proto1 era1, ShelleyBlock proto2 era2]
  , ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
  ) =>
  ( forall blk.
    IsShelleyBlock blk =>
    Index xs blk ->
    ExtLedgerCfg blk ->
    BlockQuery blk footprint result ->
    ReadOnlyForker' m (HardForkBlock xs) ->
    m result
  ) ->
  Index xs x ->
  ExtLedgerCfg x ->
  BlockQuery x footprint result ->
  ReadOnlyForker' m (HardForkBlock xs) ->
  m result
answerShelleyBasedQueryHF f idx cfgs q forker = case idx of
  IZ -> f idx cfgs q forker
  IS IZ -> f idx cfgs q forker
  IS (IS idx') -> case idx' of {}

-- | Project the current era's values out of the hard-fork @NS@ (the forker read
-- returns values tagged with the era we injected the keys at, so the requested
-- arm is present).
projectShelleyBasedValues ::
  Index '[ShelleyBlock proto1 era1, ShelleyBlock proto2 era2] x ->
  Values (HardForkBlock '[ShelleyBlock proto1 era1, ShelleyBlock proto2 era2]) ->
  Values x
projectShelleyBasedValues idx0 =
  maybe (error "projectShelleyBasedValues: values in unexpected era") unwrapValues
    . go idx0
 where
  go :: Index xs x -> NS WrapValues xs -> Maybe (WrapValues x)
  go IZ (Z x) = Just x
  go (IS idx) (S ns) = go idx ns
  go _ _ = Nothing

instance
  ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 =>
  BlockSupportsHFLedgerQuery '[ShelleyBlock proto1 era1, ShelleyBlock proto2 era2]
  where
  answerBlockQueryHFLookup idx cfg q forker =
    answerShelleyBasedQueryHF
      ( \idx' cfg' q' forker' ->
          answerShelleyLookupQueries
            (injectNS idx' . WrapKeys)
            (projectShelleyBasedValues idx')
            cfg'
            q'
            forker'
      )
      idx
      cfg
      q
      forker

  answerBlockQueryHFTraverse idx cfg q provider forker =
    answerShelleyBasedQueryHF
      ( \idx' cfg' q' _forker' ->
          answerShelleyTraversingQueries
            (projectShelleyBasedValues idx')
            shelleyQFTraverseTablesPredicate
            cfg'
            q'
            provider
      )
      idx
      cfg
      q
      forker

{-------------------------------------------------------------------------------
  Protocol info
-------------------------------------------------------------------------------}

protocolInfoShelleyBasedHardFork ::
  forall m proto1 era1 proto2 era2.
  ( KESAgentContext (ProtoCrypto proto2) m
  , ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
  ) =>
  SomeHasFS m ->
  ProtocolParamsShelleyBased (ProtoCrypto proto1) ->
  SL.ProtVer ->
  SL.ProtVer ->
  L.TransitionConfig era2 ->
  TriggerHardFork ->
  m
    ( ProtocolInfo (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)
    , Tracer.Tracer m KESAgentClientTrace ->
      m [MkBlockForging m (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)]
    )
protocolInfoShelleyBasedHardFork
  fs
  protocolParamsShelleyBased
  protVer1
  protVer2
  transCfg2
  hardForkTrigger = do
    (protocolInfo1, blockForging1) <-
      protocolInfoTPraosShelleyBased
        fs
        protocolParamsShelleyBased
        transCfgEra1
        protVer1
    (protocolInfo2, blockForging2) <-
      protocolInfoTPraosShelleyBased
        fs
        ProtocolParamsShelleyBased
          { shelleyBasedInitialNonce
          , shelleyBasedLeaderCredentials
          }
        transCfgEra2
        protVer2
    pure $
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
    ProtocolParamsShelleyBased
      { shelleyBasedInitialNonce
      , shelleyBasedLeaderCredentials
      } = protocolParamsShelleyBased

    -- Override the protocol version inside the Shelley genesis carried by a
    -- transition config. Each era's 'createInitialState' enforces
    -- 'eraProtVerLow <= curProtVer <= eraProtVerHigh', so a single shared
    -- genesis PV cannot satisfy both era1 and era2; we derive a per-era copy.
    overrideGenesisPV ::
      forall era.
      L.EraTransition era =>
      SL.ProtVer ->
      L.TransitionConfig era ->
      L.TransitionConfig era
    overrideGenesisPV pv =
      L.tcShelleyGenesisL %~ \sg ->
        sg{SL.sgProtocolParams = SL.sgProtocolParams sg & SL.ppProtocolVersionL .~ pv}

    transCfgEra1 :: L.TransitionConfig era1
    transCfgEra1 = (transCfg2 ^. L.tcPreviousEraConfigL) & overrideGenesisPV protVer1

    transCfgEra2 :: L.TransitionConfig era2
    transCfgEra2 = transCfg2 & overrideGenesisPV protVer2

    -- Era 1

    genesis :: SL.ShelleyGenesis
    genesis = transCfg2 ^. L.tcShelleyGenesisL

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
  testReadAllValues = testReadAllValuesHfc
