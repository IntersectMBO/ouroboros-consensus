{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module is the Shelley Hard Fork Combinator
module Ouroboros.Consensus.Shelley.ShelleyHFC (
    ProtocolShelley
  , ShelleyBlockHFC
  , ShelleyPartialLedgerConfig (..)
  , crossEraForecastAcrossShelley
  , forecastAcrossShelley
  , translateChainDepStateAcrossShelley
  ) where

import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Ledger.BaseTypes as SL (mkVersion)
import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Protocol.TPraos.API as SL
import           Cardano.Slotting.EpochInfo (hoistEpochInfo)
import           Control.Monad (guard)
import           Control.Monad.Except (runExcept, throwError, withExceptT)
import qualified Data.Map.Strict as Map
import           Data.SOP.BasicFunctors
import           Data.SOP.Functors (Flip (..))
import           Data.SOP.Index (Index (..))
import           Data.SOP.InPairs (RequiringBoth (..), ignoringBoth)
import qualified Data.Text as T (pack)
import           Data.Void (Void)
import           Data.Word
import           GHC.Generics (Generic)
import           Lens.Micro ((^.))
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import qualified Ouroboros.Consensus.Forecast as Forecast
import           Ouroboros.Consensus.HardFork.Combinator hiding
                     (translateChainDepState)
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.History (Bound (boundSlot))
import           Ouroboros.Consensus.HardFork.Simple
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool (TxLimits)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol, ledgerViewForecastAt)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Protocol.TPraos hiding (PraosCrypto)
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.Inspect as Shelley.Inspect
import           Ouroboros.Consensus.Shelley.Node ()
import           Ouroboros.Consensus.TypeFamilyWrappers

{-------------------------------------------------------------------------------
  Synonym for convenience
-------------------------------------------------------------------------------}

-- | Shelley as the single era in the hard fork combinator
type ShelleyBlockHFC proto era = HardForkBlock '[ShelleyBlock proto era]

{-------------------------------------------------------------------------------
  NoHardForks instance
-------------------------------------------------------------------------------}

instance ( ShelleyCompatible proto era
         , LedgerSupportsProtocol (ShelleyBlock proto era)
         , TxLimits               (ShelleyBlock proto era)
         ) => NoHardForks (ShelleyBlock proto era) where
  getEraParams =
        shelleyEraParamsNeverHardForks
      . shelleyLedgerGenesis
      . configLedger
  toPartialLedgerConfig _ cfg = ShelleyPartialLedgerConfig {
        shelleyLedgerConfig    = cfg
      , shelleyTriggerHardFork = TriggerHardForkNotDuringThisExecution
      }

{-------------------------------------------------------------------------------
  SupportedNetworkProtocolVersion instance
-------------------------------------------------------------------------------}

-- | Forward to the ShelleyBlock instance. Only supports
-- 'HardForkNodeToNodeDisabled', which is compatible with nodes running with
-- 'ShelleyBlock'.
instance ( ShelleyCompatible proto era
         , LedgerSupportsProtocol (ShelleyBlock proto era)
         , TxLimits               (ShelleyBlock proto era)
         ) => SupportedNetworkProtocolVersion (ShelleyBlockHFC proto era) where
  supportedNodeToNodeVersions _ =
      Map.map HardForkNodeToNodeDisabled $
      supportedNodeToNodeVersions (Proxy @(ShelleyBlock proto era))

  supportedNodeToClientVersions _ =
      Map.map HardForkNodeToClientDisabled $
      supportedNodeToClientVersions (Proxy @(ShelleyBlock proto era))

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

{-------------------------------------------------------------------------------
  SerialiseHFC instance
-------------------------------------------------------------------------------}

-- | Use the default implementations. This means the serialisation of blocks
-- includes an era wrapper. Each block should do this from the start to be
-- prepared for future hard forks without having to do any bit twiddling.
instance ( ShelleyCompatible proto era
         , LedgerSupportsProtocol (ShelleyBlock proto era)
         , TxLimits               (ShelleyBlock proto era)
         ) => SerialiseHFC '[ShelleyBlock proto era]
instance ( ShelleyCompatible proto era
         , LedgerSupportsProtocol (ShelleyBlock proto era)
         , TxLimits               (ShelleyBlock proto era)
         ) => SerialiseConstraintsHFC (ShelleyBlock proto era)

{-------------------------------------------------------------------------------
  Protocol type definition
-------------------------------------------------------------------------------}

type ProtocolShelley = HardForkProtocol '[ ShelleyBlock (TPraos StandardCrypto) StandardShelley ]

{-------------------------------------------------------------------------------
  SingleEraBlock Shelley
-------------------------------------------------------------------------------}

shelleyTransition ::
     forall era proto mk. ShelleyCompatible proto era
  => PartialLedgerConfig (ShelleyBlock proto era)
  -> Word16   -- ^ Next era's initial major protocol version
  -> LedgerState (ShelleyBlock proto era) mk
  -> Maybe EpochNo
shelleyTransition ShelleyPartialLedgerConfig{..}
                  transitionMajorVersionRaw
                  state =
      isTransition
    . Shelley.Inspect.pparamsUpdate
    $ state
  where
    ShelleyTransitionInfo{..} = shelleyLedgerTransition state

    -- 'shelleyLedgerConfig' contains a dummy 'EpochInfo' but this does not
    -- matter for extracting the genesis config
    genesis :: SL.ShelleyGenesis (EraCrypto era)
    genesis = shelleyLedgerGenesis shelleyLedgerConfig

    k :: Word64
    k = SL.sgSecurityParam genesis

    isTransition :: ShelleyLedgerUpdate era -> Maybe EpochNo
    isTransition (ShelleyUpdatedPParams maybePParams newPParamsEpochNo) = do
         SL.SJust pp <- Just maybePParams
         let protVer = pp ^. SL.ppProtocolVersionL
         transitionMajorVersion <- SL.mkVersion transitionMajorVersionRaw
         guard $ SL.pvMajor protVer == transitionMajorVersion
         guard $ shelleyAfterVoting >= fromIntegral k
         return newPParamsEpochNo

instance ( ShelleyCompatible proto era
         , LedgerSupportsProtocol (ShelleyBlock proto era)
         , TxLimits               (ShelleyBlock proto era)
         ) => SingleEraBlock (ShelleyBlock proto era) where
  singleEraTransition pcfg _eraParams _eraStart ledgerState =
      -- TODO: We might be evaluating 'singleEraTransition' more than once when
      -- replaying blocks. We should investigate if this is the case, and if so,
      -- whether this is the desired behaviour. If it is not, then we need to
      -- fix it.
      --
      -- For evidence of this behaviour, replace the cased-on expression by:
      -- > @traceShowId $ shelleyTriggerHardFork pcf@
      case shelleyTriggerHardFork pcfg of
        TriggerHardForkNotDuringThisExecution        -> Nothing
        TriggerHardForkAtEpoch   epoch               -> Just epoch
        TriggerHardForkAtVersion shelleyMajorVersion ->
            shelleyTransition
              pcfg
              shelleyMajorVersion
              ledgerState

  singleEraInfo _ = SingleEraInfo {
      singleEraName = T.pack (L.eraName @era)
    }

instance PraosCrypto c => HasPartialConsensusConfig (Praos c) where
  type PartialConsensusConfig (Praos c) = PraosParams

  completeConsensusConfig _ praosEpochInfo praosParams = PraosConfig {..}

  toPartialConsensusConfig _ = praosParams

instance SL.PraosCrypto c => HasPartialConsensusConfig (TPraos c) where
  type PartialConsensusConfig (TPraos c) = TPraosParams

  completeConsensusConfig _ tpraosEpochInfo tpraosParams = TPraosConfig {..}

  toPartialConsensusConfig _ = tpraosParams

data ShelleyPartialLedgerConfig era = ShelleyPartialLedgerConfig {
      -- | We cache the non-partial ledger config containing a dummy
      -- 'EpochInfo' that needs to be replaced with the correct one.
      --
      -- We do this to avoid recomputing the ledger config each time
      -- 'completeLedgerConfig' is called, as 'mkShelleyLedgerConfig' does
      -- some rather expensive computations that shouldn't be repeated too
      -- often (e.g., 'sgActiveSlotCoeff').
      shelleyLedgerConfig    :: !(ShelleyLedgerConfig era)
    , shelleyTriggerHardFork :: !TriggerHardFork
    }
  deriving (Generic)

deriving instance (NoThunks (SL.TranslationContext era), SL.Era era) =>
    NoThunks (ShelleyPartialLedgerConfig era)

instance ShelleyCompatible proto era => HasPartialLedgerConfig (ShelleyBlock proto era) where
  type PartialLedgerConfig (ShelleyBlock proto era) = ShelleyPartialLedgerConfig era

  -- Replace the dummy 'EpochInfo' with the real one
  completeLedgerConfig _ epochInfo (ShelleyPartialLedgerConfig cfg _) =
      cfg {
          shelleyLedgerGlobals = (shelleyLedgerGlobals cfg) {
              SL.epochInfo =
                  hoistEpochInfo
                    (runExcept . withExceptT (T.pack . show))
                    epochInfo
            }
        }

-- | Forecast from a Shelley-based era to the next Shelley-based era.
forecastAcrossShelley ::
     forall protoFrom protoTo eraFrom eraTo mk.
     ( TranslateProto protoFrom protoTo
     , LedgerSupportsProtocol (ShelleyBlock protoFrom eraFrom)
     )
  => ShelleyLedgerConfig eraFrom
  -> ShelleyLedgerConfig eraTo
  -> Bound  -- ^ Transition between the two eras
  -> SlotNo -- ^ Forecast for this slot
  -> LedgerState (ShelleyBlock protoFrom eraFrom) mk
  -> Except OutsideForecastRange (WrapLedgerView (ShelleyBlock protoTo eraTo))
forecastAcrossShelley cfgFrom cfgTo transition forecastFor ledgerStateFrom
    | forecastFor < maxFor
    = return $ futureLedgerView forecastFor
    | otherwise
    = throwError $ OutsideForecastRange {
          outsideForecastAt     = ledgerTipSlot ledgerStateFrom
        , outsideForecastMaxFor = maxFor
        , outsideForecastFor    = forecastFor
        }
  where
    -- | 'SL.futureLedgerView' imposes its own bounds. Those bounds could
    -- /exceed/ the 'maxFor' we have computed, but should never be /less/.
    futureLedgerView :: SlotNo -> WrapLedgerView (ShelleyBlock protoTo era)
    futureLedgerView =
          WrapLedgerView
        . either
            (\e -> error ("futureLedgerView failed: " <> show e))
            (translateLedgerView (Proxy @(protoFrom, protoTo)))
        . runExcept
        . Forecast.forecastFor (ledgerViewForecastAt cfgFrom ledgerStateFrom)

    -- Exclusive upper bound
    maxFor :: SlotNo
    maxFor = crossEraForecastBound
               (ledgerTipSlot ledgerStateFrom)
               (boundSlot transition)
               (SL.stabilityWindow (shelleyLedgerGlobals cfgFrom))
               (SL.stabilityWindow (shelleyLedgerGlobals cfgTo))

translateChainDepStateAcrossShelley ::
     forall eraFrom eraTo protoFrom protoTo.
     ( TranslateProto protoFrom protoTo
     )
  => RequiringBoth
       WrapConsensusConfig
       (Translate WrapChainDepState)
       (ShelleyBlock protoFrom eraFrom)
       (ShelleyBlock protoTo eraTo)
translateChainDepStateAcrossShelley =
    ignoringBoth $
      Translate $ \_epochNo (WrapChainDepState chainDepState) ->
        -- Same protocol, same 'ChainDepState'. Note that we don't have to apply
        -- any changes related to an epoch transition, this is already done when
        -- ticking the state.
        WrapChainDepState $ translateChainDepState (Proxy @(protoFrom, protoTo)) chainDepState

crossEraForecastAcrossShelley ::
     forall eraFrom eraTo protoFrom protoTo.
     ( TranslateProto protoFrom protoTo
     , LedgerSupportsProtocol (ShelleyBlock protoFrom eraFrom)
     )
  => RequiringBoth
       WrapLedgerConfig
       (CrossEraForecaster LedgerState WrapLedgerView)
       (ShelleyBlock protoFrom eraFrom)
       (ShelleyBlock protoTo eraTo)
crossEraForecastAcrossShelley =
    RequireBoth $ \(WrapLedgerConfig cfgFrom)
                   (WrapLedgerConfig cfgTo) ->
      CrossEraForecaster $ forecastAcrossShelley cfgFrom cfgTo

{-------------------------------------------------------------------------------
  Translation from one Shelley-based era to another Shelley-based era
-------------------------------------------------------------------------------}

instance ( ShelleyBasedEra era
         , ShelleyBasedEra (SL.PreviousEra era)
         , SL.Era (SL.PreviousEra era)
         , EraCrypto (SL.PreviousEra era) ~ EraCrypto era
         ) => SL.TranslateEra era (ShelleyTip proto) where
  translateEra _ (ShelleyTip sno bno (ShelleyHash hash)) =
      return $ ShelleyTip sno bno (ShelleyHash hash)

instance ( ShelleyBasedEra era
         , ShelleyBasedEra (SL.PreviousEra era)
         , SL.TranslateEra era (ShelleyTip proto)
         , SL.TranslateEra era SL.NewEpochState
         , SL.TranslationError era SL.NewEpochState ~ Void
         , EraCrypto (SL.PreviousEra era) ~ EraCrypto era
         , CanMapMK mk
         ) => SL.TranslateEra era (Flip LedgerState mk :.: ShelleyBlock proto) where
  translateEra ctxt (Comp (Flip (ShelleyLedgerState tip state _transition tables))) = do
      tip'   <- mapM (SL.translateEra ctxt) tip
      state' <- SL.translateEra ctxt state
      return $ Comp $ Flip $ ShelleyLedgerState {
          shelleyLedgerTip        = tip'
        , shelleyLedgerState      = state'
        , shelleyLedgerTransition = ShelleyTransitionInfo 0
        , shelleyLedgerTables     = translateShelleyTables tables
        }

translateShelleyTables ::
     ( EraCrypto (SL.PreviousEra era) ~ EraCrypto era
     , CanMapMK mk
     , ShelleyBasedEra era
     , ShelleyBasedEra (SL.PreviousEra era)
     )
  => LedgerTables (LedgerState (ShelleyBlock proto (SL.PreviousEra era))) mk
  -> LedgerTables (LedgerState (ShelleyBlock proto                 era))  mk
translateShelleyTables (LedgerTables utxoTable) =
      LedgerTables $ mapMK SL.upgradeTxOut utxoTable

instance ( ShelleyBasedEra era
         , SL.TranslateEra era WrapTx
         ) => SL.TranslateEra era (GenTx :.: ShelleyBlock proto) where
  type TranslationError era (GenTx :.: ShelleyBlock proto) = SL.TranslationError era WrapTx
  translateEra ctxt (Comp (ShelleyTx _txId tx)) =
        Comp . mkShelleyTx . unwrapTx @era
    <$> SL.translateEra ctxt (WrapTx @(SL.PreviousEra era) tx)

instance ( ShelleyBasedEra era
         , SL.TranslateEra era WrapTx
         ) => SL.TranslateEra era (WrapValidatedGenTx :.: ShelleyBlock proto) where
  type TranslationError era (WrapValidatedGenTx :.: ShelleyBlock proto) = SL.TranslationError era WrapTx
  translateEra ctxt (Comp (WrapValidatedGenTx (ShelleyValidatedTx _txId vtx))) =
        Comp . WrapValidatedGenTx
      . mkShelleyValidatedTx . SL.coerceValidated
    <$> SL.translateValidated @era @WrapTx ctxt (SL.coerceValidated vtx)

{-------------------------------------------------------------------------------
  Canonical TxIn
-------------------------------------------------------------------------------}

instance ShelleyBasedEra era
      => HasCanonicalTxIn '[ShelleyBlock proto era] where
  newtype instance CanonicalTxIn '[ShelleyBlock proto era] = ShelleyBlockHFCTxIn {
      getShelleyBlockHFCTxIn :: SL.TxIn (EraCrypto era)
    }
    deriving stock (Show, Eq, Ord)
    deriving newtype NoThunks

  injectCanonicalTxIn IZ txIn     = ShelleyBlockHFCTxIn txIn
  injectCanonicalTxIn (IS idx') _ = case idx' of {}

  distribCanonicalTxIn IZ txIn     = getShelleyBlockHFCTxIn txIn
  distribCanonicalTxIn (IS idx') _ = case idx' of {}

  encodeCanonicalTxIn (ShelleyBlockHFCTxIn txIn) = SL.toEraCBOR @era txIn

  decodeCanonicalTxIn = ShelleyBlockHFCTxIn <$> SL.fromEraCBOR @era

{-------------------------------------------------------------------------------
  HardForkTxOut
-------------------------------------------------------------------------------}

instance HasHardForkTxOut '[ShelleyBlock proto era] where
  type instance HardForkTxOut '[ShelleyBlock proto era] = SL.TxOut era
  injectHardForkTxOut IZ txOut    = txOut
  injectHardForkTxOut (IS idx') _ = case idx' of {}
  distribHardForkTxOut IZ txOut    = txOut
  distribHardForkTxOut (IS idx') _ = case idx' of {}

instance ShelleyBasedEra era => SerializeHardForkTxOut '[ShelleyBlock proto era] where
  encodeHardForkTxOut _ = SL.toEraCBOR @era
  decodeHardForkTxOut _ = SL.fromEraCBOR @era

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

instance ( ShelleyCompatible proto era
         , ShelleyBasedEra era
         , Key (LedgerState (ShelleyBlock proto era)) ~ SL.TxIn (EraCrypto era)
         , Value (LedgerState (ShelleyBlock proto era)) ~ SL.TxOut era
         , HasHardForkTxOut '[ShelleyBlock proto era]
         ) => BlockSupportsHFLedgerQuery '[ShelleyBlock proto era] where

  answerBlockQueryHFLookup IZ cfg q dlv   =
    answerShelleyLookupQueries IZ cfg q dlv
  answerBlockQueryHFLookup (IS idx) _ _ _ = case idx of  {}

  answerBlockQueryHFTraverse IZ cfg q dlv   =
    answerShelleyTraversingQueries IZ cfg q dlv
  answerBlockQueryHFTraverse (IS idx) _ _ _ = case idx of {}

  queryLedgerGetTraversingFilter idx@IZ       = \case
    GetUTxOByAddress addrs ->
      filterGetUTxOByAddressOne addrs
    GetUTxOWhole ->
      const True
    GetCBOR q' -> queryLedgerGetTraversingFilter idx q'
  queryLedgerGetTraversingFilter (IS idx) = case idx of {}
