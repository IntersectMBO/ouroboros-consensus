{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module is the Shelley Hard Fork Combinator
module Ouroboros.Consensus.Shelley.ShelleyHFC (
    ProtocolShelley
  , ShelleyBlockHFC
  , ShelleyPartialLedgerConfig (..)
  , crossEraForecastAcrossShelley
  , forecastAcrossShelley
  , tickChainDepStateAcrossShelley
  , tickLedgerStateAcrossShelley
  , translateShelleyLedgerState
  ) where

import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Ledger.BaseTypes as SL (mkVersion)
import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Protocol.TPraos.API as SL
import           Cardano.Slotting.EpochInfo (hoistEpochInfo)
import           Control.Monad (guard)
import           Control.Monad.Except (runExcept, throwError, withExceptT)
import           Data.Coerce
import qualified Data.Map.Strict as Map
import           Data.SOP.BasicFunctors
import           Data.SOP.InPairs (RequiringBoth, RequiringBoth' (..))
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
import           Ouroboros.Consensus.HardFork.Combinator
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
import           Ouroboros.Consensus.Shelley.Protocol.Abstract
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
     forall era proto. ShelleyCompatible proto era
  => PartialLedgerConfig (ShelleyBlock proto era)
  -> Word16   -- ^ Next era's initial major protocol version
  -> LedgerState (ShelleyBlock proto era)
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

-- | Wrapper around 'forecastAcrossShelley'.
crossEraForecastAcrossShelley ::
     forall protoFrom protoTo eraFrom eraTo.
     ( TranslateProto protoFrom protoTo
     , LedgerSupportsProtocol (ShelleyBlock protoFrom eraFrom)
     )
  => RequiringBoth
       WrapLedgerConfig
       (CrossEraForecaster LedgerState WrapLedgerView)
       (ShelleyBlock protoFrom eraFrom)
       (ShelleyBlock protoTo eraTo)
crossEraForecastAcrossShelley = coerce forecastAcrossShelley

-- | Forecast from a Shelley-based era to the next Shelley-based era. We do so
-- via the "forecast-then-translate" scheme:
--
--  - First, we forecast for the given slot using the logic of @'ShelleyBlock'
--    protoFrom eraFrom@.
--
--  - Then, we translate the resulting 'LedgerView' from @protoFrom@ to
--    @protoTo@.
forecastAcrossShelley ::
     forall protoFrom protoTo eraFrom eraTo.
     ( TranslateProto protoFrom protoTo
     , LedgerSupportsProtocol (ShelleyBlock protoFrom eraFrom)
     )
  => ShelleyLedgerConfig eraFrom
  -> ShelleyLedgerConfig eraTo
  -> Bound  -- ^ Transition between the two eras
  -> SlotNo -- ^ Forecast for this slot
  -> LedgerState (ShelleyBlock protoFrom eraFrom)
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

-- | Tick the ledger state from one Shelley-based era @eraFrom@ to the the next
-- era @eraTo@. We do so via the "translate-then-tick" scheme:
--
--  - First, we translate the ledger state from @eraFrom@ to @eraTo@.
--
--  - Then, we tick the ledger state to the target slot using the logic of
--    @eraTo@.
--
-- Note that this function also allows to change the protocol; this is harmless
-- as the ledger state only depends trivially on the protocol via the
-- @HeaderHash@ contained in the tip.
tickLedgerStateAcrossShelley ::
     forall protoFrom protoTo eraFrom eraTo.
     ( ShelleyBasedEra eraTo
     , eraFrom ~ SL.PreviousEra eraTo
     , SL.TranslateEra eraTo SL.NewEpochState
     , SL.TranslationError eraTo SL.NewEpochState ~ Void
     , ProtoCrypto protoFrom ~ ProtoCrypto protoTo
     )
  => RequiringBoth
       WrapLedgerConfig
       CrossEraTickLedgerState
       (ShelleyBlock protoFrom eraFrom)
       (ShelleyBlock protoTo   eraTo)
tickLedgerStateAcrossShelley =
    RequireBoth $ \_cfgFrom (WrapLedgerConfig cfgTo) ->
      CrossEraTickLedgerState $ \_bound slot ->
          applyChainTickLedgerResult cfgTo slot
        . translateShelleyLedgerState
            (shelleyLedgerTranslationContext cfgTo)

-- | Tick the chain-dependent state from one Shelley-based era to the the next,
-- potentially changing the protocol. We do so via the "translate-then-tick"
-- scheme:
--
--  - First, we translate the chain-dependent state from @protoFrom@ to
--    @protoTo@.
--
--  - Then, we tick the chain-dependent state to the target slot using the logic
--    of @protoTo@.
--
-- Note that this function also allows to change the ledger era; this is
-- harmless as the chain-dependent state doesn't depend on it at all.
tickChainDepStateAcrossShelley ::
     forall protoFrom protoTo eraFrom eraTo.
     ( TranslateProto protoFrom protoTo
     , ConsensusProtocol protoTo
     )
  => RequiringBoth
       WrapConsensusConfig
       CrossEraTickChainDepState
       (ShelleyBlock protoFrom eraFrom)
       (ShelleyBlock protoTo eraTo)
tickChainDepStateAcrossShelley =
    RequireBoth $ \_cfgFrom (WrapConsensusConfig cfgTo) ->
      CrossEraTickChainDepState $ \_bound view slot ->
          tickChainDepState cfgTo view slot
        . translateChainDepState (Proxy @(protoFrom, protoTo))

{-------------------------------------------------------------------------------
  Translation from one Shelley-based era to another Shelley-based era
-------------------------------------------------------------------------------}

translateShelleyLedgerState ::
     ( eraFrom ~ SL.PreviousEra eraTo
     , SL.TranslateEra eraTo SL.NewEpochState
     , SL.TranslationError eraTo SL.NewEpochState ~ Void
     , ProtoCrypto protoFrom ~ ProtoCrypto protoTo
     )
  => SL.TranslationContext eraTo
  -> LedgerState (ShelleyBlock protoFrom eraFrom)
  -> LedgerState (ShelleyBlock protoTo   eraTo  )
translateShelleyLedgerState ctx (ShelleyLedgerState wo nes st) =
    ShelleyLedgerState {
        shelleyLedgerTip        = fmap castShelleyTip wo
      , shelleyLedgerState      = SL.translateEra' ctx nes
      , shelleyLedgerTransition = st
      }

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
