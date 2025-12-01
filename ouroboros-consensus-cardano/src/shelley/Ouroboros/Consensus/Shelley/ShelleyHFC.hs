{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module is the Shelley Hard Fork Combinator
module Ouroboros.Consensus.Shelley.ShelleyHFC
  ( ProtocolShelley
  , ShelleyBlockHFC
  , ShelleyPartialLedgerConfig (..)
  , crossEraForecastAcrossShelley
  , forecastAcrossShelley
  , translateChainDepStateAcrossShelley
  ) where

import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Ledger.BaseTypes as SL (mkVersion, unNonZero)
-- import Cardano.Ledger.Binary.Decoding
--   ( decShareCBOR
--   , decodeMap
--   , decodeMemPack
--   , internsFromMap
--   )
-- import Cardano.Ledger.Binary.Encoding
--   ( encodeMap
--   , encodeMemPack
--   , toPlainEncoding
--   )
-- import qualified Cardano.Ledger.Conway.State as SL
import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Shelley.API as SL
-- import qualified Cardano.Ledger.Shelley.LedgerState as SL
import Cardano.Protocol.Crypto (Crypto)
import qualified Cardano.Protocol.TPraos.API as SL
-- import Codec.CBOR.Decoding
-- import Codec.CBOR.Encoding
import Control.Monad (guard)
import Control.Monad.Except (runExcept, throwError)
import Data.Coerce
import qualified Data.Map.Strict as Map
import Data.MemPack
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Functors (Flip (..))
import Data.SOP.InPairs (RequiringBoth (..), ignoringBoth)
-- import Data.SOP.Index (Index (..))
-- import Data.SOP.Strict
-- import qualified Data.SOP.Tails as Tails
-- import qualified Data.SOP.Telescope as Telescope
import Data.Singletons
import qualified Data.Text as T (pack)
-- import Data.Typeable
import Data.Void (Void)
import Data.Word
import Lens.Micro ((^.))
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Forecast
import qualified Ouroboros.Consensus.Forecast as Forecast
import Ouroboros.Consensus.HardFork.Combinator hiding
  ( translateChainDepState
  )
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import Ouroboros.Consensus.HardFork.History (Bound (boundSlot))
import Ouroboros.Consensus.HardFork.Simple
import Ouroboros.Consensus.Ledger.Abstract
-- import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsMempool (TxLimits)
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol
  , ledgerViewForecastAt
  )
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.Praos
import Ouroboros.Consensus.Protocol.TPraos
import Ouroboros.Consensus.Shelley.Eras
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Ledger.Inspect as Shelley.Inspect
import Ouroboros.Consensus.Shelley.Node ()
import Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto)
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util.IndexedMemPack
import Ouroboros.Consensus.Util.TypeLevel

{-------------------------------------------------------------------------------
  Synonym for convenience
-------------------------------------------------------------------------------}

-- | Shelley as the single era in the hard fork combinator
type ShelleyBlockHFC proto era = HardForkBlock '[ShelleyBlock proto era]

{-------------------------------------------------------------------------------
  NoHardForks instance
-------------------------------------------------------------------------------}

instance
  ( ShelleyCompatible proto era
  , LedgerSupportsProtocol (ShelleyBlock proto era)
  ) =>
  ImmutableEraParams (ShelleyBlock proto era)
  where
  immutableEraParams =
    shelleyEraParamsNeverHardForks
      . shelleyLedgerGenesis
      . configLedger

instance
  ( ShelleyCompatible proto era
  , LedgerSupportsProtocol (ShelleyBlock proto era)
  , TxLimits (ShelleyBlock proto era)
  , GetBlockKeySets (ShelleyBlock proto era)
  , Crypto (ProtoCrypto proto)
  , All SingI (TablesForBlock (ShelleyBlock proto era))
  , ToAllDict (TableConstraints (ShelleyBlock proto era)) (TablesForBlock (ShelleyBlock proto era))
  ) =>
  NoHardForks (ShelleyBlock proto era)
  where
  toPartialLedgerConfig _ cfg =
    ShelleyPartialLedgerConfig
      { shelleyLedgerConfig = cfg
      , shelleyTriggerHardFork = TriggerHardForkNotDuringThisExecution
      }

{-------------------------------------------------------------------------------
  SupportedNetworkProtocolVersion instance
-------------------------------------------------------------------------------}

-- | Forward to the ShelleyBlock instance. Only supports
-- 'HardForkNodeToNodeDisabled', which is compatible with nodes running with
-- 'ShelleyBlock'.
instance
  ( ShelleyCompatible proto era
  , LedgerSupportsProtocol (ShelleyBlock proto era)
  , TxLimits (ShelleyBlock proto era)
  , Crypto (ProtoCrypto proto)
  ) =>
  SupportedNetworkProtocolVersion (ShelleyBlockHFC proto era)
  where
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
instance
  ( ShelleyCompatible proto era
  , GetBlockKeySets (ShelleyBlock proto era)
  , LedgerSupportsProtocol (ShelleyBlock proto era)
  , TxLimits (ShelleyBlock proto era)
  , Crypto (ProtoCrypto proto)
  , All SingI (TablesForBlock (ShelleyBlock proto era))
  , ToAllDict (TableConstraints (ShelleyBlock proto era)) (TablesForBlock (ShelleyBlock proto era))
  ) =>
  SerialiseHFC '[ShelleyBlock proto era]

instance
  ( ShelleyCompatible proto era
  , GetBlockKeySets (ShelleyBlock proto era)
  , LedgerSupportsProtocol (ShelleyBlock proto era)
  , TxLimits (ShelleyBlock proto era)
  , Crypto (ProtoCrypto proto)
  , All SingI (TablesForBlock (ShelleyBlock proto era))
  , ToAllDict (TableConstraints (ShelleyBlock proto era)) (TablesForBlock (ShelleyBlock proto era))
  ) =>
  SerialiseConstraintsHFC (ShelleyBlock proto era)

{-------------------------------------------------------------------------------
  Protocol type definition
-------------------------------------------------------------------------------}

type ProtocolShelley = HardForkProtocol '[ShelleyBlock (TPraos StandardCrypto) ShelleyEra]

{-------------------------------------------------------------------------------
  SingleEraBlock Shelley
-------------------------------------------------------------------------------}

shelleyTransition ::
  forall era proto mk.
  ShelleyCompatible proto era =>
  PartialLedgerConfig (ShelleyBlock proto era) ->
  -- | Next era's initial major protocol version
  Word16 ->
  LedgerState (ShelleyBlock proto era) mk ->
  Maybe EpochNo
shelleyTransition
  ShelleyPartialLedgerConfig{..}
  transitionMajorVersionRaw
  state =
    isTransition
      . Shelley.Inspect.pparamsUpdate
      $ state
   where
    ShelleyTransitionInfo{..} = shelleyLedgerTransition state

    -- 'shelleyLedgerConfig' contains a dummy 'EpochInfo' but this does not
    -- matter for extracting the genesis config
    genesis :: SL.ShelleyGenesis
    genesis = shelleyLedgerGenesis shelleyLedgerConfig

    k :: Word64
    k = SL.unNonZero $ SL.sgSecurityParam genesis

    isTransition :: ShelleyLedgerUpdate era -> Maybe EpochNo
    isTransition (ShelleyUpdatedPParams maybePParams newPParamsEpochNo) = do
      SL.SJust pp <- Just maybePParams
      let protVer = pp ^. SL.ppProtocolVersionL
      transitionMajorVersion <- SL.mkVersion transitionMajorVersionRaw
      guard $ SL.pvMajor protVer == transitionMajorVersion
      guard $ shelleyAfterVoting >= fromIntegral k
      return newPParamsEpochNo

instance
  ( ShelleyCompatible proto era
  , LedgerSupportsProtocol (ShelleyBlock proto era)
  , All SingI (TablesForBlock (ShelleyBlock proto era))
  , TxLimits (ShelleyBlock proto era)
  , GetBlockKeySets (ShelleyBlock proto era)
  , Crypto (ProtoCrypto proto)
  , All SingI (TablesForBlock (ShelleyBlock proto era))
  , ToAllDict (TableConstraints (ShelleyBlock proto era)) (TablesForBlock (ShelleyBlock proto era))
  ) =>
  SingleEraBlock (ShelleyBlock proto era)
  where
  singleEraTransition pcfg _eraParams _eraStart ledgerState =
    -- TODO: We might be evaluating 'singleEraTransition' more than once when
    -- replaying blocks. We should investigate if this is the case, and if so,
    -- whether this is the desired behaviour. If it is not, then we need to
    -- fix it.
    --
    -- For evidence of this behaviour, replace the cased-on expression by:
    -- > @traceShowId $ shelleyTriggerHardFork pcf@
    case shelleyTriggerHardFork pcfg of
      TriggerHardForkNotDuringThisExecution -> Nothing
      TriggerHardForkAtEpoch epoch -> Just epoch
      TriggerHardForkAtVersion shelleyMajorVersion ->
        shelleyTransition
          pcfg
          shelleyMajorVersion
          ledgerState

  singleEraInfo _ =
    SingleEraInfo
      { singleEraName = T.pack (L.eraName @era)
      }

instance Ouroboros.Consensus.Protocol.Praos.PraosCrypto c => HasPartialConsensusConfig (Praos c) where
  type PartialConsensusConfig (Praos c) = PraosParams

  completeConsensusConfig _ praosEpochInfo praosParams = PraosConfig{..}

  toPartialConsensusConfig _ = praosParams

instance SL.PraosCrypto c => HasPartialConsensusConfig (TPraos c) where
  type PartialConsensusConfig (TPraos c) = TPraosParams

  completeConsensusConfig _ tpraosEpochInfo tpraosParams = TPraosConfig{..}

  toPartialConsensusConfig _ = tpraosParams

translateChainDepStateAcrossShelley ::
  forall eraFrom eraTo protoFrom protoTo.
  TranslateProto protoFrom protoTo =>
  RequiringBoth
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
  ) =>
  RequiringBoth
    WrapLedgerConfig
    (CrossEraForecaster LedgerState WrapLedgerView)
    (ShelleyBlock protoFrom eraFrom)
    (ShelleyBlock protoTo eraTo)
crossEraForecastAcrossShelley = coerce forecastAcrossShelley

-- | Forecast from a Shelley-based era to the next Shelley-based era.
forecastAcrossShelley ::
  forall protoFrom protoTo eraFrom eraTo mk.
  ( TranslateProto protoFrom protoTo
  , LedgerSupportsProtocol (ShelleyBlock protoFrom eraFrom)
  ) =>
  ShelleyLedgerConfig eraFrom ->
  ShelleyLedgerConfig eraTo ->
  -- | Transition between the two eras
  Bound ->
  -- | Forecast for this slot
  SlotNo ->
  LedgerState (ShelleyBlock protoFrom eraFrom) mk ->
  Except OutsideForecastRange (WrapLedgerView (ShelleyBlock protoTo eraTo))
forecastAcrossShelley cfgFrom cfgTo transition forecastFor ledgerStateFrom
  | forecastFor < maxFor =
      return $ futureLedgerView forecastFor
  | otherwise =
      throwError $
        OutsideForecastRange
          { outsideForecastAt = ledgerTipSlot ledgerStateFrom
          , outsideForecastMaxFor = maxFor
          , outsideForecastFor = forecastFor
          }
 where
  -- \| 'SL.futureLedgerView' imposes its own bounds. Those bounds could
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
  maxFor =
    crossEraForecastBound
      (ledgerTipSlot ledgerStateFrom)
      (boundSlot transition)
      (SL.stabilityWindow (shelleyLedgerGlobals cfgFrom))
      (SL.stabilityWindow (shelleyLedgerGlobals cfgTo))

{-------------------------------------------------------------------------------
  Translation from one Shelley-based era to another Shelley-based era
-------------------------------------------------------------------------------}

instance
  ( ShelleyBasedEra era
  , ShelleyBasedEra (SL.PreviousEra era)
  , SL.Era (SL.PreviousEra era)
  ) =>
  SL.TranslateEra era (ShelleyTip proto)
  where
  translateEra _ (ShelleyTip sno bno (ShelleyHash hash)) =
    return $ ShelleyTip sno bno (ShelleyHash hash)

instance
  ( ShelleyBasedEra era
  , ShelleyBasedEra (SL.PreviousEra era)
  , SL.TranslateEra era (ShelleyTip proto)
  , SL.TranslateEra era SL.NewEpochState
  , SL.TranslationError era SL.NewEpochState ~ Void
  , SingI (TablesForBlock (ShelleyBlock proto era))
  , All (TableConstraints (ShelleyBlock proto era)) (TablesForBlock (ShelleyBlock proto era))
  ) =>
  SL.TranslateEra era (Flip LedgerState EmptyMK :.: ShelleyBlock proto)
  where
  translateEra ctxt (Comp (Flip (ShelleyLedgerState tip state _transition _tables))) = do
    tip' <- mapM (SL.translateEra ctxt) tip
    state' <- SL.translateEra ctxt state
    return $
      Comp $
        Flip $
          ShelleyLedgerState
            { shelleyLedgerTip = tip'
            , shelleyLedgerState = state'
            , shelleyLedgerTransition = ShelleyTransitionInfo 0
            , shelleyLedgerTables = emptyLedgerTables
            }

-- translateShelleyTables ::
--   ( CanMapMK mk
--   , ShelleyBasedEra era
--   , ShelleyBasedEra (SL.PreviousEra era)
--   ) =>
--   LedgerTables (ShelleyBlock proto (SL.PreviousEra era)) mk ->
--   LedgerTables (ShelleyBlock proto era) mk
-- translateShelleyTables (LedgerTables utxoTable) =
--   tbs & crossEraForecastAcrossShelley $ mapMK SL.upgradeTxOut utxoTable

instance
  ( ShelleyBasedEra era
  , SL.TranslateEra era SL.Tx
  ) =>
  SL.TranslateEra era (GenTx :.: ShelleyBlock proto)
  where
  type TranslationError era (GenTx :.: ShelleyBlock proto) = SL.TranslationError era SL.Tx
  translateEra ctxt (Comp (ShelleyTx _txId tx)) =
    Comp . mkShelleyTx
      <$> SL.translateEra ctxt tx

instance
  ( ShelleyBasedEra era
  , SL.TranslateEra era SL.Tx
  ) =>
  SL.TranslateEra era (WrapValidatedGenTx :.: ShelleyBlock proto)
  where
  type
    TranslationError era (WrapValidatedGenTx :.: ShelleyBlock proto) =
      SL.TranslationError era SL.Tx
  translateEra ctxt (Comp (WrapValidatedGenTx (ShelleyValidatedTx _txId vtx))) =
    Comp
      . WrapValidatedGenTx
      . mkShelleyValidatedTx
      . SL.coerceValidated
      <$> SL.translateValidated @era @SL.Tx ctxt (SL.coerceValidated vtx)

{-------------------------------------------------------------------------------
  HardForkTxOut
-------------------------------------------------------------------------------}

-- instance ShelleyCompatible proto era => HasHardForkTxOut '[ShelleyBlock proto era] where
--   type HardForkTxOut '[ShelleyBlock proto era] = SL.TxOut era
--   injectHardForkTxOut IZ txOut = txOut
--   injectHardForkTxOut (IS idx') _ = case idx' of {}
--   ejectHardForkTxOut IZ txOut = txOut
--   ejectHardForkTxOut (IS idx') _ = case idx' of {}

--   --  txOutEjections = fn (unZ . unK) :* Nil
--   txOutTranslations = Tails.mk1

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- instance
--   ( ShelleyCompatible proto era
--   , ShelleyBasedEra era
--   , TxOut (ShelleyBlock proto era) ~ SL.TxOut era
--   , HasHardForkTxOut '[ShelleyBlock proto era]
--   ) =>
--   BlockSupportsHFLedgerQuery '[ShelleyBlock proto era]
--   where
--   answerBlockQueryHFLookup = \case
--     IZ -> answerShelleyLookupQueries (injectLedgerTables IZ) id
--     IS idx -> case idx of {}

--   answerBlockQueryHFTraverse = \case
--     IZ ->
--       answerShelleyTraversingQueries
--         id
--         (queryLedgerGetTraversingFilter @('[ShelleyBlock proto era]) IZ)
--     IS idx -> case idx of {}

--   queryLedgerGetTraversingFilter = \case
--     IZ -> shelleyQFTraverseTablesPredicate
--     IS idx -> case idx of {}

-- instance
--   MemPack (Value table (HardForkBlock '[ShelleyBlock proto era])) =>
--   IndexedMemPack LedgerState (HardForkBlock '[ShelleyBlock proto era]) (table :: TABLE)
--   where
--   type IndexedValue LedgerState table (HardForkBlock '[ShelleyBlock proto era]) = Value table (HardForkBlock '[ShelleyBlock proto era])
--   indexedTypeName _ = typeName @txout
--   indexedPackedByteCount _ = packedByteCount
--   indexedPackM _ = packM
--   indexedUnpackM _ = unpackM

-- instance
--   ShelleyCompatible proto era =>
--   SerializeTablesWithHint LedgerState (HardForkBlock '[ShelleyBlock proto era]) UTxOTable
--   where
--   encodeTablesWithHint ::
--     LedgerState (HardForkBlock '[ShelleyBlock proto era]) EmptyMK ->
--     Table ValuesMK (HardForkBlock '[ShelleyBlock proto era]) UTxOTable ->
--     Encoding
--   encodeTablesWithHint (HardForkLedgerState (HardForkState idx)) _un -- (LedgerTables (ValuesMK tbs))
--     =
--     undefined

--   --  let
--   --    np = (Fn $ const $ K encOne) :* Nil
--   --   in
--   --    hcollapse $ hap np $ Telescope.tip idx
--   -- where
--   --  encOne :: Encoding
--   --  encOne = toPlainEncoding (SL.eraProtVerLow @era) $ encodeMap encodeMemPack encodeMemPack tbs

--   decodeTablesWithHint ::
--     forall s.
--     LedgerState (HardForkBlock '[ShelleyBlock proto era]) EmptyMK ->
--     Decoder s (Table ValuesMK (HardForkBlock '[ShelleyBlock proto era]) UTxOTable)
--   decodeTablesWithHint (HardForkLedgerState (HardForkState idx)) =
--     let
--       np = (Fn $ Comp . fmap K . getOne . unFlip . currentState) :* Nil
--      in
--       hcollapse <$> (hsequence' $ hap np $ Telescope.tip idx)
--    where
--     getOne ::
--       LedgerState (ShelleyBlock proto era) EmptyMK ->
--       Decoder s (Table ValuesMK (HardForkBlock '[ShelleyBlock proto era]) UTxOTable)
--     getOne st =
--       let certInterns =
--             internsFromMap $
--               shelleyLedgerState st
--                 ^. SL.nesEsL
--                   . SL.esLStateL
--                   . SL.lsCertStateL
--                   . SL.certDStateL
--                   . SL.accountsL
--                   . SL.accountsMapL
--        in Table . ValuesMK <$> SL.eraDecoder @era (decodeMap decodeMemPack (decShareCBOR certInterns))
