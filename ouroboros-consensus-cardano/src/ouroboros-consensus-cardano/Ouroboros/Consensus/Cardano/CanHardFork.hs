{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Cardano.CanHardFork (
    CardanoHardForkConstraints
  , TriggerHardFork (..)
    -- * Re-exports of Shelley code
  , ShelleyPartialLedgerConfig (..)
  , crossEraForecastAcrossShelley
  , translateChainDepStateAcrossShelley
  ) where

import           Cardano.Crypto.DSIGN (Ed25519DSIGN)
import           Cardano.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)
import qualified Cardano.Ledger.Core as SL
import           Cardano.Ledger.Crypto (ADDRHASH, Crypto, DSIGN, HASH)
import qualified Cardano.Ledger.Genesis as SL
import           Cardano.Ledger.Hashes (EraIndependentTxBody)
import           Cardano.Ledger.Keys (DSignable, Hash)
import qualified Cardano.Ledger.Shelley.API as SL
import           Cardano.Ledger.Shelley.Translation
                     (toFromByronTranslationContext)
import qualified Cardano.Protocol.TPraos.API as SL
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as SL
import qualified Cardano.Protocol.TPraos.Rules.Tickn as SL
import           Control.Monad.Except (runExcept, throwError)
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.SOP.BasicFunctors
import           Data.SOP.InPairs (RequiringBoth (..), ignoringBoth)
import qualified Data.SOP.Strict as SOP
import           Data.SOP.Tails (Tails (..))
import qualified Data.SOP.Tails as Tails
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Byron.ByronHFC ()
import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node ()
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.History (Bound (boundSlot),
                     addSlots)
import           Ouroboros.Consensus.HardFork.Simple
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32,
                     IgnoringOverflow, TxMeasure)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Protocol.Abstract (SelectView,
                     TranslateProto)
import           Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import qualified Ouroboros.Consensus.Protocol.PBFT.State as PBftState
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import           Ouroboros.Consensus.Protocol.TPraos
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node ()
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()
import           Ouroboros.Consensus.Shelley.ShelleyHFC
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (eitherToMaybe)
import           Ouroboros.Consensus.Util.RedundantConstraints

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

type CardanoHardForkConstraints c =
  ( TPraos.PraosCrypto c
  , Praos.PraosCrypto c
  , TranslateProto (TPraos c) (Praos c)
  , ShelleyCompatible (TPraos c) (ShelleyEra c)
  , LedgerSupportsProtocol (ShelleyBlock (TPraos c) (ShelleyEra c))
  , ShelleyCompatible (TPraos c) (AllegraEra c)
  , LedgerSupportsProtocol (ShelleyBlock (TPraos c) (AllegraEra c))
  , ShelleyCompatible (TPraos c) (MaryEra    c)
  , LedgerSupportsProtocol (ShelleyBlock (TPraos c) (MaryEra c))
  , ShelleyCompatible (TPraos c) (AlonzoEra  c)
  , LedgerSupportsProtocol (ShelleyBlock (TPraos c) (AlonzoEra c))
  , ShelleyCompatible (Praos c) (BabbageEra  c)
  , LedgerSupportsProtocol (ShelleyBlock (Praos c) (BabbageEra c))
  , ShelleyCompatible (Praos c) (ConwayEra  c)
  , LedgerSupportsProtocol (ShelleyBlock (Praos c) (ConwayEra c))
    -- These equalities allow the transition from Byron to Shelley, since
    -- @cardano-ledger-shelley@ requires Ed25519 for Byron bootstrap addresses and
    -- the current Byron-to-Shelley translation requires a 224-bit hash for
    -- address and a 256-bit hash for header hashes.
  , HASH     c ~ Blake2b_256
  , ADDRHASH c ~ Blake2b_224
  , DSIGN    c ~ Ed25519DSIGN
  )

instance CardanoHardForkConstraints c => CanHardFork (CardanoEras c) where
  type HardForkTxMeasure (CardanoEras c) = ConwayMeasure

  hardForkEraTranslation = EraTranslation {
      translateLedgerState   =
          PCons translateLedgerStateByronToShelleyWrapper
        $ PCons translateLedgerStateShelleyToAllegraWrapper
        $ PCons translateLedgerStateAllegraToMaryWrapper
        $ PCons translateLedgerStateMaryToAlonzoWrapper
        $ PCons translateLedgerStateAlonzoToBabbageWrapper
        $ PCons translateLedgerStateBabbageToConwayWrapper
        $ PNil
    , translateChainDepState =
          PCons translateChainDepStateByronToShelleyWrapper
        $ PCons translateChainDepStateAcrossShelley
        $ PCons translateChainDepStateAcrossShelley
        $ PCons translateChainDepStateAcrossShelley
        $ PCons translateChainDepStateAcrossShelley
        $ PCons translateChainDepStateAcrossShelley
        $ PNil
    , crossEraForecast       =
          PCons crossEraForecastByronToShelleyWrapper
        $ PCons crossEraForecastAcrossShelley
        $ PCons crossEraForecastAcrossShelley
        $ PCons crossEraForecastAcrossShelley
        $ PCons crossEraForecastAcrossShelley
        $ PCons crossEraForecastAcrossShelley
        $ PNil
    }
  hardForkChainSel =
        -- Byron <-> Shelley, ...
        TCons (SOP.hpure CompareBlockNo)
        -- Inter-Shelley-based
      $ Tails.hcpure (Proxy @(HasPraosSelectView c)) CompareSameSelectView
  hardForkInjectTxs =
        PCons (ignoringBoth $ Pair2 cannotInjectTx cannotInjectValidatedTx)
      $ PCons (   ignoringBoth
                $ Pair2
                    translateTxShelleyToAllegraWrapper
                    translateValidatedTxShelleyToAllegraWrapper
              )
      $ PCons (   ignoringBoth
                $ Pair2
                    translateTxAllegraToMaryWrapper
                    translateValidatedTxAllegraToMaryWrapper
              )
      $ PCons (RequireBoth $ \_cfgMary cfgAlonzo ->
                let ctxt = getAlonzoTranslationContext cfgAlonzo
                in
                Pair2
                  (translateTxMaryToAlonzoWrapper          ctxt)
                  (translateValidatedTxMaryToAlonzoWrapper ctxt)
              )
      $ PCons (RequireBoth $ \_cfgAlonzo _cfgBabbage ->
                let ctxt = SL.NoGenesis
                in
                Pair2
                  (translateTxAlonzoToBabbageWrapper          ctxt)
                  (translateValidatedTxAlonzoToBabbageWrapper ctxt)
              )
      $ PCons (RequireBoth $ \_cfgBabbage cfgConway ->
                let ctxt = getConwayTranslationContext cfgConway
                in
                Pair2
                  (translateTxBabbageToConwayWrapper          ctxt)
                  (translateValidatedTxBabbageToConwayWrapper ctxt)
              )
      $ PNil

  hardForkInjTxMeasure =
    fromByteSize `o`
    fromByteSize `o`
    fromByteSize `o`
    fromByteSize `o`
    fromAlonzo   `o`
    fromConway   `o`
    fromConway   `o`
    nil
    where
      nil :: SOP.NS f '[] -> a
      nil = \case {}

      infixr `o`
      o ::
           (TxMeasure x -> a)
        -> (SOP.NS WrapTxMeasure xs -> a)
        -> SOP.NS WrapTxMeasure (x : xs)
        -> a
      o f g = \case
        SOP.Z (WrapTxMeasure x) -> f x
        SOP.S y                 -> g y

      fromByteSize :: IgnoringOverflow ByteSize32 -> ConwayMeasure
      fromByteSize x = fromAlonzo $ AlonzoMeasure x mempty
      fromAlonzo   x = fromConway $ ConwayMeasure x mempty
      fromConway   x = x

class    (SelectView (BlockProtocol blk) ~ PraosChainSelectView c) => HasPraosSelectView c blk
instance (SelectView (BlockProtocol blk) ~ PraosChainSelectView c) => HasPraosSelectView c blk

{-------------------------------------------------------------------------------
  Translation from Byron to Shelley
-------------------------------------------------------------------------------}

translateHeaderHashByronToShelley ::
     forall c.
     ( ShelleyCompatible (TPraos c) (ShelleyEra c)
     , HASH c ~ Blake2b_256
     )
  => HeaderHash ByronBlock
  -> HeaderHash (ShelleyBlock (TPraos c) (ShelleyEra c))
translateHeaderHashByronToShelley =
      fromShortRawHash (Proxy @(ShelleyBlock (TPraos c) (ShelleyEra c)))
    . toShortRawHash   (Proxy @ByronBlock)
  where
    -- Byron uses 'Blake2b_256' for header hashes
    _ = keepRedundantConstraint (Proxy @(HASH c ~ Blake2b_256))

translatePointByronToShelley ::
     ( ShelleyCompatible (TPraos c) (ShelleyEra c)
     , HASH c ~ Blake2b_256
     )
  => Point ByronBlock
  -> WithOrigin BlockNo
  -> WithOrigin (ShelleyTip (TPraos c) (ShelleyEra c))
translatePointByronToShelley point bNo =
    case (point, bNo) of
      (GenesisPoint, Origin) ->
        Origin
      (BlockPoint s h, NotOrigin n) -> NotOrigin ShelleyTip {
          shelleyTipSlotNo  = s
        , shelleyTipBlockNo = n
        , shelleyTipHash    = translateHeaderHashByronToShelley h
        }
      _otherwise ->
        error "translatePointByronToShelley: invalid Byron state"

translateLedgerStateByronToShelleyWrapper ::
     ( ShelleyCompatible (TPraos c) (ShelleyEra c)
     , HASH     c ~ Blake2b_256
     , ADDRHASH c ~ Blake2b_224
     )
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       ByronBlock
       (ShelleyBlock (TPraos c) (ShelleyEra c))
translateLedgerStateByronToShelleyWrapper =
    RequireBoth $ \_ (WrapLedgerConfig cfgShelley) ->
    Translate   $ \epochNo ledgerByron ->
      ShelleyLedgerState {
        shelleyLedgerTip =
          translatePointByronToShelley
            (ledgerTipPoint ledgerByron)
            (byronLedgerTipBlockNo ledgerByron)
      , shelleyLedgerState =
          SL.translateToShelleyLedgerState
            (toFromByronTranslationContext (shelleyLedgerGenesis cfgShelley))
            epochNo
            (byronLedgerState ledgerByron)
      , shelleyLedgerTransition =
          ShelleyTransitionInfo{shelleyAfterVoting = 0}
      }

translateChainDepStateByronToShelleyWrapper ::
     RequiringBoth
       WrapConsensusConfig
       (Translate WrapChainDepState)
       ByronBlock
       (ShelleyBlock (TPraos c) (ShelleyEra c))
translateChainDepStateByronToShelleyWrapper =
    RequireBoth $ \_ (WrapConsensusConfig cfgShelley) ->
      Translate $ \_ (WrapChainDepState pbftState) ->
        WrapChainDepState $
          translateChainDepStateByronToShelley cfgShelley pbftState

translateChainDepStateByronToShelley ::
     forall bc c.
     ConsensusConfig (TPraos c)
  -> PBftState bc
  -> TPraosState c
translateChainDepStateByronToShelley TPraosConfig { tpraosParams } pbftState =
    -- Note that the 'PBftState' doesn't know about EBBs. So if the last slot of
    -- the Byron era were occupied by an EBB (and no regular block in that same
    -- slot), we would pick the wrong slot here, i.e., the slot of the regular
    -- block before the EBB.
    --
    -- Fortunately, this is impossible for two reasons:
    --
    -- 1. On mainnet we stopped producing EBBs a while before the transition.
    -- 2. The transition happens at the start of an epoch, so if the last slot
    --    were occupied by an EBB, it must have been the EBB at the start of the
    --    previous epoch. This means the previous epoch must have been empty,
    --    which is a violation of the "@k@ blocks per @2k@ slots" property.
    TPraosState (PBftState.lastSignedSlot pbftState) $
      SL.ChainDepState
        { SL.csProtocol = SL.PrtclState Map.empty nonce nonce
        , SL.csTickn    = SL.TicknState {
              SL.ticknStateEpochNonce    = nonce
            , SL.ticknStatePrevHashNonce = SL.NeutralNonce
            }
          -- Overridden before used
        , SL.csLabNonce = SL.NeutralNonce
        }
  where
    nonce = tpraosInitialNonce tpraosParams

crossEraForecastByronToShelleyWrapper ::
     forall c. Crypto c =>
     RequiringBoth
       WrapLedgerConfig
       (CrossEraForecaster LedgerState WrapLedgerView)
       ByronBlock
       (ShelleyBlock (TPraos c) (ShelleyEra c))
crossEraForecastByronToShelleyWrapper =
    RequireBoth $ \_ (WrapLedgerConfig cfgShelley) ->
      CrossEraForecaster (forecast cfgShelley)
  where
    -- We ignore the Byron ledger view and create a new Shelley.
    --
    -- The full Shelley forecast range (stability window) starts from the first
    -- slot of the Shelley era, no matter how many slots there are between the
    -- Byron ledger and the first Shelley slot. Note that this number of slots
    -- is still guaranteed to be less than the forecast range of the HFC in the
    -- Byron era.
    forecast ::
         ShelleyLedgerConfig (ShelleyEra c)
      -> Bound
      -> SlotNo
      -> LedgerState ByronBlock
      -> Except
           OutsideForecastRange
           (WrapLedgerView (ShelleyBlock (TPraos c) (ShelleyEra c)))
    forecast cfgShelley bound forecastFor currentByronState
        | forecastFor < maxFor
        = return $
            WrapLedgerView $
              SL.mkInitialShelleyLedgerView
                (toFromByronTranslationContext (shelleyLedgerGenesis cfgShelley))
        | otherwise
        = throwError $ OutsideForecastRange {
              outsideForecastAt     = ledgerTipSlot currentByronState
            , outsideForecastMaxFor = maxFor
            , outsideForecastFor    = forecastFor
            }
      where
        globals = shelleyLedgerGlobals cfgShelley
        swindow = SL.stabilityWindow globals

        -- This is the exclusive upper bound of the forecast range
        --
        -- If Shelley's stability window is 0, it means we can't forecast /at
        -- all/ in the Shelley era. Not even to the first slot in the Shelley
        -- era! Remember that forecasting to slot @S@ means forecasting the
        -- ledger view obtained from the ledger state /after/ applying the block
        -- with slot @S@. If the stability window is 0, we can't even forecast
        -- after the very first "virtual" Shelley block, meaning we can't
        -- forecast into the Shelley era when still in the Byron era.
        maxFor :: SlotNo
        maxFor = addSlots swindow (boundSlot bound)

{-------------------------------------------------------------------------------
  Translation from Shelley to Allegra
-------------------------------------------------------------------------------}

translateLedgerStateShelleyToAllegraWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       (ShelleyBlock (TPraos c) (ShelleyEra c))
       (ShelleyBlock (TPraos c) (AllegraEra c))
translateLedgerStateShelleyToAllegraWrapper =
    ignoringBoth $
      Translate $ \_epochNo ->
        unComp . SL.translateEra' SL.NoGenesis . Comp

translateTxShelleyToAllegraWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => InjectTx
       (ShelleyBlock (TPraos c) (ShelleyEra c))
       (ShelleyBlock (TPraos c) (AllegraEra c))
translateTxShelleyToAllegraWrapper = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra SL.NoGenesis . Comp

translateValidatedTxShelleyToAllegraWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => InjectValidatedTx
       (ShelleyBlock (TPraos c) (ShelleyEra c))
       (ShelleyBlock (TPraos c) (AllegraEra c))
translateValidatedTxShelleyToAllegraWrapper = InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra SL.NoGenesis . Comp

{-------------------------------------------------------------------------------
  Translation from Allegra to Mary
-------------------------------------------------------------------------------}

translateLedgerStateAllegraToMaryWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       (ShelleyBlock (TPraos c) (AllegraEra c))
       (ShelleyBlock (TPraos c) (MaryEra c))
translateLedgerStateAllegraToMaryWrapper =
    ignoringBoth $
      Translate $ \_epochNo ->
        unComp . SL.translateEra' SL.NoGenesis . Comp

translateTxAllegraToMaryWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => InjectTx
       (ShelleyBlock (TPraos c) (AllegraEra c))
       (ShelleyBlock (TPraos c) (MaryEra c))
translateTxAllegraToMaryWrapper = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra SL.NoGenesis . Comp

translateValidatedTxAllegraToMaryWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => InjectValidatedTx
       (ShelleyBlock (TPraos c) (AllegraEra c))
       (ShelleyBlock (TPraos c) (MaryEra c))
translateValidatedTxAllegraToMaryWrapper = InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra SL.NoGenesis . Comp

{-------------------------------------------------------------------------------
  Translation from Mary to Alonzo
-------------------------------------------------------------------------------}

translateLedgerStateMaryToAlonzoWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       (ShelleyBlock (TPraos c) (MaryEra c))
       (ShelleyBlock (TPraos c) (AlonzoEra c))
translateLedgerStateMaryToAlonzoWrapper =
    RequireBoth $ \_cfgMary cfgAlonzo ->
      Translate $ \_epochNo ->
        unComp . SL.translateEra' (getAlonzoTranslationContext cfgAlonzo) . Comp

getAlonzoTranslationContext ::
     WrapLedgerConfig (ShelleyBlock (TPraos c) (AlonzoEra c))
  -> SL.TranslationContext (AlonzoEra c)
getAlonzoTranslationContext =
    shelleyLedgerTranslationContext . unwrapLedgerConfig

translateTxMaryToAlonzoWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => SL.TranslationContext (AlonzoEra c)
  -> InjectTx
       (ShelleyBlock (TPraos c) (MaryEra c))
       (ShelleyBlock (TPraos c) (AlonzoEra c))
translateTxMaryToAlonzoWrapper ctxt = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp

translateValidatedTxMaryToAlonzoWrapper ::
     forall c.
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => SL.TranslationContext (AlonzoEra c)
  -> InjectValidatedTx
       (ShelleyBlock (TPraos c) (MaryEra c))
       (ShelleyBlock (TPraos c) (AlonzoEra c))
translateValidatedTxMaryToAlonzoWrapper ctxt = InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp

{-------------------------------------------------------------------------------
  Translation from Alonzo to Babbage
-------------------------------------------------------------------------------}

translateLedgerStateAlonzoToBabbageWrapper ::
     (Praos.PraosCrypto c, TPraos.PraosCrypto c)
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       (ShelleyBlock (TPraos c) (AlonzoEra c))
       (ShelleyBlock (Praos c) (BabbageEra c))
translateLedgerStateAlonzoToBabbageWrapper =
    RequireBoth $ \_cfgAlonzo _cfgBabbage ->
      Translate $ \_epochNo ->
        unComp . SL.translateEra' SL.NoGenesis . Comp . transPraosLS
  where
    transPraosLS ::
      LedgerState (ShelleyBlock (TPraos c) (AlonzoEra c)) ->
      LedgerState (ShelleyBlock (Praos c)  (AlonzoEra c))
    transPraosLS (ShelleyLedgerState wo nes st) =
      ShelleyLedgerState
        { shelleyLedgerTip        = fmap castShelleyTip wo
        , shelleyLedgerState      = nes
        , shelleyLedgerTransition = st
        }

translateTxAlonzoToBabbageWrapper ::
     (Praos.PraosCrypto c)
  => SL.TranslationContext (BabbageEra c)
  -> InjectTx
       (ShelleyBlock (TPraos c) (AlonzoEra c))
       (ShelleyBlock (Praos c) (BabbageEra c))
translateTxAlonzoToBabbageWrapper ctxt = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp . transPraosTx
  where
    transPraosTx
      :: GenTx (ShelleyBlock (TPraos c) (AlonzoEra c))
      -> GenTx (ShelleyBlock (Praos c) (AlonzoEra c))
    transPraosTx (ShelleyTx ti tx) = ShelleyTx ti (coerce tx)

translateValidatedTxAlonzoToBabbageWrapper ::
     forall c.
     (Praos.PraosCrypto c)
  => SL.TranslationContext (BabbageEra c)
  -> InjectValidatedTx
       (ShelleyBlock (TPraos c) (AlonzoEra c))
       (ShelleyBlock (Praos c) (BabbageEra c))
translateValidatedTxAlonzoToBabbageWrapper ctxt = InjectValidatedTx $
  fmap unComp
    . eitherToMaybe
    . runExcept
    . SL.translateEra ctxt
    . Comp
    . transPraosValidatedTx
 where
  transPraosValidatedTx
    :: WrapValidatedGenTx (ShelleyBlock (TPraos c) (AlonzoEra c))
    -> WrapValidatedGenTx (ShelleyBlock (Praos c) (AlonzoEra c))
  transPraosValidatedTx (WrapValidatedGenTx x) = case x of
    ShelleyValidatedTx txid vtx -> WrapValidatedGenTx $
      ShelleyValidatedTx txid (SL.coerceValidated vtx)

{-------------------------------------------------------------------------------
  Translation from Babbage to Conway
-------------------------------------------------------------------------------}

translateLedgerStateBabbageToConwayWrapper ::
     (Praos.PraosCrypto c)
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       (ShelleyBlock (Praos c) (BabbageEra c))
       (ShelleyBlock (Praos c) (ConwayEra c))
translateLedgerStateBabbageToConwayWrapper =
    RequireBoth $ \_cfgBabbage cfgConway ->
      Translate $ \_epochNo ->
        unComp . SL.translateEra' (getConwayTranslationContext cfgConway) . Comp

getConwayTranslationContext ::
     WrapLedgerConfig (ShelleyBlock (Praos c) (ConwayEra c))
  -> SL.TranslationContext (ConwayEra c)
getConwayTranslationContext =
    shelleyLedgerTranslationContext . unwrapLedgerConfig

translateTxBabbageToConwayWrapper ::
     (Praos.PraosCrypto c)
  => SL.TranslationContext (ConwayEra c)
  -> InjectTx
       (ShelleyBlock (Praos c) (BabbageEra c))
       (ShelleyBlock (Praos c) (ConwayEra c))
translateTxBabbageToConwayWrapper ctxt = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp

translateValidatedTxBabbageToConwayWrapper ::
     forall c.
     (Praos.PraosCrypto c)
  => SL.TranslationContext (ConwayEra c)
  -> InjectValidatedTx
       (ShelleyBlock (Praos c) (BabbageEra c))
       (ShelleyBlock (Praos c) (ConwayEra c))
translateValidatedTxBabbageToConwayWrapper ctxt = InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp
