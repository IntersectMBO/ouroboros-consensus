{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Legacy.Cardano.CanHardFork (LegacyCardanoHardForkConstraints) where

import           Cardano.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)
import           Cardano.Ledger.Alonzo.Translation ()
import           Cardano.Ledger.Babbage.Translation ()
import           Cardano.Ledger.Conway.Translation ()
import           Cardano.Ledger.Crypto (ADDRHASH, Crypto, HASH)
import qualified Cardano.Ledger.Era as SL
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
import           Data.SOP.Functors (Flip (..))
import           Data.SOP.InPairs (RequiringBoth (..), ignoringBoth)
import           Data.SOP.Strict (hpure, unComp, (:.:) (..))
import           Data.SOP.Tails (Tails (..))
import qualified Data.SOP.Tails as Tails
import           Data.Void (absurd)
import           Legacy.Byron.Ledger ()
import           Legacy.Cardano.Block
import           Legacy.Shelley.Ledger ()
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
                     (CardanoHardForkConstraints)
import           Ouroboros.Consensus.Forecast
import qualified Ouroboros.Consensus.Forecast as Forecast
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.History (Bound (boundSlot),
                     addSlots)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Legacy.Block
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import qualified Ouroboros.Consensus.Protocol.PBFT.State as PBftState
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import           Ouroboros.Consensus.Protocol.TPraos
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import           Ouroboros.Consensus.Protocol.Translate (TranslateProto)
import qualified Ouroboros.Consensus.Protocol.Translate as Proto
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (eitherToMaybe)
import           Ouroboros.Consensus.Util.RedundantConstraints

{-------------------------------------------------------------------------------
 Cardano
-------------------------------------------------------------------------------}

type LegacyCardanoHardForkConstraints c = CardanoHardForkConstraints c

instance LegacyCardanoHardForkConstraints c => CanHardFork (LegacyCardanoEras c) where
  hardForkEraTranslation = EraTranslation {
      translateLedgerState =
          PCons translateLedgerStateByronToShelleyWrapper
        $ PCons translateLedgerStateShelleyToAllegraWrapper
        $ PCons translateLedgerStateAllegraToMaryWrapper
        $ PCons translateLedgerStateMaryToAlonzoWrapper
        $ PCons translateLedgerStateAlonzoToBabbageWrapper
        $ PCons translateLedgerStateBabbageToConwayWrapper
        $ PNil
    , translateLedgerTables  =
          PCons translateLegacyLedgerTables
        $ PCons translateLegacyLedgerTables
        $ PCons translateLegacyLedgerTables
        $ PCons translateLegacyLedgerTables
        $ PCons translateLegacyLedgerTables
        $ PCons translateLegacyLedgerTables
        $ PNil
    , translateChainDepState =
          PCons translateChainDepStateByronToShelleyWrapper
        $ PCons translateChainDepStateAcrossShelley'
        $ PCons translateChainDepStateAcrossShelley'
        $ PCons translateChainDepStateAcrossShelley'
        $ PCons translateChainDepStateAcrossShelley'
        $ PCons translateChainDepStateAcrossShelley'
        $ PNil
    , crossEraForecast =
          PCons translateLedgerViewByronToShelleyWrapper
        $ PCons translateLedgerViewAcrossShelley'
        $ PCons translateLedgerViewAcrossShelley'
        $ PCons translateLedgerViewAcrossShelley'
        $ PCons translateLedgerViewAcrossShelley'
        $ PCons translateLedgerViewAcrossShelley'
        $ PNil
    }
  hardForkChainSel =
        -- Byron <-> Shelley, ...
        TCons (hpure CompareBlockNo)
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
                let ctxt = ()
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
  => HeaderHash (LegacyBlock ByronBlock)
  -> HeaderHash (LegacyBlock (ShelleyBlock (TPraos c) (ShelleyEra c)))
translateHeaderHashByronToShelley =
      fromShortRawHash (Proxy @(LegacyBlock (ShelleyBlock (TPraos c) (ShelleyEra c))))
    . toShortRawHash   (Proxy @(LegacyBlock ByronBlock))
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
       TranslateLedgerState
       (LegacyBlock ByronBlock)
       (LegacyBlock (ShelleyBlock (TPraos c) (ShelleyEra c)))
translateLedgerStateByronToShelleyWrapper =
    RequireBoth $ \_ (WrapLedgerConfig cfgShelley) ->
    TranslateLedgerState {
        translateLedgerStateWith = \epochNo (LegacyLedgerState ledgerByron) ->
          LegacyLedgerState $ ShelleyLedgerState {
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
          , shelleyLedgerTables = emptyLedgerTables
          }
      }

translateChainDepStateByronToShelleyWrapper ::
     RequiringBoth
       WrapConsensusConfig
       (Translate WrapChainDepState)
       (LegacyBlock ByronBlock)
       (LegacyBlock (ShelleyBlock (TPraos c) (ShelleyEra c)))
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
              ticknStateEpochNonce    = nonce
            , ticknStatePrevHashNonce = SL.NeutralNonce
            }
          -- Overridden before used
        , SL.csLabNonce = SL.NeutralNonce
        }
  where
    nonce = tpraosInitialNonce tpraosParams

translateLedgerViewByronToShelleyWrapper ::
     forall c. Crypto c =>
     RequiringBoth
       WrapLedgerConfig
       (CrossEraForecaster LedgerState WrapLedgerView)
       (LegacyBlock ByronBlock)
       (LegacyBlock (ShelleyBlock (TPraos c) (ShelleyEra c)))
translateLedgerViewByronToShelleyWrapper =
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
      -> LedgerState (LegacyBlock ByronBlock) EmptyMK
      -> Except
           OutsideForecastRange
           (Ticked (WrapLedgerView (LegacyBlock (ShelleyBlock (TPraos c) (ShelleyEra c)))))
    forecast cfgShelley bound forecastFor currentByronState
        | forecastFor < maxFor
        = return $
            WrapTickedLedgerView $ TickedPraosLedgerView $
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
       TranslateLedgerState
       (LegacyBlock (ShelleyBlock (TPraos c) (ShelleyEra c)))
       (LegacyBlock (ShelleyBlock (TPraos c) (AllegraEra c)))
translateLedgerStateShelleyToAllegraWrapper =
    ignoringBoth $
      TranslateLedgerState {
          translateLedgerStateWith = \_epochNo ->
                LegacyLedgerState
              . unFlip
              . unComp
              . SL.translateEra' ()
              . Comp
              . Flip
              . getLegacyLedgerState
        }

translateTxShelleyToAllegraWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => InjectTx
       (LegacyBlock (ShelleyBlock (TPraos c) (ShelleyEra c)))
       (LegacyBlock (ShelleyBlock (TPraos c) (AllegraEra c)))
translateTxShelleyToAllegraWrapper = InjectTx $
      fmap ( LegacyGenTx
           . unComp
           )
    . eitherToMaybe . runExcept . SL.translateEra ()
    . Comp
    . getLegacyGenTx

translateValidatedTxShelleyToAllegraWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => InjectValidatedTx
       (LegacyBlock (ShelleyBlock (TPraos c) (ShelleyEra c)))
       (LegacyBlock (ShelleyBlock (TPraos c) (AllegraEra c)))
translateValidatedTxShelleyToAllegraWrapper = InjectValidatedTx $
      fmap ( WrapValidatedGenTx
           . LegacyValidatedGenTx
           . unwrapValidatedGenTx
           . unComp
           )
    . eitherToMaybe . runExcept . SL.translateEra ()
    . Comp
    . WrapValidatedGenTx
    . getLegacyValidatedGenTx
    . unwrapValidatedGenTx

{-------------------------------------------------------------------------------
  Translation from Allegra to Mary
-------------------------------------------------------------------------------}

translateLedgerStateAllegraToMaryWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => RequiringBoth
       WrapLedgerConfig
       TranslateLedgerState
       (LegacyBlock (ShelleyBlock (TPraos c) (AllegraEra c)))
       (LegacyBlock (ShelleyBlock (TPraos c) (MaryEra c)))
translateLedgerStateAllegraToMaryWrapper =
    ignoringBoth $
      TranslateLedgerState {
          translateLedgerStateWith = \_epochNo ->
                LegacyLedgerState
              . unFlip
              . unComp
              . SL.translateEra' ()
              . Comp
              . Flip
              . getLegacyLedgerState
        }

translateTxAllegraToMaryWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => InjectTx
       (LegacyBlock (ShelleyBlock (TPraos c) (AllegraEra c)))
       (LegacyBlock (ShelleyBlock (TPraos c) (MaryEra c)))
translateTxAllegraToMaryWrapper = InjectTx $
      fmap ( LegacyGenTx
           . unComp
           )
    . eitherToMaybe . runExcept . SL.translateEra ()
    . Comp
    . getLegacyGenTx

translateValidatedTxAllegraToMaryWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => InjectValidatedTx
       (LegacyBlock (ShelleyBlock (TPraos c) (AllegraEra c)))
       (LegacyBlock (ShelleyBlock (TPraos c) (MaryEra c)))
translateValidatedTxAllegraToMaryWrapper = InjectValidatedTx $
      fmap ( WrapValidatedGenTx
           . LegacyValidatedGenTx
           . unwrapValidatedGenTx
           . unComp
           )
    . eitherToMaybe . runExcept . SL.translateEra ()
    . Comp
    . WrapValidatedGenTx
    . getLegacyValidatedGenTx
    . unwrapValidatedGenTx

{-------------------------------------------------------------------------------
  Translation from Mary to Alonzo
-------------------------------------------------------------------------------}

translateLedgerStateMaryToAlonzoWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => RequiringBoth
       WrapLedgerConfig
       TranslateLedgerState
       (LegacyBlock (ShelleyBlock (TPraos c) (MaryEra c)))
       (LegacyBlock (ShelleyBlock (TPraos c) (AlonzoEra c)))
translateLedgerStateMaryToAlonzoWrapper =
    RequireBoth $ \_cfgMary cfgAlonzo ->
      TranslateLedgerState {
          translateLedgerStateWith = \_epochNo ->
                LegacyLedgerState
              . unFlip
              . unComp
              . SL.translateEra' (getAlonzoTranslationContext cfgAlonzo)
              . Comp
              . Flip
              . getLegacyLedgerState
        }

getAlonzoTranslationContext ::
     WrapLedgerConfig (LegacyBlock (ShelleyBlock (TPraos c) (AlonzoEra c)))
  -> SL.TranslationContext (AlonzoEra c)
getAlonzoTranslationContext =
    shelleyLedgerTranslationContext . unwrapLedgerConfig

translateTxMaryToAlonzoWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => SL.TranslationContext (AlonzoEra c)
  -> InjectTx
       (LegacyBlock (ShelleyBlock (TPraos c) (MaryEra c)))
       (LegacyBlock (ShelleyBlock (TPraos c) (AlonzoEra c)))
translateTxMaryToAlonzoWrapper ctxt = InjectTx $
      fmap ( LegacyGenTx
           . unComp
           )
    . eitherToMaybe . runExcept . SL.translateEra ctxt
    . Comp
    . getLegacyGenTx

translateValidatedTxMaryToAlonzoWrapper ::
     forall c.
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => SL.TranslationContext (AlonzoEra c)
  -> InjectValidatedTx
       (LegacyBlock (ShelleyBlock (TPraos c) (MaryEra c)))
       (LegacyBlock (ShelleyBlock (TPraos c) (AlonzoEra c)))
translateValidatedTxMaryToAlonzoWrapper ctxt = InjectValidatedTx $
      fmap ( WrapValidatedGenTx
           . LegacyValidatedGenTx
           . unwrapValidatedGenTx
           . unComp
           )
    . eitherToMaybe . runExcept . SL.translateEra ctxt
    . Comp
    . WrapValidatedGenTx
    . getLegacyValidatedGenTx
    . unwrapValidatedGenTx

{-------------------------------------------------------------------------------
  Translation from Alonzo to Babbage
-------------------------------------------------------------------------------}

translateLedgerStateAlonzoToBabbageWrapper ::
     (Praos.PraosCrypto c, TPraos.PraosCrypto c)
  => RequiringBoth
       WrapLedgerConfig
       TranslateLedgerState
       (LegacyBlock (ShelleyBlock (TPraos c) (AlonzoEra c)))
       (LegacyBlock (ShelleyBlock (Praos c) (BabbageEra c)))
translateLedgerStateAlonzoToBabbageWrapper =
  RequireBoth $ \_cfgAlonzo _cfgBabbage ->
      TranslateLedgerState {
          translateLedgerStateWith = \_epochNo ->
                LegacyLedgerState
              . unFlip
              . unComp
              . SL.translateEra' ()
              . Comp
              . Flip
              . transPraosLS
              . getLegacyLedgerState
        }
  where
    transPraosLS ::
      LedgerState (ShelleyBlock (TPraos c) (AlonzoEra c)) mk ->
      LedgerState (ShelleyBlock (Praos c)  (AlonzoEra c)) mk
    transPraosLS (ShelleyLedgerState wo nes st tb) =
      ShelleyLedgerState
        { shelleyLedgerTip        = fmap castShelleyTip wo
        , shelleyLedgerState      = nes
        , shelleyLedgerTransition = st
        , shelleyLedgerTables     = coerce tb
        }

translateTxAlonzoToBabbageWrapper ::
     (Praos.PraosCrypto c)
  => SL.TranslationContext (BabbageEra c)
  -> InjectTx
       (LegacyBlock (ShelleyBlock (TPraos c) (AlonzoEra c)))
       (LegacyBlock (ShelleyBlock (Praos c) (BabbageEra c)))
translateTxAlonzoToBabbageWrapper ctxt = InjectTx $
      fmap ( LegacyGenTx
           . unComp
           )
    . eitherToMaybe . runExcept . SL.translateEra ctxt
    . Comp
    . transPraosTx
    . getLegacyGenTx
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
       (LegacyBlock (ShelleyBlock (TPraos c) (AlonzoEra c)))
       (LegacyBlock (ShelleyBlock (Praos c) (BabbageEra c)))
translateValidatedTxAlonzoToBabbageWrapper ctxt = InjectValidatedTx $
      fmap ( WrapValidatedGenTx
           . LegacyValidatedGenTx
           . unwrapValidatedGenTx
           . unComp
           )
    . eitherToMaybe
    . runExcept
    . SL.translateEra ctxt
    . Comp
    . transPraosValidatedTx
    . WrapValidatedGenTx
    . getLegacyValidatedGenTx
    . unwrapValidatedGenTx
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
       TranslateLedgerState
       (LegacyBlock (ShelleyBlock (Praos c) (BabbageEra c)))
       (LegacyBlock (ShelleyBlock (Praos c) (ConwayEra c)))
translateLedgerStateBabbageToConwayWrapper =
  RequireBoth $ \_cfgBabbage cfgConway ->
      TranslateLedgerState {
          translateLedgerStateWith = \_epochNo ->
                LegacyLedgerState
              . unFlip
              . unComp
              . SL.translateEra' (getConwayTranslationContext cfgConway)
              . Comp
              . Flip
              . getLegacyLedgerState
        }

getConwayTranslationContext ::
     WrapLedgerConfig (LegacyBlock (ShelleyBlock (Praos c) (ConwayEra c)))
  -> SL.TranslationContext (ConwayEra c)
getConwayTranslationContext =
    shelleyLedgerTranslationContext . unwrapLedgerConfig

translateTxBabbageToConwayWrapper ::
     (Praos.PraosCrypto c)
  => SL.TranslationContext (ConwayEra c)
  -> InjectTx
       (LegacyBlock (ShelleyBlock (Praos c) (BabbageEra c)))
       (LegacyBlock (ShelleyBlock (Praos c) (ConwayEra c)))
translateTxBabbageToConwayWrapper ctxt = InjectTx $
      fmap ( LegacyGenTx
           . unComp
           )
    . eitherToMaybe . runExcept . SL.translateEra ctxt
    . Comp
    . getLegacyGenTx

translateValidatedTxBabbageToConwayWrapper ::
     forall c.
     (Praos.PraosCrypto c)
  => SL.TranslationContext (ConwayEra c)
  -> InjectValidatedTx
       (LegacyBlock (ShelleyBlock (Praos c) (BabbageEra c)))
       (LegacyBlock (ShelleyBlock (Praos c) (ConwayEra c)))
translateValidatedTxBabbageToConwayWrapper ctxt = InjectValidatedTx $
      fmap ( WrapValidatedGenTx
           . LegacyValidatedGenTx
           . unwrapValidatedGenTx
           . unComp
           )
    . eitherToMaybe . runExcept . SL.translateEra ctxt
    . Comp
    . WrapValidatedGenTx
    . getLegacyValidatedGenTx
    . unwrapValidatedGenTx

{-------------------------------------------------------------------------------
  ShelleyHFC
-------------------------------------------------------------------------------}

-- | Forecast from a Shelley-based era to the next Shelley-based era.
forecastAcrossShelley' ::
     forall protoFrom protoTo eraFrom eraTo mk.
     ( TranslateProto protoFrom protoTo
     , LedgerSupportsProtocol (ShelleyBlock protoFrom eraFrom)
     )
  => ShelleyLedgerConfig eraFrom
  -> ShelleyLedgerConfig eraTo
  -> Bound  -- ^ Transition between the two eras
  -> SlotNo -- ^ Forecast for this slot
  -> LedgerState (LegacyBlock (ShelleyBlock protoFrom eraFrom)) mk
  -> Except OutsideForecastRange (Ticked (WrapLedgerView (LegacyBlock (ShelleyBlock protoTo eraTo))))
forecastAcrossShelley' cfgFrom cfgTo transition forecastFor (LegacyLedgerState ledgerStateFrom)
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
    futureLedgerView :: SlotNo -> Ticked (WrapLedgerView (LegacyBlock (ShelleyBlock protoTo era)))
    futureLedgerView =
          WrapTickedLedgerView
        . either
            (\e -> error ("futureLedgerView failed: " <> show e))
            (Proto.translateTickedLedgerView @protoFrom @protoTo)
        . runExcept
        . Forecast.forecastFor (ledgerViewForecastAt cfgFrom ledgerStateFrom)

    -- Exclusive upper bound
    maxFor :: SlotNo
    maxFor = crossEraForecastBound
               (ledgerTipSlot ledgerStateFrom)
               (boundSlot transition)
               (SL.stabilityWindow (shelleyLedgerGlobals cfgFrom))
               (SL.stabilityWindow (shelleyLedgerGlobals cfgTo))

translateChainDepStateAcrossShelley' ::
     forall eraFrom eraTo protoFrom protoTo.
     ( TranslateProto protoFrom protoTo
     )
  => RequiringBoth
       WrapConsensusConfig
       (Translate WrapChainDepState)
       (LegacyBlock (ShelleyBlock protoFrom eraFrom))
       (LegacyBlock (ShelleyBlock protoTo eraTo))
translateChainDepStateAcrossShelley' =
    ignoringBoth $
      Translate $ \_epochNo (WrapChainDepState chainDepState) ->
        -- Same protocol, same 'ChainDepState'. Note that we don't have to apply
        -- any changes related to an epoch transition, this is already done when
        -- ticking the state.
        WrapChainDepState $ Proto.translateChainDepState @protoFrom @protoTo chainDepState

translateLedgerViewAcrossShelley' ::
     forall eraFrom eraTo protoFrom protoTo.
     ( TranslateProto protoFrom protoTo
     , LedgerSupportsProtocol (ShelleyBlock protoFrom eraFrom)
     )
  => RequiringBoth
       WrapLedgerConfig
       (CrossEraForecaster LedgerState WrapLedgerView)
       (LegacyBlock (ShelleyBlock protoFrom eraFrom))
       (LegacyBlock (ShelleyBlock protoTo eraTo))
translateLedgerViewAcrossShelley' =
    RequireBoth $ \(WrapLedgerConfig cfgFrom)
                   (WrapLedgerConfig cfgTo) ->
      CrossEraForecaster $ forecastAcrossShelley' cfgFrom cfgTo

{-------------------------------------------------------------------------------
  Translate ledger tables across any era
-------------------------------------------------------------------------------}

translateLegacyLedgerTables ::
     TranslateLedgerTables
       (LegacyBlock x)
       (LegacyBlock y)
translateLegacyLedgerTables = TranslateLedgerTables {
      translateTxInWith  = absurd
    , translateTxOutWith = absurd
    }

