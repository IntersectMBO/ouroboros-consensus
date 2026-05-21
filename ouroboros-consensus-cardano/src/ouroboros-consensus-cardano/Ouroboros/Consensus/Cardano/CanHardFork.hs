{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-x-ord-preserving-coercions #-}
#if __GLASGOW_HASKELL__ < 908
{-# OPTIONS_GHC -Wno-unrecognised-warning-flags #-}
#endif

module Ouroboros.Consensus.Cardano.CanHardFork
  ( CardanoHardForkConstraints
  , TriggerHardFork (..)

    -- * Re-exports of Shelley code
  , ShelleyPartialLedgerConfig (..)
  , crossEraForecastAcrossShelley
  , translateChainDepStateAcrossShelley

    -- * Exposed for testing
  , getConwayTranslationContext
  , getDijkstraTranslationContext
  , inMemoryBackendArgs
  ) where

import Cardano.Ledger.Allegra.Translation
  ( shelleyToAllegraAVVMsToDelete
  )
import qualified Cardano.Ledger.BaseTypes as SL
import Cardano.Ledger.Binary.Decoding
import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Genesis as SL
import qualified Cardano.Ledger.Shelley.API as SL
import Cardano.Ledger.Shelley.Translation
  ( toFromByronTranslationContext
  )
import qualified Cardano.Protocol.TPraos.API as SL
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as SL
import qualified Cardano.Protocol.TPraos.Rules.Tickn as SL
import Codec.Serialise
import qualified Control.Monad as Monad
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Control.Tracer
import Data.Coerce (coerce)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Maybe.Strict (StrictMaybe (..))
import Data.MemPack
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.InPairs (RequiringBoth (..), ignoringBoth)
import qualified Data.SOP.Strict as SOP
import Data.SOP.Tails (Tails (..))
import qualified Data.SOP.Tails as Tails
import qualified Data.SOP.Telescope as Telescope
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Byron.ByronHFC ()
import Ouroboros.Consensus.Byron.Ledger
import Ouroboros.Consensus.Byron.Node ()
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Cardano.InMemory
import Ouroboros.Consensus.Forecast
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import Ouroboros.Consensus.HardFork.History
  ( Bound (boundSlot)
  , addSlots
  )
import Ouroboros.Consensus.HardFork.Simple
import Ouroboros.Consensus.Ledger.Abstract hiding (Handle, TickedHandle)
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ByteSize32
  , IgnoringOverflow
  , TxMeasure
  )
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol
  )
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Protocol.Abstract hiding
  ( translateChainDepState
  )
import Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import qualified Ouroboros.Consensus.Protocol.PBFT.State as PBftState
import Ouroboros.Consensus.Protocol.Praos (Praos)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import Ouroboros.Consensus.Protocol.Praos.Common (PraosTiebreakerView)
import Ouroboros.Consensus.Protocol.TPraos
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger
import Ouroboros.Consensus.Shelley.Node ()
import Ouroboros.Consensus.Shelley.Protocol.Praos ()
import Ouroboros.Consensus.Shelley.ShelleyHFC
import Ouroboros.Consensus.Storage.LedgerDB.Args
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.IOLike
import System.FS.API
import System.FS.CRC

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

type CardanoHardForkConstraints c =
  ( TPraos.PraosCrypto c
  , Praos.PraosCrypto c
  , LedgerSupportsProtocol (ShelleyBlock (TPraos c) ShelleyEra)
  , LedgerSupportsProtocol (ShelleyBlock (TPraos c) AllegraEra)
  , LedgerSupportsProtocol (ShelleyBlock (TPraos c) MaryEra)
  , LedgerSupportsProtocol (ShelleyBlock (TPraos c) AlonzoEra)
  , LedgerSupportsProtocol (ShelleyBlock (Praos c) BabbageEra)
  , LedgerSupportsProtocol (ShelleyBlock (Praos c) ConwayEra)
  , LedgerSupportsProtocol (ShelleyBlock (Praos c) DijkstraEra)
  )

-- | When performing era translations, two eras have special behaviours on the
-- ledger tables:
--
-- * Byron to Shelley: as Byron has no tables, the whole UTxO set is computed as
--     insertions, note that it uses 'valuesAsDiffs'
--
-- * Shelley to Allegra: some special addresses (the so called /AVVM/
--     addresses), were deleted in this transition, which influenced things like
--     the calculation of later rewards. In this transition, we consume the
--     'shelleyToAllegraAVVMsToDelete' as deletions in the ledger tables.
instance CardanoHardForkConstraints c => CanHardFork (CardanoEras c) where
  type HardForkTxMeasure (CardanoEras c) = DijkstraMeasure

  type TransCtx m (CardanoEras c) = MkHandle m

  hardForkStateHandleTranslation = \tctx ->
    StateHandleTranslation
      { translateLedgerState =
          PCons (translateLedgerStateByronToShelleyWrapper tctx) $
            PCons translateLedgerStateShelleyToAllegraWrapper $
              PCons translateLedgerStateAllegraToMaryWrapper $
                PCons translateLedgerStateMaryToAlonzoWrapper $
                  PCons translateLedgerStateAlonzoToBabbageWrapper $
                    PCons translateLedgerStateBabbageToConwayWrapper $
                      PCons translateLedgerStateConwayToDijkstraWrapper $
                        PNil
      }
  hardForkEraTranslation =
    EraTranslation
      { translateChainDepState =
          PCons translateChainDepStateByronToShelleyWrapper $
            PCons translateChainDepStateAcrossShelley $
              PCons translateChainDepStateAcrossShelley $
                PCons translateChainDepStateAcrossShelley $
                  PCons translateChainDepStateAcrossShelley $
                    PCons translateChainDepStateAcrossShelley $
                      PCons translateChainDepStateAcrossShelley $
                        PNil
      , crossEraForecast =
          PCons crossEraForecastByronToShelleyWrapper $
            PCons crossEraForecastAcrossShelley $
              PCons crossEraForecastAcrossShelley $
                PCons crossEraForecastAcrossShelley $
                  PCons crossEraForecastAcrossShelley $
                    PCons crossEraForecastAcrossShelley $
                      PCons crossEraForecastAcrossShelley $
                        PNil
      }
  hardForkChainSel =
    -- Byron <-> Shelley, ...
    TCons (SOP.hpure NoTiebreakerAcrossEras)
    -- Inter-Shelley-based
    $
      Tails.hcpure (Proxy @(HasPraosTiebreakerView c)) SameTiebreakerAcrossEras
  hardForkInjectTxs =
    PCons (ignoringBoth $ Pair2 cannotInjectTx cannotInjectValidatedTx)
      $ PCons
        ( ignoringBoth $
            Pair2
              translateTxShelleyToAllegraWrapper
              translateValidatedTxShelleyToAllegraWrapper
        )
      $ PCons
        ( ignoringBoth $
            Pair2
              translateTxAllegraToMaryWrapper
              translateValidatedTxAllegraToMaryWrapper
        )
      $ PCons
        ( RequireBoth $ \_cfgMary cfgAlonzo ->
            let ctxt = getAlonzoTranslationContext cfgAlonzo
             in Pair2
                  (translateTxMaryToAlonzoWrapper ctxt)
                  (translateValidatedTxMaryToAlonzoWrapper ctxt)
        )
      $ PCons
        ( RequireBoth $ \_cfgAlonzo _cfgBabbage ->
            let ctxt = SL.NoGenesis
             in Pair2
                  (translateTxAlonzoToBabbageWrapper ctxt)
                  (translateValidatedTxAlonzoToBabbageWrapper ctxt)
        )
      $ PCons
        ( RequireBoth $ \_cfgBabbage cfgConway ->
            let ctxt = getConwayTranslationContext cfgConway
             in Pair2
                  (translateTxBabbageToConwayWrapper ctxt)
                  (translateValidatedTxBabbageToConwayWrapper ctxt)
        )
      $ PCons
        ( RequireBoth $ \_cfgConway cfgDijkstra ->
            let ctxt = getDijkstraTranslationContext cfgDijkstra
             in Pair2
                  (translateTxConwayToDijkstraWrapper ctxt)
                  (translateValidatedTxConwayToDijkstraWrapper ctxt)
        )
      $ PNil

  hardForkInjTxMeasure =
    fromByteSize
      `o` fromByteSize
      `o` fromByteSize
      `o` fromByteSize
      `o` fromAlonzo
      `o` fromAlonzo
      `o` fromConway
      `o` fromDijkstra
      `o` nil
   where
    nil :: SOP.NS f '[] -> a
    nil = \case {}

    infixr 9 `o`
    o ::
      (TxMeasure x -> a) ->
      (SOP.NS WrapTxMeasure xs -> a) ->
      SOP.NS WrapTxMeasure (x : xs) ->
      a
    o f g = \case
      SOP.Z (WrapTxMeasure x) -> f x
      SOP.S y -> g y

    fromByteSize :: IgnoringOverflow ByteSize32 -> DijkstraMeasure
    fromByteSize x = fromAlonzo $ AlonzoMeasure x mempty
    fromAlonzo x = fromConway $ ConwayMeasure x mempty
    fromConway x = fromDijkstra $ DijkstraMeasure x
    fromDijkstra x = x

class TiebreakerView (BlockProtocol blk) ~ PraosTiebreakerView c => HasPraosTiebreakerView c blk
instance TiebreakerView (BlockProtocol blk) ~ PraosTiebreakerView c => HasPraosTiebreakerView c blk

{-------------------------------------------------------------------------------
  Translation from Byron to Shelley
-------------------------------------------------------------------------------}

translateHeaderHashByronToShelley ::
  forall c.
  ShelleyCompatible (TPraos c) ShelleyEra =>
  Proxy c ->
  HeaderHash ByronBlock ->
  HeaderHash (ShelleyBlock (TPraos c) ShelleyEra)
translateHeaderHashByronToShelley _ =
  fromShortRawHash (Proxy @(ShelleyBlock (TPraos c) ShelleyEra))
    . toShortRawHash (Proxy @ByronBlock)

translatePointByronToShelley ::
  forall c.
  ShelleyCompatible (TPraos c) ShelleyEra =>
  Point ByronBlock ->
  WithOrigin BlockNo ->
  WithOrigin (ShelleyTip (TPraos c) ShelleyEra)
translatePointByronToShelley point bNo =
  case (point, bNo) of
    (GenesisPoint, Origin) ->
      Origin
    (BlockPoint s h, NotOrigin n) ->
      NotOrigin
        ShelleyTip
          { shelleyTipSlotNo = s
          , shelleyTipBlockNo = n
          , shelleyTipHash = translateHeaderHashByronToShelley (Proxy @c) h
          }
    _otherwise ->
      error "translatePointByronToShelley: invalid Byron state"

translateLedgerStateByronToShelleyWrapper ::
  (MonadThrow m, ShelleyCompatible (TPraos c) ShelleyEra) =>
  MkHandle m ->
  RequiringBoth
    WrapLedgerConfig
    (TranslateLedgerState m)
    ByronBlock
    (ShelleyBlock (TPraos c) ShelleyEra)
translateLedgerStateByronToShelleyWrapper mkH =
  RequireBoth $
    \_ (WrapLedgerConfig cfgShelley) ->
      TranslateLedgerState
        { translateLedgerStateWith = \epochNo (ByronStateHandle ledgerByron) -> do
            let st =
                  SL.translateToShelleyLedgerState
                    (toFromByronTranslationContext (shelleyLedgerGenesis cfgShelley))
                    epochNo
                    (byronLedgerState ledgerByron)

            ShelleyStateHandle
              ( ShelleyLedgerState
                  { shelleyLedgerTip =
                      translatePointByronToShelley (ledgerTipPoint ledgerByron) (byronLedgerTipBlockNo ledgerByron)
                  , shelleyLedgerState = st
                  , shelleyLedgerTransition = ShelleyTransitionInfo{shelleyAfterVoting = 0}
                  , shelleyLedgerLatestPerasCertRound = SNothing
                  }
              )
              <$> fromNewEpochState mkH st
        }

translateChainDepStateByronToShelleyWrapper ::
  RequiringBoth
    WrapConsensusConfig
    (Translate WrapChainDepState)
    ByronBlock
    (ShelleyBlock (TPraos c) ShelleyEra)
translateChainDepStateByronToShelleyWrapper =
  RequireBoth $ \_ (WrapConsensusConfig cfgShelley) ->
    Translate $ \_ (WrapChainDepState pbftState) ->
      WrapChainDepState $
        translateChainDepStateByronToShelley cfgShelley pbftState

translateChainDepStateByronToShelley ::
  forall bc c.
  ConsensusConfig (TPraos c) ->
  PBftState bc ->
  TPraosState
translateChainDepStateByronToShelley TPraosConfig{tpraosParams} pbftState =
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
      , SL.csTickn =
          SL.TicknState
            { SL.ticknStateEpochNonce = nonce
            , SL.ticknStatePrevHashNonce = SL.NeutralNonce
            }
      , -- Overridden before used
        SL.csLabNonce = SL.NeutralNonce
      }
 where
  nonce = tpraosInitialNonce tpraosParams

crossEraForecastByronToShelleyWrapper ::
  forall c.
  RequiringBoth
    WrapLedgerConfig
    (CrossEraForecaster LedgerState WrapLedgerView)
    ByronBlock
    (ShelleyBlock (TPraos c) ShelleyEra)
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
    ShelleyLedgerConfig ShelleyEra ->
    Bound ->
    SlotNo ->
    LedgerState ByronBlock ->
    Except
      OutsideForecastRange
      (WrapLedgerView (ShelleyBlock (TPraos c) ShelleyEra))
  forecast cfgShelley bound forecastFor currentByronState
    | forecastFor < maxFor =
        return $
          WrapLedgerView $
            SL.mkInitialShelleyLedgerView
              (toFromByronTranslationContext (shelleyLedgerGenesis cfgShelley))
    | otherwise =
        throwError $
          OutsideForecastRange
            { outsideForecastAt = ledgerTipSlot currentByronState
            , outsideForecastMaxFor = maxFor
            , outsideForecastFor = forecastFor
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
  MonadThrow m =>
  RequiringBoth
    WrapLedgerConfig
    (TranslateLedgerState m)
    (ShelleyBlock (TPraos c) ShelleyEra)
    (ShelleyBlock (TPraos c) AllegraEra)
translateLedgerStateShelleyToAllegraWrapper =
  ignoringBoth $
    TranslateLedgerState
      { translateLedgerStateWith = \_epochNo (ShelleyStateHandle ls h) -> do
          let avvms = shelleyToAllegraAVVMsToDelete $ shelleyLedgerState ls
              nes = stateWithUTxO h avvms
              ls' = unComp . SL.translateEra' SL.NoGenesis $ Comp ls{shelleyLedgerState = nes}

          h' <-
            -- Written this way to ensure we don't try to hold the intermediate handle
            castHandle h (shelleyLedgerState ls')
              >>= flip applyDiff (Diff.fromMapDeletes $ SL.unUTxO avvms)

          pure $ ShelleyStateHandle ls' h'
      }

translateTxShelleyToAllegraWrapper ::
  InjectTx
    (ShelleyBlock (TPraos c) ShelleyEra)
    (ShelleyBlock (TPraos c) AllegraEra)
translateTxShelleyToAllegraWrapper =
  InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra SL.NoGenesis . Comp

translateValidatedTxShelleyToAllegraWrapper ::
  InjectValidatedTx
    (ShelleyBlock (TPraos c) ShelleyEra)
    (ShelleyBlock (TPraos c) AllegraEra)
translateValidatedTxShelleyToAllegraWrapper =
  InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra SL.NoGenesis . Comp

{-------------------------------------------------------------------------------
  Translation from Allegra to Mary
-------------------------------------------------------------------------------}

translateLedgerStateAllegraToMaryWrapper ::
  MonadThrow m =>
  RequiringBoth
    WrapLedgerConfig
    (TranslateLedgerState m)
    (ShelleyBlock (TPraos c) AllegraEra)
    (ShelleyBlock (TPraos c) MaryEra)
translateLedgerStateAllegraToMaryWrapper =
  ignoringBoth $
    TranslateLedgerState
      { translateLedgerStateWith = \_epochNo (ShelleyStateHandle st h) ->
          let st' = unComp . SL.translateEra' SL.NoGenesis $ Comp st
           in ShelleyStateHandle st' <$> castHandle h (shelleyLedgerState st')
      }

translateTxAllegraToMaryWrapper ::
  InjectTx
    (ShelleyBlock (TPraos c) AllegraEra)
    (ShelleyBlock (TPraos c) MaryEra)
translateTxAllegraToMaryWrapper =
  InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra SL.NoGenesis . Comp

translateValidatedTxAllegraToMaryWrapper ::
  InjectValidatedTx
    (ShelleyBlock (TPraos c) AllegraEra)
    (ShelleyBlock (TPraos c) MaryEra)
translateValidatedTxAllegraToMaryWrapper =
  InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra SL.NoGenesis . Comp

{-------------------------------------------------------------------------------
  Translation from Mary to Alonzo
-------------------------------------------------------------------------------}

translateLedgerStateMaryToAlonzoWrapper ::
  MonadThrow m =>
  RequiringBoth
    WrapLedgerConfig
    (TranslateLedgerState m)
    (ShelleyBlock (TPraos c) MaryEra)
    (ShelleyBlock (TPraos c) AlonzoEra)
translateLedgerStateMaryToAlonzoWrapper =
  RequireBoth $ \_cfgMary cfgAlonzo ->
    TranslateLedgerState
      { translateLedgerStateWith = \_epochNo (ShelleyStateHandle st h) ->
          let st' = unComp . SL.translateEra' (getAlonzoTranslationContext cfgAlonzo) $ Comp st
           in ShelleyStateHandle st' <$> castHandle h (shelleyLedgerState st')
      }

getAlonzoTranslationContext ::
  WrapLedgerConfig (ShelleyBlock (TPraos c) AlonzoEra) ->
  SL.TranslationContext AlonzoEra
getAlonzoTranslationContext =
  shelleyLedgerTranslationContext . unwrapLedgerConfig

translateTxMaryToAlonzoWrapper ::
  SL.TranslationContext AlonzoEra ->
  InjectTx
    (ShelleyBlock (TPraos c) MaryEra)
    (ShelleyBlock (TPraos c) AlonzoEra)
translateTxMaryToAlonzoWrapper ctxt =
  InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp

translateValidatedTxMaryToAlonzoWrapper ::
  forall c.
  SL.TranslationContext AlonzoEra ->
  InjectValidatedTx
    (ShelleyBlock (TPraos c) MaryEra)
    (ShelleyBlock (TPraos c) AlonzoEra)
translateValidatedTxMaryToAlonzoWrapper ctxt =
  InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp

{-------------------------------------------------------------------------------
  Translation from Alonzo to Babbage
-------------------------------------------------------------------------------}

translateLedgerStateAlonzoToBabbageWrapper ::
  MonadThrow m =>
  RequiringBoth
    WrapLedgerConfig
    (TranslateLedgerState m)
    (ShelleyBlock (TPraos c) AlonzoEra)
    (ShelleyBlock (Praos c) BabbageEra)
translateLedgerStateAlonzoToBabbageWrapper =
  RequireBoth $ \_cfgAlonzo _cfgBabbage ->
    TranslateLedgerState
      { translateLedgerStateWith = \_epochNo (ShelleyStateHandle st h) ->
          let st' = unComp . SL.translateEra' SL.NoGenesis $ Comp $ transPraosLS st
           in ShelleyStateHandle st' <$> castHandle h (shelleyLedgerState st')
      }
 where
  transPraosLS ::
    LedgerState (ShelleyBlock (TPraos c) AlonzoEra) ->
    LedgerState (ShelleyBlock (Praos c) AlonzoEra)
  transPraosLS (ShelleyLedgerState wo nes st lcr) =
    ShelleyLedgerState
      { shelleyLedgerTip = fmap castShelleyTip wo
      , shelleyLedgerState = nes
      , shelleyLedgerTransition = st
      , shelleyLedgerLatestPerasCertRound = lcr
      }

translateTxAlonzoToBabbageWrapper ::
  SL.TranslationContext BabbageEra ->
  InjectTx
    (ShelleyBlock (TPraos c) AlonzoEra)
    (ShelleyBlock (Praos c) BabbageEra)
translateTxAlonzoToBabbageWrapper ctxt =
  InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp . transPraosTx
 where
  transPraosTx ::
    GenTx (ShelleyBlock (TPraos c) AlonzoEra) ->
    GenTx (ShelleyBlock (Praos c) AlonzoEra)
  transPraosTx (ShelleyTx ti tx) = ShelleyTx ti (coerce tx)

translateValidatedTxAlonzoToBabbageWrapper ::
  forall c.
  SL.TranslationContext BabbageEra ->
  InjectValidatedTx
    (ShelleyBlock (TPraos c) AlonzoEra)
    (ShelleyBlock (Praos c) BabbageEra)
translateValidatedTxAlonzoToBabbageWrapper ctxt =
  InjectValidatedTx $
    fmap unComp
      . eitherToMaybe
      . runExcept
      . SL.translateEra ctxt
      . Comp
      . transPraosValidatedTx
 where
  transPraosValidatedTx ::
    WrapValidatedGenTx (ShelleyBlock (TPraos c) AlonzoEra) ->
    WrapValidatedGenTx (ShelleyBlock (Praos c) AlonzoEra)
  transPraosValidatedTx (WrapValidatedGenTx x) = case x of
    ShelleyValidatedTx txid vtx ->
      WrapValidatedGenTx $
        ShelleyValidatedTx txid (SL.coerceValidated vtx)

{-------------------------------------------------------------------------------
  Translation from Babbage to Conway
-------------------------------------------------------------------------------}

translateLedgerStateBabbageToConwayWrapper ::
  MonadThrow m =>
  RequiringBoth
    WrapLedgerConfig
    (TranslateLedgerState m)
    (ShelleyBlock (Praos c) BabbageEra)
    (ShelleyBlock (Praos c) ConwayEra)
translateLedgerStateBabbageToConwayWrapper =
  RequireBoth $ \_cfgBabbage cfgConway ->
    TranslateLedgerState
      { translateLedgerStateWith = \_epochNo (ShelleyStateHandle st h) ->
          let st' = unComp . SL.translateEra' (getConwayTranslationContext cfgConway) $ Comp st
           in ShelleyStateHandle st' <$> castHandle h (shelleyLedgerState st')
      }

getConwayTranslationContext ::
  WrapLedgerConfig (ShelleyBlock (Praos c) ConwayEra) ->
  SL.TranslationContext ConwayEra
getConwayTranslationContext =
  shelleyLedgerTranslationContext . unwrapLedgerConfig

translateTxBabbageToConwayWrapper ::
  SL.TranslationContext ConwayEra ->
  InjectTx
    (ShelleyBlock (Praos c) BabbageEra)
    (ShelleyBlock (Praos c) ConwayEra)
translateTxBabbageToConwayWrapper ctxt =
  InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp

translateValidatedTxBabbageToConwayWrapper ::
  forall c.
  SL.TranslationContext ConwayEra ->
  InjectValidatedTx
    (ShelleyBlock (Praos c) BabbageEra)
    (ShelleyBlock (Praos c) ConwayEra)
translateValidatedTxBabbageToConwayWrapper ctxt =
  InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp

{-------------------------------------------------------------------------------
  Translation from Conway to Dijkstra
-------------------------------------------------------------------------------}

translateLedgerStateConwayToDijkstraWrapper ::
  MonadThrow m =>
  RequiringBoth
    WrapLedgerConfig
    (TranslateLedgerState m)
    (ShelleyBlock (Praos c) ConwayEra)
    (ShelleyBlock (Praos c) DijkstraEra)
translateLedgerStateConwayToDijkstraWrapper =
  RequireBoth $ \_cfgConway cfgDijkstra ->
    TranslateLedgerState
      { translateLedgerStateWith = \_epochNo (ShelleyStateHandle st h) ->
          let st' = unComp . SL.translateEra' (getDijkstraTranslationContext cfgDijkstra) $ Comp st
           in ShelleyStateHandle st' <$> castHandle h (shelleyLedgerState st')
      }

getDijkstraTranslationContext ::
  WrapLedgerConfig (ShelleyBlock (Praos c) DijkstraEra) ->
  SL.TranslationContext DijkstraEra
getDijkstraTranslationContext =
  shelleyLedgerTranslationContext . unwrapLedgerConfig

translateTxConwayToDijkstraWrapper ::
  SL.TranslationContext DijkstraEra ->
  InjectTx
    (ShelleyBlock (Praos c) ConwayEra)
    (ShelleyBlock (Praos c) DijkstraEra)
translateTxConwayToDijkstraWrapper ctxt =
  InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp

translateValidatedTxConwayToDijkstraWrapper ::
  forall c.
  SL.TranslationContext DijkstraEra ->
  InjectValidatedTx
    (ShelleyBlock (Praos c) ConwayEra)
    (ShelleyBlock (Praos c) DijkstraEra)
translateValidatedTxConwayToDijkstraWrapper ctxt =
  InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}

-- | Construct the 'LedgerDbBackendArgs' for the in-memory backend.
--
-- This is the entry point that the node and tools call to wire the
-- in-memory backend into the LedgerDB. The resources acquired here are
-- the 'MkHandle' and 'MkHandleFromSnapshot' factories produced from the
-- file system passed in by 'acquireBackend'; those factories are then
-- closed over by the fields of the resulting 'BackendResources'.
--
-- The in-memory backend has no long-lived resources of its own, so
-- 'brRelease' is a no-op and 'acquireBackend' does not allocate into the
-- temporary registry.
inMemoryBackendArgs ::
  forall m c.
  (CardanoHardForkConstraints c, SerialiseHFC (CardanoEras c), IOLike m) =>
  LedgerDbBackendArgs m (CardanoBlock c)
inMemoryBackendArgs = LedgerDbBackendArgs $ \_tr shfs ->
  let
    mkH :: MkHandle m
    mkH = mkInMemoryFactory shfs

    mkHs :: MkHandleFromSnapshot m
    mkHs = mkInMemoryFromSnapshot shfs
   in
    pure
      BackendResources
        { brCreateGenesis = createGenesisInMemory mkH
        , brLoadSnapshot = loadSnapshotInMemory mkHs mkH
        , brSnapshotManager = inMemorySnapshotManager
        , brRelease = pure ()
        }

-- | Build the genesis 'ExtStateHandle' for the in-memory backend.
--
-- The 'ExtLedgerState' supplied by 'lgrGenesis' is destructured into its
-- 'LedgerState' / 'HeaderState' parts and the 'MkHandle' factory is used
-- to obtain the initial 'LedgerTablesHandle'. The two are then paired
-- with 'newStateHandle' (from 'MonadLedger') and wrapped into an
-- 'ExtStateHandle'.
--
-- The block-type-specific assembly currently goes through 'fillJavier'
-- to mirror the existing 'injectInitialExtLedgerState' wiring in
-- "Ouroboros.Consensus.Cardano.Node": once that lands the
-- 'MkHandle'-driven path will plug in directly here.
createGenesisInMemory ::
  forall m c.
  IOLike m =>
  MkHandle m ->
  ExtLedgerState (CardanoBlock c) ->
  m (ExtStateHandle m (CardanoBlock c))
createGenesisInMemory mkH (ExtLedgerState ls hs) =
  pure
    ( ExtStateHandle
        ( HardForkStateHandle
            ( SOP.hczipWith
                (Proxy @(MonadLedger m))
                (\f g -> SOP.apFn f g)
                ( SOP.Fn (flip newStateHandle ())
                    SOP.:* SOP.hpure (error "You initialized the chain not in the first era!")
                )
                (hardForkLedgerStatePerEra ls)
            )
            mkH
        )
        $ hs
    )

-- TODO @js: once 'injectInitialExtLedgerState' is wired in
-- "Ouroboros.Consensus.Cardano.Node" against 'MkHandle', call it here.

-- | Load an 'ExtStateHandle' from a snapshot using the in-memory backend.
--
-- Reads the 'ExtLedgerState' from disk, then delegates to
-- 'MkHandleFromSnapshot' to materialise the in-memory tables for the
-- snapshot's era.
loadSnapshotInMemory ::
  forall m c.
  (CardanoHardForkConstraints c, SerialiseHFC (CardanoEras c), IOLike m) =>
  MkHandleFromSnapshot m ->
  MkHandle m ->
  CodecConfig (CardanoBlock c) ->
  SomeHasFS m ->
  DiskSnapshot ->
  ExceptT
    (SnapshotFailure (CardanoBlock c))
    m
    (ExtStateHandle m (CardanoBlock c), RealPoint (CardanoBlock c))
loadSnapshotInMemory mkFromSnapshot mkH ccfg fs@(SomeHasFS hfs) ds = do
  fileEx <- lift $ doesFileExist hfs (snapshotToDirPath ds)
  Monad.when fileEx $ throwError $ InitFailureRead ReadSnapshotIsLegacy

  snapshotMeta <-
    withExceptT (InitFailureRead . ReadMetadataError (snapshotToMetadataPath ds)) $
      loadSnapshotMetadata fs ds
  Monad.when (snapshotBackend snapshotMeta /= UTxOHDMemSnapshot) $
    throwError $
      InitFailureRead $
        ReadMetadataError (snapshotToMetadataPath ds) MetadataBackendMismatch
  (ExtLedgerState ls hs, checksumAsRead) <-
    withExceptT
      (InitFailureRead . ReadSnapshotFailed)
      $ readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode (snapshotToStatePath ds)
  case pointToWithOriginRealPoint (getTip ls) of
    Origin -> throwError InitFailureGenesis
    NotOrigin pt -> do
      ns <-
        SOP.hsequence' $
          SOP.hzipWith
            SOP.apFn
            ( let sf ::
                    forall proto era.
                    ( SL.Era era
                    , MemPack (SL.TxOut era)
                    , IOLike m
                    , Share (SL.TxOut era) ~ Interns (SL.Credential SL.Staking)
                    , DecShareCBOR (SL.TxOut era)
                    , SL.EraCertState era
                    , Eq (SL.TxOut era)
                    ) =>
                    ( LedgerState SOP.-.-> ExceptT (SnapshotFailure (CardanoBlock c)) m
                        :.: ( (,) (Maybe CRC)
                                :.: StateHandle m
                            )
                    )
                      (ShelleyBlock proto era)

                  sf = SOP.Fn $ \st@(shelleyLedgerState -> nes) ->
                    Comp $ withExceptT (InitFailureOther . show) $ do
                      (x, y) <- fromSnapshot mkFromSnapshot ds nes
                      pure $ Comp (y, ShelleyStateHandle st x)
                  np =
                    SOP.Fn (\bs -> Comp $ pure $ Comp $ (Just initCRC, ByronStateHandle bs))
                      SOP.:* sf
                      SOP.:* sf
                      SOP.:* sf
                      SOP.:* sf
                      SOP.:* sf
                      SOP.:* sf
                      SOP.:* sf
                      SOP.:* SOP.Nil
                  np ::
                    SOP.NP
                      ( LedgerState SOP.-.-> ExceptT (SnapshotFailure (CardanoBlock c)) m
                          :.: ( (,) (Maybe CRC)
                                  :.: StateHandle m
                              )
                      )
                      (CardanoEras c)
               in np
            )
            (hardForkLedgerStatePerEra ls)
      let crcTables =
            fromMaybe initCRC $
              SOP.hcollapse $
                SOP.hmap (K . fst . unComp . currentState) $
                  Telescope.tip $
                    getHardForkState ns
          extLedgerSt :: HardForkState (StateHandle m) (CardanoEras c)
          extLedgerSt = SOP.hmap (snd . unComp) ns
      let computedCRC = crcOfConcat checksumAsRead crcTables
      Monad.when (computedCRC /= snapshotChecksum snapshotMeta) $
        throwError $
          InitFailureRead $
            ReadSnapshotDataCorruption
      pure (ExtStateHandle (HardForkStateHandle extLedgerSt mkH) hs, pt)

-- | The in-memory backend's 'SnapshotManager'.
--
-- 'listSnapshots' and 'deleteSnapshotIfTemporary' are the standard
-- filesystem-driven implementations. 'takeSnapshot' writes the pure
-- 'ExtLedgerState' first and then delegates to 'takeHandleSnapshot' on
-- the per-era 'TablesHandle' for the on-disk component.
inMemorySnapshotManager ::
  forall m c.
  (CardanoHardForkConstraints c, SerialiseHFC (CardanoEras c), IOLike m) =>
  CodecConfig (CardanoBlock c) ->
  Tracer m (TraceSnapshotEvent (CardanoBlock c)) ->
  SomeHasFS m ->
  SnapshotManager m (CardanoBlock c) (ExtStateHandle m (CardanoBlock c))
inMemorySnapshotManager ccfg snapTracer shfs@(SomeHasFS hasFS) =
  SnapshotManager
    { listSnapshots = defaultListSnapshots shfs
    , deleteSnapshotIfTemporary = defaultDeleteSnapshotIfTemporary shfs snapTracer
    , takeSnapshot = \suffix st ->
        case pointToWithOriginRealPoint (getTip $ extLedgerState st) of
          Origin -> return Nothing
          NotOrigin t -> do
            let number = unSlotNo (realPointSlot t)
                snapshot = DiskSnapshot number suffix
            diskSnapshots <- defaultListSnapshots shfs
            if List.any (== DiskSnapshot number suffix) diskSnapshots
              then
                return Nothing
              else do
                writeSnapshot snapshot st
                return $ Just (snapshot, t)
    }
 where
  writeSnapshot :: DiskSnapshot -> ExtStateHandle m (CardanoBlock c) -> m ()
  writeSnapshot ds st = do
    createDirectoryIfMissing hasFS True $ snapshotToDirPath ds
    crc1 <-
      writeExtLedgerState
        shfs
        (encodeDiskExtLedgerState ccfg)
        (snapshotToStatePath ds)
        (extLedgerState st)
    (crc2, bknd) <-
      fmap SOP.hcollapse $
        SOP.hsequence' $
          SOP.hzipWith
            SOP.apFn
            ( let sf ::
                    (Current (StateHandle m) SOP.-.-> m :.: K (Maybe CRC, SnapshotBackend)) (ShelleyBlock proto era)
                  sf = SOP.Fn $ \(Current _ ss) -> Comp $ K <$> takeHandleSnapshot (stateRefHandle ss) ds
               in (SOP.Fn $ \_ -> Comp $ pure $ K (Nothing, UTxOHDMemSnapshot))
                    SOP.:* sf
                    SOP.:* sf
                    SOP.:* sf
                    SOP.:* sf
                    SOP.:* sf
                    SOP.:* sf
                    SOP.:* sf
                    SOP.:* SOP.Nil
            )
            (Telescope.tip $ getHardForkState $ hardForkStateHandlePerEra $ extStateHandle st)
    writeSnapshotMetadata shfs ds $
      SnapshotMetadata
        { snapshotBackend = bknd
        , snapshotChecksum = maybe crc1 (crcOfConcat crc1) crc2
        , snapshotTablesCodecVersion = TablesCodecVersion1
        }
