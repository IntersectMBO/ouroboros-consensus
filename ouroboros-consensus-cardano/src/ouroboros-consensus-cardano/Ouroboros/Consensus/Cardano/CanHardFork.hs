{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Cardano.CanHardFork (
    ByronPartialLedgerConfig (..)
  , CardanoHardForkConstraints
  , TriggerHardFork (..)
    -- * Re-exports of Shelley code
  , ShelleyPartialLedgerConfig (..)
  , forecastAcrossShelley
  , crossEraTickChainDepStateAcrossShelley
  ) where

import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Update as CC.Update
import           Cardano.Crypto.DSIGN (Ed25519DSIGN)
import           Cardano.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)
import           Cardano.Ledger.Crypto (ADDRHASH, Crypto, DSIGN, HASH)
import qualified Cardano.Ledger.Era as SL
import           Cardano.Ledger.Hashes (EraIndependentTxBody)
import           Cardano.Ledger.Keys (DSignable, Hash)
import qualified Cardano.Ledger.Shelley.API as SL
import           Cardano.Ledger.Shelley.Translation
                     (toFromByronTranslationContext)
import qualified Cardano.Protocol.TPraos.API as SL
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as SL
import qualified Cardano.Protocol.TPraos.Rules.Tickn as SL
import           Control.Monad
import           Control.Monad.Except (runExcept, throwError)
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe, mapMaybe)
import           Data.Proxy
import           Data.SOP.BasicFunctors
import           Data.SOP.InPairs (RequiringBoth (..), ignoringBoth)
import           Data.SOP.Strict (hpure)
import           Data.SOP.Tails (Tails (..))
import qualified Data.SOP.Tails as Tails
import           Data.Void (Void)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Byron.Ledger
import qualified Ouroboros.Consensus.Byron.Ledger.Inspect as Byron.Inspect
import           Ouroboros.Consensus.Byron.Node ()
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.History (Bound (boundSlot),
                     addSlots)
import           Ouroboros.Consensus.HardFork.Simple
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT (PBft, PBftCrypto)
import qualified Ouroboros.Consensus.Protocol.PBFT.State as PBftState
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import           Ouroboros.Consensus.Protocol.TPraos
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import           Ouroboros.Consensus.Protocol.Translate (TranslateProto)
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node ()
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()
import           Ouroboros.Consensus.Shelley.ShelleyHFC
import           Ouroboros.Consensus.Ticked (WhetherTickedOrNot (..))
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (eitherToMaybe)
import           Ouroboros.Consensus.Util.RedundantConstraints

{-------------------------------------------------------------------------------
  Figure out the transition point for Byron

  The Byron ledger defines the update 'State' in
  "Cardano.Chain.Update.Validation.Interface". The critical piece of state we
  need is

  > candidateProtocolUpdates :: ![CandidateProtocolUpdate]

  which are the update proposals that have been voted on, accepted, and
  endorsed, and now need to become stable. In `tryBumpVersion`
  ("Cardano.Chain.Update.Validation.Interface.ProtocolVersionBump") we
  find the candidates that are at least 'kUpdateStabilityParam' (@== 4k@) deep,
  and then construct

  > State
  > { nextProtocolVersion    = cpuProtocolVersion
  > , nextProtocolParameters = cpuProtocolParameters
  > }

  (with 'State' from "Cardano.Chain.Update.Validation.Interface.ProtocolVersionBump")
  where 'cpuProtocolVersion'/'cpuProtocolParameters' are the version and
  parameters from the update. This then ends up in the following callstack

  > applyChainTick
  > |
  > \-- epochTransition
  >     |
  >     \-- registerEpoch
  >         |
  >         \-- tryBumpVersion

  Now, if this is changing the major version of the protocol, then this actually
  indicates the transition to Shelley, and the Byron 'applyChainTick' won't
  actually happen. Instead, in 'singleEraTransition' we will report the
  'EpochNo' of the transition as soon as it's @2k@ (not @4k@!) deep: in other
  words, as soon as it is stable; at this point, the HFC will do the rest.

  A slightly subtle point is that the Byron ledger does not record any
  information about /past/ updates to the protocol parameters, and so if we
  /were/ to ask the Byron ledger /after/ the update when the transition is
  going to take place (did take place), it will say 'Nothing': transition not
  yet known. In practice this won't matter, as it will have been translated to
  a Shelley ledger at that point.
-------------------------------------------------------------------------------}

byronTransition :: PartialLedgerConfig ByronBlock
                -> Word16   -- ^ Shelley major protocol version
                -> WhetherTickedOrNot (LedgerState ByronBlock)
                -> Maybe EpochNo
byronTransition ByronPartialLedgerConfig{..} shelleyMajorVersion wtState =
      takeAny
    . mapMaybe isTransitionToShelley
    . Byron.Inspect.protocolUpdates byronLedgerConfig
    $ wtState
  where
    ByronTransitionInfo transitionInfo = case wtState of
      NoTicked  st -> byronLedgerTransition st
      YesTicked st -> untickedByronLedgerTransition st

    genesis = byronLedgerConfig
    k       = CC.Genesis.gdK $ CC.Genesis.configGenesisData genesis

    isTransitionToShelley :: Byron.Inspect.ProtocolUpdate -> Maybe EpochNo
    isTransitionToShelley update = do
        guard $ CC.Update.pvMajor version == shelleyMajorVersion
        case Byron.Inspect.protocolUpdateState update of
          Byron.Inspect.UpdateCandidate _becameCandidateSlotNo adoptedIn -> do
            becameCandidateBlockNo <- Map.lookup version transitionInfo
            guard $ isReallyStable becameCandidateBlockNo
            return adoptedIn
          Byron.Inspect.UpdateStableCandidate adoptedIn ->
            -- If the Byron ledger thinks it's stable, it's _definitely_ stable
            return adoptedIn
          _otherwise ->
            -- The proposal isn't yet a candidate, never mind a stable one
            mzero
      where
        version :: CC.Update.ProtocolVersion
        version = Byron.Inspect.protocolUpdateVersion update

    -- Normally, stability in the ledger is defined in terms of slots, not
    -- blocks. Byron considers the proposal to be stable after the slot is more
    -- than @2k@ old. That is not wrong: after @2k@, the block indeed is stable.
    --
    -- Unfortunately, this means that the /conclusion about stability itself/
    -- is /not/ stable: if we were to switch to a denser fork, we might change
    -- our mind (on the sparse chain we thought the block was already stable,
    -- but on the dense chain we conclude it is it not yet stable).
    --
    -- It is unclear at the moment if this presents a problem; the HFC assumes
    -- monotonicity of timing info, in the sense that that any slot/time
    -- conversions are either unknown or else not subject to rollback.
    -- The problem sketched above might mean that we can go from "conversion
    -- known" to "conversion unknown", but then when we go back again to
    -- "conversion known", we /are/ guaranteed that we'd get the same answer.
    --
    -- Rather than trying to analyse this subtle problem, we instead base
    -- stability on block numbers; after the block is `k` deep, we know for sure
    -- that it is stable, and moreover, no matter which chain we switch to, that
    -- will remain to be the case.
    --
    -- The Byron 'UpdateState' records the 'SlotNo' of the block in which the
    -- proposal became a candidate (i.e., when the last required endorsement
    -- came in). That doesn't tell us very much, we need to know the block
    -- number; that's precisely what the 'ByronTransition' part of the Byron
    -- state tells us.
    isReallyStable :: BlockNo -> Bool
    isReallyStable (BlockNo bno) = distance >= CC.unBlockCount k
      where
        distance :: Word64
        distance = case rootBno of
                     Origin                  -> bno + 1
                     NotOrigin (BlockNo tip) -> tip - bno

    rootBno :: WithOrigin BlockNo
    rootBno = case wtState of
        NoTicked  st -> byronLedgerTipBlockNo st
        YesTicked st -> untickedByronLedgerTipBlockNo st

    -- We only expect a single proposal that updates to Shelley, but in case
    -- there are multiple, any one will do
    takeAny :: [a] -> Maybe a
    takeAny = listToMaybe

{-------------------------------------------------------------------------------
  SingleEraBlock Byron
-------------------------------------------------------------------------------}

instance SingleEraBlock ByronBlock where
  singleEraTransition pcfg _eraParams _eraStart ledgerState =
      case byronTriggerHardFork pcfg of
        TriggerHardForkNever                         -> Nothing
        TriggerHardForkAtEpoch   epoch               -> Just epoch
        TriggerHardForkAtVersion shelleyMajorVersion ->
            byronTransition
              pcfg
              shelleyMajorVersion
              ledgerState

  singleEraInfo _ = SingleEraInfo {
      singleEraName = "Byron"
    }

instance PBftCrypto bc => HasPartialConsensusConfig (PBft bc)
  -- Use defaults

-- | When Byron is part of the hard-fork combinator, we use the partial ledger
-- config. Standalone Byron uses the regular ledger config. This means that
-- the partial ledger config is the perfect place to store the trigger
-- condition for the hard fork to Shelley, as we don't have to modify the
-- ledger config for standalone Byron.
data ByronPartialLedgerConfig = ByronPartialLedgerConfig {
      byronLedgerConfig    :: !(LedgerConfig ByronBlock)
    , byronTriggerHardFork :: !TriggerHardFork
    }
  deriving (Generic, NoThunks)

instance HasPartialLedgerConfig ByronBlock where

  type PartialLedgerConfig ByronBlock = ByronPartialLedgerConfig

  completeLedgerConfig _ _ = byronLedgerConfig

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
  hardForkEraTranslation = EraTranslation {
      crossEraTickLedgerState =
          PCons crossEraTickLedgerStateByronToShelley
        $ PCons crossEraTickLedgerStateAcrossTPraos
        $ PCons crossEraTickLedgerStateAcrossTPraos
        $ PCons crossEraTickLedgerStateAcrossTPraos
        $ PCons crossEraTickLedgerStateTPraosToPraos
        $ PCons crossEraTickLedgerStateAcrossPraos
        $ PNil
    , crossEraTickChainDepState =
          PCons crossEraTickChainDepStateByronToShelley
        $ PCons crossEraTickChainDepStateAcrossShelley
        $ PCons crossEraTickChainDepStateAcrossShelley
        $ PCons crossEraTickChainDepStateAcrossShelley
        $ PCons crossEraTickChainDepStateAcrossShelley
        $ PCons crossEraTickChainDepStateAcrossShelley
        $ PNil
    , crossEraForecast =
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
                let ctxt = shelleyLedgerTranslationContext $ unwrapLedgerConfig cfgAlonzo
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
                let ctxt = shelleyLedgerTranslationContext $ unwrapLedgerConfig cfgConway
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

crossEraTickLedgerStateByronToShelley ::
     ( ShelleyCompatible (TPraos c) (ShelleyEra c)
     , HASH     c ~ Blake2b_256
     , ADDRHASH c ~ Blake2b_224
     )
  => CrossEraTickLedgerState
       ByronBlock
       (ShelleyBlock (TPraos c) (ShelleyEra c))
crossEraTickLedgerStateByronToShelley =
    CrossEraTickLedgerState
  $ \_byronCfg cfgShelley epochNo sno ledgerByron ->
      let st = ShelleyLedgerState {
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
      in
      -- Somewhat unintuitively: the Byron ledger rule for ticking across the
      -- epoch transition is not invoked.
      --
      -- The epoch number in the initial NewEpochState is the only sense in
      -- which it is "younger" than the ledger state immediately after the
      -- final Byron block.
      pureLedgerResult $ applyChainTickLedgerResult cfgShelley sno st

crossEraTickChainDepStateByronToShelley ::
     PraosCrypto c
  => CrossEraTickChainDepState
       ByronBlock
       (ShelleyBlock (TPraos c) (ShelleyEra c))
crossEraTickChainDepStateByronToShelley =
    CrossEraTickChainDepState
  $ \Proxy Proxy _byronCfg cfgShelley ledgerViewShelley _epochNo sno pbftState ->
    let TPraosConfig { tpraosParams } = cfgShelley

        nonce = tpraosInitialNonce tpraosParams

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
        st = TPraosState (PBftState.lastSignedSlot pbftState) $
               SL.ChainDepState
                 { SL.csProtocol = SL.PrtclState Map.empty nonce nonce
                 , SL.csTickn    = SL.TicknState {
                       ticknStateEpochNonce    = nonce
                     , ticknStatePrevHashNonce = SL.NeutralNonce
                     }
                   -- Overridden before used
                 , SL.csLabNonce = SL.NeutralNonce
                 }
    in
    tickChainDepState cfgShelley ledgerViewShelley sno st

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
           (Ticked (WrapLedgerView (ShelleyBlock (TPraos c) (ShelleyEra c))))
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

crossEraTickLedgerStateAcrossTPraos ::
     ( ShelleyBasedEra (SL.PreviousEra toEra)
     , ShelleyBasedEra toEra
     , SL.TranslateEra toEra SL.NewEpochState
     , Void ~ SL.TranslationError toEra SL.NewEpochState
     , EraCrypto toEra ~ EraCrypto (SL.PreviousEra toEra)
     )
  => CrossEraTickLedgerState
       (ShelleyBlock (TPraos c) (SL.PreviousEra toEra))
       (ShelleyBlock (TPraos c) toEra)
crossEraTickLedgerStateAcrossTPraos =
    CrossEraTickLedgerState
  $ \_cfg cfg' _epochNo sno st ->
      -- Somewhat unintuitively: the first era's ledger rule for ticking across
      -- the epoch transition is not invoked.
        pureLedgerResult
      $ applyChainTickLedgerResult cfg' sno
      $ unComp
      $ SL.translateEra' (shelleyLedgerTranslationContext cfg')
      $ Comp st

crossEraTickLedgerStateTPraosToPraos ::
     ( ShelleyBasedEra (SL.PreviousEra toEra)
     , ShelleyBasedEra toEra
     , SL.TranslateEra toEra SL.NewEpochState
     , Void ~ SL.TranslationError toEra SL.NewEpochState
     , EraCrypto toEra ~ EraCrypto (SL.PreviousEra toEra)
     )
  => CrossEraTickLedgerState
       (ShelleyBlock (TPraos c) (SL.PreviousEra toEra))
       (ShelleyBlock (Praos c) toEra)
crossEraTickLedgerStateTPraosToPraos =
    CrossEraTickLedgerState
  $ \_cfg cfg' _epochNo sno st ->
      -- Somewhat unintuitively: the first era's ledger rule for ticking across
      -- the epoch transition is not invoked.
        pureLedgerResult
      $ applyChainTickLedgerResult cfg' sno
      $ unComp
      $ SL.translateEra' (shelleyLedgerTranslationContext cfg')
      $ Comp
      $ transPraosLS st
  where
    transPraosLS ::
      LedgerState (ShelleyBlock (TPraos c) era) ->
      LedgerState (ShelleyBlock (Praos c)  era)
    transPraosLS (ShelleyLedgerState wo nes st) =
      ShelleyLedgerState
        { shelleyLedgerTip        = fmap castShelleyTip wo
        , shelleyLedgerState      = nes
        , shelleyLedgerTransition = st
        }

crossEraTickLedgerStateAcrossPraos ::
     ( ShelleyBasedEra (SL.PreviousEra toEra)
     , ShelleyBasedEra toEra
     , SL.TranslateEra toEra SL.NewEpochState
     , Void ~ SL.TranslationError toEra SL.NewEpochState
     , EraCrypto toEra ~ EraCrypto (SL.PreviousEra toEra)
     )
  => CrossEraTickLedgerState
       (ShelleyBlock (Praos (EraCrypto (SL.PreviousEra toEra))) (SL.PreviousEra toEra))
       (ShelleyBlock (Praos (EraCrypto                 toEra)) toEra)
crossEraTickLedgerStateAcrossPraos =
    CrossEraTickLedgerState
  $ \_cfg cfg' _epochNo sno st ->
      -- Somewhat unintuitively: the first era's ledger rule for ticking across
      -- the epoch transition is not invoked.
        pureLedgerResult
      $ applyChainTickLedgerResult cfg' sno
      $ unComp
      $ SL.translateEra' (shelleyLedgerTranslationContext cfg')
      $ Comp st

translateTxShelleyToAllegraWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => InjectTx
       (ShelleyBlock (TPraos c) (ShelleyEra c))
       (ShelleyBlock (TPraos c) (AllegraEra c))
translateTxShelleyToAllegraWrapper = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

translateValidatedTxShelleyToAllegraWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => InjectValidatedTx
       (ShelleyBlock (TPraos c) (ShelleyEra c))
       (ShelleyBlock (TPraos c) (AllegraEra c))
translateValidatedTxShelleyToAllegraWrapper = InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

{-------------------------------------------------------------------------------
  Translation from Allegra to Mary
-------------------------------------------------------------------------------}

translateTxAllegraToMaryWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => InjectTx
       (ShelleyBlock (TPraos c) (AllegraEra c))
       (ShelleyBlock (TPraos c) (MaryEra c))
translateTxAllegraToMaryWrapper = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

translateValidatedTxAllegraToMaryWrapper ::
     (PraosCrypto c, DSignable c (Hash c EraIndependentTxBody))
  => InjectValidatedTx
       (ShelleyBlock (TPraos c) (AllegraEra c))
       (ShelleyBlock (TPraos c) (MaryEra c))
translateValidatedTxAllegraToMaryWrapper = InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

{-------------------------------------------------------------------------------
  Translation from Mary to Alonzo
-------------------------------------------------------------------------------}

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
