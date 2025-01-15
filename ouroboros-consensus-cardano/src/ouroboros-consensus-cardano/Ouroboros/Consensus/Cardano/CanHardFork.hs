{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Cardano.CanHardFork (
    ByronPartialLedgerConfig (..)
  , CardanoHardForkConstraints
  , TriggerHardFork (..)
    -- * Re-exports of Shelley code
  , ShelleyPartialLedgerConfig (..)
  , crossEraForecastAcrossShelley
  , translateChainDepStateAcrossShelley
    -- * Exposed for testing
  , getConwayTranslationContext
  ) where


import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.Update as CC.Update
import           Cardano.Crypto.DSIGN (Ed25519DSIGN)
import           Cardano.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)
import           Cardano.Ledger.Allegra.Translation
                     (shelleyToAllegraAVVMsToDelete)
import qualified Cardano.Ledger.BaseTypes as SL
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
import           Control.Monad
import           Control.Monad.Except (runExcept, throwError)
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe, mapMaybe)
import           Data.Proxy
import           Data.SOP.BasicFunctors
import           Data.SOP.Functors (Flip (..))
import           Data.SOP.InPairs (RequiringBoth (..), ignoringBoth)
import qualified Data.SOP.Strict as SOP
import           Data.SOP.Tails (Tails (..))
import qualified Data.SOP.Tails as Tails
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
import           Ouroboros.Consensus.Ledger.SupportsMempool (ByteSize32,
                     IgnoringOverflow, TxMeasure)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT (PBft, PBftCrypto)
import           Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import qualified Ouroboros.Consensus.Protocol.PBFT.State as PBftState
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import           Ouroboros.Consensus.Protocol.TPraos
import qualified Ouroboros.Consensus.Protocol.TPraos as TPraos
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node ()
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()
import           Ouroboros.Consensus.Shelley.ShelleyHFC
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
                -> LedgerState ByronBlock mk
                -> Maybe EpochNo
byronTransition ByronPartialLedgerConfig{..} shelleyMajorVersion state =
      takeAny
    . mapMaybe isTransitionToShelley
    . Byron.Inspect.protocolUpdates byronLedgerConfig
    $ state
  where
    ByronTransitionInfo transitionInfo = byronLedgerTransition state

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
        distance = case byronLedgerTipBlockNo state of
                     Origin                  -> bno + 1
                     NotOrigin (BlockNo tip) -> tip - bno

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
        TriggerHardForkNotDuringThisExecution        -> Nothing
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
    , translateLedgerTables  =
          PCons translateLedgerTablesByronToShelleyWrapper
        $ PCons translateLedgerTablesShelleyToAllegraWrapper
        $ PCons translateLedgerTablesAllegraToMaryWrapper
        $ PCons translateLedgerTablesMaryToAlonzoWrapper
        $ PCons translateLedgerTablesAlonzoToBabbageWrapper
        $ PCons translateLedgerTablesBabbageToConwayWrapper
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
       TranslateLedgerState
       ByronBlock
       (ShelleyBlock (TPraos c) (ShelleyEra c))
translateLedgerStateByronToShelleyWrapper =
      RequireBoth
    $ \_ (WrapLedgerConfig cfgShelley) ->
        TranslateLedgerState {
            translateLedgerStateWith = \epochNo ledgerByron ->
                valuesAsDiffs
              . unstowLedgerTables
              $ ShelleyLedgerState {
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

translateLedgerTablesByronToShelleyWrapper ::
     TranslateLedgerTables ByronBlock (ShelleyBlock (TPraos c) (ShelleyEra c))
translateLedgerTablesByronToShelleyWrapper = TranslateLedgerTables undefined

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
              ticknStateEpochNonce    = nonce
            , ticknStatePrevHashNonce = SL.NeutralNonce
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
      -> LedgerState ByronBlock mk
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
       TranslateLedgerState
       (ShelleyBlock (TPraos c) (ShelleyEra c))
       (ShelleyBlock (TPraos c) (AllegraEra c))
translateLedgerStateShelleyToAllegraWrapper =
    ignoringBoth $
      TranslateLedgerState {
          translateLedgerStateWith = \_epochNo ls ->
              -- In the Shelley to Allegra transition, the AVVM addresses have
              -- to be deleted, and their balance has to be moved to the
              -- reserves. For this matter, the Ledger keeps track of these
              -- small set of entries since the Byron to Shelley transition and
              -- provides them to us through 'shelleyToAllegraAVVMsToDelete'.
              --
              -- In the long run, the ledger will already use ledger states
              -- parametrized by the map kind and therefore will already provide
              -- the differences in this translation.
              let avvms           = SL.unUTxO
                                  $ shelleyToAllegraAVVMsToDelete
                                  $ shelleyLedgerState ls

                  -- While techically we can diff the LedgerTables, it becomes
                  -- complex doing so, as we cannot perform operations with
                  -- 'LedgerTables l1 mk' and 'LedgerTables l2 mk'. Because of
                  -- this, for now we choose to generate the differences out of
                  -- thin air as we know that in this era translation these are
                  -- the only differences produced.
                  --
                  -- When adding more tables, this decision might need to be
                  -- revisited, as there might be other diffs produced in the
                  -- translation.
                  avvmsAsDeletions = ShelleyLedgerTables
                                   . DiffMK
                                   . Diff.fromMapDeletes
                                   . Map.mapKeys ShelleyTxIn
                                   . Map.map SL.upgradeTxOut
                                   $ avvms

                  -- This 'stowLedgerTables' + 'withLedgerTables' injects the
                  -- values provided by the Ledger so that the translation
                  -- operation finds those entries in the UTxO and destroys
                  -- them, modifying the reserves accordingly.
                  stowedState = stowLedgerTables
                              . withLedgerTables ls
                              . ShelleyLedgerTables
                              . ValuesMK
                              . Map.mapKeys ShelleyTxIn
                              $ avvms

                  resultingState = unFlip . unComp
                                 . SL.translateEra' SL.NoGenesis
                                 . Comp   . Flip
                                 $ stowedState

              in resultingState `withLedgerTables` avvmsAsDeletions
        }

translateLedgerTablesShelleyToAllegraWrapper ::
     forall c. PraosCrypto c
  => TranslateLedgerTables
      (ShelleyBlock (TPraos c) (ShelleyEra c))
       (ShelleyBlock (TPraos c) (AllegraEra c))
translateLedgerTablesShelleyToAllegraWrapper = TranslateLedgerTables
  $ \(ShelleyLedgerTables mk) ->
     ShelleyLedgerTables $ mapKeysMK coerce . mapMK SL.upgradeTxOut $ mk

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
       TranslateLedgerState
       (ShelleyBlock (TPraos c) (AllegraEra c))
       (ShelleyBlock (TPraos c) (MaryEra c))
translateLedgerStateAllegraToMaryWrapper =
    ignoringBoth $
      TranslateLedgerState {
          translateLedgerStateWith = \_epochNo ->
                noNewTickingDiffs
              . unFlip
              . unComp
              . SL.translateEra' SL.NoGenesis
              . Comp
              . Flip
        }

translateLedgerTablesAllegraToMaryWrapper ::
     PraosCrypto c
  => TranslateLedgerTables
       (ShelleyBlock (TPraos c) (AllegraEra c))
       (ShelleyBlock (TPraos c) (MaryEra c))
translateLedgerTablesAllegraToMaryWrapper = TranslateLedgerTables
  $ \(ShelleyLedgerTables mk) ->
     ShelleyLedgerTables $ mapKeysMK coerce . mapMK SL.upgradeTxOut $ mk

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
       TranslateLedgerState
       (ShelleyBlock (TPraos c) (MaryEra c))
       (ShelleyBlock (TPraos c) (AlonzoEra c))
translateLedgerStateMaryToAlonzoWrapper =
    RequireBoth $ \_cfgMary cfgAlonzo ->
      TranslateLedgerState {
          translateLedgerStateWith = \_epochNo ->
                noNewTickingDiffs
              . unFlip
              . unComp
              . SL.translateEra' (getAlonzoTranslationContext cfgAlonzo)
              . Comp
              . Flip
        }

translateLedgerTablesMaryToAlonzoWrapper ::
     PraosCrypto c
  => TranslateLedgerTables
       (ShelleyBlock (TPraos c) (MaryEra c))
       (ShelleyBlock (TPraos c) (AlonzoEra c))
translateLedgerTablesMaryToAlonzoWrapper = TranslateLedgerTables
  $ \(ShelleyLedgerTables mk) ->
     ShelleyLedgerTables $ mapKeysMK coerce . mapMK SL.upgradeTxOut $ mk

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
       TranslateLedgerState
       (ShelleyBlock (TPraos c) (AlonzoEra c))
       (ShelleyBlock (Praos c) (BabbageEra c))
translateLedgerStateAlonzoToBabbageWrapper =
  RequireBoth $ \_cfgAlonzo _cfgBabbage ->
      TranslateLedgerState {
          translateLedgerStateWith = \_epochNo ->
                noNewTickingDiffs
              . unFlip
              . unComp
              . SL.translateEra' SL.NoGenesis
              . Comp
              . Flip
              . transPraosLS
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

translateLedgerTablesAlonzoToBabbageWrapper ::
     Praos.PraosCrypto c
  => TranslateLedgerTables
       (ShelleyBlock (TPraos c) (AlonzoEra c))
       (ShelleyBlock (Praos c) (BabbageEra c))
translateLedgerTablesAlonzoToBabbageWrapper = TranslateLedgerTables
  $ \(ShelleyLedgerTables mk) ->
     ShelleyLedgerTables $ mapKeysMK coerce . mapMK SL.upgradeTxOut $ mk

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
     forall c. (Praos.PraosCrypto c)
  => RequiringBoth
      WrapLedgerConfig
      TranslateLedgerState
      (ShelleyBlock (Praos c) (BabbageEra c))
      (ShelleyBlock (Praos c) (ConwayEra c))
translateLedgerStateBabbageToConwayWrapper =
  RequireBoth $ \_cfgBabbage cfgConway ->
      TranslateLedgerState {
          translateLedgerStateWith = \_epochNo ->
                noNewTickingDiffs
              . unFlip
              . unComp
              . SL.translateEra' (getConwayTranslationContext cfgConway)
              . Comp
              . Flip
        }

translateLedgerTablesBabbageToConwayWrapper ::
     Praos.PraosCrypto c
  => TranslateLedgerTables
       (ShelleyBlock (Praos c) (BabbageEra c))
       (ShelleyBlock (Praos c) (ConwayEra c))
translateLedgerTablesBabbageToConwayWrapper = TranslateLedgerTables
  $ \(ShelleyLedgerTables mk) ->
     ShelleyLedgerTables $ mapKeysMK coerce . mapMK SL.upgradeTxOut $ mk

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
