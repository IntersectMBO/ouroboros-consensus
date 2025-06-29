{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.Ledger
  ( LedgerState (..)
  , LedgerTables (..)
  , ShelleyBasedEra
  , ShelleyTip (..)
  , ShelleyTransition (..)
  , Ticked (..)
  , castShelleyTip
  , shelleyLedgerTipPoint
  , shelleyTipToPoint

    -- * Ledger config
  , ShelleyLedgerConfig (..)
  , ShelleyPartialLedgerConfig (..)
  , mkShelleyLedgerConfig
  , shelleyEraParams
  , shelleyEraParamsNeverHardForks
  , shelleyLedgerGenesis

    -- * Auxiliary
  , ShelleyLedgerEvent (..)
  , ShelleyReapplyException (..)
  , getPParams

    -- * Serialisation
  , decodeShelleyAnnTip
  , decodeShelleyLedgerState
  , encodeShelleyAnnTip
  , encodeShelleyHeaderState
  , encodeShelleyLedgerState

    -- * Low-level UTxO manipulations
  , slUtxoL
  ) where

import qualified Cardano.Ledger.BHeaderView as SL (BHeaderView)
import qualified Cardano.Ledger.BaseTypes as SL (epochInfoPure)
import Cardano.Ledger.BaseTypes.NonZero (unNonZero)
import Cardano.Ledger.Binary.Decoding
  ( decShareCBOR
  , decodeMap
  , decodeMemPack
  , internsFromMap
  )
import Cardano.Ledger.Binary.Encoding
  ( encodeMap
  , encodeMemPack
  , toPlainEncoding
  )
import Cardano.Ledger.Binary.Plain
  ( FromCBOR (..)
  , ToCBOR (..)
  , enforceSize
  )
import qualified Cardano.Ledger.Block as Core
import Cardano.Ledger.Core
  ( Era
  , eraDecoder
  , ppMaxBHSizeL
  , ppMaxTxSizeL
  )
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.Governance as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Cardano.Ledger.UMap as SL
import Cardano.Slotting.EpochInfo
import Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise (decode, encode)
import Control.Arrow (left, second)
import qualified Control.Exception as Exception
import Control.Monad.Except
import qualified Control.State.Transition.Extended as STS
import Data.Coerce (coerce)
import Data.Functor.Identity
import Data.MemPack
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Word
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Extras (view)
import NoThunks.Class (NoThunks (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.HardFork.History.Util
import Ouroboros.Consensus.HardFork.Simple
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.CommonProtocolParams
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Protocol.Ledger.Util (isNewEpoch)
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Consensus.Shelley.Ledger.Config
import Ouroboros.Consensus.Shelley.Ledger.Protocol ()
import Ouroboros.Consensus.Shelley.Protocol.Abstract
  ( EnvelopeCheckError
  , envelopeChecks
  , mkHeaderView
  )
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.Util.CBOR
  ( decodeWithOrigin
  , encodeWithOrigin
  )
import Ouroboros.Consensus.Util.IndexedMemPack
import Ouroboros.Consensus.Util.Versioned

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

data ShelleyLedgerConfig era = ShelleyLedgerConfig
  { shelleyLedgerCompactGenesis :: !CompactGenesis
  , shelleyLedgerGlobals :: !SL.Globals
  -- ^ Derived from 'shelleyLedgerGenesis' but we store a cached version
  -- because it used very often.
  , shelleyLedgerTranslationContext :: !(Core.TranslationContext era)
  }
  deriving Generic

deriving instance
  (NoThunks (Core.TranslationContext era), Era era) =>
  NoThunks (ShelleyLedgerConfig era)

deriving instance Show (Core.TranslationContext era) => Show (ShelleyLedgerConfig era)

shelleyLedgerGenesis :: ShelleyLedgerConfig era -> SL.ShelleyGenesis
shelleyLedgerGenesis = getCompactGenesis . shelleyLedgerCompactGenesis

shelleyEraParams ::
  SL.ShelleyGenesis ->
  HardFork.EraParams
shelleyEraParams genesis =
  HardFork.EraParams
    { eraEpochSize = SL.sgEpochLength genesis
    , eraSlotLength = mkSlotLength $ SL.fromNominalDiffTimeMicro $ SL.sgSlotLength genesis
    , eraSafeZone = HardFork.StandardSafeZone stabilityWindow
    , eraGenesisWin = GenesisWindow stabilityWindow
    }
 where
  stabilityWindow =
    SL.computeStabilityWindow
      (unNonZero $ SL.sgSecurityParam genesis)
      (SL.sgActiveSlotCoeff genesis)

-- | Separate variant of 'shelleyEraParams' to be used for a Shelley-only chain.
shelleyEraParamsNeverHardForks :: SL.ShelleyGenesis -> HardFork.EraParams
shelleyEraParamsNeverHardForks genesis =
  HardFork.EraParams
    { eraEpochSize = SL.sgEpochLength genesis
    , eraSlotLength = mkSlotLength $ SL.fromNominalDiffTimeMicro $ SL.sgSlotLength genesis
    , eraSafeZone = HardFork.UnsafeIndefiniteSafeZone
    , eraGenesisWin = GenesisWindow stabilityWindow
    }
 where
  stabilityWindow =
    SL.computeStabilityWindow
      (unNonZero $ SL.sgSecurityParam genesis)
      (SL.sgActiveSlotCoeff genesis)

mkShelleyLedgerConfig ::
  SL.ShelleyGenesis ->
  Core.TranslationContext era ->
  EpochInfo (Except HardFork.PastHorizonException) ->
  ShelleyLedgerConfig era
mkShelleyLedgerConfig genesis transCtxt epochInfo =
  ShelleyLedgerConfig
    { shelleyLedgerCompactGenesis = compactGenesis genesis
    , shelleyLedgerGlobals =
        SL.mkShelleyGlobals
          genesis
          (hoistEpochInfo (left (Text.pack . show) . runExcept) epochInfo)
    , shelleyLedgerTranslationContext = transCtxt
    }

type instance LedgerCfg (LedgerState (ShelleyBlock proto era)) = ShelleyLedgerConfig era

data ShelleyPartialLedgerConfig era = ShelleyPartialLedgerConfig
  { shelleyLedgerConfig :: !(ShelleyLedgerConfig era)
  -- ^ We cache the non-partial ledger config containing a dummy
  -- 'EpochInfo' that needs to be replaced with the correct one.
  --
  -- We do this to avoid recomputing the ledger config each time
  -- 'completeLedgerConfig' is called, as 'mkShelleyLedgerConfig' does
  -- some rather expensive computations that shouldn't be repeated too
  -- often (e.g., 'sgActiveSlotCoeff').
  , shelleyTriggerHardFork :: !TriggerHardFork
  }
  deriving Generic

deriving instance Show (ShelleyLedgerConfig era) => Show (ShelleyPartialLedgerConfig era)

deriving instance
  (NoThunks (Core.TranslationContext era), Core.Era era) =>
  NoThunks (ShelleyPartialLedgerConfig era)

instance ShelleyCompatible proto era => HasPartialLedgerConfig (ShelleyBlock proto era) where
  type PartialLedgerConfig (ShelleyBlock proto era) = ShelleyPartialLedgerConfig era

  -- Replace the dummy 'EpochInfo' with the real one
  completeLedgerConfig _ epochInfo (ShelleyPartialLedgerConfig cfg _) =
    cfg
      { shelleyLedgerGlobals =
          (shelleyLedgerGlobals cfg)
            { SL.epochInfo =
                hoistEpochInfo
                  (runExcept . withExceptT (T.pack . show))
                  epochInfo
            }
      }

{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

data ShelleyTip proto era = ShelleyTip
  { shelleyTipSlotNo :: !SlotNo
  , shelleyTipBlockNo :: !BlockNo
  , shelleyTipHash :: !(HeaderHash (ShelleyBlock proto era))
  }
  deriving (Eq, Show, Generic, NoThunks)

shelleyTipToPoint :: WithOrigin (ShelleyTip proto era) -> Point (ShelleyBlock proto era)
shelleyTipToPoint Origin = GenesisPoint
shelleyTipToPoint (NotOrigin tip) =
  BlockPoint
    (shelleyTipSlotNo tip)
    (shelleyTipHash tip)

castShelleyTip :: ShelleyTip proto era -> ShelleyTip proto' era'
castShelleyTip (ShelleyTip sn bn hh) =
  ShelleyTip
    { shelleyTipSlotNo = sn
    , shelleyTipBlockNo = bn
    , shelleyTipHash = coerce hh
    }

data instance LedgerState (ShelleyBlock proto era) mk = ShelleyLedgerState
  { shelleyLedgerTip :: !(WithOrigin (ShelleyTip proto era))
  , shelleyLedgerState :: !(SL.NewEpochState era)
  , shelleyLedgerTransition :: !ShelleyTransition
  , shelleyLedgerTables :: !(LedgerTables (LedgerState (ShelleyBlock proto era)) mk)
  }
  deriving Generic

deriving instance
  (ShelleyBasedEra era, EqMK mk) =>
  Eq (LedgerState (ShelleyBlock proto era) mk)
deriving instance
  (ShelleyBasedEra era, NoThunksMK mk) =>
  NoThunks (LedgerState (ShelleyBlock proto era) mk)
deriving instance
  (ShelleyBasedEra era, ShowMK mk) =>
  Show (LedgerState (ShelleyBlock proto era) mk)

-- | Information required to determine the hard fork point from Shelley to the
-- next ledger
newtype ShelleyTransition = ShelleyTransitionInfo
  { shelleyAfterVoting :: Word32
  -- ^ The number of blocks in this epoch past the voting deadline
  --
  -- We record this to make sure that we can tell the HFC about hard forks
  -- if and only if we are certain:
  --
  -- 1. Blocks that came in within an epoch after the 4k/f voting deadline
  --    are not relevant (10k/f - 2 * 3k/f).
  -- 2. Since there are slots between blocks, we are probably only sure that
  --    there will be no more relevant block when we have seen the first
  --    block after the deadline.
  -- 3. If we count how many blocks we have seen post deadline, and we have
  --    reached k of them, we know that that last pre-deadline block won't
  --    be rolled back anymore.
  -- 4. At this point we can look at the ledger state and see if there is
  --    a new protocol version update scheduled on the next epoch boundary,
  --    and notify the HFC that we need to transition into a new era at that
  --    point.
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype NoThunks

shelleyLedgerTipPoint ::
  LedgerState (ShelleyBlock proto era) mk ->
  Point (ShelleyBlock proto era)
shelleyLedgerTipPoint = shelleyTipToPoint . shelleyLedgerTip

instance ShelleyCompatible proto era => UpdateLedger (ShelleyBlock proto era)

type instance TxIn (LedgerState (ShelleyBlock proto era)) = SL.TxIn
type instance TxOut (LedgerState (ShelleyBlock proto era)) = Core.TxOut era

instance
  (txout ~ Core.TxOut era, MemPack txout) =>
  IndexedMemPack (LedgerState (ShelleyBlock proto era) EmptyMK) txout
  where
  indexedTypeName _ = typeName @txout
  indexedPackedByteCount _ = packedByteCount
  indexedPackM _ = packM
  indexedUnpackM _ = unpackM

instance
  ShelleyCompatible proto era =>
  SerializeTablesWithHint (LedgerState (ShelleyBlock proto era))
  where
  encodeTablesWithHint _ (LedgerTables (ValuesMK tbs)) =
    toPlainEncoding (Core.eraProtVerLow @era) $ encodeMap encodeMemPack encodeMemPack tbs
  decodeTablesWithHint st =
    let certInterns =
          internsFromMap $
            shelleyLedgerState st
              ^. SL.nesEsL
                . SL.esLStateL
                . SL.lsCertStateL
                . SL.certDStateL
                . SL.dsUnifiedL
                . SL.umElemsL
     in LedgerTables . ValuesMK <$> (eraDecoder @era $ decodeMap decodeMemPack (decShareCBOR certInterns))

instance
  ShelleyBasedEra era =>
  HasLedgerTables (LedgerState (ShelleyBlock proto era))
  where
  projectLedgerTables = shelleyLedgerTables
  withLedgerTables st tables =
    ShelleyLedgerState
      { shelleyLedgerTip
      , shelleyLedgerState
      , shelleyLedgerTransition
      , shelleyLedgerTables = tables
      }
   where
    ShelleyLedgerState
      { shelleyLedgerTip
      , shelleyLedgerState
      , shelleyLedgerTransition
      } = st

instance
  ShelleyBasedEra era =>
  HasLedgerTables (Ticked (LedgerState (ShelleyBlock proto era)))
  where
  projectLedgerTables = castLedgerTables . tickedShelleyLedgerTables
  withLedgerTables st tables =
    TickedShelleyLedgerState
      { untickedShelleyLedgerTip
      , tickedShelleyLedgerTransition
      , tickedShelleyLedgerState
      , tickedShelleyLedgerTables = castLedgerTables tables
      }
   where
    TickedShelleyLedgerState
      { untickedShelleyLedgerTip
      , tickedShelleyLedgerTransition
      , tickedShelleyLedgerState
      } = st

instance
  ShelleyBasedEra era =>
  CanStowLedgerTables (LedgerState (ShelleyBlock proto era))
  where
  stowLedgerTables st =
    ShelleyLedgerState
      { shelleyLedgerTip = shelleyLedgerTip
      , shelleyLedgerState = shelleyLedgerState'
      , shelleyLedgerTransition = shelleyLedgerTransition
      , shelleyLedgerTables = emptyLedgerTables
      }
   where
    (_, shelleyLedgerState') = shelleyLedgerState `slUtxoL` SL.UTxO m
    ShelleyLedgerState
      { shelleyLedgerTip
      , shelleyLedgerState
      , shelleyLedgerTransition
      , shelleyLedgerTables = LedgerTables (ValuesMK m)
      } = st
  unstowLedgerTables st =
    ShelleyLedgerState
      { shelleyLedgerTip = shelleyLedgerTip
      , shelleyLedgerState = shelleyLedgerState'
      , shelleyLedgerTransition = shelleyLedgerTransition
      , shelleyLedgerTables = LedgerTables (ValuesMK (SL.unUTxO tbs))
      }
   where
    (tbs, shelleyLedgerState') = shelleyLedgerState `slUtxoL` mempty
    ShelleyLedgerState
      { shelleyLedgerTip
      , shelleyLedgerState
      , shelleyLedgerTransition
      } = st

instance
  ShelleyBasedEra era =>
  CanStowLedgerTables (Ticked (LedgerState (ShelleyBlock proto era)))
  where
  stowLedgerTables st =
    TickedShelleyLedgerState
      { untickedShelleyLedgerTip = untickedShelleyLedgerTip
      , tickedShelleyLedgerTransition = tickedShelleyLedgerTransition
      , tickedShelleyLedgerState = tickedShelleyLedgerState'
      , tickedShelleyLedgerTables = emptyLedgerTables
      }
   where
    (_, tickedShelleyLedgerState') =
      tickedShelleyLedgerState `slUtxoL` SL.UTxO tbs
    TickedShelleyLedgerState
      { untickedShelleyLedgerTip
      , tickedShelleyLedgerTransition
      , tickedShelleyLedgerState
      , tickedShelleyLedgerTables = LedgerTables (ValuesMK tbs)
      } = st

  unstowLedgerTables st =
    TickedShelleyLedgerState
      { untickedShelleyLedgerTip = untickedShelleyLedgerTip
      , tickedShelleyLedgerTransition = tickedShelleyLedgerTransition
      , tickedShelleyLedgerState = tickedShelleyLedgerState'
      , tickedShelleyLedgerTables = LedgerTables (ValuesMK (SL.unUTxO tbs))
      }
   where
    (tbs, tickedShelleyLedgerState') = tickedShelleyLedgerState `slUtxoL` mempty
    TickedShelleyLedgerState
      { untickedShelleyLedgerTip
      , tickedShelleyLedgerTransition
      , tickedShelleyLedgerState
      } = st

slUtxoL :: SL.NewEpochState era -> SL.UTxO era -> (SL.UTxO era, SL.NewEpochState era)
slUtxoL st vals =
  st
    & SL.nesEsL
      . SL.esLStateL
      . SL.lsUTxOStateL
      . SL.utxoL
      <<.~ vals

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

instance GetTip (LedgerState (ShelleyBlock proto era)) where
  getTip = castPoint . shelleyLedgerTipPoint

instance GetTip (Ticked (LedgerState (ShelleyBlock proto era))) where
  getTip = castPoint . untickedShelleyLedgerTipPoint

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

-- | Ticking only affects the state itself
data instance Ticked (LedgerState (ShelleyBlock proto era)) mk = TickedShelleyLedgerState
  { untickedShelleyLedgerTip :: !(WithOrigin (ShelleyTip proto era))
  , tickedShelleyLedgerTransition :: !ShelleyTransition
  -- ^ We are counting blocks within an epoch, this means:
  --
  -- 1. We are only incrementing this when /applying/ a block, not when ticking.
  -- 2. However, we count within an epoch, which is slot-based. So the count
  --    must be reset when /ticking/, not when applying a block.
  , tickedShelleyLedgerState :: !(SL.NewEpochState era)
  , tickedShelleyLedgerTables ::
      !(LedgerTables (LedgerState (ShelleyBlock proto era)) mk)
  }
  deriving Generic

untickedShelleyLedgerTipPoint ::
  TickedLedgerState (ShelleyBlock proto era) mk ->
  Point (ShelleyBlock proto era)
untickedShelleyLedgerTipPoint = shelleyTipToPoint . untickedShelleyLedgerTip

instance ShelleyBasedEra era => IsLedger (LedgerState (ShelleyBlock proto era)) where
  type LedgerErr (LedgerState (ShelleyBlock proto era)) = SL.BlockTransitionError era

  type AuxLedgerEvent (LedgerState (ShelleyBlock proto era)) = ShelleyLedgerEvent era

  applyChainTickLedgerResult
    evs
    cfg
    slotNo
    ShelleyLedgerState
      { shelleyLedgerTip
      , shelleyLedgerState
      , shelleyLedgerTransition
      } =
      appTick globals shelleyLedgerState slotNo <&> \l' ->
        TickedShelleyLedgerState
          { untickedShelleyLedgerTip = shelleyLedgerTip
          , tickedShelleyLedgerTransition =
              -- The voting resets each epoch
              if isNewEpoch ei (shelleyTipSlotNo <$> shelleyLedgerTip) slotNo
                then
                  ShelleyTransitionInfo{shelleyAfterVoting = 0}
                else
                  shelleyLedgerTransition
          , tickedShelleyLedgerState = l'
          , -- The UTxO set is only mutated by block/transaction execution and
            -- era translations, that is why we put empty tables here.
            tickedShelleyLedgerTables = emptyLedgerTables
          }
     where
      globals = shelleyLedgerGlobals cfg

      ei :: EpochInfo Identity
      ei = SL.epochInfoPure globals

      appTick =
        uncurry (flip LedgerResult) ..: case evs of
          ComputeLedgerEvents ->
            second (map ShelleyLedgerEventTICK)
              ..: SL.applyTick STS.EPReturn
          OmitLedgerEvents ->
            (,[]) ..: SL.applyTickNoEvents

-- | All events emitted by the Shelley ledger API
data ShelleyLedgerEvent era
  = -- | An event emitted when (re)applying a block
    ShelleyLedgerEventBBODY (STS.Event (Core.EraRule "BBODY" era))
  | -- | An event emitted during the chain tick
    ShelleyLedgerEventTICK (STS.Event (Core.EraRule "TICK" era))

instance
  ShelleyCompatible proto era =>
  ApplyBlock (LedgerState (ShelleyBlock proto era)) (ShelleyBlock proto era)
  where
  -- Note: in the Shelley ledger, the @CHAIN@ rule is used to apply a whole
  -- block. In consensus, we split up the application of a block to the ledger
  -- into separate steps that are performed together by 'applyExtLedgerState':
  --
  -- + 'applyChainTickLedgerResult': executes the @TICK@ transition
  -- + 'validateHeader':
  --    - 'validateEnvelope': executes the @chainChecks@
  --    - 'updateChainDepState': executes the @PRTCL@ transition
  -- + 'applyBlockLedgerResult': executes the @BBODY@ transition
  --
  applyBlockLedgerResultWithValidation doValidate evs =
    liftEither ..: applyHelper appBlk
   where
    -- Apply the BBODY transition using the ticked state
    appBlk =
      fmap (uncurry (flip LedgerResult)) ..: case evs of
        ComputeLedgerEvents ->
          fmap (second (map ShelleyLedgerEventBBODY))
            ..: SL.applyBlockEither STS.EPReturn doValidate
        OmitLedgerEvents ->
          fmap (,[])
            ..: SL.applyBlockEitherNoEvents doValidate

  applyBlockLedgerResult = defaultApplyBlockLedgerResult

  reapplyBlockLedgerResult =
    defaultReapplyBlockLedgerResult (\err -> Exception.throw $! ShelleyReapplyException @era err)

  getBlockKeySets =
    LedgerTables
      . KeysMK
      . Core.neededTxInsForBlock
      . shelleyBlockRaw

data ShelleyReapplyException
  = forall era.
    Show (SL.BlockTransitionError era) =>
    ShelleyReapplyException (SL.BlockTransitionError era)

instance Show ShelleyReapplyException where
  show (ShelleyReapplyException err) = "(ShelleyReapplyException " <> show err <> ")"

instance Exception.Exception ShelleyReapplyException

applyHelper ::
  forall proto era.
  ShelleyCompatible proto era =>
  ( SL.Globals ->
    SL.NewEpochState era ->
    SL.Block SL.BHeaderView era ->
    Either
      (SL.BlockTransitionError era)
      ( LedgerResult
          (LedgerState (ShelleyBlock proto era))
          (SL.NewEpochState era)
      )
  ) ->
  LedgerConfig (ShelleyBlock proto era) ->
  ShelleyBlock proto era ->
  Ticked (LedgerState (ShelleyBlock proto era)) ValuesMK ->
  Either
    (SL.BlockTransitionError era)
    ( LedgerResult
        (LedgerState (ShelleyBlock proto era))
        (LedgerState (ShelleyBlock proto era) DiffMK)
    )
applyHelper f cfg blk stBefore = do
  let TickedShelleyLedgerState
        { tickedShelleyLedgerTransition
        , tickedShelleyLedgerState
        } = stowLedgerTables stBefore

  ledgerResult <-
    f
      globals
      tickedShelleyLedgerState
      ( let b = shelleyBlockRaw blk
            h' = mkHeaderView (SL.bheader b)
         in SL.Block h' (SL.bbody b)
      )

  let track ::
        LedgerState (ShelleyBlock proto era) ValuesMK ->
        LedgerState (ShelleyBlock proto era) TrackingMK
      track = calculateDifference stBefore

  return $
    ledgerResult <&> \newNewEpochState ->
      trackingToDiffs $
        track $
          unstowLedgerTables $
            ShelleyLedgerState
              { shelleyLedgerTip =
                  NotOrigin
                    ShelleyTip
                      { shelleyTipBlockNo = blockNo blk
                      , shelleyTipSlotNo = blockSlot blk
                      , shelleyTipHash = blockHash blk
                      }
              , shelleyLedgerState =
                  newNewEpochState
              , shelleyLedgerTransition =
                  ShelleyTransitionInfo
                    { shelleyAfterVoting =
                        -- We count the number of blocks that have been applied after the
                        -- voting deadline has passed.
                        (if blockSlot blk >= votingDeadline then succ else id) $
                          shelleyAfterVoting tickedShelleyLedgerTransition
                    }
              , shelleyLedgerTables = emptyLedgerTables
              }
 where
  globals = shelleyLedgerGlobals cfg
  swindow = SL.stabilityWindow globals

  ei :: EpochInfo Identity
  ei = SL.epochInfoPure globals

  -- The start of the next epoch is within the safe zone, always.
  startOfNextEpoch :: SlotNo
  startOfNextEpoch = runIdentity $ do
    blockEpoch <- epochInfoEpoch ei (blockSlot blk)
    let nextEpoch = succ blockEpoch
    epochInfoFirst ei nextEpoch

  -- The block must come in strictly before the voting deadline
  -- See Fig 13, "Protocol Parameter Update Inference Rules", of the
  -- Shelley specification.
  votingDeadline :: SlotNo
  votingDeadline = subSlots (2 * swindow) startOfNextEpoch

instance HasHardForkHistory (ShelleyBlock proto era) where
  type HardForkIndices (ShelleyBlock proto era) = '[ShelleyBlock proto era]
  hardForkSummary =
    neverForksHardForkSummary $
      shelleyEraParamsNeverHardForks . shelleyLedgerGenesis

instance
  ShelleyCompatible proto era =>
  CommonProtocolParams (ShelleyBlock proto era)
  where
  maxHeaderSize = fromIntegral . view ppMaxBHSizeL . getPParams . shelleyLedgerState
  maxTxSize = view ppMaxTxSizeL . getPParams . shelleyLedgerState

{-------------------------------------------------------------------------------
  ValidateEnvelope
-------------------------------------------------------------------------------}

instance ShelleyCompatible proto era => BasicEnvelopeValidation (ShelleyBlock proto era)

-- defaults all OK

instance ShelleyCompatible proto era => ValidateEnvelope (ShelleyBlock proto era) where
  type
    OtherHeaderEnvelopeError (ShelleyBlock proto era) =
      EnvelopeCheckError proto

  additionalEnvelopeChecks cfg lv hdr =
    envelopeChecks (configConsensus cfg) lv (shelleyHeaderRaw hdr)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getPParams :: SL.EraGov era => SL.NewEpochState era -> Core.PParams era
getPParams = view $ SL.newEpochStateGovStateL . SL.curPParamsGovStateL

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

-- | Current version
--
-- o 'serialisationFormatVersion0' used to include the 'LedgerViewHistory', but
--   since we had to break binary backwards compatibility of the 'TPraosState',
--   we dropped backwards compatibility with 'serialisationFormatVersion0' too.
-- o 'serialisationFormatVersion1' did not include a 'BlockNo' at the tip of
--   the ledger, which was introduced in version 2. Again, since we broke
--   compat anyway, we dropped support for version 1.
serialisationFormatVersion2 :: VersionNumber
serialisationFormatVersion2 = 2

encodeShelleyAnnTip :: AnnTip (ShelleyBlock proto era) -> Encoding
encodeShelleyAnnTip = defaultEncodeAnnTip toCBOR

decodeShelleyAnnTip :: Decoder s (AnnTip (ShelleyBlock proto era))
decodeShelleyAnnTip = defaultDecodeAnnTip fromCBOR

encodeShelleyHeaderState ::
  ShelleyCompatible proto era =>
  HeaderState (ShelleyBlock proto era) ->
  Encoding
encodeShelleyHeaderState =
  encodeHeaderState
    encode
    encodeShelleyAnnTip

encodeShelleyTip :: ShelleyTip proto era -> Encoding
encodeShelleyTip
  ShelleyTip
    { shelleyTipSlotNo
    , shelleyTipBlockNo
    , shelleyTipHash
    } =
    mconcat
      [ CBOR.encodeListLen 3
      , encode shelleyTipSlotNo
      , encode shelleyTipBlockNo
      , encode shelleyTipHash
      ]

decodeShelleyTip :: Decoder s (ShelleyTip proto era)
decodeShelleyTip = do
  enforceSize "ShelleyTip" 3
  shelleyTipSlotNo <- decode
  shelleyTipBlockNo <- decode
  shelleyTipHash <- decode
  return
    ShelleyTip
      { shelleyTipSlotNo
      , shelleyTipBlockNo
      , shelleyTipHash
      }

encodeShelleyTransition :: ShelleyTransition -> Encoding
encodeShelleyTransition ShelleyTransitionInfo{shelleyAfterVoting} =
  mconcat
    [ CBOR.encodeWord32 shelleyAfterVoting
    ]

decodeShelleyTransition :: Decoder s ShelleyTransition
decodeShelleyTransition = do
  shelleyAfterVoting <- CBOR.decodeWord32
  return ShelleyTransitionInfo{shelleyAfterVoting}

encodeShelleyLedgerState ::
  ShelleyCompatible proto era =>
  LedgerState (ShelleyBlock proto era) EmptyMK ->
  Encoding
encodeShelleyLedgerState
  ShelleyLedgerState
    { shelleyLedgerTip
    , shelleyLedgerState
    , shelleyLedgerTransition
    } =
    encodeVersion serialisationFormatVersion2 $
      mconcat
        [ CBOR.encodeListLen 3
        , encodeWithOrigin encodeShelleyTip shelleyLedgerTip
        , toCBOR shelleyLedgerState
        , encodeShelleyTransition shelleyLedgerTransition
        ]

decodeShelleyLedgerState ::
  forall era proto s.
  ShelleyCompatible proto era =>
  Decoder s (LedgerState (ShelleyBlock proto era) EmptyMK)
decodeShelleyLedgerState =
  decodeVersion
    [ (serialisationFormatVersion2, Decode decodeShelleyLedgerState2)
    ]
 where
  decodeShelleyLedgerState2 :: Decoder s' (LedgerState (ShelleyBlock proto era) EmptyMK)
  decodeShelleyLedgerState2 = do
    enforceSize "LedgerState ShelleyBlock" 3
    shelleyLedgerTip <- decodeWithOrigin decodeShelleyTip
    shelleyLedgerState <- fromCBOR
    shelleyLedgerTransition <- decodeShelleyTransition
    return
      ShelleyLedgerState
        { shelleyLedgerTip
        , shelleyLedgerState
        , shelleyLedgerTransition
        , shelleyLedgerTables = emptyLedgerTables
        }

instance CanUpgradeLedgerTables (LedgerState (ShelleyBlock proto era)) where
  upgradeTables _ _ = id
