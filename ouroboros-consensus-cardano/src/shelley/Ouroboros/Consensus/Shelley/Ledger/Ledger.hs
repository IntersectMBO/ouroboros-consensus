{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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
{-# OPTIONS_GHC -Wno-orphans -Wno-x-ord-preserving-coercions #-}
#if __GLASGOW_HASKELL__ < 908
{-# OPTIONS_GHC -Wno-unrecognised-warning-flags #-}
#endif

module Ouroboros.Consensus.Shelley.Ledger.Ledger
  ( LedgerState (..)
  , ShelleyBasedEra
  , ShelleyTip (..)
  , ShelleyTransition (..)
  , Ticked (..)
  , castShelleyTip
  , shelleyLedgerTipPoint
  , shelleyTipToPoint
  , StateHandle (..)
  , TickedStateHandle (..)

    -- * Handles
  , TablesHandle (..)
  , MkHandle (..)
  , MkHandleFromSnapshot (..)
  , BackendError (..)
  , slUtxoL

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
  , BigEndianTxIn (..)
  ) where

import qualified Cardano.Ledger.BHeaderView as SL (BHeaderView)
import qualified Cardano.Ledger.BaseTypes as SL (TxIx (..), epochInfoPure)
import Cardano.Ledger.BaseTypes.NonZero (unNonZero)
import Cardano.Ledger.Binary.Decoding (DecShareCBOR, Interns, Share)
import Cardano.Ledger.Binary.Plain
  ( FromCBOR (..)
  , ToCBOR (..)
  , enforceSize
  )
import qualified Cardano.Ledger.Block as Core
import qualified Cardano.Ledger.Conway.State as SL
import Cardano.Ledger.Core
  ( Era
  , ppMaxBHSizeL
  , ppMaxTxSizeL
  )
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Core as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import Cardano.Slotting.EpochInfo
import Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise (decode, encode)
import Control.Arrow (left, second)
import qualified Control.Exception as Exception
import Control.Monad.Except
import Control.Monad.Trans (lift)
import qualified Control.State.Transition.Extended as STS
import Data.Coerce
import Data.Functor.Identity
import Data.Maybe.Strict (StrictMaybe (..), maybeToStrictMaybe, strictMaybeToMaybe)
import Data.MemPack
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Word
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Extras (view)
import NoThunks.Class (OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.HardFork.History.EraParams (EraParams (..))
import Ouroboros.Consensus.HardFork.History.Util
import Ouroboros.Consensus.HardFork.Simple
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract hiding (Handle)
import Ouroboros.Consensus.Ledger.CommonProtocolParams
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsPeras (LedgerSupportsPeras (..))
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Protocol.Ledger.Util (isNewEpoch)
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Consensus.Shelley.Ledger.Config
import Ouroboros.Consensus.Shelley.Ledger.Protocol ()
import Ouroboros.Consensus.Shelley.Protocol.Abstract
  ( EnvelopeCheckError
  , envelopeChecks
  , mkHeaderView
  )
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.CBOR hiding (Decoder)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.Versioned
import System.FS.CRC

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
    , -- TODO(geo2a): enabled Peras conditionally in the Dijkstra era
      -- see https://github.com/tweag/cardano-peras/issues/112
      eraPerasRoundLength = HardFork.NoPerasEnabled
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
    , eraPerasRoundLength = HardFork.NoPerasEnabled
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

type instance LedgerCfg LedgerState (ShelleyBlock proto era) = ShelleyLedgerConfig era

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

data instance LedgerState (ShelleyBlock proto era) = ShelleyLedgerState
  { shelleyLedgerTip :: !(WithOrigin (ShelleyTip proto era))
  , shelleyLedgerState :: !(SL.NewEpochState era)
  , shelleyLedgerTransition :: !ShelleyTransition
  , shelleyLedgerLatestPerasCertRound :: !(StrictMaybe PerasRoundNo)
  }
  deriving Generic

deriving instance
  ShelleyBasedEra era =>
  Eq (LedgerState (ShelleyBlock proto era))
deriving instance
  ShelleyBasedEra era =>
  NoThunks (LedgerState (ShelleyBlock proto era))
deriving instance
  ShelleyBasedEra era =>
  Show (LedgerState (ShelleyBlock proto era))

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
  LedgerState (ShelleyBlock proto era) ->
  Point (ShelleyBlock proto era)
shelleyLedgerTipPoint = shelleyTipToPoint . shelleyLedgerTip

instance ShelleyCompatible proto era => UpdateLedger (ShelleyBlock proto era)

-- | The only purpose of this type is to modify the MemPack instance to use big
-- endian serialization. This is necessary to ensure streaming functions of the
-- UTxO set preserve the order of the entries, as otherwise we would get
-- different sortings if sorting via the Serialized form and the Haskell Ord
-- instance.
--
-- TODO: fix this in the Ledger. See cardano-ledger#5336.
newtype BigEndianTxIn = BigEndianTxIn {getOriginalTxIn :: SL.TxIn}
  deriving newtype (Eq, Show, Ord, NoThunks)

newtype BigEndianTxIx = BigEndianTxIx {getOriginalTxIx :: SL.TxIx}

instance MemPack BigEndianTxIx where
  typeName = "BigEndianTxIx"
  packedByteCount = packedByteCount . getOriginalTxIx
  packM (BigEndianTxIx (SL.TxIx w)) = packM (byteSwap16 w)
  unpackM = BigEndianTxIx . SL.TxIx . byteSwap16 <$> unpackM

instance MemPack BigEndianTxIn where
  typeName = "BigEndianTxIn"
  packedByteCount = packedByteCount . getOriginalTxIn
  packM (BigEndianTxIn (SL.TxIn txid txix)) = do
    packM txid
    packM (BigEndianTxIx txix)
  unpackM = do
    BigEndianTxIn <$> (SL.TxIn <$> unpackM <*> (getOriginalTxIx <$> unpackM))

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

instance GetTip LedgerState (ShelleyBlock proto era) where
  getTip = castPoint . shelleyLedgerTipPoint

instance GetTip (Ticked LedgerState) (ShelleyBlock proto era) where
  getTip = castPoint . untickedShelleyLedgerTipPoint

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

-- | Ticking only affects the state itself
data instance Ticked LedgerState (ShelleyBlock proto era) = TickedShelleyLedgerState
  { untickedShelleyLedgerTip :: !(WithOrigin (ShelleyTip proto era))
  , tickedShelleyLedgerTransition :: !ShelleyTransition
  -- ^ We are counting blocks within an epoch, this means:
  --
  -- 1. We are only incrementing this when /applying/ a block, not when ticking.
  -- 2. However, we count within an epoch, which is slot-based. So the count
  --    must be reset when /ticking/, not when applying a block.
  , tickedShelleyLedgerState :: !(SL.NewEpochState era)
  , tickedShelleyLedgerLatestPerasCertRound :: !(StrictMaybe PerasRoundNo)
  }
  deriving Generic

deriving instance
  ShelleyBasedEra era =>
  NoThunks (Ticked LedgerState (ShelleyBlock proto era))

untickedShelleyLedgerTipPoint ::
  TickedLedgerState (ShelleyBlock proto era) ->
  Point (ShelleyBlock proto era)
untickedShelleyLedgerTipPoint = shelleyTipToPoint . untickedShelleyLedgerTip

type instance AuxLedgerEvent (ShelleyBlock proto era) = ShelleyLedgerEvent era

type instance LedgerTablesHandle m (ShelleyBlock proto era) = TablesHandle m era

slUtxoL :: Lens' (SL.NewEpochState era) (SL.UTxO era)
slUtxoL = SL.nesEsL . SL.esLStateL . SL.lsUTxOStateL . SL.utxoL

data MkHandle m = MkHandle
  { fromNewEpochState ::
      forall era.
      (SL.Era era, MemPack (SL.TxOut era), MonadThrow m, Eq (SL.TxOut era)) =>
      SL.NewEpochState era -> m (TablesHandle m era)
  }

data MkHandleFromSnapshot m = MkHandleFromSnapshot
  { fromSnapshot ::
      forall era.
      ( SL.Era era
      , MemPack (SL.TxOut era)
      , IOLike m
      , Share (SL.TxOut era) ~ Interns (SL.Credential SL.Staking)
      , DecShareCBOR (SL.TxOut era)
      , SL.EraCertState era
      , Eq (SL.TxOut era)
      ) =>
      DiskSnapshot -> SL.NewEpochState era -> ExceptT BackendError m (TablesHandle m era, Maybe CRC)
  }

data BackendError = BackendReadErr ReadIncrementalErr | BackendCorruptedData
  deriving Show

data TablesHandle m era = TablesHandle
  { stateWith :: Set SL.TxIn -> m (SL.NewEpochState era)
  -- ^ Given a set of TxIns, produce a NewEpochState that has the
  -- TxOuts we could find in the backend
  , stateWithUTxO :: SL.UTxO era -> SL.NewEpochState era
  -- ^ Only used for the AVVMs, create a NewEpochState as if the
  -- given UTxOs had been read from the disk.
  , applyDiff :: Diff.Diff SL.TxIn (SL.TxOut era) -> m (TablesHandle m era)
  -- ^ Only used for AVVMs. Push a bunch of diffs to this reference
  -- without duplicating it. In the OnDisk backend
  -- this will mutate the database.
  , duplWithDiffs :: SL.NewEpochState era -> SL.NewEpochState era -> m (TablesHandle m era)
  -- ^ Given the before and after states, produce a new handle on
  -- the after state.
  --
  -- The full states are passed here so that the handle can in the
  -- InMemory case just use the second state, and in the LSM case
  -- it can compute the differences to push them to a duplicated
  -- handle.
  , duplicateHandle :: m (TablesHandle m era)
  -- ^ Create a duplicated reference to this handle
  , readUTxOWhole :: m (SL.UTxO era)
  -- ^ Read the whole UTxO set from the tables. This method inside will
  -- use pagination if accessing the disk.
  , readUTxOFiltered :: (SL.TxOut era -> Bool) -> m (SL.UTxO era)
  -- ^ Read the UTxO set filtered by a predicate on TxOuts. Will use
  -- pagination if accessing the disk.
  , readTxOuts :: Set SL.TxIn -> m (SL.UTxO era)
  -- ^ Get a particular (TxIn,TxOut) pair.
  , closeHandle :: m ()
  -- ^ Release the on-disk handle
  , getStatsHandle :: Statistics
  -- ^ Get the size of the tables for this handle
  , takeHandleSnapshot :: DiskSnapshot -> m (Maybe CRC, SnapshotBackend)
  -- ^ Take a snapshot with the given name
  , castHandle ::
      forall era'.
      (SL.Era era', MemPack (SL.TxOut era'), Eq (SL.TxOut era')) =>
      SL.NewEpochState era' -> m (TablesHandle m era')
  , injectValues :: SL.NewEpochState era -> m (TablesHandle m era)
  }

instance BlockSupportsLedgerHD m (ShelleyBlock proto era) where
  data StateHandle m (ShelleyBlock proto era) = ShelleyStateHandle
    { stateRefState :: LedgerState (ShelleyBlock proto era)
    , stateRefHandle :: TablesHandle m era
    }

  data TickedStateHandle m (ShelleyBlock proto era) = TickedShelleyStateHandle
    { tickedStateHandleState :: Ticked LedgerState (ShelleyBlock proto era)
    , tickedStateHandleHandle :: TablesHandle m era
    }

  newStateHandle = ShelleyStateHandle

  state = stateRefState
  tickedState = tickedStateHandleState

  close = closeHandle . stateRefHandle
  closeTicked = closeHandle . tickedStateHandleHandle

  duplicate (ShelleyStateHandle s h) = ShelleyStateHandle s <$> duplicateHandle h
  duplicateTicked (TickedShelleyStateHandle s h) = TickedShelleyStateHandle s <$> duplicateHandle h

  getStats = getStatsHandle . stateRefHandle

deriving via
  OnlyCheckWhnfNamed "ShelleyStateHandle" (StateHandle m (ShelleyBlock proto era))
  instance
    NoThunks (StateHandle m (ShelleyBlock proto era))

deriving via
  OnlyCheckWhnfNamed
    "TickedShelleyStateHandle"
    (TickedStateHandle m (ShelleyBlock proto era))
  instance
    NoThunks (TickedStateHandle m (ShelleyBlock proto era))

instance ShelleyBasedEra era => IsLedger LedgerState (ShelleyBlock proto era) where
  type LedgerErr LedgerState (ShelleyBlock proto era) = SL.BlockTransitionError era

  applyChainTickLedgerResult
    evs
    cfg
    slotNo
    ( ShelleyStateHandle
        ShelleyLedgerState
          { shelleyLedgerTip
          , shelleyLedgerState
          , shelleyLedgerTransition
          , shelleyLedgerLatestPerasCertRound
          }
        h
      ) =
      pure $
        appTick globals shelleyLedgerState slotNo <&> \l' ->
          TickedShelleyStateHandle
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
              , tickedShelleyLedgerLatestPerasCertRound =
                  shelleyLedgerLatestPerasCertRound
              }
            h
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
  ApplyBlock LedgerState (ShelleyBlock proto era)
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
    applyHelper appBlk
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

getBlockKeySets :: ShelleyCompatible proto era => ShelleyBlock proto era -> Set SL.TxIn
getBlockKeySets =
  Core.neededTxInsForBlock
    . shelleyBlockRaw

data ShelleyReapplyException
  = forall era.
    Show (SL.BlockTransitionError era) =>
    ShelleyReapplyException (SL.BlockTransitionError era)

instance Show ShelleyReapplyException where
  show (ShelleyReapplyException err) = "(ShelleyReapplyException " <> show err <> ")"

instance Exception.Exception ShelleyReapplyException

applyHelper ::
  forall m proto era.
  (Monad m, ShelleyCompatible proto era) =>
  ( SL.Globals ->
    SL.NewEpochState era ->
    SL.Block SL.BHeaderView era ->
    Either
      (SL.BlockTransitionError era)
      ( LedgerResult
          (ShelleyBlock proto era)
          (SL.NewEpochState era)
      )
  ) ->
  LedgerConfig (ShelleyBlock proto era) ->
  ShelleyBlock proto era ->
  TickedStateHandle m (ShelleyBlock proto era) ->
  ExceptT
    (SL.BlockTransitionError era)
    m
    ( LedgerResult
        (ShelleyBlock proto era)
        (StateHandle m (ShelleyBlock proto era))
    )
applyHelper f cfg blk stBefore = do
  let TickedShelleyStateHandle
        TickedShelleyLedgerState
          { tickedShelleyLedgerTransition
          }
        h = stBefore

  tickedShelleyLedgerState' <- lift $ stateWith h (getBlockKeySets blk)
  LedgerResult evs st' <-
    ExceptT $
      pure $
        f
          globals
          tickedShelleyLedgerState'
          ( let b = shelleyBlockRaw blk
                h' = mkHeaderView (SL.blockHeader b)
             in SL.Block h' (SL.blockBody b)
          )

  h' <- lift $ duplWithDiffs h tickedShelleyLedgerState' st'

  pure $
    LedgerResult evs $
      ShelleyStateHandle
        ShelleyLedgerState
          { shelleyLedgerTip =
              NotOrigin
                ShelleyTip
                  { shelleyTipBlockNo = blockNo blk
                  , shelleyTipSlotNo = blockSlot blk
                  , shelleyTipHash = blockHash blk
                  }
          , shelleyLedgerState = st'
          , shelleyLedgerTransition =
              ShelleyTransitionInfo
                { shelleyAfterVoting =
                    -- We count the number of blocks that have been applied after the
                    -- voting deadline has passed.
                    (if blockSlot blk >= votingDeadline then succ else id) $
                      shelleyAfterVoting tickedShelleyLedgerTransition
                }
          , shelleyLedgerLatestPerasCertRound =
              shelleyLedgerLatestPerasCertRound'
          }
        h'
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

  -- Update the latest Peras certificate round if the new block contains a
  -- certificate from a round more recent than the currently cached one.
  shelleyLedgerLatestPerasCertRound' :: StrictMaybe PerasRoundNo
  shelleyLedgerLatestPerasCertRound' =
    case getPerasCertRoundInBlock blk of
      SNothing ->
        tickedShelleyLedgerLatestPerasCertRound $ tickedStateHandleState stBefore
      SJust certRoundInBlock ->
        case tickedShelleyLedgerLatestPerasCertRound $ tickedStateHandleState stBefore of
          SNothing ->
            SJust certRoundInBlock
          SJust latestCertRoundInLedgerState ->
            SJust (certRoundInBlock `max` latestCertRoundInLedgerState)

  -- Extract the round number of the Peras certificate stored in a block, if any
  getPerasCertRoundInBlock :: ShelleyBlock proto era -> StrictMaybe PerasRoundNo
  getPerasCertRoundInBlock =
    fmap getPerasCertRound . maybeToStrictMaybe . getPerasCertInBlock

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
  LedgerState (ShelleyBlock proto era) ->
  Encoding
encodeShelleyLedgerState
  ShelleyLedgerState
    { shelleyLedgerTip
    , shelleyLedgerState
    , shelleyLedgerTransition
    , shelleyLedgerLatestPerasCertRound
    } =
    encodeVersion serialisationFormatVersion2 $
      mconcat $
        [ CBOR.encodeListLen 4
        , encodeWithOrigin encodeShelleyTip shelleyLedgerTip
        , toCBOR shelleyLedgerState
        , encodeShelleyTransition shelleyLedgerTransition
        , encodeStrictMaybe toCBOR shelleyLedgerLatestPerasCertRound
        ]

decodeShelleyLedgerState ::
  forall era proto s.
  ShelleyCompatible proto era =>
  Decoder s (LedgerState (ShelleyBlock proto era))
decodeShelleyLedgerState =
  decodeVersion
    [ (serialisationFormatVersion2, Decode decodeShelleyLedgerState2)
    ]
 where
  decodeShelleyLedgerState2 :: Decoder s' (LedgerState (ShelleyBlock proto era))
  decodeShelleyLedgerState2 = do
    enforceSize "ShelleyLedgerState" 4
    shelleyLedgerTip <- decodeWithOrigin decodeShelleyTip
    shelleyLedgerState <- fromCBOR
    shelleyLedgerTransition <- decodeShelleyTransition
    shelleyLedgerLatestPerasCertRound <- decodeStrictMaybe fromCBOR
    return
      ShelleyLedgerState
        { shelleyLedgerTip
        , shelleyLedgerState
        , shelleyLedgerTransition
        , shelleyLedgerLatestPerasCertRound
        }

{-------------------------------------------------------------------------------
  LedgerSupportsPeras
-------------------------------------------------------------------------------}

instance LedgerSupportsPeras (ShelleyBlock proto era) where
  getLatestPerasCertRound =
    strictMaybeToMaybe
      . shelleyLedgerLatestPerasCertRound
