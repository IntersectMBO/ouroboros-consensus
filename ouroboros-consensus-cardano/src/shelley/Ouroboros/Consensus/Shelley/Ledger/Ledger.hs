{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 910
{-# OPTIONS_GHC -Wno-x-shelley-empty-utxo #-}
#else
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
#endif
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
  , shelleyLedgerState
  , shelleyLedgerTipPoint
  , shelleyTipToPoint
  , tickedShelleyLedgerState

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

import qualified Cardano.Ledger.BaseTypes as SL (TxIx (..), epochInfoPure)
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
import qualified Cardano.Ledger.State as SL
import Cardano.Slotting.EpochInfo
import Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise (decode, encode)
import Control.Arrow (left)
import qualified Control.Exception as Exception
import Control.Monad.Except
import qualified Control.State.Transition.Extended as STS
import Data.Coerce
import Data.Functor.Identity
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..), maybeToStrictMaybe, strictMaybeToMaybe)
import Data.MemPack
import Data.Set (Set)
import qualified Data.Set as Set
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
import Ouroboros.Consensus.HardFork.History.EraParams (EraParams (..))
import Ouroboros.Consensus.HardFork.History.Util
import Ouroboros.Consensus.HardFork.Simple
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.CommonProtocolParams
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsPeras (LedgerSupportsPeras (..))
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Protocol.Ledger.Util (isNewEpoch)
import Ouroboros.Consensus.Shelley.Ledger.Block
import Ouroboros.Consensus.Shelley.Ledger.Config
import Ouroboros.Consensus.Shelley.Ledger.LedgerCallShim
  ( NewEpochStateNoUTxOs
  , applyBlockShim
  , applyTickShim
  , mkNewEpochStateNoUTxOs
  , newEpochStateWithEmptyUTxO
  )
import Ouroboros.Consensus.Shelley.Ledger.Protocol ()
import Ouroboros.Consensus.Shelley.Protocol.Abstract
  ( EnvelopeCheckError
  , envelopeChecks
  )
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.CBOR
  ( decodeStrictMaybe
  , decodeWithOrigin
  , encodeStrictMaybe
  , encodeWithOrigin
  , unpackEither
  )
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

-- | The UTxO is not stored in the Shelley ledger state: it lives outside,
-- threaded as the @'Values'@\/@'Diff'@ of 'BlockSupportsUTxOHD'. The wrapped
-- 'SL.NewEpochState' has a UTxO field which we hold empty here (the entries live
-- in the backend); 'applyBlockLedgerResultWithValidation' injects the read
-- values, runs the ledger rules, and extracts the resulting diff.
data instance LedgerState (ShelleyBlock proto era) = ShelleyLedgerState
  { shelleyLedgerTip :: !(WithOrigin (ShelleyTip proto era))
  , shelleyLedgerStateNoUTxO :: !(NewEpochStateNoUTxOs era)
  -- ^ The new-epoch state with its UTxO field held empty; the UTxO lives in
  -- the backend (see 'Ouroboros.Consensus.Shelley.Ledger.LedgerCallShim'). Use
  -- the 'shelleyLedgerState' accessor for the (UTxO-free) 'SL.NewEpochState'.
  , shelleyLedgerTransition :: !ShelleyTransition
  , shelleyLedgerLatestPerasCertRound :: !(StrictMaybe PerasRoundNo)
  }
  deriving Generic

-- | The Shelley 'SL.NewEpochState'.
--
-- ⚠️  Its UTxO field is EMPTY by design: under UTxO-HD the live UTxO lives in the
-- ledger tables (the LedgerDB backend), not the ledger state. Use this for the
-- /non-UTxO/ parts only (protocol parameters, stake distribution, pools, …); to
-- read the UTxO, go through the LedgerDB forker \/ ledger tables. See
-- 'newEpochStateWithEmptyUTxO'.
shelleyLedgerState ::
  LedgerState (ShelleyBlock proto era) -> SL.NewEpochState era
shelleyLedgerState = newEpochStateWithEmptyUTxO . shelleyLedgerStateNoUTxO

#if __GLASGOW_HASKELL__ >= 910
{-# WARNING in "x-shelley-empty-utxo" shelleyLedgerState "This NewEpochState's UTxO is EMPTY by design: UTxO-HD keeps the live UTxO in the ledger tables, not the ledger state, so reading its UTxO yields an empty map. Read the UTxO via the LedgerDB forker / ledger tables. If you only need the non-UTxO parts, suppress with -Wno-x-shelley-empty-utxo (GHC >= 9.10) or -Wno-warnings-deprecations." #-}
#else
{-# WARNING shelleyLedgerState "This NewEpochState's UTxO is EMPTY by design: UTxO-HD keeps the live UTxO in the ledger tables, not the ledger state, so reading its UTxO yields an empty map. Read the UTxO via the LedgerDB forker / ledger tables. If you only need the non-UTxO parts, suppress with -Wno-x-shelley-empty-utxo (GHC >= 9.10) or -Wno-warnings-deprecations." #-}
#endif

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

-- | The Shelley @TxIn@ is the plain ledger 'SL.TxIn'. Big-endian serialisation
-- (needed so the on-disk\/streamed entries sort the same as the Haskell 'Ord')
-- is applied only at the (de)serialisation boundary, by casting the map keys
-- through 'BigEndianTxIn'. See 'encodeValues'\/'decodeValues'.
type instance TxIn (ShelleyBlock proto era) = SL.TxIn

type instance TxOut (ShelleyBlock proto era) = Core.TxOut era

instance
  ShelleyCompatible proto era =>
  BlockSupportsUTxOHD (ShelleyBlock proto era)
  where
  type Keys (ShelleyBlock proto era) = Set (TxIn (ShelleyBlock proto era))
  type
    Values (ShelleyBlock proto era) =
      Map (TxIn (ShelleyBlock proto era)) (TxOut (ShelleyBlock proto era))
  type
    Diff (ShelleyBlock proto era) =
      Diff.Diff (TxIn (ShelleyBlock proto era)) (TxOut (ShelleyBlock proto era))

  blockKeys = Core.neededTxInsForBlock . shelleyBlockRaw

  -- One era ⇒ no translation. Replay the in-flight diffs in chain order; the
  -- 'Diff' 'Monoid' composes them so later ones win.
  forward diffs vals = Diff.applyDiff vals (mconcat diffs)

  restrictValues keys vals = vals `Map.restrictKeys` keys

  valuesSize = Map.size

  -- The keys are cast through 'BigEndianTxIn' so the serialised entries sort the
  -- same as the Haskell 'Ord' on 'SL.TxIn' (the streaming-order invariant).
  encodeValues tbs =
    toPlainEncoding (Core.eraProtVerLow @era) $
      encodeMap (encodeMemPack . BigEndianTxIn) encodeMemPack tbs

  -- The state is the era hint: it supplies the credential interns for sharing.
  decodeValues st =
    let certInterns =
          internsFromMap $
            shelleyLedgerState st
              ^. SL.nesEsL
                . SL.esLStateL
                . SL.lsCertStateL
                . SL.certDStateL
                . SL.accountsL
                . SL.accountsMapL
     in eraDecoder @era $
          decodeMap (getOriginalTxIn <$> decodeMemPack) (decShareCBOR certInterns)

instance
  ShelleyCompatible proto era =>
  SingleEraUTxOHDBlock (ShelleyBlock proto era)
  where
  emptyValues = Map.empty
  emptyDiffs = mempty

instance
  ShelleyCompatible proto era =>
  SingleEraBlockSupportsUTxOHD (ShelleyBlock proto era)
  where
  rangeReadValues (mbPrev, n) vals =
    let toRead = case mbPrev of
          Nothing -> vals
          Just prev -> snd (Map.split prev vals)
        page = Map.take n toRead
     in (page, fst <$> Map.lookupMax page)
  keysToList = Set.toList
  valuesToList = Map.toList
  valuesFromList = Map.fromList
  diffToList (Diff.Diff m) = Map.toList m

  -- The on-disk key bytes are cast through 'BigEndianTxIn' so they sort the same
  -- as the Haskell 'Ord' on 'SL.TxIn' (the range-read\/streaming-order
  -- invariant); ledger's own 'MemPack' on 'SL.TxIn' is little-endian on the
  -- 'TxIx' and would sort differently. Mirrors 'encodeValues'\/'decodeValues'.
  packTxInBytes = packByteArray True . BigEndianTxIn
  unpackTxInBytes = fmap getOriginalTxIn . unpackEither

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

instance GetTip (LedgerState (ShelleyBlock proto era)) where
  getTip = castPoint . shelleyLedgerTipPoint

instance GetTip (Ticked LedgerState (ShelleyBlock proto era)) where
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
  , tickedShelleyLedgerStateNoUTxO :: !(NewEpochStateNoUTxOs era)
  -- ^ Like 'shelleyLedgerStateNoUTxO': the UTxO field is held empty. Use the
  -- 'tickedShelleyLedgerState' accessor for the (UTxO-free) 'SL.NewEpochState'.
  , tickedShelleyLedgerLatestPerasCertRound :: !(StrictMaybe PerasRoundNo)
  }
  deriving Generic

-- | The ticked Shelley 'SL.NewEpochState'.
--
-- ⚠️  Its UTxO field is EMPTY by design (see 'shelleyLedgerState'): the live
-- UTxO lives in the ledger tables, not the state.
tickedShelleyLedgerState ::
  Ticked LedgerState (ShelleyBlock proto era) -> SL.NewEpochState era
tickedShelleyLedgerState = newEpochStateWithEmptyUTxO . tickedShelleyLedgerStateNoUTxO

#if __GLASGOW_HASKELL__ >= 910
{-# WARNING in "x-shelley-empty-utxo" tickedShelleyLedgerState "This ticked NewEpochState's UTxO is EMPTY by design: UTxO-HD keeps the live UTxO in the ledger tables, not the ledger state. Read the UTxO via the LedgerDB forker / ledger tables. If you only need the non-UTxO parts, suppress with -Wno-x-shelley-empty-utxo (GHC >= 9.10) or -Wno-warnings-deprecations." #-}
#else
{-# WARNING tickedShelleyLedgerState "This ticked NewEpochState's UTxO is EMPTY by design: UTxO-HD keeps the live UTxO in the ledger tables, not the ledger state. Read the UTxO via the LedgerDB forker / ledger tables. If you only need the non-UTxO parts, suppress with -Wno-x-shelley-empty-utxo (GHC >= 9.10) or -Wno-warnings-deprecations." #-}
#endif

untickedShelleyLedgerTipPoint ::
  TickedLedgerState (ShelleyBlock proto era) ->
  Point (ShelleyBlock proto era)
untickedShelleyLedgerTipPoint = shelleyTipToPoint . untickedShelleyLedgerTip

type instance AuxLedgerEvent (ShelleyBlock proto era) = ShelleyLedgerEvent era

instance ShelleyCompatible proto era => IsLedger LedgerState (ShelleyBlock proto era) where
  type LedgerErr LedgerState (ShelleyBlock proto era) = SL.BlockTransitionError era

  applyChainTickLedgerResult
    evs
    cfg
    slotNo
    ShelleyLedgerState
      { shelleyLedgerTip
      , shelleyLedgerStateNoUTxO
      , shelleyLedgerTransition
      , shelleyLedgerLatestPerasCertRound
      } =
      let (tickedNoUTxO, events) =
            applyTickShim evs globals slotNo shelleyLedgerStateNoUTxO
       in LedgerResult
            (map ShelleyLedgerEventTICK events)
            ( TickedShelleyLedgerState
                { untickedShelleyLedgerTip = shelleyLedgerTip
                , tickedShelleyLedgerTransition =
                    -- The voting resets each epoch
                    if isNewEpoch ei (shelleyTipSlotNo <$> shelleyLedgerTip) slotNo
                      then ShelleyTransitionInfo{shelleyAfterVoting = 0}
                      else shelleyLedgerTransition
                , tickedShelleyLedgerStateNoUTxO = tickedNoUTxO
                , tickedShelleyLedgerLatestPerasCertRound =
                    shelleyLedgerLatestPerasCertRound
                }
            , -- Within a single era, ticking does not mutate the UTxO (the UTxO
              -- is only changed by block/transaction execution and by era
              -- translations, which happen at the hard-fork level), so it
              -- produces no diff.
              mempty
            )
     where
      globals = shelleyLedgerGlobals cfg

      ei :: EpochInfo Identity
      ei = SL.epochInfoPure globals

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
    liftEither ...: applyHelper evs doValidate

  applyBlockLedgerResult = defaultApplyBlockLedgerResult

  reapplyBlockLedgerResult =
    defaultReapplyBlockLedgerResult (\err -> Exception.throw $! ShelleyReapplyException @era err)

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
  ComputeLedgerEvents ->
  STS.ValidationPolicy ->
  LedgerConfig (ShelleyBlock proto era) ->
  ShelleyBlock proto era ->
  Values (ShelleyBlock proto era) ->
  Ticked LedgerState (ShelleyBlock proto era) ->
  Either
    (SL.BlockTransitionError era)
    ( LedgerResult
        (ShelleyBlock proto era)
        (LedgerState (ShelleyBlock proto era), Diff (ShelleyBlock proto era))
    )
applyHelper evs doValidate cfg blk values stBefore = do
  let TickedShelleyLedgerState
        { tickedShelleyLedgerTransition
        , tickedShelleyLedgerStateNoUTxO
        } = stBefore

  -- The shim stows the read values, runs BBODY, and clears+diffs the field; the
  -- stored state carries no UTxO (the entries live in the backend) and the diff
  -- against the read values is what the storage layer threads on. The raw
  -- protocol-header block is handed straight to the ledger (as prepare-11.1
  -- does); the ledger's BBODY rule extracts the header view it needs.
  (nesOut, diff, events) <-
    applyBlockShim evs doValidate globals (shelleyBlockRaw blk) values tickedShelleyLedgerStateNoUTxO

  let st' =
        ShelleyLedgerState
          { shelleyLedgerTip =
              NotOrigin
                ShelleyTip
                  { shelleyTipBlockNo = blockNo blk
                  , shelleyTipSlotNo = blockSlot blk
                  , shelleyTipHash = blockHash blk
                  }
          , shelleyLedgerStateNoUTxO =
              nesOut
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
  pure $ LedgerResult (map ShelleyLedgerEventBBODY events) (st', diff)
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
        tickedShelleyLedgerLatestPerasCertRound stBefore
      SJust certRoundInBlock ->
        case tickedShelleyLedgerLatestPerasCertRound stBefore of
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
    , shelleyLedgerStateNoUTxO
    , shelleyLedgerTransition
    , shelleyLedgerLatestPerasCertRound
    } =
    encodeVersion serialisationFormatVersion2 $
      mconcat $
        [ CBOR.encodeListLen 4
        , encodeWithOrigin encodeShelleyTip shelleyLedgerTip
        , toCBOR (newEpochStateWithEmptyUTxO shelleyLedgerStateNoUTxO)
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
    -- The snapshot stores the state with an empty UTxO field (the tables are
    -- serialised separately); 'mkNewEpochStateNoUTxOs' clears it regardless.
    shelleyLedgerStateNoUTxO <- mkNewEpochStateNoUTxOs <$> fromCBOR
    shelleyLedgerTransition <- decodeShelleyTransition
    shelleyLedgerLatestPerasCertRound <- decodeStrictMaybe fromCBOR
    return
      ShelleyLedgerState
        { shelleyLedgerTip
        , shelleyLedgerStateNoUTxO
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
