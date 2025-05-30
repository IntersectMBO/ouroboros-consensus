{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Instances requires for consensus/ledger integration
module Ouroboros.Consensus.Byron.Ledger.Ledger
  ( ByronTransition (..)

    -- * Ledger integration
  , byronEraParams
  , byronEraParamsNeverHardForks
  , initByronLedgerState

    -- * Serialisation
  , decodeByronAnnTip
  , decodeByronLedgerState
  , decodeByronQuery
  , decodeByronResult
  , encodeByronAnnTip
  , encodeByronExtLedgerState
  , encodeByronHeaderState
  , encodeByronLedgerState
  , encodeByronQuery
  , encodeByronResult

    -- * Type family instances
  , BlockQuery (..)
  , LedgerState (..)
  , LedgerTables (..)
  , Ticked (..)

    -- * Auxiliary
  , validationErrorImpossible
  ) where

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Byron.API as CC
import qualified Cardano.Chain.Common as Gen
import qualified Cardano.Chain.Genesis as Gen
import qualified Cardano.Chain.UTxO as CC
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.Update.Validation.Endorsement as UPE
import qualified Cardano.Chain.Update.Validation.Interface as UPI
import qualified Cardano.Chain.ValidationMode as CC
import Cardano.Ledger.BaseTypes (unNonZero)
import Cardano.Ledger.Binary (fromByronCBOR, toByronCBOR)
import Cardano.Ledger.Binary.Plain (encodeListLen, enforceSize)
import Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise (decode, encode)
import Control.Monad (replicateM)
import Control.Monad.Except (Except, runExcept, throwError)
import qualified Control.State.Transition.Extended as STS
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Byron.Ledger.Block
import Ouroboros.Consensus.Byron.Ledger.Conversions
import Ouroboros.Consensus.Byron.Ledger.HeaderValidation ()
import Ouroboros.Consensus.Byron.Ledger.PBFT
import Ouroboros.Consensus.Byron.Ledger.Serialisation
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Forecast
import Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.CommonProtocolParams
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.SupportsPeerSelection
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.Util (ShowProxy (..))
import Ouroboros.Consensus.Util.IndexedMemPack

{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

data instance LedgerState ByronBlock mk = ByronLedgerState
  { byronLedgerTipBlockNo :: !(WithOrigin BlockNo)
  , byronLedgerState :: !CC.ChainValidationState
  , byronLedgerTransition :: !ByronTransition
  }
  deriving (Eq, Show, Generic, NoThunks)

-- | Information required to determine the transition from Byron to Shelley
data ByronTransition
  = -- | Per candidate proposal, the 'BlockNo' in which it became a candidate
    --
    -- The HFC needs to know when a candidate proposal becomes stable. We cannot
    -- reliably do this using 'SlotNo': doing so would mean that if we were to
    -- switch to a denser fork, something that was previously deemed stable is
    -- suddenly not deemed stable anymore (although in actuality it still is).
    -- We therefore must do this based on 'BlockNo' instead, but unfortunately
    -- the Byron ledger does not record this information. Therefore, we record
    -- it here instead.
    --
    -- Invariant: the domain of this map should equal the set of candidate
    -- proposals.
    ByronTransitionInfo !(Map Update.ProtocolVersion BlockNo)
  deriving (Eq, Show, Generic, NoThunks)

instance UpdateLedger ByronBlock

type instance LedgerCfg (LedgerState ByronBlock) = Gen.Config

initByronLedgerState ::
  Gen.Config ->
  -- | Optionally override UTxO
  Maybe CC.UTxO ->
  LedgerState ByronBlock mk
initByronLedgerState genesis mUtxo =
  ByronLedgerState
    { byronLedgerState = override mUtxo initState
    , byronLedgerTipBlockNo = Origin
    , byronLedgerTransition = ByronTransitionInfo Map.empty
    }
 where
  initState :: CC.ChainValidationState
  initState = case runExcept $ CC.initialChainValidationState genesis of
    Right st -> st
    Left e ->
      error $
        "could not create initial ChainValidationState: " <> show e

  override ::
    Maybe CC.UTxO ->
    CC.ChainValidationState ->
    CC.ChainValidationState
  override Nothing st = st
  override (Just utxo) st = st{CC.cvsUtxo = utxo}

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

instance GetTip (LedgerState ByronBlock) where
  getTip = castPoint . getByronTip . byronLedgerState

instance GetTip (Ticked (LedgerState ByronBlock)) where
  getTip = castPoint . getByronTip . tickedByronLedgerState

getByronTip :: CC.ChainValidationState -> Point ByronBlock
getByronTip state =
  case CC.cvsPreviousHash state of
    -- In this case there are no blocks in the ledger state. The genesis
    -- block does not occupy a slot, so its point is Origin.
    Left _genHash -> GenesisPoint
    Right hdrHash -> BlockPoint slot (ByronHash hdrHash)
     where
      slot = fromByronSlotNo (CC.cvsLastSlot state)

{-------------------------------------------------------------------------------
  Ticked ledger state
-------------------------------------------------------------------------------}

-- | The ticked Byron ledger state
data instance Ticked (LedgerState ByronBlock) mk = TickedByronLedgerState
  { tickedByronLedgerState :: !CC.ChainValidationState
  , untickedByronLedgerTransition :: !ByronTransition
  }
  deriving (Generic, NoThunks)

instance IsLedger (LedgerState ByronBlock) where
  type LedgerErr (LedgerState ByronBlock) = CC.ChainValidationError

  type
    AuxLedgerEvent (LedgerState ByronBlock) =
      VoidLedgerEvent (LedgerState ByronBlock)

  applyChainTickLedgerResult _ cfg slotNo ByronLedgerState{..} =
    pureLedgerResult $
      TickedByronLedgerState
        { tickedByronLedgerState =
            CC.applyChainTick cfg (toByronSlotNo slotNo) byronLedgerState
        , untickedByronLedgerTransition =
            byronLedgerTransition
        }

type instance TxIn (LedgerState ByronBlock) = Void
type instance TxOut (LedgerState ByronBlock) = Void

instance LedgerTablesAreTrivial (LedgerState ByronBlock) where
  convertMapKind (ByronLedgerState x y z) = ByronLedgerState x y z
instance LedgerTablesAreTrivial (Ticked (LedgerState ByronBlock)) where
  convertMapKind (TickedByronLedgerState x y) = TickedByronLedgerState x y

deriving via
  Void
  instance
    IndexedMemPack (LedgerState ByronBlock EmptyMK) Void

deriving via
  TrivialLedgerTables (LedgerState ByronBlock)
  instance
    HasLedgerTables (LedgerState ByronBlock)
deriving via
  TrivialLedgerTables (Ticked (LedgerState ByronBlock))
  instance
    HasLedgerTables (Ticked (LedgerState ByronBlock))
deriving via
  TrivialLedgerTables (LedgerState ByronBlock)
  instance
    CanStowLedgerTables (LedgerState ByronBlock)
deriving via
  TrivialLedgerTables (LedgerState ByronBlock)
  instance
    SerializeTablesWithHint (LedgerState ByronBlock)

{-------------------------------------------------------------------------------
  Supporting the various consensus interfaces
-------------------------------------------------------------------------------}

instance ApplyBlock (LedgerState ByronBlock) ByronBlock where
  applyBlockLedgerResultWithValidation doValidation opts =
    fmap pureLedgerResult ..: applyByronBlock doValidation opts
  applyBlockLedgerResult = defaultApplyBlockLedgerResult
  reapplyBlockLedgerResult = defaultReapplyBlockLedgerResult validationErrorImpossible

  getBlockKeySets _ = emptyLedgerTables

data instance BlockQuery ByronBlock fp result where
  GetUpdateInterfaceState :: BlockQuery ByronBlock QFNoTables UPI.State

instance BlockSupportsLedgerQuery ByronBlock where
  answerPureBlockQuery _cfg GetUpdateInterfaceState dlv =
    CC.cvsUpdateState (byronLedgerState ledgerState)
   where
    ExtLedgerState{ledgerState} = dlv
  answerBlockQueryLookup _cfg q _dlv = case q of {}
  answerBlockQueryTraverse _cfg q _dlv = case q of {}
  blockQueryIsSupportedOnVersion GetUpdateInterfaceState = const True

instance SameDepIndex2 (BlockQuery ByronBlock) where
  sameDepIndex2 GetUpdateInterfaceState GetUpdateInterfaceState = Just Refl

deriving instance Eq (BlockQuery ByronBlock fp result)
deriving instance Show (BlockQuery ByronBlock fp result)

instance ShowQuery (BlockQuery ByronBlock fp) where
  showResult GetUpdateInterfaceState = show

instance ShowProxy (BlockQuery ByronBlock)

instance LedgerSupportsPeerSelection ByronBlock where
  getPeers = const []

instance CommonProtocolParams ByronBlock where
  maxHeaderSize = fromIntegral . Update.ppMaxHeaderSize . getProtocolParameters
  maxTxSize = fromIntegral . Update.ppMaxTxSize . getProtocolParameters

-- | Return the protocol parameters adopted by the given ledger.
getProtocolParameters :: LedgerState ByronBlock mk -> Update.ProtocolParameters
getProtocolParameters =
  CC.adoptedProtocolParameters
    . CC.cvsUpdateState
    . byronLedgerState

instance LedgerSupportsProtocol ByronBlock where
  protocolLedgerView _cfg =
    toPBftLedgerView
      . CC.getDelegationMap
      . tickedByronLedgerState

  -- Create a forecast of the delegation state
  --
  -- We can return forecasts for slots in the @[NOW .. NOW+2k)@ window, where
  -- @NOW@ is the slot number of the last block applied to the ledger.
  --
  -- These forecasts will be used to validate future headers, i.e., to check
  -- whether they have been created by the right delegates.
  --
  -- We cannot look more than @2k@ slots ahead, because there might be
  -- delegation state changes present in the blocks between the last block
  -- applied to the ledger and the header to validate that can kick in after
  -- @2k@ slots.
  --
  -- To create a forecast, take the delegation state from the given ledger
  -- state, and apply the updates that should be applied by the given slot.
  ledgerViewForecastAt cfg (ByronLedgerState _tipBlkNo st _) = Forecast at $ \for ->
    toPBftLedgerView
      <$> if
        | for == lastSlot ->
            return $ CC.getDelegationMap st
        | for < maxFor ->
            return $ CC.previewDelegationMap (toByronSlotNo for) st
        | otherwise ->
            throwError $
              OutsideForecastRange
                { outsideForecastAt = at
                , outsideForecastMaxFor = maxFor
                , outsideForecastFor = for
                }
   where
    k = unNonZero $ maxRollbacks $ genesisSecurityParam cfg
    lastSlot = fromByronSlotNo $ CC.cvsLastSlot st
    at = NotOrigin lastSlot

    -- The upper bound is exclusive
    maxFor :: SlotNo
    maxFor = case at of
      Origin -> SlotNo $ 2 * k
      NotOrigin s -> SlotNo $ unSlotNo s + 1 + (2 * k)

-- | To be used for a Byron-to-X (where X is typically Shelley) chain.
byronEraParams :: Gen.Config -> HardFork.EraParams
byronEraParams genesis =
  HardFork.EraParams
    { eraEpochSize = fromByronEpochSlots $ Gen.configEpochSlots genesis
    , eraSlotLength = fromByronSlotLength $ genesisSlotLength genesis
    , eraSafeZone = HardFork.StandardSafeZone (2 * k)
    , eraGenesisWin = GenesisWindow (2 * k)
    }
 where
  k = unNonZero $ maxRollbacks $ genesisSecurityParam genesis

-- | Separate variant of 'byronEraParams' to be used for a Byron-only chain.
byronEraParamsNeverHardForks :: Gen.Config -> HardFork.EraParams
byronEraParamsNeverHardForks genesis =
  HardFork.EraParams
    { eraEpochSize = fromByronEpochSlots $ Gen.configEpochSlots genesis
    , eraSlotLength = fromByronSlotLength $ genesisSlotLength genesis
    , eraSafeZone = HardFork.UnsafeIndefiniteSafeZone
    , eraGenesisWin = GenesisWindow (2 * Gen.unBlockCount (Gen.configK genesis))
    }

instance HasHardForkHistory ByronBlock where
  type HardForkIndices ByronBlock = '[ByronBlock]
  hardForkSummary = neverForksHardForkSummary byronEraParamsNeverHardForks

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Mark computation as validation error free
--
-- Given a 'BlockValidationMode' of 'NoBlockValidation', a call to
-- 'applyByronBlock' shouldn't fail since the ledger layer won't be performing
-- any block validation checks. However, because 'applyByronBlock' can fail in
-- the event it is given a 'BlockValidationMode' of 'BlockValidation', it still
-- /looks/ like it can fail (since its type doesn't change based on the
-- 'ValidationMode') and we must still treat it as such.
validationErrorImpossible :: forall err a. err -> a
validationErrorImpossible _ = error "validationErrorImpossible: unexpected error"

{-------------------------------------------------------------------------------
  Applying a block

  Most of the work here is done by the ledger layer. We just need to pass
  the right arguments, and maintain the snapshots.
-------------------------------------------------------------------------------}

applyByronBlock ::
  STS.ValidationPolicy ->
  ComputeLedgerEvents ->
  LedgerConfig ByronBlock ->
  ByronBlock ->
  TickedLedgerState ByronBlock mk1 ->
  Except (LedgerError ByronBlock) (LedgerState ByronBlock mk2)
applyByronBlock
  doValidation
  _doEvents
  cfg
  blk@(ByronBlock raw _ (ByronHash blkHash))
  ls =
    case raw of
      CC.ABOBBlock raw' -> applyABlock byronOpts cfg raw' blkHash blkNo ls
      CC.ABOBBoundary raw' -> applyABoundaryBlock cfg raw' blkNo ls
   where
    blkNo :: BlockNo
    blkNo = blockNo blk

    byronOpts =
      CC.fromBlockValidationMode $ case doValidation of
        STS.ValidateAll -> CC.BlockValidation
        STS.ValidateNone -> CC.NoBlockValidation
        STS.ValidateSuchThat _ -> CC.BlockValidation

applyABlock ::
  CC.ValidationMode ->
  Gen.Config ->
  CC.ABlock ByteString ->
  CC.HeaderHash ->
  BlockNo ->
  TickedLedgerState ByronBlock mk1 ->
  Except (LedgerError ByronBlock) (LedgerState ByronBlock mk2)
applyABlock validationMode cfg blk blkHash blkNo TickedByronLedgerState{..} = do
  st' <- CC.validateBlock cfg validationMode blk blkHash tickedByronLedgerState

  let updState :: UPI.State
      updState = CC.cvsUpdateState st'

      -- Transition info as it would look like if all entries were new
      ifNew :: Map Update.ProtocolVersion BlockNo
      ifNew = Map.fromList $ map aux (UPI.candidateProtocolUpdates updState)
       where
        aux ::
          UPE.CandidateProtocolUpdate ->
          (Update.ProtocolVersion, BlockNo)
        aux candidate = (UPE.cpuProtocolVersion candidate, blkNo)

      transition' :: ByronTransition
      transition' =
        case untickedByronLedgerTransition of
          ByronTransitionInfo oldEntries ->
            ByronTransitionInfo $
              -- Candidates that have /just/ become candidates
              let newEntries :: Map Update.ProtocolVersion BlockNo
                  newEntries = ifNew `Map.difference` oldEntries
               in -- Remove any entries that aren't candidates anymore
                  (oldEntries `Map.intersection` ifNew) `Map.union` newEntries

  return
    ByronLedgerState
      { byronLedgerTipBlockNo = NotOrigin blkNo
      , byronLedgerState = st'
      , byronLedgerTransition = transition'
      }

-- | Apply boundary block
--
-- Since boundary blocks don't modify the delegation state, they also don't
-- modify the delegation history.
applyABoundaryBlock ::
  Gen.Config ->
  CC.ABoundaryBlock ByteString ->
  BlockNo ->
  TickedLedgerState ByronBlock mk1 ->
  Except (LedgerError ByronBlock) (LedgerState ByronBlock mk2)
applyABoundaryBlock cfg blk blkNo TickedByronLedgerState{..} = do
  st' <- CC.validateBoundary cfg blk tickedByronLedgerState
  return
    ByronLedgerState
      { byronLedgerTipBlockNo = NotOrigin blkNo
      , byronLedgerState = st'
      , byronLedgerTransition = untickedByronLedgerTransition
      }

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeByronAnnTip :: AnnTip ByronBlock -> Encoding
encodeByronAnnTip = encodeAnnTipIsEBB encodeByronHeaderHash

decodeByronAnnTip :: Decoder s (AnnTip ByronBlock)
decodeByronAnnTip = decodeAnnTipIsEBB decodeByronHeaderHash

encodeByronExtLedgerState :: ExtLedgerState ByronBlock mk -> Encoding
encodeByronExtLedgerState =
  encodeExtLedgerState
    encodeByronLedgerState
    encodeByronChainDepState
    encodeByronAnnTip

encodeByronHeaderState :: HeaderState ByronBlock -> Encoding
encodeByronHeaderState =
  encodeHeaderState
    encodeByronChainDepState
    encodeByronAnnTip

-- | Encode transition info
--
-- We encode the absence of any info separately. This gives us a bit more
-- wiggle room to change our mind about what we store in snapshots, as they
-- typically don't contain any transition info.
--
-- Implementation note: we should have encoded the absence of data with the
-- inclusion of a list length. We didn't, so the decoder is a bit awkward :/
--
-- TODO: If we break compatibility anyway, we might decide to clean this up.
encodeByronTransition :: ByronTransition -> Encoding
encodeByronTransition (ByronTransitionInfo bNos)
  | Map.null bNos = CBOR.encodeWord8 0
  | otherwise =
      CBOR.encodeListLen (fromIntegral (Map.size bNos))
        <> mconcat (map aux (Map.toAscList bNos))
 where
  aux :: (Update.ProtocolVersion, BlockNo) -> Encoding
  aux (Update.ProtocolVersion{pvMajor, pvMinor, pvAlt}, bno) =
    mconcat
      [ CBOR.encodeListLen 4
      , encode pvMajor
      , encode pvMinor
      , encode pvAlt
      , encode bno
      ]

-- | Decode Byron transition info
--
-- See comments for 'encodeByronTransition'.
decodeByronTransition :: Decoder s ByronTransition
decodeByronTransition = do
  ttype <- CBOR.peekTokenType
  fmap ByronTransitionInfo $ case ttype of
    CBOR.TypeUInt -> do
      tag <- CBOR.decodeWord8
      case tag of
        0 -> return $ Map.empty
        _otherwise -> fail "decodeByronTransition: unexpected tag"
    CBOR.TypeListLen -> do
      size <- CBOR.decodeListLen
      Map.fromAscList <$> replicateM size aux
    _otherwise ->
      fail "decodeByronTransition: unexpected token type"
 where
  aux :: Decoder s (Update.ProtocolVersion, BlockNo)
  aux = do
    enforceSize "decodeByronTransition.aux" 4
    pvMajor <- decode
    pvMinor <- decode
    pvAlt <- decode
    bno <- decode
    return (Update.ProtocolVersion{pvMajor, pvMinor, pvAlt}, bno)

encodeByronLedgerState :: LedgerState ByronBlock mk -> Encoding
encodeByronLedgerState ByronLedgerState{..} =
  mconcat
    [ encodeListLen 3
    , encode byronLedgerTipBlockNo
    , encode byronLedgerState
    , encodeByronTransition byronLedgerTransition
    ]

decodeByronLedgerState :: Decoder s (LedgerState ByronBlock mk)
decodeByronLedgerState = do
  enforceSize "ByronLedgerState" 3
  ByronLedgerState
    <$> decode
    <*> decode
    <*> decodeByronTransition

encodeByronQuery :: BlockQuery ByronBlock fp result -> Encoding
encodeByronQuery query = case query of
  GetUpdateInterfaceState -> CBOR.encodeWord8 0

decodeByronQuery :: Decoder s (SomeBlockQuery (BlockQuery ByronBlock))
decodeByronQuery = do
  tag <- CBOR.decodeWord8
  case tag of
    0 -> return $ SomeBlockQuery GetUpdateInterfaceState
    _ -> fail $ "decodeByronQuery: invalid tag " <> show tag

encodeByronResult :: BlockQuery ByronBlock fp result -> result -> Encoding
encodeByronResult query = case query of
  GetUpdateInterfaceState -> toByronCBOR

decodeByronResult ::
  BlockQuery ByronBlock fp result ->
  forall s.
  Decoder s result
decodeByronResult query = case query of
  GetUpdateInterfaceState -> fromByronCBOR

instance CanUpgradeLedgerTables (LedgerState ByronBlock) where
  upgradeTables _ _ = id
