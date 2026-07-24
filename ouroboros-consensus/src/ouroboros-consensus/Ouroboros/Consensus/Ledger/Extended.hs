{-# LANGUAGE DefaultSignatures #-}
{- HLINT ignore "Unused LANGUAGE pragma" -}
-- False hint on TypeOperators
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Ledger.Extended
  ( -- * Extended ledger state
    ExtLedgerCfg (..)
  , ExtLedgerState (..)
  , ExtValidationError (..)

    -- * Serialisation
  , decodeDiskExtLedgerState
  , decodeExtLedgerState
  , encodeDiskExtLedgerState
  , encodeExtLedgerState
  , initPerasEpochContextResolver
  , mkPerasEpochContextResolverHandle

    -- * Type family instances
  , LedgerTables (..)
  , Ticked (..)
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Codec.CBOR.Decoding (Decoder, decodeListLenOf)
import Codec.CBOR.Encoding (Encoding, encodeListLen)
import Control.DeepSeq (NFData)
import Control.Monad.Except
import Control.Monad.Trans.Except (except)
import Data.Functor ((<&>))
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Proxy
import Data.SOP.Constraint (All, Top)
import Data.Typeable
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import NoThunks.Class (NoThunks (..))
import Ouroboros.Consensus.Block.Abstract
  ( BlockConfig
  , BlockProtocol
  , CodecConfig
  , GetHeader (getHeader)
  , HeaderHash
  , StandardHash
  , StorageConfig
  , castPoint
  )
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , IsPerasCert (..)
  , PerasRoundNo
  )
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (HardForkIndices))
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Peras.Context
  ( PerasEpochContextNotFoundForRound
  , PerasEpochContextResolver
  , PerasEpochContextResolverHandle (..)
  , StateSupportsPerasEpochContext (..)
  , initPerasEpochContextResolver
  , resolveRoundNo
  , tickPerasEpochContextResolver
  )
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util.CBOR (decodeStrictMaybe, encodeStrictMaybe)
import Ouroboros.Consensus.Util.IOLike (MonadSTM (STM))
import Ouroboros.Consensus.Util.IndexedMemPack

{-------------------------------------------------------------------------------
  Extended ledger state
-------------------------------------------------------------------------------}

data ExtValidationError blk
  = ExtValidationErrorLedger !(LedgerErr LedgerState blk)
  | ExtValidationErrorHeader !(HeaderError blk)
  | ExtValidationErrorPerasEpochContextResolver !PerasEpochContextNotFoundForRound
  | ExtValidationErrorPerasCertInBlock !(PerasError blk)
  deriving Generic

deriving instance
  ( Eq (PerasError blk)
  , LedgerSupportsProtocol blk
  ) =>
  Eq (ExtValidationError blk)
deriving instance
  ( NoThunks (PerasError blk)
  , LedgerSupportsProtocol blk
  ) =>
  NoThunks (ExtValidationError blk)
deriving instance
  ( Show (PerasError blk)
  , LedgerSupportsProtocol blk
  ) =>
  Show (ExtValidationError blk)

-- | Extended ledger state
--
-- This is the combination of the header state and the ledger state proper.
data ExtLedgerState blk mk = ExtLedgerState
  { ledgerState :: !(LedgerState blk mk)
  , headerState :: !(HeaderState blk)
  , perasEpochContextResolver :: !(PerasEpochContextResolver blk)
  , latestPerasCertOnChainRound :: !(StrictMaybe PerasRoundNo)
  }
  deriving Generic

mkPerasEpochContextResolverHandle ::
  MonadSTM m => STM m (ExtLedgerState blk mk) -> PerasEpochContextResolverHandle m blk
mkPerasEpochContextResolverHandle getLedgerStateSTM =
  PerasEpochContextResolverHandle $ perasEpochContextResolver <$> getLedgerStateSTM

deriving instance
  ( EqMK mk
  , LedgerSupportsProtocol blk
  , Eq (PerasEpochContextResolver blk)
  ) =>
  Eq (ExtLedgerState blk mk)
deriving instance
  ( ShowMK mk
  , LedgerSupportsProtocol blk
  , Show (PerasEpochContextResolver blk)
  ) =>
  Show (ExtLedgerState blk mk)

-- | We override 'showTypeOf' to show the type of the block
--
-- This makes debugging a bit easier, as the block gets used to resolve all
-- kinds of type families.
instance
  ( NoThunksMK mk
  , LedgerSupportsProtocol blk
  , NoThunks (PerasEpochContextResolver blk)
  ) =>
  NoThunks (ExtLedgerState blk mk)
  where
  showTypeOf _ = show $ typeRep (Proxy @(ExtLedgerState blk))

type instance HeaderHash (ExtLedgerState blk) = HeaderHash (LedgerState blk)
instance
  ( NFData (HeaderHash blk)
  , NoThunks (HeaderHash blk)
  , Typeable (HeaderHash blk)
  , Show (HeaderHash blk)
  , Ord (HeaderHash blk)
  , Eq (HeaderHash blk)
  ) =>
  StandardHash (ExtLedgerState blk)

instance IsLedger LedgerState blk => GetTip (ExtLedgerState blk) where
  getTip = castPoint . getTip . ledgerState

{-------------------------------------------------------------------------------
  The extended ledger configuration
-------------------------------------------------------------------------------}

-- | " Ledger " configuration for the extended ledger
--
-- Since the extended ledger also does the consensus protocol validation, we
-- also need the consensus config.
newtype ExtLedgerCfg blk = ExtLedgerCfg
  { getExtLedgerCfg :: TopLevelConfig blk
  }
  deriving Generic

instance
  ( ConsensusProtocol (BlockProtocol blk)
  , NoThunks (BlockConfig blk)
  , NoThunks (CodecConfig blk)
  , NoThunks (LedgerConfig blk)
  , NoThunks (StorageConfig blk)
  , NoThunks (HeaderHash blk)
  ) =>
  NoThunks (ExtLedgerCfg blk)

type instance LedgerCfg ExtLedgerState blk = ExtLedgerCfg blk

{-------------------------------------------------------------------------------
  The ticked extended ledger state
-------------------------------------------------------------------------------}

data instance Ticked ExtLedgerState blk mk = TickedExtLedgerState
  { tickedLedgerState :: Ticked LedgerState blk mk
  , ledgerView :: LedgerView (BlockProtocol blk)
  , tickedHeaderState :: Ticked (HeaderState blk)
  , tickedPerasEpochContextResolver :: PerasEpochContextResolver blk
  , tickedLatestPerasCertOnChainRound :: StrictMaybe PerasRoundNo
  }

instance IsLedger LedgerState blk => GetTip (Ticked ExtLedgerState blk) where
  getTip = castPoint . getTip . tickedLedgerState

instance
  ( LedgerSupportsProtocol blk
  , BlockSupportsPeras blk
  , StateSupportsPerasEpochContext blk
  , All Top (HardForkIndices blk)
  ) =>
  IsLedger ExtLedgerState blk
  where
  type LedgerErr ExtLedgerState blk = ExtValidationError blk

  applyChainTickLedgerResult evs cfg slot ExtLedgerState{ledgerState, headerState, latestPerasCertOnChainRound, perasEpochContextResolver} =
    castLedgerResult ledgerResult <&> \tickedLedgerState ->
      let ledgerView :: LedgerView (BlockProtocol blk)
          ledgerView = protocolLedgerView lcfg tickedLedgerState

          tickedHeaderState :: Ticked (HeaderState blk)
          tickedHeaderState =
            tickHeaderState
              (configConsensus $ getExtLedgerCfg cfg)
              ledgerView
              slot
              headerState

          tickedPerasEpochContextResolver :: PerasEpochContextResolver blk
          tickedPerasEpochContextResolver =
            tickPerasEpochContextResolver
              lcfg
              (perasEpochContextResolver, ledgerState, headerState)
              (slot, tickedLedgerState, tickedHeaderState)

          tickedLatestPerasCertOnChainRound :: StrictMaybe PerasRoundNo
          tickedLatestPerasCertOnChainRound = latestPerasCertOnChainRound
       in TickedExtLedgerState{..}
   where
    lcfg :: LedgerConfig blk
    lcfg = configLedger $ getExtLedgerCfg cfg

    ledgerResult = applyChainTickLedgerResult evs lcfg slot ledgerState

applyHelper ::
  forall blk.
  ( HasCallStack
  , LedgerSupportsProtocol blk
  , BlockSupportsPeras blk
  ) =>
  ( HasCallStack =>
    ComputeLedgerEvents ->
    LedgerCfg LedgerState blk ->
    blk ->
    Ticked LedgerState blk ValuesMK ->
    Except
      (LedgerErr LedgerState blk)
      (LedgerResult blk (LedgerState blk DiffMK))
  ) ->
  ComputeLedgerEvents ->
  LedgerCfg ExtLedgerState blk ->
  blk ->
  Ticked ExtLedgerState blk ValuesMK ->
  Except
    (LedgerErr ExtLedgerState blk)
    (LedgerResult blk (ExtLedgerState blk DiffMK))
applyHelper f opts cfg blk TickedExtLedgerState{..} = do
  ledgerResult <-
    withExcept ExtValidationErrorLedger $
      f
        opts
        (configLedger $ getExtLedgerCfg cfg)
        blk
        tickedLedgerState
  hdr <-
    withExcept ExtValidationErrorHeader $
      validateHeader @blk
        (getExtLedgerCfg cfg)
        ledgerView
        (getHeader blk)
        tickedHeaderState

  -- Only when ticking the 'ExtLedgerState' do we need to update the
  -- 'PerasEpochContextResolver'. When applying block on top of a 'Ticked
  -- ExtLedgerState', the 'PerasEpochContextResolver' has already been put to
  -- the right state by the ticking.
  let perasResolver = tickedPerasEpochContextResolver

  -- Update the latest Peras certificate round if the new block contains a
  -- certificate from a round more recent than the currently cached one.
  latestPerasCertOnChainRound <-
    case getPerasCertInBlock blk of
      -- The block does not contain a Peras certificate => keep the previous one
      Nothing -> do
        pure tickedLatestPerasCertOnChainRound
      -- The block contains a Peras certificate => make sure it is valid,
      -- extract its round number, and compare it with the previously stored one
      Just certInBlock -> do
        certInBlockRound <-
          validatePerasCertAndExtractRoundNo
            perasResolver
            certInBlock
        case tickedLatestPerasCertOnChainRound of
          SNothing ->
            pure (SJust certInBlockRound)
          SJust prevLatestCertOnChainRound ->
            pure (SJust (certInBlockRound `max` prevLatestCertOnChainRound))
  pure $
    (\l -> ExtLedgerState l hdr perasResolver latestPerasCertOnChainRound)
      <$> castLedgerResult ledgerResult

-- | Validate a given Peras certificate and extract its round number.
validatePerasCertAndExtractRoundNo ::
  forall blk.
  BlockSupportsPeras blk =>
  PerasEpochContextResolver blk ->
  PerasCert blk ->
  Except (LedgerErr ExtLedgerState blk) PerasRoundNo
validatePerasCertAndExtractRoundNo perasResolver cert = do
  let roundNo = getPerasCertRound cert
  context <-
    withExcept ExtValidationErrorPerasEpochContextResolver $
      except $
        resolveRoundNo perasResolver roundNo
  validatedCert <-
    withExcept ExtValidationErrorPerasCertInBlock $
      except $
        verifyPerasCert context cert
  pure (getPerasCertRound validatedCert)

instance
  ( GetBlockKeySets blk
  , LedgerSupportsProtocol blk
  , BlockSupportsPeras blk
  , StateSupportsPerasEpochContext blk
  , All Top (HardForkIndices blk)
  ) =>
  ApplyBlock ExtLedgerState blk
  where
  applyBlockLedgerResultWithValidation doValidate =
    applyHelper (applyBlockLedgerResultWithValidation doValidate)

  applyBlockLedgerResult =
    applyHelper applyBlockLedgerResult

  reapplyBlockLedgerResult evs cfg blk TickedExtLedgerState{..} =
    (\l -> ExtLedgerState l hdr perasResolver latestPerasCertOnChainRound)
      <$> castLedgerResult ledgerResult
   where
    ledgerResult =
      reapplyBlockLedgerResult
        evs
        (configLedger $ getExtLedgerCfg cfg)
        blk
        tickedLedgerState
    hdr =
      revalidateHeader
        (getExtLedgerCfg cfg)
        ledgerView
        (getHeader blk)
        tickedHeaderState

    -- Only when ticking the 'ExtLedgerState' do we need to update the
    -- 'PerasEpochContextResolver'. When applying block on top of a 'Ticked
    -- ExtLedgerState', the 'PerasEpochContextResolver' has already been put to
    -- the right state by the ticking.
    perasResolver = tickedPerasEpochContextResolver
    latestPerasCertOnChainRound = tickedLatestPerasCertOnChainRound

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeExtLedgerState ::
  (LedgerState blk mk -> Encoding) ->
  (ChainDepState (BlockProtocol blk) -> Encoding) ->
  (AnnTip blk -> Encoding) ->
  (PerasEpochContextResolver blk -> Encoding) ->
  ExtLedgerState blk mk ->
  Encoding
encodeExtLedgerState
  encodeLedgerState
  encodeChainDepState
  encodeAnnTip
  encodePerasEpochContextResolver
  ExtLedgerState
    { ledgerState
    , headerState
    , perasEpochContextResolver
    , latestPerasCertOnChainRound
    } =
    mconcat
      [ encodeListLen 4
      , encodeLedgerState ledgerState
      , encodeHeaderState' headerState
      , encodePerasEpochContextResolver perasEpochContextResolver
      , encodeLatestPerasCertOnChainRound latestPerasCertOnChainRound
      ]
   where
    encodeHeaderState' =
      encodeHeaderState
        encodeChainDepState
        encodeAnnTip

    encodeLatestPerasCertOnChainRound :: StrictMaybe PerasRoundNo -> Encoding
    encodeLatestPerasCertOnChainRound = encodeStrictMaybe toCBOR

encodeDiskExtLedgerState ::
  forall blk.
  ( EncodeDisk blk (LedgerState blk EmptyMK)
  , EncodeDisk blk (ChainDepState (BlockProtocol blk))
  , EncodeDisk blk (AnnTip blk)
  , EncodeDisk blk (PerasEpochContextResolver blk)
  ) =>
  (CodecConfig blk -> ExtLedgerState blk EmptyMK -> Encoding)
encodeDiskExtLedgerState cfg =
  encodeExtLedgerState
    (encodeDisk cfg)
    (encodeDisk cfg)
    (encodeDisk cfg)
    (encodeDisk cfg)

decodeExtLedgerState ::
  (forall s. Decoder s (LedgerState blk EmptyMK)) ->
  (forall s. Decoder s (ChainDepState (BlockProtocol blk))) ->
  (forall s. Decoder s (AnnTip blk)) ->
  (forall s. Decoder s (PerasEpochContextResolver blk)) ->
  (forall s. Decoder s (ExtLedgerState blk EmptyMK))
decodeExtLedgerState
  decodeLedgerState
  decodeChainDepState
  decodeAnnTip
  decodePerasEpochContextResolver = do
    decodeListLenOf 4
    ledgerState <- decodeLedgerState
    headerState <- decodeHeaderState'
    perasEpochContextResolver <- decodePerasEpochContextResolver
    latestPerasCertOnChainRound <- decodeLatestPerasCertOnChainRound
    return
      ExtLedgerState
        { ledgerState
        , headerState
        , perasEpochContextResolver
        , latestPerasCertOnChainRound
        }
   where
    decodeHeaderState' =
      decodeHeaderState
        decodeChainDepState
        decodeAnnTip

    decodeLatestPerasCertOnChainRound :: forall s. Decoder s (StrictMaybe PerasRoundNo)
    decodeLatestPerasCertOnChainRound = decodeStrictMaybe fromCBOR

decodeDiskExtLedgerState ::
  forall blk.
  ( DecodeDisk blk (LedgerState blk EmptyMK)
  , DecodeDisk blk (ChainDepState (BlockProtocol blk))
  , DecodeDisk blk (AnnTip blk)
  , DecodeDisk blk (PerasEpochContextResolver blk)
  ) =>
  (CodecConfig blk -> forall s. Decoder s (ExtLedgerState blk EmptyMK))
decodeDiskExtLedgerState cfg =
  decodeExtLedgerState
    (decodeDisk cfg)
    (decodeDisk cfg)
    (decodeDisk cfg)
    (decodeDisk cfg)

{-------------------------------------------------------------------------------
  Ledger Tables
-------------------------------------------------------------------------------}

instance
  (NoThunks (TxIn blk), NoThunks (TxOut blk), HasLedgerTables LedgerState blk) =>
  HasLedgerTables ExtLedgerState blk
  where
  projectLedgerTables (ExtLedgerState lstate _ _ _) =
    projectLedgerTables lstate
  withLedgerTables (ExtLedgerState lstate hstate perasResolver latestPerasCertOnChainRound) tables =
    ExtLedgerState
      (lstate `withLedgerTables` tables)
      hstate
      perasResolver
      latestPerasCertOnChainRound

instance
  (NoThunks (TxIn blk), NoThunks (TxOut blk), HasLedgerTables (Ticked LedgerState) blk) =>
  HasLedgerTables (Ticked ExtLedgerState) blk
  where
  projectLedgerTables (TickedExtLedgerState lstate _view _hstate _perasResolver _latestPerasCertOnChainRound) =
    projectLedgerTables lstate
  withLedgerTables
    (TickedExtLedgerState lstate view hstate perasResolver latestPerasCertOnChainRound)
    tables =
      TickedExtLedgerState
        (lstate `withLedgerTables` tables)
        view
        hstate
        perasResolver
        latestPerasCertOnChainRound

instance
  CanStowLedgerTables (LedgerState blk) =>
  CanStowLedgerTables (ExtLedgerState blk)
  where
  stowLedgerTables (ExtLedgerState lstate hstate perasResolver latestPerasCertOnChainRound) =
    ExtLedgerState (stowLedgerTables lstate) hstate perasResolver latestPerasCertOnChainRound

  unstowLedgerTables (ExtLedgerState lstate hstate perasResolver latestPerasCertOnChainRound) =
    ExtLedgerState (unstowLedgerTables lstate) hstate perasResolver latestPerasCertOnChainRound

instance
  CanUpgradeLedgerTables LedgerState blk =>
  CanUpgradeLedgerTables ExtLedgerState blk
  where
  upgradeTables (ExtLedgerState st0 _ _ _) (ExtLedgerState st1 _ _ _) =
    upgradeTables st0 st1

instance
  (txout ~ TxOut blk, IndexedMemPack LedgerState blk txout) =>
  IndexedMemPack ExtLedgerState blk txout
  where
  indexedTypeName p (ExtLedgerState st _ _ _) = indexedTypeName p st
  indexedPackedByteCount (ExtLedgerState st _ _ _) = indexedPackedByteCount st
  indexedPackM (ExtLedgerState st _ _ _) = indexedPackM st
  indexedUnpackM (ExtLedgerState st _ _ _) = indexedUnpackM st

instance LedgerTablesAreTrivial LedgerState blk => LedgerTablesAreTrivial ExtLedgerState blk where
  convertMapKind (ExtLedgerState st hst perasResolver latestPerasCertOnChainRound) =
    ExtLedgerState (convertMapKind st) hst perasResolver latestPerasCertOnChainRound

instance SerializeTablesWithHint LedgerState blk => SerializeTablesWithHint ExtLedgerState blk where
  decodeTablesWithHint st = decodeTablesWithHint (ledgerState st)
  encodeTablesWithHint st tbs = encodeTablesWithHint (ledgerState st) tbs
