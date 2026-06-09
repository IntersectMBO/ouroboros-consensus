{-# LANGUAGE DefaultSignatures #-}
{- HLINT ignore "Unused LANGUAGE pragma" -}
-- False hint on TypeOperators
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  , mkPerasEpochContextResolverHandle

    -- * Type family instances
  , LedgerTables (..)
  , Ticked (..)
  ) where

import Codec.CBOR.Decoding (Decoder, decodeListLenOf)
import Codec.CBOR.Encoding (Encoding, encodeListLen)
import Control.DeepSeq (NFData)
import Control.Monad.Except
import Data.Functor ((<&>))
import Data.Proxy
import Data.Typeable
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import NoThunks.Class (NoThunks (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Peras.Context
  ( PerasEpochContextResolver
  , PerasEpochContextResolverHandle (..)
  )
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util.IOLike (MonadSTM (STM))
import Ouroboros.Consensus.Util.IndexedMemPack

{-------------------------------------------------------------------------------
  Extended ledger state
-------------------------------------------------------------------------------}

data ExtValidationError blk
  = ExtValidationErrorLedger !(LedgerErr LedgerState blk)
  | ExtValidationErrorHeader !(HeaderError blk)
  deriving Generic

deriving instance LedgerSupportsProtocol blk => Eq (ExtValidationError blk)
deriving instance LedgerSupportsProtocol blk => NoThunks (ExtValidationError blk)
deriving instance LedgerSupportsProtocol blk => Show (ExtValidationError blk)

-- | Extended ledger state
--
-- This is the combination of the header state and the ledger state proper.
data ExtLedgerState blk mk = ExtLedgerState
  { ledgerState :: !(LedgerState blk mk)
  , headerState :: !(HeaderState blk)
  , perasEpochContextResolver :: !(PerasEpochContextResolver blk)
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
  }

instance IsLedger LedgerState blk => GetTip (Ticked ExtLedgerState blk) where
  getTip = castPoint . getTip . tickedLedgerState

instance
  ( LedgerSupportsProtocol blk
  , Show (PerasEpochContextResolver blk)
  , Eq (PerasEpochContextResolver blk)
  , NoThunks (PerasEpochContextResolver blk)
  ) =>
  IsLedger ExtLedgerState blk
  where
  type LedgerErr ExtLedgerState blk = ExtValidationError blk

  applyChainTickLedgerResult evs cfg slot (ExtLedgerState ledger header perasResolver) =
    castLedgerResult ledgerResult <&> \tickedLedgerState ->
      let ledgerView :: LedgerView (BlockProtocol blk)
          ledgerView = protocolLedgerView lcfg tickedLedgerState

          tickedHeaderState :: Ticked (HeaderState blk)
          tickedHeaderState =
            tickHeaderState
              (configConsensus $ getExtLedgerCfg cfg)
              ledgerView
              slot
              header
       in TickedExtLedgerState{..}
   where
    lcfg :: LedgerConfig blk
    lcfg = configLedger $ getExtLedgerCfg cfg

    ledgerResult = applyChainTickLedgerResult evs lcfg slot ledger

    -- [TODO EPOCH CONTEXT PLUMBING/UPDATING] We need to understand if this needs extra
    -- care or not.
    tickedPerasEpochContextResolver = perasResolver

applyHelper ::
  forall blk.
  (HasCallStack, LedgerSupportsProtocol blk) =>
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
  -- [TODO EPOCH CONTEXT PLUMBING/UPDATING] We need to understand if this needs extra care or not.
  let perasResolver = tickedPerasEpochContextResolver
  pure $ (\l -> ExtLedgerState l hdr perasResolver) <$> castLedgerResult ledgerResult

instance
  ( GetBlockKeySets blk
  , LedgerSupportsProtocol blk
  , Show (PerasEpochContextResolver blk)
  , Eq (PerasEpochContextResolver blk)
  , NoThunks (PerasEpochContextResolver blk)
  ) =>
  ApplyBlock ExtLedgerState blk
  where
  applyBlockLedgerResultWithValidation doValidate =
    applyHelper (applyBlockLedgerResultWithValidation doValidate)

  applyBlockLedgerResult =
    applyHelper applyBlockLedgerResult

  reapplyBlockLedgerResult evs cfg blk TickedExtLedgerState{..} =
    (\l -> ExtLedgerState l hdr perasResolver) <$> castLedgerResult ledgerResult
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

    -- [TODO EPOCH CONTEXT PLUMBING/UPDATING] We need to understand if this needs extra care or not.
    perasResolver = tickedPerasEpochContextResolver

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
  ExtLedgerState{ledgerState, headerState, perasEpochContextResolver} =
    mconcat
      [ encodeListLen 3
      , encodeLedgerState ledgerState
      , encodeHeaderState' headerState
      , encodePerasEpochContextResolver perasEpochContextResolver
      ]
   where
    encodeHeaderState' =
      encodeHeaderState
        encodeChainDepState
        encodeAnnTip

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
    decodeListLenOf 3
    ledgerState <- decodeLedgerState
    headerState <- decodeHeaderState'
    perasEpochContextResolver <- decodePerasEpochContextResolver
    return ExtLedgerState{ledgerState, headerState, perasEpochContextResolver}
   where
    decodeHeaderState' =
      decodeHeaderState
        decodeChainDepState
        decodeAnnTip

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
  projectLedgerTables (ExtLedgerState lstate _ _) =
    projectLedgerTables lstate
  withLedgerTables (ExtLedgerState lstate hstate perasResolver) tables =
    ExtLedgerState
      (lstate `withLedgerTables` tables)
      hstate
      perasResolver

instance
  (NoThunks (TxIn blk), NoThunks (TxOut blk), HasLedgerTables (Ticked LedgerState) blk) =>
  HasLedgerTables (Ticked ExtLedgerState) blk
  where
  projectLedgerTables (TickedExtLedgerState lstate _view _hstate _perasResolver) =
    projectLedgerTables lstate
  withLedgerTables
    (TickedExtLedgerState lstate view hstate perasResolver)
    tables =
      TickedExtLedgerState
        (lstate `withLedgerTables` tables)
        view
        hstate
        perasResolver

instance
  CanStowLedgerTables (LedgerState blk) =>
  CanStowLedgerTables (ExtLedgerState blk)
  where
  stowLedgerTables (ExtLedgerState lstate hstate perasResolver) =
    ExtLedgerState (stowLedgerTables lstate) hstate perasResolver

  unstowLedgerTables (ExtLedgerState lstate hstate perasResolver) =
    ExtLedgerState (unstowLedgerTables lstate) hstate perasResolver

instance
  CanUpgradeLedgerTables LedgerState blk =>
  CanUpgradeLedgerTables ExtLedgerState blk
  where
  upgradeTables (ExtLedgerState st0 _ _) (ExtLedgerState st1 _ _) =
    upgradeTables st0 st1

instance
  (txout ~ TxOut blk, IndexedMemPack LedgerState blk txout) =>
  IndexedMemPack ExtLedgerState blk txout
  where
  indexedTypeName p (ExtLedgerState st _ _) = indexedTypeName p st
  indexedPackedByteCount (ExtLedgerState st _ _) = indexedPackedByteCount st
  indexedPackM (ExtLedgerState st _ _) = indexedPackM st
  indexedUnpackM (ExtLedgerState st _ _) = indexedUnpackM st

instance LedgerTablesAreTrivial LedgerState blk => LedgerTablesAreTrivial ExtLedgerState blk where
  convertMapKind (ExtLedgerState st hst perasResolver) = ExtLedgerState (convertMapKind st) hst perasResolver

instance SerializeTablesWithHint LedgerState blk => SerializeTablesWithHint ExtLedgerState blk where
  decodeTablesWithHint st = decodeTablesWithHint (ledgerState st)
  encodeTablesWithHint st tbs = encodeTablesWithHint (ledgerState st) tbs
