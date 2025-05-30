{- HLINT ignore "Unused LANGUAGE pragma" -}
-- False hint on TypeOperators
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

    -- * Type family instances
  , LedgerTables (..)
  , Ticked (..)
  ) where

import Codec.CBOR.Decoding (Decoder, decodeListLenOf)
import Codec.CBOR.Encoding (Encoding, encodeListLen)
import Control.Monad.Except
import Data.Functor ((<&>))
import Data.MemPack
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
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util.IndexedMemPack

{-------------------------------------------------------------------------------
  Extended ledger state
-------------------------------------------------------------------------------}

data ExtValidationError blk
  = ExtValidationErrorLedger !(LedgerError blk)
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
  }
  deriving Generic

deriving instance
  (EqMK mk, LedgerSupportsProtocol blk) =>
  Eq (ExtLedgerState blk mk)
deriving instance
  (ShowMK mk, LedgerSupportsProtocol blk) =>
  Show (ExtLedgerState blk mk)

-- | We override 'showTypeOf' to show the type of the block
--
-- This makes debugging a bit easier, as the block gets used to resolve all
-- kinds of type families.
instance
  (NoThunksMK mk, LedgerSupportsProtocol blk) =>
  NoThunks (ExtLedgerState blk mk)
  where
  showTypeOf _ = show $ typeRep (Proxy @(ExtLedgerState blk))

type instance HeaderHash (ExtLedgerState blk) = HeaderHash (LedgerState blk)
instance
  ( NoThunks (HeaderHash blk)
  , Typeable (HeaderHash blk)
  , Show (HeaderHash blk)
  , Ord (HeaderHash blk)
  , Eq (HeaderHash blk)
  ) =>
  StandardHash (ExtLedgerState blk)

instance IsLedger (LedgerState blk) => GetTip (ExtLedgerState blk) where
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

type instance LedgerCfg (ExtLedgerState blk) = ExtLedgerCfg blk

{-------------------------------------------------------------------------------
  The ticked extended ledger state
-------------------------------------------------------------------------------}

data instance Ticked (ExtLedgerState blk) mk = TickedExtLedgerState
  { tickedLedgerState :: Ticked (LedgerState blk) mk
  , ledgerView :: LedgerView (BlockProtocol blk)
  , tickedHeaderState :: Ticked (HeaderState blk)
  }

instance IsLedger (LedgerState blk) => GetTip (Ticked (ExtLedgerState blk)) where
  getTip = castPoint . getTip . tickedLedgerState

instance
  LedgerSupportsProtocol blk =>
  IsLedger (ExtLedgerState blk)
  where
  type LedgerErr (ExtLedgerState blk) = ExtValidationError blk

  type AuxLedgerEvent (ExtLedgerState blk) = AuxLedgerEvent (LedgerState blk)

  applyChainTickLedgerResult evs cfg slot (ExtLedgerState ledger header) =
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

applyHelper ::
  forall blk.
  (HasCallStack, LedgerSupportsProtocol blk) =>
  ( HasCallStack =>
    ComputeLedgerEvents ->
    LedgerCfg (LedgerState blk) ->
    blk ->
    Ticked (LedgerState blk) ValuesMK ->
    Except
      (LedgerErr (LedgerState blk))
      (LedgerResult (LedgerState blk) (LedgerState blk DiffMK))
  ) ->
  ComputeLedgerEvents ->
  LedgerCfg (ExtLedgerState blk) ->
  blk ->
  Ticked (ExtLedgerState blk) ValuesMK ->
  Except
    (LedgerErr (ExtLedgerState blk))
    (LedgerResult (ExtLedgerState blk) (ExtLedgerState blk DiffMK))
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
  pure $ (\l -> ExtLedgerState l hdr) <$> castLedgerResult ledgerResult

instance LedgerSupportsProtocol blk => ApplyBlock (ExtLedgerState blk) blk where
  applyBlockLedgerResultWithValidation doValidate =
    applyHelper (applyBlockLedgerResultWithValidation doValidate)

  applyBlockLedgerResult =
    applyHelper applyBlockLedgerResult

  reapplyBlockLedgerResult evs cfg blk TickedExtLedgerState{..} =
    (\l -> ExtLedgerState l hdr) <$> castLedgerResult ledgerResult
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

  getBlockKeySets = castLedgerTables . getBlockKeySets @(LedgerState blk)

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeExtLedgerState ::
  (LedgerState blk mk -> Encoding) ->
  (ChainDepState (BlockProtocol blk) -> Encoding) ->
  (AnnTip blk -> Encoding) ->
  ExtLedgerState blk mk ->
  Encoding
encodeExtLedgerState
  encodeLedgerState
  encodeChainDepState
  encodeAnnTip
  ExtLedgerState{ledgerState, headerState} =
    mconcat
      [ encodeListLen 2
      , encodeLedgerState ledgerState
      , encodeHeaderState' headerState
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
  ) =>
  (CodecConfig blk -> ExtLedgerState blk EmptyMK -> Encoding)
encodeDiskExtLedgerState cfg =
  encodeExtLedgerState
    (encodeDisk cfg)
    (encodeDisk cfg)
    (encodeDisk cfg)

decodeExtLedgerState ::
  (forall s. Decoder s (LedgerState blk EmptyMK)) ->
  (forall s. Decoder s (ChainDepState (BlockProtocol blk))) ->
  (forall s. Decoder s (AnnTip blk)) ->
  (forall s. Decoder s (ExtLedgerState blk EmptyMK))
decodeExtLedgerState
  decodeLedgerState
  decodeChainDepState
  decodeAnnTip = do
    decodeListLenOf 2
    ledgerState <- decodeLedgerState
    headerState <- decodeHeaderState'
    return ExtLedgerState{ledgerState, headerState}
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
  ) =>
  (CodecConfig blk -> forall s. Decoder s (ExtLedgerState blk EmptyMK))
decodeDiskExtLedgerState cfg =
  decodeExtLedgerState
    (decodeDisk cfg)
    (decodeDisk cfg)
    (decodeDisk cfg)

{-------------------------------------------------------------------------------
  Ledger Tables
-------------------------------------------------------------------------------}

type instance TxIn (ExtLedgerState blk) = TxIn (LedgerState blk)
type instance TxOut (ExtLedgerState blk) = TxOut (LedgerState blk)

instance
  ( HasLedgerTables (LedgerState blk)
  , NoThunks (TxOut (LedgerState blk))
  , NoThunks (TxIn (LedgerState blk))
  , Show (TxOut (LedgerState blk))
  , Show (TxIn (LedgerState blk))
  , Eq (TxOut (LedgerState blk))
  , Ord (TxIn (LedgerState blk))
  , MemPack (TxIn (LedgerState blk))
  ) =>
  HasLedgerTables (ExtLedgerState blk)
  where
  projectLedgerTables (ExtLedgerState lstate _) =
    castLedgerTables (projectLedgerTables lstate)
  withLedgerTables (ExtLedgerState lstate hstate) tables =
    ExtLedgerState
      (lstate `withLedgerTables` castLedgerTables tables)
      hstate

instance
  LedgerTablesAreTrivial (LedgerState blk) =>
  LedgerTablesAreTrivial (ExtLedgerState blk)
  where
  convertMapKind (ExtLedgerState x y) = ExtLedgerState (convertMapKind x) y

instance
  LedgerTablesAreTrivial (Ticked (LedgerState blk)) =>
  LedgerTablesAreTrivial (Ticked (ExtLedgerState blk))
  where
  convertMapKind (TickedExtLedgerState x y z) =
    TickedExtLedgerState (convertMapKind x) y z

instance
  ( HasLedgerTables (Ticked (LedgerState blk))
  , NoThunks (TxOut (LedgerState blk))
  , NoThunks (TxIn (LedgerState blk))
  , Show (TxOut (LedgerState blk))
  , Show (TxIn (LedgerState blk))
  , Eq (TxOut (LedgerState blk))
  , Ord (TxIn (LedgerState blk))
  , MemPack (TxIn (LedgerState blk))
  ) =>
  HasLedgerTables (Ticked (ExtLedgerState blk))
  where
  projectLedgerTables (TickedExtLedgerState lstate _view _hstate) =
    castLedgerTables (projectLedgerTables lstate)
  withLedgerTables
    (TickedExtLedgerState lstate view hstate)
    tables =
      TickedExtLedgerState
        (lstate `withLedgerTables` castLedgerTables tables)
        view
        hstate

instance
  CanStowLedgerTables (LedgerState blk) =>
  CanStowLedgerTables (ExtLedgerState blk)
  where
  stowLedgerTables (ExtLedgerState lstate hstate) =
    ExtLedgerState (stowLedgerTables lstate) hstate

  unstowLedgerTables (ExtLedgerState lstate hstate) =
    ExtLedgerState (unstowLedgerTables lstate) hstate

instance
  (txout ~ (TxOut (LedgerState blk)), IndexedMemPack (LedgerState blk EmptyMK) txout) =>
  IndexedMemPack (ExtLedgerState blk EmptyMK) txout
  where
  indexedTypeName (ExtLedgerState st _) = indexedTypeName @(LedgerState blk EmptyMK) @txout st
  indexedPackedByteCount (ExtLedgerState st _) = indexedPackedByteCount st
  indexedPackM (ExtLedgerState st _) = indexedPackM st
  indexedUnpackM (ExtLedgerState st _) = indexedUnpackM st

instance SerializeTablesWithHint (LedgerState blk) => SerializeTablesWithHint (ExtLedgerState blk) where
  decodeTablesWithHint st = castLedgerTables <$> decodeTablesWithHint (ledgerState st)
  encodeTablesWithHint st tbs = encodeTablesWithHint (ledgerState st) (castLedgerTables tbs)
