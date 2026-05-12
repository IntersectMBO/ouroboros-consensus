{- HLINT ignore "Unused LANGUAGE pragma" -}
-- False hint on TypeOperators
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
  , Ticked (..)
  ) where

import Codec.CBOR.Decoding (Decoder, decodeListLenOf)
import Codec.CBOR.Encoding (Encoding, encodeListLen)
import Control.Monad.Except
import Data.Functor ((<&>))
import Data.Proxy
import Data.Typeable
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Extended ledger state
-------------------------------------------------------------------------------}

data ExtValidationError blk
  = ExtValidationErrorLedger !(LedgerErr LedgerState blk)
  | ExtValidationErrorHeader !(HeaderError blk)
  deriving Generic

deriving instance
  (Eq (LedgerErr LedgerState blk), Eq (HeaderError blk)) => Eq (ExtValidationError blk)
deriving instance
  (NoThunks (LedgerErr LedgerState blk), NoThunks (HeaderError blk)) =>
  NoThunks (ExtValidationError blk)
deriving instance
  (Show (LedgerErr LedgerState blk), Show (HeaderError blk)) => Show (ExtValidationError blk)

-- | Extended ledger state
--
-- This is the combination of the header state and the ledger state proper.
data ExtLedgerState m blk = ExtLedgerState
  { ledgerState :: !(LedgerState m blk)
  , headerState :: !(HeaderState blk)
  }
  deriving Generic

deriving instance
  (Eq (LedgerState m blk), Eq (HeaderState blk)) =>
  Eq (ExtLedgerState m blk)
deriving instance
  (Show (LedgerState m blk), Show (HeaderState blk)) =>
  Show (ExtLedgerState m blk)

-- | We override 'showTypeOf' to show the type of the block
--
-- This makes debugging a bit easier, as the block gets used to resolve all
-- kinds of type families.
instance
  (NoThunks (LedgerState m blk), NoThunks (HeaderState blk), Typeable m, Typeable blk) =>
  NoThunks (ExtLedgerState m blk)
  where
  showTypeOf _ = show $ typeRep (Proxy @(ExtLedgerState m blk))

type instance HeaderHash (ExtLedgerState m blk) = HeaderHash (LedgerState m blk)
instance StandardHash blk => StandardHash (ExtLedgerState m blk)

instance GetTip LedgerState blk => GetTip ExtLedgerState blk where
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

instance NoThunks (TopLevelConfig blk) => NoThunks (ExtLedgerCfg blk)

type instance LedgerCfg ExtLedgerState blk = ExtLedgerCfg blk

{-------------------------------------------------------------------------------
  The ticked extended ledger state
-------------------------------------------------------------------------------}

data instance Ticked ExtLedgerState m blk = TickedExtLedgerState
  { tickedLedgerState :: Ticked LedgerState m blk
  , ledgerView :: LedgerView (BlockProtocol blk)
  , tickedHeaderState :: Ticked (HeaderState blk)
  }

instance GetTip (Ticked LedgerState) blk => GetTip (Ticked ExtLedgerState) blk where
  getTip = castPoint . getTip . tickedLedgerState

instance
  ( NoThunks (LedgerCfg ExtLedgerState blk)
  , Show (ExtValidationError blk)
  , Eq (ExtValidationError blk)
  , NoThunks (ExtValidationError blk)
  , GetTip ExtLedgerState blk
  , GetTip (Ticked ExtLedgerState) blk
  , IsLedger LedgerState blk
  , LedgerSupportsProtocol blk
  ) =>
  IsLedger ExtLedgerState blk
  where
  type LedgerErr ExtLedgerState blk = ExtValidationError blk

  applyChainTickLedgerResult evs cfg slot (ExtLedgerState ledger header) = do
    lrRes <- applyChainTickLedgerResult evs lcfg slot ledger
    pure $
      lrRes <&> \tickedLedgerState ->
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

applyHelper ::
  forall m blk.
  (HasCallStack, LedgerSupportsProtocol blk, Monad m) =>
  ( HasCallStack =>
    ComputeLedgerEvents ->
    LedgerCfg LedgerState blk ->
    blk ->
    Ticked LedgerState m blk ->
    ExceptT
      (LedgerErr LedgerState blk)
      m
      (LedgerResult blk (LedgerState m blk))
  ) ->
  ComputeLedgerEvents ->
  LedgerCfg ExtLedgerState blk ->
  blk ->
  Ticked ExtLedgerState m blk ->
  ExceptT
    (LedgerErr ExtLedgerState blk)
    m
    (LedgerResult blk (ExtLedgerState m blk))
applyHelper f opts cfg blk TickedExtLedgerState{..} = do
  ledgerResult <-
    withExceptT ExtValidationErrorLedger $
      f
        opts
        (configLedger $ getExtLedgerCfg cfg)
        blk
        tickedLedgerState
  hdr <-
    ExceptT $
      pure $
        runExcept $
          withExcept ExtValidationErrorHeader $
            validateHeader @blk
              (getExtLedgerCfg cfg)
              ledgerView
              (getHeader blk)
              tickedHeaderState
  pure $ (\l -> ExtLedgerState l hdr) <$> castLedgerResult ledgerResult

instance
  ( ApplyBlock LedgerState blk
  , IsLedger ExtLedgerState blk
  , LedgerSupportsProtocol blk
  ) =>
  ApplyBlock ExtLedgerState blk
  where
  applyBlockLedgerResultWithValidation doValidate =
    applyHelper (applyBlockLedgerResultWithValidation doValidate)

  applyBlockLedgerResult =
    applyHelper applyBlockLedgerResult

  reapplyBlockLedgerResult evs cfg blk TickedExtLedgerState{..} = do
    res <-
      reapplyBlockLedgerResult
        evs
        (configLedger $ getExtLedgerCfg cfg)
        blk
        tickedLedgerState
    pure $ (\l -> ExtLedgerState l hdr) <$> res
   where
    hdr =
      revalidateHeader
        (getExtLedgerCfg cfg)
        ledgerView
        (getHeader blk)
        tickedHeaderState

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeExtLedgerState ::
  (LedgerState m blk -> Encoding) ->
  (ChainDepState (BlockProtocol blk) -> Encoding) ->
  (AnnTip blk -> Encoding) ->
  ExtLedgerState m blk ->
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
  forall m blk.
  ( EncodeDisk blk (LedgerState m blk)
  , EncodeDisk blk (ChainDepState (BlockProtocol blk))
  , EncodeDisk blk (AnnTip blk)
  ) =>
  (CodecConfig blk -> ExtLedgerState m blk -> Encoding)
encodeDiskExtLedgerState cfg =
  encodeExtLedgerState
    (encodeDisk cfg)
    (encodeDisk cfg)
    (encodeDisk cfg)

decodeExtLedgerState ::
  (forall s. Decoder s (LedgerState m blk)) ->
  (forall s. Decoder s (ChainDepState (BlockProtocol blk))) ->
  (forall s. Decoder s (AnnTip blk)) ->
  (forall s. Decoder s (ExtLedgerState m blk))
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
  forall m blk.
  ( DecodeDisk blk (LedgerState m blk)
  , DecodeDisk blk (ChainDepState (BlockProtocol blk))
  , DecodeDisk blk (AnnTip blk)
  ) =>
  (CodecConfig blk -> forall s. Decoder s (ExtLedgerState m blk))
decodeDiskExtLedgerState cfg =
  decodeExtLedgerState
    (decodeDisk cfg)
    (decodeDisk cfg)
    (decodeDisk cfg)

{-------------------------------------------------------------------------------
  Ledger Tables
-------------------------------------------------------------------------------}

-- instance
--   (NoThunks (TxIn blk), NoThunks (TxOut blk), HasLedgerTables LedgerState blk) =>
--   HasLedgerTables ExtLedgerState blk
--   where
--   projectLedgerTables (ExtLedgerState lstate _) =
--     projectLedgerTables lstate
--   withLedgerTables (ExtLedgerState lstate hstate) tables =
--     ExtLedgerState
--       (lstate `withLedgerTables` tables)
--       hstate

-- instance
--   (NoThunks (TxIn blk), NoThunks (TxOut blk), HasLedgerTables (Ticked LedgerState) blk) =>
--   HasLedgerTables (Ticked ExtLedgerState) blk
--   where
--   projectLedgerTables (TickedExtLedgerState lstate _view _hstate) =
--     projectLedgerTables lstate
--   withLedgerTables
--     (TickedExtLedgerState lstate view hstate)
--     tables =
--       TickedExtLedgerState
--         (lstate `withLedgerTables` tables)
--         view
--         hstate

-- instance
--   CanStowLedgerTables (LedgerState blk) =>
--   CanStowLedgerTables (ExtLedgerState blk)
--   where
--   stowLedgerTables (ExtLedgerState lstate hstate) =
--     ExtLedgerState (stowLedgerTables lstate) hstate

--   unstowLedgerTables (ExtLedgerState lstate hstate) =
--     ExtLedgerState (unstowLedgerTables lstate) hstate

-- instance
--   CanUpgradeLedgerTables LedgerState blk =>
--   CanUpgradeLedgerTables ExtLedgerState blk
--   where
--   upgradeTables (ExtLedgerState st0 _) (ExtLedgerState st1 _) =
--     upgradeTables st0 st1

-- instance
--   (txout ~ TxOut blk, IndexedMemPack LedgerState blk txout) =>
--   IndexedMemPack ExtLedgerState blk txout
--   where
--   indexedTypeName p (ExtLedgerState st _) = indexedTypeName p st
--   indexedPackedByteCount (ExtLedgerState st _) = indexedPackedByteCount st
--   indexedPackM (ExtLedgerState st _) = indexedPackM st
--   indexedUnpackM (ExtLedgerState st _) = indexedUnpackM st

-- instance LedgerTablesAreTrivial LedgerState blk => LedgerTablesAreTrivial ExtLedgerState blk where
--   convertMapKind (ExtLedgerState st hst) = ExtLedgerState (convertMapKind st) hst

-- instance SerializeTablesWithHint LedgerState blk => SerializeTablesWithHint ExtLedgerState blk where
--   decodeTablesWithHint st = decodeTablesWithHint (ledgerState st)
--   encodeTablesWithHint st tbs = encodeTablesWithHint (ledgerState st) tbs
