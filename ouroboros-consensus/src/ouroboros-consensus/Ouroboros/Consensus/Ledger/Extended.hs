{-# LANGUAGE BangPatterns #-}
{- HLINT ignore "Unused LANGUAGE pragma" -}
-- False hint on TypeOperators
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
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
  , valuesMKEncoder
  , valuesMKDecoder
  , SerializeTablesWithHint (..)
  , SerializeTablesHint
  , defaultEncodeTablesWithHint
  , defaultDecodeTablesWithHint
  ) where

import Codec.CBOR.Decoding (Decoder, decodeListLenOf)
import qualified Codec.CBOR.Decoding as CBOR
import Codec.CBOR.Encoding (Encoding, encodeListLen)
import qualified Codec.CBOR.Encoding as CBOR
import Control.Monad.Except
import Data.Functor ((<&>))
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.MemPack (packByteString, unpackMonadFail)
import Data.Proxy
import Data.SOP.BasicFunctors ((:.:) (..))
import Data.SOP.Constraint
import Data.SOP.Sing (lengthSList)
import Data.SOP.Strict (hcfoldMap, hcpure, hsequence')
import Data.Typeable
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import NoThunks.Class (NoThunks (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.LedgerStateType
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
  (Eq (LedgerState blk mk), LedgerSupportsProtocol blk) =>
  Eq (ExtLedgerState blk mk)
deriving instance
  (Show (LedgerState blk mk), LedgerSupportsProtocol blk) =>
  Show (ExtLedgerState blk mk)

-- | We override 'showTypeOf' to show the type of the block
--
-- This makes debugging a bit easier, as the block gets used to resolve all
-- kinds of type families.
instance
  (NoThunks (LedgerState blk mk), LedgerSupportsProtocol blk) =>
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

instance
  ( HasLedgerTables ExtLedgerState blk
  , HasLedgerTables (TickedL ExtLedgerState) blk
  , LedgerSupportsProtocol blk
  ) =>
  ApplyBlock ExtLedgerState blk
  where
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

instance
  HasLedgerTables LedgerState blk =>
  HasLedgerTables ExtLedgerState blk
  where
  projectLedgerTables (ExtLedgerState lstate _) =
    projectLedgerTables lstate
  withLedgerTables (ExtLedgerState lstate hstate) tables =
    ExtLedgerState
      (lstate `withLedgerTables` tables)
      hstate

instance
  HasLedgerTables (TickedL LedgerState) blk =>
  HasLedgerTables (TickedL ExtLedgerState) blk
  where
  projectLedgerTables (TickedL (TickedExtLedgerState lstate _view _hstate)) =
    projectLedgerTables (TickedL lstate)
  withLedgerTables
    (TickedL (TickedExtLedgerState lstate view hstate))
    tables =
      TickedL $
        TickedExtLedgerState
          (unTickedL $ TickedL lstate `withLedgerTables` tables)
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
  IndexedMemPack LedgerState blk table =>
  IndexedMemPack ExtLedgerState blk table
  where
  type IndexedValue ExtLedgerState table blk = IndexedValue LedgerState table blk
  indexedTypeName _ p q = indexedTypeName (Proxy @LedgerState) p q
  indexedPackedByteCount _ p q (ExtLedgerState st _) = indexedPackedByteCount (Proxy @LedgerState) p q st
  indexedPackM _ p q (ExtLedgerState st _) = indexedPackM (Proxy @LedgerState) p q st
  indexedUnpackM _ p q (ExtLedgerState st _) = indexedUnpackM (Proxy @LedgerState) p q st

{-------------------------------------------------------------------------------
  Serialization Codecs
-------------------------------------------------------------------------------}

-- | Default encoder of @'LedgerTables' l ''ValuesMK'@ to be used by the
-- in-memory backing store.
valuesMKEncoder ::
  forall l blk.
  All (SerializeTablesWithHint l blk) (TablesForBlock blk) =>
  l blk EmptyMK ->
  LedgerTables blk ValuesMK ->
  Encoding
valuesMKEncoder st (LedgerTables tbs) =
  mconcat
    [ CBOR.encodeListLen (fromIntegral $ lengthSList (Proxy @(TablesForBlock blk)))
    , hcfoldMap (Proxy @(SerializeTablesWithHint l blk)) (encodeTablesWithHint st) tbs
    ]

-- | Default decoder of @'LedgerTables' l ''ValuesMK'@ to be used by the
-- in-memory backing store.
valuesMKDecoder ::
  forall l blk s.
  All (SerializeTablesWithHint l blk) (TablesForBlock blk) =>
  l blk EmptyMK ->
  Decoder s (LedgerTables blk ValuesMK)
valuesMKDecoder st = do
  _ <- CBOR.decodeListLenOf (lengthSList (Proxy @(TablesForBlock blk)))
  LedgerTables
    <$> hsequence' (hcpure (Proxy @(SerializeTablesWithHint l blk)) (Comp $ decodeTablesWithHint st))

-- | When decoding the tables and in particular the UTxO set we want
-- to share data in the TxOuts in the same way the Ledger did (see the
-- @Share (TxOut era)@ instances). We need to provide the state in the
-- HFC case so that we can call 'eraDecoder' and also to extract the
-- interns from the state.
--
-- As we will decode with 'eraDecoder' we also need to use such era
-- for the encoding thus we need the hint also in the encoding.
--
-- See @SerializeTablesWithHint (LedgerState (HardForkBlock
-- (CardanoBlock c)))@ for a good example, the rest of the instances
-- are somewhat degenerate.
class SerializeTablesWithHint l blk tag where
  encodeTablesWithHint ::
    SerializeTablesHint l (LedgerTables blk ValuesMK) ->
    Table ValuesMK blk tag ->
    Encoding
  decodeTablesWithHint ::
    SerializeTablesHint l (LedgerTables blk ValuesMK) ->
    Decoder s (Table ValuesMK blk tag)

-- This is just for the BackingStore Lockstep tests. Once V1 is gone
-- we can inline it above.

-- | The hint for 'SerializeTablesWithHint'
type SerializeTablesHint :: StateKind -> Type -> Type
type family SerializeTablesHint l values :: Type

type instance SerializeTablesHint l (LedgerTables blk ValuesMK) = l blk EmptyMK

defaultEncodeTablesWithHint ::
  (TableConstraints blk tag, MemPack (Value tag blk)) =>
  SerializeTablesHint l (LedgerTables blk ValuesMK) ->
  Table ValuesMK blk tag ->
  Encoding
defaultEncodeTablesWithHint _ (Table (ValuesMK tbs)) =
  mconcat
    [ CBOR.encodeMapLen (fromIntegral $ Map.size tbs)
    , Map.foldMapWithKey
        ( \k v ->
            mconcat
              [ CBOR.encodeBytes (packByteString k)
              , CBOR.encodeBytes (packByteString v)
              ]
        )
        tbs
    ]

defaultDecodeTablesWithHint ::
  (TableConstraints blk tag, MemPack (Value tag blk)) =>
  SerializeTablesHint l (LedgerTables blk ValuesMK) ->
  Decoder s (Table ValuesMK blk tag)
defaultDecodeTablesWithHint _ = do
  n <- CBOR.decodeMapLen
  Table . ValuesMK <$> go n Map.empty
 where
  go 0 m = pure m
  go n !m = do
    (k, v) <- (,) <$> (unpackMonadFail =<< CBOR.decodeBytes) <*> (unpackMonadFail =<< CBOR.decodeBytes)
    go (n - 1) (Map.insert k v m)
