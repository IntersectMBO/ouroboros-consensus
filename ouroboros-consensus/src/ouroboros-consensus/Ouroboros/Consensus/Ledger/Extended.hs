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
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.Serialisation

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
  }
  deriving Generic

deriving instance
  (LedgerSupportsProtocol blk, Eq (LedgerState blk mk)) =>
  Eq (ExtLedgerState blk mk)
deriving instance
  (LedgerSupportsProtocol blk, Show (LedgerState blk mk)) =>
  Show (ExtLedgerState blk mk)

-- | We override 'showTypeOf' to show the type of the block
--
-- This makes debugging a bit easier, as the block gets used to resolve all
-- kinds of type families.
instance
  (LedgerSupportsProtocol blk, NoThunks (LedgerState blk mk)) =>
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
  }

instance IsLedger LedgerState blk => GetTip (Ticked ExtLedgerState blk) where
  getTip = castPoint . getTip . tickedLedgerState

instance
  (LedgerSupportsProtocol blk, BlockSupportsUTxOHD blk) =>
  IsLedger ExtLedgerState blk
  where
  type LedgerErr ExtLedgerState blk = ExtValidationError blk

  applyChainTickLedgerResult evs cfg slot (ExtLedgerState ledger header) =
    castLedgerResult ledgerResult <&> \(tickedLedgerState, diff) ->
      let ledgerView :: LedgerView (BlockProtocol blk)
          ledgerView = protocolLedgerView lcfg tickedLedgerState

          tickedHeaderState :: Ticked (HeaderState blk)
          tickedHeaderState =
            tickHeaderState
              (configConsensus $ getExtLedgerCfg cfg)
              ledgerView
              slot
              header
       in (TickedExtLedgerState{..}, diff)
   where
    lcfg :: LedgerConfig blk
    lcfg = configLedger $ getExtLedgerCfg cfg

    ledgerResult = applyChainTickLedgerResult evs lcfg slot ledger

applyHelper ::
  forall blk mk.
  (HasCallStack, LedgerSupportsProtocol blk) =>
  ( HasCallStack =>
    ComputeLedgerEvents ->
    LedgerCfg LedgerState blk ->
    blk ->
    Values blk ->
    Ticked LedgerState blk EmptyMK ->
    Except
      (LedgerErr LedgerState blk)
      (LedgerResult blk (LedgerState blk EmptyMK, Diff blk))
  ) ->
  ComputeLedgerEvents ->
  LedgerCfg ExtLedgerState blk ->
  blk ->
  Values blk ->
  Ticked ExtLedgerState blk EmptyMK ->
  Except
    (LedgerErr ExtLedgerState blk)
    (LedgerResult blk (ExtLedgerState blk EmptyMK, Diff blk))
applyHelper f opts cfg blk vals TickedExtLedgerState{..} = do
  ledgerResult <-
    withExcept ExtValidationErrorLedger $
      f
        opts
        (configLedger $ getExtLedgerCfg cfg)
        blk
        vals
        tickedLedgerState
  hdr <-
    withExcept ExtValidationErrorHeader $
      validateHeader @blk
        (getExtLedgerCfg cfg)
        ledgerView
        (getHeader blk)
        tickedHeaderState
  pure $ (\(l, d) -> (ExtLedgerState l hdr, d)) <$> castLedgerResult ledgerResult

instance (LedgerSupportsProtocol blk, BlockSupportsUTxOHD blk) => ApplyBlock ExtLedgerState blk where
  applyBlockLedgerResultWithValidation doValidate =
    applyHelper (applyBlockLedgerResultWithValidation doValidate)

  applyBlockLedgerResult =
    applyHelper applyBlockLedgerResult

  reapplyBlockLedgerResult evs cfg blk vals TickedExtLedgerState{..} =
    (\(l, d) -> (ExtLedgerState l hdr, d)) <$> castLedgerResult ledgerResult
   where
    ledgerResult =
      reapplyBlockLedgerResult
        evs
        (configLedger $ getExtLedgerCfg cfg)
        blk
        vals
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
  forall blk mk.
  ( EncodeDisk blk (LedgerState blk mk)
  , EncodeDisk blk (ChainDepState (BlockProtocol blk))
  , EncodeDisk blk (AnnTip blk)
  ) =>
  (CodecConfig blk -> ExtLedgerState blk mk -> Encoding)
encodeDiskExtLedgerState cfg =
  encodeExtLedgerState
    (encodeDisk cfg)
    (encodeDisk cfg)
    (encodeDisk cfg)

decodeExtLedgerState ::
  (forall s. Decoder s (LedgerState blk mk)) ->
  (forall s. Decoder s (ChainDepState (BlockProtocol blk))) ->
  (forall s. Decoder s (AnnTip blk)) ->
  (forall s. Decoder s (ExtLedgerState blk mk))
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
  forall blk mk.
  ( DecodeDisk blk (LedgerState blk mk)
  , DecodeDisk blk (ChainDepState (BlockProtocol blk))
  , DecodeDisk blk (AnnTip blk)
  ) =>
  (CodecConfig blk -> forall s. Decoder s (ExtLedgerState blk mk))
decodeDiskExtLedgerState cfg =
  decodeExtLedgerState
    (decodeDisk cfg)
    (decodeDisk cfg)
    (decodeDisk cfg)
