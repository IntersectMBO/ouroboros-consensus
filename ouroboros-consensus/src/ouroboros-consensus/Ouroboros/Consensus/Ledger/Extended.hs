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
  , StateRef (..)
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
data ExtLedgerState blk = ExtLedgerState
  { ledgerState :: !(LedgerState blk)
  , headerState :: !(HeaderState blk)
  }
  deriving Generic

deriving instance
  (Eq (LedgerState blk), LedgerSupportsProtocol blk) =>
  Eq (ExtLedgerState blk)
deriving instance
  (Show (LedgerState blk), LedgerSupportsProtocol blk) =>
  Show (ExtLedgerState blk)

-- | We override 'showTypeOf' to show the type of the block
--
-- This makes debugging a bit easier, as the block gets used to resolve all
-- kinds of type families.
instance
  (NoThunks (LedgerState blk), LedgerSupportsProtocol blk) =>
  NoThunks (ExtLedgerState blk)
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

instance IsLedger LedgerState blk => GetTip ExtLedgerState blk where
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

data instance Ticked ExtLedgerState blk = TickedExtLedgerState
  { tickedLedgerState :: Ticked LedgerState blk
  , ledgerView :: LedgerView (BlockProtocol blk)
  , tickedHeaderState :: Ticked (HeaderState blk)
  }

instance StateRefHasState m LedgerState blk => StateRefHasState m ExtLedgerState blk where
  data StateRef m ExtLedgerState blk = ExtStateRef
    { extState :: StateRef m LedgerState blk
    , extHeader :: HeaderState blk
    }

  state (ExtStateRef s h) = ExtLedgerState (state s) h
  mkStateRef (ExtLedgerState a b) tbs = ExtStateRef (mkStateRef a tbs) b

instance StateRefHasState m (Ticked LedgerState) blk => StateRefHasState m (Ticked ExtLedgerState) blk where
  data StateRef m (Ticked ExtLedgerState) blk = TickedExtStateRef
    { extTickedState :: StateRef m (Ticked LedgerState) blk
    , extLedgerView :: LedgerView (BlockProtocol blk)
    , extTickedHeader :: Ticked (HeaderState blk)
    }

  state (TickedExtStateRef s v h) = TickedExtLedgerState (state s) v h
  mkStateRef (TickedExtLedgerState a b c) tbs = TickedExtStateRef (mkStateRef a tbs) b c

instance IsLedger LedgerState blk => GetTip (Ticked ExtLedgerState) blk where
  getTip = castPoint . getTip . tickedLedgerState

instance
  LedgerSupportsProtocol blk =>
  IsLedger ExtLedgerState blk
  where
  type LedgerErr ExtLedgerState blk = ExtValidationError blk

  applyChainTickLedgerResult evs cfg slot (ExtStateRef ledger header) = do
    ledgerResult <- applyChainTickLedgerResult evs lcfg slot ledger
    pure $
      castLedgerResult ledgerResult <&> \tickedLedgerState ->
        let ledgerView :: LedgerView (BlockProtocol blk)
            ledgerView = protocolLedgerView lcfg (state tickedLedgerState)

            tickedHeaderState :: Ticked (HeaderState blk)
            tickedHeaderState =
              tickHeaderState
                (configConsensus $ getExtLedgerCfg cfg)
                ledgerView
                slot
                header
         in TickedExtStateRef tickedLedgerState ledgerView tickedHeaderState
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
    StateRef m (Ticked LedgerState) blk ->
    ExceptT
      (LedgerErr LedgerState blk)
      m
      (LedgerResult blk (StateRef m LedgerState blk))
  ) ->
  ComputeLedgerEvents ->
  LedgerCfg ExtLedgerState blk ->
  blk ->
  StateRef m (Ticked ExtLedgerState) blk ->
  ExceptT
    (LedgerErr ExtLedgerState blk)
    m
    (LedgerResult blk (StateRef m ExtLedgerState blk))
applyHelper f opts cfg blk (TickedExtStateRef tickedLedgerState ledgerView tickedHeaderState) = do
  ledgerResult <-
    withExceptT ExtValidationErrorLedger $
      f
        opts
        (configLedger $ getExtLedgerCfg cfg)
        blk
        tickedLedgerState
  hdr <-
    withExceptT ExtValidationErrorHeader $
      ExceptT $
        pure $
          runExcept $
            validateHeader @blk
              (getExtLedgerCfg cfg)
              ledgerView
              (getHeader blk)
              tickedHeaderState
  pure $ (\l -> ExtStateRef l hdr) <$> castLedgerResult ledgerResult

instance LedgerSupportsProtocol blk => ApplyBlock ExtLedgerState blk where
  applyBlockLedgerResultWithValidation doValidate =
    applyHelper (applyBlockLedgerResultWithValidation doValidate)

  applyBlockLedgerResult =
    applyHelper applyBlockLedgerResult

  reapplyBlockLedgerResult evs cfg blk (TickedExtStateRef tickedLedgerState ledgerView tickedHeaderState) = do
    ledgerResult <-
      reapplyBlockLedgerResult
        evs
        (configLedger $ getExtLedgerCfg cfg)
        blk
        tickedLedgerState
    pure $ (\l -> ExtStateRef l hdr) <$> castLedgerResult ledgerResult
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
  (LedgerState blk -> Encoding) ->
  (ChainDepState (BlockProtocol blk) -> Encoding) ->
  (AnnTip blk -> Encoding) ->
  ExtLedgerState blk ->
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
  ( EncodeDisk blk (LedgerState blk)
  , EncodeDisk blk (ChainDepState (BlockProtocol blk))
  , EncodeDisk blk (AnnTip blk)
  ) =>
  (CodecConfig blk -> ExtLedgerState blk -> Encoding)
encodeDiskExtLedgerState cfg =
  encodeExtLedgerState
    (encodeDisk cfg)
    (encodeDisk cfg)
    (encodeDisk cfg)

decodeExtLedgerState ::
  (forall s. Decoder s (LedgerState blk)) ->
  (forall s. Decoder s (ChainDepState (BlockProtocol blk))) ->
  (forall s. Decoder s (AnnTip blk)) ->
  (forall s. Decoder s (ExtLedgerState blk))
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
  ( DecodeDisk blk (LedgerState blk)
  , DecodeDisk blk (ChainDepState (BlockProtocol blk))
  , DecodeDisk blk (AnnTip blk)
  ) =>
  (CodecConfig blk -> forall s. Decoder s (ExtLedgerState blk))
decodeDiskExtLedgerState cfg =
  decodeExtLedgerState
    (decodeDisk cfg)
    (decodeDisk cfg)
    (decodeDisk cfg)
