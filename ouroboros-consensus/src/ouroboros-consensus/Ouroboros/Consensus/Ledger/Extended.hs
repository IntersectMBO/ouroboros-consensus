{- HLINT ignore "Unused LANGUAGE pragma" -}
-- False hint on TypeOperators
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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

    -- * Handles
  , ExtStateHandle (..)
  , TickedExtStateHandle (..)
  , extLedgerState
  , tickedExtLedgerState
  , closeExt
  , closeTickedExt
  , duplicateExt
  , duplicateTickedExt
  , getStatsExt
  , getStatsTickedExt

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

instance IsLedger LedgerState blk => GetTip ExtLedgerState blk where
  getTip = getTip . ledgerState

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

-- | A handle for an 'ExtLedgerState'.
--
-- Plain record: bundles a 'StateHandle' (which owns the on-disk tables) with
-- the pure 'HeaderState'. Constructed and destructed directly — no
-- 'MonadLedger' instance for 'ExtLedgerState' is needed.
data ExtStateHandle m blk = ExtStateHandle
  { extStateHandle :: !(StateHandle m blk)
  , extHeaderState :: !(HeaderState blk)
  }

-- | A handle for a 'Ticked' 'ExtLedgerState'.
data TickedExtStateHandle m blk = TickedExtStateHandle
  { tickedExtStateHandle :: !(TickedStateHandle m blk)
  , tickedExtLedgerView :: !(LedgerView (BlockProtocol blk))
  , tickedExtHeaderState :: !(Ticked (HeaderState blk))
  }

-- | Pure projection of the extended ledger state from an 'ExtStateHandle'.
extLedgerState ::
  MonadLedger m blk => ExtStateHandle m blk -> ExtLedgerState blk
extLedgerState (ExtStateHandle s h) = ExtLedgerState (state s) h

-- | Pure projection of the ticked extended ledger state from a
-- 'TickedExtStateHandle'.
tickedExtLedgerState ::
  MonadLedger m blk =>
  TickedExtStateHandle m blk -> Ticked ExtLedgerState blk
tickedExtLedgerState (TickedExtStateHandle s lv h) =
  TickedExtLedgerState (tickedState s) lv h

closeExt :: MonadLedger m blk => ExtStateHandle m blk -> m ()
closeExt (ExtStateHandle s _) = close s

closeTickedExt :: MonadLedger m blk => TickedExtStateHandle m blk -> m ()
closeTickedExt (TickedExtStateHandle s _ _) = closeTicked s

duplicateExt ::
  MonadLedger m blk => ExtStateHandle m blk -> m (ExtStateHandle m blk)
duplicateExt (ExtStateHandle s h) = flip ExtStateHandle h <$> duplicate s

duplicateTickedExt ::
  MonadLedger m blk =>
  TickedExtStateHandle m blk -> m (TickedExtStateHandle m blk)
duplicateTickedExt (TickedExtStateHandle s lv h) =
  (\s' -> TickedExtStateHandle s' lv h) <$> duplicateTicked s

getStatsExt :: MonadLedger m blk => ExtStateHandle m blk -> Statistics
getStatsExt (ExtStateHandle s _) = getStats s

getStatsTickedExt ::
  MonadLedger m blk => TickedExtStateHandle m blk -> Statistics
getStatsTickedExt (TickedExtStateHandle s _ _) = getStatsTicked s

instance IsLedger LedgerState blk => GetTip (Ticked ExtLedgerState) blk where
  getTip = getTip . tickedLedgerState

instance
  LedgerSupportsProtocol blk =>
  IsLedger ExtLedgerState blk
  where
  type LedgerErr ExtLedgerState blk = ExtValidationError blk
  type Handle ExtLedgerState = ExtStateHandle
  type TickedHandle ExtLedgerState = TickedExtStateHandle

  applyChainTickLedgerResult evs cfg slot (ExtStateHandle ledger header) = do
    ledgerResult <- applyChainTickLedgerResult evs lcfg slot ledger
    pure $
      castLedgerResult ledgerResult <&> \tickedLedgerState' ->
        let lv :: LedgerView (BlockProtocol blk)
            lv = protocolLedgerView lcfg (tickedState tickedLedgerState')

            tickedHeaderState' :: Ticked (HeaderState blk)
            tickedHeaderState' =
              tickHeaderState
                (configConsensus $ getExtLedgerCfg cfg)
                lv
                slot
                header
         in TickedExtStateHandle tickedLedgerState' lv tickedHeaderState'
   where
    lcfg :: LedgerConfig blk
    lcfg = configLedger $ getExtLedgerCfg cfg

applyHelper ::
  forall m blk.
  (HasCallStack, LedgerSupportsProtocol blk, Monad m, MonadLedger m blk) =>
  ( HasCallStack =>
    ComputeLedgerEvents ->
    LedgerCfg LedgerState blk ->
    blk ->
    TickedStateHandle m blk ->
    ExceptT
      (LedgerErr LedgerState blk)
      m
      (LedgerResult blk (StateHandle m blk))
  ) ->
  ComputeLedgerEvents ->
  LedgerCfg ExtLedgerState blk ->
  blk ->
  TickedExtStateHandle m blk ->
  ExceptT
    (LedgerErr ExtLedgerState blk)
    m
    (LedgerResult blk (ExtStateHandle m blk))
applyHelper f opts cfg blk (TickedExtStateHandle ticked lv tickedHdr) = do
  ledgerResult <-
    withExceptT ExtValidationErrorLedger $
      f
        opts
        (configLedger $ getExtLedgerCfg cfg)
        blk
        ticked
  hdr <-
    withExceptT ExtValidationErrorHeader $
      ExceptT $
        pure $
          runExcept $
            validateHeader @blk
              (getExtLedgerCfg cfg)
              lv
              (getHeader blk)
              tickedHdr
  pure $ (\l -> ExtStateHandle l hdr) <$> castLedgerResult ledgerResult

instance LedgerSupportsProtocol blk => ApplyBlock ExtLedgerState blk where
  applyBlockLedgerResultWithValidation doValidate =
    applyHelper (applyBlockLedgerResultWithValidation doValidate)

  applyBlockLedgerResult =
    applyHelper applyBlockLedgerResult

  reapplyBlockLedgerResult evs cfg blk (TickedExtStateHandle ticked lv tickedHdr) = do
    ledgerResult <-
      reapplyBlockLedgerResult
        evs
        (configLedger $ getExtLedgerCfg cfg)
        blk
        ticked
    pure $ (\l -> ExtStateHandle l hdr) <$> castLedgerResult ledgerResult
   where
    hdr =
      revalidateHeader
        (getExtLedgerCfg cfg)
        lv
        (getHeader blk)
        tickedHdr

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
