{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

-- | See 'BlockSupportsLeios'.
module Ouroboros.Consensus.Block.SupportsLeios
  ( BlockSupportsLeios (..)
  , ReadLedgerTables
  , LookupTxCache
  , RecordTxCache
  , defaultApplyEndorserBlock
  , defaultBlockLeiosCert
  , defaultLedgerAnnouncedLeiosPoint
  , defaultValidateEndorserBlock
  , InvalidEndorserBlock (..)
  , ValidationSummary (..)
  ) where

import Cardano.Crypto.Leios (LeiosCert)
import Cardano.Ledger.Binary (DecoderError (DecoderErrorCustom), Version)
import Data.Typeable (Typeable, cast)
import Ouroboros.Consensus.Ledger.Basics (LedgerConfig, LedgerState)
import Ouroboros.Consensus.Ledger.Tables.Basics (LedgerTables)
import Ouroboros.Consensus.Ledger.Tables.MapKind (DiffMK, EmptyMK, KeysMK, ValuesMK)
import Ouroboros.Consensus.Leios.Types
  ( LeiosPoint
  , SerialisedEndorserBlock
  , TxHash
  , TxOffset
  )

-- | What a block and its ledger say about Leios.
class BlockSupportsLeios blk where
  -- | The Leios certificate the block contains, if any, which certifies the
  -- endorser block its predecessor announced.
  blockLeiosCert :: blk -> Maybe LeiosCert

  -- | The endorser block the tip of the ledger state announced, if any, as the
  -- ledger records it to check a certificate against.
  ledgerAnnouncedLeiosPoint :: LedgerState blk mk -> Maybe LeiosPoint

  -- | Apply the endorser block the tip of the ledger state announced, without
  -- validating its transactions: the certificate of the next block vouches
  -- for them.
  --
  -- The endorser block decodes in the era of the state, as certification never
  -- crosses eras.
  applyEndorserBlock ::
    Monad m =>
    LedgerConfig blk ->
    ReadLedgerTables m blk ->
    SerialisedEndorserBlock ->
    LedgerState blk EmptyMK ->
    m (Either DecoderError (LedgerState blk DiffMK))

  -- | Validate the endorser block the tip of the ledger state
  -- announced, to vote for it: check the block against what one may hold,
  -- then apply its transactions one after the other, as a certificate for it
  -- would, reapplying those validated before.
  validateEndorserBlock ::
    Monad m =>
    LedgerConfig blk ->
    ReadLedgerTables m blk ->
    LookupTxCache m ->
    RecordTxCache m ->
    SerialisedEndorserBlock ->
    LedgerState blk EmptyMK ->
    m (Either InvalidEndorserBlock ValidationSummary)

-- | What validating a valid endorser block took.
data ValidationSummary = MkValidationSummary
  { summaryReapplied :: !Int
  -- ^ How many transactions were validated before, so only reapplied.
  , summaryValidated :: !Int
  -- ^ How many transactions were validated in full.
  }
  deriving (Eq, Show)

-- | Why an endorser block is not one to vote for, as the block type tells.
data InvalidEndorserBlock
  = forall e. (Eq e, Show e, Typeable e) => InvalidEndorserBlock e
  | -- | The block type, or the era of the ledger state, has no endorser blocks.
    EndorserBlocksUnsupported

instance Eq InvalidEndorserBlock where
  InvalidEndorserBlock e == InvalidEndorserBlock e' = Just e == cast e'
  EndorserBlocksUnsupported == EndorserBlocksUnsupported = True
  _ == _ = False

instance Show InvalidEndorserBlock where
  showsPrec d (InvalidEndorserBlock e) = showsPrec d e
  showsPrec _ EndorserBlocksUnsupported = showString "EndorserBlocksUnsupported"

-- | Read the values of the given keys from the ledger tables of a state.
--
-- Reading lives in @m@ because the tables may be on disk. It yields the
-- values of those keys that exist in the tables.
type ReadLedgerTables m blk = LedgerTables blk KeysMK -> m (LedgerTables blk ValuesMK)

-- | Which of the given transactions the node's tx cache holds as validated
-- under the given major protocol version.
--
-- A transaction the cache does not hold is validated in full.
type LookupTxCache m = Version -> [TxHash] -> m [Bool]

-- | Record in the node's tx cache that the transaction at this offset of the
-- endorser block was validated in full under a major protocol version, as soon
-- as it was, so that the work outlives a validation cut short.
type RecordTxCache m = Version -> TxOffset -> TxHash -> m ()

-- | 'blockLeiosCert' for a block type that never contains a Leios certificate.
defaultBlockLeiosCert :: blk -> Maybe LeiosCert
defaultBlockLeiosCert _blk = Nothing

-- | 'ledgerAnnouncedLeiosPoint' for a block type without endorser blocks.
defaultLedgerAnnouncedLeiosPoint :: LedgerState blk mk -> Maybe LeiosPoint
defaultLedgerAnnouncedLeiosPoint _st = Nothing

-- | 'applyEndorserBlock' for a block type without endorser blocks.
defaultApplyEndorserBlock ::
  Monad m =>
  LedgerConfig blk ->
  ReadLedgerTables m blk ->
  SerialisedEndorserBlock ->
  LedgerState blk EmptyMK ->
  m (Either DecoderError (LedgerState blk DiffMK))
defaultApplyEndorserBlock _cfg _readTables _serialised _st =
  pure . Left $ DecoderErrorCustom "EndorserBlock" "this block type has no endorser blocks"

-- | 'validateEndorserBlock' for a block type without endorser blocks.
defaultValidateEndorserBlock ::
  Monad m =>
  LedgerConfig blk ->
  ReadLedgerTables m blk ->
  LookupTxCache m ->
  RecordTxCache m ->
  SerialisedEndorserBlock ->
  LedgerState blk EmptyMK ->
  m (Either InvalidEndorserBlock ValidationSummary)
defaultValidateEndorserBlock _cfg _readTables _lookupTxCache _recordTxCache _eb _st =
  pure $ Left EndorserBlocksUnsupported
