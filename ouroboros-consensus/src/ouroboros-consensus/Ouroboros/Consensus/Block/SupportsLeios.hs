{-# LANGUAGE OverloadedStrings #-}

-- | See 'BlockSupportsLeios'.
module Ouroboros.Consensus.Block.SupportsLeios
  ( BlockSupportsLeios (..)
  , ReadLedgerTables
  , LookupValidatedTxs
  , RecordValidatedTx
  , defaultApplyEndorserBlock
  , defaultBlockLeiosCert
  , defaultLedgerAnnouncedLeiosPoint
  , defaultValidateLeiosClosure
  , LeiosClosureValidation (..)
  , LeiosClosureInvalid (..)
  ) where

import Cardano.Crypto.Leios (LeiosCert)
import Cardano.Ledger.Binary (DecoderError (DecoderErrorCustom))
import Data.Text (Text)
import Ouroboros.Consensus.Ledger.Basics (LedgerConfig, LedgerState)
import Ouroboros.Consensus.Ledger.Tables.Basics (LedgerTables)
import Ouroboros.Consensus.Ledger.Tables.MapKind (DiffMK, EmptyMK, KeysMK, ValuesMK)
import Ouroboros.Consensus.Leios.Types
  ( LeiosPoint
  , ProtVerParity (EvenProtVer)
  , SerialisedEndorserBlock
  , TxHash
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

  -- | Validate the closure of the endorser block the tip of the ledger state
  -- announced, to vote for it: check the block against what one may hold,
  -- then apply its transactions one after the other, as a certificate for it
  -- would, reapplying those validated before.
  validateLeiosClosure ::
    Monad m =>
    LedgerConfig blk ->
    ReadLedgerTables m blk ->
    LookupValidatedTxs m ->
    RecordValidatedTx m ->
    SerialisedEndorserBlock ->
    LedgerState blk EmptyMK ->
    m LeiosClosureValidation

-- | What 'validateLeiosClosure' made of a closure.
data LeiosClosureValidation = MkLeiosClosureValidation
  { closureVerdict :: !(Either LeiosClosureInvalid ())
  , closureParity :: !ProtVerParity
  -- ^ The protocol version parity the transactions were validated under.
  , closureReapplied :: !Int
  -- ^ How many transactions were reapplied, as validated before.
  , closureValidated :: ![TxHash]
  -- ^ The transactions validated in full, before the verdict.
  }

-- | Why a closure is not one to vote for.
data LeiosClosureInvalid
  = -- | The block references more than an endorser block may hold, by what it
    -- claims of its transactions or by what they turn out to take.
    ClosureOverCapacity
  | -- | The closure does not hold one transaction per reference.
    ClosureIncomplete
  | -- | The block or one of its transactions does not decode, or does not
    -- apply.
    ClosureInvalid !Text
  deriving (Eq, Show)

-- | Read the values of the given keys from the ledger tables of a state.
--
-- Reading lives in @m@ because the tables may be on disk. It yields the
-- values of those keys that exist in the tables.
type ReadLedgerTables m blk = LedgerTables blk KeysMK -> m (LedgerTables blk ValuesMK)

-- | Which of the given transactions the node validated before, under the
-- given protocol version parity.
--
-- Looking up lives in @m@ because the node keeps what it validated outside
-- the ledger state. A transaction not known to be validated before is
-- validated in full.
type LookupValidatedTxs m = ProtVerParity -> [TxHash] -> m [Bool]

-- | Record that a transaction was validated in full under a protocol version
-- parity, as soon as it was, so that the work outlives a validation cut short.
type RecordValidatedTx m = ProtVerParity -> TxHash -> m ()

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

-- | 'validateLeiosClosure' for a block type without endorser blocks.
defaultValidateLeiosClosure ::
  Monad m =>
  LedgerConfig blk ->
  ReadLedgerTables m blk ->
  LookupValidatedTxs m ->
  RecordValidatedTx m ->
  SerialisedEndorserBlock ->
  LedgerState blk EmptyMK ->
  m LeiosClosureValidation
defaultValidateLeiosClosure _cfg _readTables _wasValidated _recordValidated _closure _st =
  pure
    MkLeiosClosureValidation
      { closureVerdict = Left (ClosureInvalid "this block type has no endorser blocks")
      , closureParity = EvenProtVer
      , closureReapplied = 0
      , closureValidated = []
      }
