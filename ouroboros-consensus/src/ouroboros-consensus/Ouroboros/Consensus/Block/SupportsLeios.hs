{-# LANGUAGE OverloadedStrings #-}

-- | See 'BlockSupportsLeios'.
module Ouroboros.Consensus.Block.SupportsLeios
  ( BlockSupportsLeios (..)
  , ReadLedgerTables
  , defaultApplyEndorserBlock
  , defaultBlockLeiosCert
  , defaultLedgerAnnouncedLeiosPoint
  ) where

import Cardano.Crypto.Leios (LeiosCert)
import Cardano.Ledger.Binary (DecoderError (DecoderErrorCustom))
import Ouroboros.Consensus.Ledger.Basics (LedgerConfig, LedgerState)
import Ouroboros.Consensus.Ledger.Tables.Basics (LedgerTables)
import Ouroboros.Consensus.Ledger.Tables.MapKind (DiffMK, EmptyMK, KeysMK, ValuesMK)
import Ouroboros.Consensus.Leios.Types (LeiosPoint, SerialisedEndorserBlock)

-- | What a block and its ledger say about Leios.
class BlockSupportsLeios blk where
  -- | The Leios certificate the block contains, if any, which certifies the
  -- endorser block its predecessor announced.
  blockLeiosCert :: blk -> Maybe LeiosCert

  -- | The endorser block announced by the ranking block at the tip of the
  -- ledger state, if any.
  ledgerAnnouncedLeiosPoint :: LedgerState blk mk -> Maybe LeiosPoint

  -- | Apply the endorser block the tip of the ledger state announced, without
  -- validating its transactions: the certificate of the next block vouches
  -- for them.
  --
  -- Decode the endorser block in the era of the slot in which it was announced.
  applyEndorserBlock ::
    Monad m =>
    LedgerConfig blk ->
    ReadLedgerTables m blk ->
    SerialisedEndorserBlock ->
    LedgerState blk EmptyMK ->
    m (Either DecoderError (LedgerState blk DiffMK))

-- | Read the values of the given keys from the ledger tables of a state.
--
-- Reading lives in @m@ because the tables may be on disk.
type ReadLedgerTables m blk = LedgerTables blk KeysMK -> m (LedgerTables blk ValuesMK)

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
