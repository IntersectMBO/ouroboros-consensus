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

  -- | Put the transactions of an endorser block into the body of the block
  -- that certifies it. This is the form node-to-client ChainSync sends.
  --
  -- The header stays the same, so its body hash doesn't match the body.
  -- The transactions are decoded in the era of the block: an endorser block
  -- is always certified in the same era it was announced in.
  --
  -- By default this returns 'Left', which suits block types that have no
  -- endorser blocks.
  inlineEndorserBlockTxs :: SerialisedEndorserBlock -> blk -> Either DecoderError blk
  inlineEndorserBlockTxs _serialised _blk =
    Left $ DecoderErrorCustom "EndorserBlock" "this block type has no endorser blocks"

-- | Read the values of the given keys from the ledger tables of a state.
--
-- Reading lives in @m@ because the tables may be on disk. It yields the
-- values of those keys that exist in the tables.
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
