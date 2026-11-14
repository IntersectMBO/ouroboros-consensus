{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | See 'BlockSupportsLeios'.
module Ouroboros.Consensus.Block.SupportsLeios
  ( BlockSupportsLeios (..)
  , defaultBlockLeiosCert
  , defaultDecodeLedgerEndorserBlock
  , defaultLedgerAnnouncedEb
  ) where

import Cardano.Crypto.Leios (LeiosCert)
import Cardano.Ledger.Binary (DecoderError (DecoderErrorCustom))
import Ouroboros.Consensus.Ledger.Basics (LedgerConfig, LedgerState)
import Ouroboros.Consensus.Ledger.Tables.Basics (LedgerTables)
import Ouroboros.Consensus.Ledger.Tables.MapKind (DiffMK, KeysMK, ValuesMK)
import Ouroboros.Consensus.Leios.Types (LeiosClosure, LeiosPoint)

-- | What a block and its ledger say about Leios.
class BlockSupportsLeios blk where
  -- | An endorser block with the transactions it references, as the ledger of
  -- the block type applies it.
  data LedgerEndorserBlock blk

  -- | The Leios certificate the block contains, if any, which certifies the
  -- endorser block its predecessor announced.
  blockLeiosCert :: blk -> Maybe LeiosCert

  -- | Decode the closure of the endorser block that a block certifies, in the
  -- era of that block: certification never crosses eras.
  decodeLedgerEndorserBlock :: blk -> LeiosClosure -> Either DecoderError (LedgerEndorserBlock blk)

  -- | Given an endorser block, get the key-sets that we need to apply it to a
  -- ledger state.
  getEndorserBlockKeySets :: LedgerEndorserBlock blk -> LedgerTables blk KeysMK

  -- | The endorser block the tip of the ledger state announced, if any.
  ledgerAnnouncedEb :: LedgerState blk mk -> Maybe LeiosPoint

  -- | Apply an endorser block to the ledger state whose tip announced it,
  -- without validating its transactions: the certificate of the next block
  -- vouches for them.
  applyLedgerEndorserBlock ::
    LedgerConfig blk ->
    LedgerEndorserBlock blk ->
    LedgerState blk ValuesMK ->
    LedgerState blk DiffMK

-- | 'blockLeiosCert' for a block type that never contains a Leios certificate.
defaultBlockLeiosCert :: blk -> Maybe LeiosCert
defaultBlockLeiosCert _blk = Nothing

-- | 'decodeLedgerEndorserBlock' for a block type without endorser blocks.
defaultDecodeLedgerEndorserBlock :: blk -> LeiosClosure -> Either DecoderError a
defaultDecodeLedgerEndorserBlock _blk _closure =
  Left $ DecoderErrorCustom "EndorserBlock" "this block type has no endorser blocks"

-- | 'ledgerAnnouncedEb' for a ledger that never announces an endorser block.
defaultLedgerAnnouncedEb :: LedgerState blk mk -> Maybe LeiosPoint
defaultLedgerAnnouncedEb _st = Nothing
