{-# LANGUAGE OverloadedStrings #-}

-- | See 'BlockSupportsLeios'.
module Ouroboros.Consensus.Block.SupportsLeios
  ( BlockSupportsLeios (..)
  , ReadLedgerTables
  , defaultApplyEndorserBlock
  , defaultBlockLeiosCert
  , defaultLedgerAnnouncedLeiosPoint
  , defaultDecodeEbTx
  , defaultEbTxsProtVerParity
  , defaultAssumeValidatedEbTx
  ) where

import Cardano.Crypto.Leios (LeiosCert)
import Cardano.Ledger.Binary (DecoderError (DecoderErrorCustom))
import Ouroboros.Consensus.Block.Abstract (Header, SlotNo)
import Ouroboros.Consensus.Ledger.Basics (LedgerConfig, LedgerState, TickedLedgerState)
import Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, Validated)
import Ouroboros.Consensus.Ledger.Tables.Basics (LedgerTables)
import Ouroboros.Consensus.Ledger.Tables.MapKind (DiffMK, EmptyMK, KeysMK, ValuesMK)
import Ouroboros.Consensus.Leios.Types
  ( LeiosPoint
  , LeiosTx
  , ProtVerParity (EvenProtVer)
  , SerialisedEndorserBlock
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

  -- | Decode a transaction of a closure in the era of the given header, the
  -- one announcing it, to validate it.
  decodeEbTx :: Header blk -> LeiosTx -> Either DecoderError (GenTx blk)

  -- | The protocol version, modulo 2, that the transactions of a closure are
  -- validated under in the given ledger state.
  ebTxsProtVerParity :: TickedLedgerState blk mk -> ProtVerParity

  -- | A transaction the node validated before, to reapply it to the given
  -- ledger state with 'reapplyTx', which runs only the checks that depend on
  -- the state; or 'Nothing', then the voter applies it in full.
  assumeValidatedEbTx ::
    LedgerConfig blk ->
    SlotNo ->
    TickedLedgerState blk ValuesMK ->
    GenTx blk ->
    Maybe (Validated (GenTx blk))

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

-- | 'decodeEbTx' for a block type without endorser blocks.
defaultDecodeEbTx :: Header blk -> LeiosTx -> Either DecoderError (GenTx blk)
defaultDecodeEbTx _hdr _tx =
  Left $ DecoderErrorCustom "EndorserBlock" "this block type has no endorser blocks"

-- | 'ebTxsProtVerParity' for a block type without endorser blocks.
defaultEbTxsProtVerParity :: TickedLedgerState blk mk -> ProtVerParity
defaultEbTxsProtVerParity _st = EvenProtVer

-- | 'assumeValidatedEbTx' for a block type without endorser blocks.
defaultAssumeValidatedEbTx ::
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk ValuesMK ->
  GenTx blk ->
  Maybe (Validated (GenTx blk))
defaultAssumeValidatedEbTx _cfg _slot _st _tx = Nothing
