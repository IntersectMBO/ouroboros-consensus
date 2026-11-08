{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What a voter needs of the ledger to vote for an endorser block, beyond the
-- mempool API it applies the endorsed transactions with.
--
-- The ledger has no endpoint for this yet, the coming EBODY rule.
module Ouroboros.Consensus.Ledger.SupportsEndorserBlocks
  ( LedgerSupportsEndorserBlocks (..)
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Ouroboros.Consensus.Block (SlotNo)
import Ouroboros.Consensus.Ledger.Basics (LedgerConfig, TickedLedgerState)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( GenTx
  , LedgerSupportsMempool (..)
  , Validated
  )
import Ouroboros.Consensus.Ledger.Tables (ValuesMK)
import Ouroboros.Consensus.Leios.Types (ProtVerParity (EvenProtVer))

class LedgerSupportsMempool blk => LedgerSupportsEndorserBlocks blk where
  -- | Decode a transaction of a closure in the era of the given ledger state,
  -- or say why it is none.
  decodeEbTx :: TickedLedgerState blk mk -> ByteString -> Either Text (GenTx blk)
  decodeEbTx _ _ = Left "the block has no endorser blocks"

  -- | The protocol version, modulo 2, that the transactions of a closure are
  -- validated under in the given ledger state.
  ebTxsProtVerParity :: TickedLedgerState blk mk -> ProtVerParity
  ebTxsProtVerParity _ = EvenProtVer

  -- | A transaction the node validated before, to reapply it to the given
  -- ledger state with 'reapplyTx', which runs only the checks that depend on
  -- the state; or 'Nothing', then the voter applies it in full.
  assumeValidatedEbTx ::
    LedgerConfig blk ->
    SlotNo ->
    TickedLedgerState blk ValuesMK ->
    GenTx blk ->
    Maybe (Validated (GenTx blk))
  assumeValidatedEbTx _ _ _ _ = Nothing
