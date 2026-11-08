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
import Ouroboros.Consensus.Ledger.Basics (TickedLedgerState)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( GenTx
  , LedgerSupportsMempool (..)
  )

class LedgerSupportsMempool blk => LedgerSupportsEndorserBlocks blk where
  -- | Decode a transaction of a closure in the era of the given ledger state,
  -- or say why it is none.
  decodeEbTx :: TickedLedgerState blk mk -> ByteString -> Either Text (GenTx blk)
  decodeEbTx _ _ = Left "the block has no endorser blocks"
