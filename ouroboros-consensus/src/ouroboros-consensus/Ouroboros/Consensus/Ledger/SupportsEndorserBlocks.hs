{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What a voter needs of the ledger to vote for an endorser block: to apply
-- the transactions it endorses, its closure.
--
-- The ledger has no endpoint for this yet, the coming EBODY rule. So this is
-- shaped like 'LedgerSupportsMempool', the ledger's mempool API, and by
-- default applies a closure one transaction after the other with it.
module Ouroboros.Consensus.Ledger.SupportsEndorserBlocks
  ( LedgerSupportsEndorserBlocks (..)
  ) where

import Control.Monad (foldM_)
import Control.Monad.Except (Except)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Ouroboros.Consensus.Block (SlotNo)
import Ouroboros.Consensus.Ledger.Abstract (LedgerConfig)
import Ouroboros.Consensus.Ledger.Basics (TickedLedgerState)
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ApplyTxErr
  , GenTx
  , LedgerSupportsMempool (..)
  , WhetherToIntervene (Intervene)
  )
import Ouroboros.Consensus.Ledger.Tables (ValuesMK)
import Ouroboros.Consensus.Ledger.Tables.Utils (applyDiffs)

class LedgerSupportsMempool blk => LedgerSupportsEndorserBlocks blk where
  -- | Decode a transaction of a closure in the era of the given ledger state,
  -- or say why it is none.
  decodeEbTx :: TickedLedgerState blk mk -> ByteString -> Either Text (GenTx blk)
  decodeEbTx _ _ = Left "the block has no endorser blocks"

  -- | Apply a closure to the ledger state of the block that announced it,
  -- ticked to that block's slot, as a certificate for it would.
  --
  -- The default applies with 'Intervene', so that the ledger does not flip a
  -- transaction's validity flag, which applying a certified closure would not
  -- either.
  applyEbTxs ::
    LedgerConfig blk ->
    SlotNo ->
    [GenTx blk] ->
    TickedLedgerState blk ValuesMK ->
    Except (ApplyTxErr blk) ()
  applyEbTxs cfg slot txs st0 = foldM_ step st0 txs
   where
    step st tx = applyDiffs st . fst <$> applyTx cfg Intervene slot tx st
