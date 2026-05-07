-- | Compatibility shim for the removed @Tables.Combinators@ module.
--
-- The barbie-style combinators (@ltmap@, @lttraverse@, @ltsequence@, @ltap@,
-- @ltpure@, @ltprod@, @ltliftA{,2,3,4}@, @ltzipWith2A@, @ltcollapse@) and the
-- lifted-function helpers (@fn2_1@…@fn2_4@, @(-..->)@, @(:..:)@) were removed
-- when the @LedgerTables@ newtype disappeared. The single concrete table for a
-- ledger state is now @mk blk@ directly, so each former combinator collapses to
-- a plain function call on the table.
--
-- The 'Ouroboros.Consensus.Util.K2' constant bifunctor moved to
-- "Ouroboros.Consensus.Util" and is re-exported here for convenience during
-- the transition.
module Ouroboros.Consensus.Ledger.Tables.Combinators
  {-# DEPRECATED
    "Use Ouroboros.Consensus.Ledger.Tables. The lt* combinators were removed with the LedgerTables newtype; K2 moved to Ouroboros.Consensus.Util."
    #-}
  ( K2 (..)
  ) where

import Ouroboros.Consensus.Util (K2 (..))
