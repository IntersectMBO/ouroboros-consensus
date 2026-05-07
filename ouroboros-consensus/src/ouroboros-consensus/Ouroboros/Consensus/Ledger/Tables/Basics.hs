-- | Compatibility shim for the removed @Tables.Basics@ module.
--
-- All entities live in "Ouroboros.Consensus.Ledger.Tables" now. The
-- @LedgerTables@ newtype and the @MapKind@ kind have been replaced (see the
-- module deprecation message and the changelog for details).
module Ouroboros.Consensus.Ledger.Tables.Basics
  {-# DEPRECATED
    "Use Ouroboros.Consensus.Ledger.Tables. The LedgerTables newtype is gone (concrete tables are now indexed by blk directly), and MapKind has been replaced by TableKind."
    #-}
  ( LedgerStateKind
  , StateKind
  , TxIn
  , TxOut
  ) where

import Ouroboros.Consensus.Ledger.Tables
  ( LedgerStateKind
  , StateKind
  , TxIn
  , TxOut
  )
