-- | Compatibility shim for the removed @Tables.Kinds@ module.
module Ouroboros.Consensus.Ledger.Tables.Kinds
  {-# DEPRECATED
    "Use Ouroboros.Consensus.Ledger.Tables. The two-argument MapKind has been replaced by the single-argument TableKind."
    #-}
  ( LedgerStateKind
  , StateKind
  ) where

import Ouroboros.Consensus.Ledger.Tables
  ( LedgerStateKind
  , StateKind
  )
