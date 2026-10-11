module Ouroboros.Consensus.Ledger.Tables.Kinds (MapKind, LedgerStateKind, StateKind) where

import Data.Kind

{-------------------------------------------------------------------------------
  Kinds
-------------------------------------------------------------------------------}

-- | Something that holds two types, which intend to represent /keys/ and
-- /values/.
type MapKind = Type {- key -} -> Type {- value -} -> Type

-- | A @LedgerStateKind@ is the kind of any type that takes a single @MapKind@
-- parameter. The canonical inhabitant is a ledger state applied to a block type,
-- for example @LedgerState blk@.
type LedgerStateKind = MapKind -> Type

-- | A @StateKind@ is the kind of a ledger state *before* it receives
-- its block argument.
--
-- The four inhabitants in this codebase are @[Ticked] [Ext]LedgerState@.
type StateKind = Type -> LedgerStateKind
