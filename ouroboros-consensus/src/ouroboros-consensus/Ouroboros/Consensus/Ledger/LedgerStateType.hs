{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | The types basic definitions for a LedgerState
module Ouroboros.Consensus.Ledger.LedgerStateType
  ( -- * Kinds
    F2
  , LedgerStateKind
  , StateKind

    -- * LedgerState type
  , LedgerState
  , TickedLedgerState
  , TickedL (..)
  )
where

import Data.Kind (Type)
import Ouroboros.Consensus.Ticked
import Ouroboros.Network.Block (HeaderHash, StandardHash)

-- | The kind of 'Data.Map'
type F2 = Type -> Type -> Type

-- | The kind of 'LedgerState blk'
type LedgerStateKind = F2 -> Type

-- | The kind of 'LedgerState'
type StateKind = Type -> LedgerStateKind

-- | Ledger state associated with a block
--
-- This is the Consensus notion of a Ledger /ledger state/. Each block type is
-- associated with one of the Ledger types for the /ledger state/. Virtually
-- every concept in this codebase revolves around this type, or the referenced
-- @blk@. Whenever we use the type variable @l@ we intend to signal that the
-- expected instantiation is either a 'LedgerState' or some wrapper over it
-- (like the 'Ouroboros.Consensus.Ledger.Extended.ExtLedgerState').
--
-- This type is parametrized over @mk :: 'MapKind'@ to express the
-- 'LedgerTables' contained in such a 'LedgerState'. See 'LedgerTables' for a
-- more thorough description.
--
-- The main operations we can do with a 'LedgerState' are /ticking/ (defined in
-- 'IsLedger'), and /applying a block/ (defined in
-- 'Ouroboros.Consensus.Ledger.Abstract.ApplyBlock').
type LedgerState :: Type -> LedgerStateKind
data family LedgerState blk mk

type TickedLedgerState blk = Ticked (LedgerState blk)

-- | Useful for referring to @TickedL l@ when dealing with 'StateKind' constraints.
type TickedL :: StateKind -> Type -> F2 -> Type
newtype TickedL l blk mk = TickedL {unTickedL :: Ticked (l blk) mk}

type instance HeaderHash (LedgerState blk) = HeaderHash blk

instance StandardHash blk => StandardHash (LedgerState blk)
