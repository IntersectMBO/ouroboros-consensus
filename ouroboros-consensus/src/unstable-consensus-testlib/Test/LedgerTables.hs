{-# LANGUAGE FlexibleContexts #-}

module Test.LedgerTables
  ( prop_hasledgertables_laws
  , prop_stowable_laws
  ) where

import Data.Function (on)
import Ouroboros.Consensus.Ledger.Basics
import Test.QuickCheck

-- | We compare the Ledger Tables of the result because the comparison with the
-- rest of the LedgerState takes considerably more time to run.
(==?) ::
  ( CanMapMK mk
  , CanMapKeysMK mk
  , ZeroableMK mk
  , EqMK mk
  , ShowMK mk
  , HasLedgerTables (LedgerState blk)
  ) =>
  LedgerState blk mk ->
  LedgerState blk mk ->
  Property
(==?) = (===) `on` projectLedgerTables

infix 4 ==?

-- | The StowableLedgerTables instances should follow these two laws:
--
-- > stow . unstow == id
--
-- > unstow . stow == id
prop_stowable_laws ::
  ( HasLedgerTables (LedgerState blk)
  , CanStowLedgerTables (LedgerState blk)
  ) =>
  LedgerState blk EmptyMK ->
  LedgerState blk ValuesMK ->
  Property
prop_stowable_laws = \ls ls' ->
  stowLedgerTables (unstowLedgerTables ls) ==? ls
    .&&. unstowLedgerTables (stowLedgerTables ls') ==? ls'

-- | The HasLedgerTables instances should follow these two laws:
--
-- > with . project == id
--
-- > project . with == id
prop_hasledgertables_laws ::
  HasLedgerTables (LedgerState blk) =>
  LedgerState blk EmptyMK ->
  LedgerTables (LedgerState blk) ValuesMK ->
  Property
prop_hasledgertables_laws = \ls tbs ->
  (ls `withLedgerTables` (projectLedgerTables ls)) ==? ls
    .&&. projectLedgerTables (ls `withLedgerTables` tbs) === tbs
