{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | A collection of useful combinators to shorten the code in other places.
--
-- This whole module provides ways to combine tables of two ledger states to
-- produce another one. It is written very much ad-hoc and we should probably
-- think of some way to make this more ergonomic. In particular for functions
-- that take two ledger states, it is unclear if it will keep the in-memory part
-- of the first or the second one.
module Ouroboros.Consensus.Ledger.Tables.Utils
  ( -- * Basic operations
    emptyValues
  , emptyDiff
  , emptyTables
  , emptyKeys
  , forgetLedgerTables

    -- * Operations on 'DiffMK'

    -- ** Apply diffs
  , applyDiffForKeys
  , applyDiffForKeysOnTables
  , applyDiffs

    -- ** Create diffs
  , calculateDifference
  , noNewTickingDiffs
  , valuesAsDiffs

    -- ** Combining diffs
  , prependDiffs
  , prependDiffs'

    -- * Union values
  , unionValues

    -- * Exposed for @cardano-api@
  , restrictValues
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ouroboros.Consensus.Ledger.Tables
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff

{-------------------------------------------------------------------------------
  Utils aliases: tables
-------------------------------------------------------------------------------}

-- | Replace tables with an empty diff. Can be used to specify that a ledger
-- state tick produces no new UTXO entries.
noNewTickingDiffs ::
  HasLedgerTables l blk =>
  l blk any ->
  l blk DiffMK
noNewTickingDiffs l = withLedgerTables l emptyDiff

-- | Remove the ledger tables
forgetLedgerTables :: HasLedgerTables l blk => l blk mk -> l blk EmptyMK
forgetLedgerTables l = withLedgerTables l emptyTables

-- | Empty values for every table
emptyValues :: ValuesMK a b
emptyValues = ValuesMK Map.empty

emptyKeys :: KeysMK a b
emptyKeys = KeysMK Set.empty

emptyDiff :: DiffMK a b
emptyDiff = DiffMK (Diff.Diff Map.empty)

emptyTables :: EmptyMK a b
emptyTables = EmptyMK

--
-- Prepend diffs
--

-- | Prepend the diffs from @l1@ to @l2@. Returns @l2@.
prependDiffs ::
  (HasLedgerTables l blk, HasLedgerTables l' blk, Ord (TxIn blk)) =>
  l blk DiffMK -> l' blk DiffMK -> l' blk DiffMK
prependDiffs l1 l2 = l2 `withLedgerTables` t
 where
  DiffMK d1 = projectLedgerTables l1
  DiffMK d2 = projectLedgerTables l2
  t = DiffMK (d1 <> d2)

-- | Prepend the diffs from @l1@ to @l2@. Returns @l2@.
prependDiffs' ::
  (HasLedgerTables l blk, Ord (TxIn blk)) =>
  Diffs blk -> l blk DiffMK -> l blk DiffMK
prependDiffs' (DiffMK d1) l2 = l2 `withLedgerTables` t
 where
  DiffMK d2 = projectLedgerTables l2
  t = DiffMK (d1 <> d2)

--
-- Apply diffs
--

-- | Apply diffs from @l2@ on values from @l1@. Returns @l2@.
applyDiffs ::
  (HasLedgerTables l blk, HasLedgerTables l' blk, Ord (TxIn blk)) =>
  l blk ValuesMK -> l' blk DiffMK -> l' blk ValuesMK
applyDiffs l1 l2 = l2 `withLedgerTables` t
 where
  ValuesMK t1 = projectLedgerTables l1
  DiffMK t2 = projectLedgerTables l2
  t = ValuesMK (Diff.applyDiff t1 t2)

-- | Apply diffs in @l3@ for keys in @l2@ and @l1@ on values from @l1@. Returns @l3@.
applyDiffForKeys ::
  (HasLedgerTables l blk, HasLedgerTables l' blk, Ord (TxIn blk)) =>
  l blk ValuesMK -> Keys blk -> l' blk DiffMK -> l' blk ValuesMK
applyDiffForKeys l1 (KeysMK t2) l3 = l3 `withLedgerTables` t
 where
  ValuesMK t1 = projectLedgerTables l1
  DiffMK t3 = projectLedgerTables l3
  t = ValuesMK (Diff.applyDiffForKeys t1 t2 t3)

-- | Apply diffs in @l3@ for keys in @l2@ and @l1@ on values from @l1@. Returns @l3@.
applyDiffForKeysOnTables ::
  (HasLedgerTables l blk, Ord (TxIn blk)) =>
  Values blk -> Keys blk -> l blk DiffMK -> l blk ValuesMK
applyDiffForKeysOnTables (ValuesMK t1) (KeysMK t2) l3 = l3 `withLedgerTables` t
 where
  DiffMK t3 = projectLedgerTables l3
  t = ValuesMK (Diff.applyDiffForKeys t1 t2 t3)

--
-- Calculate differences
--

-- | Promote values to diffs, for cases in which all existing values must be
-- considered diffs. In particular this is used when populating the ledger
-- tables for the first time.
valuesAsDiffs ::
  (HasLedgerTables l blk, Eq (TxOut blk), Ord (TxIn blk)) => l blk ValuesMK -> l blk DiffMK
valuesAsDiffs l = l `withLedgerTables` t
 where
  ValuesMK t1 = projectLedgerTables l
  t = DiffMK (Diff.diff Map.empty t1)

-- | Calculate the differences between two ledger states. The first ledger state
-- is considered /before/, the second ledger state is considered /after/.
-- Returns the second ledger state.
calculateDifference ::
  (HasLedgerTables l blk, HasLedgerTables l' blk, Ord (TxIn blk), Eq (TxOut blk)) =>
  l blk ValuesMK -> l' blk ValuesMK -> l' blk DiffMK
calculateDifference l1 l2 = l2 `withLedgerTables` t
 where
  ValuesMK t1 = projectLedgerTables l1
  ValuesMK t2 = projectLedgerTables l2
  t = DiffMK (Diff.diff t1 t2)

-- Restrict values

restrictValues ::
  Ord k =>
  ValuesMK k v ->
  KeysMK k v ->
  ValuesMK k v
restrictValues (ValuesMK v) (KeysMK k) = ValuesMK $ v `Map.restrictKeys` k

---

-- | For this first UTxO-HD iteration, there can't be two keys with
-- different values on the tables, thus there will never be
-- conflicting collisions.
unionValues ::
  Ord k =>
  ValuesMK k v ->
  ValuesMK k v ->
  ValuesMK k v
unionValues (ValuesMK m1) (ValuesMK m2) = ValuesMK $ Map.union m1 m2
