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
  ( -- * Projection and injection
    ltprj
  , ltwith

    -- * Basic operations
  , emptyLedgerTables
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
  , rawPrependDiffs

    -- * Union values
  , unionValues

    -- * Exposed for @cardano-api@
  , applyDiffsMK
  , restrictValuesMK

    -- * Testing
  , applyDiffs'
  , rawCalculateDifference -- used in test
  ) where

import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.Ledger.Tables
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff

{-------------------------------------------------------------------------------
  Projection and injection
-------------------------------------------------------------------------------}

ltwith ::
  ( HasLedgerTables l blk
  , CanMapMK mk'
  , CanMapKeysMK mk'
  , ZeroableMK mk'
  ) =>
  l blk mk ->
  LedgerTables blk mk' ->
  l blk mk'
ltwith = withLedgerTables

ltprj ::
  (HasLedgerTables l blk, CanMapMK mk, CanMapKeysMK mk, ZeroableMK mk) =>
  l blk mk ->
  LedgerTables blk mk
ltprj = projectLedgerTables

{-------------------------------------------------------------------------------
  Utils aliases: tables
-------------------------------------------------------------------------------}

-- | Replace tables with an empty diff. Can be used to specify that a ledger
-- state tick produces no new UTXO entries.
noNewTickingDiffs ::
  HasLedgerTables l blk =>
  l blk any ->
  l blk DiffMK
noNewTickingDiffs l = withLedgerTables l emptyLedgerTables

-- | Remove the ledger tables
forgetLedgerTables :: HasLedgerTables l blk => l blk mk -> l blk EmptyMK
forgetLedgerTables l = withLedgerTables l emptyLedgerTables

-- | Empty values for every table
emptyLedgerTables :: (ZeroableMK mk, LedgerTableConstraints blk) => LedgerTables blk mk
emptyLedgerTables = ltpure emptyMK

--
-- Prepend diffs
--

rawPrependDiffs ::
  Ord k =>
  -- | Earlier differences
  DiffMK k v ->
  -- | Later differences
  DiffMK k v ->
  DiffMK k v
rawPrependDiffs (DiffMK d1) (DiffMK d2) = DiffMK (d1 <> d2)

-- | Prepend diffs from the first ledger state to the diffs from the second
-- ledger state. Returns ledger tables.
prependDiffs' ::
  ( HasLedgerTables l blk
  , HasLedgerTables l' blk
  ) =>
  l blk DiffMK -> l' blk DiffMK -> LedgerTables blk DiffMK
prependDiffs' l1 l2 = ltliftA2 rawPrependDiffs (ltprj l1) (ltprj l2)

-- | Prepend the diffs from @l1@ to @l2@. Returns @l2@.
prependDiffs ::
  (HasLedgerTables l blk, HasLedgerTables l' blk) =>
  l blk DiffMK -> l' blk DiffMK -> l' blk DiffMK
prependDiffs l1 l2 = ltwith l2 $ prependDiffs' l1 l2

--
-- Apply diffs
--

applyDiffsMK ::
  Ord k =>
  -- | Values to which differences are applied
  ValuesMK k v ->
  -- | Differences to apply
  DiffMK k v ->
  ValuesMK k v
applyDiffsMK (ValuesMK vals) (DiffMK diffs) = ValuesMK (Diff.applyDiff vals diffs)

-- | Apply diffs from the second ledger state to the values of the first ledger
-- state. Returns ledger tables.
applyDiffs' ::
  ( HasLedgerTables l blk
  , HasLedgerTables l' blk
  ) =>
  l blk ValuesMK -> l' blk DiffMK -> LedgerTables blk ValuesMK
applyDiffs' l1 l2 = ltliftA2 applyDiffsMK (ltprj l1) (ltprj l2)

-- | Apply diffs from @l2@ on values from @l1@. Returns @l2@.
applyDiffs ::
  (HasLedgerTables l blk, HasLedgerTables l' blk) =>
  l blk ValuesMK -> l' blk DiffMK -> l' blk ValuesMK
applyDiffs l1 l2 = ltwith l2 $ applyDiffs' l1 l2

rawApplyDiffForKeys ::
  Ord k =>
  ValuesMK k v ->
  KeysMK k v ->
  DiffMK k v ->
  ValuesMK k v
rawApplyDiffForKeys (ValuesMK vals) (KeysMK keys) (DiffMK diffs) =
  ValuesMK (Diff.applyDiffForKeys vals keys diffs)

-- | Apply diffs in @l3@ for keys in @l2@ and @l1@ on values from @l1@. Returns @l3@.
applyDiffForKeys ::
  (HasLedgerTables l blk, HasLedgerTables l' blk) =>
  l blk ValuesMK -> LedgerTables blk KeysMK -> l' blk DiffMK -> l' blk ValuesMK
applyDiffForKeys l1 l2 l3 = ltwith l3 $ applyDiffForKeys' (ltprj l1) l2 l3

applyDiffForKeys' ::
  HasLedgerTables l blk =>
  LedgerTables blk ValuesMK -> LedgerTables blk KeysMK -> l blk DiffMK -> LedgerTables blk ValuesMK
applyDiffForKeys' l1 l2 l3 = ltliftA3 rawApplyDiffForKeys l1 l2 (ltprj l3)

-- | Apply diffs in @l3@ for keys in @l2@ and @l1@ on values from @l1@. Returns @l3@.
applyDiffForKeysOnTables ::
  HasLedgerTables l blk =>
  LedgerTables blk ValuesMK -> LedgerTables blk KeysMK -> l blk DiffMK -> l blk ValuesMK
applyDiffForKeysOnTables l1 l2 l3 = ltwith l3 $ applyDiffForKeys' l1 l2 l3

--
-- Calculate differences
--

rawCalculateDifference ::
  (Ord k, Eq v) =>
  ValuesMK k v ->
  ValuesMK k v ->
  DiffMK k v
rawCalculateDifference (ValuesMK before) (ValuesMK after) = DiffMK (Diff.diff before after)

-- | Promote values to diffs, for cases in which all existing values must be
-- considered diffs. In particular this is used when populating the ledger
-- tables for the first time.
valuesAsDiffs :: HasLedgerTables l blk => l blk ValuesMK -> l blk DiffMK
valuesAsDiffs l = ltwith l $ ltliftA (rawCalculateDifference emptyMK) (ltprj l)

-- | Calculate the differences between two ledger states. The first ledger state
-- is considered /before/, the second ledger state is considered /after/.
-- Returns the second ledger state.
calculateDifference ::
  (HasLedgerTables l blk, HasLedgerTables l' blk) =>
  l blk ValuesMK -> l' blk ValuesMK -> l' blk DiffMK
calculateDifference l1 l2 = ltwith l2 $ ltliftA2 rawCalculateDifference (ltprj l1) (ltprj l2)

-- Restrict values

restrictValuesMK ::
  Ord k =>
  ValuesMK k v ->
  KeysMK k v ->
  ValuesMK k v
restrictValuesMK (ValuesMK v) (KeysMK k) = ValuesMK $ v `Map.restrictKeys` k

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
