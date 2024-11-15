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
module Ouroboros.Consensus.Ledger.Tables.Utils (
    -- * Projection and injection
    ltprj
  , ltwith
    -- * Utils aliases: tables
  , applyDiffForKeys
  , applyDiffForKeysOnTables
  , applyDiffs
  , applyDiffs'
  , attachAndApplyDiffs
  , attachAndApplyDiffs'
  , attachEmptyDiffs
  , calculateDifference
  , calculateDifference'
  , emptyLedgerTables
  , forgetLedgerTables
  , forgetTrackingDiffs
  , forgetTrackingValues
  , noNewTickingDiffs
  , prependDiffs
  , prependDiffs'
  , prependTrackingDiffs
  , prependTrackingDiffs'
  , reapplyTracking
  , restrictValues
  , restrictValues'
  , valuesAsDiffs
    -- * Testing
  , rawApplyDiffs
  , rawAttachAndApplyDiffs
  , rawAttachEmptyDiffs
  , rawCalculateDifference
  , rawForgetTrackingDiffs
  , rawForgetTrackingValues
  , rawPrependDiffs
  , rawPrependTrackingDiffs
  , rawReapplyTracking
  , rawRestrictValues
  ) where

import qualified Data.Map.Strict as Map
import           Ouroboros.Consensus.Ledger.Tables
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff

{-------------------------------------------------------------------------------
  Projection and injection
-------------------------------------------------------------------------------}

ltwith ::
     ( HasLedgerTables l
     , CanMapMK mk'
     , CanMapKeysMK mk'
     , ZeroableMK mk'
     )
  => l mk
  -> LedgerTables l mk'
  -> l mk'
ltwith = withLedgerTables

ltprj ::
     (HasLedgerTables l, SameUtxoTypes l l', CanMapMK mk, CanMapKeysMK mk, ZeroableMK mk)
  => l mk
  -> LedgerTables l' mk
ltprj = castLedgerTables . projectLedgerTables

{-------------------------------------------------------------------------------
  Utils aliases: tables
-------------------------------------------------------------------------------}

-- | Augment with an empty diff. Can be used to specify that a ledger state tick
-- produces no new UTXO entries.
noNewTickingDiffs :: HasLedgerTables l
                  => l any
                  -> l DiffMK
noNewTickingDiffs l = withLedgerTables l emptyLedgerTables

forgetLedgerTables :: HasLedgerTables l => l mk -> l EmptyMK
forgetLedgerTables l = withLedgerTables l emptyLedgerTables

-- | Empty values for every table
emptyLedgerTables :: (ZeroableMK mk, LedgerTableConstraints l) => LedgerTables l mk
emptyLedgerTables = ltpure emptyMK

--
-- Forget parts of 'TrackingMK'
--

rawForgetTrackingValues :: TrackingMK k v -> DiffMK k v
rawForgetTrackingValues (TrackingMK _vs d) = DiffMK d

forgetTrackingValues :: (HasLedgerTables l, LedgerTableConstraints l) => l TrackingMK -> l DiffMK
forgetTrackingValues l = ltwith l $ ltmap rawForgetTrackingValues (ltprj l)

--
-- Forget diffs
--

rawForgetTrackingDiffs :: TrackingMK k v -> ValuesMK k v
rawForgetTrackingDiffs (TrackingMK vs _ds) = ValuesMK vs

forgetTrackingDiffs :: (LedgerTableConstraints l, HasLedgerTables l) => l TrackingMK -> l ValuesMK
forgetTrackingDiffs l = ltwith l $ ltmap rawForgetTrackingDiffs (ltprj l)

--
-- Prepend diffs
--

rawPrependDiffs ::
     Ord k
  => DiffMK k v -- ^ Earlier differences
  -> DiffMK k v -- ^ Later differences
  -> DiffMK k v
rawPrependDiffs (DiffMK d1) (DiffMK d2) = DiffMK (d1 <> d2)

-- | Prepend diffs from the first ledger state to the diffs from the second
-- ledger state. Returns ledger tables.
prependDiffs' ::
     (SameUtxoTypes l l'', SameUtxoTypes l' l'', HasLedgerTables l, HasLedgerTables l')
  => l DiffMK -> l' DiffMK -> LedgerTables l'' DiffMK
prependDiffs' l1 l2 = ltliftA2 rawPrependDiffs (ltprj l1) (ltprj l2)

-- | Like 'prependDiffs'', but puts the ledger tables inside the second ledger
-- state.
prependDiffs ::
     (SameUtxoTypes l l', HasLedgerTables l, HasLedgerTables l')
  => l DiffMK -> l' DiffMK -> l' DiffMK
prependDiffs l1 l2 = ltwith l2 $ prependDiffs' l1 l2

--
-- Apply diffs
--

rawApplyDiffs ::
     Ord k
  => ValuesMK k v -- ^ Values to which differences are applied
  -> DiffMK   k v -- ^ Differences to apply
  -> ValuesMK k v
rawApplyDiffs (ValuesMK vals) (DiffMK diffs) = ValuesMK (Diff.applyDiff vals diffs)

-- | Apply diffs from the second ledger state to the values of the first ledger
-- state. Returns ledger tables.
applyDiffs' ::
     (SameUtxoTypes l l'', SameUtxoTypes l' l'', HasLedgerTables l, HasLedgerTables l')
  => l ValuesMK -> l' DiffMK -> LedgerTables l'' ValuesMK
applyDiffs' l1 l2 = ltliftA2 rawApplyDiffs (ltprj l1) (ltprj l2)

-- | Like 'applyDiffs'', but puts the ledger tables inside the second ledger
-- state.
applyDiffs ::
     (SameUtxoTypes l l', HasLedgerTables l, HasLedgerTables l')
  => l ValuesMK -> l' DiffMK -> l' ValuesMK
applyDiffs l1 l2 = ltwith l2 $ applyDiffs' l1 l2

rawApplyDiffForKeys ::
     Ord k
  => ValuesMK k v
  -> KeysMK k v
  -> DiffMK k v
  -> ValuesMK k v
rawApplyDiffForKeys (ValuesMK vals) (KeysMK keys) (DiffMK diffs) =
  ValuesMK (Diff.applyDiffForKeys vals keys diffs)

applyDiffForKeys' ::
     (SameUtxoTypes l l'', SameUtxoTypes l l', HasLedgerTables l, HasLedgerTables l')
  => l ValuesMK -> LedgerTables l KeysMK -> l' DiffMK -> LedgerTables l'' ValuesMK
applyDiffForKeys' l1 l2 l3 = ltliftA3 rawApplyDiffForKeys (ltprj l1) (castLedgerTables l2) (ltprj l3)

applyDiffForKeys ::
     (SameUtxoTypes l l', HasLedgerTables l, HasLedgerTables l')
  => l ValuesMK -> LedgerTables l KeysMK -> l' DiffMK -> l' ValuesMK
applyDiffForKeys l1 l2 l3 = ltwith l3 $ applyDiffForKeys' l1 l2 l3

applyDiffForKeys'onTables ::
     (SameUtxoTypes l l'', SameUtxoTypes l l', HasLedgerTables l, HasLedgerTables l')
  => LedgerTables l ValuesMK -> LedgerTables l KeysMK -> l' DiffMK -> LedgerTables l'' ValuesMK
applyDiffForKeys'onTables l1 l2 l3 = ltliftA3 rawApplyDiffForKeys (castLedgerTables l1) (castLedgerTables l2) (ltprj l3)

applyDiffForKeysOnTables ::
     (SameUtxoTypes l l', HasLedgerTables l, HasLedgerTables l')
  => LedgerTables l ValuesMK -> LedgerTables l KeysMK -> l' DiffMK -> l' ValuesMK
applyDiffForKeysOnTables l1 l2 l3 = ltwith l3 $ applyDiffForKeys'onTables l1 l2 l3

--
-- Calculate differences
--

rawCalculateDifference ::
     (Ord k, Eq v)
  => ValuesMK   k v
  -> ValuesMK   k v
  -> TrackingMK k v
rawCalculateDifference (ValuesMK before) (ValuesMK after) = TrackingMK after (Diff.diff before after)

valuesAsDiffs ::
     (LedgerTableConstraints l, HasLedgerTables l)
  => l ValuesMK -> l DiffMK
valuesAsDiffs l = forgetTrackingValues $ ltwith l $ ltliftA (rawCalculateDifference emptyMK) (ltprj l)

-- | Calculate the differences between two ledger states. The first ledger state
-- is considered /before/, the second ledger state is considered /after/.
-- Returns ledger tables.
calculateDifference' ::
     (SameUtxoTypes l l'', SameUtxoTypes l' l'', HasLedgerTables l, HasLedgerTables l')
  => l ValuesMK -> l' ValuesMK -> LedgerTables l'' TrackingMK
calculateDifference' l1 l2 = ltliftA2 rawCalculateDifference (ltprj l1) (ltprj l2)

-- | Like 'calculcateDifference'', but puts the ledger tables inside the second
-- leger state.
calculateDifference ::
     (SameUtxoTypes l l', HasLedgerTables l, HasLedgerTables l')
  => l ValuesMK -> l' ValuesMK -> l' TrackingMK
calculateDifference l1 l2 = ltwith l2 $ calculateDifference' l1 l2

--
-- Attaching and/or applying diffs
--

rawAttachAndApplyDiffs ::
     Ord k
  => ValuesMK   k v
  -> DiffMK     k v
  -> TrackingMK k v
rawAttachAndApplyDiffs (ValuesMK v) (DiffMK d) = TrackingMK (Diff.applyDiff v d) d

-- | Apply the differences from the first ledger state to the values of the
-- second ledger state, and returns the resulting values together with the
-- applied diff.
attachAndApplyDiffs' ::
     (SameUtxoTypes l l'', SameUtxoTypes l' l'', HasLedgerTables l, HasLedgerTables l')
  => l' ValuesMK -> l DiffMK -> LedgerTables l'' TrackingMK
attachAndApplyDiffs' l1 l2 = ltliftA2 rawAttachAndApplyDiffs (ltprj l1) (ltprj l2)

-- | Like 'attachAndApplyDiffs'', but puts the ledger tables inside the second
-- leger state.
attachAndApplyDiffs ::
     (SameUtxoTypes l l', HasLedgerTables l, HasLedgerTables l')
  => l ValuesMK -> l' DiffMK -> l' TrackingMK
attachAndApplyDiffs l1 l2 = ltwith l2 $ attachAndApplyDiffs' l1 l2

rawAttachEmptyDiffs :: Ord k => ValuesMK k v -> TrackingMK k v
rawAttachEmptyDiffs (ValuesMK v) = TrackingMK v mempty

-- | Make a 'TrackingMK' with empty diffs.
attachEmptyDiffs :: HasLedgerTables l => l ValuesMK -> l TrackingMK
attachEmptyDiffs l1 = ltwith l1 $ ltmap rawAttachEmptyDiffs (ltprj l1)

--
-- Prepend tracking diffs
--

-- | Prepend the former tracking diffs to the latter tracking diffs. Keep the
-- second tracking values.
--
-- PRECONDITION: Given that the first argument is @TrackingMK v1 d1@, and the
-- second argument is @TrackingMK v2 d2@, it should be the case that @applyDiff
-- v1 d2 == v2@.
rawPrependTrackingDiffs ::
      Ord k
   => TrackingMK k v
   -> TrackingMK k v
   -> TrackingMK k v
rawPrependTrackingDiffs (TrackingMK _ d1) (TrackingMK v d2) =
  TrackingMK v (d1 <> d2)

-- | Prepend tracking diffs from the first ledger state to the tracking diffs
-- from the second ledger state. Keep the tracking values of the second ledger
-- state.
--
-- PRECONDITION:  See 'rawPrependTrackingDiffs'.
prependTrackingDiffs' ::
     (SameUtxoTypes l l'', SameUtxoTypes l' l'', HasLedgerTables l, HasLedgerTables l')
  => l TrackingMK -> l' TrackingMK -> LedgerTables l'' TrackingMK
prependTrackingDiffs' l1 l2 = ltliftA2 rawPrependTrackingDiffs (ltprj l1) (ltprj l2)

-- | Like 'prependTrackingDiffs'', but puts the ledger tables inside the second
-- leger state.
prependTrackingDiffs ::
     (SameUtxoTypes l l', HasLedgerTables l, HasLedgerTables l')
  => l TrackingMK -> l' TrackingMK -> l' TrackingMK
prependTrackingDiffs l1 l2 = ltwith l2 $ prependTrackingDiffs' l1 l2

-- Reapply tracking diffs

rawReapplyTracking ::
     Ord k
  => TrackingMK k v
  -> ValuesMK   k v
  -> TrackingMK k v
rawReapplyTracking (TrackingMK _v d) (ValuesMK v) = TrackingMK (Diff.applyDiff v d) d

-- | Replace the tables in the first parameter with the tables of the second
-- parameter after applying the differences in the first parameter to them
reapplyTracking :: LedgerTableConstraints l => LedgerTables l TrackingMK -> LedgerTables l ValuesMK -> LedgerTables l TrackingMK
reapplyTracking = ltliftA2 rawReapplyTracking

-- Restrict values

rawRestrictValues ::
     Ord k
  => ValuesMK k v
  -> KeysMK k v
  -> ValuesMK k v
rawRestrictValues (ValuesMK v) (KeysMK k) = ValuesMK $ v `Map.restrictKeys` k

restrictValues' ::
     (SameUtxoTypes l l'', SameUtxoTypes l' l'', HasLedgerTables l, HasLedgerTables l')
  => l ValuesMK -> l' KeysMK -> LedgerTables l'' ValuesMK
restrictValues' l1 l2 = ltliftA2 rawRestrictValues (ltprj l1) (ltprj l2)

restrictValues ::
     (SameUtxoTypes l l', HasLedgerTables l, HasLedgerTables l')
  => l ValuesMK -> l' KeysMK -> l ValuesMK
restrictValues l1 l2 = ltwith l1 $ ltliftA2 rawRestrictValues (ltprj l1) (ltprj l2)
