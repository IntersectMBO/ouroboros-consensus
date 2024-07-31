{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | A collection of useful combinators to shorten the code in other places.
--
-- TODO: #4394 provide better ergonomics. This whole module provides ways to
-- combine tables of two ledger states to produce another one. It is written
-- very much ad-hoc and we should probably think of some way to make this more
-- ergonomic. In particular for functions that take two ledger states, it is
-- unclear if it will keep the in-memory part of the first or the second one.
module Ouroboros.Consensus.Ledger.Tables.Utils (
    -- * Projection and injection
    ltprj
  , over
    -- * Utils aliases: tables
  , applyDiffForKeys
  , applyDiffForKeysOnTables
  , applyDiffs
  , applyDiffs'
  , attachAndApplyDiffs
  , attachAndApplyDiffs'
  , attachEmptyDiffs
  , calculateAdditions
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
    -- * Testing
  , rawApplyDiffForKeys
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
import qualified Ouroboros.Consensus.Ledger.Tables.UtxoDiff as Diff

{-------------------------------------------------------------------------------
  Projection and injection
-------------------------------------------------------------------------------}

over ::
     ( HasLedgerTables l
     , CanMapMK mk'
     , CanMapKeysMK mk'
     , ZeroableMK mk'
     )
  => l mk
  -> LedgerTables l mk'
  -> l mk'
over = withLedgerTables

ltprj ::
     (HasLedgerTables l, Castable l l', CanMapMK mk, CanMapKeysMK mk, ZeroableMK mk)
  => l mk
  -> LedgerTables l' mk
ltprj = castLedgerTables . projectLedgerTables

{-------------------------------------------------------------------------------
  Utils aliases: tables
-------------------------------------------------------------------------------}

-- | When applying a block that is not on an era transition, ticking won't
-- generate new values, so this function can be used to wrap the call to the
-- ledger rules that perform the tick.
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
forgetTrackingValues l = over l $ ltmap rawForgetTrackingValues (ltprj l)

--
-- Forget diffs
--

rawForgetTrackingDiffs :: TrackingMK k v -> ValuesMK k v
rawForgetTrackingDiffs (TrackingMK vs _ds) = ValuesMK vs

forgetTrackingDiffs :: (LedgerTableConstraints l, HasLedgerTables l) => l TrackingMK -> l ValuesMK
forgetTrackingDiffs l = over l $ ltmap rawForgetTrackingDiffs (ltprj l)

--
-- Prepend diffs
--

rawPrependDiffs ::
     Ord k
  => DiffMK k v -- ^ Earlier differences
  -> DiffMK k v -- ^ Later differences
  -> Maybe (DiffMK k v)
rawPrependDiffs (DiffMK d1) (DiffMK d2) =
  case Diff.AUtxoDiff d1 <> Diff.AUtxoDiff d2 of
    Diff.NotAUtxoDiff -> Nothing
    Diff.AUtxoDiff d  -> Just $ DiffMK d

-- | Prepend diffs from the first ledger state to the diffs from the second
-- ledger state. Returns ledger tables.
prependDiffs' ::
     (Castable l l'', Castable l' l'', HasLedgerTables l, HasLedgerTables l')
  => l DiffMK -> l' DiffMK -> Maybe (LedgerTables l'' DiffMK)
prependDiffs' l1 l2 = lttraverse2 rawPrependDiffs (ltprj l1) (ltprj l2)

-- | Like 'prependDiffs'', but puts the ledger tables inside the second ledger
-- state.
prependDiffs ::
     (Castable l l', HasLedgerTables l, HasLedgerTables l')
  => l DiffMK -> l' DiffMK -> Maybe (l' DiffMK)
prependDiffs l1 l2 = over l2 <$> prependDiffs' l1 l2

--
-- Apply diffs
--

rawApplyDiffs ::
     Ord k
  => ValuesMK k v -- ^ Values to which differences are applied
  -> DiffMK   k v -- ^ Differences to apply
  -> ValuesMK k v
rawApplyDiffs (ValuesMK vals) (DiffMK diffs) = ValuesMK (Diff.applyUtxoDiff vals diffs)

-- | Apply diffs from the second ledger state to the values of the first ledger
-- state. Returns ledger tables.
applyDiffs' ::
     (Castable l l'', Castable l' l'', HasLedgerTables l, HasLedgerTables l')
  => l ValuesMK -> l' DiffMK -> LedgerTables l'' ValuesMK
applyDiffs' l1 l2 = ltliftA2 rawApplyDiffs (ltprj l1) (ltprj l2)

-- | Like 'applyDiffs'', but puts the ledger tables inside the second ledger
-- state.
applyDiffs ::
     (Castable l l', HasLedgerTables l, HasLedgerTables l')
  => l ValuesMK -> l' DiffMK -> l' ValuesMK
applyDiffs l1 l2 = over l2 $ applyDiffs' l1 l2

rawApplyDiffForKeys ::
     Ord k
  => ValuesMK k v
  -> KeysMK k v
  -> DiffMK k v
  -> ValuesMK k v
rawApplyDiffForKeys (ValuesMK vals) (KeysMK keys) (DiffMK diffs) =
  ValuesMK (Diff.applyUtxoDiffForKeys vals keys diffs)

applyDiffForKeys' ::
     (Castable l l'', Castable l l', HasLedgerTables l, HasLedgerTables l')
  => l ValuesMK -> LedgerTables l KeysMK -> l' DiffMK -> LedgerTables l'' ValuesMK
applyDiffForKeys' l1 l2 l3 = ltliftA3 rawApplyDiffForKeys (ltprj l1) (castLedgerTables l2) (ltprj l3)

applyDiffForKeys ::
     (Castable l l', HasLedgerTables l, HasLedgerTables l')
  => l ValuesMK -> LedgerTables l KeysMK -> l' DiffMK -> l' ValuesMK
applyDiffForKeys l1 l2 l3 = over l3 $ applyDiffForKeys' l1 l2 l3

applyDiffForKeys'onTables ::
     (Castable l l'', Castable l l', HasLedgerTables l, HasLedgerTables l')
  => LedgerTables l ValuesMK -> LedgerTables l KeysMK -> l' DiffMK -> LedgerTables l'' ValuesMK
applyDiffForKeys'onTables l1 l2 l3 = ltliftA3 rawApplyDiffForKeys (castLedgerTables l1) (castLedgerTables l2) (ltprj l3)

applyDiffForKeysOnTables ::
     (Castable l l', HasLedgerTables l, HasLedgerTables l')
  => LedgerTables l ValuesMK -> LedgerTables l KeysMK -> l' DiffMK -> l' ValuesMK
applyDiffForKeysOnTables l1 l2 l3 = over l3 $ applyDiffForKeys'onTables l1 l2 l3

--
-- Calculate differences
--

rawCalculateDifference ::
     Ord k
  => ValuesMK   k v
  -> ValuesMK   k v
  -> TrackingMK k v
rawCalculateDifference (ValuesMK before) (ValuesMK after) = TrackingMK after (Diff.diff before after)

calculateAdditions ::
     (LedgerTableConstraints l, HasLedgerTables l)
  => l ValuesMK -> l TrackingMK
calculateAdditions l = over l $ ltliftA (rawCalculateDifference emptyMK) (ltprj l)

-- | Calculate the differences between two ledger states. The first ledger state
-- is considered /before/, the second ledger state is considered /after/.
-- Returns ledger tables.
calculateDifference' ::
     (Castable l l'', Castable l' l'', HasLedgerTables l, HasLedgerTables l')
  => l ValuesMK -> l' ValuesMK -> LedgerTables l'' TrackingMK
calculateDifference' l1 l2 = ltliftA2 rawCalculateDifference (ltprj l1) (ltprj l2)

-- | Like 'calculcateDifference'', but puts the ledger tables inside the second
-- leger state.
calculateDifference ::
     (Castable l l', HasLedgerTables l, HasLedgerTables l')
  => l ValuesMK -> l' ValuesMK -> l' TrackingMK
calculateDifference l1 l2 = over l2 $ calculateDifference' l1 l2

--
-- Attaching and/or applying diffs
--

rawAttachAndApplyDiffs ::
     Ord k
  => DiffMK     k v
  -> ValuesMK   k v
  -> TrackingMK k v
rawAttachAndApplyDiffs (DiffMK d) (ValuesMK v) = TrackingMK (Diff.applyUtxoDiff v d) d

-- | Apply the differences from the first ledger state to the values of the
-- second ledger state, and returns the resulting values together with the
-- applied diff.
attachAndApplyDiffs' ::
     (Castable l l'', Castable l' l'', HasLedgerTables l, HasLedgerTables l')
  => l DiffMK -> l' ValuesMK -> LedgerTables l'' TrackingMK
attachAndApplyDiffs' l1 l2 = ltliftA2 rawAttachAndApplyDiffs (ltprj l1) (ltprj l2)

-- | Like 'attachAndApplyDiffs'', but puts the ledger tables inside the first
-- leger state.
attachAndApplyDiffs ::
     (Castable l l', HasLedgerTables l, HasLedgerTables l')
  => l DiffMK -> l' ValuesMK -> l TrackingMK
attachAndApplyDiffs l1 l2 = over l1 $ attachAndApplyDiffs' l1 l2

rawAttachEmptyDiffs :: Ord k => ValuesMK k v -> TrackingMK k v
rawAttachEmptyDiffs (ValuesMK v) = TrackingMK v Diff.empty

-- | Make a 'TrackingMK' with empty diffs.
attachEmptyDiffs :: HasLedgerTables l => l ValuesMK -> l TrackingMK
attachEmptyDiffs l1 = over l1 $ ltmap rawAttachEmptyDiffs (ltprj l1)

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
   -> Maybe (TrackingMK k v)
rawPrependTrackingDiffs (TrackingMK _ d1) (TrackingMK v d2) =
  case Diff.AUtxoDiff d1 <> Diff.AUtxoDiff d2 of
    Diff.NotAUtxoDiff -> Nothing
    Diff.AUtxoDiff d  -> Just $ TrackingMK v d


-- | Prepend tracking diffs from the first ledger state to the tracking diffs
-- from the second ledger state. Keep the tracking values of the second ledger
-- state.
--
-- PRECONDITION:  See 'rawPrependTrackingDiffs'.
prependTrackingDiffs' ::
     (Castable l l'', Castable l' l'', HasLedgerTables l, HasLedgerTables l')
  => l TrackingMK -> l' TrackingMK -> Maybe (LedgerTables l'' TrackingMK)
prependTrackingDiffs' l1 l2 = lttraverse2 rawPrependTrackingDiffs (ltprj l1) (ltprj l2)

-- | Like 'prependTrackingDiffs'', but puts the ledger tables inside the second
-- leger state.
prependTrackingDiffs ::
     (Castable l l', HasLedgerTables l, HasLedgerTables l')
  => l TrackingMK -> l' TrackingMK -> Maybe (l' TrackingMK)
prependTrackingDiffs l1 l2 = over l2 <$> prependTrackingDiffs' l1 l2

-- Reapply tracking diffs

rawReapplyTracking ::
     Ord k
  => TrackingMK k v
  -> ValuesMK   k v
  -> TrackingMK k v
rawReapplyTracking (TrackingMK _v d) (ValuesMK v) = TrackingMK (Diff.applyUtxoDiff v d) d

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
     (Castable l l'', Castable l' l'', HasLedgerTables l, HasLedgerTables l')
  => l ValuesMK -> l' KeysMK -> LedgerTables l'' ValuesMK
restrictValues' l1 l2 = ltliftA2 rawRestrictValues (ltprj l1) (ltprj l2)

restrictValues ::
     (Castable l l', HasLedgerTables l, HasLedgerTables l')
  => l ValuesMK -> l' KeysMK -> l ValuesMK
restrictValues l1 l2 = over l1 $ ltliftA2 rawRestrictValues (ltprj l1) (ltprj l2)
