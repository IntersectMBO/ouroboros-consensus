{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | This module defines the 'LedgerTables', a portion of the Ledger notion of a
-- /ledger state/ (not to confuse with our
-- 'Ouroboros.Consensus.Ledger.Basics.LedgerState') that together with it,
-- conforms a complete Ledger /ledger state/.
--
-- 'LedgerTables' are parametrized by two types: keys and values. For now, their
-- only current instantiation is to hold the UTxO set, but future features will
-- extend this to hold other parts of the ledger state that now live in memory.
-- However, 'LedgerTables' don't necessarily have to contain maps from keys to
-- values, and the particular instantiation might choose to ignore some of those
-- types (as phantom types). See 'KeysMK' for an example.
--
-- This type is used for two main purposes. Firstly, we use ledger tables to
-- /extract/ data from the /ledger state/ and store it on secondary storage (eg
-- a solid-state hard-drive). Secondly, when we load data from disk onto memory,
-- we use ledger tables to /inject/ data into the /ledger state/. This mechanism
-- allows us to keep most of the data on disk, which is rarely used, reducing
-- the memory usage of the Consensus layer.
--
-- = __Example__
--
-- As an example, consider a LedgerState that contains a Ledger /ledger state/
-- (such as the @NewEpochState@) and a UTxO set:
--
-- @
-- data instance t'Ouroboros.Consensus.Ledger.Basics.LedgerState' (Block era) mk = LedgerState {
--     theLedgerLedgerState :: NewEpochState era
--   , theTables            :: 'LedgerTables' (Block era) mk
-- }
-- @
--
-- The Ledger /ledger state/ contains a UTxO set as well, and with
-- @stowLedgerTables@ and @unstowLedgerTables@ we move those between the Ledger
-- /ledger state/ and the 'LedgerTables', for example:
--
-- @
-- 'unstowLedgerTables' (LedgerState {
--                         theLedgerLedgerState = NewEpochState {
--                             ...
--                           , utxoSet = Map.fromList [(\'a\', 100), (\'b\', 100), ...]
--                         }
--                       , theTables = 'EmptyMK'
--                     })
--  ==
--  LedgerState {
--      theLedgerLedgerState = NewEpochState {
--          ...
--        , utxoSet = Map.empty
--        }
--    , theTables = 'ValuesMK' (Map.fromList [(\'a\', 100), (\'b\', 100), ...])
--    })
-- @
--
-- @
-- 'stowLedgerTables' (LedgerState {
--                       theLedgerLedgerState = NewEpochState {
--                           ...
--                         , utxoSet = Map.empty
--                       }
--                     , theTables = 'ValuesMK' (Map.fromList [(\'a\', 100), (\'b\', 100), ...])
--                   })
--  ==
--  LedgerState {
--      theLedgerLedgerState = NewEpochState {
--          ...
--        , utxoSet = Map.fromList [(\'a\', 100), (\'b\', 100), ...]
--        }
--    , theTables = 'EmptyMK'
--    })
-- @
--
-- Using these functions we can extract the data from the Ledger /ledger state/
-- for us Consensus to manipulate, and we can then inject it back so that we
-- provide the expected data to the ledger. Note that the Ledger rules for
-- applying a block are defined in a way that it only needs the subset of the
-- UTxO set that the block being applied will consume.
--
-- Now using 'Ouroboros.Consensus.Ledger.Tables.Utils.calculateDifference', we
-- can compare two (successive) t'Ouroboros.Consensus.Ledger.Basics.LedgerState's
-- to produce differences:
--
-- @
-- 'Ouroboros.Consensus.Ledger.Tables.Utils.calculateDifference'
--   (LedgerState {
--       ...
--     , theTables = 'ValuesMK' (Map.fromList [(\'a\', 100), (\'b\', 100)])
--     })
--   (LedgerState {
--       ...
--     , theTables = 'ValuesMK' (Map.fromList [(\'a\', 100), (\'c\', 200)])
--     })
-- ==
--  'TrackingMK'
--    (Map.fromList [(\'a\', 100),    (\'c\', 200)])
--    (Map.fromList [(\'b\', Delete), (\'c\', Insert 200)])
-- @
--
-- This operation provided a 'TrackingMK' which is in fact just a 'ValuesMK' and
-- 'DiffMK' put together.
--
-- We can then use those differences to /forward/ a collection of values, so for
-- example (taking the example above):
--
-- @
-- let tables1 = 'ValuesMK' (Map.fromList [(\'a\', 100), (\'b\', 100)])
--     tables2 = 'ValuesMK' (Map.fromList [(\'a\', 100), (\'c\', 200)])
--     diffs = 'Ouroboros.Consensus.Ledger.Tables.Utils.rawForgetTrackingValues'
--           $ 'Ouroboros.Consensus.Ledger.Tables.Utils.rawCalculateDifference' tables1 tables2
-- in
--   'Ouroboros.Consensus.Ledger.Tables.Utils.rawApplyDiffs' tables1 diffs == tables2
-- @
--
-- Note: we usually don't call the @raw*@ methods directly but instead call the
-- corresponding function that operates on
-- t'Ouroboros.Consensus.Ledger.Basics.LedgerState's. See
-- "Ouroboros.Consensus.Ledger.Tables.Utils".
--
-- Also when applying a block that contains some transactions, we can produce
-- 'LedgerTable's of @KeysMK@, by gathering the txins required by the
-- transactions:
--
-- @
-- 'Ouroboros.Consensus.Ledger.Abstract.getBlockKeySets' (Block {..., txs = [Tx { input = [\'a\', \'b\'], outputs = [\'c\', \'d\'] }]})
--  == 'KeysMK' (Set.fromList [\'a\', \'b\'])
-- @
--
-- We shall use those later on to read the txouts from some storage.
--
-- We call those types ending in \"MK\" mapkinds. They model the different types
-- of collections and contained data in the tables. This example already covered
-- most of the standard mapkinds, in particular:
--
--   ['EmptyMK']: A nullary data constructor, an empty table.
--
--   ['ValuesMK']: Contains a @Data.Map@ from txin to txouts.
--
--   ['DiffMK']: Contains a @Data.Map@ from txin to a change on the value.
--
--   ['TrackingMK']: Contains both a 'ValuesMK' and 'DiffMK'.
--
--   ['KeysMK']: Contains a @Data.Set@ of txins.
--
--   ['SeqDiffMK']: A fingertree of 'DiffMK's.
module Ouroboros.Consensus.Ledger.Tables
  ( -- * Core
    module Ouroboros.Consensus.Ledger.Tables.Basics
  , module Ouroboros.Consensus.Ledger.Tables.MapKind

    -- * Utilities
  , module Ouroboros.Consensus.Ledger.Tables.Combinators

    -- * Basic LedgerState classes

    -- ** Stowing ledger tables
  , CanStowLedgerTables (..)

    -- ** Extracting and injecting ledger tables
  , HasLedgerTables (..)
  , HasTickedLedgerTables
  ) where

import Data.Kind (Type)
import Data.SOP.Constraint
import Ouroboros.Consensus.Ledger.LedgerStateType
import Ouroboros.Consensus.Ledger.Tables.Basics
import Ouroboros.Consensus.Ledger.Tables.Combinators
import Ouroboros.Consensus.Ledger.Tables.MapKind

{-------------------------------------------------------------------------------
  Basic LedgerState classes
-------------------------------------------------------------------------------}

-- | Extracting @'LedgerTables'@ from @l mk@ (which will share the same @mk@),
-- or replacing the @'LedgerTables'@ associated to a particular @l@.
type HasLedgerTables :: (Type -> LedgerStateKind) -> Type -> Constraint
class (LedgerTableConstraints blk, LedgerTableConstraintsMK blk DiffMK) => HasLedgerTables l blk where
  -- | Extract the ledger tables from a ledger state
  --
  -- The constraints on @mk@ are necessary because the 'CardanoBlock' instance
  -- uses them.
  projectLedgerTables ::
    (CanMapMK mk, ZeroableMK mk) =>
    l blk mk ->
    LedgerTables blk mk

  -- | Overwrite the tables in the given ledger state.
  --
  -- The contents of the tables should not be /younger/ than the content of the
  -- ledger state. In particular, for a
  -- 'Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock' ledger, the
  -- tables argument should not contain any data from eras that succeed the
  -- current era of the ledger state argument.
  --
  -- The constraints on @mk@ are necessary because the 'CardanoBlock' instance
  -- uses them.
  withLedgerTables ::
    (CanMapMK mk, ZeroableMK mk) =>
    l blk any ->
    LedgerTables blk mk ->
    l blk mk

-- | Convenience class, useful for partially applying the composition of
-- 'HasLedgerTables' and 'Ticked'.
type HasTickedLedgerTables :: StateKind -> Type -> Constraint
class HasLedgerTables (TickedL l) blk => HasTickedLedgerTables l blk

instance HasLedgerTables (TickedL l) blk => HasTickedLedgerTables l blk

-- | LedgerTables are projections of data from a LedgerState and as such they
-- can be injected back into a LedgerState. This is necessary because the Ledger
-- rules are currently unaware of UTxO-HD changes. Thus, by stowing the ledger
-- tables, we are able to provide a Ledger State with a restricted UTxO set that
-- is enough to execute the Ledger rules.
--
-- In particular, HardForkBlock LedgerStates are never given diretly to the
-- ledger but rather unwrapped and then it is the inner ledger state the one we
-- give to the ledger. This means that all the single era blocks must be an
-- instance of this class, but HardForkBlocks might avoid doing so.
type CanStowLedgerTables :: LedgerStateKind -> Constraint
class CanStowLedgerTables l where
  stowLedgerTables :: l ValuesMK -> l EmptyMK
  unstowLedgerTables :: l EmptyMK -> l ValuesMK
