{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Ledger tables: the on-disk-backed portion of a ledger state.
--
-- In the UTxO-HD design, the ledger state of a block @blk@ is split into two
-- parts: the in-memory \"hull\" — the @l blk NoTables@ value — and a
-- (potentially large) collection of key/value entries kept on disk and referred
-- to here as /ledger tables/. The currently-instantiated tables for a ledger
-- state are selected by a phantom 'TableKind' parameter @mk@, so that the same
-- ledger state shape can be specialised to hold the actual entries ('Values'),
-- only a set of keys ('Keys'), a pending differential update ('Diffs'), or no
-- entries at all ('NoTables'/'Stowed'). Operations on a ledger state then
-- become operations on the table associated with that state via the
-- 'HasLedgerTables' class.
--
-- The module collects:
--
-- * The kinds used by the family ('TableKind', 'LedgerStateKind',
--   'StateKind').
-- * The five concrete tables ('Values', 'Keys', 'Diffs', 'NoTables', 'Stowed')
--   and the type families ('TxIn', 'TxOut') that determine their key/value
--   types per block.
-- * Generic operations on tables ('EmptyTable', 'BimapTables') and the
--   ledger-state-side interface that ties a ledger state to its tables
--   ('HasLedgerTables', 'CanStowLedgerTables', 'CanUpgradeLedgerTables').
-- * Convenience helpers that lift table-level operations to whole ledger
--   states (e.g. 'applyDiffs', 'calculateDifference', 'forgetLedgerTables').
-- * Support for ledgers whose tables are statically empty ('TrivialTables').
-- * The CBOR serialisation interface for tables ('SerializeTablesWithHint'
--   and friends).
module Ouroboros.Consensus.Ledger.Tables
  ( -- * Kinds
    TableKind
  , LedgerStateKind
  , StateKind

    -- * Transaction inputs and outputs
  , TxIn
  , TxOut

    -- * Concrete tables
  , Values (..)
  , Keys (..)
  , Diffs (..)
  , NoTables (..)
  , Stowed (..)

    -- ** Operations on tables
  , EmptyTable (..)
  , BimapTables (..)

    -- * Ledger state interface
  , HasLedgerTables (..)
  , CanStowLedgerTables (..)
  , CanUpgradeLedgerTables (..)

    -- * Operations on ledger state tables
  , forgetLedgerTables
  , fmapTables

    -- ** Diffs
  , applyDiffs
  , prependDiffs
  , applyDiffRestricted
  , valuesAsDiffs
  , calculateDifference

    -- ** Values
  , restrictValues
  , unionValues

    -- * Trivial tables
  , TrivialTables (..)

    -- * Serialisation
  , SerializeTablesWithHint (..)
  , canonicalTablesEncoder
  , canonicalTablesDecoder
  , defaultEncodeTablesWithHint
  , defaultDecodeTablesWithHint
  , trivialEncodeTablesWithHint
  , trivialDecodeTablesWithHint
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Data.Kind
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MemPack
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import NoThunks.Class
import Ouroboros.Consensus.Ledger.Tables.Diff (Diff)
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff

{-------------------------------------------------------------------------------
  Kinds
-------------------------------------------------------------------------------}

-- | The kind of a concrete ledger table indexed by a block type.
--
-- The four canonical inhabitants in this module are 'Values', 'Keys', 'Diffs',
-- 'Stowed' and 'NoTables'. Selecting one of these as the @mk@ in @l blk mk@
-- controls which entries (if any) the ledger state carries.
type TableKind = Type -> Type

-- | A @'LedgerStateKind'@ is the kind of any type that takes a single
-- 'TableKind' parameter. The canonical inhabitant is a ledger state applied to
-- a block type, for example @'Ouroboros.Consensus.Ledger.Basics.LedgerState'
-- blk@.
type LedgerStateKind = TableKind -> Type

-- | A @'StateKind'@ is the kind of a ledger state /before/ it receives its
-- block argument. We normally wouldn't want to define code based on a type
-- variable of this kind, but on some classes we need the @blk@ variable to be
-- exposed, and we still need to support multiple @l@ so instead of @Foo
-- (LedgerState blk) blk@ and @Foo (ExtLedgerState blk) blk@ we have @Foo
-- LedgerState blk@ and @Foo ExtLedgerState blk@.
--
-- The four inhabitants in this codebase are:
--
-- - 'LedgerState'
-- - 'ExtLedgerState'
-- - @'Ticked' 'LedgerState'@
-- - @'Ticked' 'ExtLedgerState'@
type StateKind = Type -> LedgerStateKind

{-------------------------------------------------------------------------------
  Transaction inputs and outputs
-------------------------------------------------------------------------------}

-- | The key type of the ledger table associated with @blk@.
--
-- For UTxO-style ledgers this is a transaction input. The single-table
-- assumption baked into this family will need to be relaxed once a ledger
-- carries more than one table.
type TxIn :: Type -> Type
type family TxIn blk

-- | The value type of the ledger table associated with @blk@.
--
-- For UTxO-style ledgers this is a transaction output. The single-table
-- assumption baked into this family will need to be relaxed once a ledger
-- carries more than one table.
type TxOut :: Type -> Type
type family TxOut blk

{-------------------------------------------------------------------------------
  Concrete tables
-------------------------------------------------------------------------------}

-- | A complete snapshot (in the InMemory backing store) or a restricted subset
-- (when returned from a Forker's 'forkerReadTables') of the tables values.
newtype Values blk = Values {getValues :: Map (TxIn blk) (TxOut blk)}

-- | Just the set of keys of a table, with no associated values.
--
-- This is what gets handed to the backend when we want to know which entries to
-- read.
newtype Keys blk = Keys {getKeys :: Set (TxIn blk)}

-- | A differential update to a table: the inserts, deletes and updates that,
-- when applied to a starting set of 'Values', produce the next one.
newtype Diffs blk = Diffs {getDiffs :: Diff (TxIn blk) (TxOut blk)}

-- | The trivial table: no entries at all.
--
-- Used to mark a ledger state whose table contents are empty and also has no
-- values inside the 'NewEpochState'.
data NoTables blk = NoTables deriving (Generic, NoThunks, Eq, Show)

-- | The stowed table.
--
-- Isomorphic to the 'NoTables' variant but conveying the message that there are
-- in fact values inside the 'NewEpochState'. 'stowLedgerTables' and
-- 'unstowLedgerTables' make use of this type.
data Stowed blk = Stowed deriving (Generic, NoThunks, Eq, Show)

deriving newtype instance Ord (TxIn blk) => Semigroup (Keys blk)
deriving newtype instance Ord (TxIn blk) => Monoid (Keys blk)

deriving newtype instance (NoThunks (TxIn blk), NoThunks (TxOut blk)) => NoThunks (Diffs blk)
deriving newtype instance NoThunks (TxIn blk) => NoThunks (Keys blk)
deriving newtype instance (NoThunks (TxIn blk), NoThunks (TxOut blk)) => NoThunks (Values blk)

deriving newtype instance (Eq (TxIn blk), Eq (TxOut blk)) => Eq (Diffs blk)
deriving newtype instance (Show (TxIn blk), Show (TxOut blk)) => Show (Diffs blk)
deriving newtype instance Ord (TxIn blk) => Semigroup (Diffs blk)

deriving newtype instance (Eq (TxIn blk), Eq (TxOut blk)) => Eq (Values blk)
deriving newtype instance (Show (TxIn blk), Show (TxOut blk)) => Show (Values blk)

{-------------------------------------------------------------------------------
  Operations on tables
-------------------------------------------------------------------------------}

-- | Tables that have a canonical empty value.
--
-- This is the table-level analogue of 'Data.Monoid.mempty', restricted to
-- 'TableKind's: it gives us a way to manufacture an empty table for any block
-- without involving the surrounding ledger state.
class EmptyTable mk where
  emptyTable :: mk blk

instance EmptyTable Values where
  emptyTable = Values Map.empty

instance EmptyTable Keys where
  emptyTable = Keys Set.empty

instance EmptyTable NoTables where
  emptyTable = NoTables

instance EmptyTable Diffs where
  emptyTable = Diffs Diff.empty

instance EmptyTable Stowed where
  emptyTable = Stowed

-- | Tables that can be re-indexed from one block type to another.
--
-- We need this primarily for the HardFork combinator, which translates tables
-- back and forth between the per-era block types and the umbrella
-- 'Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock'.
class BimapTables mk where
  bimapLedgerTables ::
    Ord (TxIn y) =>
    (TxIn x -> TxIn y) ->
    (TxOut x -> TxOut y) ->
    mk x ->
    mk y

instance BimapTables Keys where
  bimapLedgerTables f _ (Keys k) = Keys $ Set.map f k

instance BimapTables Values where
  bimapLedgerTables f g (Values k) = Values $ Map.mapKeys f $ Map.map g k

instance BimapTables Diffs where
  bimapLedgerTables f g (Diffs k) = Diffs $ Diff.mapKeys f $ Diff.map g k

instance BimapTables NoTables where
  bimapLedgerTables _ _ NoTables = NoTables

instance BimapTables Stowed where
  bimapLedgerTables _ _ Stowed = Stowed

{-------------------------------------------------------------------------------
  Ledger state interface
-------------------------------------------------------------------------------}

-- | Extracting the ledger tables from @l blk mk@ (which will share the same
-- @mk@), or replacing the tables associated to a particular @l blk@.
type HasLedgerTables :: StateKind -> Type -> Constraint
class HasLedgerTables l blk where
  -- | Extract the ledger tables from a ledger state.
  projectLedgerTables ::
    -- Needed for the HFC bimapping to inject and eject
    BimapTables mk =>
    -- Needed for Byron using trivial tables
    EmptyTable mk =>
    l blk mk ->
    mk blk

  -- | Overwrite the tables in the given ledger state.
  --
  -- The contents of the tables should not be /younger/ than the content of the
  -- ledger state. In particular, for a
  -- 'Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock' ledger, the
  -- tables argument should not contain any data from eras that succeed the
  -- current era of the ledger state argument.
  withLedgerTables ::
    BimapTables mk =>
    l blk any ->
    mk blk ->
    l blk mk

-- | Move the table contents in and out of the in-memory part of the ledger
-- state.
--
-- \"Stowing\" hides the table by folding its values back into the ledger
-- state's in-memory representation, leaving the table-level slot as
-- 'Stowed'. \"Unstowing\" is the inverse. This is used to put the values inside
-- the NewEpochState before calling the Ledger rules.
type CanStowLedgerTables :: LedgerStateKind -> Constraint
class CanStowLedgerTables lblk where
  stowLedgerTables :: lblk Values -> lblk Stowed
  unstowLedgerTables :: lblk Stowed -> lblk Values

-- | Adjust the values of a ledger table when crossing a hard fork boundary.
--
-- A hard fork can change the representation of @'TxOut' blk@. When applying a
-- block that triggers such a transition we may need to translate the
-- pre-existing values to the new format before they can be combined with the
-- post-fork ledger state.
--
-- This is only used for performance reasons in the InMemory backend. It mimicks
-- the translation that already happened in era boundaries before UTxO-HD.
type CanUpgradeLedgerTables :: StateKind -> Type -> Constraint
class CanUpgradeLedgerTables l blk where
  upgradeTables ::
    -- | The original ledger state before the upgrade. This will be the
    -- tip before applying the block.
    l blk mk1 ->
    -- | The ledger state after the upgrade, which might be in a
    -- different era than the one above.
    l blk mk2 ->
    -- | The tables we want to maybe upgrade.
    Values blk ->
    Values blk

{-------------------------------------------------------------------------------
  Operations on ledger state tables
-------------------------------------------------------------------------------}

-- | Replace the tables of a ledger state with 'NoTables'.
forgetLedgerTables :: HasLedgerTables l blk => l blk mk -> l blk NoTables
forgetLedgerTables = (`withLedgerTables` NoTables)

-- | Internal helper: project the tables of a ledger state and feed them to a
-- continuation.
withTablesFrom ::
  (EmptyTable mk, BimapTables mk, HasLedgerTables l blk) =>
  l blk mk -> (mk blk -> t) -> t
withTablesFrom l k =
  k (projectLedgerTables l)

-- | Apply a function to the tables of a ledger state, possibly changing the
-- table kind from @mk1@ to @mk2@.
fmapTables ::
  (EmptyTable mk1, BimapTables mk1, BimapTables mk2, HasLedgerTables l blk) =>
  l blk mk1 -> (mk1 blk -> mk2 blk) -> l blk mk2
fmapTables l k =
  l `withLedgerTables` k (projectLedgerTables l)

{-------------------------------------------------------------------------------
  Operations on ledger state tables: diffs
-------------------------------------------------------------------------------}

-- | Concatenate the diffs from one ledger state in front of those of another.
--
-- The result keeps the in-memory part of @l'@ and carries the combined
-- 'Diffs', i.e. \"first apply the diffs of @l1@, then the diffs of @l2@\".
prependDiffs ::
  (Ord (TxIn blk), HasLedgerTables l blk, HasLedgerTables l' blk) =>
  l blk Diffs -> l' blk Diffs -> l' blk Diffs
prependDiffs l1 l2 = fmapTables l2 $ withTablesFrom l1 (<>)

-- | Apply the diffs carried by @l'@ to the values carried by @l@, returning a
-- ledger state of the shape of @l'@ that holds the resulting 'Values'.
applyDiffs ::
  (Ord (TxIn blk), HasLedgerTables l blk, HasLedgerTables l' blk) =>
  l blk Values -> l' blk Diffs -> l' blk Values
applyDiffs l1 l2 =
  fmapTables l2 $
    withTablesFrom l1 $
      \(Values x) (Diffs y) -> Values (Diff.applyDiff x y)

-- | Apply the diffs carried by a ledger state, but consider the entries whose
-- keys lie in the supplied 'Keys' set in addition to the ones in the 'Values'
-- table (for example for newly created keys that come to existence in the
-- 'Diffs').
applyDiffRestricted ::
  (Ord (TxIn blk), HasLedgerTables l blk) =>
  Values blk -> Keys blk -> l blk Diffs -> l blk Values
applyDiffRestricted (Values t1) (Keys t2) l3 =
  fmapTables l3 $
    \(Diffs t3) -> Values (Diff.applyDiffForKeys t1 t2 t3)

-- | Reinterpret the values carried by a ledger state as a diff: every entry
-- becomes an insertion on top of the empty table.
valuesAsDiffs ::
  (Ord (TxIn blk), Eq (TxOut blk), HasLedgerTables l blk) => l blk Values -> l blk Diffs
valuesAsDiffs l =
  fmapTables l $
    Diffs . Diff.diff Map.empty . getValues

-- | Compute the diff that, when applied to the values of @l1@, yields the
-- values of @l2@. The result is attached to the in-memory part of @l2@.
calculateDifference ::
  (Ord (TxIn blk), Eq (TxOut blk), HasLedgerTables l blk, HasLedgerTables l' blk) =>
  l blk Values -> l' blk Values -> l' blk Diffs
calculateDifference l1 l2 =
  fmapTables l2 $
    withTablesFrom l1 $
      \(Values t1) (Values t2) -> Diffs $ Diff.diff t1 t2

{-------------------------------------------------------------------------------
  Operations on ledger state tables: values
-------------------------------------------------------------------------------}

-- | Restrict a 'Values' table to those entries whose keys appear in the given
-- 'Keys' set.
restrictValues ::
  Ord (TxIn blk) =>
  Values blk ->
  Keys blk ->
  Values blk
restrictValues (Values v) (Keys k) = Values $ v `Map.restrictKeys` k

-- | Left-biased union of two 'Values' tables.
--
-- When both tables contain the same key, the entry from the first argument is
-- retained.
unionValues ::
  Ord (TxIn blk) =>
  Values blk ->
  Values blk ->
  Values blk
unionValues (Values m1) (Values m2) = Values $ Map.union m1 m2

{-------------------------------------------------------------------------------
  Trivial tables
-------------------------------------------------------------------------------}

-- | Ledger states whose tables are statically empty for every table kind.
--
-- Examples are pre-UTxO-HD ledgers (e.g. Byron) that have no on-disk state at
-- all, and test ledgers that intentionally don't track any. Instances may
-- discharge the 'HasLedgerTables', 'CanStowLedgerTables' and
-- 'SerializeTablesWithHint' classes via the @trivial*@ helpers below.
type TrivialTables :: StateKind -> Type -> Constraint
class TrivialTables l blk where
  -- | Coerce a ledger state from one table kind to another. This is sound
  -- precisely because the table is known to be empty regardless of the kind.
  convertTrivialTables :: l blk mk -> l blk mk'

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

-- | CBOR encode/decode a 'Values' table given a ledger state \"hint\".
--
-- The accompanying @l blk NoTables@ argument lets the encoder/decoder consult
-- in-memory ledger context (e.g. the era of a HardFork ledger state) when
-- deciding how to serialise individual entries. Concrete instances will often
-- delegate to 'defaultEncodeTablesWithHint' / 'defaultDecodeTablesWithHint'.
--
-- 'SerializeTablesWithHint' is to whole tables what 'IndexedMemPack' is to
-- single values. The former is used in the InMemory backend (as we write and
-- load whole tables on snapshots), the latter is used in the on-disk backend
-- (as we write and load single values to the database).
class SerializeTablesWithHint l blk where
  encodeTablesWithHint :: l blk NoTables -> Values blk -> CBOR.Encoding
  decodeTablesWithHint :: l blk NoTables -> CBOR.Decoder s (Values blk)

-- | Wrap an 'encodeTablesWithHint' in a one-element CBOR list.
--
-- This is the canonical framing used by the snapshot machinery, so that the
-- decoder side can detect a missing payload as a list-length error rather than
-- misparsing the entries.
canonicalTablesEncoder ::
  SerializeTablesWithHint l blk => l blk NoTables -> Values blk -> CBOR.Encoding
canonicalTablesEncoder st tbs = CBOR.encodeListLen 1 <> encodeTablesWithHint st tbs

-- | Counterpart to 'canonicalTablesEncoder': expect a one-element CBOR list,
-- then defer to 'decodeTablesWithHint'.
canonicalTablesDecoder ::
  SerializeTablesWithHint l blk => l blk NoTables -> CBOR.Decoder s (Values blk)
canonicalTablesDecoder st =
  CBOR.decodeListLenOf 1 >> decodeTablesWithHint st

-- | A reasonable default 'encodeTablesWithHint' that pickles each key/value
-- using its 'MemPack' instance and packages them into a CBOR map.
--
-- The hint argument is unused; this default is appropriate when the
-- serialisation of an entry does not depend on the surrounding ledger state
-- (like single era blocks).
defaultEncodeTablesWithHint ::
  (MemPack (TxIn blk), MemPack (TxOut blk)) =>
  l blk NoTables ->
  Values blk ->
  CBOR.Encoding
defaultEncodeTablesWithHint _ (Values tbs) =
  mconcat
    [ CBOR.encodeMapLen (fromIntegral $ Map.size tbs)
    , Map.foldMapWithKey
        ( \k v ->
            mconcat
              [ CBOR.encodeBytes (packByteString k)
              , CBOR.encodeBytes (packByteString v)
              ]
        )
        tbs
    ]

-- | Counterpart to 'defaultEncodeTablesWithHint': read a CBOR map and unpickle
-- each key/value via 'MemPack'.
defaultDecodeTablesWithHint ::
  (Ord (TxIn blk), MemPack (TxIn blk), MemPack (TxOut blk)) =>
  l blk NoTables ->
  CBOR.Decoder s (Values blk)
defaultDecodeTablesWithHint _ = do
  n <- CBOR.decodeMapLen
  Values <$> go n Map.empty
 where
  go 0 m = pure m
  go n !m = do
    (k, v) <- (,) <$> (unpackMonadFail =<< CBOR.decodeBytes) <*> (unpackMonadFail =<< CBOR.decodeBytes)
    go (n - 1) (Map.insert k v m)

-- | Default 'decodeTablesWithHint' for a 'TrivialTables' ledger: consume a
-- CBOR map and discard its (necessarily empty) contents.
trivialDecodeTablesWithHint ::
  l blk NoTables ->
  CBOR.Decoder s (Values blk)
trivialDecodeTablesWithHint _ = do
  _ <- CBOR.decodeMapLen
  pure emptyTable

-- | Default 'encodeTablesWithHint' for a 'TrivialTables' ledger: emit an empty
-- CBOR map.
trivialEncodeTablesWithHint ::
  l blk NoTables ->
  Values blk ->
  CBOR.Encoding
trivialEncodeTablesWithHint _ _ = CBOR.encodeMapLen 0
