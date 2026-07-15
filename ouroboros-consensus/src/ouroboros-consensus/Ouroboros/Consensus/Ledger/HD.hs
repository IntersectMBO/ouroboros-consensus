{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Ledger.HD
  ( -- * API
    BlockSupportsLedgerHD (..)
  , SingleEraBlockSupportsLedgerHD (..)

    -- * Trivial blocks
  , UnitTables (..)

    -- * Diffs
  , TickDiff (..)
  , BlockDiff (..)
  , TickAndBlockDiff (..)
  , TxsDiff (..)
  ) where

import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)
import Codec.Serialise (Serialise)
import Data.Array.Byte (ByteArray)
import Data.Kind (Constraint, Type)
import Data.MemPack (MemPack)
import Data.MemPack.Buffer (Buffer)
import Data.MemPack.Error (SomeError)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Ledger.Basics
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff

-- | A type isomorphic to unit to more precisely convey that the block has no
-- | LedgerHD tables.
data UnitTables = UnitTables
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NoThunks, Serialise)

instance Semigroup UnitTables where
  UnitTables <> UnitTables = UnitTables

-- | The changes a tick produces to the tables.
newtype TickDiff blk = TickDiff {unTickDiff :: Diff blk}

-- | The changes a block (re)application produces to the tables.
newtype BlockDiff blk = BlockDiff {unBlockDiff :: Diff blk}

-- | The combined changes from ticking and (re)applying a block
newtype TickAndBlockDiff blk = TickAndBlockDiff {unTickAndBlockDiff :: Diff blk}

-- | The changes a transaction produces to the tables: a 'Semigroup'
-- so that a sequence of diffs composes.
--
-- This semigroup is not commutative! The 'TxsDiff's must be
-- composed in the same order the originating transactions were
-- applied.
newtype TxsDiff blk = TxsDiff {unTxsDiff :: Diff blk}

-- | The per-block era\/table logic of UTxO-HD: the opaque table payloads
-- (@Keys@\/@Values@\/@*Diff blk@) plus the pure operations on them.
--
-- Instances:
--
--   * Shelley carries the real instance: @Keys = Set TxIn@,
--     @Values = Map TxIn TxOut@, @*Diff = Diff TxIn TxOut@.
--
--   * Byron\/mock blocks have no on-disk tables, so their @Keys@\/@Values@\/
--     @*Diff@ are trivial ('UnitTables').
--
--   * The hard-fork combinator uses era-tagged @NS@ payloads; 'forwardTickDiff'
--     translates values across a era boundary using the per-era translations
--     carried on the 'CanHardFork' class.
type BlockSupportsLedgerHD :: Type -> Constraint
class (Semigroup (TxsDiff blk), Semigroup (Keys blk)) => BlockSupportsLedgerHD blk where
  -- | The keys a block needs used to query the backend for the corresponding
  -- | 'Values'.
  type Keys blk :: Type

  -- | The values read for a set of 'Keys', e.g. the @TxOut@s those @TxIn@s.
  -- resolve to.
  type Values blk :: Type

  -- | Changes produced by various operations. See 'TickDiff', 'BlockDiff',
  -- | 'TickAndBlockDiff' and 'TxsDiff'.
  type Diff blk :: Type

  -- | Extract the keys a block requests.
  blockKeys :: blk -> Keys blk

  -- | Combine the diffs from ticking and block application, as for
  -- @tickThenApply@, for example.
  --
  -- INVARIANT For the
  -- 'Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock',
  -- the two argument diffs will _already_ be in the same era.
  combineTickAndBlockDiff :: TickDiff blk -> BlockDiff blk -> TickAndBlockDiff blk

  forwardTickDiff :: TickDiff blk -> Values blk -> Values blk

  -- TODO this seems to only be needed by the @Dual@ block; I suspect
  -- we could refactor there to eliminate the need for this
  forwardBlockDiff :: BlockDiff blk -> Values blk -> Values blk
  forwardTickAndBlockDiff :: TickAndBlockDiff blk -> Values blk -> Values blk
  forwardTxsDiff :: TxsDiff blk -> Values blk -> Values blk

  -- | Restrict the values to the given keys.
  restrictValues :: Keys blk -> Values blk -> Values blk

  -- | The number of entries, for the @tablesSize@ statistic.
  valuesSize :: Values blk -> Int

  -- | Serialise the values for an InMemory snapshot. No era tag: single-era
  -- instances just use the era's @toCBOR@\/@toEraCBOR@. The hard-fork combinator
  -- encodes the current era's @NS@ arm directly (the @NS@ already carries the
  -- era).
  encodeValuesForInMemory :: Values blk -> Encoding

  -- | Deserialise the values for an InMemory snapshot.
  --
  -- Takes the already-loaded 'LedgerState' as an era hint: there is no on-disk
  -- era tag (see 'encodeValues'), so the hard-fork combinator uses the state's
  -- current era to pick which @NS@ arm to decode into. Single-era instances
  -- ignore it. The snapshot loads the state before the tables, so it is always
  -- available at the call site.
  decodeValuesForInMemory :: forall s. LedgerState blk -> Decoder s (Values blk)

-- | The on-disk table operations that only /single-era/ blocks support, split
-- out of 'BlockSupportsLedgerHD'.
--
-- These are the operations that need to see the entries of a table at the
-- /concrete era level/: paging for @QFTraverseTables@ queries, and the
-- entry-level (de)construction the on-disk (LSM) backend needs to turn the
-- opaque @'Keys'@\/@'Values'@\/@*Diff@ into individual @('TxIn', 'TxOut')@
-- rows.
--
-- The 'Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock' has
-- /no/ instance: it answers such requests by dispatching to the current era
-- (via @matchNS@, see the LSM backend's @BlockSupportsLSM@) and working there,
-- so there is no single @TxIn@\/@TxOut@ at the @HardForkBlock@ level.
--
-- The 'MemPack' codecs for the era's @TxIn@\/@TxOut@ are superclasses: the
-- on-disk (LSM) backend serialises individual entries with them, and bundling
-- them here means the hard-fork dispatch reaches them through @proxySingle@
-- (via @SingleEraBlock@) with no extra per-era constraint.
type SingleEraBlockSupportsLedgerHD :: Type -> Constraint
class
  ( BlockSupportsLedgerHD blk
  , MemPack (TxIn blk)
  , MemPack (TxOut blk)
  ) =>
  SingleEraBlockSupportsLedgerHD blk
  where
  type TxIn blk

  type TxOut blk

  -- | Read a bounded range of values, optionally starting just after a given
  -- key. Returns the read values and the last key read (if any), to be fed back
  -- for the next page.
  rangeReadValues ::
    (Maybe (TxIn blk), Int) -> Values blk -> (Values blk, Maybe (TxIn blk))

  -- | The keys as a plain list of @'TxIn'@s (e.g. for lookups on the on-disk
  -- backend).
  keysToList :: Keys blk -> [TxIn blk]

  -- | The values as a plain list of entries (e.g. to populate / stream the
  -- on-disk backend).
  valuesToList :: Values blk -> [(TxIn blk, TxOut blk)]

  -- | Rebuild the values from a list of entries (e.g. from a backend page /
  -- lookup result).
  valuesFromList :: [(TxIn blk, TxOut blk)] -> Values blk

  -- | The diff as a plain list of per-key insert\/delete deltas (e.g. to batch
  -- onto the on-disk backend).
  diffToList :: TickAndBlockDiff blk -> [(TxIn blk, Diff.Delta (TxOut blk))]

  -- | Serialise a @'TxIn'@ to on-disk key bytes whose unsigned lexicographic
  -- order matches the Haskell @'Ord' ('TxIn' blk)@.
  --
  -- The on-disk (LSM) backend stores entries keyed by these bytes and pages
  -- them in byte order (range reads, snapshot streaming), so the byte order
  -- /must/ agree with @'Ord' ('TxIn' blk)@ or pages would skip or repeat keys.
  -- The era's plain 'MemPack' codec need not preserve order (Shelley's
  -- 'Ouroboros.Consensus.Shelley.Ledger.SL.TxIn' is little-endian on the
  -- index), so this is a distinct method the era implements correctly (Shelley casts
  -- through @BigEndianTxIn@, mirroring 'encodeValues'\/'decodeValues').
  packTxInBytes :: TxIn blk -> ByteArray

  -- | Inverse of 'packTxInBytes': decode a @'TxIn'@ from its on-disk key bytes.
  unpackTxInBytes :: Buffer b => b -> Either SomeError (TxIn blk)

  emptyValues :: Values blk

  emptyTickDiff :: TickDiff blk

  -- | Combine 'TickDiff' that arose from translation with 'TickDiff' that arose
  -- from the _actual_ tick
  --
  -- For the 'Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock' tick
  -- logic: when the tick's destination slot is in the next era, the HFC first
  -- translates the given ledger state into the next era, and /then/ calls this
  -- function for the /new era/ to reach the given destination slot. Thus the
  -- result of the translation function should anticipate that subsequent tick
  -- call in the new era and the tick logic should anticipate that its starting
  -- state might be the translation of a ledger state from /before/ the era
  -- boundary.
  --
  -- In particular, the combination of the translation and the subsequent tick
  -- will need to correctly implement any necessary "end of epoch" and "end of
  -- era" logic specific to the old era.
  --
  -- This method must specifically combine the translation's 'TickDiff' with the
  -- subsequent tick's 'TickDiff'.
  --
  -- At time of writing, all era-specific 'applyChainTickLedgerResult' methods
  -- always return 'emptyTickDiff'. It's only ever 'translateLedgerStateWith'
  -- that /sometimes/ generates a non-empty 'TickDiff'. Thus, this method's
  -- second argument is /currently/ always 'emptyTickDiff'. And in fact its
  -- first argument /usually/ is also 'emptyTickDiff'.
  combineTransAndTickDiff :: TickDiff blk -> TickDiff blk -> TickDiff blk
