{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Definition is 'IsLedger'
--
-- Normally this is imported from "Ouroboros.Consensus.Ledger.Abstract". We
-- pull this out to avoid circular module dependencies.
module Ouroboros.Consensus.Ledger.Basics
  ( -- * The 'LedgerState' definition
    LedgerCfg
  , LedgerState
  , TickedLedgerState

    -- * The mk-skin (review intermediate — see mk-skin-plan.md)
  , MapKind
  , LedgerStateKind
  , LedgerTables (..)
  , EmptyMK (..)
  , KeysMK (..)
  , ValuesMK (..)
  , DiffMK (..)
  , HasLedgerTables (..)
  , emptyLedgerTables
  , forgetLedgerTables

    -- * On-disk table vocabulary
  , TxIn
  , TxOut

    -- * UTxO-HD block axis
  , BlockSupportsUTxOHD (..)
  , SingleEraBlockSupportsUTxOHD (..)
  , SingleEraUTxOHDBlock (..)

    -- * Definition of a ledger independent of a choice of block
  , ComputeLedgerEvents (..)
  , IsLedger (..)
  , AuxLedgerEvent
  , applyChainTick

    -- * Ledger Events
  , LedgerResult (..)
  , VoidLedgerEvent
  , castLedgerResult
  , embedLedgerResult
  , pureLedgerResult

    -- * GetTip
  , GetTip (..)
  , GetTipSTM (..)
  , getTipHash
  , getTipM
  , getTipSlot

    -- * Associated types by block type
  , LedgerConfig
  , LedgerError
  ) where

import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)
import Data.Array.Byte (ByteArray)
import Data.Kind (Constraint, Type)
import Data.MemPack (MemPack, packByteArray)
import Data.MemPack.Buffer (Buffer)
import Data.MemPack.Error (SomeError)
import GHC.Generics
import Ouroboros.Consensus.Block.Abstract
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Ticked
import Ouroboros.Consensus.Util ((...:))
import Ouroboros.Consensus.Util.CBOR (unpackEither)
import Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  On-disk table vocabulary
-------------------------------------------------------------------------------}

-- | Each block has a notion of a @TxIn@: the key of the on-disk UTxO table.
--
-- This is era-stable across all Shelley-based eras. It is convenient per-block
-- vocabulary for the Shelley\/dual instances that build their opaque 'Keys' out
-- of it.
type TxIn :: Type -> Type
type family TxIn blk

-- | Each block has a notion of a @TxOut@: the value of the on-disk UTxO table.
type TxOut :: Type -> Type
type family TxOut blk

{-------------------------------------------------------------------------------
  Tip
-------------------------------------------------------------------------------}

type GetTip :: Type -> Constraint
class GetTip l where
  -- | Point of the most recently applied block
  --
  -- Should be 'GenesisPoint' when no blocks have been applied yet
  getTip :: l -> Point l

getTipHash :: GetTip l => l -> ChainHash l
getTipHash = pointHash . getTip

getTipSlot :: GetTip l => l -> WithOrigin SlotNo
getTipSlot = pointSlot . getTip

type GetTipSTM :: (Type -> Type) -> Type -> Constraint
class GetTipSTM m l where
  getTipSTM :: l -> STM m (Point l)

getTipM :: (GetTipSTM m l, MonadSTM m) => l -> m (Point l)
getTipM = atomically . getTipSTM

{-------------------------------------------------------------------------------
  Events directly from the ledger
-------------------------------------------------------------------------------}

-- | A 'Data.Void.Void' isomorph for explicitly declaring that some ledger has
-- no events
type VoidLedgerEvent :: Type
data VoidLedgerEvent

-- | The result of invoke a ledger function that does validation
--
-- Note: we do not instantiate 'Applicative' or 'Monad' for this type because
-- those interfaces would typically incur space leaks. We encourage you to
-- process the events each time you invoke a ledger function.
type LedgerResult :: Type -> Type -> Type
data LedgerResult blk a = LedgerResult
  { lrEvents :: [AuxLedgerEvent blk]
  , lrResult :: !a
  }
  deriving (Foldable, Functor, Traversable)

castLedgerResult ::
  AuxLedgerEvent l ~ AuxLedgerEvent l' =>
  LedgerResult l a ->
  LedgerResult l' a
castLedgerResult (LedgerResult x0 x1) = LedgerResult x0 x1

embedLedgerResult ::
  (AuxLedgerEvent l -> AuxLedgerEvent l') ->
  LedgerResult l a ->
  LedgerResult l' a
embedLedgerResult inj lr = lr{lrEvents = inj `map` lrEvents lr}

pureLedgerResult :: a -> LedgerResult l a
pureLedgerResult a =
  LedgerResult
    { lrEvents = mempty
    , lrResult = a
    }

{-------------------------------------------------------------------------------
  Definition of a ledger independent of a choice of block
-------------------------------------------------------------------------------}

-- | Static environment required for the ledger
--
-- Types that inhabit this family will come from the Ledger code.
type LedgerCfg :: StateKind -> Type -> Type
type family LedgerCfg l blk :: Type

-- | Event emitted by the ledger
--
-- TODO we call this 'AuxLedgerEvent' to differentiate from 'LedgerEvent' in
-- 'InspectLedger'. When that module is rewritten to make use of ledger
-- derived events, we may rename this type.
type AuxLedgerEvent :: Type -> Type
type family AuxLedgerEvent blk :: Type

-- | Whether we tell the ledger layer to compute ledger events
--
-- At the moment events are not emitted in any case in the consensus
-- layer (i.e. there is no handler for those events, nor are they
-- traced), so they are not really forced, we always discard
-- them. This behavior does not incur big costs thanks to laziness.
--
-- By passing 'OmitLedgerEvents' we tell the Ledger layer to not even
-- allocate thunks for those events, as we explicitly don't want them.
data ComputeLedgerEvents = ComputeLedgerEvents | OmitLedgerEvents
  deriving (Eq, Show, Generic, NoThunks)

type IsLedger :: (Type -> Type) -> Type -> Constraint
class
  ( -- Requirements on the ledger state itself
    Eq (l blk)
  , NoThunks (l blk)
  , Show (l blk)
  , -- Requirements on 'LedgerCfg'
    NoThunks (LedgerCfg l blk)
  , -- Requirements on 'LedgerErr'
    Show (LedgerErr l blk)
  , Eq (LedgerErr l blk)
  , NoThunks (LedgerErr l blk)
  , -- Get the tip
    --
    -- See comment for 'applyChainTickLedgerResult' about the tip of the
    -- ticked ledger.
    GetTip (l blk)
  , GetTip (Ticked l blk)
  , -- The block axis of UTxO-HD: provides the opaque @'Diff' blk@ that ticking
    -- (and block application) produces, together with its 'Semigroup' instance.
    BlockSupportsUTxOHD blk
  ) =>
  IsLedger l blk
  where
  -- | Errors that can arise when updating the ledger
  --
  -- This is defined here rather than in 'ApplyBlock', since the /type/ of
  -- these errors does not depend on the type of the block.
  type LedgerErr l blk :: Type

  -- | Apply "slot based" state transformations
  --
  -- When a block is applied to the ledger state, a number of things happen
  -- purely based on the slot number of that block. For example:
  --
  -- * In Byron, scheduled updates are applied, and the update system state is
  --   updated.
  -- * In Shelley, delegation state is updated (on epoch boundaries).
  --
  -- The consensus layer must be able to apply such a "chain tick" function,
  -- primarily when validating transactions in the mempool (which, conceptually,
  -- live in "some block in the future") or when extracting valid transactions
  -- from the mempool to insert into a new block to be produced.
  --
  -- This is not allowed to throw any errors. After all, if this could fail,
  -- it would mean a /previous/ block set up the ledger state in such a way
  -- that as soon as a certain slot was reached, /any/ block would be invalid.
  --
  -- Ticking a ledger state needs no values from the on-disk tables (in
  -- particular it does not read the UTxO), but it may nonetheless /produce/ a
  -- @'Diff' blk@ out of nothing: era transitions happen when ticking, and some
  -- of them delete entries from the UTxO (e.g. the AVVM addresses removed at
  -- the Shelley-to-Allegra boundary). That diff is returned alongside the
  -- ticked state and must be composed with the block-application diff (and
  -- forwarded onto any values read against the pre-tick state).
  --
  -- PRECONDITION: The slot number must be strictly greater than the slot at
  -- the tip of the ledger (except for EBBs, obviously..).
  --
  -- NOTE: 'applyChainTickLedgerResult' should /not/ change the tip of the
  -- underlying ledger state, which should still refer to the most recent
  -- applied /block/. In other words, we should have:
  --
  -- prop> ledgerTipPoint (fst (applyChainTick cfg slot st)) == ledgerTipPoint st
  applyChainTickLedgerResult ::
    ComputeLedgerEvents ->
    LedgerCfg l blk ->
    SlotNo ->
    l blk ->
    LedgerResult blk (Ticked l blk, Diff blk)

-- | 'lrResult' after 'applyChainTickLedgerResult'
applyChainTick ::
  IsLedger l blk =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  SlotNo ->
  l blk ->
  (Ticked l blk, Diff blk)
applyChainTick = lrResult ...: applyChainTickLedgerResult

{-------------------------------------------------------------------------------
  Link block to its ledger
-------------------------------------------------------------------------------}

-- | Ledger state associated with a block
--
-- This is the Consensus notion of a Ledger /ledger state/. Each block type is
-- associated with one of the Ledger types for the /ledger state/. Virtually
-- every concept in this codebase revolves around this type, or the referenced
-- @blk@. Whenever we use the type variable @l@ we intend to signal that the
-- expected instantiation is either a 'LedgerState' or some wrapper over it
-- (like the 'Ouroboros.Consensus.Ledger.Extended.ExtLedgerState').
--
-- The on-disk table data (the UTxO) is not a parameter of this state; it is
-- carried separately as the opaque @Values blk@\/@Diff blk@ payloads of
-- 'BlockSupportsUTxOHD', threaded through the read\/apply functions.
--
-- The main operations we can do with a 'LedgerState' are /ticking/ (defined in
-- 'IsLedger'), and /applying a block/ (defined in
-- 'Ouroboros.Consensus.Ledger.Abstract.ApplyBlock').
type LedgerState :: Type -> LedgerStateKind
data family LedgerState blk mk

type TickedLedgerState blk = Ticked LedgerState blk

type instance HeaderHash (LedgerState blk) = HeaderHash blk

instance StandardHash blk => StandardHash (LedgerState blk)

type LedgerConfig blk = LedgerCfg LedgerState blk
type LedgerError blk = LedgerErr LedgerState blk

{-------------------------------------------------------------------------------
  The mk-skin (review intermediate — see mk-skin-plan.md)

  A thin newtype layer that re-expresses the opaque 'Keys'\/'Values'\/'Diff blk'
  payloads in @main@'s map-kind vocabulary, so that the review diff against the
  prepare-11.1 base cancels the vocabulary churn and leaves the genuine
  structural redesign.

  This is deliberately /not/ @main@'s machinery:

    * @l@ is the block (@l = blk@), the only well-kinded reading;

    * the map-kind is single-argument (@'MapKind' = Type -> Type@), so each
      wrapper is a thin newtype over the /existing/ opaque payload — there is no
      @CanMapMK@\/@mapKeysMK@ combinator zoo and no canonical machinery;

    * @'LedgerTables' blk 'ValuesMK' ≅ 'Values' blk@, and likewise for
      'KeysMK'\/'DiffMK'.

  The whole layer (and the @mk@ argument of 'LedgerState') is to be stripped in
  the final commit; see @mk-skin-plan.md@.
-------------------------------------------------------------------------------}

-- | The kind of a map-kind: a wrapper that turns a block into the table payload
-- of one phase (keys\/values\/diff). Single-argument, unlike @main@'s @k -> v ->
-- Type@.
type MapKind = Type -> Type

-- | The kind of a ledger-state-like type once it carries an 'mk' argument.
type LedgerStateKind = MapKind -> Type

-- | The kind of an unapplied ledger-state functor (the generic @l@ in
-- @l blk mk@), e.g. 'LedgerState' itself.
type StateKind = Type -> LedgerStateKind

-- | The on-disk tables of a block, in the chosen map-kind. A thin newtype over
-- @mk blk@ (e.g. @'LedgerTables' blk 'ValuesMK' ≅ 'Values' blk@).
type LedgerTables :: Type -> MapKind -> Type
newtype LedgerTables l mk = LedgerTables (mk l)

-- | No tables.
type EmptyMK :: MapKind
data EmptyMK l = EmptyMK

-- | The keys phase: a thin wrapper over the opaque 'Keys'.
type KeysMK :: MapKind
newtype KeysMK l = KeysMK (Keys l)

-- | The values phase: a thin wrapper over the opaque 'Values'.
type ValuesMK :: MapKind
newtype ValuesMK l = ValuesMK (Values l)

-- | The diff phase: a thin wrapper over the opaque 'Diff'.
type DiffMK :: MapKind
newtype DiffMK l = DiffMK (Diff l)

-- | @main@'s vocabulary for getting\/setting a 'LedgerState'\'s tables, restored
-- over the skin so call sites (@projectLedgerTables@\/@withLedgerTables@\/
-- @forgetLedgerTables@) read exactly as on @main@ and cancel in the review diff.
--
-- This is /not/ @main@'s combinator class (no @LedgerTableConstraints@\/
-- @ltliftA2@ zoo). It is a per-@blk@ class over @'LedgerState' blk mk@;
-- 'Ouroboros.Consensus.Ledger.Extended.ExtLedgerState' gets its own thin
-- wrapper. To be stripped with the rest of the skin.
type HasLedgerTables :: Type -> Constraint
class BlockSupportsUTxOHD blk => HasLedgerTables blk where
  projectLedgerTables :: LedgerState blk mk -> LedgerTables blk mk
  withLedgerTables ::
    LedgerState blk any -> LedgerTables blk mk -> LedgerState blk mk

-- | The empty tables.
emptyLedgerTables :: LedgerTables blk EmptyMK
emptyLedgerTables = LedgerTables EmptyMK

-- | Drop a 'LedgerState'\'s tables.
forgetLedgerTables ::
  HasLedgerTables blk => LedgerState blk mk -> LedgerState blk EmptyMK
forgetLedgerTables st = withLedgerTables st emptyLedgerTables

{-------------------------------------------------------------------------------
  UTxO-HD block axis
-------------------------------------------------------------------------------}

-- | The per-block era\/table logic of UTxO-HD: the opaque table payloads
-- (@Keys@\/@Values@\/@Diff blk@) plus the pure operations on them.
--
-- This lives in "Ouroboros.Consensus.Ledger.Basics" (rather than its own
-- module) because 'IsLedger' has it as a superclass (it provides the @Diff blk@
-- that ticking produces), so the two must share a module to avoid an import
-- cycle.
--
-- Instances:
--
--   * Shelley carries the real instance: @Keys = Set TxIn@,
--     @Values = Map TxIn TxOut@, @Diff = Diff TxIn TxOut@.
--
--   * Byron\/mock blocks have no on-disk tables, so their @Keys@\/@Values@\/
--     @Diff@ are trivial (@Void@\/@()@).
--
--   * The hard-fork combinator uses era-tagged @NS@ payloads; 'forward'
--     translates values across a rare era boundary using the per-era
--     translations carried on the 'CanHardFork' class (no config needed).
type BlockSupportsUTxOHD :: Type -> Constraint
class (Semigroup (Diff blk), Semigroup (Keys blk)) => BlockSupportsUTxOHD blk where
  -- | The keys a block consumes, e.g. the @TxIn@s spent by its transactions.
  type Keys blk :: Type

  -- | The values read for a set of 'Keys', e.g. the @TxOut@s those @TxIn@s
  -- resolve to.
  type Values blk :: Type

  -- | The change a block produces to the tables: a 'Semigroup' so that a
  -- sequence of diffs composes (later diffs winning).
  type Diff blk :: Type

  -- | Extract the keys a block consumes (phase 1). Pure and essentially free.
  blockKeys :: blk -> Keys blk

  -- | Apply diffs to read values, ordered oldest-to-newest. Pure and
  -- config-free.
  --
  -- For single-era blocks this is just @'applyDiff' ('mconcat' diffs)@. For the
  -- hard-fork combinator the common case is same-era (the diff and values share
  -- the era tag); the rare boundary case (a ticked diff at era @E+1@ over values
  -- still at era @E@) first upgrades the values @E → E+1@ using the per-era
  -- translations carried on the 'CanHardFork' class (@hardForkEraTranslation@),
  -- so no config is needed here.
  --
  -- @'forward' [] = 'id'@ must hold genuinely so that, per block, applying an
  -- empty diff never rebuilds the values.
  --
  -- Note: @blk@ occurs only under the non-injective families 'Diff'\/'Values',
  -- so call sites disambiguate with @'forward' \@blk diffs vals@.
  forward :: [Diff blk] -> Values blk -> Values blk

  -- | Restrict the values to the given keys. This is the point read of the
  -- InMemory backend.
  restrictValues :: Keys blk -> Values blk -> Values blk

  -- | The number of entries, for the @tablesSize@ statistic.
  valuesSize :: Values blk -> Int

  -- | Serialise the values for an InMemory snapshot. No era tag: single-era
  -- instances just use the era's @toCBOR@\/@toEraCBOR@. The hard-fork combinator
  -- encodes the current era's @NS@ arm directly (the @NS@ already carries the
  -- era).
  encodeValues :: Values blk -> Encoding

  -- | Deserialise the values for an InMemory snapshot.
  --
  -- Takes the already-loaded 'LedgerState' as an era hint: there is no on-disk
  -- era tag (see 'encodeValues'), so the hard-fork combinator uses the state's
  -- current era to pick which @NS@ arm to decode into. Single-era instances
  -- ignore it. The snapshot loads the state before the tables, so it is always
  -- available at the call site.
  decodeValues :: forall s. LedgerState blk EmptyMK -> Decoder s (Values blk)

-- | The on-disk table operations that only /single-era/ blocks support, split
-- out of 'BlockSupportsUTxOHD'.
--
-- These are the operations that need to see the entries of a table at the
-- /concrete era level/: paging for @QFTraverseTables@ queries, and the
-- entry-level (de)construction the on-disk (LSM) backend needs to turn the
-- opaque @'Keys'@\/@'Values'@\/@'Diff'@ into individual @('TxIn', 'TxOut')@
-- rows. The 'Ouroboros.Consensus.HardFork.Combinator.Basics.HardForkBlock' has
-- /no/ instance: it answers such requests by dispatching to the current era
-- (via @matchNS@, see the LSM backend's @BlockSupportsLSM@) and working there,
-- so there is no single @TxIn@\/@TxOut@ at the @HardForkBlock@ level.
-- The 'MemPack' codecs for the era's @TxIn@\/@TxOut@ are superclasses: the
-- on-disk (LSM) backend serialises individual entries with them, and bundling
-- them here means the hard-fork dispatch reaches them through @proxySingle@
-- (via @SingleEraBlock@) with no extra per-era constraint.
type SingleEraBlockSupportsUTxOHD :: Type -> Constraint
class
  ( BlockSupportsUTxOHD blk
  , MemPack (TxIn blk)
  , MemPack (TxOut blk)
  ) =>
  SingleEraBlockSupportsUTxOHD blk
  where
  -- | Read a bounded range of values, optionally starting just after a given
  -- key. Returns the read values and the last key read (if any), to be fed back
  -- for the next page.
  rangeReadValues ::
    (Maybe (TxIn blk), Int) -> Values blk -> (Values blk, Maybe (TxIn blk))

  -- | The keys as a plain list of @'TxIn'@s (e.g. for point lookups on the
  -- on-disk backend).
  keysToList :: Keys blk -> [TxIn blk]

  -- | The values as a plain list of entries (e.g. to populate / stream the
  -- on-disk backend).
  valuesToList :: Values blk -> [(TxIn blk, TxOut blk)]

  -- | Rebuild the values from a list of entries (e.g. from a backend page /
  -- point-lookup result).
  valuesFromList :: [(TxIn blk, TxOut blk)] -> Values blk

  -- | The diff as a plain list of per-key insert\/delete deltas (e.g. to batch
  -- onto the on-disk backend).
  diffToList :: Diff blk -> [(TxIn blk, Diff.Delta (TxOut blk))]

  -- | Serialise a @'TxIn'@ to on-disk key bytes whose unsigned lexicographic
  -- order matches the Haskell @'Ord' ('TxIn' blk)@.
  --
  -- The on-disk (LSM) backend stores entries keyed by these bytes and pages
  -- them in byte order (range reads, snapshot streaming), so the byte order
  -- /must/ agree with @'Ord' ('TxIn' blk)@ or pages would skip or repeat keys.
  -- The era's plain 'MemPack' codec need not preserve order (Shelley's
  -- 'Ouroboros.Consensus.Shelley.Ledger.SL.TxIn' is little-endian on the
  -- index), so this is a distinct method the era implements correctly (Shelley casts
  -- through @BigEndianTxIn@, mirroring 'encodeValues'\/'decodeValues'). The
  -- default is the plain 'MemPack' codec, valid for eras whose key
  -- serialisation is already order-preserving (e.g. the trivial Byron key).
  packTxInBytes :: TxIn blk -> ByteArray
  default packTxInBytes :: TxIn blk -> ByteArray
  packTxInBytes = packByteArray True

  -- | Inverse of 'packTxInBytes': decode a @'TxIn'@ from its on-disk key bytes.
  unpackTxInBytes :: Buffer b => b -> Either SomeError (TxIn blk)
  default unpackTxInBytes :: Buffer b => b -> Either SomeError (TxIn blk)
  unpackTxInBytes = unpackEither

-- | Operations only the single-era blocks support. The hard-fork combinator
-- has no single era to point at, so it cannot provide 'emptyValues' \/ 'emptyDiffs'
-- (and never needs them).
type SingleEraUTxOHDBlock :: Type -> Constraint
class BlockSupportsUTxOHD blk => SingleEraUTxOHDBlock blk where
  -- | The empty set of values.
  emptyValues :: Values blk

  -- | The empty diff.
  emptyDiffs :: Diff blk
