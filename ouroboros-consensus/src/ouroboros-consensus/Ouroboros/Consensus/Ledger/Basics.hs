{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

-- | Core ledger vocabulary that the rest of the Consensus layer is built on.
--
-- This module exists to break import cycles: it holds the constraint-free
-- pieces (the 'LedgerState' data family, the 'IsLedger' class with ticking,
-- 'GetTip', 'LedgerResult', 'Statistics') alongside the per-block handle
-- vocabulary ('BlockSupportsLedgerHD', 'LedgerTablesHandle', 'Handle')
-- needed to tick a ledger. Block application ('ApplyBlock') is layered on
-- top in "Ouroboros.Consensus.Ledger.Abstract", which is the module most
-- consumers should import.
module Ouroboros.Consensus.Ledger.Basics
  ( -- * The 'LedgerState' definition
    LedgerCfg
  , LedgerState
  , TickedLedgerState
  , Statistics (..)

    -- * IsLedger
  , ComputeLedgerEvents (..)
  , IsLedger (..)
  , AuxLedgerEvent
  , applyChainTick

    -- * Per-block handle vocabulary
  , BlockSupportsLedgerHD (..)
  , LedgerTablesHandle
  , Handle

    -- * Ledger Events
  , LedgerResult (..)
  , VoidLedgerEvent
  , castLedgerResult
  , embedLedgerResult
  , pureLedgerResult

    -- * GetTip
  , GetTip (..)
  , getTipHash
  , getTipSlot

    -- * Associated types by block type
  , LedgerConfig
  , LedgerError
  ) where

import Data.Kind (Constraint, Type)
import GHC.Generics
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Ticked
import Ouroboros.Consensus.Util ((...:))
import Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Tip
-------------------------------------------------------------------------------}

type GetTip :: (Type -> Type) -> Type -> Constraint
class GetTip l blk where
  -- | Point of the most recently applied block
  --
  -- Should be 'GenesisPoint' when no blocks have been applied yet
  getTip :: l blk -> Point blk

getTipHash :: GetTip l blk => l blk -> ChainHash blk
getTipHash = pointHash . getTip

getTipSlot :: GetTip l blk => l blk -> WithOrigin SlotNo
getTipSlot = pointSlot . getTip

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
type LedgerCfg :: (Type -> Type) -> Type -> Type
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
  ( -- Requirements on 'LedgerCfg'
    NoThunks (LedgerCfg l blk)
  , -- Requirements on 'LedgerErr'
    Show (LedgerErr l blk)
  , Eq (LedgerErr l blk)
  , NoThunks (LedgerErr l blk)
  , -- Get the tip
    --
    -- See comment for 'applyChainTickLedgerResult' about the tip of the
    -- ticked ledger.
    GetTip l blk
  , GetTip (Ticked l) blk
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
  -- Ticking does not read on-disk tables, but may emit /diffs/ over them
  -- (notably at era boundaries in the hard-fork combinator). The returned
  -- handle owns a fresh 'LedgerTablesHandle' independent of the input.
  --
  -- PRECONDITION: The slot number must be strictly greater than the slot at
  -- the tip of the ledger (except for EBBs, obviously..).
  --
  -- NOTE: 'applyChainTickLedgerResult' should /not/ change the tip of the
  -- underlying ledger state, which should still refer to the most recent
  -- applied /block/. In other words, we should have:
  --
  -- prop> ledgerTipPoint (applyChainTick cfg slot st) == ledgerTipPoint st
  applyChainTickLedgerResult ::
    (BlockSupportsLedgerHD m blk, MonadThrow m) =>
    ComputeLedgerEvents ->
    LedgerCfg l blk ->
    SlotNo ->
    Handle l m blk ->
    m (LedgerResult blk (Handle (Ticked l) m blk))

-- | 'lrResult' after 'applyChainTickLedgerResult'
applyChainTick ::
  (BlockSupportsLedgerHD m blk, MonadThrow m, IsLedger l blk) =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  SlotNo ->
  Handle l m blk ->
  m (Handle (Ticked l) m blk)
applyChainTick = fmap lrResult ...: applyChainTickLedgerResult

{-------------------------------------------------------------------------------
  Link block to its ledger
-------------------------------------------------------------------------------}

-- | Ledger state associated with a block.
--
-- This is the Consensus notion of a /ledger state/. Each block type is
-- associated with one of the Ledger types via this data family. Virtually
-- every concept in this codebase revolves around this type. Whenever we use
-- the type variable @l@ we intend to signal that the expected instantiation
-- is either a 'LedgerState' or some wrapper over it (like
-- 'Ouroboros.Consensus.Ledger.Extended.ExtLedgerState').
--
-- The on-disk part of the state (currently: the UTxO) is /not/ stored
-- inside the 'LedgerState' itself; it lives in a 'LedgerTablesHandle'
-- bundled into the 'StateHandle' that wraps the 'LedgerState'.
--
-- The main operations we can do with a 'LedgerState' are /ticking/ (defined
-- in 'IsLedger') and /applying a block/ (defined in
-- 'Ouroboros.Consensus.Ledger.Abstract.ApplyBlock').
type LedgerState :: Type -> Type
data family LedgerState blk

-- | Per-block handle for reading and writing the on-disk part of a ledger
-- state (currently: UTxO).
--
-- This is a type family because only ledgers that actually maintain an
-- on-disk component (notably Shelley) instantiate it concretely.
-- 'HardForkBlock' has no instance: its handles are managed era-by-era and
-- carried inside the per-era 'StateHandle's.
--
-- The expected shape of an instance is a record of operations in @m@
-- (e.g. @readKeys@ / @writeKeys@ / @close@). Carrying @m@ as a visible
-- type parameter is necessary because the record fields close over @m@.
type LedgerTablesHandle :: (Type -> Type) -> Type -> Type
type family LedgerTablesHandle m blk

-- | The handle representing a ledger view at @l blk@.
--
-- Standalone (not associated with 'IsLedger') because the family depends
-- only on the ledger view @l@, not on the block — and 'IsLedger' is
-- per-@(l, blk)@ pair. Having one canonical instance per @l@ matches the
-- reality: @Handle LedgerState = StateHandle@ regardless of which block.
--
-- For 'LedgerState' this is 'StateHandle' (a data family on 'BlockSupportsLedgerHD');
-- for 'ExtLedgerState' this is the plain record
-- 'Ouroboros.Consensus.Ledger.Extended.ExtStateHandle'.
type Handle :: (Type -> Type) -> (Type -> Type) -> Type -> Type
type family Handle l = r | r -> l

type instance Handle LedgerState = StateHandle
type instance Handle (Ticked LedgerState) = TickedStateHandle

-- | How to manage the resource-bearing state of a ledger view of @blk@ in
-- monad @m@.
--
-- The two associated data families are opaque handles owned by the
-- 'Ouroboros.Consensus.Storage.LedgerDB.LedgerDB' backend. They bundle a
-- pure 'LedgerState' (or its ticked variant) together with a
-- 'LedgerTablesHandle' that can serve table reads in @m@.
--
-- This class only deals with concrete @blk@-specific state. The
-- 'Ouroboros.Consensus.Ledger.Extended.ExtLedgerState' /
-- 'Ticked' 'Ouroboros.Consensus.Ledger.Extended.ExtLedgerState' wrappers
-- do not have 'BlockSupportsLedgerHD' instances — they are plain records that embed
-- a 'StateHandle' or 'TickedStateHandle' (see
-- "Ouroboros.Consensus.Ledger.Extended").
type BlockSupportsLedgerHD :: (Type -> Type) -> Type -> Constraint
class BlockSupportsLedgerHD m blk where
  -- | Opaque handle to an un-ticked ledger state plus its tables.
  data StateHandle m blk

  -- | Opaque handle to a ticked ledger state plus its tables.
  data TickedStateHandle m blk

  -- | Per-block context needed to construct fresh 'LedgerTablesHandle's
  -- (and, in the HFC case, to translate them across era boundaries).
  --
  -- For blocks that maintain no on-disk tables, or that capture their
  -- factory by closure at protocol-setup time, this is @()@ — see Byron
  -- and Shelley. For the hard-fork combinator this is
  -- 'HFLedgerTablesFactory' (defined on 'CanHardFork'), which is the
  -- recipe used by era-translation functions to materialise the
  -- destination era's tables. The bridging equation
  -- @LedgerTablesFactory m (HardForkBlock xs) = HFLedgerTablesFactory m xs@
  -- lives in the HFC's 'BlockSupportsLedgerHD' instance.
  --
  -- Threaded into 'Ouroboros.Consensus.Node.ProtocolInfo.pInfoInitLedger'
  -- so genesis construction can use the same factory the backend
  -- provides; the HFC also stashes its value on every
  -- 'HardForkStateHandle' so that ticking across an era boundary has
  -- access to it without an extra argument.
  type LedgerTablesFactory m blk
  type LedgerTablesFactory m blk = ()

  newStateHandle :: LedgerState blk -> LedgerTablesHandle m blk -> StateHandle m blk

  -- | Project the pure ledger state out of a handle.
  --
  -- Cheap; does not touch the backing 'LedgerTablesHandle'.
  state :: StateHandle m blk -> LedgerState blk

  -- | Project the pure ticked ledger state out of a handle.
  tickedState :: TickedStateHandle m blk -> Ticked LedgerState blk

  -- | Release the backing resources. Idempotent.
  close :: Monad m => StateHandle m blk -> m ()

  closeTicked :: Monad m => TickedStateHandle m blk -> m ()

  -- | Produce an independent copy of the handle, with its own backing
  -- resources. Useful before applying a block that may or may not be
  -- committed.
  duplicate :: Monad m => StateHandle m blk -> m (StateHandle m blk)

  duplicateTicked :: Monad m => TickedStateHandle m blk -> m (TickedStateHandle m blk)

  -- | Snapshot of operational statistics for the handle.
  getStats :: StateHandle m blk -> Statistics

-- | Operational stats for a 'StateHandle'.
--
-- Intentionally a record so backends can attach further diagnostics over
-- time without changing call-site code. Currently the only metric is the
-- number of UTxO entries in the backing store.
newtype Statistics = Statistics
  { ledgerTableSize :: Int
  }

type TickedLedgerState blk = Ticked LedgerState blk

type LedgerConfig blk = LedgerCfg LedgerState blk
type LedgerError blk = LedgerErr LedgerState blk
