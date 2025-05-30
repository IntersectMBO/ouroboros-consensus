{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Definition is 'IsLedger'
--
-- Normally this is imported from "Ouroboros.Consensus.Ledger.Abstract". We
-- pull this out to avoid circular module dependencies.
module Ouroboros.Consensus.Ledger.Basics
  ( -- * The 'LedgerState' definition
    LedgerCfg
  , LedgerState
  , TickedLedgerState

    -- * Definition of a ledger independent of a choice of block
  , ComputeLedgerEvents (..)
  , IsLedger (..)
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

    -- * Re-exports
  , module Ouroboros.Consensus.Ledger.Tables
  ) where

import Data.Kind (Constraint, Type)
import GHC.Generics
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Ledger.Tables
import Ouroboros.Consensus.Ticked
import Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Tip
-------------------------------------------------------------------------------}

type GetTip :: LedgerStateKind -> Constraint
class GetTip l where
  -- | Point of the most recently applied block
  --
  -- Should be 'GenesisPoint' when no blocks have been applied yet
  getTip :: forall mk. l mk -> Point l

getTipHash :: GetTip l => l mk -> ChainHash l
getTipHash = pointHash . getTip

getTipSlot :: GetTip l => l mk -> WithOrigin SlotNo
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
type VoidLedgerEvent :: LedgerStateKind -> Type
data VoidLedgerEvent l

-- | The result of invoke a ledger function that does validation
--
-- Note: we do not instantiate 'Applicative' or 'Monad' for this type because
-- those interfaces would typically incur space leaks. We encourage you to
-- process the events each time you invoke a ledger function.
data LedgerResult l a = LedgerResult
  { lrEvents :: [AuxLedgerEvent l]
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
type LedgerCfg :: LedgerStateKind -> Type
type family LedgerCfg l :: Type

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

type IsLedger :: LedgerStateKind -> Constraint
class
  ( -- Requirements on the ledger state itself
    forall mk. EqMK mk => Eq (l mk)
  , forall mk. NoThunksMK mk => NoThunks (l mk)
  , forall mk. ShowMK mk => Show (l mk)
  , -- Requirements on 'LedgerCfg'
    NoThunks (LedgerCfg l)
  , -- Requirements on 'LedgerErr'
    Show (LedgerErr l)
  , Eq (LedgerErr l)
  , NoThunks (LedgerErr l)
  , -- Get the tip
    --
    -- See comment for 'applyChainTickLedgerResult' about the tip of the
    -- ticked ledger.
    GetTip l
  , GetTip (Ticked l)
  ) =>
  IsLedger l
  where
  -- | Errors that can arise when updating the ledger
  --
  -- This is defined here rather than in 'ApplyBlock', since the /type/ of
  -- these errors does not depend on the type of the block.
  type LedgerErr l :: Type

  -- | Event emitted by the ledger
  --
  -- TODO we call this 'AuxLedgerEvent' to differentiate from 'LedgerEvent' in
  -- 'InspectLedger'. When that module is rewritten to make use of ledger
  -- derived events, we may rename this type.
  type AuxLedgerEvent l :: Type

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
  -- Ticking a ledger state may not use any data from the 'LedgerTables',
  -- however it might produce differences in the tables, in particular because
  -- era transitions happen when ticking a ledger state.
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
    ComputeLedgerEvents ->
    LedgerCfg l ->
    SlotNo ->
    l EmptyMK ->
    LedgerResult l (Ticked l DiffMK)

-- | 'lrResult' after 'applyChainTickLedgerResult'
applyChainTick ::
  IsLedger l =>
  ComputeLedgerEvents ->
  LedgerCfg l ->
  SlotNo ->
  l EmptyMK ->
  Ticked l DiffMK
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
-- This type is parametrized over @mk :: 'MapKind'@ to express the
-- 'LedgerTables' contained in such a 'LedgerState'. See 'LedgerTables' for a
-- more thorough description.
--
-- The main operations we can do with a 'LedgerState' are /ticking/ (defined in
-- 'IsLedger'), and /applying a block/ (defined in
-- 'Ouroboros.Consensus.Ledger.Abstract.ApplyBlock').
type LedgerState :: Type -> LedgerStateKind
data family LedgerState blk mk

type TickedLedgerState blk = Ticked (LedgerState blk)

type instance HeaderHash (LedgerState blk) = HeaderHash blk

instance StandardHash blk => StandardHash (LedgerState blk)

type LedgerConfig blk = LedgerCfg (LedgerState blk)
type LedgerError blk = LedgerErr (LedgerState blk)
