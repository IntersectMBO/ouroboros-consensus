{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Interface to the ledger layer
--
-- This module defines how to apply blocks to a ledger state, and re-exports
-- (from "Ouroboros.Consensus.Ledger.Basics") how to tick ledger states. These
-- are the two main operations we can do with a 'LedgerState'.
module Ouroboros.Consensus.Ledger.Abstract
  ( -- * Type-level validation marker
    Validated

    -- * Apply block
  , ApplyBlock (..)
  , UpdateLedger
  , defaultApplyBlockLedgerResult
  , defaultReapplyBlockLedgerResult

    -- * Derived
  , applyLedgerBlock
  , foldLedger
  , reapplyLedgerBlock
  , refoldLedger
  , tickThenApply
  , tickThenApplyLedgerResult
  , tickThenReapply
  , tickThenReapplyLedgerResult

    -- ** Short-hand
  , ledgerTipHash
  , ledgerTipPoint
  , ledgerTipSlot

    -- * Re-exports
  , module Ouroboros.Consensus.Ledger.Basics
  ) where

import Control.Monad.Except
import Control.Monad.Trans (lift)
import qualified Control.State.Transition.Extended as STS
import Data.Kind (Type)
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ticked (Ticked)
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.IOLike

-- | " Validated " transaction or block
--
-- The ledger defines how to validate transactions and blocks. It's possible the
-- type before and after validation may be distinct (eg Alonzo transactions),
-- which originally motivated this family.
--
-- We also gain the related benefit that certain interface functions, such as
-- those that /reapply/ blocks, can have a more precise type now. TODO
--
-- Similarly, the Node-to-Client mini protocols can explicitly indicate that the
-- client trusts the blocks from the local server, by having the server send
-- 'Validated' blocks to the client. TODO
--
-- Note that validation has different implications for a transaction than for a
-- block. In particular, a validated transaction can be " reapplied " to
-- different ledger states, whereas a validated block must only be " reapplied "
-- to the exact same ledger state (eg as part of rebuilding from an on-disk
-- ledger snapshot).
--
-- Since the ledger defines validation, see the ledger details for concrete
-- examples of what determines the validity (wrt to a 'LedgerState') of a
-- transaction and/or block. Example properties include: a transaction's claimed
-- inputs exist and are still unspent, a block carries a sufficient
-- cryptographic signature, etc.
data family Validated x :: Type

{-------------------------------------------------------------------------------
  Apply block to ledger state
-------------------------------------------------------------------------------}

class
  ( IsLedger l blk
  , HasHeader blk
  , HasHeader (Header blk)
  ) =>
  ApplyBlock l blk
  where
  -- | Apply a block to the ledger state.
  --
  -- This is passed the ledger state ticked to the slot of the given block, so
  -- 'applyChainTickLedgerResult' has already been called.
  --
  -- Users of this function can set any validation level allowed by the
  -- @small-steps@ package. See "Control.State.Transition.Extended".
  applyBlockLedgerResultWithValidation ::
    (Monad m, HasCallStack, BlockSupportsLedgerHD m blk) =>
    STS.ValidationPolicy ->
    ComputeLedgerEvents ->
    LedgerCfg l blk ->
    blk ->
    Handle (Ticked l) m blk ->
    ExceptT (LedgerErr l blk) m (LedgerResult blk (Handle l m blk))

  -- | Apply a block to the ledger state.
  --
  -- This is passed the ledger state ticked to the slot of the given block, so
  -- 'applyChainTickLedgerResult' has already been called.
  --
  -- This function will use 'ValidateAll' policy for calling the ledger rules.
  applyBlockLedgerResult ::
    (Monad m, HasCallStack, BlockSupportsLedgerHD m blk) =>
    ComputeLedgerEvents ->
    LedgerCfg l blk ->
    blk ->
    Handle (Ticked l) m blk ->
    ExceptT (LedgerErr l blk) m (LedgerResult blk (Handle l m blk))

  -- | Re-apply a block to the very same ledger state it was applied in before.
  --
  -- Since a block can only be applied to a single, specific, ledger state,
  -- if we apply a previously applied block again it will be applied in the
  -- very same ledger state, and therefore can't possibly fail.
  --
  -- It is worth noting that since we already know that the block is valid in
  -- the provided ledger state, the ledger layer should not perform /any/
  -- validation checks. Thus this function will call the ledger rules with
  -- 'ValidateNone' policy.
  reapplyBlockLedgerResult ::
    (Monad m, HasCallStack, BlockSupportsLedgerHD m blk) =>
    ComputeLedgerEvents ->
    LedgerCfg l blk ->
    blk ->
    Handle (Ticked l) m blk ->
    m (LedgerResult blk (Handle l m blk))

defaultApplyBlockLedgerResult ::
  (Monad m, HasCallStack, ApplyBlock l blk, BlockSupportsLedgerHD m blk) =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  Handle (Ticked l) m blk ->
  ExceptT (LedgerErr l blk) m (LedgerResult blk (Handle l m blk))
defaultApplyBlockLedgerResult =
  applyBlockLedgerResultWithValidation STS.ValidateAll

defaultReapplyBlockLedgerResult ::
  (Monad m, HasCallStack, ApplyBlock l blk, BlockSupportsLedgerHD m blk) =>
  (LedgerErr l blk -> LedgerResult blk (Handle l m blk)) ->
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  Handle (Ticked l) m blk ->
  m (LedgerResult blk (Handle l m blk))
defaultReapplyBlockLedgerResult throwReapplyError =
  ( fmap (either throwReapplyError id)
      . runExceptT
  )
    ...: applyBlockLedgerResultWithValidation STS.ValidateNone

-- | Interaction with the ledger layer.
--
-- The 'Eq', 'Show' and 'NoThunks' superclasses bundle the data-shape
-- contract that concrete ledger states are expected to provide. They live
-- on 'UpdateLedger' (rather than on 'IsLedger') because the latter has
-- multiple inhabitants per block ('LedgerState', 'Ticked' 'LedgerState',
-- 'ExtLedgerState', 'Ticked' 'ExtLedgerState'), not all of which need
-- those instances.
class
  ( ApplyBlock LedgerState blk
  , Eq (LedgerState blk)
  , Show (LedgerState blk)
  , NoThunks (LedgerState blk)
  ) =>
  UpdateLedger blk

{-------------------------------------------------------------------------------
  Derived functionality
-------------------------------------------------------------------------------}

-- | 'lrResult' after 'applyBlockLedgerResult'
applyLedgerBlock ::
  (Monad m, ApplyBlock l blk, HasCallStack, BlockSupportsLedgerHD m blk) =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  Handle (Ticked l) m blk ->
  ExceptT (LedgerErr l blk) m (Handle l m blk)
applyLedgerBlock = fmap lrResult ...: applyBlockLedgerResult

-- | 'lrResult' after 'reapplyBlockLedgerResult'
reapplyLedgerBlock ::
  (ApplyBlock l blk, HasCallStack, Monad m, BlockSupportsLedgerHD m blk) =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  Handle (Ticked l) m blk ->
  m (Handle l m blk)
reapplyLedgerBlock = fmap lrResult ...: reapplyBlockLedgerResult

tickThenApplyLedgerResult ::
  (BlockSupportsLedgerHD m blk, ApplyBlock l blk, MonadThrow m) =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  Handle l m blk ->
  ExceptT (LedgerErr l blk) m (LedgerResult blk (Handle l m blk))
tickThenApplyLedgerResult evs cfg blk l = do
  lrTick <- lift $ applyChainTickLedgerResult evs cfg (blockSlot blk) l
  lrBlock <-
    applyBlockLedgerResult
      evs
      cfg
      blk
      (lrResult lrTick)
  pure
    LedgerResult
      { lrEvents = lrEvents lrTick <> lrEvents lrBlock
      , lrResult = lrResult lrBlock
      }

tickThenReapplyLedgerResult ::
  (BlockSupportsLedgerHD m blk, ApplyBlock l blk, MonadThrow m) =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  Handle l m blk ->
  m (LedgerResult blk (Handle l m blk))
tickThenReapplyLedgerResult evs cfg blk l = do
  lrTick <- applyChainTickLedgerResult evs cfg (blockSlot blk) l
  lrBlock <-
    reapplyBlockLedgerResult
      evs
      cfg
      blk
      (lrResult lrTick)
  pure
    LedgerResult
      { lrEvents = lrEvents lrTick <> lrEvents lrBlock
      , lrResult = lrResult lrBlock
      }

tickThenApply ::
  (BlockSupportsLedgerHD m blk, ApplyBlock l blk, MonadThrow m) =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  Handle l m blk ->
  ExceptT (LedgerErr l blk) m (Handle l m blk)
tickThenApply = fmap lrResult ...: tickThenApplyLedgerResult

tickThenReapply ::
  (BlockSupportsLedgerHD m blk, ApplyBlock l blk, MonadThrow m) =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  Handle l m blk ->
  m (Handle l m blk)
tickThenReapply = fmap lrResult ...: tickThenReapplyLedgerResult

foldLedger ::
  (BlockSupportsLedgerHD m blk, ApplyBlock l blk, MonadThrow m) =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  [blk] ->
  Handle l m blk ->
  ExceptT (LedgerErr l blk) m (Handle l m blk)
foldLedger evs cfg =
  repeatedlyM (tickThenApply evs cfg)

refoldLedger ::
  (BlockSupportsLedgerHD m blk, ApplyBlock l blk, MonadThrow m) =>
  ComputeLedgerEvents -> LedgerCfg l blk -> [blk] -> Handle l m blk -> m (Handle l m blk)
refoldLedger evs cfg =
  repeatedlyM (tickThenReapply evs cfg)

{-------------------------------------------------------------------------------
  Short-hand
-------------------------------------------------------------------------------}

ledgerTipPoint ::
  UpdateLedger blk =>
  LedgerState blk -> Point blk
ledgerTipPoint = getTip

ledgerTipHash ::
  UpdateLedger blk =>
  LedgerState blk -> ChainHash blk
ledgerTipHash = pointHash . ledgerTipPoint

ledgerTipSlot ::
  UpdateLedger blk =>
  LedgerState blk -> WithOrigin SlotNo
ledgerTipSlot = pointSlot . ledgerTipPoint
