{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Interface to the ledger layer
--
-- This module defines how to apply blocks to a ledger state, and re-exports
-- (from "Ouroboros.Consensus.Ledger.Basics") how to tick ledger states. These
-- are the two main operations we can do with a 'LedgerState'.
--
-- The on-disk values flow as explicit, opaque @Values blk@ arguments (see
-- 'Ouroboros.Consensus.Ledger.Basics.BlockSupportsLedgerHD'):
--
--   * /reading/ the values a block consumes ('blockKeys') is the only monadic
--     step, and it lives on the backend axis (the storage handle), not here;
--
--   * /ticking/ is pure but may produce a @'Diff' blk@ out of nothing (era
--     transitions such as the AVVM removal at Shelley→Allegra);
--
--   * /applying/ a block is pure: given the (read + forwarded) @Values blk@ and
--     the ticked state, it yields the new state and the block's @'Diff' blk@.
module Ouroboros.Consensus.Ledger.Abstract
  ( -- * Type-level validation marker
    Validated

    -- * Apply block
  , ApplyBlock (..)
  , ComputeLedgerEvents (..)
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
import qualified Control.State.Transition.Extended as STS
import Data.Kind (Type)
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ticked
import Ouroboros.Consensus.Util

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
  , HeaderHash (l blk) ~ HeaderHash blk
  , HasHeader blk
  , HasHeader (Header blk)
  ) =>
  ApplyBlock l blk
  where
  -- | Apply a block to the ledger state.
  --
  -- This is passed the ledger state ticked to the slot of the given block, so
  -- 'applyChainTickLedgerResult' has already been called, together with the
  -- @Values blk@ the block consumes. Those values must already be read from the
  -- backend and forwarded through the tick's diff (see
  -- 'tickThenApplyLedgerResult'). The application is pure; it returns the new
  -- state and the @'Diff' blk@ the block produces.
  --
  -- Users of this function can set any validation level allowed by the
  -- @small-steps@ package. See "Control.State.Transition.Extended".
  applyBlockLedgerResultWithValidation ::
    HasCallStack =>
    STS.ValidationPolicy ->
    ComputeLedgerEvents ->
    LedgerCfg l blk ->
    blk ->
    Values blk ->
    Ticked l blk ->
    Except (LedgerErr l blk) (LedgerResult blk (l blk, BlockDiff blk))

  -- | Apply a block to the ledger state.
  --
  -- Like 'applyBlockLedgerResultWithValidation', but using the 'STS.ValidateAll'
  -- policy.
  applyBlockLedgerResult ::
    HasCallStack =>
    ComputeLedgerEvents ->
    LedgerCfg l blk ->
    blk ->
    Values blk ->
    Ticked l blk ->
    Except (LedgerErr l blk) (LedgerResult blk (l blk, BlockDiff blk))

  -- | Re-apply a block to the very same ledger state it was applied in before.
  --
  -- Since a block can only be applied to a single, specific, ledger state,
  -- if we apply a previously applied block again it will be applied in the
  -- very same ledger state, and therefore can't possibly fail.
  --
  -- It is worth noting that since we already know that the block is valid in
  -- the provided ledger state, the ledger layer should not perform /any/
  -- validation checks. Thus this function will call the ledger rules with
  -- 'STS.ValidateNone' policy.
  reapplyBlockLedgerResult ::
    HasCallStack =>
    ComputeLedgerEvents ->
    LedgerCfg l blk ->
    blk ->
    Values blk ->
    Ticked l blk ->
    LedgerResult blk (l blk, BlockDiff blk)

defaultApplyBlockLedgerResult ::
  (HasCallStack, ApplyBlock l blk) =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  Values blk ->
  Ticked l blk ->
  Except (LedgerErr l blk) (LedgerResult blk (l blk, BlockDiff blk))
defaultApplyBlockLedgerResult =
  applyBlockLedgerResultWithValidation STS.ValidateAll

defaultReapplyBlockLedgerResult ::
  (HasCallStack, ApplyBlock l blk) =>
  (LedgerErr l blk -> LedgerResult blk (l blk, BlockDiff blk)) ->
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  Values blk ->
  Ticked l blk ->
  LedgerResult blk (l blk, BlockDiff blk)
defaultReapplyBlockLedgerResult throwReapplyError evs cfg blk vals ticked =
  either throwReapplyError id . runExcept $
    applyBlockLedgerResultWithValidation STS.ValidateNone evs cfg blk vals ticked

-- | Interaction with the ledger layer
class ApplyBlock LedgerState blk => UpdateLedger blk

{-------------------------------------------------------------------------------
  Derived functionality
-------------------------------------------------------------------------------}

-- | 'lrResult' after 'applyBlockLedgerResult'
applyLedgerBlock ::
  (ApplyBlock l blk, HasCallStack) =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  Values blk ->
  Ticked l blk ->
  Except (LedgerErr l blk) (l blk, BlockDiff blk)
applyLedgerBlock = fmap lrResult ....: applyBlockLedgerResult

-- | 'lrResult' after 'reapplyBlockLedgerResult'
reapplyLedgerBlock ::
  (ApplyBlock l blk, HasCallStack) =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  Values blk ->
  Ticked l blk ->
  (l blk, BlockDiff blk)
reapplyLedgerBlock = lrResult ....: reapplyBlockLedgerResult

tickThenApplyLedgerResult ::
  forall l blk.
  ApplyBlock l blk =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  -- | The values the block consumes, read against the (pre-tick) state.
  Values blk ->
  l blk ->
  Except (LedgerErr l blk) (LedgerResult blk (l blk, TickAndBlockDiff blk))
tickThenApplyLedgerResult evs cfg blk vals l = do
  let lrTick = applyChainTickLedgerResult evs cfg (blockSlot blk) l
      (tickedSt, tickDiff) = lrResult lrTick
      -- The values were read against the pre-tick state; ticking may have
      -- removed some of them (e.g. AVVM addresses), so forward them through the
      -- tick's diff before applying the block.
      vals' = forwardTickDiff @blk tickDiff vals
  lrBlock <- applyBlockLedgerResult evs cfg blk vals' tickedSt
  let (st', blockDiff) = lrResult lrBlock
  pure
    LedgerResult
      { lrEvents = lrEvents lrTick <> lrEvents lrBlock
      , lrResult = (st', combineTickAndBlockDiff @blk tickDiff blockDiff)
      }

tickThenReapplyLedgerResult ::
  forall l blk.
  ApplyBlock l blk =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  Values blk ->
  l blk ->
  LedgerResult blk (l blk, TickAndBlockDiff blk)
tickThenReapplyLedgerResult evs cfg blk vals l =
  let lrTick = applyChainTickLedgerResult evs cfg (blockSlot blk) l
      (tickedSt, tickDiff) = lrResult lrTick
      vals' = forwardTickDiff @blk tickDiff vals
      lrBlock = reapplyBlockLedgerResult evs cfg blk vals' tickedSt
      (st', blockDiff) = lrResult lrBlock
   in LedgerResult
        { lrEvents = lrEvents lrTick <> lrEvents lrBlock
        , lrResult = (st', combineTickAndBlockDiff @blk tickDiff blockDiff)
        }

tickThenApply ::
  ApplyBlock l blk =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  Values blk ->
  l blk ->
  Except (LedgerErr l blk) (l blk, TickAndBlockDiff blk)
tickThenApply = fmap lrResult ....: tickThenApplyLedgerResult

tickThenReapply ::
  ApplyBlock l blk =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  Values blk ->
  l blk ->
  (l blk, TickAndBlockDiff blk)
tickThenReapply = lrResult ....: tickThenReapplyLedgerResult

-- | Apply a sequence of blocks to a full, in-memory @(state, values)@ pair.
--
-- This is the InMemory model of block application: the @Values blk@ holds the
-- entire table(s), and each block's total diff is folded back into them via
-- 'forward'. With on-disk backends the values come from the storage handle
-- instead, and this fold is not used.
foldLedger ::
  forall l blk.
  ApplyBlock l blk =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  [blk] ->
  (l blk, Values blk) ->
  Except (LedgerErr l blk) (l blk, Values blk)
foldLedger evs cfg =
  repeatedlyM $ \blk (st, vals) -> do
    (st', diff) <- tickThenApply evs cfg blk vals st
    pure (st', forwardTickAndBlockDiff @blk diff vals)

refoldLedger ::
  forall l blk.
  ApplyBlock l blk =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  [blk] ->
  (l blk, Values blk) ->
  (l blk, Values blk)
refoldLedger evs cfg =
  repeatedly $ \blk (st, vals) ->
    let (st', diff) = tickThenReapply evs cfg blk vals st
     in (st', forwardTickAndBlockDiff @blk diff vals)

{-------------------------------------------------------------------------------
  Short-hand
-------------------------------------------------------------------------------}

ledgerTipPoint ::
  UpdateLedger blk =>
  LedgerState blk -> Point blk
ledgerTipPoint = castPoint . getTip

ledgerTipHash ::
  UpdateLedger blk =>
  LedgerState blk -> ChainHash blk
ledgerTipHash = pointHash . ledgerTipPoint

ledgerTipSlot ::
  UpdateLedger blk =>
  LedgerState blk -> WithOrigin SlotNo
ledgerTipSlot = pointSlot . ledgerTipPoint
