{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Interface to the ledger layer
--
-- This module defines how to tick a ledger state and how to apply blocks to a
-- ledger state. These are the two main operations we can do with a
-- 'LedgerState'.
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

    -- * Tick a ledger state
  , IsLedger (..)
  , applyChainTick
  , LedgerError

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
  , module Ouroboros.Consensus.Ledger.HD
  ) where

import Control.Monad.Except
import qualified Control.State.Transition.Extended as STS
import Data.Kind
import GHC.Generics
import GHC.Stack (HasCallStack)
import NoThunks.Class
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.HD
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
  Tick a ledger state
-------------------------------------------------------------------------------}

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
  , -- The block axis of UTxO-HD: provides the opaque @'TickAndBlockDiff' blk@
    -- that ticking and block application produces.
    BlockSupportsLedgerHD blk
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
  -- @'TickDiff' blk@ out of nothing: era transitions happen when ticking, and some
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
    LedgerResult blk (Ticked l blk, TickDiff blk)

-- | 'lrResult' after 'applyChainTickLedgerResult'
applyChainTick ::
  IsLedger l blk =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  SlotNo ->
  l blk ->
  (Ticked l blk, TickDiff blk)
applyChainTick = lrResult ...: applyChainTickLedgerResult

type LedgerError blk = LedgerErr LedgerState blk

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
  -- @Values blk@ the block needs. Those values must already be read from the
  -- backend and forwarded through the tick's diff (see
  -- 'tickThenApplyLedgerResult'). It returns the new state and the @'Diff' blk@
  -- the block produces.
  --
  -- Users of this function can set any validation level allowed by the
  -- @small-steps@ package. See "Control.State.Transition.Extended".
  applyBlockLedgerResultWithValidation ::
    HasCallStack =>
    STS.ValidationPolicy ->
    ComputeLedgerEvents ->
    LedgerCfg l blk ->
    blk ->
    Ticked l blk ->
    Values blk ->
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
    Ticked l blk ->
    Values blk ->
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
    Ticked l blk ->
    Values blk ->
    LedgerResult blk (l blk, BlockDiff blk)

defaultApplyBlockLedgerResult ::
  (HasCallStack, ApplyBlock l blk) =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  Ticked l blk ->
  Values blk ->
  Except (LedgerErr l blk) (LedgerResult blk (l blk, BlockDiff blk))
defaultApplyBlockLedgerResult =
  applyBlockLedgerResultWithValidation STS.ValidateAll

defaultReapplyBlockLedgerResult ::
  (HasCallStack, ApplyBlock l blk) =>
  (LedgerErr l blk -> LedgerResult blk (l blk, BlockDiff blk)) ->
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  Ticked l blk ->
  Values blk ->
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
  Ticked l blk ->
  Values blk ->
  Except (LedgerErr l blk) (l blk, BlockDiff blk)
applyLedgerBlock = fmap lrResult ....: applyBlockLedgerResult

-- | 'lrResult' after 'reapplyBlockLedgerResult'
reapplyLedgerBlock ::
  (ApplyBlock l blk, HasCallStack) =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  Ticked l blk ->
  Values blk ->
  (l blk, BlockDiff blk)
reapplyLedgerBlock = lrResult ....: reapplyBlockLedgerResult

tickThenApplyLedgerResult ::
  forall l blk.
  ApplyBlock l blk =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  l blk ->
  -- | The values the block needs, read against the (pre-tick) state.
  Values blk ->
  Except (LedgerErr l blk) (LedgerResult blk (l blk, TickAndBlockDiff blk))
tickThenApplyLedgerResult evs cfg blk l vals = do
  let lrTick = applyChainTickLedgerResult evs cfg (blockSlot blk) l
      (tickedSt, tickDiff) = lrResult lrTick
      -- The values were read against the pre-tick state; ticking may have
      -- removed some of them (e.g. AVVM addresses), so forward them through the
      -- tick's diff before applying the block.
      vals' = forwardTickDiff @blk tickDiff vals
  lrBlock <- applyBlockLedgerResult evs cfg blk tickedSt vals'
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
  l blk ->
  Values blk ->
  LedgerResult blk (l blk, TickAndBlockDiff blk)
tickThenReapplyLedgerResult evs cfg blk l vals =
  let lrTick = applyChainTickLedgerResult evs cfg (blockSlot blk) l
      (tickedSt, tickDiff) = lrResult lrTick
      vals' = forwardTickDiff @blk tickDiff vals
      lrBlock = reapplyBlockLedgerResult evs cfg blk tickedSt vals'
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
  l blk ->
  Values blk ->
  Except (LedgerErr l blk) (l blk, TickAndBlockDiff blk)
tickThenApply = fmap lrResult ....: tickThenApplyLedgerResult

tickThenReapply ::
  ApplyBlock l blk =>
  ComputeLedgerEvents ->
  LedgerCfg l blk ->
  blk ->
  l blk ->
  Values blk ->
  (l blk, TickAndBlockDiff blk)
tickThenReapply = lrResult ....: tickThenReapplyLedgerResult

-- | Apply a sequence of blocks to a full, in-memory @(state, values)@ pair.
--
-- This is the InMemory model of block application: the @Values blk@ holds the
-- values all these blocks need, and each block's total diff is folded back into
-- them via 'forward'. With on-disk backends the values come from the storage
-- handle instead, and this fold is not used.
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
    (st', diff) <- tickThenApply evs cfg blk st vals
    pure (st', forwardTickAndBlockDiff @blk diff vals)

-- | Has the same caveats as 'foldLedger'.
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
    let (st', diff) = tickThenReapply evs cfg blk st vals
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
