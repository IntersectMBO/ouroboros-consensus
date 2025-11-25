{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeApplications #-}

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
  , GetBlockKeySets (..)
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
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Ticked
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Ledger.LedgerStateType

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
  ( IsLedger (l blk)
  , HeaderHash (l blk) ~ HeaderHash blk
  , HasHeader blk
  , HasHeader (Header blk)
  , HasLedgerTables (l blk)
  , HasLedgerTables (Ticked (l blk))
  , LBlock (l blk) ~ blk
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
    HasCallStack =>
    STS.ValidationPolicy ->
    ComputeLedgerEvents ->
    LedgerCfg (l blk) ->
    blk ->
    Ticked (l blk) ValuesMK ->
    Except (LedgerErr (l blk)) (LedgerResult (l blk) (l blk DiffMK))

  -- | Apply a block to the ledger state.
  --
  -- This is passed the ledger state ticked to the slot of the given block, so
  -- 'applyChainTickLedgerResult' has already been called.
  --
  -- This function will use 'ValidateAll' policy for calling the ledger rules.
  applyBlockLedgerResult ::
    HasCallStack =>
    ComputeLedgerEvents ->
    LedgerCfg (l blk) ->
    blk ->
    Ticked (l blk) ValuesMK ->
    Except (LedgerErr (l blk)) (LedgerResult (l blk) (l blk DiffMK))

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
    HasCallStack =>
    ComputeLedgerEvents ->
    LedgerCfg (l blk) ->
    blk ->
    Ticked (l blk) ValuesMK ->
    LedgerResult (l blk) (l blk DiffMK)

class GetBlockKeySets blk where
  -- | Given a block, get the key-sets that we need to apply it to a ledger
  -- state.
  getBlockKeySets :: blk -> LedgerTables blk KeysMK

defaultApplyBlockLedgerResult ::
  (HasCallStack, ApplyBlock l blk) =>
  ComputeLedgerEvents ->
  LedgerCfg (l blk) ->
  blk ->
  Ticked (l blk) ValuesMK ->
  Except (LedgerErr (l blk)) (LedgerResult (l blk) (l blk DiffMK))
defaultApplyBlockLedgerResult =
  applyBlockLedgerResultWithValidation STS.ValidateAll

defaultReapplyBlockLedgerResult ::
  (HasCallStack, ApplyBlock l blk) =>
  (LedgerErr (l blk)-> LedgerResult (l blk) (l blk DiffMK)) ->
  ComputeLedgerEvents ->
  LedgerCfg (l blk) ->
  blk ->
  Ticked (l blk) ValuesMK ->
  LedgerResult (l blk) (l blk DiffMK)
defaultReapplyBlockLedgerResult throwReapplyError =
  (either throwReapplyError id . runExcept)
    ...: applyBlockLedgerResultWithValidation STS.ValidateNone

-- | Interaction with the ledger layer
class ApplyBlock LedgerState blk => UpdateLedger blk

{-------------------------------------------------------------------------------
  Derived functionality
-------------------------------------------------------------------------------}

-- | 'lrResult' after 'applyBlockLedgerResult'
applyLedgerBlock ::
  forall l blk.
  (ApplyBlock l blk, HasCallStack) =>
  ComputeLedgerEvents ->
  LedgerCfg (l blk) ->
  blk ->
  Ticked (l blk) ValuesMK ->
  Except (LedgerErr (l blk)) (l blk DiffMK)
applyLedgerBlock = fmap lrResult ...: applyBlockLedgerResult

-- | 'lrResult' after 'reapplyBlockLedgerResult'
reapplyLedgerBlock ::
  forall l blk.
  (ApplyBlock l blk, HasCallStack) =>
  ComputeLedgerEvents ->
  LedgerCfg (l blk) ->
  blk ->
  Ticked (l blk) ValuesMK ->
  l blk DiffMK
reapplyLedgerBlock = lrResult ...: reapplyBlockLedgerResult

tickThenApplyLedgerResult ::
  forall (l :: Type -> LedgerStateKind) blk. (ApplyBlock l blk, GetBlockKeySets blk) =>
  ComputeLedgerEvents ->
  LedgerCfg (l blk) ->
  blk ->
  l blk ValuesMK ->
  Except (LedgerErr (l blk)) (LedgerResult (l blk) (l blk DiffMK))
tickThenApplyLedgerResult evs cfg blk l = do
  let lrTick = applyChainTickLedgerResult evs cfg (blockSlot blk) (forgetLedgerTables l)
  lrBlock <-
    applyBlockLedgerResult
      evs
      cfg
      blk
      (applyDiffForKeys l (getBlockKeySets blk) (lrResult lrTick))
  pure
    LedgerResult
      { lrEvents = lrEvents lrTick <> lrEvents lrBlock
      , lrResult = prependDiffs @(TickedL l) @l @blk (TickedL $ lrResult lrTick) (lrResult lrBlock)
      }

tickThenReapplyLedgerResult ::
  forall l blk.
  (ApplyBlock l blk, GetBlockKeySets blk) =>
  ComputeLedgerEvents ->
  LedgerCfg (l blk) ->
  blk ->
  l blk ValuesMK ->
  LedgerResult (l blk) (l blk DiffMK)
tickThenReapplyLedgerResult evs cfg blk l =
  let lrTick = applyChainTickLedgerResult evs cfg (blockSlot blk) (forgetLedgerTables l)
      lrBlock =
        reapplyBlockLedgerResult
          evs
          cfg
          blk
          (applyDiffForKeys l (getBlockKeySets blk) (lrResult lrTick))
   in LedgerResult
        { lrEvents = lrEvents lrTick <> lrEvents lrBlock
        , lrResult = prependDiffs @(TickedL l) @l @blk (TickedL $ lrResult lrTick) (lrResult lrBlock)
        }

tickThenApply ::
  forall l blk.
  (ApplyBlock l blk, GetBlockKeySets blk) =>
  ComputeLedgerEvents ->
  LedgerCfg (l blk) ->
  blk ->
  l blk ValuesMK ->
  Except (LedgerErr (l blk)) (l blk DiffMK)
tickThenApply = fmap lrResult ...: tickThenApplyLedgerResult

tickThenReapply ::
  forall l blk.
  (ApplyBlock l blk, GetBlockKeySets blk) =>
  ComputeLedgerEvents ->
  LedgerCfg (l blk) ->
  blk ->
  l blk ValuesMK ->
  l blk DiffMK
tickThenReapply = lrResult ...: tickThenReapplyLedgerResult

foldLedger ::
  (ApplyBlock l blk, GetBlockKeySets blk) =>
  ComputeLedgerEvents -> LedgerCfg (l blk) -> [blk] -> l blk ValuesMK -> Except (LedgerErr (l blk)) (l blk ValuesMK)
foldLedger evs cfg =
  repeatedlyM
    (\blk state -> applyDiffForKeys state (getBlockKeySets blk) <$> tickThenApply evs cfg blk state)

refoldLedger ::
  (ApplyBlock l blk, GetBlockKeySets blk) =>
  ComputeLedgerEvents -> LedgerCfg (l blk) -> [blk] -> l blk ValuesMK -> l blk ValuesMK
refoldLedger evs cfg =
  repeatedly
    (\blk state -> applyDiffForKeys state (getBlockKeySets blk) $ tickThenReapply evs cfg blk state)

{-------------------------------------------------------------------------------
  Short-hand
-------------------------------------------------------------------------------}

ledgerTipPoint ::
  UpdateLedger blk =>
  LedgerState blk mk -> Point blk
ledgerTipPoint = castPoint . getTip

ledgerTipHash ::
  UpdateLedger blk =>
  LedgerState blk mk -> ChainHash blk
ledgerTipHash = pointHash . ledgerTipPoint

ledgerTipSlot ::
  UpdateLedger blk =>
  LedgerState blk mk -> WithOrigin SlotNo
ledgerTipSlot = pointSlot . ledgerTipPoint
