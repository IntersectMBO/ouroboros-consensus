{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Interface to the ledger layer
--
-- This module defines how to apply blocks to a ledger state, and re-exports
-- (from "Ouroboros.Consensus.Ledger.Basics") how to tick ledger states. These
-- are the two main operations we can do with a 'LedgerState'.
module Ouroboros.Consensus.Ledger.Abstract (
    -- * Type-level validation marker
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

import           Control.Monad.Except
import qualified Control.State.Transition.Extended as STS
import           Data.Kind (Type)
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util

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

class ( IsLedger l
      , HeaderHash l ~ HeaderHash blk
      , HasHeader blk
      , HasHeader (Header blk)
      ) => ApplyBlock l blk where

  -- | Apply a block to the ledger state.
  --
  -- This is passed the ledger state ticked to the slot of the given block, so
  -- 'applyChainTickLedgerResult' has already been called.
  --
  -- Users of this function can set any validation level allowed by the
  -- @small-steps@ package. See "Control.State.Transition.Extended".
  applyBlockLedgerResultWithValidation ::
       HasCallStack
    => STS.ValidationPolicy
    -> ComputeLedgerEvents
    -> LedgerCfg l
    -> blk
    -> Ticked l
    -> Except (LedgerErr l) (LedgerResult l l)

  -- | Apply a block to the ledger state.
  --
  -- This is passed the ledger state ticked to the slot of the given block, so
  -- 'applyChainTickLedgerResult' has already been called.
  --
  -- This function will use 'ValidateAll' policy for calling the ledger rules.
  applyBlockLedgerResult ::
       HasCallStack
    => ComputeLedgerEvents
    -> LedgerCfg l
    -> blk
    -> Ticked l
    -> Except (LedgerErr l) (LedgerResult l l)

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
       HasCallStack
    => ComputeLedgerEvents
    -> LedgerCfg l
    -> blk
    -> Ticked l
    -> LedgerResult l l

defaultApplyBlockLedgerResult ::
     (HasCallStack, ApplyBlock l blk)
  => ComputeLedgerEvents
  -> LedgerCfg l
  -> blk
  -> Ticked l
  -> Except (LedgerErr l) (LedgerResult l l)
defaultApplyBlockLedgerResult =
  applyBlockLedgerResultWithValidation STS.ValidateAll

defaultReapplyBlockLedgerResult ::
     (HasCallStack, ApplyBlock l blk)
  => (LedgerErr l -> LedgerResult l l)
  -> ComputeLedgerEvents
  -> LedgerCfg l
  -> blk
  -> Ticked l
  -> (LedgerResult l l)
defaultReapplyBlockLedgerResult throwReapplyError =
       (either throwReapplyError id . runExcept)
  ...: applyBlockLedgerResultWithValidation STS.ValidateNone

-- | Interaction with the ledger layer
class ApplyBlock (LedgerState blk) blk => UpdateLedger blk

{-------------------------------------------------------------------------------
  Derived functionality
-------------------------------------------------------------------------------}

-- | 'lrResult' after 'applyBlockLedgerResult'
applyLedgerBlock ::
     forall l blk.
     (ApplyBlock l blk, HasCallStack)
  => ComputeLedgerEvents
  -> LedgerCfg l
  -> blk
  -> Ticked l
  -> Except (LedgerErr l) l
applyLedgerBlock = fmap lrResult ...: applyBlockLedgerResult

-- | 'lrResult' after 'reapplyBlockLedgerResult'
reapplyLedgerBlock ::
     forall l blk.
     (ApplyBlock l blk, HasCallStack)
  => ComputeLedgerEvents
  -> LedgerCfg l
  -> blk
  -> Ticked l
  -> l
reapplyLedgerBlock =
  lrResult ...: reapplyBlockLedgerResult

tickThenApplyLedgerResult ::
     ApplyBlock l blk
  => ComputeLedgerEvents
  -> LedgerCfg l
  -> blk
  -> l
  -> Except (LedgerErr l) (LedgerResult l l)
tickThenApplyLedgerResult opts cfg blk l = do
  let lrTick = applyChainTickLedgerResult opts cfg (blockSlot blk) l
  lrBlock <-   applyBlockLedgerResult     opts cfg            blk  (lrResult lrTick)
  pure LedgerResult {
      lrEvents = lrEvents lrTick <> lrEvents lrBlock
    , lrResult = lrResult lrBlock
    }

tickThenReapplyLedgerResult ::
     forall l blk.
     ApplyBlock l blk
  => ComputeLedgerEvents
  -> LedgerCfg l
  -> blk
  -> l
  -> LedgerResult l l
tickThenReapplyLedgerResult evs cfg blk l =
  let lrTick  = applyChainTickLedgerResult evs cfg (blockSlot blk) l
      lrBlock = reapplyBlockLedgerResult   evs cfg            blk (lrResult lrTick)
  in LedgerResult {
      lrEvents = lrEvents lrTick <> lrEvents lrBlock
    , lrResult = lrResult lrBlock
    }

tickThenApply ::
     forall l blk.
     ApplyBlock l blk
  => ComputeLedgerEvents
  -> LedgerCfg l
  -> blk
  -> l
  -> Except (LedgerErr l) l
tickThenApply = fmap lrResult ...: tickThenApplyLedgerResult

tickThenReapply ::
     forall l blk.
     ApplyBlock l blk
  => ComputeLedgerEvents
  -> LedgerCfg l
  -> blk
  -> l
  -> l
tickThenReapply = lrResult ...: tickThenReapplyLedgerResult

foldLedger ::
     ApplyBlock l blk
  => ComputeLedgerEvents -> LedgerCfg l -> [blk] -> l -> Except (LedgerErr l) l
foldLedger = repeatedlyM .: tickThenApply

refoldLedger ::
     ApplyBlock l blk
  => ComputeLedgerEvents -> LedgerCfg l -> [blk] -> l -> l
refoldLedger = repeatedly .: tickThenReapply

{-------------------------------------------------------------------------------
  Short-hand
-------------------------------------------------------------------------------}

ledgerTipPoint ::
     UpdateLedger blk
  => LedgerState blk -> Point blk
ledgerTipPoint = castPoint . getTip

ledgerTipHash ::
     UpdateLedger blk
  => LedgerState blk -> ChainHash blk
ledgerTipHash = pointHash . ledgerTipPoint

ledgerTipSlot ::
     UpdateLedger blk
  => LedgerState blk -> WithOrigin SlotNo
ledgerTipSlot = pointSlot . ledgerTipPoint
