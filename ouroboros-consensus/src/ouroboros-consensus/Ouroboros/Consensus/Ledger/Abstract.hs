{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

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
  , UpdateLedger
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
import           Data.Kind (Type)
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util (repeatedly, repeatedlyM)

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
      , HasLedgerTables l
      , HasLedgerTables (Ticked1 l)
      ) => ApplyBlock l blk where

  -- | Apply a block to the ledger state.
  --
  -- This is passed the ledger state ticked to the slot of the given block, so
  -- 'applyChainTickLedgerResult' has already been called.
  applyBlockLedgerResult ::
       HasCallStack
    => LedgerCfg l
    -> blk
    -> Ticked1 l ValuesMK
    -> Except (LedgerErr l) (LedgerResult l (l DiffMK))

  -- | Re-apply a block to the very same ledger state it was applied in before.
  --
  -- Since a block can only be applied to a single, specific, ledger state,
  -- if we apply a previously applied block again it will be applied in the
  -- very same ledger state, and therefore can't possibly fail.
  --
  -- It is worth noting that since we already know that the block is valid in
  -- the provided ledger state, the ledger layer should not perform /any/
  -- validation checks.
  reapplyBlockLedgerResult ::
       HasCallStack
    => LedgerCfg l
    -> blk
    -> Ticked1 l ValuesMK
    -> LedgerResult l (l DiffMK)

  -- | Given a block, get the key-sets that we need to apply it to a ledger
  -- state.
  getBlockKeySets :: blk -> LedgerTables l KeysMK

-- | Interaction with the ledger layer
class ApplyBlock (LedgerState blk) blk => UpdateLedger blk

{-------------------------------------------------------------------------------
  Derived functionality
-------------------------------------------------------------------------------}

-- | 'lrResult' after 'applyBlockLedgerResult'
applyLedgerBlock ::
     (ApplyBlock l blk, HasCallStack)
  => LedgerCfg l
  -> blk
  -> Ticked1 l ValuesMK
  -> Except (LedgerErr l) (l DiffMK)
applyLedgerBlock = fmap lrResult ..: applyBlockLedgerResult

-- | 'lrResult' after 'reapplyBlockLedgerResult'
reapplyLedgerBlock ::
     (ApplyBlock l blk, HasCallStack)
  => LedgerCfg l
  -> blk
  -> Ticked1 l ValuesMK
  -> l DiffMK
reapplyLedgerBlock = lrResult ..: reapplyBlockLedgerResult

tickThenApplyLedgerResult ::
     ApplyBlock l blk
  => LedgerCfg l
  -> blk
  -> l ValuesMK
  -> Except (LedgerErr l) (LedgerResult l (l DiffMK))
tickThenApplyLedgerResult cfg blk l = do
  let lrTick = applyChainTickLedgerResult cfg (blockSlot blk) (forgetLedgerTables l)
  lrBlock <-   applyBlockLedgerResult     cfg            blk  (applyDiffForKeys l (getBlockKeySets blk) (lrResult lrTick))
  case prependDiffs (lrResult lrTick) (lrResult lrBlock) of
    Nothing     -> error "Critical error! ledger rules produced diffs that do not satisfy the UTxO property!"
    Just result -> pure LedgerResult {
        lrEvents = lrEvents lrTick <> lrEvents lrBlock
      , lrResult = result
      }

tickThenReapplyLedgerResult ::
     ApplyBlock l blk
  => LedgerCfg l
  -> blk
  -> l ValuesMK
  -> LedgerResult l (l DiffMK)
tickThenReapplyLedgerResult cfg blk l =
  let lrTick  = applyChainTickLedgerResult cfg (blockSlot blk) (forgetLedgerTables l)
      lrBlock = reapplyBlockLedgerResult   cfg            blk  (applyDiffForKeys l (getBlockKeySets blk) (lrResult lrTick))
  in case prependDiffs (lrResult lrTick) (lrResult lrBlock) of
    Nothing     -> error "Critical error! ledger rules produced diffs that do not satisfy the UTxO property!"
    Just result -> LedgerResult {
        lrEvents = lrEvents lrTick <> lrEvents lrBlock
      , lrResult = result
      }

tickThenApply ::
     ApplyBlock l blk
  => LedgerCfg l
  -> blk
  -> l ValuesMK
  -> Except (LedgerErr l) (l DiffMK)
tickThenApply = fmap lrResult ..: tickThenApplyLedgerResult

tickThenReapply ::
     ApplyBlock l blk
  => LedgerCfg l
  -> blk
  -> l ValuesMK
  -> l DiffMK
tickThenReapply = lrResult ..: tickThenReapplyLedgerResult

foldLedger ::
     ApplyBlock l blk
  => LedgerCfg l -> [blk] -> l ValuesMK -> Except (LedgerErr l) (l ValuesMK)
foldLedger cfg = repeatedlyM (\blk state -> applyDiffForKeys state (getBlockKeySets blk) <$> tickThenApply cfg blk state)

refoldLedger ::
     ApplyBlock l blk
  => LedgerCfg l -> [blk] -> l ValuesMK -> l ValuesMK
refoldLedger cfg = repeatedly (\blk state -> applyDiffForKeys state (getBlockKeySets blk) $ tickThenReapply cfg blk state)

{-------------------------------------------------------------------------------
  Short-hand
-------------------------------------------------------------------------------}

ledgerTipPoint ::
     UpdateLedger blk
  => LedgerState blk mk -> Point blk
ledgerTipPoint = castPoint . getTip

ledgerTipHash ::
     UpdateLedger blk
  => LedgerState blk mk -> ChainHash blk
ledgerTipHash = pointHash . ledgerTipPoint

ledgerTipSlot ::
     UpdateLedger blk
  => LedgerState blk mk -> WithOrigin SlotNo
ledgerTipSlot = pointSlot . ledgerTipPoint
