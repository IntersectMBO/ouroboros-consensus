{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The mempool's transactions re-applied on the ledger state a block will
-- extend, at the slot it will be forged in.
--
-- A block producer prepares a view ahead of a slot it leads, so that forging
-- splits the view instead of re-applying the mempool. The view only reads the
-- mempool: transactions that do not apply at the slot are left out of it, but
-- stay in the mempool.
module Ouroboros.Consensus.Mempool.ForgeView
  ( ForgeView
  , forgeView
  , extendView
  , forgeViewIsFull
  , forgeViewLastTicket
  , forgeViewPartition
  , forgeViewPoint
  , forgeViewSize
  , forgeViewSlot
  ) where

import qualified Data.Foldable as Foldable
import qualified Data.Measure as Measure
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.Block (Point, SlotNo, castPoint)
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables.Utils (rawPrependDiffs)
import Ouroboros.Consensus.Mempool.API
  ( MempoolMeasure (..)
  , TicketNo
  , ValidatedTxWithDiffs (..)
  , zeroTicketNo
  )
import Ouroboros.Consensus.Mempool.Capacity (MempoolSize (..))
import Ouroboros.Consensus.Mempool.Impl.Common (partitionTxSeq)
import Ouroboros.Consensus.Mempool.TxSeq (TxSeq, TxTicket (..))
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Storage.LedgerDB.Forker (ReadOnlyForker (..))

data ForgeView blk = ForgeView
  { fvConfig :: !(LedgerConfig blk)
  , fvSlot :: !SlotNo
  , fvLedger :: !(TickedLedgerState blk DiffMK)
  -- ^ The ledger state ticked to the slot, with the view's transactions
  -- applied.
  , fvBlockCapacity :: !(TxMeasure blk)
  -- ^ What the block itself can carry.
  , fvEbCapacity :: !(TxEbMeasure blk)
  -- ^ What the endorser block it announces can carry.
  , fvTxs :: !(TxSeq (MempoolMeasure blk) (Validated (GenTx blk)))
  , fvFill :: !(Fill blk)
  , fvLastTicket :: !TicketNo
  -- ^ The last mempool transaction the view considered.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "ForgeView" (ForgeView blk)

-- | How far the view's transactions fill the block and the endorser block, the
-- way 'partitionTxSeq' splits them: the block takes the longest prefix that
-- fits it, and the endorser block the longest prefix of the rest.
data Fill blk
  = InBlock !(TxMeasure blk)
  | InEb !(TxEbMeasure blk)
  | Full

-- | An empty view for forging at a slot on a ledger state ticked to it, for a
-- block that can carry this much itself: nothing if it certifies an endorser
-- block.
forgeView ::
  TxLimits blk =>
  LedgerConfig blk ->
  SlotNo ->
  TxMeasure blk ->
  TickedLedgerState blk DiffMK ->
  ForgeView blk
forgeView cfg slot blockCapacity st =
  ForgeView
    { fvConfig = cfg
    , fvSlot = slot
    , fvLedger = st
    , fvBlockCapacity = blockCapacity
    , fvEbCapacity = ebCapacityTxMeasure cfg st
    , fvTxs = TxSeq.Empty
    , fvFill = InBlock Measure.zero
    , fvLastTicket = zeroTicketNo
    }

-- | Re-apply, in order, the given transactions the view has not considered
-- yet, leaving out those that do not apply. It re-applies only those that
-- would still fit if all of them applied, so never more than the view needs.
extendView ::
  forall m blk.
  (Monad m, LedgerSupportsMempool blk) =>
  -- | Reads the transactions' inputs on the ledger state the view is on.
  ReadOnlyForker m LedgerState blk ->
  ForgeView blk ->
  [TxTicket (MempoolMeasure blk) (ValidatedTxWithDiffs blk)] ->
  m (ForgeView blk)
extendView frk view@ForgeView{fvConfig, fvSlot, fvLedger, fvTxs, fvFill, fvLastTicket} txs =
  case fitting fvFill [t | t@(TxTicket _ tn _) <- txs, tn > fvLastTicket] of
    ([], fill) -> pure view{fvFill = fill}
    (new, _) -> do
      let inputs = [(validatedTx tx, validatedTxDiffs tx, (tn, tz)) | TxTicket tx tn tz <- new]
          keys = Foldable.foldMap' (\(tx, _, _) -> getTransactionKeySets (txForgetValidated tx)) inputs
      values <- roforkerReadTables frk keys
      let ReapplyTxsResult _ applied st' =
            reapplyTxs @blk @Collect fvConfig fvSlot inputs (applyMempoolDiffs values keys fvLedger)
          diffs = Foldable.foldl' rawPrependDiffs (DiffMK mempty) [d | (_, LedgerTables d, _) <- applied]
      pure
        view
          { fvLedger =
              st' `withLedgerTables` ltliftA2 rawPrependDiffs (projectLedgerTables fvLedger) (LedgerTables diffs)
          , fvTxs = Foldable.foldl' (TxSeq.:>) fvTxs [TxTicket tx tn tz | (tx, _, (tn, tz)) <- applied]
          , fvFill = Foldable.foldl' (\fill (_, _, (_, tz)) -> maybe Full id (fillWith tz fill)) fvFill applied
          , fvLastTicket = txTicketNo (last new)
          }
 where
  -- The longest prefix that fits, as if all of it applied, and the fill
  -- after it; 'Full' if the first transaction left out does not fit.
  fitting fill = \case
    [] -> ([], fill)
    t@(TxTicket _ _ tz) : ts -> case fillWith tz fill of
      Nothing -> ([], Full)
      Just fill' -> let (more, fill'') = fitting fill' ts in (t : more, fill'')

  fillWith tz = \case
    InBlock used
      | let used' = used `Measure.plus` mmTxMeasure tz
      , used' Measure.<= fvBlockCapacity view ->
          Just (InBlock used')
      | otherwise -> fillWith tz (InEb Measure.zero)
    InEb used
      | let used' = used `Measure.plus` mmTxEbMeasure tz
      , used' Measure.<= fvEbCapacity view ->
          Just (InEb used')
      | otherwise -> Nothing
    Full -> Nothing

-- | Split the view into the transactions for the block and for the endorser
-- block it announces, with their measures.
forgeViewPartition ::
  TxLimits blk =>
  ForgeView blk ->
  ( [Validated (GenTx blk)]
  , MempoolMeasure blk
  , [Validated (GenTx blk)]
  , MempoolMeasure blk
  )
forgeViewPartition ForgeView{fvBlockCapacity, fvEbCapacity, fvTxs} =
  (toTxs inBlock, TxSeq.toSize inBlock, toTxs inEb, TxSeq.toSize inEb)
 where
  (inBlock, inEb, _) = partitionTxSeq Nothing fvBlockCapacity fvEbCapacity fvTxs
  toTxs = map txTicketTx . TxSeq.toList

forgeViewIsFull :: ForgeView blk -> Bool
forgeViewIsFull view = case fvFill view of
  Full -> True
  _ -> False

forgeViewLastTicket :: ForgeView blk -> TicketNo
forgeViewLastTicket = fvLastTicket

-- | The point of the ledger state the view is on.
forgeViewPoint :: GetTip (TickedLedgerState blk) => ForgeView blk -> Point blk
forgeViewPoint = castPoint . getTip . fvLedger

forgeViewSlot :: ForgeView blk -> SlotNo
forgeViewSlot = fvSlot

forgeViewSize :: TxLimits blk => ForgeView blk -> MempoolSize
forgeViewSize ForgeView{fvTxs} =
  MempoolSize
    { msNumTxs = fromIntegral (length fvTxs)
    , msNumBytes = txMeasureByteSize (mmTxMeasure (TxSeq.toSize fvTxs))
    }
