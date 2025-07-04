{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Operations that update the mempool. They are internally divided in the pure
-- and impure sides of the operation.
module Ouroboros.Consensus.Mempool.Update
  ( implAddTx
  , implRemoveTxsEvenIfValid
  , implSyncWithLedger
  ) where

import Cardano.Slotting.Slot
import Control.Monad.Except (runExcept)
import Control.Tracer
import qualified Data.Foldable as Foldable
import Data.Functor.Contravariant ((>$<))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Measure as Measure
import qualified Data.Set as Set
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables.Utils (emptyLedgerTables)
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Mempool.Capacity
import Ouroboros.Consensus.Mempool.Impl.Common
import Ouroboros.Consensus.Mempool.TxSeq (TxTicket (..))
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Storage.LedgerDB.Forker hiding (trace)
import Ouroboros.Consensus.Util (whenJust)
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.IOLike hiding (withMVar)
import Ouroboros.Consensus.Util.NormalForm.StrictMVar
import Ouroboros.Consensus.Util.STM
import Ouroboros.Network.Block

{-------------------------------------------------------------------------------
  Add transactions
-------------------------------------------------------------------------------}

-- | Add a single transaction to the mempool, blocking if there is no space.
implAddTx ::
  ( IOLike m
  , LedgerSupportsMempool blk
  , ValidateEnvelope blk
  , HasTxId (GenTx blk)
  ) =>
  MempoolEnv m blk ->
  -- | Whether we're acting on behalf of a remote peer or a local client.
  AddTxOnBehalfOf ->
  -- | The transaction to add to the mempool.
  GenTx blk ->
  m (MempoolAddTxResult blk)
implAddTx mpEnv onbehalf tx =
  -- To ensure fair behaviour between threads that are trying to add
  -- transactions, we make them all queue in a fifo. Only the one at the head
  -- of the queue gets to actually wait for space to get freed up in the
  -- mempool. This avoids small transactions repeatedly squeezing in ahead of
  -- larger transactions.
  --
  -- The fifo behaviour is implemented using a simple MVar. And take this
  -- MVar lock on a transaction by transaction basis. So if several threads
  -- are each trying to add several transactions, then they'll interleave at
  -- transaction granularity, not batches of transactions.
  --
  -- To add back in a bit of deliberate unfairness, we want to prioritise
  -- transactions being added on behalf of local clients, over ones being
  -- added on behalf of remote peers. We do this by using a pair of mvar
  -- fifos: remote peers must wait on both mvars, while local clients only
  -- need to wait on the second.
  case onbehalf of
    AddTxForRemotePeer ->
      withMVar remoteFifo $ \() ->
        withMVar allFifo $ \() ->
          -- This action can also block. Holding the MVars means
          -- there is only a single such thread blocking at once.
          implAddTx'
    AddTxForLocalClient ->
      withMVar allFifo $ \() ->
        -- As above but skip the first MVar fifo so we will get
        -- service sooner if there's lots of other remote
        -- threads waiting.
        implAddTx'
 where
  MempoolEnv
    { mpEnvAddTxsRemoteFifo = remoteFifo
    , mpEnvAddTxsAllFifo = allFifo
    , mpEnvTracer = trcr
    } = mpEnv

  implAddTx' = do
    TransactionProcessingResult _ result ev <-
      doAddTx
        mpEnv
        (whetherToIntervene onbehalf)
        tx
    traceWith trcr ev
    return result

  whetherToIntervene :: AddTxOnBehalfOf -> WhetherToIntervene
  whetherToIntervene AddTxForRemotePeer = DoNotIntervene
  whetherToIntervene AddTxForLocalClient = Intervene

-- | Tried to add a transaction, was it processed or is there no space left?
data TriedToAddTx blk
  = -- | Adding the next transaction would put the mempool over capacity.
    NotEnoughSpaceLeft
  | Processed (TransactionProcessed blk)

-- | The new state, if the transaction was accepted
data TransactionProcessed blk
  = TransactionProcessingResult
      -- | If the transaction was accepted, the new state that can be written to
      -- the TVar.
      (Maybe (InternalState blk))
      -- | The result of trying to add the transaction to the mempool.
      (MempoolAddTxResult blk)
      -- | The event emitted by the operation.
      (TraceEventMempool blk)

-- | This function returns whether the transaction was added or rejected, and
-- will block if the mempool is full.
--
-- This function returns whether the transaction was added or rejected, or if
-- the Mempool capacity is reached. See 'implAddTx' for a function that blocks
-- in case the Mempool capacity is reached.
--
-- Transactions are added one by one, updating the Mempool each time one was
-- added successfully.
--
-- See the necessary invariants on the Haddock for 'API.addTxs'.
--
-- NOTE when using V1 LedgerDB: This function does not sync the Mempool contents
-- with the ledger state in case the latter changes in a way that doesn't
-- invalidate the db changelog, it relies on the background thread to do
-- that. If the db changelog is invalidated (by rolling back the last synced
-- ledger state), it will sync in-place.
--
-- INVARIANT: The code needs that read and writes on the state are coupled
-- together or inconsistencies will arise.
doAddTx ::
  ( LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  , ValidateEnvelope blk
  , IOLike m
  ) =>
  MempoolEnv m blk ->
  WhetherToIntervene ->
  -- | The transaction to add to the mempool.
  GenTx blk ->
  m (TransactionProcessed blk)
doAddTx mpEnv wti tx =
  doAddTx' Nothing
 where
  MempoolEnv
    { mpEnvForker = forker
    , mpEnvLedgerCfg = cfg
    , mpEnvStateVar = istate
    , mpEnvTracer = trcr
    } = mpEnv

  doAddTx' mbPrevSize = do
    traceWith trcr $ TraceMempoolAttemptingAdd tx

    -- If retrying, wait until the mempool size changes before attempting to
    -- add the tx again
    let additionalCheck is =
          case mbPrevSize of
            Nothing -> pure ()
            Just prevSize -> check $ isMempoolSize is /= prevSize

    res <- withTMVarAnd istate additionalCheck $
      \is () -> do
        frkr <- readMVar forker
        tbs <-
          castLedgerTables
            <$> roforkerReadTables frkr (castLedgerTables $ getTransactionKeySets tx)
        case pureTryAddTx cfg wti tx is tbs of
          NotEnoughSpaceLeft -> do
            pure (Left (isMempoolSize is), is)
          Processed outcome@(TransactionProcessingResult is' _ _) -> do
            pure (Right outcome, fromMaybe is is')
    either (doAddTx' . Just) pure res

pureTryAddTx ::
  ( LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  ) =>
  -- | The ledger configuration.
  LedgerCfg (LedgerState blk) ->
  WhetherToIntervene ->
  -- | The transaction to add to the mempool.
  GenTx blk ->
  -- | The current internal state of the mempool.
  InternalState blk ->
  LedgerTables (LedgerState blk) ValuesMK ->
  TriedToAddTx blk
pureTryAddTx cfg wti tx is values =
  let st =
        applyMempoolDiffs
          values
          (getTransactionKeySets tx)
          (isLedgerState is)
   in case runExcept $ txMeasure cfg st tx of
        Left err ->
          -- The transaction does not have a valid measure (eg its ExUnits is
          -- greater than what this ledger state allows for a single transaction).
          --
          -- It might seem simpler to remove the failure case from 'txMeasure' and
          -- simply fully validate the tx before determining whether it'd fit in
          -- the mempool; that way we could reject invalid txs ASAP. However, for a
          -- valid tx, we'd pay that validation cost every time the node's
          -- selection changed, even if the tx wouldn't fit. So it'd very much be
          -- as if the mempool were effectively over capacity! What's worse, each
          -- attempt would not be using 'extendVRPrevApplied'.
          Processed $
            TransactionProcessingResult
              Nothing
              (MempoolTxRejected tx err)
              ( TraceMempoolRejectedTx
                  tx
                  err
                  (isMempoolSize is)
              )
        Right txsz
          -- Check for overflow
          --
          -- No measure of a transaction can ever be negative, so the only way
          -- adding two measures could result in a smaller measure is if some
          -- modular arithmetic overflowed. Also, overflow necessarily yields a
          -- lesser result, since adding 'maxBound' is modularly equivalent to
          -- subtracting one. Recall that we're checking each individual addition.
          --
          -- We assume that the 'txMeasure' limit and the mempool capacity
          -- 'isCapacity' are much smaller than the modulus, and so this should
          -- never happen. Despite that, blocking until adding the transaction
          -- doesn't overflow seems like a reasonable way to handle this case.
          | not $ currentSize Measure.<= currentSize `Measure.plus` txsz ->
              NotEnoughSpaceLeft
          -- We add the transaction if and only if it wouldn't overrun any component
          -- of the mempool capacity.
          --
          -- In the past, this condition was instead @TxSeq.toSize (isTxs is) <
          -- isCapacity is@. Thus the effective capacity of the mempool was
          -- actually one increment less than the reported capacity plus one
          -- transaction. That subtlety's cost paid for two benefits.
          --
          -- First, the absence of addition avoids a risk of overflow, since the
          -- transaction's sizes (eg ExUnits) have not yet been bounded by
          -- validation (which presumably enforces a low enough bound that any
          -- reasonably-sized mempool would never overflow the representation's
          -- 'maxBound').
          --
          -- Second, it is more fair, since it does not depend on the transaction
          -- at all. EG a large transaction might struggle to win the race against
          -- a firehose of tiny transactions.
          --
          -- However, we prefer to avoid the subtlety. Overflow is handled by the
          -- previous guard. And fairness is already ensured elsewhere (the 'MVar's
          -- in 'implAddTx' --- which the "Test.Consensus.Mempool.Fairness" test
          -- exercises). Moreover, the notion of "is under capacity" becomes
          -- difficult to assess independently of the pending tx when the measure
          -- is multi-dimensional; both typical options (any component is not full
          -- or every component is not full) lead to some confusing behaviors
          -- (denying some txs that would "obviously" fit and accepting some txs
          -- that "obviously" don't, respectively).
          --
          -- Even with the overflow handler, it's important that 'txMeasure'
          -- returns a well-bounded result. Otherwise, if an adversarial tx arrived
          -- that could't even fit in an empty mempool, then that thread would
          -- never release the 'MVar'. In particular, we tacitly assume here that a
          -- tx that wouldn't even fit in an empty mempool would be rejected by
          -- 'txMeasure'.
          | not $ currentSize `Measure.plus` txsz Measure.<= isCapacity is ->
              NotEnoughSpaceLeft
          | otherwise ->
              case validateNewTransaction cfg wti tx txsz values st is of
                (Left err, _) ->
                  Processed $
                    TransactionProcessingResult
                      Nothing
                      (MempoolTxRejected tx err)
                      ( TraceMempoolRejectedTx
                          tx
                          err
                          (isMempoolSize is)
                      )
                (Right vtx, is') ->
                  Processed $
                    TransactionProcessingResult
                      (Just is')
                      (MempoolTxAdded vtx)
                      ( TraceMempoolAddedTx
                          vtx
                          (isMempoolSize is)
                          (isMempoolSize is')
                      )
 where
  currentSize = TxSeq.toSize (isTxs is)

{-------------------------------------------------------------------------------
  Remove transactions
-------------------------------------------------------------------------------}

-- | See 'Ouroboros.Consensus.Mempool.API.removeTxsEvenIfValid'.
implRemoveTxsEvenIfValid ::
  ( IOLike m
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  , ValidateEnvelope blk
  ) =>
  MempoolEnv m blk ->
  NE.NonEmpty (GenTxId blk) ->
  m ()
implRemoveTxsEvenIfValid mpEnv toRemove =
  withTMVar istate $
    \is -> do
      let toKeep =
            filter
              ( (`notElem` Set.fromList (NE.toList toRemove))
                  . txId
                  . txForgetValidated
                  . txTicketTx
              )
              (TxSeq.toList $ isTxs is)
          toKeep' = Foldable.foldMap' (getTransactionKeySets . txForgetValidated . TxSeq.txTicketTx) toKeep
      frkr <- readMVar forker
      tbs <- castLedgerTables <$> roforkerReadTables frkr (castLedgerTables toKeep')
      let (is', t) =
            pureRemoveTxs
              capacityOverride
              cfg
              (isSlotNo is)
              (isLedgerState is `withLedgerTables` emptyLedgerTables)
              tbs
              (isLastTicketNo is)
              toKeep
              toRemove
      traceWith trcr t
      pure ((), is')
 where
  MempoolEnv
    { mpEnvStateVar = istate
    , mpEnvForker = forker
    , mpEnvTracer = trcr
    , mpEnvLedgerCfg = cfg
    , mpEnvCapacityOverride = capacityOverride
    } = mpEnv

-- | Craft a 'RemoveTxs' that manually removes the given transactions from the
-- mempool, returning inside it an updated InternalState.
pureRemoveTxs ::
  ( LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  ) =>
  MempoolCapacityBytesOverride ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  LedgerTables (LedgerState blk) ValuesMK ->
  TicketNo ->
  -- | Txs to keep
  [TxTicket (TxMeasure blk) (Validated (GenTx blk))] ->
  -- | IDs to remove
  NE.NonEmpty (GenTxId blk) ->
  (InternalState blk, TraceEventMempool blk)
pureRemoveTxs capacityOverride lcfg slot lstate values tkt txs txIds =
  let RevalidateTxsResult is' removed =
        revalidateTxsFor
          capacityOverride
          lcfg
          slot
          lstate
          values
          tkt
          txs
      trace =
        TraceMempoolManuallyRemovedTxs
          txIds
          (map getInvalidated removed)
          (isMempoolSize is')
   in (is', trace)

{-------------------------------------------------------------------------------
  Sync with ledger
-------------------------------------------------------------------------------}

-- | See 'Ouroboros.Consensus.Mempool.API.syncWithLedger'.
implSyncWithLedger ::
  ( IOLike m
  , LedgerSupportsMempool blk
  , ValidateEnvelope blk
  , HasTxId (GenTx blk)
  ) =>
  MempoolEnv m blk ->
  m (MempoolSnapshot blk)
implSyncWithLedger mpEnv =
  encloseTimedWith (TraceMempoolSynced >$< mpEnvTracer mpEnv) $ do
    res <-
      -- There could possibly be a race condition if we used there the state
      -- that triggered the re-syncing in the background watcher, if a different
      -- action acquired the state before the revalidation started.
      --
      -- For that reason, we read the state again here in the same STM
      -- transaction in which we acquire the internal state of the mempool.
      --
      -- This implies that the watcher might be triggered again with the same
      -- state from the point of view of the mempool, if after the watcher saw a
      -- new state and this read for re-syncing, the state has changed. The
      -- watcher will see it once again and trigger re-validation again. Just
      -- for performance reasons, we will avoid re-validating the mempool if the
      -- state didn't change.
      withTMVarAnd istate (const $ getCurrentLedgerState ldgrInterface registry) $
        \is (MempoolLedgerDBView ls meFrk) -> do
          eFrk <- meFrk
          case eFrk of
            -- This case should happen only if the tip has moved again, this time
            -- to a separate fork, since the background thread saw a change in the
            -- tip, which should happen very rarely
            Left{} -> do
              traceWith trcr TraceMempoolTipMovedBetweenSTMBlocks
              pure (Nothing, is)
            Right frk -> do
              let (slot, ls') = tickLedgerState cfg $ ForgeInUnknownSlot ls
              if pointHash (isTip is) == castHash (getTipHash ls) && isSlotNo is == slot
                then do
                  -- The tip didn't change, put the same state.
                  traceWith trcr $ TraceMempoolSyncNotNeeded (isTip is)
                  pure (Just (snapshotFromIS is), is)
                else do
                  -- The tip changed, we have to revalidate
                  modifyMVar_
                    forkerMVar
                    ( \oldFrk -> do
                        roforkerClose oldFrk
                        pure frk
                    )
                  tbs <- castLedgerTables <$> roforkerReadTables frk (castLedgerTables $ isTxKeys is)
                  let (is', mTrace) =
                        pureSyncWithLedger
                          capacityOverride
                          cfg
                          slot
                          ls'
                          tbs
                          is
                  whenJust mTrace (traceWith trcr)
                  pure (Just (snapshotFromIS is'), is')
    maybe
      (implSyncWithLedger mpEnv)
      pure
      res
 where
  MempoolEnv
    { mpEnvStateVar = istate
    , mpEnvForker = forkerMVar
    , mpEnvLedger = ldgrInterface
    , mpEnvRegistry = registry
    , mpEnvTracer = trcr
    , mpEnvLedgerCfg = cfg
    , mpEnvCapacityOverride = capacityOverride
    } = mpEnv

-- | Create a 'SyncWithLedger' value representing the values that will need to
-- be stored for committing this synchronization with the Ledger.
--
-- See the documentation of 'runSyncWithLedger' for more context.
pureSyncWithLedger ::
  (LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  MempoolCapacityBytesOverride ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  LedgerTables (LedgerState blk) ValuesMK ->
  InternalState blk ->
  ( InternalState blk
  , Maybe (TraceEventMempool blk)
  )
pureSyncWithLedger capacityOverride lcfg slot lstate values istate =
  let RevalidateTxsResult is' removed =
        revalidateTxsFor
          capacityOverride
          lcfg
          slot
          lstate
          values
          (isLastTicketNo istate)
          (TxSeq.toList $ isTxs istate)
      mTrace =
        if null removed
          then
            Nothing
          else
            Just $
              TraceMempoolRemoveTxs (map (\x -> (getInvalidated x, getReason x)) removed) (isMempoolSize is')
   in (is', mTrace)
