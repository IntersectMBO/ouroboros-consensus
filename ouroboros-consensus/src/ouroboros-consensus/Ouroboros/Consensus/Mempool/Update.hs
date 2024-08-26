{-# LANGUAGE FlexibleContexts #-}

-- | Operations that update the mempool. They are internally divided in the pure
-- and impure sides of the operation.
module Ouroboros.Consensus.Mempool.Update (
    implAddTx
  , implRemoveTxs
  , implSyncWithLedger
  ) where

import           Control.Concurrent.Class.MonadMVar (MVar, withMVar)
import           Control.Exception (assert)
import           Control.Monad.Except (runExcept)
import           Control.Tracer
import           Data.Maybe (isJust)
import qualified Data.Measure as Measure
import qualified Data.Set as Set
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.Capacity
import           Ouroboros.Consensus.Mempool.Impl.Common
import           Ouroboros.Consensus.Mempool.TxSeq (TxTicket (..))
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.IOLike hiding (withMVar)

{-------------------------------------------------------------------------------
  Add transactions
-------------------------------------------------------------------------------}

-- | Add a single transaction to the mempool, blocking if there is no space.
--
implAddTx ::
     ( MonadSTM m
     , MonadMVar m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     )
  => StrictTVar m (InternalState blk)
     -- ^ The InternalState TVar.
  -> MVar m ()
      -- ^ The FIFO for remote peers
  -> MVar m ()
      -- ^ The FIFO for all remote peers and local clients
  -> LedgerConfig blk
     -- ^ The configuration of the ledger.
  -> Tracer m (TraceEventMempool blk)
  -> AddTxOnBehalfOf
     -- ^ Whether we're acting on behalf of a remote peer or a local client.
  -> GenTx blk
     -- ^ The transaction to add to the mempool.
  -> m (MempoolAddTxResult blk)
implAddTx istate remoteFifo allFifo cfg trcr onbehalf tx =
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
    implAddTx' = do
      (result, ev) <- atomically $ do
        outcome <- implTryAddTx istate cfg
                                (whetherToIntervene onbehalf)
                                tx
        case outcome of
          TryAddTx _ result ev -> do return (result, ev)

          -- or block until space is available to fit the next transaction
          NotEnoughSpaceLeft   -> retry

      traceWith trcr ev
      return result

    whetherToIntervene :: AddTxOnBehalfOf -> WhetherToIntervene
    whetherToIntervene AddTxForRemotePeer  = DoNotIntervene
    whetherToIntervene AddTxForLocalClient = Intervene

-- | Result of trying to add a transaction to the mempool.
data TryAddTx blk =
    -- | Adding the next transaction would put the mempool over capacity.
    NotEnoughSpaceLeft
    -- | A transaction was processed.
  | TryAddTx
      (Maybe (InternalState blk))
      -- ^ If the transaction was accepted, the new state that can be written to
      -- the TVar.
      (MempoolAddTxResult blk)
      -- ^ The result of trying to add the transaction to the mempool.
      (TraceEventMempool blk)
      -- ^ The event emitted by the operation.

-- | Add a single transaction by interpreting a 'TryAddTx' from 'pureTryAddTx'.
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
-- This function does not sync the Mempool contents with the ledger state in
-- case the latter changes, it relies on the background thread to do that.
--
-- INVARIANT: The code needs that read and writes on the state are coupled
-- together or inconsistencies will arise. To ensure that STM transactions are
-- short, each iteration of the helper function is a separate STM transaction.
implTryAddTx ::
     ( MonadSTM m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     )
  => StrictTVar m (InternalState blk)
     -- ^ The InternalState TVar.
  -> LedgerConfig blk
     -- ^ The configuration of the ledger.
  -> WhetherToIntervene
  -> GenTx blk
     -- ^ The transaction to add to the mempool.
  -> STM m (TryAddTx blk)
implTryAddTx istate cfg wti tx = do
        is <- readTVar istate
        let outcome = pureTryAddTx cfg wti tx is
        case outcome of
          TryAddTx (Just is') _ _ -> writeTVar istate is'
          TryAddTx Nothing    _ _ -> return ()
          NotEnoughSpaceLeft      -> return ()
        return outcome

-- | See the documentation of 'implTryAddTx' for some more context.
pureTryAddTx ::
     ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     )
  => LedgerCfg (LedgerState blk)
     -- ^ The ledger configuration.
  -> WhetherToIntervene
  -> GenTx blk
     -- ^ The transaction to add to the mempool.
  -> InternalState blk
     -- ^ The current internal state of the mempool.
  -> TryAddTx blk
pureTryAddTx cfg wti tx is =
  case runExcept $ txMeasure cfg (isLedgerState is) tx of
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
      TryAddTx
        Nothing
        (MempoolTxRejected tx err)
        (TraceMempoolRejectedTx
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
     | not $ currentSize Measure.<= currentSize `Measure.plus` txsz
     ->
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
      | not $ currentSize `Measure.plus` txsz Measure.<= isCapacity is
      ->
        NotEnoughSpaceLeft
      | otherwise
      ->
        case extendVRNew cfg wti tx $ validationResultFromIS is of
          Left err ->
            TryAddTx
              Nothing
              (MempoolTxRejected tx err)
              (TraceMempoolRejectedTx
               tx
               err
               (isMempoolSize is)
              )
          Right (vtx, vr) ->
            let is' = internalStateFromVR vr
            in
            assert (isJust (vrNewValid vr)) $
              TryAddTx
                (Just is')
                (MempoolTxAdded vtx)
                (TraceMempoolAddedTx
                  vtx
                  (isMempoolSize is)
                  (isMempoolSize is')
                )
  where
    currentSize = TxSeq.toSize (isTxs is)

{-------------------------------------------------------------------------------
  Remove transactions
-------------------------------------------------------------------------------}

-- | A datatype containing the state resulting after removing the requested
-- transactions from the mempool and maybe a message to be traced while removing
-- them.
data RemoveTxs blk =
    WriteRemoveTxs (InternalState blk) (Maybe (TraceEventMempool blk))

-- | See 'Ouroboros.Consensus.Mempool.API.removeTxs'.
implRemoveTxs ::
     ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => MempoolEnv m blk
  -> [GenTxId blk]
  -> m ()
implRemoveTxs menv txs
  | null txs = pure ()
  | otherwise = do
    tr <- atomically $ do
        is <- readTVar istate
        ls <- getCurrentLedgerState ldgrInterface
        let WriteRemoveTxs is' t = pureRemoveTxs cfg co txs is ls
        writeTVar istate is'
        pure t
    whenJust tr (traceWith trcr)
  where
    MempoolEnv { mpEnvStateVar = istate
               , mpEnvLedger = ldgrInterface
               , mpEnvTracer = trcr
               , mpEnvLedgerCfg = cfg
               , mpEnvCapacityOverride = co
               } = menv

-- | Craft a 'RemoveTxs' that manually removes the given transactions from the
-- mempool, returning inside it an updated InternalState.
pureRemoveTxs ::
     ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> [GenTxId blk]
  -> InternalState blk
  -> LedgerState blk
  -> RemoveTxs blk
pureRemoveTxs cfg capacityOverride txIds is lstate =
    -- Filtering is O(n), but this function will rarely be used, as it is an
    -- escape hatch when there's an inconsistency between the ledger and the
    -- mempool.
    let toRemove       = Set.fromList txIds
        txTickets'     = filter
                           (   (`notElem` toRemove)
                             . txId
                             . txForgetValidated
                             . txTicketTx
                           )
                           (TxSeq.toList (isTxs is))
        (slot, ticked) = tickLedgerState cfg (ForgeInUnknownSlot lstate)
        vr             = revalidateTxsFor
                           capacityOverride
                           cfg
                           slot
                           ticked
                           (isLastTicketNo is)
                           txTickets'
        is'            = internalStateFromVR vr
        needsTrace     = if null txIds
                         then
                           Nothing
                         else
                           Just $ TraceMempoolManuallyRemovedTxs
                             txIds
                             (map fst (vrInvalid vr))
                             (isMempoolSize is')
    in WriteRemoveTxs is' needsTrace

{-------------------------------------------------------------------------------
  Sync with ledger
-------------------------------------------------------------------------------}

-- | A datatype containing the new state produced by syncing with the Ledger, a
-- snapshot of that mempool state and, if needed, a tracing message.
data SyncWithLedger blk =
    NewSyncedState (InternalState blk)
                   (MempoolSnapshot blk)
                   (Maybe (TraceEventMempool blk))

-- | See 'Ouroboros.Consensus.Mempool.API.syncWithLedger'.
implSyncWithLedger ::
     (
       IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => MempoolEnv m blk
  -> m (MempoolSnapshot blk)
implSyncWithLedger menv = do
    (mTrace, mp) <- atomically $ do
      is <- readTVar istate
      ls <- getCurrentLedgerState ldgrInterface
      let NewSyncedState is' msp mTrace = pureSyncWithLedger is ls cfg co
      writeTVar istate is'
      return (mTrace, msp)
    whenJust mTrace (traceWith trcr)
    return mp
  where
    MempoolEnv { mpEnvStateVar = istate
               , mpEnvLedger = ldgrInterface
               , mpEnvTracer = trcr
               , mpEnvLedgerCfg = cfg
               , mpEnvCapacityOverride = co
               } = menv

-- | Create a 'SyncWithLedger' value representing the values that will need to
-- be stored for committing this synchronization with the Ledger.
--
-- See the documentation of 'runSyncWithLedger' for more context.
pureSyncWithLedger ::
     (LedgerSupportsMempool blk, HasTxId (GenTx blk), ValidateEnvelope blk)
  => InternalState blk
  -> LedgerState blk
  -> LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> SyncWithLedger blk
pureSyncWithLedger istate lstate lcfg capacityOverride =
    let vr          = validateStateFor
                        capacityOverride
                        lcfg
                        (ForgeInUnknownSlot lstate)
                        istate
        removed     = vrInvalid vr
        istate'     = internalStateFromVR vr
        mTrace      = if null removed
                      then
                        Nothing
                      else
                        Just $ TraceMempoolRemoveTxs removed (isMempoolSize istate')
        snapshot    = snapshotFromIS istate'
    in
      NewSyncedState istate' snapshot mTrace
