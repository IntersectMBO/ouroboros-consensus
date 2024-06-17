{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Operations that update the mempool. They are internally divided in the pure
-- and impure sides of the operation.
module Ouroboros.Consensus.Mempool.Update (
    implAddTx
  , implRemoveTxs
  , implSyncWithLedger
  ) where

import           Cardano.Slotting.Slot
import           Control.Concurrent.Class.MonadMVar (withMVar)
import           Control.Tracer
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Ouroboros.Consensus.Block.Abstract (castHash, castPoint,
                     pointHash)
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
implAddTx ::
     ( IOLike m
     , LedgerSupportsMempool blk
     , ValidateEnvelope blk
     , HasTxId (GenTx blk)
     )
  => MempoolEnv m blk
  -> AddTxOnBehalfOf
     -- ^ Whether we're acting on behalf of a remote peer or a local client.
  -> GenTx blk
     -- ^ The transaction to add to the mempool.
  -> m (MempoolAddTxResult blk)
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
      AddTxForRemotePeer -> do
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
    MempoolEnv {
        mpEnvAddTxsRemoteFifo = remoteFifo
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
    whetherToIntervene AddTxForRemotePeer  = DoNotIntervene
    whetherToIntervene AddTxForLocalClient = Intervene

-- | Tried to add a transaction, was it processed or is there no space left?
data TriedToAddTx blk =
    NoSpaceLeft
  | Processed (TransactionProcessed blk)

-- | A transaction was processed, either accepted or rejected.
data TransactionProcessed blk =
  TransactionProcessingResult
    (Maybe (InternalState blk))
    -- ^ If the transaction was accepted, the new state that can be written to
    -- the TVar.
    (MempoolAddTxResult blk)
    -- ^ The result of trying to add the transaction to the mempool.
    (TraceEventMempool blk)
    -- ^ The event emitted by the operation.

-- | This function returns whether the transaction was added or rejected, and
-- will block if the mempool is full.
--
-- See the necessary invariants on the Haddock for 'API.addTx'.
--
-- This function does not sync the Mempool contents with the ledger state in
-- case the latter changes in a way that doesn't invalidate the db changelog, it
-- relies on the background thread to do that. If the db changelog is
-- invalidated (by rolling back the last synced ledger state), it will sync
-- in-place.
--
-- INVARIANT: The code needs that read and writes on the state are coupled
-- together or inconsistencies will arise.
doAddTx ::
     ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     , IOLike m
     )
  => MempoolEnv m blk
  -> WhetherToIntervene
  -> GenTx blk
     -- ^ The transaction to add to the mempool.
  -> m (TransactionProcessed blk)
doAddTx mpEnv wti tx =
    doAddTx' Nothing
  where
    MempoolEnv {
        mpEnvLedger = ldgrInterface
      , mpEnvLedgerCfg = cfg
      , mpEnvTxSize = txSize
      , mpEnvStateVar = istate
      , mpEnvTracer = trcr
      } = mpEnv

    doAddTx' s = do
      traceWith trcr $ TraceMempoolAttemptingAdd tx
      is <- atomically $ do
        i <- takeTMVar istate
        case s of
          Nothing -> pure ()
          Just s' -> check $ isMempoolSize i /= s'
        pure i
      mTbs <- getLedgerTablesAtFor ldgrInterface (isTip is) [tx]
      case mTbs of
        Just tbs -> do
          traceWith trcr $ TraceMempoolLedgerFound (isTip is)
          case pureTryAddTx cfg txSize wti tx is tbs of
            NoSpaceLeft -> do
              atomically $ putTMVar istate is
              doAddTx' (Just $ isMempoolSize is)
            Processed outcome@(TransactionProcessingResult is' _ _) -> do
              atomically $ putTMVar istate $ fromMaybe is is'
              pure outcome
        Nothing -> do
          traceWith trcr $ TraceMempoolLedgerNotFound (isTip is)
          -- We couldn't retrieve the values because the state is no longer on
          -- the db. We need to resync.
          atomically $ putTMVar istate is
          (_, mTrace) <- implSyncWithLedger mpEnv
          whenJust mTrace (traceWith trcr)
          doAddTx' s

-- | Craft a 'TriedToAddTx' value containing the resulting state if
-- applicable, the tracing event and the result of adding this transaction. See
-- the documentation of 'implAddTx' for some more context.
--
-- It returns 'NoSpaceLeft' only when the current mempool size is bigger or
-- equal than then mempool capacity. Otherwise it will validate the transaction
-- and add it to the mempool if there is at least one byte free on the mempool.
pureTryAddTx ::
     ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     )
  => LedgerCfg (LedgerState blk)
     -- ^ The ledger configuration.
  -> (GenTx blk -> TxSizeInBytes)
     -- ^ The function to calculate the size of a transaction.
  -> WhetherToIntervene
  -> GenTx blk
     -- ^ The transaction to add to the mempool.
  -> InternalState blk
     -- ^ The current internal state of the mempool.
  -> LedgerTables (LedgerState blk) ValuesMK
     -- ^ Values for this transaction
  -> TriedToAddTx blk
pureTryAddTx cfg txSize wti tx is values
    -- We add the transaction if there is at least one byte free left in the
    -- mempool, and...
  | let curSize = msNumBytes  $ isMempoolSize is
  , curSize < getMempoolCapacityBytes (isCapacity is)
    -- ... if the mempool has less than 2.5 mebibytes of ref scripts.
  , let maxTotalRefScriptSize = 5 * 512 * 1024 -- 2.5 Mebibytes
        curTotalRefScriptSize = isTotalRefScriptSize is
  , curTotalRefScriptSize Prelude.< maxTotalRefScriptSize
  =
  Processed $ case eVtx of
      Right vtx ->
        TransactionProcessingResult
          (Just is')
          (MempoolTxAdded vtx)
          (TraceMempoolAddedTx
            vtx
            (isMempoolSize is)
            (isMempoolSize is')
          )
      Left err ->
        TransactionProcessingResult
          Nothing
          (MempoolTxRejected tx err)
          (TraceMempoolRejectedTx
            tx
            err
            (isMempoolSize is)
          )
  | otherwise
  = NoSpaceLeft
  where
    (eVtx, is') = validateNewTransaction cfg txSize wti tx values is

{-------------------------------------------------------------------------------
  Remove transactions
-------------------------------------------------------------------------------}

-- | See 'Ouroboros.Consensus.Mempool.API.removeTxs'.
implRemoveTxs ::
     ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
   => MempoolEnv m blk
   -> NE.NonEmpty (GenTxId blk)
   -> m ()
implRemoveTxs mpEnv toRemove = do
    (is, ls) <- atomically $ do
      is <- takeTMVar istate
      ls <- getCurrentLedgerState ldgrInterface
      pure (is, ls)
    let toKeep = filter
                 (   (`notElem` Set.fromList (NE.toList toRemove))
                     . txId
                     . txForgetValidated
                     . txTicketTx
                 )
                 (TxSeq.toList $ isTxs is)
        (slot, ticked) = tickLedgerState cfg (ForgeInUnknownSlot ls)
        toKeep' = [ txForgetValidated . TxSeq.txTicketTx $ tx | tx <- toKeep ]
    mTbs <- getLedgerTablesAtFor ldgrInterface (castPoint (getTip ls)) toKeep'
    case mTbs of
      Nothing -> do
        atomically $ putTMVar istate is
        implRemoveTxs mpEnv toRemove
      Just tbs -> do
        let (is', t) = pureRemoveTxs
                         capacityOverride
                         cfg
                         slot
                         ticked
                         tbs
                         (isLastTicketNo is)
                         toKeep
                         toRemove
        atomically $ putTMVar istate is'
        traceWith trcr t
  where
    MempoolEnv { mpEnvStateVar         = istate
               , mpEnvLedger           = ldgrInterface
               , mpEnvTracer           = trcr
               , mpEnvLedgerCfg        = cfg
               , mpEnvCapacityOverride = capacityOverride
               } = mpEnv

-- | Craft a 'RemoveTxs' that manually removes the given transactions from the
-- mempool, returning inside it an updated InternalState.
pureRemoveTxs ::
     ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     )
  => MempoolCapacityBytesOverride
  -> LedgerConfig blk
  -> SlotNo
  -> TickedLedgerState blk DiffMK
  -> LedgerTables (LedgerState blk) ValuesMK
  -> TicketNo
  -> [TxTicket (Validated (GenTx blk))] -- ^ Txs to keep
  -> NE.NonEmpty (GenTxId blk) -- ^ IDs to remove
  -> (InternalState blk, TraceEventMempool blk)
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
        trace = TraceMempoolManuallyRemovedTxs
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
     )
  => MempoolEnv m blk
  -> m (MempoolSnapshot blk, Maybe (TraceEventMempool blk))
implSyncWithLedger mpEnv = do
  traceWith trcr TraceMempoolAttemptingSync
  (is, ls) <- atomically $ do
    is <- takeTMVar istate
    ls <- getCurrentLedgerState ldgrInterface
    pure (is, ls)

  let (slot, ls') = tickLedgerState cfg $ ForgeInUnknownSlot ls

  if pointHash (isTip is) == castHash (getTipHash ls) &&
     isSlotNo is == slot
    then do
    -- The tip didn't change, put the same state.
    atomically $ putTMVar istate is
    traceWith trcr $ TraceMempoolSyncNotNeeded (isTip is) (castPoint $ getTip ls)
    pure (snapshotFromIS is, Nothing)
    else do
    -- We need to revalidate
    let pt = castPoint (getTip ls)
        txs = [ txForgetValidated . TxSeq.txTicketTx $ tx
              | tx <- TxSeq.toList $ isTxs is
              ]
    mTbs <- getLedgerTablesAtFor ldgrInterface pt txs
    case mTbs of
      Just tbs -> do
        let (is', mTrace) = pureSyncWithLedger
                              capacityOverride
                              cfg
                              slot
                              ls'
                              tbs
                              is
        atomically $ putTMVar istate is'
        whenJust mTrace (traceWith trcr)
        traceWith trcr TraceMempoolSyncDone
        return (snapshotFromIS is', mTrace)
      Nothing -> do
        -- If the point is gone, resync
        atomically $ putTMVar istate is
        implSyncWithLedger mpEnv
  where
    MempoolEnv { mpEnvStateVar         = istate
               , mpEnvLedger           = ldgrInterface
               , mpEnvTracer           = trcr
               , mpEnvLedgerCfg        = cfg
               , mpEnvCapacityOverride = capacityOverride
               } = mpEnv

-- | Create a 'SyncWithLedger' value representing the values that will need to
-- be stored for committing this synchronization with the Ledger.
--
-- See the documentation of 'runSyncWithLedger' for more context.
pureSyncWithLedger
  :: (LedgerSupportsMempool blk, HasTxId (GenTx blk))
  => MempoolCapacityBytesOverride
  -> LedgerConfig blk
  -> SlotNo
  -> TickedLedgerState blk DiffMK
  -> LedgerTables (LedgerState blk) ValuesMK
  -> InternalState blk
  -> ( InternalState blk
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
      mTrace = if null removed
               then
                 Nothing
               else
                 Just $ TraceMempoolRemoveTxs (map (\x -> (getInvalidated x, getReason x)) removed) (isMempoolSize is')
  in (is', mTrace)
