{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definition of common types used in "Ouroboros.Consensus.Mempool.Init",
-- "Ouroboros.Consensus.Mempool.Update" and "Ouroboros.Consensus.Mempool.Query".
module Ouroboros.Consensus.Mempool.Impl.Common
  ( -- * Internal state
    InternalState (..)
  , isMempoolSize

    -- * Mempool environment
  , MempoolEnv (..)
  , initMempoolEnv

    -- * Ledger interface
  , LedgerInterface (..)
  , chainDBLedgerInterface

    -- * Validation
  , RevalidateTxsResult (..)
  , computeSnapshot
  , revalidateTxsFor
  , validateNewTransaction

    -- * Tracing
  , TraceEventMempool (..)

    -- * Conversions
  , snapshotFromIS

    -- * Ticking a ledger state
  , tickLedgerState
  ) where

import Control.Concurrent.Class.MonadMVar (MVar, newMVar)
import Control.Concurrent.Class.MonadSTM.Strict.TMVar (newTMVarIO)
import Control.Monad.Trans.Except (runExcept)
import Control.Tracer
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Mempool.Capacity
import Ouroboros.Consensus.Mempool.TxSeq (TxSeq (..), TxTicket (..))
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Util.Enclose (EnclosingTimed)
import Ouroboros.Consensus.Util.IOLike hiding (newMVar)

{-------------------------------------------------------------------------------
  Internal State
-------------------------------------------------------------------------------}

-- | Internal state in the mempool
data InternalState blk = IS
  { isTxs :: !(TxSeq (TxMeasure blk) (Validated (GenTx blk)))
  -- ^ Transactions currently in the mempool
  --
  -- NOTE: the total size of the transactions in 'isTxs' may exceed the
  -- current capacity ('isCapacity'). When the capacity computed from the
  -- ledger has shrunk, we don't remove transactions from the Mempool to
  -- satisfy the new lower limit. We let the transactions get removed in
  -- the normal way: by becoming invalid w.r.t. the updated ledger state.
  -- We treat a Mempool /over/ capacity in the same way as a Mempool /at/
  -- capacity.
  , isTxIds :: !(Set (GenTxId blk))
  -- ^ The cached IDs of transactions currently in the mempool.
  --
  -- This allows one to more quickly lookup transactions by ID from a
  -- 'MempoolSnapshot' (see 'snapshotHasTx').
  --
  -- This should always be in-sync with the transactions in 'isTxs'.
  , isTxKeys :: !(LedgerTables (LedgerState blk) KeysMK)
  -- ^ The cached set of keys needed for the transactions
  -- currently in the mempool.
  --
  -- INVARIANT: @'isTxKeys' == foldMap (getTransactionKeySets . txForgetValidated) $ toList 'isTxs'@
  , isTxValues :: !(LedgerTables (LedgerState blk) ValuesMK)
  -- ^ The cached values corresponding to reading 'isTxKeys' at
  -- 'isLedgerState'. These values can be used unless we switch to
  -- a different ledger state. It usually happens in the forging
  -- loop that the same ledger state that was in 'isLedgerState'
  -- is used, but ticked to a different slot so we can reuse these
  -- values.
  --
  -- INVARIANT: 'isTxValues' should be equal to @getForkerAtTarget ... 'isLedgerState' >>= \f -> forkerReadTables f isTxKeys@
  , isLedgerState :: !(TickedLedgerState blk DiffMK)
  -- ^ The cached ledger state after applying the transactions in the
  -- Mempool against the chain's ledger state. New transactions will be
  -- validated against this ledger.
  --
  -- INVARIANT: 'isLedgerState' is the ledger resulting from applying the
  -- transactions in 'isTxs' against the ledger identified 'isTip' as tip.
  , isTip :: !(Point blk)
  -- ^ The tip of the chain that 'isTxs' was validated against
  , isSlotNo :: !SlotNo
  -- ^ The most recent 'SlotNo' that 'isTxs' was validated against
  --
  -- Note in particular that if the mempool is revalidated against a state S
  -- at slot s, then the state will be ticked (for now to the successor
  -- slot, see 'tickLedgerState') and 'isSlotNo' will be set to @succ s@,
  -- which is different from the slot of the original ledger state, which
  -- will remain in 'isTip'.
  , isLastTicketNo :: !TicketNo
  -- ^ The mempool 'TicketNo' counter.
  --
  -- See 'vrLastTicketNo' for more information.
  , isCapacity :: !(TxMeasure blk)
  -- ^ Current maximum capacity of the Mempool. Result of
  -- 'computeMempoolCapacity' using the current chain's
  -- 'TickedLedgerState'.
  --
  -- NOTE: this does not correspond to 'isLedgerState', which is the
  -- 'TickedLedgerState' /after/ applying the transactions in the Mempool.
  -- There might be a transaction in the Mempool triggering a change in
  -- the maximum transaction capacity of a block, which would change the
  -- Mempool's capacity (unless overridden). We don't want the Mempool's
  -- capacity to depend on its contents. The mempool is assuming /all/ its
  -- transactions will be in the next block. So any changes caused by that
  -- block will take effect after applying it and will only affect the
  -- next block.
  }
  deriving Generic

deriving instance
  ( NoThunks (Validated (GenTx blk))
  , NoThunks (GenTxId blk)
  , NoThunks (TickedLedgerState blk DiffMK)
  , NoThunks (TxIn (LedgerState blk))
  , NoThunks (TxOut (LedgerState blk))
  , NoThunks (TxMeasure blk)
  , StandardHash blk
  , Typeable blk
  ) =>
  NoThunks (InternalState blk)

-- | \( O(1) \). Return the number of transactions in the internal state of
-- the Mempool paired with their total size in bytes.
isMempoolSize :: TxLimits blk => InternalState blk -> MempoolSize
isMempoolSize is =
  MempoolSize
    { msNumTxs = fromIntegral $ length $ isTxs is
    , msNumBytes = txMeasureByteSize $ TxSeq.toSize $ isTxs is
    }

initInternalState ::
  LedgerSupportsMempool blk =>
  MempoolCapacityBytesOverride ->
  -- | Used for 'isLastTicketNo'
  TicketNo ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState blk DiffMK ->
  InternalState blk
initInternalState capacityOverride lastTicketNo cfg slot st =
  IS
    { isTxs = TxSeq.Empty
    , isTxIds = Set.empty
    , isTxKeys = emptyLedgerTables
    , isTxValues = emptyLedgerTables
    , isLedgerState = st
    , isTip = castPoint $ getTip st
    , isSlotNo = slot
    , isLastTicketNo = lastTicketNo
    , isCapacity = computeMempoolCapacity cfg st capacityOverride
    }

{-------------------------------------------------------------------------------
  Ledger Interface
-------------------------------------------------------------------------------}

-- | Abstract interface needed to run a Mempool.
data LedgerInterface m blk = LedgerInterface
  { getCurrentLedgerState :: STM m (LedgerState blk EmptyMK)
  -- ^ Get the current tip of the LedgerDB.
  , getLedgerTablesAtFor ::
      Point blk ->
      LedgerTables (LedgerState blk) KeysMK ->
      m (Maybe (LedgerTables (LedgerState blk) ValuesMK))
  -- ^ Get values at the given point on the chain. Returns Nothing if the
  -- anchor moved or if the state is not found on the ledger db.
  }

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDBLedgerInterface ::
  IOLike m =>
  ChainDB m blk -> LedgerInterface m blk
chainDBLedgerInterface chainDB =
  LedgerInterface
    { getCurrentLedgerState =
        ledgerState <$> ChainDB.getCurrentLedger chainDB
    , getLedgerTablesAtFor = \pt keys ->
        fmap castLedgerTables <$> ChainDB.getLedgerTablesAtFor chainDB pt (castLedgerTables keys)
    }

{-------------------------------------------------------------------------------
  Mempool environment
-------------------------------------------------------------------------------}

-- | The mempool environment captures all the associated variables wrt the
-- Mempool and is accessed by the Mempool interface on demand to perform the
-- different operations.
data MempoolEnv m blk = MempoolEnv
  { mpEnvLedger :: LedgerInterface m blk
  , mpEnvLedgerCfg :: LedgerConfig blk
  , mpEnvStateVar :: StrictTMVar m (InternalState blk)
  , mpEnvAddTxsRemoteFifo :: MVar m ()
  , mpEnvAddTxsAllFifo :: MVar m ()
  , mpEnvTracer :: Tracer m (TraceEventMempool blk)
  , mpEnvCapacityOverride :: MempoolCapacityBytesOverride
  }

initMempoolEnv ::
  ( IOLike m
  , LedgerSupportsMempool blk
  , ValidateEnvelope blk
  ) =>
  LedgerInterface m blk ->
  LedgerConfig blk ->
  MempoolCapacityBytesOverride ->
  Tracer m (TraceEventMempool blk) ->
  m (MempoolEnv m blk)
initMempoolEnv ledgerInterface cfg capacityOverride tracer = do
  st <- atomically $ getCurrentLedgerState ledgerInterface
  let (slot, st') = tickLedgerState cfg (ForgeInUnknownSlot st)
  isVar <-
    newTMVarIO $
      initInternalState capacityOverride TxSeq.zeroTicketNo cfg slot st'
  addTxRemoteFifo <- newMVar ()
  addTxAllFifo <- newMVar ()
  return
    MempoolEnv
      { mpEnvLedger = ledgerInterface
      , mpEnvLedgerCfg = cfg
      , mpEnvStateVar = isVar
      , mpEnvAddTxsRemoteFifo = addTxRemoteFifo
      , mpEnvAddTxsAllFifo = addTxAllFifo
      , mpEnvTracer = tracer
      , mpEnvCapacityOverride = capacityOverride
      }

{-------------------------------------------------------------------------------
  Ticking the ledger state
-------------------------------------------------------------------------------}

-- | Tick the 'LedgerState' using the given 'BlockSlot'.
tickLedgerState ::
  forall blk.
  (UpdateLedger blk, ValidateEnvelope blk) =>
  LedgerConfig blk ->
  ForgeLedgerState blk ->
  (SlotNo, TickedLedgerState blk DiffMK)
tickLedgerState _cfg (ForgeInKnownSlot slot st) = (slot, st)
tickLedgerState cfg (ForgeInUnknownSlot st) =
  (slot, applyChainTick OmitLedgerEvents cfg slot st)
 where
  -- Optimistically assume that the transactions will be included in a block
  -- in the next available slot
  --
  -- TODO: We should use time here instead
  -- <https://github.com/IntersectMBO/ouroboros-network/issues/1298>
  -- Once we do, the ValidateEnvelope constraint can go.
  slot :: SlotNo
  slot = case ledgerTipSlot st of
    Origin -> minimumPossibleSlotNo (Proxy @blk)
    NotOrigin s -> succ s

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

-- | Extend 'InternalState' with a new transaction (one which we have not
-- previously validated) that may or may not be valid in this ledger state.
validateNewTransaction ::
  (LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  LedgerConfig blk ->
  WhetherToIntervene ->
  GenTx blk ->
  TxMeasure blk ->
  -- | Values to cache if success
  LedgerTables (LedgerState blk) ValuesMK ->
  -- | This state is the internal state with the tables for this transaction
  -- advanced through the diffs in the internal state. One could think we can
  -- create this value here, but it is needed for some other uses like calling
  -- 'txMeasure' before this function.
  TickedLedgerState blk ValuesMK ->
  InternalState blk ->
  ( Either (ApplyTxErr blk) (Validated (GenTx blk))
  , InternalState blk
  )
validateNewTransaction cfg wti tx txsz origValues st is =
  case runExcept (applyTx cfg wti isSlotNo tx st) of
    Left err -> (Left err, is)
    Right (st', vtx) ->
      ( Right vtx
      , is
          { isTxs = isTxs :> TxTicket vtx nextTicketNo txsz
          , isTxKeys = isTxKeys <> getTransactionKeySets tx
          , isTxValues = ltliftA2 unionValues isTxValues origValues
          , isTxIds = Set.insert (txId tx) isTxIds
          , isLedgerState = prependMempoolDiffs isLedgerState st'
          , isLastTicketNo = nextTicketNo
          }
      )
 where
  IS
    { isTxs
    , isTxIds
    , isTxKeys
    , isTxValues
    , isLedgerState
    , isLastTicketNo
    , isSlotNo
    } = is

  nextTicketNo = succ isLastTicketNo

-- | Revalidate the given transactions against the given ticked ledger state,
-- producing a new 'InternalState'.
--
-- Note that this function will perform revalidation so it is expected that the
-- transactions given to it were previously applied, for example if we are
-- revalidating the whole set of transactions onto a new state, or if we remove
-- some transactions and revalidate the remaining ones.
revalidateTxsFor ::
  (LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  MempoolCapacityBytesOverride ->
  LedgerConfig blk ->
  SlotNo ->
  -- | The ticked ledger state againt which txs will be revalidated
  TickedLedgerState blk DiffMK ->
  -- | The tables with all the inputs for the transactions
  LedgerTables (LedgerState blk) ValuesMK ->
  -- | 'isLastTicketNo' and 'vrLastTicketNo'
  TicketNo ->
  [TxTicket (TxMeasure blk) (Validated (GenTx blk))] ->
  RevalidateTxsResult blk
revalidateTxsFor capacityOverride cfg slot st values lastTicketNo txTickets =
  let theTxs = map wrap txTickets
      wrap = (\(TxTicket tx tk tz) -> (tx, (tk, tz)))
      unwrap = (\(tx, (tk, tz)) -> TxTicket tx tk tz)
      ReapplyTxsResult err val st' =
        reapplyTxs ComputeDiffs cfg slot theTxs $
          applyMempoolDiffs
            values
            (Foldable.foldMap' (getTransactionKeySets . txForgetValidated . fst) theTxs)
            st
      keys = Foldable.foldMap' (getTransactionKeySets . txForgetValidated . fst) val
   in RevalidateTxsResult
        ( IS
            { isTxs = TxSeq.fromList $ map unwrap val
            , isTxIds = Set.fromList $ map (txId . txForgetValidated . fst) val
            , isTxKeys = keys
            , isTxValues = ltliftA2 restrictValuesMK values keys
            , isLedgerState = trackingToDiffs st'
            , isTip = castPoint $ getTip st
            , isSlotNo = slot
            , isLastTicketNo = lastTicketNo
            , isCapacity = computeMempoolCapacity cfg st' capacityOverride
            }
        )
        err

data RevalidateTxsResult blk
  = RevalidateTxsResult
  { newInternalState :: !(InternalState blk)
  -- ^ The internal state after revalidation
  , removedTxs :: ![Invalidated blk]
  -- ^ The previously valid transactions that were now invalid
  }

-- | Compute snapshot is largely the same as revalidate the transactions
-- but we ignore the diffs.
computeSnapshot ::
  (LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  MempoolCapacityBytesOverride ->
  LedgerConfig blk ->
  SlotNo ->
  -- | The ticked ledger state againt which txs will be revalidated
  TickedLedgerState blk DiffMK ->
  -- | The tables with all the inputs for the transactions
  LedgerTables (LedgerState blk) ValuesMK ->
  -- | 'isLastTicketNo' and 'vrLastTicketNo'
  TicketNo ->
  [TxTicket (TxMeasure blk) (Validated (GenTx blk))] ->
  MempoolSnapshot blk
computeSnapshot capacityOverride cfg slot st values lastTicketNo txTickets =
  let theTxs = map wrap txTickets
      wrap = (\(TxTicket tx tk tz) -> (tx, (tk, tz)))
      unwrap = (\(tx, (tk, tz)) -> TxTicket tx tk tz)
      ReapplyTxsResult _ val st' =
        reapplyTxs IgnoreDiffs cfg slot theTxs $
          applyMempoolDiffs
            values
            (Foldable.foldMap' (getTransactionKeySets . txForgetValidated . fst) theTxs)
            st
   in snapshotFromIS $
        IS
          { isTxs = TxSeq.fromList $ map unwrap val
          , isTxIds = Set.fromList $ map (txId . txForgetValidated . fst) val
          , -- These two can be empty since we don't need the resulting
            -- values at all when making a snapshot, as we won't update
            -- the internal state.
            isTxKeys = emptyLedgerTables
          , isTxValues = emptyLedgerTables
          , isLedgerState = trackingToDiffs st'
          , isTip = castPoint $ getTip st
          , isSlotNo = slot
          , isLastTicketNo = lastTicketNo
          , isCapacity = computeMempoolCapacity cfg st' capacityOverride
          }

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

-- | Create a Mempool Snapshot from a given Internal State of the mempool.
snapshotFromIS ::
  forall blk.
  (HasTxId (GenTx blk), TxLimits blk, GetTip (TickedLedgerState blk)) =>
  InternalState blk ->
  MempoolSnapshot blk
snapshotFromIS is =
  MempoolSnapshot
    { snapshotTxs = implSnapshotGetTxs is
    , snapshotTxsAfter = implSnapshotGetTxsAfter is
    , snapshotLookupTx = implSnapshotGetTx is
    , snapshotHasTx = implSnapshotHasTx is
    , snapshotMempoolSize = implSnapshotGetMempoolSize is
    , snapshotSlotNo = isSlotNo is
    , snapshotStateHash = getTipHash $ isLedgerState is
    , snapshotTake = implSnapshotTake is
    }
 where
  implSnapshotGetTxs ::
    InternalState blk ->
    [(Validated (GenTx blk), TicketNo, TxMeasure blk)]
  implSnapshotGetTxs = flip implSnapshotGetTxsAfter TxSeq.zeroTicketNo

  implSnapshotGetTxsAfter ::
    InternalState blk ->
    TicketNo ->
    [(Validated (GenTx blk), TicketNo, TxMeasure blk)]
  implSnapshotGetTxsAfter IS{isTxs} =
    TxSeq.toTuples . snd . TxSeq.splitAfterTicketNo isTxs

  implSnapshotTake ::
    InternalState blk ->
    TxMeasure blk ->
    [Validated (GenTx blk)]
  implSnapshotTake IS{isTxs} =
    map TxSeq.txTicketTx . TxSeq.toList . fst . TxSeq.splitAfterTxSize isTxs

  implSnapshotGetTx ::
    InternalState blk ->
    TicketNo ->
    Maybe (Validated (GenTx blk))
  implSnapshotGetTx IS{isTxs} = (isTxs `TxSeq.lookupByTicketNo`)

  implSnapshotHasTx ::
    InternalState blk ->
    GenTxId blk ->
    Bool
  implSnapshotHasTx IS{isTxIds} = flip Set.member isTxIds

  implSnapshotGetMempoolSize ::
    InternalState blk ->
    MempoolSize
  implSnapshotGetMempoolSize = isMempoolSize

{-------------------------------------------------------------------------------
  Tracing support for the mempool operations
-------------------------------------------------------------------------------}

-- | Events traced by the Mempool.
data TraceEventMempool blk
  = TraceMempoolAddedTx
      -- | New, valid transaction that was added to the Mempool.
      (Validated (GenTx blk))
      -- | The size of the Mempool before adding the transaction.
      MempoolSize
      -- | The size of the Mempool after adding the transaction.
      MempoolSize
  | TraceMempoolRejectedTx
      -- | New, invalid transaction thas was rejected and thus not added to
      -- the Mempool.
      (GenTx blk)
      -- | The reason for rejecting the transaction.
      (ApplyTxErr blk)
      -- | The current size of the Mempool.
      MempoolSize
  | TraceMempoolRemoveTxs
      -- | Previously valid transactions that are no longer valid because of
      -- changes in the ledger state (details are in the provided 'ApplyTxErr').
      -- These transactions have been removed from the Mempool.
      [(Validated (GenTx blk), ApplyTxErr blk)]
      -- | The current size of the Mempool.
      MempoolSize
  | TraceMempoolManuallyRemovedTxs
      -- | Transactions that have been manually removed from the Mempool.
      (NE.NonEmpty (GenTxId blk))
      -- | Previously valid transactions that are no longer valid because they
      -- dependend on transactions that were manually removed from the
      -- Mempool. These transactions have also been removed from the Mempool.
      --
      -- This list shares not transactions with the list of manually removed
      -- transactions.
      [Validated (GenTx blk)]
      -- | The current size of the Mempool.
      MempoolSize
  | -- | Emitted when the mempool is adjusted after the tip has changed.
    TraceMempoolSynced
      -- | How long the sync operation took.
      EnclosingTimed
  | -- | A sync is not needed, as the point at the tip of the LedgerDB and the
    -- point at the mempool are the same.
    TraceMempoolSyncNotNeeded (Point blk)
  | -- | We will try to add a transaction. Adding a transaction might need to
    -- trigger a re-sync.
    TraceMempoolAttemptingAdd (GenTx blk)
  | -- | When adding a transaction, the ledger state in the mempool was found
    -- in the LedgerDB, and therefore we can read values, even if it is not the
    -- tip of the LedgerDB. An async re-sync will be performed eventually in
    -- that case.
    TraceMempoolLedgerFound (Point blk)
  | -- | When adding a transaction, the ledger state in the mempool is gone
    -- from the LedgerDB, so we cannot read values for the new
    -- transaction. This forces an in-place re-sync.
    TraceMempoolLedgerNotFound (Point blk)
  deriving Generic

deriving instance
  ( Eq (GenTx blk)
  , Eq (Validated (GenTx blk))
  , Eq (GenTxId blk)
  , Eq (ApplyTxErr blk)
  , StandardHash blk
  ) =>
  Eq (TraceEventMempool blk)

deriving instance
  ( Show (GenTx blk)
  , Show (Validated (GenTx blk))
  , Show (GenTxId blk)
  , Show (ApplyTxErr blk)
  , StandardHash blk
  ) =>
  Show (TraceEventMempool blk)
