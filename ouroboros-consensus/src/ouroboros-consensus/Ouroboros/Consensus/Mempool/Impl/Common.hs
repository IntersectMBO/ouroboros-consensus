{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definition of common types used in "Ouroboros.Consensus.Mempool.Init",
-- "Ouroboros.Consensus.Mempool.Update" and "Ouroboros.Consensus.Mempool.Query".
module Ouroboros.Consensus.Mempool.Impl.Common (
    -- * Internal state
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
  , revalidateTxsFor
  , validateNewTransaction
    -- * Tracing
  , TraceEventMempool (..)
    -- * Conversions
  , snapshotFromIS
    -- * Ticking a ledger state
  , tickLedgerState
  ) where

import           Control.Concurrent.Class.MonadMVar (MVar, newMVar)
import           Control.Concurrent.Class.MonadSTM.Strict.TMVar (newTMVarIO)
import           Control.Monad.Trans.Except (runExcept)
import           Control.Tracer
import           Data.Foldable
import qualified Data.List.NonEmpty as NE
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended (ledgerState)
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.Capacity
import           Ouroboros.Consensus.Mempool.TxSeq (TxSeq (..), TxTicket (..))
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Internal State
-------------------------------------------------------------------------------}

-- | Internal state in the mempool
data InternalState blk = IS {
      -- | Transactions currently in the mempool
      --
      -- NOTE: the total size of the transactions in 'isTxs' may exceed the
      -- current capacity ('isCapacity'). When the capacity computed from the
      -- ledger has shrunk, we don't remove transactions from the Mempool to
      -- satisfy the new lower limit. We let the transactions get removed in
      -- the normal way: by becoming invalid w.r.t. the updated ledger state.
      -- We treat a Mempool /over/ capacity in the same way as a Mempool /at/
      -- capacity.
      isTxs          :: !(TxSeq (Validated (GenTx blk)))

      -- | The cached IDs of transactions currently in the mempool.
      --
      -- This allows one to more quickly lookup transactions by ID from a
      -- 'MempoolSnapshot' (see 'snapshotHasTx').
      --
      -- This should always be in-sync with the transactions in 'isTxs'.
    , isTxIds        :: !(Set (GenTxId blk))

      -- | The cached ledger state after applying the transactions in the
      -- Mempool against the chain's ledger state. New transactions will be
      -- validated against this ledger.
      --
      -- INVARIANT: 'isLedgerState' is the ledger resulting from applying the
      -- transactions in 'isTxs' against the ledger identified 'isTip' as tip.
    , isLedgerState  :: !(TickedLedgerState blk DiffMK)

      -- | The tip of the chain that 'isTxs' was validated against
    , isTip          :: !(Point blk)

      -- | The most recent 'SlotNo' that 'isTxs' was validated against
      --
      -- Note in particular that if the mempool is revalidated against a state S
      -- at slot s, then the state will be ticked (for now to the successor
      -- slot, see 'tickLedgerState') and 'isSlotNo' will be set to @succ s@,
      -- which is different from the slot of the original ledger state, which
      -- will remain in 'isTip'.
    , isSlotNo       :: !SlotNo

      -- | The mempool 'TicketNo' counter.
      --
      -- See 'vrLastTicketNo' for more information.
    , isLastTicketNo :: !TicketNo

      -- | Current maximum capacity of the Mempool. Result of
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
    , isCapacity     :: !MempoolCapacityBytes
    }
  deriving (Generic)

deriving instance ( NoThunks (Validated (GenTx blk))
                  , NoThunks (GenTxId blk)
                  , NoThunks (TickedLedgerState blk DiffMK)
                  , StandardHash blk
                  , Typeable blk
                  ) => NoThunks (InternalState blk)

-- | \( O(1) \). Return the number of transactions in the internal state of
-- the Mempool paired with their total size in bytes.
isMempoolSize :: InternalState blk -> MempoolSize
isMempoolSize = TxSeq.toMempoolSize . isTxs

initInternalState
  :: LedgerSupportsMempool blk
  => MempoolCapacityBytesOverride
  -> TicketNo  -- ^ Used for 'isLastTicketNo'
  -> SlotNo
  -> TickedLedgerState blk DiffMK
  -> InternalState blk
initInternalState capacityOverride lastTicketNo slot st = IS {
      isTxs          = TxSeq.Empty
    , isTxIds        = Set.empty
    , isLedgerState  = st
    , isTip          = castPoint $ getTip st
    , isSlotNo       = slot
    , isLastTicketNo = lastTicketNo
    , isCapacity     = computeMempoolCapacity st capacityOverride
    }

{-------------------------------------------------------------------------------
  Ledger Interface
-------------------------------------------------------------------------------}

-- | Abstract interface needed to run a Mempool.
data LedgerInterface m blk = LedgerInterface
    { -- | Get the current tip of the LedgerDB.
      getCurrentLedgerState :: STM m (LedgerState blk EmptyMK)
      -- | Get values at the given point on the chain. Returns Nothing if the
      -- anchor moved or if the state is not found on the ledger db.
    , getLedgerTablesAtFor
        :: Point blk
        -> [GenTx blk]
        -> m (Maybe (LedgerTables (LedgerState blk) ValuesMK))
    }

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDBLedgerInterface ::
     ( IOLike m
     , LedgerSupportsMempool blk
     )
  => ChainDB m blk -> LedgerInterface m blk
chainDBLedgerInterface chainDB = LedgerInterface
    { getCurrentLedgerState =
        ledgerState <$> ChainDB.getCurrentLedger chainDB
    , getLedgerTablesAtFor = \pt txs -> do
        let keys = castLedgerTables
                 $ foldl' (<>) emptyLedgerTables
                 $ map getTransactionKeySets txs
        fmap castLedgerTables <$> ChainDB.getLedgerTablesAtFor chainDB pt keys
    }

{-------------------------------------------------------------------------------
  Mempool environment
-------------------------------------------------------------------------------}

-- | The mempool environment captures all the associated variables wrt the
-- Mempool and is accessed by the Mempool interface on demand to perform the
-- different operations.
data MempoolEnv m blk = MempoolEnv {
      mpEnvLedger           :: LedgerInterface m blk
    , mpEnvLedgerCfg        :: LedgerConfig blk
    , mpEnvStateVar         :: StrictTMVar m (InternalState blk)
    , mpEnvAddTxsRemoteFifo :: MVar m ()
    , mpEnvAddTxsAllFifo    :: MVar m ()
    , mpEnvTracer           :: Tracer m (TraceEventMempool blk)
    , mpEnvTxSize           :: GenTx blk -> TxSizeInBytes
    , mpEnvCapacityOverride :: MempoolCapacityBytesOverride
    }

initMempoolEnv :: ( IOLike m
                  , LedgerSupportsMempool blk
                  , ValidateEnvelope blk
                  )
               => LedgerInterface m blk
               -> LedgerConfig blk
               -> MempoolCapacityBytesOverride
               -> Tracer m (TraceEventMempool blk)
               -> (GenTx blk -> TxSizeInBytes)
               -> m (MempoolEnv m blk)
initMempoolEnv ledgerInterface cfg capacityOverride tracer txSize = do
    st <- atomically $ getCurrentLedgerState ledgerInterface
    let (slot, st') = tickLedgerState cfg (ForgeInUnknownSlot st)
    isVar <- newTMVarIO $ initInternalState capacityOverride TxSeq.zeroTicketNo slot st'
    addTxRemoteFifo <- newMVar ()
    addTxAllFifo    <- newMVar ()
    return MempoolEnv
      { mpEnvLedger           = ledgerInterface
      , mpEnvLedgerCfg        = cfg
      , mpEnvStateVar         = isVar
      , mpEnvAddTxsRemoteFifo = addTxRemoteFifo
      , mpEnvAddTxsAllFifo    = addTxAllFifo
      , mpEnvTracer           = tracer
      , mpEnvTxSize           = txSize
      , mpEnvCapacityOverride = capacityOverride
      }

{-------------------------------------------------------------------------------
  Ticking the ledger state
-------------------------------------------------------------------------------}

-- | Tick the 'LedgerState' using the given 'BlockSlot'.
tickLedgerState
  :: forall blk. (UpdateLedger blk, ValidateEnvelope blk)
  => LedgerConfig     blk
  -> ForgeLedgerState blk
  -> (SlotNo, TickedLedgerState blk DiffMK)
tickLedgerState _cfg (ForgeInKnownSlot slot st) = (slot, st)
tickLedgerState  cfg (ForgeInUnknownSlot st) =
    (slot, applyChainTick cfg slot st)
  where
    -- Optimistically assume that the transactions will be included in a block
    -- in the next available slot
    --
    -- TODO: We should use time here instead
    -- <https://github.com/input-output-hk/ouroboros-network/issues/1298>
    -- Once we do, the ValidateEnvelope constraint can go.
    slot :: SlotNo
    slot = case ledgerTipSlot st of
             Origin      -> minimumPossibleSlotNo (Proxy @blk)
             NotOrigin s -> succ s

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

-- | Extend 'InternalState' with a new transaction (one which we have not
-- previously validated) that may or may not be valid in this ledger state.
validateNewTransaction
  :: (LedgerSupportsMempool blk, HasTxId (GenTx blk))
  => LedgerConfig blk
  -> (GenTx blk -> TxSizeInBytes)
  -> WhetherToIntervene
  -> GenTx blk
  -> LedgerTables (LedgerState blk) ValuesMK
  -> InternalState blk
  -> ( Either (ApplyTxErr blk) (Validated (GenTx blk))
     , InternalState blk
     )
validateNewTransaction cfg txSize wti tx values is =
    case runExcept (applyTx cfg wti isSlotNo tx $ applyDiffs values isLedgerState) of
      Left err         -> ( Left err, is )
      Right (st', vtx) ->
        ( Right vtx
        , is { isTxs          = isTxs :> TxTicket vtx nextTicketNo (txSize tx)
             , isTxIds        = Set.insert (txId tx) isTxIds
             , isLedgerState  = prependDiffs isLedgerState st'
             , isLastTicketNo = nextTicketNo
             }
        )
  where
    IS {
        isTxs
      , isTxIds
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
revalidateTxsFor
  :: (LedgerSupportsMempool blk, HasTxId (GenTx blk))
  => MempoolCapacityBytesOverride
  -> LedgerConfig blk
  -> SlotNo
  -> TickedLedgerState blk DiffMK
     -- ^ The ticked ledger state againt which txs will be revalidated
  -> LedgerTables (LedgerState blk) ValuesMK
     -- ^ The tables with all the inputs for the transactions
  -> TicketNo -- ^ 'isLastTicketNo' & 'vrLastTicketNo'
  -> [TxTicket (Validated (GenTx blk))]
  -> RevalidateTxsResult blk
revalidateTxsFor capacityOverride cfg slot st values lastTicketNo txTickets =
  let ReapplyTxsResult err val st' =
        reapplyTxs cfg slot (map txTicketTx txTickets) (values `applyDiffs` st)

      -- TODO: This is ugly, but I couldn't find a way to sneak the 'TxTicket' into
      -- 'reapplyTxs'.
      filterTxTickets _ [] = []
      filterTxTickets (t1 : t1s) t2ss@(t2 : t2s)
        | txId (txForgetValidated $ txTicketTx t1) == txId (txForgetValidated t2)
        = t1 : filterTxTickets t1s t2s
        | otherwise
        = filterTxTickets t1s t2ss
      filterTxTickets [] _ =
        error "There are less transactions given to the revalidate function than \
              \ transactions revalidated! This is unacceptable (and impossible)!"

  in RevalidateTxsResult
      (IS {
         isTxs          = foldl (:>) TxSeq.Empty $ filterTxTickets txTickets val
       , isTxIds        = Set.fromList $ map (txId . txForgetValidated) val
       , isLedgerState  = st'
       , isTip          = castPoint $ getTip st
       , isSlotNo       = slot
       , isLastTicketNo = lastTicketNo
       , isCapacity     = computeMempoolCapacity st capacityOverride
       })
       err

data RevalidateTxsResult blk =
  RevalidateTxsResult {
     -- | The internal state after revalidation
     newInternalState :: !(InternalState blk)
     -- | The previously valid transactions that were now invalid
   , removedTxs       :: ![Invalidated blk]
   }

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

-- | Create a Mempool Snapshot from a given Internal State of the mempool.
snapshotFromIS
  :: (HasTxId (GenTx blk), GetTip (Ticked1 (LedgerState blk)))
  => InternalState blk
  -> MempoolSnapshot blk
snapshotFromIS is = MempoolSnapshot {
      snapshotTxs         = implSnapshotGetTxs         is
    , snapshotTxsAfter    = implSnapshotGetTxsAfter    is
    , snapshotLookupTx    = implSnapshotGetTx          is
    , snapshotHasTx       = implSnapshotHasTx          is
    , snapshotMempoolSize = implSnapshotGetMempoolSize is
    , snapshotSlotNo      = isSlotNo                   is
    , snapshotTipHash     = pointHash $ castPoint $ getTip $ isLedgerState is
    , snapshotState       = isLedgerState is
    }
 where
  implSnapshotGetTxs :: InternalState blk
                     -> [(Validated (GenTx blk), TicketNo)]
  implSnapshotGetTxs = flip implSnapshotGetTxsAfter TxSeq.zeroTicketNo

  implSnapshotGetTxsAfter :: InternalState blk
                          -> TicketNo
                          -> [(Validated (GenTx blk), TicketNo)]
  implSnapshotGetTxsAfter IS{isTxs} =
    TxSeq.toTuples . snd . TxSeq.splitAfterTicketNo isTxs

  implSnapshotGetTx :: InternalState blk
                    -> TicketNo
                    -> Maybe (Validated (GenTx blk))
  implSnapshotGetTx IS{isTxs} = (isTxs `TxSeq.lookupByTicketNo`)

  implSnapshotHasTx :: Ord (GenTxId blk)
                    => InternalState blk
                    -> GenTxId blk
                    -> Bool
  implSnapshotHasTx IS{isTxIds} = flip Set.member isTxIds

  implSnapshotGetMempoolSize :: InternalState blk
                             -> MempoolSize
  implSnapshotGetMempoolSize = TxSeq.toMempoolSize . isTxs

{-------------------------------------------------------------------------------
  Tracing support for the mempool operations
-------------------------------------------------------------------------------}

-- | Events traced by the Mempool.
data TraceEventMempool blk
  = TraceMempoolAddedTx
      (Validated (GenTx blk))
      -- ^ New, valid transaction that was added to the Mempool.
      MempoolSize
      -- ^ The size of the Mempool before adding the transaction.
      MempoolSize
      -- ^ The size of the Mempool after adding the transaction.
  | TraceMempoolRejectedTx
      (GenTx blk)
      -- ^ New, invalid transaction thas was rejected and thus not added to
      -- the Mempool.
      (ApplyTxErr blk)
      -- ^ The reason for rejecting the transaction.
      MempoolSize
      -- ^ The current size of the Mempool.
  | TraceMempoolRemoveTxs
      [Validated (GenTx blk)]
      -- ^ Previously valid transactions that are no longer valid because of
      -- changes in the ledger state. These transactions have been removed
      -- from the Mempool.
      MempoolSize
      -- ^ The current size of the Mempool.
  | TraceMempoolManuallyRemovedTxs
      (NE.NonEmpty (GenTxId blk))
      -- ^ Transactions that have been manually removed from the Mempool.
      [Validated (GenTx blk)]
      -- ^ Previously valid transactions that are no longer valid because they
      -- dependend on transactions that were manually removed from the
      -- Mempool. These transactions have also been removed from the Mempool.
      --
      -- This list shares not transactions with the list of manually removed
      -- transactions.
      MempoolSize
      -- ^ The current size of the Mempool.
  | TraceMempoolAttemptingSync
  | TraceMempoolSyncNotNeeded (Point blk) (Point blk)
  | TraceMempoolSyncDone
  | TraceMempoolAttemptingAdd (GenTx blk)
  | TraceMempoolLedgerFound (Point blk)
  | TraceMempoolLedgerNotFound (Point blk)
  deriving (Generic)

deriving instance ( Eq (GenTx blk)
                  , Eq (Validated (GenTx blk))
                  , Eq (GenTxId blk)
                  , Eq (ApplyTxErr blk)
                  , StandardHash blk
                  ) => Eq (TraceEventMempool blk)

deriving instance ( Show (GenTx blk)
                  , Show (Validated (GenTx blk))
                  , Show (GenTxId blk)
                  , Show (ApplyTxErr blk)
                  , StandardHash blk
                  ) => Show (TraceEventMempool blk)

deriving instance ( NoThunks (GenTx blk)
                  , NoThunks (Validated (GenTx blk))
                  , NoThunks (GenTxId blk)
                  , NoThunks (ApplyTxErr blk)
                  , StandardHash blk
                  ) => NoThunks (TraceEventMempool blk)
