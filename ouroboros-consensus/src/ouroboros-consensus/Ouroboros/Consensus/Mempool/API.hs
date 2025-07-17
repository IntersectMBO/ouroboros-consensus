{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Exposes the @'Mempool'@ datatype which captures the public API of the
-- Mempool. Also exposes all the types used to interact with said API.
--
-- The interface is then initialized in "Ouroboros.Consensus.Mempool.Init" with
-- the functions from "Ouroboros.Consensus.Mempool.Update" and
-- "Ouroboros.Consensus.Mempool.Query".
module Ouroboros.Consensus.Mempool.API
  ( -- * Mempool
    Mempool (..)

    -- * Transaction adding
  , AddTxOnBehalfOf (..)
  , MempoolAddTxResult (..)
  , addLocalTxs
  , addTxs
  , isMempoolTxAdded
  , isMempoolTxRejected
  , mempoolTxAddedToMaybe

    -- * Ledger state to forge on top of
  , ForgeLedgerState (..)

    -- * Mempool Snapshot
  , MempoolSnapshot (..)

    -- * Re-exports
  , SizeInBytes
  , TicketNo
  , zeroTicketNo
  ) where

import Control.ResourceRegistry
import qualified Data.List.NonEmpty as NE
import Ouroboros.Consensus.Block (ChainHash, Point, SlotNo)
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import qualified Ouroboros.Consensus.Mempool.Capacity as Cap
import Ouroboros.Consensus.Mempool.TxSeq (TicketNo, zeroTicketNo)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Protocol.TxSubmission2.Type (SizeInBytes)

{-------------------------------------------------------------------------------
  Mempool API
-------------------------------------------------------------------------------}

-- | Mempool
--
-- The mempool is the set of transactions that should be included in the next
-- block. In principle this is a /set/ of all the transactions that we receive
-- from our peers. In order to avoid flooding the network with invalid
-- transactions, however, we only want to keep /valid/ transactions in the
-- mempool. That raises the question: valid with respect to which ledger state?
--
-- We opt for a very simple answer to this: the mempool will be interpreted
-- as a /list/ of transactions; which are validated strictly in order, starting
-- from the current ledger state. This has a number of advantages:
--
-- * It's simple to implement and it's efficient. In particular, no search for
--   a valid subset is ever required.
-- * When producing a block, we can simply take the longest possible prefix
--   of transactions that fits in a block.
-- * It supports wallets that submit dependent transactions (where later
--   transaction depends on outputs from earlier ones).
--
-- The mempool provides fairness guarantees for the case of multiple threads
-- performing 'addTx' concurrently. Implementations of this interface must
-- provide this guarantee, and users of this interface may rely on it.
-- Specifically, multiple threads that continuously use 'addTx' will, over
-- time, get a share of the mempool resource (measured by the number of txs
-- only, not their sizes) roughly proportional to their \"weight\". The weight
-- depends on the 'AddTxOnBehalfOf': either acting on behalf of remote peers
-- ('AddTxForRemotePeer') or on behalf of a local client
-- ('AddTxForLocalClient'). The weighting for threads acting on behalf of
-- remote peers is the same for all remote peers, so all remote peers will get
-- a roughly equal share of the resource. The weighting for local clients is
-- the same for all local clients but /may/ be higher than the weighting for
-- remote peers. The weighting is not unboundedly higher however, so there is
-- still (weighted) fairness between remote peers and local clients. Thus
-- local clients will also get a roughly equal share of the resource, but that
-- share may be strictly greater than the share for each remote peer.
-- Furthermore, this implies local clients cannot starve remote peers, despite
-- their higher weighting.
--
-- This fairness specification in terms of weighting is deliberately
-- non-specific, which allows multiple strategies. The existing default
-- strategy (for the implementation in "Ouroboros.Consensus.Mempool") is as
-- follows. The design uses two FIFOs, to give strictly in-order behaviour.
-- All remote peers get equal weight and all local clients get equal weight.
-- The relative weight between remote and local is that if there are N remote
-- peers and M local clients, each local client gets weight 1/(M+1), while all
-- of the N remote peers together also get total weight 1/(M+1). This means
-- individual remote peers get weight 1/(N * (M+1)). Intuitively: a single local
-- client has the same weight as all the remote peers put together.
data Mempool m blk = Mempool
  { addTx ::
      AddTxOnBehalfOf ->
      GenTx blk ->
      m (MempoolAddTxResult blk)
  -- ^ Add a single transaction to the mempool.
  --
  -- The new transaction provided will be validated, /in order/, against
  -- the ledger state obtained by applying all the transactions already in
  -- the mempool. Transactions which are found to be invalid are dropped,
  -- whereas valid transactions are added to the mempool.
  --
  -- Note that transactions that are invalid will /never/ be added to the
  -- mempool. However, it is possible that, at a given point in time,
  -- transactions which were valid in an older ledger state but are invalid in
  -- the current ledger state, could exist within the mempool until they are
  -- revalidated and dropped from the mempool via a call to by the background
  -- thread that watches the ledger for changes or by 'testSyncWithLedger' in
  -- testing scenarios.
  --
  -- This action returns one of two results.
  --
  --  * A 'MempoolTxAdded' value if the transaction provided was found to
  --    be valid. This transactions is now in the mempool.
  --
  --  * A 'MempoolTxRejected' value if the transaction provided was found
  --    to be invalid, along with its accompanying validation errors. This
  --    transactions is not in the mempool.
  --
  -- Note that this is a blocking action. It will block until the
  -- transaction fits into the mempool. This includes transactions that
  -- turn out to be invalid: the action waits for there to be space for
  -- the transaction before validation is attempted.
  --
  -- Note that it is safe to use this from multiple threads concurrently.
  --
  -- POSTCONDITION:
  -- > let prj = \case
  -- >       MempoolTxAdded vtx        -> txForgetValidated vtx
  -- >       MempoolTxRejected tx _err -> tx
  -- > processed <- addTx wti txs
  -- > prj processed == tx
  --
  -- In principle it is possible that validation errors are transient; for
  -- example, it is possible that a transaction is rejected because one of
  -- its inputs is not /yet/ available in the UTxO (the transaction it
  -- depends on is not yet in the chain, nor in the mempool). In practice
  -- however it is likely that rejected transactions will still be
  -- rejected later, and should just be dropped.
  --
  -- It is important to note one important special case of transactions
  -- being "invalid": a transaction will /also/ be considered invalid if
  -- /that very same transaction/ is already included on the blockchain
  -- (after all, by definition that must mean its inputs have been used).
  -- Rejected transactions are therefore not necessarily a sign of
  -- malicious behaviour. Indeed, we would expect /most/ transactions that
  -- are reported as invalid by 'addTxs' to be invalid precisely because
  -- they have already been included. Distinguishing between these two
  -- cases can be done in theory, but it is expensive unless we have an
  -- index of transaction hashes that have been included on the blockchain.
  --
  -- As long as we keep the mempool entirely in-memory this could live in
  -- @STM m@; we keep it in @m@ instead to leave open the possibility of
  -- persistence.
  , removeTxsEvenIfValid :: NE.NonEmpty (GenTxId blk) -> m ()
  -- ^ Manually remove the given transactions from the mempool.
  , getSnapshot :: STM m (MempoolSnapshot blk)
  -- ^ Get a snapshot of the current mempool state. This allows for
  -- further pure queries on the snapshot.
  --
  -- This doesn't look at the ledger state at all.
  , getSnapshotFor ::
      SlotNo ->
      TickedLedgerState blk DiffMK ->
      (LedgerTables (LedgerState blk) KeysMK -> m (LedgerTables (LedgerState blk) ValuesMK)) ->
      m (MempoolSnapshot blk)
  -- ^ Get a snapshot of the mempool state that is valid with respect to
  -- the given ledger state
  --
  -- This does not update the state of the mempool.
  --
  -- The arguments:
  --
  -- - The current slot in which we want the snapshot
  --
  -- - The ledger state ticked to the given slot number (with the diffs from ticking)
  --
  -- - A function that reads values for keys at the unticked ledger state.
  , getCapacity :: STM m (TxMeasure blk)
  -- ^ Get the mempool's capacity
  --
  -- Note that the capacity of the Mempool, unless it is overridden with
  -- 'MempoolCapacityBytesOverride', can dynamically change when the ledger
  -- state is updated: it will be set to twice the current ledger's maximum
  -- transaction capacity of a block.
  --
  -- When the capacity happens to shrink at some point, we /do not/ remove
  -- transactions from the Mempool to satisfy this new lower limit.
  -- Instead, we treat it the same way as a Mempool which is /at/
  -- capacity, i.e., we won't admit new transactions until some have been
  -- removed because they have become invalid.
  , testSyncWithLedger :: m (MempoolSnapshot blk)
  -- ^ ONLY FOR TESTS
  --
  -- Sync the transactions in the mempool with the current ledger state
  --  of the 'ChainDB'.
  --
  -- The transactions that exist within the mempool will be revalidated
  -- against the current ledger state. Transactions which are found to be
  -- invalid with respect to the current ledger state, will be dropped
  -- from the mempool, whereas valid transactions will remain.
  --
  -- We keep this in @m@ instead of @STM m@ to leave open the possibility
  -- of persistence. Additionally, this makes it possible to trace the
  -- removal of invalid transactions.
  --
  -- n.b. in our current implementation, when one opens a mempool, we
  -- spawn a thread which performs this action whenever the 'ChainDB' tip
  -- point changes.
  , testForkMempoolThread :: forall a. String -> m a -> m (Thread m a)
  -- ^ FOR TESTS ONLY
  --
  -- If we want to run a thread that can perform syncs in the mempool, it needs
  -- to be registered in the mempool's internal registry. This function exposes
  -- such functionality.
  --
  -- The 'String' passed will be used as the thread label, and the @m a@ will be
  -- the action forked in the thread.
  }

{-------------------------------------------------------------------------------
  Result of adding a transaction to the mempool
-------------------------------------------------------------------------------}

-- | The result of attempting to add a transaction to the mempool.
data MempoolAddTxResult blk
  = -- | The transaction was added to the mempool.
    MempoolTxAdded !(Validated (GenTx blk))
  | -- | The transaction was rejected and could not be added to the mempool
    -- for the specified reason.
    MempoolTxRejected !(GenTx blk) !(ApplyTxErr blk)

deriving instance
  (Eq (GenTx blk), Eq (Validated (GenTx blk)), Eq (ApplyTxErr blk)) => Eq (MempoolAddTxResult blk)
deriving instance
  (Show (GenTx blk), Show (Validated (GenTx blk)), Show (ApplyTxErr blk)) =>
  Show (MempoolAddTxResult blk)

mempoolTxAddedToMaybe :: MempoolAddTxResult blk -> Maybe (Validated (GenTx blk))
mempoolTxAddedToMaybe (MempoolTxAdded vtx) = Just vtx
mempoolTxAddedToMaybe _ = Nothing

isMempoolTxAdded :: MempoolAddTxResult blk -> Bool
isMempoolTxAdded MempoolTxAdded{} = True
isMempoolTxAdded _ = False

isMempoolTxRejected :: MempoolAddTxResult blk -> Bool
isMempoolTxRejected MempoolTxRejected{} = True
isMempoolTxRejected _ = False

-- | A wrapper around 'addTx' that adds a sequence of transactions on behalf of
-- a remote peer.
--
-- Note that transactions are added one by one, and can interleave with other
-- concurrent threads using 'addTx'.
--
-- See 'addTx' for further details.
addTxs ::
  forall m blk t.
  (MonadSTM m, Traversable t) =>
  Mempool m blk ->
  t (GenTx blk) ->
  m (t (MempoolAddTxResult blk))
addTxs mempool = mapM (addTx mempool AddTxForRemotePeer)

-- | A wrapper around 'addTx' that adds a sequence of transactions on behalf of
-- a local client. This reports more errors for local clients, see 'Intervene'.
--
-- Note that transactions are added one by one, and can interleave with other
-- concurrent threads using 'addTx'.
--
-- See 'addTx' for further details.
addLocalTxs ::
  forall m blk t.
  (MonadSTM m, Traversable t) =>
  Mempool m blk ->
  t (GenTx blk) ->
  m (t (MempoolAddTxResult blk))
addLocalTxs mempool = mapM (addTx mempool AddTxForLocalClient)

-- | Who are we adding a tx on behalf of, a remote peer or a local client?
--
-- This affects two things:
--
-- 1. how certain errors are treated: we want to be helpful to local clients.
-- 2. priority of service: local clients are prioritised over remote peers.
--
-- See 'Mempool' for a discussion of fairness and priority.
data AddTxOnBehalfOf = AddTxForRemotePeer | AddTxForLocalClient

{-------------------------------------------------------------------------------
  Ledger state considered for forging
-------------------------------------------------------------------------------}

-- | The ledger state wrt to which we should produce a block
--
-- The transactions in the mempool will be part of the body of a block, but a
-- block consists of a header and a body, and the full validation of a block
-- consists of first processing its header and only then processing the body.
-- This is important, because processing the header may change the state of the
-- ledger: the update system might be updated, scheduled delegations might be
-- applied, etc., and such changes should take effect before we validate any
-- transactions.
data ForgeLedgerState blk
  = -- | The slot number of the block is known
    --
    -- This will only be the case when we realized that we are the slot leader
    -- and we are actually producing a block. It is the caller's responsibility
    -- to call 'applyChainTick' and produce the ticked ledger state.
    ForgeInKnownSlot SlotNo (TickedLedgerState blk DiffMK)
  | -- | The slot number of the block is not yet known
    --
    -- When we are validating transactions before we know in which block they
    -- will end up, we have to make an assumption about which slot number to use
    -- for 'applyChainTick' to prepare the ledger state; we will assume that
    -- they will end up in the slot after the slot at the tip of the ledger.
    ForgeInUnknownSlot (LedgerState blk EmptyMK)

{-------------------------------------------------------------------------------
  Snapshot of the mempool
-------------------------------------------------------------------------------}

-- | A pure snapshot of the contents of the mempool. It allows fetching
-- information about transactions in the mempool, and fetching individual
-- transactions.
--
-- This uses a transaction sequence number type for identifying transactions
-- within the mempool sequence. The sequence number is local to this mempool,
-- unlike the transaction hash. This allows us to ask for all transactions
-- after a known sequence number, to get new transactions. It is also used to
-- look up individual transactions.
--
-- Note that it is expected that 'getTx' will often return 'Nothing'
-- even for tx sequence numbers returned in previous snapshots. This happens
-- when the transaction has been removed from the mempool between snapshots.
data MempoolSnapshot blk = MempoolSnapshot
  { snapshotTxs :: [(Validated (GenTx blk), TicketNo, TxMeasure blk)]
  -- ^ Get all transactions (oldest to newest) in the mempool snapshot along
  -- with their ticket number.
  , snapshotTxsAfter :: TicketNo -> [(Validated (GenTx blk), TicketNo, TxMeasure blk)]
  -- ^ Get all transactions (oldest to newest) in the mempool snapshot,
  -- along with their ticket number, which are associated with a ticket
  -- number greater than the one provided.
  , snapshotTake :: TxMeasure blk -> [Validated (GenTx blk)]
  -- ^ Get the greatest prefix (oldest to newest) that respects the given
  -- block capacity.
  , snapshotLookupTx :: TicketNo -> Maybe (Validated (GenTx blk))
  -- ^ Get a specific transaction from the mempool snapshot by its ticket
  -- number, if it exists.
  , snapshotHasTx :: GenTxId blk -> Bool
  -- ^ Determine whether a specific transaction exists within the mempool
  -- snapshot.
  , snapshotMempoolSize :: Cap.MempoolSize
  -- ^ Get the size of the mempool snapshot.
  , snapshotSlotNo :: SlotNo
  -- ^ The block number of the "virtual block" under construction
  , snapshotStateHash :: ChainHash (TickedLedgerState blk)
  -- ^ The resulting state currently in the mempool after applying the
  -- transactions
  , snapshotPoint :: Point blk
  }
