{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
  , ValidatedTxWithDiffs (..)
  , isMempoolSize

    -- * Mempool environment
  , MempoolEnv (..)
  , initMempoolEnv

    -- * Ledger interface
  , LedgerInterface (..)
  , MempoolLedgerDBView (..)
  , chainDBLedgerInterface

    -- * Validation
  , RevalidateTxsResult (..)
  , computeSnapshot
  , revalidateTxsFor
  , validateNewTransaction

    -- * Tracing
  , MempoolRejectionDetails (..)
  , TraceEventMempool (..)
  , jsonMempoolRejectionDetails

    -- * Conversions
  , snapshotFromValidTxs

    -- * Ticking a ledger state
  , tickLedgerState
  ) where

import Control.Concurrent.Class.MonadSTM.Strict.TMVar (newTMVarIO)
import Control.Monad.Trans.Except (runExceptT)
import Control.Tracer
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import Data.Bifunctor (second)
import qualified Data.Foldable as Foldable
import Data.Functor ((<&>))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Typeable
import GHC.Generics (Generic)
import NoThunks.Class
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
import Ouroboros.Consensus.Storage.LedgerDB.Forker
import Ouroboros.Consensus.Util.Enclose (EnclosingTimed)
import Ouroboros.Consensus.Util.IOLike hiding (newMVar)
import Ouroboros.Consensus.Util.NormalForm.StrictMVar
import Ouroboros.Network.Protocol.LocalStateQuery.Type

{-------------------------------------------------------------------------------
  Internal State
-------------------------------------------------------------------------------}

-- | We cache the differences produced by validating each transaction, as the
-- current differences in UTxO-HD do not depend on which block the transaction
-- was validated. This means that these differences cannot be "stale", as long
-- as the transaction is considered, the differences will be the same. If we
-- extend UTxO-HD to consider more differences, this might be violated and we
-- will have to reconsider what we can cache and what we can't.
data ValidatedTxWithDiffs blk = ValidatedTxWithDiffs
  { validatedTx :: !(Validated (GenTx blk))
  }
  deriving Generic

deriving instance
  NoThunks (Validated (GenTx blk)) =>
  NoThunks (ValidatedTxWithDiffs blk)

-- | Internal state in the mempool
data InternalState m blk = IS
  { isTxs :: !(TxSeq (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk))
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
  , isLedgerState :: !(TickedLedgerState m blk)
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
  , NoThunks (TickedLedgerState m blk)
  , NoThunks (TxMeasure blk)
  , StandardHash blk
  , Typeable blk
  ) =>
  NoThunks (InternalState m blk)

-- | \( O(1) \). Return the number of transactions in the internal state of
-- the Mempool paired with their total size in bytes.
isMempoolSize :: TxLimits blk => InternalState m blk -> MempoolSize
isMempoolSize is =
  MempoolSize
    { msNumTxs = fromIntegral $ length $ isTxs is
    , msNumBytes = txMeasureByteSize $ forgetTxMeasureWithDiffTime $ TxSeq.toSize $ isTxs is
    }

initInternalState ::
  LedgerSupportsMempool blk =>
  MempoolCapacityBytesOverride ->
  -- | Used for 'isLastTicketNo'
  TicketNo ->
  LedgerConfig blk ->
  SlotNo ->
  TickedLedgerState m blk ->
  InternalState m blk
initInternalState capacityOverride lastTicketNo cfg slot st =
  IS
    { isTxs = TxSeq.Empty
    , isTxIds = Set.empty
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
newtype LedgerInterface m blk = LedgerInterface
  { getCurrentLedgerState :: STM m (MempoolLedgerDBView m blk)
  }

data MempoolLedgerDBView m blk = MempoolLedgerDBView
  { mldViewState :: LedgerState m blk
  -- ^ The ledger state currently at the tip of the LedgerDB
  }

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDBLedgerInterface ::
  (IOLike m, IsLedger LedgerState blk) =>
  ChainDB m blk ->
  LedgerInterface m blk
chainDBLedgerInterface chainDB =
  LedgerInterface
    { getCurrentLedgerState =
        MempoolLedgerDBView . ledgerState <$> ChainDB.getCurrentLedger chainDB
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
  , mpEnvStateVar :: StrictTMVar m (InternalState m blk)
  , mpEnvAddTxsRemoteFifo :: StrictMVar m ()
  , mpEnvAddTxsAllFifo :: StrictMVar m ()
  , mpEnvTracer :: Tracer m (TraceEventMempool blk)
  , mpEnvCapacityOverride :: MempoolCapacityBytesOverride
  , mpEnvTimeoutConfig :: Maybe MempoolTimeoutConfig
  }

initMempoolEnv ::
  ( IOLike m
  , LedgerSupportsMempool blk
  , ValidateEnvelope blk
  ) =>
  LedgerInterface m blk ->
  LedgerConfig blk ->
  MempoolCapacityBytesOverride ->
  Maybe MempoolTimeoutConfig ->
  Tracer m (TraceEventMempool blk) ->
  m (MempoolEnv m blk)
initMempoolEnv ledgerInterface cfg capacityOverride mbTimeoutConfig tracer = do
  MempoolLedgerDBView st <- atomically $ getCurrentLedgerState ledgerInterface
  (slot, st') <- tickLedgerState cfg (ForgeInUnknownSlot st)
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
      , mpEnvTimeoutConfig = mbTimeoutConfig
      }

{-------------------------------------------------------------------------------
  Ticking the ledger state
-------------------------------------------------------------------------------}

-- | Tick the 'LedgerState' using the given 'BlockSlot'.
tickLedgerState ::
  forall m blk.
  (UpdateLedger blk, ValidateEnvelope blk) =>
  LedgerConfig blk ->
  ForgeLedgerState m blk ->
  m (SlotNo, TickedLedgerState m blk)
tickLedgerState _cfg (ForgeInKnownSlot slot st) = pure (slot, st)
tickLedgerState cfg (ForgeInUnknownSlot st) =
  (\x -> (slot, x)) <$> applyChainTick OmitLedgerEvents cfg slot st
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
  -- | This state is the internal state with the tables for this transaction
  -- advanced through the diffs in the internal state. One could think we can
  -- create this value here, but it is needed for some other uses like calling
  -- 'txMeasure' before this function.
  TickedLedgerState m blk ->
  InternalState m blk ->
  m
    ( Either (ApplyTxErr blk) (Validated (GenTx blk))
    , DiffTimeMeasure -> InternalState m blk
    )
validateNewTransaction cfg wti tx txsz st is =
  runExceptT (applyTx cfg wti isSlotNo tx st) <&> \case
    Left err -> (Left err, \_dur -> is)
    Right (st', vtx) ->
      ( Right vtx
      , \dur ->
          is
            { isTxs =
                isTxs
                  :> TxTicket
                    (ValidatedTxWithDiffs vtx)
                    nextTicketNo
                    (MkTxMeasureWithDiffTime txsz dur)
            , isTxIds = Set.insert (txId tx) isTxIds
            , isLedgerState = st'
            , isLastTicketNo = nextTicketNo
            }
      )
 where
  IS
    { isTxs
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
revalidateTxsFor ::
  forall m blk.
  (LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  MempoolCapacityBytesOverride ->
  LedgerConfig blk ->
  SlotNo ->
  -- | The ticked ledger state againt which txs will be revalidated
  TickedLedgerState m blk ->
  -- | 'isLastTicketNo' and 'vrLastTicketNo'
  TicketNo ->
  [TxTicket (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)] ->
  m (InternalState m blk, [GenTx blk])
revalidateTxsFor capacityOverride cfg slot st lastTicketNo txTickets = do
  let inputTxs = map wrap txTickets

  (st', err) <- undefined $ reapplyTx @blk @m cfg slot undefined -- inputTxs st
  pure
    ( IS
        { isTxs = TxSeq.fromList $ map unwrap undefined -- validTxs
        , isTxIds = Set.fromList $ map (txId . txForgetValidated . fst3) undefined -- validTxs
        , isLedgerState = st'
        , isTip = castPoint $ getTip st
        , isSlotNo = slot
        , isLastTicketNo = lastTicketNo
        , isCapacity = computeMempoolCapacity cfg st' capacityOverride
        }
    , [undefined]
    )
 where
  wrap = \(TxTicket (ValidatedTxWithDiffs tx) tk tz) -> (tx, (tk, tz))
  unwrap = \(tx, (tk, tz)) -> TxTicket (ValidatedTxWithDiffs tx) tk tz
  fst3 (x, _, _) = x
  snd3 (_, x, _) = x

-- | Compute snapshot is largely the same as revalidate the transactions
-- but we ignore the diffs.
computeSnapshot ::
  forall m blk.
  (LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  LedgerConfig blk ->
  SlotNo ->
  -- | The ticked ledger state againt which txs will be revalidated
  TickedLedgerState m blk ->
  [TxTicket (TxMeasureWithDiffTime blk) (Validated (GenTx blk))] ->
  MempoolSnapshot blk
computeSnapshot cfg slot st values txTickets =
  let inputTxs = map wrap txTickets
      inputKeys = Foldable.foldMap' (getTransactionKeySets . txForgetValidated . fst3) inputTxs
   in snapshotFromValidTxs
        ( map unwrap $
            validatedTxs $
              reapplyTxs @blk @Discard cfg slot inputTxs $
                applyMempoolDiffs values inputKeys st
        )
        (castPoint $ getTip st)
        slot
 where
  fst3 (x, _, _) = x
  wrap = (\(TxTicket tx tk tz) -> (tx, (), (tk, tz)))
  unwrap = (\(tx, (), (tk, tz)) -> (TxTicket tx tk tz))

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

-- | Create a Mempool Snapshot from a given Internal State of the mempool.
snapshotFromValidTxs ::
  forall blk.
  (LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  [TxTicket (TxMeasureWithDiffTime blk) (Validated (GenTx blk))] ->
  Point blk ->
  SlotNo ->
  MempoolSnapshot blk
snapshotFromValidTxs validTxs tipPoint slot =
  MempoolSnapshot
    { snapshotTxs = implSnapshotGetTxs
    , snapshotTxsAfter = implSnapshotGetTxsAfter
    , snapshotLookupTx = implSnapshotGetTx
    , snapshotHasTx = implSnapshotHasTx
    , snapshotMempoolSize = implSnapshotGetMempoolSize
    , snapshotSlotNo = slot
    , snapshotStateHash = pointHash tipPoint
    , snapshotTake = implSnapshotTake
    , snapshotPoint = castPoint tipPoint
    }
 where
  txs = TxSeq.fromList validTxs
  txIds = Set.fromList $ map (txId . txForgetValidated . txTicketTx) validTxs

  implSnapshotGetTxs ::
    [(Validated (GenTx blk), TicketNo, TxMeasure blk)]
  implSnapshotGetTxs = implSnapshotGetTxsAfter TxSeq.zeroTicketNo

  implSnapshotGetTxsAfter ::
    TicketNo ->
    [(Validated (GenTx blk), TicketNo, TxMeasure blk)]
  implSnapshotGetTxsAfter =
    (\x -> [(a, b, forgetTxMeasureWithDiffTime c) | (a, b, c) <- x])
      . TxSeq.toTuples
      . snd
      . TxSeq.splitAfterTicketNo txs

  implSnapshotTake ::
    TxMeasure blk ->
    ([Validated (GenTx blk)], TxMeasureWithDiffTime blk)
  implSnapshotTake limit =
    (map TxSeq.txTicketTx (TxSeq.toList x), TxSeq.toSize x)
   where
    (x, _y) = TxSeq.splitAfterTxSize txs $ MkTxMeasureWithDiffTime limit InfiniteDiffTimeMeasure

  implSnapshotGetTx ::
    TicketNo ->
    Maybe (Validated (GenTx blk))
  implSnapshotGetTx = (txs `TxSeq.lookupByTicketNo`)

  implSnapshotHasTx ::
    GenTxId blk ->
    Bool
  implSnapshotHasTx = (`Set.member` txIds)

  implSnapshotGetMempoolSize ::
    MempoolSize
  implSnapshotGetMempoolSize =
    MempoolSize
      { msNumTxs = fromIntegral $ length $ txs
      , msNumBytes = txMeasureByteSize $ forgetTxMeasureWithDiffTime $ TxSeq.toSize $ txs
      }

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
      -- | More details about the reason
      MempoolRejectionDetails
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
  | -- | We will try to add a transaction.
    TraceMempoolAttemptingAdd (GenTx blk)
  | -- | When performing a re-sync we will read the LedgerDB tip twice. This
    -- trace will be emitted if in between those two steps the LedgerDB moved to
    -- an alternative fork. It is completely innocuous but we would like to
    -- double check that it happens very rarely or almost never.
    TraceMempoolTipMovedBetweenSTMBlocks
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

data MempoolRejectionDetails
  = -- | The ledger's @MEMPOOL@ rule rejected the tx
    MempoolRejectedByLedger
  | -- | The tx violated 'mempoolTimeoutSoft'
    --
    -- It did not violate 'mempoolTimeoutHard', since that would raise an
    -- exception instead of merely rejecting the tx (not even constructing a
    -- 'MempoolTxRejected').
    MempoolRejectedByTimeoutSoft !DiffTime
  deriving (Eq, Show)

jsonMempoolRejectionDetails :: MempoolRejectionDetails -> Aeson.Value
jsonMempoolRejectionDetails = \case
  MempoolRejectedByLedger ->
    Aeson.String
      (Text.pack "MempoolRejectedByLedger")
  MempoolRejectedByTimeoutSoft dt ->
    Aeson.object
      [AesonKey.fromString "MempoolRejectedByTimeoutSoft" Aeson..= dt]
