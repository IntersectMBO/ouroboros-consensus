{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

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
  , MempoolRejectionDetails (..)
  , TraceEventMempool (..)
  , jsonMempoolRejectionDetails

    -- * Conversions
  , snapshotFromValidTxs

    -- * Ticking a ledger state
  , tickLedgerState
  ) where

import Control.Concurrent.Class.MonadSTM.Strict.TMVar (newTMVarIO)
import Control.Monad (foldM)
import Control.Monad.Except (runExcept)
import Control.Tracer
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Foldable as Foldable
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
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Mempool.API
import Ouroboros.Consensus.Mempool.Capacity
import Ouroboros.Consensus.Mempool.TxSeq (TxSeq (..), TxTicket (..))
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Util.Enclose (EnclosingTimed)
import Ouroboros.Consensus.Util.IOLike hiding (newMVar)
import Ouroboros.Consensus.Util.NormalForm.StrictMVar

{-------------------------------------------------------------------------------
  Internal State
-------------------------------------------------------------------------------}

-- | Internal state in the mempool
data InternalState blk = IS
  { isTxs :: !(TxSeq (TxMeasureWithDiffTime blk) (Validated (GenTx blk)))
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
  , isCache :: !(MempoolCache blk)
  -- ^ Per-block pre-read cache of the UTxO values needed by the
  -- transactions currently in the mempool, plus the diffs each
  -- transaction introduces.
  --
  -- The cached differences produced by validating each transaction do
  -- not depend on which block the transaction was validated against, so
  -- as long as a tx remains in the mempool its cached contribution
  -- stays valid. (If UTxO-HD is later extended to consider more
  -- differences, this invariant might be violated and the cache will
  -- need reconsidering.)
  , isLedgerState :: !(TickedLedgerState blk)
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
  , NoThunks (TickedLedgerState blk)
  , NoThunks (TxMeasure blk)
  , NoThunks (MempoolCache blk)
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
    , msNumBytes = txMeasureByteSize $ forgetTxMeasureWithDiffTime $ TxSeq.toSize $ isTxs is
    }

initInternalState ::
  (MonadLedger m blk, LedgerSupportsMempool blk) =>
  MempoolCapacityBytesOverride ->
  -- | Used for 'isLastTicketNo'
  TicketNo ->
  LedgerConfig blk ->
  SlotNo ->
  TickedStateHandle m blk ->
  InternalState blk
initInternalState capacityOverride lastTicketNo cfg slot stref@(tickedState -> st) =
  IS
    { isTxs = TxSeq.Empty
    , isTxIds = Set.empty
    , isLedgerState = st
    , isTip = getTip st
    , isSlotNo = slot
    , isLastTicketNo = lastTicketNo
    , isCapacity = computeMempoolCapacity cfg st capacityOverride
    , isCache = mkMempoolCache stref
    }

{-------------------------------------------------------------------------------
  Ledger Interface
-------------------------------------------------------------------------------}

-- | Abstract interface needed to run a Mempool.
newtype LedgerInterface m blk = LedgerInterface
  { getCurrentLedgerState :: STM m (StateHandle m blk)
  }

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDBLedgerInterface ::
  (IOLike m, IsLedger LedgerState blk) =>
  ChainDB m blk ->
  LedgerInterface m blk
chainDBLedgerInterface chainDB =
  LedgerInterface
    { getCurrentLedgerState =
        extStateHandle <$> ChainDB.getCurrentLedgerRef chainDB
    }

{-------------------------------------------------------------------------------
  Mempool environment
-------------------------------------------------------------------------------}

-- | The mempool environment captures all the associated variables wrt the
-- Mempool and is accessed by the Mempool interface on demand to perform the
-- different operations.
data MempoolEnv m blk = MempoolEnv
  { mpEnvLedger :: LedgerInterface m blk
  , mpEnvTickedHandle :: StrictMVar m (TickedStateHandle m blk)
  -- ^ The currently-cached ticked state handle. Swapped (and the
  -- previous one closed) whenever the mempool detects that the
  -- ledger tip has moved and revalidates against the new state.
  , mpEnvLedgerCfg :: LedgerConfig blk
  , mpEnvStateVar :: StrictTMVar m (InternalState blk)
  , mpEnvAddTxsRemoteFifo :: StrictMVar m ()
  , mpEnvAddTxsAllFifo :: StrictMVar m ()
  , mpEnvTracer :: Tracer m (TraceEventMempool blk)
  , mpEnvCapacityOverride :: MempoolCapacityBytesOverride
  , mpEnvTimeoutConfig :: Maybe MempoolTimeoutConfig
  }

initMempoolEnv ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsMempool blk
  , ValidateEnvelope blk
  , MonadLedger m blk
  , NoThunks (TickedStateHandle m blk)
  ) =>
  LedgerInterface m blk ->
  LedgerConfig blk ->
  MempoolCapacityBytesOverride ->
  Maybe MempoolTimeoutConfig ->
  Tracer m (TraceEventMempool blk) ->
  m (MempoolEnv m blk)
initMempoolEnv ledgerInterface cfg capacityOverride mbTimeoutConfig tracer = do
  st <- atomically $ getCurrentLedgerState ledgerInterface
  (slot, ts) <- tickLedgerState cfg (ForgeInUnknownSlot st)
  tsMVar <- newMVar ts
  isVar <-
    newTMVarIO $
      initInternalState capacityOverride TxSeq.zeroTicketNo cfg slot ts
  addTxRemoteFifo <- newMVar ()
  addTxAllFifo <- newMVar ()
  return
    MempoolEnv
      { mpEnvLedger = ledgerInterface
      , mpEnvLedgerCfg = cfg
      , mpEnvTickedHandle = tsMVar
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
  ( MonadLedger m blk
  , UpdateLedger blk
  , ValidateEnvelope blk
  ) =>
  LedgerConfig blk ->
  ForgeLedgerState m blk ->
  m (SlotNo, TickedStateHandle m blk)
tickLedgerState _cfg (ForgeInKnownSlot slot st) = pure (slot, st)
tickLedgerState cfg (ForgeInUnknownSlot st) = do
  ts <- applyChainTick OmitLedgerEvents cfg slot st
  pure (slot, ts)
 where
  -- Optimistically assume that the transactions will be included in a block
  -- in the next available slot
  --
  -- TODO: We should use time here instead
  -- <https://github.com/IntersectMBO/ouroboros-network/issues/1298>
  -- Once we do, the ValidateEnvelope constraint can go.
  slot :: SlotNo
  slot = case ledgerTipSlot $ state st of
    Origin -> minimumPossibleSlotNo (Proxy @blk)
    NotOrigin s -> succ s

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

-- | Extend 'InternalState' with a new transaction (one which we have not
-- previously validated) that may or may not be valid in this ledger state.
--
-- Pure: 'applyTx' is pure and the cache passed in is expected to have
-- been pre-loaded by the caller via 'addToCache'.
validateNewTransaction ::
  (LedgerSupportsMempool blk, MonadLedger m blk, HasTxId (GenTx blk)) =>
  LedgerConfig blk ->
  WhetherToIntervene ->
  GenTx blk ->
  TxMeasure blk ->
  -- | Cache populated by 'addToCache' for this tx.
  MempoolCache blk ->
  -- | The ticked ledger handle backing the cache.
  TickedStateHandle m blk ->
  InternalState blk ->
  ( Either (ApplyTxErr blk) (Validated (GenTx blk), TickedStateHandle m blk)
  , DiffTimeMeasure -> InternalState blk
  )
validateNewTransaction cfg wti tx txsz cache st is =
  case runExcept (applyTx cfg wti isSlotNo tx cache st) of
    Left err -> (Left err, \_dur -> is)
    Right (st', cache', vtx) ->
      ( Right (vtx, st')
      , \dur ->
          is
            { isTxs =
                isTxs
                  :> TxTicket
                    vtx
                    nextTicketNo
                    (MkTxMeasureWithDiffTime txsz dur)
            , isTxIds = Set.insert (txId tx) isTxIds
            , isCache = cache'
            , isLastTicketNo = nextTicketNo
            }
      )
 where
  IS
    { isTxs
    , isTxIds
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
--
-- The 'MempoolCache' argument is the starting cache for the fold. Callers
-- choose what to pass:
--
-- * @doSyncWithLedger@ — ticked state changed, so values may be stale:
--   pass @'mkMempoolCache' st0@ (fresh).
-- * @doRemoveTxs@ — ticked state unchanged, only some txs being
--   evicted: pass the existing cache with the dropped txs removed via
--   'forgetTxFromCache'. This avoids re-reading the kept txs' values
--   from disk.
revalidateTxsFor ::
  forall m blk.
  ( LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  , MonadLedger m blk
  ) =>
  MempoolCapacityBytesOverride ->
  LedgerConfig blk ->
  SlotNo ->
  -- | The ticked ledger state against which txs will be revalidated
  TickedStateHandle m blk ->
  -- | Starting cache for the revalidation fold.
  MempoolCache blk ->
  -- | 'isLastTicketNo' / 'vrLastTicketNo'
  TicketNo ->
  [TxTicket (TxMeasureWithDiffTime blk) (Validated (GenTx blk))] ->
  m (RevalidateTxsResult blk)
revalidateTxsFor capacityOverride cfg slot st0 cache0 lastTicketNo txTickets = do
  -- Pre-load cache with every tx's read values via 'addToCache'.
  -- After this step, the actual reapplication fold is pure.
  cachePreloaded <-
    foldM
      (\c tkt -> addToCache st0 (txForgetValidated (txTicketTx tkt)) c)
      cache0
      txTickets
  let (stFinal, cacheFinal, validRev, invalidRev) =
        Foldable.foldl' step (st0, cachePreloaded, [], []) txTickets
      validTxs = reverse validRev
      tipSt = tickedState st0
      is' =
        IS
          { isTxs = TxSeq.fromList validTxs
          , isTxIds =
              Set.fromList $
                map (txId . txForgetValidated . txTicketTx) validTxs
          , isCache = cacheFinal
          , isLedgerState = tickedState stFinal
          , isTip = getTip tipSt
          , isSlotNo = slot
          , isLastTicketNo = lastTicketNo
          , isCapacity = computeMempoolCapacity cfg tipSt capacityOverride
          }
  pure $ RevalidateTxsResult is' (reverse invalidRev)
 where
  step ::
    ( TickedStateHandle m blk
    , MempoolCache blk
    , [TxTicket (TxMeasureWithDiffTime blk) (Validated (GenTx blk))]
    , [Invalidated blk]
    ) ->
    TxTicket (TxMeasureWithDiffTime blk) (Validated (GenTx blk)) ->
    ( TickedStateHandle m blk
    , MempoolCache blk
    , [TxTicket (TxMeasureWithDiffTime blk) (Validated (GenTx blk))]
    , [Invalidated blk]
    )
  step (st, cache, valid, invalid) tkt =
    case runExcept (reapplyTx cfg slot (txTicketTx tkt) cache st) of
      Right (st', cache') -> (st', cache', tkt : valid, invalid)
      Left (err, cache') ->
        (st, cache', valid, Invalidated (txTicketTx tkt) err : invalid)

data RevalidateTxsResult blk
  = RevalidateTxsResult
  { newInternalState :: !(InternalState blk)
  -- ^ The internal state after revalidation
  , removedTxs :: ![Invalidated blk]
  -- ^ The previously valid transactions that were now invalid
  }

-- | Compute a snapshot by reapplying the given transactions against the
-- ticked ledger state. Same shape as 'revalidateTxsFor' but the resulting
-- state and cache are discarded — we only return the surviving txs.
--
-- The 'MempoolCache' argument is the starting cache for the fold. See
-- 'revalidateTxsFor' for the rationale on passing it explicitly.
computeSnapshot ::
  forall m blk.
  ( LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  , MonadLedger m blk
  ) =>
  LedgerConfig blk ->
  SlotNo ->
  -- | The ticked ledger state against which txs will be reapplied
  TickedStateHandle m blk ->
  -- | Starting cache for the snapshot fold.
  MempoolCache blk ->
  [TxTicket (TxMeasureWithDiffTime blk) (Validated (GenTx blk))] ->
  m (MempoolSnapshot blk)
computeSnapshot cfg slot st0 cache0 txTickets = do
  cachePreloaded <-
    foldM
      (\c tkt -> addToCache st0 (txForgetValidated (txTicketTx tkt)) c)
      cache0
      txTickets
  let (_, _, validRev) = Foldable.foldl' step (st0, cachePreloaded, []) txTickets
  pure $
    snapshotFromValidTxs (reverse validRev) (getTip (tickedState st0)) slot
 where
  step (st, cache, valid) tkt =
    case runExcept (reapplyTx cfg slot (txTicketTx tkt) cache st) of
      Right (st', cache') -> (st', cache', tkt : valid)
      Left (_, cache') -> (st, cache', valid)

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
    , snapshotPoint = tipPoint
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
