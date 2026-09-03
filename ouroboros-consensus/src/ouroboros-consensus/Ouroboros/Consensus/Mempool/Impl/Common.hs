{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
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
  , revalidateTxsFor'
  , validateNewTransaction

    -- * Tracing
  , MempoolRejectionDetails (..)
  , TraceEventMempool (..)
  , jsonMempoolRejectionDetails

    -- * Conversions
  , snapshotFromIS

    -- * Ticking a ledger state
  , tickLedgerState
  ) where

import Control.Concurrent.Class.MonadSTM.Strict.TMVar (newTMVarIO)
import Control.Monad.Trans.Except (runExcept)
import Control.Tracer
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import Data.Bifunctor (second)
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..), maybeToStrictMaybe, strictMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Typeable
import Data.Word (Word64)
import GHC.Generics (Generic)
import LeiosDemoTypes.LeiosJobs (TxHash)
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
  , validatedTxDiffs :: !(LedgerTables (TickedLedgerState blk) DiffMK)
  , validatedTxLeiosHash :: !(StrictMaybe TxHash)
  -- ^ Cached here so it isn't computed on every resync.
  --
  -- 'SNothing' for txs that can't be in a Leios block (eg when the Mempool is
  -- in a Cardano era in which Leios is not enabled).
  }
  deriving Generic

deriving instance
  ( NoThunks (Validated (GenTx blk))
  , NoThunks (TxIn (LedgerState blk))
  , NoThunks (TxOut (LedgerState blk))
  ) =>
  NoThunks (ValidatedTxWithDiffs blk)

-- | Internal state in the mempool
data InternalState blk = IS
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
  , isLeiosTxIndex :: !(Map TxHash (Validated (GenTx blk)))
  -- ^ The mempool's transactions indexed by their Leios EB-tx hash (a hash of a
  -- /different/ preimage than 'GenTxId', so 'isTxIds' can't answer it). Lets the
  -- Leios fetch logic find an EB's referenced txs in our mempool by hash without
  -- rescanning. Maintained in lockstep with 'isTxs' via the block's
  -- 'ResolveLeiosBlock' @leiosTxHashOfGenTx@; empty when that returns 'Nothing'
  -- for every tx (i.e. a non-Leios setup).
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
  , isRemovalGen :: !Word64
  -- ^ A monotonic counter bumped each time 'implRemoveTxsEvenIfValid' drops a
  -- transaction. A sync carries it along its (off-lock) candidate and re-checks
  -- it against the committed state under the lock: a mismatch means a removal
  -- raced the sync, whose candidate may have resurrected the dropped tx, so it
  -- must retry. Preserved by adds and syncs, bumped only by removals.
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
    , msNumBytes = txMeasureByteSize $ forgetTxMeasureWithDiffTime $ TxSeq.toSize $ isTxs is
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
    , isLeiosTxIndex = Map.empty
    , isTxKeys = emptyLedgerTables
    , isTxValues = emptyLedgerTables
    , isLedgerState = st
    , isTip = castPoint $ getTip st
    , isSlotNo = slot
    , isLastTicketNo = lastTicketNo
    , isRemovalGen = 0
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
  { mldViewState :: LedgerState blk EmptyMK
  -- ^ The ledger state currently at the tip of the LedgerDB
  , mldViewGetForker :: m (Either GetForkerError (ReadOnlyForker m (LedgerState blk)))
  -- ^ An action to get a forker at 'mldViewState' or an error in the unlikely
  -- case that such state is now gone from the LedgerDB.
  --
  -- The forker is not tracked as a resource because shutting down the mempool
  -- only happens if the system is going down, and in that case open forkers are unimportant.
  }

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDBLedgerInterface ::
  (IOLike m, IsLedger (LedgerState blk)) =>
  ChainDB m blk ->
  LedgerInterface m blk
chainDBLedgerInterface chainDB =
  LedgerInterface
    { getCurrentLedgerState = do
        st <- ChainDB.getCurrentLedger chainDB
        pure
          $ MempoolLedgerDBView
            (ledgerState st)
          $ fmap (second ledgerStateReadOnlyForker)
          $ ChainDB.openReadOnlyForkerAtPoint
            chainDB
            (SpecificPoint (castPoint $ getTip st))
    }

{-------------------------------------------------------------------------------
  Mempool environment
-------------------------------------------------------------------------------}

-- | The mempool environment captures all the associated variables wrt the
-- Mempool and is accessed by the Mempool interface on demand to perform the
-- different operations.
data MempoolEnv m blk = MempoolEnv
  { mpEnvLedger :: LedgerInterface m blk
  , mpEnvForker :: StrictMVar m (ReadOnlyForker m (LedgerState blk))
  , mpEnvLedgerCfg :: LedgerConfig blk
  , mpEnvStateVar :: StrictTMVar m (InternalState blk)
  -- ^ The single, authoritative internal state of the mempool, which doubles as
  -- the /writer/ lock. Writers (adds, removes and the sync merge) 'takeTMVar'
  -- it, do their work, and 'putTMVar' the new state; readers ('getSnapshot',
  -- 'getCapacity', 'getSnapshotFor') 'readTMVar' it. Because it is a single
  -- cell, the whole capacity accounting has one source of truth and cannot
  -- diverge. A reader only ever blocks for the duration a writer holds the
  -- lock; the sync keeps that short by doing its large LedgerDB read /before/
  -- taking the lock (see 'implSyncWithLedger'), so only the (sub-second) merge
  -- is under it.
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
  MempoolLedgerDBView st meFrk <- atomically $ getCurrentLedgerState ledgerInterface
  eFrk <- meFrk
  case eFrk of
    -- This should happen very rarely, if between getting the state and getting
    -- the forker, the ledgerdb has changed. We just loop to try again here.
    Left{} -> do
      initMempoolEnv ledgerInterface cfg capacityOverride mbTimeoutConfig tracer
    Right frk -> do
      frkMVar <- newMVar frk
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
          , mpEnvForker = frkMVar
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
  (LedgerSupportsMempool blk, HasTxId (GenTx blk), ResolveLeiosBlock blk) =>
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
  ( Either (ApplyTxErr blk) (Validated (GenTx blk), LedgerTables (TickedLedgerState blk) DiffMK)
  , DiffTimeMeasure -> InternalState blk
  )
validateNewTransaction cfg wti tx txsz origValues st is =
  case runExcept (applyTx cfg wti isSlotNo tx st) of
    Left err -> (Left err, \_dur -> is)
    Right (st', vtx) ->
      ( Right (vtx, projectLedgerTables st')
      , \dur ->
          is
            { isTxs =
                isTxs
                  :> TxTicket
                    (ValidatedTxWithDiffs vtx (projectLedgerTables st') leiosHash)
                    nextTicketNo
                    (MkTxMeasureWithDiffTime txsz dur)
            , isTxKeys = isTxKeys <> getTransactionKeySets tx
            , isTxValues = ltliftA2 unionValues isTxValues origValues
            , isTxIds = Set.insert (txId tx) isTxIds
            , isLeiosTxIndex = strictMaybe id (\h -> Map.insert h vtx) leiosHash isLeiosTxIndex
            , isLedgerState = prependMempoolDiffs isLedgerState st'
            , isLastTicketNo = nextTicketNo
            }
      )
 where
  IS
    { isTxs
    , isTxIds
    , isLeiosTxIndex
    , isTxKeys
    , isTxValues
    , isLedgerState
    , isLastTicketNo
    , isSlotNo
    } = is

  nextTicketNo = succ isLastTicketNo

  leiosHash = maybeToStrictMaybe (leiosTxHashOfGenTx tx)

-- | Revalidate the given transactions against the given ticked ledger state,
-- producing a new 'InternalState'.
--
-- Note that this function will perform revalidation so it is expected that the
-- transactions given to it were previously applied, for example if we are
-- revalidating the whole set of transactions onto a new state, or if we remove
-- some transactions and revalidate the remaining ones.
revalidateTxsFor ::
  forall m blk.
  (Monad m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  -- | The forker to read the transactions' inputs from.
  ReadOnlyForker m (LedgerState blk) ->
  MempoolCapacityBytesOverride ->
  LedgerConfig blk ->
  SlotNo ->
  -- | The ticked ledger state againt which txs will be revalidated
  TickedLedgerState blk DiffMK ->
  -- | 'isLastTicketNo' and 'vrLastTicketNo'
  TicketNo ->
  -- | The removal generation to stamp on the result (see 'isRemovalGen').
  Word64 ->
  [TxTicket (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)] ->
  m (RevalidateTxsResult blk)
revalidateTxsFor frk capacityOverride cfg slot st lastTicketNo removalGen txTickets =
  -- A from-scratch revalidation is just 'revalidateTxsFor'' onto an empty candidate
  -- at this base: no prior txs, ledger = @st@. Sharing the one implementation
  -- keeps the two byte-identical by construction.
  revalidateTxsFor' frk capacityOverride cfg slot emptyResult lastTicketNo txTickets
 where
  -- Seed the empty candidate with the real 'lastTicketNo' (not zero): each
  -- reapplied tx keeps its own 'TicketNo' (carried in its 'TxTicket'), and
  -- 'revalidateTxsFor'' sets 'isLastTicketNo' to 'lastTicketNo' on the result, so
  -- the mempool's ticket counter is preserved and the next add continues from it.
  emptyResult =
    RevalidateTxsResult
      (initInternalState capacityOverride lastTicketNo cfg slot st){isRemovalGen = removalGen}
      []

-- | The general revalidation step: reapply a /delta/ of already-validated txs on
-- top of the candidate carried in the given 'RevalidateTxsResult', without
-- reprocessing what it already holds, appending any newly-removed txs to those
-- carried in. 'revalidateTxsFor' is the special case that starts from an empty
-- candidate.
--
-- @deltaTxTickets@ are the txs added since the candidate was revalidated, in
-- ascending ticket order. Their inputs are read from @frk@ here rather than by
-- the caller — the keys to read are derived from the txs anyway. Seeding the
-- delta from the candidate's post-reapply ledger ('isLedgerState') lets a sync
-- shrink its work off the lock and hold the lock only for a small final delta
-- ('implSyncWithLedger').
revalidateTxsFor' ::
  forall m blk.
  (Monad m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  -- | The forker to read the delta txs' inputs from.
  ReadOnlyForker m (LedgerState blk) ->
  MempoolCapacityBytesOverride ->
  LedgerConfig blk ->
  SlotNo ->
  -- | The result so far: its state is extended in place with the delta (no full
  -- rebuild), and its removed txs are carried forward so the loop accumulates
  -- them without any bookkeeping of its own. The candidate already carries the
  -- base it was revalidated against (via 'isLedgerState'/'isTip'), so the base
  -- ledger need not be passed separately.
  RevalidateTxsResult blk ->
  -- | The new 'isLastTicketNo' (the mempool's current ticket counter).
  TicketNo ->
  -- | The delta txs, in ascending 'TicketNo' order.
  [TxTicket (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)] ->
  m (RevalidateTxsResult blk)
revalidateTxsFor' frk capacityOverride cfg slot (RevalidateTxsResult cand removedSoFar) lastTicketNo deltaTxTickets = do
  let deltaTxs = map wrap deltaTxTickets
      deltaKeys = Foldable.foldMap' (getTransactionKeySets . txForgetValidated . fst3) deltaTxs
  deltaValues <- castLedgerTables <$> roforkerReadTables frk (castLedgerTables deltaKeys)
  let
    -- Seed the delta reapplication from @cand@'s post-reapply ledger state, so
    -- a delta tx spending one of @cand@'s outputs sees it. This is exactly the
    -- state a full reapplication would be in after processing @cand@'s txs.
    ReapplyTxsResult errDelta validDelta st' =
      reapplyTxs @blk @Collect cfg slot deltaTxs $
        applyMempoolDiffs deltaValues deltaKeys (isLedgerState cand)

    -- The delta's surviving txs' contributions — all O(delta), extending the
    -- candidate in place rather than rebuilding from all survivors.
    survivorKeys = Foldable.foldMap' (getTransactionKeySets . txForgetValidated . fst3) validDelta
    survivorDiffs = Foldable.foldl' rawPrependDiffs (DiffMK mempty) $ map (getLedgerTables . snd3) validDelta

    newIS =
      cand
        { isTxs = Foldable.foldl' (:>) (isTxs cand) (map unwrap validDelta)
        , isTxIds = isTxIds cand <> Set.fromList (map (txId . txForgetValidated . fst3) validDelta)
        , isLeiosTxIndex =
            -- Reuse each survivor's memoized hash (carried through 'reapplyTxs'' in
            -- the per-tx payload) -- never recompute it on a resync.
            isLeiosTxIndex cand
              <> Map.fromList
                [ (h, vtx)
                | (vtx, _df, (_tk, _tz, SJust h)) <- validDelta
                ]
        , isTxKeys = isTxKeys cand <> survivorKeys
        , -- REVIEW(utxo-hd): incremental value cache. Equal to the from-scratch
          -- @restrictValuesMK (isTxValues cand `union` deltaValues) (allKeys)@:
          -- 'isTxValues cand' is already restricted to the candidate's keys and
          -- 'deltaValues' covers the delta's keys, so the outer restrict is a
          -- no-op; any key shared with the candidate (a reference input) carries
          -- the same base value, so the union is unambiguous.
          isTxValues =
            ltliftA2 unionValues (isTxValues cand) (ltliftA2 restrictValuesMK deltaValues survivorKeys)
        , -- REVIEW(utxo-hd): incremental ledger tables. Prepends only the delta's
          -- diffs onto the candidate's tables (base ⊕ cand-diffs). Equal to the
          -- from-scratch @rawPrependDiffs base (candDiffs ⊕ deltaDiffs)@ iff
          -- 'rawPrependDiffs' is associative over the (disjoint) per-tx diffs.
          isLedgerState =
            st'
              `withLedgerTables` ltliftA2 rawPrependDiffs (projectLedgerTables (isLedgerState cand)) (LedgerTables survivorDiffs)
        , isCapacity = computeMempoolCapacity cfg st' capacityOverride
        , isLastTicketNo = lastTicketNo
        }
  pure $ RevalidateTxsResult newIS (removedSoFar ++ errDelta)
 where
  wrap (TxTicket (ValidatedTxWithDiffs tx df mh) tk tz) = (tx, df, (tk, tz, mh))
  unwrap (tx, df, (tk, tz, mh)) = TxTicket (ValidatedTxWithDiffs tx df mh) tk tz
  fst3 (x, _, _) = x
  snd3 (_, x, _) = x

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
  forall blk.
  (LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  MempoolCapacityBytesOverride ->
  LedgerConfig blk ->
  SlotNo ->
  -- | The ticked ledger state againt which txs will be revalidated
  TickedLedgerState blk DiffMK ->
  -- | The tables with all the inputs for the transactions
  LedgerTables (LedgerState blk) ValuesMK ->
  TicketNo ->
  [TxTicket (TxMeasureWithDiffTime blk) (ValidatedTxWithDiffs blk)] ->
  MempoolSnapshot blk
computeSnapshot capacityOverride cfg slot st values lastTicketNo txTickets =
  let inputTxs = map wrap txTickets
      inputKeys = Foldable.foldMap' (getTransactionKeySets . txForgetValidated . fst3) inputTxs

      ReapplyTxsResult _ validatedTxs st' =
        reapplyTxs @blk @Discard cfg slot inputTxs $
          applyMempoolDiffs values inputKeys st
   in snapshotFromIS $
        IS
          { isTxs = TxSeq.fromList $ map unwrap validatedTxs
          , isTxIds = Set.fromList $ map (txId . txForgetValidated . fst3) validatedTxs
          , -- The Leios index is read from the committed state, never from a
            -- 'getSnapshotFor' snapshot, so leave it empty here.
            isLeiosTxIndex = Map.empty
          , -- These two can be empty since we don't need the resulting
            -- values at all when making a snapshot, as we won't update
            -- the internal state.
            isTxKeys = emptyLedgerTables
          , isTxValues = emptyLedgerTables
          , -- This one can use the empty tables because we don't use the
            -- resulting ledger state except for its point
            isLedgerState = st' `withLedgerTables` emptyLedgerTables
          , isTip = castPoint $ getTip st
          , isSlotNo = slot
          , isLastTicketNo = lastTicketNo
          , -- Irrelevant: this snapshot is transient, never committed.
            isRemovalGen = 0
          , isCapacity = computeMempoolCapacity cfg st' capacityOverride
          }
 where
  fst3 (x, _, _) = x
  wrap = (\(TxTicket (ValidatedTxWithDiffs tx df mh) tk tz) -> (tx, (), (df, tk, tz, mh)))
  unwrap = (\(tx, (), (df, tk, tz, mh)) -> (TxTicket (ValidatedTxWithDiffs tx df mh) tk tz))

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
    , snapshotStateHash = pointHash $ castPoint $ getTip $ isLedgerState is
    , snapshotTake = implSnapshotTake is
    , snapshotPoint = castPoint $ getTip $ isLedgerState is
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
    (\x -> [(validatedTx a, b, forgetTxMeasureWithDiffTime c) | (a, b, c) <- x])
      . TxSeq.toTuples
      . snd
      . TxSeq.splitAfterTicketNo isTxs

  implSnapshotTake ::
    InternalState blk ->
    TxMeasure blk ->
    ([Validated (GenTx blk)], TxMeasureWithDiffTime blk)
  implSnapshotTake IS{isTxs} limit =
    (map (validatedTx . TxSeq.txTicketTx) (TxSeq.toList x), TxSeq.toSize x)
   where
    (x, _y) = TxSeq.splitAfterTxSize isTxs $ MkTxMeasureWithDiffTime limit InfiniteDiffTimeMeasure

  implSnapshotGetTx ::
    InternalState blk ->
    TicketNo ->
    Maybe (Validated (GenTx blk))
  implSnapshotGetTx IS{isTxs} = fmap validatedTx . (isTxs `TxSeq.lookupByTicketNo`)

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
  | TraceMempoolCacheHit (Point blk)
  | TraceMempoolCacheMiss (Point blk)
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
