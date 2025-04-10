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
  , ValidationResult (..)
  , extendVRNew
  , extendVRPrevApplied
  , revalidateTxsFor
  , validateStateFor
    -- * Tracing
  , TraceEventMempool (..)
    -- * Conversions
  , internalStateFromVR
  , snapshotFromIS
  , validationResultFromIS
    -- * Ticking a ledger state
  , tickLedgerState
  ) where

import           Control.Concurrent.Class.MonadMVar (MVar, newMVar)
import           Control.Exception (assert)
import           Control.Monad.Trans.Except (runExcept)
import           Control.Tracer
import           Data.Maybe (isNothing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended (ledgerState)
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.Capacity
import           Ouroboros.Consensus.Mempool.TxSeq (TxSeq (..), TxTicket (..))
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Util (repeatedly)
import           Ouroboros.Consensus.Util.Enclose (EnclosingTimed)
import           Ouroboros.Consensus.Util.IOLike hiding (newMVar)

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
      isTxs          :: !(TxSeq (TxMeasure blk) (Validated (GenTx blk)))

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
    , isLedgerState  :: !(TickedLedgerState blk)

      -- | The tip of the chain that 'isTxs' was validated against
      --
      -- This comes from the underlying ledger state ('tickedLedgerState')
    , isTip          :: !(ChainHash blk)

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
    , isCapacity     :: !(TxMeasure blk)
    }
  deriving (Generic)

deriving instance ( NoThunks (Validated (GenTx blk))
                  , NoThunks (GenTxId blk)
                  , NoThunks (Ticked (LedgerState blk))
                  , NoThunks (TxMeasure blk)
                  , StandardHash blk
                  , Typeable blk
                  ) => NoThunks (InternalState blk)

-- | \( O(1) \). Return the number of transactions in the internal state of
-- the Mempool paired with their total size in bytes.
isMempoolSize :: TxLimits blk => InternalState blk -> MempoolSize
isMempoolSize is = MempoolSize {
    msNumTxs   = fromIntegral $ length $ isTxs is
  , msNumBytes = txMeasureByteSize $ TxSeq.toSize $ isTxs is
  }

initInternalState ::
     LedgerSupportsMempool blk
  => MempoolCapacityBytesOverride
  -> TicketNo  -- ^ Used for 'isLastTicketNo'
  -> LedgerConfig blk
  -> SlotNo
  -> TickedLedgerState blk
  -> InternalState blk
initInternalState capacityOverride lastTicketNo cfg slot st = IS {
      isTxs          = TxSeq.Empty
    , isTxIds        = Set.empty
    , isLedgerState  = st
    , isTip          = castHash (getTipHash st)
    , isSlotNo       = slot
    , isLastTicketNo = lastTicketNo
    , isCapacity     = computeMempoolCapacity cfg st capacityOverride
    }

{-------------------------------------------------------------------------------
  Ledger Interface
-------------------------------------------------------------------------------}

-- | Abstract interface needed to run a Mempool.
data LedgerInterface m blk = LedgerInterface
    { getCurrentLedgerState :: STM m (LedgerState blk)
    }

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDBLedgerInterface ::
     (IOLike m, IsLedger (LedgerState blk))
  => ChainDB m blk -> LedgerInterface m blk
chainDBLedgerInterface chainDB = LedgerInterface
    { getCurrentLedgerState = ledgerState <$> ChainDB.getCurrentLedger chainDB
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
    , mpEnvStateVar         :: StrictTVar m (InternalState blk)
    , mpEnvAddTxsRemoteFifo :: MVar m ()
    , mpEnvAddTxsAllFifo    :: MVar m ()
    , mpEnvTracer           :: Tracer m (TraceEventMempool blk)
    , mpEnvCapacityOverride :: MempoolCapacityBytesOverride
    }

initMempoolEnv :: ( IOLike m
                  , NoThunks (GenTxId blk)
                  , LedgerSupportsMempool blk
                  , ValidateEnvelope blk
                  )
               => LedgerInterface m blk
               -> LedgerConfig blk
               -> MempoolCapacityBytesOverride
               -> Tracer m (TraceEventMempool blk)
               -> m (MempoolEnv m blk)
initMempoolEnv ledgerInterface cfg capacityOverride tracer = do
    st <- atomically $ getCurrentLedgerState ledgerInterface
    let (slot, st') = tickLedgerState cfg (ForgeInUnknownSlot st)
    isVar <-
        newTVarIO
      $ initInternalState capacityOverride TxSeq.zeroTicketNo cfg slot st'
    addTxRemoteFifo <- newMVar ()
    addTxAllFifo    <- newMVar ()
    return MempoolEnv
      { mpEnvLedger           = ledgerInterface
      , mpEnvLedgerCfg        = cfg
      , mpEnvStateVar         = isVar
      , mpEnvAddTxsRemoteFifo = addTxRemoteFifo
      , mpEnvAddTxsAllFifo    = addTxAllFifo
      , mpEnvTracer           = tracer
      , mpEnvCapacityOverride = capacityOverride
      }

{-------------------------------------------------------------------------------
  Ticking the ledger state
-------------------------------------------------------------------------------}

-- | Tick the 'LedgerState' using the given 'BlockSlot'.
tickLedgerState ::
     forall blk. (UpdateLedger blk, ValidateEnvelope blk)
  => LedgerConfig     blk
  -> ForgeLedgerState blk
  -> (SlotNo, TickedLedgerState blk)
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
             Origin      -> minimumPossibleSlotNo (Proxy @blk)
             NotOrigin s -> succ s

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

data ValidationResult invalidTx blk = ValidationResult {
      -- | The tip of the chain before applying these transactions
      vrBeforeTip      :: ChainHash blk

      -- | The slot number of the (imaginary) block the txs will be placed in
    , vrSlotNo         :: SlotNo

      -- | Capacity of the Mempool. Corresponds to 'vrBeforeTip' and
      -- 'vrBeforeSlotNo', /not/ 'vrAfter'.
    , vrBeforeCapacity :: TxMeasure blk

      -- | The transactions that were found to be valid (oldest to newest)
    , vrValid          :: TxSeq (TxMeasure blk) (Validated (GenTx blk))

      -- | The cached IDs of transactions that were found to be valid (oldest to
      -- newest)
    , vrValidTxIds     :: Set (GenTxId blk)

      -- | A new transaction (not previously known) which was found to be valid.
      --
      -- n.b. This will only contain a valid transaction that was /newly/ added
      -- to the mempool (not a previously known valid transaction).
    , vrNewValid       :: Maybe (Validated (GenTx blk))

      -- | The state of the ledger after applying 'vrValid' against the ledger
      -- state identifeid by 'vrBeforeTip'.
    , vrAfter          :: TickedLedgerState blk

      -- | The transactions that were invalid, along with their errors
      --
      -- From oldest to newest.
    , vrInvalid        :: [(invalidTx, ApplyTxErr blk)]

      -- | The mempool 'TicketNo' counter.
      --
      -- When validating new transactions, this should be incremented, starting
      -- from 'isLastTicketNo' of the 'InternalState'.
      -- When validating previously applied transactions, this field should not
      -- be affected.
    , vrLastTicketNo   :: TicketNo
  }

-- | Extend 'ValidationResult' with a previously validated transaction that
-- may or may not be valid in this ledger state
--
-- n.b. Even previously validated transactions may not be valid in a different
-- ledger state;  it is /still/ useful to indicate whether we have previously
-- validated this transaction because, if we have, we can utilize 'reapplyTx'
-- rather than 'applyTx' and, therefore, skip things like cryptographic
-- signatures.
extendVRPrevApplied :: (LedgerSupportsMempool blk, HasTxId (GenTx blk))
                    => LedgerConfig blk
                    -> TxTicket (TxMeasure blk) (Validated (GenTx blk))
                    -> ValidationResult (Validated (GenTx blk)) blk
                    -> ValidationResult (Validated (GenTx blk)) blk
extendVRPrevApplied cfg txTicket vr =
    case runExcept (reapplyTx cfg vrSlotNo tx vrAfter) of
      Left err  -> vr { vrInvalid = (tx, err) : vrInvalid
                      }
      Right st' -> vr { vrValid      = vrValid :> txTicket
                      , vrValidTxIds = Set.insert (txId (txForgetValidated tx)) vrValidTxIds
                      , vrAfter      = st'
                      }
  where
    TxTicket { txTicketTx = tx } = txTicket
    ValidationResult { vrValid, vrSlotNo, vrValidTxIds, vrAfter, vrInvalid } = vr

-- | Extend 'ValidationResult' with a new transaction (one which we have not
-- previously validated) that may or may not be valid in this ledger state.
--
-- PRECONDITION: 'vrNewValid' is 'Nothing'. In other words: new transactions
-- should be validated one-by-one, not by calling 'extendVRNew' on its result
-- again.
extendVRNew :: (LedgerSupportsMempool blk, HasTxId (GenTx blk))
            => LedgerConfig blk
            -> WhetherToIntervene
            -> GenTx blk
            -> ValidationResult (GenTx blk) blk
            -> Either
                 (ApplyTxErr blk)
                 ( Validated        (GenTx blk)
                 , ValidationResult (GenTx blk) blk
                 )
extendVRNew cfg wti tx vr =
    assert (isNothing vrNewValid) $ runExcept m
  where
    ValidationResult {
        vrValid
      , vrValidTxIds
      , vrAfter
      , vrLastTicketNo
      , vrNewValid
      , vrSlotNo
      } = vr

    m = do
      txsz <- txMeasure cfg vrAfter tx
      (st', vtx) <- applyTx cfg wti vrSlotNo tx vrAfter
      let nextTicketNo = succ vrLastTicketNo
      pure
        ( vtx
        , vr { vrValid        = vrValid :> TxTicket vtx nextTicketNo txsz
             , vrValidTxIds   = Set.insert (txId tx) vrValidTxIds
             , vrNewValid     = Just vtx
             , vrAfter        = st'
             , vrLastTicketNo = nextTicketNo
             }
        )

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

-- | Construct internal state from 'ValidationResult'
--
-- Discards information about invalid and newly valid transactions
internalStateFromVR :: ValidationResult invalidTx blk -> InternalState blk
internalStateFromVR vr = IS {
      isTxs          = vrValid
    , isTxIds        = vrValidTxIds
    , isLedgerState  = vrAfter
    , isTip          = vrBeforeTip
    , isSlotNo       = vrSlotNo
    , isLastTicketNo = vrLastTicketNo
    , isCapacity     = vrBeforeCapacity
    }
  where
    ValidationResult {
        vrBeforeTip
      , vrSlotNo
      , vrBeforeCapacity
      , vrValid
      , vrValidTxIds
      , vrAfter
      , vrLastTicketNo
      } = vr

-- | Construct a 'ValidationResult' from internal state.
validationResultFromIS :: InternalState blk -> ValidationResult invalidTx blk
validationResultFromIS is = ValidationResult {
      vrBeforeTip      = isTip
    , vrSlotNo         = isSlotNo
    , vrBeforeCapacity = isCapacity
    , vrValid          = isTxs
    , vrValidTxIds     = isTxIds
    , vrNewValid       = Nothing
    , vrAfter          = isLedgerState
    , vrInvalid        = []
    , vrLastTicketNo   = isLastTicketNo
    }
  where
    IS {
        isTxs
      , isTxIds
      , isLedgerState
      , isTip
      , isSlotNo
      , isLastTicketNo
      , isCapacity
      } = is

-- | Create a Mempool Snapshot from a given Internal State of the mempool.
snapshotFromIS :: forall blk.
     (HasTxId (GenTx blk), TxLimits blk)
  => InternalState blk
  -> MempoolSnapshot blk
snapshotFromIS is = MempoolSnapshot {
      snapshotTxs         = implSnapshotGetTxs         is
    , snapshotTxsAfter    = implSnapshotGetTxsAfter    is
    , snapshotLookupTx    = implSnapshotGetTx          is
    , snapshotHasTx       = implSnapshotHasTx          is
    , snapshotMempoolSize = implSnapshotGetMempoolSize is
    , snapshotSlotNo      = isSlotNo                   is
    , snapshotLedgerState = isLedgerState              is
    , snapshotTake        = implSnapshotTake           is
    }
 where
  implSnapshotGetTxs :: InternalState blk
                     -> [(Validated (GenTx blk), TicketNo, TxMeasure blk)]
  implSnapshotGetTxs = flip implSnapshotGetTxsAfter TxSeq.zeroTicketNo

  implSnapshotGetTxsAfter :: InternalState blk
                          -> TicketNo
                          -> [(Validated (GenTx blk), TicketNo, TxMeasure blk)]
  implSnapshotGetTxsAfter IS{isTxs} =
    TxSeq.toTuples . snd . TxSeq.splitAfterTicketNo isTxs

  implSnapshotTake :: InternalState blk
                   -> TxMeasure blk
                   -> [Validated (GenTx blk)]
  implSnapshotTake IS{isTxs} =
    map TxSeq.txTicketTx . TxSeq.toList . fst . TxSeq.splitAfterTxSize isTxs

  implSnapshotGetTx :: InternalState blk
                    -> TicketNo
                    -> Maybe (Validated (GenTx blk))
  implSnapshotGetTx IS{isTxs} = (isTxs `TxSeq.lookupByTicketNo`)

  implSnapshotHasTx :: InternalState blk
                    -> GenTxId blk
                    -> Bool
  implSnapshotHasTx IS{isTxIds} = flip Set.member isTxIds

  implSnapshotGetMempoolSize :: InternalState blk
                             -> MempoolSize
  implSnapshotGetMempoolSize = isMempoolSize

{-------------------------------------------------------------------------------
  Validating txs or states
-------------------------------------------------------------------------------}

-- | Given a (valid) internal state, validate it against the given ledger
-- state and 'BlockSlot'.
--
-- When these match the internal state's 'isTip' and 'isSlotNo', this is very
-- cheap, as the given internal state will already be valid against the given
-- inputs.
--
-- When these don't match, the transaction in the internal state will be
-- revalidated ('revalidateTxsFor').
validateStateFor ::
     (LedgerSupportsMempool blk, HasTxId (GenTx blk), ValidateEnvelope blk)
  => MempoolCapacityBytesOverride
  -> LedgerConfig     blk
  -> ForgeLedgerState blk
  -> InternalState    blk
  -> ValidationResult (Validated (GenTx blk)) blk
validateStateFor capacityOverride cfg blockLedgerState is
    | isTip    == castHash (getTipHash st')
    , isSlotNo == slot
    = validationResultFromIS is
    | otherwise
    = revalidateTxsFor
        capacityOverride
        cfg
        slot
        st'
        isLastTicketNo
        (TxSeq.toList isTxs)
  where
    IS { isTxs, isTip, isSlotNo, isLastTicketNo } = is
    (slot, st') = tickLedgerState cfg blockLedgerState

-- | Revalidate the given transactions (@['TxTicket' ('GenTx' blk)]@), which
-- are /all/ the transactions in the Mempool against the given ticked ledger
-- state, which corresponds to the chain's ledger state.
revalidateTxsFor ::
     (LedgerSupportsMempool blk, HasTxId (GenTx blk))
  => MempoolCapacityBytesOverride
  -> LedgerConfig blk
  -> SlotNo
  -> TickedLedgerState blk
  -> TicketNo
     -- ^ 'isLastTicketNo' & 'vrLastTicketNo'
  -> [TxTicket (TxMeasure blk) (Validated (GenTx blk))]
  -> ValidationResult (Validated (GenTx blk)) blk
revalidateTxsFor capacityOverride cfg slot st lastTicketNo txTickets =
    repeatedly
      (extendVRPrevApplied cfg)
      txTickets
      (validationResultFromIS is)
  where
    is = initInternalState capacityOverride lastTicketNo cfg slot st

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
      [(Validated (GenTx blk), ApplyTxErr blk)]
      -- ^ Previously valid transactions that are no longer valid because of
      -- changes in the ledger state (details are in the provided 'ApplyTxErr').
      -- These transactions have been removed from the Mempool.
      MempoolSize
      -- ^ The current size of the Mempool.
  | TraceMempoolManuallyRemovedTxs
      [GenTxId blk]
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
   | TraceMempoolSynced
      -- ^ Emitted when the mempool is adjusted after the tip has changed.
      EnclosingTimed
      -- ^ How long the sync operation took.

deriving instance ( Eq (GenTx blk)
                  , Eq (Validated (GenTx blk))
                  , Eq (GenTxId blk)
                  , Eq (ApplyTxErr blk)
                  ) => Eq (TraceEventMempool blk)

deriving instance ( Show (GenTx blk)
                  , Show (Validated (GenTx blk))
                  , Show (GenTxId blk)
                  , Show (ApplyTxErr blk)
                  ) => Show (TraceEventMempool blk)
