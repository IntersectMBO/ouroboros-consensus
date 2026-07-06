{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

module LeiosDemoDb.Common
  ( withLeiosDb
  , LeiosDbHandle (..)
  , LeiosEbNotification (..)
  , LeiosDbConnection (..)
  , CompletedEbs
  ) where

import Cardano.Slotting.Slot (SlotNo)
import Control.Concurrent.Class.MonadSTM.Strict (StrictTChan)
import Data.ByteString (ByteString)
import GHC.Stack (HasCallStack)
import LeiosDemoTypes
  ( BytesSize
  , EbHash
  , LeiosEb
  , LeiosPoint
  , TxHash
  )
import Ouroboros.Consensus.Util.IOLike (MonadThrow, NoThunks (..), bracket)

withLeiosDb :: MonadThrow m => LeiosDbHandle m -> (LeiosDbConnection m -> m a) -> m a
withLeiosDb db action =
  bracket (open db) close $ \conn ->
    action conn

data LeiosDbHandle m = LeiosDbHandle
  { subscribeEbNotifications :: HasCallStack => m (StrictTChan m LeiosEbNotification)
  -- ^ Subscribe to new EBs and EBTxs being stored by the LeiosDB. This will
  -- only inform about new additions, starting from when this function was
  -- called.
  -- TODO: make return type more descriptive (e.g. Subscription { getNext :: STM m LeiosEbNotification })
  , open :: m (LeiosDbConnection m)
  -- ^ Open a new connection to the LeiosDb.
  , leiosDbScanCompleteEbClosuresNotOlderThanSlot :: HasCallStack => SlotNo -> m [EbHash]
  -- ^ Scan the EBs whose tx closure is already complete and that were announced
  -- by an RB no older than the given slot. The ChainDB calls this once at
  -- startup -- passing the immutable tip slot -- to seed the acquired-EB
  -- closures set it owns (see @cdbAcquiredLeiosEbs@); thereafter it learns of
  -- newly-completed closures from 'subscribeEbNotifications' ('AcquiredEbTxs').
  --
  -- The slot is a plain query bound, not retained state: the LeiosDb does not
  -- know about (nor track) the immutable tip; it just answers the query. The
  -- acquired set itself -- the @complete ∩ announced-no-older-than-tip@
  -- projection that ChainSel consults -- is owned by the ChainDB, since its
  -- definition depends on the immutable tip, which is a ChainDB concept.
  , leiosDbGarbageCollect :: HasCallStack => SlotNo -> m ()
  -- ^ Evict LeiosDb data that is no longer needed now that everything up to the
  -- given slot is immutable. The ChainDB drives this from its GC scheduler,
  -- passing the same slot it uses to GC the VolatileDB\/PerasCertDB (see
  -- @garbageCollectBlocks@); like those stores, the LeiosDb stays dumb about
  -- /why/ -- it is handed a slot, nothing more (it never tracks the immutable
  -- tip itself).
  --
  -- Currently a no-op. When implemented it can stay purely slot-based: the EBs
  -- the immutable chain still needs (for ledger replay of an immutable cert-RB,
  -- or for serving peers) are preserved by 'leiosDbPromoteToImmutable' before
  -- they would age out here, so eviction itself need not reason about which EB
  -- data is still required.
  , leiosDbPromoteToImmutable :: HasCallStack => LeiosPoint -> m ()
  -- ^ Promote the given EB's body and tx closure into immutable LeiosDb
  -- storage, so they survive the slot-based 'leiosDbGarbageCollect' that will
  -- later evict the volatile data. The ChainDB's copier (@copyToImmutableDB@)
  -- drives this as it copies blocks to the ImmutableDB: for each cert-RB it
  -- copies, it promotes the EB that cert-RB /certifies/ (the one its predecessor
  -- announced). This is precise -- only EBs the immutable chain actually
  -- references -- and gap-free: by the parking invariant a cert-RB is only
  -- selected once its certified EB's closure is acquired, so an immutalised
  -- cert-RB's closure is necessarily present (and complete). Promotion rides the
  -- copy while eviction rides the later scheduled GC slot, so the data is always
  -- promoted before it becomes eligible for eviction.
  --
  -- Currently a no-op -- the companion of 'leiosDbGarbageCollect': the immutable
  -- storage it would promote into is not yet implemented.
  }

data LeiosEbNotification
  = AcquiredEb LeiosPoint BytesSize
  | AcquiredEbTxs LeiosPoint

-- | Single connection to the LeiosDb.
--
-- NOTE: Not thread-safe, so do not share this across threads.
data LeiosDbConnection m = LeiosDbConnection
  { close :: m ()
  -- ^ Close the connection and free up resources. After calling this, the connection may not be used anymore.
  , leiosDbScanEbPoints :: HasCallStack => m [(SlotNo, EbHash)]
  , leiosDbInsertEbPoint :: HasCallStack => LeiosPoint -> BytesSize -> m ()
  -- ^ Insert an announced EB point with its expected size. Called on
  -- the announcement path (forge issuing an EB, peer receiving an
  -- announcement). Idempotent — a second insert at the same point is
  -- a no-op.
  , leiosDbLookupEbBody :: HasCallStack => EbHash -> m [(TxHash, BytesSize)]
  -- ^ Read the EB "body": the ordered list of tx-hash + tx-byte-size
  -- pairs that constitute this EB. No tx bytes are fetched; contrast
  -- with 'leiosDbLookupEbClosure' which joins with the 'txs' table.
  , leiosDbInsertEbBody :: HasCallStack => LeiosPoint -> LeiosEb -> m ()
  -- ^ Persist an EB body. The point MUST already have been inserted
  -- via 'leiosDbInsertEbPoint' (announcement path). Yields an
  -- 'AcquiredEb' notification.
  , leiosDbInsertTxs :: HasCallStack => [(TxHash, ByteString)] -> m CompletedEbs
  -- ^ Insert transactions into the global 'txs' table (INSERT OR IGNORE).
  -- After inserting, checks which EBs referencing these txs are now complete
  -- and emits 'AcquiredEbTxs' notifications for each.
  --
  -- NOTE: Duplicate notifications may be emitted if the same EB becomes
  -- complete via multiple insert batches (e.g., if txs are inserted twice).
  -- Consumers should handle notifications idempotently.
  --
  -- REVIEW: return type only used for tracing, necessary?
  , leiosDbBatchRetrieveTxs :: HasCallStack => EbHash -> [Int] -> m [(Int, TxHash, Maybe ByteString)]
  , leiosDbFilterMissingEbBodies :: HasCallStack => [LeiosPoint] -> m [LeiosPoint]
  -- ^ Batch filter: returns the subset of input LeiosPoints whose EB bodies are missing.
  , leiosDbFilterMissingTxs :: HasCallStack => [TxHash] -> m [TxHash]
  -- ^ Batch filter: returns the subset of input TxHashes that we do NOT have.
  , leiosDbLookupEbClosure :: HasCallStack => EbHash -> m (Maybe [(TxHash, ByteString)])
  -- ^ Read the EB "closure": the tx hashes AND their tx bytes. Contrast
  -- with 'leiosDbLookupEbBody' which returns only hashes + sizes.
  -- Used by chain-sel's 'resolveLeiosClosure' to splice the EB's txs
  -- back into the CertRB before applying to the ledger.
  }

instance NoThunks (LeiosDbHandle m) where
  showTypeOf _ = "LeiosDbHandle m"
  noThunks _ctx _a = return Nothing
  wNoThunks _ctx _a = return Nothing

instance NoThunks (LeiosDbConnection m) where
  showTypeOf _ = "LeiosDbConnection m"
  noThunks _ctx _a = return Nothing
  wNoThunks _ctx _a = return Nothing

type CompletedEbs = [LeiosPoint]
