{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

module LeiosDemoDb.Common
  ( withLeiosDb
  , LeiosDbHandle (..)
  , LeiosDbStats (..)
  , LeiosEbNotification (..)
  , LeiosDbConnection (..)
  , CompletedEbs
  ) where

import Cardano.Slotting.Slot (SlotNo)
import Control.Concurrent.Class.MonadSTM.Strict (StrictTChan)
import Data.ByteString (ByteString)
import GHC.Stack (HasCallStack)
import LeiosDemoDb.Trace (LeiosDbStats (..))
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
  , -- NOTE: 'subscribeEbNotifications' and 'open' should be the _only_
    -- methods of this handle. If you're thinking about adding another,
    -- strongly consider adding it to 'LeiosDbConnection' instead. (See
    -- https://github.com/input-output-hk/ouroboros-leios/issues/983 for
    -- example motivation.)

    leiosDbGarbageCollect :: HasCallStack => SlotNo -> m ()
  -- ^ Trigger garbage collection of the LeiosDB.
  --
  --   This function does not do have to do the actual GC, but rather
  --   act as the MARK phase of GC and "create work" for a background SWEEP thread.
  --
  --   See 'sqlGarbageCollect' for the SQL backend implementation.
  , leiosDbPromoteToImmutable :: HasCallStack => LeiosPoint -> m ()
  -- ^ Promote the given EB's body and tx closure from volatile into immutable LeiosDb.
  , leiosDbSampleStats :: HasCallStack => m LeiosDbStats
  -- ^ Sample 'LeiosDbStats' counters.
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
  , leiosDbScanCompleteEbClosuresNotOlderThanSlot :: HasCallStack => SlotNo -> m [LeiosPoint]
  -- ^ Scan the EBs whose tx closure is complete and whose announcer is no older
  -- than the given slot. The ChainDB opens a transient connection at startup --
  -- passing the immutable tip slot -- to seed the acquired-EB-closures set it
  -- owns (see @cdbAcquiredLeiosEbs@); thereafter it learns of newly-completed
  -- closures from 'subscribeEbNotifications' ('AcquiredEbTxs'). The slot is a
  -- plain query bound, not retained state.
  , leiosDbInsertEbPoint :: HasCallStack => LeiosPoint -> BytesSize -> m ()
  -- ^ Insert an announced EB point with its expected size. Called on
  -- the announcement path (forge issuing an EB, peer receiving an
  -- announcement). Idempotent — a second insert at the same point is
  -- a no-op.
  , leiosDbLookupEbBody :: HasCallStack => EbHash -> m [(TxHash, BytesSize)]
  -- ^ Read the EB "body": the ordered list of tx-hash + tx-byte-size
  -- pairs that constitute this EB. No tx bytes are fetched; contrast
  -- with 'leiosDbLookupEbClosure' which joins with the 'txs' table.
  , leiosDbInsertEbBody :: HasCallStack => LeiosPoint -> LeiosEb -> m CompletedEbs
  -- ^ Persist an EB body. The point MUST already have been inserted via
  -- 'leiosDbInsertEbPoint' (announcement path). Yields an 'AcquiredEb'
  -- notification.
  --
  -- Returns any EBs whose closure just became complete because their body
  -- landed after all their txs were already present in the DB. Those EBs also
  -- get an 'AcquiredEbTxs' notification.
  --
  -- XXX: return type only used for tracing
  , leiosDbInsertTxs :: HasCallStack => [(TxHash, ByteString)] -> m CompletedEbs
  -- ^ Insert transactions into the global 'txs' table (INSERT OR IGNORE).
  -- After inserting, checks which EBs referencing these txs are now complete
  -- and emits 'AcquiredEbTxs' notifications for each.
  --
  -- NOTE: Duplicate notifications may be emitted if the same EB becomes
  -- complete via multiple insert batches (e.g., if txs are inserted twice).
  -- Consumers should handle notifications idempotently.
  --
  -- XXX: return type only used for tracing
  , leiosDbBatchRetrieveTxs :: HasCallStack => EbHash -> [Int] -> m [(Int, TxHash, Maybe ByteString)]
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
