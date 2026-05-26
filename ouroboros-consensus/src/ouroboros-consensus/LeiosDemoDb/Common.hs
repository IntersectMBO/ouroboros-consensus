{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

module LeiosDemoDb.Common
  ( withLeiosDb
  , LeiosDbHandle (..)
  , LeiosEbNotification (..)
  , LeiosDbConnection (..)
  , LeiosFetchWork (..)
  , CompletedEbs
  ) where

import Cardano.Slotting.Slot (SlotNo)
import Control.Concurrent.Class.MonadSTM.Strict (STM, StrictTChan)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Set (Set)
import GHC.Stack (HasCallStack)
import LeiosDemoTypes
  ( BytesSize
  , EbHash
  , LeiosCertificate
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
  , readCompletedClosures :: HasCallStack => m (Set EbHash)
  -- ^ EB hashes for which the corresponding EB closure is complete:
  -- the body is stored locally and every referenced tx is present.
  -- Backed by a cache the handle seeds at construction and updates
  -- inside the insert paths, so the read is O(1) on the ChainSel hot
  -- path.
  --
  -- TODO: cap the cache.  Only EBs within @k@ of the immutable tip can
  -- be referenced by a candidate chain, so the cache only needs to hold
  -- that window; older entries can be evicted and answered by a DB
  -- query on miss.
  , readCompletedClosuresSTM :: HasCallStack => STM m (Set EbHash)
  -- ^ STM-typed sibling of 'readCompletedClosures'.  Reads the same
  -- backing TVar.  Lets callers combine the read with other STM
  -- writes in a single transaction.
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
  , leiosDbLookupEbPoint :: HasCallStack => EbHash -> m (Maybe SlotNo)
  -- ^ Check if an EB point exists in the database. Returns the slot if found.
  , leiosDbInsertEbPoint :: HasCallStack => LeiosPoint -> BytesSize -> m ()
  -- ^ Insert an announced EB point with its expected size.
  , leiosDbLookupEbBody :: HasCallStack => EbHash -> m [(TxHash, BytesSize)]
  , leiosDbQueryFetchWork :: HasCallStack => m LeiosFetchWork
  -- ^ Query all work needed for the fetch logic (used at startup):
  -- - Missing EB bodies: EBs in ebs with NULL missingTxCount
  -- - Missing TXs: TXs in ebTxs without entries in txs
  -- NOTE: This is O(n) and should only be used at startup for initialization.
  , -- NOTE: yields an 'AcquiredEb' notification.  Additionally, for
    -- every EB whose closure has become complete via this body insert
    -- (i.e. all referenced txs are already present, which can happen
    -- when two EBs reference the same 'TxHash'es and the txs were
    -- inserted in service of the other EB), yields an 'AcquiredEbTxs'
    -- notification and returns the completed 'LeiosPoint's.  Mirrors
    -- the 'CompletedEbs' return of 'leiosDbInsertTxs'.
    leiosDbInsertEbBody :: HasCallStack => LeiosPoint -> LeiosEb -> m CompletedEbs
  , -- TODO: Take [LeiosTx] and hash on insert?
    leiosDbInsertTxs :: HasCallStack => [(TxHash, ByteString)] -> m CompletedEbs
  -- ^ Insert transactions into the global txs table (INSERT OR IGNORE).
  -- After inserting, checks which EBs referencing these txs are now complete
  -- and emits 'AcquiredEbTxs' notifications for each.
  --
  -- NOTE: Duplicate notifications may be emitted if the same EB becomes
  -- complete via multiple insert batches (e.g., if txs are inserted twice).
  -- Consumers should handle notifications idempotently.
  --
  -- REVIEW: return type only used for tracing, necessary?
  , -- TODO: Return LeiosTx?
    leiosDbBatchRetrieveTxs :: HasCallStack => EbHash -> [Int] -> m [(Int, TxHash, Maybe ByteString)]
  , leiosDbFilterMissingEbBodies :: HasCallStack => [LeiosPoint] -> m [LeiosPoint]
  -- ^ Batch filter: returns the subset of input LeiosPoints whose EB bodies are missing.
  , leiosDbFilterMissingTxs :: HasCallStack => [TxHash] -> m [TxHash]
  -- ^ Batch filter: returns the subset of input TxHashes that we do NOT have.
  , leiosDbQueryCompletedEbByPoint :: HasCallStack => LeiosPoint -> m (Maybe [(TxHash, ByteString)])
  , leiosDbQueryCertificateByPoint :: HasCallStack => LeiosPoint -> m (Maybe LeiosCertificate)
  }

instance NoThunks (LeiosDbHandle m) where
  showTypeOf _ = "LeiosDbHandle m"
  noThunks _ctx _a = return Nothing
  wNoThunks _ctx _a = return Nothing

type CompletedEbs = [LeiosPoint]

-- | Result of querying the database for fetch work.
-- Contains all the information needed by the fetch logic to make decisions.
data LeiosFetchWork = LeiosFetchWork
  { missingEbBodies :: !(Map LeiosPoint BytesSize)
  -- ^ EBs that have been announced but whose bodies haven't been downloaded yet
  , missingEbTxs :: !(Map LeiosPoint [(Int, TxHash, BytesSize)])
  -- ^ EBs whose bodies we have, but with missing TXs (offset, hash, size)
  }
  deriving (Eq, Show)
