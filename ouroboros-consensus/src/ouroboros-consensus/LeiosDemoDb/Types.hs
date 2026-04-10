{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

module LeiosDemoDb.Types
  ( LeiosDbHandle (..)
  , LeiosDbConnection (..)
  , LeiosFetchWork (..)
  , CompletedEbs
  ) where

import Cardano.Slotting.Slot (SlotNo)
import Control.Concurrent.Class.MonadSTM.Strict (StrictTChan)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import GHC.Stack (HasCallStack)
import LeiosDemoTypes
  ( BytesSize
  , EbHash
  , LeiosCertificate
  , LeiosEb
  , LeiosNotification
  , LeiosPoint
  , TxHash
  )
import Ouroboros.Consensus.Util.IOLike (NoThunks (..))

data LeiosDbHandle m = LeiosDbHandle
  { subscribeEbNotifications :: HasCallStack => m (StrictTChan m LeiosNotification)
  -- ^ Subscribe to new EBs and EBTxs being stored by the LeiosDB. This will
  -- only inform about new additions, starting from when this function was
  -- called.
  -- TODO: make return type more descriptive (e.g. Subscription { getNext :: STM m LeiosNotification })
  , open :: m (LeiosDbConnection m)
  -- ^ Open a new connection to the LeiosDb.
  }

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
  -- - Missing EB bodies: EBs in ebPoints without entries in ebTxs
  -- - Missing TXs: TXs in ebTxs without entries in txs
  -- NOTE: This is O(n) and should only be used at startup for initialization.
  , -- NOTE: yields a LeiosOfferBlock notification
    leiosDbInsertEbBody :: HasCallStack => LeiosPoint -> LeiosEb -> m ()
  , -- TODO: Take [LeiosTx] and hash on insert?
    leiosDbInsertTxs :: HasCallStack => [(TxHash, ByteString)] -> m CompletedEbs
  -- ^ Insert transactions into the global txs table (INSERT OR IGNORE).
  -- After inserting, checks which EBs referencing these txs are now complete
  -- and emits LeiosOfferBlockTxs notifications for each.
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
