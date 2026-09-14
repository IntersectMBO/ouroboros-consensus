{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}

module LeiosDemoDb.Common
  ( -- * Handle
    LeiosDbHandle (..)
  , LeiosEbNotification (..)

    -- * Reading
  , LeiosDbReader (..)
  , withReader
  , newReader

    -- * Writing
  , LeiosDbWriter (..)
  , Promise (..)
  , withWriter
  , newWriter
  , CompletedEbs
  ) where

import Cardano.Slotting.Slot (SlotNo)
import Control.Concurrent.Class.MonadSTM.Strict (StrictTChan)
import Control.ResourceRegistry (ResourceRegistry, allocate)
import Data.ByteString (ByteString)
import GHC.Stack (HasCallStack)
import LeiosDemoTypes
  ( BytesSize
  , EbHash
  , LeiosEb
  , LeiosPoint
  , TxHash
  )
import Ouroboros.Consensus.Util.IOLike (IOLike, MonadThrow, NoThunks (..), bracket)

-- | The database. Hands out readers and writers; owns neither.
data LeiosDbHandle m = LeiosDbHandle
  { openReader :: HasCallStack => m (LeiosDbReader m)
  , openWriter :: HasCallStack => m (LeiosDbWriter m)
  -- ^ Writers share whatever the backend needs to serialise them (for SQLite,
  -- one connection and one worker thread), reference-counted: the first open
  -- creates it, the last 'close' tears it down, and a later open starts
  -- afresh.
  , subscribeEbNotifications :: HasCallStack => m (StrictTChan m LeiosEbNotification)
  -- ^ New EBs and EB closures as they are stored, from the moment of
  -- subscription.
  -- TODO: make return type more descriptive (e.g. Subscription { getNext :: STM m LeiosEbNotification })
  , leiosDbGarbageCollect :: HasCallStack => SlotNo -> m ()
  -- ^ Evict data no longer needed now that everything up to the given slot is
  -- immutable. Driven by the ChainDB's GC scheduler. Currently a no-op.
  , leiosDbPromoteToImmutable :: HasCallStack => LeiosPoint -> m ()
  -- ^ Promote an EB's body and closure into immutable storage before the
  -- slot-based GC can evict them. Driven by the ChainDB's copier. Currently a
  -- no-op.
  }

-- | Queries. Never writes, so any number may run concurrently -- each on its
-- own backing connection.
data LeiosDbReader m = LeiosDbReader
  { close :: m ()
  , lookupEbBody :: HasCallStack => EbHash -> m [(TxHash, BytesSize)]
  -- ^ The EB "body": tx hashes and sizes in order, no tx bytes.
  , lookupEbClosure :: HasCallStack => EbHash -> m (Maybe [(TxHash, ByteString)])
  -- ^ The EB "closure": tx hashes /and/ their bytes, or 'Nothing' if the EB is
  -- not complete.
  , batchRetrieveTxs ::
      HasCallStack =>
      EbHash -> [Int] -> m [(Int, TxHash, Maybe ByteString)]
  -- ^ Tx bytes for a batch of offsets into one EB.
  , scanEbPoints :: HasCallStack => m [(SlotNo, EbHash)]
  -- ^ Every announced EB point. No node path wants this; it is how the tests
  -- observe point writes and truncation.
  , scanCompleteEbClosuresNotOlderThanSlot ::
      HasCallStack =>
      SlotNo -> m [LeiosPoint]
  -- ^ EBs whose closure is complete and whose announcer is no older than the
  -- given slot. Seeds the ChainDB's acquired-closures set at startup.
  }

-- | The whole write surface. Writes are serialised by the backend, so holding
-- one of these is not permission to write concurrently -- it is a submission
-- point.
--
-- Each operation returns as soon as the write is /queued/; the 'Promise' is
-- how a caller waits for it to be durable. The forge waits (its EB must be on
-- disk before the RB referencing it propagates); the fetch path need not.
data LeiosDbWriter m = LeiosDbWriter
  { close :: m ()
  , writeEbPoint :: HasCallStack => LeiosPoint -> BytesSize -> m (Promise m ())
  -- ^ Record an announced EB's point and expected size. Idempotent.
  , writeEbBody :: HasCallStack => LeiosPoint -> LeiosEb -> m (Promise m CompletedEbs)
  -- ^ Persist an EB body; its point must already have been submitted.
  -- Yields the EBs whose closure this completed.
  , writeTxs ::
      HasCallStack =>
      [(TxHash, ByteString)] -> m (Promise m CompletedEbs)
  -- ^ Persist tx bodies. Yields the EBs whose closure this completed.
  }

-- | The result of a submitted write.
--
-- 'await' rethrows whatever the write threw, in the awaiting thread. Awaiting
-- twice is fine; not awaiting at all is fine too.
newtype Promise m a = Promise {await :: m a}

-- | EBs whose tx closure became complete as a result of a write.
type CompletedEbs = [LeiosPoint]

data LeiosEbNotification
  = AcquiredEb LeiosPoint BytesSize
  | AcquiredEbTxs LeiosPoint

withReader :: MonadThrow m => LeiosDbHandle m -> (LeiosDbReader m -> m a) -> m a
withReader db = bracket (openReader db) (\r -> r.close)

withWriter :: MonadThrow m => LeiosDbHandle m -> (LeiosDbWriter m -> m a) -> m a
withWriter db = bracket (openWriter db) (\w -> w.close)

newReader :: IOLike m => ResourceRegistry m -> LeiosDbHandle m -> m (LeiosDbReader m)
newReader registry db = snd <$> allocate registry (\_ -> openReader db) (\r -> r.close)

newWriter :: IOLike m => ResourceRegistry m -> LeiosDbHandle m -> m (LeiosDbWriter m)
newWriter registry db = snd <$> allocate registry (\_ -> openWriter db) (\w -> w.close)

instance NoThunks (LeiosDbHandle m) where
  showTypeOf _ = "LeiosDbHandle"
  wNoThunks _ctx _a = return Nothing

instance NoThunks (LeiosDbReader m) where
  showTypeOf _ = "LeiosDbReader"
  wNoThunks _ctx _a = return Nothing

instance NoThunks (LeiosDbWriter m) where
  showTypeOf _ = "LeiosDbWriter"
  wNoThunks _ctx _a = return Nothing
