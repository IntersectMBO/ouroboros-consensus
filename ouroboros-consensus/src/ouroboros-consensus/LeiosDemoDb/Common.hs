{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}

module LeiosDemoDb.Common
  ( -- * Handle
    LeiosDbHandle (..)
  , LeiosDbStats (..)
  , LeiosEbNotification (..)

    -- * Reading
  , LeiosDbReader (..)
  , withReader
  , allocateReader

    -- * Writing
  , LeiosDbWriter (..)
  , Promise (..)
  , awaitAll
  , withWriter
  , allocateWriter
  , CompletedEbs
  ) where

import Cardano.Slotting.Slot (SlotNo)
import Control.Concurrent.Class.MonadSTM.Strict (StrictTChan)
import Control.ResourceRegistry (ResourceRegistry, allocate)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import GHC.Stack (HasCallStack)
import LeiosDemoDb.Trace (LeiosDbStats (..))
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
  -- ^ The database's one write path. Opening allocates nothing -- the backend
  -- creates whatever serialises writes (for SQLite, one connection and one
  -- worker thread) together with the database -- so every writer submits to
  -- the same place and 'close' merely flushes this caller's writes.
  , subscribeEbNotifications :: HasCallStack => m (StrictTChan m LeiosEbNotification)
  -- ^ New EBs and EB closures as they are stored, from the moment of
  -- subscription.
  -- TODO: make return type more descriptive (e.g. Subscription { getNext :: STM m LeiosEbNotification })
  , leiosDbGarbageCollect :: HasCallStack => SlotNo -> m ()
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

-- | Queries. Never writes, so any number may run concurrently -- each on its
-- own backing connection.
--
-- NOTE: Not thread-safe, so do not share a reader across threads.
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
  -- ^ Not a teardown -- the write path outlives every writer. Flushes: when
  -- this returns, everything this caller submitted is durable.
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
-- 'await' rethrows whatever the write threw, in the awaiting thread, as a
-- 'LeiosDbException' naming the write and the site that submitted it.
-- Awaiting twice is fine. Not awaiting discards the failure along with the
-- result: collect promises and 'awaitAll' them instead, unless the write's
-- fate genuinely does not matter.
newtype Promise m a = Promise {await :: m a}
  deriving stock Functor

-- | Await every promise, in order.
awaitAll :: (Foldable t, Applicative m) => t (Promise m ()) -> m ()
awaitAll = traverse_ await

-- | EBs whose tx closure became complete as a result of a write.
type CompletedEbs = [LeiosPoint]

data LeiosEbNotification
  = AcquiredEb LeiosPoint BytesSize
  | AcquiredEbTxs LeiosPoint

withReader :: MonadThrow m => LeiosDbHandle m -> (LeiosDbReader m -> m a) -> m a
withReader db = bracket (openReader db) (\r -> r.close)

withWriter :: MonadThrow m => LeiosDbHandle m -> (LeiosDbWriter m -> m a) -> m a
withWriter db = bracket (openWriter db) (\w -> w.close)

allocateReader :: IOLike m => ResourceRegistry m -> LeiosDbHandle m -> m (LeiosDbReader m)
allocateReader registry db = snd <$> allocate registry (\_ -> openReader db) (\r -> r.close)

allocateWriter :: IOLike m => ResourceRegistry m -> LeiosDbHandle m -> m (LeiosDbWriter m)
allocateWriter registry db = snd <$> allocate registry (\_ -> openWriter db) (\w -> w.close)

instance NoThunks (LeiosDbHandle m) where
  showTypeOf _ = "LeiosDbHandle"
  wNoThunks _ctx _a = return Nothing

instance NoThunks (LeiosDbReader m) where
  showTypeOf _ = "LeiosDbReader"
  wNoThunks _ctx _a = return Nothing

instance NoThunks (LeiosDbWriter m) where
  showTypeOf _ = "LeiosDbWriter"
  wNoThunks _ctx _a = return Nothing
