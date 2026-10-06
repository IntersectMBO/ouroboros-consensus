{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}

module Ouroboros.Consensus.Storage.LeiosDB.API
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
  , TxOffset
  ) where

import Cardano.Slotting.Slot (SlotNo)
import Control.Concurrent.Class.MonadSTM.Strict (StrictTChan)
import Control.ResourceRegistry (ResourceRegistry, allocate)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import GHC.Stack (HasCallStack)
import NoThunks.Class (OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EbHash
  , LeiosEb
  , LeiosPoint
  , TxHash
  , TxOffset
  )
import Ouroboros.Consensus.Storage.LeiosDB.Trace (LeiosDbStats (..))
import Ouroboros.Consensus.Util.IOLike (IOLike, MonadThrow, NoThunks (..), bracket)

-- | The Leios database. Hands out readers, writers and subscriptions.
data LeiosDbHandle m = LeiosDbHandle
  { closeLeiosDbHandle :: m ()
  -- ^ Close the database: flush what is in flight, stop whatever it runs
  -- behind the scenes, release its connections. Readers and writers handed
  -- out earlier are not usable afterwards.
  , openReader :: HasCallStack => m (LeiosDbReader m)
  -- ^ Get a new reader. No interaction between readers or writers.
  , openWriter :: HasCallStack => m (LeiosDbWriter m)
  -- ^ Get a new writer. All writes of all writers are serialised.
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
  , leiosDbPromoteToImmutable :: HasCallStack => [LeiosPoint] -> m ()
  -- ^ Promote the given EBs' bodies and tx closures from volatile into immutable
  --   LeiosDb. Takes a batch: the caller holds the ImmutableDB write lock and
  --   blocks on the shared writer queue, so this must be one round-trip per
  --   'copyToImmutableDB' pass, not one per certified EB.
  , leiosDbSampleStats :: HasCallStack => m LeiosDbStats
  -- ^ Sample 'LeiosDbStats' counters.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "LeiosDbHandle" (LeiosDbHandle m)

-- | Query API into the LeiosDb. Do not use a reader in two concurrent threads.
data LeiosDbReader m = LeiosDbReader
  { closeReader :: m ()
  , lookupEbBody :: HasCallStack => EbHash -> m [(TxHash, BytesSize)]
  -- ^ The EB "body": tx hashes and sizes in order, no tx bytes.
  , lookupEbClosure :: HasCallStack => EbHash -> m (Maybe [(TxHash, ByteString)])
  -- ^ The EB "closure": tx hashes /and/ their bytes, or 'Nothing' if the EB is
  -- not complete.
  , batchRetrieveTxs ::
      HasCallStack =>
      EbHash -> [Int] -> m [(Int, TxHash, Maybe ByteString)]
  -- ^ Tx bytes for a batch of offsets into one EB.
  , testScanEbPoints :: HasCallStack => m [(SlotNo, EbHash)]
  -- ^ Every announced EB point.
  --
  -- Not used in production, only used in tests to observe point
  -- writes and truncation.
  , scanCompleteEbClosuresNotOlderThanSlot ::
      HasCallStack =>
      SlotNo -> m [LeiosPoint]
  -- ^ EBs whose closure is complete and whose announcer is no older than the
  -- given slot. Seeds the ChainDB's acquired-closures set at startup.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "LeiosDbReader" (LeiosDbReader m)

-- | Modifying API into the LeiosDb.
--
-- Asynchronous interface where each operation returns upon submission. The
-- returned 'Promise' can be used to 'await' the write being performed. Any
-- exceptions are thrown through 'await'.
data LeiosDbWriter m = LeiosDbWriter
  { closeWriter :: m ()
  -- ^ Close writer and flush all remaining writes.
  , writeEbPoint :: HasCallStack => LeiosPoint -> BytesSize -> m (Promise m ())
  -- ^ Record an announced EB's point and expected size.
  , writeEbBody :: HasCallStack => LeiosPoint -> LeiosEb -> m (Promise m CompletedEbs)
  -- ^ Persist an EB body. Returns any EBs whose closure this completed.
  --
  -- The returned EB is only used for tracing and too broad: it can actually only
  -- be this same EB which got completed.
  , writeTxs ::
      HasCallStack =>
      LeiosPoint -> [(TxOffset, ByteString)] -> m (Promise m CompletedEbs)
  -- ^ Persist tx bodies for the EB identified by 'LeiosPoint'.
  --
  -- The transactions the given with bytes and an offset into the EB body.
  --
  -- ASSUMPTION: the EB body must already be written, otherwise the txs are dropped.
  --
  -- A transaction may be dropped by the implementation if:
  -- - is not in the body,
  -- - or it is already stored,
  -- - or its size is not the declared one.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "LeiosDbWriter" (LeiosDbWriter m)

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
  = -- | An EB body was written.
    AcquiredEb
      -- | the EB's point
      LeiosPoint
      -- | its size in bytes
      BytesSize
  | -- | Every tx of the EB is now in the DB.
    AcquiredEbTxs
      -- | the EB's point
      LeiosPoint

withReader :: MonadThrow m => LeiosDbHandle m -> (LeiosDbReader m -> m a) -> m a
withReader db = bracket (openReader db) closeReader

withWriter :: MonadThrow m => LeiosDbHandle m -> (LeiosDbWriter m -> m a) -> m a
withWriter db = bracket (openWriter db) closeWriter

allocateReader :: IOLike m => ResourceRegistry m -> LeiosDbHandle m -> m (LeiosDbReader m)
allocateReader registry db = snd <$> allocate registry (\_ -> openReader db) closeReader

allocateWriter :: IOLike m => ResourceRegistry m -> LeiosDbHandle m -> m (LeiosDbWriter m)
allocateWriter registry db = snd <$> allocate registry (\_ -> openWriter db) closeWriter
