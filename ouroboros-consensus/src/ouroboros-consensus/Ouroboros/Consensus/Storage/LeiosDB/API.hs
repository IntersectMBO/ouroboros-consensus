{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}

module Ouroboros.Consensus.Storage.LeiosDB.API
  ( -- * Handle
    LeiosDbHandle (..)
  , LeiosDbStats (..)

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
  , storeForgedEb
  , TxOffset
  ) where

import Cardano.Slotting.Slot (SlotNo)
import Control.Monad (void)
import Control.ResourceRegistry (ResourceRegistry, allocate)
import Data.ByteString (ByteString)
import Data.Foldable (toList, traverse_)
import GHC.Stack (HasCallStack)
import NoThunks.Class (OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , EndorserBlockHash
  , ForgedEndorserBlock
  , LeiosPoint (..)
  , TxOffset
  , encodeEndorserBlockSize
  , endorserBlockHash
  , forgedEbBody
  , forgedEbTxs
  )
import Ouroboros.Consensus.Storage.LeiosDB.Trace (LeiosDbStats (..))
import Ouroboros.Consensus.Util.IOLike (IOLike, MonadThrow, NoThunks (..), bracket)

-- | The Leios database. Hands out readers and writers.
data LeiosDbHandle m = LeiosDbHandle
  { closeLeiosDbHandle :: m ()
  -- ^ Close the database: flush what is in flight, stop whatever it runs
  -- behind the scenes, release its connections. Readers and writers handed
  -- out earlier are not usable afterwards.
  , openReader :: HasCallStack => m (LeiosDbReader m)
  -- ^ Get a new reader. No interaction between readers or writers.
  , openWriter :: HasCallStack => m (LeiosDbWriter m)
  -- ^ Get a new writer. All writes of all writers are serialised.
  , garbageCollect :: HasCallStack => SlotNo -> m ()
  -- ^ Trigger garbage collection of the LeiosDB.
  --
  --   This function does not do have to do the actual GC, but rather
  --   act as the MARK phase of GC and "create work" for a background SWEEP thread.
  --
  --   See 'sqlGarbageCollect' for the SQL backend implementation.
  , leiosDbPromoteToImmutable :: HasCallStack => [LeiosPoint] -> m ()
  -- ^ Copy the given EBs' bodies and tx closures from volatile into immutable
  --   LeiosDb, before returning. Takes a batch: the caller holds the ImmutableDB
  --   write lock, so this is one transaction per 'copyToImmutableDB' pass, not
  --   one per certified EB. Throws if an EB's closure is incomplete.
  , leiosDbSampleStats :: HasCallStack => m LeiosDbStats
  -- ^ Sample 'LeiosDbStats' counters.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "LeiosDbHandle" (LeiosDbHandle m)

-- | Query API into the LeiosDb. Do not use a reader in two concurrent threads.
data LeiosDbReader m = LeiosDbReader
  { closeReader :: m ()
  , lookupEbBody :: HasCallStack => EndorserBlockHash -> m (Maybe ByteString)
  -- ^ The EB "body", in the bytes it arrived in
  -- ('Ouroboros.Consensus.Leios.Types.serialiseEndorserBlock'), or 'Nothing'
  -- if the LeiosDB does not hold it.
  , lookupEbClosure :: HasCallStack => EndorserBlockHash -> m (Maybe [ByteString])
  -- ^ The EB "closure": the bytes of its txs in body order, or 'Nothing' unless
  -- each of them is written.
  , batchRetrieveTxs ::
      HasCallStack =>
      EndorserBlockHash -> [Int] -> m [(Int, Maybe ByteString)]
  -- ^ Tx bytes for a batch of offsets into one EB, in ascending order: the
  -- offsets the body declares, each with its bytes, or 'Nothing' if not
  -- written.
  , testScanEbPoints :: HasCallStack => m [(SlotNo, EndorserBlockHash)]
  -- ^ Every announced EB point.
  --
  -- Not used in production, only used in tests to observe point
  -- writes and truncation.
  , scanCompleteEbClosuresNotOlderThanSlot ::
      HasCallStack =>
      SlotNo -> m [LeiosPoint]
  -- ^ EBs whose closure is complete and whose announcer is no older than the
  -- given slot. Seeds the ChainDB's acquired-closures set at startup.
  --
  -- The LeiosDB does not track completeness, so this hashes the txs of every
  -- such EB against its body. Startup only.
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
  -- ^ Record an announced EB's point and expected size. If another point of
  -- the EB holds its body, the new point holds a copy of the body and closure.
  , writeEbBody :: HasCallStack => LeiosPoint -> EndorserBlock -> m (Promise m ())
  -- ^ Persist an EB body into each point of the EB.
  --
  -- Its point must already have been written, by 'writeEbPoint'.
  , writeTxs ::
      HasCallStack =>
      LeiosPoint -> [(TxOffset, ByteString)] -> m (Promise m ())
  -- ^ Persist tx bodies for the EB identified by 'LeiosPoint'. Whether that
  -- completes the EB is for the caller to know: the LeiosDB does not track it.
  --
  -- No backend uses the point's slot: the tx bytes belong to the EB body,
  -- which every point with the same EB hash shares.
  --
  -- Each transaction is given by its offset into the EB body and its bytes.
  --
  -- ASSUMPTION: the EB body must already be written, otherwise the txs are dropped.
  --
  -- ASSUMPTION: each transaction's bytes hash to the tx hash the EB body lists
  -- at its offset. The implementations do not check this.
  --
  -- A transaction may be dropped by the implementation if:
  -- - is not in the body,
  -- - or its size is not the declared one.
  --
  -- A transaction written again overwrites itself, with the same bytes.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "LeiosDbWriter" (LeiosDbWriter m)

-- | The result of a submitted write.
--
-- 'await' rethrows whatever the write threw, in the awaiting thread, as a
-- 'LeiosDbException' naming the write and the site that submitted it.
-- Awaiting twice is fine. Not awaiting does not discard a failure: a failed
-- write is not survivable and stops the writer either way.
newtype Promise m a = Promise {await :: m a}
  deriving stock Functor

-- | Await every promise, in order.
awaitAll :: (Foldable t, Applicative m) => t (Promise m ()) -> m ()
awaitAll = traverse_ await

withReader :: MonadThrow m => LeiosDbHandle m -> (LeiosDbReader m -> m a) -> m a
withReader db = bracket (openReader db) closeReader

withWriter :: MonadThrow m => LeiosDbHandle m -> (LeiosDbWriter m -> m a) -> m a
withWriter db = bracket (openWriter db) closeWriter

allocateReader :: IOLike m => ResourceRegistry m -> LeiosDbHandle m -> m (LeiosDbReader m)
allocateReader registry db = snd <$> allocate registry (\_ -> openReader db) closeReader

allocateWriter :: IOLike m => ResourceRegistry m -> LeiosDbHandle m -> m (LeiosDbWriter m)
allocateWriter registry db = snd <$> allocate registry (\_ -> openWriter db) closeWriter

-- | Write an endorser block that this node forged to the LeiosDB.
--
-- The slot is the slot of the ranking block that announces the endorser block.
-- That ranking block must announce the 'endorserBlockHash' and the
-- 'encodeEndorserBlockSize' of the endorser block body.
--
-- This function does not await the writes. On SQLite, it blocks while the
-- write queue is full, and if a write fails, the node stops. Peers can get the
-- header before the endorser block is stored.
storeForgedEb :: Monad m => LeiosDbWriter m -> SlotNo -> ForgedEndorserBlock -> m ()
storeForgedEb writer slot forgedEb = do
  _ <- writeEbPoint writer point (encodeEndorserBlockSize body)
  _ <- writeEbBody writer point body
  void $ writeTxs writer point (zip [0 ..] (toList (forgedEbTxs forgedEb)))
 where
  body = forgedEbBody forgedEb
  point = MkLeiosPoint slot (endorserBlockHash body)
