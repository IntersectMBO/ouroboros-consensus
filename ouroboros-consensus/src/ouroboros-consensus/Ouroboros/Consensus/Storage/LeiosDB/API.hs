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
  , LocalFills
  , TxOffset
  ) where

import Cardano.Slotting.Slot (SlotNo)
import Control.Monad (void)
import Control.ResourceRegistry (ResourceRegistry, allocate)
import Data.ByteString (ByteString)
import Data.Foldable (toList, traverse_)
import Data.IntMap.Strict (IntMap)
import GHC.Stack (HasCallStack)
import NoThunks.Class (OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlock
  , ForgedEndorserBlock
  , LeiosPoint (..)
  , TxLocation
  , TxOffset
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
  , garbageCollect :: HasCallStack => m ()
  -- ^ Delete the volatile EBs announced before the promoted slot.
  --
  -- See 'sqlGarbageCollect' for the SQL backend implementation.
  , leiosDbPromoteToImmutable :: HasCallStack => SlotNo -> [LeiosPoint] -> m ()
  -- ^ Copy the given EBs' bodies and tx closures from volatile into immutable
  -- LeiosDb, then advance the promoted slot to the given one, the ImmutableDB
  -- tip's once the blocks certifying them are appended. Takes a batch: the
  -- caller holds the ImmutableDB write lock, so this is one transaction per
  -- 'copyToImmutableDB' pass, not one per certified EB. Throws if an EB's
  -- closure is incomplete, advancing nothing.
  --
  -- The promoted slot goes back only by 'leiosDbTruncateImmutable'. Reopened,
  -- it is the one after the newest promoted point.
  , leiosDbTruncateImmutable :: HasCallStack => SlotNo -> m ()
  -- ^ Delete the immutable EBs announced at or after the slot, the ImmutableDB
  -- tip's, and set the promoted slot to it: a block of the ImmutableDB
  -- certifies only an EB announced before its tip. Restores, when the ChainDB
  -- opens, that the immutable partition does not run ahead of the ImmutableDB,
  -- which a crash, a truncation or a lost row can break. The volatile partition
  -- keeps its EBs, so a deleted EB that is still there is promoted again, and
  -- any other fetched again.
  , leiosDbSampleStats :: HasCallStack => m LeiosDbStats
  -- ^ Sample 'LeiosDbStats' counters.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "LeiosDbHandle" (LeiosDbHandle m)

-- | Query API into the LeiosDb. Do not use a reader in two concurrent threads.
--
-- The reads of a point older than the promoted slot go to the immutable
-- partition, which holds an EB at the point it was certified at, and those of
-- any other point to the volatile one. Neither falls back to the other, unless
-- a promotion overtook the read.
data LeiosDbReader m = LeiosDbReader
  { closeReader :: m ()
  , lookupEbBody :: HasCallStack => LeiosPoint -> m (Maybe ByteString)
  -- ^ The EB "body" at this point, in the bytes it arrived in
  -- ('Ouroboros.Consensus.Leios.Types.serialiseEndorserBlock'), or 'Nothing'
  -- if the LeiosDB does not hold it there.
  , lookupEbClosure :: HasCallStack => LeiosPoint -> m (Maybe [ByteString])
  -- ^ The EB "closure" at this point: the bytes of its txs in body order, or
  -- 'Nothing' unless each of them is written. The body lists their hashes.
  , batchRetrieveTxs ::
      HasCallStack =>
      LeiosPoint -> [TxOffset] -> m [(TxOffset, Maybe ByteString)]
  -- ^ Tx bytes for a batch of offsets into the EB at one point, in ascending
  -- order: the offsets the body declares, each with its bytes, or 'Nothing' if
  -- not written.
  , isImmutable :: HasCallStack => LeiosPoint -> m Bool
  -- ^ Whether the immutable partition holds the EB at this point. Checks the
  -- ImmutableDB against the LeiosDB when the ChainDB opens.
  , scanCompleteEbClosuresNotOlderThanSlot ::
      HasCallStack =>
      SlotNo -> m [LeiosPoint]
  -- ^ The points no older than the given slot whose closure is complete. Seeds
  -- the ChainDB's acquired-closures set at startup.
  --
  -- A closure is complete once each of its txs is written. The LeiosDB checks
  -- no hashes: 'writeTxs' requires its caller to. Startup only.
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
  , writeEbBody ::
      HasCallStack =>
      LeiosPoint -> EndorserBlock -> LocalFills -> m (Promise m [TxOffset])
  -- ^ Persist an EB body at this point, recording the point. Every point holds
  -- its own body and txs, even when another point names the same EB.
  --
  -- Then fill what it can from the bytes the LeiosDB holds: 'LocalFills' maps
  -- an offset in this body to the 'TxLocation' of the same tx at another
  -- point, copied in the same transaction. A fill
  -- lands only if the source's bytes hash to the tx this body lists at its
  -- offset, so a stale or wrong location fills nothing, and that tx stays
  -- fetchable.
  --
  -- Returns the offsets at which the point holds its txs once the write is
  -- done: those filled, or written since an earlier delivery of the same body.
  --
  -- PREREQ: the EB's body and closure sizes are within the protocol limits.
  -- The LeiosDB allocates the closure at the size the body declares.
  , writeTxs ::
      HasCallStack =>
      LeiosPoint -> [(TxOffset, ByteString)] -> m (Promise m [TxOffset])
  -- ^ Persist tx bodies for the EB at this point, and only there. Whether that
  -- completes the EB is for the caller to know: the LeiosDB does not track it.
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
  --
  -- Returns the offsets of the transactions dropped because the EB referenced
  -- them with another size: no such closure can be stored, and an honest peer
  -- never serves one.
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

-- | Fills from local bytes: fill the EB's tx at each offset from the tx's
-- 'TxLocation' at another point. See 'writeEbBody'.
type LocalFills = IntMap {- TxOffset -} TxLocation

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
  _ <- writeEbBody writer point body mempty
  void $ writeTxs writer point (zip [0 ..] (toList (forgedEbTxs forgedEb)))
 where
  body = forgedEbBody forgedEb
  point = MkLeiosPoint slot (endorserBlockHash body)
