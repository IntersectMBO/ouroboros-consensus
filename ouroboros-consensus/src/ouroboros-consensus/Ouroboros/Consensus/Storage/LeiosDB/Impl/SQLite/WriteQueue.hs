{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The submission side of the single writer: the jobs it serves and the
-- bounded queue that carries them. The worker that drains the queue is
-- 'Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.startWriter'.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.WriteQueue
  ( WriteJob (..)
  , WriteResult
  , WriteQueue (..)
  , submitJob
  , describeJob
  , failJob
  , maxJobsBetweenMaintenance
  , writerQueueDepth
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTBQueue
  , StrictTMVar
  , StrictTVar
  , isFullTBQueue
  , newEmptyTMVarIO
  , putTMVar
  , readTMVar
  , readTVar
  , writeTBQueue
  )
import Control.Exception (SomeException, throwIO)
import Control.Monad (join)
import Control.Monad.Class.MonadThrow (onException)
import Control.Tracer (Tracer, traceWith)
import Data.ByteString (ByteString)
import GHC.Stack (HasCallStack)
import qualified GHC.Stack
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EbHash (..)
  , LeiosEb
  , LeiosPoint (..)
  , TxHash (..)
  , leiosEbTxs
  )
import Ouroboros.Consensus.Storage.LeiosDB.API (CompletedEbs, Promise (..))
import Ouroboros.Consensus.Storage.LeiosDB.Exception
  ( LeiosDbException (..)
  , LeiosDbWriteFailure (..)
  )
import Ouroboros.Consensus.Storage.LeiosDB.Trace (TraceLeiosDb (..))
import Ouroboros.Consensus.Util.IOLike (atomically)

-- * The single writer

-- | One queued write, carrying the variable its result lands in.
--
-- The insert jobs come from 'LeiosDbWriter', the rest from the handle's
-- promote\/GC entry points. One worker running them all is what makes the
-- single-writer property total: no two in-process connections ever contend
-- for a write lock on the volatile partition. Sweeping is not a job at all
-- -- the worker does it between jobs; see 'startWriter'.
data WriteJob
  = WriteEbPoint
      -- | the announced EB
      !LeiosPoint
      -- | its announced size
      !BytesSize
      -- | where the worker puts the result
      !(WriteResult ())
  | WriteEbBody
      -- | the EB's point
      !LeiosPoint
      -- | the EB body
      !LeiosEb
      -- | where the worker puts the result
      !(WriteResult CompletedEbs)
  | WriteTxs
      -- | txs with their bytes
      ![(TxHash, ByteString)]
      -- | where the worker puts the result
      !(WriteResult CompletedEbs)
  | -- | Does nothing; awaiting it after the queue's FIFO order means every
    -- write submitted before it has landed.
    Flush !(WriteResult ())
  | -- | Pin EBs for promotion; see 'sqlPromoteToImmutable'.
    PinEb
      -- | the EBs to pin
      ![EbHash]
      -- | where the worker puts the result
      !(WriteResult ())
  | -- | The volatile half of a copy: the EB is in the immutable partition
    -- now, so its volatile rows are evictable. Submitted by 'startCopier'.
    MarkCopied
      -- | the EBs now in the immutable partition
      ![EbHash]
      -- | where the worker puts the result
      !(WriteResult ())
  | -- | The GC MARK phase; see 'gcMark'.
    GcMark
      -- | the GC frontier slot
      !SlotNo
      -- | where the worker puts the result
      !(WriteResult ())
  | -- | Stop the worker: awaiting it after the queue's FIFO order means every
    -- write submitted before it has landed and the connections are closed.
    Shutdown !(WriteResult ())

type WriteResult a = StrictTMVar IO (Either SomeException a)

-- | The writer's submission side: the job queue, and -- once the worker has
-- stopped -- the exception every submission throws instead of queueing.
data WriteQueue = WriteQueue
  { wqJobs :: !(StrictTBQueue IO WriteJob)
  , wqSealed :: !(StrictTVar IO (Maybe SomeException))
  , wqTracer :: !(Tracer IO TraceLeiosDb)
  }

-- | Hand a job to the writer; the 'Promise' waits for its result. Blocks
-- only while the queue is full, which is the backpressure -- and which is
-- traced, since it is the one thing about the writer that no other trace
-- reports: every producer is now waiting on it.
--
-- Checking the seal and queueing are one transaction, so after the worker
-- seals the queue no job can slip in unserved: submission throws the
-- worker's parting exception instead -- also waking any submitter that was
-- blocked on a full queue.
submitJob :: HasCallStack => WriteQueue -> (WriteResult a -> WriteJob) -> IO (Promise IO a)
submitJob WriteQueue{wqJobs, wqSealed, wqTracer} mkJob = do
  resultVar <- newEmptyTMVarIO
  let job = mkJob resultVar
      -- Failures surface far from their submitter -- on the worker, or on
      -- whichever thread awaits -- so name the write and its submission site.
      wrap cause =
        LeiosDbWriteException
          LeiosDbWriteFailure
            { ldwfWriteJob = describeJob job
            , ldwfSubmittedFrom = GHC.Stack.prettyCallStack GHC.Stack.callStack
            , ldwfWriteFailure = cause
            }
      -- Take a slot if there is one; 'Left' once the queue is sealed.
      offer =
        readTVar wqSealed >>= \case
          Just cause -> pure (Left cause)
          Nothing ->
            isFullTBQueue wqJobs >>= \case
              True -> pure (Right False)
              False -> Right True <$ writeTBQueue wqJobs job
      -- And wait for one otherwise.
      park =
        readTVar wqSealed >>= \case
          Just cause -> pure (throwIO (wrap cause))
          Nothing -> writeTBQueue wqJobs job >> pure (pure ())
  atomically offer >>= \case
    Left cause -> throwIO (wrap cause)
    Right True -> pure ()
    Right False -> do
      traceWith wqTracer $ TraceLeiosDbWriterQueueFull (describeJob job)
      -- Otherwise silent: no exception reaches the write path, and there is
      -- no continuation yet to carry one.
      join (atomically park)
        `onException` traceWith wqTracer (TraceLeiosDbWriteAbandoned (describeJob job))
  pure $ Promise (either (throwIO . wrap) pure =<< atomically (readTMVar resultVar))

-- | Name a job for 'LeiosDbWriteException': what it is and what it is about,
-- never its payload.
describeJob :: WriteJob -> String
describeJob = \case
  WriteEbPoint point _ _ -> "WriteEbPoint " <> show point
  WriteEbBody point eb _ -> "WriteEbBody " <> show point <> " (" <> show (length (leiosEbTxs eb)) <> " txs)"
  WriteTxs txs _ -> "WriteTxs (" <> show (length txs) <> " txs)"
  Flush _ -> "Flush"
  PinEb ebHashes _ -> "PinEb (" <> show (length ebHashes) <> " ebs)"
  MarkCopied ebHashes _ -> "MarkCopied (" <> show (length ebHashes) <> " ebs)"
  GcMark slot _ -> "GcMark " <> show slot
  Shutdown _ -> "Shutdown"

-- | Publish the worker's parting exception as a queued job's result.
failJob :: SomeException -> WriteJob -> IO ()
failJob cause = \case
  WriteEbPoint _ _ rv -> put rv
  WriteEbBody _ _ rv -> put rv
  WriteTxs _ rv -> put rv
  Flush rv -> put rv
  PinEb _ rv -> put rv
  MarkCopied _ rv -> put rv
  GcMark _ rv -> put rv
  Shutdown rv -> put rv
 where
  put :: WriteResult a -> IO ()
  put rv = atomically $ putTMVar rv (Left cause)

-- | How many queued jobs the writer serves back-to-back before it takes a
-- turn of maintenance anyway.
--
-- Maintenance runs when the queue drains, which is the common case; this is
-- the floor under a queue that never does. One queue-full: under saturation
-- maintenance still gets a turn as often as the producers can refill, and
-- the resulting share (one turn in @'writerQueueDepth' + 1@) is far above
-- what copying and eviction ask for -- they are per promoted or expired EB,
-- not per write. 'TraceLeiosDbStats' is where a volatile partition that
-- still grows would show up.
maxJobsBetweenMaintenance :: Int
maxJobsBetweenMaintenance = fromIntegral writerQueueDepth

-- | Depth of the write queue. One slot per producer that can be mid-write --
-- each upstream peer's fetch client, the forge, and the maintenance
-- schedulers (copier, sweeper, the ChainDB's GC and promote calls) -- and a
-- little slack. Deliberately shallow: depth beyond that adds no throughput
-- (there is one worker) and only delays the commit-time notifications that
-- gate vote scheduling and chain selection. A producer that outruns the disk
-- blocks on submission, which is the backpressure.
writerQueueDepth :: Natural
writerQueueDepth = numUpstreamPeers + forge + maintenance + slack
 where
  numUpstreamPeers = 20
  forge = 1
  maintenance = 4
  slack = 2
