{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Call-traced variants of 'LeiosDbHandle', 'LeiosDbReader', and
-- 'LeiosDbWriter'. Each method takes an additional 'CallCtx' as its first
-- argument, emits 'CallStart' / 'CallEnd' events around the delegated call,
-- and is otherwise identical to the underlying implementation.
module LeiosDemoDb.WithCallTrace
  ( -- * Handle
    HandleWithCallTrace (..)
  , withCallTraceHandle
  , handleOpenReader
  , handleOpenWriter
  , withReader
  , withReaderAndWriter

    -- * Reader
  , ReaderWithCallTrace (..)
  , withCallTraceReader
  , readerClose

    -- * Writer
  , WriterWithCallTrace (..)
  , withCallTraceWriter
  , writerClose
  , withWriter
  ) where

import Cardano.Slotting.Slot (SlotNo)
import Control.Concurrent.Class.MonadSTM.Strict (StrictTChan)
import Control.Tracer (Tracer, traceWith)
import Data.ByteString (ByteString)
import LeiosDemoDb.Common
  ( CompletedEbs
  , LeiosDbHandle
  , LeiosDbReader
  , LeiosDbStats
  , LeiosDbWriter
  , LeiosEbNotification
  , Promise
  )
import qualified LeiosDemoDb.Common as DB
import LeiosDemoDb.Trace (TraceLeiosDb (..))
import LeiosDemoTypes (BytesSize, EbHash, LeiosEb, LeiosPoint, TxHash)
import LeiosUtils.CallTrace
  ( CallCtx
  , CallName
  , SomeJsonCallTrace (..)
  , callTraceVia
  )
import NoThunks.Class (NoThunks (..))
import Ouroboros.Consensus.Util.IOLike (IOLike, bracket)

-- | Like 'LeiosDbHandle' but every method takes a 'CallCtx' as its first
-- argument, which is used as the parent for the emitted call-trace event.
data HandleWithCallTrace m = HandleWithCallTrace
  { close :: CallCtx m -> m ()
  , openReader :: CallCtx m -> m (ReaderWithCallTrace m)
  , openWriter :: CallCtx m -> m (WriterWithCallTrace m)
  , subscribeEbNotifications :: CallCtx m -> m (StrictTChan m LeiosEbNotification)
  , leiosDbGarbageCollect :: CallCtx m -> SlotNo -> m ()
  , leiosDbPromoteToImmutable :: CallCtx m -> [LeiosPoint] -> m ()
  , leiosDbSampleStats :: CallCtx m -> m LeiosDbStats
  }

-- | Like 'LeiosDbReader' but every method takes a 'CallCtx' as its first
-- argument.
data ReaderWithCallTrace m = ReaderWithCallTrace
  { close :: CallCtx m -> m ()
  , lookupEbBody :: CallCtx m -> EbHash -> m [(TxHash, BytesSize)]
  , lookupEbClosure :: CallCtx m -> EbHash -> m (Maybe [(TxHash, ByteString)])
  , batchRetrieveTxs :: CallCtx m -> EbHash -> [Int] -> m [(Int, TxHash, Maybe ByteString)]
  , scanEbPoints :: CallCtx m -> m [(SlotNo, EbHash)]
  , scanCompleteEbClosuresNotOlderThanSlot :: CallCtx m -> SlotNo -> m [LeiosPoint]
  }

-- | Like 'LeiosDbWriter' but every method takes a 'CallCtx' as its first
-- argument. Only the submission side of each write is traced; the returned
-- 'Promise' is not modified.
data WriterWithCallTrace m = WriterWithCallTrace
  { close :: CallCtx m -> m ()
  , writeEbPoint :: CallCtx m -> LeiosPoint -> BytesSize -> m (Promise m ())
  , writeEbBody :: CallCtx m -> LeiosPoint -> LeiosEb -> m (Promise m CompletedEbs)
  , writeTxs :: CallCtx m -> [(TxHash, ByteString)] -> m (Promise m CompletedEbs)
  }

-- | Wrap a 'LeiosDbHandle': each method call-traces itself using the 'CallCtx'
-- supplied by the call site, then delegates to the underlying handle.
withCallTraceHandle ::
  IOLike m =>
  Tracer m TraceLeiosDb ->
  LeiosDbHandle m ->
  HandleWithCallTrace m
withCallTraceHandle tracer h =
  HandleWithCallTrace
    { close = \ctx ->
        callWith tracer ctx "leios-db-close" "" $ \_ctx' ->
          h.close
    , openReader = \ctx ->
        callWith tracer ctx "leios-db-open-reader" "" $ \_ctx' ->
          withCallTraceReader tracer <$> DB.openReader h
    , openWriter = \ctx ->
        callWith tracer ctx "leios-db-open-writer" "" $ \_ctx' ->
          withCallTraceWriter tracer <$> DB.openWriter h
    , subscribeEbNotifications = \ctx ->
        callWith tracer ctx "leios-db-subscribe-eb-notifications" "" $ \_ctx' ->
          DB.subscribeEbNotifications h
    , leiosDbGarbageCollect = \ctx slot ->
        callWith tracer ctx "leios-db-garbage-collect" (show slot) $ \_ctx' ->
          DB.leiosDbGarbageCollect h slot
    , leiosDbPromoteToImmutable = \ctx points ->
        callWith tracer ctx "leios-db-promote-to-immutable" (show (length points)) $ \_ctx' ->
          DB.leiosDbPromoteToImmutable h points
    , leiosDbSampleStats = \ctx ->
        callWith tracer ctx "leios-db-sample-stats" "" $ \_ctx' ->
          DB.leiosDbSampleStats h
    }

-- | Wrap a 'LeiosDbReader': each method call-traces itself using the 'CallCtx'
-- supplied by the call site.
withCallTraceReader ::
  IOLike m =>
  Tracer m TraceLeiosDb ->
  LeiosDbReader m ->
  ReaderWithCallTrace m
withCallTraceReader tracer r =
  ReaderWithCallTrace
    { close = \ctx ->
        callWith tracer ctx "leios-db-reader-close" "" $ \_ctx' ->
          r.close
    , lookupEbBody = \ctx ebHash ->
        callWith tracer ctx "leios-db-lookup-eb-body" (show ebHash) $ \_ctx' ->
          DB.lookupEbBody r ebHash
    , lookupEbClosure = \ctx ebHash ->
        callWith tracer ctx "leios-db-lookup-eb-closure" (show ebHash) $ \_ctx' ->
          DB.lookupEbClosure r ebHash
    , batchRetrieveTxs = \ctx ebHash offsets ->
        callWith tracer ctx "leios-db-batch-retrieve-txs" (show ebHash) $ \_ctx' ->
          DB.batchRetrieveTxs r ebHash offsets
    , scanEbPoints = \ctx ->
        callWith tracer ctx "leios-db-scan-eb-points" "" $ \_ctx' ->
          DB.scanEbPoints r
    , scanCompleteEbClosuresNotOlderThanSlot = \ctx slot ->
        callWith tracer ctx "leios-db-scan-complete-eb-closures" (show slot) $ \_ctx' ->
          DB.scanCompleteEbClosuresNotOlderThanSlot r slot
    }

-- | Wrap a 'LeiosDbWriter': each method call-traces itself using the 'CallCtx'
-- supplied by the call site.
withCallTraceWriter ::
  IOLike m =>
  Tracer m TraceLeiosDb ->
  LeiosDbWriter m ->
  WriterWithCallTrace m
withCallTraceWriter tracer w =
  WriterWithCallTrace
    { close = \ctx ->
        callWith tracer ctx "leios-db-writer-close" "" $ \_ctx' ->
          w.close
    , writeEbPoint = \ctx point size ->
        callWith tracer ctx "leios-db-write-eb-point" (show point) $ \_ctx' ->
          DB.writeEbPoint w point size
    , writeEbBody = \ctx point eb ->
        callWith tracer ctx "leios-db-write-eb-body" (show point) $ \_ctx' ->
          DB.writeEbBody w point eb
    , writeTxs = \ctx txs ->
        callWith tracer ctx "leios-db-write-txs" (show (length txs)) $ \_ctx' ->
          DB.writeTxs w txs
    }

-- | Instrument one call: emit 'CallStart' before and 'CallEnd' after the
-- action. The result is projected to '()' in the trace so any return type is
-- accepted; the actual result is returned unchanged. The argument is a
-- 'String' built from the relevant call inputs.
callWith ::
  IOLike m =>
  Tracer m TraceLeiosDb ->
  CallCtx m ->
  CallName ->
  String ->
  (CallCtx m -> m r) ->
  m r
callWith tracer =
  callTraceVia
    (\_ -> ())
    (traceWith tracer . TraceLeiosDbCall . SomeJsonCallTrace)

-- | Open a per-call reader from a 'HandleWithCallTrace'. Provided as an
-- explicit function so callers without 'DuplicateRecordFields' can invoke it
-- without relying on 'OverloadedRecordDot' disambiguation.
handleOpenReader :: HandleWithCallTrace m -> CallCtx m -> m (ReaderWithCallTrace m)
handleOpenReader h = h.openReader

-- | Open a per-call writer from a 'HandleWithCallTrace'.
handleOpenWriter :: HandleWithCallTrace m -> CallCtx m -> m (WriterWithCallTrace m)
handleOpenWriter h = h.openWriter

-- | Close a 'ReaderWithCallTrace'. Provided as an explicit function for the
-- same reason as 'handleOpenReader'.
readerClose :: ReaderWithCallTrace m -> CallCtx m -> m ()
readerClose r = r.close

-- | Close a 'WriterWithCallTrace'.
writerClose :: WriterWithCallTrace m -> CallCtx m -> m ()
writerClose w = w.close

-- | Bracket-style equivalent of 'LeiosDemoDb.withReader' for a
-- 'HandleWithCallTrace': opens a reader, runs the continuation, then closes.
withReader ::
  IOLike m =>
  HandleWithCallTrace m ->
  CallCtx m ->
  (ReaderWithCallTrace m -> m a) ->
  m a
withReader h cctx = bracket (handleOpenReader h cctx) (\r -> readerClose r cctx)

-- | Bracket-style equivalent of 'LeiosDemoDb.withWriter' for a
-- 'HandleWithCallTrace': opens a writer, runs the continuation, then closes.
withWriter ::
  IOLike m =>
  HandleWithCallTrace m ->
  CallCtx m ->
  (WriterWithCallTrace m -> m a) ->
  m a
withWriter h cctx = bracket (handleOpenWriter h cctx) (\w -> writerClose w cctx)

-- | Bracket-style open of both a reader and a writer from a 'HandleWithCallTrace'.
withReaderAndWriter ::
  IOLike m =>
  HandleWithCallTrace m ->
  CallCtx m ->
  (ReaderWithCallTrace m -> WriterWithCallTrace m -> m a) ->
  m a
withReaderAndWriter h cctx f = withReader h cctx $ \r -> withWriter h cctx $ \w -> f r w

instance NoThunks (HandleWithCallTrace m) where
  showTypeOf _ = "HandleWithCallTrace"
  wNoThunks _ctx _a = return Nothing

instance NoThunks (ReaderWithCallTrace m) where
  showTypeOf _ = "ReaderWithCallTrace"
  wNoThunks _ctx _a = return Nothing

instance NoThunks (WriterWithCallTrace m) where
  showTypeOf _ = "WriterWithCallTrace"
  wNoThunks _ctx _a = return Nothing
