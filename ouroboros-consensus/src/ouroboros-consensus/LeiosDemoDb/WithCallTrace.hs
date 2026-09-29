{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Call-traced variants of 'LeiosDemoDb.Common.LeiosDbHandle',
-- 'LeiosDemoDb.Common.LeiosDbReader', and 'LeiosDemoDb.Common.LeiosDbWriter'.
-- Each method takes an additional 'CallCtx' as its first argument, emits
-- 'CallStart' / 'CallEnd' events around the delegated call, and is otherwise
-- identical to the underlying implementation.
module LeiosDemoDb.WithCallTrace
  ( -- * Handle
    LeiosDbHandle (..)
  , withCallTraceHandle
  , withReader
  , withReaderAndWriter

    -- * Reader
  , LeiosDbReader (..)
  , mkCallTraceReader

    -- * Writer
  , LeiosDbWriter (..)
  , mkCallTraceWriter
  , withWriter

    -- * Promise
  , Promise (..)
  , awaitAll
  ) where

import Cardano.Slotting.Slot (SlotNo)
import Control.Concurrent.Class.MonadSTM.Strict (StrictTChan)
import Control.Tracer (Tracer)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import LeiosDemoDb.Common
  ( CompletedEbs
  , LeiosDbStats
  , LeiosEbNotification
  )
import qualified LeiosDemoDb.Common as DB
import LeiosDemoTypes (BytesSize, EbHash, LeiosEb, LeiosPoint, TxHash)
import LeiosUtils.CallTrace.Json
  ( CallCtx
  , CallName
  , CallTrace (..)
  , callTraceVia
  )
import NoThunks.Class (NoThunks (..))
import Ouroboros.Consensus.Util.IOLike (IOLike, bracket)

-- | Like 'DB.LeiosDbHandle' but every method takes a 'CallCtx' as its first
-- argument, which is used as the parent for the emitted call-trace event.
data LeiosDbHandle m = LeiosDbHandle
  { close :: CallCtx m -> m ()
  , openReader :: CallCtx m -> m (LeiosDbReader m)
  , openWriter :: CallCtx m -> m (LeiosDbWriter m)
  , subscribeEbNotifications :: CallCtx m -> m (StrictTChan m LeiosEbNotification)
  , leiosDbGarbageCollect :: CallCtx m -> SlotNo -> m ()
  , leiosDbPromoteToImmutable :: CallCtx m -> [LeiosPoint] -> m ()
  , leiosDbSampleStats :: CallCtx m -> m LeiosDbStats
  }

-- | Like 'DB.LeiosDbReader' but every method takes a 'CallCtx' as its first
-- argument.
data LeiosDbReader m = LeiosDbReader
  { closeReader :: CallCtx m -> m ()
  , lookupEbBody :: CallCtx m -> EbHash -> m [(TxHash, BytesSize)]
  , lookupEbClosure :: CallCtx m -> EbHash -> m (Maybe [(TxHash, ByteString)])
  , batchRetrieveTxs :: CallCtx m -> EbHash -> [Int] -> m [(Int, TxHash, Maybe ByteString)]
  , scanEbPoints :: CallCtx m -> m [(SlotNo, EbHash)]
  , scanCompleteEbClosuresNotOlderThanSlot :: CallCtx m -> SlotNo -> m [LeiosPoint]
  }

-- | Like 'DB.Promise' but 'await' takes a 'CallCtx', so the blocking half of
-- an async write is visible in the call trace under a name that identifies
-- which write operation is being awaited.
newtype Promise m a = Promise {await :: CallCtx m -> m a}

instance Functor m => Functor (Promise m) where
  fmap f (Promise g) = Promise (fmap f . g)

-- | Await every promise in order, passing the same 'CallCtx' to each.
awaitAll :: (Foldable t, Applicative m) => CallCtx m -> t (Promise m ()) -> m ()
awaitAll cctx = traverse_ (\p -> await p cctx)

-- | Wrap a raw 'DB.Promise' so that awaiting it emits a call-trace event.
-- @arg@ mirrors the argument of the corresponding submit call.
-- @projectResult@ projects the result to a JSON for tracing; pass
-- @'const' ()@ when the result carries no useful information.
wrapPromise ::
  (IOLike m, Aeson.ToJSON arg, Aeson.ToJSON res') =>
  Tracer m CallTrace ->
  CallName ->
  arg ->
  (res -> res') ->
  DB.Promise m res ->
  Promise m res
wrapPromise tracer name arg projectResult rawPromise =
  Promise $ \ctx ->
    callTraceVia
      projectResult
      tracer
      ctx
      name
      arg
      $ \_ctx' -> DB.await rawPromise

-- | Like 'DB.LeiosDbWriter' but every method takes a 'CallCtx' as its first
-- argument. Both the submission and the 'await' of each write are traced; the
-- returned 'Promise' carries the await call name baked in.
data LeiosDbWriter m = LeiosDbWriter
  { closeWriter :: CallCtx m -> m ()
  , writeEbPoint :: CallCtx m -> LeiosPoint -> BytesSize -> m (Promise m ())
  , writeEbBody :: CallCtx m -> LeiosPoint -> LeiosEb -> m (Promise m CompletedEbs)
  , writeTxs :: CallCtx m -> [(TxHash, ByteString)] -> m (Promise m CompletedEbs)
  }

-- | Wrap a 'DB.LeiosDbHandle': each method call-traces itself using the
-- 'CallCtx' supplied by the call site, then delegates to the underlying handle.
withCallTraceHandle ::
  IOLike m =>
  Tracer m CallTrace ->
  DB.LeiosDbHandle m ->
  LeiosDbHandle m
withCallTraceHandle tracer h =
  let DB.LeiosDbHandle{close = rawClose} = h
   in LeiosDbHandle
        { close = \ctx ->
            callWith tracer ctx "leios-db-close" "" $ \_ctx' ->
              rawClose
        , openReader = \ctx ->
            callWith tracer ctx "leios-db-open-reader" "" $ \_ctx' ->
              mkCallTraceReader tracer <$> DB.openReader h
        , openWriter = \ctx ->
            callWith tracer ctx "leios-db-open-writer" "" $ \_ctx' ->
              mkCallTraceWriter tracer <$> DB.openWriter h
        , subscribeEbNotifications = \ctx ->
            callWith tracer ctx "leios-db-subscribe-eb-notifications" "" $ \_ctx' ->
              DB.subscribeEbNotifications h
        , leiosDbGarbageCollect = \ctx slot ->
            callWith tracer ctx "leios-db-garbage-collect" (show slot) $ \_ctx' ->
              DB.leiosDbGarbageCollect h slot
        , leiosDbPromoteToImmutable = \ctx points ->
            callWith tracer ctx "leios-db-promote-to-immutable" (length points) $ \_ctx' ->
              DB.leiosDbPromoteToImmutable h points
        , leiosDbSampleStats = \ctx ->
            callWith tracer ctx "leios-db-sample-stats" "" $ \_ctx' ->
              DB.leiosDbSampleStats h
        }

-- | Wrap a 'DB.LeiosDbReader': each method call-traces itself using the
-- 'CallCtx' supplied by the call site.
mkCallTraceReader ::
  IOLike m =>
  Tracer m CallTrace ->
  DB.LeiosDbReader m ->
  LeiosDbReader m
mkCallTraceReader tracer r =
  let DB.LeiosDbReader{close = rawClose} = r
   in LeiosDbReader
        { closeReader = \ctx ->
            callWith tracer ctx "leios-db-reader-close" "" $ \_ctx' ->
              rawClose
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

-- | Wrap a 'DB.LeiosDbWriter': each method call-traces itself using the
-- 'CallCtx' supplied by the call site.
mkCallTraceWriter ::
  IOLike m =>
  Tracer m CallTrace ->
  DB.LeiosDbWriter m ->
  LeiosDbWriter m
mkCallTraceWriter tracer w =
  let DB.LeiosDbWriter{close = rawClose} = w
   in LeiosDbWriter
        { closeWriter = \ctx ->
            callWith tracer ctx "leios-db-writer-close" "" $ \_ctx' ->
              rawClose
        , writeEbPoint = \ctx point size ->
            callWith tracer ctx "leios-db-write-eb-point" (show point) $ \_ctx' ->
              wrapPromise tracer "await-leios-db-write-eb-point" (show point) (const ())
                <$> DB.writeEbPoint w point size
        , writeEbBody = \ctx point eb ->
            callWith tracer ctx "leios-db-write-eb-body" (show point) $ \_ctx' ->
              wrapPromise tracer "await-leios-db-write-eb-body" (show point) length <$> DB.writeEbBody w point eb
        , writeTxs = \ctx txs ->
            callWith tracer ctx "leios-db-write-txs" (length txs) $ \_ctx' ->
              wrapPromise tracer "await-leios-db-write-txs" (length txs) length <$> DB.writeTxs w txs
        }

-- | Instrument one call: emit 'CallStart' before and 'CallEnd' after the
-- action. The result is projected to '()' in the trace so any return type is
-- accepted; the actual result is returned unchanged.
callWith ::
  (IOLike m, Aeson.ToJSON a) =>
  Tracer m CallTrace ->
  CallCtx m ->
  CallName ->
  a ->
  (CallCtx m -> m r) ->
  m r
callWith tracer =
  callTraceVia
    (\_ -> ())
    tracer

-- | Bracket-style equivalent of 'LeiosDemoDb.withReader' for a
-- 'LeiosDbHandle': opens a reader, runs the continuation, then closes.
withReader ::
  IOLike m =>
  LeiosDbHandle m ->
  CallCtx m ->
  (LeiosDbReader m -> m a) ->
  m a
withReader h cctx k = bracket (openReader h cctx) (\r -> closeReader r cctx) k

-- | Bracket-style equivalent of 'LeiosDemoDb.withWriter' for a
-- 'LeiosDbHandle': opens a writer, runs the continuation, then closes.
withWriter ::
  IOLike m =>
  LeiosDbHandle m ->
  CallCtx m ->
  (LeiosDbWriter m -> m a) ->
  m a
withWriter h cctx k = bracket (openWriter h cctx) (\w -> closeWriter w cctx) k

-- | Bracket-style open of both a reader and a writer from a 'LeiosDbHandle'.
withReaderAndWriter ::
  IOLike m =>
  LeiosDbHandle m ->
  CallCtx m ->
  (LeiosDbReader m -> LeiosDbWriter m -> m a) ->
  m a
withReaderAndWriter h cctx f = withReader h cctx $ \r -> withWriter h cctx $ \w -> f r w

instance NoThunks (LeiosDbHandle m) where
  showTypeOf _ = "LeiosDbHandle"
  wNoThunks _ctx _a = return Nothing

instance NoThunks (LeiosDbReader m) where
  showTypeOf _ = "LeiosDbReader"
  wNoThunks _ctx _a = return Nothing

instance NoThunks (LeiosDbWriter m) where
  showTypeOf _ = "LeiosDbWriter"
  wNoThunks _ctx _a = return Nothing
