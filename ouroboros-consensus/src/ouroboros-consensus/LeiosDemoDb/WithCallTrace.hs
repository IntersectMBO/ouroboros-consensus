{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Wrapper around 'LeiosDbConnection' that records each call via
-- 'LeiosUtils.CallTrace'.  Every endpoint takes an explicit 'CallCtx' as its
-- first argument so callers can compose calls into a structured trace tree.
module LeiosDemoDb.WithCallTrace
  ( LeiosDbWithCallTrace (..)
  , newLeiosDbWithCallTrace
  ) where

import Cardano.Slotting.Slot (SlotNo)
import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM)
import Control.Monad.Class.MonadTime.SI (MonadMonotonicTime)
import Data.ByteString (ByteString)
import LeiosDemoDb.Common (CompletedEbs, LeiosDbConnection (..))
import LeiosDemoTypes (BytesSize, EbHash, LeiosEb, LeiosPoint, TxHash)
import LeiosUtils.CallTrace
  ( CallCtx
  , MonadAllocationCounter
  , SomeJsonCallTrace (..)
  , callTraceSameThreadVia
  )

-- | A mirror of 'LeiosDbConnection' where each endpoint takes an explicit
-- 'CallCtx' so that call timing and allocation can be recorded via
-- 'LeiosUtils.CallTrace'.
--
-- Construct via 'newLeiosDbWithCallTrace', which wraps an existing
-- 'LeiosDbConnection' and forwards all calls to it while emitting
-- 'SomeJsonCallTrace' events to the supplied sink.
data LeiosDbWithCallTrace m = LeiosDbWithCallTrace
  { leiosDbInsertEbPoint :: CallCtx m -> LeiosPoint -> BytesSize -> m ()
  , leiosDbLookupEbBody :: CallCtx m -> EbHash -> m [(TxHash, BytesSize)]
  , leiosDbInsertEbBody :: CallCtx m -> LeiosPoint -> LeiosEb -> m CompletedEbs
  , leiosDbInsertTxs :: CallCtx m -> [(TxHash, ByteString)] -> m CompletedEbs
  , leiosDbBatchRetrieveTxs :: CallCtx m -> EbHash -> [Int] -> m [(Int, TxHash, Maybe ByteString)]
  , leiosDbLookupEbClosure :: CallCtx m -> EbHash -> m (Maybe [(TxHash, ByteString)])
  , leiosDbScanEbPoints :: CallCtx m -> m [(SlotNo, EbHash)]
  , leiosDbScanCompleteEbClosuresNotOlderThanSlot :: CallCtx m -> SlotNo -> m [LeiosPoint]
  }

-- | Wrap a 'LeiosDbConnection' so that every call is bracketed by
-- 'CallStart'\/'CallEnd' events forwarded to @emit@.
--
-- The call argument recorded in each trace event is a 'String' built from
-- 'show' on the relevant inputs (e.g. the 'EbHash' or 'LeiosPoint').
-- The result is projected to @()@ so no 'ToJSON' instance is needed on the
-- (often large) return values.
newLeiosDbWithCallTrace ::
  (MonadSTM m, MonadMonotonicTime m, MonadAllocationCounter m) =>
  -- | Sink for trace events; called once at call-start and once at call-end.
  (SomeJsonCallTrace -> m ()) ->
  LeiosDbConnection m ->
  LeiosDbWithCallTrace m
newLeiosDbWithCallTrace emit conn =
  LeiosDbWithCallTrace
    { leiosDbInsertEbPoint = \ctx point size ->
        go ctx "leios-db-insert-eb-point" (show point) $
          \_ -> connInsertEbPoint point size
    , leiosDbLookupEbBody = \ctx ebHash ->
        go ctx "leios-db-lookup-eb-body" (show ebHash) $
          \_ -> connLookupEbBody ebHash
    , leiosDbInsertEbBody = \ctx point eb ->
        go ctx "leios-db-insert-eb-body" (show point) $
          \_ -> connInsertEbBody point eb
    , leiosDbInsertTxs = \ctx txs ->
        go ctx "leios-db-insert-txs" (show (length txs) <> " txs") $
          \_ -> connInsertTxs txs
    , leiosDbBatchRetrieveTxs = \ctx ebHash offsets ->
        go ctx "leios-db-batch-retrieve-txs" (show ebHash) $
          \_ -> connBatchRetrieveTxs ebHash offsets
    , leiosDbLookupEbClosure = \ctx ebHash ->
        go ctx "leios-db-lookup-eb-closure" (show ebHash) $
          \_ -> connLookupEbClosure ebHash
    , leiosDbScanEbPoints = \ctx ->
        go ctx "leios-db-scan-eb-points" "()" $
          \_ -> connScanEbPoints
    , leiosDbScanCompleteEbClosuresNotOlderThanSlot = \ctx slotNo ->
        go ctx "leios-db-scan-complete-eb-closures-not-older-than-slot" (show slotNo) $
          \_ -> connScanComplete slotNo
    }
 where
  LeiosDbConnection
    { leiosDbInsertEbPoint = connInsertEbPoint
    , leiosDbLookupEbBody = connLookupEbBody
    , leiosDbInsertEbBody = connInsertEbBody
    , leiosDbInsertTxs = connInsertTxs
    , leiosDbBatchRetrieveTxs = connBatchRetrieveTxs
    , leiosDbLookupEbClosure = connLookupEbClosure
    , leiosDbScanEbPoints = connScanEbPoints
    , leiosDbScanCompleteEbClosuresNotOlderThanSlot = connScanComplete
    } = conn
  go ctx callName arg action =
    callTraceSameThreadVia
      (const ())
      (\ct -> emit (SomeJsonCallTrace ct))
      ctx
      callName
      arg
      action
