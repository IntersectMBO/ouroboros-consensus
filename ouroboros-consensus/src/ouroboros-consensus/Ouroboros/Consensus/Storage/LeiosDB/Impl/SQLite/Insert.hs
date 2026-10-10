{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The insert writes behind
-- 'Ouroboros.Consensus.Storage.LeiosDB.API.LeiosDbWriter'. They run on the
-- single writer, which calls them from its
-- 'Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.WriteQueue.WriteJob's.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Insert
  ( sqlInsertEbPoint
  , sqlInsertEbBody
  , sqlInsertTxs
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (forM_, when)
import Control.Tracer (Tracer)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Database.SQLite3.Direct as DB
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EbHash (..)
  , LeiosEb
  , LeiosPoint (..)
  , TxHash (..)
  , encodeLeiosEbSize
  , leiosEbBodyItems
  )
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( CompletedEbs
  , LeiosEbNotification (..)
  )
import Ouroboros.Consensus.Storage.LeiosDB.Exception (throwLeiosDbException)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Connection
  ( Conn (..)
  , dbWithWriteTransaction
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Maintenance (bumpVolatileStats)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Primitives
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Statements
import Ouroboros.Consensus.Storage.LeiosDB.Trace (TraceLeiosDb (..))

sqlInsertEbPoint :: Conn -> LeiosPoint -> BytesSize -> IO ()
sqlInsertEbPoint conn point ebBytesSize = do
  inserted <- dbWithWriteTransaction conn $ useStmt stmt $ do
    dbBindInt64 stmt 1 (fromIntegral $ unSlotNo (pointSlotNo point))
    dbBindBlob stmt 2 (ebHashBytes (pointEbHash point))
    dbBindInt64 stmt 3 (fromIntegral ebBytesSize)
    dbStep1 stmt
    DB.changes db
  bumpVolatileStats conn inserted
 where
  Conn{conVolDb = db, connVolStmts = VolStmts{stInsertEbPoint = stmt}} = conn

-- | Persist an EB body. The point MUST already be present (inserted
-- via 'sqlInsertEbPoint' on the announcement path).
sqlInsertEbBody ::
  Tracer IO TraceLeiosDb ->
  Conn ->
  (LeiosEbNotification -> IO ()) ->
  LeiosPoint ->
  LeiosEb ->
  IO CompletedEbs
sqlInsertEbBody tracer conn notify point eb = do
  when (null items) $
    throwLeiosDbException "writeEbBody: empty EB body (programmer error)"
  completedNow <- dbWithWriteTransaction conn $ do
    forM_ items $ \(txOffset, txHash, txBytesSize) -> useStmt stInsertEbTxsRow $ do
      dbBindBlob stInsertEbTxsRow 1 (ebHashBytes (pointEbHash point))
      dbBindInt64 stInsertEbTxsRow 2 (fromIntegral txOffset)
      dbBindBlob stInsertEbTxsRow 3 (let MkTxHash bytes = txHash in bytes)
      dbBindInt64 stInsertEbTxsRow 4 (fromIntegral txBytesSize)
      dbStepInsertOrTrace
        tracer
        "ebTxs"
        (show (pointEbHash point) <> "@" <> show txOffset)
        stInsertEbTxsRow
    -- Record which of this body's txs we still lack, then count them. Both in
    -- this transaction, so an arrival can never see the rows without the count
    -- or the other way round.
    useStmt stInsertMissingTxs $ do
      dbBindBlob stInsertMissingTxs 1 (ebHashBytes (pointEbHash point))
      dbStep1 stInsertMissingTxs
    -- Initialize missingTxCount and read the resulting value via
    -- @RETURNING missingTxCount@. Only /this/ point's row can have
    -- transitioned to 0 as a consequence of the insert above.
    missingCount <- useStmt stInitMissingCount $ do
      dbBindBlob stInitMissingCount 1 (ebHashBytes (pointEbHash point))
      dbBindBlob stInitMissingCount 2 (ebHashBytes (pointEbHash point))
      dbBindInt64 stInitMissingCount 3 (fromIntegral $ unSlotNo (pointSlotNo point))
      readReturningInt64 stInitMissingCount
    if missingCount == 0
      then do
        useStmt stMarkPointNotified $ do
          dbBindInt64 stMarkPointNotified 1 (fromIntegral $ unSlotNo (pointSlotNo point))
          dbBindBlob stMarkPointNotified 2 (ebHashBytes (pointEbHash point))
          dbStep1 stMarkPointNotified
        pure [point]
      else pure []
  notify $ AcquiredEb point ebBytesSize
  forM_ completedNow $ \p -> notify (AcquiredEbTxs p)
  pure completedNow
 where
  items = leiosEbBodyItems eb
  ebBytesSize = encodeLeiosEbSize eb
  Conn{connVolStmts} = conn
  VolStmts
    { stInsertEbTxsRow
    , stInsertMissingTxs
    , stInitMissingCount
    , stMarkPointNotified
    } = connVolStmts

sqlInsertTxs ::
  Tracer IO TraceLeiosDb ->
  Conn ->
  (LeiosEbNotification -> IO ()) ->
  [(TxHash, ByteString)] ->
  IO CompletedEbs
sqlInsertTxs _tracer conn notify txs = do
  -- Skip txs already persisted in 'txs'. Under mempool backlog,
  -- successive forges (or overlapping peer EBs) re-present the same tx
  -- hashes; attempting the INSERT and catching a constraint violation
  -- still pays the bind + PK-lookup + reset cost per row.
  missing <- Set.fromList <$> sqlFilterMissingTxs conn (map fst txs)
  completed <- dbWithWriteTransaction conn $ do
    -- 'dbStepInsert' still handles the rare race where a concurrent
    -- writer inserted the same hash between the filter above and the
    -- INSERT below.
    forM_ (novel missing) $ \(txHash, txBytes) -> do
      let txBytesSize = fromIntegral $ BS.length txBytes
          txHashBytes = let MkTxHash bytes = txHash in bytes
      inserted <- useStmt stInsertTx $ do
        dbBindBlob stInsertTx 1 txHashBytes
        dbBindBlob stInsertTx 2 txBytes
        dbBindInt64 stInsertTx 3 txBytesSize
        dbStepInsert stInsertTx
      when inserted $ do
        useStmt stDecrMissingCount $ do
          dbBindBlob stDecrMissingCount 1 txHashBytes
          dbStep1 stDecrMissingCount
        -- Strictly after the decrement, which reads these rows.
        useStmt stDeleteMissingTxs $ do
          dbBindBlob stDeleteMissingTxs 1 txHashBytes
          dbStep1 stDeleteMissingTxs
    -- Find newly-complete EBs (missingTxCount reached 0)
    completed <- useStmt stFindCompleteEbs $ do
      let loop acc =
            dbStep stFindCompleteEbs >>= \case
              DB.Done -> pure (reverse acc)
              DB.Row -> do
                ebHash <- MkEbHash <$> DB.columnBlob stFindCompleteEbs 0
                slot <- SlotNo . fromIntegral <$> DB.columnInt64 stFindCompleteEbs 1
                loop (MkLeiosPoint slot ebHash : acc)
      loop []
    -- Mark them as notified so they are not found again
    useStmt stMarkNotifiedEbs $ dbStep1 stMarkNotifiedEbs
    pure completed
  -- Emit a closure-completion notification for each completed EB
  forM_ completed $ \point -> notify (AcquiredEbTxs point)
  pure completed
 where
  Conn{connVolStmts} = conn
  VolStmts
    { stInsertTx
    , stDecrMissingCount
    , stDeleteMissingTxs
    , stFindCompleteEbs
    , stMarkNotifiedEbs
    } = connVolStmts
  novel missing = filter (\(h, _) -> h `Set.member` missing) txs

-- | Batch-filter tx hashes against @txs@: passes txHashes as a JSON array
-- of hex strings; SQL decodes with @unhex()@ so index lookups on
-- @txs.txHashBytes@ still fire. Used internally by 'sqlInsertTxs' to skip
-- already-persisted txs.
sqlFilterMissingTxs :: Conn -> [TxHash] -> IO [TxHash]
sqlFilterMissingTxs conn txHashes =
  dbWithTransaction db $ useStmt stmt $ do
    dbBindUtf8 stmt 1 (jsonHexArray [b | MkTxHash b <- txHashes])
    loop []
 where
  Conn{conVolDb = db, connVolStmts = VolStmts{stFilterMissingTxs = stmt}} = conn
  loop acc =
    dbStep stmt >>= \case
      DB.Done -> pure (reverse acc)
      DB.Row -> do
        txHash <- MkTxHash <$> DB.columnBlob stmt 0
        loop (txHash : acc)
