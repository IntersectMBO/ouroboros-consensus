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
import Control.Monad (foldM, forM_, when)
import Control.Tracer (Tracer)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Database.SQLite3.Direct as DB
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EbHash (..)
  , LeiosEb
  , LeiosPoint (..)
  , TxHash (..)
  , TxOffset
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
    -- Allocate this body's tx-bytes rows, then count the unfilled ones. Both
    -- in this transaction, so a fill can never see the rows without the count
    -- or the other way round.
    useStmt stPreallocEbTxBytes $ do
      dbBindBlob stPreallocEbTxBytes 1 (ebHashBytes (pointEbHash point))
      dbStep1 stPreallocEbTxBytes
    -- Initialize missingTxCount and read the resulting value via
    -- @RETURNING missingTxCount@. Only /this/ point's row can have
    -- transitioned to 0 as a consequence of the insert above: a body
    -- redelivered at a second point finds the first point's fills.
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
    , stPreallocEbTxBytes
    , stInitMissingCount
    , stMarkPointNotified
    } = connVolStmts

-- | Persist tx bytes for one EB by filling the rows 'sqlInsertEbBody'
-- pre-allocated.
--
-- A transaction is dropped if:
--
-- * its offset is not in the body;
-- * its offset is already filled (@filled = 1@);
-- * or its size is not the declared one.
sqlInsertTxs ::
  Tracer IO TraceLeiosDb ->
  Conn ->
  (LeiosEbNotification -> IO ()) ->
  -- | The EB the txs belong to
  LeiosPoint ->
  -- | Tx bytes by offset into the EB's body
  [(TxOffset, ByteString)] ->
  IO CompletedEbs
sqlInsertTxs _tracer conn notify point txBytesWithOffsets = do
  completed <- dbWithWriteTransaction conn $ do
    -- Fill the pre-allocated rows of the ebTxBytes table.
    -- Count how many tx bytes were successfully filled (i.e. not dropped).
    nFilled <-
      foldM
        ( \acc (txOffset, txBytes) -> do
            useStmt stFillEbTxBytes $ do
              dbBindBlob stFillEbTxBytes 1 ebHash
              dbBindInt64 stFillEbTxBytes 2 (fromIntegral txOffset)
              dbBindBlob stFillEbTxBytes 3 txBytes
              dbStep1 stFillEbTxBytes
            changed <- DB.changes db
            pure (acc + fromIntegral changed)
        )
        (0 :: Int64)
        txBytesWithOffsets
    if nFilled == 0
      then pure []
      else do
        -- Decrement every announcement of this content hash and collect the
        -- ones this batch completed.
        completedSlots <- useStmt stDecrMissingCount $ do
          dbBindBlob stDecrMissingCount 1 ebHash
          dbBindInt64 stDecrMissingCount 2 nFilled
          let loop acc =
                dbStep stDecrMissingCount >>= \case
                  DB.Done -> pure (reverse acc)
                  DB.Row -> do
                    slot <- SlotNo . fromIntegral <$> DB.columnInt64 stDecrMissingCount 0
                    left <- DB.columnInt64 stDecrMissingCount 1
                    loop (if left == 0 then slot : acc else acc)
          loop []
        -- Mark them notified so they are not completed twice.
        forM_ completedSlots $ \slot -> useStmt stMarkPointNotified $ do
          dbBindInt64 stMarkPointNotified 1 (fromIntegral $ unSlotNo slot)
          dbBindBlob stMarkPointNotified 2 ebHash
          dbStep1 stMarkPointNotified
        pure [MkLeiosPoint slot (pointEbHash point) | slot <- completedSlots]
  -- Emit a closure-completion notification for each completed EB
  forM_ completed $ \p -> notify (AcquiredEbTxs p)
  pure completed
 where
  ebHash = ebHashBytes (pointEbHash point)
  Conn{conVolDb = db, connVolStmts} = conn
  VolStmts
    { stFillEbTxBytes
    , stDecrMissingCount
    , stMarkPointNotified
    } = connVolStmts
