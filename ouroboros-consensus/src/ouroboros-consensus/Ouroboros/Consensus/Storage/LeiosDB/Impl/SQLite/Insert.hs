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
import Control.Monad (foldM, forM, forM_, when)
import Control.Tracer (Tracer)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Database.SQLite3.Direct as DB
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , LeiosPoint (..)
  , TxLocation (..)
  , TxOffset
  , endorserBlockReferences
  )
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( CompletedEbs
  , LocalFill
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
    dbBindPoint stmt point
    dbBindInt64 stmt 3 (fromIntegral ebBytesSize)
    dbStep1 stmt
    DB.changes db
  bumpVolatileStats conn inserted
 where
  Conn{conVolDb = db, connVolStmts = VolStmts{stInsertEbPoint = stmt}} = conn

-- | Persist an EB body at a point, then fill what it can from the bytes other
-- points hold, in the same transaction. The point MUST already be present
-- (inserted via 'sqlInsertEbPoint').
sqlInsertEbBody ::
  Tracer IO TraceLeiosDb ->
  Conn ->
  LeiosPoint ->
  EndorserBlock ->
  [LocalFill] ->
  IO (CompletedEbs, [TxOffset])
sqlInsertEbBody tracer conn point eb fills = do
  when (null items) $
    throwLeiosDbException "writeEbBody: empty EB body (programmer error)"
  dbWithWriteTransaction conn $ do
    forM_ items $ \(txOffset, (txHash, txBytesSize)) -> useStmt stInsertEbTxsRow $ do
      dbBindPoint stInsertEbTxsRow point
      dbBindInt64 stInsertEbTxsRow 3 (fromIntegral txOffset)
      dbBindHash stInsertEbTxsRow 4 txHash
      dbBindInt64 stInsertEbTxsRow 5 (fromIntegral txBytesSize)
      dbStepInsertOrTrace
        tracer
        "ebTxs"
        (show point <> "@" <> show txOffset)
        stInsertEbTxsRow
    -- Allocate this body's tx-bytes rows, then count the unfilled ones. Both
    -- in this transaction, so a fill can never see the rows without the count
    -- or the other way round.
    useStmt stPreallocEbTxBytes $ do
      dbBindPoint stPreallocEbTxBytes point
      dbStep1 stPreallocEbTxBytes
    -- Before the count, so that it counts only what is still unfilled.
    filledOffs <- fmap concat $ forM fills $ \(txOffset, MkTxLocation src srcOffset) -> do
      useStmt stFillEbTxBytesLocally $ do
        dbBindPoint stFillEbTxBytesLocally point
        dbBindInt64 stFillEbTxBytesLocally 3 (fromIntegral txOffset)
        dbBindInt64 stFillEbTxBytesLocally 4 (fromIntegral $ unSlotNo (pointSlotNo src))
        dbBindHash stFillEbTxBytesLocally 5 (pointEbHash src)
        dbBindInt64 stFillEbTxBytesLocally 6 (fromIntegral srcOffset)
        dbStep1 stFillEbTxBytesLocally
      changed <- DB.changes db
      pure [txOffset | changed > 0]
    -- Initialize missingTxCount and read the resulting value via
    -- @RETURNING missingTxCount@. If the body was already written at this
    -- point, by an earlier delivery, the inserts above changed nothing, and the
    -- count accounts for every tx written since.
    missingCount <- useStmt stInitMissingCount $ do
      dbBindPoint stInitMissingCount point
      readReturningInt64 stInitMissingCount
    completedNow <-
      if missingCount == 0
        then do
          useStmt stMarkPointNotified $ do
            dbBindPoint stMarkPointNotified point
            dbStep1 stMarkPointNotified
          pure [point]
        else pure []
    pure (completedNow, filledOffs)
 where
  items = zip [0 :: Int ..] (endorserBlockReferences eb)
  Conn{conVolDb = db, connVolStmts} = conn
  VolStmts
    { stInsertEbTxsRow
    , stPreallocEbTxBytes
    , stFillEbTxBytesLocally
    , stInitMissingCount
    , stMarkPointNotified
    } = connVolStmts

-- | Persist tx bytes for one point by filling the rows 'sqlInsertEbBody'
-- pre-allocated.
--
-- PREREQ: each tx's bytes hash to the tx hash the body lists at its offset.
--
-- A transaction is dropped if:
--
-- * its offset is not in the body;
-- * its offset is already filled (@filled = 1@);
-- * or its size is not the declared one.
sqlInsertTxs ::
  Tracer IO TraceLeiosDb ->
  Conn ->
  -- | The EB the txs belong to
  LeiosPoint ->
  -- | Tx bytes by offset into the EB's body
  [(TxOffset, ByteString)] ->
  IO CompletedEbs
sqlInsertTxs _tracer conn point txBytesWithOffsets = do
  dbWithWriteTransaction conn $ do
    -- Fill the pre-allocated rows of the ebTxBytes table.
    -- Count how many txs were successfully filled (i.e. not dropped).
    nFilled <-
      foldM
        ( \acc (txOffset, txBytes) -> do
            useStmt stFillEbTxBytes $ do
              dbBindPoint stFillEbTxBytes point
              dbBindInt64 stFillEbTxBytes 3 (fromIntegral txOffset)
              dbBindBlob stFillEbTxBytes 4 txBytes
              dbStep1 stFillEbTxBytes
            changed <- DB.changes db
            pure (acc + fromIntegral changed)
        )
        (0 :: Int64)
        txBytesWithOffsets
    if nFilled == 0
      then pure []
      else do
        -- Decrement the point's missingTxCount, and see whether this batch
        -- completed it.
        completed <- useStmt stDecrMissingCount $ do
          dbBindPoint stDecrMissingCount point
          dbBindInt64 stDecrMissingCount 3 nFilled
          dbStep stDecrMissingCount >>= \case
            DB.Done -> pure False
            DB.Row -> (== 0) <$> DB.columnInt64 stDecrMissingCount 0
        -- Mark it notified so it is not completed twice.
        if completed
          then do
            useStmt stMarkPointNotified $ do
              dbBindPoint stMarkPointNotified point
              dbStep1 stMarkPointNotified
            pure [point]
          else pure []
 where
  Conn{conVolDb = db, connVolStmts} = conn
  VolStmts
    { stFillEbTxBytes
    , stDecrMissingCount
    , stMarkPointNotified
    } = connVolStmts
