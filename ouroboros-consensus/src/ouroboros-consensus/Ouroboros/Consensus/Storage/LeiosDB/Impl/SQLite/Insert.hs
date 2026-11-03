{-# LANGUAGE NamedFieldPuns #-}

-- | The insert writes behind
-- 'Ouroboros.Consensus.Storage.LeiosDB.API.LeiosDbWriter'. They run on the
-- single writer, which calls them from its
-- 'Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.WriteQueue.WriteJob's.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Insert
  ( sqlInsertEbBody
  , sqlInsertTxs
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (foldM, forM, forM_, when)
import Control.Tracer (Tracer)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Database.SQLite3.Direct as DB
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlock
  , LeiosPoint (..)
  , TxLocation (..)
  , TxOffset
  , endorserBlockReferences
  )
import Ouroboros.Consensus.Storage.LeiosDB.API (LocalFill)
import Ouroboros.Consensus.Storage.LeiosDB.Exception (throwLeiosDbException)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Connection
  ( Conn (..)
  , dbWithWriteTransaction
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Maintenance (bumpVolatileStats)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Primitives
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Statements
import Ouroboros.Consensus.Storage.LeiosDB.Trace (TraceLeiosDb (..))

-- | Persist an EB body at a point, recording the point, then fill what it can
-- from the bytes other points hold, in the same transaction.
sqlInsertEbBody ::
  Tracer IO TraceLeiosDb ->
  Conn ->
  LeiosPoint ->
  EndorserBlock ->
  [LocalFill] ->
  IO [TxOffset]
sqlInsertEbBody tracer conn point eb fills = do
  when (null items) $
    throwLeiosDbException "writeEbBody: empty EB body (programmer error)"
  (inserted, filledOffs) <- dbWithWriteTransaction conn $ do
    inserted <- useStmt stInsertEbPoint $ do
      dbBindPoint stInsertEbPoint point
      dbStep1 stInsertEbPoint
      DB.changes db
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
    -- If the body was already written at this point, by an earlier delivery,
    -- the inserts above changed nothing, and the count accounts for every tx
    -- written since.
    useStmt stInitMissingCount $ do
      dbBindPoint stInitMissingCount point
      dbStep1 stInitMissingCount
    pure (inserted, filledOffs)
  bumpVolatileStats conn inserted
  pure filledOffs
 where
  items = zip [0 :: Int ..] (endorserBlockReferences eb)
  Conn{conVolDb = db, connVolStmts} = conn
  VolStmts
    { stInsertEbPoint
    , stInsertEbTxsRow
    , stPreallocEbTxBytes
    , stFillEbTxBytesLocally
    , stInitMissingCount
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
  IO ()
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
    when (nFilled > 0) $
      useStmt stDecrMissingCount $ do
        dbBindPoint stDecrMissingCount point
        dbBindInt64 stDecrMissingCount 3 nFilled
        dbStep1 stDecrMissingCount
 where
  Conn{conVolDb = db, connVolStmts} = conn
  VolStmts
    { stFillEbTxBytes
    , stDecrMissingCount
    } = connVolStmts
