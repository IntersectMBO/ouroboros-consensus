{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The insert writes behind
-- 'Ouroboros.Consensus.Storage.LeiosDB.API.LeiosDbWriter'. They run on the
-- single writer, which calls them from its
-- 'Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.WriteQueue.WriteJob's.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Insert
  ( sqlInsertEbBody
  , sqlInsertTxs
  ) where

import Control.Monad (foldM, forM_, when)
import Control.Tracer (Tracer)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Database.SQLite3.Direct as DB
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlock
  , LeiosPoint (..)
  , TxOffset
  , endorserBlockBytes
  , endorserBlockReferences
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

-- | Persist an EB body at a point, recording the point, in one transaction.
-- Returns the offsets at which the point then holds its txs.
--
-- Those offsets are read back from 'ebTxBytes': a range of the primary key,
-- one row per tx the body references, so up to
-- 'Ouroboros.Consensus.Leios.Types.maxTxsPerEndorserBlock' rows. If that read
-- is ever too slow, the point's 'ebs' row could keep a bitfield of which of its
-- txs are filled, about 2 kB at that many bits, updated in the same transaction
-- as the bytes: then this would read that one small blob instead.
sqlInsertEbBody ::
  Tracer IO TraceLeiosDb ->
  Conn ->
  LeiosPoint ->
  EndorserBlock ->
  IO [TxOffset]
sqlInsertEbBody tracer conn point eb = do
  when (null items) $
    throwLeiosDbException "writeEbBody: empty EB body (programmer error)"
  (inserted, held) <- dbWithWriteTransaction conn $ do
    inserted <- useStmt stInsertEbPoint $ do
      dbBindPoint stInsertEbPoint point
      dbBindBlob stInsertEbPoint 3 (endorserBlockBytes eb)
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
    -- If the body was already written at this point, by an earlier delivery,
    -- the inserts above changed nothing, and the count accounts for every tx
    -- written since.
    useStmt stInitMissingCount $ do
      dbBindPoint stInitMissingCount point
      dbStep1 stInitMissingCount
    -- Those written since an earlier delivery of the same body.
    held <- useStmt stHeldOffsets $ do
      dbBindPoint stHeldOffsets point
      let loop acc =
            dbStep stHeldOffsets >>= \case
              DB.Done -> pure (reverse acc)
              DB.Row -> do
                off <- fromIntegral <$> DB.columnInt64 stHeldOffsets 0
                loop (off : acc)
      loop []
    pure (inserted, held)
  bumpVolatileStats conn inserted
  pure held
 where
  items = zip [0 :: Int ..] (endorserBlockReferences eb)
  Conn{conVolDb = db, connVolStmts} = conn
  VolStmts
    { stInsertEbPoint
    , stInsertEbTxsRow
    , stPreallocEbTxBytes
    , stInitMissingCount
    , stHeldOffsets
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
--
-- Returns the offsets of those dropped because the EB referenced them with the
-- wrong size.
sqlInsertTxs ::
  Tracer IO TraceLeiosDb ->
  Conn ->
  -- | The EB the txs belong to
  LeiosPoint ->
  -- | Tx bytes by offset into the EB's body
  [(TxOffset, ByteString)] ->
  IO [TxOffset]
sqlInsertTxs _tracer conn point txBytesWithOffsets = do
  dbWithWriteTransaction conn $ do
    -- Fill the pre-allocated rows of the ebTxBytes table, counting how many
    -- txs filled, and collecting those dropped because the EB referenced them
    -- with the wrong size.
    (nFilled, wrongSize) <-
      foldM
        ( \(n, wrong) (txOffset, txBytes) -> do
            useStmt stFillEbTxBytes $ do
              dbBindPoint stFillEbTxBytes point
              dbBindInt64 stFillEbTxBytes 3 (fromIntegral txOffset)
              dbBindBlob stFillEbTxBytes 4 txBytes
              dbStep1 stFillEbTxBytes
            changed <- DB.changes db
            if changed > 0
              then pure (n + fromIntegral changed, wrong)
              else do
                declared <- useStmt stDeclaredTxSize $ do
                  dbBindPoint stDeclaredTxSize point
                  dbBindInt64 stDeclaredTxSize 3 (fromIntegral txOffset)
                  dbStep stDeclaredTxSize >>= \case
                    DB.Done -> pure Nothing
                    DB.Row -> Just <$> DB.columnInt64 stDeclaredTxSize 0
                pure $ case declared of
                  Just size | size /= fromIntegral (BS.length txBytes) -> (n, txOffset : wrong)
                  _ -> (n, wrong)
        )
        (0 :: Int64, [])
        txBytesWithOffsets
    when (nFilled > 0) $
      useStmt stDecrMissingCount $ do
        dbBindPoint stDecrMissingCount point
        dbBindInt64 stDecrMissingCount 3 nFilled
        dbStep1 stDecrMissingCount
    pure (reverse wrongSize)
 where
  Conn{conVolDb = db, connVolStmts} = conn
  VolStmts
    { stFillEbTxBytes
    , stDecrMissingCount
    , stDeclaredTxSize
    } = connVolStmts
