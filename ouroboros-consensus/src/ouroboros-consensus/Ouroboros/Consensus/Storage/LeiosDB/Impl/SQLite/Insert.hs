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

import Cardano.Crypto.Hash (hashToBytes)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (forM, forM_, when)
import Control.Tracer (Tracer)
import Data.ByteString (ByteString)
import qualified Database.SQLite3.Direct as DB
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , LeiosPoint (..)
  , SerialisedEndorserBlock (..)
  , TxLocation (..)
  , TxOffset
  , endorserBlockReferences
  , serialiseEndorserBlock
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

sqlInsertEbPoint :: Conn -> LeiosPoint -> BytesSize -> IO ()
sqlInsertEbPoint conn point ebBytesSize = do
  inserted <- dbWithWriteTransaction conn $ useStmt stmt $ do
    dbBindInt64 stmt 1 (fromIntegral $ unSlotNo (pointSlotNo point))
    dbBindHash stmt 2 (pointEbHash point)
    dbBindInt64 stmt 3 (fromIntegral ebBytesSize)
    dbStep1 stmt
    DB.changes db
  bumpVolatileStats conn inserted
 where
  Conn{conVolDb = db, connVolStmts = VolStmts{stInsertEbPoint = stmt}} = conn

-- | Persist an EB body, then fill what it can from the bytes other EBs hold,
-- in the same transaction. The point MUST already be present (inserted via
-- 'sqlInsertEbPoint' on the announcement path).
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
  dbWithWriteTransaction conn $ do
    useStmt stInsertEbBodyBytes $ do
      dbBindHash stInsertEbBodyBytes 1 (pointEbHash point)
      dbBindBlob stInsertEbBodyBytes 2 (serialisedEndorserBlockBytes (serialiseEndorserBlock eb))
      dbStep1 stInsertEbBodyBytes
    forM_ items $ \(txOffset, (txHash, txBytesSize)) -> useStmt stInsertEbTxsRow $ do
      dbBindHash stInsertEbTxsRow 1 (pointEbHash point)
      dbBindInt64 stInsertEbTxsRow 2 (fromIntegral txOffset)
      dbBindHash stInsertEbTxsRow 3 txHash
      dbBindInt64 stInsertEbTxsRow 4 (fromIntegral txBytesSize)
      dbStepInsertOrTrace
        tracer
        "ebTxs"
        (show (pointEbHash point) <> "@" <> show txOffset)
        stInsertEbTxsRow
    -- Allocate this body's tx-bytes rows, in this transaction, so a fill can
    -- never see the body without them.
    useStmt stPreallocEbTxBytes $ do
      dbBindHash stPreallocEbTxBytes 1 (pointEbHash point)
      dbStep1 stPreallocEbTxBytes
    fmap concat $ forM fills $ \(txOffset, MkTxLocation srcHash srcOffset) -> do
      useStmt stFillEbTxBytesLocally $ do
        dbBindHash stFillEbTxBytesLocally 1 (pointEbHash point)
        dbBindInt64 stFillEbTxBytesLocally 2 (fromIntegral txOffset)
        dbBindHash stFillEbTxBytesLocally 3 srcHash
        dbBindInt64 stFillEbTxBytesLocally 4 (fromIntegral srcOffset)
        dbStep1 stFillEbTxBytesLocally
      changed <- DB.changes db
      pure [txOffset | changed > 0]
 where
  items = zip [0 :: Int ..] (endorserBlockReferences eb)
  Conn{conVolDb = db, connVolStmts} = conn
  VolStmts
    { stInsertEbBodyBytes
    , stInsertEbTxsRow
    , stPreallocEbTxBytes
    , stFillEbTxBytesLocally
    } = connVolStmts

-- | Persist tx bytes for one EB by filling the rows 'sqlInsertEbBody'
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
sqlInsertTxs _tracer conn point txBytesWithOffsets =
  dbWithWriteTransaction conn $
    -- Fill the pre-allocated rows of the ebTxBytes table.
    forM_ txBytesWithOffsets $ \(txOffset, txBytes) ->
      useStmt stFillEbTxBytes $ do
        dbBindBlob stFillEbTxBytes 1 ebHash
        dbBindInt64 stFillEbTxBytes 2 (fromIntegral txOffset)
        dbBindBlob stFillEbTxBytes 3 txBytes
        dbStep1 stFillEbTxBytes
 where
  ebHash = hashToBytes (pointEbHash point)
  Conn{connVolStmts = VolStmts{stFillEbTxBytes}} = conn
