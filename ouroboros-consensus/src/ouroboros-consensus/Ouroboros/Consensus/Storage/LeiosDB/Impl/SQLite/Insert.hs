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
import Control.Monad (forM_, unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List.NonEmpty as NE
import qualified Database.SQLite3.Direct as DB
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , EndorserBlockHash
  , LeiosPoint (..)
  , TxOffset
  , endorserBlockReferences
  , serialiseEndorserBlock
  )
import Ouroboros.Consensus.Storage.LeiosDB.Exception (throwLeiosDbException)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Connection
  ( Conn (..)
  , dbWithWriteTransaction
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Maintenance (bumpVolatileStats)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Primitives
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Read
  ( EbIndex (..)
  , boundAt
  , consecutiveRuns
  , encodeTxBounds
  , lookupEbIndex
  , readTxBounds
  , withBlob
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Statements

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

-- | Persist an EB body: the bytes it arrived in, and where each of its txs
-- will be in the closure. The point MUST already be present (inserted via
-- 'sqlInsertEbPoint' on the announcement path).
sqlInsertEbBody ::
  Conn ->
  LeiosPoint ->
  EndorserBlock ->
  IO ()
sqlInsertEbBody conn point eb = do
  when (null (endorserBlockReferences eb)) $
    throwLeiosDbException "writeEbBody: empty EB body (programmer error)"
  dbWithWriteTransaction conn $ do
    known <- useStmt stHasPoint $ do
      dbBindInt64 stHasPoint 1 (fromIntegral $ unSlotNo (pointSlotNo point))
      dbBindHash stHasPoint 2 (pointEbHash point)
      (== DB.Row) <$> dbStep stHasPoint
    unless known $
      throwLeiosDbException "writeEbBody: point not registered (programmer error)"
    useStmt stInsertEbBody $ do
      dbBindHash stInsertEbBody 1 (pointEbHash point)
      dbBindBlob stInsertEbBody 2 (encodeTxBounds eb)
      dbBindBlob stInsertEbBody 3 (serialiseEndorserBlock eb)
      dbStep1 stInsertEbBody
 where
  Conn{connVolStmts = VolStmts{stHasPoint, stInsertEbBody}} = conn

-- | Persist tx bytes for one EB, in place in its closure, which the first
-- write allocates with zeroes. How many txs a write carries is the caller's to
-- decide; whether the EB is complete, too.
--
-- PREREQ: each tx's bytes hash to the tx hash the body lists at its offset.
-- So a tx written twice is the same bytes, and overwriting it is harmless.
sqlInsertTxs ::
  Conn ->
  -- | The EB the txs belong to
  LeiosPoint ->
  -- | Tx bytes by offset into the EB's body
  [(TxOffset, ByteString)] ->
  IO ()
sqlInsertTxs conn point txBytesWithOffsets =
  dbWithWriteTransaction conn $
    () <$ writeTxBytes conn (pointEbHash point) txBytesWithOffsets

-- | Write txs into the EB's closure, within the caller's transaction, and
-- return the offsets written. A transaction is dropped if:
--
-- * the EB's body is not stored;
-- * its offset is not in the body, or earlier in the same write;
-- * or its size is not the declared one.
writeTxBytes :: Conn -> EndorserBlockHash -> [(TxOffset, ByteString)] -> IO [TxOffset]
writeTxBytes conn ebHash txBytesWithOffsets =
  lookupEbIndex rsLookupEbIndex ebHash >>= \case
    Nothing -> pure []
    Just ix@EbIndex{eiTxCount = n, eiClosureRowid} -> do
      bounds <- readTxBounds db ix 0 (n - 1)
      let start i = boundAt bounds i
          end i = boundAt bounds (i + 1)
          accepted =
            IntMap.toAscList $
              IntMap.fromListWith
                (\_new old -> old)
                [ (txOffset, txBytes)
                | (txOffset, txBytes) <- txBytesWithOffsets
                , txOffset >= 0
                , txOffset < n
                , BS.length txBytes == end txOffset - start txOffset
                ]
      unless (null accepted) $ do
        rowid <- case eiClosureRowid of
          Just rowid -> pure rowid
          Nothing -> do
            useStmt stInsertEbClosure $ do
              dbBindHash stInsertEbClosure 1 ebHash
              dbBindInt64 stInsertEbClosure 2 (fromIntegral (end (n - 1)))
              dbStep1 stInsertEbClosure
            DB.lastInsertRowId db
        withBlob db "ebClosures" "txs" rowid True $ \blob ->
          forM_ (consecutiveRuns fst accepted) $ \run ->
            withDie db (DB.blobWrite blob (BS.concat (map snd (NE.toList run))) (start (fst (NE.head run))))
      pure (map fst accepted)
 where
  Conn
    { conVolDb = db
    , connVolStmts = VolStmts{stInsertEbClosure, stReads = ReadStmts{rsLookupEbIndex}}
    } = conn
