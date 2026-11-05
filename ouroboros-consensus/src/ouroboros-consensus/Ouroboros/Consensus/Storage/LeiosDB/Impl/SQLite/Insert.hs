{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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

-- | Persist a point. If another point of the EB holds it, the new row copies
-- its body and closure, so each row holds the whole EB.
sqlInsertEbPoint :: Conn -> LeiosPoint -> BytesSize -> IO ()
sqlInsertEbPoint conn point ebBytesSize =
  dbWithWriteTransaction conn (insertPoint conn point ebBytesSize)
    >>= bumpVolatileStats conn

-- | 'sqlInsertEbPoint' within the caller's transaction, returning the rows it
-- inserted.
insertPoint :: Conn -> LeiosPoint -> BytesSize -> IO Int
insertPoint conn point ebBytesSize = do
  -- With the EB another point of it holds, if any.
  copied <-
    insertWith
      "INSERT OR IGNORE INTO ebs\n\
      \  (ebSlot, ebHashBytes, ebBytesSize, txCount, txBounds, body, txs)\n\
      \SELECT ?1, ?2, ?3, txCount, txBounds, body, txs FROM ebs\n\
      \WHERE ebHashBytes = ?2 AND txCount IS NOT NULL LIMIT 1"
  if copied > 0
    then pure copied
    else insertWith "INSERT OR IGNORE INTO ebs (ebSlot, ebHashBytes, ebBytesSize) VALUES (?1, ?2, ?3)"
 where
  insertWith sql = withStmt db sql $ \stmt -> do
    dbBindInt64 stmt 1 (fromIntegral $ unSlotNo (pointSlotNo point))
    dbBindHash stmt 2 (pointEbHash point)
    dbBindInt64 stmt 3 (fromIntegral ebBytesSize)
    dbStep1 stmt
    DB.changes db

  Conn{conVolDb = db} = conn

-- | Persist an EB body into each point of the EB: the bytes it arrived in,
-- where each of its txs will be in the closure, and the closure, zeroes.
-- Writes the point too, if 'sqlInsertEbPoint' has not.
sqlInsertEbBody ::
  Conn ->
  LeiosPoint ->
  EndorserBlock ->
  IO ()
sqlInsertEbBody conn point eb = do
  when (null refs) $
    throwLeiosDbException "writeEbBody: empty EB body (programmer error)"
  inserted <- dbWithWriteTransaction conn $ do
    inserted <- insertPoint conn point (fromIntegral (BS.length bodyBytes))
    insertBody
    pure inserted
  bumpVolatileStats conn inserted
 where
  -- Into each point of the EB. A second delivery of the same body changes
  -- nothing.
  insertBody =
    withStmt
      db
      "UPDATE ebs SET txCount = ?2, txBounds = ?3, body = ?4, txs = zeroblob(?5)\n\
      \WHERE ebHashBytes = ?1 AND txCount IS NULL"
      $ \stmt -> do
        dbBindHash stmt 1 (pointEbHash point)
        dbBindInt64 stmt 2 (fromIntegral (length refs))
        dbBindBlob stmt 3 (encodeTxBounds eb)
        dbBindBlob stmt 4 bodyBytes
        dbBindInt64 stmt 5 (fromIntegral (sum (map snd refs)))
        dbStep1 stmt

  bodyBytes = serialiseEndorserBlock eb
  refs = endorserBlockReferences eb
  Conn{conVolDb = db} = conn

-- | Persist tx bytes for one EB, in place in the closure of each of its
-- points. How many txs a write carries is the caller's to decide; whether the
-- EB is complete, too.
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

-- | Write txs into the closure of each point of the EB, within the caller's
-- transaction, and return the offsets written. A transaction is dropped if:
--
-- * the EB's body is not stored;
-- * its offset is not in the body, or earlier in the same write;
-- * or its size is not the declared one.
writeTxBytes :: Conn -> EndorserBlockHash -> [(TxOffset, ByteString)] -> IO [TxOffset]
writeTxBytes conn ebHash txBytesWithOffsets =
  lookupEbIndex db ebHash >>= \case
    Nothing -> pure []
    Just ix@EbIndex{eiTxCount = n} -> do
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
        rowids <- withStmt db "SELECT rowid FROM ebs WHERE ebHashBytes = ?1 AND txCount IS NOT NULL" $ \stmt -> do
          dbBindHash stmt 1 ebHash
          let loop acc =
                dbStep stmt >>= \case
                  DB.Done -> pure acc
                  DB.Row -> DB.columnInt64 stmt 0 >>= loop . (: acc)
          loop []
        forM_ rowids $ \rowid ->
          withBlob db "ebs" "txs" rowid True $ \blob ->
            forM_ (consecutiveRuns fst accepted) $ \run ->
              withDie db (DB.blobWrite blob (BS.concat (map snd (NE.toList run))) (start (fst (NE.head run))))
      pure (map fst accepted)
 where
  Conn{conVolDb = db} = conn
