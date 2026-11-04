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
import Control.Monad (forM, forM_, unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Database.SQLite3.Direct as DB
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlock
  , EndorserBlockEnvelope (..)
  , EndorserBlockHash
  , LeiosPoint (..)
  , LeiosTx (..)
  , TxLocation (..)
  , TxOffset
  , endorserBlockReferences
  , hashLeiosTx
  , mkEndorserBlockEnvelope
  )
import Ouroboros.Consensus.Storage.LeiosDB.API (LocalFill)
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
  , encodeTxBounds
  , lookupEbIndex
  , readTxBounds
  , readTxBytes
  , withBlob
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Statements
import Ouroboros.Network.Block (Serialised (..))

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
-- will be in the closure. Then fill what it can from the bytes other EBs hold,
-- in the same transaction, and return the offsets it filled. The point MUST
-- already be present (inserted via 'sqlInsertEbPoint' on the announcement
-- path).
--
-- A fill copies the source's bytes only if they hash to the tx this body lists
-- at the fill's offset, so a stale or wrong location fills nothing.
sqlInsertEbBody ::
  Conn ->
  LeiosPoint ->
  EndorserBlock ->
  [LocalFill] ->
  IO [TxOffset]
sqlInsertEbBody conn point eb fills = do
  when (null refs) $
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
      dbBindInt64 stInsertEbBody 3 (fromIntegral (envelopeVersion envelope))
      dbBindBlob stInsertEbBody 4 (BSL.toStrict (unSerialised (envelopeBytes envelope)))
      dbStep1 stInsertEbBody
    -- Each source is read once, in ascending offsets, as each blob handle walks
    -- the blob's pages from its start.
    copied <- fmap concat $ forM (Map.toList bySource) $ \(srcHash, wanted) ->
      lookupEbIndex rsLookupEbIndex srcHash >>= \case
        Nothing -> pure []
        Just srcIx -> do
          srcTxs <-
            IntMap.fromList
              <$> readTxBytes db srcIx (IntSet.toAscList (IntSet.filter (< eiTxCount srcIx) (IntMap.keysSet wanted)))
          pure
            [ (txOffset, tx)
            | (srcOffset, txOffsets) <- IntMap.toList wanted
            , Just tx <- [IntMap.lookup srcOffset srcTxs]
            , txOffset <- txOffsets
            , Just (txHash, _size) <- [IntMap.lookup txOffset refs]
            , hashLeiosTx (MkLeiosTx tx) == txHash
            ]
    writeTxBytes conn (pointEbHash point) copied
 where
  envelope = mkEndorserBlockEnvelope eb
  refs = IntMap.fromList (zip [0 ..] (endorserBlockReferences eb))
  -- The fills by source, then by offset in the source.
  bySource =
    Map.fromListWith
      (IntMap.unionWith (<>))
      [ (srcHash, IntMap.singleton srcOffset [txOffset])
      | (txOffset, MkTxLocation srcHash srcOffset) <- fills
      , srcOffset >= 0
      ]
  Conn
    { conVolDb = db
    , connVolStmts = VolStmts{stHasPoint, stInsertEbBody, stReads = ReadStmts{rsLookupEbIndex}}
    } = conn

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
          forM_ (runs accepted) $ \run ->
            withDie db (DB.blobWrite blob (BS.concat (map snd run)) (start (fst (head run))))
      pure (map fst accepted)
 where
  Conn
    { conVolDb = db
    , connVolStmts = VolStmts{stInsertEbClosure, stReads = ReadStmts{rsLookupEbIndex}}
    } = conn

  -- Maximal runs of consecutive offsets.
  runs [] = []
  runs (x : xs) = go [x] (fst x) xs
   where
    go acc prev (y : ys) | fst y == prev + 1 = go (y : acc) (fst y) ys
    go acc _ rest = reverse acc : runs rest
