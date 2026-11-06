{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The insert writes behind
-- 'Ouroboros.Consensus.Storage.LeiosDB.API.LeiosDbWriter'. They run on the
-- single writer, which calls them from its
-- 'Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.WriteQueue.WriteJob's.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Insert
  ( sqlInsertEbBody
  , sqlInsertTxs
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (forM, forM_, unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Database.SQLite3.Direct as DB
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlock
  , LeiosPoint (..)
  , LeiosTx (..)
  , TxLocation (..)
  , TxOffset
  , endorserBlockReferences
  , hashLeiosTx
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
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Read
  ( EbIndex (..)
  , boundAt
  , consecutiveRuns
  , encodeTxBounds
  , isWritten
  , lookupEbIndex
  , readTxBounds
  , readTxBytes
  , readTxWritten
  , withBlob
  )

-- | Persist an EB body at its point, recording the point: the bytes it arrived
-- in, where each of its txs will be in the closure, and the closure and the
-- markers of its written txs, zeroes. Each point holds its own copy, so a write
-- never touches another point's row. Then fill what it can from the bytes
-- other EBs hold, in the same transaction.
--
-- A fill copies the source's bytes only if they hash to the tx this body lists
-- at the fill's offset, so a stale or wrong location fills nothing.
--
-- Returns the offsets the point then holds: those filled, or written since an
-- earlier delivery of the body, which its markers tell without reading the
-- closure.
sqlInsertEbBody ::
  Conn ->
  LeiosPoint ->
  EndorserBlock ->
  [LocalFill] ->
  IO [TxOffset]
sqlInsertEbBody conn point eb fills = do
  when (null refs) $
    throwLeiosDbException "writeEbBody: empty EB body (programmer error)"
  (inserted, held) <- dbWithWriteTransaction conn $ do
    inserted <- insertWithBody
    -- Nothing inserted: the point holds the body of an earlier delivery, and
    -- the txs written since.
    atPoint <- if inserted > 0 then pure IntSet.empty else markedAtPoint
    filled <- readLocalFills
    let toWrite = IntMap.toAscList (filled `IntMap.withoutKeys` atPoint)
    dropped <- writeTxBytes conn point toWrite
    let written = IntSet.fromList [txOffset | (txOffset, _) <- toWrite, txOffset `notElem` dropped]
    pure (inserted, IntSet.toAscList (atPoint <> written))
  bumpVolatileStats conn inserted
  pure held
 where
  -- The offsets of the txs marked written at the point.
  markedAtPoint =
    lookupEbIndex db point >>= \case
      Nothing -> pure IntSet.empty
      Just ix@EbIndex{eiTxCount = n} -> do
        txWritten <- readTxWritten db ix 0 (n - 1)
        pure $ IntSet.fromList [o | o <- [0 .. n - 1], isWritten txWritten o]

  -- The bytes of the fills, by offset in this body: those the node holds that
  -- hash to the tx this body lists there. Each source is read once, in
  -- ascending offsets, as each blob handle walks the blob's pages from its
  -- start.
  readLocalFills =
    fmap IntMap.unions $ forM (Map.toList bySource) $ \(srcHash, wanted) -> do
      srcTxs <- readFromSource srcHash (IntMap.keysSet wanted)
      pure $
        IntMap.fromList
          [ (txOffset, tx)
          | (srcOffset, txOffsets) <- IntMap.toList wanted
          , Just tx <- [IntMap.lookup srcOffset srcTxs]
          , txOffset <- txOffsets
          , Just (txHash, _size) <- [IntMap.lookup txOffset refsByOffset]
          , hashLeiosTx (MkLeiosTx tx) == txHash
          ]

  -- The fills by source, then by offset in the source, but those of offsets the
  -- body does not list.
  bySource =
    Map.fromListWith
      (IntMap.unionWith (<>))
      [ (srcHash, IntMap.singleton srcOffset [txOffset])
      | (txOffset, MkTxLocation srcHash srcOffset) <- fills
      , srcOffset >= 0
      , IntMap.member txOffset refsByOffset
      ]

  -- The written txs at these offsets of the EB, from the first of its points
  -- that holds each.
  readFromSource srcHash wanted = do
    rows <- withStmt db "SELECT rowid, length(txWritten) FROM ebs WHERE ebHashBytes = ?1" $ \stmt -> do
      dbBindHash stmt 1 srcHash
      let loop acc =
            dbStep stmt >>= \case
              DB.Done -> pure (reverse acc)
              DB.Row -> do
                eiRowid <- DB.columnInt64 stmt 0
                eiTxCount <- fromIntegral <$> DB.columnInt64 stmt 1
                loop (EbIndex{eiRowid, eiTxCount} : acc)
      loop []
    let go found _ | IntSet.null found = pure IntMap.empty
        go _ [] = pure IntMap.empty
        go remaining (ix@EbIndex{eiTxCount} : rest) = do
          txs <- readTxBytes db ix (IntSet.toAscList (IntSet.filter (< eiTxCount) remaining))
          let here = IntMap.fromList [(o, tx) | (o, Just tx) <- txs]
          (here <>) <$> go (remaining `IntSet.difference` IntMap.keysSet here) rest
    go wanted rows

  refsByOffset = IntMap.fromList (zip [0 ..] refs)

  insertWithBody =
    withStmt
      db
      "INSERT OR IGNORE INTO ebs\n\
      \  (ebSlot, ebHashBytes, txWritten, txBounds, body, txs)\n\
      \VALUES (?1, ?2, zeroblob(?3), ?4, ?5, zeroblob(?6))"
      $ \stmt -> do
        dbBindInt64 stmt 1 (fromIntegral $ unSlotNo (pointSlotNo point))
        dbBindHash stmt 2 (pointEbHash point)
        dbBindInt64 stmt 3 (fromIntegral (length refs))
        dbBindBlob stmt 4 (encodeTxBounds eb)
        dbBindBlob stmt 5 bodyBytes
        dbBindInt64 stmt 6 (fromIntegral (sum (map snd refs)))
        dbStep1 stmt
        DB.changes db

  bodyBytes = serialiseEndorserBlock eb
  refs = endorserBlockReferences eb
  Conn{conVolDb = db} = conn

-- | Persist tx bytes for the EB at one point, in place in its closure. How many
-- txs a write carries is the caller's to decide; whether the
-- EB is complete, too.
--
-- PREREQ: each tx's bytes hash to the tx hash the body lists at its offset.
-- So a tx written twice is the same bytes, and overwriting it is harmless.
--
-- Returns the offsets of the txs dropped for their size.
sqlInsertTxs ::
  Conn ->
  -- | The EB the txs belong to
  LeiosPoint ->
  -- | Tx bytes by offset into the EB's body
  [(TxOffset, ByteString)] ->
  IO [TxOffset]
sqlInsertTxs conn point txBytesWithOffsets =
  dbWithWriteTransaction conn $
    writeTxBytes conn point txBytesWithOffsets

-- | Write txs into the closure of the EB at the point, and mark them written,
-- within the caller's transaction, and return the offsets of those dropped for
-- their size. A
-- transaction is dropped if:
--
-- * the EB's body is not stored;
-- * its offset is not in the body, or earlier in the same write;
-- * or its size is not the declared one.
writeTxBytes :: Conn -> LeiosPoint -> [(TxOffset, ByteString)] -> IO [TxOffset]
writeTxBytes conn point txBytesWithOffsets =
  lookupEbIndex db point >>= \case
    Nothing -> pure []
    Just ix@EbIndex{eiRowid, eiTxCount = n} -> do
      bounds <- readTxBounds db ix 0 (n - 1)
      let start i = boundAt bounds i
          end i = boundAt bounds (i + 1)
          inBody = [tx | tx@(txOffset, _) <- txBytesWithOffsets, txOffset >= 0, txOffset < n]
          sized (txOffset, txBytes) = BS.length txBytes == end txOffset - start txOffset
          accepted =
            IntMap.toAscList $
              IntMap.fromListWith (\_new old -> old) (filter sized inBody)
          runs = consecutiveRuns fst accepted
      unless (null accepted) $ do
        withBlob db "ebs" "txs" eiRowid True $ \blob ->
          forM_ runs $ \run ->
            withDie db (DB.blobWrite blob (BS.concat (map snd (NE.toList run))) (start (fst (NE.head run))))
        withBlob db "ebs" "txWritten" eiRowid True $ \blob ->
          forM_ runs $ \run ->
            withDie db (DB.blobWrite blob (BS.replicate (NE.length run) 1) (fst (NE.head run)))
      pure [txOffset | tx@(txOffset, _) <- inBody, not (sized tx)]
 where
  Conn{conVolDb = db} = conn
