{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The insert writes behind
-- 'Ouroboros.Consensus.Storage.LeiosDB.API.LeiosDbWriter'. They run on the
-- single writer, which calls them from its
-- 'Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.WriteQueue.WriteJob's.
--
-- Completion is tracked in memory ('Missing'), not in the schema. Only the
-- writer changes it: an EB is registered as missing txs before its body
-- commits, and leaves once the write completing it has committed, so a reader
-- never calls an EB complete early.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Insert
  ( sqlInsertEbPoint
  , sqlInsertEbBody
  , sqlInsertTxs
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import Codec.CBOR.Write (toStrictByteString)
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTVar
  , modifyTVar
  , readTVarIO
  )
import Control.Monad (foldM, forM, forM_, unless, when)
import Control.Tracer (traceWith)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Foldable as Foldable
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Strict as V
import qualified Database.SQLite3.Direct as DB
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , LeiosEb (..)
  , LeiosPoint (..)
  , TxLocation (..)
  , TxOffset
  , ebHashBytes
  , encodeLeiosEb
  , encodeLeiosEbSize
  , hashTxBytes
  , prettyLeiosPoint
  )
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( CompletedEbs
  , LeiosEbNotification (..)
  , LocalFill
  )
import Ouroboros.Consensus.Storage.LeiosDB.Exception (throwLeiosDbException)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Completion (Missing, txIsWritten)
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
  , withBlob
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Statements
import Ouroboros.Consensus.Storage.LeiosDB.Trace (TraceLeiosDb (..))
import Ouroboros.Consensus.Util.IOLike (atomically)

-- | Record an announced point. A novel point of an EB whose body is stored
-- repeats the body's notifications for it, see 'writeEbPoint'.
sqlInsertEbPoint ::
  Conn ->
  StrictTVar IO Missing ->
  (LeiosEbNotification -> IO ()) ->
  LeiosPoint ->
  BytesSize ->
  IO CompletedEbs
sqlInsertEbPoint conn missingVar notify point ebBytesSize = do
  (inserted, mbBodySize) <- dbWithWriteTransaction conn $ do
    inserted <- useStmt stInsertEbPoint $ do
      dbBindInt64 stInsertEbPoint 1 (fromIntegral $ unSlotNo (pointSlotNo point))
      dbBindBlob stInsertEbPoint 2 (ebHashBytes (pointEbHash point))
      dbBindInt64 stInsertEbPoint 3 (fromIntegral ebBytesSize)
      dbStep1 stInsertEbPoint
      DB.changes db
    mbBodySize <-
      if inserted == 0
        then pure Nothing
        else useStmt stLookupEbBodySize $ do
          dbBindBlob stLookupEbBodySize 1 (ebHashBytes (pointEbHash point))
          dbStep stLookupEbBodySize >>= \case
            DB.Done -> pure Nothing
            DB.Row -> Just . fromIntegral <$> DB.columnInt64 stLookupEbBodySize 0
    pure (inserted, mbBodySize)
  bumpVolatileStats conn inserted
  case mbBodySize of
    Nothing -> pure []
    Just bodySize -> do
      missing <- readTVarIO missingVar
      notify $ AcquiredEb point bodySize
      if Map.member (pointEbHash point) missing
        then pure []
        else do
          notify $ AcquiredEbTxs point
          pure [point]
 where
  Conn{conVolDb = db, connVolStmts = VolStmts{stInsertEbPoint, stLookupEbBodySize}} = conn

-- | Persist an EB body: its wire encoding, and where each of its txs will be
-- in the closure. Then copy the local fills into the closure, in the same
-- transaction. The point MUST already be present (inserted via
-- 'sqlInsertEbPoint' on the announcement path).
--
-- A body already stored changes nothing, and reports nothing.
sqlInsertEbBody ::
  Conn ->
  StrictTVar IO Missing ->
  (LeiosEbNotification -> IO ()) ->
  LeiosPoint ->
  LeiosEb ->
  [LocalFill] ->
  IO (CompletedEbs, [TxOffset])
sqlInsertEbBody conn missingVar notify point eb fills = do
  when (n == 0) $
    throwLeiosDbException "writeEbBody: empty EB body (programmer error)"
  mbResult <- dbWithWriteTransaction conn $ do
    inserted <- useStmt stInsertEbBody $ do
      dbBindBlob stInsertEbBody 1 ebHashRaw
      dbBindBlob stInsertEbBody 2 bounds
      dbBindBlob stInsertEbBody 3 (toStrictByteString (encodeLeiosEb eb))
      dbStep1 stInsertEbBody
      DB.changes db
    if inserted == 0
      then pure Nothing
      else do
        filledOffs <- if null fills then pure [] else fillLocally
        points <- lookupPoints conn point
        let missing = IntSet.fromDistinctAscList [0 .. n - 1] `IntSet.difference` IntSet.fromList filledOffs
        if IntSet.null missing
          then pure $ Just (points, points, filledOffs)
          else do
            -- Before the commit, so no reader sees the body complete.
            atomically $
              modifyTVar missingVar $
                Map.insert (pointEbHash point) missing
            pure $ Just (points, [], filledOffs)
  case mbResult of
    Nothing -> pure ([], [])
    -- Every point of the EB has its body now.
    Just (points, completed, filledOffs) -> do
      let wanted = IntSet.size (IntSet.fromList [off | (off, _) <- fills, off >= 0, off < n])
          unfilled = wanted - length filledOffs
      when (unfilled > 0) $
        traceWith (connTracer conn) $
          TraceLeiosDbFillsRejected (prettyLeiosPoint point) wanted unfilled
      forM_ points $ \p -> notify $ AcquiredEb p (encodeLeiosEbSize eb)
      forM_ completed $ notify . AcquiredEbTxs
      pure (completed, filledOffs)
 where
  Conn
    { conVolDb = db
    , connVolStmts =
      VolStmts{stInsertEbBody, stInsertEbClosure, stReads = ReadStmts{rsLookupEbIndex}}
    } = conn
  ebHashRaw = ebHashBytes (pointEbHash point)
  n = V.length (leiosEbTxs eb)
  bounds = encodeTxBounds eb
  start i = boundAt bounds i
  end i = boundAt bounds (i + 1)

  -- Copy each fill's bytes from the closure of the EB holding them. A fill
  -- whose bytes do not hash to the tx this EB lists at its offset fills
  -- nothing: its source vanished, does not hold the tx yet, or the tx cache
  -- located it wrongly. Of each source only the byte runs of the txs it supplies
  -- are read, and the new closure is written whole, once: small reads and
  -- writes at offsets into a big blob each walk its overflow pages, and whole
  -- reads cost the source's size. Which of several fills for one offset lands
  -- does not matter: the hash check makes their bytes the same.
  fillLocally = do
    filled <-
      foldM fillFrom IntMap.empty $
        Map.toList $
          Map.fromListWith
            (++)
            [(srcHash, [(dstOff, srcOff)]) | (dstOff, MkTxLocation srcHash srcOff) <- fills]
    rowid <- allocateClosure db stInsertEbClosure ebHashRaw (end (n - 1))
    unless (IntMap.null filled) $
      withBlob db "ebClosures" "txs" rowid True $ \dst ->
        withDie db (DB.blobWrite dst (closure filled) 0)
    pure (IntMap.keys filled)
   where
    -- Unfilled txs stay zeroes, as 'allocateClosure' left them.
    closure filled =
      BS.concat
        [ IntMap.findWithDefault (BS.replicate (end i - start i) 0) i filled
        | i <- [0 .. n - 1]
        ]

  fillFrom acc (srcHash, offs) =
    lookupEbIndex rsLookupEbIndex srcHash >>= \case
      Just srcIx@EbIndex{eiTxCount, eiClosureRowid = Just srcRowid} | eiTxCount > 0 -> do
        srcBounds <- readTxBounds db srcIx 0 (eiTxCount - 1)
        let at i = boundAt srcBounds i
            -- The fills this source can serve, by its offset.
            wanted =
              [ (dstOff, srcOff)
              | (dstOff, srcOff) <- offs
              , dstOff >= 0
              , dstOff < n
              , srcOff >= 0
              , srcOff < eiTxCount
              , at (srcOff + 1) - at srcOff == end dstOff - start dstOff
              ]
        runs <-
          withBlob db "ebClosures" "txs" srcRowid False $ \src ->
            forM (coalesce at (IntSet.toAscList (IntSet.fromList (map snd wanted)))) $ \(lo, hi) -> do
              bytes <- withDie db (DB.blobRead src (at (hi + 1) - at lo) (at lo))
              pure (lo, (at lo, bytes))
        let runAt = IntMap.fromDistinctAscList runs
            txBytes srcOff = case IntMap.lookupLE srcOff runAt of
              Just (_, (from, bytes)) -> BS.copy (BS.take (at (srcOff + 1) - at srcOff) (BS.drop (at srcOff - from) bytes))
              Nothing -> BS.empty
            fillOne filled (dstOff, srcOff)
              | dstOff `IntMap.member` filled = filled
              | txIsWritten bytes
              , hashTxBytes bytes == fst (leiosEbTxs eb V.! dstOff) =
                  IntMap.insert dstOff bytes filled
              | otherwise = filled
             where
              bytes = txBytes srcOff
        pure $ Foldable.foldl' fillOne acc wanted
      _ -> pure acc

-- | Group ascending tx offsets into runs to read at once, merging runs whose
-- bytes are at most 'runGap' apart: one read beats two reads of what is
-- nearly adjacent.
coalesce :: (Int -> Int) -> [Int] -> [(Int, Int)]
coalesce at = go
 where
  go [] = []
  go (o : os) = extend o o os
  extend lo hi (o : os)
    | at o - at (hi + 1) <= runGap = extend lo o os
  extend lo hi os = (lo, hi) : go os

runGap :: Int
runGap = 4096

-- | Persist tx bytes for one EB, in place in its closure, which the first
-- write allocates with zeroes. How many txs a write carries is the caller's to
-- decide.
--
-- PREREQ: each tx's bytes hash to the tx hash the body lists at its offset.
-- So a tx written twice is the same bytes, and overwriting it is harmless.
--
-- A transaction is dropped if:
--
-- * the EB's body is not stored;
-- * its offset is not in the body, or earlier in the same write;
-- * or its size is not the declared one.
--
-- Returns every point of the EB if this write completed it.
sqlInsertTxs ::
  Conn ->
  StrictTVar IO Missing ->
  (LeiosEbNotification -> IO ()) ->
  -- | The EB the txs belong to
  LeiosPoint ->
  -- | Tx bytes by offset into the EB's body
  [(TxOffset, ByteString)] ->
  IO CompletedEbs
sqlInsertTxs conn missingVar notify point txBytesWithOffsets = do
  missing0 <- readTVarIO missingVar
  mbCompleted <-
    dbWithWriteTransaction conn $
      lookupEbIndex rsLookupEbIndex (pointEbHash point) >>= \case
        Nothing -> pure Nothing
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
                    , txIsWritten txBytes
                    ]
          unless (null accepted) $ do
            rowid <- case eiClosureRowid of
              Just rowid -> pure rowid
              Nothing -> allocateClosure db stInsertEbClosure (ebHashBytes (pointEbHash point)) (end (n - 1))
            withBlob db "ebClosures" "txs" rowid True $ \blob ->
              forM_ (runs accepted) $ \run ->
                withDie db (DB.blobWrite blob (BS.concat (map snd run)) (start (fst (head run))))
          case Map.lookup (pointEbHash point) missing0 of
            Nothing -> pure Nothing
            Just offs -> do
              let offs' = offs `IntSet.difference` IntSet.fromDistinctAscList (map fst accepted)
              if IntSet.null offs'
                then Just . Left <$> lookupPoints conn point
                else pure $ Just $ Right offs'
  -- After the commit, so no reader sees the EB complete early. Only the
  -- writer changes 'Missing', so it has not changed since it was read.
  case mbCompleted of
    Nothing -> pure []
    Just (Right still) -> do
      atomically $ modifyTVar missingVar $ Map.insert (pointEbHash point) still
      pure []
    Just (Left completed) -> do
      atomically $ modifyTVar missingVar $ Map.delete (pointEbHash point)
      forM_ completed $ notify . AcquiredEbTxs
      pure completed
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

-- | Allocate an EB's closure with zeroes, returning its rowid.
allocateClosure :: DB.Database -> DB.Statement -> ByteString -> Int -> IO Int64
allocateClosure db stmt ebHashRaw size = do
  useStmt stmt $ do
    dbBindBlob stmt 1 ebHashRaw
    dbBindInt64 stmt 2 (fromIntegral size)
    dbStep1 stmt
  DB.lastInsertRowId db

-- | Every point announcing the given point's EB.
lookupPoints :: Conn -> LeiosPoint -> IO [LeiosPoint]
lookupPoints conn point =
  useStmt stmt $ do
    dbBindBlob stmt 1 (ebHashBytes (pointEbHash point))
    let loop acc =
          dbStep stmt >>= \case
            DB.Done -> pure (reverse acc)
            DB.Row -> do
              slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
              loop (MkLeiosPoint slot (pointEbHash point) : acc)
    loop []
 where
  Conn{connVolStmts = VolStmts{stLookupEbPoints = stmt}} = conn
