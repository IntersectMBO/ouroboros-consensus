{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Read operations of LeiosDB's SQLite backend.
--
-- Each lookup reads one partition, chosen by the point's slot, unless a
-- promotion overtook the read ('readPartition').
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Read
  ( sqlScanCompleteEbPointsSince
  , sqlLookupEbBody
  , sqlBatchRetrieveTxs
  , sqlLookupEbClosure
  , sqlIsImmutable
  , readPartition
  , lookupBlob
  , EbIndex (..)
  , lookupEbIndex
  , readTxBounds
  , readTxWritten
  , readTxBytes
  , isWritten
  , consecutiveRuns
  , boundAt
  , encodeTxBounds
  , withBlob
  , splitClosure
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict (readTVarIO)
import Control.Exception (bracket)
import Control.Monad (forM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Database.SQLite3.Direct as DB
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlock
  , LeiosPoint (..)
  , LeiosTx (..)
  , Serialised (..)
  , TxOffset
  , endorserBlockReferences
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Connection (Conn (..))
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Primitives

-- | Each point at or after the slot whose closure is complete in the partition
-- its reads go to ('readPartition'): in the volatile partition, if each of its
-- txs is marked written; in the immutable one, always, as only complete
-- closures are copied there.
sqlScanCompleteEbPointsSince :: Conn -> SlotNo -> IO [LeiosPoint]
sqlScanCompleteEbPointsSince conn sinceSlot = do
  promotedSlot <- readTVarIO promotedSlotVar
  marked <-
    dbWithTransaction volDb $
      withStmt volDb "SELECT ebSlot, ebHashBytes, txWritten FROM ebs WHERE ebSlot >= ?1" $ \stmt -> do
        dbBindInt64 stmt 1 (fromIntegral $ unSlotNo sinceSlot)
        let loop acc =
              dbStep stmt >>= \case
                DB.Done -> pure (reverse acc)
                DB.Row -> do
                  point <- columnPoint stmt
                  allWritten <- BS.all (/= 0) <$> DB.columnBlob stmt 2
                  loop (if allWritten then point : acc else acc)
        loop []
  let vol = [point | point <- marked, pointSlotNo point >= promotedSlot]
  imm <-
    dbWithTransaction immDb $
      withStmt immDb "SELECT ebSlot, ebHashBytes FROM ebs WHERE ebSlot >= ?1 AND ebSlot < ?2" $ \stmt -> do
        dbBindInt64 stmt 1 (fromIntegral $ unSlotNo sinceSlot)
        dbBindInt64 stmt 2 (fromIntegral $ unSlotNo promotedSlot)
        let loop acc =
              dbStep stmt >>= \case
                DB.Done -> pure (reverse acc)
                DB.Row -> do
                  point <- columnPoint stmt
                  loop (point : acc)
        loop []
  pure $ Set.toAscList (Set.fromList vol <> Set.fromList imm)
 where
  Conn{conVolDb = volDb, conImmDb = immDb, connPromotedSlot = promotedSlotVar} = conn

  columnPoint stmt =
    MkLeiosPoint
      <$> (SlotNo . fromIntegral <$> DB.columnInt64 stmt 0)
      <*> dbColumnHash stmt 1

-- | Split a closure at its 'encodeTxBounds' into its txs, each sliced before
-- the list is returned.
splitClosure :: ByteString -> ByteString -> [ByteString]
splitClosure bounds txs = go (BS.length bounds `div` 4 - 2) []
 where
  go i acc
    | i < 0 = acc
    | otherwise =
        let !tx = BS.take (boundAt bounds (i + 1) - boundAt bounds i) (BS.drop (boundAt bounds i) txs)
         in go (i - 1) (tx : acc)

-- | Whether the marker of the tx at this index of the markers is set.
isWritten :: ByteString -> Int -> Bool
isWritten txWritten i = BS.index txWritten i /= 0

-- | Read the point in one transaction of the partition its reads go to: the
-- immutable one if the point is older than the slot the latest promotion
-- reached, else the volatile one. 'Nothing' if that holds no row of the point.
--
-- A promotion and a collection may overtake a read of the volatile partition,
-- deleting the row after the read chose the partition. If the read finds no
-- row and the point is then older than the promoted slot, it reads the
-- immutable partition, which that promotion filled.
readPartition :: Conn -> LeiosPoint -> (DB.Database -> IO (Maybe a)) -> IO (Maybe a)
readPartition conn point readRow = do
  promoted <- isPromoted
  if promoted
    then fromImmutable
    else
      dbWithTransaction volDb (readRow volDb) >>= \case
        Nothing -> do
          overtaken <- isPromoted
          if overtaken then fromImmutable else pure Nothing
        found -> pure found
 where
  Conn{conVolDb = volDb, conImmDb = immDb, connPromotedSlot = promotedSlotVar} = conn

  isPromoted = (pointSlotNo point <) <$> readTVarIO promotedSlotVar

  fromImmutable = dbWithTransaction immDb (readRow immDb)

sqlLookupEbBody :: Conn -> LeiosPoint -> IO (Maybe (Serialised EndorserBlock))
sqlLookupEbBody conn point =
  fmap (Serialised . BSL.fromStrict) <$> readPartition conn point (`lookupBlob` point)

-- | Retrieve tx bytes for a batch of offsets into the EB at one point: the
-- offsets the body declares, with their bytes, or 'Nothing' where no tx was
-- written. Reads where the requested txs are from 'txBounds', then only their
-- bytes through one blob handle, one read per run of consecutive offsets.
-- Never reads the body.
sqlBatchRetrieveTxs ::
  Conn ->
  LeiosPoint ->
  Set TxOffset ->
  IO (Map TxOffset ByteString)
sqlBatchRetrieveTxs conn point offsets =
  fmap (fromMaybe Map.empty) $
    readPartition conn point $ \db ->
      lookupEbIndex db point
        >>= traverse
          ( \ix@EbIndex{eiTxCount} ->
              readTxBytes db ix [o | o <- Set.toAscList offsets, o >= 0, o < eiTxCount]
          )

-- | Whether the immutable partition holds the EB at the point, whichever
-- partition its reads go to.
sqlIsImmutable :: Conn -> LeiosPoint -> IO Bool
sqlIsImmutable Conn{conImmDb = immDb} point =
  dbWithTransaction immDb $
    withStmt immDb "SELECT 1 FROM ebs WHERE ebSlot = ?1 AND ebHashBytes = ?2" $ \stmt -> do
      dbBindInt64 stmt 1 (fromIntegral $ unSlotNo (pointSlotNo point))
      dbBindHash stmt 2 (pointEbHash point)
      dbStepSafe stmt >>= \case
        DB.Done -> pure False
        DB.Row -> pure True

-- | The bytes of the txs of the EB at the point in body order, or 'Nothing'
-- unless each of them is written.
sqlLookupEbClosure :: Conn -> LeiosPoint -> IO (Maybe [LeiosTx])
sqlLookupEbClosure conn point =
  fmap (map MkLeiosTx) <$> readPartition conn point (`completeClosureAt` point)

-- * Lookups

-- | The txs of the EB at the point in body order, or 'Nothing' unless each of
-- them is marked written. The closure is read only if each is.
completeClosureAt :: DB.Database -> LeiosPoint -> IO (Maybe [ByteString])
completeClosureAt db point =
  withStmt db "SELECT txWritten, txBounds, rowid FROM ebs WHERE ebSlot = ?1 AND ebHashBytes = ?2" $ \stmt -> do
    dbBindInt64 stmt 1 (fromIntegral $ unSlotNo (pointSlotNo point))
    dbBindHash stmt 2 (pointEbHash point)
    dbStep stmt >>= \case
      DB.Done -> pure Nothing
      DB.Row -> do
        txWritten <- DB.columnBlob stmt 0
        if BS.all (/= 0) txWritten
          then do
            bounds <- DB.columnBlob stmt 1
            rowid <- DB.columnInt64 stmt 2
            txs <- withBlob db "ebs" "txs" rowid False $ \blob ->
              withDie db (DB.blobRead blob (boundAt bounds (BS.length txWritten)) 0)
            let !closure = splitClosure bounds txs
            pure (Just closure)
          else pure Nothing

-- | An EB's body.
lookupBlob :: DB.Database -> LeiosPoint -> IO (Maybe ByteString)
lookupBlob db point =
  withStmt db "SELECT body FROM ebs WHERE ebSlot = ?1 AND ebHashBytes = ?2" $ \stmt -> do
    dbBindInt64 stmt 1 (fromIntegral $ unSlotNo (pointSlotNo point))
    dbBindHash stmt 2 (pointEbHash point)
    dbStep stmt >>= \case
      DB.Done -> pure Nothing
      DB.Row -> Just <$> DB.columnBlob stmt 0

-- | A row holding the EB.
data EbIndex = EbIndex
  { eiRowid :: !Int64
  , eiTxCount :: !Int
  }

-- | Where the EB's blobs are at the point, or 'Nothing' if no row holds it.
lookupEbIndex :: DB.Database -> LeiosPoint -> IO (Maybe EbIndex)
lookupEbIndex db point =
  withStmt db "SELECT rowid, length(txWritten) FROM ebs WHERE ebSlot = ?1 AND ebHashBytes = ?2" $ \stmt -> do
    dbBindInt64 stmt 1 (fromIntegral $ unSlotNo (pointSlotNo point))
    dbBindHash stmt 2 (pointEbHash point)
    dbStep stmt >>= \case
      DB.Done -> pure Nothing
      DB.Row -> do
        eiRowid <- DB.columnInt64 stmt 0
        eiTxCount <- fromIntegral <$> DB.columnInt64 stmt 1
        pure (Just EbIndex{eiRowid, eiTxCount})

-- | The bounds of the txs at offsets @[lo, hi]@: where each starts in the
-- closure, and where the last one ends.
readTxBounds :: DB.Database -> EbIndex -> Int -> Int -> IO ByteString
readTxBounds db EbIndex{eiRowid} lo hi =
  withBlob db "ebs" "txBounds" eiRowid False $ \blob ->
    withDie db (DB.blobRead blob ((hi - lo + 2) * 4) (lo * 4))

-- | The markers of the txs at offsets @[lo, hi]@, a byte each, set once the tx
-- is written.
readTxWritten :: DB.Database -> EbIndex -> Int -> Int -> IO ByteString
readTxWritten db EbIndex{eiRowid} lo hi =
  withBlob db "ebs" "txWritten" eiRowid False $ \blob ->
    withDie db (DB.blobRead blob (hi - lo + 1) lo)

-- | The bytes of the txs at these offsets that are marked written, each sliced
-- before the map is returned. The offsets must be ascending, distinct and in
-- the body. One read per run of consecutive written offsets.
readTxBytes :: DB.Database -> EbIndex -> [TxOffset] -> IO (Map TxOffset ByteString)
readTxBytes _ _ [] = pure Map.empty
readTxBytes db ix@EbIndex{eiRowid} (lo : rest) = do
  let hi = NE.last (lo :| rest)
  txWritten <- readTxWritten db ix lo hi
  let present = [o | o <- lo : rest, isWritten txWritten (o - lo)]
  if null present
    then pure Map.empty
    else do
      bounds <- readTxBounds db ix lo hi
      let start o = boundAt bounds (o - lo)
          end o = boundAt bounds (o - lo + 1)
      runs <- withBlob db "ebs" "txs" eiRowid False $ \blob ->
        forM (consecutiveRuns id present) $ \run -> do
          let at = start (NE.head run)
          bytes <- withDie db (DB.blobRead blob (end (NE.last run) - at) at)
          -- Strict in the values, so each slice is cut here.
          let !sliced =
                Map.fromDistinctAscList
                  [(o, BS.take (end o - start o) (BS.drop (start o - at) bytes)) | o <- NE.toList run]
          pure sliced
      pure $! Map.unions runs

-- | Open a blob of the main database, for the duration of the action.
withBlob :: DB.Database -> DB.Utf8 -> DB.Utf8 -> Int64 -> Bool -> (DB.Blob -> IO a) -> IO a
withBlob db table column rowid writable =
  bracket
    ( withDie
        db
        (DB.blobOpen db "main" table column rowid writable)
    )
    (withDie db . DB.blobClose)

-- | Maximal runs of elements at consecutive offsets, in order.
consecutiveRuns :: (a -> Int) -> [a] -> [NonEmpty a]
consecutiveRuns _ [] = []
consecutiveRuns offset (x : xs) = go (offset x) (x :| []) xs
 where
  go prev acc (y : ys) | offset y == prev + 1 = go (offset y) (y NE.<| acc) ys
  go _ acc rest = NE.reverse acc : consecutiveRuns offset rest

-- * Tx bounds

-- | Where each tx of the EB is in its closure: n + 1 positions, as 32-bit
-- big-endian words. Tx @i@ is the closure's bytes from position @i@ up to
-- position @i + 1@, and the last position is the closure's size.
encodeTxBounds :: EndorserBlock -> ByteString
encodeTxBounds eb =
  BSL.toStrict . BB.toLazyByteString $
    foldMap BB.word32BE (scanl (+) 0 [size | (_, size) <- endorserBlockReferences eb])

-- | The @i@th position in (a slice of) 'encodeTxBounds'.
boundAt :: ByteString -> Int -> Int
boundAt bounds i = foldl (\acc k -> acc * 256 + fromIntegral (BS.index bounds (i * 4 + k))) 0 [0 .. 3]
