{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Read operations of LeiosDB's SQLite backend.
--
-- Each lookup asks the volatile
-- partition first and falls back to the immutable one for EBs that were
-- copied and evicted.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Read
  ( sqlScanEbPoints
  , sqlScanCompleteEbPointsSince
  , sqlLookupEbBody
  , sqlBatchRetrieveTxs
  , sqlLookupEbClosure
  , EbIndex (..)
  , EbRow (..)
  , lookupEbIndex
  , readTxBounds
  , readTxBytes
  , consecutiveRuns
  , boundAt
  , encodeTxBounds
  , withBlob
  , splitClosure
  , closureMatches
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.DeepSeq (force)
import Control.Exception (bracket, evaluate)
import Control.Monad (filterM, forM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Database.SQLite3.Direct as DB
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlock
  , EndorserBlockHash
  , LeiosPoint (..)
  , LeiosTx (..)
  , deserialiseEndorserBlock
  , endorserBlockReferences
  , hashLeiosTx
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Connection (Conn (..))
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Primitives

sqlScanEbPoints :: Conn -> IO [(SlotNo, EndorserBlockHash)]
sqlScanEbPoints conn =
  dbWithTransaction db $
    withStmt db "SELECT ebSlot, ebHashBytes FROM ebs ORDER BY ebSlot ASC" $ \stmt ->
      loop stmt []
 where
  Conn{conVolDb = db} = conn
  loop stmt acc =
    dbStep stmt >>= \case
      DB.Done -> pure (reverse acc)
      DB.Row -> do
        slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
        hash <- dbColumnHash stmt 1
        loop stmt ((slot, hash) : acc)

-- | Each point at or after the slot whose closure is complete. A closure is
-- complete if each of its txs hashes to what the body lists. An EB the
-- immutable partition holds is complete, since only complete closures are
-- copied there. It may have been evicted from the volatile partition, and
-- fetched again only in part.
sqlScanCompleteEbPointsSince :: Conn -> SlotNo -> IO [LeiosPoint]
sqlScanCompleteEbPointsSince conn sinceSlot = do
  points <-
    dbWithTransaction volDb $
      withStmt volDb "SELECT ebSlot, ebHashBytes FROM ebs WHERE ebSlot >= ?1 ORDER BY ebSlot" $ \stmt -> do
        dbBindInt64 stmt 1 (fromIntegral $ unSlotNo sinceSlot)
        loop stmt []
  filterM isComplete points
 where
  Conn{conVolDb = volDb, conImmDb = immDb} = conn

  loop stmt acc =
    dbStep stmt >>= \case
      DB.Done -> pure (reverse acc)
      DB.Row -> do
        slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
        hash <- dbColumnHash stmt 1
        loop stmt (MkLeiosPoint slot hash : acc)

  -- The immutable partition first: that is an index lookup, not a hash of
  -- every tx.
  isComplete point =
    lookupEbIndex immDb (AnyOf (pointEbHash point)) >>= \case
      Just _ -> pure True
      Nothing -> dbWithTransaction volDb $ do
        mbBody <- lookupBlob volDb (AtPoint point)
        mbTxs <- closureFrom volDb (AtPoint point)
        pure $ case (mbBody, mbTxs) of
          (Just body, Just txs) -> maybe False (closureMatches body) (sequence txs)
          _ -> False

-- | Whether these txs are the closure of the EB with these body bytes: each
-- hashes to what the body lists at its offset.
closureMatches :: ByteString -> [ByteString] -> Bool
closureMatches body txs = case deserialiseEndorserBlock body of
  Left _ -> False
  Right eb ->
    length refs == length txs
      && and (zipWith (\(txHash, _size) tx -> hashLeiosTx (MkLeiosTx tx) == txHash) refs txs)
   where
    refs = endorserBlockReferences eb

-- | Split a closure at its 'encodeTxBounds' into its txs, 'Nothing' where no
-- tx was written.
splitClosure :: ByteString -> ByteString -> [Maybe ByteString]
splitClosure bounds txs =
  [ written (BS.take (boundAt bounds (i + 1) - boundAt bounds i) (BS.drop (boundAt bounds i) txs))
  | i <- [0 .. BS.length bounds `div` 4 - 2]
  ]

sqlLookupEbBody :: Conn -> LeiosPoint -> IO (Maybe ByteString)
sqlLookupEbBody conn point = do
  vol <- dbWithTransaction volDb $ lookupBlob volDb (AtPoint point)
  -- No body means the EB may have been copied to the immutable partition and
  -- evicted.
  maybe (lookupBlob immDb (AnyOf (pointEbHash point))) (pure . Just) vol
 where
  Conn{conVolDb = volDb, conImmDb = immDb} = conn

-- | Retrieve tx bytes for a batch of offsets into the EB at one point: the
-- offsets the body declares, with their bytes, or 'Nothing' where no tx was
-- written. Reads where the requested txs are from 'txBounds', then only their
-- bytes through one blob handle, one read per run of consecutive offsets.
-- Never reads the body.
--
-- A tx the volatile partition has not written yet is read from the immutable
-- one, which holds it if the EB was copied, evicted, and is being fetched
-- again.
sqlBatchRetrieveTxs ::
  Conn ->
  LeiosPoint ->
  [Int] ->
  IO [(Int, Maybe ByteString)]
sqlBatchRetrieveTxs conn point offsets = (evaluate . force =<<) $ do
  vol <- dbWithTransaction volDb $ retrieveFrom volDb (AtPoint point) offsets
  case vol of
    Nothing -> fromMaybe [] <$> retrieveFrom immDb immRow offsets
    Just txs -> case [o | (o, Nothing) <- txs] of
      [] -> pure txs
      missing -> do
        imm <- IntMap.fromList . fromMaybe [] <$> retrieveFrom immDb immRow missing
        pure [(o, maybe (IntMap.findWithDefault Nothing o imm) Just mbBytes) | (o, mbBytes) <- txs]
 where
  Conn{conVolDb = volDb, conImmDb = immDb} = conn
  immRow = AnyOf (pointEbHash point)

  -- 'Nothing' if the body is not stored here.
  retrieveFrom db row wantedOffsets =
    lookupEbIndex db row >>= \case
      Nothing -> pure Nothing
      Just ix@EbIndex{eiTxCount} ->
        Just
          <$> readTxBytes
            db
            ix
            [o | o <- IntSet.toAscList (IntSet.fromList wantedOffsets), o >= 0, o < eiTxCount]

-- | The bytes of the txs of the EB at the point in body order, or 'Nothing'
-- unless each of them is written. Like 'sqlBatchRetrieveTxs', it reads the
-- immutable partition's closure if the volatile one lacks a tx.
sqlLookupEbClosure :: Conn -> LeiosPoint -> IO (Maybe [ByteString])
sqlLookupEbClosure conn point = (evaluate . force =<<) $ do
  vol <- dbWithTransaction volDb $ closureFrom volDb (AtPoint point)
  case sequence =<< vol of
    Just txs -> pure (Just txs)
    Nothing -> (sequence =<<) <$> closureFrom immDb (AnyOf (pointEbHash point))
 where
  Conn{conVolDb = volDb, conImmDb = immDb} = conn

-- * Lookups

-- | Which row holding an EB a lookup takes: the one at a point, or any. The
-- immutable partition holds an EB at the point it was certified at, which a
-- read of it at another point does not name.
data EbRow = AtPoint !LeiosPoint | AnyOf !EndorserBlockHash

-- | Run a query of the row, with @?1@ its slot and @?2@ its hash.
withRowStmt :: DB.Database -> EbRow -> (DB.Utf8, DB.Utf8) -> (DB.Statement -> IO a) -> IO a
withRowStmt db row (atPoint, anyOf) k = case row of
  AtPoint point -> withStmt db atPoint $ \stmt -> do
    dbBindInt64 stmt 1 (fromIntegral $ unSlotNo (pointSlotNo point))
    dbBindHash stmt 2 (pointEbHash point)
    k stmt
  AnyOf ebHash -> withStmt db anyOf $ \stmt -> do
    dbBindHash stmt 2 ebHash
    k stmt

closureFrom :: DB.Database -> EbRow -> IO (Maybe [Maybe ByteString])
closureFrom db row =
  withRowStmt
    db
    row
    ( "SELECT txBounds, txs FROM ebs WHERE ebSlot = ?1 AND ebHashBytes = ?2"
    , "SELECT txBounds, txs FROM ebs WHERE ebHashBytes = ?2 LIMIT 1"
    )
    $ \stmt ->
      dbStep stmt >>= \case
        DB.Done -> pure Nothing
        DB.Row -> do
          bounds <- DB.columnBlob stmt 0
          txs <- DB.columnBlob stmt 1
          pure $ Just (splitClosure bounds txs)

-- | An EB's body.
lookupBlob :: DB.Database -> EbRow -> IO (Maybe ByteString)
lookupBlob db row =
  withRowStmt
    db
    row
    ( "SELECT body FROM ebs WHERE ebSlot = ?1 AND ebHashBytes = ?2"
    , "SELECT body FROM ebs WHERE ebHashBytes = ?2 LIMIT 1"
    )
    $ \stmt ->
      dbStep stmt >>= \case
        DB.Done -> pure Nothing
        DB.Row -> Just <$> DB.columnBlob stmt 0

-- | A row holding the EB.
data EbIndex = EbIndex
  { eiRowid :: !Int64
  , eiTxCount :: !Int
  }

-- | Where the EB's blobs are, or 'Nothing' if no row holds it.
lookupEbIndex :: DB.Database -> EbRow -> IO (Maybe EbIndex)
lookupEbIndex db row =
  withRowStmt
    db
    row
    ( "SELECT rowid, txCount FROM ebs WHERE ebSlot = ?1 AND ebHashBytes = ?2"
    , "SELECT rowid, txCount FROM ebs WHERE ebHashBytes = ?2 LIMIT 1"
    )
    $ \stmt ->
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

-- | The bytes of the txs at these offsets, which must be ascending, distinct
-- and in the body, or 'Nothing' where no tx was written: one read per run of
-- consecutive offsets.
readTxBytes :: DB.Database -> EbIndex -> [Int] -> IO [(Int, Maybe ByteString)]
readTxBytes _ _ [] = pure []
readTxBytes db ix@EbIndex{eiRowid} (lo : rest) = do
  bounds <- readTxBounds db ix lo (NE.last (lo :| rest))
  let start o = boundAt bounds (o - lo)
      end o = boundAt bounds (o - lo + 1)
  fmap concat $ withBlob db "ebs" "txs" eiRowid False $ \blob ->
    forM (consecutiveRuns id (lo : rest)) $ \run -> do
      let at = start (NE.head run)
      bytes <- withDie db (DB.blobRead blob (end (NE.last run) - at) at)
      pure [(o, written (BS.take (end o - start o) (BS.drop (start o - at) bytes))) | o <- NE.toList run]

-- | A tx's bytes as the closure holds them, or 'Nothing' if they are zeroes,
-- as the closure is allocated: a valid tx is never all zeroes.
written :: ByteString -> Maybe ByteString
written bytes
  | BS.all (== 0) bytes = Nothing
  | otherwise = Just bytes

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
