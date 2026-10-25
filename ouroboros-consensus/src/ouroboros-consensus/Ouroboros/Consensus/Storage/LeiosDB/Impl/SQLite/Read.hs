{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Read operations of LeiosDB's SQLite backend.
--
-- Each lookup asks the volatile
-- partition first and falls back to the immutable one for EBs that were
-- copied and evicted.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Read
  ( sqlScanEbPoints
  , sqlLookupEbBody
  , sqlBatchRetrieveTxs
  , sqlLookupEbClosure
  , sqlScanBodiedPointsSince
  , EbIndex (..)
  , decodeStoredTxHashes
  , lookupBlob
  , lookupEbIndex
  , readTxBounds
  , boundAt
  , encodeTxBounds
  , withBlob
  ) where

import Cardano.Binary.FixedSizeCodec (decodeFixedSized)
import Cardano.Crypto.PackedBytes (PackedBytes)
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import Control.DeepSeq (force)
import Control.Exception (bracket, evaluate)
import Control.Monad (forM, replicateM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import qualified Data.Vector.Strict as V
import qualified Database.SQLite3.Direct as DB
import Ouroboros.Consensus.Leios.Types
  ( EbHash
  , LeiosEb (..)
  , LeiosPoint (..)
  , TxHash (..)
  , ebHashBytes
  )
import Ouroboros.Consensus.Storage.LeiosDB.Exception (throwLeiosDbException)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Connection (Conn (..))
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Primitives
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Queries (sql_scan_bodied_points_since)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Statements

sqlScanEbPoints :: Conn -> IO [(SlotNo, EbHash)]
sqlScanEbPoints conn =
  dbWithTransaction db $ useStmt stmt $ loop []
 where
  Conn{conVolDb = db, connVolStmts = VolStmts{stScanEbPoints = stmt}} = conn
  loop acc =
    dbStep stmt >>= \case
      DB.Done -> pure (reverse acc)
      DB.Row -> do
        slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
        hash <- ebHashFromBlob =<< DB.columnBlob stmt 1
        loop ((slot, hash) : acc)

sqlLookupEbBody :: Conn -> EbHash -> IO (Maybe ByteString)
sqlLookupEbBody conn ebHash = do
  vol <- dbWithTransaction volDb $ lookupBlob (rsLookupEbBody volReads) ebHash
  -- No body means the EB may have been copied to the immutable partition and
  -- evicted.
  maybe (lookupBlob (rsLookupEbBody immReads) ebHash) (pure . Just) vol
 where
  Conn
    { conVolDb = volDb
    , connVolStmts = VolStmts{stReads = volReads}
    , connImmStmts = ImmStmts{immReads}
    } = conn

-- | Retrieve tx bytes for a batch of offsets into one EB: the offsets the body
-- declares, with their bytes as stored, zeroes where no tx was written. Reads
-- where the requested txs are from 'txBounds', then only their bytes through
-- one blob handle, one read per run of consecutive offsets. Never reads the
-- body.
sqlBatchRetrieveTxs ::
  Conn ->
  EbHash ->
  [Int] ->
  IO [(Int, ByteString)]
sqlBatchRetrieveTxs conn ebHash offsets = (evaluate . force =<<) $ do
  vol <- dbWithTransaction volDb $ retrieveFrom volDb volReads
  -- TODO This misses an EB that was copied to the immutable partition, evicted
  -- from the volatile partition, and then fetched again: until the volatile
  -- partition has all of its txs again, the missing ones read as zeroes,
  -- although the immutable partition holds them. The same holds for
  -- 'sqlLookupEbClosure'.
  case vol of
    Just txs -> pure txs
    Nothing -> fromMaybe [] <$> retrieveFrom immDb immReads
 where
  Conn
    { conVolDb = volDb
    , conImmDb = immDb
    , connVolStmts = VolStmts{stReads = volReads}
    , connImmStmts = ImmStmts{immReads}
    } = conn

  -- 'Nothing' if the body is not stored here.
  retrieveFrom db ReadStmts{rsLookupEbIndex} =
    lookupEbIndex rsLookupEbIndex ebHash >>= \case
      Nothing -> pure Nothing
      Just ix@EbIndex{eiTxCount, eiClosureRowid} ->
        case [o | o <- IntSet.toAscList (IntSet.fromList offsets), o >= 0, o < eiTxCount] of
          [] -> pure (Just [])
          wanted@(lo : _) -> do
            bounds <- readTxBounds db ix lo (last wanted)
            let start o = boundAt bounds (o - lo)
                end o = boundAt bounds (o - lo + 1)
                runs = consecutiveRuns wanted
            Just . concat <$> case eiClosureRowid of
              Nothing ->
                pure [[(o, BS.replicate (end o - start o) 0) | o <- run] | run <- runs]
              Just rowid ->
                withBlob db "ebClosures" "txs" rowid False $ \blob ->
                  forM runs $ \run -> do
                    let at = start (head run)
                    bytes <- withDie db (DB.blobRead blob (end (last run) - at) at)
                    pure [(o, BS.take (end o - start o) (BS.drop (start o - at) bytes)) | o <- run]

-- | The hashes and bytes of the EB's txs in body order, zeroes where no tx
-- was written, or 'Nothing' if the body is not stored.
sqlLookupEbClosure :: Conn -> EbHash -> IO (Maybe [(TxHash, ByteString)])
sqlLookupEbClosure conn ebHash = (evaluate . force =<<) $ do
  vol <- dbWithTransaction volDb $ closureFrom volReads
  maybe (closureFrom immReads) (pure . Just) vol
 where
  Conn
    { conVolDb = volDb
    , connVolStmts = VolStmts{stReads = volReads}
    , connImmStmts = ImmStmts{immReads}
    } = conn

  closureFrom ReadStmts{rsLookupEbClosure = stmt} =
    useStmt stmt $ do
      dbBindBlob stmt 1 (ebHashBytes ebHash)
      dbStep stmt >>= \case
        DB.Done -> pure Nothing
        DB.Row -> do
          bounds <- DB.columnBlob stmt 0
          let n = BS.length bounds `div` 4 - 1
          txs <-
            DB.columnType stmt 1 >>= \case
              DB.NullColumn -> pure (BS.replicate (boundAt bounds n) 0)
              _ -> DB.columnBlob stmt 1
          txHashes <- decodeStoredTxHashes =<< DB.columnBlob stmt 2
          pure $
            Just $
              zip
                txHashes
                [ BS.take (boundAt bounds (i + 1) - boundAt bounds i) (BS.drop (boundAt bounds i) txs)
                | i <- [0 .. n - 1]
                ]

-- | The points no older than the given slot whose EB body is stored, oldest
-- first.
sqlScanBodiedPointsSince :: Conn -> SlotNo -> IO [LeiosPoint]
sqlScanBodiedPointsSince conn (SlotNo slot) =
  dbWithTransaction db $ withStmt db sql_scan_bodied_points_since $ \stmt -> do
    dbBindInt64 stmt 1 (fromIntegral slot)
    let loop acc =
          dbStep stmt >>= \case
            DB.Done -> pure (reverse acc)
            DB.Row -> do
              s <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
              h <- ebHashFromBlob =<< DB.columnBlob stmt 1
              loop (MkLeiosPoint s h : acc)
    loop []
 where
  Conn{conVolDb = db} = conn

-- | The tx hashes of a stored body ('encodeLeiosEb'), in order.
decodeStoredTxHashes :: ByteString -> IO [TxHash]
decodeStoredTxHashes body =
  case CBOR.deserialiseFromBytes decoder (BSL.fromStrict body) of
    Left err -> throwLeiosDbException $ "stored EB body does not decode: " <> show err
    Right (_, hashes) -> pure hashes
 where
  decoder :: CBOR.Decoder s [TxHash]
  decoder = do
    n <- CBOR.decodeMapLen
    replicateM n $ do
      h <- decodeFixedSized :: CBOR.Decoder s (PackedBytes 32)
      _size <- CBOR.decodeWord32
      pure (MkTxHash h)

-- * Statements

lookupBlob :: DB.Statement -> EbHash -> IO (Maybe ByteString)
lookupBlob stmt ebHash =
  useStmt stmt $ do
    dbBindBlob stmt 1 (ebHashBytes ebHash)
    dbStep stmt >>= \case
      DB.Done -> pure Nothing
      DB.Row -> Just <$> DB.columnBlob stmt 0

-- | Where an EB's blobs are.
data EbIndex = EbIndex
  { eiBodyRowid :: !Int64
  , eiTxCount :: !Int
  , eiClosureRowid :: !(Maybe Int64)
  -- ^ 'Nothing' until a tx is written
  }

-- | Where the EB's blobs are, or 'Nothing' if its body is not stored.
lookupEbIndex :: DB.Statement -> EbHash -> IO (Maybe EbIndex)
lookupEbIndex stmt ebHash =
  useStmt stmt $ do
    dbBindBlob stmt 1 (ebHashBytes ebHash)
    dbStep stmt >>= \case
      DB.Done -> pure Nothing
      DB.Row -> do
        eiBodyRowid <- DB.columnInt64 stmt 0
        eiTxCount <- fromIntegral <$> DB.columnInt64 stmt 1
        eiClosureRowid <-
          DB.columnType stmt 2 >>= \case
            DB.NullColumn -> pure Nothing
            _ -> Just <$> DB.columnInt64 stmt 2
        pure (Just EbIndex{eiBodyRowid, eiTxCount, eiClosureRowid})

-- | The bounds of the txs at offsets @[lo, hi]@: where each starts in the
-- closure, and where the last one ends.
readTxBounds :: DB.Database -> EbIndex -> Int -> Int -> IO ByteString
readTxBounds db EbIndex{eiBodyRowid} lo hi =
  withBlob db "ebBodies" "txBounds" eiBodyRowid False $ \blob ->
    withDie db (DB.blobRead blob ((hi - lo + 2) * 4) (lo * 4))

-- | Open a blob of the main database, for the duration of the action.
withBlob :: DB.Database -> String -> String -> Int64 -> Bool -> (DB.Blob -> IO a) -> IO a
withBlob db table column rowid writable =
  bracket
    ( withDie
        db
        (DB.blobOpen db (fromString "main") (fromString table) (fromString column) rowid writable)
    )
    (withDie db . DB.blobClose)

-- | Maximal runs of consecutive offsets, in order.
consecutiveRuns :: [Int] -> [[Int]]
consecutiveRuns [] = []
consecutiveRuns (o : os) = go o [o] os
 where
  go prev acc (x : xs) | x == prev + 1 = go x (x : acc) xs
  go _ acc rest = reverse acc : consecutiveRuns rest

-- * Tx bounds

-- | Where each tx of the EB is in its closure: n + 1 positions, as 32-bit
-- big-endian words. Tx @i@ is the closure's bytes from position @i@ up to
-- position @i + 1@, and the last position is the closure's size.
encodeTxBounds :: LeiosEb -> ByteString
encodeTxBounds eb =
  BSL.toStrict . BB.toLazyByteString $
    foldMap BB.word32BE (scanl (+) 0 [size | (_, size) <- V.toList (leiosEbTxs eb)])

-- | The @i@th position in (a slice of) 'encodeTxBounds'.
boundAt :: ByteString -> Int -> Int
boundAt bounds i = foldl (\acc k -> acc * 256 + fromIntegral (BS.index bounds (i * 4 + k))) 0 [0 .. 3]
