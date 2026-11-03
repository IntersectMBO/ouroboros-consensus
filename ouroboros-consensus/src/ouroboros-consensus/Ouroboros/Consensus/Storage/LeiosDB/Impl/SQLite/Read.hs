{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

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
  , lookupEbIndex
  , readTxBounds
  , readTxBytes
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
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.String (fromString)
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
import Ouroboros.Consensus.Storage.LeiosDB.API (unwrittenTx)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Connection (Conn (..))
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Primitives
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Statements

sqlScanEbPoints :: Conn -> IO [(SlotNo, EndorserBlockHash)]
sqlScanEbPoints conn =
  dbWithTransaction db $ useStmt stmt $ loop []
 where
  Conn{conVolDb = db, connVolStmts = VolStmts{stScanEbPoints = stmt}} = conn
  loop acc =
    dbStep stmt >>= \case
      DB.Done -> pure (reverse acc)
      DB.Row -> do
        slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
        hash <- dbColumnHash stmt 1
        loop ((slot, hash) : acc)

-- | Each EB whose closure is complete, at its youngest point at or after the
-- slot. A closure is complete if each of its txs hashes to what the body lists.
-- An EB the immutable partition holds is complete, since only complete closures
-- are copied there. It may have been evicted from the volatile partition, and
-- fetched again only in part.
sqlScanCompleteEbPointsSince :: Conn -> SlotNo -> IO [LeiosPoint]
sqlScanCompleteEbPointsSince conn sinceSlot = do
  youngest <-
    fmap (Map.fromListWith max) $
      dbWithTransaction volDb $
        useStmt stmt $ do
          dbBindInt64 stmt 1 (fromIntegral $ unSlotNo sinceSlot)
          loop []
  complete <- filterM (isComplete . fst) (Map.toList youngest)
  pure [MkLeiosPoint slot ebHash | (ebHash, slot) <- complete]
 where
  Conn
    { conVolDb = volDb
    , connVolStmts = VolStmts{stScanEbsSince = stmt, stReads = volReads}
    , connImmStmts = ImmStmts{immReads}
    } = conn

  loop acc =
    dbStep stmt >>= \case
      DB.Done -> pure (reverse acc)
      DB.Row -> do
        slot <- SlotNo . fromIntegral <$> DB.columnInt64 stmt 0
        hash <- dbColumnHash stmt 1
        loop ((hash, slot) : acc)

  -- The immutable partition first: that is an index lookup, not a hash of
  -- every tx.
  isComplete ebHash =
    lookupEbIndex (rsLookupEbIndex immReads) ebHash >>= \case
      Just _ -> pure True
      Nothing -> dbWithTransaction volDb $ do
        mbBody <- lookupBlob (rsLookupEbBody volReads) ebHash
        mbTxs <- closureFrom volReads ebHash
        pure $ case (mbBody, mbTxs) of
          (Just body, Just txs) -> closureMatches body txs
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

-- | Split a closure at its 'encodeTxBounds' into its txs.
splitClosure :: ByteString -> ByteString -> [ByteString]
splitClosure bounds txs =
  [ BS.take (boundAt bounds (i + 1) - boundAt bounds i) (BS.drop (boundAt bounds i) txs)
  | i <- [0 .. BS.length bounds `div` 4 - 2]
  ]

sqlLookupEbBody :: Conn -> EndorserBlockHash -> IO (Maybe ByteString)
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
--
-- A tx the volatile partition has not written yet is read from the immutable
-- one, which holds it if the EB was copied, evicted, and is being fetched
-- again.
sqlBatchRetrieveTxs ::
  Conn ->
  EndorserBlockHash ->
  [Int] ->
  IO [(Int, ByteString)]
sqlBatchRetrieveTxs conn ebHash offsets = (evaluate . force =<<) $ do
  vol <- dbWithTransaction volDb $ retrieveFrom volDb volReads offsets
  case vol of
    Nothing -> fromMaybe [] <$> retrieveFrom immDb immReads offsets
    Just txs -> case [o | (o, bytes) <- txs, unwrittenTx bytes] of
      [] -> pure txs
      missing -> do
        imm <- IntMap.fromList . fromMaybe [] <$> retrieveFrom immDb immReads missing
        pure
          [ (o, if unwrittenTx bytes then IntMap.findWithDefault bytes o imm else bytes)
          | (o, bytes) <- txs
          ]
 where
  Conn
    { conVolDb = volDb
    , conImmDb = immDb
    , connVolStmts = VolStmts{stReads = volReads}
    , connImmStmts = ImmStmts{immReads}
    } = conn

  -- 'Nothing' if the body is not stored here.
  retrieveFrom db ReadStmts{rsLookupEbIndex} wantedOffsets =
    lookupEbIndex rsLookupEbIndex ebHash >>= \case
      Nothing -> pure Nothing
      Just ix@EbIndex{eiTxCount} ->
        Just
          <$> readTxBytes
            db
            ix
            [o | o <- IntSet.toAscList (IntSet.fromList wantedOffsets), o >= 0, o < eiTxCount]

-- | The bytes of the EB's txs in body order, zeroes where no tx was written,
-- or 'Nothing' if the body is not stored. Like 'sqlBatchRetrieveTxs', it reads
-- the immutable partition's closure if the volatile one lacks a tx.
sqlLookupEbClosure :: Conn -> EndorserBlockHash -> IO (Maybe [ByteString])
sqlLookupEbClosure conn ebHash = (evaluate . force =<<) $ do
  vol <- dbWithTransaction volDb $ closureFrom volReads ebHash
  case vol of
    Just txs | not (any unwrittenTx txs) -> pure vol
    _ -> maybe vol Just <$> closureFrom immReads ebHash
 where
  Conn
    { conVolDb = volDb
    , connVolStmts = VolStmts{stReads = volReads}
    , connImmStmts = ImmStmts{immReads}
    } = conn

-- * Statements

closureFrom :: ReadStmts -> EndorserBlockHash -> IO (Maybe [ByteString])
closureFrom ReadStmts{rsLookupEbClosure = stmt} ebHash =
  useStmt stmt $ do
    dbBindHash stmt 1 ebHash
    dbStep stmt >>= \case
      DB.Done -> pure Nothing
      DB.Row -> do
        bounds <- DB.columnBlob stmt 0
        txs <-
          DB.columnType stmt 1 >>= \case
            DB.NullColumn -> pure (BS.replicate (boundAt bounds (BS.length bounds `div` 4 - 1)) 0)
            _ -> DB.columnBlob stmt 1
        pure $ Just (splitClosure bounds txs)

lookupBlob :: DB.Statement -> EndorserBlockHash -> IO (Maybe ByteString)
lookupBlob stmt ebHash =
  useStmt stmt $ do
    dbBindHash stmt 1 ebHash
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
lookupEbIndex :: DB.Statement -> EndorserBlockHash -> IO (Maybe EbIndex)
lookupEbIndex stmt ebHash =
  useStmt stmt $ do
    dbBindHash stmt 1 ebHash
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

-- | The bytes of the txs at these offsets, which must be ascending, distinct
-- and in the body, zeroes where no tx was written: one read per run of
-- consecutive offsets.
readTxBytes :: DB.Database -> EbIndex -> [Int] -> IO [(Int, ByteString)]
readTxBytes _ _ [] = pure []
readTxBytes db ix@EbIndex{eiClosureRowid} wanted@(lo : _) = do
  bounds <- readTxBounds db ix lo (last wanted)
  let start o = boundAt bounds (o - lo)
      end o = boundAt bounds (o - lo + 1)
      runs = consecutiveRuns wanted
  concat <$> case eiClosureRowid of
    Nothing ->
      pure [[(o, BS.replicate (end o - start o) 0) | o <- run] | run <- runs]
    Just rowid ->
      withBlob db "ebClosures" "txs" rowid False $ \blob ->
        forM runs $ \run -> do
          let at = start (head run)
          bytes <- withDie db (DB.blobRead blob (end (last run) - at) at)
          pure [(o, BS.take (end o - start o) (BS.drop (start o - at) bytes)) | o <- run]

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
encodeTxBounds :: EndorserBlock -> ByteString
encodeTxBounds eb =
  BSL.toStrict . BB.toLazyByteString $
    foldMap BB.word32BE (scanl (+) 0 [size | (_, size) <- endorserBlockReferences eb])

-- | The @i@th position in (a slice of) 'encodeTxBounds'.
boundAt :: ByteString -> Int -> Int
boundAt bounds i = foldl (\acc k -> acc * 256 + fromIntegral (BS.index bounds (i * 4 + k))) 0 [0 .. 3]
