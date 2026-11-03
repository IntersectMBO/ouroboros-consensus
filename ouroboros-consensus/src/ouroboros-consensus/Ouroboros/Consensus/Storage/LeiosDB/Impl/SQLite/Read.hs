{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Read operations of LeiosDB's SQLite backend.
--
-- Each lookup asks the volatile partition first and falls back to the
-- immutable one for points that were copied and then evicted.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Read
  ( sqlScanEbPoints
  , sqlScanCompleteEbPointsSince
  , sqlLookupEbBody
  , sqlBatchRetrieveTxs
  , sqlLookupEbClosure
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import Data.ByteString (ByteString)
import qualified Database.SQLite3.Direct as DB
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EndorserBlockHash
  , LeiosPoint (..)
  , TxHash
  )
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

sqlScanCompleteEbPointsSince :: Conn -> SlotNo -> IO [LeiosPoint]
sqlScanCompleteEbPointsSince conn sinceSlot =
  dbWithTransaction db $ useStmt stmt $ do
    dbBindInt64 stmt 1 (fromIntegral $ unSlotNo sinceSlot)
    loop []
 where
  Conn{conVolDb = db, connVolStmts = VolStmts{stScanCompleteEbsSince = stmt}} = conn
  loop acc =
    dbStep stmt >>= \case
      DB.Done -> pure (reverse acc)
      DB.Row -> do
        point <- dbColumnPoint stmt 0
        loop (point : acc)

sqlLookupEbBody :: Conn -> LeiosPoint -> IO [(TxHash, BytesSize)]
sqlLookupEbBody conn point = do
  vol <-
    dbWithTransaction db $ useStmt stmt $ do
      dbBindPoint stmt point
      bodyLoop stmt []
  -- Bodies insert atomically, so the empty list is a complete miss: the point
  -- may have been copied to the immutable partition and evicted.
  if null vol then immLookupEbBody conn point else pure vol
 where
  Conn{conVolDb = db, connVolStmts = VolStmts{stLookupEbBody = stmt}} = conn

-- | Immutable-partition fallback of 'sqlLookupEbBody'.
immLookupEbBody :: Conn -> LeiosPoint -> IO [(TxHash, BytesSize)]
immLookupEbBody conn point =
  useStmt stmt $ do
    dbBindPoint stmt point
    bodyLoop stmt []
 where
  Conn{connImmStmts = ImmStmts{immStLookupEbBody = stmt}} = conn

bodyLoop :: DB.Statement -> [(TxHash, BytesSize)] -> IO [(TxHash, BytesSize)]
bodyLoop stmt acc =
  dbStep stmt >>= \case
    DB.Done -> pure (reverse acc)
    DB.Row -> do
      txHash <- dbColumnHash stmt 0
      size <- fromIntegral <$> DB.columnInt64 stmt 1
      bodyLoop stmt ((txHash, size) : acc)

-- | Retrieve tx bytes for a batch of offsets into one point's body. Passes
-- the offsets list as a JSON int array bound to a single parameter;
-- SQLite's 'json_each' virtual table joins it against 'ebTxs' + 'ebTxBytes'.
--
-- No temp tables, no attached databases, no per-item INSERT round-trips.
-- Works on strictly read-only connections.
sqlBatchRetrieveTxs ::
  Conn ->
  LeiosPoint ->
  [Int] ->
  IO [(Int, TxHash, Maybe ByteString)]
sqlBatchRetrieveTxs conn point offsets = do
  vol <-
    dbWithTransaction db $ useStmt stmt $ do
      dbBindPoint stmt point
      dbBindUtf8 stmt 3 (jsonIntArray offsets)
      retrieveLoop stmt []
  -- Zero rows means the point's body is absent from the volatile partition
  -- entirely (a present body joins every requested offset): copied and
  -- evicted.
  if null vol && not (null offsets)
    then immBatchRetrieveTxs conn point offsets
    else pure vol
 where
  Conn{conVolDb = db, connVolStmts = VolStmts{stBatchRetrieveTxs = stmt}} = conn

-- | Immutable-partition fallback of 'sqlBatchRetrieveTxs'. Closures land
-- there whole, so the joined tx bytes are never NULL.
immBatchRetrieveTxs ::
  Conn -> LeiosPoint -> [Int] -> IO [(Int, TxHash, Maybe ByteString)]
immBatchRetrieveTxs conn point offsets =
  useStmt stmt $ do
    dbBindPoint stmt point
    dbBindUtf8 stmt 3 (jsonIntArray offsets)
    retrieveLoop stmt []
 where
  Conn{connImmStmts = ImmStmts{immStBatchRetrieveTxs = stmt}} = conn

retrieveLoop ::
  DB.Statement ->
  [(Int, TxHash, Maybe ByteString)] ->
  IO [(Int, TxHash, Maybe ByteString)]
retrieveLoop stmt acc =
  dbStep stmt >>= \case
    DB.Done -> pure (reverse acc)
    DB.Row -> do
      offset <- fromIntegral <$> DB.columnInt64 stmt 0
      txHash <- dbColumnHash stmt 1
      -- Column 2 is NULL if the tx's bytes have not arrived yet
      txBytes <- DB.columnBlob stmt 2
      let mbTxBytes = if txBytes == mempty then Nothing else Just txBytes
      retrieveLoop stmt ((offset, txHash, mbTxBytes) : acc)

sqlLookupEbClosure :: Conn -> LeiosPoint -> IO (Maybe [(TxHash, ByteString)])
sqlLookupEbClosure conn point = do
  vol <-
    dbWithTransaction db $ useStmt stmt $ do
      dbBindPoint stmt point
      closureLoop stmt []
  -- 'Nothing' covers both no-body and any-tx-missing; a point copied and then
  -- evicted is answered by the immutable partition.
  case vol of
    Just rows -> pure (Just rows)
    Nothing -> immLookupEbClosure conn point
 where
  Conn{conVolDb = db, connVolStmts = VolStmts{stLookupEbClosure = stmt}} = conn

-- | Immutable-partition fallback of 'sqlLookupEbClosure'. Closures land there
-- atomically and whole, so any rows are all the rows.
immLookupEbClosure :: Conn -> LeiosPoint -> IO (Maybe [(TxHash, ByteString)])
immLookupEbClosure conn point =
  useStmt stmt $ do
    dbBindPoint stmt point
    closureLoop stmt []
 where
  Conn{connImmStmts = ImmStmts{immStLookupEbClosure = stmt}} = conn

closureLoop ::
  DB.Statement -> [(TxHash, ByteString)] -> IO (Maybe [(TxHash, ByteString)])
closureLoop stmt acc =
  dbStep stmt >>= \case
    DB.Done ->
      -- No rows means the body hasn't been written at this point
      if null acc then pure Nothing else pure $ Just (reverse acc)
    DB.Row -> do
      txHash <- dbColumnHash stmt 0
      txBytes :: ByteString <- DB.columnBlob stmt 1
      if txBytes == mempty
        then return Nothing
        else closureLoop stmt ((txHash, txBytes) : acc)
