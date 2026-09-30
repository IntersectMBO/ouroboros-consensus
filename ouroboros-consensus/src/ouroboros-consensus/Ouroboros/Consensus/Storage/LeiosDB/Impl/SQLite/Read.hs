{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Read operations of LeiosDB's SQLite backend.
--
-- Each lookup asks the volatile
-- partition first and falls back to the immutable one for EBs that were
-- copied evicted.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Read
  ( sqlScanEbPoints
  , sqlScanCompleteEbPointsSince
  , sqlLookupEbBody
  , sqlBatchRetrieveTxs
  , sqlLookupEbClosure
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import Data.ByteString (ByteString)
import qualified Data.Set as Set
import qualified Database.SQLite3.Direct as DB
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , EbHash (..)
  , LeiosPoint (..)
  , TxHash (..)
  )
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Connection (Conn (..))
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Primitives
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Queries
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
        hash <- MkEbHash <$> DB.columnBlob stmt 1
        loop ((slot, hash) : acc)

sqlScanCompleteEbPointsSince :: Conn -> SlotNo -> IO [LeiosPoint]
sqlScanCompleteEbPointsSince conn sinceSlot = do
  (volComplete, recent) <-
    dbWithTransaction db $ do
      volComplete <- useStmt stmt $ do
        dbBindInt64 stmt 1 slot
        accPointWith stmt []
      -- Every recent hash, with or without completeness evidence, from the
      -- same snapshot.
      recent <- withStmt db sql_scan_recent_ebs $ \recentStmt -> do
        dbBindInt64 recentStmt 1 slot
        accPointWith recentStmt []
      pure (volComplete, recent)
  -- A recent hash without volatile completeness evidence may be a copied EB
  -- whose closure rows were evicted (its recent announcement never got a body
  -- insert). Presence in the immutable partition is proof of completeness:
  -- copies are atomic and only complete EBs are copied. Without this probe, a
  -- cert-RB parked across a restart would stay parked forever.
  let volCompleteSet = Set.fromList [ebHashBytes (pointEbHash p) | p <- volComplete]
      unknown = [p | p <- recent, ebHashBytes (pointEbHash p) `Set.notMember` volCompleteSet]
  if null unknown
    then pure volComplete
    else do
      present <- immFilterPresent conn [ebHashBytes (pointEbHash p) | p <- unknown]
      let presentSet = Set.fromList present
      pure $
        volComplete
          <> [p | p <- unknown, ebHashBytes (pointEbHash p) `Set.member` presentSet]
 where
  slot = fromIntegral $ unSlotNo sinceSlot
  Conn{conVolDb = db, connVolStmts = VolStmts{stScanCompleteEbsSince = stmt}} = conn

  accPointWith :: DB.Statement -> [LeiosPoint] -> IO [LeiosPoint]
  accPointWith pointsStmt acc =
    dbStep pointsStmt >>= \case
      DB.Done -> pure (reverse acc)
      DB.Row -> do
        ebSlot <- SlotNo . fromIntegral <$> DB.columnInt64 pointsStmt 0
        hash <- MkEbHash <$> DB.columnBlob pointsStmt 1
        accPointWith pointsStmt (MkLeiosPoint ebSlot hash : acc)

-- | Which of the given EB hashes the immutable partition holds.
immFilterPresent :: Conn -> [ByteString] -> IO [ByteString]
immFilterPresent conn hashes =
  useStmt stmt $ do
    dbBindUtf8 stmt 1 (jsonHexArray hashes)
    let loop acc =
          dbStep stmt >>= \case
            DB.Done -> pure (reverse acc)
            DB.Row -> do
              hashBytes <- DB.columnBlob stmt 0
              loop (hashBytes : acc)
    loop []
 where
  Conn{connImmStmts = ImmStmts{immStFilterPresent = stmt}} = conn

sqlLookupEbBody :: Conn -> EbHash -> IO [(TxHash, BytesSize)]
sqlLookupEbBody conn ebHash = do
  vol <-
    dbWithTransaction db $ useStmt stmt $ do
      dbBindBlob stmt 1 (let MkEbHash bytes = ebHash in bytes)
      bodyLoop stmt []
  -- Bodies insert atomically, so the empty list is a complete miss: the EB
  -- may have been copied to the immutable partition and evicted.
  if null vol then immLookupEbBody conn ebHash else pure vol
 where
  Conn{conVolDb = db, connVolStmts = VolStmts{stLookupEbBody = stmt}} = conn

-- | Immutable-partition fallback of 'sqlLookupEbBody'.
immLookupEbBody :: Conn -> EbHash -> IO [(TxHash, BytesSize)]
immLookupEbBody conn ebHash =
  useStmt stmt $ do
    dbBindBlob stmt 1 (let MkEbHash bytes = ebHash in bytes)
    bodyLoop stmt []
 where
  Conn{connImmStmts = ImmStmts{immStLookupEbBody = stmt}} = conn

bodyLoop :: DB.Statement -> [(TxHash, BytesSize)] -> IO [(TxHash, BytesSize)]
bodyLoop stmt acc =
  dbStep stmt >>= \case
    DB.Done -> pure (reverse acc)
    DB.Row -> do
      txHash <- MkTxHash <$> DB.columnBlob stmt 0
      size <- fromIntegral <$> DB.columnInt64 stmt 1
      bodyLoop stmt ((txHash, size) : acc)

-- | Retrieve tx bytes for a batch of @(ebHash, txOffset)@ points. Passes
-- the offsets list as a JSON int array bound to a single parameter;
-- SQLite's 'json_each' virtual table joins it against 'ebTxs' + 'txs'.
--
-- No temp tables, no attached databases, no per-item INSERT round-trips.
-- Works on strictly read-only connections.
sqlBatchRetrieveTxs ::
  Conn ->
  EbHash ->
  [Int] ->
  IO [(Int, TxHash, Maybe ByteString)]
sqlBatchRetrieveTxs conn ebHash offsets = do
  vol <-
    dbWithTransaction db $ useStmt stmt $ do
      dbBindBlob stmt 1 (let MkEbHash bytes = ebHash in bytes)
      dbBindUtf8 stmt 2 (jsonIntArray offsets)
      retrieveLoop stmt []
  -- Zero rows means the EB's body is absent from the volatile partition
  -- entirely (a present body joins every requested offset): copied+evicted.
  if null vol && not (null offsets)
    then immBatchRetrieveTxs conn ebHash offsets
    else pure vol
 where
  Conn{conVolDb = db, connVolStmts = VolStmts{stBatchRetrieveTxs = stmt}} = conn

-- | Immutable-partition fallback of 'sqlBatchRetrieveTxs'. Closures land
-- there whole, so the joined tx bytes are never NULL.
immBatchRetrieveTxs ::
  Conn -> EbHash -> [Int] -> IO [(Int, TxHash, Maybe ByteString)]
immBatchRetrieveTxs conn ebHash offsets =
  useStmt stmt $ do
    dbBindBlob stmt 1 (let MkEbHash bytes = ebHash in bytes)
    dbBindUtf8 stmt 2 (jsonIntArray offsets)
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
      txHash <- MkTxHash <$> DB.columnBlob stmt 1
      -- Column 2 is from LEFT JOIN, NULL if tx not in txs table
      txBytes <- DB.columnBlob stmt 2
      let mbTxBytes = if txBytes == mempty then Nothing else Just txBytes
      retrieveLoop stmt ((offset, txHash, mbTxBytes) : acc)

sqlLookupEbClosure :: Conn -> EbHash -> IO (Maybe [(TxHash, ByteString)])
sqlLookupEbClosure conn ebHash = do
  vol <-
    dbWithTransaction db $ useStmt stmt $ do
      dbBindBlob stmt 1 (ebHashBytes ebHash)
      -- FIXME(bladyjoker): This should have a SlotNo as the second part of the key
      closureLoop stmt []
  -- 'Nothing' covers both no-body and any-tx-missing, which includes a copied
  -- EB re-announced and mid-refetch: the immutable partition must still
  -- answer for it, or replaying its cert-RB fails.
  case vol of
    Just rows -> pure (Just rows)
    Nothing -> immLookupEbClosure conn ebHash
 where
  Conn{conVolDb = db, connVolStmts = VolStmts{stLookupEbClosure = stmt}} = conn

-- | Immutable-partition fallback of 'sqlLookupEbClosure'. Closures land there
-- atomically and whole, so any rows are all the rows.
immLookupEbClosure :: Conn -> EbHash -> IO (Maybe [(TxHash, ByteString)])
immLookupEbClosure conn ebHash =
  useStmt stmt $ do
    dbBindBlob stmt 1 (ebHashBytes ebHash)
    closureLoop stmt []
 where
  Conn{connImmStmts = ImmStmts{immStLookupEbClosure = stmt}} = conn

closureLoop ::
  DB.Statement -> [(TxHash, ByteString)] -> IO (Maybe [(TxHash, ByteString)])
closureLoop stmt acc =
  dbStep stmt >>= \case
    DB.Done ->
      -- No rows means the EB body hasn't been downloaded yet
      if null acc then pure Nothing else pure $ Just (reverse acc)
    DB.Row -> do
      txHash <- MkTxHash <$> DB.columnBlob stmt 0
      txBytes :: ByteString <- DB.columnBlob stmt 1
      if txBytes == mempty
        then return Nothing
        else closureLoop stmt ((txHash, txBytes) : acc)
