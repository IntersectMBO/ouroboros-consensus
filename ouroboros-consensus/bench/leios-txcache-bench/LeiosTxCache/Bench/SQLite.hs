{-# LANGUAGE OverloadedStrings #-}

-- | 'LeiosTxCache' handles backed by a dedicated SQLite database, for
-- benchmarking the cache against an on-disk by-hash index. This module is built
-- /only/ as part of the @leios-txcache-bench@ benchmark — never the library —
-- hence the @Bench@ in its name.
--
-- The handles track only tx /presence/ — enough for the lookup path the
-- benchmark measures — not the announcement and eviction bookkeeping the
-- in-memory implementations maintain, so 'insertAnnouncement' and
-- 'evictOlderThan' are inert.
--
-- There are two factories over the same file, because one configuration cannot
-- serve both phases:
--
--   * 'newSQLiteLeiosTxCacheForPopulation' — fast bulk insert.
--   * 'newSQLiteLeiosTxCacheForQueries' — the representative, /coolable/ read
--     configuration.
--
-- The benchmark populates through the first, @fsync@s the file, then reads
-- through the second (a separate connection, so its private page cache starts
-- cold). This is @not@ reusing "LeiosDemoDb.SQLite": its @WAL@ + @mmap@ pin the
-- pages and defeat cooling.
module LeiosTxCache.Bench.SQLite
  ( newSQLiteLeiosTxCacheForPopulation
  , newSQLiteLeiosTxCacheForQueries
  ) where

import Control.Monad (zipWithM_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.SQLite3 as DB
import LeiosDemoTypes (TxHash (..))
import LeiosTxCache.API
  ( LeiosTxCache (..)
  , ReferencesTxsByHash (..)
  )

-- | Fast, unsafe configuration for bulk population: no journal and no @fsync@,
-- with a large private cache so the whole random-key B-tree stays hot in memory
-- (32 kB pages otherwise make each random-key insert re-log a full page — the
-- write amplification that makes the safe configuration unbearably slow to
-- populate). Not crash-safe, which is fine for a benchmark. The caller must
-- @fsync@ the file before reading, so the query connection's pages are clean and
-- hence evictable by @posix_fadvise@.
newSQLiteLeiosTxCacheForPopulation ::
  ReferencesTxsByHash b => FilePath -> IO (LeiosTxCache IO () () b)
newSQLiteLeiosTxCacheForPopulation =
  newSQLiteLeiosTxCacheWith
    [ "PRAGMA page_size = 32768;"
    , "PRAGMA journal_mode = OFF;"
    , "PRAGMA synchronous = OFF;"
    , "PRAGMA temp_store = MEMORY;"
    , "PRAGMA cache_size = -262144;" -- ~256 MB: holds the whole index hot
    , "CREATE TABLE IF NOT EXISTS txs (txHashBytes BLOB PRIMARY KEY);"
    ]

-- | Representative, coolable configuration for the timed lookups. Returns the
-- handle together with a /reopen/ action: to measure a genuinely cold batch, the
-- caller reopens the connection (fresh, empty private cache) and
-- @posix_fadvise@s the file (cold OS cache) before each batch.
--
--   * @mmap_size = 0@ — reads go through @read()@, so @posix_fadvise(DONTNEED)@
--     on the file actually evicts them.
--   * @cache_size@ large enough to hold the batch's working set — so /within/ a
--     batch each index leaf is read once (cold), not once per lookup. The
--     adversary owns the OS page cache (cooled per batch), not SQLite's
--     intra-batch reuse: that is the honest "best SQLite can do cold".
--   * DELETE journal — data in the single main file, no WAL\/-shm (itself an
--     mmap) to cool separately; equivalent to WAL for a cold, disk-bound lookup.
--   * @page_size = 32768@ — matches "LeiosDemoDb.SQLite".
--
-- The batch is resolved by the buffer-and-lie trick: each 'look' records its
-- hash and answers 'Nothing' (only the timing matters), then one @IN@ query runs
-- over the whole buffer. (This variant's reported hit count is thus always 0 —
-- cosmetic.) @cacheSize@ is the @PRAGMA cache_size@ value; @nParams@ the fixed
-- probe length, so the statement is prepared once per connection.
newSQLiteLeiosTxCacheForQueries ::
  Int -> Int -> FilePath -> IO (LeiosTxCache IO () () b, IO ())
newSQLiteLeiosTxCacheForQueries cacheSize nParams path = do
  -- A large @IN (?,..)@ rather than the @json_each(?)@ + @unhex@ form that
  -- LeiosDemoDb.SQLite uses: json_each was measured both slower (parsing the
  -- array and unhex-decoding every element) and far heavier on allocation
  -- (~50 MiB/batch building the hex text vs a few MiB here), and the
  -- SQLITE_MAX_VARIABLE_NUMBER worry that motivated it never bit — the limit is
  -- 32766, well above the ~15k probe.
  let inSql =
        "SELECT count(*) FROM txs WHERE txHashBytes IN ("
          <> T.intercalate "," (replicate nParams "?")
          <> ");"
      openConn = do
        db <- DB.open (fromString path)
        mapM_
          (DB.exec db)
          [ "PRAGMA page_size = 32768;"
          , "PRAGMA journal_mode = DELETE;"
          , "PRAGMA synchronous = NORMAL;"
          , "PRAGMA mmap_size = 0;"
          , fromString ("PRAGMA cache_size = " <> show cacheSize <> ";")
          , "CREATE TABLE IF NOT EXISTS txs (txHashBytes BLOB PRIMARY KEY);"
          ]
        stmt <- DB.prepare db inSql
        pure (db, stmt)
  connRef <- newIORef =<< openConn
  let reopen = do
        (oldDb, oldStmt) <- readIORef connRef
        DB.finalize oldStmt
        DB.close oldDb
        writeIORef connRef =<< openConn
      batchLookup stmt hashes = do
        zipWithM_
          (\i (MkTxHash bs) -> DB.bindBlob stmt (fromIntegral (i :: Int)) bs)
          [1 ..]
          hashes
        _ <- DB.step stmt
        DB.reset stmt
  let handle =
        LeiosTxCache
          { insertAnnouncement = \_slot _rbh _ebh -> pure (Set.empty, Set.empty)
          , evictOlderThan = \_boundary -> pure (Set.empty, Set.empty)
          , insertBody = \_ebh _body _nil _snoc -> pure Nothing
          , lookupBody = \_ebh -> pure Nothing
          , withLockedInsertUnappliedTx = \k -> do _ <- k () (\w _txh _ -> pure w); pure ()
          , withLockedInsertAppliedTx = \k -> do _ <- k () (\w _txh _ -> pure w); pure ()
          , withLookupTx = \k -> do
              (_, stmt) <- readIORef connRef
              buf <- newIORef []
              r <- k (\txh -> modifyIORef' buf (txh :) >> pure Nothing)
              hashes <- readIORef buf
              batchLookup stmt hashes
              pure r
          }
  pure (handle, reopen)

-- | Open a connection at @path@, run @pragmas@ (page_size must precede the first
-- table for it to take on a fresh file), and build the handle. Only @a = v = ()@
-- is meaningful; a present tx reads back as @Just (Left ())@, an absent one as
-- 'Nothing'.
newSQLiteLeiosTxCacheWith ::
  ReferencesTxsByHash b => [Text] -> FilePath -> IO (LeiosTxCache IO () () b)
newSQLiteLeiosTxCacheWith pragmas path = do
  db <- DB.open (fromString path)
  mapM_ (DB.exec db) pragmas
  insertStmt <- DB.prepare db "INSERT OR IGNORE INTO txs (txHashBytes) VALUES (?);"
  lookupStmt <- DB.prepare db "SELECT 1 FROM txs WHERE txHashBytes = ? LIMIT 1;"
  let insertOne (MkTxHash bs) = do
        DB.bindBlob insertStmt 1 bs
        _ <- DB.step insertStmt
        DB.reset insertStmt
      lookupOne (MkTxHash bs) = do
        DB.bindBlob lookupStmt 1 bs
        r <- DB.step lookupStmt
        DB.reset lookupStmt
        pure $ case r of
          DB.Row -> Just (Left ())
          DB.Done -> Nothing
  pure
    LeiosTxCache
      { insertAnnouncement = \_slot _rbh _ebh -> pure (Set.empty, Set.empty)
      , evictOlderThan = \_boundary -> pure (Set.empty, Set.empty)
      , insertBody = \_ebh body _nil _snoc -> do
          DB.exec db "BEGIN;"
          mapM_ insertOne (foldTxReferences (\acc txh _sz -> txh : acc) [] body)
          DB.exec db "COMMIT;"
          pure Nothing
      , lookupBody = \_ebh -> pure Nothing
      , withLockedInsertUnappliedTx = \k -> do _ <- k () (\w _txh _ -> pure w); pure ()
      , withLockedInsertAppliedTx = \k -> do _ <- k () (\w _txh _ -> pure w); pure ()
      , withLookupTx = \k -> k lookupOne
      }
