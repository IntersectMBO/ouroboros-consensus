-- | The schema of the LeiosDB partition files.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Schema
  ( sql_schema
  , sql_schema_gc
  ) where

-- | Schema of both partitions (@leios.vol.db@ and @leios.imm.db@): identical
-- on purpose, so the fallback reads reuse the volatile SQL verbatim and the
-- copy is a server-side @INSERT ... SELECT@ over ATTACH. In the immutable
-- file 'missingTxCount', @status@ and @ebsMissingTxs@ are unused (rows land
-- complete, with the canonical @missingTxCount = -1, status = 2@).
-- Idempotent: @initialiseLeiosDbFiles@ applies it on every handle creation.
sql_schema :: String
sql_schema =
  unlines
    [ "CREATE TABLE IF NOT EXISTS ebs ("
    , "  ebSlot INTEGER NOT NULL,"
    , "  ebHashBytes BLOB NOT NULL,"
    , "  ebBytesSize INTEGER NOT NULL,"
    , -- NULL = body not downloaded, >0 = txs missing, 0 = just completed, <0 = notified
      "  missingTxCount INTEGER,"
    , -- 0 = volatile, 1 = certified/pinned awaiting copy,
      -- 2 = copied to the immutable partition (evictable),
      -- 3 = marked for GC, awaiting the sweeper
      "  status INTEGER NOT NULL DEFAULT 0,"
    , "  PRIMARY KEY (ebSlot, ebHashBytes)"
    , ");"
    , "CREATE INDEX IF NOT EXISTS idx_ebs_ebHashBytes ON ebs(ebHashBytes);"
    , "CREATE TABLE IF NOT EXISTS ebTxs ("
    , "  ebHashBytes BLOB NOT NULL,"
    , "  txOffset INTEGER NOT NULL,"
    , "  txHashBytes BLOB NOT NULL,"
    , "  txBytesSize INTEGER NOT NULL,"
    , "  PRIMARY KEY (ebHashBytes, txOffset)"
    , ");"
    , -- This index speeds up tx -> EB lookups, which is necessary for GCing orphaned transactions
      -- after their EB was GCed.
      "CREATE INDEX IF NOT EXISTS idx_ebTxs_txHashBytes ON ebTxs(txHashBytes);"
    , "CREATE TABLE IF NOT EXISTS ebsMissingTxs ("
    , "  txHashBytes BLOB NOT NULL,"
    , "  ebHashBytes BLOB NOT NULL,"
    , "  PRIMARY KEY (txHashBytes, ebHashBytes)"
    , ");"
    , "CREATE INDEX IF NOT EXISTS idx_ebsMissingTxs_ebHashBytes ON ebsMissingTxs(ebHashBytes);"
    , "CREATE TABLE IF NOT EXISTS txs ("
    , "  txHashBytes BLOB NOT NULL PRIMARY KEY,"
    , "  txBytes BLOB NOT NULL,"
    , "  txBytesSize INTEGER NOT NULL"
    , ");"
    ]

-- | GC-only objects of the volatile partition, applied idempotently by
-- @initialiseLeiosDbFiles@, so pre-existing files migrate on the next handle
-- creation. Deliberately not part of 'sql_schema': in the immutable
-- partition every row has @status = 2@, so @idx_ebs_sweepable@ there would
-- index the whole table for nothing.
sql_schema_gc :: String
sql_schema_gc =
  unlines
    [ -- Persistent orphan-tx hints: txs of GC-marked EBs, deleted only once
      -- provably unreferenced ('sql_sweep_orphan_txs'). Survives restarts
      -- together with the status = 3 marks.
      "CREATE TABLE IF NOT EXISTS gcTxCandidates (txHashBytes BLOB NOT NULL PRIMARY KEY);"
    , -- What the mark scan reads; marking removes the row from it, so
      -- each row is marked at most once.
      "CREATE INDEX IF NOT EXISTS idx_ebs_sweepable ON ebs(ebSlot) WHERE status IN (0, 2);"
    , -- What the sweeper's batch pick reads.
      "CREATE INDEX IF NOT EXISTS idx_ebs_markedForGc ON ebs(ebHashBytes) WHERE status = 3;"
    , -- Pinned EBs awaiting the copy into the immutable partition; see
      -- 'sql_next_pinned_eb'.
      "CREATE INDEX IF NOT EXISTS idx_ebs_pinned ON ebs(ebSlot) WHERE status = 1;"
    ]
