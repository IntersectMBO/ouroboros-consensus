-- | The schema of the LeiosDB partition files.
--
-- The immutable partition holds a subset of the volatile one: the same
-- tables with fewer 'ebs' columns, and none of the volatile-only tables and
-- indexes.
--
-- Every statement is @IF NOT EXISTS@, so the schemas are idempotent:
-- @initialiseLeiosDbFiles@ applies them on every handle creation.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Schema
  ( sql_schema_vol
  , sql_schema_imm
  ) where

-- | Schema of the volatile partition (@leios.vol.db@): 'sql_schema_imm' with
-- the columns and objects that track completeness and GC.
sql_schema_vol :: String
sql_schema_vol =
  unlines
    ( ebsTable
        [ -- NULL = body not downloaded, >0 = txs missing, 0 = just completed, <0 = notified
          "  missingTxCount INTEGER,"
        , -- 0 = volatile, 1 = certified/pinned awaiting copy,
          -- 2 = copied to the immutable partition (evictable),
          -- 3 = marked for GC, awaiting the sweeper
          "  status INTEGER NOT NULL DEFAULT 0,"
        ]
        <> sharedTables
    )
    <> sql_schema_gc

-- | GC-only objects of the volatile partition, part of 'sql_schema_vol'.
sql_schema_gc :: String
sql_schema_gc =
  unlines
    [ -- What the mark scan reads; marking removes the row from it, so
      -- each row is marked at most once.
      "CREATE INDEX IF NOT EXISTS idx_ebs_sweepable ON ebs(ebSlot) WHERE status IN (0, 2);"
    , -- What the sweeper's batch pick reads.
      "CREATE INDEX IF NOT EXISTS idx_ebs_markedForGc ON ebs(ebHashBytes) WHERE status = 3;"
    , -- Pinned EBs awaiting the copy into the immutable partition; see
      -- 'sql_next_pinned_eb'.
      "CREATE INDEX IF NOT EXISTS idx_ebs_pinned ON ebs(ebSlot) WHERE status = 1;"
    ]

-- | Schema of the immutable partition (@leios.imm.db@): the subset of
-- 'sql_schema_vol' that the copier writes and the fallback reads use. The
-- tables it keeps have the volatile columns, so the fallback reads reuse the
-- volatile SQL verbatim and the copy is a server-side @INSERT ... SELECT@
-- over ATTACH.
--
-- Left out, since EBs land here complete and are never collected:
-- @ebs.missingTxCount@, @ebs.status@ and the GC indexes. Files created
-- before the split still have some of them; nothing reads them.
sql_schema_imm :: String
sql_schema_imm = unlines $ ebsTable [] <> sharedTables

-- | The 'ebs' table and its hash index, with the given columns added after
-- the ones both partitions have.
--
-- The volatile columns go in here rather than through a later
-- @ALTER TABLE ... ADD COLUMN@: that has no @IF NOT EXISTS@, so it would
-- fail the second time the schema is applied.
ebsTable :: [String] -> [String]
ebsTable extraColumns =
  [ "CREATE TABLE IF NOT EXISTS ebs ("
  , "  ebSlot INTEGER NOT NULL,"
  , "  ebHashBytes BLOB NOT NULL,"
  , "  ebBytesSize INTEGER NOT NULL,"
  ]
    <> extraColumns
    <> [ "  PRIMARY KEY (ebSlot, ebHashBytes)"
       , ");"
       , -- Lookups by hash alone, e.g. 'sql_imm_filter_present'.
         "CREATE INDEX IF NOT EXISTS idx_ebs_ebHashBytes ON ebs(ebHashBytes);"
       ]

-- | The tables that are identical in both partitions.
--
-- Tx bytes are owned by the referencing EB: 'ebTxBytes' shares the
-- @(ebHashBytes, txOffset)@ key with 'ebTxs', so writing and evicting an EB's
-- closure touches one contiguous key range, and a tx shared by two EBs is
-- stored twice, deliberately. A row is allocated as a @zeroblob@ of the
-- declared size when the body arrives and filled in place when the tx does
-- (@filled = 1@); it never changes size, so pages never split.
sharedTables :: [String]
sharedTables =
  [ "CREATE TABLE IF NOT EXISTS ebTxs ("
  , "  ebHashBytes BLOB NOT NULL,"
  , "  txOffset INTEGER NOT NULL,"
  , "  txHashBytes BLOB NOT NULL,"
  , "  txBytesSize INTEGER NOT NULL,"
  , "  PRIMARY KEY (ebHashBytes, txOffset)"
  , ");"
  , "CREATE TABLE IF NOT EXISTS ebTxBytes ("
  , "  ebHashBytes BLOB NOT NULL,"
  , "  txOffset INTEGER NOT NULL,"
  , -- Before the blob, so probing it never touches overflow pages.
    "  filled INTEGER NOT NULL DEFAULT 0,"
  , "  txBytes BLOB NOT NULL,"
  , "  PRIMARY KEY (ebHashBytes, txOffset)"
  , ");"
  ]
