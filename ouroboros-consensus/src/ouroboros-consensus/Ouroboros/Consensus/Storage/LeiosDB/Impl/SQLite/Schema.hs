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
-- the columns and objects that track GC.
sql_schema_vol :: String
sql_schema_vol =
  unlines
    ( ebsTable
        [ -- 0 = volatile, 1 = certified/pinned awaiting copy,
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
-- @ebs.status@ and the GC indexes.
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
       , -- Lookups by hash alone, e.g. 'sql_pin_eb'.
         "CREATE INDEX IF NOT EXISTS idx_ebs_ebHashBytes ON ebs(ebHashBytes);"
       ]

-- | The tables that are identical in both partitions.
--
-- An EB's body is stored as received, in its wire encoding, to serve it as
-- is, next to 'txBounds': where each of its txs is in the closure
-- ('Read.encodeTxBounds'). The closure is one row, its txs' bytes
-- concatenated. The first tx write allocates it with zeroes, and every write
-- fills its txs in place.
sharedTables :: [String]
sharedTables =
  [ "CREATE TABLE IF NOT EXISTS ebBodies ("
  , "  ebHashBytes BLOB NOT NULL PRIMARY KEY,"
  , "  txBounds BLOB NOT NULL,"
  , "  body BLOB NOT NULL"
  , ");"
  , "CREATE TABLE IF NOT EXISTS ebClosures ("
  , "  ebHashBytes BLOB NOT NULL PRIMARY KEY,"
  , "  txs BLOB NOT NULL"
  , ");"
  ]
