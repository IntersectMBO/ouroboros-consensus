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
        <> [ -- What 'sql_lookup_stored_eb_size' seeks on. Partial, so the rows that
             -- name a hash without having fetched its body are never visited: an
             -- attacker announcing one hash in every slot it wins makes them as
             -- numerous as its elections in the live window, and every later
             -- announcement of that hash would otherwise visit them all again.
             "CREATE INDEX IF NOT EXISTS idx_ebs_acquired ON ebs(ebHashBytes) WHERE missingTxCount IS NOT NULL;"
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
-- @ebs.missingTxCount@, @ebs.status@ and the indexes on them.
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
  , -- the body's actual size once this point has acquired it (see
    -- 'sql_lookup_stored_eb_size'), and the first size it was announced at
    -- before that
    "  ebBytesSize INTEGER NOT NULL,"
  ]
    <> extraColumns
    <> [ "  PRIMARY KEY (ebSlot, ebHashBytes)"
       , ");"
       , -- Lookups by hash alone, e.g. 'sql_imm_filter_present'.
         "CREATE INDEX IF NOT EXISTS idx_ebs_ebHashBytes ON ebs(ebHashBytes);"
       ]

-- | The tables that are identical in both partitions.
sharedTables :: [String]
sharedTables =
  [ -- The body: one row per referenced tx, written in one pass when the body
    -- arrives. 'txHashBytes' is a payload here, not a key -- nothing indexes
    -- by tx hash, so the insert is sequential within the EB. Optimized for
    -- the worst case: an adversary shares no txs between EBs, so a shared
    -- store only ever saved bytes an adversary declines to share, while its
    -- hash-scattered writes cost ~1 dirtied page per row on every EB
    -- (measured 79% of the body write) and scale with a tx count the EB
    -- producer chooses.
    "CREATE TABLE IF NOT EXISTS ebTxs ("
  , "  ebHashBytes BLOB NOT NULL,"
  , "  txOffset INTEGER NOT NULL,"
  , "  txHashBytes BLOB NOT NULL,"
  , "  txBytesSize INTEGER NOT NULL,"
  , "  PRIMARY KEY (ebHashBytes, txOffset)"
  , ");"
  , -- The closure: tx bytes owned by the referencing EB, same key as its
    -- 'ebTxs' row. A tx shared by two EBs is stored twice; in exchange
    -- writes are sequential, completion is counting, and eviction is a
    -- range delete -- no orphan concept, no tx-hash index.
    --
    -- Every row is allocated at body-arrival time as @zeroblob@ of the
    -- declared size, in one offset-ordered pass, so the range's pages are
    -- physically contiguous and fully packed. A closure is written once but
    -- served many times: peers fetch it as offset ranges, voting reads it
    -- whole, the copier streams it -- all sequential reads over that
    -- layout. Arrivals then overwrite rows in place ('filled' 0 -> 1),
    -- which cannot split pages because the size cannot change.
    --
    -- 'filled' precedes the blob so probing it never touches overflow pages.
    "CREATE TABLE IF NOT EXISTS ebTxBytes ("
  , "  ebHashBytes BLOB NOT NULL,"
  , "  txOffset INTEGER NOT NULL,"
  , "  filled INTEGER NOT NULL DEFAULT 0,"
  , "  txBytes BLOB NOT NULL,"
  , "  PRIMARY KEY (ebHashBytes, txOffset)"
  , ");"
  ]
