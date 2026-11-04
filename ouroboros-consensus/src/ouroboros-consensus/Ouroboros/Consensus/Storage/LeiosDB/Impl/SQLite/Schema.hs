-- | The schema of the LeiosDB partition files.
--
-- Both partitions have the same 'ebs' table.
--
-- Every statement is @IF NOT EXISTS@, so the schemas are idempotent:
-- @initialiseLeiosDbFiles@ applies them on every handle creation.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Schema
  ( sql_schema_vol
  , sql_schema_imm
  ) where

-- | Schema of the volatile partition (@leios.vol.db@).
sql_schema_vol :: String
sql_schema_vol = unlines ebsTable

-- | Schema of the immutable partition (@leios.imm.db@). EBs land here complete
-- and are never collected.
sql_schema_imm :: String
sql_schema_imm = unlines ebsTable

-- | One row per announced point, holding the whole EB: its body as received,
-- where each of its txs is in the closure ('Read.encodeTxBounds'), and the
-- closure, its txs' bytes concatenated. The body's arrival allocates the
-- closure with zeroes, and every write fills its txs in place.
--
-- An EB announced at several points has a row at each, with the same body and
-- closure, so collecting a point never touches another's row.
--
-- Columns go from small to large, as reading one walks the overflow pages of
-- every column before it. Nothing updates a row once its body is written, but
-- through the blob API, as an update reads, and may rewrite, the whole row.
--
-- The point is the key, which also serves GC's scan by slot. The blob API
-- opens a row by its implicit rowid.
ebsTable :: [String]
ebsTable =
  [ "CREATE TABLE IF NOT EXISTS ebs ("
  , "  ebSlot INTEGER NOT NULL,"
  , "  ebHashBytes BLOB NOT NULL,"
  , "  ebBytesSize INTEGER NOT NULL,"
  , "  txCount INTEGER,"
  , "  txBounds BLOB,"
  , "  body BLOB,"
  , "  txs BLOB,"
  , "  PRIMARY KEY (ebSlot, ebHashBytes)"
  , ");"
  , -- Lookups by hash alone, e.g. serving.
    "CREATE INDEX IF NOT EXISTS idx_ebs_ebHashBytes ON ebs(ebHashBytes);"
  ]
