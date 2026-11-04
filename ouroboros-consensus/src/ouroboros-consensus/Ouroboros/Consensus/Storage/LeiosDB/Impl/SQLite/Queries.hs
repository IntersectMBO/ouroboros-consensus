-- | The SQL text of every statement the SQLite-backed LeiosDB runs. The
-- schema they run against is in
-- "Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Schema".
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Queries
  ( sql_scan_ebs
  , sql_scan_ebs_since
  , sql_insert_eb
  , sql_insert_eb_copy
  , sql_has_point
  , sql_insert_eb_body
  , sql_lookup_eb_body
  , sql_lookup_eb_index
  , sql_lookup_eb_rows
  , sql_lookup_eb_closure
  , sql_pin_eb
  , sql_mark_as_copied
  , sql_copy_closure
  , sql_copy_insert_eb
  , sql_next_pinned_eb
  , sql_gc_watermark
  , sql_gc_sweep
  , sql_gc_frontier_empty
  ) where

sql_scan_ebs :: String
sql_scan_ebs =
  "SELECT ebSlot, ebHashBytes\n\
  \FROM ebs\n\
  \ORDER BY ebSlot ASC\n\
  \"

-- | The points announced at or after slot @?1@.
sql_scan_ebs_since :: String
sql_scan_ebs_since =
  "SELECT ebSlot, ebHashBytes FROM ebs WHERE ebSlot >= ?1"

-- | Insert a point without its EB.
--
-- Parameters: 1 = ebSlot, 2 = ebHashBytes, 3 = ebBytesSize
sql_insert_eb :: String
sql_insert_eb =
  "INSERT OR IGNORE INTO ebs (ebSlot, ebHashBytes, ebBytesSize) VALUES (?1, ?2, ?3)"

-- | Insert a point with the EB another point of it holds, if any.
--
-- Parameters: as 'sql_insert_eb'
sql_insert_eb_copy :: String
sql_insert_eb_copy =
  "INSERT OR IGNORE INTO ebs (ebSlot, ebHashBytes, ebBytesSize, txCount, txBounds, body, txs)\n\
  \SELECT ?1, ?2, ?3, txCount, txBounds, body, txs FROM ebs\n\
  \WHERE ebHashBytes = ?2 AND txCount IS NOT NULL LIMIT 1\n\
  \"

-- | Whether the point was written.
--
-- Parameters: 1 = ebSlot, 2 = ebHashBytes
sql_has_point :: String
sql_has_point =
  "SELECT 1 FROM ebs WHERE ebSlot = ?1 AND ebHashBytes = ?2"

-- | Write an EB's body into each of its points, with a closure of zeroes. A
-- second delivery of the same body changes nothing.
--
-- Parameters: 1 = ebHashBytes, 2 = txCount, 3 = txBounds, 4 = body, 5 =
-- closure size
sql_insert_eb_body :: String
sql_insert_eb_body =
  "UPDATE ebs SET txCount = ?2, txBounds = ?3, body = ?4, txs = zeroblob(?5)\n\
  \WHERE ebHashBytes = ?1 AND txCount IS NULL\n\
  \"

-- | An EB's body, as received.
sql_lookup_eb_body :: String
sql_lookup_eb_body =
  "SELECT body FROM ebs WHERE ebHashBytes = ?1 AND txCount IS NOT NULL LIMIT 1"

-- | A row holding the EB, and how many txs it lists.
sql_lookup_eb_index :: String
sql_lookup_eb_index =
  "SELECT rowid, txCount FROM ebs WHERE ebHashBytes = ?1 AND txCount IS NOT NULL LIMIT 1"

-- | Every row holding the EB.
sql_lookup_eb_rows :: String
sql_lookup_eb_rows =
  "SELECT rowid FROM ebs WHERE ebHashBytes = ?1 AND txCount IS NOT NULL"

-- | Where an EB's txs are, and its closure.
sql_lookup_eb_closure :: String
sql_lookup_eb_closure =
  "SELECT txBounds, txs FROM ebs WHERE ebHashBytes = ?1 AND txCount IS NOT NULL LIMIT 1"

-- ** Promoting an EB to the immutable partition

-- | Pin a certified point, awaiting the copy, if the volatile partition holds
-- its EB. The durable record that survives a crash before the copy lands.
--
-- Parameters: 1 = ebSlot, 2 = ebHashBytes
sql_pin_eb :: String
sql_pin_eb =
  "INSERT OR IGNORE INTO pinned (ebHashBytes, ebSlot)\n\
  \SELECT ?2, ?1 WHERE EXISTS (SELECT 1 FROM ebs WHERE ebHashBytes = ?2)\n\
  \"

-- | Retire a pin. A volatile write, so it goes through the writer as
-- @MarkCopied@ -- only ever strictly after the immutable partition committed
-- the EB.
--
-- Parameters: 1 = ebSlot, 2 = ebHashBytes
sql_mark_as_copied :: String
sql_mark_as_copied =
  "DELETE FROM pinned WHERE ebSlot = ?1 AND ebHashBytes = ?2"

-- | The EB's body as stored, where its txs are, and its closure, from the
-- volatile partition.
sql_copy_closure :: String
sql_copy_closure =
  "SELECT body, txBounds, txs FROM vol.ebs\n\
  \WHERE ebHashBytes = ?1 AND txCount IS NOT NULL LIMIT 1\n\
  \"

-- | Copy the EB into the immutable partition, at the certified point.
--
-- @OR IGNORE@: retiring the pin is a separate volatile write, so a crash in
-- between leaves the EB copied and still pinned, and the copier repeats the
-- copy on the next start.
--
-- Parameters: 1 = ebSlot, 2 = ebHashBytes
sql_copy_insert_eb :: String
sql_copy_insert_eb =
  "INSERT OR IGNORE INTO ebs (ebSlot, ebHashBytes, ebBytesSize, txCount, txBounds, body, txs)\n\
  \SELECT ?1, ebHashBytes, ebBytesSize, txCount, txBounds, body, txs FROM vol.ebs\n\
  \WHERE ebHashBytes = ?2 AND txCount IS NOT NULL LIMIT 1\n\
  \"

-- | The next points waiting to be copied into the immutable partition, oldest
-- first.
--
-- The pin is the copy work list. It is in the database rather than in
-- memory, so a copy interrupted by a crash is simply still pending on the
-- next start.
sql_next_pinned_eb :: String
sql_next_pinned_eb =
  "SELECT ebSlot, ebHashBytes FROM vol.pinned ORDER BY ebSlot LIMIT ?1"

-- ** Garbage collection of the volatile partition

-- | The newest row: a GC collects only rows up to it, not those written after.
sql_gc_watermark :: String
sql_gc_watermark =
  "SELECT coalesce(max(id), 0) FROM ebs"

-- | Delete up to @?1@ rows before slot @?2@ and up to row @?3@, but those of
-- pinned EBs.
sql_gc_sweep :: String
sql_gc_sweep =
  "DELETE FROM ebs WHERE id IN (\n\
  \  SELECT id FROM ebs\n\
  \  WHERE ebSlot < ?2 AND id <= ?3\n\
  \    AND ebHashBytes NOT IN (SELECT ebHashBytes FROM pinned)\n\
  \  LIMIT ?1)\n\
  \"

-- | Whether no row is before slot @?1@ and up to row @?2@, pinned or not.
sql_gc_frontier_empty :: String
sql_gc_frontier_empty =
  "SELECT NOT EXISTS (SELECT 1 FROM ebs WHERE ebSlot < ?1 AND id <= ?2)"
