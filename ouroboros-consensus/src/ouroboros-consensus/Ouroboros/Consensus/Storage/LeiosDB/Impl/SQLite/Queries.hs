-- | The SQL text of every statement the SQLite-backed LeiosDB runs. The
-- schema they run against is in
-- "Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Schema".
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Queries
  ( sql_scan_ebs
  , sql_insert_eb
  , sql_insert_eb_body
  , sql_lookup_eb_body
  , sql_lookup_eb_index
  , sql_insert_eb_closure
  , sql_lookup_eb_closure
  , sql_pin_eb
  , sql_mark_as_copied
  , sql_copy_has_closure
  , sql_copy_insert_eb
  , sql_copy_insert_ebBody
  , sql_copy_insert_ebClosures
  , sql_next_pinned_eb
  , sql_gc_has_work
  , sql_gc_markable
  , sql_gc_mark
  , sql_sweep_pick_marked
  , sql_gc_ebBodies
  , sql_gc_ebClosures
  , sql_gc_ebs_by_hash
  ) where

sql_scan_ebs :: String
sql_scan_ebs =
  "SELECT ebSlot, ebHashBytes\n\
  \FROM ebs\n\
  \ORDER BY ebSlot ASC\n\
  \"

sql_insert_eb :: String
sql_insert_eb =
  "INSERT OR IGNORE INTO ebs (ebSlot, ebHashBytes, ebBytesSize) VALUES (?, ?, ?)"

-- | Insert an EB's body. @OR IGNORE@: a second delivery of the same body
-- changes nothing.
--
-- Parameters: 1 = ebHashBytes, 2 = txBounds, 3 = body (wire encoding)
sql_insert_eb_body :: String
sql_insert_eb_body =
  "INSERT OR IGNORE INTO ebBodies (ebHashBytes, txBounds, body) VALUES (?1, ?2, ?3)"

-- | An EB's body, in its wire encoding.
sql_lookup_eb_body :: String
sql_lookup_eb_body =
  "SELECT body FROM ebBodies WHERE ebHashBytes = ?1"

-- | Where an EB's blobs are: the rowid of its body, how many txs it lists,
-- and the rowid of its closure, NULL until a tx is written. @length@ of a
-- blob reads its row header only, never the blob.
sql_lookup_eb_index :: String
sql_lookup_eb_index =
  "SELECT b.rowid, length(b.txBounds) / 4 - 1, c.rowid\n\
  \FROM ebBodies b LEFT JOIN ebClosures c ON c.ebHashBytes = b.ebHashBytes\n\
  \WHERE b.ebHashBytes = ?1\n\
  \"

-- | Allocate an EB's closure with zeroes, which tx writes then overwrite in
-- place.
--
-- Parameters: 1 = ebHashBytes, 2 = closure size
sql_insert_eb_closure :: String
sql_insert_eb_closure =
  "INSERT INTO ebClosures (ebHashBytes, txs) VALUES (?1, zeroblob(?2))"

-- | Where an EB's txs are, and its closure, NULL until a tx is written.
sql_lookup_eb_closure :: String
sql_lookup_eb_closure =
  "SELECT b.txBounds, c.txs\n\
  \FROM ebBodies b LEFT JOIN ebClosures c ON c.ebHashBytes = b.ebHashBytes\n\
  \WHERE b.ebHashBytes = ?1\n\
  \"

-- ** Promoting an EB to the immutable partition

-- | Pin every announcement row of the EB: certified, awaiting copy, never
-- evicted. The durable record that survives a crash before the copy lands.
-- Also rescues GC-marked rows (@status = 3@): a promotion arriving between
-- mark and sweep unmarks the hash, and the sweeper's all-marked pick
-- then skips it.
sql_pin_eb :: String
sql_pin_eb =
  "UPDATE ebs SET status = 1 WHERE ebHashBytes = ? AND status IN (0, 3)"

-- | Mark a pinned EB as copied (evictable). A volatile write, so it goes
-- through the writer as @MarkCopied@ -- only ever strictly after the
-- immutable partition committed the EB's closure.
sql_mark_as_copied :: String
sql_mark_as_copied =
  "UPDATE ebs SET status = 2 WHERE ebHashBytes = ? AND status = 1"

-- | Whether the volatile partition holds a closure for the EB. Whether it is
-- complete is the fetch logic's to know.
sql_copy_has_closure :: String
sql_copy_has_closure =
  "SELECT 1 FROM vol.ebClosures WHERE ebHashBytes = ?1"

-- | Copy the EB's newest announcement row. Only the columns the immutable
-- partition has ('sql_schema_imm'): completeness and GC status are
-- volatile-only.
--
-- @OR IGNORE@: the mark that retires the pin is a separate volatile write, so
-- a crash in between leaves the EB copied and still pinned, and the copier
-- repeats the copy on the next start.
sql_copy_insert_eb :: String
sql_copy_insert_eb =
  "INSERT OR IGNORE INTO ebs (ebSlot, ebHashBytes, ebBytesSize)\n\
  \SELECT ebSlot, ebHashBytes, ebBytesSize FROM vol.ebs\n\
  \WHERE ebHashBytes = ?1\n\
  \ORDER BY ebSlot DESC LIMIT 1\n\
  \"

-- | Copy the EB's body. @OR IGNORE@ for the same reason as
-- 'sql_copy_insert_eb'.
sql_copy_insert_ebBody :: String
sql_copy_insert_ebBody =
  "INSERT OR IGNORE INTO ebBodies (ebHashBytes, txBounds, body)\n\
  \SELECT ebHashBytes, txBounds, body FROM vol.ebBodies WHERE ebHashBytes = ?1\n\
  \"

-- | Copy the EB's closure. @OR IGNORE@ for the same reason as
-- 'sql_copy_insert_eb'.
sql_copy_insert_ebClosures :: String
sql_copy_insert_ebClosures =
  "INSERT OR IGNORE INTO ebClosures (ebHashBytes, txs)\n\
  \SELECT ebHashBytes, txs FROM vol.ebClosures WHERE ebHashBytes = ?1\n\
  \"

-- ** Garbage collection of the volatile partition

-- | Pinned EBs the copier has not marked as copied yet, for self-heal
-- re-enqueueing.
-- | The next EB waiting to be copied into the immutable partition: pinned
-- ('sql_pin_eb') but not yet marked copied ('sql_mark_as_copied'). Oldest
-- first, and covered by @idx_ebs_pinned@.
--
-- The pin is the copy work list. It is in the database rather than in
-- memory, so a copy interrupted by a crash is simply still pending on the
-- next start.
sql_next_pinned_eb :: String
sql_next_pinned_eb =
  "SELECT ebHashBytes FROM vol.ebs WHERE status = 1 ORDER BY ebSlot LIMIT ?1"

-- | Whether a GC at slot @?1@ could has any work, i.e.
--   if there are any old volatile EBs, already copied EBs or leftover EBs
--   from an interrupted GC pass.
sql_gc_has_work :: String
sql_gc_has_work =
  "SELECT EXISTS (SELECT 1 FROM ebs WHERE status IN (0, 2, 3) AND ebSlot < ?1)"

-- | The markability predicate of 'sql_gc_mark'. @c@ is the row under test; it is markable if it is
--   - old enough (its slot is before the GC frontier @?1@) and
--   - either volatile (status 0) or already copied (status 2) and
--   - not vetoed by a live row of the same hash (pinned, or announced at or
--     after the frontier).
sql_gc_markable :: String
sql_gc_markable =
  "c.status IN (0, 2) AND c.ebSlot < ?1\n\
  \  AND NOT EXISTS\n\
  \    (SELECT 1 FROM ebs live\n\
  \     WHERE live.ebHashBytes = c.ebHashBytes\n\
  \       AND (live.status = 1 OR live.ebSlot >= ?1))"

-- | Mark for GC (@status = 3@) every row satisfying 'sql_gc_markable'.
sql_gc_mark :: String
sql_gc_mark =
  "UPDATE ebs AS c SET status = 3\n\
  \WHERE "
    <> sql_gc_markable

-- | Up to @?1@ GC-marked EBs ready to sweep.
--
-- Intuition: give me up to N hash values that are currently in status 3
-- and have never been assigned any status other than 3.
--
-- Note: 'FROM ebs cand' and 'FROM ebs live' allows referring to the rows
-- of the 'ebs' table using the alias 'cand' and 'live'.
--
-- TODO(geo2a): think how to simplify this query.
sql_sweep_pick_marked :: String
sql_sweep_pick_marked =
  "SELECT DISTINCT cand.ebHashBytes FROM ebs cand\n\
  \WHERE cand.status = 3\n\
  \  AND NOT EXISTS\n\
  \    (SELECT 1 FROM ebs live\n\
  \     WHERE live.ebHashBytes = cand.ebHashBytes AND live.status <> 3)\n\
  \LIMIT ?1\n\
  \"

-- | Evict the 'ebBodies' rows with the specified 'ebHashBytes' (a JSON array of byte strings).
sql_gc_ebBodies :: String
sql_gc_ebBodies =
  "DELETE FROM ebBodies WHERE ebHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)"

-- | Evict the 'ebClosures' rows with the specified 'ebHashBytes' (a JSON array of byte strings).
sql_gc_ebClosures :: String
sql_gc_ebClosures =
  "DELETE FROM ebClosures WHERE ebHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)"

-- | Evict the 'ebs' rows with the specified 'ebHashBytes' (a JSON array of byte strings).
sql_gc_ebs_by_hash :: String
sql_gc_ebs_by_hash =
  "DELETE FROM ebs WHERE ebHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)"
