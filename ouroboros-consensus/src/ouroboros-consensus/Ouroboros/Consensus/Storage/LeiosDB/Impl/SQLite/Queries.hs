-- | The SQL text of every statement the SQLite-backed LeiosDB runs. The
-- schema they run against is in
-- "Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Schema".
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Queries
  ( sql_scan_ebs
  , sql_scan_complete_ebs_since
  , sql_insert_eb
  , sql_lookup_ebBodies
  , sql_insert_ebBody
  , sql_prealloc_ebTxBytes
  , sql_fill_ebTxBytes
  , sql_decrement_missing_tx_count
  , sql_init_missing_tx_count
  , sql_mark_point_notified
  , sql_retrieve_from_ebTxs_json
  , sql_lookup_eb_closure
  , sql_scan_recent_ebs
  , sql_pin_eb
  , sql_mark_as_copied
  , sql_copy_completeness
  , sql_copy_insert_eb
  , sql_copy_insert_ebTxs
  , sql_copy_insert_ebTxBytes
  , sql_imm_filter_present
  , sql_next_pinned_eb
  , sql_gc_has_work
  , sql_gc_markable
  , sql_gc_mark
  , sql_sweep_pick_marked
  , sql_gc_ebTxBytes
  , sql_gc_ebTxs
  , sql_gc_ebs_by_hash
  ) where

sql_scan_ebs :: String
sql_scan_ebs =
  "SELECT ebSlot, ebHashBytes\n\
  \FROM ebs\n\
  \ORDER BY ebSlot ASC\n\
  \"

-- | For @sqlScanCompleteEbPointsSince@
--
-- The two conditions are decoupled across rows: the same EB hash can have
-- several @(ebSlot, ebHashBytes)@ rows (one per announcer slot), and
-- 'missingTxCount' is maintained per row on body insert but per hash on tx
-- arrival, so the /complete/ row and the /recent/ row can differ. Requiring
-- both on a single row would wrongly drop a complete EB re-announced recently
-- (its recent row never got a body insert, so its @missingTxCount@ is still
-- NULL), leaving its cert-RB parked forever. Hence: keep a hash that has
-- /any/ complete row and /any/ row at @ebSlot >= ?@.
sql_scan_complete_ebs_since :: String
sql_scan_complete_ebs_since =
  "SELECT MAX(ebSlot), ebHashBytes FROM ebs\n\
  \WHERE ebSlot >= ?\n\
  \  AND ebHashBytes IN\n\
  \      (SELECT ebHashBytes FROM ebs WHERE missingTxCount IS NOT NULL AND missingTxCount <= 0)\n\
  \GROUP BY ebHashBytes\n\
  \"

sql_insert_eb :: String
sql_insert_eb =
  "INSERT OR IGNORE INTO ebs (ebSlot, ebHashBytes, ebBytesSize) VALUES (?, ?, ?)"

sql_lookup_ebBodies :: String
sql_lookup_ebBodies =
  "SELECT txHashBytes, txBytesSize FROM ebTxs\n\
  \WHERE ebHashBytes = ?\n\
  \ORDER BY txOffset ASC\n\
  \"

sql_insert_ebBody :: String
sql_insert_ebBody =
  "INSERT INTO ebTxs (ebHashBytes, txOffset, txHashBytes, txBytesSize) VALUES (?, ?, ?, ?)\n\
  \"

-- | Allocate the body's whole 'ebTxBytes' range in one offset-ordered pass:
-- one @zeroblob@ row per body row, physically contiguous and fully packed.
-- @OR IGNORE@: ignore duplicates (a second delivery of the same EB body).
--
-- Parameter 1: ebHashBytes
sql_prealloc_ebTxBytes :: String
sql_prealloc_ebTxBytes =
  "INSERT OR IGNORE INTO ebTxBytes (ebHashBytes, txOffset, filled, txBytes)\n\
  \SELECT ebHashBytes, txOffset, 0, zeroblob(txBytesSize) FROM ebTxs\n\
  \WHERE ebHashBytes = ?1 ORDER BY txOffset\n\
  \"

-- | Fill one pre-allocated row in place. Guarded three ways: the row must
-- exist (a bogus offset updates nothing), must not be filled yet (a duplicate
-- delivery updates nothing), and the payload must have exactly the declared
-- size (the row was allocated as @zeroblob@ of it, and an in-place overwrite
-- must not change the row size). One successful UPDATE is therefore exactly
-- one previously-missing valid tx, which is what makes counting by
-- @changes()@ sound.
--
-- Parameters: 1 = ebHashBytes, 2 = txOffset, 3 = txBytes
sql_fill_ebTxBytes :: String
sql_fill_ebTxBytes =
  "UPDATE ebTxBytes SET txBytes = ?3, filled = 1\n\
  \WHERE ebHashBytes = ?1 AND txOffset = ?2 AND filled = 0\n\
  \  AND length(txBytes) = length(?3)\n\
  \"

-- | Decrement missingTxCount on every announcement of this content hash by
-- the number of tx-bytes rows a batch actually filled, returning each
-- touched row so the caller can spot the ones that just completed. Bytes are
-- keyed by @(ebHash, txOffset)@, so a fill can only ever close a hole in this
-- EB.
--
-- Parameters: 1 = ebHashBytes, 2 = rows filled
sql_decrement_missing_tx_count :: String
sql_decrement_missing_tx_count =
  "UPDATE ebs SET missingTxCount = missingTxCount - ?2\n\
  \WHERE ebHashBytes = ?1 AND status = 0 AND missingTxCount IS NOT NULL\n\
  \RETURNING ebSlot, missingTxCount\n\
  \"

-- | Initialize missingTxCount after an EB body is inserted: the rows still
-- unfilled (a redelivered body at a second point finds the first point's
-- fills). RETURNING lets the caller detect @missingTxCount = 0@ with a PK
-- lookup on the touched row.
--
-- Parameters: 1 = ebHashBytes, 2 = ebHashBytes, 3 = ebSlot
sql_init_missing_tx_count :: String
sql_init_missing_tx_count =
  "UPDATE ebs SET missingTxCount = (\n\
  \    SELECT COUNT(*) FROM ebTxBytes WHERE ebHashBytes = ?1 AND filled = 0\n\
  \) WHERE ebHashBytes = ?2 AND ebSlot = ?3\n\
  \RETURNING missingTxCount\n\
  \"

-- | Mark a specific EB as notified (@missingTxCount = -1@), once a write
-- completed its closure.
--
-- Parameters: 1 = ebSlot, 2 = ebHashBytes
sql_mark_point_notified :: String
sql_mark_point_notified =
  "UPDATE ebs SET missingTxCount = -1 WHERE ebSlot = ? AND ebHashBytes = ?"

-- | Batch retrieve of tx bytes for a batch of @(ebHash, offset)@ points.
-- @?1@ is the ebHash blob (all offsets belong to the same EB); @?2@ is a
-- JSON int array of offsets. The join uses ebTxs' PK
-- @(ebHashBytes, txOffset)@, so index lookups still fire.
sql_retrieve_from_ebTxs_json :: String
sql_retrieve_from_ebTxs_json =
  "SELECT je.value, e.txHashBytes,\n\
  \       CASE WHEN b.filled = 1 THEN b.txBytes END\n\
  \FROM json_each(?2) je\n\
  \JOIN ebTxs e ON e.ebHashBytes = ?1 AND e.txOffset = je.value\n\
  \LEFT JOIN ebTxBytes b ON b.ebHashBytes = ?1 AND b.txOffset = je.value\n\
  \ORDER BY je.value ASC\n\
  \"

sql_lookup_eb_closure :: String
sql_lookup_eb_closure =
  unlines
    [ "SELECT ebTx.txHashBytes, CASE WHEN b.filled = 1 THEN b.txBytes END"
    , "FROM ebTxs as ebTx"
    , "LEFT JOIN ebTxBytes as b ON b.ebHashBytes = ebTx.ebHashBytes AND b.txOffset = ebTx.txOffset"
    , "WHERE ebTx.ebHashBytes = ?"
    , "ORDER BY ebTx.txOffset ASC"
    ]

-- | Every recent announcement, with or without completeness evidence.
-- Companion of 'sql_scan_complete_ebs_since': the difference of the two sets
-- is probed against the immutable partition (see
-- @sqlScanCompleteEbPointsSince@).
sql_scan_recent_ebs :: String
sql_scan_recent_ebs =
  "SELECT MAX(ebSlot), ebHashBytes FROM ebs\n\
  \WHERE ebSlot >= ?\n\
  \GROUP BY ebHashBytes\n\
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

-- | Body row count and closure row count of the EB in the volatile
-- partition, in one probe: the EB's closure is complete iff both counts are
-- equal (and non-zero). Two range COUNTs on the shared PK.
sql_copy_completeness :: String
sql_copy_completeness =
  "SELECT (SELECT COUNT(*) FROM vol.ebTxs WHERE ebHashBytes = ?1),\n\
  \       (SELECT COUNT(*) FROM vol.ebTxBytes WHERE ebHashBytes = ?1 AND filled = 1)\n\
  \"

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

-- | Copy the EB's body rows. @OR IGNORE@ for the same reason as
-- 'sql_copy_insert_eb'.
sql_copy_insert_ebTxs :: String
sql_copy_insert_ebTxs =
  "INSERT OR IGNORE INTO ebTxs (ebHashBytes, txOffset, txHashBytes, txBytesSize)\n\
  \SELECT ebHashBytes, txOffset, txHashBytes, txBytesSize FROM vol.ebTxs\n\
  \WHERE ebHashBytes = ?1\n\
  \"

-- | Copy the EB's tx bytes: a range copy on the shared PK. @OR IGNORE@ for
-- the same reason as 'sql_copy_insert_eb'.
sql_copy_insert_ebTxBytes :: String
sql_copy_insert_ebTxBytes =
  "INSERT OR IGNORE INTO ebTxBytes (ebHashBytes, txOffset, filled, txBytes)\n\
  \SELECT ebHashBytes, txOffset, filled, txBytes FROM vol.ebTxBytes\n\
  \WHERE ebHashBytes = ?1 ORDER BY txOffset\n\
  \"

-- | Which of the given hashes (JSON hex array) the immutable partition holds.
-- Presence is proof of a complete closure: copies land atomically and only
-- complete EBs are copied.
sql_imm_filter_present :: String
sql_imm_filter_present =
  "SELECT unhex(je.value) FROM json_each(?1) je\n\
  \WHERE EXISTS (SELECT 1 FROM ebs e WHERE e.ebHashBytes = unhex(je.value))\n\
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
--   if there are any old volatile EBs or already copied EBs.
sql_gc_has_work :: String
sql_gc_has_work =
  "SELECT EXISTS (SELECT 1 FROM ebs WHERE status IN (0, 2) AND ebSlot < ?1)"

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

-- | Evict the 'ebTxs' rows with the specified 'ebHashBytes' (a JSON array of byte strings).
sql_gc_ebTxs :: String
sql_gc_ebTxs =
  "DELETE FROM ebTxs WHERE ebHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)"

-- | Evict the 'ebTxBytes' rows with the specified 'ebHashBytes' (a JSON array of byte strings).
sql_gc_ebTxBytes :: String
sql_gc_ebTxBytes =
  "DELETE FROM ebTxBytes WHERE ebHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)"

-- | Evict the 'ebs' rows with the specified 'ebHashBytes' (a JSON array of byte strings).
sql_gc_ebs_by_hash :: String
sql_gc_ebs_by_hash =
  "DELETE FROM ebs WHERE ebHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)"
