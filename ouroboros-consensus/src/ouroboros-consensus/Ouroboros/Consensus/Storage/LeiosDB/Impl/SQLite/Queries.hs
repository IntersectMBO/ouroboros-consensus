-- | The SQL text of every statement the SQLite-backed LeiosDB runs. The
-- schema they run against is in
-- "Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Schema".
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Queries
  ( sql_scan_ebs
  , sql_scan_complete_ebs_since
  , sql_insert_eb
  , sql_lookup_ebBodies
  , sql_insert_ebBody
  , sql_insert_tx
  , sql_filter_missing_txs_json
  , sql_find_complete_ebs
  , sql_mark_notified_ebs
  , sql_decrement_missing_tx_count
  , sql_delete_missing_txs
  , sql_insert_missing_txs
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
  , sql_copy_insert_txs
  , sql_imm_filter_present
  , sql_next_pinned_eb
  , sql_gc_has_work
  , sql_gc_markable
  , sql_gc_stage_marked
  , sql_gc_mark
  , sql_sweep_pick_marked
  , sql_gc_ebTxs
  , sql_gc_missing_txs
  , sql_gc_ebs_by_hash
  , sql_sweep_any_marked
  , sql_sweep_pick_orphans
  , sql_sweep_orphan_txs
  , sql_sweep_pop_orphans
  , sql_has_unstaged_gc_candidates
  , sql_unstaged_gc_candidates_page
  , sql_insert_gc_candidates
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

sql_insert_tx :: String
sql_insert_tx =
  "INSERT INTO txs (txHashBytes, txBytes, txBytesSize) VALUES (?, ?, ?)\n\
  \"

-- | Batch-filter txHashes via JSON1. Parameter is a JSON array of hex
-- strings; 'unhex(je.value)' decodes back into a BLOB comparable against
-- the indexed @txs.txHashBytes@ column.
sql_filter_missing_txs_json :: String
sql_filter_missing_txs_json =
  "SELECT unhex(je.value) FROM json_each(?) je\n\
  \WHERE NOT EXISTS (SELECT 1 FROM txs t WHERE t.txHashBytes = unhex(je.value))\n\
  \"

-- | Find EBs that are now complete (missingTxCount reached 0). Volatile rows
-- only: completion is decided within the one coherent volatile set.
sql_find_complete_ebs :: String
sql_find_complete_ebs =
  "SELECT ebHashBytes, ebSlot FROM ebs WHERE missingTxCount = 0 AND status = 0"

-- | Mark complete EBs as notified so they are not found again by
-- 'sql_find_complete_ebs'. Uses -1 as a sentinel for "already notified".
sql_mark_notified_ebs :: String
sql_mark_notified_ebs =
  "UPDATE ebs SET missingTxCount = -1 WHERE missingTxCount = 0 AND status = 0"

-- | Decrement missingTxCount for every EB still /waiting/ on the given txHash.
--
-- Uses 'ebsMissingTxs' rather than 'ebTxs', which makes this more efficient
-- than a full scan of 'ebTxs' in the average case.
--
-- Must be paired with 'sql_delete_missing_txs' in the same transaction.
--
-- Parameter 1: txHashBytes
sql_decrement_missing_tx_count :: String
sql_decrement_missing_tx_count =
  "UPDATE ebs SET missingTxCount = missingTxCount - 1\n\
  \WHERE ebHashBytes IN (SELECT ebHashBytes FROM ebsMissingTxs WHERE txHashBytes = ?)\n\
  \  AND status = 0\n\
  \"

-- | Retire the waiting rows for a tx that has just landed.
-- Parameter 1: txHashBytes
sql_delete_missing_txs :: String
sql_delete_missing_txs =
  "DELETE FROM ebsMissingTxs WHERE txHashBytes = ?"

-- | Record which of a freshly-inserted body's txs we do not yet hold.
--
-- One anti-join over the EB's own 'ebTxs' range -- the same work
-- 'sql_init_missing_tx_count' used to do to produce a count, now materialised so
-- that the arrival side reads the rows instead of recomputing them. Paying it
-- here rather than on every tx arrival is what earns the index removal: this
-- runs once per body, against ~4.7 times per tx for the old reverse lookup.
--
-- Parameter 1: ebHashBytes
sql_insert_missing_txs :: String
sql_insert_missing_txs =
  "INSERT OR IGNORE INTO ebsMissingTxs (txHashBytes, ebHashBytes)\n\
  \SELECT e.txHashBytes, e.ebHashBytes FROM ebTxs e\n\
  \LEFT JOIN txs t ON e.txHashBytes = t.txHashBytes\n\
  \WHERE e.ebHashBytes = ? AND t.txHashBytes IS NULL\n\
  \"

-- | Initialize missingTxCount after EB body is inserted, returning the
-- resulting count. Counts ebTxs entries that don't yet have a corresponding
-- tx in the txs table. The RETURNING clause lets the caller detect the
-- special case @missingTxCount = 0@ (all referenced txs already present) with
-- a PK lookup on the row that was just touched, instead of a full-table
-- scan via 'sql_find_complete_ebs'.
--
-- Parameters: 1 = ebHashBytes, 2 = ebHashBytes, 3 = ebSlot
sql_init_missing_tx_count :: String
sql_init_missing_tx_count =
  "UPDATE ebs SET missingTxCount = (\n\
  \    SELECT COUNT(*) FROM ebsMissingTxs WHERE ebHashBytes = ?\n\
  \) WHERE ebHashBytes = ? AND ebSlot = ?\n\
  \RETURNING missingTxCount\n\
  \"

-- | Mark a specific EB as notified (@missingTxCount = -1@). PK-scoped
-- variant of 'sql_mark_notified_ebs'; used by 'sqlInsertEbBody' when the
-- body's arrival is what completed the closure.
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
  "SELECT je.value, e.txHashBytes, t.txBytes\n\
  \FROM json_each(?2) je\n\
  \JOIN ebTxs e ON e.ebHashBytes = ?1 AND e.txOffset = je.value\n\
  \LEFT JOIN txs t ON e.txHashBytes = t.txHashBytes\n\
  \ORDER BY je.value ASC\n\
  \"

sql_lookup_eb_closure :: String
sql_lookup_eb_closure =
  unlines
    [ "SELECT ebTx.txHashBytes, tx.txBytes"
    , "FROM ebTxs as ebTx"
    , "LEFT JOIN txs as tx ON ebTx.txHashBytes = tx.txHashBytes"
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
-- partition, in one probe: the LEFT JOIN's second count skips missing txs,
-- so the EB's closure is complete iff both counts are equal (and non-zero).
sql_copy_completeness :: String
sql_copy_completeness =
  "SELECT COUNT(*), COUNT(t.txHashBytes)\n\
  \FROM vol.ebTxs e LEFT JOIN vol.txs t ON t.txHashBytes = e.txHashBytes\n\
  \WHERE e.ebHashBytes = ?1\n\
  \"

-- | Copy the EB's newest announcement row, with the canonical immutable
-- column values (@missingTxCount = -1@: complete and notified; @status = 2@:
-- copied).
--
-- TODO(geo2a): should not need to copy missingTxCount and status to immutable,
-- this is only relevant for the volatile.
-- @OR IGNORE@: the mark that retires the pin is a separate volatile write, so
-- a crash in between leaves the EB copied and still pinned, and the copier
-- repeats the copy on the next start.
sql_copy_insert_eb :: String
sql_copy_insert_eb =
  "INSERT OR IGNORE INTO ebs (ebSlot, ebHashBytes, ebBytesSize, missingTxCount, status)\n\
  \SELECT ebSlot, ebHashBytes, ebBytesSize, -1, 2 FROM vol.ebs\n\
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

-- | Copy the EB's txs. @OR IGNORE@: a tx shared with an earlier-copied EB is
-- already present.
sql_copy_insert_txs :: String
sql_copy_insert_txs =
  "INSERT OR IGNORE INTO txs (txHashBytes, txBytes, txBytesSize)\n\
  \SELECT t.txHashBytes, t.txBytes, t.txBytesSize FROM vol.txs t\n\
  \WHERE t.txHashBytes IN\n\
  \  (SELECT txHashBytes FROM vol.ebTxs WHERE ebHashBytes = ?1)\n\
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

-- | The markability predicate, shared by 'sql_gc_mark' and
-- 'sql_gc_stage_marked' so the marked set and the staged set can never
-- diverge. @c@ is the row under test; it is markable if it is
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

-- | Add the txs of every EB 'sql_gc_mark' is about to hit as GC candidates.
-- Must run strictly BEFORE 'sql_gc_mark' in the same transaction:
-- the UPDATE changes the 'status' of EBs and orphans the transactions.
sql_gc_stage_marked :: String
sql_gc_stage_marked =
  "INSERT OR IGNORE INTO gcTxCandidates (txHashBytes)\n\
  \SELECT DISTINCT e.txHashBytes FROM ebTxs e\n\
  \WHERE e.ebHashBytes IN\n\
  \  (SELECT DISTINCT c.ebHashBytes FROM ebs c\n\
  \   WHERE "
    <> sql_gc_markable
    <> ")"

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

-- | Evict the 'ebsMissingTxs' rows with the specified 'ebHashBytes' (a JSON array of byte strings).
sql_gc_missing_txs :: String
sql_gc_missing_txs =
  "DELETE FROM ebsMissingTxs WHERE ebHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)"

-- | Evict the 'ebs' rows with the specified 'ebHashBytes' (a JSON array of byte strings).
sql_gc_ebs_by_hash :: String
sql_gc_ebs_by_hash =
  "DELETE FROM ebs WHERE ebHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)"

-- | Whether any GC-marked EBs remain.
sql_sweep_any_marked :: String
sql_sweep_any_marked =
  "SELECT EXISTS (SELECT 1 FROM ebs WHERE status = 3)"

-- | Get up to @?1@ transactions to be evicted.
sql_sweep_pick_orphans :: String
sql_sweep_pick_orphans =
  "SELECT txHashBytes FROM gcTxCandidates LIMIT ?1"

-- | Evict transactions with the specified hashes (a JSON array of byte strings),
--   making sure that they are not referenced by any EBs.
sql_sweep_orphan_txs :: String
sql_sweep_orphan_txs =
  "DELETE FROM txs\n\
  \WHERE txHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)\n\
  \  AND NOT EXISTS\n\
  \    (SELECT 1 FROM ebTxs WHERE ebTxs.txHashBytes = txs.txHashBytes)\n\
  \"

-- | Delete GC transaction candidates with the specified hashes (a JSON array of byte strings).
sql_sweep_pop_orphans :: String
sql_sweep_pop_orphans =
  "DELETE FROM gcTxCandidates\n\
  \WHERE txHashBytes IN (SELECT unhex(je.value) FROM json_each(?1) je)\n\
  \"

-- | Whether the volatile partition holds any unstaged GC candidates (txs no
-- EB references).
sql_has_unstaged_gc_candidates :: String
sql_has_unstaged_gc_candidates =
  "SELECT EXISTS (SELECT 1 FROM txs WHERE NOT EXISTS\n\
  \  (SELECT 1 FROM ebTxs WHERE ebTxs.txHashBytes = txs.txHashBytes))\n\
  \"

-- | One keyset page of unstaged GC candidates (txs no EB references), for
-- @gcReinit@: @?1@ = cursor (exclusive), @?2@ = page size.
sql_unstaged_gc_candidates_page :: String
sql_unstaged_gc_candidates_page =
  "SELECT txHashBytes FROM txs\n\
  \WHERE txHashBytes > ?1\n\
  \  AND NOT EXISTS (SELECT 1 FROM ebTxs WHERE ebTxs.txHashBytes = txs.txHashBytes)\n\
  \ORDER BY txHashBytes LIMIT ?2\n\
  \"

-- | Stage one page of GC candidates (JSON hex array @?1@).
sql_insert_gc_candidates :: String
sql_insert_gc_candidates =
  "INSERT OR IGNORE INTO gcTxCandidates (txHashBytes)\n\
  \SELECT unhex(je.value) FROM json_each(?1) je\n\
  \"
