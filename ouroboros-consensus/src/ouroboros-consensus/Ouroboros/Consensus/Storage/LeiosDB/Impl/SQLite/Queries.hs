-- | The SQL text of every statement the SQLite-backed LeiosDB runs. The
-- schema they run against is in
-- "Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Schema".
--
-- Every row belongs to one endorser block point. A statement about a point
-- takes its slot as @?1@ and its hash as @?2@.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Queries
  ( sql_scan_ebs
  , sql_scan_complete_ebs_since
  , sql_insert_eb
  , sql_lookup_ebBodies
  , sql_insert_ebBody
  , sql_prealloc_ebTxBytes
  , sql_fill_ebTxBytes
  , sql_fill_ebTxBytes_locally
  , sql_decrement_missing_tx_count
  , sql_init_missing_tx_count
  , sql_mark_point_notified
  , sql_retrieve_from_ebTxs_json
  , sql_lookup_eb_closure
  , sql_pin_eb
  , sql_mark_as_copied
  , sql_copy_completeness
  , sql_copy_insert_eb
  , sql_copy_insert_ebTxs
  , sql_copy_insert_ebTxBytes
  , sql_next_pinned_eb
  , sql_gc_has_work
  , sql_gc_mark
  , sql_sweep_pick_marked
  , sql_gc_ebTxBytes
  , sql_gc_ebTxs
  , sql_gc_ebs
  ) where

sql_scan_ebs :: String
sql_scan_ebs =
  "SELECT ebSlot, ebHashBytes\n\
  \FROM ebs\n\
  \ORDER BY ebSlot ASC\n\
  \"

-- | For @sqlScanCompleteEbPointsSince@: the points no older than @?1@ whose
-- closure is complete.
sql_scan_complete_ebs_since :: String
sql_scan_complete_ebs_since =
  "SELECT ebSlot, ebHashBytes FROM ebs\n\
  \WHERE ebSlot >= ?1 AND missingTxCount IS NOT NULL AND missingTxCount <= 0\n\
  \ORDER BY ebSlot, ebHashBytes\n\
  \"

sql_insert_eb :: String
sql_insert_eb =
  "INSERT OR IGNORE INTO ebs (ebSlot, ebHashBytes, ebBytesSize) VALUES (?1, ?2, ?3)"

sql_lookup_ebBodies :: String
sql_lookup_ebBodies =
  "SELECT txHashBytes, txBytesSize FROM ebTxs\n\
  \WHERE ebSlot = ?1 AND ebHashBytes = ?2\n\
  \ORDER BY txOffset ASC\n\
  \"

-- | Parameters: 3 = txOffset, 4 = txHashBytes, 5 = txBytesSize
sql_insert_ebBody :: String
sql_insert_ebBody =
  "INSERT INTO ebTxs (ebSlot, ebHashBytes, txOffset, txHashBytes, txBytesSize)\n\
  \VALUES (?1, ?2, ?3, ?4, ?5)\n\
  \"

-- | Allocate the body's whole 'ebTxBytes' range in one offset-ordered pass:
-- one @zeroblob@ row per body row, physically contiguous and fully packed.
-- @OR IGNORE@: ignore duplicates (a second delivery of the same body at the
-- same point).
sql_prealloc_ebTxBytes :: String
sql_prealloc_ebTxBytes =
  "INSERT OR IGNORE INTO ebTxBytes (ebSlot, ebHashBytes, txOffset, filled, txBytes)\n\
  \SELECT ebSlot, ebHashBytes, txOffset, 0, zeroblob(txBytesSize) FROM ebTxs\n\
  \WHERE ebSlot = ?1 AND ebHashBytes = ?2 ORDER BY txOffset\n\
  \"

-- | Fill one pre-allocated row in place. Guarded three ways: the row must
-- exist (a bogus offset updates nothing), must not be filled yet (a duplicate
-- delivery updates nothing), and the payload must have exactly the declared
-- size (the row was allocated as @zeroblob@ of it, and an in-place overwrite
-- must not change the row size). One successful UPDATE is therefore exactly
-- one previously-missing valid tx, which is what makes counting by
-- @changes()@ sound.
--
-- Parameters: 3 = txOffset, 4 = txBytes
sql_fill_ebTxBytes :: String
sql_fill_ebTxBytes =
  "UPDATE ebTxBytes SET txBytes = ?4, filled = 1\n\
  \WHERE ebSlot = ?1 AND ebHashBytes = ?2 AND txOffset = ?3 AND filled = 0\n\
  \  AND length(txBytes) = length(?4)\n\
  \"

-- | Fill one pre-allocated row from another point's filled row, if that row
-- holds the tx this point's body lists at this offset. Guarded like
-- 'sql_fill_ebTxBytes', and also by the two bodies listing the same tx hash at
-- their offsets, so a stale or wrong location updates nothing. The source's
-- bytes hash to the hash its body lists, since a write must, so this needs no
-- hashing.
--
-- Parameters: 3 = txOffset, 4 = source ebSlot, 5 = source ebHashBytes,
-- 6 = source txOffset
sql_fill_ebTxBytes_locally :: String
sql_fill_ebTxBytes_locally =
  "UPDATE ebTxBytes SET filled = 1, txBytes = (\n\
  \    SELECT src.txBytes FROM ebTxBytes AS src\n\
  \    WHERE src.ebSlot = ?4 AND src.ebHashBytes = ?5 AND src.txOffset = ?6\n\
  \)\n\
  \WHERE ebSlot = ?1 AND ebHashBytes = ?2 AND txOffset = ?3 AND filled = 0\n\
  \  AND EXISTS (\n\
  \    SELECT 1 FROM ebTxs AS dstTx, ebTxs AS srcTx, ebTxBytes AS src\n\
  \    WHERE dstTx.ebSlot = ?1 AND dstTx.ebHashBytes = ?2 AND dstTx.txOffset = ?3\n\
  \      AND srcTx.ebSlot = ?4 AND srcTx.ebHashBytes = ?5 AND srcTx.txOffset = ?6\n\
  \      AND srcTx.txHashBytes = dstTx.txHashBytes\n\
  \      AND src.ebSlot = ?4 AND src.ebHashBytes = ?5 AND src.txOffset = ?6\n\
  \      AND src.filled = 1\n\
  \      AND length(src.txBytes) = length(ebTxBytes.txBytes)\n\
  \  )\n\
  \"

-- | Decrement the point's missingTxCount, if it has one, by the number of
-- tx-bytes rows a batch actually filled, returning the new count so the caller
-- can spot that it just completed.
--
-- Parameters: 3 = rows filled
sql_decrement_missing_tx_count :: String
sql_decrement_missing_tx_count =
  "UPDATE ebs SET missingTxCount = missingTxCount - ?3\n\
  \WHERE ebSlot = ?1 AND ebHashBytes = ?2 AND status = 0 AND missingTxCount IS NOT NULL\n\
  \RETURNING missingTxCount\n\
  \"

-- | Initialize missingTxCount after a body is inserted: the number of the
-- point's 'ebTxBytes' rows not yet filled. RETURNING lets the caller detect
-- @missingTxCount = 0@.
sql_init_missing_tx_count :: String
sql_init_missing_tx_count =
  "UPDATE ebs SET missingTxCount = (\n\
  \    SELECT COUNT(*) FROM ebTxBytes\n\
  \    WHERE ebSlot = ?1 AND ebHashBytes = ?2 AND filled = 0\n\
  \) WHERE ebSlot = ?1 AND ebHashBytes = ?2\n\
  \RETURNING missingTxCount\n\
  \"

-- | Mark a point as notified (@missingTxCount = -1@), once a write completed
-- its closure.
sql_mark_point_notified :: String
sql_mark_point_notified =
  "UPDATE ebs SET missingTxCount = -1 WHERE ebSlot = ?1 AND ebHashBytes = ?2"

-- | Batch retrieve of tx bytes for a batch of offsets into one point's body;
-- @?3@ is a JSON int array of offsets. The join uses the PK, so index lookups
-- still fire.
--
-- A tx whose bytes have not arrived yet reads as NULL rather than as its
-- zero-filled placeholder. So a request for txs this node does not have, such
-- as a bad @MsgLeiosBlockTxsRequest@, is detected rather than answered with
-- zeros.
sql_retrieve_from_ebTxs_json :: String
sql_retrieve_from_ebTxs_json =
  "SELECT je.value, e.txHashBytes,\n\
  \       CASE WHEN b.filled = 1 THEN b.txBytes END\n\
  \FROM json_each(?3) je\n\
  \JOIN ebTxs e ON e.ebSlot = ?1 AND e.ebHashBytes = ?2 AND e.txOffset = je.value\n\
  \LEFT JOIN ebTxBytes b ON b.ebSlot = ?1 AND b.ebHashBytes = ?2 AND b.txOffset = je.value\n\
  \ORDER BY je.value ASC\n\
  \"

-- | The point's txs in body order.
--
-- A tx whose bytes have not arrived yet reads as NULL rather than as its
-- zero-filled placeholder, which @sqlLookupEbClosure@ reports as an incomplete
-- closure.
sql_lookup_eb_closure :: String
sql_lookup_eb_closure =
  unlines
    [ "SELECT ebTx.txHashBytes, CASE WHEN b.filled = 1 THEN b.txBytes END"
    , "FROM ebTxs as ebTx"
    , "LEFT JOIN ebTxBytes as b"
    , "  ON b.ebSlot = ebTx.ebSlot AND b.ebHashBytes = ebTx.ebHashBytes AND b.txOffset = ebTx.txOffset"
    , "WHERE ebTx.ebSlot = ?1 AND ebTx.ebHashBytes = ?2"
    , "ORDER BY ebTx.txOffset ASC"
    ]

-- ** Promoting a point to the immutable partition

-- | Pin the point: certified, awaiting copy, never evicted. The durable record
-- that survives a crash before the copy lands. Also rescues a GC-marked row
-- (@status = 3@): a promotion arriving between mark and sweep unmarks it, and
-- the sweeper then skips it.
sql_pin_eb :: String
sql_pin_eb =
  "UPDATE ebs SET status = 1 WHERE ebSlot = ?1 AND ebHashBytes = ?2 AND status IN (0, 3)"

-- | Mark a pinned point as copied (evictable). A volatile write, so it goes
-- through the writer as @MarkCopied@ -- only ever strictly after the
-- immutable partition committed the point's closure.
sql_mark_as_copied :: String
sql_mark_as_copied =
  "UPDATE ebs SET status = 2 WHERE ebSlot = ?1 AND ebHashBytes = ?2 AND status = 1"

-- | Body row count and closure row count of the point in the volatile
-- partition, in one probe: the closure is complete iff both counts are equal
-- (and non-zero). Two range COUNTs on the shared PK.
sql_copy_completeness :: String
sql_copy_completeness =
  "SELECT (SELECT COUNT(*) FROM vol.ebTxs WHERE ebSlot = ?1 AND ebHashBytes = ?2),\n\
  \       (SELECT COUNT(*) FROM vol.ebTxBytes\n\
  \        WHERE ebSlot = ?1 AND ebHashBytes = ?2 AND filled = 1)\n\
  \"

-- | Copy the point's row. Only the columns the immutable partition has
-- ('sql_schema_imm'): completeness and GC status are volatile-only.
--
-- @OR IGNORE@: the mark that retires the pin is a separate volatile write, so
-- a crash in between leaves the point copied and still pinned, and the copier
-- repeats the copy on the next start.
sql_copy_insert_eb :: String
sql_copy_insert_eb =
  "INSERT OR IGNORE INTO ebs (ebSlot, ebHashBytes, ebBytesSize)\n\
  \SELECT ebSlot, ebHashBytes, ebBytesSize FROM vol.ebs\n\
  \WHERE ebSlot = ?1 AND ebHashBytes = ?2\n\
  \"

-- | Copy the point's body rows. @OR IGNORE@ for the same reason as
-- 'sql_copy_insert_eb'.
sql_copy_insert_ebTxs :: String
sql_copy_insert_ebTxs =
  "INSERT OR IGNORE INTO ebTxs (ebSlot, ebHashBytes, txOffset, txHashBytes, txBytesSize)\n\
  \SELECT ebSlot, ebHashBytes, txOffset, txHashBytes, txBytesSize FROM vol.ebTxs\n\
  \WHERE ebSlot = ?1 AND ebHashBytes = ?2\n\
  \"

-- | Copy the point's tx bytes: a range copy on the shared PK. @OR IGNORE@ for
-- the same reason as 'sql_copy_insert_eb'.
sql_copy_insert_ebTxBytes :: String
sql_copy_insert_ebTxBytes =
  "INSERT OR IGNORE INTO ebTxBytes (ebSlot, ebHashBytes, txOffset, filled, txBytes)\n\
  \SELECT ebSlot, ebHashBytes, txOffset, filled, txBytes FROM vol.ebTxBytes\n\
  \WHERE ebSlot = ?1 AND ebHashBytes = ?2 ORDER BY txOffset\n\
  \"

-- | The next points waiting to be copied into the immutable partition, at
-- most @?1@ of them: pinned ('sql_pin_eb') but not yet marked copied
-- ('sql_mark_as_copied'). Oldest first, and covered by @idx_ebs_pinned@.
--
-- The pin is the copy work list. It is in the database rather than in
-- memory, so a copy interrupted by a crash is simply still pending on the
-- next start.
sql_next_pinned_eb :: String
sql_next_pinned_eb =
  "SELECT ebSlot, ebHashBytes FROM vol.ebs WHERE status = 1 ORDER BY ebSlot LIMIT ?1"

-- ** Garbage collection of the volatile partition

-- | Whether a GC at slot @?1@ has any work, i.e. if there are any old volatile
-- points, already copied points or points left over from an interrupted GC
-- pass.
sql_gc_has_work :: String
sql_gc_has_work =
  "SELECT EXISTS (SELECT 1 FROM ebs WHERE status IN (0, 2, 3) AND ebSlot < ?1)"

-- | Mark for GC (@status = 3@) every point older than the GC frontier @?1@ that
-- is volatile (status 0) or already copied (status 2).
sql_gc_mark :: String
sql_gc_mark =
  "UPDATE ebs SET status = 3 WHERE status IN (0, 2) AND ebSlot < ?1"

-- | Up to @?1@ GC-marked points ready to sweep.
sql_sweep_pick_marked :: String
sql_sweep_pick_marked =
  "SELECT ebSlot, ebHashBytes FROM ebs WHERE status = 3 LIMIT ?1"

-- | Evict the point's 'ebTxBytes' rows.
sql_gc_ebTxBytes :: String
sql_gc_ebTxBytes =
  "DELETE FROM ebTxBytes WHERE ebSlot = ?1 AND ebHashBytes = ?2"

-- | Evict the point's 'ebTxs' rows.
sql_gc_ebTxs :: String
sql_gc_ebTxs =
  "DELETE FROM ebTxs WHERE ebSlot = ?1 AND ebHashBytes = ?2"

-- | Evict the point's 'ebs' row.
sql_gc_ebs :: String
sql_gc_ebs =
  "DELETE FROM ebs WHERE ebSlot = ?1 AND ebHashBytes = ?2"
