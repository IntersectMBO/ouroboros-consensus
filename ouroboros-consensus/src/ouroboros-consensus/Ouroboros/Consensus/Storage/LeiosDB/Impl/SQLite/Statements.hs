{-# LANGUAGE RecordWildCards #-}

-- | Bundles of prepared statements used by LeiosDB's SQLite backend.
--
-- 'DB.Statement's are compiled SQL statements ready to be run on
-- an SQLite connection.
-- Consult [SQLite documentation](https://www.sqlite.org/c3ref/prepare.html)
-- for more details.
module Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Statements
  ( VolStmts (..)
  , prepareVolStmts
  , finalizeVolStmts
  , ImmStmts (..)
  , prepareImmStmts
  , finalizeImmStmts
  , GcStmts (..)
  , prepareGcStmts
  , finalizeGcStmts
  , CopierStmts (..)
  , prepareCopierStmts
  , finalizeCopierStmts
  , SweeperStmts (..)
  , prepareSweeperStmts
  , finalizeSweeperStmts
  ) where

import Control.Exception (mask_, onException)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.String (fromString)
import qualified Database.SQLite3.Direct as DB
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Primitives (dbFinalize, dbPrepare)
import Ouroboros.Consensus.Storage.LeiosDB.Impl.SQLite.Queries

-- | Prepare a bundle of statements with the given preparer. If preparing one
-- throws, the ones already prepared are finalized before the exception
-- propagates: a connection with an open statement refuses to close, so a
-- half-prepared bundle would leak the connection too.
preparingStmts ::
  HasCallStack => DB.Database -> ((String -> IO DB.Statement) -> IO a) -> IO a
preparingStmts db k = do
  preparedVar <- newIORef []
  let prep sql = mask_ $ do
        stmt <- dbPrepare db (fromString sql)
        modifyIORef' preparedVar (stmt :)
        pure stmt
  k prep `onException` (readIORef preparedVar >>= mapM_ dbFinalize)

-- | The copy statements, prepared on the copier's immutable connection --
-- main is the immutable file, the volatile file is ATTACHed as @vol@.
data CopierStmts = CopierStmts
  { ccCompleteness :: !DB.Statement
  -- ^ 'sql_copy_completeness'
  , ccInsertEb :: !DB.Statement
  -- ^ 'sql_copy_insert_eb'
  , ccInsertEbTxs :: !DB.Statement
  -- ^ 'sql_copy_insert_ebTxs'
  , ccInsertTxs :: !DB.Statement
  -- ^ 'sql_copy_insert_txs'
  }

-- | Prepare the copy statements on the copier's immutable connection, which
-- must already have the volatile partition ATTACHed as @vol@.
prepareCopierStmts :: HasCallStack => DB.Database -> IO CopierStmts
prepareCopierStmts db = preparingStmts db $ \prep -> do
  ccCompleteness <- prep sql_copy_completeness
  ccInsertEb <- prep sql_copy_insert_eb
  ccInsertEbTxs <- prep sql_copy_insert_ebTxs
  ccInsertTxs <- prep sql_copy_insert_txs
  pure CopierStmts{..}

finalizeCopierStmts :: CopierStmts -> IO ()
finalizeCopierStmts CopierStmts{..} = do
  dbFinalize ccCompleteness
  dbFinalize ccInsertEb
  dbFinalize ccInsertEbTxs
  dbFinalize ccInsertTxs

-- | The GC tick's prepared statements, prepared once on the writer's
-- volatile connection.
data GcStmts = GcStmts
  { gsHasWork :: !DB.Statement
  -- ^ 'sql_gc_has_work'
  , gsAddGcCandidatesTxs :: !DB.Statement
  -- ^ 'sql_gc_stage_marked'
  , gsMarkEbForGC :: !DB.Statement
  -- ^ 'sql_gc_mark'
  }

prepareGcStmts :: HasCallStack => DB.Database -> IO GcStmts
prepareGcStmts db = preparingStmts db $ \prep -> do
  gsHasWork <- prep sql_gc_has_work
  gsAddGcCandidatesTxs <- prep sql_gc_stage_marked
  gsMarkEbForGC <- prep sql_gc_mark
  pure GcStmts{..}

finalizeGcStmts :: GcStmts -> IO ()
finalizeGcStmts GcStmts{..} = do
  dbFinalize gsHasWork
  dbFinalize gsAddGcCandidatesTxs
  dbFinalize gsMarkEbForGC

-- | The sweep statements, prepared on the writer's volatile connection;
-- lifecycle mirrors 'CopierStmts'.
data SweeperStmts = SweeperStmts
  { swPickMarked :: !DB.Statement
  -- ^ 'sql_sweep_pick_marked'
  , swEvictEbTxs :: !DB.Statement
  -- ^ 'sql_gc_ebTxs'
  , swEvictMissingTxs :: !DB.Statement
  -- ^ 'sql_gc_missing_txs'
  , swEvictEbs :: !DB.Statement
  -- ^ 'sql_gc_ebs_by_hash'
  , swAnyMarked :: !DB.Statement
  -- ^ 'sql_sweep_any_marked'
  , swPickOrphans :: !DB.Statement
  -- ^ 'sql_sweep_pick_orphans'
  , swOrphanTxs :: !DB.Statement
  -- ^ 'sql_sweep_orphan_txs'
  , swPopOrphans :: !DB.Statement
  -- ^ 'sql_sweep_pop_orphans'
  , swHasUnstagedGcCandidates :: !DB.Statement
  -- ^ 'sql_has_unstaged_gc_candidates'
  , swUnstagedGcCandidatesPage :: !DB.Statement
  -- ^ 'sql_unstaged_gc_candidates_page'
  , swInsertGcCandidates :: !DB.Statement
  -- ^ 'sql_insert_gc_candidates'
  }

-- | Prepare the sweep statements on the writer's volatile connection.
prepareSweeperStmts :: HasCallStack => DB.Database -> IO SweeperStmts
prepareSweeperStmts db = preparingStmts db $ \prep -> do
  swPickMarked <- prep sql_sweep_pick_marked
  swEvictEbTxs <- prep sql_gc_ebTxs
  swEvictMissingTxs <- prep sql_gc_missing_txs
  swEvictEbs <- prep sql_gc_ebs_by_hash
  swAnyMarked <- prep sql_sweep_any_marked
  swPickOrphans <- prep sql_sweep_pick_orphans
  swOrphanTxs <- prep sql_sweep_orphan_txs
  swPopOrphans <- prep sql_sweep_pop_orphans
  swHasUnstagedGcCandidates <- prep sql_has_unstaged_gc_candidates
  swUnstagedGcCandidatesPage <- prep sql_unstaged_gc_candidates_page
  swInsertGcCandidates <- prep sql_insert_gc_candidates
  pure SweeperStmts{..}

finalizeSweeperStmts :: SweeperStmts -> IO ()
finalizeSweeperStmts SweeperStmts{..} = do
  dbFinalize swPickMarked
  dbFinalize swEvictEbTxs
  dbFinalize swEvictMissingTxs
  dbFinalize swEvictEbs
  dbFinalize swAnyMarked
  dbFinalize swPickOrphans
  dbFinalize swOrphanTxs
  dbFinalize swPopOrphans
  dbFinalize swHasUnstagedGcCandidates
  dbFinalize swUnstagedGcCandidatesPage
  dbFinalize swInsertGcCandidates

-- | Compiled SQL statements for the Volatile partition.
data VolStmts = VolStmts
  { stScanEbPoints :: !DB.Statement
  , stInsertEbPoint :: !DB.Statement
  , stLookupEbBody :: !DB.Statement
  , stInsertEbTxsRow :: !DB.Statement
  , stInitMissingCount :: !DB.Statement
  , stInsertTx :: !DB.Statement
  , stDecrMissingCount :: !DB.Statement
  , stInsertMissingTxs :: !DB.Statement
  , stDeleteMissingTxs :: !DB.Statement
  , stFindCompleteEbs :: !DB.Statement
  , stMarkNotifiedEbs :: !DB.Statement
  , stMarkPointNotified :: !DB.Statement
  , stBatchRetrieveTxs :: !DB.Statement
  , stFilterMissingTxs :: !DB.Statement
  , stLookupEbClosure :: !DB.Statement
  , stScanCompleteEbsSince :: !DB.Statement
  }

-- | Prepared statements of the fallback reads into the immutable partition.
-- The partitions share one schema, so these are the volatile SQL strings
-- prepared against the immutable connection (plus the presence probe).
-- Lifecycle mirrors 'VolStmts': prepared at open time, finalized in 'close'
-- before their connection.
data ImmStmts = ImmStmts
  { immStLookupEbBody :: !DB.Statement
  , immStLookupEbClosure :: !DB.Statement
  , immStBatchRetrieveTxs :: !DB.Statement
  , immStFilterPresent :: !DB.Statement
  }

prepareImmStmts :: HasCallStack => DB.Database -> IO ImmStmts
prepareImmStmts db = preparingStmts db $ \prep -> do
  immStLookupEbBody <- prep sql_lookup_ebBodies
  immStLookupEbClosure <- prep sql_lookup_eb_closure
  immStBatchRetrieveTxs <- prep sql_retrieve_from_ebTxs_json
  immStFilterPresent <- prep sql_imm_filter_present
  pure ImmStmts{..}

-- | Same use-after-free discipline as 'finalizeVolStmts'.
finalizeImmStmts :: ImmStmts -> IO ()
finalizeImmStmts ImmStmts{..} = do
  dbFinalize immStLookupEbBody
  dbFinalize immStLookupEbClosure
  dbFinalize immStBatchRetrieveTxs
  dbFinalize immStFilterPresent

-- | Prepare every statement 'VolStmts' names. Order is not observable.
prepareVolStmts :: DB.Database -> IO VolStmts
prepareVolStmts db = preparingStmts db $ \prep -> do
  stScanEbPoints <- prep sql_scan_ebs
  stInsertEbPoint <- prep sql_insert_eb
  stLookupEbBody <- prep sql_lookup_ebBodies
  stInsertEbTxsRow <- prep sql_insert_ebBody
  stInitMissingCount <- prep sql_init_missing_tx_count
  stInsertTx <- prep sql_insert_tx
  stDecrMissingCount <- prep sql_decrement_missing_tx_count
  stInsertMissingTxs <- prep sql_insert_missing_txs
  stDeleteMissingTxs <- prep sql_delete_missing_txs
  stFindCompleteEbs <- prep sql_find_complete_ebs
  stMarkNotifiedEbs <- prep sql_mark_notified_ebs
  stMarkPointNotified <- prep sql_mark_point_notified
  stBatchRetrieveTxs <- prep sql_retrieve_from_ebTxs_json
  stFilterMissingTxs <- prep sql_filter_missing_txs_json
  stLookupEbClosure <- prep sql_lookup_eb_closure
  stScanCompleteEbsSince <- prep sql_scan_complete_ebs_since
  pure VolStmts{..}

-- | Finalise every statement in 'VolStmts'. Called from 'close' immediately
-- before 'sqlite3_close_v2', on the connection's owner thread.
finalizeVolStmts :: VolStmts -> IO ()
finalizeVolStmts VolStmts{..} = do
  dbFinalize stScanEbPoints
  dbFinalize stInsertEbPoint
  dbFinalize stLookupEbBody
  dbFinalize stInsertEbTxsRow
  dbFinalize stInitMissingCount
  dbFinalize stInsertTx
  dbFinalize stDecrMissingCount
  dbFinalize stInsertMissingTxs
  dbFinalize stDeleteMissingTxs
  dbFinalize stFindCompleteEbs
  dbFinalize stMarkNotifiedEbs
  dbFinalize stMarkPointNotified
  dbFinalize stBatchRetrieveTxs
  dbFinalize stFilterMissingTxs
  dbFinalize stLookupEbClosure
  dbFinalize stScanCompleteEbsSince
