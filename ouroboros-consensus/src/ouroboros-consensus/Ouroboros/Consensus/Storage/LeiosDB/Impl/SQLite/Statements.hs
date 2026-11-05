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
  , ccInsertEbTxBytes :: !DB.Statement
  -- ^ 'sql_copy_insert_ebTxBytes'
  }

-- | Prepare the copy statements on the copier's immutable connection, which
-- must already have the volatile partition ATTACHed as @vol@.
prepareCopierStmts :: HasCallStack => DB.Database -> IO CopierStmts
prepareCopierStmts db = preparingStmts db $ \prep -> do
  ccCompleteness <- prep sql_copy_completeness
  ccInsertEb <- prep sql_copy_insert_eb
  ccInsertEbTxs <- prep sql_copy_insert_ebTxs
  ccInsertEbTxBytes <- prep sql_copy_insert_ebTxBytes
  pure CopierStmts{..}

finalizeCopierStmts :: CopierStmts -> IO ()
finalizeCopierStmts CopierStmts{..} = do
  dbFinalize ccCompleteness
  dbFinalize ccInsertEb
  dbFinalize ccInsertEbTxs
  dbFinalize ccInsertEbTxBytes

-- | The sweep statements, prepared on the writer's volatile connection;
-- lifecycle mirrors 'CopierStmts'.
data SweeperStmts = SweeperStmts
  { swPick :: !DB.Statement
  -- ^ 'sql_sweep_pick'
  , swEvictEbTxBytes :: !DB.Statement
  -- ^ 'sql_gc_ebTxBytes'
  , swEvictEbTxs :: !DB.Statement
  -- ^ 'sql_gc_ebTxs'
  , swEvictEbs :: !DB.Statement
  -- ^ 'sql_gc_ebs'
  }

-- | Prepare the sweep statements on the writer's volatile connection.
prepareSweeperStmts :: HasCallStack => DB.Database -> IO SweeperStmts
prepareSweeperStmts db = preparingStmts db $ \prep -> do
  swPick <- prep sql_sweep_pick
  swEvictEbTxBytes <- prep sql_gc_ebTxBytes
  swEvictEbTxs <- prep sql_gc_ebTxs
  swEvictEbs <- prep sql_gc_ebs
  pure SweeperStmts{..}

finalizeSweeperStmts :: SweeperStmts -> IO ()
finalizeSweeperStmts SweeperStmts{..} = do
  dbFinalize swPick
  dbFinalize swEvictEbTxBytes
  dbFinalize swEvictEbTxs
  dbFinalize swEvictEbs

-- | Compiled SQL statements for the Volatile partition.
data VolStmts = VolStmts
  { stScanEbPoints :: !DB.Statement
  , stInsertEbPoint :: !DB.Statement
  , stLookupEbBody :: !DB.Statement
  , stInsertEbTxsRow :: !DB.Statement
  , stPreallocEbTxBytes :: !DB.Statement
  , stInitMissingCount :: !DB.Statement
  , stFillEbTxBytes :: !DB.Statement
  , stHeldOffsets :: !DB.Statement
  , stDeclaredTxSize :: !DB.Statement
  , stConfirmedTxOffsets :: !DB.Statement
  -- ^ 'sql_confirmed_tx_offsets_json' for volatile presence receipts.
  , stDecrMissingCount :: !DB.Statement
  , stBatchRetrieveTxs :: !DB.Statement
  , stLookupEbClosure :: !DB.Statement
  , stScanCompleteEbsSince :: !DB.Statement
  }

-- | Prepared statements of the fallback reads into the immutable partition.
-- The partitions share one schema, so these are the volatile SQL strings
-- prepared against the immutable connection.
-- Lifecycle mirrors 'VolStmts': prepared at open time, finalized in 'close'
-- before their connection.
data ImmStmts = ImmStmts
  { immStLookupEbBody :: !DB.Statement
  , immStLookupEbClosure :: !DB.Statement
  , immStBatchRetrieveTxs :: !DB.Statement
  , immStConfirmedTxOffsets :: !DB.Statement
  -- ^ 'sql_confirmed_tx_offsets_json' for immutable presence receipts.
  }

prepareImmStmts :: HasCallStack => DB.Database -> IO ImmStmts
prepareImmStmts db = preparingStmts db $ \prep -> do
  immStLookupEbBody <- prep sql_lookup_ebBodies
  immStLookupEbClosure <- prep sql_lookup_eb_closure
  immStBatchRetrieveTxs <- prep sql_retrieve_from_ebTxs_json
  immStConfirmedTxOffsets <- prep sql_confirmed_tx_offsets_json
  pure ImmStmts{..}

-- | Same use-after-free discipline as 'finalizeVolStmts'.
finalizeImmStmts :: ImmStmts -> IO ()
finalizeImmStmts ImmStmts{..} = do
  dbFinalize immStLookupEbBody
  dbFinalize immStLookupEbClosure
  dbFinalize immStBatchRetrieveTxs
  dbFinalize immStConfirmedTxOffsets

-- | Prepare every statement 'VolStmts' names. Order is not observable.
prepareVolStmts :: DB.Database -> IO VolStmts
prepareVolStmts db = preparingStmts db $ \prep -> do
  stScanEbPoints <- prep sql_scan_ebs
  stInsertEbPoint <- prep sql_insert_eb
  stLookupEbBody <- prep sql_lookup_ebBodies
  stInsertEbTxsRow <- prep sql_insert_ebBody
  stPreallocEbTxBytes <- prep sql_prealloc_ebTxBytes
  stInitMissingCount <- prep sql_init_missing_tx_count
  stFillEbTxBytes <- prep sql_fill_ebTxBytes
  stHeldOffsets <- prep sql_held_offsets
  stDeclaredTxSize <- prep sql_declared_tx_size
  stConfirmedTxOffsets <- prep sql_confirmed_tx_offsets_json
  stDecrMissingCount <- prep sql_decrement_missing_tx_count
  stBatchRetrieveTxs <- prep sql_retrieve_from_ebTxs_json
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
  dbFinalize stPreallocEbTxBytes
  dbFinalize stInitMissingCount
  dbFinalize stFillEbTxBytes
  dbFinalize stHeldOffsets
  dbFinalize stDeclaredTxSize
  dbFinalize stConfirmedTxOffsets
  dbFinalize stDecrMissingCount
  dbFinalize stBatchRetrieveTxs
  dbFinalize stLookupEbClosure
  dbFinalize stScanCompleteEbsSince
