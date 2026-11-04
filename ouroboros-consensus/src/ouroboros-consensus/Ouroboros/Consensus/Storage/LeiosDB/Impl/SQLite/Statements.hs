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
  , ReadStmts (..)
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
  { ccClosure :: !DB.Statement
  -- ^ 'sql_copy_closure'
  , ccInsertEb :: !DB.Statement
  -- ^ 'sql_copy_insert_eb'
  }

-- | Prepare the copy statements on the copier's immutable connection, which
-- must already have the volatile partition ATTACHed as @vol@.
prepareCopierStmts :: HasCallStack => DB.Database -> IO CopierStmts
prepareCopierStmts db = preparingStmts db $ \prep -> do
  ccClosure <- prep sql_copy_closure
  ccInsertEb <- prep sql_copy_insert_eb
  pure CopierStmts{..}

finalizeCopierStmts :: CopierStmts -> IO ()
finalizeCopierStmts CopierStmts{..} = do
  dbFinalize ccClosure
  dbFinalize ccInsertEb

-- | The sweep statements, prepared on the writer's volatile connection;
-- lifecycle mirrors 'CopierStmts'.
data SweeperStmts = SweeperStmts
  { swSweep :: !DB.Statement
  -- ^ 'sql_gc_sweep'
  , swFrontierEmpty :: !DB.Statement
  -- ^ 'sql_gc_frontier_empty'
  }

-- | Prepare the sweep statements on the writer's volatile connection.
prepareSweeperStmts :: HasCallStack => DB.Database -> IO SweeperStmts
prepareSweeperStmts db = preparingStmts db $ \prep -> do
  swSweep <- prep sql_gc_sweep
  swFrontierEmpty <- prep sql_gc_frontier_empty
  pure SweeperStmts{..}

finalizeSweeperStmts :: SweeperStmts -> IO ()
finalizeSweeperStmts SweeperStmts{..} = do
  dbFinalize swSweep
  dbFinalize swFrontierEmpty

-- | Compiled SQL statements for the Volatile partition.
data VolStmts = VolStmts
  { stScanEbPoints :: !DB.Statement
  , stInsertEbPoint :: !DB.Statement
  , stInsertEbCopy :: !DB.Statement
  , stHasPoint :: !DB.Statement
  , stInsertEbBody :: !DB.Statement
  , stLookupEbRows :: !DB.Statement
  , stScanEbsSince :: !DB.Statement
  , stReads :: !ReadStmts
  }

-- | The statements of the reads, prepared on both partitions.
data ReadStmts = ReadStmts
  { rsLookupEbBody :: !DB.Statement
  , rsLookupEbIndex :: !DB.Statement
  , rsLookupEbClosure :: !DB.Statement
  }

prepareReadStmts :: (String -> IO DB.Statement) -> IO ReadStmts
prepareReadStmts prep = do
  rsLookupEbBody <- prep sql_lookup_eb_body
  rsLookupEbIndex <- prep sql_lookup_eb_index
  rsLookupEbClosure <- prep sql_lookup_eb_closure
  pure ReadStmts{..}

finalizeReadStmts :: ReadStmts -> IO ()
finalizeReadStmts ReadStmts{..} = do
  dbFinalize rsLookupEbBody
  dbFinalize rsLookupEbIndex
  dbFinalize rsLookupEbClosure

-- | Prepared statements of the fallback reads into the immutable partition.
-- The partitions share one schema, so these are the volatile SQL strings
-- prepared against the immutable connection.
-- Lifecycle mirrors 'VolStmts': prepared at open time, finalized in 'close'
-- before their connection.
newtype ImmStmts = ImmStmts
  { immReads :: ReadStmts
  }

prepareImmStmts :: HasCallStack => DB.Database -> IO ImmStmts
prepareImmStmts db = preparingStmts db $ \prep -> ImmStmts <$> prepareReadStmts prep

-- | Same use-after-free discipline as 'finalizeVolStmts'.
finalizeImmStmts :: ImmStmts -> IO ()
finalizeImmStmts = finalizeReadStmts . immReads

-- | Prepare every statement 'VolStmts' names. Order is not observable.
prepareVolStmts :: DB.Database -> IO VolStmts
prepareVolStmts db = preparingStmts db $ \prep -> do
  stScanEbPoints <- prep sql_scan_ebs
  stInsertEbPoint <- prep sql_insert_eb
  stInsertEbCopy <- prep sql_insert_eb_copy
  stHasPoint <- prep sql_has_point
  stInsertEbBody <- prep sql_insert_eb_body
  stLookupEbRows <- prep sql_lookup_eb_rows
  stScanEbsSince <- prep sql_scan_ebs_since
  stReads <- prepareReadStmts prep
  pure VolStmts{..}

-- | Finalise every statement in 'VolStmts'. Called from 'close' immediately
-- before 'sqlite3_close_v2', on the connection's owner thread.
finalizeVolStmts :: VolStmts -> IO ()
finalizeVolStmts VolStmts{..} = do
  dbFinalize stScanEbPoints
  dbFinalize stInsertEbPoint
  dbFinalize stInsertEbCopy
  dbFinalize stHasPoint
  dbFinalize stInsertEbBody
  dbFinalize stLookupEbRows
  dbFinalize stScanEbsSince
  finalizeReadStmts stReads
