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
  { ccHasClosure :: !DB.Statement
  -- ^ 'sql_copy_has_closure'
  , ccInsertEb :: !DB.Statement
  -- ^ 'sql_copy_insert_eb'
  , ccInsertEbBody :: !DB.Statement
  -- ^ 'sql_copy_insert_ebBody'
  , ccInsertEbClosures :: !DB.Statement
  -- ^ 'sql_copy_insert_ebClosures'
  }

-- | Prepare the copy statements on the copier's immutable connection, which
-- must already have the volatile partition ATTACHed as @vol@.
prepareCopierStmts :: HasCallStack => DB.Database -> IO CopierStmts
prepareCopierStmts db = preparingStmts db $ \prep -> do
  ccHasClosure <- prep sql_copy_has_closure
  ccInsertEb <- prep sql_copy_insert_eb
  ccInsertEbBody <- prep sql_copy_insert_ebBody
  ccInsertEbClosures <- prep sql_copy_insert_ebClosures
  pure CopierStmts{..}

finalizeCopierStmts :: CopierStmts -> IO ()
finalizeCopierStmts CopierStmts{..} = do
  dbFinalize ccHasClosure
  dbFinalize ccInsertEb
  dbFinalize ccInsertEbBody
  dbFinalize ccInsertEbClosures

-- | The GC tick's prepared statements, prepared once on the writer's
-- volatile connection.
data GcStmts = GcStmts
  { gsHasWork :: !DB.Statement
  -- ^ 'sql_gc_has_work'
  , gsMarkEbForGC :: !DB.Statement
  -- ^ 'sql_gc_mark'
  }

prepareGcStmts :: HasCallStack => DB.Database -> IO GcStmts
prepareGcStmts db = preparingStmts db $ \prep -> do
  gsHasWork <- prep sql_gc_has_work
  gsMarkEbForGC <- prep sql_gc_mark
  pure GcStmts{..}

finalizeGcStmts :: GcStmts -> IO ()
finalizeGcStmts GcStmts{..} = do
  dbFinalize gsHasWork
  dbFinalize gsMarkEbForGC

-- | The sweep statements, prepared on the writer's volatile connection;
-- lifecycle mirrors 'CopierStmts'.
data SweeperStmts = SweeperStmts
  { swPickMarked :: !DB.Statement
  -- ^ 'sql_sweep_pick_marked'
  , swEvictEbBodies :: !DB.Statement
  -- ^ 'sql_gc_ebBodies'
  , swEvictEbClosures :: !DB.Statement
  -- ^ 'sql_gc_ebClosures'
  , swEvictEbs :: !DB.Statement
  -- ^ 'sql_gc_ebs_by_hash'
  }

-- | Prepare the sweep statements on the writer's volatile connection.
prepareSweeperStmts :: HasCallStack => DB.Database -> IO SweeperStmts
prepareSweeperStmts db = preparingStmts db $ \prep -> do
  swPickMarked <- prep sql_sweep_pick_marked
  swEvictEbBodies <- prep sql_gc_ebBodies
  swEvictEbClosures <- prep sql_gc_ebClosures
  swEvictEbs <- prep sql_gc_ebs_by_hash
  pure SweeperStmts{..}

finalizeSweeperStmts :: SweeperStmts -> IO ()
finalizeSweeperStmts SweeperStmts{..} = do
  dbFinalize swPickMarked
  dbFinalize swEvictEbBodies
  dbFinalize swEvictEbClosures
  dbFinalize swEvictEbs

-- | Compiled SQL statements for the Volatile partition.
data VolStmts = VolStmts
  { stScanEbPoints :: !DB.Statement
  , stInsertEbPoint :: !DB.Statement
  , stInsertEbBody :: !DB.Statement
  , stInsertEbClosure :: !DB.Statement
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
  stInsertEbBody <- prep sql_insert_eb_body
  stInsertEbClosure <- prep sql_insert_eb_closure
  stReads <- prepareReadStmts prep
  pure VolStmts{..}

-- | Finalise every statement in 'VolStmts'. Called from 'close' immediately
-- before 'sqlite3_close_v2', on the connection's owner thread.
finalizeVolStmts :: VolStmts -> IO ()
finalizeVolStmts VolStmts{..} = do
  dbFinalize stScanEbPoints
  dbFinalize stInsertEbPoint
  dbFinalize stInsertEbBody
  dbFinalize stInsertEbClosure
  finalizeReadStmts stReads
