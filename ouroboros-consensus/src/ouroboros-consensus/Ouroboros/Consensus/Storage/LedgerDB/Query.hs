{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- | Queries on the ledger db
module Ouroboros.Consensus.Storage.LedgerDB.Query (
    acquireLDBReadView
  , acquireLDBReadView'
  , getCurrent
  , getLedgerTablesAtFor
  , getPrevApplied
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict
import           Data.Set
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog.Query
import           Ouroboros.Consensus.Storage.LedgerDB.Lock
import           Ouroboros.Consensus.Storage.LedgerDB.ReadsKeySets
import           Ouroboros.Consensus.Storage.LedgerDB.Types
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike (IOLike)

getPrevApplied :: IOLike m => LedgerDBState m blk -> STM m (Set (RealPoint blk))
getPrevApplied = readTVar . varPrevApplied

getCurrent :: IOLike m => LedgerDBState m blk -> STM m (DbChangelog' blk)
getCurrent = readTVar . ldbChangelog

-- | Read and forward the values up to the given point on the chain.
getLedgerTablesAtFor ::
  ( IOLike m
  , LedgerSupportsProtocol blk
  , StandardHash (ExtLedgerState blk)
  )
  => Point blk
  -> LedgerTables (ExtLedgerState blk) KeysMK
  -> StrictTVar m (DbChangelog' blk)
  -> LedgerBackingStore m (ExtLedgerState blk)
  -> m (Either
        (PointNotFound blk)
        (LedgerTables (ExtLedgerState blk) ValuesMK))
getLedgerTablesAtFor pt keys dbvar bstore = do
  lgrDb <- readTVarIO dbvar
  case rollback pt lgrDb of
    Nothing -> pure $ Left $ PointNotFound pt
    Just l  -> do
      eValues <-
        getLedgerTablesFor l keys (readKeySets bstore)
      case eValues of
        Right v -> pure $ Right v
        Left _  -> getLedgerTablesAtFor pt keys dbvar bstore

-- | Given a point (or Left () for the tip), acquire both a value handle and a
-- db changelog at the requested point. Holds a read lock while doing so.
acquireLDBReadView ::
     (IOLike m, LedgerSupportsProtocol blk)
  => StaticEither b () (Point blk)
  -> StrictTVar m (DbChangelog' blk)
  -> LedgerDBLock m
  -> LedgerBackingStore m (ExtLedgerState blk)
  -> m (StaticEither b
        (LedgerBackingStoreValueHandle m (ExtLedgerState blk), DbChangelog' blk)
        (Either
          (Point blk)
          (LedgerBackingStoreValueHandle m (ExtLedgerState blk), DbChangelog' blk))
       )
acquireLDBReadView p ldb lock bstore =
  snd <$> acquireLDBReadView' p ldb lock bstore (pure ())


-- | Same as 'acquireLDBReadView' but with another argument which will be
-- executed in the same atomic block.
acquireLDBReadView' ::
     forall a b m blk.
     (IOLike m, LedgerSupportsProtocol blk, HasCallStack)
  => StaticEither b () (Point blk)
  -> StrictTVar m (DbChangelog' blk)
  -> LedgerDBLock m
  -> LedgerBackingStore m (ExtLedgerState blk)
  -> STM m a
     -- ^ STM operation that we want to run in the same atomic block as the
     -- acquisition of the LedgerDB
  -> m ( a
       , StaticEither b
           (LedgerBackingStoreValueHandle m (ExtLedgerState blk), DbChangelog' blk)
           (Either
            (Point blk)
            (LedgerBackingStoreValueHandle m (ExtLedgerState blk), DbChangelog' blk))
       )
acquireLDBReadView' p dbvar lock (LedgerBackingStore bs) stmAct =
  withReadLock lock $ do
    (a, ldb') <- atomically $ do
      (,) <$> stmAct <*> readTVar dbvar
    (a,) <$> case p of
      StaticLeft () -> StaticLeft <$> acquire ldb'
      StaticRight actualPoint -> StaticRight <$>
        case rollback actualPoint ldb' of
          Nothing    -> pure $ Left $ castPoint $ getTip $ anchor ldb'
          Just ldb'' -> Right <$> acquire ldb''
 where
   acquire ::
        DbChangelog' blk
     -> m (LedgerBackingStoreValueHandle m (ExtLedgerState blk), DbChangelog' blk)
   acquire l = do
     (slot, vh) <- bsValueHandle bs
     if slot == getTipSlot (changelogLastFlushedState l)
       then pure (LedgerBackingStoreValueHandle slot vh, l)
       else error ("Critical error: Value handles are created at "
                   <> show slot
                   <> " while the db changelog is at "
                   <> show (getTipSlot $ changelogLastFlushedState l)
                   <> ". There is either a race condition or a logic bug"
                  )
