{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- | Queries on the ledger db
module Ouroboros.Consensus.Storage.LedgerDB.Query (
    acquireLDBReadView
  , getCurrent
  , getLedgerTablesAtFor
  , getPrevApplied
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict
import           Data.Set
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.LedgerDB.API (LedgerDBView (..),
                     LedgerDBView')
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
  , HeaderHash blk ~ HeaderHash l
  , IsLedger l
  , StandardHash l
  , HasTickedLedgerTables l
  )
  => Point blk
  -> LedgerTables l KeysMK
  -> StrictTVar m (DbChangelog l)
  -> LedgerBackingStore m l
  -> m (Either
        (PointNotFound blk)
        (LedgerTables l ValuesMK))
getLedgerTablesAtFor pt keys dbvar bstore = do
  lgrDb <- anchorlessChangelog <$> readTVarIO dbvar
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
     forall a b m blk.
     (IOLike m, LedgerSupportsProtocol blk, HasCallStack)
  => StaticEither b () (Point blk)
  -> StrictTVar m (DbChangelog' blk)
  -> LedgerDBLock m
  -> LedgerBackingStore' m blk
  -> STM m a
     -- ^ STM operation that we want to run in the same atomic block as the
     -- acquisition of the LedgerDB
  -> m ( a
       , StaticEither b
           (LedgerDBView' m blk)
           (Either
            (Point blk)
            (LedgerDBView' m blk))
       )
acquireLDBReadView p dbvar lock (LedgerBackingStore bs) stmAct =
  withReadLock lock $ do
    (a, ldb') <- atomically $ do
      (,) <$> stmAct <*> (anchorlessChangelog <$> readTVar dbvar)
    (a,) <$> case p of
      StaticLeft () -> StaticLeft <$> acquire ldb'
      StaticRight actualPoint -> StaticRight <$>
        case rollback actualPoint ldb' of
          Nothing    -> pure $ Left $ castPoint $ getTip $ anchor ldb'
          Just ldb'' -> Right <$> acquire ldb''
 where
   acquire ::
        AnchorlessDbChangelog' blk
     -> m (LedgerDBView' m blk)
   acquire l = do
     (slot, vh) <- bsValueHandle bs
     if slot == adcSlot l
       then pure $ LedgerDBView (LedgerBackingStoreValueHandle slot vh) l
       else error ("Critical error: Value handles are created at "
                   <> show slot
                   <> " while the db changelog is at "
                   <> show (adcSlot l)
                   <> ". There is either a race condition or a logic bug"
                  )
