{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
-- | Queries on the ledger db

module Ouroboros.Consensus.Storage.LedgerDB.Query (
    acquireLDBReadView
  , getCurrent
  , getLedgerTablesAtFor
  , getPrevApplied
  , getStatistics
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict
import           Data.Monoid (Sum (..))
import           Data.Set
import           Data.SOP (K (K))
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables.DiffSeq
import           Ouroboros.Consensus.Storage.LedgerDB.API (LedgerDBView (..),
                     LedgerDBView')
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.Config
                     (DiskPolicy (onDiskQueryBatchSize))
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
  , HasLedgerTables l
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
  -> DiskPolicy
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
acquireLDBReadView p dbvar lock bs policy stmAct =
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
     vh <- bsValueHandle bs
     if bsvhAtSlot vh == adcLastFlushedSlot l
       then pure $ LedgerDBView vh l (onDiskQueryBatchSize policy)
       else error ("Critical error: Value handles are created at "
                   <> show (bsvhAtSlot vh)
                   <> " while the db changelog is at "
                   <> show (adcLastFlushedSlot l)
                   <> ". There is either a race condition or a logic bug"
                  )

-- | Obtain statistics for a combination of backing store value handle and
-- changelog.
getStatistics ::
     (Monad m, IsLedger l, HasLedgerTables l)
  => LedgerDBView m l
  -> m Statistics
getStatistics (LedgerDBView lbsvh dblog _) = do
    Statistics{sequenceNumber = seqNo', numEntries = n} <- bsvhStat lbsvh
    if seqNo /= seqNo' then
      error $ show (seqNo, seqNo')
    else
      pure $ Statistics {
          sequenceNumber = getTipSlot $ K dblog
        , numEntries     = n + nInserts - nDeletes
        }
  where
    diffs = adcDiffs  dblog
    seqNo = adcLastFlushedSlot dblog

    nInserts = getSum
            $ ltcollapse
            $ ltmap (K2 . numInserts . getSeqDiffMK)
              diffs
    nDeletes = getSum
            $ ltcollapse
            $ ltmap (K2 . numDeletes . getSeqDiffMK)
              diffs
