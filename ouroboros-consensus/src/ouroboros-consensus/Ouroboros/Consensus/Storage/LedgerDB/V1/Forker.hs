{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.V1.Forker
  ( ForkerEnv (..)
  , closeForkerEnv
  , implForkerCommit
  , implForkerGetLedgerState
  , implForkerPush
  , implForkerRangeReadTables
  , implForkerReadStatistics
  , implForkerReadTables
  ) where

import qualified Control.Monad as Monad
import Control.ResourceRegistry
import Control.Tracer
import Data.Functor.Contravariant ((>$<))
import qualified Data.Map.Strict as Map
import Data.Semigroup
import qualified Data.Set as Set
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Storage.LedgerDB.Args
import Ouroboros.Consensus.Storage.LedgerDB.Forker as Forker
import Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API as BackingStore
import Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog
import Ouroboros.Consensus.Storage.LedgerDB.V1.DiffSeq
  ( numDeletes
  , numInserts
  )
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.DiffSeq as DS
import Ouroboros.Consensus.Storage.LedgerDB.V1.Lock
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Network.AnchoredSeq as AS

{-------------------------------------------------------------------------------
  Forkers
-------------------------------------------------------------------------------}

data ForkerEnv m l blk = ForkerEnv
  { foeBackingStoreValueHandle ::
      !( StrictMVar
           m
           ( Either
               (LedgerDBLock m, LedgerBackingStore m l, ResourceRegistry m)
               (ResourceKey m, LedgerBackingStoreValueHandle m l)
           )
       )
  -- ^ Either the ingredients to create a value handle or a value handle, i.e. a
  -- local, consistent view of backing store. Use 'getValueHandle' to promote
  -- this if needed.
  , foeChangelog :: !(StrictTVar m (DbChangelog l))
  -- ^ In memory db changelog, 'foeBackingStoreValueHandle' must refer to
  -- the anchor of this changelog.
  , foeSwitchVar :: !(StrictTVar m (DbChangelog l))
  -- ^ The same 'StrictTVar' as 'ldbChangelog'
  --
  -- The anchor of this and 'foeChangelog' might get out of sync if diffs are
  -- flushed, but 'forkerCommit' will take care of this.
  , foeTracer :: !(Tracer m TraceForkerEvent)
  -- ^ Config
  , foeWasCommitted :: !(StrictTVar m Bool)
  }
  deriving Generic

deriving instance
  ( IOLike m
  , LedgerSupportsProtocol blk
  , NoThunks (l EmptyMK)
  , NoThunks (TxIn l)
  , NoThunks (TxOut l)
  ) =>
  NoThunks (ForkerEnv m l blk)

{-------------------------------------------------------------------------------
  Close
-------------------------------------------------------------------------------}

closeForkerEnv :: IOLike m => ForkerEnv m l blk -> m ()
closeForkerEnv ForkerEnv{foeBackingStoreValueHandle} = do
  -- If this MVar is empty, we already closed the forker in the past so do nothing.
  tryTakeMVar foeBackingStoreValueHandle >>= \case
    -- We already closed the forker in the past, so do nothing.
    Nothing -> pure ()
    -- Release the read lock.
    Just (Left (lock, _, _)) -> atomically $ unsafeReleaseReadAccess lock
    -- Release the value handle
    Just (Right (bsvh, _)) -> Monad.void $ release bsvh

{-------------------------------------------------------------------------------
  Acquiring consistent views
-------------------------------------------------------------------------------}

-- | Get the value handle in a forker, creating it on demand if this is the
-- first time we access the tables.
getValueHandle :: (GetTip l, IOLike m) => ForkerEnv m l blk -> m (LedgerBackingStoreValueHandle m l)
getValueHandle ForkerEnv{foeBackingStoreValueHandle, foeChangelog} =
  modifyMVar foeBackingStoreValueHandle $ \case
    r@(Right (_, bsvh)) -> pure (r, bsvh)
    Left (l, bs, rr) -> do
      (k, bsvh) <- allocate rr (\_ -> bsValueHandle bs) bsvhClose
      dblogSlot <- getTipSlot . changelogLastFlushedState <$> readTVarIO foeChangelog
      if bsvhAtSlot bsvh == dblogSlot
        then do
          -- Release the read lock acquired when opening the forker.
          atomically $ unsafeReleaseReadAccess l
          pure (Right (k, bsvh), bsvh)
        else
          release k
            >> error
              ( "Critical error: Value handles are created at "
                  <> show (bsvhAtSlot bsvh)
                  <> " while the db changelog is at "
                  <> show dblogSlot
                  <> ". There is either a race condition or a logic bug"
              )

implForkerReadTables ::
  (IOLike m, HasLedgerTables l, GetTip l) =>
  ForkerEnv m l blk ->
  LedgerTables l KeysMK ->
  m (LedgerTables l ValuesMK)
implForkerReadTables env ks =
  encloseTimedWith (ForkerReadTables >$< foeTracer env) $ do
    chlog <- readTVarIO (foeChangelog env)
    bsvh <- getValueHandle env
    unfwd <- readKeySetsWith bsvh (changelogLastFlushedState chlog) ks
    case forwardTableKeySets chlog unfwd of
      Left _err -> error "impossible!"
      Right vs -> pure vs

implForkerRangeReadTables ::
  (IOLike m, GetTip l, HasLedgerTables l) =>
  QueryBatchSize ->
  ForkerEnv m l blk ->
  RangeQueryPrevious l ->
  m (LedgerTables l ValuesMK, Maybe (TxIn l))
implForkerRangeReadTables qbs env rq0 =
  encloseTimedWith (ForkerRangeReadTables >$< foeTracer env) $ do
    ldb <- readTVarIO $ foeChangelog env
    let
      -- Get the differences without the keys that are greater or equal
      -- than the maximum previously seen key.
      diffs =
        maybe
          id
          (ltliftA2 doDropLTE)
          (BackingStore.rqPrev rq)
          $ ltmap prj
          $ changelogDiffs ldb
      -- (1) Ensure that we never delete everything read from disk (ie if
      --     our result is non-empty then it contains something read from
      --     disk, as we only get an empty result if we reached the end of
      --     the table).
      --
      -- (2) Also, read one additional key, which we will not include in
      --     the result but need in order to know which in-memory
      --     insertions to include.
      maxDeletes = ltcollapse $ ltmap (K2 . numDeletesDiffMK) diffs
      nrequested = 1 + max (BackingStore.rqCount rq) (1 + maxDeletes)

    let st = changelogLastFlushedState ldb
    bsvh <- getValueHandle env
    (values, mx) <- BackingStore.bsvhRangeRead bsvh st (rq{BackingStore.rqCount = nrequested})
    let res = ltliftA2 (doFixupReadResult nrequested) diffs values
    pure (res, mx)
 where
  rq = BackingStore.RangeQuery rq1 (fromIntegral $ defaultQueryBatchSize qbs)

  rq1 = case rq0 of
    NoPreviousQuery -> Nothing
    PreviousQueryWasFinal -> Just (LedgerTables $ KeysMK Set.empty)
    PreviousQueryWasUpTo k -> Just (LedgerTables $ KeysMK $ Set.singleton k)

  prj ::
    (Ord k, Eq v) =>
    SeqDiffMK k v ->
    DiffMK k v
  prj (SeqDiffMK sq) = DiffMK (DS.fromAntiDiff $ DS.cumulativeDiff sq)

  -- Remove all diff elements that are <= to the greatest given key
  doDropLTE ::
    Ord k =>
    KeysMK k v ->
    DiffMK k v ->
    DiffMK k v
  doDropLTE (KeysMK ks) (DiffMK ds) =
    DiffMK $
      case Set.lookupMax ks of
        Nothing -> ds
        Just k -> Diff.filterWithKeyOnly (> k) ds

  -- NOTE: this is counting the deletions wrt disk because deletions of values
  -- created along the diffs will have been collapsed to the empty diff.
  numDeletesDiffMK :: DiffMK k v -> Int
  numDeletesDiffMK (DiffMK d) =
    getSum $ Diff.foldMapDelta (Sum . oneIfDel) d
   where
    oneIfDel x = case x of
      Diff.Delete -> 1
      Diff.Insert _ -> 0

  -- INVARIANT: nrequested > 0
  --
  -- (1) if we reached the end of the store, then simply yield the given diff
  --     applied to the given values
  -- (2) otherwise, the readset must be non-empty, since 'rqCount' is positive
  -- (3) remove the greatest read key
  -- (4) remove all diff elements that are >= the greatest read key
  -- (5) apply the remaining diff
  -- (6) (the greatest read key will be the first fetched if the yield of this
  --     result is next passed as 'rqPrev')
  --
  -- Note that if the in-memory changelog contains the greatest key, then
  -- we'll return that in step (1) above, in which case the next passed
  -- 'rqPrev' will contain it, which will cause 'doDropLTE' to result in an
  -- empty diff, which will result in an entirely empty range query result,
  -- which is the termination case.
  doFixupReadResult ::
    Ord k =>
    Int ->
    -- \^ Number of requested keys from the backing store.
    DiffMK k v ->
    -- \^ Differences that will be applied to the values read from the backing
    -- store.
    ValuesMK k v ->
    -- \^ Values read from the backing store. The number of values read should
    -- be at most @nrequested@.
    ValuesMK k v
  doFixupReadResult
    nrequested
    (DiffMK ds)
    (ValuesMK vs) =
      let includingAllKeys =
            Diff.applyDiff vs ds
          definitelyNoMoreToFetch = Map.size vs < nrequested
       in ValuesMK $
            case Map.maxViewWithKey vs of
              Nothing ->
                if definitelyNoMoreToFetch
                  then includingAllKeys
                  else error $ "Size of values " <> show (Map.size vs) <> ", nrequested " <> show nrequested
              Just ((k, _v), vs') ->
                if definitelyNoMoreToFetch
                  then includingAllKeys
                  else
                    Diff.applyDiff
                      vs'
                      (Diff.filterWithKeyOnly (< k) ds)

implForkerGetLedgerState ::
  (MonadSTM m, GetTip l) =>
  ForkerEnv m l blk ->
  STM m (l EmptyMK)
implForkerGetLedgerState env = current <$> readTVar (foeChangelog env)

-- | Obtain statistics for a combination of backing store value handle and
-- changelog.
implForkerReadStatistics ::
  (IOLike m, HasLedgerTables l, GetTip l) =>
  ForkerEnv m l blk ->
  m (Maybe Forker.Statistics)
implForkerReadStatistics env = do
  traceWith (foeTracer env) ForkerReadStatistics
  dblog <- readTVarIO (foeChangelog env)
  bsvh <- getValueHandle env
  let seqNo = getTipSlot $ changelogLastFlushedState dblog
  BackingStore.Statistics{sequenceNumber = seqNo', numEntries = n} <- bsvhStat bsvh
  if seqNo /= seqNo'
    then
      error $
        "Statistics seqNo ("
          ++ show seqNo'
          ++ ") is different from the seqNo in the DbChangelog last flushed field ("
          ++ show seqNo
          ++ ")"
    else do
      let
        diffs = changelogDiffs dblog

        nInserts =
          ltcollapse $
            ltmap
              (K2 . getSum . numInserts . getSeqDiffMK)
              diffs
        nDeletes =
          ltcollapse $
            ltmap
              (K2 . getSum . numDeletes . getSeqDiffMK)
              diffs
      pure . Just $
        Forker.Statistics
          { ledgerTableSize = n + nInserts - nDeletes
          }

implForkerPush ::
  (IOLike m, GetTip l, HasLedgerTables l) =>
  ForkerEnv m l blk ->
  l DiffMK ->
  m ()
implForkerPush env newState =
  encloseTimedWith (ForkerPush >$< foeTracer env) $ do
    atomically $ do
      chlog <- readTVar (foeChangelog env)
      let chlog' = extend newState chlog
      writeTVar (foeChangelog env) chlog'

implForkerCommit ::
  (MonadSTM m, GetTip l, StandardHash l, HasLedgerTables l) =>
  ForkerEnv m l blk ->
  STM m ()
implForkerCommit env = do
  dblog <- readTVar (foeChangelog env)
  modifyTVar (foeSwitchVar env) $ \orig ->
    -- We don't need to distinguish Origin from 0 because Origin has no diffs
    -- (SeqDiffMK is a fingertree measured by slot so there cannot be an entry
    -- for Origin).
    let s =
          fromWithOrigin 0
            . pointSlot
            . getTip
            $ changelogLastFlushedState orig
        -- The 'DbChangelog' might have gotten pruned in the meantime.
        splitAfterOrigAnchor =
          AS.splitAfterMeasure (pointSlot origAnchor) (either sameState sameState)
         where
          sameState = (origAnchor ==) . getTip
          origAnchor = getTip $ anchor orig
     in DbChangelog
          { changelogLastFlushedState = changelogLastFlushedState orig
          , changelogStates = case splitAfterOrigAnchor (changelogStates dblog) of
              Nothing -> error "Forker chain does no longer intersect with selected chain."
              Just (_, suffix) -> suffix
          , changelogDiffs =
              ltliftA2 (doPrune s) (changelogDiffs orig) (changelogDiffs dblog)
          }
  Monad.void $ swapTVar (foeWasCommitted env) True
 where
  -- Prune the diffs from the forker's log that have already been flushed to
  -- disk
  doPrune ::
    (Ord k, Eq v) =>
    SlotNo ->
    SeqDiffMK k v ->
    SeqDiffMK k v ->
    SeqDiffMK k v
  doPrune s (SeqDiffMK prunedSeq) (SeqDiffMK extendedSeq) =
    SeqDiffMK $
      -- This is acceptable because Byron has no tables, so combination of Byron
      -- block and EBB diffs will always result in the empty ledger table hence
      -- it doesn't matter.
      if DS.minSlot prunedSeq == DS.minSlot extendedSeq
        then extendedSeq
        else snd $ DS.splitAtSlot s extendedSeq
