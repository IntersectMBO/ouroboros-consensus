{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , forkerCommon
  , forkerValueHandle
  , ForkerEnvCommon (..)
  , upgradeForkerToRead
  , mkSimpleForker
  ) where

import Control.ResourceRegistry
import Control.Tracer
import Data.Functor.Contravariant ((>$<))
import qualified Data.Map.Strict as Map
import Data.Semigroup
import qualified Data.Set as Set
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Storage.LedgerDB.API
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
import Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Forkers
-------------------------------------------------------------------------------}

-- | The environment used by a forker.
data ForkerEnv m l blk ft where
  ReadForkerEnv ::
    { rfoeBackingStoreValueHandle :: !(LedgerBackingStoreValueHandle m l)
    -- ^ Local, consistent view of backing store.
    , rfoeCommon :: !(ForkerEnvCommon m l)
    -- ^ The environment common to all forkers.
    } ->
    ForkerEnv m l blk TablesForker
  SimpleForkerEnv ::
    { sfoeUpgradeIngredients :: !(LedgerDBLock m, LedgerBackingStore m l, ResourceRegistry m)
    -- ^ Ingredients for updating to a forker with tables.
    , sfoeCommon :: !(ForkerEnvCommon m l)
    -- ^ The environment common to all forkers.
    } ->
    ForkerEnv m l blk NoTablesForker

data ForkerEnvCommon m l = ForkerEnvCommon
  { foeChangelog :: !(StrictTVar m (DbChangelog l))
  -- ^ In memory db changelog, 'foeBackingStoreValueHandle' must refer to
  -- the anchor of this changelog.
  , foeSecurityParam :: !SecurityParam
  -- ^ Config
  , foeTracer :: !(Tracer m TraceForkerEvent)
  -- ^ Config
  }

forkerCommon :: ForkerEnv m l blk ft -> ForkerEnvCommon m l
forkerCommon ReadForkerEnv{rfoeCommon} = rfoeCommon
forkerCommon SimpleForkerEnv{sfoeCommon} = sfoeCommon

forkerValueHandle :: ForkerEnv m l blk TablesForker -> LedgerBackingStoreValueHandle m l
forkerValueHandle ReadForkerEnv{rfoeBackingStoreValueHandle} = rfoeBackingStoreValueHandle

mkSimpleForker ::
  IOLike m =>
  LedgerDBLock m ->
  LedgerBackingStore m l ->
  ResourceRegistry m ->
  StrictTVar m (DbChangelog l) ->
  SecurityParam ->
  Tracer m TraceForkerEventWithKey ->
  ForkerKey ->
  m (ForkerEnv m l blk NoTablesForker)
mkSimpleForker lock backingStore rr changelog secParam trcr forkerKey = do
  atomically $ unsafeAcquireReadAccess lock
  pure
    SimpleForkerEnv
      { sfoeUpgradeIngredients =
          (lock, backingStore, rr)
      , sfoeCommon =
          ForkerEnvCommon
            { foeChangelog = changelog
            , foeSecurityParam = secParam
            , foeTracer = TraceForkerEventWithKey forkerKey >$< trcr
            }
      }

upgradeForkerToRead ::
  (IOLike m, GetTip l) =>
  ForkerEnv m l blk NoTablesForker -> m (ForkerEnv m l blk TablesForker)
upgradeForkerToRead SimpleForkerEnv{sfoeUpgradeIngredients = (lk, bs, rr), sfoeCommon} = do
  -- bsvhClose is idempotent, so we let the resource call it even if the value
  -- handle might have been closed somewhere else
  (_, vh) <-
    allocate
      rr
      ( \_ -> do
          vh <- bsValueHandle bs
          atomically $ unsafeReleaseReadAccess lk
          pure vh
      )
      bsvhClose
  dblogSlot <- getTipSlot . changelogLastFlushedState <$> readTVarIO (foeChangelog sfoeCommon)
  if bsvhAtSlot vh == dblogSlot
    then
      pure
        ReadForkerEnv
          { rfoeCommon = sfoeCommon
          , rfoeBackingStoreValueHandle = vh
          }
    else
      bsvhClose vh
        >> error
          ( "Critical error: Value handles are created at "
              <> show (bsvhAtSlot vh)
              <> " while the db changelog is at "
              <> show dblogSlot
              <> ". There is either a race condition or a logic bug"
          )

{-------------------------------------------------------------------------------
  Close
-------------------------------------------------------------------------------}

closeForkerEnv :: IOLike m => ForkerEnv m l blk ht -> m ()
closeForkerEnv ReadForkerEnv{rfoeBackingStoreValueHandle} = bsvhClose rfoeBackingStoreValueHandle
closeForkerEnv SimpleForkerEnv{sfoeUpgradeIngredients = (lk, _, _)} = atomically $ unsafeReleaseReadAccess lk

{-------------------------------------------------------------------------------
  Acquiring consistent views
-------------------------------------------------------------------------------}

implForkerReadTables ::
  (MonadSTM m, HasLedgerTables l, GetTip l) =>
  ForkerEnv m l blk TablesForker ->
  LedgerTables l KeysMK ->
  m (LedgerTables l ValuesMK)
implForkerReadTables env ks = do
  traceWith (foeTracer $ forkerCommon env) ForkerReadTablesStart
  chlog <- readTVarIO (foeChangelog $ forkerCommon env)
  unfwd <- readKeySetsWith lvh (changelogLastFlushedState chlog) ks
  case forwardTableKeySets chlog unfwd of
    Left _err -> error "impossible!"
    Right vs -> do
      traceWith (foeTracer $ forkerCommon env) ForkerReadTablesEnd
      pure vs
 where
  lvh = forkerValueHandle env

implForkerRangeReadTables ::
  (MonadSTM m, HasLedgerTables l) =>
  QueryBatchSize ->
  ForkerEnv m l blk TablesForker ->
  RangeQueryPrevious l ->
  m (LedgerTables l ValuesMK)
implForkerRangeReadTables qbs env rq0 = do
  traceWith (foeTracer $ forkerCommon env) ForkerRangeReadTablesStart
  ldb <- readTVarIO $ foeChangelog $ forkerCommon env
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
  values <- BackingStore.bsvhRangeRead lvh st (rq{BackingStore.rqCount = nrequested})
  traceWith (foeTracer $ forkerCommon env) ForkerRangeReadTablesEnd
  pure $ ltliftA2 (doFixupReadResult nrequested) diffs values
 where
  lvh = forkerValueHandle env

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
  ForkerEnv m l blk ht ->
  STM m (l EmptyMK)
implForkerGetLedgerState env = current <$> readTVar (foeChangelog $ forkerCommon env)

-- | Obtain statistics for a combination of backing store value handle and
-- changelog.
implForkerReadStatistics ::
  (MonadSTM m, HasLedgerTables l, GetTip l) =>
  ForkerEnv m l blk TablesForker ->
  m (Maybe Forker.Statistics)
implForkerReadStatistics env = do
  traceWith (foeTracer $ forkerCommon env) ForkerReadStatistics
  dblog <- readTVarIO (foeChangelog $ forkerCommon env)

  let seqNo = getTipSlot $ changelogLastFlushedState dblog
  BackingStore.Statistics{sequenceNumber = seqNo', numEntries = n} <- bsvhStat lbsvh
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
 where
  lbsvh = forkerValueHandle env

implForkerPush ::
  (MonadSTM m, GetTip l, HasLedgerTables l) =>
  ForkerEnv m l blk TablesForker ->
  l DiffMK ->
  m ()
implForkerPush env newState = do
  traceWith (foeTracer $ forkerCommon env) ForkerPushStart
  atomically $ do
    chlog <- readTVar (foeChangelog $ forkerCommon env)
    let chlog' =
          prune (LedgerDbPruneKeeping (foeSecurityParam $ forkerCommon env)) $
            extend newState chlog
    writeTVar (foeChangelog $ forkerCommon env) chlog'
  traceWith (foeTracer $ forkerCommon env) ForkerPushEnd

implForkerCommit ::
  (MonadSTM m, GetTip l, HasLedgerTables l) =>
  ForkerEnv m l blk TablesForker ->
  StrictTVar m (DbChangelog l) ->
  STM m ()
implForkerCommit env switchVar = do
  dblog <- readTVar (foeChangelog $ forkerCommon env)
  modifyTVar switchVar $ \orig ->
    -- We don't need to distinguish Origin from 0 because Origin has no diffs
    -- (SeqDiffMK is a fingertree measured by slot so there cannot be an entry
    -- for Origin).
    let s =
          fromWithOrigin 0
            . pointSlot
            . getTip
            $ changelogLastFlushedState orig
     in DbChangelog
          { changelogLastFlushedState = changelogLastFlushedState orig
          , changelogStates = changelogStates dblog
          , changelogDiffs =
              ltliftA2 (doPrune s) (changelogDiffs orig) (changelogDiffs dblog)
          }
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
