{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Storage.LedgerDB.V1.Forker (
    closeAllForkers
  , newForkerAtTarget
  , newForkerByRollback
  ) where

import           Control.ResourceRegistry
import           Control.Tracer
import           Data.Functor.Contravariant ((>$<))
import qualified Data.Map.Strict as Map
import           Data.Semigroup
import qualified Data.Set as Set
import           Data.Word
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import           Ouroboros.Consensus.Ledger.Tables.DiffSeq (numDeletes,
                     numInserts)
import qualified Ouroboros.Consensus.Ledger.Tables.DiffSeq as DS
import           Ouroboros.Consensus.Storage.LedgerDB.API as API
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Common
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Args
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API as BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Common
import           Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Lock
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Protocol.LocalStateQuery.Type

{-------------------------------------------------------------------------------
  Close
-------------------------------------------------------------------------------}

-- | Will call 'error' if the point is not on the LedgerDB
newForkerAtTarget ::
     ( HeaderHash l ~ HeaderHash blk
     , IOLike m
     , IsLedger l
     , StandardHash l
     , HasLedgerTables l
     , LedgerSupportsProtocol blk
     )
  => LedgerDBHandle m l blk
  -> ResourceRegistry m
  -> Target (Point blk)
  ->  m (Either GetForkerError (Forker m l blk))
newForkerAtTarget h rr pt = getEnv h $ \ldbEnv ->
    withReadLock (ldbLock ldbEnv) (acquireAtTarget ldbEnv rr (Right pt)) >>= traverse (newForker h ldbEnv)

newForkerByRollback ::
     ( HeaderHash l ~ HeaderHash blk
     , IOLike m
     , IsLedger l
     , StandardHash l
     , HasLedgerTables l
     , LedgerSupportsProtocol blk
     )
  => LedgerDBHandle m l blk
  -> ResourceRegistry m
     -- | How many blocks to rollback from the tip
  -> Word64
  -> m (Either GetForkerError (Forker m l blk))
newForkerByRollback h rr n = getEnv h $ \ldbEnv -> do
    withReadLock (ldbLock ldbEnv) (acquireAtTarget ldbEnv rr (Left n)) >>= traverse (newForker h ldbEnv)

-- | Close all open block and header 'Forker's.
closeAllForkers ::
     IOLike m
  => LedgerDBEnv m l blk
  -> m ()
closeAllForkers ldbEnv =
  do
    forkerEnvs <- atomically $ do
      forkerEnvs <- Map.elems <$> readTVar forkersVar
      writeTVar forkersVar Map.empty
      return forkerEnvs
    mapM_ closeForkerEnv forkerEnvs
  where
    forkersVar = ldbForkers ldbEnv

closeForkerEnv :: ForkerEnv m l blk -> m ()
closeForkerEnv ForkerEnv { foeBackingStoreValueHandle } = bsvhClose foeBackingStoreValueHandle

{-------------------------------------------------------------------------------
  Acquiring consistent views
-------------------------------------------------------------------------------}

type Resources m l =
    (LedgerBackingStoreValueHandle m l, DbChangelog l)

-- | Acquire both a value handle and a db changelog at the tip. Holds a read lock
-- while doing so.
acquireAtTarget ::
     forall m l blk. (
       HeaderHash l ~ HeaderHash blk
     , IOLike m
     , IsLedger l
     , StandardHash l
     , HasLedgerTables l
     , LedgerSupportsProtocol blk
     )
  => LedgerDBEnv m l blk
  -> ResourceRegistry m
  -> Either Word64 (Target (Point blk))
  -> ReadLocked m (Either GetForkerError (Resources m l))
acquireAtTarget ldbEnv rr (Right VolatileTip) =
    readLocked $ do
      dblog <- readTVarIO (ldbChangelog ldbEnv)
      Right . (,dblog) <$> acquire ldbEnv rr dblog
acquireAtTarget ldbEnv rr (Right ImmutableTip) =
    readLocked $ do
      dblog <- readTVarIO (ldbChangelog ldbEnv)
      Right . (, rollbackToAnchor dblog)
        <$> acquire ldbEnv rr dblog
acquireAtTarget ldbEnv rr (Right (SpecificPoint pt)) =
    readLocked $ do
      dblog <- readTVarIO (ldbChangelog ldbEnv)
      let immTip = getTip $ anchor dblog
      case rollback pt dblog of
        Nothing     | pointSlot pt < pointSlot immTip -> pure $ Left $ PointTooOld Nothing
                    | otherwise   -> pure $ Left PointNotOnChain
        Just dblog' -> Right . (,dblog') <$> acquire ldbEnv rr dblog'
acquireAtTarget ldbEnv rr (Left n) = readLocked $ do
      dblog <- readTVarIO (ldbChangelog ldbEnv)
      case rollbackN n dblog of
        Nothing ->
          return $ Left $ PointTooOld $ Just $ ExceededRollback {
              API.rollbackMaximum   = maxRollback dblog
            , API.rollbackRequested = n
            }
        Just dblog' ->
           Right . (,dblog') <$> acquire ldbEnv rr dblog'

acquire ::
     (IOLike m, GetTip l)
  => LedgerDBEnv m l blk
  -> ResourceRegistry m
  -> DbChangelog l
  -> m (LedgerBackingStoreValueHandle m l)
acquire ldbEnv rr dblog =  do
  -- bsvhClose is idempotent, so we let the resource call it even if the value
  -- handle might have been closed somewhere else
  (_, vh) <- allocate rr (\_ -> bsValueHandle $ ldbBackingStore ldbEnv) bsvhClose
  let dblogSlot = getTipSlot (changelogLastFlushedState dblog)
  if bsvhAtSlot vh == dblogSlot
    then pure vh
    else bsvhClose vh >>
         error (  "Critical error: Value handles are created at "
                <> show (bsvhAtSlot vh)
                <> " while the db changelog is at "
                <> show dblogSlot
                <> ". There is either a race condition or a logic bug"
                )

{-------------------------------------------------------------------------------
  Make forkers from consistent views
-------------------------------------------------------------------------------}

newForker ::
     ( IOLike m
     , HasLedgerTables l
     , LedgerSupportsProtocol blk
     , NoThunks (l EmptyMK)
     , GetTip l
     )
  => LedgerDBHandle m l blk
  -> LedgerDBEnv m l blk
  -> Resources m l
  -> m (Forker m l blk)
newForker h ldbEnv (vh, dblog) = do
  dblogVar     <- newTVarIO dblog
  forkerKey    <- atomically $ stateTVar (ldbNextForkerKey ldbEnv) $ \r -> (r, r + 1)
  let forkerEnv = ForkerEnv {
      foeBackingStoreValueHandle = vh
    , foeChangelog               = dblogVar
    , foeSwitchVar               = ldbChangelog ldbEnv
    , foeSecurityParam           = ledgerDbCfgSecParam $ ldbCfg ldbEnv
    , foeQueryBatchSize          = ldbQueryBatchSize ldbEnv
    , foeTracer                  = LedgerDBForkerEvent . TraceForkerEventWithKey forkerKey >$< ldbTracer ldbEnv
    }
  atomically $ modifyTVar (ldbForkers ldbEnv) $ Map.insert forkerKey forkerEnv
  traceWith (foeTracer forkerEnv) ForkerOpen
  pure $ mkForker h forkerKey

mkForker ::
     ( IOLike m
     , HasHeader blk
     , HasLedgerTables l
     , GetTip l
     )
  => LedgerDBHandle m l blk
  -> ForkerKey
  -> Forker m l blk
mkForker h forkerKey = Forker {
      forkerClose                  = implForkerClose h forkerKey
    , forkerReadTables             = getForkerEnv1   h forkerKey implForkerReadTables
    , forkerRangeReadTables        = getForkerEnv1   h forkerKey implForkerRangeReadTables
    , forkerGetLedgerState         = getForkerEnvSTM h forkerKey implForkerGetLedgerState
    , forkerReadStatistics         = getForkerEnv    h forkerKey implForkerReadStatistics
    , forkerPush                   = getForkerEnv1   h forkerKey implForkerPush
    , forkerCommit                 = getForkerEnvSTM h forkerKey implForkerCommit
    }

implForkerClose ::
     IOLike m
  => LedgerDBHandle m l blk
  -> ForkerKey
  -> m ()
implForkerClose (LDBHandle varState) forkerKey = do
    envMay <- atomically $ readTVar varState >>= \case
      LedgerDBClosed       -> pure Nothing
      LedgerDBOpen ldbEnv -> do
        stateTVar
            (ldbForkers ldbEnv)
            (Map.updateLookupWithKey (\_ _ -> Nothing) forkerKey)
    whenJust envMay closeForkerEnv

implForkerReadTables ::
     (MonadSTM m, HasLedgerTables l, GetTip l)
  => ForkerEnv m l blk
  -> LedgerTables l KeysMK
  -> m (LedgerTables l ValuesMK)
implForkerReadTables env ks = do
    traceWith (foeTracer env) ForkerReadTablesStart
    chlog <- readTVarIO (foeChangelog env)
    unfwd <- readKeySetsWith lvh ks
    case forwardTableKeySets chlog unfwd of
      Left _err -> error "impossible!"
      Right vs  -> do
        traceWith (foeTracer env) ForkerReadTablesEnd
        pure vs
  where
    lvh = foeBackingStoreValueHandle env

implForkerRangeReadTables ::
     (MonadSTM m, HasLedgerTables l)
  => ForkerEnv m l blk
  -> RangeQueryPrevious l
  -> m (LedgerTables l ValuesMK)
implForkerRangeReadTables env rq0 = do
    traceWith (foeTracer env) ForkerRangeReadTablesStart
    ldb <- readTVarIO $ foeChangelog env
    let -- Get the differences without the keys that are greater or equal
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

    values <- BackingStore.bsvhRangeRead lvh (rq{BackingStore.rqCount = nrequested})
    traceWith (foeTracer env) ForkerRangeReadTablesEnd
    pure $ ltliftA2 (doFixupReadResult nrequested) diffs values
  where
    lvh = foeBackingStoreValueHandle env

    rq = BackingStore.RangeQuery rq1 (fromIntegral $ queryBatchSize $ foeQueryBatchSize env)

    rq1 = case rq0 of
      NoPreviousQuery        -> Nothing
      PreviousQueryWasFinal  -> Just (LedgerTables $ KeysMK Set.empty)
      PreviousQueryWasUpTo k -> Just (LedgerTables $ KeysMK $ Set.singleton k)

    prj ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> DiffMK k v
    prj (SeqDiffMK sq) = DiffMK (Diff.fromAntiDiff $ DS.cumulativeDiff sq)

    -- Remove all diff elements that are <= to the greatest given key
    doDropLTE ::
         Ord k
      => KeysMK k v
      -> DiffMK k v
      -> DiffMK k v
    doDropLTE (KeysMK ks) (DiffMK ds) =
        DiffMK
      $ case Set.lookupMax ks of
          Nothing -> ds
          Just k  -> Diff.filterWithKeyOnly (> k) ds

    -- NOTE: this is counting the deletions wrt disk because deletions of values
    -- created along the diffs will have been collapsed to the empty diff.
    numDeletesDiffMK :: DiffMK k v -> Int
    numDeletesDiffMK (DiffMK d) =
      getSum $ Diff.foldMapDelta (Sum . oneIfDel) d
      where
        oneIfDel x = case x of
          Diff.Delete   -> 1
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
         Ord k
      => Int
      -- ^ Number of requested keys from the backing store.
      -> DiffMK   k v
      -- ^ Differences that will be applied to the values read from the backing
      -- store.
      -> ValuesMK k v
      -- ^ Values read from the backing store. The number of values read should
      -- be at most @nrequested@.
      -> ValuesMK k v
    doFixupReadResult
      nrequested
      (DiffMK ds)
      (ValuesMK vs) =
        let includingAllKeys        =
              Diff.applyDiff vs ds
            definitelyNoMoreToFetch = Map.size vs < nrequested
        in
        ValuesMK
      $ case Map.maxViewWithKey vs of
          Nothing             ->
              if definitelyNoMoreToFetch
              then includingAllKeys
              else error $ "Size of values " <> show (Map.size vs) <> ", nrequested " <> show nrequested
          Just ((k, _v), vs') ->
            if definitelyNoMoreToFetch then includingAllKeys else
            Diff.applyDiff
              vs'
               (Diff.filterWithKeyOnly (< k) ds)

implForkerGetLedgerState ::
     (MonadSTM m, GetTip l)
  => ForkerEnv m l blk
  -> STM m (l EmptyMK)
implForkerGetLedgerState env = current <$> readTVar (foeChangelog env)

-- | Obtain statistics for a combination of backing store value handle and
-- changelog.
implForkerReadStatistics ::
     (MonadSTM m, HasLedgerTables l, GetTip l)
  => ForkerEnv m l blk
  -> m (Maybe API.Statistics)
implForkerReadStatistics env = do
    traceWith (foeTracer env) ForkerReadStatistics
    dblog <- readTVarIO (foeChangelog env)

    let seqNo = getTipSlot $ changelogLastFlushedState dblog
    BackingStore.Statistics{sequenceNumber = seqNo', numEntries = n} <- bsvhStat lbsvh
    if seqNo /= seqNo' then
      error $ "Statistics seqNo ("
            ++ show seqNo'
            ++ ") is different from the seqNo in the DbChangelog last flushed field ("
            ++ show seqNo
            ++ ")"
    else do
      let
        diffs = changelogDiffs dblog

        nInserts = ltcollapse
                 $ ltmap (K2 . getSum . numInserts . getSeqDiffMK)
                   diffs
        nDeletes = ltcollapse
                 $ ltmap (K2 . getSum . numDeletes . getSeqDiffMK)
                   diffs
      pure . Just $ API.Statistics {
          ledgerTableSize = n + nInserts - nDeletes
        }
  where
    lbsvh = foeBackingStoreValueHandle env

implForkerPush ::
     (MonadSTM m, GetTip l, HasLedgerTables l)
  => ForkerEnv m l blk
  -> l DiffMK
  -> m ()
implForkerPush env newState = do
  traceWith (foeTracer env) ForkerPushStart
  atomically $ do
    chlog <- readTVar (foeChangelog env)
    let chlog' = prune (foeSecurityParam env)
               $ extend newState chlog
    writeTVar (foeChangelog env) chlog'
  traceWith (foeTracer env) ForkerPushEnd

implForkerCommit ::
     (MonadSTM m, GetTip l, HasLedgerTables l)
  => ForkerEnv m l blk
  -> STM m ()
implForkerCommit env = do
  dblog <- readTVar (foeChangelog env)
  modifyTVar (foeSwitchVar env) $ \orig ->
    -- We don't need to distinguish Origin from 0 because Origin has no diffs
    -- (SeqDiffMK is a fingertree measured by slot so there cannot be an entry
    -- for Origin).
    let s = fromWithOrigin 0
          . pointSlot
          . getTip
          $ changelogLastFlushedState orig
    in DbChangelog {
          changelogLastFlushedState = changelogLastFlushedState orig
        , changelogStates           = changelogStates dblog
        , changelogDiffs            =
                ltliftA2 (doPrune s) (changelogDiffs orig) (changelogDiffs dblog)
        }
  where
    -- Prune the diffs from the forker's log that have already been flushed to
    -- disk
    doPrune :: (Ord k, Eq v)
          => SlotNo
          -> SeqDiffMK k v
          -> SeqDiffMK k v
          -> SeqDiffMK k v
    doPrune s (SeqDiffMK prunedSeq) (SeqDiffMK extendedSeq) = SeqDiffMK $
      -- This is acceptable because Byron has no tables, so combination of Byron
      -- block and EBB diffs will always result in the empty ledger table hence
      -- it doesn't matter.
      if DS.minSlot prunedSeq == DS.minSlot extendedSeq
      then extendedSeq
      else snd $ DS.splitAtSlot s extendedSeq
