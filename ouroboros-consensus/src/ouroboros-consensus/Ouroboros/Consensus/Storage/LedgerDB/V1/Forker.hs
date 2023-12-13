{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Storage.LedgerDB.V1.Forker (
    -- * Main API
    closeAllForkers
  , newForkerAtFromTip
  , newForkerAtPoint
  , newForkerAtTip
    -- * Acquire consistent views
  , acquireAtFromTip
  , acquireAtPoint
  , acquireAtTip
  ) where

import qualified Data.Map.Diff.Strict as Diff
import qualified Data.Map.Strict as Map
import           Data.Semigroup
import qualified Data.Set as Set
import           Data.Word
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables.DiffSeq (numDeletes,
                     numInserts)
import qualified Ouroboros.Consensus.Ledger.Tables.DiffSeq as DS
import           Ouroboros.Consensus.Storage.LedgerDB.API as API
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore.API as BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog hiding
                     (ExceededRollback)
import qualified Ouroboros.Consensus.Storage.LedgerDB.DbChangelog as DbCh
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Common
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

{-------------------------------------------------------------------------------
  Close
-------------------------------------------------------------------------------}

newForkerAtTip ::
     ( IOLike m
     , IsLedger l
     , HasLedgerTables l
     , LedgerSupportsProtocol blk
     )
  => LedgerDBHandle m l blk
  -> ResourceRegistry m
  -> m (Forker m l blk)
newForkerAtTip h rr = getEnv h $ \ldbEnv -> do
    acquireAtTip ldbEnv rr >>= newForker h ldbEnv

newForkerAtPoint ::
     ( HeaderHash l ~ HeaderHash blk
     , IOLike m
     , IsLedger l
     , StandardHash l
     , HasLedgerTables l
     , LedgerSupportsProtocol blk
     )
  => LedgerDBHandle m l blk
  -> ResourceRegistry m
  -> Point blk
  -> m (Either (Point blk) (Forker m l blk))
newForkerAtPoint h rr pt = getEnv h $ \ldbEnv -> do
    acquireAtPoint ldbEnv rr pt >>= traverse (newForker h ldbEnv)

newForkerAtFromTip ::
     ( IOLike m
     , IsLedger l
     , HasLedgerTables l
     , LedgerSupportsProtocol blk
     )
  => LedgerDBHandle m l blk
  -> ResourceRegistry m
  -> Word64
  -> m (Either ExceededRollback (Forker m l blk))
newForkerAtFromTip h rr n = getEnv h $ \ldbEnv -> do
    acquireAtFromTip ldbEnv rr n >>= traverse (newForker h ldbEnv)

-- | Close all open block and header 'Follower's.
closeAllForkers ::
     MonadSTM m
  => LedgerDBEnv m l blk
  -> m ()
closeAllForkers ldbEnv = do
    forkerEnvs <- atomically $ do
      forkerEnvs <- Map.elems <$> readTVar forkersVar
      writeTVar forkersVar Map.empty
      return forkerEnvs
    mapM_ closeForkerEnv forkerEnvs
  where
    forkersVar = ldbForkers ldbEnv

closeForkerEnv :: ForkerEnv m l blk -> m ()
closeForkerEnv ForkerEnv { foeBackingStoreValueHandle } = do
        bsvhClose foeBackingStoreValueHandle

{-------------------------------------------------------------------------------
  Acquiring consistent views
-------------------------------------------------------------------------------}

type Resources m l =
    (LedgerBackingStoreValueHandle m l, AnchorlessDbChangelog l)

-- Acquire both a value handle and a db changelog at the tip. Holds a read lock
-- while doing so.
acquireAtTip ::
     IOLike m
  => LedgerDBEnv m l blk
  -> ResourceRegistry m
  -> m (Resources m l)
acquireAtTip ldbEnv rr =
    withReadLock (ldbLock ldbEnv) $ do
      dblog <- anchorlessChangelog <$> readTVarIO (ldbChangelog ldbEnv)
      (,dblog) <$> acquire ldbEnv rr dblog

-- Acquire both a value handle and a db changelog at the requested point. Holds
-- a read lock while doing so.
acquireAtPoint ::
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
  -> Point blk
  -> m (Either (Point blk) (Resources m l))
acquireAtPoint ldbEnv rr pt =
    withReadLock (ldbLock ldbEnv) $ do
      dblog <- anchorlessChangelog <$> readTVarIO (ldbChangelog ldbEnv)
      case rollback pt dblog of
        Nothing     -> pure $ Left $ castPoint $ getTip $ anchor dblog
        Just dblog' -> Right . (,dblog') <$> acquire ldbEnv rr dblog'

-- Acquire both a value handle and a db changelog at n blocks before the tip.
-- Holds a read lock while doing so.
acquireAtFromTip ::
     forall m l blk. (
       IOLike m
     , IsLedger l
     , HasLedgerTables l
     )
  => LedgerDBEnv m l blk
  -> ResourceRegistry m
  -> Word64
  -> m (Either ExceededRollback (Resources m l))
acquireAtFromTip ldbEnv rr n =
    withReadLock (ldbLock ldbEnv) $ do
      dblog <- anchorlessChangelog <$> readTVarIO (ldbChangelog ldbEnv)
      case rollbackN n dblog of
        Nothing ->
          return $ Left $ ExceededRollback {
              API.rollbackMaximum   = maxRollback dblog
            , API.rollbackRequested = n
            }
        Just dblog' -> Right . (,dblog') <$> acquire ldbEnv rr dblog'

acquire ::
     IOLike m
  => LedgerDBEnv m l blk
  -> ResourceRegistry m
  -> AnchorlessDbChangelog l
  -> m (LedgerBackingStoreValueHandle m l)
acquire ldbEnv rr dblog =  do
  (_, vh) <- allocate rr (\_ -> bsValueHandle $ ldbBackingStore ldbEnv) bsvhClose
  if bsvhAtSlot vh == adcLastFlushedSlot dblog
    then pure vh
    else error (  "Critical error: Value handles are created at "
                <> show (bsvhAtSlot vh)
                <> " while the db changelog is at "
                <> show (adcLastFlushedSlot dblog)
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
  dblogVar <- newTVarIO dblog
  let forkerEnv = ForkerEnv {
      foeBackingStoreValueHandle = vh
    , foeChangelog               = dblogVar
    , foeSwitchVar               = ldbChangelog ldbEnv
    , foeSecurityParam           = ldbSecParam ldbEnv
    , foeQueryBatchSize          = ldbQueryBatchSize ldbEnv
    }
  forkerKey <- atomically $ stateTVar (ldbNextForkerKey ldbEnv) $ \r -> (r, succ r)
  atomically $ modifyTVar (ldbForkers ldbEnv) $ Map.insert forkerKey forkerEnv
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
    , forkerRangeReadTablesDefault = getForkerEnv1   h forkerKey implForkerRangeReadTablesDefault
    , forkerGetLedgerState         = getForkerEnvSTM h forkerKey implForkerGetLedgerState
    , forkerReadStatistics         = getForkerEnv    h forkerKey implForkerReadStatistics
    , forkerPush                   = getForkerEnv1   h forkerKey implForkerPush
    , forkerCommit                 = getForkerEnvSTM h forkerKey implForkerCommit
    }

implForkerClose ::
     MonadSTM m
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
     (MonadSTM m, HasLedgerTables l)
  => ForkerEnv m l blk
  -> LedgerTables l KeysMK
  -> m (LedgerTables l ValuesMK)
implForkerReadTables env ks = do
    ldb <- readTVarIO $ foeChangelog env
    let rew = rewindTableKeySets ldb ks
    unfwd <- readKeySetsWith lvh rew
    case forwardTableKeySets ldb unfwd of
        Left _err -> error "impossible!"
        Right vs  -> pure vs
  where
    lvh = foeBackingStoreValueHandle env

implForkerRangeReadTables ::
     (MonadSTM m, HasLedgerTables l)
  => ForkerEnv m l blk
  -> API.RangeQuery l
  -> m (LedgerTables l ValuesMK)
implForkerRangeReadTables env rq0 = do
    ldb <- readTVarIO $ foeChangelog env
    let -- Get the differences without the keys that are greater or equal
        -- than the maximum previously seen key.
        diffs =
          maybe
            id
            (ltliftA2 doDropLTE)
            (BackingStore.rqPrev rq)
            $ ltmap prj
            $ adcDiffs ldb
            -- (1) Ensure that we never delete everything read from disk (ie
            --     if our result is non-empty then it contains something read
            --     from disk).
            --
            -- (2) Also, read one additional key, which we will not include in
            --     the result but need in order to know which in-memory
            --     insertions to include.
        maxDeletes = ltcollapse $ ltmap (K2 . numDeletesDiffMK) diffs
        nrequested = 1 + max (BackingStore.rqCount rq) (1 + maxDeletes)

    values <- BackingStore.bsvhRangeRead lvh (rq{BackingStore.rqCount = nrequested})
    pure $ ltliftA2 (doFixupReadResult nrequested) diffs values
  where
    lvh = foeBackingStoreValueHandle env

    rq = BackingStore.RangeQuery (API.rqPrev rq0) (API.rqCount rq0)

    prj ::
         (Ord k, Eq v)
      => SeqDiffMK k v
      -> DiffMK k v
    prj (SeqDiffMK sq) = DiffMK (DS.cumulativeDiff sq)

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
          Just k  -> Diff.filterOnlyKey (> k) ds

    -- NOTE: this is counting the deletions wrt disk.
    numDeletesDiffMK :: DiffMK k v -> Int
    numDeletesDiffMK (DiffMK d) =
      getSum $ Diff.foldMapDelta (Sum . oneIfDel) d
      where
        oneIfDel x = case x of
          Diff.Delete _ -> 1
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
               (Diff.filterOnlyKey (< k) ds)

implForkerRangeReadTablesDefault ::
     (MonadSTM m, HasLedgerTables l)
  => ForkerEnv m l blk
  -> Maybe (LedgerTables l KeysMK)
  -> m (LedgerTables l ValuesMK)
implForkerRangeReadTablesDefault env prev =
    implForkerRangeReadTables env (API.RangeQuery prev (fromIntegral n))
  where
    n = defaultQueryBatchSize $ foeQueryBatchSize env

implForkerGetLedgerState ::
     (MonadSTM m, GetTip l)
  => ForkerEnv m l blk
  -> STM m (l EmptyMK)
implForkerGetLedgerState env = current <$> readTVar (foeChangelog env)

-- | Obtain statistics for a combination of backing store value handle and
-- changelog.
implForkerReadStatistics ::
     (MonadSTM m, HasLedgerTables l)
  => ForkerEnv m l blk
  -> m (Maybe API.Statistics)
implForkerReadStatistics env = do
    dblog <- readTVarIO $ foeChangelog env
    let seqNo = adcLastFlushedSlot dblog
    BackingStore.Statistics{sequenceNumber = seqNo', numEntries = n} <- bsvhStat lbsvh
    if seqNo /= seqNo' then
      error $ show (seqNo, seqNo')
    else do
      let
        diffs = adcDiffs dblog

        nInserts = getSum
                $ ltcollapse
                $ ltmap (K2 . numInserts . getSeqDiffMK)
                  diffs
        nDeletes = getSum
                $ ltcollapse
                $ ltmap (K2 . numDeletes . getSeqDiffMK)
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
implForkerPush env newState = atomically $ do
  db <- readTVar (foeChangelog env)
  let db' = prune (foeSecurityParam env)
          $ extend (DbCh.ValidLedgerState newState) db
  writeTVar (foeChangelog env) db'

implForkerCommit ::
     (MonadSTM m, GetTip l, HasLedgerTables l)
  => ForkerEnv m l blk
  -> STM m ()
implForkerCommit env = do
  dblog <- readTVar (foeChangelog env)
  modifyTVar (foeSwitchVar env) (\pruned ->
    let s = fromWithOrigin 0
          . pointSlot
          . getTip
          $ changelogLastFlushedState pruned
    in DbChangelog {
          changelogLastFlushedState = changelogLastFlushedState pruned
        , anchorlessChangelog       = AnchorlessDbChangelog {
              adcLastFlushedSlot = adcLastFlushedSlot $ anchorlessChangelog pruned
            , adcStates          = adcStates dblog
            , adcDiffs           =
                ltliftA2 (f s) (adcDiffs $ anchorlessChangelog pruned) (adcDiffs dblog)
            }
        })
  where
    f :: (Ord k, Eq v)
      => SlotNo
      -> SeqDiffMK k v
      -> SeqDiffMK k v
      -> SeqDiffMK k v
    f s (SeqDiffMK prunedSeq) (SeqDiffMK extendedSeq) = SeqDiffMK $
      if DS.minSlot prunedSeq == DS.minSlot extendedSeq
      then extendedSeq
      else snd $ DS.splitAtSlot s extendedSeq