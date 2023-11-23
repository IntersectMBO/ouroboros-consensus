{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: forkers should be closed when the LedgerDB is closed
-- TODO: resource registry
module Ouroboros.Consensus.Storage.LedgerDB.V1.Forker (newForker) where

import qualified Data.Map.Diff.Strict as Diff
import qualified Data.Map.Strict as Map
import           Data.Semigroup
import qualified Data.Set as Set
import           GHC.Generics
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables.DiffSeq (numDeletes,
                     numInserts)
import qualified Ouroboros.Consensus.Ledger.Tables.DiffSeq as DS
import           Ouroboros.Consensus.Storage.LedgerDB.API as API
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore.API as BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
import qualified Ouroboros.Consensus.Storage.LedgerDB.DbChangelog as DbCh
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Common
                     (LedgerDBEnv (..))
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Accessing the environment
-------------------------------------------------------------------------------}

newtype ForkerHandle m l blk = ForkerHandle (StrictTVar m (ForkerState m l blk))
  deriving Generic

data ForkerState m l blk =
    ForkerOpen !(ForkerEnv m l blk)
  | ForkerClosed
  deriving Generic

deriving instance ( IOLike m
                  , LedgerSupportsProtocol blk
                  , NoThunks (l EmptyMK)
                  , NoThunks (Key l)
                  , NoThunks (Value l)
                  ) => NoThunks (ForkerState m l blk)

data ForkerEnv m l blk = ForkerEnv {
    foeBackingStoreValueHandle :: !(LedgerBackingStoreValueHandle m l)
  , foeChangelog               :: !(StrictTVar m (AnchorlessDbChangelog l))
  , foeSecurityParam           :: !SecurityParam
  , switchVar                  :: !(StrictTVar m (DbChangelog l))
  }
  deriving Generic

deriving instance ( IOLike m
                  , LedgerSupportsProtocol blk
                  , NoThunks (l EmptyMK)
                  , NoThunks (Key l)
                  , NoThunks (Value l)
                  ) => NoThunks (ForkerEnv m l blk)

getEnv :: forall m l blk r. (IOLike m, HasCallStack, HasHeader blk)
       => ForkerHandle m l blk
       -> (ForkerEnv m l blk -> m r)
       -> m r
getEnv (ForkerHandle varState) f = readTVarIO varState >>= \case
    ForkerOpen env -> f env
    ForkerClosed   -> throwIO $ ClosedForkerError @blk prettyCallStack

getEnv1 :: (IOLike m, HasCallStack, HasHeader blk)
        => ForkerHandle m l blk
        -> (ForkerEnv m l blk -> a -> m r)
        -> a -> m r
getEnv1 h f a = getEnv h (\env -> f env a)

getEnvSTM :: forall m l blk r. (IOLike m, HasCallStack, HasHeader blk)
          => ForkerHandle m l blk
          -> (ForkerEnv m l blk -> STM m r)
          -> STM m r
getEnvSTM (ForkerHandle varState) f = readTVar varState >>= \case
    ForkerOpen env -> f env
    ForkerClosed   -> throwSTM $ ClosedForkerError @blk prettyCallStack

{-------------------------------------------------------------------------------
  Forkers
-------------------------------------------------------------------------------}

newForker ::
     ( IOLike m
     , HasLedgerTables l
     , GetTip l
     , LedgerSupportsProtocol blk
     , NoThunks (l EmptyMK)
     )
  => LedgerDBEnv m l blk
  -> LedgerBackingStoreValueHandle m l
  -> AnchorlessDbChangelog l
  -> m (Forker m l blk)
newForker ldbEnv vh dblog = do
    dblogVar <- newTVarIO dblog
    h <- ForkerHandle <$> newTVarIO (ForkerOpen $ forkerEnv dblogVar)
    pure $ mkForker h
  where
    forkerEnv dblogVar = ForkerEnv {
        foeBackingStoreValueHandle = vh
      , foeChangelog = dblogVar
      , foeSecurityParam = ldbSecParam ldbEnv
      , switchVar = ldbChangelog ldbEnv
      }

mkForker ::
     ( IOLike m
     , HasHeader blk
     , HasLedgerTables l
     , GetTip l
     )
  => ForkerHandle m l blk
  -> Forker m l blk
mkForker h = Forker {
      forkerClose = forkerClose' h
    , forkerReadTables = getEnv1 h forkerReadTables'
    , forkerRangeReadTables = getEnv1 h forkerRangeReadTables'
    , forkerGetLedgerState = getEnvSTM h forkerGetLedgerState'
    , forkerReadStatistics = getEnv h forkerReadStatistics'
    , forkerPush = getEnv1 h forkerPush'
    , forkerCommit = getEnvSTM h forkerCommit'
    }

forkerClose' :: MonadSTM m => ForkerHandle m l blk -> m ()
forkerClose' (ForkerHandle varState) = do
    envMay <- atomically $ readTVar varState >>= \case
      ForkerClosed   -> pure Nothing
      ForkerOpen env -> do
        writeTVar varState ForkerClosed
        pure $ Just env
    whenJust envMay cleanup
  where
    cleanup ForkerEnv { foeBackingStoreValueHandle } =
      bsvhClose foeBackingStoreValueHandle

forkerReadTables' ::
     (MonadSTM m, HasLedgerTables l)
  => ForkerEnv m l blk
  -> LedgerTables l KeysMK
  -> m (LedgerTables l ValuesMK)
forkerReadTables' env ks = do
    ldb <- readTVarIO $ foeChangelog env
    let rew = rewindTableKeySets ldb ks
    unfwd <- readKeySetsWith lvh rew
    case forwardTableKeySets ldb unfwd of
        Left _err -> error "impossible!"
        Right vs  -> pure vs
  where
    lvh = foeBackingStoreValueHandle env

forkerRangeReadTables' ::
     (MonadSTM m, HasLedgerTables l)
  => ForkerEnv m l blk
  -> API.RangeQuery l
  -> m (LedgerTables l ValuesMK)
forkerRangeReadTables' env rq0 = do
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

forkerGetLedgerState' ::
     (MonadSTM m, GetTip l)
  => ForkerEnv m l blk
  -> STM m (l EmptyMK)
forkerGetLedgerState' env = current <$> readTVar (foeChangelog env)

-- | Obtain statistics for a combination of backing store value handle and
-- changelog.
forkerReadStatistics' ::
     (MonadSTM m, HasLedgerTables l)
  => ForkerEnv m l blk
  -> m (Maybe API.Statistics)
forkerReadStatistics' env = do
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

forkerPush' ::
     (MonadSTM m, GetTip l, HasLedgerTables l)
  => ForkerEnv m l blk
  -> l DiffMK
  -> m ()
forkerPush' env newState = atomically $ do
  db <- readTVar (foeChangelog env)
  let db' = prune (foeSecurityParam env)
          $ extend (DbCh.ValidLedgerState newState) db
  writeTVar (foeChangelog env) db'

forkerCommit' ::
     (MonadSTM m, GetTip l, HasLedgerTables l)
  => ForkerEnv m l blk
  -> STM m ()
forkerCommit' env = do
  dblog <- readTVar (foeChangelog env)
  modifyTVar (switchVar env) (\pruned ->
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
