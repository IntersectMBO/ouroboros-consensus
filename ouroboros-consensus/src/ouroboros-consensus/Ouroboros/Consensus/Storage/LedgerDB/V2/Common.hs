{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.Common (
    -- * LedgerDBEnv
    LDBLock (..)
  , LedgerDBEnv (..)
  , LedgerDBHandle (..)
  , LedgerDBState (..)
  , closeAllForkers
  , getEnv
  , getEnv2
  , getEnv5
  , getEnvSTM
    -- * Forkers
  , newForkerAtTarget
  , newForkerByRollback
  ) where

import           Control.Arrow
import           Control.Monad ((>=>))
import           Control.RAWLock (RAWLock)
import qualified Control.RAWLock as RAWLock
import           Control.ResourceRegistry
import           Control.Tracer
import           Data.Functor.Contravariant ((>$<))
import           Data.Kind
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import           Data.Word
import           GHC.Generics
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Storage.LedgerDB.API
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Common
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Snapshots
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Validate
import           Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.NormalForm.StrictTVar ()
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
import           Prelude hiding (read)
import           System.FS.API

{-------------------------------------------------------------------------------
  The LedgerDBEnv
-------------------------------------------------------------------------------}

data LDBLock = LDBLock deriving (Generic, NoThunks)

type LedgerDBEnv :: (Type -> Type) -> LedgerStateKind -> Type -> Type
data LedgerDBEnv m l blk = LedgerDBEnv {
    -- | INVARIANT: the tip of the 'LedgerDB' is always in sync with the tip of
    -- the current chain of the ChainDB.
    ldbSeq             :: !(StrictTVar m (LedgerSeq m l))
    -- | INVARIANT: this set contains only points that are in the
    -- VolatileDB.
    --
    -- INVARIANT: all points on the current chain fragment are in this set.
    --
    -- The VolatileDB might contain invalid blocks, these will not be in
    -- this set.
    --
    -- When a garbage-collection is performed on the VolatileDB, the points
    -- of the blocks eligible for garbage-collection should be removed from
    -- this set.
  , ldbPrevApplied     :: !(StrictTVar m (Set (RealPoint blk)))
    -- | Open forkers.
    --
    -- INVARIANT: a forker is open iff its 'ForkerKey' is in this 'Map.
  , ldbForkers         :: !(StrictTVar m (Map ForkerKey (ForkerEnv m l blk)))
  , ldbNextForkerKey   :: !(StrictTVar m ForkerKey)

  , ldbSnapshotPolicy  :: !SnapshotPolicy
  , ldbTracer          :: !(Tracer m (TraceLedgerDBEvent blk))
  , ldbCfg             :: !(LedgerDbCfg l)
  , ldbHasFS           :: !(SomeHasFS m)
  , ldbResolveBlock    :: !(ResolveBlock m blk)
  , ldbQueryBatchSize  :: !(Maybe Int)
  , ldbOpenHandlesLock :: !(RAWLock m LDBLock)
  } deriving (Generic)

deriving instance ( IOLike m
                  , LedgerSupportsProtocol blk
                  , NoThunks (l EmptyMK)
                  , NoThunks (TxIn l)
                  , NoThunks (TxOut l)
                  , NoThunks (LedgerCfg l)
                  ) => NoThunks (LedgerDBEnv m l blk)

{-------------------------------------------------------------------------------
  The LedgerDBHandle
-------------------------------------------------------------------------------}

type LedgerDBHandle :: (Type -> Type) -> LedgerStateKind -> Type -> Type
newtype LedgerDBHandle m l blk =
    LDBHandle (StrictTVar m (LedgerDBState m l blk))
  deriving Generic

data LedgerDBState m l blk =
    LedgerDBOpen !(LedgerDBEnv m l blk)
  | LedgerDBClosed
  deriving Generic

deriving instance ( IOLike m
                  , LedgerSupportsProtocol blk
                  , NoThunks (l EmptyMK)
                  , NoThunks (TxIn l)
                  , NoThunks (TxOut l)
                  , NoThunks (LedgerCfg l)
                  ) => NoThunks (LedgerDBState m l blk)


-- | Check if the LedgerDB is open, if so, executing the given function on the
-- 'LedgerDBEnv', otherwise, throw a 'CloseDBError'.
getEnv ::
     forall m l blk r. (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> (LedgerDBEnv m l blk -> m r)
  -> m r
getEnv (LDBHandle varState) f = readTVarIO varState >>= \case
    LedgerDBOpen env -> f env
    LedgerDBClosed   -> throwIO $ ClosedDBError @blk prettyCallStack

-- | Variant 'of 'getEnv' for functions taking two arguments.
getEnv2 ::
     (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> (LedgerDBEnv m l blk -> a -> b -> m r)
  -> a -> b -> m r
getEnv2 h f a b = getEnv h (\env -> f env a b)

-- | Variant 'of 'getEnv' for functions taking five arguments.
getEnv5 ::
     (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> (LedgerDBEnv m l blk -> a -> b -> c -> d -> e -> m r)
  -> a -> b -> c -> d -> e -> m r
getEnv5 h f a b c d e = getEnv h (\env -> f env a b c d e)

-- | Variant of 'getEnv' that works in 'STM'.
getEnvSTM ::
     forall m l blk r. (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> (LedgerDBEnv m l blk -> STM m r)
  -> STM m r
getEnvSTM (LDBHandle varState) f = readTVar varState >>= \case
    LedgerDBOpen env -> f env
    LedgerDBClosed   -> throwSTM $ ClosedDBError @blk prettyCallStack

{-------------------------------------------------------------------------------
  Forker operations
-------------------------------------------------------------------------------}

data ForkerEnv m l blk = ForkerEnv {
    -- | Local version of the LedgerSeq
    foeLedgerSeq          :: !(StrictTVar m (LedgerSeq m l))
    -- | This TVar is the same as the LedgerDB one
  , foeSwitchVar          :: !(StrictTVar m (LedgerSeq m l))
    -- | Config
  , foeSecurityParam      :: !SecurityParam
    -- | The batch size
  , foeQueryBatchSize     :: !(Maybe Int)
    -- | Config
  , foeTracer             :: !(Tracer m TraceForkerEvent)
    -- | Release the resources
  , foeResourcesToRelease :: !(StrictTVar m (m ()))
  }
  deriving Generic

closeForkerEnv :: IOLike m => (LedgerDBEnv m l blk, ForkerEnv m l blk) -> m ()
closeForkerEnv (LedgerDBEnv{ldbOpenHandlesLock}, frkEnv) =
  RAWLock.withWriteAccess ldbOpenHandlesLock $
    const $ do
      id =<< readTVarIO (foeResourcesToRelease frkEnv)
      atomically $ writeTVar (foeResourcesToRelease frkEnv) (pure ())
      pure ((), LDBLock)

deriving instance ( IOLike m
                  , LedgerSupportsProtocol blk
                  , NoThunks (l EmptyMK)
                  , NoThunks (TxIn l)
                  , NoThunks (TxOut l)
                  ) => NoThunks (ForkerEnv m l blk)

getForkerEnv ::
     forall m l blk r. (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> ForkerKey
  -> (ForkerEnv m l blk -> m r)
  -> m r
getForkerEnv (LDBHandle varState) forkerKey f = do
    forkerEnv <- atomically $ readTVar varState >>= \case
      LedgerDBClosed   -> throwIO $ ClosedDBError @blk prettyCallStack
      LedgerDBOpen env -> readTVar (ldbForkers env) >>= (Map.lookup forkerKey >>> \case
        Nothing        -> throwSTM $ ClosedForkerError @blk forkerKey prettyCallStack
        Just forkerEnv -> pure forkerEnv)
    f forkerEnv

getForkerEnv1 ::
     (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> ForkerKey
  -> (ForkerEnv m l blk -> a -> m r)
  -> a -> m r
getForkerEnv1 h forkerKey f a = getForkerEnv h forkerKey (`f` a)

getForkerEnvSTM ::
     forall m l blk r. (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> ForkerKey
  -> (ForkerEnv m l blk -> STM m r)
  -> STM m r
getForkerEnvSTM (LDBHandle varState) forkerKey f = readTVar varState >>= \case
    LedgerDBClosed   -> throwIO $ ClosedDBError @blk prettyCallStack
    LedgerDBOpen env -> readTVar (ldbForkers env) >>= (Map.lookup forkerKey >>> \case
      Nothing        -> throwSTM $ ClosedForkerError @blk forkerKey prettyCallStack
      Just forkerEnv -> f forkerEnv)

newForker ::
     ( IOLike m
     , HasLedgerTables l
     , LedgerSupportsProtocol blk
     , NoThunks (l EmptyMK)
     , GetTip l
     , StandardHash l
     )
  => LedgerDBHandle m l blk
  -> LedgerDBEnv m l blk
  -> ResourceRegistry m
  -> StateRef m l
  -> m (Forker m l blk)
newForker h ldbEnv rr st = do
    forkerKey <- atomically $ stateTVar (ldbNextForkerKey ldbEnv) $ \r -> (r, r + 1)
    let tr = LedgerDBForkerEvent . TraceForkerEventWithKey forkerKey >$< ldbTracer ldbEnv
    traceWith tr ForkerOpen
    lseqVar   <- newTVarIO . LedgerSeq . AS.Empty $ st
    (_, toRelease) <- allocate rr (\_ -> newTVarIO (pure ())) (readTVarIO >=> id)
    let forkerEnv = ForkerEnv {
        foeLedgerSeq          = lseqVar
      , foeSwitchVar          = ldbSeq ldbEnv
      , foeSecurityParam      = ledgerDbCfgSecParam $ ldbCfg ldbEnv
      , foeQueryBatchSize     = ldbQueryBatchSize ldbEnv
      , foeTracer             = tr
      , foeResourcesToRelease = toRelease
      }
    atomically $ modifyTVar (ldbForkers ldbEnv) $ Map.insert forkerKey forkerEnv
    pure $ Forker {
        forkerReadTables             = getForkerEnv1   h forkerKey implForkerReadTables
      , forkerRangeReadTables        = getForkerEnv1   h forkerKey implForkerRangeReadTables
      , forkerGetLedgerState         = getForkerEnvSTM h forkerKey implForkerGetLedgerState
      , forkerReadStatistics         = getForkerEnv    h forkerKey implForkerReadStatistics
      , forkerPush                   = getForkerEnv1   h forkerKey implForkerPush
      , forkerCommit                 = getForkerEnvSTM h forkerKey implForkerCommit
      , forkerClose                  = implForkerClose h forkerKey
      }

-- | Will release all handles in the 'foeLedgerSeq'.
implForkerClose ::
     IOLike m
  => LedgerDBHandle m l blk
  -> ForkerKey
  -> m ()
implForkerClose (LDBHandle varState) forkerKey = do
    menv <- atomically $ readTVar varState >>= \case
      LedgerDBClosed      -> pure Nothing
      LedgerDBOpen ldbEnv -> fmap (ldbEnv,) <$>
        stateTVar
            (ldbForkers ldbEnv)
            (Map.updateLookupWithKey (\_ _ -> Nothing) forkerKey)
    whenJust menv closeForkerEnv

implForkerReadTables ::
     (MonadSTM m, GetTip l)
  => ForkerEnv m l blk
  -> LedgerTables l KeysMK
  -> m (LedgerTables l ValuesMK)
implForkerReadTables env ks = do
    traceWith (foeTracer env) ForkerReadTablesStart
    lseq <- readTVarIO (foeLedgerSeq env)
    tbs <- read (tables $ currentHandle lseq) ks
    traceWith (foeTracer env) ForkerReadTablesEnd
    pure tbs

implForkerRangeReadTables ::
     (MonadSTM m, GetTip l, HasLedgerTables l)
  => ForkerEnv m l blk
  -> RangeQueryPrevious l
  -> m (LedgerTables l ValuesMK)
implForkerRangeReadTables env rq0 = do
    traceWith (foeTracer env) ForkerRangeReadTablesStart
    ldb <- readTVarIO $ foeLedgerSeq env
    let n = maybe 100_000 id $ foeQueryBatchSize env
    case rq0 of
      NoPreviousQuery -> readRange (tables $ currentHandle ldb) (Nothing, n)
      PreviousQueryWasFinal -> pure $ LedgerTables emptyMK
      PreviousQueryWasUpTo k -> do
        tbs <- readRange (tables $ currentHandle ldb) (Just k, n)
        traceWith (foeTracer env) ForkerRangeReadTablesEnd
        pure tbs

implForkerGetLedgerState ::
     (MonadSTM m, GetTip l)
  => ForkerEnv m l blk
  -> STM m (l EmptyMK)
implForkerGetLedgerState env = current <$> readTVar (foeLedgerSeq env)

implForkerReadStatistics ::
     (MonadSTM m, GetTip l)
  => ForkerEnv m l blk
  -> m (Maybe Statistics)
implForkerReadStatistics env = do
  traceWith (foeTracer env) ForkerReadStatistics
  fmap (fmap Statistics) . tablesSize . tables . currentHandle =<< readTVarIO (foeLedgerSeq env)

implForkerPush ::
     (IOLike m, GetTip l, HasLedgerTables l, HasCallStack)
  => ForkerEnv m l blk
  -> l DiffMK
  -> m ()
implForkerPush env newState = do
  traceWith (foeTracer env) ForkerPushStart
  lseq <- readTVarIO (foeLedgerSeq env)
  let (st, tbs) = (forgetLedgerTables newState, ltprj newState)

  bracketOnError
    (duplicate (tables $ currentHandle lseq))
    close
    (\newtbs -> do
        pushDiffs newtbs tbs

        let lseq' = extend (StateRef st newtbs) lseq

        traceWith (foeTracer env) ForkerPushEnd
        atomically $ do
               writeTVar (foeLedgerSeq env) lseq'
               modifyTVar (foeResourcesToRelease env) (>> close newtbs)
     )

implForkerCommit ::
     (IOLike m, GetTip l, StandardHash l)
  => ForkerEnv m l blk
  -> STM m ()
implForkerCommit env = do
  LedgerSeq lseq <- readTVar foeLedgerSeq
  let intersectionSlot = getTipSlot $ state $ AS.anchor lseq
  let predicate = (== getTipHash (state (AS.anchor lseq))) . getTipHash . state
  (discardedBySelection, LedgerSeq discardedByPruning) <- do
    stateTVar
      foeSwitchVar
      (\(LedgerSeq olddb) -> fromMaybe theImpossible $ do
          -- Split the selection at the intersection point. The snd component will
          -- have to be closed.
          (olddb', toClose) <- AS.splitAfterMeasure intersectionSlot (either predicate predicate) olddb
          -- Join the prefix of the selection with the sequence in the forker
          newdb <- AS.join (const $ const True) olddb' lseq
          -- Prune the resulting sequence to keep @k@ states
          let (l, s) = prune (foeSecurityParam env) (LedgerSeq newdb)
          pure ((toClose, l), s)
      )

  -- We are discarding the previous value in the TVar because we had accumulated
  -- actions for closing the states pushed to the forker. As we are committing
  -- those we have to close the ones discarded in this function and forget about
  -- those releasing actions.
  writeTVar foeResourcesToRelease $
       mapM_ (close . tables) $ AS.toOldestFirst discardedBySelection ++ AS.toOldestFirst discardedByPruning

  where
    ForkerEnv {
        foeLedgerSeq
      , foeSwitchVar
      , foeResourcesToRelease
      } = env

    theImpossible =
      error $ unwords [ "Critical invariant violation:"
                      , "Forker chain does no longer intersect with selected chain."
                      ]

{-------------------------------------------------------------------------------
  Acquiring consistent views
-------------------------------------------------------------------------------}

-- | This function must hold the 'LDBLock' such that handles are not released
-- before they are duplicated.
acquireAtTarget ::
     ( HeaderHash l ~ HeaderHash blk
     , IOLike m
     , GetTip l
     , StandardHash l
     , LedgerSupportsProtocol blk
     )
  => LedgerDBEnv m l blk
  -> Either Word64 (Target (Point blk))
  -> LDBLock
  -> m (Either GetForkerError (StateRef m l))
acquireAtTarget ldbEnv (Right VolatileTip) _ = do
  l <- readTVarIO (ldbSeq ldbEnv)
  let StateRef st tbs = currentHandle l
  t <- duplicate tbs
  pure $ Right $ StateRef st t
acquireAtTarget ldbEnv (Right ImmutableTip) _ = do
  l <- readTVarIO (ldbSeq ldbEnv)
  let StateRef st tbs = anchorHandle l
  t <- duplicate tbs
  pure $ Right $ StateRef st t
acquireAtTarget ldbEnv (Right (SpecificPoint pt)) _ = do
  dblog <- readTVarIO (ldbSeq ldbEnv)
  let immTip = getTip $ anchor dblog
  case currentHandle <$> rollback pt dblog of
    Nothing | pointSlot pt < pointSlot immTip -> pure $ Left $ PointTooOld Nothing
            | otherwise   -> pure $ Left PointNotOnChain
    Just (StateRef st tbs) ->
          Right . StateRef st <$> duplicate tbs
acquireAtTarget ldbEnv (Left n) _ = do
      dblog <- readTVarIO (ldbSeq ldbEnv)
      case currentHandle <$> rollbackN n dblog of
        Nothing ->
          return $ Left $ PointTooOld $ Just $ ExceededRollback {
              rollbackMaximum   = maxRollback dblog
            , rollbackRequested = n
            }
        Just (StateRef st tbs) ->
              Right . StateRef st <$> duplicate tbs

newForkerAtTarget ::
     ( HeaderHash l ~ HeaderHash blk
     , IOLike m
     , IsLedger l
     , HasLedgerTables l
     , LedgerSupportsProtocol blk
     , StandardHash l
     )
  => LedgerDBHandle m l blk
  -> ResourceRegistry m
  -> Target (Point blk)
  -> m (Either GetForkerError (Forker m l blk))
newForkerAtTarget h rr pt = getEnv h $ \ldbEnv@LedgerDBEnv{ldbOpenHandlesLock = lock} ->
    RAWLock.withReadAccess lock (acquireAtTarget ldbEnv (Right pt)) >>= traverse (newForker h ldbEnv rr)

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
  -> Word64
  -> m (Either GetForkerError (Forker m l blk))
newForkerByRollback h rr n = getEnv h $ \ldbEnv@LedgerDBEnv{ldbOpenHandlesLock = lock} -> do
    RAWLock.withReadAccess lock (acquireAtTarget ldbEnv (Left n)) >>= traverse (newForker h ldbEnv rr)

-- | Close all open 'Forker's.
closeAllForkers ::
     IOLike m
  => LedgerDBEnv m l blk
  -> m ()
closeAllForkers ldbEnv = do
    toClose <- fmap (ldbEnv,) <$> (atomically $ stateTVar forkersVar (, Map.empty))
    mapM_ closeForkerEnv toClose
  where
    forkersVar = ldbForkers ldbEnv
