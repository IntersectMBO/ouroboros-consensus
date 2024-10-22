{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
#if __GLASGOW_HASKELL__ <= 906
{-# LANGUAGE GADTs #-}
#endif
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.V1.Common (
    -- * LedgerDB internal state
    LedgerDBEnv (..)
  , LedgerDBHandle (..)
  , LedgerDBState (..)
  , getEnv
  , getEnv1
  , getEnv2
  , getEnv5
  , getEnvSTM
  , getEnvSTM1
    -- * Forkers
  , ForkerEnv (..)
  , getForkerEnv
  , getForkerEnv1
  , getForkerEnvSTM
  ) where

import           Control.Arrow
import           Control.Tracer
import           Data.Kind
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.LedgerDB.API as API
import           Ouroboros.Consensus.Storage.LedgerDB.API.Config
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Common
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Snapshots
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Validate
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Args
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Lock
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  LedgerDB internal state
-------------------------------------------------------------------------------}

newtype LedgerDBHandle m l blk = LDBHandle (StrictTVar m (LedgerDBState m l blk))
  deriving Generic

data LedgerDBState m l blk =
    LedgerDBOpen !(LedgerDBEnv m l blk)
  | LedgerDBClosed
  deriving Generic

deriving instance ( IOLike m
                  , LedgerSupportsProtocol blk
                  , NoThunks (l EmptyMK)
                  , NoThunks (Key l)
                  , NoThunks (Value l)
                  , NoThunks (LedgerCfg l)
                  ) => NoThunks (LedgerDBState m l blk)

type LedgerDBEnv :: (Type -> Type) -> LedgerStateKind -> Type -> Type
data LedgerDBEnv m l blk = LedgerDBEnv {
    -- | INVARIANT: the tip of the 'LedgerDB' is always in sync with the tip of
    -- the current chain of the ChainDB.
    ldbChangelog      :: !(StrictTVar m (DbChangelog l))
    -- | Handle to the ledger's backing store, containing the parts that grow too
    -- big for in-memory residency
  , ldbBackingStore   :: !(LedgerBackingStore m l)
    -- | The flush lock to the 'BackingStore'. This lock is crucial when it
    -- comes to keeping the data in memory consistent with the data on-disk.
    --
    -- This lock should be held whenever we want to keep a consistent view of
    -- the backing store for some time. In particular we use this:
    --
    -- - when performing a query on the ledger state, we need to hold a
    --   'LocalStateQueryView' which, while live, must maintain a consistent view
    --   of the DB, and therefore we acquire a Read lock.
    --
    -- - when taking a snapshot of the ledger db, we need to prevent others
    --   from altering the backing store at the same time, thus we acquire a
    --   Write lock.
  , ldbLock           :: !(AllowThunk (LedgerDBLock m))
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
  , ldbPrevApplied    :: !(StrictTVar m (Set (RealPoint blk)))
    -- | Open forkers.
    --
    -- INVARIANT: a forker is open iff its 'ForkerKey' is in this 'Map.
  , ldbForkers        :: !(StrictTVar m (Map ForkerKey (ForkerEnv m l blk)))
  , ldbNextForkerKey  :: !(StrictTVar m ForkerKey)

  , ldbSnapshotPolicy :: !SnapshotPolicy
  , ldbTracer         :: !(Tracer m (TraceLedgerDBEvent blk))
  , ldbCfg            :: !(LedgerDbCfg l)
  , ldbHasFS          :: !(SnapshotsFS m)
  , ldbShouldFlush    :: !(Word64 -> Bool)
  , ldbQueryBatchSize :: !QueryBatchSize
  , ldbResolveBlock   :: !(ResolveBlock m blk)
  } deriving (Generic)

deriving instance ( IOLike m
                  , LedgerSupportsProtocol blk
                  , NoThunks (l EmptyMK)
                  , NoThunks (Key l)
                  , NoThunks (Value l)
                  , NoThunks (LedgerCfg l)
                  ) => NoThunks (LedgerDBEnv m l blk)

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

-- | Variant 'of 'getEnv' for functions taking one argument.
getEnv1 ::
     (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> (LedgerDBEnv m l blk -> a -> m r)
  -> a -> m r
getEnv1 h f a = getEnv h (`f` a)

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

-- | Variant of 'getEnv1' that works in 'STM'.
getEnvSTM1 ::
     forall m l blk a r. (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> (LedgerDBEnv m l blk -> a -> STM m r)
  -> a -> STM m r
getEnvSTM1 (LDBHandle varState) f a = readTVar varState >>= \case
    LedgerDBOpen env -> f env a
    LedgerDBClosed   -> throwSTM $ ClosedDBError @blk prettyCallStack

{-------------------------------------------------------------------------------
  Forkers
-------------------------------------------------------------------------------}

data ForkerEnv m l blk = ForkerEnv {
    -- | Local, consistent view of backing store
    foeBackingStoreValueHandle :: !(LedgerBackingStoreValueHandle m l)
    -- | In memory db changelog
  , foeChangelog               :: !(StrictTVar m (AnchorlessDbChangelog l))
    -- | Points to 'ldbChangelog'.
  , foeSwitchVar               :: !(StrictTVar m (DbChangelog l))
    -- | Config
  , foeSecurityParam           :: !SecurityParam
     -- | Config
  , foeQueryBatchSize          :: !QueryBatchSize
    -- | Resource registry
  , foeTracer                  :: !(Tracer m TraceForkerEvent)
  }
  deriving Generic

deriving instance ( IOLike m
                  , LedgerSupportsProtocol blk
                  , NoThunks (l EmptyMK)
                  , NoThunks (Key l)
                  , NoThunks (Value l)
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
