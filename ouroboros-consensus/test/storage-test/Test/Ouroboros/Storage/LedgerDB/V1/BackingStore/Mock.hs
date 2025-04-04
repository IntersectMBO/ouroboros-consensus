{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Ouroboros.Storage.LedgerDB.V1.BackingStore.Mock (
    -- * Types
    Err (..)
  , ID (..)
  , Mock (..)
  , ValueHandle (..)
  , ValueHandleStatus (..)
  , emptyMock
    -- * Type classes
  , ApplyDiff (..)
  , DiffSize (..)
  , EmptyValues (..)
  , HasOps
  , KeysSize (..)
  , LookupKeys (..)
  , LookupKeysRange (..)
  , MakeDiff (..)
  , MakeInitHint (..)
  , MakeReadHint (..)
  , MakeWriteHint (..)
  , ValuesLength (..)
    -- * State monad to run the mock in
  , MockMonad (..)
  , runMockMonad
    -- * Mocked @'BackingStore'@ operations
  , mBSClose
  , mBSCopy
  , mBSInitFromCopy
  , mBSInitFromValues
  , mBSVHAtSlot
  , mBSVHClose
  , mBSVHRangeRead
  , mBSVHRead
  , mBSVHStat
  , mBSValueHandle
  , mBSWrite
  , mGuardBSClosed
  , mGuardBSVHClosed
  ) where

import           Control.Monad
import           Control.Monad.Except (ExceptT (..), MonadError (throwError),
                     runExceptT)
import           Control.Monad.State (MonadState, State, StateT (StateT), gets,
                     modify, runState)
import           Data.Data (Proxy, Typeable)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Ouroboros.Consensus.Block.Abstract (SlotNo, WithOrigin (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore as BS
import qualified System.FS.API.Types as FS

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data Mock vs = Mock {
    backingValues :: vs
  , backingSeqNo  :: WithOrigin SlotNo
  , copies        :: Map FS.FsPath (WithOrigin SlotNo, vs)
  , isClosed      :: Bool
    -- | Track whether value handles have been closed.
  , valueHandles  :: Map ID ValueHandleStatus
    -- | The next id to use if a new value handle is opened.
  , nextId        :: ID
  }
  deriving stock (Show, Eq)

data ValueHandleStatus = Open | ClosedByStore | ClosedByHandle
  deriving stock (Show, Eq)

data ValueHandle values = ValueHandle {
    getId  :: ID
  , values :: values
  , seqNo  :: WithOrigin SlotNo
  }
  deriving stock Show

instance Eq (ValueHandle vs) where
  x == y = getId x == getId y

instance Ord (ValueHandle vs) where
  x <= y = getId x < getId y

-- | An ID for a mocked value handle.
newtype ID = ID Word
  deriving stock (Show, Eq, Ord)
  deriving newtype Num

-- | An empty mock state.
emptyMock :: EmptyValues vs => Mock vs
emptyMock = Mock {
    backingValues = emptyValues
  , backingSeqNo  = Origin
  , copies        = Map.empty
  , isClosed      = False
  , valueHandles  = Map.empty
  , nextId        = 0
  }

data Err =
    ErrBackingStoreClosed
  | ErrBackingStoreValueHandleClosed
  | ErrCopyPathAlreadyExists
  | ErrCopyPathDoesNotExist
  | ErrNonMonotonicSeqNo (WithOrigin SlotNo) (WithOrigin SlotNo)
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Type classes
-------------------------------------------------------------------------------}

-- | Abstract over interactions between values, keys and diffs.
class ( EmptyValues vs, ApplyDiff vs d, LookupKeysRange ks vs
      , LookupKeys ks vs, ValuesLength vs, MakeDiff vs d
      , DiffSize d, KeysSize ks
      , MakeInitHint vs, MakeWriteHint d, MakeReadHint vs
      , Show ks, Show vs, Show d
      , Show (BS.InitHint vs), Show (BS.WriteHint d)
      , Show (BS.ReadHint vs)
      , Eq ks, Eq vs, Eq d
      , Eq (BS.InitHint vs), Eq (BS.WriteHint d)
      , Eq (BS.ReadHint vs)
      , Typeable ks, Typeable vs, Typeable d
      , Typeable (BS.InitHint vs), Typeable (BS.WriteHint d)
      , Typeable (BS.ReadHint vs)
      ) => HasOps ks vs d

class EmptyValues vs where
  emptyValues :: vs

class ApplyDiff vs d where
  applyDiff :: vs -> d -> vs

class LookupKeysRange ks vs where
  lookupKeysRange :: Maybe ks -> Int -> vs -> vs

class LookupKeys ks vs where
  lookupKeys :: ks -> vs -> vs

class ValuesLength vs where
  valuesLength :: vs -> Int

class MakeDiff vs d where
  diff :: vs -> vs -> d

-- | Counts how many diffs are there. Not to be confused with how many values
-- result from the diffs.
class DiffSize d where
  diffSize :: d -> Int

class KeysSize ks where
  keysSize :: ks -> Int

class MakeInitHint vs where
  makeInitHint :: Proxy vs -> BS.InitHint vs

class MakeWriteHint d where
  makeWriteHint :: Proxy d -> BS.WriteHint d

class MakeReadHint vs where
  makeReadHint :: Proxy vs -> BS.ReadHint vs

{-------------------------------------------------------------------------------
  State monad to run the mock in
-------------------------------------------------------------------------------}

-- | State within which the mock runs.
newtype MockMonad ks vs d a =
    MockMonad (ExceptT Err (State (Mock vs)) a)
  deriving stock     Functor
  deriving newtype ( Applicative
                   , Monad
                   , MonadState (Mock vs)
                   , MonadError Err
                   )

runMockMonad ::
     MockMonad ks vs d a
  -> Mock vs
  -> (Either Err a, Mock vs)
runMockMonad (MockMonad t) = runState . runExceptT $ t

{------------------------------------------------------------------------------
  Mocked @'BackingStore'@ operations
------------------------------------------------------------------------------}

mBSInitFromValues ::
     forall vs m. (MonadState (Mock vs) m)
  => WithOrigin SlotNo
  -> BS.InitHint vs
  -> vs
  -> m ()
mBSInitFromValues sl _st vs = modify (\m -> m {
    backingValues = vs
  , backingSeqNo  = sl
  , isClosed      = False
  })

mBSInitFromCopy ::
     forall vs m. (MonadState (Mock vs) m, MonadError Err m)
  => BS.InitHint vs
  -> FS.FsPath
  -> m ()
mBSInitFromCopy _st bsp = do
  cps <- gets copies
  case Map.lookup bsp cps of
    Nothing       -> throwError ErrCopyPathDoesNotExist
    Just (sl, vs) -> modify (\m -> m {
        backingValues = vs
      , backingSeqNo  = sl
      , isClosed      = False
      })

-- | Throw an error if the backing store has been closed.
mGuardBSClosed :: (MonadState (Mock vs) m, MonadError Err m) => m ()
mGuardBSClosed = do
  closed <- gets isClosed
  when closed $
    throwError ErrBackingStoreClosed

-- | Close the backing store.
--
-- Closing is idempotent.
mBSClose :: MonadState (Mock vs) m => m ()
mBSClose = do
    closed <- gets isClosed
    unless closed $
      modify (\m -> m {
          isClosed = True
        , valueHandles = fmap (const ClosedByStore) (valueHandles m)
        })

-- | Copy the contents of the backing store to the given path.
mBSCopy :: (MonadState (Mock vs) m, MonadError Err m) => FS.FsPath ->  m ()
mBSCopy bsp = do
  mGuardBSClosed
  cps <- gets copies
  when (bsp `Map.member` cps) $
    throwError ErrCopyPathAlreadyExists
  modify (\m -> m {
      copies = Map.insert bsp (backingSeqNo m, backingValues m) (copies m)
    })

-- | Open a new value handle, which captures the state of the backing store
-- at the time of opening the handle.
mBSValueHandle ::
     (MonadState (Mock vs) m, MonadError Err m)
  => m (ValueHandle vs)
mBSValueHandle = do
  mGuardBSClosed
  vs <- gets backingValues
  seqNo <- gets backingSeqNo
  nxt <- gets nextId
  let
    vh = ValueHandle nxt vs seqNo
  modify (\m -> m {
      valueHandles = Map.insert nxt Open (valueHandles m)
    , nextId = nxt + 1
    })

  pure vh

-- | Write a diff to the backing store.
mBSWrite ::
     (MonadState (Mock vs) m, MonadError Err m, ApplyDiff vs d)
  => SlotNo
  -> BS.WriteHint d
  -> d
  -> m ()
mBSWrite sl _st d = do
  mGuardBSClosed
  vs <- gets backingValues
  seqNo <- gets backingSeqNo
  when (seqNo > NotOrigin sl) $
    throwError $ ErrNonMonotonicSeqNo (NotOrigin sl) seqNo
  modify (\m -> m {
      backingValues = applyDiff vs d
    , backingSeqNo = NotOrigin sl
    })

-- | Throw an error if the given backing store value handle has been closed.
mGuardBSVHClosed ::
     (MonadState (Mock vs) m, MonadError Err m)
  => ValueHandle vs
  -> m ()
mGuardBSVHClosed vh = do
  status <- mLookupValueHandle vh
  case status of
    ClosedByStore  -> throwError ErrBackingStoreClosed
    ClosedByHandle -> throwError ErrBackingStoreValueHandleClosed
    _              -> pure ()

mLookupValueHandle ::
     MonadState (Mock vs) m
  => ValueHandle vs
  -> m ValueHandleStatus
mLookupValueHandle vh = do
  vhs <- gets valueHandles
  case Map.lookup (getId vh) vhs of
    Nothing     -> error "Value handle not found"
    Just status -> pure status

-- | Close a backing store value handle.
--
-- Closing is idempotent.
mBSVHClose ::
     MonadState (Mock vs) m
  => ValueHandle vs
  -> m ()
mBSVHClose vh = do
    status <- mLookupValueHandle vh
    case status of
      ClosedByStore  -> pure ()
      ClosedByHandle -> pure ()
      _              ->
        modify (\m -> m {
            valueHandles = Map.adjust (const ClosedByHandle) (getId vh) (valueHandles m)
          })

-- | Perform a range read on a backing store value handle.
mBSVHRangeRead ::
     (MonadState (Mock vs) m, MonadError Err m, LookupKeysRange ks vs)
  => ValueHandle vs
  -> BS.ReadHint vs
  -> BS.RangeQuery ks
  -> m vs
mBSVHRangeRead vh _ BS.RangeQuery{BS.rqPrev, BS.rqCount} = do
  mGuardBSClosed
  mGuardBSVHClosed vh
  let
    vs = values vh
  pure $ lookupKeysRange rqPrev rqCount vs

-- | Perform a regular read on a backing store value handle
mBSVHRead ::
     (MonadState (Mock vs) m, MonadError Err m, LookupKeys ks vs)
  => ValueHandle vs
  -> BS.ReadHint vs
  -> ks
  -> m vs
mBSVHRead vh _ ks = do
  mGuardBSClosed
  mGuardBSVHClosed vh
  let vs = values vh
  pure $ lookupKeys ks vs

-- | Read the slot number out of a value handle
mBSVHAtSlot :: Monad m => ValueHandle vs -> m (WithOrigin SlotNo)
mBSVHAtSlot = pure . seqNo

-- | Retrieve statistics for the backing store value handle.
mBSVHStat ::
     (MonadState (Mock vs) m, MonadError Err m, ValuesLength vs)
  => ValueHandle vs
  -> m BS.Statistics
mBSVHStat vh = do
  mGuardBSClosed
  mGuardBSVHClosed vh
  pure $ BS.Statistics (seqNo vh) (valuesLength $ values vh)
