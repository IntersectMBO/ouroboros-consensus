{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | LMDB resource status with read-append-write locking
module Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB.Status
  ( -- * Status
    Status (..)
  , StatusLock

    -- * Locks
  , new
  , withReadAccess
  , withWriteAccess
  ) where

import Control.RAWLock (RAWLock)
import qualified Control.RAWLock as RAW
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Util.IOLike (IOLike)

{-------------------------------------------------------------------------------
  Status
-------------------------------------------------------------------------------}

-- | A 'RAWLock' for 'Status'.
newtype StatusLock m = StatusLock {getStatusLock :: RAWLock m Status}

-- | Whether a resource is open or closed.
--
-- Resources that we keep track of are: (i) the full LMDB backing store, and
-- (ii) each of the LMDB backing store value handles.
data Status = Open | Closed
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

{-------------------------------------------------------------------------------
  Locks
-------------------------------------------------------------------------------}

-- | Create a new 'StatusLock'.
new :: IOLike m => Status -> m (StatusLock m)
new st = StatusLock <$> RAW.new st

-- | A variant of 'RAW.withWriteAccess' that throws an exception if @'Status' ==
-- 'Closed'@.
--
-- Note: contrary to 'RAW.withWriteAccess', the action to perform with the
-- acquired lock is not of type @'Status' -> ('Status', a)@. The 'Status' is
-- known to be 'Open', or an exception would have been thrown.
withWriteAccess ::
  IOLike m =>
  StatusLock m ->
  -- | Action to perform if closed
  m a ->
  -- | Action to perform if open, possibly updating the 'Status'
  m (a, Status) ->
  m a
withWriteAccess lock ifClosed ifOpen =
  RAW.withWriteAccess (getStatusLock lock) $ \case
    Open -> ifOpen
    Closed -> ifClosed <&> (,Closed)

-- | A variant of 'RAW.withReadAccess' that throws an exception if @'Status' ==
-- 'Closed'@.
--
-- Note: contrary to 'RAW.withReadAccess', the action to perform with the
-- acquired lock is not of type @'Status' -> a@. The 'Status' is known to be
-- 'Open', or an exception would have been thrown.
withReadAccess ::
  IOLike m =>
  StatusLock m ->
  -- | Action to perform when closed
  m a ->
  -- | Action to perform when open
  m a ->
  m a
withReadAccess lock ifClosed ifOpen =
  RAW.withReadAccess (getStatusLock lock) $ \case
    Open -> ifOpen
    Closed -> ifClosed
