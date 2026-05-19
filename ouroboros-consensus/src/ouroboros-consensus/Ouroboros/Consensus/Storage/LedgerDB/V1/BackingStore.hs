{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | See "Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API" for the
-- documentation. This module just puts together the implementations for the
-- API, currently two:
--
-- * "Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.InMemory": a
--   @TVar@ holding a "Data.Map".
--
-- * "Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB": an
--   external disk-based database.
module Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore
  ( -- * API

    -- | Most of the documentation on the behaviour of the 'BackingStore' lives
    -- in this module.
    module Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API

    -- * Initialization
  , newBackingStore
  , restoreBackingStore

    -- * Tracing
  , SomeBackendTrace (..)
  , SomeBackendArgs (..)
  , Backend (..)
  ) where

import Cardano.Slotting.Slot
import Control.Tracer
import Data.Proxy
import Data.Typeable
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API
import System.FS.API

type BackingStoreInitialiser m l =
  InitFrom (LedgerTables l ValuesMK) ->
  m (LedgerBackingStore m l)

-- | Overwrite the 'BackingStore' tables with the snapshot's tables
restoreBackingStore ::
  Tracer m SomeBackendTrace ->
  SomeBackendArgs m l ->
  SnapshotsFS m ->
  l EmptyMK ->
  FsPath ->
  m (LedgerBackingStore m l)
restoreBackingStore trcr (SomeBackendArgs bArgs) fs st loadPath =
  newBackingStoreInitialiser trcr bArgs fs (InitFromCopy st loadPath)

-- | Create a 'BackingStore' from the given initial tables.
newBackingStore ::
  Tracer m SomeBackendTrace ->
  SomeBackendArgs m l ->
  SnapshotsFS m ->
  l EmptyMK ->
  LedgerTables l ValuesMK ->
  m (LedgerBackingStore m l)
newBackingStore trcr (SomeBackendArgs bArgs) fs st tables =
  newBackingStoreInitialiser trcr bArgs fs (InitFromValues Origin st tables)

data SomeBackendArgs m l where
  SomeBackendArgs :: Backend m backend l => Args m backend -> SomeBackendArgs m l

data SomeBackendTrace where
  SomeBackendTrace :: (Show (Trace backend), Typeable backend) => Trace backend -> SomeBackendTrace

instance Show SomeBackendTrace where
  show (SomeBackendTrace tr) = show tr

class Backend m backend l where
  data Args m backend

  data Trace backend

  isRightBackendForSnapshot ::
    Proxy l ->
    Args m backend ->
    SnapshotBackend ->
    Bool

  newBackingStoreInitialiser ::
    Tracer m SomeBackendTrace ->
    Args m backend ->
    SnapshotsFS m ->
    BackingStoreInitialiser m l
