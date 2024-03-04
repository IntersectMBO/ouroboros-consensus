{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | See "Ouroboros.Consensus.Storage.LedgerDB.BackingStore.API" for the
-- documentation. This module just puts together the implementations for the
-- API, currently two:
--
-- * "Ouroboros.Consensus.Storage.LedgerDB.BackingStore.Impl.InMemory": a @TVar@
--   holding a "Data.Map".
--
-- * "Ouroboros.Consensus.Storage.LedgerDB.BackingStore.Impl.LMDB": an external
--   disk-based database.
module Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore (
    -- * API
    --
    -- | Most of the documentation on the behaviour of the 'BackingStore' lives
    -- in this module.
    module Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API
    -- * Initialization
  , newBackingStore
  , restoreBackingStore
    -- * Tracing
  , FlavorImplSpecificTrace (..)
    -- * Testing
  , newBackingStoreInitialiser
  ) where

import           Cardano.Slotting.Slot
import           Control.Tracer
import           Data.Functor.Contravariant
import           Data.SOP.Dict (Dict (..))
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Args
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.InMemory as InMemory
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB as LMDB
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.IOLike
import           System.FS.API

type BackingStoreInitializer m l =
     SomeHasFS m
  -> SomeHasFS m
  -> InitFrom (LedgerTables l ValuesMK)
  -> m (LedgerBackingStore m l)

-- | Overwrite the 'BackingStore' tables with the snapshot's tables
restoreBackingStore ::
     ( IOLike m
     , HasLedgerTables l
     , CanSerializeLedgerTables l
     , HasCallStack
     )
  => Tracer m FlavorImplSpecificTrace
  -> Complete BackingStoreArgs m
  -> SomeHasFS m
  -> SomeHasFS m
  -> FsPath
  -> m (LedgerBackingStore m l)
restoreBackingStore trcr bss someHasFs snapFs loadPath =
    newBackingStoreInitialiser trcr bss someHasFs snapFs (InitFromCopy loadPath)

-- | Create a 'BackingStore' from the given initial tables.
newBackingStore ::
     ( IOLike m
     , HasLedgerTables l
     , CanSerializeLedgerTables l
     , HasCallStack
     )
  => Tracer m FlavorImplSpecificTrace
  -> Complete BackingStoreArgs m
  -> SomeHasFS m
  -> SomeHasFS m
  -> LedgerTables l ValuesMK
  -> m (LedgerBackingStore m l)
newBackingStore trcr bss someHasFS snapFs tables =
    newBackingStoreInitialiser trcr bss someHasFS snapFs (InitFromValues Origin tables)

newBackingStoreInitialiser ::
     forall m l.
     ( IOLike m
     , HasLedgerTables l
     , CanSerializeLedgerTables l
     , HasCallStack
     )
  => Tracer m FlavorImplSpecificTrace
  -> Complete BackingStoreArgs m
  -> BackingStoreInitializer m l
newBackingStoreInitialiser trcr bss =
  case bss of
    LMDBBackingStoreArgs limits Dict ->
      LMDB.newLMDBBackingStore
        (FlavorImplSpecificTraceOnDisk . OnDiskBackingStoreTrace >$< trcr)
        limits
    InMemoryBackingStoreArgs ->
      InMemory.newInMemoryBackingStore
        (FlavorImplSpecificTraceInMemory . InMemoryBackingStoreTrace >$< trcr)

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

data FlavorImplSpecificTrace =
    FlavorImplSpecificTraceInMemory FlavorImplSpecificTraceInMemory
  | FlavorImplSpecificTraceOnDisk FlavorImplSpecificTraceOnDisk
  deriving (Eq, Show)

data FlavorImplSpecificTraceInMemory  =
    InMemoryBackingStoreInitialise
  | InMemoryBackingStoreTrace BackingStoreTrace
  deriving (Eq, Show)

data FlavorImplSpecificTraceOnDisk =
    OnDiskBackingStoreInitialise LMDB.LMDBLimits
  | OnDiskBackingStoreTrace BackingStoreTrace
  deriving (Eq, Show)