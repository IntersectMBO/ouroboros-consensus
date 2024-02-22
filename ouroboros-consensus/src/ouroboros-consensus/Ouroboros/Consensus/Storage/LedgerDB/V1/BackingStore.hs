{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

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
  , BackingStoreSelector (..)
  , newBackingStore
  , restoreBackingStore
    -- * Tracing
  , BackingStoreTraceByBackend (..)
  , TraceBackingStoreInitEvent (..)
    -- * Testing
  , newBackingStoreInitialiser
  ) where

import           Cardano.Slotting.Slot
import           Control.Monad.IO.Class
import           Control.Tracer
import           Data.Functor.Contravariant
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.API
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.InMemory as InMemory
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB as LMDB
import           Ouroboros.Consensus.Util.IOLike
import           System.FS.API
import           System.FS.API.Types

type BackingStoreInitializer m l =
     SomeHasFS m
  -> InitFrom (LedgerTables l ValuesMK)
  -> m (LedgerBackingStore m l)

-- | Overwrite the 'BackingStore' tables with the snapshot's tables
restoreBackingStore ::
     ( IOLike m
     , HasLedgerTables l
     , CanSerializeLedgerTables l
     , HasCallStack
     )
  => Tracer m BackingStoreTraceByBackend
  -> BackingStoreSelector m
  -> SomeHasFS m
  -> FsPath
  -> m (LedgerBackingStore m l)
restoreBackingStore trcr bss someHasFs loadPath =
    newBackingStoreInitialiser trcr bss someHasFs (InitFromCopy loadPath)

-- | Create a 'BackingStore' from the given initial tables.
newBackingStore ::
     ( IOLike m
     , HasLedgerTables l
     , CanSerializeLedgerTables l
     , HasCallStack
     )
  => Tracer m BackingStoreTraceByBackend
  -> BackingStoreSelector m
  -> SomeHasFS m
  -> LedgerTables l ValuesMK
  -> m (LedgerBackingStore m l)
newBackingStore trcr bss someHasFS tables =
    newBackingStoreInitialiser trcr bss someHasFS (InitFromValues Origin tables)

newBackingStoreInitialiser ::
     ( IOLike m
     , HasLedgerTables l
     , CanSerializeLedgerTables l
     , HasCallStack
     )
  => Tracer m BackingStoreTraceByBackend
  -> BackingStoreSelector m
  -> BackingStoreInitializer m l
newBackingStoreInitialiser trcr bss =
  case bss of
    LMDBBackingStore limits ->
      LMDB.newLMDBBackingStore
        (LMDBTrace >$< trcr)
        limits
    InMemoryBackingStore ->
      InMemory.newInMemoryBackingStore
        (InMemoryTrace >$< trcr)

-- | The selector to choose which backend we would like to use.
data BackingStoreSelector m where
  LMDBBackingStore     :: MonadIO m => !LMDB.LMDBLimits -> BackingStoreSelector m
  InMemoryBackingStore ::                                  BackingStoreSelector m

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

-- | A union type to hold traces of the particular 'BackingStore' implementation
-- in use.
data BackingStoreTraceByBackend = LMDBTrace BackingStoreTrace
                                | InMemoryTrace BackingStoreTrace
                                | InitEvent TraceBackingStoreInitEvent
  deriving (Show)

-- | A trace event for the backing store that we have initialised.
data TraceBackingStoreInitEvent =
    BackingStoreInitialisedLMDB LMDB.LMDBLimits
  | BackingStoreInitialisedInMemory
  deriving (Show, Eq)
