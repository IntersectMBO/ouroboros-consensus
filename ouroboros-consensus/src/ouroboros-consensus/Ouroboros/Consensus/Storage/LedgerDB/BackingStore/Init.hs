{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

-- | Initializing a backing store
module Ouroboros.Consensus.Storage.LedgerDB.BackingStore.Init (
    BackingStoreInitializer
  , BackingStoreSelector (..)
  , newBackingStore
  , newBackingStoreInitialiser
  , restoreBackingStore
  ) where

import           Cardano.Slotting.Slot
import           Control.Monad.IO.Class
import           Control.Tracer
import           Data.Functor.Contravariant
import qualified Data.Map.Diff.Strict as Diff
import qualified Data.Map.Strict as Map
import           Data.Monoid (Sum (..))
import qualified Data.Set as Set
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore.InMemory as InMemory
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore.LMDB as LMDB
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import           Ouroboros.Consensus.Storage.LedgerDB.Types
import           Ouroboros.Consensus.Util.IOLike
import           System.FS.API

type BackingStoreInitializer m l =
     SomeHasFS m
  -> InitFrom (LedgerTables l ValuesMK)
  -> m (BackingStore m
         (LedgerTables l KeysMK)
         (LedgerTables l ValuesMK)
         (LedgerTables l DiffMK))

-- | Overwrite the ChainDB tables with the snapshot's tables
restoreBackingStore ::
     BackingStoreInitializer m l
  -> SomeHasFS m
  -> DiskSnapshot
  -> m (LedgerBackingStore m l)
restoreBackingStore bsi someHasFs snapshot =
    bsi someHasFs (InitFromCopy (BackingStorePath loadPath))
  where
    loadPath = snapshotToTablesPath snapshot

-- | Create a backing store from the given genesis ledger state
newBackingStore ::
     BackingStoreInitializer m l
  -> SomeHasFS m
  -> LedgerTables l ValuesMK
  -> m (LedgerBackingStore m l)
newBackingStore bsi someHasFS tables =
    bsi someHasFS (InitFromValues Origin tables)

newBackingStoreInitialiser ::
     ( IOLike m
     , NoThunks (LedgerTables l ValuesMK)
     , HasLedgerTables l
     , CanSerializeLedgerTables l
     , HasCallStack
     )
  => Tracer m BackingStoreTrace
  -> BackingStoreSelector m
  -> BackingStoreInitializer m l
newBackingStoreInitialiser trcr bss =
  case bss of
    LMDBBackingStore limits ->
      LMDB.newLMDBBackingStoreInitialiser
        (LMDBTrace >$< trcr)
        limits
    InMemoryBackingStore -> InMemory.newTVarBackingStoreInitialiser
      (InMemoryTrace >$< trcr)
      (ltliftA2 lookup_)
      (\rq values -> case rqPrev rq of
          Nothing   ->
            ltmap (rangeRead0_ (rqCount rq))      values
          Just keys ->
            ltliftA2 (rangeRead_  (rqCount rq)) keys values
      )
      (ltliftA2 applyDiff_)
      (getSum . ltcollapse . ltmap (K2 . count_))
      valuesMKEncoder
      valuesMKDecoder
  where
    lookup_ ::
         Ord k
      => KeysMK   k v
      -> ValuesMK k v
      -> ValuesMK k v
    lookup_ (KeysMK ks) (ValuesMK vs) =
      ValuesMK (Map.restrictKeys vs ks)

    rangeRead0_ ::
         Int
      -> ValuesMK k v
      -> ValuesMK k v
    rangeRead0_ n (ValuesMK vs) =
      ValuesMK $ Map.take n vs

    rangeRead_ ::
         Ord k
      => Int
      -> KeysMK   k v
      -> ValuesMK k v
      -> ValuesMK k v
    rangeRead_ n (KeysMK ks) (ValuesMK vs) =
        case Set.lookupMax ks of
          Nothing -> ValuesMK Map.empty
          Just  k -> ValuesMK  $ Map.take n $ snd $ Map.split k vs

    applyDiff_ ::
         Ord k
      => ValuesMK k v
      -> DiffMK   k v
      -> ValuesMK k v
    applyDiff_ (ValuesMK values) (DiffMK diff) =
      ValuesMK (Diff.applyDiff values diff)

    count_ :: ValuesMK k v -> Sum Int
    count_ (ValuesMK values) = Sum $ Map.size values

-- | The backing store selector
data BackingStoreSelector m where
  LMDBBackingStore     :: MonadIO m => !LMDB.LMDBLimits -> BackingStoreSelector m
  InMemoryBackingStore ::                                  BackingStoreSelector m
