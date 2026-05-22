{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | Common interface for LedgerDB V2 backends.
--
-- A backend is no longer a typeclass. It is a value of type
-- 'LedgerDbBackendArgs' (essentially a closure over the user-supplied
-- arguments for a specific backend) whose 'acquireBackend' action produces
-- a 'BackendResources' record. The 'BackendResources' holds all the
-- per-backend operations the LedgerDB driver needs at runtime:
-- construct a genesis 'ExtStateHandle', load one from a snapshot, the
-- 'SnapshotManager' and a release action.
--
-- Adding a new backend (e.g. an LSM-based one in a separate package) means
-- exporting a constructor function that returns a 'LedgerDbBackendArgs'.
-- Nothing in @ouroboros-consensus@ needs to know that backend exists.
module Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
  ( -- * Backend API
    LedgerDbBackendArgs (..)
  , BackendResources (..)

    -- * Tracing
  , LedgerDBV2Trace (..)
  , SomeBackendTrace (..)
  ) where

import Control.Monad.Except
import Control.ResourceRegistry
import Control.Tracer
import Data.Kind (Type)
import Data.Typeable
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Util.Enclose (EnclosingTimed)
import System.FS.API hiding (Handle)

-- | A backend for LedgerDB V2: how to acquire and release the resources
-- needed to materialise per-block-type 'LedgerTablesHandle's, plus the
-- corresponding snapshot manager.
--
-- A backend constructor (e.g. @inMemoryBackendArgs@ in
-- @Ouroboros.Consensus.Cardano.InMemory@) closes over its own arguments and
-- returns this record. The LedgerDB driver only ever sees this opaque
-- interface.
type LedgerDbBackendArgs :: (Type -> Type) -> Type -> Type
newtype LedgerDbBackendArgs m blk = LedgerDbBackendArgs
  { acquireBackend ::
      forall fState.
      Tracer m LedgerDBV2Trace ->
      SomeHasFS m ->
      WithTempRegistry fState m (BackendResources m blk)
  }

-- | The resources produced by 'acquireBackend': everything the LedgerDB
-- driver needs to construct and tear down 'ExtStateHandle's for @blk@.
--
-- The arguments stored in 'LedgerDbBackendArgs' are turned into raw
-- resources (a filesystem, an LSM session, ...) inside 'acquireBackend',
-- and those resources are then closed over by the fields of this record.
type BackendResources :: (Type -> Type) -> Type -> Type
data BackendResources m blk = BackendResources
  { brLoadSnapshot ::
      CodecConfig blk ->
      SomeHasFS m ->
      DiskSnapshot ->
      ExceptT (SnapshotFailure blk) m (ExtStateHandle m blk, RealPoint blk)
  -- ^ Load an 'ExtStateHandle' from a snapshot on disk.
  , brSnapshotManager ::
      CodecConfig blk ->
      Tracer m (TraceSnapshotEvent blk) ->
      SomeHasFS m ->
      SnapshotManager m blk (ExtStateHandle m blk)
  -- ^ Build the per-backend snapshot manager. The backend closes over
  -- its own session/handle types here.
  , brRelease :: m ()
  -- ^ Release any resources allocated by 'acquireBackend' that are not
  -- managed by the temporary registry (e.g. handles that have to outlive
  -- the registry handoff).
  , ledgerTablesFactory :: LedgerTablesFactory m blk
  }
  deriving Generic
  deriving NoThunks via OnlyCheckWhnfNamed "BackendResources" (BackendResources m blk)

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

-- | An opaque per-backend trace event. Backends that want to emit
-- structured events through the LedgerDB tracer can wrap them in this
-- existential — it preserves the @Show@ representation without forcing
-- the consensus library to know the backend trace types.
data SomeBackendTrace where
  SomeBackendTrace ::
    (Show t, Typeable t) => t -> SomeBackendTrace

instance Show SomeBackendTrace where
  show (SomeBackendTrace tr) = show tr

data LedgerDBV2Trace
  = -- | Created a new 'LedgerTablesHandle', potentially by duplicating an
    -- existing one.
    TraceLedgerTablesHandleCreate EnclosingTimed
  | -- | Closed a 'LedgerTablesHandle'.
    TraceLedgerTablesHandleClose EnclosingTimed
  | TraceLedgerTablesHandleRead EnclosingTimed
  | TraceLedgerTablesHandleDuplicate EnclosingTimed
  | TraceLedgerTablesHandleCreateFirst EnclosingTimed
  | TraceLedgerTablesHandlePush EnclosingTimed
  | BackendTrace SomeBackendTrace

deriving instance Show LedgerDBV2Trace
