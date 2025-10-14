{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Common interface for LedgerDB V2 backends
module Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
  ( -- * Backend API
    Backend (..)

    -- * Existentials
  , SomeBackendTrace (..)
  , SomeBackendArgs (..)
  , SomeResources (..)

    -- * Tracing
  , LedgerDBV2Trace (..)
  ) where

import Control.Monad.Except
import Control.ResourceRegistry
import Control.Tracer
import Data.Proxy
import Data.Typeable
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import System.FS.API

-- | Operations needed to open and operate a LedgerDB V2
class NoThunks (Resources m backend) => Backend m backend blk where
  -- | The Arguments that will be used initially to create the 'Resources'.
  data Args m backend

  -- | The Resources that will be stored in the LedgerDB environment and given
  -- to the handle operations.
  data Resources m backend

  -- | A trace dependent on the particular backend.
  data Trace backend

  -- | Transform 'Args' into 'Resources', with some context made up of
  -- 'LedgerDbArgs'.
  mkResources ::
    Proxy blk ->
    Tracer m LedgerDBV2Trace ->
    Args m backend ->
    ResourceRegistry m ->
    SomeHasFS m ->
    m (Resources m backend)

  -- | Release the acquired resources.
  releaseResources :: Proxy blk -> Resources m backend -> m ()

  -- | Create a new handle from the given values. This will only be called when
  -- starting Consensus from Genesis.
  newHandleFromValues ::
    Tracer m LedgerDBV2Trace ->
    ResourceRegistry m ->
    Resources m backend ->
    ExtLedgerState blk ValuesMK ->
    m (LedgerTablesHandle m (ExtLedgerState blk))

  -- | Create a new handle from a snapshot.
  newHandleFromSnapshot ::
    Tracer m LedgerDBV2Trace ->
    ResourceRegistry m ->
    CodecConfig blk ->
    SomeHasFS m ->
    Resources m backend ->
    DiskSnapshot ->
    ExceptT (SnapshotFailure blk) m (LedgerSeq' m blk, RealPoint blk)

  -- | Instantiate the 'SnapshotManager' for this backend.
  snapshotManager ::
    Proxy blk ->
    Resources m backend ->
    CodecConfig blk ->
    Tracer m (TraceSnapshotEvent blk) ->
    SomeHasFS m ->
    SnapshotManager m m blk (StateRef m (ExtLedgerState blk))

{-------------------------------------------------------------------------------
  Existentials
-------------------------------------------------------------------------------}

data SomeBackendTrace where
  SomeBackendTrace ::
    (Show (Trace backend), Typeable backend) => Trace backend -> SomeBackendTrace

instance Show SomeBackendTrace where
  show (SomeBackendTrace tr) = show tr

data SomeBackendArgs m blk where
  SomeBackendArgs :: Backend m backend blk => Args m backend -> SomeBackendArgs m blk

data SomeResources m blk where
  SomeResources :: Backend m backend blk => Resources m backend -> SomeResources m blk

instance NoThunks (SomeResources m blk) where
  wNoThunks ctxt (SomeResources res) = wNoThunks ctxt res
  noThunks ctxt (SomeResources res) = noThunks ctxt res
  showTypeOf _ = "SomeResources"

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

data LedgerDBV2Trace
  = -- | Created a new 'LedgerTablesHandle', potentially by duplicating an
    -- existing one.
    TraceLedgerTablesHandleCreate
  | -- | Closed a 'LedgerTablesHandle'.
    TraceLedgerTablesHandleClose
  | BackendTrace SomeBackendTrace

deriving instance Show SomeBackendTrace => Show LedgerDBV2Trace
