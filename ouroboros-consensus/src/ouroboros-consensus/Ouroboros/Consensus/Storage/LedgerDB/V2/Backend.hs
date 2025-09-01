{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
  ( Backend (..)
  , SomeBackendTrace (..)
  , SomeBackendArgs (..)
  , SomeResources (..)
  , LedgerDBV2Trace (..)
  ) where

import Control.Monad.Except
import Control.ResourceRegistry
import Control.Tracer
import Data.Proxy
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import System.FS.API

data SomeBackendTrace where
  SomeBackendTrace :: Trace m backend -> SomeBackendTrace

data SomeBackendArgs m blk where
  SomeBackendArgs :: Backend m backend blk => Args m backend -> SomeBackendArgs m blk

data SomeResources m blk where
  SomeResources :: Backend m backend blk => Resources m backend -> SomeResources m blk

instance NoThunks (SomeResources m blk) where
  wNoThunks ctxt (SomeResources res) = wNoThunks ctxt res
  noThunks ctxt (SomeResources res) = noThunks ctxt res
  showTypeOf _ = "SomeResources"

class NoThunks (Resources m backend) => Backend m backend blk where
  data Args m backend

  data Resources m backend

  data Trace m backend

  mkResources ::
    Proxy blk ->
    Tracer m LedgerDBV2Trace ->
    Args m backend ->
    ResourceRegistry m ->
    SomeHasFS m ->
    m (Resources m backend)

  releaseResources :: Proxy blk -> Resources m backend -> m ()

  newHandleFromValues ::
    Tracer m LedgerDBV2Trace ->
    ResourceRegistry m ->
    Resources m backend ->
    ExtLedgerState blk ValuesMK ->
    m (LedgerTablesHandle m (ExtLedgerState blk))

  newHandleFromSnapshot ::
    Tracer m LedgerDBV2Trace ->
    ResourceRegistry m ->
    CodecConfig blk ->
    SomeHasFS m ->
    Resources m backend ->
    DiskSnapshot ->
    ExceptT (SnapshotFailure blk) m (LedgerSeq' m blk, RealPoint blk)

  snapshotManager ::
    Proxy blk ->
    Resources m backend ->
    CodecConfig blk ->
    Tracer m (TraceSnapshotEvent blk) ->
    SomeHasFS m ->
    SnapshotManager m m blk (StateRef m (ExtLedgerState blk))

data LedgerDBV2Trace
  = -- | Created a new 'LedgerTablesHandle', potentially by duplicating an
    -- existing one.
    TraceLedgerTablesHandleCreate
  | -- | Closed a 'LedgerTablesHandle'.
    TraceLedgerTablesHandleClose
  | BackendTrace SomeBackendTrace

deriving instance Show SomeBackendTrace => Show LedgerDBV2Trace
