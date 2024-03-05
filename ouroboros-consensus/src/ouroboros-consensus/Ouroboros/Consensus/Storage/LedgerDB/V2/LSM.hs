{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-orphans #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.LSM (
    loadSnapshot
  , newLSMLedgerTablesHandle
  , takeSnapshot
  ) where

import           Control.Tracer
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Common
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Snapshots
import           Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import           Ouroboros.Consensus.Util.IOLike
import           System.FS.API

newLSMLedgerTablesHandle ::
     ( IOLike m
     , HasLedgerTables l
     , CanSerializeLedgerTables l
     )
  => LedgerTables l ValuesMK
  -> m (LedgerTablesHandle m l)
newLSMLedgerTablesHandle = undefined

loadSnapshot ::
     ( LedgerDbSerialiseConstraints blk
     , LedgerSupportsProtocol blk
     , IOLike m
     )
  => CodecConfig blk
  -> SomeHasFS m
  -> DiskSnapshot
  -> m (Either (SnapshotFailure blk) (LedgerSeq' m blk, RealPoint blk))
loadSnapshot = undefined

takeSnapshot ::
     CodecConfig blk
  -> Tracer m (TraceSnapshotEvent blk)
  -> SomeHasFS m
  -> StateRef m (ExtLedgerState blk)
  -> m (Maybe (DiskSnapshot, RealPoint blk))
takeSnapshot = undefined
