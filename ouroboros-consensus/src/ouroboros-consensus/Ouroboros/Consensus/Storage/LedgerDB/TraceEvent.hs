{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Storage.LedgerDB.TraceEvent
  ( TraceEvent (..)
  ) where

import GHC.Generics
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Backends
import Ouroboros.Consensus.Storage.LedgerDB.Forker
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

data TraceEvent blk
  = LedgerDBSnapshotEvent !(TraceSnapshotEvent blk)
  | LedgerReplayEvent !(TraceReplayEvent blk)
  | LedgerDBForkerEvent !TraceForkerEventWithKey
  | LedgerDBFlavorImplEvent !(SomeBackendTrace blk)
  deriving Generic

deriving instance
  (StandardHash blk, InspectLedger blk) =>
  Show (TraceEvent blk)
