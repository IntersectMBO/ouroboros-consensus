{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.TraceEvent
  ( FlavorImplSpecificTrace (..)
  , TraceEvent (..)
  ) where

import GHC.Generics
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Forker
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore as V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Backend as V2

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

data FlavorImplSpecificTrace blk
  = FlavorImplSpecificTraceV1 V1.SomeBackendTrace
  | FlavorImplSpecificTraceV2 (V2.LedgerDBV2Trace (ExtLedgerState blk))

deriving instance StandardHash blk => Show (FlavorImplSpecificTrace blk)

data TraceEvent blk
  = LedgerDBSnapshotEvent !(TraceSnapshotEvent blk)
  | LedgerReplayEvent !(TraceReplayEvent blk)
  | LedgerDBForkerEvent !TraceForkerEventWithKey
  | LedgerDBFlavorImplEvent !(FlavorImplSpecificTrace blk)
  | LedgerDBGCingLdbToClose
  | LedgerDBGCedLdbToClose
  | LedgerDBPruning
  | LedgerDBPruned
  deriving Generic

deriving instance
  ( StandardHash blk
  , InspectLedger blk
  ) =>
  Show (TraceEvent blk)
