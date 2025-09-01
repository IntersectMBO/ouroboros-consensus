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
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Forker
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore as V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Backend as V2

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

data FlavorImplSpecificTrace
  = FlavorImplSpecificTraceV1 V1.SomeBackendTrace
  | FlavorImplSpecificTraceV2 V2.LedgerDBV2Trace

deriving instance
  (Show V1.SomeBackendTrace, Show V2.SomeBackendTrace) => Show FlavorImplSpecificTrace

data TraceEvent blk
  = LedgerDBSnapshotEvent !(TraceSnapshotEvent blk)
  | LedgerReplayEvent !(TraceReplayEvent blk)
  | LedgerDBForkerEvent !TraceForkerEventWithKey
  | LedgerDBFlavorImplEvent !FlavorImplSpecificTrace
  deriving Generic

deriving instance
  ( StandardHash blk
  , InspectLedger blk
  , Show V1.SomeBackendTrace
  , Show V2.SomeBackendTrace
  ) =>
  Show (TraceEvent blk)
