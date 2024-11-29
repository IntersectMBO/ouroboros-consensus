{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Storage.LedgerDB.TraceEvent (
    FlavorImplSpecificTrace (..)
  , TraceEvent (..)
  ) where

import           GHC.Generics
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Storage.LedgerDB.API
import           Ouroboros.Consensus.Storage.LedgerDB.Forker
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore as V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Args as V2

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

data FlavorImplSpecificTrace =
    FlavorImplSpecificTraceV1 V1.FlavorImplSpecificTrace
  | FlavorImplSpecificTraceV2 V2.FlavorImplSpecificTrace
  deriving (Show, Eq)

data TraceEvent blk =
      LedgerDBSnapshotEvent   !(TraceSnapshotEvent blk)
    | LedgerReplayEvent       !(TraceReplayEvent blk)
    | LedgerDBForkerEvent     !TraceForkerEventWithKey
    | LedgerDBFlavorImplEvent !FlavorImplSpecificTrace
  deriving (Generic)

deriving instance
  (StandardHash blk, InspectLedger blk)
  => Show (TraceEvent blk)
deriving instance
  (StandardHash blk, InspectLedger blk)
  => Eq (TraceEvent blk)
