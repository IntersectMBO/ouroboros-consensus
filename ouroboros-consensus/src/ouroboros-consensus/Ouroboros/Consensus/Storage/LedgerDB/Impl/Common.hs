{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Some minor stuff that is (currently) common to all implementations

module Ouroboros.Consensus.Storage.LedgerDB.Impl.Common (
    -- * Serialise
    LedgerDbSerialiseConstraints
    -- * Tracing
  , FlavorImplSpecificTrace (..)
  , ReplayGoal (..)
  , ReplayStart (..)
  , TraceLedgerDBEvent (..)
  , TraceReplayEvent (..)
  , TraceReplayProgressEvent (..)
  , TraceReplayStartEvent (..)
  , decorateReplayTracerWithGoal
  , decorateReplayTracerWithStart
  ) where

import           Codec.Serialise (Serialise)
import           Control.Tracer
import           Data.Functor.Contravariant ((>$<))
import           GHC.Generics
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.LedgerDB.API
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore as V1
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Args as V2
import           Ouroboros.Consensus.Storage.Serialisation

-- | Serialization constraints required by the 'LedgerDB' to be properly
-- instantiated with a @blk@.
type LedgerDbSerialiseConstraints blk =
  ( Serialise      (HeaderHash  blk)
  , EncodeDisk blk (LedgerState blk EmptyMK)
  , DecodeDisk blk (LedgerState blk EmptyMK)
  , EncodeDisk blk (AnnTip      blk)
  , DecodeDisk blk (AnnTip      blk)
  , EncodeDisk blk (ChainDepState (BlockProtocol blk))
  , DecodeDisk blk (ChainDepState (BlockProtocol blk))
  , CanSerializeLedgerTables (LedgerState blk)
  )

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

data FlavorImplSpecificTrace =
    FlavorImplSpecificTraceV1 V1.FlavorImplSpecificTrace
  | FlavorImplSpecificTraceV2 V2.FlavorImplSpecificTrace
  deriving (Show, Eq)

data TraceLedgerDBEvent blk =
      LedgerDBSnapshotEvent   !(TraceSnapshotEvent blk)
    | LedgerReplayEvent       !(TraceReplayEvent blk)
    | LedgerDBForkerEvent     !TraceForkerEventWithKey
    | LedgerDBFlavorImplEvent !FlavorImplSpecificTrace
  deriving (Generic)

deriving instance
  (StandardHash blk, InspectLedger blk)
  => Show (TraceLedgerDBEvent blk)
deriving instance
  (StandardHash blk, InspectLedger blk)
  => Eq (TraceLedgerDBEvent blk)

{-------------------------------------------------------------------------------
  Trace replay events
-------------------------------------------------------------------------------}

data TraceReplayEvent blk =
      TraceReplayStartEvent (TraceReplayStartEvent blk)
    | TraceReplayProgressEvent (TraceReplayProgressEvent blk)
    deriving (Show, Eq)

-- | Add the tip of the Immutable DB to the trace event
--
-- Between the tip of the immutable DB and the point of the starting block,
-- the node could (if it so desired) easily compute a "percentage complete".
decorateReplayTracerWithGoal
  :: Point blk -- ^ Tip of the ImmutableDB
  -> Tracer m (TraceReplayProgressEvent blk)
  -> Tracer m (ReplayGoal blk -> TraceReplayProgressEvent blk)
decorateReplayTracerWithGoal immTip = (($ ReplayGoal immTip) >$<)

-- | Add the block at which a replay started.
--
-- This allows to compute a "percentage complete" when tracing the events.
decorateReplayTracerWithStart
  :: Point blk -- ^ Starting point of the replay
  -> Tracer m (ReplayGoal blk -> TraceReplayProgressEvent blk)
  -> Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayProgressEvent blk)
decorateReplayTracerWithStart start = (($ ReplayStart start) >$<)

-- | Which point the replay started from
newtype ReplayStart blk = ReplayStart (Point blk) deriving (Eq, Show)

-- | Which point the replay is expected to end at
newtype ReplayGoal blk = ReplayGoal (Point blk) deriving (Eq, Show)

-- | Events traced while replaying blocks against the ledger to bring it up to
-- date w.r.t. the tip of the ImmutableDB during initialisation. As this
-- process takes a while, we trace events to inform higher layers of our
-- progress.
data TraceReplayStartEvent blk
  = -- | There were no LedgerDB snapshots on disk, so we're replaying all blocks
    -- starting from Genesis against the initial ledger.
    ReplayFromGenesis
    -- | There was a LedgerDB snapshot on disk corresponding to the given tip.
    -- We're replaying more recent blocks against it.
  | ReplayFromSnapshot
        DiskSnapshot
        (ReplayStart blk) -- ^ the block at which this replay started
  deriving (Generic, Eq, Show)

-- | We replayed the given block (reference) on the genesis snapshot during
-- the initialisation of the LedgerDB. Used during ImmutableDB replay.
data TraceReplayProgressEvent blk =
  ReplayedBlock
    (RealPoint blk)   -- ^ the block being replayed
    [LedgerEvent blk]
    (ReplayStart blk) -- ^ the block at which this replay started
    (ReplayGoal blk)  -- ^ the block at the tip of the ImmutableDB
  deriving (Generic, Eq, Show)
