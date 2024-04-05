{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State (
    ChainSyncClientHandle (..)
  , ChainSyncJumpingJumperState (..)
  , ChainSyncJumpingState (..)
  , ChainSyncState (..)
  ) where

import           Cardano.Slotting.Slot (SlotNo, WithOrigin)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block (HasHeader, Header, Point)
import           Ouroboros.Consensus.Util.IOLike (IOLike, NoThunks, StrictTVar)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)

-- | A ChainSync client's state that's used by other components, like the GDD or
-- the jumping governor.
data ChainSyncState blk = ChainSyncState {

    -- | The current candidate fragment.
    csCandidate  :: !(AnchoredFragment (Header blk))

    -- | This ChainSync client should ensure that its peer sets this flag while
    -- and only while both of the following conditions are satisfied: the
    -- peer's latest message has been fully processed (especially that its
    -- candidate has been updated; previous argument) and its latest message
    -- did not claim that it already has headers that extend its candidate.
    --
    -- It's more important that the flag is unset promptly than it is for the
    -- flag to be set promptly, because of how this is used by the GSM to
    -- determine that the node is done syncing.
  , csIdling     :: !Bool

    -- | When the client receives a new header, it updates this field before
    -- processing it further, and the latest slot may refer to a header beyond
    -- the forecast horizon while the candidate fragment isn't extended yet, to
    -- signal to GDD that the density is known up to this slot.
  , csLatestSlot :: !(Maybe (WithOrigin SlotNo))
  }
  deriving stock (Generic)

deriving anyclass instance (
  HasHeader blk,
  NoThunks (Header blk)
  ) => NoThunks (ChainSyncState blk)

-- | An interface to a ChainSync client that's used by other components, like
-- the GDD governor.
data ChainSyncClientHandle m blk = ChainSyncClientHandle {
    -- | Disconnects from the peer when the GDD considers it adversarial
    cschGDDKill :: !(m ())

    -- | Data shared between the client and external components like GDD.
  , cschState   :: !(StrictTVar m (ChainSyncState blk))

  -- FIXME: move into cschState.
  , cschJumping :: !(ChainSyncJumpingState m blk)
  }
  deriving stock (Generic)

deriving anyclass instance (
  IOLike m,
  HasHeader blk,
  NoThunks (Header blk)
  ) => NoThunks (ChainSyncClientHandle m blk)

data ChainSyncJumpingState m blk
  = Dynamo
      -- | The last slot at which we triggered jumps for the jumpers.
      !(WithOrigin SlotNo)
  | Objector
      -- | The last known point where the objector agrees with the dynamo.
      !(Point blk)
  | Jumper
      -- | The next jump to be executed.
      !(StrictTVar m (Maybe (Point blk)))
      -- | The result of the last jump.
      !(Point blk)
      -- | More precisely, the state of the jumper.
      !(ChainSyncJumpingJumperState blk)
  deriving (Generic)

deriving anyclass instance (IOLike m, HasHeader blk, NoThunks (Header blk)) => NoThunks (ChainSyncJumpingState m blk)

data ChainSyncJumpingJumperState blk
  = -- | The jumper is happy with the dynamo, at least as far as the point in
    -- the 'Jumper' constructor is concerned.
    Happy
  | -- | The jumper disagrees with the dynamo and we are searching where exactly
    -- that happens. All we know is a point where the jumper agrees with the
    -- dynamo (in the 'Jumper' constructor) and a point where the jumper
    -- disagrees with the dynamo, carried by this constructor.
    LookingForIntersection !(Point blk)
  | -- | The jumper disagrees with the dynamo and we have determined the latest
    -- point where dynamo and jumper agree. This point is stored in the
    -- 'ChainSyncJumpingState'.
    FoundIntersection
  deriving (Generic)

deriving anyclass instance (HasHeader blk, NoThunks (Header blk)) => NoThunks (ChainSyncJumpingJumperState blk)
