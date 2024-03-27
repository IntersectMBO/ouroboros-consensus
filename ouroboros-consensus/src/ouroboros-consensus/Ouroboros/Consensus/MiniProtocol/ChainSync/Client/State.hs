{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State (
    ChainSyncClientHandle (..)
  , ChainSyncState (..)
  , Instruction (..)
  , JumpResult (..)
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
    csCandidate       :: !(AnchoredFragment (Header blk))

    -- | This ChainSync client should ensure that its peer sets this flag while
    -- and only while both of the following conditions are satisfied: the
    -- peer's latest message has been fully processed (especially that its
    -- candidate has been updated; previous argument) and its latest message
    -- did not claim that it already has headers that extend its candidate.
    --
    -- It's more important that the flag is unset promptly than it is for the
    -- flag to be set promptly, because of how this is used by the GSM to
    -- determine that the node is done syncing.
  , csIdling          :: !Bool

  , csNextInstruction :: !(Maybe (Instruction blk))

  , csJumpResult      :: !(Maybe (JumpResult blk))

    -- | When the client receives a new header, it updates this field before
    -- processing it further, and the latest slot may refer to a header beyond
    -- the forecast horizon while the candidate fragment isn't extended yet, to
    -- signal to GDD that the density is known up to this slot.
  , csLatestSlot      :: !(Maybe (WithOrigin SlotNo))
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
  }
  deriving stock (Generic)

deriving anyclass instance (
  IOLike m,
  HasHeader blk,
  NoThunks (Header blk)
  ) => NoThunks (ChainSyncClientHandle m blk)

-- | Instruction from the jumping governor, either to run normal ChainSync, or
-- to jump to follow the given peer with the given fragment.
data Instruction blk
  = RunNormally
  | JumpTo !(Point blk)
  deriving (Generic)

deriving instance (HasHeader blk, Eq (Header blk)) => Eq (Instruction blk)
deriving instance (HasHeader blk, Show (Header blk)) => Show (Instruction blk)
deriving anyclass instance (HasHeader blk, NoThunks (Header blk)) => NoThunks (Instruction blk)

data JumpResult blk
  = AcceptedJump !(Point blk)
  | RejectedJump !(Point blk)
  deriving (Generic)

deriving instance (HasHeader blk, Eq (Header blk)) => Eq (JumpResult blk)
deriving instance (HasHeader blk, Show (Header blk)) => Show (JumpResult blk)
deriving anyclass instance (HasHeader blk, NoThunks (Header blk)) => NoThunks (JumpResult blk)
