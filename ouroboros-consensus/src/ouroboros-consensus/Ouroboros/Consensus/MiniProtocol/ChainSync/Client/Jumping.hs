{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.MiniProtocol.ChainSync.Client.Jumping (runGovernor) where

import           Cardano.Prelude (forever)
import           Cardano.Slotting.Slot (SlotNo, WithOrigin (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block (HasHeader, Header, Point (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State
                     (ChainSyncClientHandle (..))
import           Ouroboros.Consensus.Util.IOLike

data ClientState blk
  = Dynamo
  | Objector
      -- | The last known point where the objector agrees with the dynamo.
      !(Point blk)
  | Jumper
      -- | The result of the last jump.
      !(Point blk)
      -- | More precisely, the state of the jumper.
      !(JumperState blk)
  deriving (Generic)

deriving anyclass instance (HasHeader blk, NoThunks (Header blk)) => NoThunks (ClientState blk)

data JumperState blk
  = -- | The jumper is happy with the dynamo, at least as far as the point in
    -- the 'Jumper' constructor is concerned.
    Happy
  | -- | The jumper disagrees with the dynamo and we are searching where exactly
    -- that happens. All we know is a point where the jumper agrees with the
    -- dynamo (in the 'Jumper' constructor) and a point where the jumper
    -- disagrees with the dynamo, carried by this constructor.
    LookingForIntersection !(Point blk)
  | -- | The jumper disagrees with the dynamo and we have found where exactly.
    -- The last point where the jumper agrees with the dynamo is stored in the
    -- ClientState.
    FoundIntersection
  deriving (Generic)

deriving anyclass instance (HasHeader blk, NoThunks (Header blk)) => NoThunks (JumperState blk)

data GovernorState m peer blk = GovernorState
  { clientStates :: !(Map peer (ClientState blk, ChainSyncClientHandle m blk)),
    lastJumpSlot :: !(WithOrigin SlotNo),
    jumpSize     :: !SlotNo
  }
  deriving (Generic)

deriving anyclass instance (IOLike m, NoThunks peer, HasHeader blk, NoThunks (Header blk)) => NoThunks (GovernorState m peer blk)

-- | Align the client state with the handles. Peers might have died and new
-- peers might have been added. After this operation, @clientStates@ and the
-- handles have the same keys. New peers are added as happy jumpers from
-- Genesis.
alignStateWithHandles ::
  ( Ord peer
  ) =>
  Map peer (ChainSyncClientHandle m blk) ->
  GovernorState m peer blk ->
  GovernorState m peer blk
alignStateWithHandles handles state =
  let currentPeers = Map.keysSet handles
      newPeers = currentPeers `Set.difference` Map.keysSet (clientStates state)
   in state
        { clientStates =
            Map.union
              (Map.restrictKeys (clientStates state) $ currentPeers)
              (Map.fromSet ((Jumper GenesisPoint Happy,) . (handles Map.!)) newPeers)
        }

runGovernor ::
  ( IOLike m,
    Ord peer,
    HasHeader blk,
    NoThunks peer,
    NoThunks (Header blk)
  ) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  m ()
runGovernor varHandles = do
  varState <-
    newTVarIO
      GovernorState
        { clientStates = mempty,
          lastJumpSlot = Origin,
          jumpSize = 2 -- FIXME: get from configuration
        }
  forever $ atomically $ do
    clientHandles <- readTVar varHandles
    modifyTVar varState $ alignStateWithHandles clientHandles
