{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.State
  ( ObjectDiffusionInboundState (..)
  , ObjectDiffusionInboundHandle (..)
  , ObjectDiffusionInboundHandleCollection (..)
  , newObjectDiffusionInboundHandleCollection
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block (BlockSupportsProtocol, HasHeader, Header)
import Ouroboros.Consensus.Util.IOLike
  ( IOLike (..)
  , MonadSTM (..)
  , StrictTVar
  , modifyTVar
  , newTVar
  , readTVar
  )

-- | An ObjectDiffusion inbound client state that's used by other components.
data ObjectDiffusionInboundState blk = ObjectDiffusionInboundState
  { odisIdling :: !Bool
  -- ^ Whether we have received all objects from a peer
  }
  deriving stock Generic

deriving anyclass instance
  ( HasHeader blk
  , NoThunks (Header blk)
  ) =>
  NoThunks (ObjectDiffusionInboundState blk)

-- | An interface to an ObjectDiffusion inbound client that's used by other components.
data ObjectDiffusionInboundHandle m blk = ObjectDiffusionInboundHandle
  { odihState :: !(StrictTVar m (ObjectDiffusionInboundState blk))
  -- ^ Data shared between the client and external components.
  }
  deriving stock Generic

deriving anyclass instance
  ( IOLike m
  , HasHeader blk
  , NoThunks (Header blk)
  ) =>
  NoThunks (ObjectDiffusionInboundHandle m blk)

-- | A collection of ObjectDiffusion inbound client handles for the peers of this node.
data ObjectDiffusionInboundHandleCollection peer m blk = ObjectDiffusionInboundHandleCollection
  { odihcMap :: !(STM m (Map peer (ObjectDiffusionInboundHandle m blk)))
  -- ^ A map containing the handles for the peers in the collection
  , odihcAddHandle :: !(peer -> ObjectDiffusionInboundHandle m blk -> STM m ())
  -- ^ Add the handle for the given peer to the collection
  , odihcRemoveHandle :: !(peer -> STM m ())
  -- ^ Remove the handle for the given peer from the collection
  , odihcRemoveAllHandles :: !(STM m ())
  -- ^ Remove all the handles from the collection
  }
  deriving stock Generic

newObjectDiffusionInboundHandleCollection ::
  (Ord peer, IOLike m, NoThunks peer, BlockSupportsProtocol blk) =>
  STM m (ObjectDiffusionInboundHandleCollection peer m blk)
newObjectDiffusionInboundHandleCollection = do
  handlesMap <- newTVar mempty
  return
    ObjectDiffusionInboundHandleCollection
      { odihcMap = readTVar handlesMap
      , odihcAddHandle = \peer handle ->
          modifyTVar handlesMap (Map.insert peer handle)
      , odihcRemoveHandle = \peer ->
          modifyTVar handlesMap (Map.delete peer)
      , odihcRemoveAllHandles =
          modifyTVar handlesMap (const mempty)
      }
