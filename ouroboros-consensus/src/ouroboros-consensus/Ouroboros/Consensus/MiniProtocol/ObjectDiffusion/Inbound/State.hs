{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.State
  ( ObjectDiffusionInboundState (..)
  , initObjectDiffusionInboundState
  , ObjectDiffusionInboundHandle (..)
  , ObjectDiffusionInboundHandleCollection (..)
  , ObjectDiffusionInboundStateView (..)
  , newObjectDiffusionInboundHandleCollection
  , bracketObjectDiffusionInbound
  )
where

import Control.Monad.Class.MonadThrow (bracket)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block (BlockSupportsProtocol, HasHeader, Header)
import Ouroboros.Consensus.MiniProtocol.Util.Idling (Idling (..))
import Ouroboros.Consensus.Node.NetworkProtocolVersion (NodeToNodeVersion)
import Ouroboros.Consensus.Util.IOLike
  ( IOLike (..)
  , MonadSTM (..)
  , StrictTVar
  , modifyTVar
  , newTVar
  , newTVarIO
  , readTVar
  )

-- | An ObjectDiffusion inbound client state that's used by other components.
--
-- NOTE: 'blk' is not needed for now, but we keep it for future use.
data ObjectDiffusionInboundState blk = ObjectDiffusionInboundState
  { odisIdling :: !Bool
  -- ^ Whether the client is currently idling
  , odisNodeToNodeVersion :: !NodeToNodeVersion
  -- ^ Negotiated version of the protocol with the peer.
  --
  -- This is used to determine later on whether other mini-protocols are
  -- expected to run in parallel with this one.
  }
  deriving stock Generic

deriving anyclass instance
  ( HasHeader blk
  , NoThunks (Header blk)
  ) =>
  NoThunks (ObjectDiffusionInboundState blk)

initObjectDiffusionInboundState :: NodeToNodeVersion -> ObjectDiffusionInboundState blk
initObjectDiffusionInboundState version =
  ObjectDiffusionInboundState
    { odisIdling = True
    , odisNodeToNodeVersion = version
    }

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
      }

-- | Interface for the ObjectDiffusion client to its state allocated by
-- 'bracketObjectDiffusionInbound'.
data ObjectDiffusionInboundStateView m = ObjectDiffusionInboundStateView
  { odisvIdling :: !(Idling m)
  }
  deriving stock Generic

bracketObjectDiffusionInbound ::
  forall m peer blk a.
  (IOLike m, HasHeader blk, NoThunks (Header blk)) =>
  NodeToNodeVersion ->
  ObjectDiffusionInboundHandleCollection peer m blk ->
  peer ->
  (ObjectDiffusionInboundStateView m -> m a) ->
  m a
bracketObjectDiffusionInbound version handles peer body = do
  odiState <- newTVarIO (initObjectDiffusionInboundState version)
  bracket (acquireContext odiState) releaseContext body
 where
  acquireContext odiState = atomically $ do
    odihcAddHandle handles peer $
      ObjectDiffusionInboundHandle
        { odihState = odiState
        }
    return
      ObjectDiffusionInboundStateView
        { odisvIdling =
            Idling
              { idlingStart = atomically $ modifyTVar odiState $ \s -> s{odisIdling = True}
              , idlingStop = atomically $ modifyTVar odiState $ \s -> s{odisIdling = False}
              }
        }

  releaseContext _ = atomically $ do
    odihcRemoveHandle handles peer
