{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.State
  ( ObjectDiffusionInboundState (..)
  , ObjectDiffusionInboundStatus (..)
  , ObjectDiffusionInboundHandle (..)
  , ObjectDiffusionInboundHandleCollection (..)
  , newObjectDiffusionInboundHandleCollection
  , ObjectDiffusionInboundStateView (..)
  , bracketObjectDiffusionInbound
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block (BlockSupportsProtocol, HasHeader, Header)
import Ouroboros.Consensus.MiniProtocol.Util.Idling (Idling (idlingStart, idlingStop))
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , MonadSTM (STM, atomically)
  , MonadThrow (bracket_)
  , NoThunks
  , StrictTVar
  , modifyTVar
  , newTVar
  , newTVarIO
  , readTVar
  )
import qualified Ouroboros.Consensus.MiniProtocol.Util.Idling as Util.Idling

-- | The curent status of the ObjectDiffusion mini protocol, exposed through
-- @ObjectDiffusionInboundState@.
--
-- Active implies that the protocol is currently connected to a peer and
-- receiving objects.
--
-- Idling indicates that the client has reached the server's current object-ID
-- front. We use "idling" consistently with ChainSync: it starts when the server
-- sends @MsgAwaitReply@ and ends when the server supplies new object IDs. In
-- this sense, idling means that the client is caught up with this particular
-- server, and contributes to the GSM caught-up decision. This is distinct
-- from the Object Diffusion protocol state @StIdle@. After @MsgAwaitReply@ the
-- protocol is in @StObjectIds (StObjectIdsBlocking StMustReply)@, where the
-- server has agency. Moreover, after @MsgServerIdle@ returns the protocol to
-- @StIdle@, this flag deliberately remains 'True' until the server supplies new
-- object IDs.
--
-- Finally, Blocked indicates that the client is paused because the first
-- unrequested object ID in the peer's advertised FIFO is not currently
-- requestable according to 'opwIsRequestable'. Unlike 'odIdling', this does not
-- establish that the client has reached the server's current object-ID front.
data ObjectDiffusionInboundStatus = Active | Idling | Blocked
  deriving stock Generic
  deriving Eq

deriving anyclass instance
  NoThunks ObjectDiffusionInboundStatus


data ObjectDiffusionInboundState blk = ObjectDiffusionInboundState { unState :: ObjectDiffusionInboundStatus }
  deriving stock Generic

deriving anyclass instance
  ( HasHeader blk
  , NoThunks (Header blk)
  ) =>
  NoThunks (ObjectDiffusionInboundState blk)

initObjectDiffusionInboundState :: ObjectDiffusionInboundState blk
initObjectDiffusionInboundState = ObjectDiffusionInboundState Active

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
  -- ^ Actions that record whether the client has reached the server's current
  -- object-ID front. See 'odIdling'.
  , odisvSetRequestBlocked :: !(Bool -> m ())
  }
  deriving stock Generic

bracketObjectDiffusionInbound ::
  forall m peer blk a.
  (IOLike m, HasHeader blk, NoThunks (Header blk)) =>
  ObjectDiffusionInboundHandleCollection peer m blk ->
  peer ->
  (ObjectDiffusionInboundStateView m -> m a) ->
  m a
bracketObjectDiffusionInbound handles peer body = do
  odiState <- newTVarIO initObjectDiffusionInboundState
  bracket_ (acquireContext odiState) releaseContext
    . body
    $ ObjectDiffusionInboundStateView
      { odisvIdling =
          Util.Idling.Idling
            { idlingStart = atomically $ modifyTVar odiState $ \s -> s{unState = Idling}
            , idlingStop = atomically $ modifyTVar odiState $ \s -> s{unState = Active}
            }
      , odisvSetRequestBlocked =
          \blocked -> atomically $ modifyTVar odiState $ \s -> s {unState = case blocked of
          True -> Blocked
          False -> Active}
      }
 where
  acquireContext odiState =
    atomically
      . odihcAddHandle handles peer
      $ ObjectDiffusionInboundHandle
        { odihState = odiState
        }

  releaseContext = atomically $ odihcRemoveHandle handles peer
