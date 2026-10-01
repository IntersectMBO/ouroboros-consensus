{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.State
  ( ObjectDiffusionInboundState (..)
  , NextOutstandingRoundNumber (..)
  , ObjectDiffusionInboundHandle (..)
  , ObjectDiffusionInboundHandleCollection (..)
  , newObjectDiffusionInboundHandleCollection
  , ObjectDiffusionInboundStateView (..)
  , bracketObjectDiffusionInbound
  )
where

import Control.Monad (when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block (BlockSupportsProtocol, HasHeader, Header, PerasRoundNo)
import Ouroboros.Consensus.MiniProtocol.Util.Idling (Idling (Idling, idlingStart, idlingStop))
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
  , writeTVar
  )

-- | Certificate progress relative to one peer, for external components to
-- inspect. This does not describe the node's global synchronization state.
data NextOutstandingRoundNumber
  = Uninitialized
  | NextOutstandingRoundNumber !PerasRoundNo
  | CaughtUp
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

-- | An ObjectDiffusion inbound client state that's used by other components.
--
-- This state is registered for certificate diffusion. The generic client uses
-- 'ObjectDiffusionInboundStateView', with a no-op view for vote diffusion.
--
-- NOTE: 'blk' is not needed for now, but we keep it for future use.
data ObjectDiffusionInboundState blk = ObjectDiffusionInboundState
  { nextOutstandingRoundNumber :: !NextOutstandingRoundNumber
  -- ^ The round at the head of the outstanding certificate FIFO. It remains
  -- outstanding while being downloaded, validated, or processed by ChainDB,
  -- including while the client waits for its validation context to advance.
  --
  -- If the FIFO empties before the server confirms its front, retain the last
  -- reported round as a conservative lower bound. 'CaughtUp' is established
  -- only by @MsgAwaitReply@ after all outstanding processing has completed,
  -- and persists across @MsgServerIdle@ until new IDs arrive. 'Uninitialized'
  -- means that no round or server-front information is available yet.
  --
  -- 'CaughtUp' is also the certificate client's idling indication for the GSM.
  -- This is distinct from the protocol state @StIdle@: that state can follow
  -- either @MsgServerIdle@ (still caught up) or @MsgReplyObjectIds@ (not caught up).
  }
  deriving stock (Eq, Show, Generic)

deriving anyclass instance
  ( HasHeader blk
  , NoThunks (Header blk)
  ) =>
  NoThunks (ObjectDiffusionInboundState blk)

initObjectDiffusionInboundState :: ObjectDiffusionInboundState blk
initObjectDiffusionInboundState =
  ObjectDiffusionInboundState
    { nextOutstandingRoundNumber = Uninitialized
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
data ObjectDiffusionInboundStateView objectId m = ObjectDiffusionInboundStateView
  { odisvIdling :: !(Idling m)
  -- ^ Actions that record whether the client has reached the server's current
  -- object-ID front. See 'nextOutstandingRoundNumber'.
  , odisvSetNextOutstandingObjectId :: !(objectId -> m ())
  -- ^ Record the first outstanding ID, before downloading or processing it.
  -- Certificate diffusion maps this to 'nextOutstandingRoundNumber'. This
  -- only publishes state for readers; it does not notify a governor or trigger
  -- chain selection.
  }
  deriving stock Generic

bracketObjectDiffusionInbound ::
  forall m peer blk a.
  (IOLike m, HasHeader blk, NoThunks (Header blk)) =>
  ObjectDiffusionInboundHandleCollection peer m blk ->
  peer ->
  (ObjectDiffusionInboundStateView PerasRoundNo m -> m a) ->
  m a
bracketObjectDiffusionInbound handles peer body = do
  odiState <- newTVarIO initObjectDiffusionInboundState
  bracket_ (acquireContext odiState) releaseContext
    . body
    $ ObjectDiffusionInboundStateView
      { odisvIdling =
          Idling
            { idlingStart = updateState odiState $ \s ->
                s{nextOutstandingRoundNumber = CaughtUp}
            , idlingStop = updateState odiState $ \s ->
                s
                  { nextOutstandingRoundNumber = case nextOutstandingRoundNumber s of
                      CaughtUp -> Uninitialized
                      progress -> progress
                  }
            }
      , odisvSetNextOutstandingObjectId = \roundNo ->
          updateState odiState $ \s ->
            s{nextOutstandingRoundNumber = NextOutstandingRoundNumber roundNo}
      }
 where
  updateState var f = atomically $ do
    old <- readTVar var
    let new = f old
    when (new /= old) $ writeTVar var new

  acquireContext odiState =
    atomically
      . odihcAddHandle handles peer
      $ ObjectDiffusionInboundHandle
        { odihState = odiState
        }

  releaseContext = atomically $ odihcRemoveHandle handles peer
