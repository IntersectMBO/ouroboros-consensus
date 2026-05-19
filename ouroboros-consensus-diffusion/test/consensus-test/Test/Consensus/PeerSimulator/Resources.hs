{-# LANGUAGE NamedFieldPuns #-}

-- | Data types and resource allocating constructors for the concurrency
-- primitives used by ChainSync and BlockFetch in the handlers that implement
-- the block tree analysis specific to our peer simulator.
module Test.Consensus.PeerSimulator.Resources
  ( BlockFetchResources (..)
  , ChainSyncResources (..)
  , PeerResources (..)
  , PeerSimulatorResources (..)
  , SharedResources (..)
  , makeChainSyncResources
  , makePeerResources
  , makePeerSimulatorResources
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
  ( atomically
  , dupTChan
  , newBroadcastTChan
  , readTChan
  , writeTChan
  )
import Control.Tracer (Tracer)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import Ouroboros.Consensus.Block.Abstract
  ( GetHeader
  , Header
  , Point (..)
  , WithOrigin (Origin)
  )
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( ChainSyncClientHandleCollection
  , newChainSyncClientHandleCollection
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , MonadSTM (STM)
  , StrictTVar
  , readTVar
  , uncheckedNewTVarM
  , writeTVar
  )
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (Tip (..))
import Ouroboros.Network.Protocol.BlockFetch.Server (BlockFetchServer)
import Ouroboros.Network.Protocol.ChainSync.Server
  ( ChainSyncServer (..)
  )
import Test.Consensus.BlockTree (BlockTree)
import Test.Consensus.PeerSimulator.Handlers
import Test.Consensus.PeerSimulator.ScheduledBlockFetchServer
  ( BlockFetchServerHandlers (..)
  , runScheduledBlockFetchServer
  )
import Test.Consensus.PeerSimulator.ScheduledChainSyncServer
import Test.Consensus.PeerSimulator.Trace (TraceEvent)
import Test.Consensus.PointSchedule.NodeState
import Test.Consensus.PointSchedule.Peers (PeerId)
import Test.Util.Orphans.IOLike ()

-- | Resources used by both ChainSync and BlockFetch for a single peer.
data SharedResources m blk
  = SharedResources
  { srPeerId :: PeerId
  -- ^ The name of the peer.
  , srBlockTree :: BlockTree blk
  -- ^ The block tree in which the test is taking place. In combination to
  -- 'csssCurrentIntersection' and the current point schedule tick, it allows
  -- to define which blocks to serve to the client.
  , srCurrentState :: StrictTVar m (Maybe (NodeState blk))
  -- ^ The currently active schedule point.
  --
  -- This is 'Maybe' because we cannot wait for the initial state otherwise.
  , srTracer :: Tracer m (TraceEvent blk)
  }

-- | The data used by the point scheduler to interact with the mocked protocol handler in
-- "Test.Consensus.PeerSimulator.ScheduledChainSyncServer".
data ChainSyncResources m blk
  = ChainSyncResources
  { csrCurrentIntersection :: StrictTVar m (Point blk)
  -- ^ The current known intersection with the chain of the client.
  , csrServer :: ChainSyncServer (Header blk) (Point blk) (Tip blk) m ()
  -- ^ The final server passed to typed-protocols.
  , csrTickStarted :: STM m ()
  -- ^ This action blocks while this peer is inactive in the point schedule.
  }

-- | The data used by the point scheduler to interact with the mocked protocol handler in
-- "Test.Consensus.PeerSimulator.BlockFetch".
data BlockFetchResources m blk
  = BlockFetchResources
  { bfrServer :: BlockFetchServer blk (Point blk) m ()
  -- ^ The final server passed to typed-protocols.
  , bfrTickStarted :: STM m ()
  -- ^ This action blocks while this peer is inactive in the point schedule.
  }

-- | The totality of resources used by a single peer in ChainSync and BlockFetch and by
-- the scheduler to interact with it.
data PeerResources m blk
  = PeerResources
  { prShared :: SharedResources m blk
  -- ^ Resources used by ChainSync and BlockFetch.
  , prChainSync :: ChainSyncResources m blk
  -- ^ Resources used by ChainSync only.
  , prBlockFetch :: BlockFetchResources m blk
  -- ^ Resources used by BlockFetch only.
  , prUpdateState :: NodeState blk -> STM m ()
  -- ^ An action used by the scheduler to update the peer's advertised points and
  -- resume processing for the ChainSync and BlockFetch servers.
  }

-- | Resources for the peer simulator.
data PeerSimulatorResources m blk
  = PeerSimulatorResources
  { psrPeers :: Map PeerId (PeerResources m blk)
  -- ^ Resources for individual peers.
  , psrHandles :: ChainSyncClientHandleCollection PeerId m blk
  -- ^ Handles to interact with the ChainSync client of each peer.
  -- See 'ChainSyncClientHandle' for more details.
  }

-- | Create 'ChainSyncServerHandlers' for our default implementation using 'NodeState'.
makeChainSyncServerHandlers ::
  ( IOLike m
  , GetHeader blk
  , AF.HasHeader blk
  , Eq blk
  ) =>
  StrictTVar m (Point blk) ->
  BlockTree blk ->
  ChainSyncServerHandlers m (NodeState blk) blk
makeChainSyncServerHandlers currentIntersection blockTree =
  ChainSyncServerHandlers
    { csshFindIntersection = handlerFindIntersection currentIntersection blockTree
    , csshRequestNext = handlerRequestNext currentIntersection blockTree
    }

-- | Create all the resources used exclusively by the ChainSync handlers, and
-- the ChainSync protocol server that uses the handlers to interface with the
-- typed-protocols engine.
--
-- TODO move server construction to Run?
makeChainSyncResources ::
  ( IOLike m
  , GetHeader blk
  , AF.HasHeader blk
  , Eq blk
  ) =>
  STM m () ->
  SharedResources m blk ->
  m (ChainSyncResources m blk)
makeChainSyncResources csrTickStarted SharedResources{srPeerId, srTracer, srBlockTree, srCurrentState} = do
  csrCurrentIntersection <- uncheckedNewTVarM $ AF.Point Origin
  let
    handlers = makeChainSyncServerHandlers csrCurrentIntersection srBlockTree
    csrServer = runScheduledChainSyncServer srPeerId csrTickStarted (readTVar srCurrentState) srTracer handlers
  pure ChainSyncResources{csrTickStarted, csrServer, csrCurrentIntersection}

makeBlockFetchResources ::
  ( IOLike m
  , AF.HasHeader blk
  , Eq blk
  ) =>
  STM m () ->
  SharedResources m blk ->
  BlockFetchResources m blk
makeBlockFetchResources bfrTickStarted SharedResources{srPeerId, srTracer, srBlockTree, srCurrentState} =
  BlockFetchResources
    { bfrTickStarted
    , bfrServer
    }
 where
  handlers =
    BlockFetchServerHandlers
      { bfshBlockFetch = handlerBlockFetch srBlockTree
      , bfshSendBlocks = handlerSendBlocks srBlockTree
      }
  bfrServer =
    runScheduledBlockFetchServer
      srPeerId
      bfrTickStarted
      (readTVar srCurrentState)
      srTracer
      handlers

-- | Create the concurrency transactions for communicating the begin of a peer's
-- tick and its new state to the ChainSync and BlockFetch servers.
--
-- We use a 'TChan' with two consumers and return only an action that takes a
-- 'NodeState', which should be called by the scheduler in each of this peer's
-- ticks.
--
-- The action writes the new state (converted to 'Maybe') to the shared TVar,
-- and publishes an item to the channel _only if_ the state is 'NodeOnline'.
--
-- If the peer's servers block on the channel whenever they have exhausted the
-- possible actions for a tick, the scheduler will be resumed.
-- When the scheduler then calls the update action in this peer's next tick,
-- both consumers will be unblocked and able to fetch the new state from the
-- TVar.
updateState ::
  IOLike m =>
  StrictTVar m (Maybe (NodeState blk)) ->
  m (NodeState blk -> STM m (), STM m (), STM m ())
updateState srCurrentState =
  atomically $ do
    publisher <- newBroadcastTChan
    consumer1 <- dupTChan publisher
    consumer2 <- dupTChan publisher
    let
      newState points = do
        writeTVar srCurrentState =<< do
          -- REVIEW: Is it ok to only unblock the peer when it is online?
          -- So far we've handled Nothing in the ChainSync server by skipping the tick.
          writeTChan publisher ()
          pure (Just points)
    pure (newState, readTChan consumer1, readTChan consumer2)

-- | Create all concurrency resources and the ChainSync protocol server used
-- for a single peer.
--
-- A peer performs BlockFetch and ChainSync using a state of
-- type 'NodeState' that is updated by a separate scheduler, waking up
-- the protocol handlers to process messages until the conditions of the new
-- state are satisfied.
--
-- TODO pass BFR and CSR to runScheduled... rather than passing the individual resources in and storing the result
makePeerResources ::
  ( IOLike m
  , AF.HasHeader blk
  , GetHeader blk
  , Eq blk
  ) =>
  Tracer m (TraceEvent blk) ->
  BlockTree blk ->
  PeerId ->
  m (PeerResources m blk)
makePeerResources srTracer srBlockTree srPeerId = do
  srCurrentState <- uncheckedNewTVarM Nothing
  (prUpdateState, csrTickStarted, bfrTickStarted) <- updateState srCurrentState
  let prShared = SharedResources{srTracer, srBlockTree, srPeerId, srCurrentState}
      prBlockFetch = makeBlockFetchResources bfrTickStarted prShared
  prChainSync <- makeChainSyncResources csrTickStarted prShared
  pure PeerResources{prShared, prChainSync, prBlockFetch, prUpdateState}

-- | Create resources for all given peers operating on the given block tree.
makePeerSimulatorResources ::
  ( IOLike m
  , LedgerSupportsProtocol blk
  , Eq blk
  ) =>
  Tracer m (TraceEvent blk) ->
  BlockTree blk ->
  NonEmpty PeerId ->
  m (PeerSimulatorResources m blk)
makePeerSimulatorResources tracer blockTree peers = do
  resources <- for peers $ \peerId -> do
    peerResources <- makePeerResources tracer blockTree peerId
    pure (peerId, peerResources)
  psrHandles <- atomically newChainSyncClientHandleCollection
  pure PeerSimulatorResources{psrPeers = Map.fromList $ toList resources, psrHandles}
