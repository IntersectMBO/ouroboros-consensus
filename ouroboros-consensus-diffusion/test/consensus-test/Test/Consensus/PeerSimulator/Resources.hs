{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

-- | Data types and resource allocating constructors for the concurrency
-- primitives used by ChainSync and BlockFetch in the handlers that implement
-- the block tree analysis specific to our peer simulator.
module Test.Consensus.PeerSimulator.Resources (
    BlockFetchResources (..)
  , ChainSyncResources (..)
  , PeerResources (..)
  , PeerSimulatorResources (..)
  , SharedResources (..)
  , makeChainSyncResources
  , makePeerResources
  , makePeerSimulatorResources
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict (atomically, dupTChan,
                     newBroadcastTChan, readTChan, writeTChan)
import           Control.Tracer (Tracer)
import           Data.Foldable (toList)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Traversable (for)
import           Ouroboros.Consensus.Block (WithOrigin (Origin))
import           Ouroboros.Consensus.Block.Abstract (Header, Point (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (ChainSyncClientHandle)
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike, MonadSTM (STM),
                     StrictTVar, readTVar, uncheckedNewTVarM, writeTVar)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Tip (..))
import           Ouroboros.Network.Protocol.BlockFetch.Server (BlockFetchServer)
import           Ouroboros.Network.Protocol.ChainSync.Server
                     (ChainSyncServer (..))
import           Test.Consensus.BlockTree (BlockTree)
import           Test.Consensus.PeerSimulator.Handlers
import           Test.Consensus.PeerSimulator.ScheduledBlockFetchServer
                     (BlockFetchServerHandlers (..),
                     runScheduledBlockFetchServer)
import           Test.Consensus.PeerSimulator.ScheduledChainSyncServer
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Peers (PeerId)
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock)

-- | Resources used by both ChainSync and BlockFetch for a single peer.
data SharedResources m =
  SharedResources {
    -- | The name of the peer.
    srPeerId       :: PeerId,

    -- | The block tree in which the test is taking place. In combination to
    -- 'csssCurrentIntersection' and the current point schedule tick, it allows
    -- to define which blocks to serve to the client.
    srBlockTree    :: BlockTree TestBlock,

    -- | The currently active schedule point.
    --
    -- This is 'Maybe' because we cannot wait for the initial state otherwise.
    srCurrentState :: StrictTVar m (Maybe AdvertisedPoints),

    srTracer       :: Tracer m String
  }

-- | The data used by the point scheduler to interact with the mocked protocol handler in
-- "Test.Consensus.PeerSimulator.ScheduledChainSyncServer".
data ChainSyncResources m =
  ChainSyncResources {
    -- | The current known intersection with the chain of the client.
    csrCurrentIntersection :: StrictTVar m (Point TestBlock),

    -- | The final server passed to typed-protocols.
    csrServer :: ChainSyncServer (Header TestBlock) (Point TestBlock) (Tip TestBlock) m (),

    -- | This action blocks while this peer is inactive in the point schedule.
    csrTickStarted :: STM m ()
  }

-- | The data used by the point scheduler to interact with the mocked protocol handler in
-- "Test.Consensus.PeerSimulator.BlockFetch".
data BlockFetchResources m =
  BlockFetchResources {
    -- | The final server passed to typed-protocols.
    bfrServer      :: BlockFetchServer TestBlock (Point TestBlock) m (),

    -- | This action blocks while this peer is inactive in the point schedule.
    bfrTickStarted :: STM m ()
  }

-- | The totality of resources used by a single peer in ChainSync and BlockFetch and by
-- the scheduler to interact with it.
data PeerResources m =
  PeerResources {
    -- | Resources used by ChainSync and BlockFetch.
    prShared      :: SharedResources m,

    -- | Resources used by ChainSync only.
    prChainSync   :: ChainSyncResources m,

    -- | Resources used by BlockFetch only.
    prBlockFetch  :: BlockFetchResources m,

    -- | An action used by the scheduler to update the peer's advertised points and
    -- resume processing for the ChainSync and BlockFetch servers.
    prUpdateState :: NodeState -> STM m ()
  }

-- | Resources for the peer simulator.
data PeerSimulatorResources m =
  PeerSimulatorResources {
    -- | Resources for individual peers.
    psrPeers      :: Map PeerId (PeerResources m),

    -- | The shared candidate fragments used by ChainDB, ChainSync and BlockFetch.
    psrCandidates :: StrictTVar m (Map PeerId (StrictTVar m TestFragH)),

    -- | The kill action and tip accessor that the Genesis governor uses to interact with ChainSync.
    psrHandles :: StrictTVar m (Map PeerId (ChainSyncClientHandle m TestBlock))
  }

-- | Create 'ChainSyncServerHandlers' for our default implementation using 'AdvertisedPoints'.
makeChainSyncServerHandlers ::
  IOLike m =>
  StrictTVar m (Point TestBlock) ->
  BlockTree TestBlock ->
  ChainSyncServerHandlers m AdvertisedPoints
makeChainSyncServerHandlers currentIntersection blockTree =
  ChainSyncServerHandlers {
    csshFindIntersection = handlerFindIntersection currentIntersection blockTree,
    csshRequestNext = handlerRequestNext currentIntersection blockTree
  }

-- | Create all the resources used exclusively by the ChainSync handlers, and
-- the ChainSync protocol server that uses the handlers to interface with the
-- typed-protocols engine.
--
-- TODO move server construction to Run?
makeChainSyncResources ::
  IOLike m =>
  STM m () ->
  SharedResources m ->
  m (ChainSyncResources m)
makeChainSyncResources csrTickStarted SharedResources {srPeerId, srTracer, srBlockTree, srCurrentState} = do
  csrCurrentIntersection <- uncheckedNewTVarM $ AF.Point Origin
  let
    handlers = makeChainSyncServerHandlers csrCurrentIntersection srBlockTree
    csrServer = runScheduledChainSyncServer (condense srPeerId) csrTickStarted (readTVar srCurrentState) srTracer handlers
  pure ChainSyncResources {csrTickStarted, csrServer, csrCurrentIntersection}

makeBlockFetchResources ::
  IOLike m =>
  STM m () ->
  SharedResources m ->
  BlockFetchResources m
makeBlockFetchResources bfrTickStarted SharedResources {srPeerId, srTracer, srBlockTree, srCurrentState} =
  BlockFetchResources {
    bfrTickStarted,
    bfrServer
  }
  where
    handlers = BlockFetchServerHandlers {
      bfshBlockFetch = handlerBlockFetch srBlockTree,
      bfshSendBlocks = handlerSendBlocks
    }
    bfrServer = runScheduledBlockFetchServer (condense srPeerId) bfrTickStarted (readTVar srCurrentState) srTracer handlers


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
  StrictTVar m (Maybe AdvertisedPoints) ->
  m (NodeState -> STM m (), STM m (), STM m ())
updateState srCurrentState =
  atomically $ do
    publisher <- newBroadcastTChan
    consumer1 <- dupTChan publisher
    consumer2 <- dupTChan publisher
    let
      newState s = do
        writeTVar srCurrentState =<< case s of
          NodeOffline     -> pure Nothing
          NodeOnline tick -> do
            -- REVIEW: Is it ok to only unblock the peer when it is online?
            -- So far we've handled Nothing in the ChainSync server by skipping the tick.
            writeTChan publisher ()
            pure (Just tick)
    pure (newState, readTChan consumer1, readTChan consumer2)

-- | Create all concurrency resources and the ChainSync protocol server used
-- for a single peer.
--
-- A peer performs BlockFetch and ChainSync using a state of
-- type 'AdvertisedPoints' that is updated by a separate scheduler, waking up
-- the protocol handlers to process messages until the conditions of the new
-- state are satisfied.
--
-- TODO pass BFR and CSR to runScheduled... rather than passing the individual resources in and storing the result
makePeerResources ::
  IOLike m =>
  Tracer m String ->
  BlockTree TestBlock ->
  PeerId ->
  m (PeerResources m)
makePeerResources srTracer srBlockTree srPeerId = do
  srCurrentState <- uncheckedNewTVarM Nothing
  (prUpdateState, csrTickStarted, bfrTickStarted) <- updateState srCurrentState
  let prShared = SharedResources {srTracer, srBlockTree, srPeerId, srCurrentState}
      prBlockFetch = makeBlockFetchResources bfrTickStarted prShared
  prChainSync <- makeChainSyncResources csrTickStarted prShared
  pure PeerResources {prShared, prChainSync, prBlockFetch, prUpdateState}

-- | Create resources for all given peers operating on the given block tree.
makePeerSimulatorResources ::
  IOLike m =>
  Tracer m String ->
  BlockTree TestBlock ->
  NonEmpty PeerId ->
  m (PeerSimulatorResources m)
makePeerSimulatorResources tracer blockTree peers = do
  resources <- for peers $ \ peerId -> do
    peerResources <- makePeerResources tracer blockTree peerId
    pure (peerId, peerResources)
  psrCandidates <- uncheckedNewTVarM mempty
  psrHandles <- uncheckedNewTVarM mempty
  pure PeerSimulatorResources {psrCandidates, psrPeers = Map.fromList $ toList resources, psrHandles}
