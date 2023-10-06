{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-- | Data types and resource allocating constructors for the concurrency
-- primitives used by ChainSync and BlockFetch in the handlers that implement
-- the block tree analysis specific to our peer simulator.
module Test.Ouroboros.Consensus.PeerSimulator.Resources (
    ChainSyncResources (..)
  , PeerResources (..)
  , SharedResources (..)
  , makeChainSyncResources
  , makePeerResources
  , makePeersResources
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict (newEmptyTMVarIO,
                     takeTMVar)
import           Control.Tracer (Tracer)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Traversable (for)
import           Ouroboros.Consensus.Block (WithOrigin (Origin))
import           Ouroboros.Consensus.Block.Abstract (Header, Point (..))
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike, MonadSTM (STM),
                     StrictTMVar, StrictTVar, readTVar, uncheckedNewTVarM,
                     writeTVar)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Tip (..))
import           Ouroboros.Network.Protocol.ChainSync.Server
                     (ChainSyncServer (..))
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.BlockTree
                     (BlockTree)
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.PointSchedule
import           Test.Ouroboros.Consensus.PeerSimulator.Handlers
import           Test.Ouroboros.Consensus.PeerSimulator.ScheduledChainSyncServer
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock)

-- | Resources used by both ChainSync and BlockFetch for a single peer.
data SharedResources m =
  SharedResources {
    -- | The name of the peer.
    srPeerId            :: PeerId,

    -- | The block tree in which the test is taking place. In combination to
    -- 'csssCurrentIntersection' and the current point schedule tick, it allows
    -- to define which blocks to serve to the client.
    srBlockTree         :: BlockTree TestBlock,

    -- | The currently active schedule point.
    --
    -- This is 'Maybe' because we cannot wait for the initial state otherwise.
    srCurrentState      :: StrictTVar m (Maybe AdvertisedPoints),

    -- | The candidate fragment for a peer is shared by ChainSync, BlockFetch and the ChainDB.
    srCandidateFragment :: StrictTVar m TestFragH,

    srTracer            :: Tracer m String
  }

-- | The data used by the point scheduler to interact with the mocked protocol handler in
-- "Test.Ouroboros.Consensus.PeerSimulator.ScheduledChainSyncServer".
data ChainSyncResources m =
  ChainSyncResources {
    -- | A mailbox of node states that is updated by the scheduler in the peer's active tick,
    -- waking up the chain sync server.
    csrNextState :: StrictTMVar m NodeState,

    -- | The current known intersection with the chain of the client.
    csrCurrentIntersection :: StrictTVar m (Point TestBlock),

    -- | The final server passed to typed-protocols.
    csrServer :: ChainSyncServer (Header TestBlock) (Point TestBlock) (Tip TestBlock) m ()
  }

-- | The totality of resources used by a single peer in ChainSync and BlockFetch.
data PeerResources m =
  PeerResources {
    -- | Resources used by ChainSync and BlockFetch.
    prShared    :: SharedResources m,

    -- | Resources used by ChainSync only.
    prChainSync :: ChainSyncResources m
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

-- | Transaction that blocks until the next turn of the current peer, when the
-- scheduler puts the new state into the TMVar, and updates the TVar that is
-- read by all of the ChainSync and BlockFetch handlers.
--
-- The ChainSync protocol handler mock is agnostic of our state type,
-- 'NodeState', so we convert it to 'Maybe'.
waitForNextState ::
  IOLike m =>
  StrictTMVar m NodeState ->
  StrictTVar m (Maybe AdvertisedPoints) ->
  STM m (Maybe AdvertisedPoints)
waitForNextState nextState currentState =
  takeTMVar nextState >>= \ newState -> do
    let
      a = case newState of
        NodeOffline     -> Nothing
        NodeOnline tick -> Just tick
    writeTVar currentState a
    pure a

-- | Create all the resources used exclusively by the ChainSync handlers, and
-- the ChainSync protocol server that uses the handlers to interface with the
-- typed-protocols engine.
makeChainSyncResources ::
  IOLike m =>
  SharedResources m ->
  m (ChainSyncResources m)
makeChainSyncResources SharedResources {..} = do
  csrNextState <- newEmptyTMVarIO
  csrCurrentIntersection <- uncheckedNewTVarM $ AF.Point Origin
  let
    wait = waitForNextState csrNextState srCurrentState
    handlers = makeChainSyncServerHandlers csrCurrentIntersection srBlockTree
    csrServer = runScheduledChainSyncServer (condense srPeerId) wait (readTVar srCurrentState) srTracer handlers
  pure ChainSyncResources {..}

-- | Create all concurrency resources and the ChainSync protocol server used
-- for a single peer.
--
-- A peer performs BlockFetch and ChainSync using a state of
-- type 'AdvertisedPoints' that is updated by a separate scheduler, waking up
-- the protocol handlers to process messages until the conditions of the new
-- state are satisfied.
makePeerResources ::
  IOLike m =>
  Tracer m String ->
  BlockTree TestBlock ->
  PeerId ->
  m (PeerResources m)
makePeerResources srTracer srBlockTree srPeerId = do
  srCandidateFragment <- uncheckedNewTVarM $ AF.Empty AF.AnchorGenesis
  srCurrentState <- uncheckedNewTVarM Nothing
  let prShared = SharedResources {..}
  prChainSync <- makeChainSyncResources prShared
  pure PeerResources {..}

-- | Create resources for all given peers operating on the given block tree.
makePeersResources ::
  IOLike m =>
  Tracer m String ->
  BlockTree TestBlock ->
  [PeerId] ->
  m (Map PeerId (PeerResources m))
makePeersResources tracer blockTree peers = do
  resources <- for peers $ \ peerId -> do
    peerResources <- makePeerResources tracer blockTree peerId
    pure (peerId, peerResources)
  pure (Map.fromList resources)
