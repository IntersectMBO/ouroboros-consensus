{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

-- | Data types and resource allocating constructors for the concurrency
-- primitives used by ChainSync and BlockFetch in the handlers that implement
-- the block tree analysis specific to our peer simulator.
module Test.Consensus.PeerSimulator.Resources (
    ChainSyncResources (..)
  , PeerResources (..)
  , PeerSimulatorResources (..)
  , SharedResources (..)
  , makeChainSyncResources
  , makePeerResources
  , makePeerSimulatorResources
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict (newEmptyTMVarIO,
                     takeTMVar)
import           Control.Tracer (Tracer)
import           Data.Foldable (toList)
import           Data.List.NonEmpty (NonEmpty)
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
import           Test.Consensus.BlockTree (BlockTree)
import           Test.Consensus.PeerSimulator.Handlers
import           Test.Consensus.PeerSimulator.ScheduledChainSyncServer
import           Test.Consensus.PointSchedule
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

-- | Resources for the peer simulator.
data PeerSimulatorResources m =
  PeerSimulatorResources {
    -- | Resources for individual peers.
    psrPeers      :: Map PeerId (PeerResources m),

    -- | The shared candidate fragments used by ChainDB, ChainSync and BlockFetch.
    psrCandidates :: StrictTVar m (Map PeerId (StrictTVar m TestFragH))
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
makeChainSyncResources SharedResources {srPeerId, srTracer, srBlockTree, srCurrentState} = do
  csrNextState <- newEmptyTMVarIO
  csrCurrentIntersection <- uncheckedNewTVarM $ AF.Point Origin
  let
    wait = waitForNextState csrNextState srCurrentState
    handlers = makeChainSyncServerHandlers csrCurrentIntersection srBlockTree
    csrServer = runScheduledChainSyncServer (condense srPeerId) wait (readTVar srCurrentState) srTracer handlers
  pure ChainSyncResources {csrServer, csrNextState, csrCurrentIntersection}

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
  srCurrentState <- uncheckedNewTVarM Nothing
  let prShared = SharedResources {srTracer, srBlockTree, srPeerId, srCurrentState}
  prChainSync <- makeChainSyncResources prShared
  pure PeerResources {prChainSync, prShared}

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
  pure PeerSimulatorResources {psrCandidates, psrPeers = Map.fromList $ toList resources}
