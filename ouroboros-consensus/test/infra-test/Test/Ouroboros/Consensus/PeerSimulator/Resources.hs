{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Ouroboros.Consensus.PeerSimulator.Resources (
    ChainSyncResources (..)
  , ChainSyncServerState (..)
  , makeChainSyncResources
  , makeChainSyncServerState
  ) where

import           Control.Concurrent.Class.MonadSTM
                     (MonadSTM (TMVar, newTMVarIO))
import           Control.Concurrent.Class.MonadSTM.Strict (newEmptyTMVarIO,
                     readTMVar, takeTMVar)
import           Control.Tracer (Tracer (Tracer))
import           Ouroboros.Consensus.Block (WithOrigin (Origin))
import           Ouroboros.Consensus.Block.Abstract (Header, Point (..))
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike,
                     MonadSTM (TQueue, readTQueue), StrictTMVar, StrictTVar,
                     TQueue, newTQueueIO, readTQueue, readTVar,
                     uncheckedNewTVarM, writeTVar)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Tip (..))
import           Ouroboros.Network.Protocol.ChainSync.Server
                     (ChainSyncServer (..))
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.BlockTree
                     (BlockTree)
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.PointSchedule
import           Test.Ouroboros.Consensus.PeerSimulator.Handlers
import           Test.Ouroboros.Consensus.PeerSimulator.ScheduledChainSyncServer
import           Test.Ouroboros.Consensus.PeerSimulator.Trace (traceUnitWith)
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock)

-- | The data used by the handler implementation in "Test.Ouroboros.Consensus.PeerSimulator.Handlers".
data ChainSyncServerState m =
  ChainSyncServerState {
    -- | The current known intersection with the chain of the client.
    csssCurrentIntersection :: StrictTVar m (Point TestBlock),

    -- | The block tree in which the test is taking place. In combination to
    -- 'csssCurrentIntersection' and the current point schedule tick, it allows
    -- to define which blocks to serve to the client.
    csssTree                :: BlockTree TestBlock
  }

-- | The data used by the point scheduler to interact with the mocked protocol handler in
-- "Test.Ouroboros.Consensus.PeerSimulator.ScheduledChainSyncServer".
data ChainSyncResources m =
  ChainSyncResources {
    -- | A mailbox of node states coming from the scheduler.
    csrNextState :: StrictTMVar m NodeState,

    -- | The current schedule point that is updated by the scheduler in the peer's active tick,
    -- waking up the chain sync server.
    csrCurrentState :: StrictTVar m (Maybe AdvertisedPoints),

    -- | REVIEW: Not sure why we need this.
    csrCandidateFragment :: StrictTVar m TestFragH,

    -- | The final server passed to typed-protocols.
    csrServer :: ChainSyncServer (Header TestBlock) (Point TestBlock) (Tip TestBlock) m ()
  }

makeChainSyncServerState ::
  IOLike m =>
  BlockTree TestBlock ->
  m (ChainSyncServerState m)
makeChainSyncServerState csssTree = do
  csssCurrentIntersection <- uncheckedNewTVarM $ AF.Point Origin
  pure ChainSyncServerState {..}

makeChainSyncServerHandlers ::
  IOLike m =>
  ChainSyncServerState m ->
  ChainSyncServerHandlers m AdvertisedPoints
makeChainSyncServerHandlers ChainSyncServerState {..} =
  ChainSyncServerHandlers {
    csshFindIntersection = handlerFindIntersection csssCurrentIntersection csssTree,
    csshRequestNext = handlerRequestNext csssCurrentIntersection csssTree
  }

makeChainSyncResources ::
  IOLike m =>
  Tracer m String ->
  PeerId ->
  ChainSyncServerState m ->
  m (ChainSyncResources m)
makeChainSyncResources tracer peerId state = do
  csrNextState <- newEmptyTMVarIO
  csrCandidateFragment <- uncheckedNewTVarM $ AF.Empty AF.AnchorGenesis
  csrCurrentState <- uncheckedNewTVarM Nothing
  let
    wait =
      takeTMVar csrNextState >>= \ newState -> do
        let
          a = case newState of
            NodeOffline     -> Nothing
            NodeOnline tick -> Just tick
        writeTVar csrCurrentState a
        pure a
    csrServer = runScheduledChainSyncServer (condense peerId) wait (readTVar csrCurrentState) tracer handlers
  pure ChainSyncResources {..}
  where
    handlers = makeChainSyncServerHandlers state
