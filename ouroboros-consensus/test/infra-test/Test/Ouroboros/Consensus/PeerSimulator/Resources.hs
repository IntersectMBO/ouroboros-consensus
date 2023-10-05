{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Ouroboros.Consensus.PeerSimulator.Resources (
  ChainSyncResources (..),
  ChainSyncServerState (..),
  makeChainSyncServerResources,
  makeChainSyncServerState,
) where

import Control.Tracer (Tracer (Tracer))
import Data.Functor ((<&>))
import Ouroboros.Consensus.Block (WithOrigin (Origin))
import Ouroboros.Consensus.Block.Abstract (Header, Point (..))
import Ouroboros.Consensus.Util.Condense (Condense (..))
import Ouroboros.Consensus.Util.IOLike (
  IOLike,
  MonadSTM (TQueue, readTQueue),
  StrictTVar,
  TQueue,
  newTQueueIO,
  readTQueue,
  uncheckedNewTVarM,
  )
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (Tip (..))
import Ouroboros.Network.Protocol.ChainSync.Server (ChainSyncServer (..))
import Test.Util.Orphans.IOLike ()
import Test.Util.TestBlock (TestBlock)

import Test.Ouroboros.Consensus.ChainGenerator.Tests.BlockTree (BlockTree)
import Test.Ouroboros.Consensus.ChainGenerator.Tests.PointSchedule
import Test.Ouroboros.Consensus.PeerSimulator.Handlers
import Test.Ouroboros.Consensus.PeerSimulator.ScheduledChainSyncServer
import Test.Ouroboros.Consensus.PeerSimulator.Trace (traceUnitWith)

-- | The data used by the handler implementation in "Test.Ouroboros.Consensus.PeerSimulator.Handlers".
data ChainSyncServerState m =
  ChainSyncServerState {
    -- | The current known intersection with the chain of the client.
    csssCurrentIntersection :: StrictTVar m (Point TestBlock),

    -- | The block tree in which the test is taking place. In combination to
    -- 'csssCurrentIntersection' and the current point schedule tick, it allows
    -- to define which blocks to serve to the client.
    csssTree :: BlockTree TestBlock
  }

-- | The data used by the point scheduler to interact with the mocked protocol handler in
-- "Test.Ouroboros.Consensus.PeerSimulator.ScheduledChainSyncServer".
data ChainSyncResources m =
  ChainSyncResources {
    -- | A queue of node states coming from the scheduler.
    csrQueue :: TQueue m NodeState,

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
  Tracer m String ->
  ChainSyncServerState m ->
  ChainSyncServerHandlers m AdvertisedPoints
makeChainSyncServerHandlers tracer ChainSyncServerState {..} =
  ChainSyncServerHandlers {
    csshFindIntersection = handlerFindIntersection csssCurrentIntersection csssTree,
    csshRequestNext = handlerRequestNext csssCurrentIntersection csssTree tracer
  }

makeChainSyncServerResources ::
  IOLike m =>
  Tracer m String ->
  PeerId ->
  ChainSyncServerState m ->
  m (ChainSyncResources m)
makeChainSyncServerResources tracer peerId state = do
  csrQueue <- newTQueueIO
  csrCandidateFragment <- uncheckedNewTVarM $ AF.Empty AF.AnchorGenesis
  let
    wait = readTQueue csrQueue <&> \case
      NodeOffline -> Nothing
      NodeOnline tick -> Just tick
  csrServer <- runScheduledChainSyncServer wait serverTracer handlers
  pure ChainSyncResources {..}
  where
    handlers = makeChainSyncServerHandlers handlersTracer state
    handlersTracer = Tracer $ traceUnitWith tracer ("ChainSyncServerHandlers " ++ condense peerId)
    serverTracer = Tracer $ traceUnitWith tracer ("ScheduledChainSyncServer " ++ condense peerId)
