{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Consensus.PeerSimulator.StateView
  ( PeerSimulatorComponent (..)
  , PeerSimulatorComponentResult (..)
  , PeerSimulatorResult (..)
  , StateView (..)
  , StateViewTracers (..)
  , collectDisconnectedPeers
  , defaultStateViewTracers
  , exceptionsByComponent
  , pscrToException
  , snapshotStateView
  , stateViewTracersWithInitial
  ) where

import Control.Tracer (Tracer, traceWith)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (for_)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Network.TypedProtocol.Codec (AnyMessage)
import Ouroboros.Consensus.Block (Header, Point)
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client as CSClient
import Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Util.Condense
  ( Condense (..)
  , CondenseList (..)
  , PaddingDirection (..)
  , padListWith
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , SomeException
  , atomically
  )
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import Ouroboros.Network.Block (StandardHash, Tip)
import Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import Test.Consensus.PeerSimulator.Trace (TraceEvent)
import Test.Consensus.PointSchedule.Peers (PeerId)
import Test.Util.TersePrinting
  ( terseBlock
  , terseHFragment
  , terseMaybe
  )
import Test.Util.TestBlock (TestBlock)
import Test.Util.Tracer (recordingTracerTVar)

-- | A record to associate an exception thrown by a thread
-- running a component of the peer simulator with the peer
-- that it was running for.
data PeerSimulatorResult blk = PeerSimulatorResult
  { psePeerId :: PeerId
  , pseResult :: PeerSimulatorComponentResult blk
  }
  deriving (Eq, Ord)

data PeerSimulatorComponent
  = ChainSyncClient
  | ChainSyncServer
  | BlockFetchClient
  | BlockFetchServer
  deriving (Eq, Ord)

data PeerSimulatorComponentResult blk
  = SomeChainSyncClientResult
      ( Either
          SomeException
          ( CSClient.ChainSyncClientResult
          , Maybe (ChainSyncResult blk)
          )
      )
  | SomeChainSyncServerResult
      ( Either
          SomeException
          (Maybe (ChainSyncResult blk))
      )
  | SomeBlockFetchClientResult
      ( Either
          SomeException
          (Maybe (BlockFetchResult blk))
      )
  | SomeBlockFetchServerResult
      ( Either
          SomeException
          (Maybe (BlockFetchResult blk))
      )

toComponent :: PeerSimulatorComponentResult blk -> PeerSimulatorComponent
toComponent (SomeChainSyncClientResult _) = ChainSyncClient
toComponent (SomeChainSyncServerResult _) = ChainSyncServer
toComponent (SomeBlockFetchClientResult _) = BlockFetchClient
toComponent (SomeBlockFetchServerResult _) = BlockFetchServer

pscrToException :: PeerSimulatorComponentResult blk -> Maybe SomeException
pscrToException (SomeChainSyncClientResult (Left exn)) = Just exn
pscrToException (SomeChainSyncServerResult (Left exn)) = Just exn
pscrToException (SomeBlockFetchClientResult (Left exn)) = Just exn
pscrToException (SomeBlockFetchServerResult (Left exn)) = Just exn
pscrToException _ = Nothing

instance Eq (PeerSimulatorComponentResult blk) where
  (==) a b = toComponent a == toComponent b

instance Ord (PeerSimulatorComponentResult blk) where
  compare a b = compare (toComponent a) (toComponent b)

instance (StandardHash blk, Show blk, Show (Header blk)) => Condense (PeerSimulatorComponentResult blk) where
  condense (SomeChainSyncClientResult (Left exn)) =
    "(ChainSyncClient  - Interrupted) : " ++ show exn
  condense (SomeChainSyncServerResult (Left exn)) =
    "(ChainSyncServer  - Interrupted) : " ++ show exn
  condense (SomeBlockFetchClientResult (Left exn)) =
    "(BlockFetchClient - Interrupted) : " ++ show exn
  condense (SomeBlockFetchServerResult (Left exn)) =
    "(BlockFetchServer - Interrupted) : " ++ show exn
  condense (SomeChainSyncClientResult (Right res)) =
    "(ChainSyncClient  - Success)     : " ++ show res
  condense (SomeChainSyncServerResult (Right res)) =
    "(ChainSyncServer  - Success)     : " ++ show res
  condense (SomeBlockFetchClientResult (Right res)) =
    "(BlockFetchClient - Success)     : " ++ show res
  condense (SomeBlockFetchServerResult (Right res)) =
    "(BlockFetchServer - Success)     : " ++ show res

type ChainSyncResult blk = AnyMessage (ChainSync (Header blk) (Point blk) (Tip blk))
type BlockFetchResult blk = AnyMessage (BlockFetch blk (Point blk))

instance (StandardHash blk, Show blk, Show (Header blk)) => Condense (PeerSimulatorResult blk) where
  condense PeerSimulatorResult{psePeerId, pseResult} =
    condense psePeerId ++ " " ++ condense pseResult

instance (StandardHash blk, Show blk, Show (Header blk)) => CondenseList (PeerSimulatorResult blk) where
  condenseList results =
    zipWith
      (\peerId result -> peerId ++ " " ++ result)
      (padListWith PadRight $ map (show . psePeerId) results)
      (padListWith PadRight $ map (condense . pseResult) results)

-- | A state view is a partial view of the state of the whole peer simulator.
-- This includes information about the part of the code that is being tested
-- (for instance the fragment that is selected by the ChainDB) but also
-- information about the mocked peers (for instance the exceptions raised in the
-- mocked ChainSync server threads).
data StateView blk = StateView
  { svSelectedChain :: AnchoredFragment (Header blk)
  , svPeerSimulatorResults :: [PeerSimulatorResult blk]
  , svTipBlock :: Maybe blk
  -- ^ This field holds the most recent point in the selection (incl. anchor)
  -- for which we have a full block (not just a header).
  , svTrace :: [TraceEvent blk]
  -- ^ List of all TraceEvent that have been sent during the simulation.
  }

instance Condense (StateView TestBlock) where
  condense StateView{svSelectedChain, svPeerSimulatorResults, svTipBlock} =
    "SelectedChain: "
      ++ terseHFragment svSelectedChain
      ++ "\n"
      ++ "TipBlock: "
      ++ terseMaybe terseBlock svTipBlock
      ++ "\n"
      ++ "PeerSimulatorResults:\n"
      ++ unlines (fmap ("  - " ++) $ condenseList $ sort svPeerSimulatorResults)

-- | Return the list of peer ids for all peers whose ChainSync thread or
-- BlockFetch thread was terminated.
collectDisconnectedPeers :: StateView blk -> [PeerId]
collectDisconnectedPeers stateView =
  nubOrd $
    map psePeerId (svPeerSimulatorResults stateView)

-- | State view tracers are a lightweight mechanism to record information that
-- can later be used to produce a state view. This mechanism relies on
-- contra-tracers which we already use in a pervasives way.
data StateViewTracers blk m = StateViewTracers
  { svtPeerSimulatorResultsTracer :: Tracer m (PeerSimulatorResult blk)
  , svtGetPeerSimulatorResults :: m [PeerSimulatorResult blk]
  , svtTraceTracer :: Tracer m (TraceEvent blk)
  , svtGetTracerTrace :: m [TraceEvent blk]
  }

-- | Helper to get exceptions from a StateView.
exceptionsByComponent ::
  PeerSimulatorComponent ->
  StateView blk ->
  [SomeException]
exceptionsByComponent component StateView{svPeerSimulatorResults} =
  mapMaybe (matchComponent component) $ pseResult <$> svPeerSimulatorResults
 where
  matchComponent :: PeerSimulatorComponent -> PeerSimulatorComponentResult blk -> Maybe SomeException
  matchComponent = \case
    ChainSyncClient -> \case
      SomeChainSyncClientResult (Left exn) -> Just exn
      _ -> Nothing
    ChainSyncServer -> \case
      SomeChainSyncServerResult (Left exn) -> Just exn
      _ -> Nothing
    BlockFetchClient -> \case
      SomeBlockFetchClientResult (Left exn) -> Just exn
      _ -> Nothing
    BlockFetchServer -> \case
      SomeBlockFetchServerResult (Left exn) -> Just exn
      _ -> Nothing

-- | Make default state view tracers. The tracers are all freshly initialised
-- and contain no information.
defaultStateViewTracers ::
  IOLike m =>
  m (StateViewTracers blk m)
defaultStateViewTracers = do
  (svtPeerSimulatorResultsTracer, svtGetPeerSimulatorResults) <- recordingTracerTVar
  (svtTraceTracer, svtGetTracerTrace) <- recordingTracerTVar
  pure
    StateViewTracers
      { svtPeerSimulatorResultsTracer
      , svtGetPeerSimulatorResults
      , svtTraceTracer
      , svtGetTracerTrace
      }

-- | Call 'defaultStateViewTracers' and add the provided results.
stateViewTracersWithInitial ::
  IOLike m =>
  [PeerSimulatorResult blk] ->
  m (StateViewTracers blk m)
stateViewTracersWithInitial initial = do
  svt <- defaultStateViewTracers
  for_ initial (traceWith (svtPeerSimulatorResultsTracer svt))
  pure svt

-- | Use the state view tracers as well as some extra information to produce a
-- state view. This mostly consists in reading and storing the current state of
-- the tracers.
snapshotStateView ::
  IOLike m =>
  StateViewTracers blk m ->
  ChainDB m blk ->
  m (StateView blk)
snapshotStateView StateViewTracers{svtGetPeerSimulatorResults, svtGetTracerTrace} chainDb = do
  svPeerSimulatorResults <- svtGetPeerSimulatorResults
  svTrace <- svtGetTracerTrace
  svSelectedChain <- atomically $ ChainDB.getCurrentChain chainDb
  svTipBlock <- ChainDB.getTipBlock chainDb
  pure StateView{svSelectedChain, svPeerSimulatorResults, svTipBlock, svTrace}
