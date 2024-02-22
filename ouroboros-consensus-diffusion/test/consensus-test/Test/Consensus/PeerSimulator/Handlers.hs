{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Business logic of the ChainSync and BlockFetch protocol handlers that operate
-- on the 'NodeState' of a point schedule.
--
-- These are separated from the scheduling related mechanics of the
-- server mocks that the peer simulator uses, in
-- "Test.Consensus.PeerSimulator.ScheduledChainSyncServer" and
-- "Test.Consensus.PeerSimulator.ScheduledBlockFetchServer".
module Test.Consensus.PeerSimulator.Handlers (
    handlerBlockFetch
  , handlerFindIntersection
  , handlerRequestNext
  ) where

import           Cardano.Slotting.Slot (WithOrigin)
import           Control.Monad.Trans (lift)
import           Control.Monad.Writer.Strict (MonadWriter (tell),
                     WriterT (runWriterT))
import           Data.Maybe (fromJust, fromMaybe)
import           Ouroboros.Consensus.Block (HasHeader, HeaderHash,
                     Point (GenesisPoint), castPoint, getHeader, withOrigin)
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike, STM, StrictTVar,
                     readTVar, writeTVar)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Tip (TipGenesis), blockPoint,
                     getTipPoint, tipFromHeader)
import           Ouroboros.Network.BlockFetch.ClientState
                     (ChainRange (ChainRange))
import qualified Test.Consensus.BlockTree as BT
import           Test.Consensus.BlockTree (BlockTree)
import           Test.Consensus.Network.AnchoredFragment.Extras (intersectWith)
import           Test.Consensus.PeerSimulator.ScheduledBlockFetchServer
                     (BlockFetch (..))
import           Test.Consensus.PeerSimulator.ScheduledChainSyncServer
                     (FindIntersect (..),
                     RequestNext (AwaitReply, RollBackward, RollForward))
import           Test.Consensus.PointSchedule
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TersePrinting (terseFragment, tersePoint)
import           Test.Util.TestBlock (TestBlock, TestHash)

toPoint :: (HasHeader block, HeaderHash block ~ TestHash) => WithOrigin block -> Point TestBlock
toPoint = castPoint . withOrigin GenesisPoint blockPoint

-- | Handle a @MsgFindIntersect@ message.
--
-- Extracts the fragment up to the current advertised tip from the block tree,
-- then searches for any of the client's points in it.
handlerFindIntersection ::
  IOLike m =>
  StrictTVar m (Point TestBlock) ->
  BlockTree TestBlock ->
  [Point TestBlock] ->
  NodeState ->
  STM m (Maybe FindIntersect, [String])
handlerFindIntersection currentIntersection blockTree clientPoints points = do
  let tip' = nsTipTip points
      tipPoint = getTipPoint tip'
      fragment = fromJust $ BT.findFragment tipPoint blockTree
  case intersectWith fragment clientPoints of
    Nothing ->
      pure (Just (IntersectNotFound tip'), [])
    Just intersection -> do
      writeTVar currentIntersection intersection
      pure (Just (IntersectFound intersection tip'), [])

-- | Handle a @MsgRequestNext@ message.
--
-- Finds the potential path from the current intersection to the advertised header point for this turn,
-- which can have four distinct configurations for the anchor point and the path:
--
-- - Anchor == intersection == HP
-- - HP after intersection == HP
-- - HP before intersection (special case for the point scheduler architecture)
-- - Anchor != intersection
handlerRequestNext ::
  forall m .
  IOLike m =>
  StrictTVar m (Point TestBlock) ->
  BlockTree TestBlock ->
  NodeState ->
  STM m (Maybe RequestNext, [String])
handlerRequestNext currentIntersection blockTree points =
  runWriterT $ do
    intersection <- lift $ readTVar currentIntersection
    trace $ "  last intersection is " ++ tersePoint intersection
    withHeader intersection (nsHeader points)
  where
    withHeader :: Point TestBlock -> WithOrigin TestBlock -> WriterT [String] (STM m) (Maybe RequestNext)
    withHeader intersection h =
      maybe noPathError (analysePath hp) (BT.findPath intersection hp blockTree)
      where
        hp = toPoint h

    noPathError = error "serveHeader: intersection and headerPoint should always be in the block tree"

    analysePath hp = \case
      -- If the anchor is the intersection (the source of the path-finding) but
      -- the fragment is empty, then the intersection is exactly our header
      -- point and there is nothing to do. If additionally the header point is
      -- also the tip point (because we served our whole chain, or we are
      -- stalling as an adversarial behaviour), then we ask the client to wait;
      -- otherwise we just do nothing.
      (BT.PathAnchoredAtSource True, AF.Empty _) | getTipPoint tip' == hp -> do
        trace "  chain has been fully served"
        pure (Just AwaitReply)
      (BT.PathAnchoredAtSource True, AF.Empty _) -> do
        trace "  intersection is exactly our header point"
        pure Nothing
      -- If the anchor is the intersection and the fragment is non-empty, then
      -- we have something to serve.
      (BT.PathAnchoredAtSource True, fragmentAhead@(next AF.:< _)) -> do
        trace "  intersection is before our header point"
        trace $ "  fragment ahead: " ++ terseFragment fragmentAhead
        lift $ writeTVar currentIntersection $ blockPoint next
        pure $ Just (RollForward (getHeader next) (nsTipTip points))
      -- If the anchor is not the intersection but the fragment is empty, then
      -- the intersection is further than the tip that we can serve.
      (BT.PathAnchoredAtSource False, AF.Empty _) -> do
        trace "  intersection is further than our header point"
        -- REVIEW: The following is a hack that allows the honest peer to not
        -- get disconnected when it falls behind. Why does a peer doing that not
        -- get disconnected from?
        --
        -- We decided to hold off on making this work with timeouts, so we'll return
        -- Nothing here for now.
        -- The consequence of this is that a slow peer will just block until it reaches
        -- the fork intersection in its schedule.
        -- pure (Just AwaitReply)
        pure Nothing
      -- If the anchor is not the intersection and the fragment is non-empty,
      -- then we require a rollback
      (BT.PathAnchoredAtSource False, fragment) -> do
        trace $ "  we will require a rollback to" ++ condense (AF.anchorPoint fragment)
        trace $ "  fragment: " ++ condense fragment
        let
          point = AF.anchorPoint fragment
        lift $ writeTVar currentIntersection point
        pure $ Just (RollBackward point tip')

    tip' = withOrigin TipGenesis tipFromHeader $ nsTip points

    trace = tell . pure

-- | Handle the BlockFetch message (it actually has only one unnamed entry point).
--
-- If the requested range ends not after BP, send them.
-- If BP moved to a fork without serving all blocks corresponding to advertised headers, serve them.
-- Otherwise, stall.
handlerBlockFetch ::
  forall m .
  IOLike m =>
  BlockTree TestBlock ->
  ChainRange (Point TestBlock) ->
  NodeState ->
  STM m (Maybe BlockFetch, [String])
handlerBlockFetch blockTree (ChainRange from to) NodeState {nsBlock, nsHeader} =
  runWriterT (serveFromBpFragment (AF.sliceRange bpChain from to))
  where
    -- Check whether the requested range is contained in the fragment before the block point.
    -- We may only serve the full range; if the server has only some of the blocks, it must refuse.
    serveFromBpFragment = \case
      Just slice -> do
        trace ("Starting batch for slice " ++ terseFragment slice)
        pure (Just (StartBatch (AF.toOldestFirst slice)))
      Nothing    -> do
        -- If we cannot serve blocks from the BP chain, decide whether to yield control to the scheduler
        -- or serve blocks anyway.
        --
        -- If the @to@ point is not part of the HP chain but BP is, we must have switched to a fork without ensuring
        -- that BP advances to the last HP advertised on the old chain.
        -- This should be a precondition violation, but because it would make the schedule generator more complex, we
        -- simply serve the missing blocks anyway here.
        --
        -- Otherwise, we simply have to wait for BP to advance sufficiently, and we block without sending
        -- a message, to simulate a slow response.
        case not (AF.withinFragmentBounds to hpChain) && AF.withinFragmentBounds (toPoint nsBlock) hpChain of
          True ->
            case AF.sliceRange (fragmentUpTo "requested point" to) from to of
              Just slice -> do
                trace "Sending range due to incomplete fork switch"
                pure (Just (StartBatch (AF.toOldestFirst slice)))
              Nothing    -> error "BlockFetch: Client requested blocks that aren't in the block tree"
          False  -> do
            trace ("Waiting for next tick for range: " ++ tersePoint from ++ " -> " ++ tersePoint to)
            pure Nothing

    bpChain = fragmentUpTo "block point" (toPoint nsBlock)

    hpChain = fragmentUpTo "header point" (toPoint nsHeader)

    trace = tell . pure

    fragmentUpTo sort b =
      fromMaybe (fatal sort) (BT.findFragment b blockTree)

    fatal sort = error ("BlockFetch: Could not find " ++ sort ++ " in the block tree")
