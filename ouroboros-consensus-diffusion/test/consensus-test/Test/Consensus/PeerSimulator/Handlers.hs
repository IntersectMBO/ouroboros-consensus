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
  , handlerSendBlocks
  ) where

import           Cardano.Slotting.Slot (WithOrigin (..))
import           Control.Monad.Trans (lift)
import           Control.Monad.Writer.Strict (MonadWriter (tell),
                     WriterT (runWriterT))
import           Data.List (isSuffixOf)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust, fromMaybe)
import           Ouroboros.Consensus.Block (GetHeader, HasHeader, HeaderHash,
                     Point (GenesisPoint), blockHash, getHeader, withOrigin)
import           Ouroboros.Consensus.Util.IOLike (IOLike, STM, StrictTVar,
                     readTVar, writeTVar)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Tip (TipGenesis), blockPoint,
                     getTipPoint, tipFromHeader)
import           Ouroboros.Network.BlockFetch.ClientState
                     (ChainRange (ChainRange))
import qualified Test.Consensus.BlockTree as BT
import           Test.Consensus.BlockTree (BlockTree)
import           Test.Consensus.Network.AnchoredFragment.Extras (intersectWith)
import           Test.Consensus.PeerSimulator.ScheduledBlockFetchServer
                     (BlockFetch (..), SendBlocks (..))
import           Test.Consensus.PeerSimulator.ScheduledChainSyncServer
                     (FindIntersect (..),
                     RequestNext (AwaitReply, RollBackward, RollForward))
import           Test.Consensus.PeerSimulator.Trace
                     (TraceScheduledBlockFetchServerEvent (..),
                     TraceScheduledChainSyncServerEvent (..))
import           Test.Consensus.PointSchedule
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock, TestHash (TestHash))

-- | More efficient implementation of a check used in some of the handlers,
-- determining whether the first argument is on the chain that ends in the
-- second argument.
-- We would usually call @withinFragmentBounds@ for this, but since we're
-- using 'TestBlock', looking at the hash is cheaper.
--
-- TODO: Unify with 'Test.UtilTestBlock.isAncestorOf' which basically does the
-- same thing except not on 'WithOrigin'.
isAncestorOf ::
  HasHeader blk1 =>
  HasHeader blk2 =>
  HeaderHash blk1 ~ TestHash =>
  HeaderHash blk2 ~ TestHash =>
  WithOrigin blk1 ->
  WithOrigin blk2 ->
  Bool
isAncestorOf (At ancestor) (At descendant) =
  isSuffixOf (NonEmpty.toList hashA) (NonEmpty.toList hashD)
  where
    TestHash hashA = blockHash ancestor
    TestHash hashD = blockHash descendant
isAncestorOf (At _) Origin = False
isAncestorOf Origin _ = True

-- | Handle a @MsgFindIntersect@ message.
--
-- Extracts the fragment up to the current advertised tip from the block tree,
-- then searches for any of the client's points in it.
handlerFindIntersection ::
  (IOLike m, HasHeader blk) =>
  StrictTVar m (Point blk) ->
  BlockTree blk ->
  [Point blk] ->
  NodeState blk ->
  STM m (Maybe (FindIntersect blk), [TraceScheduledChainSyncServerEvent (NodeState blk) blk])
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
-- - Anchor == intersection == header point
-- - header point after intersection == header point
-- - header point before intersection (special case for the point scheduler architecture)
-- - Anchor != intersection
handlerRequestNext ::
  forall m blk.
  (IOLike m, HasHeader blk, GetHeader blk) =>
  StrictTVar m (Point blk) ->
  BlockTree blk ->
  NodeState blk ->
  STM m (Maybe (RequestNext blk), [TraceScheduledChainSyncServerEvent (NodeState blk) blk])
handlerRequestNext currentIntersection blockTree points =
  runWriterT $ do
    intersection <- lift $ readTVar currentIntersection
    trace $ TraceLastIntersection intersection
    withHeader intersection (nsHeader points)
  where
    withHeader ::
      Point blk ->
      WithOrigin blk ->
      WriterT
        [TraceScheduledChainSyncServerEvent (NodeState blk) blk]
        (STM m)
        (Maybe (RequestNext blk))
    withHeader intersection h =
      maybe noPathError (analysePath hp) (BT.findPath intersection hp blockTree)
      where
        hp = withOrigin GenesisPoint blockPoint h

    noPathError = error "serveHeader: intersection and headerPoint should always be in the block tree"

    analysePath hp = \case
      -- If the anchor is the intersection (the source of the path-finding) but
      -- the fragment is empty, then the intersection is exactly our header
      -- point and there is nothing to do. If additionally the header point is
      -- also the tip point (because we served our whole chain, or we are
      -- stalling as an adversarial behaviour), then we ask the client to wait;
      -- otherwise we just do nothing.
      (BT.PathAnchoredAtSource True, AF.Empty _) | getTipPoint tip' == hp -> do
        trace TraceChainIsFullyServed
        pure (Just AwaitReply)
      (BT.PathAnchoredAtSource True, AF.Empty _) -> do
        trace TraceIntersectionIsHeaderPoint
        pure Nothing
      -- If the anchor is the intersection and the fragment is non-empty, then
      -- we have something to serve.
      (BT.PathAnchoredAtSource True, fragmentAhead@(next AF.:< _)) -> do
        trace $ TraceIntersectionIsStrictAncestorOfHeaderPoint fragmentAhead
        lift $ writeTVar currentIntersection $ blockPoint next
        pure $ Just (RollForward (getHeader next) (nsTipTip points))
      -- If the anchor is not the intersection but the fragment is empty, then
      -- the intersection is further than the tip that we can serve.
      (BT.PathAnchoredAtSource False, AF.Empty _) -> do
        trace TraceIntersectionIsStrictDescendentOfHeaderPoint
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
        let point = AF.anchorPoint fragment
        lift $ writeTVar currentIntersection point
        pure $ Just (RollBackward point tip')

    tip' = withOrigin TipGenesis tipFromHeader $ nsTip points

    trace = tell . pure

-- REVIEW: We call this a lot, and I assume it creates some significant overhead.
-- We should figure out a cheaper way to achieve what we're doing with the result.
-- Since we're in blk, we can at least check whether a block can extend another block,
-- but then we won't be able to generalize later.
fragmentUpTo :: HasHeader blk => BlockTree blk -> String -> Point blk -> AnchoredFragment blk
fragmentUpTo blockTree desc b =
  fromMaybe fatal (BT.findFragment b blockTree)
  where
    fatal = error ("BlockFetch: Could not find " ++ desc ++ " in the block tree")

-- | Handle the BlockFetch message (it actually has only one unnamed entry point).
--
-- If the requested range ends not after the block point, send them. If the
-- block point moved to a fork without serving all blocks corresponding to
-- advertised headers, serve them. Otherwise, stall.
handlerBlockFetch ::
  forall m blk.
  (IOLike m, HasHeader blk) =>
  BlockTree blk ->
  -- ^ The tree of blocks in this scenario -- aka. the “universe”.
  ChainRange (Point blk) ->
  -- ^ A requested range of blocks. If the client behaves correctly, they
  -- correspond to headers that have been sent before, and if the scheduled
  -- ChainSync server behaves correctly, then they are all in the block tree.
  NodeState blk ->
  -- ^ The current advertised points (tip point, header point and block point).
  -- They are in the block tree.
  STM m (Maybe (BlockFetch blk), [TraceScheduledBlockFetchServerEvent (NodeState blk) blk])
handlerBlockFetch blockTree (ChainRange from to) NodeState {nsHeader} =
  runWriterT (serveFromBpFragment (AF.sliceRange hpChain from to))
  where
    -- Check whether the requested range is contained in the fragment before the header point.
    -- We may only initiate batch serving if the full range is available; if the server has only some of the blocks, it
    -- must refuse.
    serveFromBpFragment = \case
      Just slice -> do
        trace $ TraceStartingBatch slice
        pure (Just (StartBatch (AF.toOldestFirst slice)))
      Nothing    -> do
        trace $ TraceWaitingForRange from to
        pure Nothing

    hpChain = fragmentUpTo blockTree "header point" (withOrigin GenesisPoint blockPoint nsHeader)

    trace = tell . pure

{-
If we cannot serve blocks from the block point chain (that is the chain on which
the block point is), decide whether to yield control to the scheduler or serve
blocks anyway.

If the next block to deliver is part of the header point chain, we have to wait
for the block point to advance sufficiently, and we block without sending a
message, to simulate a slow response.

If the next block to deliver is not part of the header point chain, but the
block point is, we must have switched to a fork without ensuring that the block
point advances to the last header point advertised on the old chain.

While the block point is not on the same chain as the header point we wait,
because the block point might still advance to allow the next block to be sent.
If the block point is in the same chain as the header point, we interpret that
the block point has left the old branch, and the requested blocks should be sent
at this time.

The cases to consider follow:

 0 1 2 3 4 5 6
 x-x-x-x-x-x-x
         ^BP ^HP
       \-x-x-x
         ^next
✅ send the blocks because a rollback happened

 0 1 2 3 4 5 6
 x-x-x-x-x-x-x
   ^BP       ^HP
       \-x-x-x
         ^next

❌ BP might still go on the fork where next is, so don't send the blocks yet

 0 1 2 3 4 5 6
 x-x-x-x-x-x-x
   ^BP
     \-x-x-x-x
       ^next ^HP

❌ BP could still go on the fork where next is, don't send the blocks yet

 0 1 2 3 4 5 6
 x-x-x-x-x-x-x
     ^BP
   \-x-x-x-x-x
     ^next   ^HP

❌ BP could still go on the fork where next is, don't send the blocks yet

 0 1 2 3 4 5 6
 x-x-x-x-x-x-x
     ^BP
   \-x-x-x-x-x
     ^next
   \-x-x-x-x-x
       ^HP

❌ BP could still go on the fork where next is, don't send the blocks yet

 0 1 2 3 4 5 6
 x-x-x-x-x-x-x
     ^HP
   \-x-x-x-x-x
     ^next   ^BP

✅ send the blocks because BP is after next

 0 1 2 3 4 5 6
 x-x-x-x-x-x-x
     ^HP
   \-x-x-x-x-x
     ^BP   ^next

❌ BP could still advance past next, don't send the blocks yet

-}
handlerSendBlocks ::
  forall m .
  IOLike m =>
  [TestBlock] ->
  NodeState TestBlock ->
  STM m (Maybe (SendBlocks TestBlock), [TraceScheduledBlockFetchServerEvent (NodeState TestBlock) TestBlock])
handlerSendBlocks blocks NodeState {nsHeader, nsBlock} =
  runWriterT (checkDone blocks)
  where
    checkDone = \case
      [] -> do
        trace TraceBatchIsDone
        pure (Just BatchDone)
      (next : future) ->
        blocksLeft next future

    blocksLeft next future
      | isAncestorOf (At next) nsBlock
      || compensateForScheduleRollback next
      = do
        trace $ TraceSendingBlock next
        pure (Just (SendBlock next future))

      | otherwise
      = do
        trace TraceBlockPointIsBehind
        pure Nothing

    -- Here we encode the conditions for the special situation mentioned above.
    -- These use aliases for @withinFragmentBounds@ to illustrate what we're testing.
    -- The names don't precisely match semantically, but it's difficult to understand the
    -- circumstances otherwise.
    --
    -- The involved points are BP, HP, and @next@, which is the block we're deciding whether to
    -- send or not.
    --
    -- Remember that at this point, we already know that we cannot send @next@ regularly, i.e.
    -- @next@ is not on the chain leading up to BP.
    -- The conditions in which we send @next@ to compensate for rollbacks are:
    --
    -- * @next@ is not on the chain leading up to HP – HP moved to another chain, and
    --
    -- * BP is in the same chain as HP and is not an ancestor of @next@ - BP also moved away from the chain of @next@.
    --
    -- Precondition: @not (isAncestorOf (At next) bp)@
    compensateForScheduleRollback next =
      not (isAncestorOf (At next) nsHeader)
        && isAncestorOf nsBlock nsHeader
        && not (isAncestorOf nsBlock (At next))

    trace = tell . pure
