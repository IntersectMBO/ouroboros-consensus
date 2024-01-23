{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Business logic of the ChainSync and BlockFetch protocol handlers that operate
-- on the 'AdvertisedPoints' of a point schedule.
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
import           Data.Coerce (coerce)
import           Data.List (isSuffixOf)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust, fromMaybe)
import           Ouroboros.Consensus.Block (HasHeader, Header, HeaderHash,
                     Point (GenesisPoint), blockHash, castPoint, getHeader,
                     withOrigin)
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike, STM, StrictTVar,
                     readTVar, writeTVar)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (blockPoint, getTipPoint)
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
import           Test.Consensus.PointSchedule
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TersePrinting (terseBlock, terseFrag, tersePoint)
import           Test.Util.TestBlock (TestBlock, TestHash (TestHash))

toPoint :: (HasHeader block, HeaderHash block ~ TestHash) => WithOrigin block -> Point TestBlock
toPoint = castPoint . withOrigin GenesisPoint blockPoint

-- | More efficient implementation of a check used in some of the handlers,
-- determining whether the first argument is on the chain that ends in the
-- second argument.
-- We would usually call @withinFragmentBounds@ for this, but since we're
-- using 'TestBlock', looking at the hash is cheaper.
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
  IOLike m =>
  StrictTVar m (Point TestBlock) ->
  BlockTree TestBlock ->
  [Point TestBlock] ->
  AdvertisedPoints ->
  STM m (Maybe FindIntersect, [String])
handlerFindIntersection currentIntersection blockTree clientPoints points = do
  let TipPoint tip' = tip points
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
  AdvertisedPoints ->
  STM m (Maybe RequestNext, [String])
handlerRequestNext currentIntersection blockTree points =
  runWriterT $ do
    intersection <- lift $ readTVar currentIntersection
    trace $ "  last intersection is " ++ condense intersection
    withHeader intersection (coerce (header points))
  where
    withHeader :: Point TestBlock -> WithOrigin (Header TestBlock) -> WriterT [String] (STM m) (Maybe RequestNext)
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
        trace $ "  fragment ahead: " ++ terseFrag fragmentAhead
        lift $ writeTVar currentIntersection $ blockPoint next
        pure $ Just (RollForward (getHeader next) (coerce (tip points)))
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

    TipPoint tip' = tip points

    trace = tell . pure

-- REVIEW: We call this a lot, and I assume it creates some significant overhead.
-- We should figure out a cheaper way to achieve what we're doing with the result.
-- Since we're in TestBlock, we can at least check whether a block can extend another block,
-- but then we won't be able to generalize later.
fragmentUpTo :: BlockTree TestBlock -> String -> Point TestBlock -> AnchoredFragment TestBlock
fragmentUpTo blockTree desc b =
  fromMaybe fatal (BT.findFragment b blockTree)
  where
    fatal = error ("BlockFetch: Could not find " ++ desc ++ " in the block tree")

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
  AdvertisedPoints ->
  STM m (Maybe BlockFetch, [String])
handlerBlockFetch blockTree (ChainRange from to) AdvertisedPoints {header = HeaderPoint hp} =
  runWriterT (serveFromBpFragment (AF.sliceRange hpChain from to))
  where
    -- Check whether the requested range is contained in the fragment before the header point.
    -- We may only initiate batch serving if the full range is available; if the server has only some of the blocks, it
    -- must refuse.
    serveFromBpFragment = \case
      Just slice -> do
        trace ("Starting batch for slice " ++ terseFrag slice)
        pure (Just (StartBatch (AF.toOldestFirst slice)))
      Nothing    -> do
        trace ("Waiting for next tick for range: " ++ tersePoint from ++ " -> " ++ tersePoint to)
        pure Nothing

    hpChain = fragmentUpTo blockTree "header point" (toPoint hp)

    trace = tell . pure

{-
If we cannot serve blocks from the BP chain, we need to decide whether to yield control to the scheduler
or serve blocks anyway.

If the next block to deliver is part of the HP chain, we have to wait for BP to advance
sufficiently, and we block without sending a message, to simulate a slow response.

If the next block to deliver is not part of the HP chain, we must have switched to a fork without
ensuring that BP advances to the last HP advertised on the old chain.
While BP is not on the same chain as HP we wait, because BP might still advance to
allow the next block to be sent. If BP is in the same chain as HP, we interpret
that BP has left the old branch, and the requested blocks should be sent at this
time.

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
  AdvertisedPoints ->
  STM m (Maybe SendBlocks, [String])
handlerSendBlocks blocks AdvertisedPoints {header = HeaderPoint hp, block = BlockPoint bp} =
  runWriterT (checkDone blocks)
  where
    checkDone = \case
      [] -> do
        trace "Batch done"
        pure (Just BatchDone)
      (next : future) ->
        blocksLeft next future

    blocksLeft next future
      | isAncestorOf (At next) bp
      || compensateForScheduleRollback next
      = do
        trace ("Sending " ++ terseBlock next)
        pure (Just (SendBlock next future))

      | otherwise
      = do
        trace "BP is behind"
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
      not (isAncestorOf (At next) hp) && isAncestorOf bp hp && not (isAncestorOf bp (At next))

    trace = tell . pure
