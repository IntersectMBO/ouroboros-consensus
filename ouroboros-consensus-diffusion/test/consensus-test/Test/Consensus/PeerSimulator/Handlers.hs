{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  ) where

import           Cardano.Slotting.Slot (WithOrigin (At))
import           Control.Monad.Trans (lift)
import           Control.Monad.Writer.Strict (MonadWriter (tell),
                     WriterT (runWriterT))
import           Data.Coerce (coerce)
import           Data.Maybe (fromJust)
import           Data.Monoid (First (..))
import           Ouroboros.Consensus.Block (Header, Point (GenesisPoint), getHeader, headerPoint, withOrigin)
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
import           Test.Consensus.PeerSimulator.ScheduledBlockFetchServer
                     (BlockFetch (..))
import           Test.Consensus.PeerSimulator.ScheduledChainSyncServer
                     (FindIntersect (..),
                     RequestNext (AwaitReply, RollBackward, RollForward))
import           Test.Consensus.PeerSimulator.Trace (terseFrag, tersePoint)
import           Test.Consensus.PointSchedule
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock)

-- | Find the first fragment contained in the first arg that starts at one of the given points.
intersectWith :: AnchoredFragment TestBlock -> [Point TestBlock] -> Maybe (Point TestBlock)
intersectWith fullFrag pts =
  AF.anchorPoint . snd <$> getFirst (foldMap (First . AF.splitAfterPoint fullFrag) pts)

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
        hp = AF.castPoint $ withOrigin GenesisPoint blockPoint h

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

handlerBlockFetch ::
  forall m .
  IOLike m =>
  BlockTree TestBlock ->
  ChainRange (Point TestBlock) ->
  AdvertisedPoints ->
  STM m (Maybe BlockFetch, [String])
handlerBlockFetch blockTree (ChainRange from to) points =
  runWriterT $ case points of
    AdvertisedPoints {header = HeaderPoint (At h), block = BlockPoint (At b)} -> do
      case BT.findFragment (headerPoint h) blockTree of
        Just f  -> withHpFragment (f, b)
        Nothing -> error "header point is not in the block tree"
    AdvertisedPoints {} -> pure Nothing
  where

    withHpFragment (hpChain, bp) =
      -- First, check whether the block point is on the same chain, and before or equal to, the header point.
      case AF.rollback (blockPoint bp) hpChain of
        Nothing ->
          -- REVIEW: Should this be fatal?
          error "Block point isn't on the same chain as header point"
          -- pure (Just NoBlocks)
        Just bpChain ->
          -- Next, check whether the requested range is contained in the fragment before the block point.
          -- REVIEW: this is Nothing if only _part_ of the range is on the current chain.
          -- Is it correct that we don't want to send anything in this case?
          case AF.sliceRange bpChain from to of
            Nothing    -> do
              trace ("Waiting for next tick for range: " ++ tersePoint from ++ " -> " ++ tersePoint to)
              -- Finally, if we cannot serve blocks, decide whether to send NoBlocks before yielding control
              -- to the scheduler.
              -- If the @to@ point is not part of the chain up to HP, we must have switched to a fork, so we
              -- need to give the client a chance to adapt the range before trying again with the next tick's
              -- chain.
              -- Otherwise, we simply have to wait for BP to advance sufficiently, and we block without sending
              -- a message, to simulate a slow response.
              case AF.withinFragmentBounds to hpChain of
                False -> pure (Just NoBlocks)
                True  -> pure Nothing
            Just slice -> do
              trace ("Sending slice " ++ terseFrag slice)
              pure (Just (StartBatch (AF.toOldestFirst slice)))

    trace = tell . pure
