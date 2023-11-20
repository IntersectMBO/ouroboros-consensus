{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Business logic of the SyncChain protocol handlers that operates
-- on the 'AdvertisedPoints' of a point schedule.
--
-- These are separated from the scheduling related mechanics of the
-- ChainSync server mock that the peer simulator uses, in
-- "Test.Consensus.PeerSimulator.ScheduledChainSyncServer".
module Test.Consensus.PeerSimulator.Handlers (
    handlerFindIntersection
  , handlerRequestNext
  ) where

import           Control.Monad.Trans (lift)
import           Control.Monad.Writer.Strict (MonadWriter (tell),
                     WriterT (runWriterT))
import           Data.Coerce (coerce)
import           Data.Maybe (fromJust)
import           Ouroboros.Consensus.Block.Abstract (Header, Point (..),
                     getHeader, withOrigin)
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike, STM, StrictTVar,
                     readTVar, writeTVar)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (blockPoint, getTipPoint)
import qualified Test.Consensus.BlockTree as BT
import           Test.Consensus.BlockTree (BlockTree)
import           Test.Consensus.Network.AnchoredFragment.Extras (intersectWith)
import           Test.Consensus.PeerSimulator.ScheduledChainSyncServer
                     (FindIntersect (..),
                     RequestNext (AwaitReply, RollBackward, RollForward))
import           Test.Consensus.PeerSimulator.Trace (terseFrag)
import           Test.Consensus.PointSchedule (AdvertisedPoints (header, tip),
                     HeaderPoint (HeaderPoint), TipPoint (TipPoint))
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock)


-- | Handle a @MsgFindIntersect@ message.
--
-- Extracts the fragment up to the current advertised tip from the block tree,
-- then searches for any of the client's points in it.
handlerFindIntersection ::
  IOLike m =>
  StrictTVar m (Point TestBlock) ->
  BlockTree TestBlock ->
  AdvertisedPoints ->
  [Point TestBlock] ->
  STM m (FindIntersect, [String])
handlerFindIntersection currentIntersection blockTree points clientPoints = do
  let TipPoint tip' = tip points
      tipPoint = getTipPoint tip'
      fragment = fromJust $ BT.findFragment tipPoint blockTree
  case intersectWith fragment clientPoints of
    Nothing ->
      pure (IntersectNotFound tip', [])
    Just intersection -> do
      writeTVar currentIntersection intersection
      pure (IntersectFound intersection tip', [])
  where

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
    withOrigin headerIsOrigin (withHeader intersection) (coerce (header points))
  where
    headerIsOrigin = pure Nothing

    withHeader :: Point TestBlock -> Header TestBlock -> WriterT [String] (STM m) (Maybe RequestNext)
    withHeader intersection h =
      maybe noPathError (analysePath hp) (BT.findPath intersection hp blockTree)
      where
        hp = AF.castPoint $ blockPoint h

    noPathError = error "serveHeader: intersection and headerPoint should always be in the block tree"

    analysePath headerPoint = \case
      -- If the anchor is the intersection (the source of the path-finding) but
      -- the fragment is empty, then the intersection is exactly our header
      -- point and there is nothing to do. If additionally the header point is
      -- also the tip point (because we served our whole chain, or we are
      -- stalling as an adversarial behaviour), then we ask the client to wait;
      -- otherwise we just do nothing.
      (BT.PathAnchoredAtSource True, AF.Empty _) | getTipPoint tip' == headerPoint -> do
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
