module Test.Ouroboros.Consensus.PeerSimulator.Handlers (
    handlerFindIntersection
  , handlerRequestNext
  ) where

import           Data.Coerce (coerce)
import           Data.Maybe (fromJust)
import           Ouroboros.Consensus.Block.Abstract (Point (..),
                     getHeader)
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike, StrictTVar, writeTVar, STM, readTVar)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (blockPoint, getTipPoint)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Tests.BlockTree as BT
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.BlockTree
                     (BlockTree)
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.PointSchedule
                     (AdvertisedPoints (header, tip), HeaderPoint (HeaderPoint),
                     TipPoint (TipPoint))
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.Sync
                     (intersectWith)
import           Test.Ouroboros.Consensus.PeerSimulator.ScheduledChainSyncServer
                     (FindIntersect (..), RequestNext (RollForward, RollBackward))
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock)
import Control.Monad.Writer.Strict (WriterT (runWriterT), MonadWriter (tell))
import Control.Monad.Trans (lift)

handlerFindIntersection ::
  IOLike m =>
  StrictTVar m (Point TestBlock) ->
  BlockTree TestBlock ->
  AdvertisedPoints ->
  [Point TestBlock] ->
  STM m (FindIntersect, [String])
handlerFindIntersection currentIntersection blockTree points pts = do
  let TipPoint tip' = tip points
      tipPoint = Ouroboros.Network.Block.getTipPoint tip'
      fragment = fromJust $ BT.findFragment tipPoint blockTree
  case intersectWith fragment pts of
    Nothing ->
      pure (IntersectNotFound tip', [])
    Just intersection -> do
      writeTVar currentIntersection intersection
      pure (IntersectFound intersection tip', [])

serveHeader ::
  IOLike m =>
  StrictTVar m (Point TestBlock) ->
  BlockTree TestBlock ->
  AdvertisedPoints ->
  WriterT [String] (STM m) (Maybe RequestNext)
serveHeader currentIntersection blockTree points = do
  intersection <- lift $ readTVar currentIntersection
  trace $ "  last intersection is " ++ condense intersection
  let HeaderPoint header' = header points
      headerPoint = AF.castPoint $ blockPoint header'
  case BT.findPath intersection headerPoint blockTree of
    Nothing -> error "serveHeader: intersection and and headerPoint should always be in the block tree"
    Just findPathResult ->
      case findPathResult of
        -- If the anchor is the intersection (the source of the path-finding)
        -- but the fragment is empty, then the intersection is exactly our
        -- header point and there is nothing to do.
        (BT.PathAnchoredAtSource True, AF.Empty _) -> do
          trace "  intersection is exactly our header point"
          pure Nothing
        -- If the anchor is the intersection and the fragment is non-empty, then
        -- we have something to serve.
        (BT.PathAnchoredAtSource True, fragmentAhead@(next AF.:< _)) -> do
          trace "  intersection is before our header point"
          trace $ "  fragment ahead: " ++ condense fragmentAhead
          lift $ writeTVar currentIntersection $ blockPoint next
          pure $ Just (RollForward (getHeader next) (coerce (tip points)))
        -- If the anchor is not the intersection but the fragment is empty, then
        -- the intersection is further than the tip that we can serve.
        (BT.PathAnchoredAtSource False, AF.Empty _) -> do
          trace "  intersection is further than our header point"
          pure Nothing
        -- If the anchor is not the intersection and the fragment is non-empty,
        -- then we require a rollback
        (BT.PathAnchoredAtSource False, fragment) -> do
          trace $ "  we will require a rollback to" ++ condense (AF.anchorPoint fragment)
          trace $ "  fragment: " ++ condense fragment
          let
            tip' = coerce (tip points)
            point = AF.anchorPoint fragment
          lift $ writeTVar currentIntersection point
          pure $ Just (RollBackward point tip')
  where
    trace = tell . pure

handlerRequestNext ::
  IOLike m =>
  StrictTVar m (Point TestBlock) ->
  BlockTree TestBlock ->
  AdvertisedPoints ->
  STM m (Maybe RequestNext, [String])
handlerRequestNext currentIntersection blockTree advertisedPoints =
  runWriterT (serveHeader currentIntersection blockTree advertisedPoints)
