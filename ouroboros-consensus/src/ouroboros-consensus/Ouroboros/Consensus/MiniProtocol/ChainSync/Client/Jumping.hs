{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.MiniProtocol.ChainSync.Client.Jumping (
    Instruction (..)
  , JumpResult (..)
  , nextInstruction
  , processJumpResult
  , registerClient
  , unregisterClient
  ) where

import           Cardano.Slotting.Slot (SlotNo, WithOrigin (..))
import           Control.Monad (forM_, when)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust, mapMaybe)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block (HasHeader, Header, Point (..),
                     castPoint, pointSlot, succWithOrigin)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State
                     (ChainSyncClientHandle (..),
                     ChainSyncJumpingJumperState (..),
                     ChainSyncJumpingState (..), ChainSyncState (..))
import           Ouroboros.Consensus.Util.IOLike hiding (handle)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     blockPoint, headPoint, headSlot, splitAfterPoint,
                     splitBeforePoint, toOldestFirst)

-- | Instruction from the jumping governor, either to run normal ChainSync, or
-- to jump to follow the given peer with the given fragment.
data Instruction blk
  = RunNormally
  | JumpTo !(Point blk)
  deriving (Generic)

deriving instance (HasHeader blk, Eq (Header blk)) => Eq (Instruction blk)
deriving instance (HasHeader blk, Show (Header blk)) => Show (Instruction blk)
deriving anyclass instance (HasHeader blk, NoThunks (Header blk)) => NoThunks (Instruction blk)

data JumpResult blk
  = AcceptedJump !(Point blk)
  | RejectedJump !(Point blk)
  deriving (Generic)

deriving instance (HasHeader blk, Eq (Header blk)) => Eq (JumpResult blk)
deriving instance (HasHeader blk, Show (Header blk)) => Show (JumpResult blk)
deriving anyclass instance (HasHeader blk, NoThunks (Header blk)) => NoThunks (JumpResult blk)

nextInstruction ::
  ( MonadSTM m,
    Ord peer,
    HasHeader blk, HasHeader (Header blk)
  ) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  peer ->
  STM m (Instruction blk)
nextInstruction handlesVar peer =
  (cschJumping . (Map.! peer) <$> readTVar handlesVar) >>= \case
    Dynamo lastJumpSlot -> maybeSetNextJump lastJumpSlot >> pure RunNormally
    Objector _ -> pure RunNormally
    Jumper nextJumpVar _ _ ->
      readTVar nextJumpVar >>= \case
        Nothing -> retry
        Just fragment -> do
          writeTVar nextJumpVar Nothing
          pure $ JumpTo fragment
  where
    -- | When the tip of the candidate fragment is 'jumpSize' slots younger than
    -- the last jump, set Happy jumpers to jump to it.
    maybeSetNextJump lastJumpSlot = do
      dynamoFragment <- fromJust <$> getDynamoFragment handlesVar
      when (succWithOrigin (headSlot dynamoFragment) >= succWithOrigin lastJumpSlot + jumpSize) $ do
        handles <- readTVar handlesVar
        forM_ (map cschJumping $ Map.elems handles) $ \case
          Jumper nextJumpVar _ Happy -> writeTVar nextJumpVar $ Just $! castPoint (headPoint dynamoFragment)
          _ -> pure ()

setJumpingState ::
  ( MonadSTM m,
    Ord peer
  ) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  peer ->
  ChainSyncJumpingState m blk ->
  STM m ()
setJumpingState handlesVar peer cschJumping =
  modifyTVar handlesVar $ Map.update (\handle -> Just $! handle {cschJumping}) peer

processJumpResult ::
  ( MonadSTM m,
    Ord peer, HasHeader blk, HasHeader (Header blk)
  ) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  peer ->
  JumpResult blk ->
  STM m ()
processJumpResult handlesVar peer jumpResult =
  (cschJumping . (Map.! peer) <$> readTVar handlesVar) >>= \case
    Dynamo _ -> pure ()
    Objector _ -> pure ()
    Jumper nextJumpVar goodPoint state ->
        case (state, jumpResult) of
          (FoundIntersection, _) ->
            -- Ignore jump results when we already found the intersection.
            pure ()
          (Happy, AcceptedJump goodPoint') -> do
            -- The jump was accepted; we set the jumper's candidate fragment to
            -- the dynamo's candidate fragment up to the accepted point.
            --
            -- The candidate fragments of jumpers don't grow otherwise, as only the
            -- objector and the dynamo request further headers.
            dynamoFragment <- fromJust <$> getDynamoFragment handlesVar
            let slicedDynamoFragment = fst $ fromJust $ splitAfterPoint dynamoFragment goodPoint'
            chainSyncStateVar <- cschState . (Map.! peer) <$> readTVar handlesVar
            modifyTVar chainSyncStateVar $ \chainSyncState -> chainSyncState {csCandidate = slicedDynamoFragment}
            setJumpingState handlesVar peer $ Jumper nextJumpVar goodPoint' Happy
          (Happy, RejectedJump badPoint) ->
            -- The previously happy peer just rejected a jump. We look for an
            -- intersection.
            lookForIntersection nextJumpVar goodPoint badPoint
          (LookingForIntersection badPoint, AcceptedJump goodPoint') ->
            -- The peer that was looking for an intersection accepts the given
            -- point, helping us narrow things down.
            lookForIntersection nextJumpVar goodPoint' badPoint
          (LookingForIntersection _, RejectedJump badPoint') ->
            -- The peer that was looking for an intersection rejects the given
            -- point, helping us narrow things down.
            lookForIntersection nextJumpVar goodPoint badPoint'
  where
    lookForIntersection nextJumpVar goodPoint badPoint = do
      (dynamoFragment0 :: AnchoredFragment (Header blk)) <- fromJust <$> getDynamoFragment handlesVar -- full fragment
      let dynamoFragment = fromJust $ sliceRangeExcl dynamoFragment0 goodPoint badPoint
      case (toOldestFirst dynamoFragment :: [Header blk]) of
        [] -> do
          setJumpingState handlesVar peer $ Jumper nextJumpVar goodPoint FoundIntersection
          electNewObjector handlesVar
        points -> do
          let (middlePoint :: Header blk) = points !! (length points `div` 2)
          writeTVar nextJumpVar $ Just $! (castPoint (blockPoint middlePoint) :: Point blk)
          setJumpingState handlesVar peer $ Jumper nextJumpVar goodPoint (LookingForIntersection badPoint)

    -- | Select a slice of an anchored fragment between two points, inclusive.
    -- Both points must exist on the chain, in order, or the result is Nothing.
    -- REVIEW: How does this function behave if @fromExcl == toExcl@? How should
    -- it behave? It doesn't matter much because we never have this in our code,
    -- but still.
    sliceRangeExcl fragment fromExcl toExcl = do
      fragmentFrom <- snd <$> splitAfterPoint fragment fromExcl
      fst <$> splitBeforePoint fragmentFrom toExcl

getDynamo ::
  (MonadSTM m) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  STM m (Maybe peer)
getDynamo handlesVar =
  (fst <$>) . List.find (isDynamo . cschJumping . snd) . Map.toList <$> readTVar handlesVar
  where
    isDynamo (Dynamo _) = True
    isDynamo _          = False

getDynamoFragment ::
  (MonadSTM m, Ord peer) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  STM m (Maybe (AnchoredFragment (Header blk)))
getDynamoFragment handlesVar = do
  getDynamo handlesVar >>= \case
    Nothing -> pure Nothing
    Just dynamo -> do
      handles <- readTVar handlesVar
      Just . csCandidate <$> (readTVar $ cschState $ handles Map.! dynamo)

-- | If there is an objector, demote it back to being a jumper.
demoteObjector ::
  ( MonadSTM m,
    Ord peer,
    HasHeader blk
  ) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  STM m ()
demoteObjector handlesVar = do
  handles <- Map.toList <$> readTVar handlesVar
  case List.find (isObjector . cschJumping . snd) handles of
    Just (objector, ChainSyncClientHandle {cschJumping = Objector intersection}) ->
      setJumpingState handlesVar objector =<< newJumper intersection FoundIntersection
    _ -> pure ()
  where
    isObjector (Objector _) = True
    isObjector _            = False

newJumper ::
  ( MonadSTM m,
    HasHeader blk
  ) =>
  Point blk ->
  ChainSyncJumpingJumperState blk ->
  STM m (ChainSyncJumpingState m blk)
newJumper intersection state = do
  nextJumpVar <- newTVar Nothing
  pure $ Jumper nextJumpVar intersection state

-- | Register a new ChainSync client to the given map of handles. If there is no
-- dynamo, then it starts as a dynamo; otherwise, it starts as a jumper.
registerClient ::
  ( MonadSTM m,
    Ord peer,
    HasHeader blk
  ) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  peer ->
  -- | A function to make a client handle from a jumping state.
  (ChainSyncJumpingState m blk -> ChainSyncClientHandle m blk) ->
  STM m ()
registerClient handlesVar peer mkHandle = do
  cschJumping <-
    getDynamo handlesVar >>= \case
      Nothing -> pure $ Dynamo Origin
      Just _ -> newJumper GenesisPoint Happy
  modifyTVar handlesVar $ Map.insert peer $ mkHandle cschJumping

unregisterClient ::
  ( MonadSTM m,
    Ord peer,
    HasHeader blk
  ) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  peer ->
  STM m ()
unregisterClient handlesVar peer = do
  handle <- (Map.! peer) <$> readTVar handlesVar
  modifyTVar handlesVar $ Map.delete peer
  case cschJumping handle of
    Jumper _ _ _ -> pure ()
    Objector _   -> electNewObjector handlesVar
    Dynamo _     -> electNewDynamo handlesVar

-- | Look into all objector candidates and promote the one with the oldest
-- intersection with the dynamo as the new objector.
electNewObjector ::
  ( MonadSTM m,
    Ord peer,
    HasHeader blk
  ) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  STM m ()
electNewObjector handlesVar = do
  demoteObjector handlesVar
  -- Get all the jumpers that disagree with the dynamo and for which we know the
  -- precise intersection; find the one with the oldest intersection (which is
  -- very important) and promote it to objector.
  sortedObjectorCandidates <-
    List.sortOn (pointSlot . snd)
      . mapMaybe
        ( \(peer, handle) ->
            case cschJumping handle of
              Jumper _ intersection FoundIntersection -> Just (peer, intersection)
              _ -> Nothing
        )
      . Map.toList
      <$> readTVar handlesVar
  case sortedObjectorCandidates of
    [] -> pure ()
    (objector, intersection) : _ ->
      setJumpingState handlesVar objector $ Objector intersection

electNewDynamo ::
  ( MonadSTM m,
    Ord peer,
    HasHeader blk
  ) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  STM m ()
electNewDynamo handlesVar = do
  -- Get everybody; choose an unspecified new dynamo and put everyone else back
  -- to being jumpers. NOTE: This might stop an objector from syncing and send
  -- it back to being a jumper only for it to start again later, but nevermind.
  (Map.keys <$> readTVar handlesVar)
    >>= \case
      [] -> pure ()
      dynamo : jumpers -> do
        setJumpingState handlesVar dynamo $ Dynamo Origin
        forM_ jumpers $ \jumper ->
          setJumpingState handlesVar jumper =<< newJumper GenesisPoint Happy

-- | Size of jumps, in number of slots. FIXME: Make this configurable.
jumpSize :: SlotNo
jumpSize = 2