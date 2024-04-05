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
{-# LANGUAGE TupleSections        #-}
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
import           Control.Monad (forM, forM_, when)
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
    HasHeader blk,
    HasHeader (Header blk)
  ) =>
  -- | A TVar to the state of the peer in question. Most of the time, this is
  -- sufficient to compute the next instruction.
  StrictTVar m (ChainSyncState m blk) ->
  -- | A TVar containing all the handles of all the peers. It is necessary but
  -- used only in very specific situations.
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  STM m (Instruction blk)
nextInstruction stateVar handlesVar =
  (csJumping <$> readTVar stateVar) >>= \case
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
        forM_ (Map.elems handles) $ \handle' ->
          (csJumping <$> readTVar (cschState handle')) >>= \case
            Jumper nextJumpVar _ Happy -> writeTVar nextJumpVar $ Just $! castPoint (headPoint dynamoFragment)
            _ -> pure ()

setJumpingState ::
  (MonadSTM m) =>
  StrictTVar m (ChainSyncState m blk) ->
  ChainSyncJumpingState m blk ->
  STM m ()
setJumpingState stateVar csJumping =
  modifyTVar stateVar $ \state -> state {csJumping}

processJumpResult ::
  ( MonadSTM m,
    HasHeader blk, HasHeader (Header blk)
  ) =>
  -- | A TVar to the state of the peer in question. Most of the time, this is
  -- sufficient to process a jump result.
  StrictTVar m (ChainSyncState m blk) ->
  -- | A TVar containing all the handles of all the peers. It is necessary but
  -- used only in very specific situations.
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  JumpResult blk ->
  STM m ()
processJumpResult stateVar handlesVar jumpResult = do
  (csJumping <$> readTVar stateVar) >>= \case
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
            modifyTVar stateVar $ \csState -> csState {csCandidate = slicedDynamoFragment}
            setJumpingState stateVar $ Jumper nextJumpVar goodPoint' Happy
          (Happy, RejectedJump badPoint) ->
            -- The previously happy peer just rejected a jump. We look for an
            -- intersection.
            lookForIntersection stateVar nextJumpVar goodPoint badPoint
          (LookingForIntersection badPoint, AcceptedJump goodPoint') ->
            -- The peer that was looking for an intersection accepts the given
            -- point, helping us narrow things down.
            lookForIntersection stateVar nextJumpVar goodPoint' badPoint
          (LookingForIntersection _, RejectedJump badPoint') ->
            -- The peer that was looking for an intersection rejects the given
            -- point, helping us narrow things down.
            lookForIntersection stateVar nextJumpVar goodPoint badPoint'
  where
    lookForIntersection handle nextJumpVar goodPoint badPoint = do
      (dynamoFragment0 :: AnchoredFragment (Header blk)) <- fromJust <$> getDynamoFragment handlesVar -- full fragment
      let dynamoFragment = fromJust $ sliceRangeExcl dynamoFragment0 goodPoint badPoint
      case (toOldestFirst dynamoFragment :: [Header blk]) of
        [] -> do
          setJumpingState handle $ Jumper nextJumpVar goodPoint FoundIntersection
          electNewObjector handlesVar
        points -> do
          let (middlePoint :: Header blk) = points !! (length points `div` 2)
          writeTVar nextJumpVar $ Just $! (castPoint (blockPoint middlePoint) :: Point blk)
          setJumpingState handle $ Jumper nextJumpVar goodPoint (LookingForIntersection badPoint)

    -- | Select a slice of an anchored fragment between two points, inclusive.
    -- Both points must exist on the chain, in order, or the result is Nothing.
    -- REVIEW: How does this function behave if @fromExcl == toExcl@? How should
    -- it behave? It doesn't matter much because we never have this in our code,
    -- but still.
    sliceRangeExcl fragment fromExcl toExcl = do
      fragmentFrom <- snd <$> splitAfterPoint fragment fromExcl
      fst <$> splitBeforePoint fragmentFrom toExcl

-- | Finds the dynamo and returns everything that we know about it.
getDynamo ::
  (MonadSTM m) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  STM m (Maybe (peer, ChainSyncClientHandle m blk, ChainSyncState m blk))
getDynamo handlesVar = do
  handles <- Map.toList <$> readTVar handlesVar
  handlesWithState <- forM handles (\(peer, handle) -> (peer,handle,) <$> readTVar (cschState handle))
  pure $ List.find (isDynamo . csJumping . thd3) $ handlesWithState
  where
    thd3 :: (a, b, c) -> c
    thd3 (_, _, z) = z
    isDynamo (Dynamo _) = True
    isDynamo _          = False

getDynamoFragment ::
  (MonadSTM m) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  STM m (Maybe (AnchoredFragment (Header blk)))
getDynamoFragment handlesVar =
  ((csCandidate . thd3) <$>) <$> getDynamo handlesVar
  where
    thd3 :: (a, b, c) -> c
    thd3 (_, _, z) = z

-- | If there is an objector, demote it back to being a jumper.
demoteObjector ::
  ( MonadSTM m,
    HasHeader blk
  ) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  STM m ()
demoteObjector handlesVar = do
  handles <- Map.toList <$> readTVar handlesVar
  handlesWithState <- forM handles (\(peer, handle) -> (peer,handle,) <$> readTVar (cschState handle))
  case List.find (isObjector . csJumping . thd3) handlesWithState of
    Just (_, handle, ChainSyncState {csJumping = Objector intersection}) ->
      setJumpingState (cschState handle) =<< newJumper intersection FoundIntersection
    _ -> pure ()
  where
    thd3 :: (a, b, c) -> c
    thd3 (_, _, z) = z
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
  (ChainSyncJumpingState m blk -> STM m (ChainSyncClientHandle m blk)) ->
  STM m (ChainSyncClientHandle m blk)
registerClient handlesVar peer mkHandle = do
  csJumping <-
    getDynamo handlesVar >>= \case
      Nothing -> pure $ Dynamo Origin
      Just _ -> newJumper GenesisPoint Happy
  handle <- mkHandle csJumping
  modifyTVar handlesVar $ Map.insert peer $ handle
  pure handle

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
  (csJumping <$> readTVar (cschState handle)) >>= \case
    Jumper _ _ _ -> pure ()
    Objector _ -> electNewObjector handlesVar
    Dynamo _ -> electNewDynamo handlesVar

-- | Look into all objector candidates and promote the one with the oldest
-- intersection with the dynamo as the new objector.
electNewObjector ::
  ( MonadSTM m,
    HasHeader blk
  ) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  STM m ()
electNewObjector handlesVar = do
  demoteObjector handlesVar
  -- Get all the jumpers that disagree with the dynamo and for which we know the
  -- precise intersection; find the one with the oldest intersection (which is
  -- very important) and promote it to objector.
  handles <- Map.toList <$> readTVar handlesVar
  handlesWithState <- forM handles (\(peer, handle) -> (peer,handle,) <$> readTVar (cschState handle))
  let sortedObjectorCandidates =
        List.sortOn (pointSlot . snd) $
          mapMaybe
            ( \(_, handle, state) ->
                case csJumping state of
                  Jumper _ intersection FoundIntersection -> Just (handle, intersection)
                  _ -> Nothing
            )
            handlesWithState
  case sortedObjectorCandidates of
    [] -> pure ()
    (handle, intersection) : _ ->
      setJumpingState (cschState handle) $ Objector intersection

electNewDynamo ::
  ( MonadSTM m,
    HasHeader blk
  ) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  STM m ()
electNewDynamo handlesVar = do
  -- Get everybody; choose an unspecified new dynamo and put everyone else back
  -- to being jumpers. NOTE: This might stop an objector from syncing and send
  -- it back to being a jumper only for it to start again later, but nevermind.
  (map cschState . Map.elems <$> readTVar handlesVar) >>= \case
    [] -> pure ()
    dynamo : jumpers -> do
      setJumpingState dynamo $ Dynamo Origin
      forM_ jumpers $ \jumper ->
        setJumpingState jumper =<< newJumper GenesisPoint Happy

-- | Size of jumps, in number of slots. FIXME: Make this configurable.
jumpSize :: SlotNo
jumpSize = 2
