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

module Ouroboros.Consensus.MiniProtocol.ChainSync.Client.JumpingGovernor (
    ClientState
  , Handle
  , Instruction (..)
  , JumpResult (..)
  , dummyHandle
  , newHandle
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
                     WithOrigin (..), castPoint, succWithOrigin)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State
                     (ChainSyncClientHandle (..), ChainSyncState (..))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     blockPoint, headPoint, headSlot, splitAfterPoint,
                     splitBeforePoint, toOldestFirst)

data Handle m peer blk = Handle
  { clientHandles :: !(StrictTVar m (Map peer (ChainSyncClientHandle m blk))),
    clientStates  :: !(StrictTVar m (Map peer (ClientState m blk)))
  }

data ClientState m blk
  = Dynamo
      -- | The last slot at which we triggered jumps for the jumpers.
      !(WithOrigin SlotNo)
  | Objector
      -- | The last known point where the objector agrees with the dynamo.
      !(Point blk)
  | Jumper
      -- | The next jump to be executed.
      !(StrictTVar m (Maybe (Point blk)))
      -- | The result of the last jump.
      !(Point blk)
      -- | More precisely, the state of the jumper.
      !(JumperState blk)
  deriving (Generic)

deriving anyclass instance (IOLike m, HasHeader blk, NoThunks (Header blk)) => NoThunks (ClientState m blk)

data JumperState blk
  = -- | The jumper is happy with the dynamo, at least as far as the point in
    -- the 'Jumper' constructor is concerned.
    Happy
  | -- | The jumper disagrees with the dynamo and we are searching where exactly
    -- that happens. All we know is a point where the jumper agrees with the
    -- dynamo (in the 'Jumper' constructor) and a point where the jumper
    -- disagrees with the dynamo, carried by this constructor.
    LookingForIntersection !(Point blk)
  | -- | The jumper disagrees with the dynamo and we have found where exactly.
    -- The last point where the jumper agrees with the dynamo is stored in the
    -- ClientState.
    FoundIntersection
  deriving (Generic)

deriving anyclass instance (HasHeader blk, NoThunks (Header blk)) => NoThunks (JumperState blk)

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
  Handle m peer blk ->
  peer ->
  STM m (Instruction blk)
nextInstruction governor@Handle {clientStates} peer =
  ((Map.! peer) <$> readTVar clientStates) >>= \case
    Dynamo lastJumpSlot -> maybeJump lastJumpSlot >> pure RunNormally
    Objector _ -> pure RunNormally
    Jumper nextJumpVar _ _ ->
      readTVar nextJumpVar >>= \case
        Nothing -> retry
        Just fragment -> do
          writeTVar nextJumpVar Nothing
          pure $ JumpTo fragment
  where
    maybeJump lastJumpSlot = do
      dynamoFragment <- fromJust <$> getDynamoFragment governor
      when (succWithOrigin (headSlot dynamoFragment) >= succWithOrigin lastJumpSlot + jumpSize) $ do
        clientStates' <- readTVar clientStates
        forM_ (Map.elems clientStates') $ \case
          Jumper nextJumpVar _ Happy -> writeTVar nextJumpVar $ Just $! castPoint (headPoint dynamoFragment)
          _ -> pure ()

processJumpResult ::
  ( MonadSTM m,
    Ord peer, HasHeader blk, HasHeader (Header blk)
  ) =>
  Handle m peer blk ->
  peer ->
  JumpResult blk ->
  STM m ()
processJumpResult governor@Handle {clientStates, clientHandles} peer jumpResult =
  ((Map.! peer) <$> readTVar clientStates) >>= \case
    Dynamo _ -> pure ()
    Objector _ -> pure ()
    Jumper nextJumpVar goodPoint state ->
        case (state, jumpResult) of
          (FoundIntersection, _) ->
            -- Ignore jump results when we already found the intersection.
            pure ()
          (Happy, AcceptedJump goodPoint') -> do
            -- The jump was accepted; we bump the peer's candidate fragment to
            -- the dynamo's candidate fragment up to the accepted point.
            dynamoFragment <- fromJust <$> getDynamoFragment governor
            let slicedDynamoFragment = fst $ fromJust $ splitAfterPoint dynamoFragment goodPoint'
            chainSyncStateVar <- cschState . (Map.! peer) <$> readTVar clientHandles
            modifyTVar chainSyncStateVar $ \chainSyncState -> chainSyncState {csCandidate = slicedDynamoFragment}
            modifyTVar clientStates . Map.insert peer $ Jumper nextJumpVar goodPoint' Happy
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
      (dynamoFragment0 :: AnchoredFragment (Header blk)) <- fromJust <$> getDynamoFragment governor -- full fragment
      let (dynamoFragment1 :: AnchoredFragment (Header blk)) = snd $ fromJust $ splitAfterPoint dynamoFragment0 goodPoint -- fragment from goodPoint (excl)
      let (dynamoFragment2 :: AnchoredFragment (Header blk)) = fst $ fromJust $ splitBeforePoint dynamoFragment1 badPoint -- fragment from goodPoint to badPoint (excl)
      case (toOldestFirst dynamoFragment2 :: [Header blk]) of
        [] -> do
          modifyTVar clientStates . Map.insert peer $ Jumper nextJumpVar goodPoint FoundIntersection
          electNewObjector governor
        points -> do
          let (middlePoint :: Header blk) = points !! (length points `div` 2)
          writeTVar nextJumpVar $ Just $! (castPoint (blockPoint middlePoint) :: Point blk)
          modifyTVar clientStates . Map.insert peer $ Jumper nextJumpVar goodPoint $ LookingForIntersection badPoint

getDynamo ::
  (MonadSTM m) =>
  Handle m peer blk ->
  STM m (Maybe peer)
getDynamo Handle {clientStates} =
  (fst <$>) . List.find (\(_, clientState) -> case clientState of Dynamo _ -> True; _ -> False) . Map.toList <$> readTVar clientStates

getDynamoFragment ::
  (MonadSTM m, Ord peer) =>
  Handle m peer blk ->
  STM m (Maybe (AnchoredFragment (Header blk)))
getDynamoFragment governor@Handle {clientHandles} = do
  maybeDynamo <- getDynamo governor
  case maybeDynamo of
    Nothing -> pure Nothing
    Just dynamo -> do
      clientHandles' <- readTVar clientHandles
      Just . csCandidate <$> (readTVar $ cschState $ clientHandles' Map.! dynamo)

-- | If there is an objector, demote it back to being a jumper.
demoteObjector ::
  ( MonadSTM m,
    Ord peer,
    HasHeader blk
  ) =>
  Handle m peer blk ->
  STM m ()
demoteObjector Handle {clientStates} = do
  clientStates' <- Map.toList <$> readTVar clientStates
  forM_ clientStates' $ \case
    (peer, Objector intersection) -> modifyTVar clientStates . Map.insert peer =<< newJumper intersection FoundIntersection
    _ -> pure ()

newJumper ::
  ( MonadSTM m,
    HasHeader blk
  ) =>
  Point blk ->
  JumperState blk ->
  STM m (ClientState m blk)
newJumper intersection state = do
  nextJumpVar <- newTVar Nothing
  pure $ Jumper nextJumpVar intersection state

registerClient ::
  ( MonadSTM m,
    Ord peer,
    HasHeader blk
  ) =>
  Handle m peer blk ->
  peer ->
  STM m ()
registerClient governor@Handle {clientStates} peer = do
  maybeDynamo <- getDynamo governor
  clientState <-
    if maybeDynamo == Nothing
      then pure $ Dynamo Origin
      else newJumper GenesisPoint Happy
  modifyTVar clientStates $ Map.insert peer clientState

unregisterClient ::
  ( MonadSTM m,
    Ord peer,
    HasHeader blk
  ) =>
  Handle m peer blk ->
  peer ->
  STM m ()
unregisterClient governor@Handle {clientStates} peer = do
  clientState <- (Map.! peer) <$> readTVar clientStates
  modifyTVar clientStates $ Map.delete peer
  case clientState of
    Jumper _ _ _ -> pure ()
    Objector _   -> electNewObjector governor
    Dynamo _     -> electNewDynamo governor

-- | REVIEW: This must exist somewhere else.
pointSlot :: Point blk -> WithOrigin SlotNo
pointSlot = \case
  GenesisPoint -> Origin
  BlockPoint slot _ -> NotOrigin slot

-- | Look into all objector candidates and promote the one with the oldest
-- intersection with the dynamo as the new objector.
electNewObjector ::
  ( MonadSTM m,
    Ord peer,
    HasHeader blk
  ) =>
  Handle m peer blk ->
  STM m ()
electNewObjector governor@Handle {clientStates} = do
  demoteObjector governor
  -- Get all the jumpers that disagree with the dynamo and for which we know the
  -- precise intersection; find the one with the oldest intersection (which is
  -- very important) and promote it to objector.
  sortedObjectorCandidates <-
    List.sortOn (pointSlot . snd)
      . mapMaybe
        ( \(peer, clientState) ->
            case clientState of
              Jumper _ intersection FoundIntersection -> Just (peer, intersection)
              _ -> Nothing
        )
      . Map.toList
      <$> readTVar clientStates
  case sortedObjectorCandidates of
    [] -> pure ()
    (objector, intersection) : _ -> modifyTVar clientStates $ Map.insert objector $ Objector intersection

electNewDynamo ::
  ( MonadSTM m,
    Ord peer,
    HasHeader blk
  ) =>
  Handle m peer blk ->
  STM m ()
electNewDynamo Handle {clientStates} = do
  -- Get everybody; choose a random new dynamo and put everyone else back to
  -- being jumpers. NOTE: This might stop an objector from syncing and send it
  -- back to being a jumper only for it to start again later, but nevermind.
  (Map.keys <$> readTVar clientStates)
    >>= \case
      [] -> pure ()
      dynamo : jumpers -> do
        modifyTVar clientStates $ Map.insert dynamo $ Dynamo Origin
        forM_ jumpers $ \jumper ->
          modifyTVar clientStates . Map.insert jumper =<< newJumper GenesisPoint Happy

newHandle ::
  ( IOLike m,
    HasHeader blk,
    Ord peer,
    NoThunks peer,
    NoThunks (Header blk)
  ) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  m (Handle m peer blk)
newHandle clientHandles = do
  clientStates <- newTVarIO mempty
  pure Handle {clientHandles, clientStates}

dummyHandle ::
  ( IOLike m,
    HasHeader blk,
    Ord peer,
    NoThunks peer,
    NoThunks (Header blk)
  ) =>
  m (Handle m peer blk)
dummyHandle = newHandle =<< newTVarIO mempty
-- ^ FIXME: Does not work. The handle should be a record of endpoints (eg.
-- nextInstruction, notifyJumpResult, run, etc.). As it is now, this dummy
-- probably causes an exception in the run function.

-- | Size of jumps, in number of slots. FIXME: Make this configurable.
jumpSize :: SlotNo
jumpSize = 2
