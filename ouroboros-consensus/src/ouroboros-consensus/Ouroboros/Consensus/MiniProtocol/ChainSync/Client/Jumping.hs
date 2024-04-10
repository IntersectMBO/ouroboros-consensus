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
    Context
  , ContextWith (..)
  , Instruction (..)
  , JumpResult (..)
  , makeContext
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

-- | A context for ChainSync jumping, pointing for some data.
--
-- Invariants:
--
-- - If 'handlesVar' is not empty, then there is exactly one dynamo in it.
-- - There is at most one objector in 'handlesVar'.
data ContextWith peerField handleField m peer blk = Context
  { peer       :: !peerField,
    handle     :: !handleField,
    handlesVar :: !(StrictTVar m (Map peer (ChainSyncClientHandle m blk))),
    jumpSize   :: !SlotNo
  }

-- | A non-specific, generic context for ChainSync jumping.
type Context = ContextWith () ()

-- | A peer-specific context for ChainSync jumping. This is a 'PointedContext'
-- pointing on the handler of the peer in question.
--
-- Invariant: The binding from 'peer' to 'handle' is present in 'handlesVar'.
type PeerContext m peer blk = ContextWith peer (ChainSyncClientHandle m blk) m peer blk

makeContext ::
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  SlotNo ->
  -- ^ The size of jumps, in number of slots.
  Context m peer blk
makeContext = Context () ()

-- | Get a generic context from a peer context by stripping away the
-- peer-specific fields.
stripContext :: PeerContext m peer blk -> Context m peer blk
stripContext context = context {peer = (), handle = ()}

-- | Instruction from the jumping governor, either to run normal ChainSync, or
-- to jump to follow the given peer with the given fragment.
data Instruction blk
  = RunNormally
  | JumpTo !(Point blk)
  deriving (Generic)

deriving instance (HasHeader blk, Eq (Header blk)) => Eq (Instruction blk)
deriving instance (HasHeader blk, Show (Header blk)) => Show (Instruction blk)
deriving anyclass instance (HasHeader blk, NoThunks (Header blk)) => NoThunks (Instruction blk)

-- | The result of a jump request, either accepted or rejected.
data JumpResult blk
  = AcceptedJump !(Point blk)
  | RejectedJump !(Point blk)
  deriving (Generic)

deriving instance (HasHeader blk, Eq (Header blk)) => Eq (JumpResult blk)
deriving instance (HasHeader blk, Show (Header blk)) => Show (JumpResult blk)
deriving anyclass instance (HasHeader blk, NoThunks (Header blk)) => NoThunks (JumpResult blk)

-- | Compute the next instruction for the given peer. In the majority of cases,
-- this consists in reading the peer's handle, having the dynamo and objector
-- run normally and the jumpers wait for the next jump. As such, this function
-- mostly only reads the handle of the peer. For the dynamo, every once in a
-- while, we need to indicate to the jumpers that they need to jump, and this
-- requires writing to all TVars.
nextInstruction ::
  ( MonadSTM m,
    HasHeader blk,
    HasHeader (Header blk)
  ) =>
  PeerContext m peer blk ->
  STM m (Instruction blk)
nextInstruction context =
  readTVar (cschJumping (handle context)) >>= \case
    Dynamo lastJumpSlot -> maybeSetNextJump lastJumpSlot >> pure RunNormally
    Objector _ -> pure RunNormally
    Jumper nextJumpVar _ _ ->
      readTVar nextJumpVar >>= \case
        Nothing -> retry
        Just fragment -> do
          writeTVar nextJumpVar Nothing
          pure $ JumpTo fragment
  where
    -- | We are the dynamo. When the tip of our candidate fragment is 'jumpSize'
    -- slots younger than the last jump, set happy jumpers to jump to it.
    maybeSetNextJump lastJumpSlot = do
      dynamoFragment <- csCandidate <$> readTVar (cschState (handle context))
      when (succWithOrigin (headSlot dynamoFragment) >= succWithOrigin lastJumpSlot + jumpSize context) $ do
        handles <- readTVar (handlesVar context)
        forM_ (Map.elems handles) $ \ChainSyncClientHandle{cschJumping = cschJumping'} ->
          readTVar cschJumping' >>= \case
            -- REVIEW: We are now proposing a jump to 'headPoint', which is the
            -- first block _after_ (including) @lastJumpSlot + jumpSize@. We
            -- might want to propose the jump to the last block _before_ that
            -- point.
            Jumper nextJumpVar _ Happy -> writeTVar nextJumpVar $ Just $! castPoint (headPoint dynamoFragment)
            _ -> pure ()
        writeTVar (cschJumping (handle context)) $ Dynamo (headSlot dynamoFragment)

-- | Process the result of a jump. In the happy case, this only consists in
-- updating the peer's handle to take the new candidate fragment and the new
-- last jump point into account. When disagreeing with the dynamo, though, we
-- enter a phase of several jumps to pinpoint exactly where the disagreement
-- occurs. Once this phase is finished, we trigger the election of a new
-- objector, which might update many TVars.
processJumpResult ::
  ( MonadSTM m,
    HasHeader blk, HasHeader (Header blk)
  ) =>
  PeerContext m peer blk ->
  JumpResult blk ->
  STM m ()
processJumpResult context jumpResult = do
  readTVar (cschJumping (handle context)) >>= \case
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
            dynamoFragment <- csCandidate <$> (readTVar . cschState =<< getDynamo' context)
            let slicedDynamoFragment = fst $ fromJust $ splitAfterPoint dynamoFragment goodPoint'
            modifyTVar (cschState (handle context)) $ \csState -> csState {csCandidate = slicedDynamoFragment}
            writeTVar (cschJumping (handle context)) $ Jumper nextJumpVar goodPoint' Happy
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
    -- | Given a good point (where we know we agree with the dynamo) and a bad
    -- point (where we know we disagree with the dynamo), either decide that we
    -- know the intersection for sure (if the bad point is the successor of the
    -- good point) or program a jump somewhere in the middle to refine those
    -- points.
    lookForIntersection nextJumpVar goodPoint badPoint = do
      (dynamoFragment0 :: AnchoredFragment (Header blk)) <- csCandidate <$> (readTVar . cschState =<< getDynamo' context) -- full fragment
      let dynamoFragment = fromJust $ sliceRangeExcl dynamoFragment0 goodPoint badPoint
      case (toOldestFirst dynamoFragment :: [Header blk]) of
        [] -> do
          writeTVar (cschJumping (handle context)) $ Jumper nextJumpVar goodPoint FoundIntersection
          electNewObjector (stripContext context)
        points -> do
          let (middlePoint :: Header blk) = points !! (length points `div` 2)
          writeTVar nextJumpVar $ Just $! (castPoint (blockPoint middlePoint) :: Point blk)
          writeTVar (cschJumping (handle context)) $ Jumper nextJumpVar goodPoint (LookingForIntersection badPoint)

    -- | Select a slice of an anchored fragment between two points, inclusive.
    -- Both points must exist on the chain, in order, or the result is Nothing.
    -- REVIEW: How does this function behave if @fromExcl == toExcl@? How should
    -- it behave? It doesn't matter much because we never have this in our code,
    -- but still.
    sliceRangeExcl fragment fromExcl toExcl = do
      fragmentFrom <- snd <$> splitAfterPoint fragment fromExcl
      fst <$> splitBeforePoint fragmentFrom toExcl

-- | Find the dynamo in a TVar containing a map of handles. Returns then handle
-- of the dynamo, or 'Nothing' if there is none.
getDynamo ::
  (MonadSTM m) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  STM m (Maybe (ChainSyncClientHandle m blk))
getDynamo handlesVar = do
  handles <- Map.toList <$> readTVar handlesVar
  handlesWithState <- forM handles (\(_, handle) -> (handle,) <$> readTVar (cschJumping handle))
  pure $ (fst <$>) $ List.find (isDynamo . snd) $ handlesWithState
  where
    isDynamo (Dynamo _) = True
    isDynamo _          = False

-- | Find the dynamo in a 'PeerContext'. Because we know that at least one peer
-- exists, our invariants guarantee that there will be a dynamo.
getDynamo' ::
  (MonadSTM m) =>
  PeerContext m peer blk ->
  STM m (ChainSyncClientHandle m blk)
getDynamo' context =
  getDynamo (handlesVar context) >>= \case
    Nothing -> error "getDynamo': invariant violation"
    Just dynamo -> pure dynamo

-- | If there is an objector, demote it back to being a jumper.
demoteObjector ::
  ( MonadSTM m,
    HasHeader blk
  ) =>
  Context m peer blk ->
  STM m ()
demoteObjector context = do
  handles <- Map.toList <$> readTVar (handlesVar context)
  handlesWithState <- forM handles (\(_, handle) -> (handle,) <$> readTVar (cschJumping handle))
  case List.find (isObjector . snd) handlesWithState of
    Just (handle, Objector intersection) ->
      writeTVar (cschJumping handle) =<< newJumper intersection FoundIntersection
    _ -> pure ()
  where
    isObjector (Objector _) = True
    isObjector _            = False

-- | Convenience function that, given an intersection point and a jumper state,
-- make a fresh 'Jumper' constructor.
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

-- | Register a new ChainSync client to a context, returning a 'PeerContext' for
-- that peer. If there is no dynamo, the peer starts as dynamo; otherwise, it
-- starts as a jumper.
registerClient ::
  ( Ord peer,
    HasHeader blk,
    IOLike m,
    NoThunks (Header blk)
  ) =>
  Context m peer blk ->
  peer ->
  -- | A function to make a client handle from a jumping state.
  (StrictTVar m (ChainSyncJumpingState m blk) -> ChainSyncClientHandle m blk) ->
  STM m (PeerContext m peer blk)
registerClient context peer mkHandle = do
  cschJumping <-
    newTVar
      =<< ( getDynamo (handlesVar context) >>= \case
              Nothing -> pure $ Dynamo Origin
              Just _ -> newJumper GenesisPoint Happy
          )
  let handle = mkHandle cschJumping
  modifyTVar (handlesVar context) $ Map.insert peer handle
  pure $ context {peer, handle}

-- | Unregister a client from a 'PeerContext'; this might trigger the election
-- of a new dynamo or objector if the peer was one of these two.
unregisterClient ::
  ( MonadSTM m,
    Ord peer,
    HasHeader blk
  ) =>
  PeerContext m peer blk ->
  STM m ()
unregisterClient context = do
  modifyTVar (handlesVar context) $ Map.delete (peer context)
  let context' = stripContext context
  readTVar (cschJumping (handle context)) >>= \case
    Jumper _ _ _ -> pure ()
    Objector _ -> electNewObjector context'
    Dynamo _ -> electNewDynamo context'

-- | Look into all objector candidates and promote the one with the oldest
-- intersection with the dynamo as the new objector.
electNewObjector ::
  ( MonadSTM m,
    HasHeader blk
  ) =>
  Context m peer blk ->
  STM m ()
electNewObjector context = do
  demoteObjector context
  -- Get all the jumpers that disagree with the dynamo and for which we know the
  -- precise intersection; find the one with the oldest intersection (which is
  -- very important) and promote it to objector.
  handles <- Map.toList <$> readTVar (handlesVar context)
  handlesWithState <- forM handles (\(_, handle) -> (handle,) <$> readTVar (cschJumping handle))
  let sortedObjectorCandidates =
        List.sortOn (pointSlot . snd) $
          mapMaybe
            ( \(handle, state) ->
                case state of
                  Jumper _ intersection FoundIntersection -> Just (handle, intersection)
                  _ -> Nothing
            )
            handlesWithState
  case sortedObjectorCandidates of
    [] -> pure ()
    (handle, intersection) : _ ->
      writeTVar (cschJumping handle) $ Objector intersection

-- | Get everybody; choose an unspecified new dynamo and put everyone else back
-- to being fresh, happy jumpers.
--
-- NOTE: This might stop an objector from syncing and send it back to being a
-- jumper only for it to start again later, but nevermind.
electNewDynamo ::
  ( MonadSTM m,
    HasHeader blk
  ) =>
  Context m peer blk ->
  STM m ()
electNewDynamo context = do
  (Map.elems <$> readTVar (handlesVar context)) >>= \case
    [] -> pure ()
    dynamo : jumpers -> do
      writeTVar (cschJumping dynamo) $ Dynamo Origin
      forM_ jumpers $ \jumper ->
        writeTVar (cschJumping jumper) =<< newJumper GenesisPoint Happy
