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
  , onRollForward
  , onAwaitReply
  ) where

import           Cardano.Slotting.Slot (SlotNo(..), WithOrigin (..))
import           Control.Monad (forM, forM_, when)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block (HasHeader, Header, Point (..),
                     blockSlot, castPoint, pointSlot, succWithOrigin,
                     GenesisWindow (unGenesisWindow))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State
                     (ChainSyncClientHandle (..),
                     ChainSyncJumpingState (..), ChainSyncState (..))
import           Ouroboros.Consensus.Util.IOLike hiding (handle)
import           Ouroboros.Network.AnchoredFragment (
                     headPoint, headSlot, splitAfterPoint)

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
    jumpSize   :: !SlotNo,
    genesisWindow :: !GenesisWindow
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
  GenesisWindow ->
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
    Jumper nextJumpVar ->
      readTVar nextJumpVar >>= \case
        Nothing -> retry
        Just fragment -> do
          writeTVar nextJumpVar Nothing
          pure $ JumpTo fragment
  where
    -- | We are the dynamo. When the tip of our candidate fragment is 'jumpSize'
    -- slots younger than the last jump, set jumpers to jump to it.
    maybeSetNextJump lastJumpSlot = do
      dynamoFragment <- csCandidate <$> readTVar (cschState (handle context))
      when (succWithOrigin (headSlot dynamoFragment) >= succWithOrigin lastJumpSlot + jumpSize context) $ do
        handles <- readTVar (handlesVar context)
        forM_ (Map.elems handles) $ \ChainSyncClientHandle{cschJumping = cschJumping'} ->
          readTVar cschJumping' >>= \case
            Jumper nextJumpVar -> writeTVar nextJumpVar $ Just $! castPoint (headPoint dynamoFragment)
            _ -> pure ()
        writeTVar (cschJumping (handle context)) $ Dynamo (headSlot dynamoFragment)

data ChainSyncJumpingExceptions
   = -- | The CSJ dynamo offers an empty genesis window in the chain it is serving.
     DynamoOffersEmptyGenesisWindow
   | -- | The CSJ dynamo offers an empty genesis window in the chain it is serving.
     ObjectorRolledBackAsJumper
  deriving (Exception, Show)

-- | This function is called when we receive a 'MsgRollForward' message.
--
-- When a dynamo rolls forward, we might need to jump to the candidate
-- fragment's tip to advance the jumpers if the dynamo sent all the headers
-- in the genesis window since the last jump. e.g.
--
-- > -- last jump -- h0 -- h1 -- ... -- tip -- genesis window limit -- next header
--
-- This is necessary when the tip is before the next jump according to the jump
-- size.
--
onRollForward ::
  ( MonadSTM m,
    MonadThrow (STM m),
    HasHeader (Header blk)
  ) =>
  PeerContext m peer blk ->
  Header blk ->
  STM m ()
onRollForward context hdr =
  readTVar (cschJumping (handle context)) >>= \case
    Objector badPoint
      | blockSlot hdr /= pointSlot badPoint -> pure ()
      | blockSlot hdr >= currentSlot - genesisWindowSlot -> pure ()
      | headerHash hdr /= pointHash badPoint -> throwSTM ObjectorRolledBackAsJumper
      | otherwise -> pure ()
    Jumper{} -> pure ()
    Dynamo lastJumpSlot
      | blockSlot hdr >= succWithOrigin lastJumpSlot + genesisWindowSlot -> do
          csTipPoint <- headPoint . csCandidate <$> readTVar (cschState (handle context))
          if pointSlot csTipPoint > lastJumpSlot
            then setJumps (castPoint csTipPoint)
            else throwSTM DynamoOffersEmptyGenesisWindow
      | otherwise -> pure ()
  where
    genesisWindowSlot = SlotNo (unGenesisWindow (genesisWindow context))
    setJumps point = do
        handles <- readTVar (handlesVar context)
        forM_ (Map.elems handles) $ \h ->
          readTVar (cschJumping h) >>= \case
            Jumper nextJumpVar -> writeTVar nextJumpVar $ Just $! point
            _ -> pure ()

-- | This function is called when we receive a 'MsgAwaitReply' message.
--
-- If this is the dynamo, we need to elect a new dynamo as no more headers
-- are available.
onAwaitReply ::
  ( MonadSTM m,
    HasHeader blk,
    Eq peer
  ) =>
  PeerContext m peer blk ->
  STM m ()
onAwaitReply context =
  readTVar (cschJumping (handle context)) >>= \case
    Objector{} -> pure ()
    Jumper{} -> pure ()
    Dynamo{} -> electNewDynamo (stripContext context)

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
    Jumper nextJumpVar ->
        case jumpResult of
          AcceptedJump goodPoint' -> do
            -- The jump was accepted; we set the jumper's candidate fragment to
            -- the dynamo's candidate fragment up to the accepted point.
            --
            -- The candidate fragments of jumpers don't grow otherwise, as only the
            -- objector and the dynamo request further headers.
            dynamoFragment <- csCandidate <$> (readTVar . cschState =<< getDynamo' context)
            let slicedDynamoFragment = fst $ fromJust $ splitAfterPoint dynamoFragment goodPoint'
            modifyTVar (cschState (handle context)) $ \csState -> csState {csCandidate = slicedDynamoFragment}
            writeTVar (cschJumping (handle context)) $ Jumper nextJumpVar
          RejectedJump badPoint ->
            writeTVar (cschJumping (handle context)) $ Objector badPoint

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

-- | Convenience function that, given an intersection point and a jumper state,
-- make a fresh 'Jumper' constructor.
newJumper ::
  ( MonadSTM m,
    HasHeader blk
  ) =>
  STM m (ChainSyncJumpingState m blk)
newJumper = do
  nextJumpVar <- newTVar Nothing
  pure $ Jumper nextJumpVar

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
              Just _ -> newJumper
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
    Jumper{} -> pure ()
    Objector{} -> pure ()
    Dynamo _ -> electNewDynamo context'

-- | Choose an unspecified new non-idling dynamo and demote all other peers to
-- jumpers.
electNewDynamo ::
  ( MonadSTM m,
    HasHeader blk,
    Eq peer
  ) =>
  Context m peer blk ->
  STM m ()
electNewDynamo context = do
  peerStates <- Map.toList <$> readTVar (handlesVar context)
  mDynamo <- findNonIdling peerStates
  case mDynamo of
    Nothing -> pure () -- TODO: turn off CSJ
    Just (dynId, dynamo) -> do
      writeTVar (cschJumping dynamo) $ Dynamo Origin
      forM_ peerStates $ \(peer, st) ->
        when (peer /= dynId) $
          writeTVar (cschJumping st) =<< newJumper
  where
    findNonIdling [] = pure Nothing
    findNonIdling ((peer, st) : rest) = do
      idling <- csIdling <$> readTVar (cschState st)
      if idling
        then findNonIdling rest
        else pure $ Just (peer, st)
