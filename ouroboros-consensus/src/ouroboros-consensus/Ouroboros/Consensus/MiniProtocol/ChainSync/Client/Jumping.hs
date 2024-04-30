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
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

-- | ChainSync jumping (CSJ) is an optimization for the ChainSync protocol that
-- allows nodes to sync without downloading headers from all of the honest
-- peers. This load is undesirable as it slows down all the peers involved.
--
-- The idea is to download the headers of a chain from a single peer (the
-- dynamo) and then ask periodically to the other peers (the jumpers) whether
-- they agree with the dynamo's chain.
--
-- When the jumpers disagree with the dynamo, the jumper with the oldest
-- intersection is asked to compete with the dynamo in the GDD logic. If the
-- dynamo is disconnected, a new dynamo is elected and the objector is demoted
-- to a jumper.
--
-- If the objector is disconnected, the syncing process continues with the
-- dynamo and the remaining jumpers.
--
-- The main property of the algorithm is that it never
-- downloads headers from more than two plausibly honest peers at a time (a
-- dynamo and an objector). All other peers are either waiting their turn to
-- compete with the dynamo, or are in agreement with it, or are disengaged
-- (see next section).
--
-- The algorithm might still download headers redundantly from peers that do
-- historical rollbacks. These rollbacks, however, constitute dishonest
-- behavior, and CSJ does not concern itself with avoiding load to dishonest
-- peers. Avoiding the load induced by dishonest peers on the syncing node would
-- require additionally to disconnect peers that do historical rollbacks. This
-- is not done by CSJ.
--
-- Interactions with the Genesis Density Disconnection logic
-- ---------------------------------------------------------
--
-- It is possible that neither the dynamo nor the objector are disconnected.
-- This could happen if:
-- 1. They both serve the same chain, or
-- 2. They both claim to have no more headers.
--
-- To avoid (1) CSJ checks that the objector disagrees with the dynamo at the
-- point it claimed to disagree as a jumper. If the objector agrees with the
-- dynamo, it is disengaged. A disengaged peer is not asked to jump or act as
-- dynamo or objector. Instead, it continues to offer headers for the rest of
-- the syncing. When the objector is disengaged, a new objector is elected
-- among the dissenting jumpers. If there are no dissenting jumpers left, the
-- syncing continues with the dynamo and the remaining jumpers.
--
-- To prevent the dynamo from agreeing with the objector instead, the dynamo is
-- not allowed to rollback before the last jump it requested. If the dynamo
-- tries to rollback before the last jump, it is disengaged and a new dynamo is
-- elected.
--
-- To avoid (2) CSJ disengages a peer as soon as it claims to have no more
-- headers. Syncing continues with a new elected dynamo or objector depending on
-- the disengaged peer's role.
--
-- CSJ finishes and is turned off when all peers have been disengaged.
--
-- Interactions with the ChainSync client
-- --------------------------------------
--
-- The ChainSync client interacts with CSJ through some callbacks that determine
-- when the client should pause, download headers, or ask about agreement with
-- a given point (jumping). See the 'Jumping' type for more details.
--
-- Interactions with the Limit on Patience
-- ---------------------------------------
--
-- Jumpers don't leak the Limit on Patience (LoP) bucket until they are promoted
-- to dynamos or objectors. And the leaking is stopped as soon as they are
-- disengaged or demoted.
--
-- If a jumper refrains from answering to jumps, they will be disconnected with
-- the 'intersectTimeout' (in 'ChainSyncTimeout').
--
-- A jumper answering just before the timeout will not delay the syncing
-- process by a large amount. If they agree with the dynamo, the dynamo will be
-- busy downloading headers and validating blocks while the jumper answers. If
-- the jumper disagrees with the dynamo, CSJ will look for the precise
-- intersection with the dynamo's chain. This could take a few minutes, but it
-- is a path that will end up in one of the dynamo and the jumper being
-- disconnected or disengaged.
--
--
-- Overview of the state transitions
-- ---------------------------------
--
-- See 'ChainSyncJumpingState' for the implementation of the states.
--
-- >                j       ╔════════╗
-- >            ╭────────── ║ Dynamo ║ ◀─────────╮
-- >            │           ╚════════╝           │f
-- >            ▼                  ▲             │
-- >    ┌────────────┐             │     k     ┌──────────┐
-- >    │ Disengaged │ ◀───────────│────────── │ Objector │
-- >    └────────────┘       ╭─────│────────── └──────────┘
-- >                         │     │             ▲    ▲ │
-- >                        g│     │e         b  │    │ │
-- >                         │     │       ╭─────╯   i│ │c
-- >                 ╭╌╌╌╌╌╌╌▼╌╌╌╌╌╌╌╌╌╌╌╌╌│╌╌╌╌╌╌╌╌╌╌│╌▼╌╌╌╮
-- >                 ┆ ╔═══════╗  a   ┌──────┐  d   ┌─────┐ |
-- >                 ┆ ║ Happy ║ ───▶ │ LFI* │ ───▶ │ FI* │ |
-- >                 ┆ ╚═══════╝ ◀─╮  └──────┘      └─────┘ |
-- >                 ┆ Jumper      ╰─────┴────────────╯h    |
-- >                 ╰╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╯
--
-- *: LookingForIntersection and FoundIntersection, abbreviated for this
--    drawing only; this abbreviation will not be used elsewhere.
--
-- A new peer starts as the dynamo if there is no other peer or as a Happy
-- jumper otherwise. The dynamo periodically requests jumps from happy
-- jumpers who, in the ideal case, accept them.
--
-- In the event that a jumper rejects a jump, it goes from Happy to LFI* (a).
-- From there starts a back-and-forth of intersection search messages until
-- the exact point of disagreement with the dynamo is found.
--
-- Once the exact point of disagreement is found, and if there is no objector
-- yet, the jumper becomes the objector (b). If there is an objector, then we
-- compare the intersections of the objector and the jumper. If the jumper's
-- intersection is strictly older, then the jumper replaces the objector (b+c).
-- Otherwise, the jumper is marked as FI* (d).
--
-- If the dynamo disconnects or is disengaged, one peer is elected as the new
-- dynamo (e|f) and all other peers revert to being happy jumpers (g+h).
--
-- If the objector disconnects or is disengaged, and there are FI* jumpers, then
-- the one with the oldest intersection with the dynamo gets elected (i).
--
-- If the dynamo rolls back to a point older than the last jump it requested, it
-- is disengaged (j) and a new dynamo is elected (e|f).
--
-- If the objector agrees with the dynamo, it is disengaged (k). If there are
-- FI* jumpers, then one of them gets elected as the new objector (i).
--
-- If dynamo or objector claim to have no more headers, they are disengaged
-- (j|k).
--
module Ouroboros.Consensus.MiniProtocol.ChainSync.Client.Jumping (
    Context
  , ContextWith (..)
  , Instruction (..)
  , JumpInstruction (..)
  , JumpResult (..)
  , Jumping (..)
  , makeContext
  , mkJumping
  , noJumping
  , registerClient
  , unregisterClient
  ) where

import           Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import           Control.Monad (forM, forM_, when)
import           Data.List (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block (HasHeader (getHeaderFields), Header,
                     Point (..), castPoint, pointSlot, succWithOrigin)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State
                     (ChainSyncClientHandle (..),
                     ChainSyncJumpingJumperState (..),
                     ChainSyncJumpingState (..), ChainSyncState (..),
                     DisengagedInitState (..), DynamoInitState (..),
                     JumpInfo (..), ObjectorInitState (..))
import           Ouroboros.Consensus.Util.IOLike hiding (handle)
import qualified Ouroboros.Network.AnchoredFragment as AF

-- | Hooks for ChainSync jumping.
data Jumping m blk = Jumping
  { -- | Get the next instruction to execute, which can be either to run normal
    -- ChainSync or to jump to a given point. When the peer is a jumper and
    -- there is no jump request, 'jgNextInstruction' blocks until a jump request
    -- is made.
    jgNextInstruction   :: !(m (Instruction blk)),

    -- | To be called whenever the peer claims to have no more headers.
    jgOnAwaitReply      :: !(m ()),

    -- | To be called whenever a header is received from the peer
    -- before it is validated.
    jgOnRollForward     :: !(Point (Header blk) -> m ()),

    -- | To be called whenever a peer rolls back.
    jgOnRollBackward    :: !(WithOrigin SlotNo -> m ()),

    -- | Process the result of a jump, either accepted or rejected.
    --
    -- The jump result is used to decide on the next jumps or whether to elect
    -- an objector.
    jgProcessJumpResult :: !(JumpResult blk -> m ()),

    -- | To be called to update the last known jump possible to the tip of
    -- the peers candidate fragment. The ChainSync clients for all peers should
    -- call this function in case they are or they become dynamos.
    --
    -- JumpInfo is meant to be a snapshot of the @KnownIntersectionState@ of
    -- the ChainSync client. See 'JumpInfo' for more details.
    jgUpdateJumpInfo    :: !(JumpInfo blk -> STM m ())
  }
  deriving stock (Generic)

deriving anyclass instance
  ( IOLike m,
    HasHeader blk,
    NoThunks (Header blk)
  ) =>
  NoThunks (Jumping m blk)

-- | No-op implementation of CSJ
noJumping :: (MonadSTM m) => Jumping m blk
noJumping =
  Jumping
    { jgNextInstruction = pure RunNormally
    , jgOnAwaitReply = pure ()
    , jgOnRollForward = const $ pure ()
    , jgOnRollBackward = const $ pure ()
    , jgProcessJumpResult = const $ pure ()
    , jgUpdateJumpInfo = const $ pure ()
    }

-- | Create the callbacks for a given peer.
mkJumping ::
  ( MonadSTM m,
    Eq peer,
    LedgerSupportsProtocol blk
  ) =>
  PeerContext m peer blk ->
  Jumping m blk
mkJumping peerContext = Jumping
  { jgNextInstruction = atomically $ nextInstruction peerContext
  , jgOnAwaitReply = atomically $ onAwaitReply peerContext
  , jgOnRollForward = atomically . onRollForward peerContext
  , jgOnRollBackward = atomically . onRollBackward peerContext
  , jgProcessJumpResult = atomically . processJumpResult peerContext
  , jgUpdateJumpInfo = updateJumpInfo peerContext
  }

-- | A context for ChainSync jumping
--
-- Invariants:
--
-- - If 'handlesVar' is not empty, then there is exactly one dynamo in it.
-- - There is at most one objector in 'handlesVar'.
-- - If there exist 'FoundIntersection' jumpers in 'handlesVar', then there
--   is an objector and the intersection of the objector with the dynamo is
--   at least as old as the oldest intersection of the `FoundIntersection` jumpers
--   with the dynamo.
data ContextWith peerField handleField m peer blk = Context
  { peer       :: !peerField,
    handle     :: !handleField,
    handlesVar :: !(StrictTVar m (Map peer (ChainSyncClientHandle m blk))),
    jumpSize   :: !SlotNo
  }

-- | A non-specific, generic context for ChainSync jumping.
type Context = ContextWith () ()

-- | A peer-specific context for ChainSync jumping. This is a 'ContextWith'
-- pointing on the handler of the peer in question.
--
-- Invariant: The binding from 'peer' to 'handle' is present in 'handlesVar'.
type PeerContext m peer blk = ContextWith peer (ChainSyncClientHandle m blk) m peer blk

makeContext ::
  MonadSTM m =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  SlotNo ->
  -- ^ The size of jumps, in number of slots.
  STM m (Context m peer blk)
makeContext h jumpSize = do
  pure $ Context () () h jumpSize

-- | Get a generic context from a peer context by stripping away the
-- peer-specific fields.
stripContext :: PeerContext m peer blk -> Context m peer blk
stripContext context = context {peer = (), handle = ()}

-- | Instruction from the jumping governor, either to run normal ChainSync, or
-- to jump to follow a dynamo with the given fragment, or to restart ChainSync.
data Instruction blk
  = RunNormally
  | Restart
  | -- | Jump to the tip of the given fragment.
    JumpInstruction !(JumpInstruction blk)
  deriving (Generic)

deriving instance (HasHeader (Header blk), Eq (Header blk)) => Eq (Instruction blk)
deriving instance (HasHeader (Header blk), Show (Header blk)) => Show (Instruction blk)
deriving anyclass instance
  ( HasHeader blk,
    LedgerSupportsProtocol blk,
    NoThunks (Header blk)
  ) => NoThunks (Instruction blk)

data JumpInstruction blk
  = JumpTo !(JumpInfo blk)
  | -- | Used to set the intersection of the servers of starting objectors.
    -- Otherwise, the ChainSync server wouldn't know which headers to start
    -- serving.
    JumpToGoodPoint !(JumpInfo blk)
  deriving (Generic)

deriving instance (HasHeader (Header blk), Eq (Header blk)) => Eq (JumpInstruction blk)
instance (HasHeader (Header blk), Show (Header blk)) => Show (JumpInstruction blk) where
  showsPrec p = \case
    JumpTo jumpInfo ->
      showParen (p > 10) $ showString "JumpTo " . shows (AF.headPoint $ jTheirFragment jumpInfo)
    JumpToGoodPoint jumpInfo ->
      showParen (p > 10) $ showString "JumpToGoodPoint " . shows (AF.headPoint $ jTheirFragment jumpInfo)

deriving anyclass instance
  ( HasHeader blk,
    LedgerSupportsProtocol blk,
    NoThunks (Header blk)
  ) => NoThunks (JumpInstruction blk)

-- | The result of a jump request, either accepted or rejected.
data JumpResult blk
  = AcceptedJump !(JumpInstruction blk)
  | RejectedJump !(JumpInstruction blk)
  deriving (Generic)

deriving instance (HasHeader (Header blk), Eq (Header blk)) => Eq (JumpResult blk)
deriving instance (HasHeader (Header blk), Show (Header blk)) => Show (JumpResult blk)

deriving anyclass instance
  ( HasHeader blk,
    LedgerSupportsProtocol blk,
    NoThunks (Header blk)
  ) => NoThunks (JumpResult blk)

-- | Compute the next instruction for the given peer. In the majority of cases,
-- this consists in reading the peer's handle, having the dynamo and objector
-- run normally and the jumpers wait for the next jump. As such, this function
-- mostly only reads from and writes to the handle of the peer. For the dynamo, every once in a
-- while, we need to indicate to the jumpers that they need to jump, and this
-- requires writing to a TVar for every jumper.
nextInstruction ::
  ( MonadSTM m ) =>
  PeerContext m peer blk ->
  STM m (Instruction blk)
nextInstruction context =
  readTVar (cschJumping (handle context)) >>= \case
    Disengaged DisengagedDone -> pure RunNormally
    Disengaged Disengaging -> do
      writeTVar (cschJumping (handle context)) (Disengaged DisengagedDone)
      pure Restart
    Dynamo (DynamoStarting goodJumpInfo) _ ->
      pure $ JumpInstruction $ JumpToGoodPoint goodJumpInfo
    Dynamo DynamoStarted _ ->
      pure RunNormally
    Objector Starting goodJump _ -> do
      pure $ JumpInstruction $ JumpToGoodPoint goodJump
    Objector Started _ _ -> pure RunNormally
    Jumper nextJumpVar _ -> do
      readTVar nextJumpVar >>= \case
        Nothing -> retry
        Just jumpInfo -> do
          writeTVar nextJumpVar Nothing
          pure $ JumpInstruction $ JumpTo jumpInfo

-- | This function is called when we receive a 'MsgRollForward' message before
-- validating it.
--
-- We request jumpers to jump here if the next header received by the dynamo is
-- at least jump size slots after the last jump. Note that, since this function
-- runs before validating the next header, it will not be part of the fragment
-- considered for the jump.
--
-- We also check that the Objector disagrees with the header sent at its
-- rejected jump. If it agrees to it, we disengage it.
--
onRollForward :: forall m peer blk.
  ( MonadSTM m,
    LedgerSupportsProtocol blk
  ) =>
  PeerContext m peer blk ->
  Point (Header blk) ->
  STM m ()
onRollForward context point =
  readTVar (cschJumping (handle context)) >>= \case
    Objector _ _ badPoint
      | badPoint == castPoint point -> do
          disengage (handle context)
          electNewObjector (stripContext context)
      | otherwise -> pure ()
    Disengaged{} -> pure ()
    Jumper{} -> pure ()
    Dynamo _ lastJumpSlot
      | let jumpBoundaryPlus1 = jumpSize context + succWithOrigin lastJumpSlot
      , succWithOrigin (pointSlot point) > jumpBoundaryPlus1 -> do
          mJumpInfo <- readTVar (cschJumpInfo (handle context))
          setJumps mJumpInfo
      | otherwise -> pure ()
  where
    setJumps Nothing = error "onRollForward: Dynamo without jump info"
    setJumps (Just jumpInfo) = do
        writeTVar (cschJumping (handle context)) $
          Dynamo DynamoStarted $ pointSlot $ AF.headPoint $ jTheirFragment jumpInfo
        handles <- readTVar (handlesVar context)
        forM_ (Map.elems handles) $ \h ->
          readTVar (cschJumping h) >>= \case
            Jumper nextJumpVar Happy{} -> writeTVar nextJumpVar (Just jumpInfo)
            _ -> pure ()

-- | This function is called when we receive a 'MsgRollBackward' message.
--
-- Here we check if the peer is trying to roll back to a point before the last
-- jump. If so, we disengage the peer. This prevents adversaries from sending
-- as objectors the same chain as the dynamo.
--
onRollBackward :: forall m peer blk.
  ( MonadSTM m,
    Eq peer,
    LedgerSupportsProtocol blk
  ) =>
  PeerContext m peer blk ->
  WithOrigin SlotNo ->
  STM m ()
onRollBackward context slot =
  readTVar (cschJumping (handle context)) >>= \case
    Objector _ _ badPoint
      | slot < pointSlot badPoint -> do
          disengage (handle context)
          electNewObjector (stripContext context)
      | otherwise -> pure ()
    Disengaged{} -> pure ()
    Jumper{} -> pure ()
    Dynamo _ lastJumpSlot
      | slot < lastJumpSlot -> do
          disengage (handle context)
          electNewDynamo (stripContext context)
      | otherwise -> pure ()

-- | This function is called when we receive a 'MsgAwaitReply' message.
--
-- If this is the dynamo, we need to elect a new dynamo as no more headers
-- are available.
onAwaitReply ::
  ( MonadSTM m,
    Eq peer,
    LedgerSupportsProtocol blk
  ) =>
  PeerContext m peer blk ->
  STM m ()
onAwaitReply context =
  readTVar (cschJumping (handle context)) >>= \case
    Dynamo{} -> do
      disengage (handle context)
      electNewDynamo (stripContext context)
    Objector{} -> do
      disengage (handle context)
      electNewObjector (stripContext context)
    Jumper{} ->
      -- A jumper might be receiving a 'MsgAwaitReply' message if it was
      -- previously an objector and a new dynamo was elected.
      disengage (handle context)
    Disengaged{} ->
      pure ()

-- | Process the result of a jump. In the happy case, this only consists in
-- updating the peer's handle to take the new candidate fragment and the new
-- last jump point into account. When disagreeing with the dynamo, though, we
-- enter a phase of several jumps to pinpoint exactly where the disagreement
-- occurs. Once this phase is finished, we trigger the election of a new
-- objector, which might update many TVars.
processJumpResult :: forall m peer blk.
  ( MonadSTM m,
    Eq peer,
    LedgerSupportsProtocol blk
  ) =>
  PeerContext m peer blk ->
  JumpResult blk ->
  STM m ()
processJumpResult context jumpResult =
  readTVar (cschJumping (handle context)) >>= \case
    Dynamo DynamoStarting{} lastJumpSlot ->
      case jumpResult of
        AcceptedJump (JumpToGoodPoint jumpInfo) -> do
          writeTVar (cschJumping (handle context)) $
            Dynamo DynamoStarted lastJumpSlot
          updateChainSyncState (handle context) jumpInfo
        RejectedJump JumpToGoodPoint{} -> do
          disengage (handle context)
          electNewDynamo (stripContext context)

        -- Not interesting in the dynamo state
        AcceptedJump JumpTo{} -> pure ()
        RejectedJump JumpTo{} -> pure ()

    Dynamo DynamoStarted _lastJumpSlot -> pure ()

    Disengaged{} -> pure ()
    Objector Starting goodJump badPoint ->
      case jumpResult of
        AcceptedJump (JumpToGoodPoint jumpInfo) -> do
          writeTVar (cschJumping (handle context)) $
            Objector Started goodJump badPoint
          updateChainSyncState (handle context) jumpInfo
        RejectedJump JumpToGoodPoint{} -> do
          -- If the objector rejects a good point, it is a sign of a rollback
          -- to earlier than the last jump.
          disengage (handle context)
          electNewObjector (stripContext context)

        -- Not interesting in the objector state
        AcceptedJump JumpTo{} -> pure ()
        RejectedJump JumpTo{} -> pure ()

    Objector Started _ _ -> pure ()
    Jumper nextJumpVar jumperState ->
        case jumpResult of
          AcceptedJump (JumpTo goodJumpInfo) -> do
            -- The jump was accepted; we set the jumper's candidate fragment to
            -- the dynamo's candidate fragment up to the accepted point.
            --
            -- The candidate fragments of jumpers don't grow otherwise, as only the
            -- objector and the dynamo request further headers.
            updateChainSyncState (handle context) goodJumpInfo
            writeTVar (cschJumpInfo (handle context)) $ Just goodJumpInfo
            case jumperState of
              LookingForIntersection _goodJumpInfo badJumpInfo ->
                -- @AF.headPoint fragment@ is in @badFragment@, as the jumper
                -- looking for an intersection is the only client asking for its
                -- jumps.
                lookForIntersection nextJumpVar goodJumpInfo badJumpInfo
              Happy _mGoodJumpInfo ->
                writeTVar (cschJumping (handle context)) $
                  Jumper nextJumpVar $ Happy $ Just goodJumpInfo
              FoundIntersection{} ->
                -- Only happy jumpers are asked to jump by the dynamo, and only
                -- jumpers looking for an intersection are asked to jump by
                -- themselves.
                error "processJumpResult: Jumpers in state FoundIntersection shouldn't be further jumping."

          RejectedJump (JumpTo badJumpInfo) ->
            -- The tip of @goodFragment@ is in @jTheirFragment jumpInfo@ or is
            -- an ancestor of it. If the jump was requested by the dynamo, this
            -- holds because the dynamo is not allowed to rollback before the
            -- jumps that it requests.
            --
            -- If the jump was requested by the jumper, this holds because the
            -- jumper is looking for an intersection, and such jumper only asks
            -- for jumps that meet this condition.
            case jumperState of
              LookingForIntersection goodJumpInfo _ ->
                lookForIntersection nextJumpVar goodJumpInfo badJumpInfo
              Happy mGoodJumpInfo ->
                lookForIntersection nextJumpVar (mkGoodJumpInfo mGoodJumpInfo badJumpInfo) badJumpInfo
              FoundIntersection{} ->
                error "processJumpResult (rejected): Jumpers in state FoundIntersection shouldn't be further jumping."

          -- These aren't interesting in the case of jumpers.
          AcceptedJump JumpToGoodPoint{} -> pure ()
          RejectedJump JumpToGoodPoint{} -> pure ()
  where
    -- Avoid redundant constraint "HasHeader blk" reported by some ghc's
    _ = getHeaderFields @blk

    updateChainSyncState :: ChainSyncClientHandle m blk -> JumpInfo blk -> STM m ()
    updateChainSyncState handle jump = do
      let fragment = jTheirFragment jump
      modifyTVar (cschState handle) $ \csState ->
        csState {csCandidate = fragment, csLatestSlot = Just (AF.headSlot fragment) }
      writeTVar (cschJumpInfo handle) $ Just jump

    mkGoodJumpInfo :: Maybe (JumpInfo blk) -> JumpInfo blk -> JumpInfo blk
    mkGoodJumpInfo mGoodJumpInfo badJumpInfo = do
      let badFragment = jTheirFragment badJumpInfo
          -- use the jump info of the rejected jump if the good jump info is
          -- not available (i.e. there were no accepted jumps)
          badFragmentStart = AF.takeOldest 0 badFragment
       in fromMaybe (badJumpInfo {jTheirFragment = badFragmentStart}) mGoodJumpInfo

    -- | Given a good point (where we know we agree with the dynamo) and a bad
    -- fragment (where we know the tip disagrees with the dynamo), either decide
    -- that we know the intersection for sure (if the bad point is the successor
    -- of the good point) or program a jump somewhere in the middle to refine
    -- those points.
    --
    -- PRECONDITION: The good point is in the candidate fragment of
    -- @badJumpInfo@ or it is an ancestor of it.
    lookForIntersection nextJumpVar goodJumpInfo badJumpInfo = do
      let badFragment = jTheirFragment badJumpInfo
          -- If the good point is not in the bad fragment, the anchor of the bad
          -- fragment should be a good point too.
          searchFragment =
              maybe badFragment snd $
                AF.splitAfterPoint badFragment (AF.headPoint $ jTheirFragment goodJumpInfo)
      let len = AF.length searchFragment
      if len <= 1 then do
        -- If the fragment only contains the bad tip, we know the
        -- intersection is the good point.
        -- Clear any subsequent jumps requested by the dynamo.
        writeTVar nextJumpVar Nothing
        maybeElectNewObjector nextJumpVar goodJumpInfo (AF.headPoint badFragment)
      else do
        let middlePoint = len `div` 2
            theirFragment = AF.dropNewest middlePoint badFragment
        writeTVar nextJumpVar $ Just
          badJumpInfo { jTheirFragment = theirFragment }
        writeTVar (cschJumping (handle context)) $
          Jumper nextJumpVar (LookingForIntersection goodJumpInfo badJumpInfo)

    maybeElectNewObjector nextJumpVar goodJumpInfo badPoint = do
      findObjector (stripContext context) >>= \case
        Nothing ->
          -- There is no objector yet. Promote the jumper to objector.
          writeTVar (cschJumping (handle context)) (Objector Starting goodJumpInfo badPoint)
        Just (oGoodJump, oPoint, oHandle)
          | pointSlot oPoint <= pointSlot badPoint ->
              -- The objector's intersection is still old enough. Keep it.
              writeTVar (cschJumping (handle context)) $
                Jumper nextJumpVar (FoundIntersection goodJumpInfo badPoint)
          | otherwise -> do
              -- Found an earlier intersection. Demote the old objector and
              -- promote the jumper to objector.
              newJumper Nothing (FoundIntersection oGoodJump oPoint) >>=
                writeTVar (cschJumping oHandle)
              writeTVar (cschJumping (handle context)) (Objector Starting goodJumpInfo badPoint)

updateJumpInfo ::
  (MonadSTM m) =>
  PeerContext m peer blk ->
  JumpInfo blk ->
  STM m ()
updateJumpInfo context jumpInfo =
  readTVar (cschJumping (handle context)) >>= \case
    Disengaged{} -> pure ()
    _ -> writeTVar (cschJumpInfo (handle context)) $ Just jumpInfo

-- | Find the dynamo in a TVar containing a map of handles. Returns then handle
-- of the dynamo, or 'Nothing' if there is none.
getDynamo ::
  (MonadSTM m) =>
  StrictTVar m (Map peer (ChainSyncClientHandle m blk)) ->
  STM m (Maybe (ChainSyncClientHandle m blk))
getDynamo handlesVar = do
  handles <- Map.elems <$> readTVar handlesVar
  findM (\handle -> isDynamo <$> readTVar (cschJumping handle)) handles
  where
    isDynamo Dynamo{} = True
    isDynamo _        = False

-- | Disengage a peer, meaning that it will no longer be asked to jump or
-- act as dynamo or objector.
disengage :: MonadSTM m => ChainSyncClientHandle m blk -> STM m ()
disengage handle = do
  writeTVar (cschJumping handle) (Disengaged Disengaging)
  writeTVar (cschJumpInfo handle) Nothing

-- | Convenience function that, given an intersection point and a jumper state,
-- make a fresh 'Jumper' constructor.
newJumper ::
  ( MonadSTM m,
    LedgerSupportsProtocol blk
  ) =>
  Maybe (JumpInfo blk) ->
  ChainSyncJumpingJumperState blk ->
  STM m (ChainSyncJumpingState m blk)
newJumper jumpInfo jumperState = do
  nextJumpVar <- newTVar jumpInfo
  pure $ Jumper nextJumpVar jumperState

-- | Register a new ChainSync client to a context, returning a 'PeerContext' for
-- that peer. If there is no dynamo, the peer starts as dynamo; otherwise, it
-- starts as a jumper.
registerClient ::
  ( Ord peer,
    LedgerSupportsProtocol blk,
    IOLike m
  ) =>
  Context m peer blk ->
  peer ->
  StrictTVar m (ChainSyncState blk) ->
  -- | A function to make a client handle from a jumping state.
  (StrictTVar m (ChainSyncJumpingState m blk) -> ChainSyncClientHandle m blk) ->
  STM m (PeerContext m peer blk)
registerClient context peer csState mkHandle = do
  csjState <- getDynamo (handlesVar context) >>= \case
    Nothing -> do
      fragment <- csCandidate <$> readTVar csState
      pure $ Dynamo DynamoStarted $ pointSlot $ AF.anchorPoint fragment
    Just handle -> do
      mJustInfo <- readTVar (cschJumpInfo handle)
      newJumper mJustInfo (Happy Nothing)
  cschJumping <- newTVar csjState
  let handle = mkHandle cschJumping
  modifyTVar (handlesVar context) $ Map.insert peer handle
  pure $ context {peer, handle}

-- | Unregister a client from a 'PeerContext'; this might trigger the election
-- of a new dynamo or objector if the peer was one of these two.
unregisterClient ::
  ( MonadSTM m,
    Ord peer,
    LedgerSupportsProtocol blk
  ) =>
  PeerContext m peer blk ->
  STM m ()
unregisterClient context = do
  modifyTVar (handlesVar context) $ Map.delete (peer context)
  let context' = stripContext context
  readTVar (cschJumping (handle context)) >>= \case
    Disengaged{} -> pure ()
    Jumper{} -> pure ()
    Objector{} -> electNewObjector context'
    Dynamo{} -> electNewDynamo context'

-- | Choose an unspecified new non-idling dynamo and demote all other peers to
-- jumpers.
electNewDynamo ::
  ( MonadSTM m,
    Eq peer,
    LedgerSupportsProtocol blk
  ) =>
  Context m peer blk ->
  STM m ()
electNewDynamo context = do
  peerStates <- Map.toList <$> readTVar (handlesVar context)
  mDynamo <- findNonDisengaged peerStates
  case mDynamo of
    Nothing -> pure ()
    Just (dynId, dynamo) -> do
      fragment <- csCandidate <$> readTVar (cschState dynamo)
      mJumpInfo <- readTVar (cschJumpInfo dynamo)
      -- If there is no jump info, the dynamo must be just starting and
      -- there is no need to set the intersection of the ChainSync server.
      let dynamoInitState = maybe DynamoStarted DynamoStarting mJumpInfo
      writeTVar (cschJumping dynamo) $
        Dynamo dynamoInitState $ pointSlot $ AF.headPoint fragment
      -- Demote all other peers to jumpers
      forM_ peerStates $ \(peer, st) ->
        when (peer /= dynId) $ do
          jumpingState <- readTVar (cschJumping st)
          when (not (isDisengaged jumpingState)) $
            newJumper mJumpInfo (Happy Nothing)
              >>= writeTVar (cschJumping st)
  where
    findNonDisengaged =
      findM $ \(_, st) -> not . isDisengaged <$> readTVar (cschJumping st)
    isDisengaged Disengaged{} = True
    isDisengaged _            = False

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM p (x : xs) = p x >>= \case
  True -> pure (Just x)
  False -> findM p xs

-- | Find the objector in a context, if there is one.
findObjector ::
  (MonadSTM m) =>
  Context m peer blk ->
  STM m (Maybe (JumpInfo blk, Point (Header blk), ChainSyncClientHandle m blk))
findObjector context = do
  readTVar (handlesVar context) >>= go . Map.toList
  where
    go [] = pure Nothing
    go ((_, handle):xs) =
      readTVar (cschJumping handle) >>= \case
        Objector _ goodJump badPoint -> pure $ Just (goodJump, badPoint, handle)
        _ -> go xs

-- | Look into all dissenting jumper and promote the one with the oldest
-- intersection with the dynamo as the new objector.
electNewObjector ::
  (MonadSTM m) =>
  Context m peer blk ->
  STM m ()
electNewObjector context = do
  peerStates <- Map.toList <$> readTVar (handlesVar context)
  dissentingJumpers <- collectDissentingJumpers peerStates
  let sortedJumpers = sortOn (pointSlot . fst) dissentingJumpers
  case sortedJumpers of
    (badPoint, (goodJumpInfo, handle)):_ ->
      writeTVar (cschJumping handle) $ Objector Starting goodJumpInfo badPoint
    _ ->
      pure ()
  where
    collectDissentingJumpers peerStates =
      fmap catMaybes $
      forM peerStates $ \(_, handle) ->
        readTVar (cschJumping handle) >>= \case
          Jumper _ (FoundIntersection goodJumpInfo badPoint) ->
            pure $ Just (badPoint, (goodJumpInfo, handle))
          _ ->
            pure Nothing
