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
-- intersection is asked to compete with the dynamo in the GDD logic (becoming
-- an objector). If the dynamo is disconnected, a new dynamo is elected and the
-- objector is demoted to a jumper.
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
-- CSJ depends on the ChainSync client to disconnect dynamos that have an empty
-- genesis window after their intersection with the selection. This is necessary
-- because otherwise there are no points to jump to, and CSJ could would get
-- stuck when the dynamo blocks on the forecast horizon. See
-- Note [Candidate comparing beyond the forecast horizon] in
-- "Ouroboros.Consensus.MiniProtocol.ChainSync.Client".
--
-- Interactions with the BlockFetch logic
-- --------------------------------------
--
-- When syncing, the BlockFetch logic might request to change the dynamo with
-- a call to 'rotateDynamo'. This is because the choice of dynamo influences
-- which peer is selected to download blocks. See the note "Interactions with
-- ChainSync Jumping" in "Ouroboros.Network.BlockFetch.Decision.BulkSync".
--
-- Interactions with the Limit on Patience
-- ---------------------------------------
--
-- Jumpers don't leak the Limit on Patience (LoP) bucket until they are promoted
-- to dynamos or objectors. And the leaking is stopped as soon as they are
-- demoted.
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
-- >            │        ╭──╚════════╝           │f
-- >            ▼        │         ▲             │
-- >    ┌────────────┐   │         │     k     ┌──────────┐
-- >    │ Disengaged │ ◀─│─────────│────────── │ Objector │
-- >    └────────────┘   │   ╭─────│────────── └──────────┘
-- >                     │   │     │             ▲    ▲ │
-- >                    l│  g│     │e         b  │    │ │
-- >                     │   │     │       ╭─────╯   i│ │c
-- >                 ╭╌╌╌▼╌╌╌▼╌╌╌╌╌╌╌╌╌╌╌╌╌│╌╌╌╌╌╌╌╌╌╌│╌▼╌╌╌╮
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
-- The BlockFetch logic can ask to change the dynamo if it is not serving blocks
-- fast enough. If there are other non-disengaged peers, the dynamo (and the
-- objector if there is one) is demoted to a jumper (l+g) and a new dynamo is
-- elected.
--
module Ouroboros.Consensus.MiniProtocol.ChainSync.Client.Jumping (
    Context
  , ContextWith (..)
  , Instruction (..)
  , JumpInstruction (..)
  , JumpResult (..)
  , Jumping (..)
  , TraceEvent (..)
  , getDynamo
  , makeContext
  , mkJumping
  , noJumping
  , registerClient
  , rotateDynamo
  , unregisterClient
  ) where

import           Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import           Control.Monad (forM, forM_, void, when)
import           Control.Tracer (Tracer, traceWith)
import           Data.Foldable (toList, traverse_)
import           Data.List (sortOn)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Maybe.Strict (StrictMaybe (..))
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as Seq
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block (HasHeader (getHeaderFields), Header,
                     Point (..), castPoint, pointSlot, succWithOrigin)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State
                     (ChainSyncClientHandle (..),
                     ChainSyncClientHandleCollection (..),
                     ChainSyncJumpingJumperState (..),
                     ChainSyncJumpingState (..), ChainSyncState (..),
                     DisengagedInitState (..), DynamoInitState (..),
                     JumpInfo (..), JumperInitState (..),
                     ObjectorInitState (..))
import           Ouroboros.Consensus.Util.IOLike hiding (handle)
import qualified Ouroboros.Network.AnchoredFragment as AF

-- | Hooks for ChainSync jumping.
data Jumping m blk = Jumping
  { -- | Get the next instruction to execute, which can be either to run normal
    -- ChainSync, to jump to a given point, or to restart ChainSync. When the
    -- peer is a jumper and there is no jump request, 'jgNextInstruction' blocks
    -- until a jump request is made.
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
-- - If 'handlesCol' is not empty, then there is exactly one dynamo in it.
-- - There is at most one objector in 'handlesCol'.
-- - If there exist 'FoundIntersection' jumpers in 'handlesCol', then there
--   is an objector and the intersection of the objector with the dynamo is
--   at least as old as the oldest intersection of the `FoundIntersection` jumpers
--   with the dynamo.
data ContextWith peerField handleField m peer blk = Context
  { peer       :: !peerField,
    handle     :: !handleField,
    handlesCol :: !(ChainSyncClientHandleCollection peer m blk),
    jumpSize   :: !SlotNo
  }

-- | A non-specific, generic context for ChainSync jumping.
type Context = ContextWith () ()

-- | A peer-specific context for ChainSync jumping. This is a 'ContextWith'
-- pointing on the handler of the peer in question.
--
-- Invariant: The binding from 'peer' to 'handle' is present in 'handlesCol'.
type PeerContext m peer blk = ContextWith peer (ChainSyncClientHandle m blk) m peer blk

makeContext ::
  MonadSTM m =>
  ChainSyncClientHandleCollection peer m blk ->
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
    -- | The restart instruction restarts the ChainSync protocol. This is
    -- necessary when disengaging a peer of which we know no point that we
    -- could set the intersection of the ChainSync server to.
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
  | -- | Used to set the intersection of the ChainSync servers of starting
    -- objectors and dynamos. Otherwise, the ChainSync server wouldn't know
    -- which headers to start serving.
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
    Dynamo (DynamoStarting goodJumpInfo) lastJumpSlot -> do
      writeTVar (cschJumping (handle context)) $
        Dynamo DynamoStarted lastJumpSlot
      pure $ JumpInstruction $ JumpToGoodPoint goodJumpInfo
    Dynamo DynamoStarted _ ->
      pure RunNormally
    Objector Starting goodJump badPoint -> do
      writeTVar (cschJumping (handle context)) $
        Objector Started goodJump badPoint
      pure $ JumpInstruction $ JumpToGoodPoint goodJump
    Objector Started _ _ -> pure RunNormally
    Jumper nextJumpVar jumperState -> do
      readTVar nextJumpVar >>= \case
        Nothing -> retry
        Just jumpInfo -> do
          writeTVar nextJumpVar Nothing
          case jumperState of
            Happy FreshJumper mGoodJumpInfo ->
              writeTVar (cschJumping (handle context)) $
                Jumper nextJumpVar $ Happy StartedJumper mGoodJumpInfo
            _ -> pure ()
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
        handles <- cschcSeq (handlesCol context)
        forM_ handles $ \(_, h) ->
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
          void $ electNewDynamo (stripContext context)
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
      void $ electNewDynamo (stripContext context)
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
    Dynamo{} ->
      case jumpResult of
        AcceptedJump (JumpToGoodPoint jumpInfo) ->
          updateChainSyncState (handle context) jumpInfo
        RejectedJump JumpToGoodPoint{} -> do
          startDisengaging (handle context)
          void $ electNewDynamo (stripContext context)

        -- Not interesting in the dynamo state
        AcceptedJump JumpTo{} -> pure ()
        RejectedJump JumpTo{} -> pure ()

    Disengaged{} -> pure ()
    Objector{} ->
      case jumpResult of
        AcceptedJump (JumpToGoodPoint jumpInfo) ->
          updateChainSyncState (handle context) jumpInfo
        RejectedJump JumpToGoodPoint{} -> do
          -- If the objector rejects a good point, it is a sign of a rollback
          -- to earlier than the last jump.
          startDisengaging (handle context)
          electNewObjector (stripContext context)

        -- Not interesting in the objector state
        AcceptedJump JumpTo{} -> pure ()
        RejectedJump JumpTo{} -> pure ()

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
              Happy StartedJumper _mGoodJumpInfo ->
                writeTVar (cschJumping (handle context)) $
                  Jumper nextJumpVar $ Happy StartedJumper $ Just goodJumpInfo
              Happy FreshJumper _mGoodJumpInfo ->
                pure ()
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
              Happy StartedJumper mGoodJumpInfo ->
                lookForIntersection nextJumpVar (mkGoodJumpInfo mGoodJumpInfo badJumpInfo) badJumpInfo
              Happy FreshJumper _ ->
                pure ()
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
        csState {csCandidate = fragment, csLatestSlot = SJust (AF.headSlot fragment) }
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
        Just (oInitState, oGoodJump, oPoint, oHandle)
          | pointSlot oPoint <= pointSlot badPoint ->
              -- The objector's intersection is still old enough. Keep it.
              writeTVar (cschJumping (handle context)) $
                Jumper nextJumpVar (FoundIntersection Starting goodJumpInfo badPoint)
          | otherwise -> do
              -- Found an earlier intersection. Demote the old objector and
              -- promote the jumper to objector.
              newJumper Nothing (FoundIntersection oInitState oGoodJump oPoint) >>=
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
  ChainSyncClientHandleCollection peer m blk ->
  STM m (Maybe (peer, ChainSyncClientHandle m blk))
getDynamo handlesCol = do
  handles <- cschcSeq handlesCol
  findM (\(_, handle) -> isDynamo <$> readTVar (cschJumping handle)) handles
  where
    isDynamo Dynamo{} = True
    isDynamo _        = False

-- | Disengage a peer, meaning that it will no longer be asked to jump or
-- act as dynamo or objector.
disengage :: MonadSTM m => ChainSyncClientHandle m blk -> STM m ()
disengage = disengageWith DisengagedDone

-- | Like 'disengage', but additionally restart ChainSync
startDisengaging :: MonadSTM m => ChainSyncClientHandle m blk -> STM m ()
startDisengaging = disengageWith Disengaging

disengageWith ::
  MonadSTM m =>
  DisengagedInitState ->
  ChainSyncClientHandle m blk ->
  STM m ()
disengageWith initState handle = do
  writeTVar (cschJumping handle) (Disengaged initState)
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
  ( LedgerSupportsProtocol blk,
    IOLike m
  ) =>
  Context m peer blk ->
  peer ->
  StrictTVar m (ChainSyncState blk) ->
  -- | A function to make a client handle from a jumping state.
  (StrictTVar m (ChainSyncJumpingState m blk) -> ChainSyncClientHandle m blk) ->
  STM m (PeerContext m peer blk)
registerClient context peer csState mkHandle = do
  csjState <- getDynamo (handlesCol context) >>= \case
    Nothing -> do
      fragment <- csCandidate <$> readTVar csState
      pure $ Dynamo DynamoStarted $ pointSlot $ AF.anchorPoint fragment
    Just (_, handle) -> do
      mJustInfo <- readTVar (cschJumpInfo handle)
      newJumper mJustInfo (Happy FreshJumper Nothing)
  cschJumping <- newTVar csjState
  let handle = mkHandle cschJumping
  cschcAddHandle (handlesCol context) peer handle
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
  cschcRemoveHandle (handlesCol context) (peer context)
  let context' = stripContext context
  readTVar (cschJumping (handle context)) >>= \case
    Disengaged{} -> pure ()
    Jumper{} -> pure ()
    Objector{} -> electNewObjector context'
    Dynamo{} -> void $ electNewDynamo context'

-- | Elects a new dynamo by demoting the given dynamo (and the objector if there
-- is one) to a jumper, moving the peer to the end of the queue of chain sync
-- handles and electing a new dynamo.
--
-- It does nothing if there is no other engaged peer to elect or if the given
-- peer is not the dynamo.
--
-- Yields the new dynamo, if there is one.
rotateDynamo ::
  ( Ord peer,
    LedgerSupportsProtocol blk,
    MonadSTM m
  ) =>
  Tracer m (TraceEvent peer) ->
  ChainSyncClientHandleCollection peer m blk ->
  peer ->
  m ()
  -- STM m (Maybe (peer, ChainSyncClientHandle m blk))
rotateDynamo tracer handlesCol peer = do
  traceEvent <- atomically $ do
    handles <- cschcMap handlesCol
    case handles Map.!? peer of
      Nothing ->
        -- Do not re-elect a dynamo if the peer has been disconnected.
        pure Nothing
      Just oldDynHandle ->
        readTVar (cschJumping oldDynHandle) >>= \case
          Dynamo{} -> do
            cschcRotateHandle handlesCol peer
            peerStates <- cschcSeq handlesCol
            mEngaged <- findNonDisengaged peerStates
            case mEngaged of
              Nothing ->
                -- There are no engaged peers. This case cannot happen, as the
                -- dynamo is always engaged.
                error "rotateDynamo: no engaged peer found"
              Just (newDynamoId, newDynHandle)
                | newDynamoId == peer ->
                  -- The old dynamo is the only engaged peer left.
                  pure Nothing
                | otherwise -> do
                  newJumper Nothing (Happy FreshJumper Nothing)
                    >>= writeTVar (cschJumping oldDynHandle)
                  promoteToDynamo peerStates newDynamoId newDynHandle
                  pure $ Just $ RotatedDynamo peer newDynamoId
          _ ->
            -- Do not re-elect a dynamo if the peer is not the dynamo.
            pure Nothing
  traverse_ (traceWith tracer) traceEvent

-- | Choose an unspecified new non-idling dynamo and demote all other peers to
-- jumpers.
electNewDynamo ::
  ( MonadSTM m,
    Eq peer,
    LedgerSupportsProtocol blk
  ) =>
  Context m peer blk ->
  STM m (Maybe (peer, ChainSyncClientHandle m blk))
electNewDynamo context = do
  peerStates <- cschcSeq (handlesCol context)
  mDynamo <- findNonDisengaged peerStates
  case mDynamo of
    Nothing -> pure Nothing
    Just (dynId, dynamo) -> do
      promoteToDynamo peerStates dynId dynamo
      pure $ Just (dynId, dynamo)

-- | Promote the given peer to dynamo and demote all other peers to jumpers.
promoteToDynamo ::
  ( MonadSTM m,
    Eq peer,
    LedgerSupportsProtocol blk
  ) =>
  StrictSeq (peer, ChainSyncClientHandle m blk) ->
  peer ->
  ChainSyncClientHandle m blk ->
  STM m ()
promoteToDynamo peerStates dynId dynamo = do
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
        newJumper mJumpInfo (Happy FreshJumper Nothing)
          >>= writeTVar (cschJumping st)

-- | Find a non-disengaged peer in the given sequence
findNonDisengaged ::
  (MonadSTM m) =>
  StrictSeq (peer, ChainSyncClientHandle m blk) ->
  STM m (Maybe (peer, ChainSyncClientHandle m blk))
findNonDisengaged =
  findM $ \(_, st) -> not . isDisengaged <$> readTVar (cschJumping st)

isDisengaged :: ChainSyncJumpingState m blk -> Bool
isDisengaged Disengaged{} = True
isDisengaged _            = False

findM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m (Maybe a)
findM p =
  foldr (\x mb -> p x >>= \case True -> pure (Just x); False -> mb) (pure Nothing)

-- | Find the objector in a context, if there is one.
findObjector ::
  (MonadSTM m) =>
  Context m peer blk ->
  STM m (Maybe (ObjectorInitState, JumpInfo blk, Point (Header blk), ChainSyncClientHandle m blk))
findObjector context =
  cschcSeq (handlesCol context) >>= go
  where
    go Seq.Empty = pure Nothing
    go ((_, handle) Seq.:<| xs) =
      readTVar (cschJumping handle) >>= \case
        Objector initState goodJump badPoint ->
          pure $ Just (initState, goodJump, badPoint, handle)
        _ -> go xs

-- | Look into all dissenting jumper and promote the one with the oldest
-- intersection with the dynamo as the new objector.
electNewObjector ::
  (MonadSTM m) =>
  Context m peer blk ->
  STM m ()
electNewObjector context = do
  peerStates <- toList <$> cschcSeq (handlesCol context)
  dissentingJumpers <- collectDissentingJumpers peerStates
  let sortedJumpers = sortOn (pointSlot . fst) dissentingJumpers
  case sortedJumpers of
    (badPoint, (initState, goodJumpInfo, handle)):_ ->
      writeTVar (cschJumping handle) $ Objector initState goodJumpInfo badPoint
    _ ->
      pure ()
  where
    collectDissentingJumpers peerStates =
      fmap catMaybes $
      forM peerStates $ \(_, handle) ->
        readTVar (cschJumping handle) >>= \case
          Jumper _ (FoundIntersection initState goodJumpInfo badPoint) ->
            pure $ Just (badPoint, (initState, goodJumpInfo, handle))
          _ ->
            pure Nothing

data TraceEvent peer
  = RotatedDynamo peer peer
  deriving (Show)
