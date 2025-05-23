{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
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
-- >            │        ╭─ ╚════════╝           │f
-- >            ▼        │         ▲             │
-- >    ┌────────────┐   │         │     k     ┌──────────┐
-- >    │ Disengaged │ ◀─│─────────│────────── │ Objector │
-- >    └────────────┘   │   ╭─────│────────── └──────────┘
-- >                     │   │     │             ▲    ▲ │
-- >                    l│  g│     │e         b  │    │ │
-- >                     │   │     │       ╭─────╯   i│ │c
-- >                 ╭╌╌╌▼╌╌╌▼╌╌╌╌╌╌╌╌╌╌╌╌╌│╌╌╌╌╌╌╌╌╌╌│╌▼╌╌╌╮
-- >                 ┆ ╔═══════╗  a   ┌──────┐  d   ┌─────┐ ┆
-- >                 ┆ ║ Happy ║ ───▶ │ LFI* │ ───▶ │ FI* │ ┆
-- >                 ┆ ╚═══════╝ ◀─╮  └──────┘      └─────┘ ┆
-- >                 ┆ Jumper      ╰─────┴────────────╯h    ┆
-- >                 ╰╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╯
--
-- *: LookingForIntersection and FoundIntersection, abbreviated for this
--    drawing only; this abbreviation will not be used elsewhere.
--
-- In the following walk-through, we will point to transitions in the drawing
-- between parentheses, like so: (a) (b+c) (e|f). We will use `+` to express
-- that both transitions happen simultaneously (for different peers) and `|` to
-- express a choice.
--
-- A new peer starts as the dynamo if there is no other peer or as a Happy
-- jumper otherwise. The dynamo periodically requests jumps from happy
-- jumpers who, in the ideal case, accept them and remain happy jumpers.
--
-- In the event that a jumper rejects a jump, it goes from Happy to LFI* (a).
-- From there starts a back-and-forth of intersection search messages until
-- the exact point of disagreement with the dynamo is found.
--
-- Once the exact point of disagreement is found, and if there is no objector
-- yet, the jumper becomes the objector (b). If there is an objector, then we
-- compare the intersection of the objector with the dynamo and the intersection
-- of the jumper with the dynamo. If the jumper's intersection is strictly
-- older, then the jumper replaces the objector, who is marked as FI* (b+c).
-- Otherwise, the jumper is marked as FI* (d).
--
-- If the dynamo disconnects or is disengaged, one peer is elected as the new
-- dynamo (e|f) and all the other peers revert to being happy jumpers (g+h).
--
-- If the objector disconnects or is disengaged, and there are FI* jumpers, then
-- the one with the oldest intersection with the dynamo gets elected (i).
-- Otherwise, we are left with no objector.
--
-- If the dynamo rolls back to a point older than the last jump it requested, it
-- is disengaged (j), a new dynamo is elected (e|f), and all the other peers
-- revert to being happy jumpers (g+h).
--
-- If the objector agrees with the dynamo, it is disengaged (k). If there are
-- FI* jumpers, then one of them gets elected as the new objector (i).
-- Otherwise, we are left with no objector.
--
-- If the dynamo or the objector claim to have no more headers, they are
-- disengaged (j|k), triggering the same chain of effect as described in the two
-- previous points.
--
-- The BlockFetch logic can ask to change the dynamo if it is not serving
-- blocks fast enough. If there are other non-disengaged peers, all peers are
-- demoted to happy jumpers (l+g+h) and a new dynamo is elected (e).
module Ouroboros.Consensus.MiniProtocol.ChainSync.Client.Jumping
  ( Context
  , ContextWith (..)
  , Instruction (..)
  , JumpInstruction (..)
  , JumpResult (..)
  , Jumping (..)
  , TraceCsjReason (..)
  , TraceEventCsj (..)
  , TraceEventDbf (..)
  , getDynamo
  , makeContext
  , mkJumping
  , noJumping
  , registerClient
  , rotateDynamo
  , unregisterClient
  ) where

import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import Control.Monad (forM, forM_, when)
import Control.Tracer (Tracer, traceWith)
import Data.Foldable (toList, traverse_)
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as Seq
import qualified Data.Strict.Either as Strict
import Data.Typeable (Typeable)
import Data.Void (absurd)
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block
  ( HasHeader (getHeaderFields)
  , Header
  , Point (..)
  , castPoint
  , pointSlot
  , succWithOrigin
  )
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State
  ( ChainSyncClientHandle (..)
  , ChainSyncClientHandleCollection (..)
  , ChainSyncJumpingJumperState (..)
  , ChainSyncJumpingState (..)
  , ChainSyncState (..)
  , DisengagedInitState (..)
  , DynamoInitState (..)
  , JumpInfo (..)
  , JumperInitState (..)
  , ObjectorInitState (..)
  )
import Ouroboros.Consensus.Node.GsmState (GsmState)
import qualified Ouroboros.Consensus.Node.GsmState as GSM
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.IOLike hiding (handle)
import qualified Ouroboros.Network.AnchoredFragment as AF

-- | Hooks for ChainSync jumping.
data Jumping m blk = Jumping
  { jgNextInstruction :: !(m (Instruction blk))
  -- ^ Get the next instruction to execute, which can be either to run normal
  -- ChainSync, to jump to a given point, or to restart ChainSync. When the
  -- peer is a jumper and there is no jump request, 'jgNextInstruction' blocks
  -- until a jump request is made.
  , jgOnAwaitReply :: !(m ())
  -- ^ To be called whenever the peer claims to have no more headers.
  , jgOnRollForward :: !(Point (Header blk) -> m ())
  -- ^ To be called whenever a header is received from the peer
  -- before it is validated.
  , jgOnRollBackward :: !(WithOrigin SlotNo -> m ())
  -- ^ To be called whenever a peer rolls back.
  , jgProcessJumpResult :: !(JumpResult blk -> m ())
  -- ^ Process the result of a jump, either accepted or rejected.
  --
  -- The jump result is used to decide on the next jumps or whether to elect
  -- an objector.
  , jgUpdateJumpInfo :: !(JumpInfo blk -> STM m ())
  -- ^ To be called to update the last known jump possible to the tip of
  -- the peers candidate fragment. The ChainSync clients for all peers should
  -- call this function in case they are or they become dynamos.
  --
  -- JumpInfo is meant to be a snapshot of the @KnownIntersectionState@ of
  -- the ChainSync client. See 'JumpInfo' for more details.
  }
  deriving stock Generic

deriving anyclass instance
  ( IOLike m
  , HasHeader blk
  , NoThunks (Header blk)
  ) =>
  NoThunks (Jumping m blk)

-- | No-op implementation of CSJ
noJumping :: MonadSTM m => Jumping m blk
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
  ( MonadSTM m
  , Eq peer
  , LedgerSupportsProtocol blk
  ) =>
  PeerContext m peer blk ->
  Jumping m blk
mkJumping peerContext =
  Jumping
    { jgNextInstruction =
        atomically (nextInstruction (pure ()) peerContext) >>= \case
          Strict.Right instr -> pure instr
          Strict.Left () -> do
            traceWith (tracer peerContext) BlockedOnJump
            id $
              fmap (Strict.either absurd id) $
                atomically $
                  nextInstruction retry peerContext
    , jgOnAwaitReply = f $ onAwaitReply peerContext
    , jgOnRollForward = f . onRollForward peerContext
    , jgOnRollBackward = f . onRollBackward peerContext
    , jgProcessJumpResult = f . processJumpResult peerContext
    , jgUpdateJumpInfo = updateJumpInfo peerContext
    }
 where
  f m = atomically m >>= traverse_ (traceWith (tracer peerContext))

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
  { peer :: !peerField
  , handle :: !handleField
  , handlesCol :: !(ChainSyncClientHandleCollection peer m blk)
  , jumpSize :: !SlotNo
  , tracer :: Tracer m (TraceEventCsj peer blk)
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
  -- | The size of jumps, in number of slots.
  Tracer m (TraceEventCsj peer blk) ->
  STM m (Context m peer blk)
makeContext h jumpSize tracer = do
  pure $ Context () () h jumpSize tracer

-- | Get a generic context from a peer context by stripping away the
-- peer-specific fields.
stripContext :: PeerContext m peer blk -> Context m peer blk
stripContext context = context{peer = (), handle = ()}

-- | Instruction from the jumping governor, either to run normal ChainSync, or
-- to jump to follow a dynamo with the given fragment, or to restart ChainSync.
data Instruction blk
  = RunNormally
  | -- | The restart instruction restarts the ChainSync protocol. This is
    -- necessary when disengaging a peer of which we know no point that we
    -- could set the intersection of the ChainSync server to.
    Restart
  | -- | Jump to the tip of the given fragment.
    JumpInstruction !(JumpInstruction blk)
  deriving Generic

deriving instance (Typeable blk, HasHeader (Header blk), Eq (Header blk)) => Eq (Instruction blk)
deriving instance
  (Typeable blk, HasHeader (Header blk), Show (Header blk)) => Show (Instruction blk)
deriving anyclass instance
  ( HasHeader blk
  , LedgerSupportsProtocol blk
  , NoThunks (Header blk)
  ) =>
  NoThunks (Instruction blk)

data JumpInstruction blk
  = JumpTo !(JumpInfo blk)
  | -- | Used to set the intersection of the ChainSync servers of starting
    -- objectors and dynamos. Otherwise, the ChainSync server wouldn't know
    -- which headers to start serving.
    JumpToGoodPoint !(JumpInfo blk)
  deriving Generic

deriving instance
  (Typeable blk, HasHeader (Header blk), Eq (Header blk)) => Eq (JumpInstruction blk)
instance (Typeable blk, HasHeader (Header blk), Show (Header blk)) => Show (JumpInstruction blk) where
  showsPrec p = \case
    JumpTo jumpInfo ->
      showParen (p > 10) $ showString "JumpTo " . shows (AF.headPoint $ jTheirFragment jumpInfo)
    JumpToGoodPoint jumpInfo ->
      showParen (p > 10) $ showString "JumpToGoodPoint " . shows (AF.headPoint $ jTheirFragment jumpInfo)

deriving anyclass instance
  ( HasHeader blk
  , LedgerSupportsProtocol blk
  , NoThunks (Header blk)
  ) =>
  NoThunks (JumpInstruction blk)

-- | The result of a jump request, either accepted or rejected.
data JumpResult blk
  = AcceptedJump !(JumpInstruction blk)
  | RejectedJump !(JumpInstruction blk)
  deriving Generic

deriving instance (Typeable blk, HasHeader (Header blk), Eq (Header blk)) => Eq (JumpResult blk)
deriving instance (Typeable blk, HasHeader (Header blk), Show (Header blk)) => Show (JumpResult blk)

deriving anyclass instance
  ( HasHeader blk
  , LedgerSupportsProtocol blk
  , NoThunks (Header blk)
  ) =>
  NoThunks (JumpResult blk)

-- | Compute the next instruction for the given peer. In the majority of cases,
-- this consists in reading the peer's handle, having the dynamo and objector
-- run normally and the jumpers wait for the next jump. As such, this function
-- mostly only reads from and writes to the handle of the peer. For the dynamo, every once in a
-- while, we need to indicate to the jumpers that they need to jump, and this
-- requires writing to a TVar for every jumper.
nextInstruction ::
  MonadSTM m =>
  STM m retry ->
  PeerContext m peer blk ->
  STM m (Strict.Either retry (Instruction blk))
nextInstruction retry_ context =
  readTVar (cschJumping (handle context)) >>= \case
    Disengaged DisengagedDone -> pur RunNormally
    Disengaged Disengaging -> do
      writeTVar (cschJumping (handle context)) (Disengaged DisengagedDone)
      pur Restart
    Dynamo (DynamoStarting goodJumpInfo) lastJumpSlot -> do
      writeTVar (cschJumping (handle context)) $
        Dynamo DynamoStarted lastJumpSlot
      pur $ JumpInstruction $ JumpToGoodPoint goodJumpInfo
    Dynamo DynamoStarted _ ->
      pur $ RunNormally
    Objector Starting goodJump badPoint -> do
      writeTVar (cschJumping (handle context)) $
        Objector Started goodJump badPoint
      pur $ JumpInstruction $ JumpToGoodPoint goodJump
    Objector Started _ _ -> pur RunNormally
    Jumper nextJumpVar jumperState -> do
      readTVar nextJumpVar >>= \case
        Nothing -> Strict.Left <$> retry_
        Just jumpInfo -> do
          writeTVar nextJumpVar Nothing
          case jumperState of
            Happy FreshJumper mGoodJumpInfo ->
              writeTVar (cschJumping (handle context)) $
                Jumper nextJumpVar $
                  Happy StartedJumper mGoodJumpInfo
            _ -> pure ()
          pur $ JumpInstruction $ JumpTo jumpInfo
 where
  pur = pure . Strict.Right

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
onRollForward ::
  forall m peer blk.
  ( MonadSTM m
  , LedgerSupportsProtocol blk
  ) =>
  PeerContext m peer blk ->
  Point (Header blk) ->
  STM m (Maybe (TraceEventCsj peer blk))
onRollForward context point =
  readTVar (cschJumping (handle context)) >>= \case
    Objector _ _ badPoint
      | badPoint == castPoint point -> do
          disengage (handle context)
          (Just . ($ BecauseCsjDisengage)) <$> electNewObjector (stripContext context)
      | otherwise -> pure Nothing
    Disengaged{} -> pure Nothing
    Jumper{} -> pure Nothing
    Dynamo _ lastJumpSlot
      | let jumpBoundaryPlus1 = jumpSize context + succWithOrigin lastJumpSlot
      , succWithOrigin (pointSlot point) > jumpBoundaryPlus1 -> do
          mJumpInfo <- readTVar (cschJumpInfo (handle context))
          setJumps mJumpInfo
      | otherwise -> pure Nothing
 where
  setJumps Nothing = error "onRollForward: Dynamo without jump info"
  setJumps (Just jumpInfo) = do
    writeTVar (cschJumping (handle context)) $
      Dynamo DynamoStarted $
        AF.headSlot $
          jTheirFragment jumpInfo
    handles <- cschcSeq (handlesCol context)
    forM_ handles $ \(_, h) ->
      readTVar (cschJumping h) >>= \case
        Jumper nextJumpVar Happy{} -> writeTVar nextJumpVar (Just jumpInfo)
        _ -> pure ()
    pure $
      Just $
        SentJumpInstruction $
          castPoint $
            AF.headPoint $
              jTheirFragment jumpInfo

-- | This function is called when we receive a 'MsgRollBackward' message.
--
-- Here we check if the peer is trying to roll back to a point before the last
-- jump. If so, we disengage the peer. This prevents adversaries from sending
-- as objectors the same chain as the dynamo.
onRollBackward ::
  forall m peer blk.
  ( MonadSTM m
  , Eq peer
  , LedgerSupportsProtocol blk
  ) =>
  PeerContext m peer blk ->
  WithOrigin SlotNo ->
  STM m (Maybe (TraceEventCsj peer blk))
onRollBackward context slot =
  readTVar (cschJumping (handle context)) >>= \case
    Objector _ _ badPoint
      | slot < pointSlot badPoint -> do
          disengage (handle context)
          (Just . ($ BecauseCsjDisengage)) <$> electNewObjector (stripContext context)
      | otherwise -> pure Nothing
    Disengaged{} -> pure Nothing
    Jumper{} -> pure Nothing
    Dynamo _ lastJumpSlot
      | slot < lastJumpSlot -> do
          disengage (handle context)
          (Just . ($ BecauseCsjDisengage) . fst) <$> backfillDynamo (stripContext context)
      | otherwise -> pure Nothing

-- | This function is called when we receive a 'MsgAwaitReply' message.
--
-- If this is the dynamo, we need to elect a new dynamo as no more headers
-- are available.
onAwaitReply ::
  ( MonadSTM m
  , Eq peer
  , LedgerSupportsProtocol blk
  ) =>
  PeerContext m peer blk ->
  STM m (Maybe (TraceEventCsj peer blk))
onAwaitReply context =
  readTVar (cschJumping (handle context)) >>= \case
    Dynamo{} -> do
      disengage (handle context)
      (Just . ($ BecauseCsjDisengage) . fst) <$> backfillDynamo (stripContext context)
    Objector{} -> do
      disengage (handle context)
      (Just . ($ BecauseCsjDisengage)) <$> electNewObjector (stripContext context)
    Jumper{} ->
      -- A jumper might be receiving a 'MsgAwaitReply' message if it was
      -- previously an objector and a new dynamo was elected.
      unitNothing <$> disengage (handle context)
    Disengaged{} ->
      pure Nothing

-- | Process the result of a jump. In the happy case, this only consists in
-- updating the peer's handle to take the new candidate fragment and the new
-- last jump point into account. When disagreeing with the dynamo, though, we
-- enter a phase of several jumps to pinpoint exactly where the disagreement
-- occurs. Once this phase is finished, we trigger the election of a new
-- objector, which might update many TVars.
processJumpResult ::
  forall m peer blk.
  ( MonadSTM m
  , Eq peer
  , LedgerSupportsProtocol blk
  ) =>
  PeerContext m peer blk ->
  JumpResult blk ->
  STM m (Maybe (TraceEventCsj peer blk))
processJumpResult context jumpResult =
  readTVar (cschJumping (handle context)) >>= \case
    Dynamo{} ->
      case jumpResult of
        AcceptedJump (JumpToGoodPoint jumpInfo) ->
          unitNothing <$> updateChainSyncState (handle context) jumpInfo
        RejectedJump JumpToGoodPoint{} -> do
          startDisengaging (handle context)
          (Just . ($ BecauseCsjDisengage) . fst) <$> backfillDynamo (stripContext context)

        -- Not interesting in the dynamo state
        AcceptedJump JumpTo{} -> pure Nothing
        RejectedJump JumpTo{} -> pure Nothing
    Disengaged{} -> pure Nothing
    Objector{} ->
      case jumpResult of
        AcceptedJump (JumpToGoodPoint jumpInfo) ->
          unitNothing <$> updateChainSyncState (handle context) jumpInfo
        RejectedJump JumpToGoodPoint{} -> do
          -- If the objector rejects a good point, it is a sign of a rollback
          -- to earlier than the last jump.
          startDisengaging (handle context)
          (Just . ($ BecauseCsjDisengage)) <$> electNewObjector (stripContext context)

        -- Not interesting in the objector state
        AcceptedJump JumpTo{} -> pure Nothing
        RejectedJump JumpTo{} -> pure Nothing
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
            Happy StartedJumper _mGoodJumpInfo -> do
              writeTVar (cschJumping (handle context)) $
                Jumper nextJumpVar $
                  Happy StartedJumper $
                    Just goodJumpInfo
              pure Nothing
            Happy FreshJumper _mGoodJumpInfo ->
              pure Nothing
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
              pure Nothing
            FoundIntersection{} ->
              error
                "processJumpResult (rejected): Jumpers in state FoundIntersection shouldn't be further jumping."
        -- These aren't interesting in the case of jumpers.
        AcceptedJump JumpToGoodPoint{} -> pure Nothing
        RejectedJump JumpToGoodPoint{} -> pure Nothing
 where
  -- Avoid redundant constraint "HasHeader blk" reported by some ghc's
  _ = getHeaderFields @blk

  updateChainSyncState :: ChainSyncClientHandle m blk -> JumpInfo blk -> STM m ()
  updateChainSyncState handle jump = do
    let fragment = jTheirFragment jump
    modifyTVar (cschState handle) $ \csState ->
      csState{csCandidate = fragment, csLatestSlot = SJust (AF.headSlot fragment)}
    writeTVar (cschJumpInfo handle) $ Just jump

  mkGoodJumpInfo :: Maybe (JumpInfo blk) -> JumpInfo blk -> JumpInfo blk
  mkGoodJumpInfo mGoodJumpInfo badJumpInfo = do
    let badFragment = jTheirFragment badJumpInfo
        -- use the jump info of the rejected jump if the good jump info is
        -- not available (i.e. there were no accepted jumps)
        badFragmentStart = AF.takeOldest 0 badFragment
     in fromMaybe (badJumpInfo{jTheirFragment = badFragmentStart}) mGoodJumpInfo

  -- \| Given a good point (where we know we agree with the dynamo) and a bad
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
    if len <= 1
      then do
        -- If the fragment only contains the bad tip, we know the
        -- intersection is the good point.
        -- Clear any subsequent jumps requested by the dynamo.
        writeTVar nextJumpVar Nothing
        maybeElectNewObjector nextJumpVar goodJumpInfo (AF.castPoint $ AF.headPoint badFragment)
      else do
        let theirFragment = AF.dropNewest (len `div` 2) badFragment
        writeTVar nextJumpVar $
          Just
            badJumpInfo{jTheirFragment = theirFragment}
        writeTVar (cschJumping (handle context)) $
          Jumper nextJumpVar (LookingForIntersection goodJumpInfo badJumpInfo)
        pure Nothing

  maybeElectNewObjector ::
    StrictTVar m (Maybe (JumpInfo blk)) ->
    JumpInfo blk ->
    Point (Header blk) ->
    STM m (Maybe (TraceEventCsj peer blk))
  maybeElectNewObjector nextJumpVar goodJumpInfo badPoint = do
    findObjector (stripContext context) >>= \case
      Nothing -> do
        -- There is no objector yet. Promote the jumper to objector.
        writeTVar (cschJumping (handle context)) (Objector Starting goodJumpInfo badPoint)
        pure $ Just $ BecomingObjector Nothing
      Just (oPeerId, oInitState, oGoodJump, oBadPoint, oHandle)
        | pointSlot oBadPoint <= pointSlot badPoint -> do
            -- The objector's intersection is still old enough. Keep it.
            writeTVar (cschJumping (handle context)) $
              Jumper nextJumpVar (FoundIntersection Starting goodJumpInfo badPoint)
            pure Nothing
        | otherwise -> do
            -- Found an earlier intersection. Demote the old objector and
            -- promote the jumper to objector.
            newJumper Nothing (FoundIntersection oInitState oGoodJump oBadPoint)
              >>= writeTVar (cschJumping oHandle)
            writeTVar (cschJumping (handle context)) (Objector Starting goodJumpInfo badPoint)
            pure $ Just $ BecomingObjector (Just oPeerId)

updateJumpInfo ::
  MonadSTM m =>
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
  MonadSTM m =>
  ChainSyncClientHandleCollection peer m blk ->
  STM m (Maybe (peer, ChainSyncClientHandle m blk))
getDynamo handlesCol = do
  handles <- cschcSeq handlesCol
  findM (\(_, handle) -> isDynamo <$> readTVar (cschJumping handle)) handles
 where
  isDynamo Dynamo{} = True
  isDynamo _ = False

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
  ( MonadSTM m
  , LedgerSupportsProtocol blk
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
--
-- @Note [Updating the CSJ State when the GSM State Changes]@:
--
-- The 'GsmState' argument to this function is the only way that the state of
-- the GSM influences CSJ. In particular, when the GSM state changes, the CSJ
-- state does not need any updates whatsoever. That is remarkable enough to
-- deserve some explanation.
--
-- - The 'GsmState' argument to this function merely causes a new client to be
--   immediately disengaged if the GSM is currently in 'GSM.CaughtUp'.
--   Otherwise, CSJ will initialize that peer as a Jumper instead of running
--   full ChainSync (unless they happen to be immediately promoted to Dynamo,
--   eg they're the first upstream peer).
--
-- - The transition into 'GSM.CaughtUp' does not raise any design questions.
--   The GSM only makes that transition when all peers are idle, and an idle
--   peer will have already disengaged from CSJ. So CSJ doesn't need to react
--   to this transition.
--
-- - The GSM only transitions out of 'GSM.CaughtUp' if the tip of its selection
--   is much older than expected (eg 20 minutes). There are many possible
--   explanations for why that could have happened, so it's not obvious what is
--   the best reaction to that transition. This is the interesting case.
--
-- The relevant high-level assumption is that in the moment the GSM exits the
-- 'GSM.CaughtUp' state, either (i) the node has no proper upstream peers or
-- (ii) the node's selection is out-of-date but not by a huge amount.
--
-- - If the node has no peers, then the CSJ state doesn't need any updates: all
--   of its state is peer-specific. This is anticipated as the main reason the
--   CSJ will leave 'GSM.CaughtUp': eg when the node process was asleep because
--   the user closed the laptop lid overnight.
--
-- - If the node still has peers, then note that they are already disengaged
--   from CSJ, since the GSM was in 'GSM.CaughtUp'. The only reason to
--   re-engage them would be to prevent unnecessary load on them. The key
--   design decision here is that the potential load the node's current peers
--   might be able to avoid if they re-engage CSJ from is not worth the extra
--   complexity in CSJ. It's only ~20min worth of ChainSync headers. And if the
--   node hadn't been, eg, asleep last ~20min, those peers would have all sent
--   those headers anyway---the only difference is that the load arrives in a
--   burst.
--
-- One key remark: the transition out of 'GSM.CaughtUp' does (elsewhere)
-- re-enable the LoP, the LoE, and the GDD, and they apply to all peers
-- regardless of whether those peers are disengaged from CSJ. So security is
-- not directly relevant to this question---recall that CSJ is merely an
-- optimization to avoid excess load on honest upstream peers.
registerClient ::
  ( LedgerSupportsProtocol blk
  , IOLike m
  ) =>
  -- | the GSM state as of when the node connected to the upstream peer
  GsmState ->
  Context m peer blk ->
  peer ->
  StrictTVar m (ChainSyncState blk) ->
  -- | A function to make a client handle from a jumping state.
  (StrictTVar m (ChainSyncJumpingState m blk) -> ChainSyncClientHandle m blk) ->
  STM m (PeerContext m peer blk, Maybe (TraceEventCsj peer blk))
registerClient gsmState context peer csState mkHandle = do
  (csjState, mbEv) <- case gsmState of
    GSM.CaughtUp -> pure (Disengaged DisengagedDone, Nothing)
    -- This branch disables CSJ while the GSM is in the CaughtUp state.
    GSM.PreSyncing -> engageClient context csState
    GSM.Syncing -> engageClient context csState
  cschJumping <- newTVar csjState
  let handle = mkHandle cschJumping
  cschcAddHandle (handlesCol context) peer handle
  pure (context{peer, handle}, mbEv)

-- | A helper for 'registerClient'
--
-- /NOT EXPORTED/
engageClient ::
  ( LedgerSupportsProtocol blk
  , IOLike m
  ) =>
  Context m peer blk ->
  StrictTVar m (ChainSyncState blk) ->
  STM m (ChainSyncJumpingState m blk, Maybe (TraceEventCsj peer blk))
engageClient context csState = do
  getDynamo (handlesCol context) >>= \case
    Nothing -> do
      fragment <- csCandidate <$> readTVar csState
      pure (Dynamo DynamoStarted $ pointSlot $ AF.anchorPoint fragment, Just InitializedAsDynamo)
    Just (_, handle) -> do
      mJustInfo <- readTVar (cschJumpInfo handle)
      (\x -> (x, Nothing)) <$> newJumper mJustInfo (Happy FreshJumper Nothing)

-- | Unregister a client from a 'PeerContext'; this might trigger the election
-- of a new dynamo or objector if the peer was one of these two.
unregisterClient ::
  ( MonadSTM m
  , Ord peer
  , LedgerSupportsProtocol blk
  ) =>
  PeerContext m peer blk ->
  STM m (Maybe (TraceEventCsj peer blk))
unregisterClient context = do
  cschcRemoveHandle (handlesCol context) (peer context)
  let context' = stripContext context
  readTVar (cschJumping (handle context)) >>= \case
    Disengaged{} -> pure Nothing
    Jumper{} -> pure Nothing
    Objector{} -> (Just . ($ BecauseCsjDisconnect)) <$> electNewObjector context'
    Dynamo{} -> (Just . ($ BecauseCsjDisconnect) . fst) <$> backfillDynamo context'

-- | Elects a new dynamo by demoting the given dynamo (and the objector if there
-- is one) to a jumper, moving the peer to the end of the queue of chain sync
-- handles and electing a new dynamo.
--
-- It does nothing if there is no other engaged peer to elect or if the given
-- peer is not the dynamo.
rotateDynamo ::
  ( Ord peer
  , LedgerSupportsProtocol blk
  , MonadSTM m
  ) =>
  Tracer m (TraceEventDbf peer) ->
  ChainSyncClientHandleCollection peer m blk ->
  peer ->
  m ()
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
--
-- Prefer an 'Objector' that has already 'Started'. Such a peer can trivially
-- transition to be the Dynamo, without any disruption to their ChainSync
-- state. Moreover, if that Objector is honest, then their being the new Dynamo
-- prevents the possibility of their candidate chain being lost and having to
-- eventually be re-downloaded, which CSJ ought to avoid.
backfillDynamo ::
  ( MonadSTM m
  , Eq peer
  , LedgerSupportsProtocol blk
  ) =>
  Context m peer blk ->
  STM m (TraceCsjReason -> TraceEventCsj peer blk, Maybe (peer, ChainSyncClientHandle m blk))
backfillDynamo context = do
  peerStates <- cschcSeq (handlesCol context)
  mDynamo <- do
    -- prefer a 'Started' 'Objector', if any exists
    findObjector context >>= \case
      Just (oId, Started, _oGoodJI, _oBad, oHandle) ->
        pure $ Just $ (oId, oHandle)
      _ ->
        findNonDisengaged peerStates
  case mDynamo of
    Nothing -> pure (NoLongerDynamo Nothing, Nothing)
    Just (dynId, dynamo) -> do
      promoteToDynamo peerStates dynId dynamo
      pure (NoLongerDynamo (Just dynId), Just (dynId, dynamo))

-- | Promote the given peer to dynamo and demote all other peers to jumpers.
promoteToDynamo ::
  ( MonadSTM m
  , Eq peer
  , LedgerSupportsProtocol blk
  ) =>
  StrictSeq (peer, ChainSyncClientHandle m blk) ->
  peer ->
  ChainSyncClientHandle m blk ->
  STM m ()
promoteToDynamo peerStates dynId dynamo = do
  mJumpInfo <- readTVar (cschJumpInfo dynamo)
  jumping' <-
    readTVar (cschJumping dynamo) >>= \case
      -- An 'Objector' that already 'Started' need not be disrupted.
      --
      -- Remark. Intuitively, a 'Starting' 'Objector' also need not be disrupted,
      -- but disrupting it wouldn't waste any @MsgRollForward@s. More concretely,
      -- it's not obvious how to build a 'DynamoStarting' from a 'Starting'.
      Objector Started oGoodJI _oBad -> do
        -- This intersection point is necessarily behind the replaced Dynamos's
        -- latest jump instruction, but its relative age is bounded.
        let islot = AF.headSlot $ jTheirFragment oGoodJI
        pure $ Dynamo DynamoStarted islot
      -- Otherwise, the peer being promoted could be a Jumper or an Objector
      -- Starting, but never Dynamo nor Disengaged.
      _ -> do
        fragment <- csCandidate <$> readTVar (cschState dynamo)
        -- If there is no jump info, the dynamo must be just starting and
        -- there is no need to set the intersection of the ChainSync server.
        let dynamoInitState = maybe DynamoStarted DynamoStarting mJumpInfo
            slot = AF.headSlot fragment
        pure $ Dynamo dynamoInitState slot
  writeTVar (cschJumping dynamo) jumping'

  -- Demote all other peers to jumpers
  forM_ peerStates $ \(peer, st) ->
    when (peer /= dynId) $ do
      jumpingState <- readTVar (cschJumping st)
      when (not (isDisengaged jumpingState)) $
        newJumper mJumpInfo (Happy FreshJumper Nothing)
          >>= writeTVar (cschJumping st)

-- | Find a non-disengaged peer in the given sequence
findNonDisengaged ::
  MonadSTM m =>
  StrictSeq (peer, ChainSyncClientHandle m blk) ->
  STM m (Maybe (peer, ChainSyncClientHandle m blk))
findNonDisengaged =
  findM $ \(_, st) -> not . isDisengaged <$> readTVar (cschJumping st)

isDisengaged :: ChainSyncJumpingState m blk -> Bool
isDisengaged Disengaged{} = True
isDisengaged _ = False

-- | Find the objector in a context, if there is one.
findObjector ::
  MonadSTM m =>
  Context m peer blk ->
  STM
    m
    (Maybe (peer, ObjectorInitState, JumpInfo blk, Point (Header blk), ChainSyncClientHandle m blk))
findObjector context =
  cschcSeq (handlesCol context) >>= go
 where
  go Seq.Empty = pure Nothing
  go ((peer, handle) Seq.:<| xs) =
    readTVar (cschJumping handle) >>= \case
      Objector initState goodJump badPoint ->
        pure $ Just (peer, initState, goodJump, badPoint, handle)
      _ -> go xs

-- | Look into all dissenting jumper and promote the one with the oldest
-- intersection with the dynamo as the new objector.
electNewObjector ::
  MonadSTM m =>
  Context m peer blk ->
  STM m (TraceCsjReason -> TraceEventCsj peer blk)
electNewObjector context =
  NoLongerObjector <$> do
    peerStates <- toList <$> cschcSeq (handlesCol context)
    dissentingJumpers <- collectDissentingJumpers peerStates
    let sortedJumpers = sortOn (pointSlot . fst . snd) dissentingJumpers
    case sortedJumpers of
      [] -> pure Nothing
      (peer, (badPoint, (initState, goodJumpInfo, handle))) : _ -> do
        writeTVar (cschJumping handle) $ Objector initState goodJumpInfo badPoint
        pure $ Just peer
 where
  collectDissentingJumpers peerStates =
    fmap catMaybes $
      forM peerStates $ \(peer, handle) ->
        readTVar (cschJumping handle) >>= \case
          Jumper _ (FoundIntersection initState goodJumpInfo badPoint) ->
            pure $ Just (peer, (badPoint, (initState, goodJumpInfo, handle)))
          _ ->
            pure Nothing

-- | Events due to the centralized Devoted BlockFetch logic
data TraceEventDbf peer
  = RotatedDynamo peer peer
  deriving Show

-- | Events arising from a specific ChainSync client
data TraceEventCsj peer blk
  = -- | previous objector
    BecomingObjector (Maybe peer)
  | BlockedOnJump
  | InitializedAsDynamo
  | -- | new dynamo if known
    NoLongerDynamo (Maybe peer) TraceCsjReason
  | -- | new objector if known
    NoLongerObjector (Maybe peer) TraceCsjReason
  | -- | jump target
    SentJumpInstruction (Point blk)
  deriving Show

data TraceCsjReason
  = BecauseCsjDisengage
  | BecauseCsjDisconnect
  deriving Show

unitNothing :: () -> Maybe a
unitNothing () = Nothing
