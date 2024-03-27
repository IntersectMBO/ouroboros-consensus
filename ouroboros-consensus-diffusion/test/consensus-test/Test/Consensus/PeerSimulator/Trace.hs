{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NumericUnderscores        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Helpers for tracing used by the peer simulator.
module Test.Consensus.PeerSimulator.Trace (
    TraceBlockFetchClientTerminationEvent (..)
  , TraceChainSyncClientTerminationEvent (..)
  , TraceEvent (..)
  , TraceScheduledBlockFetchServerEvent (..)
  , TraceScheduledChainSyncServerEvent (..)
  , TraceScheduledServerHandlerEvent (..)
  , TraceSchedulerEvent (..)
  , mkGDDTracerTestBlock
  , traceLinesWith
  , tracerTestBlock
  ) where

import           Control.Tracer (Tracer (Tracer), contramap, traceWith)
import           Data.List (intersperse)
import qualified Data.Map as Map
import           Data.Time.Clock (DiffTime, diffTimeToPicoseconds)
import           Ouroboros.Consensus.Block (GenesisWindow (..), Header, Point,
                     WithOrigin (NotOrigin, Origin), succWithOrigin)
import           Ouroboros.Consensus.Genesis.Governor (DensityBounds (..),
                     TraceGDDEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (TraceChainSyncClientEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.Jumping
                     (Instruction (..), JumpInstruction (..), JumpResult (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State
                     (ChainSyncJumpingJumperState (..),
                     ChainSyncJumpingState (..), DynamoInitState (..),
                     JumpInfo (..))
import           Ouroboros.Consensus.Storage.ChainDB.API (LoE (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
                     (TraceAddBlockEvent (..))
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.IOLike (IOLike, MonadMonotonicTime,
                     Time (Time), atomically, getMonotonicTime, readTVarIO,
                     uncheckedNewTVarM, writeTVar)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     headPoint)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (SlotNo (SlotNo), Tip, castPoint)
import           Test.Consensus.PointSchedule (NodeState)
import           Test.Consensus.PointSchedule.Peers (Peer (Peer), PeerId)
import           Test.Util.TersePrinting (terseAnchor, terseBlock,
                     terseFragment, terseHFragment, terseHeader, tersePoint,
                     terseRealPoint, terseTip, terseWithOrigin)
import           Test.Util.TestBlock (TestBlock)
import           Text.Printf (printf)

-- * Trace events for the peer simulator

-- | Trace messages sent by the scheduler.
data TraceSchedulerEvent blk
  = -- | Right before running the first tick (at time @0@) of the schedule.
    TraceBeginningOfTime
  | -- | Right after running the last tick of the schedule.
    TraceEndOfTime
  | -- | When beginning a new tick. Contains the tick number (counting from
    -- @0@), the duration of the tick, the states, the current chain, the
    -- candidate fragment, and the jumping states.
    forall m. TraceNewTick
      Int
      DiffTime
      (Peer (NodeState blk))
      (AnchoredFragment (Header blk))
      (Maybe (AnchoredFragment (Header blk)))
      [(PeerId, ChainSyncJumpingState m blk)]
  | TraceNodeShutdownStart (WithOrigin SlotNo)
  | TraceNodeShutdownComplete
  | TraceNodeStartupStart
  | TraceNodeStartupComplete (AnchoredFragment (Header blk))

type HandlerName = String

data TraceScheduledServerHandlerEvent state blk
  = TraceHandling HandlerName state
  | TraceRestarting HandlerName
  | TraceDoneHandling HandlerName

data TraceScheduledChainSyncServerEvent state blk
  = TraceHandlerEventCS (TraceScheduledServerHandlerEvent state blk)
  | TraceLastIntersection (Point blk)
  | TraceClientIsDone
  | TraceIntersectionFound (Point blk)
  | TraceIntersectionNotFound
  | TraceRollForward (Header blk) (Tip blk)
  | TraceRollBackward (Point blk) (Tip blk)
  | TraceChainIsFullyServed
  | TraceIntersectionIsHeaderPoint
  | TraceIntersectionIsStrictAncestorOfHeaderPoint (AnchoredFragment blk)
  | TraceIntersectionIsStrictDescendentOfHeaderPoint

data TraceScheduledBlockFetchServerEvent state blk
  = TraceHandlerEventBF (TraceScheduledServerHandlerEvent state blk)
  | TraceNoBlocks
  | TraceStartingBatch (AnchoredFragment blk)
  | TraceWaitingForRange (Point blk) (Point blk)
  | TraceSendingBlock blk
  | TraceBatchIsDone
  | TraceBlockPointIsBehind

data TraceChainSyncClientTerminationEvent
  = TraceExceededSizeLimitCS
  | TraceExceededTimeLimitCS
  | TraceTerminatedByGDDGovernor
  | TraceTerminatedByLoP

data TraceBlockFetchClientTerminationEvent
  = TraceExceededSizeLimitBF
  | TraceExceededTimeLimitBF

data TraceEvent blk
  = TraceSchedulerEvent (TraceSchedulerEvent blk)
  | TraceScheduledChainSyncServerEvent PeerId (TraceScheduledChainSyncServerEvent (NodeState blk) blk)
  | TraceScheduledBlockFetchServerEvent PeerId (TraceScheduledBlockFetchServerEvent (NodeState blk) blk)
  | TraceChainDBEvent (ChainDB.TraceEvent blk)
  | TraceChainSyncClientEvent PeerId (TraceChainSyncClientEvent blk)
  | TraceChainSyncClientTerminationEvent PeerId TraceChainSyncClientTerminationEvent
  | TraceBlockFetchClientTerminationEvent PeerId TraceBlockFetchClientTerminationEvent
  | TraceGenesisDDEvent (TraceGDDEvent PeerId blk)
  | TraceOther String

-- * 'TestBlock'-specific tracers for the peer simulator

tracerTestBlock ::
  (IOLike m) =>
  Tracer m String ->
  m (Tracer m (TraceEvent TestBlock))
tracerTestBlock tracer0 = do
  -- NOTE: Mostly, we read the traces on a per-tick basis, so it is important
  -- that ticks are visually separated. Also, giving the time on each line can
  -- get quite verbose when most of the time is the same as the beginning of the
  -- tick (in IOSim anyways). So we keep track of the tick time and we prefix
  -- lines by the time only if they differ. This allows seeing the time-based
  -- events (timeouts, LoP) better while keeping the interface uncluttered, and
  -- it behaves well in IO (where it prefixes all lines by the time).
  tickTimeVar <- uncheckedNewTVarM $ Time (-1)
  let setTickTime = atomically . writeTVar tickTimeVar
      tracer = Tracer $ \msg -> do
        time <- getMonotonicTime
        tickTime <- readTVarIO tickTimeVar
        let timeHeader = prettyTime time ++ " "
            prefix = if time /= tickTime
              then timeHeader
              else replicate (length timeHeader) ' '
        traceWith tracer0 $ concat $ intersperse "\n" $ map (prefix ++) $ lines msg
  pure $ Tracer $ traceEventTestBlockWith setTickTime tracer0 tracer

mkGDDTracerTestBlock ::
  Tracer m (TraceEvent TestBlock) ->
  Tracer m (TraceGDDEvent PeerId TestBlock)
mkGDDTracerTestBlock = contramap TraceGenesisDDEvent

traceEventTestBlockWith ::
  (MonadMonotonicTime m) =>
  (Time -> m ()) ->
  Tracer m String ->
  -- ^ Underlying, non-time- and tick-aware tracer. To be used only with lines
  -- that should not be prefixed by time.
  Tracer m String ->
  -- ^ Normal, time- and tick-aware tracer. Should be used by default.
  TraceEvent TestBlock ->
  m ()
traceEventTestBlockWith setTickTime tracer0 tracer = \case
    TraceSchedulerEvent traceEvent -> traceSchedulerEventTestBlockWith setTickTime tracer0 tracer traceEvent
    TraceScheduledChainSyncServerEvent peerId traceEvent -> traceScheduledChainSyncServerEventTestBlockWith tracer peerId traceEvent
    TraceScheduledBlockFetchServerEvent peerId traceEvent -> traceScheduledBlockFetchServerEventTestBlockWith tracer peerId traceEvent
    TraceChainDBEvent traceEvent -> traceChainDBEventTestBlockWith tracer traceEvent
    TraceChainSyncClientEvent peerId traceEvent -> traceChainSyncClientEventTestBlockWith peerId tracer traceEvent
    TraceChainSyncClientTerminationEvent peerId traceEvent -> traceChainSyncClientTerminationEventTestBlockWith peerId tracer traceEvent
    TraceBlockFetchClientTerminationEvent peerId traceEvent -> traceBlockFetchClientTerminationEventTestBlockWith peerId tracer traceEvent
    TraceGenesisDDEvent gddEvent -> traceWith tracer (terseGDDEvent gddEvent)
    TraceOther msg -> traceWith tracer msg

traceSchedulerEventTestBlockWith ::
  (MonadMonotonicTime m) =>
  (Time -> m ()) ->
  Tracer m String ->
  Tracer m String ->
  TraceSchedulerEvent TestBlock ->
  m ()
traceSchedulerEventTestBlockWith setTickTime tracer0 _tracer = \case
    TraceBeginningOfTime ->
      traceWith tracer0 "Running point schedule ..."
    TraceEndOfTime ->
      traceLinesWith tracer0
        [ "╶──────────────────────────────────────────────────────────────────────────────╴",
          "Finished running point schedule"
        ]
    TraceNewTick number duration (Peer pid state) currentChain mCandidateFrag jumpingStates -> do
      time <- getMonotonicTime
      setTickTime time
      traceLinesWith tracer0
        [ "┌──────────────────────────────────────────────────────────────────────────────┐",
          "└─ " ++ prettyTime time,
          "Tick:",
          "  number: " ++ show number,
          "  duration: " ++ show duration,
          "  peer: " ++ condense pid,
          "  state: " ++ condense state,
          "  current chain: " ++ terseHFragment currentChain,
          "  candidate fragment: " ++ maybe "Nothing" terseHFragment mCandidateFrag,
          "  jumping states:\n" ++ traceJumpingStates jumpingStates
        ]
    TraceNodeShutdownStart immTip ->
      traceWith tracer0 ("  Initiating node shutdown with immutable tip at slot " ++ condense immTip)
    TraceNodeShutdownComplete ->
      traceWith tracer0 "  Node shutdown complete"
    TraceNodeStartupStart ->
      traceWith tracer0 "  Initiating node startup"
    TraceNodeStartupComplete selection ->
      traceWith tracer0 ("  Node startup complete with selection " ++ terseHFragment selection)

  where
    traceJumpingStates :: [(PeerId, ChainSyncJumpingState m TestBlock)] -> String
    traceJumpingStates = unlines . map (\(pid, state) -> "    " ++ condense pid ++ ": " ++ traceJumpingState state)

    traceJumpingState :: ChainSyncJumpingState m TestBlock -> String
    traceJumpingState = \case
      Dynamo initState lastJump ->
        let showInitState = case initState of
              DynamoStarting ji -> terseJumpInfo ji
              DynamoStarted     -> "DynamoStarted"
         in unwords ["Dynamo", showInitState, terseWithOrigin show lastJump]
      Objector initState goodJumpInfo badPoint -> unwords
          [ "Objector"
          , show initState
          , terseJumpInfo goodJumpInfo
          , tersePoint (castPoint badPoint)
          ]
      Disengaged initState -> "Disengaged " ++ show initState
      Jumper _ st -> "Jumper _ " ++ traceJumperState st

    traceJumperState :: ChainSyncJumpingJumperState TestBlock -> String
    traceJumperState = \case
      Happy initState mGoodJumpInfo ->
        "Happy " ++ show initState ++ " " ++ maybe "Nothing" terseJumpInfo mGoodJumpInfo
      FoundIntersection initState goodJumpInfo point -> unwords
        [ "(FoundIntersection"
        , show initState
        , terseJumpInfo goodJumpInfo
        , tersePoint $ castPoint point, ")"
        ]
      LookingForIntersection goodJumpInfo badJumpInfo -> unwords
        ["(LookingForIntersection", terseJumpInfo goodJumpInfo, terseJumpInfo badJumpInfo, ")"]

traceScheduledServerHandlerEventTestBlockWith ::
  Tracer m String ->
  String ->
  TraceScheduledServerHandlerEvent (NodeState TestBlock) TestBlock ->
  m ()
traceScheduledServerHandlerEventTestBlockWith tracer unit = \case
    TraceHandling handler state ->
      traceLines
        [ "handling " ++ handler,
          "  state is " ++ condense state
        ]
    TraceRestarting _->
      trace "  cannot serve at this point; waiting for node state and starting again"
    TraceDoneHandling handler ->
      trace $ "done handling " ++ handler
  where
    trace = traceUnitWith tracer unit
    traceLines = traceUnitLinesWith tracer unit

traceScheduledChainSyncServerEventTestBlockWith ::
  Tracer m String ->
  PeerId ->
  TraceScheduledChainSyncServerEvent (NodeState TestBlock) TestBlock ->
  m ()
traceScheduledChainSyncServerEventTestBlockWith tracer peerId = \case
    TraceHandlerEventCS traceEvent -> traceScheduledServerHandlerEventTestBlockWith tracer unit traceEvent
    TraceLastIntersection point ->
      trace $ "  last intersection is " ++ tersePoint point
    TraceClientIsDone ->
      trace "received MsgDoneClient"
    TraceIntersectionNotFound ->
      trace "  no intersection found"
    TraceIntersectionFound point ->
      trace $ "  intersection found: " ++ tersePoint point
    TraceRollForward header tip ->
      traceLines [
        "  gotta serve " ++ terseHeader header,
        "  tip is      " ++ terseTip tip
      ]
    TraceRollBackward point tip ->
      traceLines [
        "  gotta roll back to " ++ tersePoint point,
        "  new tip is      " ++ terseTip tip
      ]
    TraceChainIsFullyServed ->
      trace "  chain has been fully served"
    TraceIntersectionIsHeaderPoint ->
      trace "  intersection is exactly our header point"
    TraceIntersectionIsStrictAncestorOfHeaderPoint fragment ->
      traceLines
        [ "  intersection is before our header point",
          "  fragment ahead: " ++ terseFragment fragment
        ]
    TraceIntersectionIsStrictDescendentOfHeaderPoint ->
      trace "  intersection is further than our header point"
  where
    unit = "ChainSyncServer " ++ condense peerId
    trace = traceUnitWith tracer unit
    traceLines = traceUnitLinesWith tracer unit

traceScheduledBlockFetchServerEventTestBlockWith ::
  Tracer m String ->
  PeerId ->
  TraceScheduledBlockFetchServerEvent (NodeState TestBlock) TestBlock ->
  m ()
traceScheduledBlockFetchServerEventTestBlockWith tracer peerId = \case
    TraceHandlerEventBF traceEvent -> traceScheduledServerHandlerEventTestBlockWith tracer unit traceEvent
    TraceNoBlocks ->
      trace "  no blocks available"
    TraceStartingBatch fragment ->
      trace $ "Starting batch for slice " ++ terseFragment fragment
    TraceWaitingForRange pointFrom pointTo ->
      trace $ "Waiting for next tick for range: " ++ tersePoint pointFrom ++ " -> " ++ tersePoint pointTo
    TraceSendingBlock block ->
      trace $ "Sending " ++ terseBlock block
    TraceBatchIsDone ->
      trace "Batch is done"
    TraceBlockPointIsBehind ->
      trace "BP is behind"
  where
    unit = "BlockFetchServer " ++ condense peerId
    trace = traceUnitWith tracer unit

traceChainDBEventTestBlockWith ::
  (Monad m) =>
  Tracer m String ->
  ChainDB.TraceEvent TestBlock ->
  m ()
traceChainDBEventTestBlockWith tracer = \case
    ChainDB.TraceAddBlockEvent event ->
      case event of
        AddedToCurrentChain _ _ _ newFragment ->
          trace $ "Added to current chain; now: " ++ terseHFragment newFragment
        SwitchedToAFork _ _ _ newFragment ->
          trace $ "Switched to a fork; now: " ++ terseHFragment newFragment
        StoreButDontChange point ->
          trace $ "Did not select block due to LoE: " ++ terseRealPoint point
        IgnoreBlockOlderThanK point ->
          trace $ "Ignored block older than k: " ++ terseRealPoint point
        ChainSelectionLoEDebug curChain (LoEEnabled loeFrag0) -> do
          trace $ "Current chain: " ++ terseHFragment curChain
          trace $ "LoE fragment: " ++ terseHFragment loeFrag0
        ChainSelectionLoEDebug _ LoEDisabled ->
          pure ()
        AddedReprocessLoEBlocksToQueue ->
          trace $ "Requested ChainSel run"
        _ -> pure ()
    _ -> pure ()
  where
    trace = traceUnitWith tracer "ChainDB"

traceChainSyncClientEventTestBlockWith ::
  PeerId ->
  Tracer m String ->
  TraceChainSyncClientEvent TestBlock ->
  m ()
traceChainSyncClientEventTestBlockWith pid tracer = \case
    TraceRolledBack point ->
      trace $ "Rolled back to: " ++ tersePoint point
    TraceFoundIntersection point _ourTip _theirTip ->
      trace $ "Found intersection at: " ++ tersePoint point
    TraceWaitingBeyondForecastHorizon slot ->
      trace $ "Waiting for " ++ show slot ++ " beyond forecast horizon"
    TraceAccessingForecastHorizon slot ->
      trace $ "Accessing " ++ show slot ++ ", previously beyond forecast horizon"
    TraceValidatedHeader header ->
      trace $ "Validated header: " ++ terseHeader header
    TraceDownloadedHeader header ->
      trace $ "Downloaded header: " ++ terseHeader header
    TraceGaveLoPToken didGive header bestBlockNo ->
      trace $
        (if didGive then "Gave" else "Did not give")
        ++ " LoP token to " ++ terseHeader header
        ++ " compared to " ++ show bestBlockNo
    TraceException exception ->
      trace $ "Threw an exception: " ++ show exception
    TraceTermination result ->
      trace $ "Terminated with result: " ++ show result
    TraceOfferJump point ->
      trace $ "Offering jump to " ++ tersePoint point
    TraceJumpResult (AcceptedJump (JumpTo ji)) ->
      trace $ "Accepted jump to " ++ terseJumpInfo ji
    TraceJumpResult (RejectedJump (JumpTo ji)) ->
      trace $ "Rejected jump to " ++ terseJumpInfo ji
    TraceJumpResult (AcceptedJump (JumpToGoodPoint ji)) ->
      trace $ "Accepted jump to good point: " ++ terseJumpInfo ji
    TraceJumpResult (RejectedJump (JumpToGoodPoint ji)) ->
      trace $ "Rejected jump to good point: " ++ terseJumpInfo ji
    TraceJumpingWaitingForNextInstruction ->
      trace "Waiting for next instruction from the jumping governor"
    TraceJumpingInstructionIs instr ->
      trace $ "Received instruction: " ++ showInstr instr
  where
    trace = traceUnitWith tracer ("ChainSyncClient " ++ condense pid)

    showInstr :: Instruction TestBlock -> String
    showInstr = \case
      JumpInstruction (JumpTo ji) -> "JumpTo " ++ terseJumpInfo ji
      JumpInstruction (JumpToGoodPoint ji) -> "JumpToGoodPoint " ++ terseJumpInfo ji
      RunNormally -> "RunNormally"
      Restart -> "Restart"

terseJumpInfo :: JumpInfo TestBlock -> String
terseJumpInfo ji = tersePoint (castPoint $ headPoint $ jTheirFragment ji)

traceChainSyncClientTerminationEventTestBlockWith ::
  PeerId ->
  Tracer m String ->
  TraceChainSyncClientTerminationEvent ->
  m ()
traceChainSyncClientTerminationEventTestBlockWith pid tracer = \case
    TraceExceededSizeLimitCS ->
      trace "Terminated because of size limit exceeded."
    TraceExceededTimeLimitCS ->
      trace "Terminated because of time limit exceeded."
    TraceTerminatedByGDDGovernor ->
      trace "Terminated by the GDD governor."
    TraceTerminatedByLoP ->
      trace "Terminated by the limit on patience."
  where
    trace = traceUnitWith tracer ("ChainSyncClient " ++ condense pid)

traceBlockFetchClientTerminationEventTestBlockWith ::
  PeerId ->
  Tracer m String ->
  TraceBlockFetchClientTerminationEvent ->
  m ()
traceBlockFetchClientTerminationEventTestBlockWith pid tracer = \case
    TraceExceededSizeLimitBF ->
      trace "Terminated because of size limit exceeded."
    TraceExceededTimeLimitBF ->
      trace "Terminated because of time limit exceeded."
  where
    trace = traceUnitWith tracer ("BlockFetchClient " ++ condense pid)

-- * Other utilities
terseGDDEvent :: TraceGDDEvent PeerId TestBlock -> String
terseGDDEvent = \case
  TraceGDDEvent {sgen = GenesisWindow sgen, curChain, bounds, candidates, candidateSuffixes, losingPeers, loeHead} ->
    unlines $ [
      "GDD | Window: " ++ window sgen loeHead,
      "      Selection: " ++ terseHFragment curChain,
      "      Candidates:"
      ] ++
      showPeers (tersePoint . castPoint . AF.headPoint <$> candidates) ++
      [
      "      Candidate suffixes (bounds):"
      ] ++
      showPeers (terseHFragment . clippedFragment <$> bounds) ++
      ["      Density bounds:"] ++
      showPeers (showBounds <$> bounds) ++
      ["      New candidate tips:"] ++
      showPeers (tersePoint . castPoint <$> Map.map AF.headPoint candidateSuffixes) ++
      [
        "      Losing peers: " ++ show losingPeers,
      "      Setting loeFrag: " ++ terseAnchor (AF.castAnchor loeHead)
      ]
  where
    showBounds DensityBounds {clippedFragment, offersMoreThanK, lowerBound, upperBound, hasBlockAfter, latestSlot, idling} =
      show lowerBound ++ "/" ++ show upperBound ++ "[" ++ more ++ "], " ++
      lastPoint ++ "latest: " ++ showLatestSlot latestSlot ++ block ++ showIdling
      where
        more = if offersMoreThanK then "+" else " "

        block = if hasBlockAfter then ", has header after sgen" else " "

        -- Note: At some point, I changed this to use @headPoint@ erroneously, so to be clear about what this signifies:
        -- The first point after the anchor (which is returned by @lastPoint@, clearly) is used for the condition that
        -- the density comparison should not be applied to two peers if they share any headers after the LoE fragment.
        lastPoint =
          "point: " ++
          tersePoint (castPoint @(Header TestBlock) @TestBlock (AF.lastPoint clippedFragment)) ++
          ", "

        showLatestSlot = \case
          Origin -> "unknown"
          NotOrigin (SlotNo slot) -> show slot

        showIdling | idling = ", idling"
                   | otherwise = ""

    window sgen loeHead =
      show winStart ++ " -> " ++ show winEnd
      where
        winEnd = winStart + sgen - 1
        SlotNo winStart = succWithOrigin (AF.anchorToSlotNo loeHead)

    showPeers :: Map.Map PeerId String -> [String]
    showPeers = fmap (\ (peer, v) -> "        " ++ condense peer ++ ": " ++ v) . Map.toList

prettyTime :: Time -> String
prettyTime (Time time) =
  let ps = diffTimeToPicoseconds time
      milliseconds = ps `quot` 1_000_000_000
      seconds = milliseconds `quot` 1_000
      minutes = seconds `quot` 60
  in printf "%02d:%02d.%03d" minutes (seconds `rem` 60) (milliseconds `rem` 1_000)

traceLinesWith ::
  Tracer m String ->
  [String] ->
  m ()
traceLinesWith tracer = traceWith tracer . mconcat . intersperse "\n"

-- Not really the maximum length, just a quick hack for a smoother display
maxUnitLength :: Int
maxUnitLength = length "BlockFetchServer adversary 9"

padUnit :: String -> String
padUnit unit = unit ++ replicate (maxUnitLength - length unit) ' '

-- | Trace using the given tracer, printing the current time (typically the time
-- of the simulation) and the unit name.
traceUnitLinesWith :: Tracer m String -> String -> [String] -> m ()
traceUnitLinesWith tracer unit msgs =
  traceLinesWith tracer $ map (printf "%s | %s" $ padUnit unit) msgs

-- | Trace using the given tracer, printing the current time (typically the time
-- of the simulation) and the unit name.
traceUnitWith :: Tracer m String -> String -> String -> m ()
traceUnitWith tracer unit msg = traceUnitLinesWith tracer unit [msg]
