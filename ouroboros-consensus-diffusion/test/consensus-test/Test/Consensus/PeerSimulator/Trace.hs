{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

-- | Helpers for tracing used by the peer simulator.
module Test.Consensus.PeerSimulator.Trace (
    TraceChainSyncClientTerminationEvent (..)
  , TraceEvent (..)
  , TraceScheduledBlockFetchServerEvent (..)
  , TraceScheduledChainSyncServerEvent (..)
  , TraceScheduledServerHandlerEvent (..)
  , TraceSchedulerEvent (..)
  , mkGDDTracerTestBlock
  , mkTracerTestBlock
  , traceLinesWith
  ) where

import           Control.Tracer (Tracer (Tracer), contramap, traceWith)
import           Data.Foldable (for_)
import           Data.List (intersperse)
import qualified Data.Map as Map
import           Data.Time.Clock (DiffTime, diffTimeToPicoseconds)
import           Ouroboros.Consensus.Block (GenesisWindow (..), Header, Point,
                     succWithOrigin)
import           Ouroboros.Consensus.Genesis.Governor (DensityBounds (..),
                     TraceGDDEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (TraceChainSyncClientEvent (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
                     (TraceAddBlockEvent (..))
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.IOLike (IOLike, MonadMonotonicTime,
                     Time (Time), getMonotonicTime)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (SlotNo (SlotNo), Tip, castPoint)
import           Test.Consensus.PointSchedule (NodeState)
import           Test.Consensus.PointSchedule.Peers (Peer (Peer), PeerId)
import           Test.Util.TersePrinting (terseAnchor, terseBlock,
                     terseFragment, terseHFragment, terseHeader, tersePoint,
                     terseRealPoint, terseTip)
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
    -- @0@), the duration of the tick and the states.
    TraceNewTick Int DiffTime (Peer (NodeState blk))

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
  | TraceWaitingForRange (Point blk) (Point blk) [AnchoredFragment blk]
  | TraceSendingBlock blk
  | TraceBatchIsDone
  | TraceBlockPointIsBehind

data TraceChainSyncClientTerminationEvent
  = TraceExceededSizeLimit
  | TraceExceededTimeLimit
  | TraceTerminatedByGDDGovernor
  | TraceTerminatedByLoP

data TraceEvent blk
  = TraceSchedulerEvent (TraceSchedulerEvent blk)
  | TraceScheduledChainSyncServerEvent PeerId (TraceScheduledChainSyncServerEvent (NodeState blk) blk)
  | TraceScheduledBlockFetchServerEvent PeerId (TraceScheduledBlockFetchServerEvent (NodeState blk) blk)
  | TraceChainDBEvent (ChainDB.TraceEvent blk)
  | TraceChainSyncClientEvent PeerId (TraceChainSyncClientEvent blk)
  | TraceChainSyncClientTerminationEvent PeerId TraceChainSyncClientTerminationEvent
  | TraceGenesisDDEvent (TraceGDDEvent PeerId blk)

-- * 'TestBlock'-specific tracers for the peer simulator

mkTracerTestBlock ::
  (IOLike m) =>
  Tracer m String ->
  Tracer m (TraceEvent TestBlock)
mkTracerTestBlock = Tracer . traceEventTestBlockWith

mkGDDTracerTestBlock ::
  Tracer m (TraceEvent TestBlock) ->
  Tracer m (TraceGDDEvent PeerId TestBlock)
mkGDDTracerTestBlock = contramap TraceGenesisDDEvent

traceEventTestBlockWith ::
  (MonadMonotonicTime m) =>
  Tracer m String ->
  TraceEvent TestBlock ->
  m ()
traceEventTestBlockWith tracer = \case
    TraceSchedulerEvent traceEvent -> traceSchedulerEventTestBlockWith tracer traceEvent
    TraceScheduledChainSyncServerEvent peerId traceEvent -> traceScheduledChainSyncServerEventTestBlockWith tracer peerId traceEvent
    TraceScheduledBlockFetchServerEvent peerId traceEvent -> traceScheduledBlockFetchServerEventTestBlockWith tracer peerId traceEvent
    TraceChainDBEvent traceEvent -> traceChainDBEventTestBlockWith tracer traceEvent
    TraceChainSyncClientEvent peerId traceEvent -> traceChainSyncClientEventTestBlockWith peerId tracer traceEvent
    TraceChainSyncClientTerminationEvent peerId traceEvent -> traceChainSyncClientTerminationEventTestBlockWith peerId tracer traceEvent
    TraceGenesisDDEvent gddEvent -> traceWith tracer (terseGDDEvent gddEvent)

traceSchedulerEventTestBlockWith ::
  (MonadMonotonicTime m) =>
  Tracer m String ->
  TraceSchedulerEvent TestBlock ->
  m ()
traceSchedulerEventTestBlockWith tracer = \case
    TraceBeginningOfTime ->
      traceWith tracer "Running point schedule ..."
    TraceEndOfTime ->
      traceLinesWith tracer
        [ hline,
          "Finished running point schedule"
        ]
    TraceNewTick number duration (Peer pid state) -> do
      time <- prettyTime -- TODO: push into a time-specific tracer printing this at new time
      traceLinesWith tracer
        [ hline,
          "Time is " ++ time,
          "Tick:",
          "  number: " ++ show number,
          "  duration: " ++ show duration,
          "  peer: " ++ condense pid,
          "  state: " ++ condense state
        ]
  where
    hline = "--------------------------------------------------------------------------------"

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
    unit = "ScheduledChainSyncServer " ++ condense peerId
    trace = traceUnitWith tracer unit
    traceLines = traceUnitLinesWith tracer unit

traceScheduledBlockFetchServerEventTestBlockWith ::
  Monad m =>
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
    TraceWaitingForRange pointFrom pointTo chains -> do
      trace $ "Waiting for next tick for range: " ++ tersePoint pointFrom ++ " -> " ++ tersePoint pointTo ++ " | chains:"
      for_ chains $ \ c -> trace (terseFragment c)
    TraceSendingBlock block ->
      trace $ "Sending " ++ terseBlock block
    TraceBatchIsDone ->
      trace "Batch is done"
    TraceBlockPointIsBehind ->
      trace "BP is behind"
  where
    unit = "ScheduledBlockFetchServer " ++ condense peerId
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
        ChainSelectionLoEDebug curChain loeFrag0 -> do
          trace $ "Current chain: " ++ terseHFragment curChain
          trace $ "LoE fragment: " ++ terseHFragment loeFrag0
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
  where
    trace = traceUnitWith tracer ("ChainSyncClient " ++ condense pid)

traceChainSyncClientTerminationEventTestBlockWith ::
  PeerId ->
  Tracer m String ->
  TraceChainSyncClientTerminationEvent ->
  m ()
traceChainSyncClientTerminationEventTestBlockWith pid tracer = \case
    TraceExceededSizeLimit ->
      trace "Terminated because of size limit exceeded."
    TraceExceededTimeLimit ->
      trace "Terminated because of time limit exceeded."
    TraceTerminatedByGDDGovernor ->
      trace "Terminated by the GDD governor."
    TraceTerminatedByLoP ->
      trace "Terminated by the limit on patience."
  where
    trace = traceUnitWith tracer ("ChainSyncClient " ++ condense pid)

terseGDDEvent :: TraceGDDEvent PeerId TestBlock -> String
terseGDDEvent = \case
  TraceGDDEvent {sgen = GenesisWindow sgen, bounds, candidateSuffixes, losingPeers, loeHead} ->
    unlines $ [
      "GDG | Window: " ++ window sgen loeHead,
      "      Candidates:"
      ] ++
      showPeers (terseHFragment . fragment <$> bounds) ++
      ["      Density bounds:"] ++
      showPeers (showBounds <$> bounds) ++
      ["      New candidate tips:"] ++
      showPeers (tersePoint . castPoint <$> Map.map AF.headPoint candidateSuffixes) ++
      [
        "      Losing peers: " ++ show losingPeers,
      "      Setting loeFrag: " ++ terseAnchor (AF.castAnchor loeHead)
      ]
  where
    showBounds DensityBounds {fragment, offersMoreThanK, lowerBound, upperBound, hasBlockAfter, lastSlot} =
      show lowerBound ++ "/" ++ show upperBound ++ "[" ++ more ++ "], " ++ lastPoint ++ slot lastSlot ++ block
      where
        more = if offersMoreThanK then "+" else " "

        block = if hasBlockAfter then ", has block" else " "

        lastPoint =
          "point: " ++
          tersePoint (castPoint @(Header TestBlock) @TestBlock (AF.lastPoint fragment)) ++
          ", "

        slot = \case
          Right (SlotNo inCandidate) -> "last: " ++ show inCandidate
          Left (SlotNo forecasted) -> "forecast: " ++ show forecasted

    window sgen loeHead =
      show winStart ++ " -> " ++ show winEnd
      where
        winEnd = winStart + sgen - 1
        SlotNo winStart = succWithOrigin (AF.anchorToSlotNo loeHead)

    showPeers :: Map.Map PeerId String -> [String]
    showPeers = fmap (\ (peer, v) -> "        " ++ condense peer ++ ": " ++ v) . Map.toList

prettyTime :: MonadMonotonicTime m => m String
prettyTime = do
  Time time <- getMonotonicTime
  let ps = diffTimeToPicoseconds time
      milliseconds = ps `quot` 1_000_000_000
      seconds = milliseconds `quot` 1_000
      minutes = seconds `quot` 60
  pure $ printf "%02d:%02d.%03d" minutes (seconds `rem` 60) (milliseconds `rem` 1_000)

traceLinesWith ::
  Tracer m String ->
  [String] ->
  m ()
traceLinesWith tracer = traceWith tracer . mconcat . intersperse "\n"

-- | Trace using the given tracer, printing the current time (typically the time
-- of the simulation) and the unit name.
traceUnitLinesWith :: Tracer m String -> String -> [String] -> m ()
traceUnitLinesWith tracer unit msgs =
  traceLinesWith tracer $ map (printf "%s | %s" unit) msgs

-- | Trace using the given tracer, printing the current time (typically the time
-- of the simulation) and the unit name.
traceUnitWith :: Tracer m String -> String -> String -> m ()
traceUnitWith tracer unit msg = traceUnitLinesWith tracer unit [msg]
