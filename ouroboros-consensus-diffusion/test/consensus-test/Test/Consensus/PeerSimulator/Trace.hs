{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

-- | Helpers for tracing used by the peer simulator.
module Test.Consensus.PeerSimulator.Trace (
    mkCdbTracer
  , mkChainSyncClientTracer
  , mkGDDTracer
  , prettyTime
  , traceLinesWith
  , traceUnitWith
  ) where

import           Control.Tracer (Tracer (Tracer), traceWith)
import           Data.List (intercalate)
import qualified Data.Map as Map
import           Data.Time.Clock (diffTimeToPicoseconds)
import           Ouroboros.Consensus.Block (GenesisWindow (..), Header,
                     fromWithOrigin, succWithOrigin)
import           Ouroboros.Consensus.Genesis.Governor (DensityBounds (..),
                     TraceGDDEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (TraceChainSyncClientEvent (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB.Impl
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
                     (TraceAddBlockEvent (..))
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.IOLike (IOLike, MonadMonotonicTime,
                     Time (Time), getMonotonicTime)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (SlotNo (SlotNo), castPoint, unSlotNo)
import           Test.Consensus.PointSchedule.Peers (PeerId)
import           Test.Util.TersePrinting (terseAnchor, terseHFragment,
                     terseHeader, tersePoint, terseRealPoint)
import           Test.Util.TestBlock (TestBlock)
import           Text.Printf (printf)

mkCdbTracer ::
  IOLike m =>
  Tracer m String ->
  Tracer m (ChainDB.Impl.TraceEvent TestBlock)
mkCdbTracer tracer =
  Tracer $ \case
    ChainDB.Impl.TraceAddBlockEvent event ->
      case event of
        AddedToCurrentChain _ _ _ newFragment ->
          trace $ "Added to current chain; now: " ++ terseHFragment newFragment
        SwitchedToAFork _ _ _ newFragment ->
          trace $ "Switched to a fork; now: " ++ terseHFragment newFragment
        StoreButDontChange point ->
          trace $ "Did not add block due to LoE: " ++ terseRealPoint point
        IgnoreBlockOlderThanK point ->
          trace $ "Ignored block older than k: " ++ terseRealPoint point
        ChainSelectionLoEDebug curChain loeFrag0 -> do
          trace $ "Current chain: " ++ terseHFragment curChain
          trace $ "LoE fragment: " ++ terseHFragment loeFrag0

        _ -> pure ()
    _ -> pure ()
  where
    trace = traceUnitWith tracer "ChainDB"

mkChainSyncClientTracer ::
  IOLike m =>
  Tracer m String ->
  Tracer m (TraceChainSyncClientEvent TestBlock)
mkChainSyncClientTracer tracer =
  Tracer $ \case
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
    _ -> pure ()
  where
    trace = traceUnitWith tracer "ChainSyncClient"

mkGDDTracer ::
  Tracer m String ->
  Tracer m (TraceGDDEvent PeerId TestBlock)
mkGDDTracer tracer =
  Tracer $ \TraceGDDEvent {sgen = GenesisWindow sgen, bounds, candidateSuffixes, losingPeers, loeHead} ->
    traceWith tracer $ unlines ([
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
      ])
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

-- | Trace using the given tracer, printing the current time (typically the time
-- of the simulation) and the unit name.
traceUnitWith :: Tracer m String -> String -> String -> m ()
traceUnitWith tracer unit msg =
  traceWith tracer $ printf "%s | %s" unit msg

traceLinesWith ::
  Tracer m String ->
  [String] ->
  m ()
traceLinesWith tracer = traceWith tracer . unlines
