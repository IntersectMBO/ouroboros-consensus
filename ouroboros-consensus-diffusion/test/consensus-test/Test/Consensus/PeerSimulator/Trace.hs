{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
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
import           Ouroboros.Consensus.Genesis.Governor (DensityBounds (..),
                     TraceGDDEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (TraceChainSyncClientEvent (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB.Impl
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
                     (SelectionChangedInfo (..), TraceAddBlockEvent (..))
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike, MonadMonotonicTime,
                     Time (Time), getMonotonicTime)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Test.Consensus.PointSchedule.Peers (PeerId)
import           Test.Util.TersePrinting (terseHFragment, tersePoint,
                     terseRealPoint)
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
        AddedToCurrentChain _ SelectionChangedInfo {newTipPoint} _ _ -> do
          trace "Added to current chain"
          trace $ "New tip: " ++ terseRealPoint newTipPoint
        SwitchedToAFork _ SelectionChangedInfo {newTipPoint} _ newFragment -> do
          trace "Switched to a fork"
          trace $ "New tip: " ++ terseRealPoint newTipPoint
          trace $ "New fragment: " ++ terseHFragment newFragment
        StoreButDontChange block -> do
          trace "Did not add block due to LoE"
          trace $ "Block: " ++ condense block
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
    _ -> pure ()
  where
    trace = traceUnitWith tracer "ChainSyncClient"

mkGDDTracer ::
  IOLike m =>
  Tracer m String ->
  Tracer m (TraceGDDEvent PeerId TestBlock)
mkGDDTracer tracer =
  Tracer $ \TraceGDDEvent {bounds, candidateSuffixes, losingPeers, loeHead} ->
    traceWith tracer $ unlines [
      "GDG | Density bounds: " ++ showPeers (showBounds <$> bounds),
      "      New candidate tips: " ++ showPeers (show <$> Map.map AF.headPoint candidateSuffixes),
      "      Losing peers: " ++ show losingPeers,
      "      Setting loeFrag: " ++ show loeHead
      ]
  where
    showBounds DensityBounds {offersMoreThanK, lowerBound, upperBound} =
      show lowerBound ++ "/" ++ show upperBound ++ "[" ++ (if offersMoreThanK then "+" else " ") ++ "]"

    showPeers :: Map.Map PeerId String -> String
    showPeers = intercalate ", " . fmap (\ (peer, v) -> show peer ++ " -> " ++ v) . Map.toList

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
