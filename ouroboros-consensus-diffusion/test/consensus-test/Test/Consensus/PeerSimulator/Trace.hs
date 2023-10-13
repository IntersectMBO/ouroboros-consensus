{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Helpers for tracing used by the peer simulator.
module Test.Consensus.PeerSimulator.Trace (
    mkCdbTracer
  , mkChainSyncClientTracer
  , traceUnitWith
  ) where

import           Control.Tracer (Tracer (Tracer), traceWith)
import           Data.Time.Clock (diffTimeToPicoseconds)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (TraceChainSyncClientEvent (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB.Impl
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
                     (SelectionChangedInfo (..), TraceAddBlockEvent (..))
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike, MonadMonotonicTime,
                     Time (Time), getMonotonicTime)
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
          trace $ "New tip: " ++ condense newTipPoint
        SwitchedToAFork _ SelectionChangedInfo {newTipPoint} _ newFragment -> do
          trace "Switched to a fork"
          trace $ "New tip: " ++ condense newTipPoint
          trace $ "New fragment: " ++ condense newFragment
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
      trace $ "Rolled back to: " ++ condense point
    TraceFoundIntersection point _ourTip _theirTip ->
      trace $ "Found intersection at: " ++ condense point
    _ -> pure ()
  where
    trace = traceUnitWith tracer "ChainSyncClient"

-- | Trace using the given tracer, printing the current time (typically the time
-- of the simulation) and the unit name.
traceUnitWith :: MonadMonotonicTime m => Tracer m String -> String -> String -> m ()
traceUnitWith tracer unit msg = do
  time <- getMonotonicTime
  traceWith tracer $ printf "%s %s | %s" (showTime time) unit msg
  where
    showTime :: Time -> String
    showTime (Time time) =
      let ps = diffTimeToPicoseconds time
          milliseconds = (ps `div` 1000000000) `mod` 1000
          seconds = (ps `div` 1000000000000) `rem` 60
          minutes = (ps `div` 1000000000000) `quot` 60
       in printf "%02d:%02d.%03d" minutes seconds milliseconds
