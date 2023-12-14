{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies       #-}

-- | Helpers for tracing used by the peer simulator.
module Test.Consensus.PeerSimulator.Trace (
    mkCdbTracer
  , mkChainSyncClientTracer
  , terseBlock
  , terseFrag
  , terseFragH
  , terseHeader
  , tersePoint
  , traceLinesWith
  , traceUnitWith
  ) where

import           Cardano.Slotting.Block (BlockNo (BlockNo))
import           Cardano.Slotting.Slot (SlotNo (SlotNo))
import           Control.Tracer (Tracer (Tracer), traceWith)
import           Data.List (intercalate)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Time.Clock (diffTimeToPicoseconds)
import           Ouroboros.Consensus.Block (Header,
                     Point (BlockPoint, GenesisPoint), blockHash, blockNo,
                     blockSlot, getHeader)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (TraceChainSyncClientEvent (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB.Impl
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
                     (SelectionChangedInfo (..), TraceAddBlockEvent (..))
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike, MonadMonotonicTime,
                     Time (Time), getMonotonicTime)
import           Ouroboros.Network.AnchoredFragment
                     (Anchor (Anchor, AnchorGenesis), AnchoredFragment,
                     AnchoredSeq (Empty), anchor, mapAnchoredFragment,
                     toOldestFirst)
import           Test.Util.TestBlock (Header (TestHeader), TestBlock,
                     TestHash (TestHash), unTestHash)
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
          milliseconds = (ps `div` 1_000_000_000) `mod` 1_000
          seconds = (ps `div` 1_000_000_000_000) `rem` 60
          minutes = (ps `div` 1_000_000_000_000) `quot` 60
       in printf "%02d:%02d.%03d" minutes seconds milliseconds

traceLinesWith ::
  Tracer m String ->
  [String] ->
  m ()
traceLinesWith tracer = traceWith tracer . unlines

terseSlotBlock :: SlotNo -> BlockNo -> String
terseSlotBlock (SlotNo slot) (BlockNo block) =
  show slot ++ "-" ++ show block

terseSlotBlockFork :: SlotNo -> BlockNo -> TestHash -> String
terseSlotBlockFork sno bno (TestHash hash) =
  terseSlotBlock sno bno ++ forkNoSuffix hash
  where
    forkNoSuffix (forkNo :| _) | forkNo == 0 = ""
                               | otherwise = "[" ++ show forkNo ++ "]"

terseBlock :: TestBlock -> String
terseBlock block =
  terseSlotBlockFork (blockSlot block) (blockNo block) (blockHash block)

terseHeader :: Header TestBlock -> String
terseHeader (TestHeader block) = terseBlock block

tersePoint :: Point TestBlock -> String
tersePoint = \case
  BlockPoint slot hash -> terseSlotBlockFork slot (BlockNo (fromIntegral (length (unTestHash hash)))) hash
  GenesisPoint -> "G"

terseFragH :: AnchoredFragment (Header TestBlock) -> String
terseFragH frag =
  renderAnchor ++ renderBlocks
  where
    renderBlocks = case frag of
      Empty _ -> ""
      _       -> " | " ++ intercalate " " (terseHeader <$> toOldestFirst frag)
    renderAnchor = case anchor frag of
      AnchorGenesis -> "G"
      Anchor slot hash block -> terseSlotBlock slot block ++ renderAnchorHash hash
    renderAnchorHash hash
      | all (== 0) (unTestHash hash) = ""
      | otherwise = condense hash

terseFrag :: AnchoredFragment TestBlock -> String
terseFrag =
  terseFragH . mapAnchoredFragment getHeader
