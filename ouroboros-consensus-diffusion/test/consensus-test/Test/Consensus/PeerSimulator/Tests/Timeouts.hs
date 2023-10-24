{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.PeerSimulator.Tests.Timeouts (tests) where

import           Cardano.Slotting.Time (SlotLength, slotLengthFromSec)
import           Control.Monad.IOSim (runSimOrThrow)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Data.Time.Clock (diffTimeToPicoseconds)
import           Ouroboros.Consensus.Block (getHeader)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike (DiffTime, fromException)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (tipFromHeader)
import           Ouroboros.Network.Driver.Limits
                     (ProtocolLimitFailure (ExceededTimeLimit))
import           Ouroboros.Network.Protocol.ChainSync.Codec (mustReplyTimeout)
import           Test.Consensus.BlockTree (btTrunk)
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.Network.Driver.Limits.Extras (chainSyncTimeouts)
import           Test.Consensus.PeerSimulator.Run (SchedulerConfig (..))
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()

tests :: TestTree
tests = testProperty "timeouts" prop_timeouts

prop_timeouts :: QC.Gen QC.Property
prop_timeouts = do
  genesisTest <- genChains 0

  let scChainSyncTimeouts = chainSyncTimeouts slotLength (gtHonestAsc genesisTest)
      schedulerConfig = SchedulerConfig {scChainSyncTimeouts}

  let schedule =
        dullSchedule
          (fromJust $ mustReplyTimeout scChainSyncTimeouts)
          (btTrunk $ gtBlockTree genesisTest)

  pure $ withMaxSuccess 10 $ runSimOrThrow $
    runTest schedulerConfig genesisTest schedule $ \stateView ->
      case svChainSyncExceptions stateView of
        [] ->
          counterexample ("result: " ++ condense (svSelectedChain stateView)) False
        [exn] ->
          case fromException $ cseException exn of
            Just (ExceededTimeLimit _) -> counterexample "REVIEW" True
            _ -> counterexample ("exception: " ++ show exn) False
        exns ->
          counterexample ("exceptions: " ++ show exns) False

  where
    -- FIXME: Should ideally not be hardcoded, especially because it is now
    -- duplicated between here and within the PeerSimulator.
    tickDuration :: DiffTime
    tickDuration = 0.100 -- seconds

    -- FIXME: Should ideally not be hardcoded, especially because it is now
    -- duplicated between here and within the PeerSimulator.
    slotLength :: SlotLength
    slotLength = slotLengthFromSec 20

    -- A schedule that advertises all the points of the chain from the start but
    -- contains just one too many ticks, therefore reaching the timeouts.
    dullSchedule :: DiffTime -> TestFrag -> PointSchedule
    dullSchedule _ (AF.Empty _) = error "requires a non-empty block tree"
    dullSchedule timeout (_ AF.:> tipBlock) =
      let tipPoint = TipPoint $ tipFromHeader tipBlock
          headerPoint = HeaderPoint $ getHeader tipBlock
          blockPoint = BlockPoint tipBlock
          state = Peer HonestPeer $ NodeOnline $ AdvertisedPoints tipPoint headerPoint blockPoint
          tick = Tick { active = state, peers = Peers state Map.empty }
          maximumNumberOfTicks = fromIntegral $ roundDiffTimeToSeconds $ timeout / tickDuration
      in
      PointSchedule (tick :| replicate maximumNumberOfTicks tick)

    roundDiffTimeToSeconds :: DiffTime -> Integer
    roundDiffTimeToSeconds = (`div` 1000000000000) . (+ 500000000000) . diffTimeToPicoseconds
