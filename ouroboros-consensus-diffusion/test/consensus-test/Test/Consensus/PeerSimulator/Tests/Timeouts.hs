module Test.Consensus.PeerSimulator.Tests.Timeouts (tests) where

import           Data.Functor (($>))
import           Data.Maybe (fromJust)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike (DiffTime, Time (Time),
                     fromException)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Driver.Limits
                     (ProtocolLimitFailure (ExceededTimeLimit))
import           Ouroboros.Network.Protocol.ChainSync.Codec (mustReplyTimeout)
import           Test.Consensus.BlockTree (btTrunk)
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.PeerSimulator.Run (SchedulerConfig (..),
                     defaultSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Peers (Peers, peersOnlyHonest)
import           Test.Consensus.PointSchedule.SinglePeer (SchedulePoint (..))
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests = adjustQuickCheckTests (`div` 10) $ testProperty "timeouts" prop_timeouts

prop_timeouts :: Gen Property
prop_timeouts = do
  genesisTest <- genChains (pure 0)

  -- Use higher tick duration to avoid the test taking really long
  let schedulerConfig = defaultSchedulerConfig (gtHonestAsc genesisTest)

      schedule =
        dullSchedule
          (fromJust $ mustReplyTimeout (scChainSyncTimeouts schedulerConfig))
          (btTrunk $ gtBlockTree genesisTest)

      genesisTest' = genesisTest $> schedule

  -- FIXME: Because the scheduler configuration depends on the generated
  -- 'GenesisTest' itself, we cannot rely on helpers such as
  -- 'forAllGenesisTest'.
  pure $
    runGenesisTest' schedulerConfig genesisTest' $ \stateView ->
      case svChainSyncExceptions stateView of
        [] ->
          counterexample ("result: " ++ condense (svSelectedChain stateView)) False
        [exn] ->
          case fromException $ cseException exn of
            Just (ExceededTimeLimit _) -> property True
            _ -> counterexample ("exception: " ++ show exn) False
        exns ->
          counterexample ("exceptions: " ++ show exns) False

  where
    -- A schedule that advertises all the points of the chain from the start but
    -- contains just one too many ticks, therefore reaching the timeouts.
    dullSchedule :: DiffTime -> TestFrag -> Peers PeerSchedule
    dullSchedule _ (AF.Empty _) = error "requires a non-empty block tree"
    dullSchedule timeout (_ AF.:> tipBlock) =
      let tickDuration = 1 -- 1s
          maximumNumberOfTicks = round $ timeout / tickDuration
       in peersOnlyHonest $
            (Time 0, ScheduleTipPoint tipBlock)
              : (Time 0, ScheduleHeaderPoint tipBlock)
              : zip (map (Time . (* tickDuration)) [0..]) (replicate maximumNumberOfTicks (ScheduleBlockPoint tipBlock))
