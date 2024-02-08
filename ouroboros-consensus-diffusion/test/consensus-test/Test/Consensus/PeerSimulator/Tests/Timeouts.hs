{-# LANGUAGE ScopedTypeVariables #-}

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
tests = testGroup "timeouts" [
  adjustQuickCheckTests (`div` 10) $ testProperty "does time out" (prop_timeouts True),
  adjustQuickCheckTests (`div` 10) $ testProperty "does not time out" (prop_timeouts False)
  ]

prop_timeouts :: Bool -> Gen Property
prop_timeouts mustTimeout = do
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
          counterexample ("result: " ++ condense (svSelectedChain stateView)) (not mustTimeout)
        [exn] ->
          case fromException $ cseException exn of
            Just (ExceededTimeLimit _) -> property mustTimeout
            _ -> counterexample ("exception: " ++ show exn) False
        exns ->
          counterexample ("exceptions: " ++ show exns) False

  where
    dullSchedule :: DiffTime -> TestFrag -> Peers PeerSchedule
    dullSchedule _ (AF.Empty _) = error "requires a non-empty block tree"
    dullSchedule timeout (_ AF.:> tipBlock) =
      let offset :: DiffTime = if mustTimeout then 1 else -1
       in peersOnlyHonest $ [
            (Time 0, ScheduleTipPoint tipBlock),
            (Time 0, ScheduleHeaderPoint tipBlock),
            (Time (timeout + offset), ScheduleBlockPoint tipBlock)
            ]
