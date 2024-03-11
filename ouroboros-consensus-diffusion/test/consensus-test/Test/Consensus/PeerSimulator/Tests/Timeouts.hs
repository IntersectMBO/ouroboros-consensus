module Test.Consensus.PeerSimulator.Tests.Timeouts (tests) where

import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Maybe (fromJust)
import           Ouroboros.Consensus.Block (getHeader)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike (DiffTime, fromException)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (tipFromHeader)
import           Ouroboros.Network.Driver.Limits
                     (ProtocolLimitFailure (ExceededTimeLimit))
import           Ouroboros.Network.Point (WithOrigin (At))
import           Ouroboros.Network.Protocol.ChainSync.Codec (mustReplyTimeout)
import           Test.Consensus.BlockTree (btTrunk)
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.PeerSimulator.Run (SchedulerConfig (..),
                     defaultSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Peers (Peer (..), PeerId (..))
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
  let scSchedule' = PointScheduleConfig {pscTickDuration = 1}

      schedulerConfig = defaultSchedulerConfig scSchedule' (gtHonestAsc genesisTest)

      schedule =
        dullSchedule
          scSchedule'
          (fromJust $ mustReplyTimeout (scChainSyncTimeouts schedulerConfig))
          (btTrunk $ gtBlockTree genesisTest)

  -- NOTE: Because the scheduler configuration depends on the generated
  -- 'GenesisTest' itself, we cannot rely on helpers such as
  -- 'forAllGenesisTest'.
  pure $
    runGenesisTest' schedulerConfig genesisTest schedule $ \stateView ->
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
    dullSchedule :: PointScheduleConfig -> DiffTime -> TestFrag -> PointSchedule
    dullSchedule _ _ (AF.Empty _) = error "requires a non-empty block tree"
    dullSchedule scheduleConfig timeout (_ AF.:> tipBlock) =
      let tipPoint = TipPoint $ tipFromHeader tipBlock
          headerPoint = HeaderPoint $ At (getHeader tipBlock)
          blockPoint = BlockPoint (At tipBlock)
          state = Peer HonestPeer $ NodeOnline $ AdvertisedPoints tipPoint headerPoint blockPoint
          tick = Tick { active = state, duration = pscTickDuration scheduleConfig, number = 0 }
          maximumNumberOfTicks = round $ timeout / pscTickDuration scheduleConfig
      in
      PointSchedule (tick :| replicate maximumNumberOfTicks tick) (HonestPeer :| [])
