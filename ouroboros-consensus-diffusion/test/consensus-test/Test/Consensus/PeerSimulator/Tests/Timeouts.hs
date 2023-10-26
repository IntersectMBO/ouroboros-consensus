{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.PeerSimulator.Tests.Timeouts (tests) where

import           Control.Monad.IOSim (runSimOrThrow)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
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
import           Test.Consensus.PeerSimulator.Run (SchedulerConfig (..),
                     defaultSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()

tests :: TestTree
tests = testGroup "timeouts" [
    testProperty "static" prop_timeouts_static,
    testProperty "dynamic" prop_timeouts_dynamic
  ]

stateViewProperty :: StateView -> QC.Property
stateViewProperty stateView =
  case svChainSyncExceptions stateView of
    [] ->
      counterexample ("result: " ++ condense (svSelectedChain stateView)) False
    [exn] ->
      case fromException $ cseException exn of
        Just (ExceededTimeLimit _) -> property True
        _ -> counterexample ("exception: " ++ show exn) False
    exns ->
      counterexample ("exceptions: " ++ show exns) False

-- | Static variant of the test. We count exactly how many ticks are going to be
-- necessary to make the test reach a timeout and we create a schedule that
-- exactly that amount of ticks.
prop_timeouts_static :: QC.Gen QC.Property
prop_timeouts_static = do
  genesisTest <- genChains 0

  -- Use higher tick duration to avoid the test taking really long
  let scSchedule = PointScheduleConfig {pscTickDuration = 1}
      schedulerConfig = defaultSchedulerConfig scSchedule (gtHonestAsc genesisTest)
      schedule =
        dullSchedule
          scSchedule
          (fromJust $ mustReplyTimeout (scChainSyncTimeouts schedulerConfig))
          (btTrunk $ gtBlockTree genesisTest)

  pure $ runSimOrThrow $
    runTest schedulerConfig genesisTest schedule stateViewProperty

  where
    -- A schedule that advertises all the points of the chain from the start but
    -- contains just one too many ticks, therefore reaching the timeouts.
    dullSchedule :: PointScheduleConfig -> DiffTime -> TestFrag -> PointSchedule
    dullSchedule _ _ (AF.Empty _) = error "requires a non-empty block tree"
    dullSchedule scheduleConfig timeout (_ AF.:> tipBlock) =
      let tipPoint = TipPoint $ tipFromHeader tipBlock
          headerPoint = HeaderPoint $ getHeader tipBlock
          blockPoint = BlockPoint tipBlock
          state = Peer HonestPeer $ NodeOnline $ AdvertisedPoints tipPoint headerPoint blockPoint
          tick = Tick { active = state, peers = Peers state Map.empty }
          maximumNumberOfTicks = round $ timeout / pscTickDuration scheduleConfig
      in
      PointSchedule (tick :| replicate maximumNumberOfTicks tick)

-- | Dynamic variant of the test. We start with one tick but provide an extender
-- that systematically adds another tick. If no exception is every triggered,
-- then this test hangs. However, it is conceptually simpler and requires less
-- inputs than its static counterpart.
prop_timeouts_dynamic :: QC.Gen QC.Property
prop_timeouts_dynamic = do
  genesisTest <- genChains 0
  let schedule = oneshotSchedule (btTrunk $ gtBlockTree genesisTest)

  -- Use higher tick duration to avoid the test taking really long. Add an
  -- extender that adds the same tick over and over again until we get a
  -- ChainSyncException.
  let scSchedule = PointScheduleConfig {pscTickDuration = 1}
      schedulerConfig =
        (defaultSchedulerConfig scSchedule (gtHonestAsc genesisTest)) {
          scExtender = \stateView ->
              case svChainSyncExceptions stateView of
                [] -> Just schedule
                _  -> Nothing
          }

  pure $ runSimOrThrow $
    runTest schedulerConfig genesisTest schedule stateViewProperty

  where
    -- A schedule that advertises all the points of the chain in one tick.
    oneshotSchedule :: TestFrag -> PointSchedule
    oneshotSchedule (AF.Empty _) = error "requires a non-empty block tree"
    oneshotSchedule (_ AF.:> tipBlock) =
      let tipPoint = TipPoint $ tipFromHeader tipBlock
          headerPoint = HeaderPoint $ getHeader tipBlock
          blockPoint = BlockPoint tipBlock
          state = Peer HonestPeer $ NodeOnline $ AdvertisedPoints tipPoint headerPoint blockPoint
          tick = Tick { active = state, peers = Peers state Map.empty }
      in PointSchedule (tick :| [])
