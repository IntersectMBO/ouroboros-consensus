{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Test.Consensus.Genesis.Tests.Uniform (tests) where

import           Cardano.Slotting.Slot (SlotNo (SlotNo), WithOrigin (..))
import           Control.Monad (replicateM)
import           Data.List (group, intercalate, sort)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Time.Clock (DiffTime)
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (GenesisWindow (..))
import           Ouroboros.Consensus.Util.Condense (condense)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (blockNo, unBlockNo)
import           Ouroboros.Network.Protocol.ChainSync.Codec
                     (ChainSyncTimeout (..))
import           Ouroboros.Network.Protocol.Limits (shortWait)
import           Test.Consensus.BlockTree (BlockTree (..), btbSuffix)
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.Genesis.Setup.Classifiers
import           Test.Consensus.Network.Driver.Limits.Extras
                     (chainSyncNoTimeouts)
import           Test.Consensus.PeerSimulator.Run (SchedulerConfig (..),
                     noTimeoutsSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Peers (PeerId (..), Peers (..),
                     value)
import           Test.Consensus.PointSchedule.Shrinking
                     (shrinkByRemovingAdversaries, shrinkPeerSchedules)
import           Test.Consensus.PointSchedule.SinglePeer
                     (SchedulePoint (ScheduleBlockPoint, ScheduleTipPoint))
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Delta (Delta))
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.QuickCheck (le)
import           Test.Util.TestBlock (TestBlock, tbSlot)
import           Test.Util.TestEnv (adjustQuickCheckMaxSize,
                     adjustQuickCheckTests)
import           Text.Printf (printf)

tests :: TestTree
tests =
  adjustQuickCheckTests (* 10) $
  adjustQuickCheckMaxSize (`div` 5) $
  testGroup "uniform" [
    -- See Note [Leashing attacks]
    testProperty "stalling leashing attack" prop_leashingAttackStalling,
    testProperty "time limited leashing attack" prop_leashingAttackTimeLimited,
    adjustQuickCheckTests (`div` 10) $
    testProperty "serve adversarial branches" prop_serveAdversarialBranches,
    adjustQuickCheckTests (`div` 100) $
    testProperty "the LoE stalls the chain, but the immutable tip is honest" prop_loeStalling
    ]

theProperty ::
  GenesisTest ->
  Peers PeerSchedule ->
  StateView ->
  Property
theProperty genesisTest schedule stateView@StateView{svSelectedChain} =
  classify genesisWindowAfterIntersection "Full genesis window after intersection" $
  classify (isOrigin immutableTipHash) "Immutable tip is Origin" $
  label disconnected $
  classify (advCount < length (btBranches gtBlockTree)) "Some adversaries performed rollbacks" $
  counterexample killedPeers $
  -- We require the honest chain to fit a Genesis window, because otherwise its tip may suggest
  -- to the governor that the density is too low.
  longerThanGenesisWindow ==>
  conjoin [
    counterexample "The honest peer was disconnected" (HonestPeer `notElem` killed),
    counterexample ("The immutable tip is not honest: " ++ show immutableTip) $
    property (isHonest immutableTipHash),
    immutableTipIsRecent
  ]
  where
    advCount = Map.size (others schedule)

    immutableTipIsRecent =
      counterexample ("Age of the immutable tip: " ++ show immutableTipAge) $
      immutableTipAge `le` s + fromIntegral d + 1

    SlotNo immutableTipAge = case (honestTipSlot, immutableTipSlot) of
      (At h, At i)   -> h - i
      (At h, Origin) -> h
      _              -> 0

    isOrigin = null

    isHonest = all (0 ==)

    immutableTipHash = simpleHash (AF.anchorToHash immutableTip)

    immutableTip = AF.anchor svSelectedChain

    immutableTipSlot = AF.anchorToSlotNo (AF.anchor svSelectedChain)

    disconnected =
      printf "disconnected %.1f%% of adversaries" disconnectedPercent

    killed = chainSyncKilled stateView

    disconnectedPercent :: Double
    disconnectedPercent =
      100 * fromIntegral (length killed) / fromIntegral advCount

    killedPeers = case killed of
      [] -> "No peers were killed"
      peers -> "Some peers were killed: " ++ intercalate ", " (condense <$> peers)

    honestTipSlot = At $ tbSlot $ snd $ last $ mapMaybe fromBlockPoint $ value $ honest schedule

    GenesisTest {gtBlockTree, gtGenesisWindow = GenesisWindow s, gtDelay = Delta d} = genesisTest

    Classifiers {genesisWindowAfterIntersection, longerThanGenesisWindow} = classifiers genesisTest

fromBlockPoint :: (DiffTime, SchedulePoint) -> Maybe (DiffTime, TestBlock)
fromBlockPoint (t, ScheduleBlockPoint bp) = Just (t, bp)
fromBlockPoint _                          = Nothing

-- | Tests that the immutable tip is not delayed and stays honest with the
-- adversarial peers serving adversarial branches.
prop_serveAdversarialBranches :: Property
prop_serveAdversarialBranches = forAllGenesisTest'

    (do gt <- genChains (QC.choose (1, 4))
        ps <- genUniformSchedulePoints gt
        pure (gt, ps))

    ((noTimeoutsSchedulerConfig defaultPointScheduleConfig)
       {scTraceState = False, scTrace = False, scEnableLoE = True})

    -- We cannot shrink by removing points from the adversarial schedules.
    -- Otherwise, the immutable tip could get stuck because a peer doesn't
    -- send any blocks or headers.
    shrinkByRemovingAdversaries

    theProperty

genUniformSchedulePoints :: GenesisTest -> QC.Gen (Peers PeerSchedule)
genUniformSchedulePoints gt = stToGen (uniformPoints (gtBlockTree gt))

-- Note [Leashing attacks]
--
-- A leashing attack would be rehearsed by a point schedule meeting either of
-- two conditions:
--
-- 1) it causes the node under test to stop making progress (i.e. the immutable
--    tip doesn't get close to the last genesis window of the honest chain), or
-- 2) it causes the node under test to still make progress but it is too slow
--    (in some sense of slow)
--
-- We produce schedules meeting the first condition by dropping random points
-- from the schedule of adversarial peers. If peers don't send the headers or
-- the blocks that they promised with a tip point, the node under test could
-- wait indefinitely without advancing the immutable tip.
--
-- We produce schedules meeting the second condition by stopping the execution
-- of the schedule after an amount of time that depends on the amount of blocks
-- to sync up and the timeouts allowed by Limit of Patience.
-- If the adversarial peers succeed in delaying the immutable tip, interrupting
-- the test at this point should cause the immutable tip to be too far behind
-- the last genesis window of the honest chain.

-- | Test that the leashing attacks do not delay the immutable tip
--
-- This test is expected to fail because we don't test a genesis implementation
-- yet.
prop_leashingAttackStalling :: Property
prop_leashingAttackStalling =
  expectFailure $ forAllGenesisTest'

    (do gt <- genChains (QC.choose (1, 4))
        ps <- genLeashingSchedule gt
        pure (gt, ps))

    ((noTimeoutsSchedulerConfig defaultPointScheduleConfig)
      {scTrace = False})

    shrinkPeerSchedules

    theProperty

  where
    -- | Produces schedules that might cause the node under test to stall.
    --
    -- This is achieved by dropping random points from the schedule of each peer
    genLeashingSchedule :: GenesisTest -> QC.Gen (Peers PeerSchedule)
    genLeashingSchedule genesisTest = do
      Peers honest advs0 <- genUniformSchedulePoints genesisTest
      advs <- mapM (mapM dropRandomPoints) advs0
      pure (Peers honest advs)

    dropRandomPoints :: [(DiffTime, SchedulePoint)] -> QC.Gen [(DiffTime, SchedulePoint)]
    dropRandomPoints ps = do
      let lenps = length ps
      dropCount <- QC.choose (0, max 1 $ div lenps 5)
      let dedup = map head . group
      is <- fmap (dedup . sort) $ replicateM dropCount $ QC.choose (0, lenps - 1)
      pure $ dropElemsAt ps is

    dropElemsAt :: [a] -> [Int] -> [a]
    dropElemsAt xs [] = xs
    dropElemsAt xs (i:is) =
      let (ys, zs) = splitAt i xs
       in ys ++ dropElemsAt (drop 1 zs) is

-- | Test that the leashing attacks do not delay the immutable tip after. The
-- immutable tip needs to be advanced enough when the honest peer has offered
-- all of its ticks.
--
-- This test is expected to fail because we don't test a genesis implementation
-- yet.
--
-- See Note [Leashing attacks]
prop_leashingAttackTimeLimited :: Property
prop_leashingAttackTimeLimited =
  expectFailure $ forAllGenesisTest'

    (do gt <- genChains (QC.choose (1, 4))
        ps <- genTimeLimitedSchedule gt
        pure (gt, ps))

    ((noTimeoutsSchedulerConfig defaultPointScheduleConfig)
      {scTrace = False})

    shrinkPeerSchedules

    theProperty

  where
    -- | A schedule which doesn't run past the last event of the honest peer
    genTimeLimitedSchedule :: GenesisTest -> QC.Gen (Peers PeerSchedule)
    genTimeLimitedSchedule genesisTest = do
      Peers honest advs0 <- genUniformSchedulePoints genesisTest
      let timeLimit = estimateTimeBound (value honest) (map value $ Map.elems advs0)
          advs = fmap (fmap (takePointsUntil timeLimit)) advs0
      pure (Peers honest advs)

    takePointsUntil limit = takeWhile ((<= limit) . fst)

    estimateTimeBound :: PeerSchedule -> [PeerSchedule] -> DiffTime
    estimateTimeBound honest advs =
      let firstTipPointBlock = headCallStack (mapMaybe fromTipPoint honest)
          lastBlockPoint = last (mapMaybe fromBlockPoint honest)
          peerCount = length advs + 1
          maxBlockNo = maximum $ 0 : blockPointNos honest ++ concatMap blockPointNos advs
          -- 0.020s is the amount of time LoP grants per interesting header
          -- 5s is the initial fill of the LoP bucket
          --
          -- Since the moment the honest peer offers the first tip, LoP should
          -- start ticking. Syncing all the blocks might take longer than it
          -- takes to dispatch all ticks to the honest peer. In this case
          -- the syncing time is the time bound for the test. If dispatching
          -- all the ticks takes longer, then the dispatching time becomes
          -- the time bound.
          --
          -- Adversarial peers might cause more ticks to be sent as well. We
          -- bound it all by considering the highest block number that is ever
          -- sent.
      in max
          (fst lastBlockPoint)
          (fst firstTipPointBlock +
              0.020 * fromIntegral maxBlockNo + 5 * fromIntegral peerCount)

    blockPointNos :: [(DiffTime, SchedulePoint)] -> [Word64]
    blockPointNos =
      map (unBlockNo . blockNo . snd) .
      mapMaybe fromBlockPoint

    fromTipPoint (t, ScheduleTipPoint bp) = Just (t, bp)
    fromTipPoint _                        = Nothing

headCallStack :: HasCallStack => [a] -> a
headCallStack xs = if null xs then error "headCallStack: empty list" else head xs

-- | Test that enabling the LoE using the updater that sets the LoE fragment to
-- the shared prefix (as used by the GDDG) causes the selection to remain at
-- the first fork intersection (keeping the immutable tip honest).
--
-- This is pretty slow since it relies on timeouts to terminate the test.
prop_loeStalling :: Property
prop_loeStalling =
  forAllGenesisTest'

    (do gt <- genChains (QC.choose (1, 4))
        ps <- genUniformSchedulePoints gt
        pure (gt, ps))

    ((noTimeoutsSchedulerConfig defaultPointScheduleConfig) {
      scTrace = False,
      scEnableLoE = True,
      scChainSyncTimeouts = chainSyncNoTimeouts {canAwaitTimeout = shortWait}
    })

    (\_ _ _ -> [])

    prop
  where
    prop GenesisTest {gtBlockTree = BlockTree {btTrunk, btBranches}} _ StateView{svSelectedChain} =
      classify (any (== selectionTip) allTips) "The selection is at a branch tip" $
      classify (any anchorIsImmutableTip suffixes) "The immutable tip is at a fork intersection" $
      property (isHonest immutableTipHash)
      where
        anchorIsImmutableTip branch = simpleHash (AF.anchorToHash (AF.anchor branch)) == immutableTipHash

        isHonest = all (0 ==)

        immutableTipHash = simpleHash (AF.anchorToHash immutableTip)

        immutableTip = AF.anchor svSelectedChain

        selectionTip = simpleHash (AF.headHash svSelectedChain)

        allTips = simpleHash . AF.headHash <$> (btTrunk : suffixes)

        suffixes = btbSuffix <$> btBranches
