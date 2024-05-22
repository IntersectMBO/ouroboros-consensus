{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Peer simulator tests based on randomly generated schedules. They share the
-- same property stating that the immutable tip should be on the trunk of the
-- block tree with the right age (roughly @k@ blocks from the tip). Contrary to
-- other tests cases (eg. long range attack), the schedules are not particularly
-- biased towards a specific situation.
module Test.Consensus.Genesis.Tests.Uniform (
    genUniformSchedulePoints
  , tests
  ) where

import           Cardano.Slotting.Slot (SlotNo (SlotNo), WithOrigin (..))
import           Control.Monad (replicateM)
import           Control.Monad.Class.MonadTime.SI (Time, addTime)
import           Data.List (intercalate, sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Block.Abstract (WithOrigin (NotOrigin))
import           Ouroboros.Consensus.Util.Condense (condense)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (blockNo, blockSlot, unBlockNo)
import           Ouroboros.Network.Protocol.ChainSync.Codec
                     (ChainSyncTimeout (..))
import           Ouroboros.Network.Protocol.Limits (shortWait)
import           Test.Consensus.BlockTree (BlockTree (..), btbSuffix)
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.Genesis.Setup.Classifiers
import           Test.Consensus.PeerSimulator.ChainSync (chainSyncNoTimeouts)
import           Test.Consensus.PeerSimulator.Run (SchedulerConfig (..),
                     defaultSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Peers (Peers (..), isHonestPeerId)
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
import           Test.Util.PartialAccessors
import           Test.Util.QuickCheck (le)
import           Test.Util.TestBlock (TestBlock)
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
    testProperty "serve adversarial branches" prop_serveAdversarialBranches,
    testProperty "the LoE stalls the chain, but the immutable tip is honest" prop_loeStalling,
    -- This is a crude way of ensuring that we don't get chains with more than 100 blocks,
    -- because this test writes the immutable chain to disk and `instance Binary TestBlock`
    -- chokes on long chains.
    adjustQuickCheckMaxSize (const 10) $
    testProperty "the node is shut down and restarted after some time" prop_downtime
    ]

theProperty ::
  GenesisTestFull TestBlock ->
  StateView TestBlock ->
  Property
theProperty genesisTest stateView@StateView{svSelectedChain} =
  classify genesisWindowAfterIntersection "Full genesis window after intersection" $
  classify (isOrigin immutableTipHash) "Immutable tip is Origin" $
  label disconnectedLabel $
  classify (advCount < length (btBranches gtBlockTree)) "Some adversaries performed rollbacks" $
  counterexample killedPeers $
  -- We require the honest chain to fit a Genesis window, because otherwise its tip may suggest
  -- to the governor that the density is too low.
  longerThanGenesisWindow ==>
  conjoin [
    counterexample "An honest peer was disconnected" (not $ any isHonestPeerId disconnected),
    counterexample ("The immutable tip is not honest: " ++ show immutableTip) $
    property (isHonest immutableTipHash),
    immutableTipIsRecent
  ]
  where
    advCount = Map.size (adversarialPeers (unPointSchedule $ gtSchedule genesisTest))

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

    disconnectedLabel =
      printf "disconnected %.1f%% of adversaries" disconnectedPercent

    disconnected = collectDisconnectedPeers stateView

    disconnectedPercent :: Double
    disconnectedPercent =
      100 * fromIntegral (length disconnected) / fromIntegral advCount

    killedPeers = case disconnected of
      [] -> "No peers were disconnected"
      peers -> "Some peers were disconnected: " ++ intercalate ", " (condense <$> peers)

    honestTipSlot = At $ blockSlot $ snd $ last $ mapMaybe fromBlockPoint $ getHonestPeer $ honestPeers $ unPointSchedule $ gtSchedule genesisTest

    GenesisTest {gtBlockTree, gtGenesisWindow = GenesisWindow s, gtDelay = Delta d} = genesisTest

    Classifiers {genesisWindowAfterIntersection, longerThanGenesisWindow} = classifiers genesisTest

fromBlockPoint :: (Time, SchedulePoint blk) -> Maybe (Time, blk)
fromBlockPoint (t, ScheduleBlockPoint (NotOrigin bp)) = Just (t, bp)
fromBlockPoint _                                      = Nothing

-- | Tests that the immutable tip is not delayed and stays honest with the
-- adversarial peers serving adversarial branches.
prop_serveAdversarialBranches :: Property
prop_serveAdversarialBranches = forAllGenesisTest

    (genChains (QC.choose (1, 4)) `enrichedWith` genUniformSchedulePoints)

    (defaultSchedulerConfig
       { scTraceState = False
       , scTrace = False
       , scEnableLoE = True
       , scEnableCSJ = True
       , scEnableLoP = False
       , scEnableChainSyncTimeouts = False
       , scEnableBlockFetchTimeouts = False
       })

    -- We cannot shrink by removing points from the adversarial schedules.
    -- Removing ticks could make an adversary unable to serve any blocks or headers.
    -- Because LoP and timeouts are disabled, this would cause the immutable tip
    -- to get stuck indefinitely, as the adversary wouldn't get disconnected.
    --
    -- We don't enable timeouts in this test and we don't wait long enough for
    -- timeouts to expire. The leashing attack tests are testing the timeouts
    -- together with LoP.
    shrinkByRemovingAdversaries

    theProperty

genUniformSchedulePoints :: GenesisTest TestBlock () -> QC.Gen (PointSchedule TestBlock)
genUniformSchedulePoints gt = stToGen (uniformPoints pointsGeneratorParams (gtBlockTree gt))
  where
    pointsGeneratorParams = PointsGeneratorParams
      { pgpExtraHonestPeers = fromIntegral $ gtExtraHonestPeers gt
      , pgpDowntime = NoDowntime
      }

-- Note [Leashing attacks]
--
-- A leashing attack would be successfully conducted by a point schedule meeting
-- either of two conditions:
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
prop_leashingAttackStalling :: Property
prop_leashingAttackStalling =
  forAllGenesisTest

    (disableBoringTimeouts <$> genChains (QC.choose (1, 4)) `enrichedWith` genLeashingSchedule)

    defaultSchedulerConfig
      { scTrace = False
      , scEnableLoE = True
      , scEnableLoP = True
      , scEnableCSJ = True
      }

    shrinkPeerSchedules

    theProperty

  where
    -- | Produces schedules that might cause the node under test to stall.
    --
    -- This is achieved by dropping random points from the schedule of each peer
    -- and by adding sufficient time at the end of a test to allow LoP and
    -- timeouts to disconnect adversaries.
    genLeashingSchedule :: GenesisTest TestBlock () -> QC.Gen (PointSchedule TestBlock)
    genLeashingSchedule genesisTest = do
      Peers honest advs0 <- unPointSchedule . ensureScheduleDuration genesisTest <$> genUniformSchedulePoints genesisTest
      advs <- mapM dropRandomPoints advs0
      pure $ PointSchedule $ Peers honest advs

    disableBoringTimeouts gt =
      gt { gtChainSyncTimeouts = (gtChainSyncTimeouts gt)
            { mustReplyTimeout = Nothing
            , idleTimeout = Nothing
            }
         }

    dropRandomPoints :: [(Time, SchedulePoint blk)] -> QC.Gen [(Time, SchedulePoint blk)]
    dropRandomPoints ps = do
      let lenps = length ps
      dropCount <- QC.choose (0, max 1 $ div lenps 5)
      let dedup = map NE.head . NE.group
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
  forAllGenesisTest

    (disableBoringTimeouts <$> genChains (QC.choose (1, 4)) `enrichedWith` genTimeLimitedSchedule)

    defaultSchedulerConfig
      { scTrace = False
      , scEnableLoE = True
      , scEnableLoP = True
      , scEnableBlockFetchTimeouts = False
      , scEnableCSJ = True
      }

    shrinkPeerSchedules

    theProperty

  where
    -- | A schedule which doesn't run past the last event of the honest peer
    genTimeLimitedSchedule :: GenesisTest TestBlock () -> QC.Gen (PointSchedule TestBlock)
    genTimeLimitedSchedule genesisTest = do
      Peers honests advs0 <- unPointSchedule <$> genUniformSchedulePoints genesisTest
      let timeLimit = estimateTimeBound
            (gtChainSyncTimeouts genesisTest)
            (gtLoPBucketParams genesisTest)
            (getHonestPeer honests)
            (Map.elems advs0)
          advs = fmap (takePointsUntil timeLimit) advs0
          extendedHonests = extendScheduleUntil timeLimit <$> honests
      pure $ PointSchedule $ Peers extendedHonests advs

    takePointsUntil limit = takeWhile ((<= limit) . fst)

    extendScheduleUntil
      :: Time -> [(Time, SchedulePoint TestBlock)] -> [(Time, SchedulePoint TestBlock)]
    extendScheduleUntil t [] = [(t, ScheduleTipPoint Origin)]
    extendScheduleUntil t xs =
        let (t', p) = last xs
         in if t < t' then xs
            else xs ++ [(t, p)]

    disableBoringTimeouts gt =
      gt { gtChainSyncTimeouts = (gtChainSyncTimeouts gt)
            { canAwaitTimeout = Nothing
            , mustReplyTimeout = Nothing
            , idleTimeout = Nothing
            }
         }

    estimateTimeBound
      :: AF.HasHeader blk
      => ChainSyncTimeout
      -> LoPBucketParams
      -> PeerSchedule blk
      -> [PeerSchedule blk]
      -> Time
    estimateTimeBound cst LoPBucketParams{lbpCapacity, lbpRate} honest advs =
      let firstTipPointTime = fst $ headCallStack (mapMaybe fromTipPoint honest)
          lastBlockPoint = last (mapMaybe fromBlockPoint honest)
          peerCount = fromIntegral $ length advs + 1
          maxBlockNo = fromIntegral $ maximum $ 0 : blockPointNos honest ++ concatMap blockPointNos advs
          timeCapacity = fromRational $ (fromIntegral lbpCapacity) / lbpRate
          timePerToken = fromRational $ 1 / lbpRate
          intersectDiffTime = fromMaybe (error "no intersect timeout") (intersectTimeout cst)
          -- Since the moment a peer offers the first tip, LoP should
          -- start ticking for it. This can be no later than what the intersect
          -- timeout allows for all peers.
          --
          -- Additionally, the actual delay might be greater if the honest peer
          -- has its last tick dispatched later.
          --
          -- Adversarial peers might cause more ticks to be sent as well. We
          -- bound it all by considering the highest block number that is ever
          -- sent.
      in addTime 1 $ max
          (fst lastBlockPoint)
          (addTime
            (intersectDiffTime + timePerToken * maxBlockNo + timeCapacity * peerCount)
            firstTipPointTime
          )

    blockPointNos :: AF.HasHeader blk => [(Time, SchedulePoint blk)] -> [Word64]
    blockPointNos =
      map (unBlockNo . blockNo . snd) .
      mapMaybe fromBlockPoint

    fromTipPoint (t, ScheduleTipPoint bp) = Just (t, bp)
    fromTipPoint _                        = Nothing

headCallStack :: HasCallStack => [a] -> a
headCallStack = \case
  x:_ -> x
  _   -> error "headCallStack: empty list"

-- | Test that enabling the LoE using the updater that sets the LoE fragment to
-- the shared prefix (as used by the GDDG) causes the selection to remain at
-- the first fork intersection (keeping the immutable tip honest).
--
-- This is pretty slow since it relies on timeouts to terminate the test.
prop_loeStalling :: Property
prop_loeStalling =
  forAllGenesisTest

    (do gt <- genChains (QC.choose (1, 4))
                `enrichedWith`
              genUniformSchedulePoints
        pure gt {gtChainSyncTimeouts = chainSyncNoTimeouts {canAwaitTimeout = shortWait}}
    )

    defaultSchedulerConfig {
      scEnableLoE = True,
      scEnableCSJ = True
    }

    shrinkPeerSchedules

    prop
  where
    prop GenesisTest {gtBlockTree = BlockTree {btTrunk, btBranches}} StateView{svSelectedChain} =
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

-- | This test sets 'scDowntime', which instructs the scheduler to shut all components down whenever a tick's duration
-- is greater than 11 seconds, and restarts it while only preserving the immutable DB after advancing the time.
--
-- This ensures that a user may shut down their machine while syncing without additional vulnerabilities.
prop_downtime :: Property
prop_downtime = forAllGenesisTest

    (genChains (QC.choose (1, 4)) `enrichedWith` \ gt ->
      ensureScheduleDuration gt <$> stToGen (uniformPoints (pointsGeneratorParams gt) (gtBlockTree gt)))

    defaultSchedulerConfig
      { scEnableLoE = True
      , scEnableLoP = True
      , scDowntime = Just 11
      , scEnableCSJ = True
      }

    shrinkPeerSchedules

    theProperty

  where
    pointsGeneratorParams gt = PointsGeneratorParams
      { pgpExtraHonestPeers = fromIntegral (gtExtraHonestPeers gt)
      , pgpDowntime = DowntimeWithSecurityParam (gtSecurityParam gt)
      }
