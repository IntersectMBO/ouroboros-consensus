{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.Consensus.Genesis.Tests.LoE (tests) where

import           Data.Functor (($>))
import           Ouroboros.Consensus.Util.IOLike (Time (Time), fromException)
import           Ouroboros.Network.AnchoredFragment (HasHeader (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Driver.Limits
                     (ProtocolLimitFailure (ExceededTimeLimit))
import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..))
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.PeerSimulator.Run (SchedulerConfig (..),
                     defaultSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Peers (peers')
import           Test.Consensus.PointSchedule.Shrinking (shrinkPeerSchedules)
import           Test.Consensus.PointSchedule.SinglePeer (scheduleBlockPoint,
                     scheduleHeaderPoint, scheduleTipPoint)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.PartialAccessors
import           Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  testGroup
    "LoE"
    [
      adjustQuickCheckTests (`div` 5) $
        testProperty "adversary does not hit timeouts" (prop_adversaryHitsTimeouts False),
      adjustQuickCheckTests (`div` 5) $
        testProperty "adversary hits timeouts" (prop_adversaryHitsTimeouts True)
    ]

-- | Tests that the selection advances in presence of the LoE when a peer is
-- killed by something that is not LoE-aware, eg. the timeouts.
-- NOTE: Same as 'LoP.prop_delayAttack' with timeouts instead of LoP.
prop_adversaryHitsTimeouts :: Bool -> Property
prop_adversaryHitsTimeouts timeoutsEnabled =
  -- Here we can't shrink because we exploit the properties of the point schedule to wait
  -- at the end of the test for the adversaries to get disconnected, by adding an extra point.
  -- If this point gets removed by the shrinker, we lose that property and the test becomes useless.
  noShrinking $
    forAllGenesisTest
      ( do
          gt@GenesisTest {gtBlockTree} <- genChains (pure 1)
          let ps = delaySchedule gtBlockTree
          pure $ gt $> ps
      )
      -- NOTE: Crucially, there must be timeouts for this test.
      ( defaultSchedulerConfig
          { scEnableChainSyncTimeouts = timeoutsEnabled,
            scEnableLoE = True,
            scEnableLoP = False
          }
      )
      shrinkPeerSchedules
      ( \GenesisTest {gtBlockTree} stateView@StateView {svSelectedChain} ->
          let -- The tip of the blocktree trunk.
              treeTipPoint = AF.headPoint $ btTrunk gtBlockTree
              -- The tip of the selection.
              selectedTipPoint = AF.castPoint $ AF.headPoint svSelectedChain
              -- If timeouts are enabled, then the adversary should have been
              -- killed and the selection should be the whole trunk.
              selectedCorrect = timeoutsEnabled == (treeTipPoint == selectedTipPoint)
              -- If timeouts are enabled, then we expect exactly one
              -- `ExceededTimeLimit` exception in the adversary's ChainSync.
              exceptionsCorrect = case exceptionsByComponent ChainSyncClient stateView of
                [] -> not timeoutsEnabled
                [fromException -> Just (ExceededTimeLimit _)] -> timeoutsEnabled
                _ -> False
           in selectedCorrect && exceptionsCorrect
      )
  where
    delaySchedule :: HasHeader blk => BlockTree blk -> PointSchedule blk
    delaySchedule tree =
      let trunkTip = getTrunkTip tree
          branch = getOnlyBranch tree
          intersectM = case btbPrefix branch of
            (AF.Empty _)       -> Nothing
            (_ AF.:> tipBlock) -> Just tipBlock
          branchTip = getOnlyBranchTip tree
          psSchedule = peers'
            -- Eagerly serve the honest tree, but after the adversary has
            -- advertised its chain.
            [ (Time 0, scheduleTipPoint trunkTip) : case intersectM of
                Nothing ->
                  [ (Time 0.5, scheduleHeaderPoint trunkTip),
                    (Time 0.5, scheduleBlockPoint trunkTip)
                  ]
                Just intersect ->
                  [ (Time 0.5, scheduleHeaderPoint intersect),
                    (Time 0.5, scheduleBlockPoint intersect),
                    (Time 5, scheduleHeaderPoint trunkTip),
                    (Time 5, scheduleBlockPoint trunkTip)
                  ]
            ]
            -- The one adversarial peer advertises and serves up to the
            -- intersection early, then waits more than the short wait timeout.
            [ (Time 0, scheduleTipPoint branchTip) : case intersectM of
                -- the alternate branch forks from `Origin`
                Nothing -> []
                -- the alternate branch forks from `intersect`
                Just intersect ->
                  [ (Time 0, scheduleHeaderPoint intersect),
                    (Time 0, scheduleBlockPoint intersect)
                  ]
            ]
          -- We want to wait more than the short wait timeout
          psMinEndTime = Time 11
       in PointSchedule {psSchedule, psMinEndTime}
