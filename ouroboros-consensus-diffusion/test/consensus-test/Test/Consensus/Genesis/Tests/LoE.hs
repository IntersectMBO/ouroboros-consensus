{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Consensus.Genesis.Tests.LoE (tests) where

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.IOLike (Time (Time))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..))
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.PeerSimulator.Run (SchedulerConfig (..),
                     defaultSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Peers (mkPeers)
import           Test.Consensus.PointSchedule.Shrinking (shrinkPeerSchedules)
import           Test.Consensus.PointSchedule.SinglePeer (SchedulePoint (..))
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TersePrinting (tersePoint)
import           Test.Util.TestBlock (TestBlock)
import           Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  testGroup
    "LoE"
    [
      adjustQuickCheckTests (`div` 5) $
        testProperty "adversary hits timeouts" prop_adversaryHitsTimeouts
    ]

-- | Tests that the selection advances in presence of the LoE when a peer is
-- killed by something that is not LoE-aware, eg. the timeouts.
prop_adversaryHitsTimeouts :: Property
prop_adversaryHitsTimeouts =
  noShrinking $
    forAllGenesisTest
      ( do
          gt@GenesisTest {gtBlockTree} <- genChains (pure 1)
          let ps = delaySchedule gtBlockTree
          pure (ps <$ gt)
      )
      -- NOTE: Crucially, there must be timeouts for this test.
      ( defaultSchedulerConfig
          { scEnableChainSyncTimeouts = True,
            scEnableLoE = True
          }
      )
      shrinkPeerSchedules
      ( \GenesisTest {gtBlockTree} StateView {svSelectedChain} ->
        tabulate "binary log buckets of the length of the honest chain" [show (round @_ @Int (2 ** (realToFrac @_ @Double (round @_ @Int (logBase 2 (realToFrac @_ @Double (1 + AF.length (btTrunk gtBlockTree))))))))] $
        counterexample ("Selection is not the honest tip: " ++ tersePoint (AF.headPoint (btTrunk gtBlockTree)) ++ " / " ++ tersePoint (AF.castPoint (AF.headPoint svSelectedChain))) $
          AF.headPoint (btTrunk gtBlockTree) == AF.castPoint (AF.headPoint svSelectedChain)
      )
  where
    getOnlyBranch :: BlockTree blk -> BlockTreeBranch blk
    getOnlyBranch BlockTree {btBranches} = case btBranches of
      [branch] -> branch
      _        -> error "tree must have exactly one alternate branch"

    delaySchedule :: BlockTree TestBlock -> PeersSchedule TestBlock
    delaySchedule tree =
      let trunkTip = NotOrigin $ case btTrunk tree of
            (AF.Empty _)       -> error "tree must have at least one block"
            (_ AF.:> tipBlock) -> tipBlock
          branch = getOnlyBranch tree
          intersectM = case btbPrefix branch of
            (AF.Empty _)       -> Nothing
            (_ AF.:> tipBlock) -> Just tipBlock
          branchTip = NotOrigin $ case btbFull branch of
            (AF.Empty _) -> error "alternate branch must have at least one block"
            (_ AF.:> tipBlock) -> tipBlock
       in mkPeers
            -- The honest peer eagerly serves its chain after 5s so as not to
            -- get disconnected by timeouts until @Time 15@.
            [ (Time 0, ScheduleTipPoint trunkTip),
              (Time 5, ScheduleHeaderPoint trunkTip),
              (Time 5, ScheduleBlockPoint trunkTip),
              (Time 16, ScheduleBlockPoint trunkTip),
              (Time 16.1, ScheduleBlockPoint trunkTip),
              (Time 16.2, ScheduleBlockPoint trunkTip)
            ]
            -- The one adversarial peer advertises and serves up to the
            -- intersection early, then waits more than the short wait timeout.
            [ (Time 0, ScheduleTipPoint branchTip) : case intersectM of
                -- the alternate branch forks from `Origin`
                Nothing -> [(Time 11, ScheduleTipPoint branchTip)]
                -- the alternate branch forks from `intersect`
                Just intersect ->
                  [ (Time 0, ScheduleHeaderPoint (NotOrigin intersect)),
                    (Time 0, ScheduleBlockPoint (NotOrigin intersect)),
                    (Time 0.5, ScheduleBlockPoint (NotOrigin intersect)),
                    (Time 16, ScheduleBlockPoint (NotOrigin intersect))
                  ]
            ]
