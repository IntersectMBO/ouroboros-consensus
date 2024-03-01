{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Consensus.Genesis.Tests.LoE (tests) where

import           Ouroboros.Consensus.Util.IOLike (Time (Time), fromException)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Driver.Limits
                     (ProtocolLimitFailure (ExceededTimeLimit))
import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..))
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.PeerSimulator.Run (SchedulerConfig (..),
                     defaultSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Peers (PeerId (..), mkPeers)
import           Test.Consensus.PointSchedule.Shrinking (shrinkPeerSchedules)
import           Test.Consensus.PointSchedule.SinglePeer (scheduleBlockPoint,
                     scheduleHeaderPoint, scheduleTipPoint)
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
        testProperty "adversary does not hit timeouts" (prop_adversaryHitsTimeouts False),
      adjustQuickCheckTests (`div` 5) $
        testProperty "adversary hits timeouts" (prop_adversaryHitsTimeouts True)
    ]

-- | Tests that the selection advances in presence of the LoE when a peer is
-- killed by something that is not LoE-aware, eg. the timeouts.
prop_adversaryHitsTimeouts :: Bool -> Property
prop_adversaryHitsTimeouts timeoutsEnabled =
  noShrinking $
    forAllGenesisTest
      ( do
          gt@GenesisTest {gtBlockTree} <- genChains (pure 1)
          let ps = delaySchedule gtBlockTree
          pure (ps <$ gt)
      )
      -- NOTE: Crucially, there must be timeouts for this test, and no LoP.
      ( defaultSchedulerConfig
          { scEnableChainSyncTimeouts = timeoutsEnabled,
            scEnableLoE = True,
            scEnableLoP = False
          }
      )
      shrinkPeerSchedules
      ( \GenesisTest {gtBlockTree} StateView {svSelectedChain, svChainSyncExceptions} ->
        tabulate "binary log buckets of the length of the honest chain" [show (round @_ @Int (2 ** (realToFrac @_ @Double (round @_ @Int (logBase 2 (realToFrac @_ @Double (1 + AF.length (btTrunk gtBlockTree))))))))] $
        counterexample ("Selection is not the honest tip: " ++ tersePoint (AF.headPoint (btTrunk gtBlockTree)) ++ " / " ++ tersePoint (AF.castPoint (AF.headPoint svSelectedChain))) $
          let treeTipPoint = AF.headPoint $ btTrunk gtBlockTree
              selectedTipPoint = AF.castPoint $ AF.headPoint svSelectedChain
              selectedCorrect = timeoutsEnabled == (treeTipPoint == selectedTipPoint)
              exceptionsCorrect = case svChainSyncExceptions of
                [] -> not timeoutsEnabled
                [ChainSyncException (PeerId _) exn] ->
                  case fromException exn of
                    Just (ExceededTimeLimit _) -> timeoutsEnabled
                    _                          -> False
                _ -> False
           in selectedCorrect && exceptionsCorrect
      )
  where
    getOnlyBranch :: BlockTree blk -> BlockTreeBranch blk
    getOnlyBranch BlockTree {btBranches} = case btBranches of
      [branch] -> branch
      _        -> error "tree must have exactly one alternate branch"

    delaySchedule :: BlockTree TestBlock -> PeersSchedule TestBlock
    delaySchedule tree =
      let trunkTip = case btTrunk tree of
            (AF.Empty _)       -> error "tree must have at least one block"
            (_ AF.:> tipBlock) -> tipBlock
          branch = getOnlyBranch tree
          intersectM = case btbPrefix branch of
            (AF.Empty _)       -> Nothing
            (_ AF.:> tipBlock) -> Just tipBlock
          branchTip = case btbFull branch of
            (AF.Empty _) -> error "alternate branch must have at least one block"
            (_ AF.:> tipBlock) -> tipBlock
       in mkPeers
            -- Eagerly serve the honest tree, but after the adversary has
            -- advertised its chain.
            ( (Time 0, scheduleTipPoint trunkTip) : case intersectM of
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
            )
            -- The one adversarial peer advertises and serves up to the
            -- intersection early, then waits more than the short wait timeout.
            [ (Time 0, scheduleTipPoint branchTip) : case intersectM of
                -- the alternate branch forks from `Origin`
                Nothing -> [(Time 11, scheduleTipPoint branchTip)]
                -- the alternate branch forks from `intersect`
                Just intersect ->
                  [ (Time 0, scheduleHeaderPoint intersect),
                    (Time 0, scheduleBlockPoint intersect),
                    (Time 11, scheduleBlockPoint intersect)
                  ]
            ]
