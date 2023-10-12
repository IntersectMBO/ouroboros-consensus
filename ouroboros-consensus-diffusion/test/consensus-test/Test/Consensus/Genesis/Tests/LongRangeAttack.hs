{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.Genesis.Tests.LongRangeAttack (tests) where

import           Control.Monad.IOSim (runSimOrThrow)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Ouroboros.Consensus.Block.Abstract hiding (Header)
import           Ouroboros.Consensus.Config
import           Ouroboros.Network.AnchoredFragment (headAnchor)
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.AnchoredFragment.Extras as AF
import qualified Test.Consensus.BlockTree as BT
import           Test.Consensus.BlockTree (BlockTreeBranch (btbFull))
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.PointSchedule
import           Test.Ouroboros.Consensus.ChainGenerator.Honest
                     (HonestRecipe (HonestRecipe))
import           Test.Ouroboros.Consensus.ChainGenerator.Params
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.Adversarial hiding
                     (tests)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.QuickCheck.Random (QCGen)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock hiding (blockTree)

tests :: TestTree
tests = testProperty "long range attack" prop_longRangeAttack

prop_longRangeAttack :: SomeTestAdversarial -> QCGen -> QC.Property
prop_longRangeAttack (SomeTestAdversarial _ _ params) seed =
  withMaxSuccess 10 $
    runSimOrThrow $ runTest setup $ \fragment ->
      pure
        $ classify genesisAfterIntersection "Long range attack"
        $ classify genesisAcrossIntersection "Genesis potential"
        $ genesisAfterIntersection ==> not $ isHonestTestFragH fragment

  where
    setup@TestSetup{..} = exampleTestSetup params seed

    isHonestTestFragH :: TestFragH -> Bool
    isHonestTestFragH frag = case headAnchor frag of
        AF.AnchorGenesis   -> True
        AF.Anchor _ hash _ -> isHonestTestHeaderHash hash

    isHonestTestHeaderHash :: HeaderHash TestBlock -> Bool
    isHonestTestHeaderHash = all (0 ==) . unTestHash

exampleTestSetup ::
  TestAdversarial base hon ->
  QCGen ->
  TestSetup
exampleTestSetup params seed =
  TestSetup {
    secParam      = SecurityParam (fromIntegral k)
  , genesisWindow = GenesisWindow (fromIntegral scg)
  , schedule      = fromJust schedule -- FIXME: discard such test cases in QuickCheck
  , ..
  }
  where
    schedule | fast = fastAdversarySchedule (Peers (Peer HonestPeer goodChain) (Map.fromList [(advId, Peer advId badChain)]))
             | otherwise = banalPointSchedule (Peers (Peer HonestPeer goodChain) (Map.fromList [(advId, Peer advId badChain)]))
    fast = True
    advId = PeerId "adversary"
    HonestRecipe (Kcp k) (Scg scg) _ (Len len) = testRecipeH params

    genesisAcrossIntersection = not genesisAfterIntersection && len > scg
    genesisAfterIntersection = fragLenA > scg && advLenAfterIntersection > k

    fragLenA = AF.slotLength badChain
    advLenAfterIntersection = AF.length badChainSuffix

    honestAsc = testAscH params

    blockTree = genChains (testAscA params) (testRecipeA params) (testRecipeA' params) seed
    goodChain = BT.btTrunk blockTree

    BT.BlockTreeBranch { btbSuffix = badChainSuffix, btbFull = badChain } =
      head $ BT.btBranches blockTree
