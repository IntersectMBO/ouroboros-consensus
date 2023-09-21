{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.GenesisTest (tests) where

import           Control.Monad.IOSim (runSimOrThrow)
import           Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           Ouroboros.Consensus.Block.Abstract hiding (Header)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.AnchoredFragment (Anchor (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.AnchoredSeq (headAnchor)
import qualified Ouroboros.Network.AnchoredSeq as AF
import           Test.Ouroboros.Consensus.ChainGenerator.Honest
                     (HonestRecipe (HonestRecipe))
import           Test.Ouroboros.Consensus.ChainGenerator.Params
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.Adversarial hiding
                     (tests)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Tests.BlockTree as BT
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.GenChain
                     (genChains)
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.PointSchedule
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.Sync
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.QuickCheck.Random (QCGen)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock hiding (blockTree)
import           Test.Util.Tracer (recordingTracerTVar)

tests :: TestTree
tests = testGroup "Genesis tests"
    [ testProperty "blargh" prop_syncGenesis
    ]

prop_syncGenesis :: SomeTestAdversarial -> QCGen -> QC.Property
prop_syncGenesis (SomeTestAdversarial _ _ params) seed =
  withMaxSuccess 10 $
    runSimOrThrow $ runTest (exampleTestSetup params seed)

newtype GenesisWindow = GenesisWindow { getGenesisWindow :: SlotNo }
  deriving stock (Show)

data TestSetup = TestSetup {
    secParam                  :: SecurityParam
  , genesisWindow             :: GenesisWindow
  , schedule                  :: PointSchedule
  , genesisAcrossIntersection :: Bool
  , genesisAfterIntersection  :: Bool
  }
  deriving stock (Show)

exampleTestSetup ::
  TestAdversarial base hon ->
  QCGen ->
  TestSetup
exampleTestSetup params seed =
  TestSetup {
    secParam      = SecurityParam (fromIntegral k)
  , genesisWindow = GenesisWindow (fromIntegral scg)
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

    fragLenA = BT.slotLength badChain
    advLenAfterIntersection = AF.length badChainSuffix

    blockTree = genChains (testAscA params) (testRecipeA params) (testRecipeA' params) seed
    goodChain = BT.trunk blockTree

    BT.BlockTreeBranch { suffix = badChainSuffix, full = badChain } =
      head $ BT.branches blockTree

stripBlockBodies :: TestFrag -> TestFragH
stripBlockBodies = AF.bimap AF.castAnchor getHeader

runTest ::
  forall m.
  IOLike m =>
  TestSetup ->
  m Property
runTest TestSetup{..} = do
    (tracer, getTrace) <- recordingTracerTVar

    let advPeer = PeerId "adversary"
    g <- makeMockedChainSyncServer HonestPeer goodChainH tracer
    b <- makeMockedChainSyncServer advPeer badChainH tracer
    let servers = Map.fromList [(HonestPeer, g), (advPeer, b)]

    frag <- either (error . show) id <$> syncPeers secParam schedule servers tracer
    trace <- unlines <$> getTrace

    pure
      $ classify genesisAfterIntersection "Long range attack"
      $ classify genesisAcrossIntersection "Genesis potential"
      $ counterexample ("result: " <> condense frag)
      $ counterexample trace
      $ genesisAfterIntersection ==> not $ isHonestTestFragH frag

  where
    goodChainH = stripBlockBodies $ value $ honest $ frags schedule
    badChainH = stripBlockBodies $ value $ fromJust (others (frags schedule) !? PeerId "adversary")

    isHonestTestFragH :: TestFragH -> Bool
    isHonestTestFragH frag = case headAnchor frag of
        AnchorGenesis   -> True
        Anchor _ hash _ -> isHonestTestHeaderHash hash

    isHonestTestHeaderHash :: HeaderHash TestBlock -> Bool
    isHonestTestHeaderHash = all (0 ==) . unTestHash
