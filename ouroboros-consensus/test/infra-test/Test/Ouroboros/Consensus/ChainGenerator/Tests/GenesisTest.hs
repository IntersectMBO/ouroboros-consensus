{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.GenesisTest (tests) where

import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Tracer (traceWith)
import qualified Data.Map.Strict as Map
import           Ouroboros.Consensus.Block.Abstract hiding (Header)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.AnchoredFragment (headAnchor)
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.AnchoredFragment.Extras as AF
import           Test.Ouroboros.Consensus.ChainGenerator.Honest
                     (HonestRecipe (HonestRecipe))
import           Test.Ouroboros.Consensus.ChainGenerator.Params
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.Adversarial hiding
                     (tests)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Tests.BlockTree as BT
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.BlockTree
                     (BlockTreeBranch (btbFull))
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
  , blockTree                 :: BT.BlockTree TestBlock
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

    fragLenA = AF.slotLength badChain
    advLenAfterIntersection = AF.length badChainSuffix

    blockTree = genChains (testAscA params) (testRecipeA params) (testRecipeA' params) seed
    goodChain = BT.btTrunk blockTree

    BT.BlockTreeBranch { btbSuffix = badChainSuffix, btbFull = badChain } =
      head $ BT.btBranches blockTree

runTest ::
  forall m.
  IOLike m =>
  TestSetup ->
  m Property
runTest TestSetup{..} = do
    (tracer, getTrace) <- recordingTracerTVar

    mapM_ (traceWith tracer) $ BT.prettyPrint blockTree

    let advPeer = PeerId "adversary"
    g <- makeMockedChainSyncServer HonestPeer tracer blockTree
    b <- makeMockedChainSyncServer advPeer tracer blockTree
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
    isHonestTestFragH :: TestFragH -> Bool
    isHonestTestFragH frag = case headAnchor frag of
        AF.AnchorGenesis   -> True
        AF.Anchor _ hash _ -> isHonestTestHeaderHash hash

    isHonestTestHeaderHash :: HeaderHash TestBlock -> Bool
    isHonestTestHeaderHash = all (0 ==) . unTestHash
