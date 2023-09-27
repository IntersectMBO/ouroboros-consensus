{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.PointScheduleTest (tests) where

import           Control.Monad.IOSim (runSimOrThrow)
import qualified Data.Map.Strict as Map
import           Debug.Trace (traceM)
import           Ouroboros.Consensus.Block.Abstract (BlockNo (unBlockNo),
                     SlotNo (unSlotNo), withOrigin)
import           Ouroboros.Consensus.Config.SecurityParam
                     (SecurityParam (SecurityParam))
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Test.Ouroboros.Consensus.ChainGenerator.Honest
                     (HonestRecipe (HonestRecipe))
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Kcp (Kcp),
                     Len (Len), Scg (Scg))
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.Adversarial
                     (SomeTestAdversarial (..), TestAdversarial (..))
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.GenChain
                     (genChains)
import           Test.Ouroboros.Consensus.ChainGenerator.Tests.PointSchedule
                     (Peer (Peer), PeerId (HonestPeer, PeerId), Peers (Peers),
                     PointSchedule, banalPointSchedule, fastAdversarySchedule)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck (Property, once, property)
import           Test.QuickCheck.Random (QCGen)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Util.Orphans.IOLike ()

tests :: TestTree
tests = testGroup "pregenerated point schedule tests"
    [ testProperty "yoink" (once prop_pointSchedule)
    ]

prop_pointSchedule :: SomeTestAdversarial -> QCGen -> QC.Property
prop_pointSchedule (SomeTestAdversarial _ _ params) seed =
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
    -- TODO: also need: at least k+1 blocks after the intersection
    genesisAfterIntersection =
         fragLenA > fromIntegral scg
      && advLenAfterIntersection > fromIntegral k
    advLenAfterIntersection =
      withOrigin 0 unBlockNo (AF.headBlockNo badChain) - unSlotNo prefixLen
    (goodChain, badChain, _, prefixLen, fragLenA) = genChains (testAscA params) (testRecipeA params) (testRecipeA' params) seed

runTest ::
  IOLike m =>
  TestSetup ->
  m Property
runTest TestSetup{..} = do
  traceM (condense schedule)
  pure (property True)
