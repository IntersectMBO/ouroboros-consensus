{-# LANGUAGE NamedFieldPuns #-}
module Test.Consensus.Genesis.Tests.DensityDisconnect (tests) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Ouroboros.Consensus.Block.Abstract (Header, getHeader)
import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam,
                     maxRollbacks)
import           Ouroboros.Consensus.Genesis.Governor (densityDisconnect,
                     sharedCandidatePrefix)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (GenesisWindow (..))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Tip (TipGenesis), tipFromHeader)
import           Test.Consensus.BlockTree
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.PointSchedule.Peers (PeerId (HonestPeer),
                     enumerateAdversaries)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock)
import           Test.Util.TestEnv (adjustQuickCheckMaxSize)

tests :: TestTree
tests =
  adjustQuickCheckMaxSize (`div` 5) $
  testGroup "gdd" [
    testProperty "density" prop_densityDisconnect
  ]

data TestCandidates =
  TestCandidates {
    k        :: SecurityParam,
    sgen     :: GenesisWindow,
    suffixes :: Map PeerId (AnchoredFragment (Header TestBlock)),
    tips     :: Map PeerId (Tip TestBlock),
    loeFrag  :: AnchoredFragment (Header TestBlock)
  }
  deriving Show

prefixCandidates :: GenesisTest -> [TestCandidates]
prefixCandidates GenesisTest {gtSecurityParam, gtGenesisWindow, gtBlockTree} =
  one . toHeaders <$> selections
  where
    one curChain =
      TestCandidates {
        k = gtSecurityParam,
        sgen = gtGenesisWindow,
        suffixes,
        tips,
        loeFrag
      }
      where
        (loeFrag, suffixes) =
          sharedCandidatePrefix gtSecurityParam curChain (toHeaders <$> candidates)

    selections = selection <$> branches

    selection branch =
      AF.takeOldest (AF.length (btbPrefix branch) + fromIntegral (maxRollbacks gtSecurityParam)) (btbFull branch)

    tips = either (const TipGenesis) tipFromHeader . AF.head <$> candidates

    candidates :: Map PeerId (AnchoredFragment TestBlock)
    candidates = Map.fromList (zip (HonestPeer : enumerateAdversaries) chains)

    chains = btTrunk gtBlockTree : (btbFull <$> branches)

    branches = btBranches gtBlockTree

    toHeaders = AF.mapAnchoredFragment getHeader

prop_densityDisconnect :: Property
prop_densityDisconnect =
  forAll gen $ \ TestCandidates {k, sgen, suffixes, tips, loeFrag} -> do
    let (disconnect, _) = densityDisconnect sgen k suffixes tips loeFrag
    not (null disconnect) && HonestPeer `notElem` disconnect
  where
    gen = do
      gt <- genChains (QC.choose (1, 4))
      elements (prefixCandidates gt)
