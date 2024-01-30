{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Test.Consensus.Genesis.Tests.DensityDisconnect (tests) where

import           Cardano.Slotting.Slot (unSlotNo)
import           Data.Bifunctor (second)
import           Data.Foldable (minimumBy, toList)
import           Data.Function (on)
import           Data.List (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup (Endo (..))
import           Ouroboros.Consensus.Block (fromWithOrigin, withOrigin)
import           Ouroboros.Consensus.Block.Abstract (Header, getHeader)
import           Ouroboros.Consensus.Config.SecurityParam
                     (SecurityParam (SecurityParam), maxRollbacks)
import           Ouroboros.Consensus.Genesis.Governor (densityDisconnect,
                     sharedCandidatePrefix)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (GenesisWindow (..))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Tip (TipGenesis), tipFromHeader)
import           Test.Consensus.BlockTree
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.PointSchedule.Peers
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TersePrinting (terseHFragment)
import           Test.Util.TestBlock (TestBlock)
import           Test.Util.TestEnv (adjustQuickCheckMaxSize,
                     adjustQuickCheckTests)

tests :: TestTree
tests =
    adjustQuickCheckTests (* 4) $
  adjustQuickCheckMaxSize (`div` 5) $
  testGroup "gdd" [
    testProperty "basic" prop_densityDisconnectStatic,
    testProperty "monotonicity" prop_densityDisconnectMonotonic
  ]

branchTip :: AnchoredFragment TestBlock -> Tip TestBlock
branchTip =
  either (const TipGenesis) tipFromHeader . AF.head

toHeaders :: AnchoredFragment TestBlock -> AnchoredFragment (Header TestBlock)
toHeaders = AF.mapAnchoredFragment getHeader

data StaticCandidates =
  StaticCandidates {
    k        :: SecurityParam,
    sgen     :: GenesisWindow,
    suffixes :: Map PeerId (AnchoredFragment (Header TestBlock)),
    tips     :: Map PeerId (Tip TestBlock),
    loeFrag  :: AnchoredFragment (Header TestBlock)
  }
  deriving Show

-- | Define one selection for each branch of the given block tree, consisting of the first @k@ blocks (or what's
-- available) of the branch's suffix.
--
-- Return a 'StaticCandidates' value for each of them, containing the candidate suffixes and LoE fragment computed by
-- 'sharedCandidatePrefix' from the selection.
staticCandidates :: GenesisTest TestBlock s -> [StaticCandidates]
staticCandidates GenesisTest {gtSecurityParam, gtGenesisWindow, gtBlockTree} =
  one . toHeaders <$> selections
  where
    one curChain =
      StaticCandidates {
        k = gtSecurityParam,
        sgen = gtGenesisWindow,
        suffixes,
        tips,
        loeFrag
      }
      where
        (loeFrag, suffixes) =
          sharedCandidatePrefix curChain (toHeaders <$> candidates)

    selections = selection <$> branches

    selection branch =
      AF.takeOldest (AF.length (btbPrefix branch) + fromIntegral (maxRollbacks gtSecurityParam)) (btbFull branch)

    tips = either (const TipGenesis) tipFromHeader . AF.head <$> candidates

    candidates :: Map PeerId (AnchoredFragment TestBlock)
    candidates = Map.fromList (zip (HonestPeer : enumerateAdversaries) chains)

    chains = btTrunk gtBlockTree : (btbFull <$> branches)

    branches = btBranches gtBlockTree

-- | Check that the GDD disconnects from some peers for each full Genesis window starting at any of a block tree's
-- intersections, and that it's not the honest peer.
prop_densityDisconnectStatic :: Property
prop_densityDisconnectStatic =
  forAll gen $ \ StaticCandidates {k, sgen, suffixes, tips, loeFrag} -> do
    let (disconnect, _) = densityDisconnect sgen k suffixes tips mempty loeFrag
    not (null disconnect) && HonestPeer `notElem` disconnect
  where
    gen = do
      gt <- genChains (QC.choose (1, 4))
      elements (staticCandidates gt)

data EvolvingPeer =
  EvolvingPeer {
    forkPrefix  :: AnchoredFragment (Header TestBlock),
    forkSuffix  :: AnchoredFragment (Header TestBlock),
    candidate   :: AnchoredFragment (Header TestBlock),
    suffix      :: [Header TestBlock],
    tip         :: Tip TestBlock,
    prefixSlots :: Int,
    killed      :: Bool
  }
  deriving Show

data EvolvingPeers =
  EvolvingPeers {
    k       :: SecurityParam,
    sgen    :: GenesisWindow,
    peers   :: Peers EvolvingPeer,
    loeFrag :: AnchoredFragment (Header TestBlock)
  }
  deriving Show

lastSlot ::
  AF.HasHeader b =>
  AnchoredFragment b ->
  Int
lastSlot =
  fromIntegral . withOrigin 0 unSlotNo . AF.headSlot

initCandidates :: GenesisTest TestBlock s -> EvolvingPeers
initCandidates GenesisTest {gtSecurityParam, gtGenesisWindow, gtBlockTree} =
  EvolvingPeers {
    k = gtSecurityParam,
    sgen = gtGenesisWindow,
    peers,
    loeFrag = AF.Empty AF.AnchorGenesis
  }
  where
    peers = mkPeers (peer trunk (AF.Empty (AF.headAnchor trunk)) (btTrunk gtBlockTree)) (branchPeer <$> branches)

    branchPeer branch = peer (btbPrefix branch) (btbSuffix branch) (btbFull branch)

    peer forkPrefix forkSuffix chain =
      EvolvingPeer {
        forkPrefix = toHeaders forkPrefix,
        forkSuffix = toHeaders forkSuffix,
        candidate = AF.Empty AF.AnchorGenesis,
        suffix = AF.toOldestFirst headers,
        tip = branchTip chain,
        prefixSlots = lastSlot forkPrefix,
        killed = False
      }
      where
        headers = toHeaders chain

    trunk = btTrunk gtBlockTree

    branches = btBranches gtBlockTree

data MonotonicityResult =
  HonestKilled
  |
  Nonmonotonic
  |
  Finished
  deriving Show

-- | Check whether the honest peer was killed or a peer's new losing state violates monotonicity, i.e. if it was found
-- to be losing before, it shouldn't be found winning later.
--
-- If that is the case, return @Left (False, peers)@ to indicate that the test is over and failed.
--
-- Otherwise, remove all adversaries that either have no more blocks or have more than @sgen@ slots after their fork
-- intersection.
--
-- If no adversaries remain, return @Left (True, peers)@ to indicate that the test is over and succeeded.
--
-- Otherwise, return @Right remaining@ to continue with the next step.
updatePeers ::
  GenesisWindow ->
  PeerId ->
  [PeerId] ->
  Peers EvolvingPeer ->
  Either (MonotonicityResult, Peers EvolvingPeer) (Peers EvolvingPeer)
updatePeers (GenesisWindow sgen) target disconnect peers
  | HonestPeer `elem` disconnect
  = Left (HonestKilled, peers)
  | killed peer
  , not (target `elem` disconnect)
  = Left (Nonmonotonic, peers)
  | null remaining
  = Left (Finished, peers)
  | otherwise
  = Right peers {others = remaining}
  where
    Peer {value = peer} = getPeer target peers

    remaining = Map.filter (not . discardPeer) (others peers)

    discardPeer Peer {value = EvolvingPeer {candidate, suffix, prefixSlots}} =
      null suffix || lastSlot candidate - prefixSlots > fromIntegral sgen

-- | Find the earliest intersection, used to compute the selection.
firstBranch :: Peers EvolvingPeer -> Peer EvolvingPeer
firstBranch peers =
  minimumBy (compare `on` predicate) (toList (others peers))
  where
    predicate = fromWithOrigin 0 . AF.anchorToSlotNo . AF.anchor . forkSuffix . value

-- | Take one block off the peer's suffix and append it to the candidate fragment.
--
-- Since we don't remove the honest peer when it's exhausted, this may be called with an empty suffix.
movePeer :: EvolvingPeer -> EvolvingPeer
movePeer = \case
  peer@EvolvingPeer {candidate, suffix = h : t} -> peer {candidate = candidate AF.:> h, suffix = t}
  peer -> peer

-- | Repeatedly run the GDD, each time updating a random peer to advance by one block.
-- The selection is set to the first k blocks of the first fork.
-- The tips are the last blocks of each full branch.
-- The returned 'Bool' indicates whether the honest peer won and no monotonicity violations were detected.
evolveBranches ::
  EvolvingPeers ->
  Gen (MonotonicityResult, EvolvingPeers)
evolveBranches EvolvingPeers {k, sgen, peers = initialPeers} =
  step initialPeers
  where
    step ps = do
      target <- elements ids
      let
          curChain = selection (value (firstBranch ps))
          next = updatePeer movePeer target ps
          (loeFrag, suffixes) = sharedCandidatePrefix curChain (candidate . value <$> toMap next)
          disconnect = fst (densityDisconnect sgen k suffixes tips mempty loeFrag)
      either (pure . second (result loeFrag)) step (updatePeers sgen target disconnect next)
      where
        result f final = EvolvingPeers {k, sgen, peers = final, loeFrag = f}

        selection branch =
          AF.takeOldest (AF.length (forkPrefix branch) + fromIntegral k') (forkSuffix branch)

        ids = toList (getPeerIds ps)

    SecurityParam k' = k
    tips = tip <$> toMap' initialPeers

peerInfo :: EvolvingPeers -> [String]
peerInfo EvolvingPeers {k = SecurityParam k, sgen = GenesisWindow sgen, peers = Peers {honest, others}, loeFrag} =
  [
    "k: " <> show k,
    "sgen: " <> show sgen,
    "loeFrag: " <> terseHFragment loeFrag,
    intercalate "\n" (prettyBlockTree tree)
  ]
  where
    tree = foldr addBranch' (mkTrunk (candidate (value honest))) (candidate . value <$> others)

prop_densityDisconnectMonotonic :: Property
prop_densityDisconnectMonotonic =
  forAllBlind gen $ \ (result, final) ->
    appEndo (foldMap (Endo . counterexample) (peerInfo final)) $
    check result
  where
    check = \case
      HonestKilled -> counterexample "Honest peer was killed" False
      Nonmonotonic -> counterexample "Peer went from losing to winning" False
      Finished -> property True

    gen = do
      gt <- genChains (QC.choose (1, 4))
      evolveBranches (initCandidates gt)
