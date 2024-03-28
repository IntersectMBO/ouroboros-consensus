{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ViewPatterns          #-}
module Test.Consensus.Genesis.Tests.DensityDisconnect (tests) where

import           Cardano.Slotting.Slot (WithOrigin (..), unSlotNo)
import           Control.Exception (fromException)
import           Control.Monad.Class.MonadTime.SI (Time (..))
import           Data.Bifunctor (second)
import           Data.Foldable (minimumBy, toList)
import           Data.Function (on)
import           Data.Functor (($>), (<&>))
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
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (ChainSyncClientException (DensityTooLow),
                     ChainSyncState (..))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (HasHeader, Tip (TipGenesis),
                     tipFromHeader)
import           Test.Consensus.BlockTree
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.Genesis.Setup.Classifiers (classifiers,
                     genesisWindowAfterIntersection)
import           Test.Consensus.PeerSimulator.Run
                     (SchedulerConfig (scEnableLoE), defaultSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView
                     (PeerSimulatorComponent (..), StateView (..),
                     exceptionsByComponent)
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Peers
import           Test.Consensus.PointSchedule.Shrinking
                     (shrinkByRemovingAdversaries)
import           Test.Consensus.PointSchedule.SinglePeer (SchedulePoint (..),
                     scheduleBlockPoint, scheduleHeaderPoint, scheduleTipPoint)
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
    testProperty "monotonicity" prop_densityDisconnectMonotonic,
    testProperty "re-triggers chain selection on disconnection" prop_densityDisconnectTriggersChainSel
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

    tips = branchTip <$> candidates

    candidates :: Map PeerId (AnchoredFragment TestBlock)
    candidates = Map.fromList (zip (HonestPeer : enumerateAdversaries) chains)

    chains = btTrunk gtBlockTree : (btbFull <$> branches)

    branches = btBranches gtBlockTree

-- | Check that the GDD disconnects from some peers for each full Genesis window starting at any of a block tree's
-- intersections, and that it's not the honest peer.
prop_densityDisconnectStatic :: Property
prop_densityDisconnectStatic =
  forAll gen $ \ StaticCandidates {k, sgen, suffixes, loeFrag} -> do
    let (disconnect, _) = densityDisconnect sgen k (mkState <$> suffixes) suffixes loeFrag
    counterexample "it should disconnect some node" (not (null disconnect))
      .&&.
     counterexample "it should not disconnect the honest peer"
       (HonestPeer `notElem` disconnect)
  where
    mkState :: AnchoredFragment (Header TestBlock) -> ChainSyncState TestBlock
    mkState frag =
      ChainSyncState {
        csCandidate = frag,
        csLatestSlot = Just (AF.headSlot frag),
        csIdling = False
      }
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
          candidates = candidate . value <$> toMap next
          states =
            candidates <&> \ csCandidate ->
              ChainSyncState {
                csCandidate,
                csIdling = False,
                csLatestSlot = Just (AF.headSlot csCandidate)
              }
          (loeFrag, suffixes) = sharedCandidatePrefix curChain candidates
          disconnect = fst (densityDisconnect sgen k states suffixes loeFrag)
      either (pure . second (result loeFrag)) step (updatePeers sgen target disconnect next)
      where
        result f final = EvolvingPeers {k, sgen, peers = final, loeFrag = f}

        selection branch =
          AF.takeOldest (AF.length (forkPrefix branch) + fromIntegral k') (forkSuffix branch)

        ids = toList (getPeerIds ps)

    SecurityParam k' = k
    _tips = tip <$> toMap' initialPeers

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

-- | Tests that a GDD disconnection re-triggers chain selection, i.e. when the current
-- selection is blocked by LoE, and the leashing adversary reveals it is not dense enough,
-- it gets disconnected and then the selection progresses.
prop_densityDisconnectTriggersChainSel :: Property
prop_densityDisconnectTriggersChainSel =
  forAllGenesisTest
    ( do
        gt@GenesisTest {gtBlockTree} <- genChains (pure 1)
        let ps = lowDensitySchedule gtBlockTree
            cls = classifiers gt
        if genesisWindowAfterIntersection cls
          then pure $ gt $> ps
          else discard
    )

    (defaultSchedulerConfig {scEnableLoE = True})

    shrinkByRemovingAdversaries

    ( \GenesisTest {gtBlockTree} stateView@StateView {svTipBlock} ->
        let
          exnCorrect = case exceptionsByComponent ChainSyncClient stateView of
            [fromException -> Just DensityTooLow] -> True
            _                                     -> False
          tipPointCorrect = Just (getTrunkTip gtBlockTree) == svTipBlock
        in exnCorrect && tipPointCorrect
    )

  where
    getOnlyBranch :: BlockTree blk -> BlockTreeBranch blk
    getOnlyBranch BlockTree {btBranches} = case btBranches of
      [branch] -> branch
      _        -> error "tree must have exactly one alternate branch"

    getTrunkTip :: HasHeader blk => BlockTree blk -> blk
    getTrunkTip tree = case btTrunk tree of
      (AF.Empty _)       -> error "tree must have at least one block"
      (_ AF.:> tipBlock) -> tipBlock

    -- 1. The adversary advertises blocks up to the intersection.
    -- 2. The honest node advertises all its chain, which is
    --    long enough to be blocked by the LoE.
    -- 3. The adversary gives a block after the genesis window,
    --    which should allow the GDD to realize that the chain
    --    is not dense enough, and that the whole of the honest
    --    chain should be selected.
    lowDensitySchedule :: HasHeader blk => BlockTree blk -> Peers (PeerSchedule blk)
    lowDensitySchedule tree =
      let trunkTip = getTrunkTip tree
          branch = getOnlyBranch tree
          intersect = case btbPrefix branch of
            (AF.Empty _)       -> Origin
            (_ AF.:> tipBlock) -> At tipBlock
          advTip = case btbFull branch of
            (AF.Empty _) -> error "alternate branch must have at least one block"
            (_ AF.:> tipBlock) -> tipBlock
       in mkPeers
            -- Eagerly serve the honest tree, but after the adversary has
            -- advertised its chain up to the intersection.
            [ (Time 0, scheduleTipPoint trunkTip),
              (Time 0.5, scheduleHeaderPoint trunkTip),
              (Time 0.5, scheduleBlockPoint trunkTip)
            ]
            -- Advertise the alternate branch early, but wait for the honest
            -- node to have served its chain before disclosing the alternate
            -- branch is not dense enough.
            [[(Time 0, scheduleTipPoint advTip),
              (Time 0, ScheduleHeaderPoint intersect),
              (Time 0, ScheduleBlockPoint intersect),
              (Time 1, scheduleHeaderPoint advTip),
              (Time 1, scheduleBlockPoint advTip)
            ]]
