{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Consensus.Genesis.Tests.DensityDisconnect (tests) where

import           Cardano.Slotting.Slot (SlotNo (unSlotNo), WithOrigin (..))
import           Control.Exception (fromException)
import           Control.Monad.Class.MonadTime.SI (Time (..))
import           Data.Bifunctor
import           Data.Foldable (maximumBy, minimumBy, toList)
import           Data.Function (on)
import           Data.Functor (($>), (<&>))
import           Data.List (intercalate)
import           Data.List.NonEmpty (nonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Maybe.Strict (StrictMaybe (..))
import           Data.Semigroup (Endo (..))
import           Data.Set (Set, (\\))
import qualified Data.Set as Set
import           Ouroboros.Consensus.Block (Point (GenesisPoint),
                     WithOrigin (NotOrigin), blockSlot, fromWithOrigin,
                     withOrigin)
import           Ouroboros.Consensus.Block.Abstract (Header, getHeader)
import           Ouroboros.Consensus.Config.SecurityParam
                     (SecurityParam (SecurityParam), maxRollbacks)
import           Ouroboros.Consensus.Genesis.Governor (DensityBounds,
                     densityDisconnect, sharedCandidatePrefix)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (ChainSyncClientException (..), ChainSyncState (..))
import           Ouroboros.Consensus.Util.Condense (condense)
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
import           Test.Consensus.PeerSimulator.Trace (prettyDensityBounds)
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Peers
import           Test.Consensus.PointSchedule.Shrinking
                     (shrinkByRemovingAdversaries)
import           Test.Consensus.PointSchedule.SinglePeer (SchedulePoint (..),
                     scheduleBlockPoint, scheduleHeaderPoint, scheduleTipPoint)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.QuickCheck.Extras (unsafeMapSuchThatJust)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.PartialAccessors
import           Test.Util.TersePrinting (terseHFragment, terseHeader)
import           Test.Util.TestBlock (TestBlock)
import           Test.Util.TestEnv (adjustQuickCheckMaxSize,
                     adjustQuickCheckTests)

tests :: TestTree
tests =
  adjustQuickCheckTests (* 10) $
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
    suffixes :: [(PeerId, AnchoredFragment (Header TestBlock))],
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
          sharedCandidatePrefix curChain (second toHeaders <$> candidates)

    selections = selection <$> branches

    selection branch =
      AF.takeOldest (AF.length (btbPrefix branch) + fromIntegral (maxRollbacks gtSecurityParam)) (btbFull branch)

    tips = branchTip <$> Map.fromList candidates

    candidates :: [(PeerId, AnchoredFragment TestBlock)]
    candidates = zip (HonestPeer 1 : enumerateAdversaries) chains

    chains = btTrunk gtBlockTree : (btbFull <$> branches)

    branches = btBranches gtBlockTree

-- | Check that the GDD disconnects from some peers for each full Genesis window starting at any of a block tree's
-- intersections, and that it's not the honest peer.
prop_densityDisconnectStatic :: Property
prop_densityDisconnectStatic =
  forAll gen $ \ StaticCandidates {k, sgen, suffixes, loeFrag} -> do
    let (disconnect, _) = densityDisconnect sgen k (mkState <$> Map.fromList suffixes) suffixes loeFrag
    counterexample "it should disconnect some node" (not (null disconnect))
      .&&.
     counterexample "it should not disconnect the honest peers"
       (not $ any isHonestPeerId disconnect)
  where
    mkState :: AnchoredFragment (Header TestBlock) -> ChainSyncState TestBlock
    mkState frag =
      ChainSyncState {
        csCandidate = frag,
        csLatestSlot = SJust (AF.headSlot frag),
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
    forkSlot    :: WithOrigin SlotNo
  }
  deriving Show

data EvolvingPeers =
  EvolvingPeers {
    k        :: SecurityParam,
    sgen     :: GenesisWindow,
    peers    :: Peers EvolvingPeer,
    loeFrag  :: AnchoredFragment (Header TestBlock),
    fullTree :: BlockTree TestBlock
  }
  deriving Show

data Evolution =
  Evolution {
    peers  :: Peers EvolvingPeer,
    killed :: Set PeerId
  }

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
    loeFrag = AF.Empty AF.AnchorGenesis,
    fullTree = gtBlockTree
  }
  where
    peers = peers' [peer trunk (AF.Empty (AF.headAnchor trunk)) (btTrunk gtBlockTree)] (branchPeer <$> branches)

    branchPeer branch = peer (btbPrefix branch) (btbSuffix branch) (btbFull branch)

    peer forkPrefix forkSuffix chain =
      EvolvingPeer {
        forkPrefix = toHeaders forkPrefix,
        forkSuffix = toHeaders forkSuffix,
        candidate = AF.Empty AF.AnchorGenesis,
        suffix = AF.toOldestFirst headers,
        tip = branchTip chain,
        prefixSlots = lastSlot forkPrefix,
        forkSlot = AF.lastSlot forkSuffix
      }
      where
        headers = toHeaders chain

    trunk = btTrunk gtBlockTree

    branches = btBranches gtBlockTree

data UpdateEvent = UpdateEvent {
     -- | The peer whose candidate was extended in this step
    target   :: PeerId
    -- | The header appended to the candidate of 'target'
  , added    :: Header TestBlock
    -- | Peers that have been disconnected in the current step
  , killed   :: Set PeerId
    -- | The GDD data
  , bounds   :: [(PeerId, DensityBounds TestBlock)]
    -- | The current chains
  , tree     :: BlockTree (Header TestBlock)
  , loeFrag  :: AnchoredFragment (Header TestBlock)
  , curChain :: AnchoredFragment (Header TestBlock)
  }

snapshotTree :: Peers EvolvingPeer -> BlockTree (Header TestBlock)
snapshotTree Peers {honestPeers, adversarialPeers} =
  foldr addBranch' (mkTrunk (candidate (getHonestPeer honestPeers))) (candidate <$> adversarialPeers)

prettyUpdateEvent :: UpdateEvent -> [String]
prettyUpdateEvent UpdateEvent {target, added, killed, bounds, tree, loeFrag, curChain} =
  [
    "Extended " ++ condense target ++ " with " ++ terseHeader added,
    "        disconnect: " ++ show killed,
    "        LoE frag: " ++ terseHFragment loeFrag,
    "        selection: " ++ terseHFragment curChain
  ]
  ++ prettyDensityBounds bounds
  ++ "" : prettyBlockTree tree

data MonotonicityResult =
  HonestKilled
  |
  Nonmonotonic UpdateEvent
  |
  Finished

-- | Check whether the honest peer was killed or a peer's new losing state
-- violates monotonicity, i.e. if it was found to be losing before, it shouldn't
-- be found winning later.
--
-- If that is the case, return @Left (HonestKilled|Nonmonotonic, peers)@ to
-- indicate that the test is over and failed.
--
-- Otherwise, remove all adversaries that either have no more blocks or have
-- more than @sgen@ slots after their fork intersection. There is not other
-- motivation to shrink the adversary set other than ensuring termination.
--
-- If no adversaries remain, return @Left (Finished, peers)@ to indicate that
-- the test is over and succeeded.
--
-- Otherwise, return @Right remaining@ to continue with the next step.
updatePeers ::
  GenesisWindow ->
  Peers EvolvingPeer ->
  -- | Peers that were disconnected previously
  Set PeerId ->
  UpdateEvent ->
  Either (MonotonicityResult, Peers EvolvingPeer) Evolution
updatePeers (GenesisWindow sgen) peers killedBefore event@UpdateEvent {target, killed = killedNow}
  | HonestPeer 1 `Set.member` killedNow
  = Left (HonestKilled, peers)
  | not (null violations)
  = Left (Nonmonotonic event, peers)
  | null remaining
  = Left (Finished, peers)
  | otherwise
  = Right evo
  where
    -- The peers that were killed in an earlier step but not in the current one
    violations = killedBefore \\ killedNow

    -- The new state if no violations were detected
    evo@Evolution {peers = Peers {adversarialPeers = remaining}}
      | targetExhausted
      -- If the target is done, reset the set of killed peers, since other peers
      -- may have lost only against the target.
      -- Remove the target from the active peers.
      = Evolution {peers = deletePeer target peers, killed = mempty}
      | otherwise
      -- Otherwise replace the killed peers with the current set
      = Evolution {peers, killed = killedNow}

    -- Whether the extended peer is uninteresting for GDD from now on
    targetExhausted =
      -- Its fragment cannot be extended anymore, or
      null suffix ||
      -- Its candidate is longer than a Genesis window
      lastSlot candidate - prefixSlots > fromIntegral sgen

    Peer {value = EvolvingPeer {candidate, suffix, prefixSlots}} = getPeer target peers

-- | Find the peer whose candidate has the earliest intersection.
-- If no peer has reached its fork suffix yet, return the one with the highest slot.
--
-- The selection will then be computed by taking up to k blocks after the immutable tip
-- on this peer's candidate fragment.
firstBranch :: Peers EvolvingPeer -> Peer EvolvingPeer
firstBranch peers =
  fromMaybe newest $
  minimumBy (compare `on` forkAnchor) <$> nonEmpty (filter hasForked (toList (adversarialPeers'' peers)))
  where
    newest = maximumBy (compare `on` (AF.headSlot . candidate . value)) (toList (honestPeers'' peers) ++ toList (adversarialPeers'' peers))
    forkAnchor = fromWithOrigin 0 . AF.anchorToSlotNo . AF.anchor . forkSuffix . value
    hasForked Peer {value = EvolvingPeer {candidate, forkSlot}} =
      AF.headSlot candidate >= forkSlot

-- | Determine the immutable tip by computing the latest point before the fork intesection
-- for all peers, and then taking the earliest among the results.
immutableTip :: Peers EvolvingPeer -> AF.Point (Header TestBlock)
immutableTip peers =
  minimum (lastHonest <$> toList (adversarialPeers'' peers))
  where
    lastHonest Peer {value = EvolvingPeer {candidate, forkSlot = NotOrigin forkSlot}} =
      AF.headPoint $
      AF.dropWhileNewest (\ b -> blockSlot b >= forkSlot) candidate
    lastHonest _ = GenesisPoint

-- | Take one block off the peer's suffix and append it to the candidate fragment.
--
-- Since we don't remove the honest peer when it's exhausted, this may be called with an empty suffix.
movePeer :: EvolvingPeer -> (EvolvingPeer, Maybe (Header TestBlock))
movePeer = \case
  peer@EvolvingPeer {candidate, suffix = h : t} ->
    (peer {candidate = candidate AF.:> h, suffix = t}, Just h)
  peer -> (peer, Nothing)

-- | Repeatedly run the GDD, each time updating the candidate fragment of a
-- random peer to advance by one header, until all peers have been discarded
-- (not the same as disconnected!) according to 'updatePeers'.
--
-- The selection is set to the first k blocks of the first fork, the
-- anchor being the intersection.
--
-- The latest slots are the youngest header of each candidate fragments.
--
-- The returned 'MonotonicityResult' indicates whether the honest peer won and
-- no monotonicity violations were detected (the peer stays being disconnected
-- if it starts being disconnected).
evolveBranches ::
  EvolvingPeers ->
  Gen (MonotonicityResult, EvolvingPeers, [UpdateEvent])
evolveBranches EvolvingPeers {k, sgen, peers = initialPeers, fullTree} =
  step [] Evolution {peers = initialPeers, killed = mempty}
  where
    step events Evolution {peers = ps, killed = killedBefore} = do
      (target, nextPeers, added) <- unsafeMapSuchThatJust $ do
        -- Select a random peer
        pid <- elements ids
        pure $ do
          -- Add a block to the candidate. If the peer has no more blocks,
          -- this returns 'Nothing' and the generator retries.
          (nextPeers, added) <- sequence (updatePeer movePeer pid ps)
          pure (pid, nextPeers, added)
      let
        -- Compute the selection.
        curChain = selection (immutableTip ps) (firstBranch ps)
        candidates = candidate . value <$> toMap nextPeers
        states =
          candidates <&> \ csCandidate ->
            ChainSyncState {
              csCandidate,
              csIdling = False,
              csLatestSlot = SJust (AF.headSlot csCandidate)
            }
        -- Run GDD.
        (loeFrag, suffixes) = sharedCandidatePrefix curChain (Map.toList candidates)
        (killedNow, bounds) = first Set.fromList $ densityDisconnect sgen k states suffixes loeFrag
        event = UpdateEvent {
          target,
          added,
          killed = killedNow,
          bounds,
          tree = snapshotTree nextPeers,
          loeFrag,
          curChain
          }
        newEvents = event : events
        -- Check the termination condition and remove exhausted peers.
        updated = updatePeers sgen nextPeers killedBefore event
      either (pure . result newEvents loeFrag) (step newEvents) updated
      where
        result evs f (res, final) = (res, EvolvingPeers {k, sgen, peers = final, loeFrag = f, fullTree}, reverse evs)

        -- Take k blocks after the immutable tip on the first fork.
        selection imm Peer {value = EvolvingPeer {candidate}} =
          case AF.splitAfterPoint candidate imm of
            Just (_, s) -> AF.takeOldest (fromIntegral k') s
            Nothing     -> error "immutable tip not on candidate"

        ids = toList (getPeerIds ps)

    SecurityParam k' = k

peerInfo :: EvolvingPeers -> [String]
peerInfo EvolvingPeers {k = SecurityParam k, sgen = GenesisWindow sgen, loeFrag} =
  [
    "k: " <> show k,
    "sgen: " <> show sgen,
    "loeFrag: " <> terseHFragment loeFrag
  ]

-- | Tests that when GDD disconnects a peer, it continues to disconnect it when
-- its candidate fragment is extended.
prop_densityDisconnectMonotonic :: Property
prop_densityDisconnectMonotonic =
  forAllBlind gen $ \ (result, final, events) ->
    appEndo (foldMap (Endo . counterexample) (peerInfo final)) $
    check final events result
  where
    check final events = \case
      HonestKilled -> withEvents $ counterexample "Honest peer was killed" False
      Nonmonotonic event -> do
        let msg = "Peer went from losing to remaining"
        withEvents $ counterexample (catLines (msg : prettyUpdateEvent event)) False
      Finished -> property True
      where
        withEvents | debug = counterexample (catLines debugInfo)
                   | otherwise = id

        debugInfo =
          "Event log:" : ((++ [""]) . prettyUpdateEvent =<< events) ++
          ["k: " ++ show k'] ++
          ("Full tree:" : prettyBlockTree (fullTree final) ++ [""])

        EvolvingPeers {k = SecurityParam k'} = final

    catLines = intercalate "\n"

    gen = do
      gt <- genChains (QC.choose (1, 4))
      evolveBranches (initCandidates gt)

    debug = True

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

    ( \GenesisTest {gtBlockTree, gtSchedule} stateView@StateView {svTipBlock} ->
        let
          othersCount = Map.size (adversarialPeers $ psSchedule gtSchedule)
          exnCorrect = case exceptionsByComponent ChainSyncClient stateView of
            [fromException -> Just DensityTooLow]        -> True
            [fromException -> Just CandidateTooSparse{}] -> True
            []                 | othersCount == 0        -> True
            _                                            -> False
          tipPointCorrect = Just (getTrunkTip gtBlockTree) == svTipBlock
        in counterexample "Unexpected exceptions" exnCorrect
            .&&.
           counterexample "The tip of the final selection is not the expected one" tipPointCorrect
    )

  where
    -- 1. The adversary advertises blocks up to the intersection.
    -- 2. The honest node advertises all its chain, which is
    --    long enough to be blocked by the LoE.
    -- 3. The adversary gives a block after the genesis window,
    --    which should allow the GDD to realize that the chain
    --    is not dense enough, and that the whole of the honest
    --    chain should be selected.
    lowDensitySchedule :: HasHeader blk => BlockTree blk -> PointSchedule blk
    lowDensitySchedule tree =
      let trunkTip = getTrunkTip tree
          branch = getOnlyBranch tree
          intersect = case btbPrefix branch of
            (AF.Empty _)       -> Origin
            (_ AF.:> tipBlock) -> At tipBlock
          advTip = getOnlyBranchTip tree
       in PointSchedule {
            psSchedule = peers'
            -- Eagerly serve the honest tree, but after the adversary has
            -- advertised its chain up to the intersection.
            [[(Time 0, scheduleTipPoint trunkTip),
              (Time 0.5, scheduleHeaderPoint trunkTip),
              (Time 0.5, scheduleBlockPoint trunkTip)
            ]]
            -- Advertise the alternate branch early, but wait for the honest
            -- node to have served its chain before disclosing the alternate
            -- branch is not dense enough.
            [[(Time 0, scheduleTipPoint advTip),
              (Time 0, ScheduleHeaderPoint intersect),
              (Time 0, ScheduleBlockPoint intersect),
              (Time 1, scheduleHeaderPoint advTip),
              (Time 1, scheduleBlockPoint advTip)
            ]],
            psStartOrder = [],
            psMinEndTime = Time 0
          }
