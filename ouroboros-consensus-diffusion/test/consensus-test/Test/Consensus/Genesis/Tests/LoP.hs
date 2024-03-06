{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.Genesis.Tests.LoP (tests) where

import           Data.Functor (($>))
import           Data.Ratio ((%))
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client as CSClient
import           Ouroboros.Consensus.Util.IOLike (DiffTime, Time (Time),
                     fromException)
import           Ouroboros.Consensus.Util.LeakyBucket
                     (secondsRationalToDiffTime)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     HasHeader)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..))
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.PeerSimulator.Run (SchedulerConfig (..),
                     defaultSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Peers (Peers, mkPeers,
                     peersOnlyHonest)
import           Test.Consensus.PointSchedule.Shrinking (shrinkPeerSchedules)
import           Test.Consensus.PointSchedule.SinglePeer (scheduleBlockPoint,
                     scheduleHeaderPoint, scheduleTipPoint)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  testGroup
    "LoP"
    [ -- \| NOTE: Running the test that must _not_ timeout (@prop_smoke False@) takes
      -- significantly more time than the one that does. This is because the former
      -- does all the computation (serving the headers, validating them, serving the
      -- block, validating them) while the former does nothing, because it timeouts
      -- before reaching the last tick of the point schedule.
      adjustQuickCheckTests (`div` 10) $
        testProperty "wait just enough" (prop_wait False),
      testProperty "wait too much" (prop_wait True),
      testProperty "wait behind forecast horizon" prop_waitBehindForecastHorizon,
      adjustQuickCheckTests (`div` 5) $
        testProperty "serve just fast enough" (prop_serve False),
      testProperty "serve too slow" (prop_serve True),
      adjustQuickCheckTests (`div` 5) $
        testProperty "delaying attack succeeds without LoP" (prop_delayAttack False),
      adjustQuickCheckTests (`div` 5) $
        testProperty "delaying attack fails with LoP" (prop_delayAttack True)
    ]

prop_wait :: Bool -> Property
prop_wait mustTimeout =
  forAllGenesisTest
    ( do
        gt@GenesisTest {gtBlockTree} <- genChains (pure 0)
        let ps = dullSchedule 10 (btTrunk gtBlockTree)
            gt' = gt {gtLoPBucketParams = LoPBucketParams {lbpCapacity = 10, lbpRate = 1}}
        pure $ gt' $> ps
    )
    -- NOTE: Crucially, there must not be timeouts for this test.
    (defaultSchedulerConfig {scEnableChainSyncTimeouts = False, scEnableLoP = True})
    shrinkPeerSchedules
    ( \_ stateView ->
        case exceptionsByComponent ChainSyncClient stateView of
          [] -> not mustTimeout
          [exn] ->
            case fromException exn of
              Just CSClient.EmptyBucket -> mustTimeout
              _                         -> False
          _ -> False
    )
  where
    dullSchedule :: (HasHeader blk) => DiffTime -> AnchoredFragment blk -> Peers (PeerSchedule blk)
    dullSchedule _ (AF.Empty _) = error "requires a non-empty block tree"
    dullSchedule timeout (_ AF.:> tipBlock) =
      let offset :: DiffTime = if mustTimeout then 1 else -1
       in peersOnlyHonest $
            [ (Time 0, scheduleTipPoint tipBlock),
              -- This last point does not matter, it is only here to leave the
              -- connection open (aka. keep the test running) long enough to
              -- pass the timeout by 'offset'.
              (Time (timeout + offset), scheduleHeaderPoint tipBlock),
              (Time (timeout + offset), scheduleBlockPoint tipBlock)
            ]

prop_waitBehindForecastHorizon :: Property
prop_waitBehindForecastHorizon =
  forAllGenesisTest
    ( do
        gt@GenesisTest {gtBlockTree} <- genChains (pure 0)
        let ps = dullSchedule (btTrunk gtBlockTree)
            gt' = gt {gtLoPBucketParams = LoPBucketParams {lbpCapacity = 10, lbpRate = 1}}
        pure $ gt' $> ps
    )
    -- NOTE: Crucially, there must not be timeouts for this test.
    (defaultSchedulerConfig {scEnableChainSyncTimeouts = False, scEnableLoP = True})
    shrinkPeerSchedules
    ( \_ stateView ->
        case exceptionsByComponent ChainSyncClient stateView of
          [] -> True
          _  -> False
    )
  where
    dullSchedule :: (HasHeader blk) => AnchoredFragment blk -> Peers (PeerSchedule blk)
    dullSchedule (AF.Empty _) = error "requires a non-empty block tree"
    dullSchedule (_ AF.:> tipBlock) =
      peersOnlyHonest $
        [ (Time 0, scheduleTipPoint tipBlock),
          (Time 0, scheduleHeaderPoint tipBlock),
          (Time 11, scheduleBlockPoint tipBlock)
        ]

-- | Simple test where we serve all the chain at regular intervals, but just
-- slow enough to lose against the LoP bucket.
--
-- Let @c@ be the bucket capacity, @r@ be the bucket rate and @t@ be the time
-- between blocks, then the bucket level right right before getting the token
-- for the @k@th block will be:
--
-- > c - krt + (k-1)
--
-- (Note: if @rt ≥ 1@, otherwise it will simply be @c - rt@.) If we are to
-- survive at least (resp. succumb before) @k > 0@ blocks, then this value will
-- be positive (resp. negative). This is equivalent to saying that @rt@ must be
-- lower (resp. greater) than @(c+k-1) / k@.
--
-- We will have two versions of this test: one where we serve the @n-1@th block
-- but succumb before serving the @n@th block, and one where we do manage to
-- serve the @n@th block, barely.
prop_serve :: Bool -> Property
prop_serve mustTimeout =
  forAllGenesisTest
    ( do
        gt@GenesisTest {gtBlockTree} <- genChains (pure 0)
        let lbpRate = borderlineRate (AF.length (btTrunk gtBlockTree))
            ps = makeSchedule (btTrunk gtBlockTree)
            gt' = gt {gtLoPBucketParams = LoPBucketParams {lbpCapacity, lbpRate}}
        pure $ gt' $> ps
    )
    -- NOTE: Crucially, there must not be timeouts for this test.
    (defaultSchedulerConfig {scEnableChainSyncTimeouts = False, scEnableLoP = True})
    shrinkPeerSchedules
    ( \_ stateView ->
        case exceptionsByComponent ChainSyncClient stateView of
          [] -> not mustTimeout
          [exn] ->
            case fromException exn of
              Just CSClient.EmptyBucket -> mustTimeout
              _                         -> False
          _ -> False
    )
  where
    lbpCapacity :: Integer = 10
    timeBetweenBlocks :: Rational = 0.100

    -- \| Rate that is almost the limit between surviving and succumbing to the
    -- LoP bucket, given a number of blocks. One should not exactly use the
    -- limit rate because it is unspecified what would happen in IOSim and it
    -- would simply be flakey in IO.
    borderlineRate :: (Integral n) => n -> Rational
    borderlineRate numberOfBlocks =
      (if mustTimeout then (105 % 100) else (95 % 100))
        * ((fromIntegral lbpCapacity + fromIntegral numberOfBlocks - 1) / (timeBetweenBlocks * fromIntegral numberOfBlocks))

    -- \| Make a schedule serving the given fragment with regularity, one block
    -- every 'timeBetweenBlocks'. NOTE: We must do something at @Time 0@
    -- otherwise the others times will be shifted such that the first one is 0.
    makeSchedule :: (HasHeader blk) => AnchoredFragment blk -> Peers (PeerSchedule blk)
    makeSchedule (AF.Empty _) = error "fragment must have at least one block"
    makeSchedule fragment@(_ AF.:> tipBlock) =
      peersOnlyHonest $
        (Time 0, scheduleTipPoint tipBlock)
          : ( flip concatMap (zip [1 ..] (AF.toOldestFirst fragment)) $ \(i, block) ->
                [ (Time (secondsRationalToDiffTime (i * timeBetweenBlocks)), scheduleHeaderPoint block),
                  (Time (secondsRationalToDiffTime (i * timeBetweenBlocks)), scheduleBlockPoint block)
                ]
            )

-- NOTE: Same as 'LoE.prop_adversaryHitsTimeouts' with LoP instead of timeouts.
prop_delayAttack :: Bool -> Property
prop_delayAttack lopEnabled =
  noShrinking $
    forAllGenesisTest
      ( do
          gt@GenesisTest {gtBlockTree} <- genChains (pure 1)
          let gt' = gt {gtLoPBucketParams = LoPBucketParams {lbpCapacity = 10, lbpRate = 1}}
              ps = delaySchedule gtBlockTree
          pure $ gt' $> ps
      )
      -- NOTE: Crucially, there must not be timeouts for this test.
      ( defaultSchedulerConfig
          { scEnableChainSyncTimeouts = False,
            scEnableLoE = True,
            scEnableLoP = lopEnabled
          }
      )
      shrinkPeerSchedules
      ( \GenesisTest {gtBlockTree} stateView@StateView {svSelectedChain} ->
          let treeTipPoint = AF.headPoint $ btTrunk gtBlockTree
              selectedTipPoint = AF.castPoint $ AF.headPoint svSelectedChain
              selectedCorrect = lopEnabled == (treeTipPoint == selectedTipPoint)
              exceptionsCorrect = case exceptionsByComponent ChainSyncClient stateView of
                [] -> not lopEnabled
                [exn] ->
                  lopEnabled == (fromException exn == Just CSClient.EmptyBucket)
                _ -> False
           in selectedCorrect && exceptionsCorrect
      )
  where
    getOnlyBranch :: BlockTree blk -> BlockTreeBranch blk
    getOnlyBranch BlockTree {btBranches} = case btBranches of
      [branch] -> branch
      _        -> error "tree must have exactly one alternate branch"

    delaySchedule :: (HasHeader blk) => BlockTree blk -> Peers (PeerSchedule blk)
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
            -- Advertise the alternate branch early, but don't serve it
            -- past the intersection, and wait for LoP bucket.
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
