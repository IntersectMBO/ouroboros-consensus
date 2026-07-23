{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | ChainSync Jumping tests.
module Test.Consensus.Genesis.Tests.CSJ
  ( TestKey
  , testSuite
  ) where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Ouroboros.Consensus.Block
  ( HasHeader
  , Header
  , blockSlot
  , succWithOrigin
  , unSlotNo
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( TraceChainSyncClientEvent (..)
  )
import Ouroboros.Consensus.Util.Condense
  ( Condense
  , PaddingDirection (..)
  , condenseListWithPadding
  )
import qualified Ouroboros.Network.AnchoredFragment as AF
import Test.Consensus.BlockTree (BlockTree (..))
import Test.Consensus.Genesis.Setup
import Test.Consensus.Genesis.TestSuite
import Test.Consensus.Genesis.Tests.Uniform (genUniformSchedulePoints)
import Test.Consensus.PeerSimulator.Run
  ( SchedulerConfig (..)
  , defaultSchedulerConfig
  )
import Test.Consensus.PeerSimulator.StateView (StateView (..))
import Test.Consensus.PeerSimulator.Trace (TraceEvent (..))
import Test.Consensus.PointSchedule
import Test.Consensus.PointSchedule.Peers (Peers (..), peers')
import qualified Test.Consensus.PointSchedule.Peers as Peers
import Test.Consensus.PointSchedule.Shrinking (shrinkPeerSchedules)
import Test.Tasty.QuickCheck
import Test.Util.Orphans.IOLike ()
import Test.Util.PartialAccessors

-- | Default adjustment of the required number of test runs.
-- Can be set individually on each test definition.
adjustTestCount :: AdjustTestCount
adjustTestCount = AdjustTestCount (* 10)

-- | Default adjustment of max test case size.
-- Can be set individually on each test definition.
adjustMaxSize :: AdjustMaxSize
adjustMaxSize = AdjustMaxSize (`div` 5)

-- | Each value of this type uniquely corresponds to a test defined in this module.
data TestKey
  = WithNoAdversariesAndOneScheduleForAllPeers
  | WithNoAdversariesAndOneSchedulePerHonestPeer
  | WithAdversariesAndOneScheduleForAllPeers
  | WithAdversariesAndOneSchedulePerHonestPeer
  deriving stock (Eq, Ord, Generic)
  deriving SmallKey via Generically TestKey

testSuite ::
  ( HasHeader blk
  , HasHeader (Header blk)
  , IssueTestBlock blk
  , Ord blk
  , Condense (Header blk)
  , Eq (Header blk)
  ) =>
  TestSuite blk TestKey
testSuite =
  let keyToFlags :: TestKey -> (WithAdversariesFlag, NumHonestSchedulesFlag)
      keyToFlags = \case
        WithNoAdversariesAndOneScheduleForAllPeers -> (NoAdversaries, OneScheduleForAllPeers)
        WithNoAdversariesAndOneSchedulePerHonestPeer -> (NoAdversaries, OneSchedulePerHonestPeer)
        WithAdversariesAndOneScheduleForAllPeers -> (WithAdversaries, OneScheduleForAllPeers)
        WithAdversariesAndOneSchedulePerHonestPeer -> (WithAdversaries, OneSchedulePerHonestPeer)
      groupName key = case fst (keyToFlags key) of
        NoAdversaries -> "Happy path"
        WithAdversaries -> "With some adversaries"
      testDescription key = case snd (keyToFlags key) of
        OneScheduleForAllPeers -> "honest peers are synchronised"
        OneSchedulePerHonestPeer -> "honest peers do their own thing"
   in group "CSJ" $
        grouping groupName $
          newTestSuite $
            \key -> uncurry (testCsj $ testDescription key) (keyToFlags key)

-- | A flag to indicate if properties are tested with adversarial peers
data WithAdversariesFlag = NoAdversaries | WithAdversaries
  deriving stock Eq

-- | A flag to indicate if properties are tested using the same schedule for the
-- honest peers, or if each peer should used its own schedule.
data NumHonestSchedulesFlag = OneScheduleForAllPeers | OneSchedulePerHonestPeer

-- | Test of ChainSync Jumping (CSJ).
--
-- This test features several peers that all sync the “honest” chain (ie. the
-- trunk of the block tree) with CSJ enabled. What we expect to observe is that
-- one of the honest peers becomes the dynamo while the others become jumpers.
-- Because the jumpers will agree to all the jumps, the whole syncing should
-- happen with CSJ.
--
-- There are two variants of this test: the “happy path” variant features no
-- adversaries. As such, everything should happen with one dynamo and no
-- objector. Another variant adds adversaries, so we expect to see some
-- dynamo-vs-objector action.
--
-- Regardless, the final property is that “honest” headers should only ever be
-- downloaded at most once from honest peers. They may however be downloaded
-- several times from adversaries. This is true except when almost caught-up:
-- when the dynamo or objector is caught-up, it gets disengaged and one of the
-- jumpers takes its place and starts serving headers. This might lead to
-- duplication of headers, but only in a window of @jumpSize@ slots near the tip
-- of the chain.
testCsj ::
  forall blk.
  ( HasHeader blk
  , HasHeader (Header blk)
  , IssueTestBlock blk
  , Ord blk
  , Condense (Header blk)
  , Eq (Header blk)
  ) =>
  String -> WithAdversariesFlag -> NumHonestSchedulesFlag -> ConformanceTest blk
testCsj description adversariesFlag numHonestSchedules = do
  let genForks = case adversariesFlag of
        NoAdversaries -> pure 0
        WithAdversaries -> choose (2, 4)
  mkConformanceTest
    description
    adjustTestCount
    adjustMaxSize
    ( disableBoringTimeouts <$> case numHonestSchedules of
        OneScheduleForAllPeers ->
          genChains genForks
            `enrichedWith` genDuplicatedHonestSchedule
        OneSchedulePerHonestPeer ->
          genChainsWithExtraHonestPeers (choose (2, 4)) genForks
            `enrichedWith` genUniformSchedulePoints
    )
    ( defaultSchedulerConfig
        { scEnableCSJ = True
        , scEnableLoE = True
        , scEnableLoP = True
        , scEnableChainSelStarvation = adversariesFlag == NoAdversaries
        }
    )
    -- \^ NOTE: When there are adversaries and the ChainSel
    -- starvation detection of BlockFetch is enabled, then our property does
    -- not actually hold, because peer simulator-based tests have virtually
    -- infinite CPU, and therefore ChainSel gets starved at every tick, which
    -- makes us cycle the dynamos, which can lead to some extra headers being
    -- downloaded.

    shrinkPeerSchedules
    ( \gt StateView{svTrace} ->
        let
          -- The list of 'TraceDownloadedHeader' events that are not newer than
          -- jumpSize from the tip of the chain. These are the ones that we
          -- expect to see only once per header if CSJ works properly.
          headerHonestDownloadEvents =
            mapMaybe
              ( \case
                  TraceChainSyncClientEvent pid (TraceDownloadedHeader hdr)
                    | not (isNewerThanJumpSizeFromTip gt hdr)
                    , Peers.HonestPeer _ <- pid ->
                        Just (pid, hdr)
                  _ -> Nothing
              )
              svTrace
          -- We receive headers at most once from honest peer. The only
          -- exception is when an honest peer gets to be the objector, until an
          -- adversary dies, and then the dynamo. In that specific case, we
          -- might re-download jumpSize blocks. TODO: If we ever choose to
          -- promote objectors to dynamo to reuse their state, then we could
          -- make this bound tighter.
          receivedHeadersAtMostOnceFromHonestPeers =
            length headerHonestDownloadEvents
              <= length (nub $ snd <$> headerHonestDownloadEvents)
                + (fromIntegral $ unSlotNo $ csjpJumpSize $ gtCSJParams gt)
         in
          tabulate
            ""
            [ if headerHonestDownloadEvents == []
                then "All headers are within the last jump window"
                else "There exist headers that have to be downloaded exactly once"
            ]
            $ counterexample
              ( "Downloaded headers (except jumpSize slots near the tip):\n"
                  ++ ( unlines $
                         fmap ("  " ++) $
                           zipWith
                             (\peer header -> peer ++ " | " ++ header)
                             (condenseListWithPadding PadRight $ fst <$> headerHonestDownloadEvents)
                             (condenseListWithPadding PadRight $ snd <$> headerHonestDownloadEvents)
                     )
              )
              receivedHeadersAtMostOnceFromHonestPeers
    )
 where
  genDuplicatedHonestSchedule :: GenesisTest blk () -> Gen (PointSchedule blk)
  genDuplicatedHonestSchedule gt@GenesisTest{gtExtraHonestPeers} = do
    ps@PointSchedule{psSchedule = Peers{honestPeers, adversarialPeers}} <- genUniformSchedulePoints gt
    pure $
      ps
        { psSchedule =
            Peers.unionWithKey
              (\_ _ _ -> error "should not happen")
              ( peers'
                  (replicate (fromIntegral gtExtraHonestPeers + 1) (getHonestPeer honestPeers))
                  []
              )
              (Peers Map.empty adversarialPeers)
        }

  isNewerThanJumpSizeFromTip :: GenesisTestFull blk -> Header blk -> Bool
  isNewerThanJumpSizeFromTip gt hdr =
    let jumpSize = csjpJumpSize $ gtCSJParams gt
        tipSlot = AF.headSlot $ btTrunk $ gtBlockTree gt
        hdrSlot = blockSlot hdr
     in -- Sanity check: add @1 +@ after @>@ and watch the World burn.
        hdrSlot + jumpSize >= succWithOrigin tipSlot

  disableBoringTimeouts gt =
    gt
      { gtChainSyncTimeouts =
          (gtChainSyncTimeouts gt)
            { mustReplyTimeout = Nothing
            , idleTimeout = Nothing
            }
      }
