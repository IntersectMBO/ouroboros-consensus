{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.Genesis.Tests.CSJ (tests) where

import           Data.List (nub)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Ouroboros.Consensus.Block (Header, blockSlot, succWithOrigin,
                     unSlotNo)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (TraceChainSyncClientEvent (..))
import           Ouroboros.Consensus.Util.Condense (PaddingDirection (..),
                     condenseListWithPadding)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Protocol.ChainSync.Codec
                     (ChainSyncTimeout (mustReplyTimeout), idleTimeout)
import           Test.Consensus.BlockTree (BlockTree (..))
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.Genesis.Tests.Uniform (genUniformSchedulePoints)
import           Test.Consensus.PeerSimulator.Run (SchedulerConfig (..),
                     defaultSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView (StateView (..))
import           Test.Consensus.PeerSimulator.Trace (TraceEvent (..))
import           Test.Consensus.PointSchedule
import qualified Test.Consensus.PointSchedule.Peers as Peers
import           Test.Consensus.PointSchedule.Peers (Peers (..), peers')
import           Test.Consensus.PointSchedule.Shrinking (shrinkPeerSchedules)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.PartialAccessors
import           Test.Util.TestBlock (TestBlock)
import           Test.Util.TestEnv (adjustQuickCheckMaxSize, adjustQuickCheckTests)

tests :: TestTree
tests =
  adjustQuickCheckTests (* 10) $
  adjustQuickCheckMaxSize (`div` 5) $
    testGroup
      "CSJ"
      [ testGroup
          "Happy Path"
          [ testProperty "honest peers are synchronised" $ prop_CSJ NoAdversaries OneScheduleForAllPeers,
            testProperty "honest peers do their own thing" $ prop_CSJ NoAdversaries OneSchedulePerHonestPeer
          ],
        testGroup
          "With some adversaries"
          [ testProperty "honest peers are synchronised" $ prop_CSJ WithAdversaries OneScheduleForAllPeers,
            testProperty "honest peers do their own thing" $ prop_CSJ WithAdversaries OneSchedulePerHonestPeer
          ]
      ]

-- | A flag to indicate if properties are tested with adversarial peers
data WithAdversariesFlag = NoAdversaries | WithAdversaries
  deriving Eq

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
--
prop_CSJ :: WithAdversariesFlag -> NumHonestSchedulesFlag -> Property
prop_CSJ adversariesFlag numHonestSchedules = do
  let genForks = case adversariesFlag of
                   NoAdversaries   -> pure 0
                   WithAdversaries -> choose (2, 4)
  forAllGenesisTest
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
      -- ^ NOTE: When there are adversaries and the ChainSel
      -- starvation detection of BlockFetch is enabled, then our property does
      -- not actually hold, because peer simulator-based tests have virtually
      -- infinite CPU, and therefore ChainSel gets starved at every tick, which
      -- makes us cycle the dynamos, which can lead to some extra headers being
      -- downloaded.
      }
    )
    shrinkPeerSchedules
    ( \gt StateView{svTrace} ->
        let
          -- The list of 'TraceDownloadedHeader' events that are not newer than
          -- jumpSize from the tip of the chain. These are the ones that we
          -- expect to see only once per header if CSJ works properly.
          headerHonestDownloadEvents =
            mapMaybe
              (\case
                TraceChainSyncClientEvent pid (TraceDownloadedHeader hdr)
                  | not (isNewerThanJumpSizeFromTip gt hdr)
                  , Peers.HonestPeer _ <- pid
                  -> Just (pid, hdr)
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
            length headerHonestDownloadEvents <=
              length (nub $ snd <$> headerHonestDownloadEvents) +
                (fromIntegral $ unSlotNo $ csjpJumpSize $ gtCSJParams gt)
        in
          tabulate ""
            [ if headerHonestDownloadEvents == []
                then "All headers are within the last jump window"
                else "There exist headers that have to be downloaded exactly once"
            ] $
          counterexample
          ("Downloaded headers (except jumpSize slots near the tip):\n" ++
            ( unlines $ fmap ("  " ++) $ zipWith
              (\peer header -> peer ++ " | " ++ header)
              (condenseListWithPadding PadRight $ fst <$> headerHonestDownloadEvents)
              (condenseListWithPadding PadRight $ snd <$> headerHonestDownloadEvents)
            )
          )
          receivedHeadersAtMostOnceFromHonestPeers
    )
  where
    genDuplicatedHonestSchedule :: GenesisTest TestBlock () -> Gen (PointSchedule TestBlock)
    genDuplicatedHonestSchedule gt@GenesisTest {gtExtraHonestPeers} = do
      ps@PointSchedule {psSchedule = Peers {honestPeers, adversarialPeers}} <- genUniformSchedulePoints gt
      pure $ ps {
        psSchedule =
          Peers.unionWithKey
            (\_ _ _ -> error "should not happen")
            ( peers'
                (replicate (fromIntegral gtExtraHonestPeers + 1) (getHonestPeer honestPeers))
                []
            )
            (Peers Map.empty adversarialPeers)
        }

    isNewerThanJumpSizeFromTip :: GenesisTestFull TestBlock -> Header TestBlock -> Bool
    isNewerThanJumpSizeFromTip gt hdr =
      let jumpSize = csjpJumpSize $ gtCSJParams gt
          tipSlot = AF.headSlot $ btTrunk $ gtBlockTree gt
          hdrSlot = blockSlot hdr
       in
        -- Sanity check: add @1 +@ after @>@ and watch the World burn.
        hdrSlot + jumpSize >= succWithOrigin tipSlot

    disableBoringTimeouts gt =
      gt
        { gtChainSyncTimeouts =
            (gtChainSyncTimeouts gt)
              { mustReplyTimeout = Nothing,
                idleTimeout = Nothing
              }
        }
