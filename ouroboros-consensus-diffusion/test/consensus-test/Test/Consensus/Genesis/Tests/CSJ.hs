{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.Genesis.Tests.CSJ (tests) where

import           Control.Monad (replicateM)
import           Data.Containers.ListUtils (nubOrd)
import           Data.Functor (($>))
import           Data.List (nub)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Ouroboros.Consensus.Block (blockSlot, succWithOrigin)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (TraceChainSyncClientEvent (..))
import           Ouroboros.Consensus.Util.Condense (PaddingDirection (..),
                     condenseListWithPadding)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Test.Consensus.BlockTree (BlockTree (..))
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.Genesis.Tests.Uniform (genUniformSchedulePoints)
import           Test.Consensus.PeerSimulator.Run (SchedulerConfig (..),
                     defaultSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView (StateView (..))
import           Test.Consensus.PeerSimulator.Trace (TraceEvent (..))
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Peers (Peers (..), peers')
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (Header, TestBlock)
import           Test.Util.TestEnv (adjustQuickCheckMaxSize)

tests :: TestTree
tests =
  adjustQuickCheckMaxSize (`div` 5) $
  testGroup
    "CSJ"
    [ testGroup "Happy Path"
      [ testProperty "synchronous" $ prop_happyPath True
      , testProperty "asynchronous" $ prop_happyPath False
      ]
    ]

-- | Test of the “happy path” scenario of ChainSync Jumping (CSJ).
--
-- This test features one chain (ie. a block tree that is only trunk) and only
-- honest peers and syncs the chain in question with CSJ enabled. What we expect
-- to observe is that one of the honest peers becomes the dynamo while the
-- others become jumpers. Because the jumpers will agree to all the jumps, the
-- whole syncing should happen with CSJ without objectors.
--
-- The final property is that headers should only ever be downloaded once and
-- only from one peer (the dynamo). This is true except when almost caught-up:
-- when the dynamo is caught-up, it gets disengaged and one of the jumpers takes
-- its place and starts serving headers. This might lead to duplication of
-- headers, but only in a window of @jumpSize@ slots near the tip of the chain.
--
-- The boolean differentiates between “synchronous” and “asynchronous”
-- scenarios. In a synchronous scenario, all the honest peers have the same
-- schedule: they serve the chain exactly in the same way. In the asynchronous
-- scenario, a random schedule is generated for each peer (but they still serve
-- the same chain).
prop_happyPath :: Bool -> Property
prop_happyPath synchronized =
  forAllGenesisTest
    ( do
        gt <- genChains $ pure 0
        honest <- genHonestSchedule gt
        numOthers <- choose (1, 3)
        otherHonests <- if synchronized
          then pure $ replicate numOthers honest
          else replicateM numOthers (genHonestSchedule gt)
        pure $ gt $> peers' [honest] otherHonests
    )
    ( defaultSchedulerConfig
      { scEnableCSJ = True
      , scEnableLoE = True
      , scEnableLoP = True
      }
    )
    ( -- NOTE: Shrinking makes the tests fail because the peers reject jumps
      -- because their TP is G. This makes them into objectors and they then
      -- start serving headers.
      \_ _ -> []
    )
    ( \gt StateView{svTrace} ->
        let
          headerDownloadEvents =
            mapMaybe
              (\case
                TraceChainSyncClientEvent pid (TraceDownloadedHeader hdr)
                  | not (isNewerThanJumpSizeFromTip gt hdr)
                  -> Just (pid, hdr)
                _ -> Nothing
              )
              svTrace
          receivedHeadersOnlyOnce = length (nub $ snd <$> headerDownloadEvents) == length headerDownloadEvents
          -- NOTE: If all the headers are newer than jumpSize from the tip, then
          -- 'headerDownloadEvents' is empty and the following condition would
          -- violated if we used @==@.
          receivedHeadersFromOnlyOnePeer = length (nubOrd $ fst <$> headerDownloadEvents) <= 1
        in
          tabulate ""
            [ if headerDownloadEvents == []
                then "All headers may be downloaded twice"
                else "There exist headers that have to be downloaded exactly once"
            ] $
          counterexample
          ("Downloaded headers (except jumpSize slots near the tip):\n" ++
            ( unlines $ fmap ("  " ++) $ zipWith
              (\peer header -> peer ++ " | " ++ header)
              (condenseListWithPadding PadRight $ fst <$> headerDownloadEvents)
              (condenseListWithPadding PadRight $ snd <$> headerDownloadEvents)
            )
          )
          (receivedHeadersOnlyOnce && receivedHeadersFromOnlyOnePeer)
    )
  where
    -- | This might seem wasteful, as we discard generated adversarial schedules.
    -- It actually isn't, since we call it on trees that have no branches besides
    -- the trunk, so no adversaries are generated.
    genHonestSchedule :: GenesisTest TestBlock () -> Gen (PeerSchedule TestBlock)
    genHonestSchedule gt = do
      ps <- genUniformSchedulePoints gt
      pure $ honestPeers ps Map.! 1

    isNewerThanJumpSizeFromTip :: GenesisTestFull TestBlock -> Header TestBlock -> Bool
    isNewerThanJumpSizeFromTip gt hdr =
      let jumpSize = csjpJumpSize $ gtCSJParams gt
          tipSlot = AF.headSlot $ btTrunk $ gtBlockTree gt
          hdrSlot = blockSlot hdr
       in
        -- Sanity check: add @1 +@ after @>@ and watch the World burn.
        hdrSlot + jumpSize >= succWithOrigin tipSlot
