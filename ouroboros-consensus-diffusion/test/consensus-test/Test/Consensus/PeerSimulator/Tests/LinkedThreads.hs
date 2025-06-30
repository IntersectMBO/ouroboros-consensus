{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

-- | The scheduled ChainSync and BlockFetch servers are supposed to be linked,
-- such that if one gets disconnected, then so does the other. This module
-- contains a collection of smoke tests to make sure of that.
module Test.Consensus.PeerSimulator.Tests.LinkedThreads (tests) where

import Control.Monad.Class.MonadAsync (AsyncCancelled (..))
import Control.Monad.Class.MonadTime.SI (Time (Time))
import Data.Functor (($>))
import Ouroboros.Consensus.Util.IOLike (fromException)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Driver.Limits
  ( ProtocolLimitFailure (ExceededTimeLimit)
  )
import Test.Consensus.BlockTree (BlockTree (..))
import Test.Consensus.Genesis.Setup
import Test.Consensus.PeerSimulator.Run
  ( SchedulerConfig (scEnableChainSyncTimeouts)
  , defaultSchedulerConfig
  )
import Test.Consensus.PeerSimulator.StateView
import Test.Consensus.PointSchedule
import Test.Consensus.PointSchedule.Peers (peersOnlyHonest)
import Test.Consensus.PointSchedule.SinglePeer
  ( scheduleHeaderPoint
  , scheduleTipPoint
  )
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.Orphans.IOLike ()

tests :: TestTree
tests = testProperty "ChainSync kills BlockFetch" prop_chainSyncKillsBlockFetch

-- | Check that when the scheduled ChainSync server gets killed, it takes the
-- BlockFetch one with it. For this, we rely on ChainSync timeouts: the
-- ChainSync server serves just one header and then waits long enough to get
-- disconnected. After that, we give a tick for the BlockFetch server to serve
-- the corresponding block. We check that the block is not served.
prop_chainSyncKillsBlockFetch :: Property
prop_chainSyncKillsBlockFetch = do
  forAllGenesisTest
    ( do
        gt@GenesisTest{gtBlockTree} <- genChains (pure 0)
        pure $ enableMustReplyTimeout $ gt $> dullSchedule (btTrunk gtBlockTree)
    )
    defaultSchedulerConfig{scEnableChainSyncTimeouts = True}
    -- No shrinking because the schedule is tiny and hand-crafted
    (\_ _ -> [])
    ( \_ stateView@StateView{svTipBlock} ->
        svTipBlock == Nothing
          && case exceptionsByComponent ChainSyncClient stateView of
            [fromException -> Just (ExceededTimeLimit _)] -> True
            _ -> False
          && case exceptionsByComponent BlockFetchClient stateView of
            [fromException -> Just AsyncCancelled] -> True
            _ -> False
          && case exceptionsByComponent ChainSyncServer stateView of
            [fromException -> Just AsyncCancelled] -> True
            _ -> False
          && case exceptionsByComponent BlockFetchServer stateView of
            [fromException -> Just AsyncCancelled] -> True
            _ -> False
    )
 where
  timeout = 10

  dullSchedule :: AF.AnchoredFragment blk -> PointSchedule blk
  dullSchedule trunk =
    let (firstBlock, secondBlock) = case AF.toOldestFirst trunk of
          b1 : b2 : _ -> (b1, b2)
          _ -> error "block tree must have two blocks"
        psSchedule =
          peersOnlyHonest $
            [ (Time 0, scheduleTipPoint secondBlock)
            , (Time 0, scheduleHeaderPoint firstBlock)
            ]
        psMinEndTime = Time $ timeout + 1
     in PointSchedule{psSchedule, psStartOrder = [], psMinEndTime}

  enableMustReplyTimeout :: GenesisTest blk schedule -> GenesisTest blk schedule
  enableMustReplyTimeout gt = gt{gtChainSyncTimeouts = (gtChainSyncTimeouts gt){mustReplyTimeout = Just timeout}}
