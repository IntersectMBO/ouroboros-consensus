{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Consensus.PeerSimulator.Tests.Timeouts
  ( TestKey
  , testSuite
  ) where

import Data.Functor (($>))
import Ouroboros.Consensus.Block (Header, HeaderHash)
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Consensus.Util.IOLike
  ( DiffTime
  , Time (Time)
  , fromException
  )
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Driver.Limits
  ( ProtocolLimitFailure (ExceededTimeLimit)
  )
import Test.Consensus.BlockTree (btTrunk)
import Test.Consensus.Genesis.Setup
import Test.Consensus.Genesis.TestSuite
import Test.Consensus.PeerSimulator.Run
  ( SchedulerConfig (scEnableChainSyncTimeouts)
  , defaultSchedulerConfig
  )
import Test.Consensus.PeerSimulator.StateView
import Test.Consensus.PointSchedule
import Test.Consensus.PointSchedule.Peers
  ( peersOnlyAdversary
  , peersOnlyHonest
  )
import Test.Consensus.PointSchedule.SinglePeer
  ( scheduleBlockPoint
  , scheduleHeaderPoint
  , scheduleTipPoint
  )
import Test.QuickCheck
import Test.Util.Orphans.IOLike ()

-- | Default adjustment of the required number of test runs.
-- Can be set individually on each test definition.
adjustTestCount :: AdjustTestCount
adjustTestCount = AdjustTestCount (`div` 10)

-- | Default adjustment of max test case size.
-- Can be set individually on each test definition.
adjustMaxSize :: AdjustMaxSize
adjustMaxSize = AdjustMaxSize id

data TestKey
  = DoesTimeout
  | DoesNotTimeout
  deriving stock (Eq, Ord, Generic)
  deriving SmallKey via Generically TestKey

testSuite ::
  ( IssueTestBlock blk
  , AF.HasHeader blk
  , AF.HasHeader (Header blk)
  , Condense (HeaderHash blk)
  , Condense (Header blk)
  ) =>
  TestSuite blk TestKey
testSuite = group "timeouts" . newTestSuite $ \case
  DoesTimeout -> testTimeouts "does time out" True
  DoesNotTimeout -> testTimeouts "does not time out" False

testTimeouts ::
  ( IssueTestBlock blk
  , AF.HasHeader blk
  , AF.HasHeader (Header blk)
  , Condense (HeaderHash blk)
  , Condense (Header blk)
  ) =>
  String -> Bool -> ConformanceTest blk
testTimeouts description mustTimeout =
  mkConformanceTest
    description
    adjustTestCount
    adjustMaxSize
    ( do
        gt@GenesisTest{gtBlockTree} <- genChains (pure 0)
        pure $ enableMustReplyTimeout $ gt $> dullSchedule (btTrunk gtBlockTree)
    )
    defaultSchedulerConfig{scEnableChainSyncTimeouts = True}
    -- Here we can't shrink because we exploit the properties of the point schedule to wait
    -- at the end of the test for the adversaries to get disconnected, by adding an extra point.
    -- If this point gets removed by the shrinker, we lose that property and the test becomes useless.
    mempty
    ( \_ stateView ->
        case exceptionsByComponent ChainSyncClient stateView of
          [] ->
            counterexample ("result: " ++ condense (svSelectedChain stateView)) (not mustTimeout)
          [fromException -> Just (ExceededTimeLimit _)] -> property mustTimeout
          exns ->
            counterexample ("exceptions: " ++ show exns) False
    )
 where
  timeout = 10

  dullSchedule :: AF.HasHeader blk => AF.AnchoredFragment blk -> PointSchedule blk
  dullSchedule (AF.Empty _) = error "requires a non-empty block tree"
  dullSchedule (_ AF.:> tipBlock) =
    let offset :: DiffTime = if mustTimeout then 1 else -1
        psSchedule =
          (if mustTimeout then peersOnlyAdversary else peersOnlyHonest) $
            [ (Time 0, scheduleTipPoint tipBlock)
            , (Time 0, scheduleHeaderPoint tipBlock)
            , (Time 0, scheduleBlockPoint tipBlock)
            ]
        -- This keeps the test running long enough to pass the timeout by 'offset'.
        psMinEndTime = Time $ timeout + offset
     in PointSchedule{psSchedule, psStartOrder = [], psMinEndTime}

  enableMustReplyTimeout :: GenesisTest blk schedule -> GenesisTest blk schedule
  enableMustReplyTimeout gt = gt{gtChainSyncTimeouts = (gtChainSyncTimeouts gt){mustReplyTimeout = Just timeout}}
