{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.PeerSimulator.Tests
  ( SmokeTestKey
  , testSuite
  , tests
  ) where

import Ouroboros.Consensus.Block.Abstract
  ( HasHeader
  , Header
  , HeaderHash
  )
import Ouroboros.Consensus.Util.Condense (Condense)
import Test.Consensus.Genesis.Setup
import Test.Consensus.Genesis.TestSuite
import qualified Test.Consensus.PeerSimulator.Tests.LinkedThreads as LinkedThreads
import qualified Test.Consensus.PeerSimulator.Tests.Rollback as Rollback
import qualified Test.Consensus.PeerSimulator.Tests.Timeouts as Timeouts
import Test.Tasty
import Test.Util.TestBlock (TestBlock)

tests :: TestTree
tests = testGroup "PeerSimulator" $ toTestTree @TestBlock testSuite

-- | Each value of this type uniquely corresponds to a basic functionality test.
data SmokeTestKey
  = LinkedThreads LinkedThreads.TestKey
  | Rollback Rollback.TestKey
  | Timeouts Timeouts.TestKey
  deriving stock (Eq, Ord, Generic)
  deriving SmallKey via Generically SmokeTestKey

testSuite ::
  ( IssueTestBlock blk
  , HasHeader blk
  , HasHeader (Header blk)
  , Condense (HeaderHash blk)
  , Condense (Header blk)
  , Eq blk
  ) =>
  TestSuite blk SmokeTestKey
testSuite = mkTestSuite $ \case
  LinkedThreads t -> at LinkedThreads.testSuite t
  Rollback t -> at Rollback.testSuite t
  Timeouts t -> at Timeouts.testSuite t
