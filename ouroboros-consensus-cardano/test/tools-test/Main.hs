{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import qualified Cardano.Tools.DBSynthesizer.Run as DBSynthesizer
import Cardano.Tools.DBSynthesizer.Types
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)
import Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import qualified Test.Cardano.Tools.DBAnalyser.NodeConfig
import qualified Test.Cardano.Tools.Headers
import Test.Consensus.Cardano.ProtocolInfo
  ( ByronSlotLengthInSeconds (..)
  , Era (..)
  , ShelleySlotLengthInSeconds (..)
  , hardForkInto
  , mkSimpleTestProtocolInfoForging
  , protocolVersionZero
  )
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.ThreadNet.Infra.Shelley as Shelley
import Test.Util.TestEnv

-- | The ChainDB the synthesizer forges into.
chainDB :: FilePath
chainDB = "ouroboros-consensus-cardano/test/tools-test/disk/chaindb"

-- | The forge limits are kept well within the KES validity window of the
-- test protocol (see 'mkSimpleTestProtocolInfoForging', which uses a small
-- number of KES-valid slots), so that both the create and the subsequent
-- append step can still forge blocks.
testSynthOptionsCreate :: DBSynthesizerOptions
testSynthOptionsCreate =
  DBSynthesizerOptions
    { synthLimit = ForgeLimitSlot 40
    , synthOpenMode = OpenCreateForce
    }

testSynthOptionsAppend :: DBSynthesizerOptions
testSynthOptionsAppend =
  DBSynthesizerOptions
    { synthLimit = ForgeLimitSlot 40
    , synthOpenMode = OpenAppend
    }

-- | How many blocks each step is expected to forge.
--
-- These numbers are empirical, but they are not arbitrary: the whole setup is
-- deterministic, so a change in either of them means the forging loop, the
-- leader schedule or the test protocol changed. Pinning them therefore catches
-- regressions that a mere @> 0@ would not.
--
-- The create step covers slots 0..39 and forges 8 blocks, the last of them in
-- slot 27. The append step continues from the tip, so its 'ForgeLimitSlot' is
-- counted from slot 28 and it covers slots 28..67 -- ie it revisits the tail of
-- the range the create step already covered -- forging 2 more blocks. The two
-- counts differ simply because the leader schedule is not uniform.
--
-- If these ever need re-baselining, the counts are what db-synthesizer prints as
-- @forged and adopted N blocks@. Note in particular that the keys behind the
-- leader schedule come from a QuickCheck generator (see 'Shelley.genCoreNode',
-- driven by a fixed seed), so a QuickCheck bump can legitimately change them:
-- that is a re-baseline, not necessarily a regression.
expectedForgedCreate, expectedForgedAppend :: Int
expectedForgedCreate = 8
expectedForgedAppend = 2

-- | Synthesize a ChainDB from scratch and then append to it, checking that both
-- steps forge exactly the blocks we expect.
--
-- The protocol is built in-process from 'mkSimpleTestProtocolInfoForging'
-- rather than from a node configuration file; constructing a forging-capable
-- protocol from a real configuration is exercised downstream, where the
-- configuration-loading machinery lives. What db-analyser derives from a
-- configuration file is covered by "Test.Cardano.Tools.DBAnalyser.NodeConfig".
blockCountTest :: (String -> IO ()) -> Assertion
blockCountTest logStep = do
  logStep "building test protocol"
  (protocolInfo, blockForging, shelleyGenesis) <-
    mkSimpleTestProtocolInfoForging @StandardCrypto
      (Shelley.DecentralizationParam 1)
      (SecurityParam $ knownNonZeroBounded @10)
      (ByronSlotLengthInSeconds 1)
      (ShelleySlotLengthInSeconds 1)
      protocolVersionZero
      (hardForkInto Conway)
  let protocol = (protocolInfo, blockForging)

  logStep "running synthesis - create"
  resultCreate <-
    DBSynthesizer.synthesize genTxs testSynthOptionsCreate shelleyGenesis chainDB protocol
  assertForged "create" expectedForgedCreate resultCreate

  logStep "running synthesis - append"
  resultAppend <-
    DBSynthesizer.synthesize genTxs testSynthOptionsAppend shelleyGenesis chainDB protocol
  assertForged "append" expectedForgedAppend resultAppend
 where
  genTxs _ _ _ _ = pure []

  assertForged step expected result =
    assertEqual
      ("wrong number of blocks forged during the " <> step <> " step")
      expected
      (resultForged result)

tests :: TestTree
tests =
  testGroup
    "cardano-tools"
    [ testCaseSteps "synthesize: blockCount\n" blockCountTest
    , Test.Cardano.Tools.DBAnalyser.NodeConfig.tests
    , Test.Cardano.Tools.Headers.tests
    ]

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests
