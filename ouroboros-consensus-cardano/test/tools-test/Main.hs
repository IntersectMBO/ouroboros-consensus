{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import qualified Cardano.Tools.DBSynthesizer.Run as DBSynthesizer
import Cardano.Tools.DBSynthesizer.Types
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)
import Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import Test.Consensus.Cardano.ProtocolInfo
  ( ByronSlotLengthInSeconds (..)
  , Era (..)
  , ShelleySlotLengthInSeconds (..)
  , hardForkInto
  , mkSimpleTestProtocolInfoForging
  , protocolVersionZero
  )
import qualified Test.Cardano.Tools.Headers
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

-- | Synthesize a ChainDB from scratch and then append to it, checking that
-- both steps forge blocks.
--
-- The protocol is built in-process from 'mkSimpleTestProtocolInfoForging'
-- rather than from a node configuration file; constructing a forging-capable
-- protocol from a real configuration is exercised downstream, where the
-- configuration-loading machinery lives.
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
      epochSize = sgEpochLength shelleyGenesis

  logStep "running synthesis - create"
  resultCreate <-
    DBSynthesizer.synthesize genTxs testSynthOptionsCreate epochSize chainDB protocol
  resultForged resultCreate > 0 @? "no blocks have been forged during create step"

  logStep "running synthesis - append"
  resultAppend <-
    DBSynthesizer.synthesize genTxs testSynthOptionsAppend epochSize chainDB protocol
  resultForged resultAppend > 0 @? "no blocks have been forged during append step"
 where
  genTxs _ _ _ _ = pure []

tests :: TestTree
tests =
  testGroup
    "cardano-tools"
    [ testCaseSteps "synthesize: blockCount\n" blockCountTest
    , Test.Cardano.Tools.Headers.tests
    ]

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests
