module Main (main) where

import qualified Cardano.Configuration.CliArgs as CLI
import Cardano.Ledger.BaseTypes (StrictMaybe (SJust))
import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import qualified Cardano.Tools.DBAnalyser.Run as DBAnalyser
import Cardano.Tools.DBAnalyser.Types
import qualified Cardano.Tools.DBImmutaliser.Run as DBImmutaliser
import qualified Cardano.Tools.DBSynthesizer.Run as DBSynthesizer
import Cardano.Tools.DBSynthesizer.Types
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Cardano.Block
import qualified Test.Cardano.Tools.DBAnalyser.NodeConfig
import qualified Test.Cardano.Tools.Headers
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.TestEnv

nodeConfig, chainDB, bulkCredentials :: FilePath
nodeConfig = "ouroboros-consensus-cardano/test/tools-test/disk/config/config.json"
chainDB = "ouroboros-consensus-cardano/test/tools-test/disk/chaindb"
bulkCredentials = "ouroboros-consensus-cardano/test/tools-test/disk/config/bulk-creds-k2.json"

-- | A tenth of an epoch, then a further 8192 slots: enough for both steps to
-- forge hundreds of blocks, small enough for the analysis to stay quick. The
-- tool also accepts block and epoch limits ('ForgeLimitBlock',
-- 'ForgeLimitEpoch').
testSynthOptionsCreate :: DBSynthesizerOptions
testSynthOptionsCreate =
  DBSynthesizerOptions
    { synthLimit = ForgeLimitSlot 43200
    , synthOpenMode = OpenCreateForce
    }

testSynthOptionsAppend :: DBSynthesizerOptions
testSynthOptionsAppend =
  DBSynthesizerOptions
    { synthLimit = ForgeLimitSlot 8192
    , synthOpenMode = OpenAppend
    }

-- | The forgers of the fixture chain: two pools whose credentials are in a bulk
-- credentials file, and which the fixture Shelley genesis gives all the stake.
testCredentials :: CLI.Credentials
testCredentials =
  CLI.emptyCredentials{CLI.bulkCredentialsFile = SJust bulkCredentials}

testImmutaliserConfig :: DBImmutaliser.Opts
testImmutaliserConfig =
  DBImmutaliser.Opts
    { DBImmutaliser.dbDirs =
        DBImmutaliser.DBDirs
          { DBImmutaliser.immDBDir = chainDB <> "/immutable"
          , DBImmutaliser.volDBDir = chainDB <> "/volatile"
          }
    , DBImmutaliser.configFile = nodeConfig
    , DBImmutaliser.verbose = False
    , DBImmutaliser.dotOut = Nothing
    , DBImmutaliser.dryRun = False
    }

testAnalyserConfig :: DBAnalyserConfig
testAnalyserConfig =
  DBAnalyserConfig
    { dbDir = chainDB
    , ldbBackend = Just V2InMem
    , verbose = False
    , selectDB = SelectImmutableDB Origin
    , validation = Just ValidateAllBlocks
    , analysis = CountBlocks
    , confLimit = Unlimited
    }

testBlockArgs :: Cardano.Args (CardanoBlock StandardCrypto)
testBlockArgs = Cardano.CardanoBlockArgs nodeConfig Nothing

-- | How many blocks each synthesis step is expected to forge.
--
-- Empirical, but not arbitrary: the setup is deterministic, so a change in these
-- means the forging loop, the leader schedule or the way a configuration becomes
-- a protocol changed. Pinning them catches what a mere @> 0@ would not.
--
-- The create step covers slots 0..43199 and the append step a further 8192, so
-- the counts differ. To re-baseline, take what db-synthesizer prints as
-- @forged and adopted N blocks@.
expectedForgedCreate, expectedForgedAppend :: Int
expectedForgedCreate = 2189
expectedForgedAppend = 407

-- | A multi-step test covering synthesis and analysis of a Cardano chain:
--
-- 1. synthesize a ChainDB from scratch from a node configuration file and the
--    forging credentials it names, and count the blocks forged;
-- 2. append to that ChainDB and count the blocks forged;
-- 3. copy the VolatileDB into the ImmutableDB;
-- 4. analyse the resulting ImmutableDB and confirm the total block count.
--
-- Steps 1 and 2 cover the path from a configuration file to a forging protocol:
-- cardano-config parses it and "Cardano.Tools.Credentials" decodes the
-- credentials into leader credentials.
blockCountTest :: (String -> IO ()) -> Assertion
blockCountTest logStep = do
  logStep "building the protocol from the node configuration"
  (shelleyGenesis, protocol) <- DBSynthesizer.initialize nodeConfig testCredentials

  logStep "running synthesis - create"
  resultCreate <-
    DBSynthesizer.synthesize genTxs testSynthOptionsCreate shelleyGenesis chainDB protocol
  assertForged "create" expectedForgedCreate resultCreate

  logStep "running synthesis - append"
  resultAppend <-
    DBSynthesizer.synthesize genTxs testSynthOptionsAppend shelleyGenesis chainDB protocol
  assertForged "append" expectedForgedAppend resultAppend

  logStep "copy volatile to immutable DB"
  DBImmutaliser.run testImmutaliserConfig

  logStep "running analysis"
  resultAnalysis <- DBAnalyser.analyse testAnalyserConfig testBlockArgs

  let blockCount = expectedForgedCreate + expectedForgedAppend
  resultAnalysis == Just (ResultCountBlock blockCount)
    @? "wrong number of blocks encountered during analysis \
       \ (counted: "
      ++ show resultAnalysis
      ++ "; expected: "
      ++ show blockCount
      ++ ")"
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
    [ testCaseSteps "synthesize and analyse: blockCount\n" blockCountTest
    , Test.Cardano.Tools.DBAnalyser.NodeConfig.tests
    , Test.Cardano.Tools.Headers.tests
    ]

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests
