module Main (main) where

import qualified Cardano.Configuration.CliArgs as CLI
import Cardano.Ledger.BaseTypes (StrictMaybe (SJust))
import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import qualified Cardano.Tools.DBAnalyser.Run as DBAnalyser
import Cardano.Tools.DBAnalyser.Types
import qualified Cardano.Tools.DBImmutaliser.Run as DBImmutaliser
import qualified Cardano.Tools.DBSynthesizer.Run as DBSynthesizer
import Cardano.Tools.DBSynthesizer.Types
import qualified Cardano.Tools.DBTruncater.Run as DBTruncater
import qualified Cardano.Tools.DBTruncater.Types as DBTruncater
import Data.String (fromString)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Leios.Types (EbHash (..), LeiosPoint (..))
import Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbReader (testScanEbPoints)
  , LeiosDbWriter (writeEbPoint)
  , Promise (await)
  , withLeiosDBSQLite
  , withReader
  , withWriter
  )
import System.IO.Temp (withSystemTempDirectory)
import qualified Test.Cardano.Tools.DBAnalyser.NodeConfig
import qualified Test.Cardano.Tools.Headers
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.TestEnv

nodeConfig, bulkCredentials :: FilePath
nodeConfig = "ouroboros-consensus-cardano/test/tools-test/disk/config/config.json"
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

testImmutaliserConfig :: FilePath -> DBImmutaliser.Opts
testImmutaliserConfig chainDB =
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

testAnalyserConfig :: FilePath -> DBAnalyserConfig
testAnalyserConfig chainDB =
  DBAnalyserConfig
    { dbDir = chainDB
    , ldbBackend = Just InMemFlag
    , verbose = False
    , selectDB = SelectImmutableDB Origin
    , validation = Just ValidateAllBlocks
    , analysis = CountBlocks
    , confLimit = Unlimited
    }

-- | The truncater cuts the chain back to this slot. Far enough into the
-- synthesized chain that a block precedes it, and far enough from its end that
-- blocks follow it.
truncateAfter :: SlotNo
truncateAfter = 4096

testTruncaterConfig :: FilePath -> DBTruncater.DBTruncaterConfig
testTruncaterConfig chainDB =
  DBTruncater.DBTruncaterConfig
    { DBTruncater.dbDir = chainDB
    , DBTruncater.truncateAfter = DBTruncater.TruncateAfterSlot truncateAfter
    , DBTruncater.verbose = False
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

-- 1. step: synthesize a ChainDB from scratch and count the amount of blocks forged.
-- 2. step: append to the previous ChainDB and coutn the amount of blocks forged.
-- 3. step: copy the VolatileDB into the ImmutableDB.
-- 4. step: analyze the ImmutableDB resulting from previous steps and confirm the total block count.
-- 5. step: write a LeiosDb next to the chain, truncate both, and confirm the LeiosDb kept
--    only the EB announced below the cut and the chain shrank.

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
blockCountTest logStep = withSystemTempDirectory "tools-chain" $ \chainDB -> do
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
  DBImmutaliser.run (testImmutaliserConfig chainDB)

  logStep "running analysis"
  resultAnalysis <- DBAnalyser.analyse (testAnalyserConfig chainDB) testBlockArgs

  let blockCount = expectedForgedCreate + expectedForgedAppend
  resultAnalysis == Just (ResultCountBlock blockCount)
    @? "wrong number of blocks encountered during analysis \
       \ (counted: "
      ++ show resultAnalysis
      ++ "; expected: "
      ++ show blockCount
      ++ ")"

  logStep "writing a LeiosDb next to the chain"
  -- DBSynthesizer writes no leios.vol.db and leios.imm.db, so the test writes
  -- them. The kept EB is announced below the truncation slot, and the dropped
  -- one above every block the synthesis forged. The handle is closed before
  -- the truncater runs: it opens the files itself and VACUUMs them.
  let volLeiosDb = chainDB <> "/leios.vol.db"
      immLeiosDb = chainDB <> "/leios.imm.db"
      keptEb = MkLeiosPoint 0 (mkEbHash '1')
      droppedEb = MkLeiosPoint 500000 (mkEbHash '2')
  withLeiosDBSQLite mempty volLeiosDb immLeiosDb $ \leiosDb ->
    withWriter leiosDb $ \con ->
      mapM_ (\point -> await =<< writeEbPoint con point 500) [keptEb, droppedEb]

  logStep "running truncation"
  DBTruncater.truncate (testTruncaterConfig chainDB) testBlockArgs

  ebPoints <- withLeiosDBSQLite mempty volLeiosDb immLeiosDb $ \leiosDb ->
    withReader leiosDb testScanEbPoints
  ebPoints == [(0, pointEbHash keptEb)]
    @? "the LeiosDb does not hold the kept EB alone: " ++ show ebPoints

  logStep "running analysis after truncation"
  resultTruncated <- DBAnalyser.analyse (testAnalyserConfig chainDB) testBlockArgs
  -- The leader schedule picks the slots, so the surviving count is not known
  -- here. Check only that the chain shrank and is not empty.
  case resultTruncated of
    Just (ResultCountBlock countAfter) ->
      (countAfter > 0 && countAfter < blockCount)
        @? "truncation left "
          ++ show countAfter
          ++ " of "
          ++ show blockCount
          ++ " blocks"
    _ -> assertFailure $ "analysis after truncation returned " ++ show resultTruncated
 where
  genTxs _ _ _ _ = pure []

  assertForged step expected result =
    assertEqual
      ("wrong number of blocks forged during the " <> step <> " step")
      expected
      (resultForged result)

  mkEbHash c = MkEbHash (fromString (replicate 32 c))

tests :: TestTree
tests =
  testGroup
    "cardano-tools"
    [ testCaseSteps "synthesize and analyse: blockCount\n" blockCountTest
    , Test.Cardano.Tools.DBAnalyser.NodeConfig.tests
    , testCaseSteps "synthesize, analyse and truncate\n" blockCountTest
    , Test.Cardano.Tools.Headers.tests
    ]

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests
