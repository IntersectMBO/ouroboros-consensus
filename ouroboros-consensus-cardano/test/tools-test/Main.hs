module Main (main) where

import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import qualified Cardano.Tools.DBAnalyser.Run as DBAnalyser
import Cardano.Tools.DBAnalyser.Types
import qualified Cardano.Tools.DBImmutaliser.Run as DBImmutaliser
import qualified Cardano.Tools.DBSynthesizer.Run as DBSynthesizer
import Cardano.Tools.DBSynthesizer.Types
import qualified Cardano.Tools.DBTruncater.Run as DBTruncater
import qualified Cardano.Tools.DBTruncater.Types as DBTruncater
import Data.String (fromString)
import LeiosDemoDb
  ( leiosDbInsertEbPoint
  , leiosDbScanEbPoints
  , newLeiosDBSQLite
  , withLeiosDb
  )
import LeiosDemoTypes (EbHash (..), LeiosPoint (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Cardano.Block
import qualified Test.Cardano.Tools.Headers
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.TestEnv

nodeConfig, chainDB :: FilePath
nodeConfig = "ouroboros-consensus-cardano/test/tools-test/disk/config/config.json"
chainDB = "ouroboros-consensus-cardano/test/tools-test/disk/chaindb"

testSynthOptionsCreate :: DBSynthesizerOptions
testSynthOptionsCreate =
  DBSynthesizerOptions
    { synthLimit = ForgeLimitEpoch 1
    , synthOpenMode = OpenCreateForce
    }

testSynthOptionsAppend :: DBSynthesizerOptions
testSynthOptionsAppend =
  DBSynthesizerOptions
    { synthLimit = ForgeLimitSlot 8192
    , synthOpenMode = OpenAppend
    }

testNodeFilePaths :: NodeFilePaths
testNodeFilePaths =
  NodeFilePaths
    { nfpConfig = nodeConfig
    , nfpChainDB = chainDB
    }

testNodeCredentials :: NodeCredentials
testNodeCredentials =
  NodeCredentials
    { credCertFile = Nothing
    , credVRFFile = Nothing
    , credKESFile = Nothing
    , credBulkFile = Just "ouroboros-consensus-cardano/test/tools-test/disk/config/bulk-creds-k2.json"
    }

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
    , ldbBackend = V2InMem
    , verbose = False
    , selectDB = SelectImmutableDB Origin
    , validation = Just ValidateAllBlocks
    , analysis = CountBlocks
    , confLimit = Unlimited
    , -- The synthesized chain holds no certifying block, and DBSynthesizer
      -- writes no leios.db, so the empty in-memory LeiosDb stub is both enough
      -- and the only option.
      stubbedLeiosDb = True
    }

-- | The truncater cuts the chain back to this slot. Far enough into the
-- synthesized chain that a block precedes it, and far enough from its end that
-- blocks follow it.
truncateAfter :: SlotNo
truncateAfter = 4096

testTruncaterConfig :: DBTruncater.DBTruncaterConfig
testTruncaterConfig =
  DBTruncater.DBTruncaterConfig
    { DBTruncater.dbDir = chainDB
    , DBTruncater.truncateAfter = DBTruncater.TruncateAfterSlot truncateAfter
    , DBTruncater.verbose = False
    , DBTruncater.stubbedLeiosDb = False
    }

testBlockArgs :: Cardano.Args (CardanoBlock StandardCrypto)
testBlockArgs = Cardano.CardanoBlockArgs nodeConfig Nothing

-- | A multi-step test including synthesis and analysis 'SomeConsensusProtocol' using the Cardano instance.
--
-- 1. step: synthesize a ChainDB from scratch and count the amount of blocks forged.
-- 2. step: append to the previous ChainDB and coutn the amount of blocks forged.
-- 3. step: copy the VolatileDB into the ImmutableDB.
-- 3. step: analyze the ImmutableDB resulting from previous steps and confirm the total block count.

--
blockCountTest :: (String -> IO ()) -> Assertion
blockCountTest logStep = do
  logStep "running synthesis - create"
  (options, protocol) <-
    either assertFailure pure
      =<< DBSynthesizer.initialize
        testNodeFilePaths
        testNodeCredentials
        testSynthOptionsCreate
  resultCreate <- DBSynthesizer.synthesize genTxs options protocol
  let blockCountCreate = resultForged resultCreate
  blockCountCreate > 0 @? "no blocks have been forged during create step"

  logStep "running synthesis - append"
  resultAppend <-
    DBSynthesizer.synthesize genTxs options{confOptions = testSynthOptionsAppend} protocol
  let blockCountAppend = resultForged resultAppend
  blockCountAppend > 0 @? "no blocks have been forged during append step"

  logStep "copy volatile to immutable DB"
  DBImmutaliser.run testImmutaliserConfig

  logStep "running analysis"
  resultAnalysis <- DBAnalyser.analyse testAnalyserConfig testBlockArgs

  let blockCount = blockCountCreate + blockCountAppend
  resultAnalysis == Just (ResultCountBlock blockCount)
    @? "wrong number of blocks encountered during analysis \
       \ (counted: "
      ++ show resultAnalysis
      ++ "; expected: "
      ++ show blockCount
      ++ ")"

  logStep "writing a LeiosDb next to the chain"
  -- DBSynthesizer writes no leios.db, so the test writes one. The kept EB is
  -- announced below the truncation slot, and the dropped one above every block
  -- the synthesis forged.
  leiosDb <- newLeiosDBSQLite mempty (chainDB <> "/leios.db")
  let keptEb = MkLeiosPoint 0 (mkEbHash '1')
      droppedEb = MkLeiosPoint 500000 (mkEbHash '2')
  withLeiosDb leiosDb $ \con ->
    mapM_ (\point -> leiosDbInsertEbPoint con point 500) [keptEb, droppedEb]

  logStep "running truncation"
  DBTruncater.truncate testTruncaterConfig testBlockArgs

  ebPoints <- withLeiosDb leiosDb leiosDbScanEbPoints
  ebPoints == [(0, pointEbHash keptEb)]
    @? "the LeiosDb does not hold the kept EB alone: " ++ show ebPoints

  logStep "running analysis after truncation"
  resultTruncated <- DBAnalyser.analyse testAnalyserConfig testBlockArgs
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

  mkEbHash c = MkEbHash (fromString (replicate 32 c))

tests :: TestTree
tests =
  testGroup
    "cardano-tools"
    [ testCaseSteps "synthesize, analyse and truncate\n" blockCountTest
    , Test.Cardano.Tools.Headers.tests
    ]

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests
