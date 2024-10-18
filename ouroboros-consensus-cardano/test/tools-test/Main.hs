module Main (main) where

import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import qualified Cardano.Tools.DBAnalyser.Run as DBAnalyser
import Cardano.Tools.DBAnalyser.Types
import qualified Cardano.Tools.DBImmutaliser.Run as DBImmutaliser
import qualified Cardano.Tools.DBSynthesizer.Run as DBSynthesizer
import Cardano.Tools.DBSynthesizer.Types
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Cardano.Block
import qualified Test.Cardano.Tools.Headers
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.TestEnv

nodeConfig, chainDB :: FilePath
nodeConfig = "test/tools-test/disk/config/config.json"
chainDB = "test/tools-test/disk/chaindb"

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
        , credBulkFile = Just "test/tools-test/disk/config/bulk-creds-k2.json"
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
        }

testAnalyserConfig :: DBAnalyserConfig
testAnalyserConfig =
    DBAnalyserConfig
        { dbDir = chainDB
        , verbose = False
        , selectDB = SelectImmutableDB Origin
        , validation = Just ValidateAllBlocks
        , analysis = CountBlocks
        , confLimit = Unlimited
        }

testBlockArgs :: Cardano.Args (CardanoBlock StandardCrypto)
testBlockArgs = Cardano.CardanoBlockArgs nodeConfig Nothing

{- | A multi-step test including synthesis and analaysis 'SomeConsensusProtocol' using the Cardano instance.

1. step: synthesize a ChainDB from scratch and count the amount of blocks forged.
2. step: append to the previous ChainDB and coutn the amount of blocks forged.
3. step: copy the VolatileDB into the ImmutableDB.
3. step: analyze the ImmutableDB resulting from previous steps and confirm the total block count.
-}

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
    resultAppend <- DBSynthesizer.synthesize genTxs options{confOptions = testSynthOptionsAppend} protocol
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
  where
    genTxs _ _ _ = pure []

tests :: TestTree
tests =
    testGroup
        "cardano-tools"
        [ testCaseSteps "synthesize and analyse: blockCount\n" blockCountTest
        , Test.Cardano.Tools.Headers.tests
        ]

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests
