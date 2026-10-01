module Main (main) where

import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import qualified Cardano.Tools.DBAnalyser.Run as DBAnalyser
import Cardano.Tools.DBAnalyser.Types
import qualified Cardano.Tools.DBImmutaliser.Run as DBImmutaliser
import qualified Cardano.Tools.DBSynthesizer.Run as DBSynthesizer
import Cardano.Tools.DBSynthesizer.Test.QueueFixture (writeQueueFixture)
import Cardano.Tools.DBSynthesizer.Test.QueueTxFile (writeQueueTxs)
import Cardano.Tools.DBSynthesizer.TxGen.File (mkFileTxGen)
import Cardano.Tools.DBSynthesizer.Types
import qualified Cardano.Tools.DBTruncater.Run as DBTruncater
import qualified Cardano.Tools.DBTruncater.Types as DBTruncater
import Cardano.Tools.LeiosDb (LeiosDbSource (..))
import Control.Tracer (nullTracer)
import Data.String (fromString)
import LeiosDemoDb
  ( LeiosDbReader (scanEbPoints)
  , LeiosDbWriter (writeEbPoint)
  , Promise (await)
  , withLeiosDBSQLite
  , withReader
  , withWriter
  )
import LeiosDemoTypes (EbHash (..), LeiosPoint (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Cardano.Block
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
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
    , nfpPaymentKey = Nothing
    }

testNodeCredentials :: NodeCredentials
testNodeCredentials =
  NodeCredentials
    { credCertFile = Nothing
    , credVRFFile = Nothing
    , credKESFile = Nothing
    , credBulkFile = Just "ouroboros-consensus-cardano/test/tools-test/disk/config/bulk-creds-k2.json"
    , credBlsFile = Nothing
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
    , -- The stub generator below makes no transactions, so DBSynthesizer
      -- announces no endorser block and forges no certifying block. The chain
      -- then needs nothing from the leios.db that DBSynthesizer writes, and the
      -- empty in-memory LeiosDb stub is enough.
      leiosDbSource = NoLeiosDb
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
    , DBTruncater.leiosDbSource = LeiosDbFiles
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
  -- The synthesis leaves both partitions behind with no EBs, because the stub
  -- generator below makes no transactions. The kept EB is announced below the
  -- truncation slot, and the dropped one above every block the synthesis forged.
  let leiosVolDb = chainDB <> "/leios.vol.db"
      leiosImmDb = chainDB <> "/leios.imm.db"
      keptEb = MkLeiosPoint 0 (mkEbHash '1')
      droppedEb = MkLeiosPoint 500000 (mkEbHash '2')
  withLeiosDBSQLite mempty leiosVolDb leiosImmDb $ \leiosDb ->
    withWriter leiosDb $ \con ->
      mapM_ (\point -> await =<< writeEbPoint con point 500) [keptEb, droppedEb]

  logStep "running truncation"
  -- The truncater VACUUMs both partitions on its own connections, so the
  -- database above is closed before this runs.
  DBTruncater.truncate testTruncaterConfig testBlockArgs

  ebPoints <-
    withLeiosDBSQLite mempty leiosVolDb leiosImmDb $ \leiosDb ->
      withReader leiosDb scanEbPoints
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
  genTxs _ _ _ _ _ = pure ([], [], pure ())

  mkEbHash c = MkEbHash (fromString (replicate 32 c))

-- | Replay a stream of transactions from a file and certify what it announces.
--
-- Everything is built here rather than committed: 'writeQueueFixture' derives a
-- genesis with enough outputs to fill a block from the one the test above uses,
-- and both keys it writes come from fixed seeds, so the fixture is the same on
-- every run. Committing it would mean committing a signing key and two thousand
-- lines of genesis for the sake of a file this makes in a moment.
--
-- The assertion that matters is the last one. A run that announces endorser
-- blocks and certifies none still forges blocks and still exits zero -- that is
-- what a reader of 'resultForged' alone would call a pass -- so the test asks
-- the LeiosDb what is actually in it.
fileTxGenTest :: (String -> IO ()) -> Assertion
fileTxGenTest logStep =
  withSystemTempDirectory "queue" $ \tmp -> do
    let fixture = tmp </> "config"
        stream = tmp </> "txs.cbor"
        db = tmp </> "chaindb"

    logStep "building the fixture"
    writeQueueFixture (takeDirectory nodeConfig) fixture 1200

    logStep "writing the transaction stream"
    -- Ten blocks alternate to five that announce an endorser block, and each of
    -- those takes a block's worth for the ranking block and another for the
    -- endorser block. At the 246 of this fixture's blocks that is ~2460; the
    -- rest is margin, since running the stream dry is a hard failure.
    writeQueueTxs (fixture </> "shelley-genesis.json") (fixture </> "payment.skey") stream 4000

    logStep "replaying it"
    genTxs <-
      either assertFailure pure =<< mkFileTxGen stream
    (options, protocol) <-
      either assertFailure pure
        =<< DBSynthesizer.initialize
          NodeFilePaths
            { nfpConfig = fixture </> "config.json"
            , nfpChainDB = db
            , nfpPaymentKey = Nothing
            }
          NodeCredentials
            { credCertFile = Nothing
            , credVRFFile = Nothing
            , credKESFile = Nothing
            , credBulkFile = Just (fixture </> "bulk-creds-k2.json")
            , credBlsFile = Just (fixture </> "bls.skey")
            }
          DBSynthesizerOptions
            { synthLimit = ForgeLimitBlock 10
            , synthOpenMode = OpenCreateForce
            }
    result <- DBSynthesizer.synthesize genTxs options protocol
    resultForged result > 0 @? "no blocks were forged from the stream"

    logStep "checking the endorser blocks were certified"
    ebs <-
      withLeiosDBSQLite
        nullTracer
        (db </> "leios.vol.db")
        (db </> "leios.imm.db")
        (\h -> withReader h scanEbPoints)
    not (null ebs)
      @? "the stream was replayed but no endorser block reached the LeiosDb"

tests :: TestTree
tests =
  testGroup
    "cardano-tools"
    [ testCaseSteps "synthesize, analyse and truncate\n" blockCountTest
    , testCaseSteps "replay a transaction stream from a file\n" fileTxGenTest
    , Test.Cardano.Tools.Headers.tests
    ]

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests
