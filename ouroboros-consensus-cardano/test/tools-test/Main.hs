{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.Block as SL
import Cardano.Ledger.Dijkstra.BlockBody (leiosCertBlockBodyL)
import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import qualified Cardano.Tools.DBAnalyser.Run as DBAnalyser
import Cardano.Tools.DBAnalyser.Types
import qualified Cardano.Tools.DBImmutaliser.Run as DBImmutaliser
import qualified Cardano.Tools.DBSynthesizer.Run as DBSynthesizer
import Cardano.Tools.DBSynthesizer.Test.QueueFixture (writeQueueFixture)
import Cardano.Tools.DBSynthesizer.Test.QueueTxFile (writeQueueTxs)
import Cardano.Tools.DBSynthesizer.TxGen.File (decodeStream, mkFileTxGen)
import Cardano.Tools.DBSynthesizer.Types
import qualified Cardano.Tools.DBTruncater.Run as DBTruncater
import qualified Cardano.Tools.DBTruncater.Types as DBTruncater
import Cardano.Tools.LeiosDb (LeiosDbSource (..))
import Control.Exception (ErrorCall, bracket, evaluate, try)
import Control.ResourceRegistry (withRegistry)
import Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as BSL
import Data.List (isInfixOf)
import Data.String (fromString)
import Data.Word (Word64)
import LeiosDemoDb
  ( LeiosDbReader (scanEbPoints)
  , LeiosDbWriter (writeEbPoint)
  , Promise (await)
  , withLeiosDBSQLite
  , withReader
  , withWriter
  )
import LeiosDemoTypes (LeiosPoint (..))
import Lens.Micro ((^.))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Config (TopLevelConfig, configCodec, configStorage)
import Ouroboros.Consensus.Node.InitStorage
  ( nodeCheckIntegrity
  , nodeImmutableDbChunkInfo
  )
import Ouroboros.Consensus.Node.ProtocolInfo (pInfoConfig)
import Ouroboros.Consensus.Shelley.Ledger.Block (shelleyBlockRaw)
import Ouroboros.Consensus.Storage.Common (BlockComponent (..))
import Ouroboros.Consensus.Storage.ImmutableDB (IteratorResult (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import Ouroboros.Consensus.Storage.ImmutableDB.Impl
import Ouroboros.Consensus.Util.Args (Complete)
import System.FS.API (MountPoint (..), SomeHasFS (..))
import System.FS.IO (ioHasFS)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import qualified Test.Cardano.Tools.Headers
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.LeiosHash (unsafeEbHashFromBytes)
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
      keptEb = MkLeiosPoint 0 (ebhOf '1')
      droppedEb = MkLeiosPoint 500000 (ebhOf '2')
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

  ebhOf c = unsafeEbHashFromBytes (fromString (replicate 32 c))

-- | Replay a stream of transactions from a file and certify what it announces.
--
-- Everything is built here rather than committed: 'writeQueueFixture' derives a
-- genesis with enough outputs to fill a block from the one the test above uses,
-- and both keys it writes come from fixed seeds, so the fixture is the same on
-- every run. Committing it would mean committing a signing key and two thousand
-- lines of genesis for the sake of a file this makes in a moment.
--
-- The assertion that matters is the last one, and it reads the chain rather
-- than the LeiosDb. A run that announces endorser blocks and certifies none
-- still forges blocks and still exits zero -- what a reader of 'resultForged'
-- alone would call a pass -- and the LeiosDb cannot tell the difference either,
-- because it gains its row when the endorser block is announced. A certificate
-- on a block is the thing only certification produces, so 'certifiedBlockSlots'
-- goes looking for those.
fileTxGenTest :: (String -> IO ()) -> Assertion
fileTxGenTest logStep =
  withSystemTempDirectory "queue" $ \tmp -> do
    let fixture = tmp </> "config"
        stream = tmp </> "txs.cbor"
        db = tmp </> "chaindb"
        -- Blocks alternate: one announces an endorser block, the next
        -- certifies it and makes none of its own. Keep this even, so the chain
        -- ends on a certifying block and every announcement has its
        -- certificate. An odd count leaves the last announcement outstanding
        -- and the final assertion fails.
        blockCount = 10 :: Word64

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
            { synthLimit = ForgeLimitBlock blockCount
            , synthOpenMode = OpenCreateForce
            }
    result <- DBSynthesizer.synthesize genTxs options protocol
    resultForged result == fromIntegral blockCount
      @? "the stream was replayed to "
        ++ show (resultForged result)
        ++ " blocks, not the "
        ++ show blockCount
        ++ " the run was limited to"

    logStep "checking the endorser blocks were announced"
    ebs <-
      withLeiosDBSQLite
        nullTracer
        (db </> "leios.vol.db")
        (db </> "leios.imm.db")
        (\h -> withReader h scanEbPoints)
    not (null ebs)
      @? "the stream was replayed but no endorser block reached the LeiosDb"

    -- The chain is ten blocks against a security parameter of 2160, so every
    -- one of them is still in the VolatileDB and the ImmutableDB is empty.
    -- Immutalise first, as the analyser test above does, or there is nothing
    -- to walk.
    logStep "immutalising the chain"
    DBImmutaliser.run
      DBImmutaliser.Opts
        { DBImmutaliser.dbDirs =
            DBImmutaliser.DBDirs
              { DBImmutaliser.immDBDir = db </> "immutable"
              , DBImmutaliser.volDBDir = db </> "volatile"
              }
        , DBImmutaliser.configFile = fixture </> "config.json"
        , DBImmutaliser.verbose = False
        , DBImmutaliser.dotOut = Nothing
        , DBImmutaliser.dryRun = False
        }

    logStep "checking the endorser blocks were certified"
    config <- pInfoConfig <$> mkProtocolInfo (Cardano.CardanoBlockArgs (fixture </> "config.json") Nothing)
    certified <- certifiedBlockSlots config db
    length certified == length ebs
      @? "endorser blocks were announced at "
        ++ show (map fst ebs)
        ++ " but certificates reached the chain only at "
        ++ show certified

-- | Decode a stream file on its own, including the two ways of damaging one.
--
-- 'fileTxGenTest' only ever hands 'decodeStream' a stream that was written
-- correctly, so nothing pins down what it does with one that was not, though
-- the module promises to reject both of these. Neither case needs a ledger or a
-- ChainDB, so the stream here is a short one.
decodeStreamTest :: (String -> IO ()) -> Assertion
decodeStreamTest logStep =
  withSystemTempDirectory "stream" $ \tmp -> do
    let fixture = tmp </> "config"
        stream = tmp </> "txs.cbor"
        txCount = 24

    logStep "writing a short stream"
    writeQueueFixture (takeDirectory nodeConfig) fixture 16
    writeQueueTxs
      (fixture </> "shelley-genesis.json")
      (fixture </> "payment.skey")
      stream
      txCount
    bytes <- BSL.readFile stream

    logStep "a finished stream decodes to what was written"
    length (decodeStream stream GenTxDijkstra bytes) @?= txCount

    logStep "a stream cut short does not decode"
    -- A finished stream ends in the break, so dropping the last byte is a
    -- stream that stops without one -- a writer interrupted mid-file.
    cutShort <- expectRejected (decodeStream stream GenTxDijkstra (BSL.init bytes))
    "does not decode" `isInfixOf` cutShort
      @? "a stream cut short was rejected, but for another reason: " ++ cutShort

    logStep "bytes after the end are rejected"
    -- The stream twice over: a second one appended behind the first's break,
    -- of which a run would otherwise replay only the first.
    trailing <- expectRejected (decodeStream stream GenTxDijkstra (bytes <> bytes))
    "after the end of the stream" `isInfixOf` trailing
      @? "an overlong stream was rejected, but for another reason: " ++ trailing
 where
  -- 'decodeStream' raises from inside the list it returns, so the list has to
  -- be forced before anything is raised at all.
  expectRejected txs =
    try @ErrorCall (evaluate (length txs)) >>= \case
      Left err -> pure (show err)
      Right n ->
        assertFailure $
          "expected the stream to be rejected, but it decoded to "
            ++ show n
            ++ " transactions"

-- | The slots of the forged blocks that carry a Leios certificate.
--
-- Reads the chain back out of the ImmutableDB the run just wrote, the way
-- 'Cardano.Tools.DBTruncater.Run.truncate' reads one, and keeps the blocks
-- whose body holds a certificate, the way the Leios ThreadNet test finds its
-- certifying blocks. A certificate on a block is what certification leaves
-- behind on the chain. The LeiosDb cannot answer this: its @ebs@ table gains a
-- row when an endorser block is announced, so a row proves an announcement and
-- says nothing about whether anything certified it.
certifiedBlockSlots ::
  TopLevelConfig (CardanoBlock StandardCrypto) ->
  FilePath ->
  IO [SlotNo]
certifiedBlockSlots config chainDbDir =
  withRegistry $ \registry -> do
    let immutableDBArgs :: Complete ImmutableDbArgs IO (CardanoBlock StandardCrypto)
        immutableDBArgs =
          (ImmutableDB.defaultArgs @IO)
            { immTracer = nullTracer
            , immRegistry = registry
            , immCheckIntegrity = nodeCheckIntegrity (configStorage config)
            , immCodecConfig = configCodec config
            , immChunkInfo = nodeImmutableDbChunkInfo (configStorage config)
            , immHasFS = SomeHasFS (ioHasFS (MountPoint (chainDbDir </> "immutable")))
            }
    bracket
      (ImmutableDB.openDBInternal immutableDBArgs)
      (ImmutableDB.closeDB . fst)
      $ \(immutableDB, _internal) -> do
        iterator <- ImmutableDB.streamAll immutableDB registry GetBlock
        let go acc =
              ImmutableDB.iteratorNext iterator >>= \case
                IteratorExhausted -> do
                  ImmutableDB.iteratorClose iterator
                  pure (reverse acc)
                IteratorResult blk -> go (carriesCert blk ++ acc)
        go []
 where
  carriesCert blk = case blk of
    BlockDijkstra dijkstraBlk
      | SL.Block _ body <- shelleyBlockRaw dijkstraBlk
      , SJust _ <- body ^. leiosCertBlockBodyL ->
          [blockSlot blk]
    _ -> []

tests :: TestTree
tests =
  testGroup
    "cardano-tools"
    [ testCaseSteps "synthesize, analyse and truncate\n" blockCountTest
    , testCaseSteps "replay a transaction stream from a file\n" fileTxGenTest
    , testCaseSteps "decode a transaction stream file\n" decodeStreamTest
    , Test.Cardano.Tools.Headers.tests
    ]

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests
