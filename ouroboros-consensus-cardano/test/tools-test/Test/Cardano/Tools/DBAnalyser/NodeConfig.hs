{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the way db-analyser interprets a node configuration file.
--
-- The parsing itself is @cardano-config@'s job, but db-analyser derives a few
-- things from the result (the initial nonce, the hard-fork triggers, the
-- LedgerDB backend), and those derivations are what these tests pin down. They
-- run against the node configuration in @disk/config@, so they also catch the
-- silent failure mode of @cardano-config@ ignoring a key db-analyser depends on.
module Test.Cardano.Tools.DBAnalyser.NodeConfig (tests) where

import qualified Cardano.Configuration as Cfg
import qualified Cardano.Crypto.Hash.Class as CryptoClass
import Cardano.Ledger.BaseTypes (Nonce (..))
import Cardano.Tools.DBAnalyser.Block.Cardano
  ( Args (CardanoBlockArgs)
  , mkHardForkTriggers
  , mkInitialNonce
  , mkLedgerDBBackend
  , resolveNodeConfiguration
  )
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfoAndBackend)
import Cardano.Tools.DBAnalyser.Types
  ( ConfigError (..)
  , LSMOptions (..)
  , LedgerDBBackend (..)
  )
import Control.Exception (try)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (intersect, isInfixOf)
import Data.SOP.BasicFunctors (K (..))
import Data.SOP.Strict (hcollapse, hmap)
import Ouroboros.Consensus.Block (EpochNo (..))
import Ouroboros.Consensus.Cardano.Node
  ( CardanoHardForkTrigger (..)
  , CardanoHardForkTriggers (..)
  )
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import System.Directory (copyFile, listDirectory)
import System.FilePath (takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

-- | The directory holding the node configuration these tests run against: a
-- legacy-format Cardano configuration that sets @Test\<era\>HardForkAtEpoch@ for
-- Shelley through Babbage and has no @LedgerDB@ section.
configDir :: FilePath
configDir = "ouroboros-consensus-cardano/test/tools-test/disk/config"

configFile :: FilePath
configFile = configDir </> "config.json"

tests :: TestTree
tests =
  testGroup
    "NodeConfig"
    [ testCase "recognises the keys we depend on" test_recognisesOurKeys
    , testCase "hard-fork triggers" test_hardForkTriggers
    , testCase "hard-fork triggers reject a gap" test_hardForkTriggersGap
    , testCase "initial nonce hashes the Shelley genesis" test_initialNonce
    , testCase "LedgerDB backend defaults to in-memory" test_ledgerDBBackendDefault
    , testCase "LedgerDB LSM backend" test_ledgerDBBackendLSM
    , testCase "LedgerDB LSM rejects an absolute path" test_ledgerDBBackendLSMAbsolute
    , testCase "a malformed configuration is a ConfigError" test_malformedIsConfigError
    , testCase "builds a ProtocolInfo" test_mkProtocolInfoAndBackend
    ]

{-------------------------------------------------------------------------------
  Configuration variants

  Rather than checking in a near-identical copy of the configuration for every
  case, the variants are derived from it at run time: the whole configuration
  directory is copied into a temporary directory, so that the genesis paths keep
  resolving, and the configuration file itself is patched there.
-------------------------------------------------------------------------------}

-- | Run an action on a copy of 'configFile' whose top-level object has been
-- extended with (or, with a 'Nothing' value, stripped of) the given keys.
withPatchedConfig :: [(Aeson.Key, Maybe Aeson.Value)] -> (FilePath -> IO a) -> IO a
withPatchedConfig patches k =
  withSystemTempDirectory "db-analyser-config" $ \tmpDir -> do
    entries <- listDirectory configDir
    mapM_ (\e -> copyFile (configDir </> e) (tmpDir </> e)) entries
    let patchedFile = tmpDir </> takeFileName configFile
    original <-
      Aeson.eitherDecodeFileStrict' patchedFile >>= \case
        Left err -> assertFailure $ "could not re-read the configuration: " <> err
        Right (Aeson.Object o) -> pure o
        Right _ -> assertFailure "the configuration is not a JSON object"
    let patched = foldr applyPatch original patches
    BL.writeFile patchedFile (Aeson.encode (Aeson.Object patched))
    k patchedFile
 where
  applyPatch (key, mbValue) =
    maybe (KeyMap.delete key) (KeyMap.insert key) mbValue

-- | Select the LSM backend, with the given @LedgerDB@ settings.
lsmBackend :: [(Aeson.Key, Aeson.Value)] -> (Aeson.Key, Maybe Aeson.Value)
lsmBackend settings =
  ( "LedgerDB"
  , Just $ Aeson.Object $ KeyMap.fromList $ ("Backend", "V2LSM") : settings
  )

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | Resolving this configuration does warn, because it is in the legacy node
-- format and carries keys that are none of db-analyser's business. What must
-- never happen is @cardano-config@ not recognising a key db-analyser /does/
-- read: that would silently fall back to a default instead of failing.
test_recognisesOurKeys :: Assertion
test_recognisesOurKeys = do
  (_nc, warns) <- resolveNodeConfiguration configFile
  let ignored = concatMap unrecognisedKeys warns
  ignored `intersect` keysWeDependOn @?= []
 where
  unrecognisedKeys :: Cfg.ConfigWarning -> [String]
  unrecognisedKeys = \case
    Cfg.UnrecognisedKeys keys -> keys
    _ -> []

  keysWeDependOn :: [String]
  keysWeDependOn =
    [ "ByronGenesisFile"
    , "ByronGenesisHash"
    , "ShelleyGenesisFile"
    , "ShelleyGenesisHash"
    , "AlonzoGenesisFile"
    , "ConwayGenesisFile"
    , "DijkstraGenesisFile"
    , "RequiresNetworkMagic"
    , "TestShelleyHardForkAtEpoch"
    , "TestAllegraHardForkAtEpoch"
    , "TestMaryHardForkAtEpoch"
    , "TestAlonzoHardForkAtEpoch"
    , "TestBabbageHardForkAtEpoch"
    , "TestConwayHardForkAtEpoch"
    , "TestDijkstraHardForkAtEpoch"
    ]

-- | The epochs the configuration sets must survive into the triggers. A silent
-- fallback to 'CardanoTriggerHardForkAtDefaultVersion' would make db-analyser
-- replay a different era than the configuration asks for, so pin the whole list
-- down rather than just spot-checking one era.
test_hardForkTriggers :: Assertion
test_hardForkTriggers = do
  (nc, _warns) <- resolveNodeConfiguration configFile
  case mkHardForkTriggers (Cfg.testingConfiguration nc) of
    Left err -> assertFailure $ "expected triggers, but got: " <> err
    Right triggers ->
      -- Shelley, Allegra, Mary, Alonzo and Babbage at epoch 0; Conway and
      -- Dijkstra are not configured, so they trigger at their default version.
      triggerEpochs triggers
        @?= [ Just (EpochNo 0)
            , Just (EpochNo 0)
            , Just (EpochNo 0)
            , Just (EpochNo 0)
            , Just (EpochNo 0)
            , Nothing
            , Nothing
            ]

-- | An era configured to hard-fork at an epoch while an earlier era is not is
-- rejected, and reported as a 'ConfigError' rather than as a crash.
test_hardForkTriggersGap :: Assertion
test_hardForkTriggersGap =
  -- Leaves Shelley and Allegra set, and Alonzo and Babbage set, with the gap at
  -- Mary.
  withPatchedConfig [("TestMaryHardForkAtEpoch", Nothing)] $ \file -> do
    (nc, _warns) <- resolveNodeConfiguration file
    case mkHardForkTriggers (Cfg.testingConfiguration nc) of
      Right triggers ->
        assertFailure $ "expected a rejection, but got: " <> show (triggerEpochs triggers)
      Left err ->
        "it must also set it for all previous eras" `isInfixOf` err
          @? "unexpected message: " <> err

    try (mkProtocolInfoAndBackend (CardanoBlockArgs file Nothing)) >>= \case
      Left (ConfigError _) -> pure ()
      Right _ -> assertFailure "expected mkProtocolInfoAndBackend to reject the configuration"

-- | The initial nonce is the Blake2b-256 hash of the Shelley genesis file. The
-- configuration states that hash itself, so this also checks that the stated
-- hash and the file on disk have not drifted apart.
test_initialNonce :: Assertion
test_initialNonce = do
  (nc, _warns) <- resolveNodeConfiguration configFile
  expected <-
    Nonce . CryptoClass.castHash . CryptoClass.hashWith id
      <$> BS.readFile (configDir </> "shelley-genesis.json")
  mkInitialNonce nc @?= expected

-- | This configuration has no @LedgerDB@ section, and @cardano-config@ defaults
-- the backend to the in-memory one, so that is what db-analyser uses when the
-- command line selects no backend.
test_ledgerDBBackendDefault :: Assertion
test_ledgerDBBackendDefault = do
  (nc, _warns) <- resolveNodeConfiguration configFile
  backendOf nc >>= \case
    Just V2InMem -> pure ()
    other -> assertFailure $ "expected the in-memory backend, but got " <> describe other

-- | The LSM paths are taken from the configuration as they are: they are
-- interpreted relative to the ChainDB, not to the configuration file, so unlike
-- the genesis paths they must not be adjusted.
test_ledgerDBBackendLSM :: Assertion
test_ledgerDBBackendLSM =
  withPatchedConfig
    [ lsmBackend
        [ ("LSMDatabasePath", "some-lsm-dir")
        , ("LSMExportPath", "some-export-dir")
        ]
    ]
    $ \file -> do
      (nc, _warns) <- resolveNodeConfiguration file
      backendOf nc >>= \case
        Just (V2LSM opts) -> do
          lsmDatabasePath opts @?= "some-lsm-dir"
          lsmExportPath opts @?= Just "some-export-dir"
          -- Not configurable via the node configuration file.
          lsmNoDiskCache opts @?= False
        other -> assertFailure $ "expected the LSM backend, but got " <> describe other

-- | @cardano-config@ happily accepts an absolute @LSMDatabasePath@, but
-- db-analyser cannot honour one, because it mounts the path inside the ChainDB.
-- Rejecting it beats silently using a different directory than the one asked
-- for.
test_ledgerDBBackendLSMAbsolute :: Assertion
test_ledgerDBBackendLSMAbsolute =
  withPatchedConfig [lsmBackend [("LSMDatabasePath", "/absolute/lsm")]] $ \file -> do
    (nc, _warns) <- resolveNodeConfiguration file
    case mkLedgerDBBackend (Cfg.storageConfiguration nc) of
      Right backend ->
        assertFailure $ "expected a rejection, but got " <> describe backend
      Left err -> do
        "LSMDatabasePath" `isInfixOf` err @? "unexpected message: " <> err
        "must be relative" `isInfixOf` err @? "unexpected message: " <> err

-- | @cardano-config@ reports a missing mandatory key by throwing, not in its
-- result, so check that such a configuration still reaches the user as a
-- 'ConfigError' -- ie as a plain message -- rather than as a backtrace.
test_malformedIsConfigError :: Assertion
test_malformedIsConfigError =
  withPatchedConfig [("ByronGenesisHash", Nothing)] $ \file ->
    try (resolveNodeConfiguration file) >>= \case
      Right _ -> assertFailure "expected the configuration to be rejected"
      Left (ConfigError msg) ->
        "ByronGenesisHash" `isInfixOf` msg @? "unexpected message: " <> msg

-- | The end-to-end path db-analyser actually takes, which additionally forces
-- every genesis file to be found and decoded.
test_mkProtocolInfoAndBackend :: Assertion
test_mkProtocolInfoAndBackend = do
  (pInfo, backend) <- mkProtocolInfoAndBackend (CardanoBlockArgs configFile Nothing)
  case backend of
    Just V2InMem -> pure ()
    other -> assertFailure $ "expected the in-memory backend, but got " <> describe other
  -- Forces the initial ledger state, and with it the transition config built
  -- from all the genesis files.
  pInfoInitLedger pInfo `seq` pure ()

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

backendOf :: Cfg.NodeConfiguration -> IO (Maybe LedgerDBBackend)
backendOf nc =
  case mkLedgerDBBackend (Cfg.storageConfiguration nc) of
    Left err -> assertFailure $ "expected a backend, but got: " <> err
    Right backend -> pure backend

-- | 'LedgerDBBackend' has no 'Show' instance, so name it for assertion messages.
describe :: Maybe LedgerDBBackend -> String
describe = \case
  Nothing -> "no backend at all"
  Just V2InMem -> "the in-memory backend"
  Just (V2LSM opts) -> "the LSM backend at " <> show (lsmDatabasePath opts)

triggerEpochs :: CardanoHardForkTriggers -> [Maybe EpochNo]
triggerEpochs = hcollapse . hmap (K . epochOf) . getCardanoHardForkTriggers
 where
  epochOf :: CardanoHardForkTrigger blk -> Maybe EpochNo
  epochOf = \case
    CardanoTriggerHardForkAtDefaultVersion -> Nothing
    CardanoTriggerHardForkAtEpoch epoch -> Just epoch
