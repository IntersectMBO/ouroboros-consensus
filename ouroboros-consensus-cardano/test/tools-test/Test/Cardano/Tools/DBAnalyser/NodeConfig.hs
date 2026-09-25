{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified Cardano.Ledger.Api.Era as L
import Cardano.Ledger.BaseTypes (Nonce (..), ProtVer (..))
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import Cardano.Ledger.Dijkstra.PParams (UpgradeDijkstraPParams (..))
import Cardano.Tools.Config
  ( ConfigError (..)
  , mkDijkstraGenesis
  , mkHardForkTriggers
  , mkInitialNonce
  , mkProtocolVersion
  , resolveNodeConfiguration
  )
import Cardano.Tools.DBAnalyser.Block.Cardano
  ( Args (CardanoBlockArgs)
  , mkLedgerDBBackend
  )
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfoAndBackend)
import Cardano.Tools.DBAnalyser.Types
  ( LSMFlags (..)
  , LSMOptions (..)
  , LedgerDBBackend (..)
  , LedgerDBBackendFlags (..)
  , selectLedgerDBBackend
  )
import Control.Exception (try)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.SOP.BasicFunctors (K (..))
import Data.SOP.Strict (hcollapse, hmap)
import Data.Word (Word32)
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
-- Cardano configuration in the current @{ $schema, Version, Configuration }@
-- format, which sets @Test\<era\>HardForkAtEpoch@ for Shelley through Conway and
-- has no @LedgerDB@ section.
configDir :: FilePath
configDir = "ouroboros-consensus-cardano/test/tools-test/disk/config"

configFile :: FilePath
configFile = configDir </> "config.json"

tests :: TestTree
tests =
  testGroup
    "NodeConfig"
    [ testCase "resolves with the expected warnings" test_resolvesWithExpectedWarnings
    , testCase "hard-fork triggers" test_hardForkTriggers
    , testCase "hard-fork triggers reject a gap" test_hardForkTriggersGap
    , testCase "initial nonce hashes the Shelley genesis" test_initialNonce
    , testCase "protocol version stops before the experimental era" test_protocolVersion
    , testCase "protocol version covers the experimental era" test_protocolVersionExperimental
    , testCase "Dijkstra genesis ignores the file when experimental is off" test_dijkstraGenesisDefault
    , testCase "Dijkstra genesis comes from the file when experimental is on" test_dijkstraGenesisFromFile
    , testCase "LedgerDB backend defaults to in-memory" test_ledgerDBBackendDefault
    , testCase "LedgerDB LSM backend" test_ledgerDBBackendLSM
    , testCase "LedgerDB LSM rejects an absolute path" test_ledgerDBBackendLSMAbsolute
    , testCase "--lsm keeps the configured LSM settings" test_lsmFlagKeepsConfig
    , testCase "--lsm-export prefers the configured export path" test_lsmExportFlagConfigured
    , testCase "--lsm over a non-LSM configuration uses the defaults" test_lsmFlagDefaults
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

-- | The sections of the @Configuration@ envelope that these tests patch.
protocolSection, storageSection, testingSection :: [Aeson.Key]
protocolSection = ["Configuration", "ProtocolConfig"]
storageSection = ["Configuration", "StorageConfig"]
testingSection = ["Configuration", "TestingConfig"]

-- | Run an action on a copy of 'configFile' in which the section at the given
-- path has been extended with (or, for a 'Nothing' value, stripped of) the given
-- keys. Missing sections are created.
--
-- The path must lead into the @Configuration@ envelope. A key added at the top
-- level instead would not merely be ignored: it makes @cardano-config@ take the
-- whole file to be in the legacy format and migrate it on the fly, so the test
-- would silently exercise the legacy path rather than the current format.
withPatchedConfig ::
  [Aeson.Key] -> [(Aeson.Key, Maybe Aeson.Value)] -> (FilePath -> IO a) -> IO a
withPatchedConfig section patches k = do
  case section of
    "Configuration" : _ : _ -> pure ()
    _ ->
      assertFailure $
        "patches must target a section under Configuration, not " <> show section
  withSystemTempDirectory "db-analyser-config" $ \tmpDir -> do
    entries <- listDirectory configDir
    mapM_ (\e -> copyFile (configDir </> e) (tmpDir </> e)) entries
    let patchedFile = tmpDir </> takeFileName configFile
    original <-
      Aeson.eitherDecodeFileStrict' patchedFile >>= \case
        Left err -> assertFailure $ "could not re-read the configuration: " <> err
        Right value -> pure value
    patched <- patchAt section original
    BL.writeFile patchedFile (Aeson.encode patched)
    k patchedFile
 where
  patchAt [] (Aeson.Object o) = pure $ Aeson.Object $ foldr applyPatch o patches
  patchAt (key : keys) (Aeson.Object o) = do
    inner <- patchAt keys (fromMaybe (Aeson.Object mempty) (KeyMap.lookup key o))
    pure $ Aeson.Object $ KeyMap.insert key inner o
  patchAt _ _ =
    assertFailure $ "not an object at " <> show section <> " in the configuration"

  applyPatch (key, mbValue) =
    maybe (KeyMap.delete key) (KeyMap.insert key) mbValue

-- | Resolve a configuration that is expected to be in the current format,
-- failing if @cardano-config@ had to migrate it from the legacy format. Every
-- test that resolves successfully goes through this, so none of them can drift
-- onto the legacy path unnoticed.
resolveNewFormat :: FilePath -> IO Cfg.NodeConfiguration
resolveNewFormat file = do
  (nc, warns) <- resolveNodeConfiguration file
  Cfg.MigratedToCurrentFormat `notElem` warns
    @? "the configuration is not in the current format: " <> show warns
  pure nc

-- | Select the LSM backend, with the given @LedgerDB.Backend.LSM@ options.
lsmBackend :: [(Aeson.Key, Aeson.Value)] -> (Aeson.Key, Maybe Aeson.Value)
lsmBackend options =
  ( "LedgerDB"
  , Just $
      Aeson.object
        [ "Backend"
            Aeson..= Aeson.object ["LSM" Aeson..= Aeson.Object (KeyMap.fromList options)]
        ]
  )

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | The whole warning list is pinned, not spot-checked.
--
-- @UnrecognisedKeys@ is the failure mode that matters: a key @cardano-config@
-- does not know is one it silently defaults instead of honouring, which for
-- @Test\<era\>HardForkAtEpoch@ would mean replaying a different era than the
-- file asks for. The configuration carries nothing db-analyser does not read,
-- and is already in the current format, so neither that nor a migration warning
-- should appear.
--
-- The configuration names a @DijkstraGenesisFile@ without enabling the
-- experimental era, which is what lets the two Dijkstra cases below tell the
-- fallback from the file. @cardano-config@ does not open the file then, and
-- does not warn about it either.
test_resolvesWithExpectedWarnings :: Assertion
test_resolvesWithExpectedWarnings = do
  (_nc, warns) <- resolveNodeConfiguration configFile
  warns @?= []

-- | The epochs the configuration sets must survive into the triggers. A silent
-- fallback to 'CardanoTriggerHardForkAtDefaultVersion' would make db-analyser
-- replay a different era than the configuration asks for, so pin the whole list
-- down rather than just spot-checking one era.
test_hardForkTriggers :: Assertion
test_hardForkTriggers = do
  nc <- resolveNewFormat configFile
  case mkHardForkTriggers (Cfg.testingConfiguration nc) of
    Left err -> assertFailure $ "expected triggers, but got: " <> err
    Right triggers ->
      -- Shelley through Conway at epoch 0; Dijkstra is not configured, so it
      -- triggers at its default version.
      triggerEpochs triggers
        @?= [ Just (EpochNo 0)
            , Just (EpochNo 0)
            , Just (EpochNo 0)
            , Just (EpochNo 0)
            , Just (EpochNo 0)
            , Just (EpochNo 0)
            , Nothing
            ]

-- | An era configured to hard-fork at an epoch while an earlier era is not is
-- rejected, and reported as a 'ConfigError' rather than as a crash.
test_hardForkTriggersGap :: Assertion
test_hardForkTriggersGap =
  -- Leaves Shelley and Allegra set, and Alonzo through Conway set, with the gap
  -- at Mary.
  withPatchedConfig testingSection [("TestMaryHardForkAtEpoch", Nothing)] $ \file -> do
    nc <- resolveNewFormat file
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
  nc <- resolveNewFormat configFile
  expected <-
    Nonce . CryptoClass.castHash . CryptoClass.hashWith id
      <$> BS.readFile (configDir </> "shelley-genesis.json")
  mkInitialNonce nc @?= expected

-- | The configuration does not set @ExperimentalHardForksEnabled@, so it takes
-- @cardano-config@'s default of @false@ and the version stops at the era before
-- the experimental one.
test_protocolVersion :: Assertion
test_protocolVersion = do
  nc <- resolveNewFormat configFile
  mkProtocolVersion nc @?= ProtVer (L.eraProtVerHigh @(L.PreviousEra L.LatestKnownEra)) 0

-- | The fixture names a @DijkstraGenesisFile@ without enabling the experimental
-- era, so the fallback is used and not the file. Its file sets
-- @maxRefScriptSizePerTx@ to a value the fallback does not have, which is what
-- tells the two apart.
test_dijkstraGenesisDefault :: Assertion
test_dijkstraGenesisDefault = do
  nc <- resolveNewFormat configFile
  maxRefScriptSizePerTxOf (mkDijkstraGenesis nc) @?= 204800

-- | With the experimental era enabled, the file is what is used.
test_dijkstraGenesisFromFile :: Assertion
test_dijkstraGenesisFromFile =
  withPatchedConfig testingSection [("ExperimentalHardForksEnabled", Just (Aeson.Bool True))] $
    \file -> do
      nc <- resolveNewFormat file
      maxRefScriptSizePerTxOf (mkDijkstraGenesis nc) @?= 123456

-- | The field that tells the fixture's file apart from the fallback.
maxRefScriptSizePerTxOf :: DijkstraGenesis -> Word32
maxRefScriptSizePerTxOf = udppMaxRefScriptSizePerTx . dgUpgradePParams

-- | With the experimental era enabled, the protocol version reaches it.
test_protocolVersionExperimental :: Assertion
test_protocolVersionExperimental =
  withPatchedConfig testingSection [("ExperimentalHardForksEnabled", Just (Aeson.Bool True))] $
    \file -> do
      nc <- resolveNewFormat file
      mkProtocolVersion nc @?= ProtVer (L.eraProtVerHigh @L.LatestKnownEra) 0

-- | This configuration has no @LedgerDB@ section, and @cardano-config@ defaults
-- the backend to the in-memory one, so that is what db-analyser uses when the
-- command line selects no backend.
test_ledgerDBBackendDefault :: Assertion
test_ledgerDBBackendDefault = do
  nc <- resolveNewFormat configFile
  backendOf nc >>= \case
    Just V2InMem -> pure ()
    other -> assertFailure $ "expected the in-memory backend, but got " <> describe other

-- | The LSM paths are taken from the configuration as they are: they are
-- interpreted relative to the ChainDB, not to the configuration file, so unlike
-- the genesis paths they must not be adjusted.
test_ledgerDBBackendLSM :: Assertion
test_ledgerDBBackendLSM =
  withPatchedConfig
    storageSection
    [ lsmBackend
        [ ("DatabasePath", "some-lsm-dir")
        , ("ExportPath", "some-export-dir")
        ]
    ]
    $ \file -> do
      nc <- resolveNewFormat file
      backendOf nc >>= \case
        Just (V2LSM opts) -> do
          lsmDatabasePath opts @?= "some-lsm-dir"
          lsmExportPath opts @?= Just "some-export-dir"
          -- Not configurable via the node configuration file.
          lsmNoDiskCache opts @?= False
        other -> assertFailure $ "expected the LSM backend, but got " <> describe other

-- | @cardano-config@ happily accepts an absolute @DatabasePath@, but
-- db-analyser cannot honour one, because it mounts the path inside the ChainDB.
-- Rejecting it beats silently using a different directory than the one asked
-- for.
test_ledgerDBBackendLSMAbsolute :: Assertion
test_ledgerDBBackendLSMAbsolute =
  withPatchedConfig storageSection [lsmBackend [("DatabasePath", "/absolute/lsm")]] $ \file -> do
    nc <- resolveNewFormat file
    case mkLedgerDBBackend (Cfg.storageConfiguration nc) of
      Right backend ->
        assertFailure $ "expected a rejection, but got " <> describe backend
      Left err -> do
        "DatabasePath" `isInfixOf` err @? "unexpected message: " <> err
        "must be relative" `isInfixOf` err @? "unexpected message: " <> err

-- | The LSM settings of a configuration that selects LSM, for the
-- 'selectLedgerDBBackend' tests.
configuredLSM :: LSMOptions
configuredLSM =
  LSMOptions
    { lsmDatabasePath = "some-lsm-dir"
    , lsmExportPath = Just "some-export-dir"
    , lsmNoDiskCache = False
    }

-- | A bare @--lsm@ must not drop what the configuration sets: in particular a
-- configured @ExportPath@ keeps exporting snapshots.
test_lsmFlagKeepsConfig :: Assertion
test_lsmFlagKeepsConfig =
  case selectLedgerDBBackend (Just (LSMFlag (LSMFlags False True))) (Just (V2LSM configuredLSM)) of
    Just (V2LSM opts) -> do
      lsmDatabasePath opts @?= "some-lsm-dir"
      lsmExportPath opts @?= Just "some-export-dir"
      lsmNoDiskCache opts @?= True
    other -> assertFailure $ "expected the LSM backend, but got " <> describe other

-- | @--lsm-export@ exports into the configured directory, not the default one.
test_lsmExportFlagConfigured :: Assertion
test_lsmExportFlagConfigured =
  case selectLedgerDBBackend (Just (LSMFlag (LSMFlags True False))) (Just (V2LSM configuredLSM)) of
    Just (V2LSM opts) -> lsmExportPath opts @?= Just "some-export-dir"
    other -> assertFailure $ "expected the LSM backend, but got " <> describe other

-- | With no LSM settings to start from, @--lsm@ uses the defaults.
test_lsmFlagDefaults :: Assertion
test_lsmFlagDefaults =
  case selectLedgerDBBackend (Just (LSMFlag (LSMFlags True False))) (Just V2InMem) of
    Just (V2LSM opts) -> do
      lsmDatabasePath opts @?= "lsm"
      lsmExportPath opts @?= Just "lsm-exported"
      lsmNoDiskCache opts @?= False
    other -> assertFailure $ "expected the LSM backend, but got " <> describe other

-- | @cardano-config@ reports a missing mandatory key by throwing, not in its
-- result, so check that such a configuration still reaches the user as a plain
-- 'ConfigError' message rather than as a backtrace.
test_malformedIsConfigError :: Assertion
test_malformedIsConfigError =
  withPatchedConfig protocolSection [("ByronGenesisHash", Nothing)] $ \file ->
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
