{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | How the tools in this package handle the node configuration.
--
-- Parsing it and loading the genesis files it names is @cardano-config@'s job.
-- Left for here is what building a Cardano 'ProtocolInfo' still needs derived
-- from the result: the hard-fork triggers, the initial nonce, the protocol
-- version and the ledger transition configuration. Both db-analyser and
-- db-synthesizer go through this module, so the two read a configuration file
-- the same way.
module Cardano.Tools.Config
  ( -- * Reading the node configuration
    resolveNodeConfiguration
  , resolveNodeConfigurationWith
  , reportConfigWarnings

    -- * Interpreting the node configuration
  , mkHardForkTriggers
  , mkDijkstraGenesis
  , mkInitialNonce
  , mkProtocolVersion
  , mkTransitionConfig

    -- * Configuration errors
  , ConfigError (..)
  , throwConfigError
  , withConfigErrorHandling
  ) where

import qualified Cardano.Configuration as Cfg
import qualified Cardano.Configuration.CliArgs as CLI
import qualified Cardano.Crypto.Hash.Class as CryptoClass
import qualified Cardano.Ledger.Api.Era as L
import qualified Cardano.Ledger.Api.Transition as SL
import Cardano.Ledger.BaseTypes
  ( Milliseconds32 (..)
  , Nonce (..)
  , ProtVer (..)
  , boundRational
  , unsafeNonZero
  )
import Cardano.Ledger.Core (MaxPledgeLeverage (..))
import Cardano.Ledger.Dijkstra.PParams
import Cardano.Ledger.Plutus
  ( ExUnits (..)
  , Language (..)
  , OrdExUnits (..)
  , costModelInitParamCount
  , mkCostModel
  )
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Exception (Exception (..), handle, throwIO, try)
import Data.Either (fromRight)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Maybe.Strict (StrictMaybe (..), strictMaybe, strictMaybeToMaybe)
import Data.Word (Word16, Word64)
import Ouroboros.Consensus.Cardano.Node
  ( CardanoHardForkTrigger (..)
  , CardanoHardForkTriggers (..)
  )
import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

--
-- Configuration errors
--

-- | A tool was invoked with a configuration it cannot work with, be it in the
-- node configuration file or on the command line.
newtype ConfigError = ConfigError String
  deriving Show

instance Exception ConfigError where
  -- Deliberately just the message: these are user errors, so neither a call
  -- stack nor a @user error (..)@ wrapper would tell the user anything useful.
  displayException (ConfigError msg) = msg

-- | Abort because of a 'ConfigError'.
--
-- Every configuration problem these tools detect is reported this way, so that
-- they all reach the user in the same shape.
throwConfigError :: String -> IO a
throwConfigError = throwIO . ConfigError

-- | Report an escaping 'ConfigError' as a plain @\<prog\>: \<message\>@ on
-- stderr and exit with a failure.
--
-- Wrap a tool's @main@ in this. GHC's default handler would otherwise bury the
-- message under the exception's qualified type name and a call-stack backtrace.
withConfigErrorHandling :: IO a -> IO a
withConfigErrorHandling =
  handle $ \(ConfigError msg) -> do
    prog <- getProgName
    hPutStrLn stderr $ prog <> ": " <> msg
    exitFailure

--
-- Reading the node configuration
--

-- | Parse and resolve the node configuration with the shared 'cardano-config'
-- package, with no command-line overrides: every value comes from the
-- configuration file (and cardano-config's own defaults layer).
--
-- The warnings emitted while resolving are returned rather than reported here,
-- so that the caller decides where they are printed.
resolveNodeConfiguration :: FilePath -> IO (Cfg.NodeConfiguration, [Cfg.ConfigWarning])
resolveNodeConfiguration = resolveNodeConfigurationWith . CLI.defaultCliArgs

-- | As 'resolveNodeConfiguration', but resolving the configuration file named
-- by the given command-line arguments against them, so that a tool that has
-- command-line options of its own (db-synthesizer and its credentials) can
-- override what the file says. 'CLI.defaultCliArgs' is the starting point for
-- building those arguments.
resolveNodeConfigurationWith ::
  CLI.CliArgs -> IO (Cfg.NodeConfiguration, [Cfg.ConfigWarning])
resolveNodeConfigurationWith cliArgs =
  -- cardano-config reports some problems by throwing rather than in its result,
  -- a missing mandatory key notably, so catch those too. Letting them escape
  -- would show a call-stack backtrace for a mistake in a configuration file.
  try resolve >>= \case
    Left (err :: Cfg.ConfigurationParsingError) -> invalid (displayException err)
    Right (Left err) -> invalid (show err)
    Right (Right res) -> pure res
 where
  resolve = do
    (fileCfg, fileWarnings) <- Cfg.parseConfigurationFiles (CLI.configFilePath cliArgs)
    pure $ case Cfg.resolveConfiguration cliArgs fileCfg of
      Left err -> Left err
      Right (nc, checkWarnings) -> Right (nc, fileWarnings <> checkWarnings)

  invalid msg = throwConfigError ("invalid node configuration: " <> msg)

-- | Report the warnings raised while resolving a node configuration on stderr.
--
-- A tool carries on afterwards, but they say that some of what the file asks for
-- was not honoured, which is worth seeing before its own output starts.
reportConfigWarnings :: [Cfg.ConfigWarning] -> IO ()
reportConfigWarnings =
  mapM_ (hPutStrLn stderr . ("WARNING: " <>) . Cfg.renderConfigWarning)

--
-- Interpreting the node configuration
--

-- | The initial nonce, ie the Blake2b-256 hash of the Shelley genesis file,
-- which 'cardano-config' records alongside the file path.
mkInitialNonce :: Cfg.NodeConfiguration -> Nonce
mkInitialNonce nc =
  Nonce $
    CryptoClass.castHash $
      Cfg.hash (Cfg.shelleyGenesis (Cfg.protocolConfiguration nc))

-- | The ledger transition configuration, ie the genesis of every Shelley-based
-- era, as parsed by 'cardano-config'.
mkTransitionConfig :: Cfg.NodeConfiguration -> SL.TransitionConfig L.LatestKnownEra
mkTransitionConfig nc =
  SL.mkLatestTransitionConfig
    (Cfg.shelleyGenesisConfig nc)
    (Cfg.alonzoGenesisConfig nc)
    (Cfg.conwayGenesisConfig nc)
    (mkDijkstraGenesis nc)

-- | The greatest protocol version the tools forge in and validate.
--
-- As in cardano-node, @ExperimentalHardForksEnabled@ decides it: the flag admits
-- the experimental era, so with it off the version stops at the era before.
--
-- The eras are named rather than written as the 12 and 11 they currently are, so
-- that adding an era moves both.
mkProtocolVersion :: Cfg.NodeConfiguration -> ProtVer
mkProtocolVersion nc
  | experimentalErasEnabled nc = ProtVer (L.eraProtVerHigh @L.LatestKnownEra) 0
  | otherwise = ProtVer (L.eraProtVerHigh @(L.PreviousEra L.LatestKnownEra)) 0

-- | The Dijkstra genesis to build the transition configuration from.
--
-- A transition configuration needs one for every Shelley-based era, including
-- eras a chain never reaches, but a configuration need not name a
-- @DijkstraGenesisFile@. @cardano-config@ reports what the file says without
-- inventing values, so both the fallback and when to use it are ours.
--
-- Both follow cardano-node, which gates this on @ExperimentalHardForksEnabled@:
-- with the flag off it uses its own fallback and does not read a named file at
-- all. The flag is what admits the experimental era, so with it off the era's
-- genesis cannot be in play whatever the file says, and a tool that read it
-- anyway would hand the ledger a genesis the node never had.
--
-- Note that @cardano-config@ still reads and hash-checks a named file whatever
-- the flag says, so a wrong @DijkstraGenesisHash@ is an error even when the
-- genesis it names goes unused.
mkDijkstraGenesis :: Cfg.NodeConfiguration -> SL.DijkstraGenesis
mkDijkstraGenesis nc
  | experimentalErasEnabled nc =
      strictMaybe defaultDijkstraGenesis id (Cfg.experimentalGenesisConfig nc)
  | otherwise = defaultDijkstraGenesis

-- | Whether the configuration admits the experimental era.
experimentalErasEnabled :: Cfg.NodeConfiguration -> Bool
experimentalErasEnabled =
  runIdentity . Cfg.experimentalHardForksEnabled . Cfg.testingConfiguration

-- | Inherited from cardano-node's @Cardano.Node.Protocol.Dijkstra@, by way of
-- the copy of that module these tools used to vendor, plus the two fields a
-- later @cardano-ledger@ added.
defaultDijkstraGenesis :: SL.DijkstraGenesis
defaultDijkstraGenesis =
  let upgradePParamsDef =
        UpgradeDijkstraPParams
          { udppMaxRefScriptSizePerBlock = 1048576
          , udppMaxRefScriptSizePerTx = 204800
          , udppRefScriptCostStride = unsafeNonZero 25600
          , udppRefScriptCostMultiplier = fromMaybe (error "impossible") $ boundRational 1.2
          , udppMaxPledgeLeverage = MaxPledgeLeverage SNothing
          , udppMinPoolMargin = fromMaybe (error "impossible") $ boundRational 0.015
          , udppPlutusV4CostModel =
              fromRight (error "impossible") $
                mkCostModel PlutusV4 (replicate (costModelInitParamCount PlutusV4) 0)
          , udppLeiosAnnouncementPeriodLength = Milliseconds32 1_000
          , udppLeiosVotePeriodLength = Milliseconds32 4_000
          , udppLeiosDiffusionPeriodLength = Milliseconds32 7_000
          , udppLeiosCommitteeSize = 900 :: Word16
          , udppLeiosQuorumStakeThreshold = fromMaybe (error "impossible") $ boundRational 0.75
          , udppMaxEndorserBlockReferencesSize = 512 * 1024
          , udppMaxEndorserBlockTxsSize = 12 * 1024 * 1024
          , udppMaxEndorserBlockExUnits = OrdExUnits $ ExUnits 7_000_000_000 2_000_000_000_000
          , udppMaxRefScriptSizePerEndorserBlock = 12 * 1024 * 1024
          }
   in SL.DijkstraGenesis{SL.dgUpgradePParams = upgradePParamsDef}

-- | Build the 'CardanoHardForkTriggers' from the @Testing@ section of the
-- configuration: each era hard-forks at its configured epoch, or at the
-- default protocol version when no epoch is given.
--
-- If an era is configured to hard-fork at a specific epoch, then so must all
-- earlier eras; otherwise the configuration is rejected.
mkHardForkTriggers ::
  Cfg.TestingConfiguration Identity -> Either String CardanoHardForkTriggers
mkHardForkTriggers testCfg
  | any (\(earlier, later) -> isNothing earlier && isJust later) (zip epochs (drop 1 epochs)) =
      Left
        "if the Cardano config file sets a Test*HardForkAtEpoch, it must also set it for all previous eras."
  | otherwise =
      Right
        CardanoHardForkTriggers'
          { triggerHardForkShelley = toTrigger (epochOf Cfg.testShelleyHardForkAtEpoch)
          , triggerHardForkAllegra = toTrigger (epochOf Cfg.testAllegraHardForkAtEpoch)
          , triggerHardForkMary = toTrigger (epochOf Cfg.testMaryHardForkAtEpoch)
          , triggerHardForkAlonzo = toTrigger (epochOf Cfg.testAlonzoHardForkAtEpoch)
          , triggerHardForkBabbage = toTrigger (epochOf Cfg.testBabbageHardForkAtEpoch)
          , triggerHardForkConway = toTrigger (epochOf Cfg.testConwayHardForkAtEpoch)
          , triggerHardForkDijkstra = toTrigger (epochOf Cfg.testDijkstraHardForkAtEpoch)
          }
 where
  -- cardano-config records the configured epochs as 'StrictMaybe'; the tools
  -- work with plain 'Maybe' here.
  epochOf ::
    (Cfg.TestingConfiguration Identity -> StrictMaybe Word64) -> Maybe Word64
  epochOf f = strictMaybeToMaybe (f testCfg)

  -- In Shelley-era order; mirrors the field order of 'CardanoHardForkTriggers''.
  epochs =
    [ epochOf Cfg.testShelleyHardForkAtEpoch
    , epochOf Cfg.testAllegraHardForkAtEpoch
    , epochOf Cfg.testMaryHardForkAtEpoch
    , epochOf Cfg.testAlonzoHardForkAtEpoch
    , epochOf Cfg.testBabbageHardForkAtEpoch
    , epochOf Cfg.testConwayHardForkAtEpoch
    , epochOf Cfg.testDijkstraHardForkAtEpoch
    ]

  toTrigger :: Maybe Word64 -> CardanoHardForkTrigger blk
  toTrigger =
    maybe
      CardanoTriggerHardForkAtDefaultVersion
      (CardanoTriggerHardForkAtEpoch . EpochNo)
