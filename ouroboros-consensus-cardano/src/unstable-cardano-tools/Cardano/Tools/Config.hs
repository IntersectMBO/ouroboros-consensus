{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | How the tools in this package handle the node configuration.
--
-- Parsing the configuration file and loading the genesis files it names is
-- @cardano-config@'s job. What is left here is what a tool still has to derive
-- from a resolved configuration in order to build a Cardano 'ProtocolInfo': the
-- hard-fork triggers, the initial nonce and the ledger transition
-- configuration. db-analyser and db-synthesizer both go through this module, so
-- that the two interpret the same configuration file the same way.
module Cardano.Tools.Config
  ( -- * Reading the node configuration
    resolveNodeConfiguration
  , resolveNodeConfigurationWith
  , reportConfigWarnings

    -- * Interpreting the node configuration
  , mkHardForkTriggers
  , mkInitialNonce
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
import Cardano.Ledger.BaseTypes (Nonce (..), boundRational, unsafeNonZero)
import Cardano.Ledger.Dijkstra.PParams
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Exception (Exception (..), handle, throwIO, try)
import Data.Functor.Identity (Identity)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Maybe.Strict (StrictMaybe, strictMaybe, strictMaybeToMaybe)
import Data.Word (Word64)
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
-- Wrap a tool's @main@ in this. Without it, GHC's default handler would bury the
-- message under the exception's fully qualified type name and a call-stack
-- backtrace, neither of which helps someone who simply mistyped a configuration
-- key.
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
  -- cardano-config reports some problems -- a missing mandatory key, notably --
  -- by throwing rather than in its result, so catch those too; letting them
  -- escape would present the user with a call-stack backtrace for what is just a
  -- mistake in their configuration file.
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
-- They are only warnings, so a tool carries on afterwards; but they say that
-- some of what the file asks for was not honoured, which is worth seeing before
-- the tool's own output starts.
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
    (strictMaybe emptyDijkstraGenesis id (Cfg.experimentalGenesisConfig nc))

-- | An empty Dijkstra genesis to be provided when none is specified in the config.
emptyDijkstraGenesis :: SL.DijkstraGenesis
emptyDijkstraGenesis =
  let upgradePParamsDef =
        UpgradeDijkstraPParams
          { udppMaxRefScriptSizePerBlock = 1048576
          , udppMaxRefScriptSizePerTx = 204800
          , udppRefScriptCostStride = unsafeNonZero 25600
          , udppRefScriptCostMultiplier = fromMaybe (error "impossible") $ boundRational 1.2
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
