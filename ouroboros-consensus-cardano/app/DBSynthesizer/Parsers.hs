module DBSynthesizer.Parsers
  ( Options (..)
  , parseCommandLine
  ) where

import qualified Cardano.Configuration.CliArgs as CLI
import Cardano.Tools.DBSynthesizer.Types
import Data.Word (Word64)
import Options.Applicative as Opt
import Ouroboros.Consensus.Block.Abstract (SlotNo (..))

-- | What db-synthesizer was asked to do.
data Options = Options
  { configFile :: FilePath
  , chainDBDir :: FilePath
  , credentials :: CLI.Credentials
  , synthOptions :: DBSynthesizerOptions
  }

parseCommandLine :: IO Options
parseCommandLine =
  Opt.customExecParser p opts
 where
  p = Opt.prefs Opt.showHelpOnEmpty
  opts = Opt.info parserCommandLine mempty

parserCommandLine :: Parser Options
parserCommandLine =
  Options
    <$> parseNodeConfigFilePath
    <*> parseChainDBFilePath
    -- The credential flags are cardano-config's own, so that they are spelled
    -- and documented exactly as they are for a node.
    <*> CLI.parseCredentials
    <*> parseDBSynthesizerOptions

parseDBSynthesizerOptions :: Parser DBSynthesizerOptions
parseDBSynthesizerOptions =
  DBSynthesizerOptions
    <$> parseForgeOptions
    <*> parseOpenMode

parseForgeOptions :: Parser ForgeLimit
parseForgeOptions =
  ForgeLimitSlot
    <$> parseSlotLimit
      <|> ForgeLimitBlock
    <$> parseBlockLimit
      <|> ForgeLimitEpoch
    <$> parseEpochLimit

parseChainDBFilePath :: Parser FilePath
parseChainDBFilePath =
  strOption
    ( long "db"
        <> metavar "PATH"
        <> help "Path to the Chain DB"
        <> completer (bashCompleter "directory")
    )

parseNodeConfigFilePath :: Parser FilePath
parseNodeConfigFilePath =
  strOption
    ( long "config"
        <> metavar "FILE"
        <> help "Path to the node's config.json"
        <> completer (bashCompleter "file")
    )

parseSlotLimit :: Parser SlotNo
parseSlotLimit =
  SlotNo
    <$> option
      auto
      ( short 's'
          <> long "slots"
          <> metavar "NUMBER"
          <> help "Amount of slots to process"
      )

parseBlockLimit :: Parser Word64
parseBlockLimit =
  option
    auto
    ( short 'b'
        <> long "blocks"
        <> metavar "NUMBER"
        <> help "Amount of blocks to forge"
    )

parseEpochLimit :: Parser Word64
parseEpochLimit =
  option
    auto
    ( short 'e'
        <> long "epochs"
        <> metavar "NUMBER"
        <> help "Amount of epochs to process"
    )

parseForce :: Parser Bool
parseForce =
  switch
    ( short 'f'
        <> help "Force overwrite an existing Chain DB"
    )

parseAppend :: Parser Bool
parseAppend =
  switch
    ( short 'a'
        <> help "Append to an existing Chain DB"
    )

parseOpenMode :: Parser DBSynthesizerOpenMode
parseOpenMode =
  (parseForce *> pure OpenCreateForce)
    <|> (parseAppend *> pure OpenAppend)
    <|> pure OpenCreate
