{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module DBSynthesizer.Parsers
  ( TxGenSource (..)
  , parseCommandLine
  ) where

import Cardano.Tools.DBSynthesizer.Types
import Data.Word (Word64)
import Options.Applicative as Opt
import Ouroboros.Consensus.Block.Abstract (SlotNo (..))
import System.Exit (die)

parseCommandLine ::
  IO (NodeFilePaths, NodeCredentials, DBSynthesizerOptions, TxGenSource)
parseCommandLine = do
  (paths, creds, opts, flags) <- Opt.customExecParser p info'
  (,,,) paths creds opts <$> resolveTxGen flags
 where
  p = Opt.prefs Opt.showHelpOnEmpty
  info' = Opt.info parserCommandLine mempty

parserCommandLine ::
  Parser (NodeFilePaths, NodeCredentials, DBSynthesizerOptions, TxGenFlags)
parserCommandLine =
  (,,,)
    <$> parseNodeFilePaths
    <*> parseNodeCredentials
    <*> parseDBSynthesizerOptions
    <*> parseTxGenFlags

-- | Which generator to run, carrying what that generator needs.
--
-- @--tx-generator file@ without @--tx-file@ does not survive
-- 'parseCommandLine', so every value of this type names a generator that can
-- actually run and the caller has nothing left to check.
data TxGenSource
  = -- | 'Cardano.Tools.DBSynthesizer.TxGen.Respend.mkRespendTxGen': a chain of
    -- 1-in\/1-out transactions, each spending the output the one before it
    -- made. It takes its payment key from @--payment-signing-key@, and forges
    -- empty blocks without one.
    FromRespend
  | -- | 'Cardano.Tools.DBSynthesizer.TxGen.File.mkFileTxGen': replays the
    -- stream of transactions this file holds. The file has to be complete
    -- before the run starts, and if the chain it is replayed onto announces
    -- endorser blocks, the forger's votes have to certify them.
    FromFile !FilePath

-- | The flags as given, before 'resolveTxGen' rules out the combination that
-- names no runnable generator.
data TxGenFlags = TxGenFlags
  { tgfGenerator :: !TxGenName
  , tgfTxFile :: !(Maybe FilePath)
  }

-- | The name @--tx-generator@ takes.
data TxGenName = NameRespend | NameFile

parseTxGenFlags :: Parser TxGenFlags
parseTxGenFlags =
  TxGenFlags
    <$> parseTxGenName
    <*> optional parseTxFile

resolveTxGen :: TxGenFlags -> IO TxGenSource
resolveTxGen TxGenFlags{tgfGenerator, tgfTxFile} =
  case tgfGenerator of
    NameRespend -> pure FromRespend
    NameFile -> case tgfTxFile of
      Just path -> pure (FromFile path)
      Nothing ->
        die
          "db-synthesizer: --tx-generator file needs --tx-file, the transaction \
          \file to replay."

parseTxFile :: Parser FilePath
parseTxFile =
  strOption
    ( long "tx-file"
        <> metavar "FILE"
        <> help
          "Path of the transaction file that --tx-generator file replays. Ignored by the other generators."
        <> completer (bashCompleter "file")
    )

parseNodeFilePaths :: Parser NodeFilePaths
parseNodeFilePaths =
  NodeFilePaths
    <$> parseNodeConfigFilePath
    <*> parseChainDBFilePath
    <*> optional parsePaymentKeyFilePath

parseNodeCredentials :: Parser NodeCredentials
parseNodeCredentials =
  NodeCredentials
    <$> optional parseOperationalCertFilePath
    <*> optional parseVrfKeyFilePath
    <*> optional parseKesKeyFilePath
    <*> optional parseBulkFilePath
    <*> optional parseBlsKeyFilePath

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

parsePaymentKeyFilePath :: Parser FilePath
parsePaymentKeyFilePath =
  strOption
    ( long "payment-signing-key"
        <> metavar "FILE"
        <> help
          "Path to a payment signing key, as cardano-cli writes it. Each forged block spends the output of this key and makes a new one. If you do not give this option, the tool forges empty blocks and announces no endorser block."
        <> completer (bashCompleter "file")
    )

parseBlsKeyFilePath :: Parser FilePath
parseBlsKeyFilePath =
  strOption
    ( long "shelley-bls-key"
        <> metavar "FILE"
        <> help
          "Path to the pool's BLS signing key, as cardano-cli writes it. The tool needs this key to vote for the endorser blocks it announces. Without it the tool casts no vote, so no block certifies an endorser block and the transactions of those blocks never reach the ledger."
        <> completer (bashCompleter "file")
    )

parseOperationalCertFilePath :: Parser FilePath
parseOperationalCertFilePath =
  strOption
    ( long "shelley-operational-certificate"
        <> metavar "FILE"
        <> help "Path to the delegation certificate (in JSON TextEnvelope format)"
        <> completer (bashCompleter "file")
    )

parseKesKeyFilePath :: Parser FilePath
parseKesKeyFilePath =
  strOption
    ( long "shelley-kes-key"
        <> metavar "FILE"
        <> help "Path to the KES signing key (in JSON TextEnvelope format)"
        <> completer (bashCompleter "file")
    )

parseVrfKeyFilePath :: Parser FilePath
parseVrfKeyFilePath =
  strOption
    ( long "shelley-vrf-key"
        <> metavar "FILE"
        <> help "Path to the VRF signing key (in JSON TextEnvelope format)"
        <> completer (bashCompleter "file")
    )

parseBulkFilePath :: Parser FilePath
parseBulkFilePath =
  strOption
    ( long "bulk-credentials-file"
        <> metavar "FILE"
        <> help
          "Path to the bulk credentials file (a JSON file containing an array of arrays containing 3 TextEnvelope objects for the opcert, VRF Signing key, KES signing key)"
        <> completer (bashCompleter "file")
    )

parseTxGenName :: Parser TxGenName
parseTxGenName =
  option
    (eitherReader reader)
    ( long "tx-generator"
        <> metavar "NAME"
        <> value NameRespend
        <> showDefaultWith name
        <> help
          "Which transaction generator fills the blocks. \"respend\" chains 1-in/1-out transactions off a single output, so each one spends what the one before it made; it needs --payment-signing-key. \"file\" replays the transactions in --tx-file, which is written ahead of time so that no transaction spends an output of its own block; it needs a Shelley genesis whose initialFunds set up enough outputs to fill a block and a --shelley-bls-key that certifies the endorser blocks it announces, but no payment key."
    )
 where
  reader = \case
    "respend" -> Right NameRespend
    "file" -> Right NameFile
    other ->
      Left $ "expected \"respend\" or \"file\", not " ++ show other
  name = \case
    NameRespend -> "respend"
    NameFile -> "file"

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
