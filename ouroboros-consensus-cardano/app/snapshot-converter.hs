{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import Cardano.Tools.DBAnalyser.Block.Cardano (configFile)
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import Control.Concurrent
import Control.Exception
import Control.Monad (forever, void)
import Control.Monad.Except
import DBAnalyser.Parsers
import qualified Data.Aeson as Aeson
import qualified Data.List as L
import qualified Data.Text as Text
import qualified Debug.Trace as Debug
import GHC.TypeLits (Symbol)
import Main.Utf8
import Options.Applicative
import Options.Applicative.Help (Doc, line)
import Ouroboros.Consensus.Cardano.SnapshotConversion
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import System.Exit
import System.FSNotify
import System.FilePath

-- | FilePaths annotated with the purpose
newtype FilePath' (s :: Symbol) = FP {rawFilePath :: FilePath}

instance Show (FilePath' s) where
  show = rawFilePath

data Config
  = -- | Run in daemon mode, with an output directory
    DaemonConfig (FilePath' "OutDir")
  | -- | Run in normal mode
    NoDaemonConfig
      -- | Which format the input snapshot is in
      Format
      -- | Which format the output snapshot must be in
      Format

-- | Information inferred from the config file
data FromConfigFile
  = FromConfigFile
      -- | The input format once supplied with a particular snapshot
      (FilePath' "Snapshot" -> Format)
      -- | The directory of snapshots, to be watched by inotify
      (FilePath' "Snapshots")

main :: IO ()
main = withStdTerminalHandles $ do
  cryptoInit
  (conf, args) <- getCommandLineConfig
  pInfo <- mkProtocolInfo args
  FromConfigFile inFormat ledgerDbPath <- getFormatFromConfig (FP $ configFile args)
  case conf of
    NoDaemonConfig f t -> do
      eRes <- runExceptT (convertSnapshot True pInfo f t)
      case eRes of
        Left err -> do
          putStrLn $ show err
          exitFailure
        Right () -> exitSuccess
    DaemonConfig outDir -> do
      withManager $ \manager -> do
        putStrLn $ "Watching " <> show ledgerDbPath
        void $
          watchTree
            manager
            (rawFilePath ledgerDbPath)
            ( \case
                CloseWrite ep _ IsFile -> "meta" `L.isSuffixOf` ep
                _ -> False
            )
            ( \case
                CloseWrite ep _ IsFile -> do
                  case reverse $ splitDirectories ep of
                    (_ : snapName@(snapshotFromPath -> Just{}) : _) -> do
                      putStrLn $ "Converting snapshot " <> ep <> " to " <> (rawFilePath outDir </> snapName)
                      res <-
                        runExceptT $
                          convertSnapshot
                            False
                            pInfo
                            (inFormat (FP @"Snapshot" $ takeDirectory ep))
                            (Mem $ rawFilePath outDir </> snapName)
                      case res of
                        Left err ->
                          putStrLn $ show err
                        Right () ->
                          putStrLn "Done"
                    _ -> pure ()
                _ -> pure ()
            )
        forever $ threadDelay 1000000

{-------------------------------------------------------------------------------
  Optparse-applicative
-------------------------------------------------------------------------------}

getCommandLineConfig :: IO (Config, CardanoBlockArgs)
getCommandLineConfig =
  execParser $
    info
      ( (,)
          <$> ( (NoDaemonConfig <$> parseConfig In <*> parseConfig Out)
                  <|> ( switch (short 'd' <> long "daemon")
                          *> (DaemonConfig . FP <$> parsePath "output-dir" "Directory for outputting snapshots")
                      )
              )
          <*> parseCardanoArgs <**> helper
      )
      ( fullDesc
          <> header "Utility for converting snapshots among the different snapshot formats used by cardano-node."
          <> progDescDoc programDescription
      )

programDescription :: Maybe Doc
programDescription =
  Just $
    mconcat
      [ "The input snapshot must correspond to a snapshot that was produced by "
      , "a cardano-node, and thus follows the naming convention used in the node."
      , line
      , "This means in particular that the filepath to the snapshot must have as "
      , "the last fragment a directory named after the slot number of the ledger "
      , "state snapshotted, plus an optional suffix, joined by an underscore."
      , line
      , line
      , "For the output, the same convention is enforced, so that the produced "
      , "snapshot can be loaded right away by a cardano-node."
      , line
      , line
      , "Note that snapshots that have a suffix will be preserved by the node. "
      , "If you produce a snapshot with a suffix and you start a node with it, "
      , "the node will take as many more snapshots as it is configured to take, "
      , "but it will never delete your snapshot, because it has a suffix on the name."
      , line
      , "Therefore, for the most common use case it is advisable to create a "
      , "snapshot without a suffix, as in:"
      , line
      , line
      , "```"
      , line
      , "$ mkdir out"
      , line
      , "$ snapshot-converter --<fmt>-in <some-path>/<slot> --<fmt>-out out/<slot> --config <path-to-config.json>"
      , line
      , "```"
      ]

data InOut = In | Out

inoutForGroup :: InOut -> String
inoutForGroup In = "Input arguments:"
inoutForGroup Out = "Output arguments:"

inoutForHelp :: InOut -> String -> Bool -> String
inoutForHelp In s b =
  mconcat $
    ("Input " <> s)
      : if b
        then
          [ ". Must be a filepath where the last fragment is named after the "
          , "slot of the snapshotted state plus an optional suffix. Example: `1645330287_suffix`."
          ]
        else []
inoutForHelp Out s b =
  mconcat $
    ("Output " <> s)
      : if b
        then
          [ ". Must be a filepath where the last fragment is named after the "
          , "slot of the snapshotted state plus an optional suffix. Example: `1645330287_suffix`."
          ]
        else []

inoutForCommand :: InOut -> String -> String
inoutForCommand In = (++ "-in")
inoutForCommand Out = (++ "-out")

parseConfig :: InOut -> Parser Format
parseConfig io =
  ( Mem
      <$> parserOptionGroup
        (inoutForGroup io)
        (parsePath (inoutForCommand io "mem") (inoutForHelp io "snapshot dir" True))
  )
    <|> ( LMDB
            <$> parserOptionGroup
              (inoutForGroup io)
              (parsePath (inoutForCommand io "lmdb") (inoutForHelp io "snapshot dir" True))
        )
    <|> ( LSM
            <$> parserOptionGroup
              (inoutForGroup io)
              (parsePath (inoutForCommand io "lsm-snapshot") (inoutForHelp io "snapshot dir" True))
            <*> parserOptionGroup
              (inoutForGroup io)
              (parsePath (inoutForCommand io "lsm-database") (inoutForHelp io "LSM database" False))
        )

parsePath :: String -> String -> Parser FilePath
parsePath optName strHelp =
  strOption
    ( mconcat
        [ long optName
        , help strHelp
        , metavar "PATH"
        ]
    )

{-------------------------------------------------------------------------------
  Parsing the configuration file
-------------------------------------------------------------------------------}

instance Aeson.FromJSON FromConfigFile where
  parseJSON = Aeson.withObject "CardanoConfigFile" $ \o -> do
    DBPaths imm vol <- Aeson.parseJSON (Aeson.Object o)
    inFmt <- ($ vol) <$> Aeson.parseJSON (Aeson.Object o)
    pure $ FromConfigFile inFmt imm

data DBPaths = DBPaths (FilePath' "Snapshots") (FilePath' "Volatile")

-- | Possible database locations:
--
--  (1) Provided as flag: we would need to receive that same flag. For simplicity we will ban this scenario for now, requiring users to put the path in the config file.
--
--  (2) Not provided: we default to "mainnet"
--
--  (3) Provided in the config file:
--
--    (1) One database path: we use that one
--
--        @@
--        "DatabasePath": "some/path"
--        @@
--
--    (2) Multiple databases paths: we use the immutable one
--
--        @@
--        "DatabasePath": {
--          "ImmutableDbPath": "some/path",
--          "VolatileDbPath": "some/other/path",
--        }
--        @@
instance Aeson.FromJSON DBPaths where
  parseJSON = Aeson.withObject "CardanoConfigFile" $ \o -> do
    pncDatabase <- o Aeson..:? "DatabasePath"
    (imm, vol) <- case pncDatabase of
      Nothing -> pure ("mainnet", "mainnet") -- (2)
      Just p@(Aeson.Object{}) ->
        Aeson.withObject
          "NodeDatabasePaths"
          (\o' -> (,) <$> o' Aeson..: "ImmutableDbPath" <*> o' Aeson..: "VolatileDbPath") -- (3.2)
          p
      Just (Aeson.String s) ->
        let
          s' = Text.unpack s
         in
          pure (s', s') -- (3.1)
      _ -> fail "NodeDatabasePaths must be an object or a string"
    pure $ DBPaths (FP $ imm </> "ledger") (FP vol)

-- | Possible formats
--
--  (1) Nothing provided: we use InMemory
--
--  (2) Provided in config file:
--
--    (1) "V2InMem": we use InMemory
--
--        @@
--        "LedgerDB": {
--          "Backend": "V2InMem"
--        }
--        @@
--
--    (2) "V1LMDB": we use LMDB
--
--        @@
--        "LedgerDB": {
--          "Backend": "V1LMDB"
--        }
--        @@
--
--    (3) "V2LSM"
--        LSM database locations:
--        (1) Nothing provided: we use "lsm" in the volatile directory
--
--        @@
--        "LedgerDB": {
--          "Backend": "V2LSM"
--        }
--        @@
--
--        (2) Provided in file: we use that one
--
--        @@
--        "LedgerDB": {
--          "Backend": "V2LSM",
--          "LSMDatabasePath": "some/path"
--        }
--        @@
instance Aeson.FromJSON (FilePath' "Volatile" -> FilePath' "Snapshot" -> Format) where
  parseJSON = Aeson.withObject "CardanoConfigFile" $ \o -> do
    ldb <- Debug.traceShowId <$> o Aeson..:? "LedgerDB"
    case ldb of
      Nothing -> pure $ const $ Mem . rawFilePath -- (1)
      Just ldb' -> do
        bkd <- ldb' Aeson..:? "Backend"
        case bkd :: Maybe String of
          Just "V1LMDB" -> pure $ const $ LMDB . rawFilePath -- (2.2)
          Just "V2LSM" -> do
            mDbPath <- ldb' Aeson..:? "LSMDatabasePath"
            case mDbPath of
              Nothing -> pure $ \v -> flip LSM (rawFilePath v </> "lsm") . rawFilePath -- (2.3.1)
              Just dbPath -> pure $ const $ flip LSM dbPath . rawFilePath -- (2.3.2)
          _ -> pure $ const $ Mem . rawFilePath -- (2.1)

getFormatFromConfig :: FilePath' "ConfigFile" -> IO FromConfigFile
getFormatFromConfig (FP configPath) =
  either (throwIO . userError) pure =<< Aeson.eitherDecodeFileStrict' configPath
