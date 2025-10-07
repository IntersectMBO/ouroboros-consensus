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
import Control.Monad (forever, void)
import Control.Monad.Except
import DBAnalyser.Parsers
import qualified Data.List as L
import Main.Utf8
import Options.Applicative
import Options.Applicative.Help (Doc, line)
import Ouroboros.Consensus.Cardano.SnapshotConversion
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import System.Exit
import System.FSNotify
import System.FilePath

data Config
  = -- | Run in daemon mode, with an output directory
    DaemonConfig (FilePath' "OutDir")
  | -- | Run in normal mode
    NoDaemonConfig
      -- | Which format the input snapshot is in
      Format
      -- | Which format the output snapshot must be in
      Format

main :: IO ()
main = withStdTerminalHandles $ do
  cryptoInit
  (conf, args) <- getCommandLineConfig
  pInfo <- configCodec . pInfoConfig <$> mkProtocolInfo args
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
