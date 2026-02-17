{-# LANGUAGE ApplicativeDo #-}
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
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import System.Exit
import System.FSNotify
import System.FilePath

data Config
  = -- | Run in daemon mode
    DaemonConfig
      -- | Where the input snapshot will be in
      SnapshotsDirectoryWithFormat
      -- | Where to put the converted snapshots
      SnapshotsDirectory
  | -- | Run in normal mode
    NoDaemonConfig
      -- | Where and in which format the input snapshot is in
      Snapshot'
      -- | Where and in which format the output snapshot must be in
      Snapshot'

data Snapshot'
  = StandaloneSnapshot' FilePath StandaloneFormat
  | LSMSnapshot' FilePath LSMDatabaseFilePath

snapshot'ToSnapshot :: Snapshot' -> Snapshot
snapshot'ToSnapshot (LSMSnapshot' s lsmfp) =
  let (snapFp, snapName) = splitFileName s
   in case snapshotFromPath snapName of
        Just snap -> Snapshot (LSMSnapshot (SnapshotsDirectory snapFp) lsmfp) snap
        Nothing -> error $ "Malformed input, last fragment of the input \"" <> s <> "\"is not a snapshot name"
snapshot'ToSnapshot (StandaloneSnapshot' s fmt) =
  let (snapFp, snapName) = splitFileName s
   in case snapshotFromPath snapName of
        Just snap -> Snapshot (StandaloneSnapshot (SnapshotsDirectory snapFp) fmt) snap
        Nothing -> error $ "Malformed input, last fragment of the input \"" <> s <> "\"is not a snapshot name"

main :: IO ()
main = withStdTerminalHandles $ do
  cryptoInit
  (conf, args) <- getCommandLineConfig
  pInfo <- mkProtocolInfo args
  case conf of
    NoDaemonConfig (snapshot'ToSnapshot -> f) (snapshot'ToSnapshot -> t) -> do
      eRes <- runExceptT (convertSnapshot True pInfo f t)
      case eRes of
        Left err -> do
          putStrLn $ show err
          exitFailure
        Right () -> exitSuccess
    DaemonConfig from@(getSnapshotDir . snapshotDirectory -> ledgerDbPath) to -> do
      withManager $ \manager -> do
        putStrLn $ "Watching " <> show ledgerDbPath
        void $
          watchTree
            manager
            ledgerDbPath
            ( \case
                CloseWrite ep _ IsFile -> "meta" `L.isSuffixOf` ep
                _ -> False
            )
            ( \case
                CloseWrite ep _ IsFile -> do
                  case reverse $ splitDirectories ep of
                    (_ : snapName@(snapshotFromPath -> Just ds) : _) -> do
                      putStrLn $ "Converting snapshot " <> ep <> " to " <> (getSnapshotDir to </> snapName)
                      res <-
                        runExceptT $
                          convertSnapshot
                            False
                            pInfo
                            (Snapshot from ds)
                            (Snapshot (StandaloneSnapshot to Mem) ds)
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
          <$> parseConfig
          <*> parseCardanoArgs <**> helper
      )
      ( fullDesc
          <> header "Utility for converting snapshots among the different snapshot formats used by cardano-node."
          <> progDescDoc programDescription
      )

parseConfig :: Parser Config
parseConfig =
  ( const DaemonConfig
      <$> switch (long "daemon" <> short 'd')
      <*> ( ( LSMSnapshot
                <$> (SnapshotsDirectory <$> strOption (long "monitor" <> short 'm'))
                <*> (LSMDatabaseFilePath <$> strOption (long "lsm-database"))
            )
              <|> ( StandaloneSnapshot
                      <$> (SnapshotsDirectory <$> strOption (long "monitor" <> short 'm'))
                      <*> pure LMDB
                  )
          )
      <*> (SnapshotsDirectory <$> strOption (long "output" <> short 'o'))
  )
    <|> ( NoDaemonConfig
            <$> ( ( LSMSnapshot'
                      <$> (strOption (long "lsm-snapshot-in"))
                      <*> (LSMDatabaseFilePath <$> strOption (long "lsm-database-in"))
                  )
                    <|> ( StandaloneSnapshot'
                            <$> strOption (long "mem-in")
                            <*> pure Mem
                        )
                    <|> ( StandaloneSnapshot'
                            <$> strOption (long "lmdb-in")
                            <*> pure LMDB
                        )
                )
            <*> ( ( LSMSnapshot'
                      <$> (strOption (long "lsm-snapshot-out"))
                      <*> (LSMDatabaseFilePath <$> strOption (long "lsm-database-out"))
                  )
                    <|> ( StandaloneSnapshot'
                            <$> strOption (long "mem-out")
                            <*> pure Mem
                        )
                    <|> ( StandaloneSnapshot'
                            <$> strOption (long "lmdb-out")
                            <*> pure LMDB
                        )
                )
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
