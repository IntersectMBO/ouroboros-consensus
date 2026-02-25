{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
#if __GLASGOW_HASKELL__ >= 912
{-# LANGUAGE MultilineStrings #-}
#endif

module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import Control.Concurrent
import Control.Monad (forever, void)
import Control.Monad.Except
import DBAnalyser.Parsers
import qualified Data.List as L
#if __GLASGOW_HASKELL__ < 912
import qualified Data.Text as T
#endif
import Main.Utf8
import Options.Applicative
import Options.Applicative.Help (Doc)
#if __GLASGOW_HASKELL__ < 912
import Options.Applicative.Help.Pretty (Pretty (pretty))
#endif
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

-- | Helper for parsing a directory that contains both the snapshots directory
-- and the particular snapshot.
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
  ( DaemonConfig
      <$> ( ( LSMSnapshot
                <$> (SnapshotsDirectory <$> strOption (long "monitor-lsm-snapshots-in"))
                <*> (LSMDatabaseFilePath <$> strOption (long "lsm-database"))
            )
              <|> ( StandaloneSnapshot
                      <$> (SnapshotsDirectory <$> strOption (long "monitor-lmdb-snapshots-in"))
                      <*> pure LMDB
                  )
          )
      <*> (SnapshotsDirectory <$> strOption (long "output-mem-snapshots-in"))
  )
    <|> ( NoDaemonConfig
            <$> ( ( LSMSnapshot'
                      <$> (strOption (long "input-lsm-snapshot"))
                      <*> (LSMDatabaseFilePath <$> strOption (long "input-lsm-database"))
                  )
                    <|> ( StandaloneSnapshot'
                            <$> strOption (long "input-mem")
                            <*> pure Mem
                        )
                    <|> ( StandaloneSnapshot'
                            <$> strOption (long "input-lmdb")
                            <*> pure LMDB
                        )
                )
            <*> ( ( LSMSnapshot'
                      <$> (strOption (long "output-lsm-snapshot"))
                      <*> (LSMDatabaseFilePath <$> strOption (long "output-lsm-database"))
                  )
                    <|> ( StandaloneSnapshot'
                            <$> strOption (long "output-mem")
                            <*> pure Mem
                        )
                    <|> ( StandaloneSnapshot'
                            <$> strOption (long "output-lmdb")
                            <*> pure LMDB
                        )
                )
        )

#if __GLASGOW_HASKELL__ >= 912
programDescription :: Maybe Doc
programDescription =
  Just
    """
    # Running in oneshot mode

        `snapshot-converter` can be invoked to convert a single snapshot to a different
        format. The three formats supported at the moment are: Mem, LMDB and LSM.

        As snapshots in Mem and LMDB are fully contained in one directory, providing
        that one is enough. On the other hand, converting an LSM snapshot requires a
        reference to the snapshot directory as well as the LSM database directory.

        To run in oneshot mode, you have to provide input and output parameters as in:
        ```
        # mem to lsm
        $ snapshot-converter --input-mem <PATH> --output-lsm-snapshot <PATH> --output-lsm-database <PATH> --config <PATH>
        # mem to lmdb
        $ snapshot-converter --input-mem <PATH> --output-lmdb <PATH> --config <PATH>

        # lmdb to lsm
        $ snapshot-converter --input-lmdb <PATH> --output-lsm-snapshot <PATH> --output-lsm-database --config <PATH>
        # lmdb to mem
        $ snapshot-converter --input-lmdb <PATH> --output-mem <PATH> --config <PATH>

        # lsm to mem
        $ snapshot-converter --input-lsm-snapshot <PATH> --input-lsm-database <PATH> --output-mem <PATH> --config <PATH>
        # lsm to mem
        $ snapshot-converter --input-lsm-snapshot <PATH> --input-lsm-database <PATH> --output-lmdb <PATH> --config <PATH>
        ```

        Note that the input and output paths need to be named after the slot number
        of the contained ledger state, this means for example that a snapshot for
        slot 100 has to be contained in a directory `100[_suffix]` and has to be
        written to a directory `100[_some_other_suffix]`. Providing a wrong slot
        number will throw an error.

        This naming convention is the same expected by `cardano-node`.

    # Running in daemon mode

        `snapshot-converter` can be invoked as a daemon to monitor and convert
        snapshots produced by a `cardano-node` into Mem format as they are
        written by the node. This is only meaningful to run if your node
        produces LMDB or LSM snapshots:
        ```
        # lsm to mem
        $ snapshot-converter --monitor-lsm-snapshots-in <PATH> --lsm-database <PATH> --output-mem-snapshots-in <PATH> --config <PATH>
        # lmdb to mem
        $ snapshot-converter --monitor-lmdb-snapshots-in <PATH> --output-mem-snapshots-in <PATH> --config <PATH>
        ```
    """
#else
programDescription :: Maybe Doc
programDescription =
  Just
   . pretty
   . T.pack
   $ unlines
     [ "# Running in oneshot mode"
     , ""
     , "    `snapshot-converter` can be invoked to convert a single snapshot to a different"
     , "    format. The three formats supported at the moment are: Mem, LMDB and LSM."
     , ""
     , "    As snapshots in Mem and LMDB are fully contained in one directory, providing"
     , "    that one is enough. On the other hand, converting an LSM snapshot requires a"
     , "    reference to the snapshot directory as well as the LSM database directory."
     , ""
     , "    To run in oneshot mode, you have to provide input and output parameters as in:"
     , "    ```"
     , "    # mem to lsm"
     , "    $ snapshot-converter --input-mem <PATH> --output-lsm-snapshot <PATH> --output-lsm-database <PATH> --config <PATH>"
     , "    # mem to lmdb"
     , "    $ snapshot-converter --input-mem <PATH> --output-lmdb <PATH> --config <PATH>"
     , ""
     , "    # lmdb to lsm"
     , "    $ snapshot-converter --input-lmdb <PATH> --output-lsm-snapshot <PATH> --output-lsm-database --config <PATH>"
     , "    # lmdb to mem"
     , "    $ snapshot-converter --input-lmdb <PATH> --output-mem <PATH> --config <PATH>"
     , ""
     , "    # lsm to mem"
     , "    $ snapshot-converter --input-lsm-snapshot <PATH> --input-lsm-database <PATH> --output-mem <PATH> --config <PATH>"
     , "    # lsm to mem"
     , "    $ snapshot-converter --input-lsm-snapshot <PATH> --input-lsm-database <PATH> --output-lmdb <PATH> --config <PATH>"
     , "    ```"
     , ""
     , "    Note that the input and output paths need to be named after the slot number"
     , "    of the contained ledger state, this means for example that a snapshot for"
     , "    slot 100 has to be contained in a directory `100[_suffix]` and has to be"
     , "    written to a directory `100[_some_other_suffix]`. Providing a wrong slot"
     , "    number will throw an error."
     , ""
     , "    This naming convention is the same expected by `cardano-node`."
     , ""
     , "# Running in daemon mode"
     , ""
     , "    `snapshot-converter` can be invoked as a daemon to monitor and convert"
     , "    snapshots produced by a `cardano-node` into Mem format as they are"
     , "    written by the node. This is only meaningful to run if your node"
     , "    produces LMDB or LSM snapshots:"
     , ""
     , "    ```"
     , "    # lsm to mem"
     , "    $ snapshot-converter --monitor-lsm-snapshots-in <PATH> --lsm-database <PATH> --output-mem-snapshots-in <PATH> --config <PATH>"
     , "    # lmdb to mem"
     , "    $ snapshot-converter --monitor-lmdb-snapshots-in <PATH> --output-mem-snapshots-in <PATH> --config <PATH>"
     , "    ```"
     ]
#endif
