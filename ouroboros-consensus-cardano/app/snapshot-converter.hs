{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import Cardano.Tools.GitRev (gitRev)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forever, void)
import Control.Monad.Except (runExceptT)
import DBAnalyser.Parsers (CardanoBlockArgs, parseCardanoArgs)
import qualified Data.List as L
import qualified Data.Text as T
import Data.Version (showVersion)
import Main.Utf8
import Options.Applicative
import Options.Applicative.Help.Pretty (Doc, pretty)
import Ouroboros.Consensus.Cardano.SnapshotConversion
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V2.LSM
  ( lsmDbExportSnapshot
  , lsmDbImportSnapshot
  )
import Paths_ouroboros_consensus (version)
import System.Exit
import System.FSNotify
import System.FilePath (splitDirectories, splitFileName, (</>))
import System.Info (arch, compilerName, compilerVersion, os)

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

-- | The various ways in which the tool can be invoked.
--
-- 'Daemon' and 'Convert' operate purely on /standalone (exported) LSM
-- snapshots/ and Mem snapshots; they never touch a live LSM database, and so
-- carry the 'CardanoBlockArgs' needed to decode ledger states.
--
-- 'LsmExport' and 'LsmImport' operate directly on a (offline) LSM database; no
-- ledger decoding is involved, hence no 'CardanoBlockArgs'.
data Command
  = Daemon DaemonOpts CardanoBlockArgs
  | Convert ConvertOpts CardanoBlockArgs
  | LsmExport LsmDbOpts
  | LsmImport LsmDbOpts

-- | Options for the daemon: watch a directory for completed snapshots and
-- convert each exported LSM snapshot into a Mem snapshot.
data DaemonOpts = DaemonOpts
  { daemonMonitorMetaDir :: FilePath
  -- ^ The directory holding the @state@/@meta@ files of the snapshots produced
  -- by the node (watched for completed snapshots).
  , daemonLsmSnapshotsExportDir :: FilePath
  -- ^ The directory into which the node exports its LSM snapshots. The exported
  -- snapshot for a snapshot named @N@ is expected at @<this>/N@.
  , daemonMemSnapshotDir :: FilePath
  -- ^ The directory into which to write the converted Mem snapshots.
  }

-- | Options for a one-shot conversion between an exported LSM snapshot and a Mem
-- snapshot (in either direction).
data ConvertOpts = ConvertOpts
  { convertSnapshotInDir :: FilePath
  -- ^ The input snapshot (a directory named after the slot, holding at least the
  -- @state@/@meta@ files).
  , convertImportInDir :: Maybe FilePath
  -- ^ If set, the input is an exported LSM snapshot whose tables live at
  -- @<this>/<input snapshot name>@; otherwise the input is a Mem snapshot.
  , convertSnapshotOutDir :: FilePath
  -- ^ The output snapshot (a directory named after the slot).
  , convertExportOutDir :: Maybe FilePath
  -- ^ If set, the output is an exported LSM snapshot whose tables are written to
  -- @<this>/<output snapshot name>@; otherwise the output is a Mem snapshot.
  }

-- | Options for the @lsm export@/@lsm import@ commands.
data LsmDbOpts = LsmDbOpts
  { lsmDbDir :: FilePath
  -- ^ The LSM database (session) directory.
  , lsmRootDir :: FilePath
  -- ^ The export-to (for @export@) or import-from (for @import@) root directory.
  -- The exported snapshot named @N@ lives at @<this>/N@.
  , lsmSnapName :: String
  -- ^ The snapshot name, e.g. @163470034@ or @163470034_my-suffix@.
  }

main :: IO ()
main = withStdTerminalHandles $ do
  cryptoInit
  cmd <- execParser opts
  case cmd of
    Daemon o args -> runDaemon o args
    Convert o args -> runConvert o args
    LsmExport o -> runLsmDb o lsmDbExportSnapshot
    LsmImport o -> runLsmDb o lsmDbImportSnapshot

{-------------------------------------------------------------------------------
  Running the commands
-------------------------------------------------------------------------------}

runConvert :: ConvertOpts -> CardanoBlockArgs -> IO ()
runConvert o args = do
  pInfo <- mkProtocolInfo args
  from <- mkSnapshot (convertSnapshotInDir o) (convertImportInDir o)
  to <- mkSnapshot (convertSnapshotOutDir o) (convertExportOutDir o)
  eRes <- runExceptT (convertSnapshot True pInfo from to)
  case eRes of
    Left err -> putStrLn (show err) >> exitFailure
    Right () -> exitSuccess

runDaemon :: DaemonOpts -> CardanoBlockArgs -> IO ()
runDaemon o args = do
  pInfo <- mkProtocolInfo args
  let monitorDir = daemonMonitorMetaDir o
  withManager $ \manager -> do
    putStrLn $ "Watching " <> show monitorDir
    void $
      watchTree
        manager
        monitorDir
        ( \case
            CloseWrite ep _ IsFile -> "meta" `L.isSuffixOf` ep
            _ -> False
        )
        ( \case
            CloseWrite ep _ IsFile ->
              case reverse $ splitDirectories ep of
                (_ : name@(snapshotFromPath -> Just ds) : _) -> do
                  let exportedDir = daemonLsmSnapshotsExportDir o </> name
                      from =
                        Snapshot
                          ( ExportedLSMSnapshot
                              (SnapshotsDirectory monitorDir)
                              (ExportedSnapshotPath exportedDir)
                          )
                          ds
                      to =
                        Snapshot
                          (StandaloneSnapshot (SnapshotsDirectory (daemonMemSnapshotDir o)) Mem)
                          ds
                  putStrLn $
                    "Converting snapshot " <> ep <> " to " <> (daemonMemSnapshotDir o </> name)
                  res <- runExceptT (convertSnapshot False pInfo from to)
                  case res of
                    Left err -> putStrLn $ show err
                    Right () -> putStrLn "Done"
                _ -> pure ()
            _ -> pure ()
        )
    forever $ threadDelay 1000000

-- | Validate the snapshot name and run an LSM database operation (export or
-- import) on the snapshot directory @<root>/<name>@, reporting the result.
runLsmDb :: LsmDbOpts -> (FilePath -> String -> FilePath -> IO ()) -> IO ()
runLsmDb o op = do
  void $ requireSnapshotName name
  res <- try $ op (lsmDbDir o) name (lsmRootDir o </> name)
  case res of
    Left (e :: SomeException) -> putStrLn (displayException e) >> exitFailure
    Right () -> putStrLn "Done" >> exitSuccess
 where
  name = lsmSnapName o

-- | Parse a snapshot name, exiting with a helpful message if it is malformed.
requireSnapshotName :: String -> IO DiskSnapshot
requireSnapshotName name =
  case snapshotFromPath name of
    Just ds -> pure ds
    Nothing -> do
      putStrLn $
        "\""
          <> name
          <> "\" is not a valid snapshot name. It should be named after the slot"
          <> " number of the contained state and an optional suffix, such as"
          <> " `163470034` or `163470034_my-suffix`."
      exitFailure

-- | Interpret a snapshot path plus an optional exported-LSM root directory as a
-- 'Snapshot'. When the root is given, the snapshot is an exported LSM snapshot
-- whose tables live at @<root>/<snapshot name>@; otherwise it is a Mem snapshot.
mkSnapshot :: FilePath -> Maybe FilePath -> IO Snapshot
mkSnapshot snapPath mExportRoot = do
  let (parent, name) = splitFileName snapPath
  ds <- requireSnapshotName name
  pure $
    Snapshot
      ( case mExportRoot of
          Just root ->
            ExportedLSMSnapshot
              (SnapshotsDirectory parent)
              (ExportedSnapshotPath (root </> name))
          Nothing -> StandaloneSnapshot (SnapshotsDirectory parent) Mem
      )
      ds

{-------------------------------------------------------------------------------
  Optparse-applicative
-------------------------------------------------------------------------------}

opts :: ParserInfo Command
opts =
  info
    (commandParser <**> versionOption <**> helper)
    ( fullDesc
        <> header
          "Utility for managing and converting the ledger snapshots used by cardano-node."
        <> progDesc
          ( "Conversions operate on Mem snapshots and standalone (exported) LSM"
              <> " snapshots, never on live LSM databases. Use the `lsm` commands to"
              <> " export snapshots out of, or import snapshots into, an offline LSM"
              <> " database."
          )
        <> footerDoc (Just examplesFooter)
    )

-- | Report the tool version. The snapshot on-disk format is tied to the
-- ouroboros-consensus code, so we report the ouroboros-consensus package
-- version together with the exact git commit this tool was built from; that
-- pair identifies which builds a given snapshot is compatible with. We also
-- report the build platform and compiler, following the @cardano-cli
-- --version@ format.
versionOption :: Parser (a -> a)
versionOption =
  infoOption
    versionString
    ( long "version"
        <> help "Show version, build platform and git commit, then exit."
    )

versionString :: String
versionString =
  "snapshot-converter, part of ouroboros-consensus "
    <> showVersion version
    <> " - "
    <> os
    <> "-"
    <> arch
    <> " - "
    <> compilerName
    <> "-"
    <> showVersion compilerVersion
    <> "\ngit rev "
    <> T.unpack gitRev

-- | Worked examples covering the expected flows, shown at the bottom of the
-- top-level @--help@ output. The @convert@ examples require @--config@; the
-- @lsm@ examples operate directly on an offline database and do not.
examplesFooter :: Doc
examplesFooter =
  pretty $
    L.intercalate
      "\n"
      [ "Typical flows (a snapshot is a directory named after its slot, e.g. `100`"
      , "or `100_my-suffix`):"
      , ""
      , "  # Mem snapshot -> standalone (exported) LSM snapshot"
      , "  snapshot-converter convert --config CONFIG \\"
      , "    --snapshot-in  SNAPSHOTS/100 \\"
      , "    --snapshot-out OUT/100 \\"
      , "    --lsm-export-to EXPORTED"
      , ""
      , "  # standalone (exported) LSM snapshot -> Mem snapshot"
      , "  snapshot-converter convert --config CONFIG \\"
      , "    --snapshot-in    SNAPSHOTS/100 \\"
      , "    --lsm-import-from EXPORTED \\"
      , "    --snapshot-out   OUT/100"
      , ""
      , "  # import a standalone (exported) LSM snapshot into a new offline LSM database"
      , "  snapshot-converter lsm import \\"
      , "    --lsm-database    LSM_DB \\"
      , "    --lsm-import-from EXPORTED \\"
      , "    --snapshot        100"
      , ""
      , "  # export a snapshot out of an offline LSM database"
      , "  snapshot-converter lsm export \\"
      , "    --lsm-database  LSM_DB \\"
      , "    --lsm-export-to EXPORTED \\"
      , "    --snapshot      100"
      ]

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "daemon"
        ( info
            daemonCmd
            ( progDesc $
                "Watch a directory for completed snapshots and convert each exported"
                  <> " LSM snapshot into a Mem snapshot as it is produced. Meaningful"
                  <> " only for a node producing LSM snapshots."
            )
        )
        <> command
          "convert"
          ( info
              convertCmd
              ( progDesc $
                  "Convert a single snapshot between an exported LSM snapshot and a Mem"
                    <> " snapshot. The input/output paths must be named after the slot"
                    <> " number of the contained state (e.g. `100` or `100_my-suffix`)."
              )
          )
        <> command
          "lsm"
          ( info
              lsmCmd
              (progDesc "Export snapshots out of / import snapshots into an offline LSM database.")
          )
    )

daemonCmd :: Parser Command
daemonCmd =
  Daemon
    <$> ( DaemonOpts
            <$> strOption
              ( long "monitor-snapshots-in"
                  <> metavar "DIR"
                  <> help "Directory with the node's snapshots (state/meta), watched for completion."
              )
            <*> strOption
              ( long "lsm-exported-path"
                  <> metavar "DIR"
                  <> help "Directory into which the node exports its LSM snapshots."
              )
            <*> strOption
              ( long "output-snapshots-in"
                  <> metavar "DIR"
                  <> help "Directory into which to write the converted Mem snapshots."
              )
        )
    <*> parseCardanoArgs

convertCmd :: Parser Command
convertCmd =
  Convert
    <$> ( ConvertOpts
            <$> strOption
              ( long "snapshot-in"
                  <> metavar "PATH"
                  <> help "The input snapshot (directory named after the slot)."
              )
            <*> optional
              ( strOption
                  ( long "lsm-import-from"
                      <> metavar "DIR"
                      <> help "If set, the input is an exported LSM snapshot rooted here."
                  )
              )
            <*> strOption
              ( long "snapshot-out"
                  <> metavar "PATH"
                  <> help "The output snapshot (directory named after the slot)."
              )
            <*> optional
              ( strOption
                  ( long "lsm-export-to"
                      <> metavar "DIR"
                      <> help "If set, the output is an exported LSM snapshot rooted here."
                  )
              )
        )
    <*> parseCardanoArgs

lsmCmd :: Parser Command
lsmCmd =
  hsubparser
    ( command
        "export"
        ( info
            ( LsmExport
                <$> lsmDbOpts "lsm-export-to" "Root directory to export the snapshot into (as <DIR>/<snapshot>)."
            )
            (progDesc "Export a snapshot out of an offline LSM database into a standalone directory.")
        )
        <> command
          "import"
          ( info
              ( LsmImport
                  <$> lsmDbOpts "lsm-import-from" "Root directory holding the exported snapshot (at <DIR>/<snapshot>)."
              )
              ( progDesc
                  "Import an exported snapshot into a new (offline) LSM database."
              )
          )
    )

lsmDbOpts :: String -> String -> Parser LsmDbOpts
lsmDbOpts dirFlag dirHelp =
  LsmDbOpts
    <$> strOption
      ( long "lsm-database"
          <> metavar "DIR"
          <> help "The LSM database (session) directory."
      )
    <*> strOption
      ( long dirFlag
          <> metavar "DIR"
          <> help dirHelp
      )
    <*> strOption
      ( long "snapshot"
          <> metavar "NAME"
          <> help "The snapshot name, e.g. 163470034 or 163470034_my-suffix."
      )
