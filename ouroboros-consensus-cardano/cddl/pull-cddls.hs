{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
-- |

module Main (main) where

import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BL8
import           Data.Maybe (isNothing)
import qualified Data.List as L
import           Paths_ouroboros_consensus_cardano
import qualified System.Directory as D
import qualified System.Environment as E
import           System.Exit (exitFailure)
import qualified System.FilePath as F
import qualified System.Process.ByteString.Lazy as P
import qualified Test.Cardano.Chain.Binary.Cddl as Byron
import qualified Test.Cardano.Ledger.Allegra.Binary.Cddl as Allegra
import qualified Test.Cardano.Ledger.Alonzo.Binary.Cddl as Alonzo
import qualified Test.Cardano.Ledger.Babbage.Binary.Cddl as Babbage
import qualified Test.Cardano.Ledger.Conway.Binary.Cddl as Conway
import qualified Test.Cardano.Ledger.Mary.Binary.Cddl as Mary
import qualified Test.Cardano.Ledger.Shelley.Binary.Cddl as Shelley

main :: IO ()
main = do
  probeTools
  setupEnv
  -- For now I just print this.
  print =<< getCDDLs

setupEnv :: IO ()
setupEnv = do
  byron   <- map takePath <$> Byron.readByronCddlFileNames
  shelley <- map takePath <$> Shelley.readShelleyCddlFileNames
  allegra <- map takePath <$> Allegra.readAllegraCddlFileNames
  mary    <- map takePath <$> Mary.readMaryCddlFileNames
  alonzo  <- map takePath <$> Alonzo.readAlonzoCddlFileNames
  babbage <- map takePath <$> Babbage.readBabbageCddlFileNames
  conway  <- map takePath <$> Conway.readConwayCddlFileNames

  localDataDir <- takePath <$> getDataDir
  let local_paths = map (localDataDir F.</>) [
          "cddl"
        , "cddl/disk"
        , "cddl/disk/snapshot"
        , "cddl/node-to-client/localstatequery/byron"
        , "cddl/node-to-client/localstatequery/consensus"
        , "cddl/node-to-client/localstatequery/shelley"
        , "cddl/node-to-client/txmonitor"
        ]

      include_path =
          mconcat
        $ L.intersperse ":"
        $ map (mconcat . L.intersperse ":") [byron, shelley, allegra, mary, alonzo, babbage, conway] <> local_paths

  E.setEnv "CDDL_INCLUDE_PATH" (include_path <> ":")

newtype CDDLSpec = CDDLSpec BL.ByteString deriving Show

data CDDLs = CDDLs {
    diskBlockCDDL :: CDDLSpec
  , diskSnapshotCDDL :: CDDLSpec

  , ntnBlockFetchBlockCDDL :: CDDLSpec
  , ntnBlockFetchPointCDDL :: CDDLSpec

  , ntnChainSyncHeaderCDDL :: CDDLSpec
  , ntnChainSyncPointCDDL  :: CDDLSpec
  , ntnChainSyncTipCDDL    :: CDDLSpec

  , ntnTxSubmissionTicketNoCDDL :: CDDLSpec
  , ntnTxSubmissionTxCDDL       :: CDDLSpec
  , ntnTxSubmissionTxIdCDDL     :: CDDLSpec

  , ntcLocalStateQueryQueryCDDL  :: CDDLSpec
  , ntcLocalStateQueryResultCDDL :: CDDLSpec

  , ntcTxMonitorTxCDDL     :: CDDLSpec
  , ntcTxMonitorTxIdCDDL   :: CDDLSpec
  , ntcTxMonitorSlotNoCDDL :: CDDLSpec
  } deriving Show

getCDDLs :: IO CDDLs
getCDDLs = CDDLs
  -- Disk
  <$> cddlc "cddl/disk/block.cddl"
  <*> cddlc "cddl/disk/snapshot.cddl"

  -- Node to node
  -- -- BlockFetch
  <*> cddlc "cddl/node-to-node/blockfetch/block.cddl"
  <*> cddlc "cddl/node-to-node/blockfetch/point.cddl"

  -- -- ChainSync
  <*> cddlc "cddl/node-to-node/chainsync/header.cddl"
  <*> cddlc "cddl/node-to-node/chainsync/point.cddl"
  <*> cddlc "cddl/node-to-node/chainsync/tip.cddl"

  -- -- TxSubmission2
  <*> cddlc "cddl/node-to-node/txsubmission2/ticketno.cddl"
  <*> cddlc "cddl/node-to-node/txsubmission2/tx.cddl"
  <*> cddlc "cddl/node-to-node/txsubmission2/txid.cddl"

  -- Node to client
  -- -- LocalStateQuery
  <*> cddlc "cddl/node-to-client/localstatequery/query.cddl"
  <*> cddlc "cddl/node-to-client/localstatequery/result.cddl"

  -- -- TxMonitor
  <*> cddlc "cddl/node-to-client/txmonitor/tx.cddl"
  <*> cddlc "cddl/node-to-client/txmonitor/txid.cddl"
  <*> cddlc "cddl/node-to-client/txmonitor/slotno.cddl"

cddlc :: FilePath -> IO CDDLSpec
cddlc dataFile = do
  putStrLn $ "Generating: " <> dataFile
  path <- getDataFileName dataFile
  (_, cddl, err) <-
#ifdef POSIX
    P.readProcessWithExitCode "cddlc" ["-u", "-2", "-t", "cddl", path] mempty
#else
    -- we cannot call @cddlc@ directly because it is not an executable in
    -- Haskell eyes, but we can call @ruby@ and pass the @cddlc@ script path as
    -- an argument
    do
    prefix <- E.getEnv "MSYSTEM_PREFIX"
    P.readProcessWithExitCode "ruby" [prefix F.</> "bin/cddlc", "-u", "-2", "-t", "cddl", path] mempty
#endif
  Monad.unless (BL.null err) $ red $ BL8.toString err
  return $ CDDLSpec cddl
 where
  red s = putStrLn $ "\ESC[31m" <> s <> "\ESC[0m"

takePath :: FilePath -> FilePath
takePath x =
#ifdef POSIX
  F.takeDirectory x
#else
  -- @cddlc@ is not capable of using backlashes
  --
  -- @cddlc@ mixes @C:@ with the separator in @CDDL_INCLUDE_PATH@, and it
  -- doesn't understand @;@ as a separator. It works if we remove @C:@ and we
  -- are running in the same drive as the cddl files.
  let f = [ if c /= '\\' then c else '/' | c <- F.takeDirectory x ]
  in if "C:" `L.isPrefixOf` f
     then drop 2 f
     else f
#endif

probeTools :: IO ()
probeTools = do
  putStrLn "Probing tools:"
#ifdef POSIX
  posixProbeTool "cddl" "install the `cddl` ruby gem"
  posixProbeTool "cddlc" "install the `cddlc` ruby gem"
  where
    posixProbeTool :: String -> Sring -> IO ()
    posixProbeTool tool suggestion = do
      putStr $ "- " <> tool <> " "
      exe <- D.findExecutable tool
      if isNothing exe
      then do
        putStrLn "not found!"
        putStrLn $ "Please " <> suggestion
        exitFailure
      else
        putStrLn "found"
#else
  -- On Windows, the cddl and cddlc files are POSIX scripts and therefore not
  -- recognized as executables by @findExecutable@, so we need to do some dirty
  -- tricks here. We check that ruby executable exists and then that there are
  -- cddl and cddlc files in the binary folder of the MSYS2 installation.
  putStr "- ruby "
  rubyExe <- D.findExecutable "ruby"
  if (isNothing rubyExe)
  then do
    putStrLn "not found!\nPlease install ruby and the `cddl` and `cddlc` gems"
    exitFailure
  else
    putStrLn "found"

  putStr "- cddl "
  cddlExe <- D.doesFileExist . (F.</> "bin/cddlc") =<< E.getEnv "MSYSTEM_PREFIX"
  if cddlExe
  then putStrLn "found"
  else do
    putStrLn "not found!\nPlease install the `cddl` ruby gem"
    exitFailure

  putStr "- cddlc "
  cddlcExe <- D.doesFileExist . (F.</> "bin/cddlc") =<< E.getEnv "MSYSTEM_PREFIX"
  if cddlcExe
  then putStrLn "found"
  else do
    putStrLn "not found!\nPlease install the `cddlc` ruby gem"
    exitFailure
  pure ()
#endif
