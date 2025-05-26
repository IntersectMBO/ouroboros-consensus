{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- |

module Test.Consensus.Cardano.GenCDDLs (withCDDLs) where

import qualified Control.Monad as Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8
import           Data.Maybe (isNothing)
import qualified Data.List as L
import           Paths_ouroboros_consensus_cardano
import qualified System.Directory as D
import qualified System.Environment as E
import           System.Exit
import qualified System.FilePath as F
import qualified System.Process.ByteString.Lazy as P

-- TODO: this is waiting to update to a newer ledger
--import qualified Test.Cardano.Chain.Binary.Cddl as Byron
import qualified Test.Cardano.Ledger.Allegra.Binary.Cddl as Allegra
import qualified Test.Cardano.Ledger.Alonzo.Binary.Cddl as Alonzo
import qualified Test.Cardano.Ledger.Babbage.Binary.Cddl as Babbage
import qualified Test.Cardano.Ledger.Conway.Binary.Cddl as Conway
import qualified Test.Cardano.Ledger.Mary.Binary.Cddl as Mary
import qualified Test.Cardano.Ledger.Shelley.Binary.Cddl as Shelley
import System.IO
import Test.Tasty
import System.IO.Temp

newtype CDDLSpec = CDDLSpec { cddlSpec :: BS.ByteString } deriving Show

withCDDLs :: TestTree -> TestTree
withCDDLs f = withResource
  (do
      probeTools
      setupCDDLCEnv
      BS.writeFile "ntnblock.cddl" . cddlSpec
          =<< (cddlc "cddl/node-to-node/blockfetch/block.cddl" >>= fixupBlockCDDL)
      BS.writeFile "ntnheader.cddl" . cddlSpec
          =<< cddlc "cddl/node-to-node/chainsync/header.cddl"
  )
  (\() -> do
      D.removeFile "ntnblock.cddl"
      D.removeFile "ntnheader.cddl"
  )
  (\_ -> f)


fixupBlockCDDL :: CDDLSpec -> IO CDDLSpec
fixupBlockCDDL spec =
  withTempFile "." "block-temp.cddl" $ \fp h -> do
    hClose h
    BS.writeFile fp . cddlSpec $ spec
    -- This is wrong, both the metadata_hash of a pool and a transaction body
    -- point to this type, but only the latter must be 32B.
    sed fp ["-i", "s/\\(metadata_hash = \\)/\\1 bytes ;/g"]
    -- For plutus, the type is actually `bytes`, but the distinct construct is
    -- for forcing generation of different values.
    sed fp ["-i", "s/\\(conway\\.distinct_VBytes = \\)/\\1 bytes ;\\//g"]
    -- These 3 below are hardcoded for generation. See cardano-ledger#5054
    sed fp ["-i", "s/\\([yaoye]\\.address = \\)/\\1 bytes ;/g"]
    sed fp ["-i", "s/\\(reward_account = \\)/\\1 bytes ;/g"]
    sed fp ["-i", "-z", "s/unit_interval = #6\\.30(\\[\\n\\s*1,\\n\\s*2,\\n\\])/unit_interval = #6.30([uint, uint])/g"]
    CDDLSpec <$> BS.readFile fp

setupCDDLCEnv :: IO ()
setupCDDLCEnv = do
  -- This is not how it should be because we can't update the Ledger
  -- to a newer one. On `master` there is a function
  -- `Byron.readByronCddlFileNames` which we would want to use.
  --
  -- Note also that cabal run will run in the root of the project and
  -- cabal test will run in `ouroboros-consensus-cardano`. This path
  -- is for the latter.
  byron   <- pure ["../../cardano-ledger/eras/byron/cddl-spec/"]
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

  writeFile "env" ("CDDL_INCLUDE_PATH=" <> include_path <> ":")
  E.setEnv "CDDL_INCLUDE_PATH" (include_path <> ":")

sed :: FilePath -> [String] -> IO ()
sed fp args =
  Monad.void $ P.readProcessWithExitCode "sed" (args ++ [fp]) mempty

cddlc :: FilePath -> IO CDDLSpec
cddlc dataFile = do
  putStrLn $ "Generating: " <> dataFile
  path <- getDataFileName dataFile
  (_, BSL.toStrict -> cddl, BSL.toStrict -> err) <-
#ifndef mingw32_HOST_OS
    P.readProcessWithExitCode "cddlc" ["-u", "-2", "-t", "cddl", path] mempty
#else
    -- we cannot call @cddlc@ directly because it is not an executable in
    -- Haskell eyes, but we can call @ruby@ and pass the @cddlc@ script path as
    -- an argument
    do
    prefix <- E.getEnv "MSYSTEM_PREFIX"
    P.readProcessWithExitCode "ruby" [prefix F.</> "bin/cddlc", "-u", "-2", "-t", "cddl", path] mempty
#endif
  Monad.unless (BS.null err) $ red $ BS8.unpack err
  return $ CDDLSpec cddl
 where
  red s = putStrLn $ "\ESC[31m" <> s <> "\ESC[0m"

takePath :: FilePath -> FilePath
takePath x =
#ifndef mingw32_HOST_OS
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
#ifndef mingw32_HOST_OS
  posixProbeTool "cddlc" "install the `cddlc` ruby gem"
  where
    posixProbeTool :: String -> String -> IO ()
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
    putStrLn "not found!\nPlease install ruby"
    exitFailure
  else
    putStrLn "found"

  putStr "- cddlc "
  cddlcExe <- D.doesFileExist . (F.</> "bin/cddlc") =<< E.getEnv "MSYSTEM_PREFIX"
  if cddlcExe
  then putStrLn "found"
  else do
    putStrLn "not found!\nPlease install the `cddlc` ruby gem"
    exitFailure
  pure ()
#endif
