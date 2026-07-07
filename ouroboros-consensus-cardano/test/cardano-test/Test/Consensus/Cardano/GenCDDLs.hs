{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Consensus.Cardano.GenCDDLs (withCDDLs) where

import qualified Control.Monad as Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as L
import Data.Maybe (isNothing)
import Paths_ouroboros_consensus
import qualified System.Directory as D
import qualified System.Environment as E
import System.Exit
import qualified System.FilePath as F
import System.IO
import System.IO.Temp
import qualified System.Process.ByteString.Lazy as P
import qualified Test.Cardano.Chain.Binary.Cddl as Byron
import qualified Test.Cardano.Ledger.Allegra.Binary.Cddl as Allegra
import qualified Test.Cardano.Ledger.Alonzo.Binary.Cddl as Alonzo
import qualified Test.Cardano.Ledger.Babbage.Binary.Cddl as Babbage
import qualified Test.Cardano.Ledger.Conway.Binary.Cddl as Conway
import qualified Test.Cardano.Ledger.Dijkstra.Binary.Cddl as Dijkstra
import qualified Test.Cardano.Ledger.Mary.Binary.Cddl as Mary
import qualified Test.Cardano.Ledger.Shelley.Binary.Cddl as Shelley
import Test.Tasty
import Test.Util.Serialisation.CDDL (isCDDLCDisabled)

newtype CDDLSpec = CDDLSpec {cddlSpec :: BS.ByteString} deriving Show

-- | This function will run the provided test-tree after generating the node to
-- node cddls for Blocks and Headers. As more CDDLs are stabilized they will
-- have to be added here. Eventually we can have a datatype with one field for
-- each CDDL so that we know always what is available.
withCDDLs :: TestTree -> TestTree
withCDDLs f =
  if isCDDLCDisabled
    then f
    else
      withResource
        ( do
            probeTools
            setupCDDLCEnv

            ntnBlock <- cddlc "ouroboros-consensus-cardano/cddl/node-to-node/blockfetch/block.cddl"
            ntnBlock' <- fixupLedgerCDDL ntnBlock
            BS.writeFile "ntnblock.cddl" . cddlSpec $ ntnBlock'

            ntnHeader <- cddlc "ouroboros-consensus-cardano/cddl/node-to-node/chainsync/header.cddl"
            BS.writeFile "ntnheader.cddl" . cddlSpec $ ntnHeader

            ntnTx <- cddlc "ouroboros-consensus-cardano/cddl/node-to-node/txsubmission2/tx.cddl"
            ntnTx' <- fixupLedgerCDDL ntnTx
            BS.writeFile "ntntx.cddl" . cddlSpec $ ntnTx'

            ntnTxId <- cddlc "ouroboros-consensus-cardano/cddl/node-to-node/txsubmission2/txId.cddl"
            BS.writeFile "ntntxid.cddl" . cddlSpec $ ntnTxId

            leiosNotify <- cddlc "cardano-blueprint/src/network/node-to-node/leios-notify/messages.cddl"
            BS.writeFile "leiosnotify.cddl" . cddlSpec $ leiosNotify
        )
        ( \() -> do
            D.removeFile "ntnblock.cddl"
            D.removeFile "ntnheader.cddl"
            D.removeFile "ntntx.cddl"
            D.removeFile "ntntxid.cddl"
            D.removeFile "leiosnotify.cddl"
        )
        (\_ -> f)

-- | The Ledger CDDL specs for transactions and blocks are too restrictive.
-- Here we do some dirty sed-replace to make them able to validate blocks.
-- See cardano-ledger#5054.
fixupLedgerCDDL :: CDDLSpec -> IO CDDLSpec
fixupLedgerCDDL spec =
  withTempFile "." "block-temp.cddl" $ \fp h -> do
    hClose h
    BS.writeFile fp . cddlSpec $ spec
    -- For plutus, the type is actually `bytes`, but the distinct construct is
    -- for forcing generation of different values. See cardano-ledger#5054
    sed fp ["-i", "s/\\(alonzo\\.distinct_bytes = \\)/\\1 bytes ;\\//g"]
    sed fp ["-i", "s/\\(babbage\\.distinct_bytes = \\)/\\1 bytes ;\\//g"]
    sed fp ["-i", "s/\\(conway\\.distinct_bytes = \\)/\\1 bytes ;\\//g"]
    sed fp ["-i", "s/\\(dijkstra\\.distinct_bytes = \\)/\\1 bytes ;\\//g"]
    -- These 3 below are hardcoded for generation. See cardano-ledger#5054
    sed fp ["-i", "s/\\([yaoye]\\.address = \\)/\\1 bytes ;/g"]
    sed fp ["-i", "s/\\(reward_account = \\)/\\1 bytes ;/g"]
    sed
      fp
      [ "-i"
      , "-z"
      , "s/unit_interval = #6\\.30(\\[\\n\\s*1,\\n\\s*2,\\n\\])/unit_interval = #6.30([uint, uint])/g"
      ]

    -- for convenience, we use this test suite to generate the complete CDDL spec for manual testing.
    -- while this sed replacement is not used in these tests, it is needed to validate some of the real blocks.
    sed fp ["-i", "s/\\(chain_code: bytes\\)/\\1, ;/g"]

    -- cuddle 1.8.0.0 does not support optional group entries in the middle of an
    -- array group (e.g. "? leios_key" between vrf_keyhash and pledge). We work
    -- around this by:
    --   1. Removing the optional "? leios_key" from dijkstra.pool_params,
    --      making it the 9-element (no leios_key) variant.
    --   2. Adding a new dijkstra.pool_params_with_leios_key rule with 10 elements.
    --   3. Adding dijkstra.pool_registration_cert_with_leios_key as an alternative
    --      in the certificate array so cuddle can validate both encodings.
    -- See cardano-ledger#5054.
    content <- BS.readFile fp
    let content' =
          bsReplace
            dijkstraPoolParamsOld
            dijkstraPoolParamsNew
            ( bsReplace
                dijkstraCertOld
                dijkstraCertNew
                content
            )
    BS.writeFile fp (content' <> dijkstraLeiosKeyPoolRules)
    CDDLSpec <$> BS.readFile fp
 where
  dijkstraPoolParamsOld =
    BS8.pack $
      "dijkstra.pool_params = (\n"
        <> "  operator: dijkstra.pool_keyhash,\n"
        <> "  vrf_keyhash: dijkstra.vrf_keyhash,\n"
        <> "  ? leios_key: dijkstra.leios_key / nil,\n"
        <> "  pledge: dijkstra.coin,\n"
        <> "  cost: dijkstra.coin,\n"
        <> "  margin: dijkstra.unit_interval,\n"
        <> "  reward_account: dijkstra.reward_account,\n"
        <> "  pool_owners: dijkstra.set<dijkstra.addr_keyhash>,\n"
        <> "  relays: [* dijkstra.relay],\n"
        <> "  pool_metadata: dijkstra.pool_metadata / nil,\n"
        <> "  )"
  dijkstraPoolParamsNew =
    BS8.pack $
      "dijkstra.pool_params = (\n"
        <> "  operator: dijkstra.pool_keyhash,\n"
        <> "  vrf_keyhash: dijkstra.vrf_keyhash,\n"
        <> "  pledge: dijkstra.coin,\n"
        <> "  cost: dijkstra.coin,\n"
        <> "  margin: dijkstra.unit_interval,\n"
        <> "  reward_account: dijkstra.reward_account,\n"
        <> "  pool_owners: dijkstra.set<dijkstra.addr_keyhash>,\n"
        <> "  relays: [* dijkstra.relay],\n"
        <> "  pool_metadata: dijkstra.pool_metadata / nil,\n"
        <> "  )"
  dijkstraCertOld =
    BS8.pack "dijkstra.pool_registration_cert // dijkstra.pool_retirement_cert"
  dijkstraCertNew =
    BS8.pack $
      "dijkstra.pool_registration_cert"
        <> " // dijkstra.pool_registration_cert_with_leios_key"
        <> " // dijkstra.pool_retirement_cert"
  dijkstraLeiosKeyPoolRules =
    BS8.pack $
      "\ndijkstra.pool_registration_cert_with_leios_key = (3, dijkstra.pool_params_with_leios_key)\n"
        <> "dijkstra.pool_params_with_leios_key = (\n"
        <> "  operator: dijkstra.pool_keyhash,\n"
        <> "  vrf_keyhash: dijkstra.vrf_keyhash,\n"
        <> "  leios_key: dijkstra.leios_key / nil,\n"
        <> "  pledge: dijkstra.coin,\n"
        <> "  cost: dijkstra.coin,\n"
        <> "  margin: dijkstra.unit_interval,\n"
        <> "  reward_account: dijkstra.reward_account,\n"
        <> "  pool_owners: dijkstra.set<dijkstra.addr_keyhash>,\n"
        <> "  relays: [* dijkstra.relay],\n"
        <> "  pool_metadata: dijkstra.pool_metadata / nil,\n"
        <> "  )\n"

-- | This sets the environment variables needed for `cddlc` to run properly.
setupCDDLCEnv :: IO ()
setupCDDLCEnv = do
  byron <- map takePath <$> Byron.readByronCddlFileNames
  shelley <- map takePath <$> Shelley.readShelleyCddlFileNames
  allegra <- map takePath <$> Allegra.readAllegraCddlFileNames
  mary <- map takePath <$> Mary.readMaryCddlFileNames
  alonzo <- map takePath <$> Alonzo.readAlonzoCddlFileNames
  babbage <- map takePath <$> Babbage.readBabbageCddlFileNames
  conway <- map takePath <$> Conway.readConwayCddlFileNames
  dijkstra <- map takePath <$> Dijkstra.readDijkstraCddlFileNames

  localDataDir <- windowsPathHack <$> getDataDir
  let local_paths =
        map
          (localDataDir F.</>)
          ["cardano-blueprint/src/codecs"] -- Directories with other cddls that we import should go here
      include_path =
        mconcat $
          L.intersperse ":" $
            map (mconcat . L.intersperse ":") [byron, shelley, allegra, mary, alonzo, babbage, conway, dijkstra]
              <> local_paths

  E.setEnv "CDDL_INCLUDE_PATH" (include_path <> ":")

-- | Call @sed@ on the given file with the given args
sed :: FilePath -> [String] -> IO ()
sed fp args =
  Monad.void $ P.readProcessWithExitCode "sed" (args ++ [fp]) mempty

-- | Replace the first occurrence of @old@ with @new@ in a 'BS.ByteString'.
bsReplace :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
bsReplace old new bs =
  let (before, rest) = BS.breakSubstring old bs
   in if BS.null rest
        then bs
        else before <> new <> BS.drop (BS.length old) rest

{- FOURMOLU_DISABLE -}

cddlc :: FilePath -> IO CDDLSpec
cddlc dataFile = do
  putStrLn $ "Generating: " <> dataFile
  path <- getDataFileName dataFile
  (exitCode, BSL.toStrict -> cddl, BSL.toStrict -> err) <-
#ifdef mingw32_HOST_OS
    -- we cannot call @cddlc@ directly because it is not an executable in
    -- Haskell eyes, but we can call @ruby@ and pass the @cddlc@ script path as
    -- an argument
    do
    prefix <- E.getEnv "MSYSTEM_PREFIX"
    P.readProcessWithExitCode "ruby" [prefix F.</> "bin/cddlc", "-u", "-2", "-t", "cddl", path] mempty
#else
    P.readProcessWithExitCode "cddlc" ["-u", "-2", "-t", "cddl", path] mempty
#endif
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure{} ->
      Monad.unless (BS.null err) $ red $ BS8.unpack err
  return $ CDDLSpec cddl
 where
  red s = putStrLn $ "\ESC[31m" <> s <> "\ESC[0m"

-- | @cddlc@ is not capable of using backlashes
--
-- @cddlc@ mixes @C:@ with the separator in @CDDL_INCLUDE_PATH@, and it
-- doesn't understand @;@ as a separator. It works if we remove @C:@ and we
-- are running in the same drive as the cddl files.
windowsPathHack :: FilePath -> FilePath
windowsPathHack x =
#ifdef mingw32_HOST_OS
  let f = [ if c /= '\\' then c else '/' | c <- x ]
  in if "C:" `L.isPrefixOf` f
     then drop 2 f
     else f
#else
  x
#endif

takePath :: FilePath -> FilePath
takePath = windowsPathHack . F.takeDirectory

probeTools :: IO ()
probeTools = do
  putStrLn "Probing tools:"
#ifdef mingw32_HOST_OS
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
#else
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
#endif

{- FOURMOLU_ENABLE -}
