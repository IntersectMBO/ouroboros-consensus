{-# LANGUAGE LambdaCase #-}

module Test.Util.Serialisation.CDDL
  ( cddlTestCase
  , cddlTest
  , isCDDLCDisabled
  , CDDLsForNodeToNode (..)
  ) where

import Control.Monad (join)
import qualified Data.ByteString as BS
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified System.Directory as D
import qualified System.Environment as E
import System.Exit
import System.IO
import System.IO.Temp
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

-- | Windows on Hydra cannot cross-compile CDDLC so we decided to skip the tests
-- there.
isCDDLCDisabled :: Bool
isCDDLCDisabled = isJust $ unsafePerformIO (E.lookupEnv "DISABLE_CDDLC")

-- | A Tasty test case running the @cuddle@
cddlTestCase :: IO BS.ByteString -> FilePath -> T.Text -> TestTree
cddlTestCase cborM cddl rule =
  testCase "CDDL compliance" $
    if isCDDLCDisabled
      then assertBool "Skipped" True
      else
        cddlTest cborM cddl rule >>= \case
          Left err -> assertFailure err
          Right _ -> pure ()

-- | Test the CDDL conformance of the given bytestring
cddlTest ::
  IO BS.ByteString ->
  String ->
  T.Text ->
  IO (Either String ())
cddlTest cborM cddl rule =
  withTempFile "." "testcase.cbor" $ \fp h -> do
    bs <- cborM
    BS.hPutStr h bs
    hClose h
    (code, _out, err) <-
      readProcessWithExitCode "cuddle" (cuddleArgs fp (T.unpack rule) cddl) mempty
    case code of
      ExitFailure _ -> do
        -- Copy the CBOR term and the CDDL file into a directory and
        -- generate a script with a cuddle call that would lead to an error
        errorReproducerDir <-
          join $
            dumpErrorReproducer <$> D.canonicalizePath fp <*> pure (T.unpack rule) <*> D.canonicalizePath cddl
        pure (Left $ err <> " cuddle reproducer written to " <> errorReproducerDir)
      ExitSuccess -> pure (Right ())
 where
  cuddleArgs :: FilePath -> String -> FilePath -> [String]
  cuddleArgs cborFile ruleName cddlFile = ["validate-cbor", "-c", cborFile, "-r", ruleName, cddlFile]

  dumpErrorReproducer :: FilePath -> String -> FilePath -> IO FilePath
  dumpErrorReproducer cborFile ruleName cddlFile = do
    errorReproducerDir <- D.canonicalizePath $ "failing_cddl_tests"
    D.createDirectoryIfMissing False errorReproducerDir
    D.withCurrentDirectory errorReproducerDir $ do
      failingCborFile <- D.canonicalizePath $ ruleName <> "_failing.cbor"
      failingCddlFile <- D.canonicalizePath $ ruleName <> "_failing.cddl"
      let failingCuddleCallFile = "call_cuddle_" <> ruleName <> "_failing.sh"
          failingCuddleCall = unwords $ "cuddle" : (cuddleArgs failingCborFile ruleName failingCddlFile)
      D.copyFile cborFile failingCborFile
      D.copyFile cddlFile failingCddlFile
      writeFile failingCuddleCallFile failingCuddleCall
    pure errorReproducerDir

-- | A collection of CDDL spec and the relevant rule to use
data CDDLsForNodeToNode = CDDLsForNodeToNode
  { blockCDDL :: (FilePath, T.Text)
  , headerCDDL :: (FilePath, T.Text)
  , txCDDL :: (FilePath, T.Text)
  , txIdCDDL :: (FilePath, T.Text)
  }
