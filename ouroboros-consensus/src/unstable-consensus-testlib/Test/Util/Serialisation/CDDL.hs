{-# LANGUAGE LambdaCase #-}

module Test.Util.Serialisation.CDDL
  ( cddlTestCase
  , cddlTest
  , isCDDLCDisabled
  , CDDLsForNodeToNode (..)
  ) where

import qualified Data.ByteString as BS
import Data.Maybe (isJust)
import qualified Data.Text as T
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
      readProcessWithExitCode "cuddle" ["validate-cbor", "-c", fp, "-r", T.unpack rule, cddl] mempty
    case code of
      ExitFailure _ -> do
        BS.writeFile "failing.cbor" bs
        pure (Left err)
      ExitSuccess -> pure (Right ())

-- | A collection of CDDL spec and the relevant rule to use
data CDDLsForNodeToNode = CDDLsForNodeToNode
  { blockCDDL :: (FilePath, T.Text)
  , headerCDDL :: (FilePath, T.Text)
  , txCDDL :: (FilePath, T.Text)
  , txIdCDDL :: (FilePath, T.Text)
  }
