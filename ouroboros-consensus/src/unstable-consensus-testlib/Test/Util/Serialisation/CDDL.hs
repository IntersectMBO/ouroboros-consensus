{-# LANGUAGE LambdaCase #-}

module Test.Util.Serialisation.CDDL (cddlTestCase, cddlTest, CDDLsForNodeToNode (..)) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import System.Exit
import System.IO
import System.IO.Temp
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

-- | A Tasty test case running the @cuddle@
cddlTestCase :: IO BS.ByteString -> FilePath -> T.Text -> TestTree
cddlTestCase cborM cddl rule =
  testCase "CDDL compliance" $
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
  }
