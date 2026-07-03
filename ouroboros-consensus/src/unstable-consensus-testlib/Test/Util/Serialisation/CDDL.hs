{-# LANGUAGE LambdaCase #-}

module Test.Util.Serialisation.CDDL
  ( cddlTestCase
  , cddlTest
  , isCDDLCDisabled
  , CDDLsForNodeToNode (..)
  ) where

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR
import Control.Monad (join)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, isJust, listToMaybe)
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
    (code, out, err) <-
      readProcessWithExitCode "cuddle" (cuddleArgs fp (T.unpack rule) cddl) mempty
    case code of
      ExitFailure _ -> do
        -- 'cuddle' does not recurse into '.cbor' controls when reporting
        -- errors. If the outer failure mentions such a control and the CBOR
        -- contains a tag-24 embedded item, re-run validation against the
        -- inner rule to surface a more specific error.
        innerReport <- innerCborReport bs (out <> err) cddl
        -- Copy the CBOR term and the CDDL file into a directory and
        -- generate a script with a cuddle call that would lead to an error
        errorReproducerDir <-
          join $
            dumpErrorReproducer <$> D.canonicalizePath fp <*> pure (T.unpack rule) <*> D.canonicalizePath cddl
        pure . Left $
          out <> err <> innerReport <> "\ncuddle reproducer written to " <> errorReproducerDir
      ExitSuccess -> pure (Right ())
 where
  cuddleArgs :: FilePath -> String -> FilePath -> [String]
  cuddleArgs cborFile ruleName cddlFile = ["validate-cbor", ruleName, cborFile, cddlFile, "-f", "binary"]

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

  innerCborReport :: BS.ByteString -> String -> FilePath -> IO String
  innerCborReport bs outerOut cddlFile =
    case (extractInnerRule outerOut, findTag24 bs) of
      (Just innerRule, Just innerBytes) ->
        withTempFile "." "inner.cbor" $ \ifp ih -> do
          BS.hPutStr ih innerBytes
          hClose ih
          (_, iout, ierr) <-
            readProcessWithExitCode "cuddle" (cuddleArgs ifp innerRule cddlFile) mempty
          pure $
            "\n--- inner CBOR validation against " <> innerRule <> " ---\n" <> iout <> ierr
      _ -> pure ""

-- | Find the rule name mentioned in the first @.cbor <rule>@ control in
-- cuddle's output. Cuddle typically prints
--
-- @
--     unsatisfied control:
--         .cbor dijkstra.header
-- @
extractInnerRule :: String -> Maybe String
extractInnerRule s =
  listToMaybe
    [ takeWhile (`notElem` " \t\r\n") (drop (length prefix) ln')
    | ln <- lines (stripAnsi s)
    , let ln' = dropWhile (== ' ') ln
    , prefix `isPrefixOf` ln'
    ]
 where
  prefix = ".cbor "

-- | Walk a CBOR term and return the payload of the first @#6.24(bytes)@ item.
findTag24 :: BS.ByteString -> Maybe BS.ByteString
findTag24 bs = do
  (_, t) <-
    either (const Nothing) Just $
      CBOR.deserialiseFromBytes CBOR.decodeTerm (BSL.fromStrict bs)
  go t
 where
  go = \case
    CBOR.TTagged 24 (CBOR.TBytes inner) -> Just inner
    CBOR.TTagged 24 (CBOR.TBytesI inner) -> Just (BSL.toStrict inner)
    CBOR.TTagged _ t' -> go t'
    CBOR.TList ts -> firstJust (map go ts)
    CBOR.TListI ts -> firstJust (map go ts)
    CBOR.TMap kvs -> firstJust (concatMap (\(k, v) -> [go k, go v]) kvs)
    CBOR.TMapI kvs -> firstJust (concatMap (\(k, v) -> [go k, go v]) kvs)
    _ -> Nothing
  firstJust = listToMaybe . catMaybes

-- | Strip ANSI CSI escape sequences (@ESC [ ... m@) so we can pattern-match
-- against cuddle's colorised output.
stripAnsi :: String -> String
stripAnsi = \case
  '\ESC' : '[' : rest -> stripAnsi (drop 1 (dropWhile (/= 'm') rest))
  c : cs -> c : stripAnsi cs
  [] -> []

-- | A collection of CDDL spec and the relevant rule to use
data CDDLsForNodeToNode = CDDLsForNodeToNode
  { blockCDDL :: (FilePath, T.Text)
  , headerCDDL :: (FilePath, T.Text)
  , txCDDL :: (FilePath, T.Text)
  , txIdCDDL :: (FilePath, T.Text)
  }
