{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Golden tests infrastructure.
--
-- Golden tests are implemented using
-- __[tasty-golden](https://github.com/UnkindPartition/tasty-golden)__.
--
-- When adding a new golden test, running the test suite locally will generate
-- the golden files. These files should be checked in as CI will fail if there
-- are missing golden files.
--
-- Failing a golden test suite when the corresponding golden files are not found
-- is done via the @--no-create@ flag, which surprisingly is opt-in. In our
-- @nix@ infrastructure, this flag for CI is set in @nix/haskell.nix@.
--
-- > testFlags = lib.mkForce [ "--no-create" ];
--
-- In particular, if we introduce golden tests in new suites, we need to add
-- a line in the nix configuration above similar to the previous ones.
module Test.Util.Serialisation.Golden
  ( ToGoldenDirectory (..)
  , goldenTest_SerialiseDisk
  , goldenTest_SerialiseNodeToClient
  , goldenTest_SerialiseNodeToNode
  , goldenTest_all
  ) where

import Cardano.Prelude (forceElemsToWHNF)
import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.FlatTerm (TermToken (..))
import qualified Codec.CBOR.FlatTerm as CBOR
import qualified Codec.CBOR.Write as CBOR
import Codec.Serialise (encode)
import Control.Exception (SomeException, evaluate, try)
import Data.Bifunctor (first)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as BS.UTF8
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.TreeDiff
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.Block (CodecConfig)
import Ouroboros.Consensus.Ledger.Extended (encodeDiskExtLedgerState)
import Ouroboros.Consensus.Ledger.Query
  ( BlockSupportsLedgerQuery
  , QueryVersion
  , SomeBlockQuery (..)
  , blockQueryIsSupportedOnVersion
  , nodeToClientVersionToQueryVersion
  )
import Ouroboros.Consensus.Ledger.Tables (valuesMKEncoder)
import Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( HasNetworkProtocolVersion (..)
  , SupportedNetworkProtocolVersion (..)
  )
import Ouroboros.Consensus.Node.Run
  ( SerialiseDiskConstraints
  , SerialiseNodeToClientConstraints
  , SerialiseNodeToNodeConstraints
  )
import Ouroboros.Consensus.Node.Serialisation
  ( SerialiseBlockQueryResult (..)
  , SerialiseNodeToClient (..)
  , SerialiseNodeToNode (..)
  )
import Ouroboros.Consensus.Storage.Serialisation (EncodeDisk (..))
import Ouroboros.Consensus.Util.CBOR (decodeAsFlatTerm)
import Ouroboros.Consensus.Util.Condense (Condense (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import Test.Cardano.Binary.TreeDiff (CBORBytes (..))
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Util.Serialisation.CDDL
import Test.Util.Serialisation.Examples (Examples (..), Labelled)
import Test.Util.Serialisation.SomeResult (SomeResult (..))

{-------------------------------------------------------------------------------
  Golden test
-------------------------------------------------------------------------------}

-- | Golden test for CBOR output. When the output doesn't match the golden
-- one, we show an 'ediff' of the 'FlatTerm' output of both.
--
-- Exceptions: when forcing an encoding throws an exception, we 'show' the
-- exception and use that as the output.
goldenTestCBOR ::
  TestName ->
  a ->
  (a -> Encoding) ->
  -- | Path to the file containing the golden output
  FilePath ->
  -- | Path to the CDDL file that defines this CBOR, and the rule name
  Maybe (FilePath, T.Text) ->
  TestTree
goldenTestCBOR testName example enc goldenFile mCddlPath =
  testGroup testName $
    [ goldenTest
        "Golden == actual"
        (Strict.readFile goldenFile)
        (either exceptionToByteString id <$> try (evaluate actualValue))
        diff
        updateGoldenFile
    ]
      ++ ( case mCddlPath of
             Nothing -> []
             Just (cddlPath, rule) ->
               [ cddlTestCase
                   (Strict.readFile goldenFile)
                   cddlPath
                   rule
               ]
         )
 where
  -- Copied from tasty-golden because it isn't exported
  updateGoldenFile :: Strict.ByteString -> IO ()
  updateGoldenFile bytes = do
    let dir = takeDirectory goldenFile
    createDirectoryIfMissing True dir
    Strict.writeFile goldenFile bytes

  actualValue :: Strict.ByteString
  actualValue = CBOR.toStrictByteString (enc example)

  exceptionToByteString :: SomeException -> Strict.ByteString
  exceptionToByteString = BS.UTF8.fromString . show

  -- \| Use 'ediff' ('ToExpr') to diff the 'FlatTerm' representation.
  diff :: Strict.ByteString -> Strict.ByteString -> IO (Maybe String)
  diff golden actual = do
    actualRes <-
      fmap (first (\(e :: SomeException) -> e))
        . try
        . evaluate
        . forceElemsToWHNF
        . CBOR.toFlatTerm
        . enc
        $ example
    return $ case (actualRes, decodeAsFlatTerm golden) of
      (Left e, Right goldenFlatTerm)
        -- Encoder threw an exception and the golden output was valid
        -- CBOR. However, sometimes the 'show'n exception is also valid
        -- CBOR. So if the exception and the golden output match, the test
        -- passes.
        | exceptionToByteString e == golden -> Nothing
        | otherwise ->
            Just $
              unlines
                [ "Exception thrown by encoder doesn't match the golden CBOR output"
                , "Exception:"
                , show e
                , "Golden term:"
                , condense goldenFlatTerm
                ]
      (Left e, Left _)
        -- Encoder threw an exception. The golden output was not valid
        -- CBOR and the bytestrings match: we expected the exception
        | exceptionToByteString e == golden -> Nothing
        | otherwise ->
            Just $
              unlines
                [ "Exception thrown by encoder doesn't match the golden output"
                , "Exception:"
                , show e
                , "Golden output:"
                , BS.UTF8.toString golden
                ]
      (Right _actualFlatTerm, Right _goldenFlatTerm)
        | actual == golden -> Nothing
        | otherwise ->
            Just $
              unlines
                [ "Golden term /= actual term, diff golden actual:"
                , show (ansiWlEditExpr (ediff (CBORBytes golden) (CBORBytes actual)))
                ]
      (Right actualFlatTerm, Left e) ->
        Just $
          unlines
            [ "Golden output /= actual term:"
            , "Golden output is not valid CBOR:"
            , BS.UTF8.toString golden
            , "Exception: "
            , show e
            , "Actual term:"
            , condense actualFlatTerm
            ]

goldenTests ::
  HasCallStack =>
  TestName ->
  Labelled a ->
  (a -> Encoding) ->
  -- | Folder containing the golden files
  FilePath ->
  Maybe (FilePath, T.Text) ->
  TestTree
goldenTests testName examples enc goldenFolder mCDDL
  | nub labels /= labels =
      error $ "Examples with the same label for " <> testName
  | [(Nothing, example)] <- examples =
      -- If there's just a single unlabelled example, no need for grouping,
      -- which makes the output more verbose.
      goldenTestCBOR testName example enc (goldenFolder </> testName) mCDDL
  | otherwise =
      testGroup
        testName
        [ goldenTestCBOR testName' example enc (goldenFolder </> testName') mCDDL
        | (mbLabel, example) <- examples
        , let testName' = case mbLabel of
                Nothing -> testName
                Just label -> testName <> "_" <> label
        ]
 where
  labels :: [Maybe String]
  labels = map fst examples

goldenTests' ::
  HasCallStack =>
  TestName ->
  Labelled (a, a -> Encoding) ->
  -- | Folder containing the golden files
  FilePath ->
  Maybe (FilePath, T.Text) ->
  TestTree
goldenTests' testName examples goldenFolder mCDDL
  | nub labels /= labels =
      error $ "Examples with the same label for " <> testName
  | [(Nothing, (example, exampleEncoder))] <- examples =
      -- If there's just a single unlabelled example, no need for grouping,
      -- which makes the output more verbose.
      goldenTestCBOR testName example exampleEncoder (goldenFolder </> testName) mCDDL
  | otherwise =
      testGroup
        testName
        [ goldenTestCBOR testName' example exampleEncoder (goldenFolder </> testName') mCDDL
        | (mbLabel, (example, exampleEncoder)) <- examples
        , let testName' = case mbLabel of
                Nothing -> testName
                Just label -> testName <> "_" <> label
        ]
 where
  labels :: [Maybe String]
  labels = map fst examples

{-------------------------------------------------------------------------------
  Skeletons
-------------------------------------------------------------------------------}

-- | Convert 'a' to a 'FilePath' that can be used as the directory containing
-- golden output files.
--
-- This class allows overriding the 'Show' in cases where that output is not
-- suitable to be used as a directory.
--
-- For example, the 'Show' output for a hard fork enabled 'NodeToNodeVersion'
-- will contain colons, asterisks, spaces, parentheses, ... and other
-- characters that we don't want to use for a directory name. For instance
-- colons cannot be used in Windows file/folder names.
class ToGoldenDirectory a where
  toGoldenDirectory :: a -> FilePath
  default toGoldenDirectory :: Show a => a -> FilePath
  toGoldenDirectory = show

-- | Golden tests for all things we serialise to disk and send across the
-- network.
--
-- Exceptions: when an encoder throws an exception, which can happen when
-- serialising a Shelley header in combination with
-- 'CardanoNodeToNodeVersion1', we 'show' the exception and use that as the
-- output.
goldenTest_all ::
  ( SerialiseDiskConstraints blk
  , SerialiseNodeToNodeConstraints blk
  , SerialiseNodeToClientConstraints blk
  , SupportedNetworkProtocolVersion blk
  , BlockSupportsLedgerQuery blk
  , ToGoldenDirectory (BlockNodeToNodeVersion blk)
  , ToGoldenDirectory (QueryVersion, BlockNodeToClientVersion blk)
  , HasCallStack
  ) =>
  CodecConfig blk ->
  -- | Path relative to the root of the repository that contains the golden
  -- files
  FilePath ->
  Maybe CDDLsForNodeToNode ->
  Examples blk ->
  TestTree
goldenTest_all codecConfig goldenDir mCDDLs examples =
  testGroup
    "Golden tests"
    [ goldenTest_SerialiseDisk codecConfig goldenDir examples
    , goldenTest_SerialiseNodeToNode codecConfig goldenDir mCDDLs examples
    , goldenTest_SerialiseNodeToClient codecConfig goldenDir examples
    ]

-- TODO how can we ensure that we have a test for each constraint listed in
-- 'SerialiseDiskConstraints'?
goldenTest_SerialiseDisk ::
  forall blk.
  ( SerialiseDiskConstraints blk
  , HasCallStack
  ) =>
  CodecConfig blk ->
  FilePath ->
  Examples blk ->
  TestTree
goldenTest_SerialiseDisk codecConfig goldenDir Examples{..} =
  testGroup
    "SerialiseDisk"
    [ test "Block" exampleBlock (encodeDisk codecConfig)
    , test "HeaderHash" exampleHeaderHash encode
    , test "LedgerState" exampleLedgerState (encodeDisk codecConfig)
    , test "AnnTip" exampleAnnTip (encodeDisk codecConfig)
    , test "ChainDepState" exampleChainDepState (encodeDisk codecConfig)
    , test "ExtLedgerState" exampleExtLedgerState encodeExt
    , testLedgerTables
    ]
 where
  test :: TestName -> Labelled a -> (a -> Encoding) -> TestTree
  test testName exampleValues enc =
    goldenTests
      testName
      exampleValues
      enc
      (goldenDir </> "disk")
      Nothing

  testLedgerTables :: TestTree
  testLedgerTables =
    goldenTests'
      "LedgerTables"
      ( zipWith
          (\(lbl, tbs) (_, st) -> (lbl, (tbs, valuesMKEncoder st)))
          exampleLedgerTables
          exampleLedgerState
      )
      (goldenDir </> "disk")
      Nothing

  encodeExt = encodeDiskExtLedgerState codecConfig

-- TODO how can we ensure that we have a test for each constraint listed in
-- 'SerialiseNodeToNodeConstraints'?
goldenTest_SerialiseNodeToNode ::
  forall blk.
  ( SerialiseNodeToNodeConstraints blk
  , SupportedNetworkProtocolVersion blk
  , ToGoldenDirectory (BlockNodeToNodeVersion blk)
  , HasCallStack
  ) =>
  CodecConfig blk ->
  FilePath ->
  Maybe CDDLsForNodeToNode ->
  Examples blk ->
  TestTree
goldenTest_SerialiseNodeToNode codecConfig goldenDir mCDDLs Examples{..} =
  testGroup
    "SerialiseNodeToNode"
    [ testVersion version
    | version <- nub $ Map.elems $ supportedNodeToNodeVersions $ Proxy @blk
    ]
 where
  testVersion :: BlockNodeToNodeVersion blk -> TestTree
  testVersion version =
    testGroup
      (toGoldenDirectory version)
      [ test "Block" exampleBlock $ fmap blockCDDL mCDDLs
      , test "Header" exampleHeader $ fmap headerCDDL mCDDLs
      , test "SerialisedBlock" exampleSerialisedBlock Nothing
      , test "SerialisedHeader" exampleSerialisedHeader Nothing
      , test "GenTx" exampleGenTx $ fmap txCDDL mCDDLs
      , test "GenTxId" exampleGenTxId $ fmap txIdCDDL mCDDLs
      ]
   where
    test :: SerialiseNodeToNode blk a => TestName -> Labelled a -> Maybe (FilePath, T.Text) -> TestTree
    test testName exampleValues =
      goldenTests
        testName
        exampleValues
        (encodeNodeToNode codecConfig version)
        (goldenDir </> toGoldenDirectory version)

-- TODO how can we ensure that we have a test for each constraint listed in
-- 'SerialiseNodeToClientConstraints'?
goldenTest_SerialiseNodeToClient ::
  forall blk.
  ( SerialiseNodeToClientConstraints blk
  , SupportedNetworkProtocolVersion blk
  , BlockSupportsLedgerQuery blk
  , ToGoldenDirectory (QueryVersion, BlockNodeToClientVersion blk)
  , HasCallStack
  ) =>
  CodecConfig blk ->
  FilePath ->
  Examples blk ->
  TestTree
goldenTest_SerialiseNodeToClient codecConfig goldenDir Examples{..} =
  testGroup
    "SerialiseNodeToClient"
    [ testVersion (queryVersion, blockVersion)
    | (queryVersion, blockVersion) <-
        nub . fmap (first nodeToClientVersionToQueryVersion) . Map.toList $
          supportedNodeToClientVersions (Proxy @blk)
    ]
 where
  testVersion :: (QueryVersion, BlockNodeToClientVersion blk) -> TestTree
  testVersion versions@(_, blockVersion) =
    testGroup
      (toGoldenDirectory versions)
      [ test "Block" exampleBlock enc'
      , test "SerialisedBlock" exampleSerialisedBlock enc'
      , test "GenTx" exampleGenTx enc'
      , test "GenTxId" exampleGenTxId enc'
      , test "ApplyTxErr" exampleApplyTxErr enc'
      , test "SlotNo" exampleSlotNo enc'
      , test "LedgerConfig" exampleLedgerConfig enc'
      , testQuery "Query" exampleQuery enc'
      , testResult "Result" exampleResult encRes
      ]
   where
    enc' :: SerialiseNodeToClient blk a => a -> Encoding
    enc' = encodeNodeToClient codecConfig blockVersion

    encRes :: SomeResult blk -> Encoding
    encRes (SomeResult q r) = encodeBlockQueryResult codecConfig blockVersion q r

    test :: TestName -> Labelled a -> (a -> Encoding) -> TestTree
    test testName exampleValues enc =
      goldenTests
        testName
        exampleValues
        enc
        (goldenDir </> toGoldenDirectory versions)
        Nothing

    testQuery name values =
      test name (filter (\(_, SomeBlockQuery q) -> blockQueryIsSupportedOnVersion q blockVersion) values)

    testResult name values =
      test name (filter (\(_, SomeResult q _) -> blockQueryIsSupportedOnVersion q blockVersion) values)

{-------------------------------------------------------------------------------
  FlatTerm
-------------------------------------------------------------------------------}

instance Condense TermToken where
  condense = show
