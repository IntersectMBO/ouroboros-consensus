{-# LANGUAGE TypeApplications #-}

-- | Serialization tests for local state queries and results.
module Test.Consensus.Ledger.Query (tests) where

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Control.Monad.Class.MonadThrow (throwIO)
import qualified Data.ByteString.Base16.Lazy as LBase16
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Ouroboros.Consensus.Ledger.Query (
  QueryVersion (..),
  queryDecodeNodeToClient,
  queryEncodeNodeToClient,
 )
import Ouroboros.Consensus.Mock.Node.Serialisation (MockBlock)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

-- | We only test the latest query version against the API.
latestVersion :: QueryVersion
latestVersion = QueryVersion3

tests :: TestTree
tests =
  testGroup
    "LocalStateQuery"
    [ -- TODO: do this for all queries
      testGroup
        "getSystemStart"
        [ testCase "Query example" $ do
            -- Decode blueprint example query with queryDecodeNodeToClient
            exampleHex <- LBS.readFile "../cardano-blueprint/src/api/examples/getSystemStart/query.cbor"
            print exampleHex
            -- FIXME: the example in cardano-blueprint is wrapping the query in the n2c network message
            let decoder =
                  queryDecodeNodeToClient
                    mockCodecConfig
                    latestVersion
                    mockBlockNodeToClientVersion
            someQuery <-
              LBase16.decodeLenient exampleHex
                & deserialiseFromBytes decoder
                & either throwIO (pure . snd)
            print someQuery
            -- TODO: encode query with queryEncodeNodeToClient
            let encoded =
                  toLazyByteString $
                    queryEncodeNodeToClient @(MockBlock ())
                      mockCodecConfig
                      latestVersion
                      mockBlockNodeToClientVersion
                      someQuery
            print $ LBase16.encode encoded
            -- TODO: check whether cbor is equal
            -- TODO: validate cbor against cddl
            fail "TODO"
        , testCase "Query roundtrip" $ do
            -- TODO: generate arbitrary query terms given cddl
            -- TODO: decode with queryDecodeNodeToClient
            -- TODO: encode query with queryEncodeNodeToClient
            -- TODO: check whether cbor is equal
            -- TODO: validate cbor against cddl
            fail "TODO"
        , testCase "Query conformance" $ do
            -- TODO: generate arbitrary 'GetSystemStart :: Query blk SystemStart' values
            -- TODO: encode with queryEncodeNodeToClient
            -- TODO: validate cbor against cddl
            fail "TODO"
        , testCase "Result example" $ do
            -- TODO: decode blueprint example result with decodeResult
            -- TODO: encode result with encodeResult
            -- TODO: check whether cbor is equal
            -- TODO: validate cbor against cddl
            fail "TODO"
        , testCase "Query roundtrip" $ do
            -- TODO: generate arbitrary result terms given cddl
            -- TODO: decode result with decodeResult
            -- TODO: encode result with encodeResult
            -- TODO: check whether cbor is equal
            -- TODO: validate cbor against cddl
            fail "TODO"
        , testCase "Result conformance" $ do
            -- TODO: generate arbitrary 'SystemStart' values
            -- TODO: encode result with encodeResult
            -- TODO: validate cbor against cddl
            fail "TODO"
        ]
    ]
 where
  mockCodecConfig = error "CodecConfig used unexpectedly"

  mockBlockNodeToClientVersion = error "BlockNodeToClientVersion used unexpectedly"
