-- | Serialization tests for local state queries and results.
module Test.Consensus.Ledger.Query (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "LocalStateQuery"
    [ -- TODO: do this for all queries
      testGroup
        "getSystemStart"
        [ testCase "Query example" $ do
            -- TODO: decode blueprint example query with queryDecodeNodeToClient
            -- TODO: encode query with queryEncodeNodeToClient
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
