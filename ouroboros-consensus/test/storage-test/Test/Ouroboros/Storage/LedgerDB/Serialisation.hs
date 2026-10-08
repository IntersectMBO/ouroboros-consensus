module Test.Ouroboros.Storage.LedgerDB.Serialisation (tests) where

import Codec.CBOR.FlatTerm
  ( FlatTerm
  , TermToken (..)
  , fromFlatTerm
  , toFlatTerm
  )
import Codec.Serialise (decode, encode)
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests =
  testGroup
    "Serialisation"
    [ testCase "encode" test_encode_ledger
    , testCase "decode" test_decode_ledger
    ]

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

-- | The LedgerDB is parametric in the ledger @l@. We use @Int@ for simplicity.
example_ledger :: Int
example_ledger = 100

golden_ledger :: FlatTerm
golden_ledger =
  [ TkListLen 2
  , -- VersionNumber
    TkInt 2
  , -- ledger: Int
    TkInt 100
  ]

test_encode_ledger :: Assertion
test_encode_ledger =
  toFlatTerm (enc example_ledger) @?= golden_ledger
 where
  enc = encodeL encode

test_decode_ledger :: Assertion
test_decode_ledger =
  fromFlatTerm dec golden_ledger @?= Right example_ledger
 where
  dec = decodeL decode
