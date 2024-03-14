{-# LANGUAGE TypeApplications     #-}

module Test.Ouroboros.Storage.LedgerDB.Serialisation (tests) where

import           Codec.CBOR.FlatTerm (FlatTerm, TermToken (..), fromFlatTerm,
                     toFlatTerm)
import           Codec.Serialise (decode, encode)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Snapshots
import           Test.Util.Orphans.Arbitrary ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Util.TestBlock

tests :: TestTree
tests = testGroup "Serialisation" [
          testCase     "encode"                 test_encode_ledger
        , testCase     "decode"                 test_decode_ledger
        , testCase     "decode ChainSummary"    test_decode_ChainSummary
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
      -- VersionNumber
    , TkInt 1
      -- ledger: Int
    , TkInt 100
    ]

-- | The old format based on the @ChainSummary@. To remain backwards compatible
-- we still accept this old format.
golden_ChainSummary :: FlatTerm
golden_ChainSummary =
    [ TkListLen 3
      -- tip: WithOrigin (RealPoint TestBlock)
    , TkListLen 1
    , TkListLen 2
    , TkInt 3
    , TkListBegin, TkInt 0, TkInt 0, TkBreak
      -- chain length: Word64
    , TkInt 10
      -- ledger: Int for simplicity
    , TkInt 100
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
    dec = decodeLBackwardsCompatible (Proxy @TestBlock) decode decode

-- | For backwards compatibility
test_decode_ChainSummary :: Assertion
test_decode_ChainSummary =
    fromFlatTerm dec golden_ChainSummary @?= Right example_ledger
  where
    dec = decodeLBackwardsCompatible (Proxy @TestBlock) decode decode
