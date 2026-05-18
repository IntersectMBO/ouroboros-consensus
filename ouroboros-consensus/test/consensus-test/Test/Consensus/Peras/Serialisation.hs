{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Serialisation roundtrip tests for Peras types
module Test.Consensus.Peras.Serialisation
  ( tests
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeFull, serialize)
import Cardano.Ledger.BaseTypes (SlotNo (..))
import qualified Data.ByteString.Lazy as LazyByteString
import Ouroboros.Consensus.Block (Point)
import Ouroboros.Consensus.Block.RealPoint (RealPoint (..), realPointToPoint)
import Test.Consensus.Peras.Util
  ( genPerasCert
  , genPerasVote
  , mkBucket
  , tabulatePerasCert
  , tabulatePerasVote
  )
import Test.Ouroboros.Storage.TestBlock (TestBlock, TestHeaderHash (..))
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Property
  , counterexample
  , forAll
  , tabulate
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.TestEnv (adjustQuickCheckTests)

-- | Generate an arbitrary 'Point' for 'TestBlock'.
-- We reuse 'TestBlock' from "Test.Ouroboros.Storage.TestBlock" which already
-- provides 'ConvertRawHash' (needed by the CBOR instances of Peras types).
genTestPoint :: Gen (Point TestBlock)
genTestPoint = do
  slotNo <- SlotNo <$> arbitrary
  hash <- TestHeaderHash <$> arbitrary
  pure $ realPointToPoint $ RealPoint slotNo hash

tests :: TestTree
tests =
  testGroup
    "Serialization roundtrip for Peras types"
    [ adjustQuickCheckTests (* 10) $
        testProperty "Roundtrip for PerasVote" $
          prop_roundtrip
            -- Generate both persistent and non-persistent votes
            (genPerasVote genTestPoint True)
            tabulatePerasVote
    , adjustQuickCheckTests (* 10) $
        testProperty "Roundtrip for PerasCert" $
          prop_roundtrip
            -- Generate certs with both persistent and non-persistent votes
            (genPerasCert genTestPoint True)
            tabulatePerasCert
    ]

-- * Properties

prop_roundtrip ::
  forall a.
  ( Eq a
  , Show a
  , ToCBOR a
  , FromCBOR a
  ) =>
  Gen a ->
  (a -> Property -> Property) ->
  Property
prop_roundtrip gen tabulateValue =
  forAll gen $ \a -> do
    let encoded = serialize a
    let decoded = decodeFull encoded
    tabulateValue a
      . tabulateEncodedSize encoded
      . counterexample
        ( unlines
            [ "Original value:"
            , show a
            , "Decoded value:"
            , show decoded
            ]
        )
      $ Right a === decoded

-- * Tabulators

tabulateEncodedSize :: LazyByteString.ByteString -> Property -> Property
tabulateEncodedSize bytes =
  tabulate
    "Encoded size"
    [mkBucket 1000 (fromIntegral (LazyByteString.length bytes)) " bytes"]
