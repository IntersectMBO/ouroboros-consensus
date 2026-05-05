{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Serialisation roundtrip tests for Peras types
module Test.Util.Peras.Serialisation
  ( tests
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeFull, serialize)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1
import Test.QuickCheck
  ( Gen
  , Property
  , counterexample
  , forAll
  , tabulate
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Peras
  ( genPerasCert
  , genPerasVote
  , mkBucket
  , tabulatePerasCert
  , tabulatePerasVote
  )
import Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  testGroup
    "Serialization roundtrip for Peras types"
    [ adjustQuickCheckTests (* 10) $
        testProperty "Roundtrip for PerasVote" $
          prop_roundtrip @(V1.PerasVote ())
            -- Generate both persistent and non-persistent votes
            (genPerasVote True)
            tabulatePerasVote
    , adjustQuickCheckTests (* 10) $
        testProperty "Roundtrip for PerasCert" $
          prop_roundtrip @(V1.PerasCert ())
            -- Generate certs with both persistent and non-persistent votes
            (genPerasCert True)
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
