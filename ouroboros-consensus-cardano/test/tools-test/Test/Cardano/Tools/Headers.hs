module Test.Cardano.Tools.Headers (tests) where

import Cardano.Tools.Headers (ValidationResult (..), validate)
import qualified Data.Aeson as Json
import Data.Function ((&))
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
import Test.Ouroboros.Consensus.Protocol.Praos.Header (
    Sample (..),
    genContext,
    genMutatedHeader,
    genSample,
    shrinkSample,
 )
import Test.QuickCheck (
    Property,
    conjoin,
    counterexample,
    forAll,
    forAllBlind,
    forAllShrinkBlind,
    label,
    property,
    shrink,
    (===),
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup
        "HeaderValidation"
        [ testProperty "roundtrip To/FromJSON samples" prop_roundtrip_json_samples
        , testProperty "validate legit header" prop_validate_legit_header
        ]

prop_roundtrip_json_samples :: Property
prop_roundtrip_json_samples =
    forAll genSample $ \sample ->
        let encoded = Json.encode sample
            decoded = Json.eitherDecode encoded
         in decoded === Right sample

prop_validate_legit_header :: Property
prop_validate_legit_header =
    forAllBlind genContext $ \context ->
        forAllBlind (genMutatedHeader context) $ \header ->
            annotate context header $
                case validate context header of
                    Valid mut -> property True & label (show mut)
                    Invalid mut err -> property False & counterexample ("Expected: " <> show mut <> "\nError: " <> err)
  where
    annotate context header =
        counterexample
            ( unlines $
                [ "context:"
                , asJson context
                , "header:"
                , asJson header
                ]
            )

    asJson :: (Json.ToJSON a) => a -> String
    asJson = LT.unpack . decodeUtf8 . Json.encode
