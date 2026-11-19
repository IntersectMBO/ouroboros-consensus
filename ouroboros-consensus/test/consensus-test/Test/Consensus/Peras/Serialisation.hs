{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Serialisation roundtrip tests for Peras types
module Test.Consensus.Peras.Serialisation
  ( tests
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeFull, serialize)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Word (Word64)
import qualified Ouroboros.Consensus.Committee.Types as Committee
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import Ouroboros.Consensus.Peras.Params
  ( PerasBlockMinSlots (..)
  , PerasBootstrapRound (..)
  , PerasCertArrivalThreshold (..)
  , PerasCertMaxRounds (..)
  , PerasCooldownRounds (..)
  , PerasIgnoranceRounds (..)
  , PerasParams (..)
  , PerasQuorumWeightThreshold (..)
  , PerasQuorumWeightThresholdSafetyMargin (..)
  , PerasWeight (..)
  )
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1
import Test.QuickCheck
  ( Gen
  , Property
  , arbitrary
  , choose
  , counterexample
  , forAll
  , frequency
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
    , adjustQuickCheckTests (* 10) $
        testProperty "Roundtrip for PerasParams" $
          prop_roundtrip @(PerasParams ()) genPerasParams (const id)
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

-- | Generate values independently for every PerasParams field.
-- Word64 fields cover their full range to exercise CBOR integer encodings;
-- bounded rationals keep their generated values compact.
genPerasParams :: Gen (PerasParams ())
genPerasParams = do
  ignoranceRounds <- PerasIgnoranceRounds <$> genWord64
  cooldownRounds <- PerasCooldownRounds <$> genWord64
  blockMinSlots <- PerasBlockMinSlots <$> genWord64
  certMaxRounds <- PerasCertMaxRounds <$> genWord64
  certArrivalThreshold <- PerasCertArrivalThreshold <$> genWord64
  weight <- PerasWeight <$> genWord64
  quorumWeightThreshold <- PerasQuorumWeightThreshold <$> genRational
  quorumWeightThresholdSafetyMargin <- PerasQuorumWeightThresholdSafetyMargin <$> genRational
  targetCommitteeSize <- Committee.TargetCommitteeSize <$> genWord64
  bootstrapRound <-
    frequency
      [ (1, pure SNothing)
      , (1, SJust . PerasBootstrapRound <$> genWord64)
      ]
  pure
    PerasParams
      { perasIgnoranceRounds = ignoranceRounds
      , perasCooldownRounds = cooldownRounds
      , perasBlockMinSlots = blockMinSlots
      , perasCertMaxRounds = certMaxRounds
      , perasCertArrivalThreshold = certArrivalThreshold
      , perasWeight = weight
      , perasQuorumWeightThreshold = quorumWeightThreshold
      , perasQuorumWeightThresholdSafetyMargin = quorumWeightThresholdSafetyMargin
      , perasTargetCommitteeSize = targetCommitteeSize
      , perasBootstrapRound = bootstrapRound
      }
 where
  genWord64 :: Gen Word64
  genWord64 = arbitrary

  genRational :: Gen Rational
  genRational = do
    numerator <- choose (0, 1000 :: Integer)
    denominator <- choose (1, 1000 :: Integer)
    pure (toRational numerator / toRational denominator)
