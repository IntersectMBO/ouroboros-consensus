{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Property tests for trivial aggregate verification helpers.
--
-- NOTE: this module reuses 'TestCrypto' for convenience here, even though it
-- already implements its own non-trivial aggregate verification routine using
-- aggregate BLS signatures.
module Test.Consensus.Committee.Crypto (tests) where

import Control.Monad.Zip (MonadZip (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Containers.NonEmpty (HasNonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsVRF (..)
  , CryptoSupportsVoteSigning (..)
  , VRFPoolContext (..)
  , trivialLiftVRFOutput
  , trivialLiftVRFVerificationKey
  , trivialLiftVoteSignature
  , trivialLiftVoteVerificationKey
  , trivialVerifyAggregateVRFOutput
  , trivialVerifyAggregateVoteSignature
  )
import Test.Consensus.Committee.TestCrypto
  ( TestCrypto
  , genElectionId
  , genKeyPair
  , genVoteCandidate
  )
import qualified Test.Consensus.Committee.TestCrypto as TestCrypto
import Test.Consensus.Committee.Utils (genEpochNonce)
import Test.QuickCheck
  ( Gen
  , Property
  , choose
  , counterexample
  , forAll
  , oneof
  , property
  , suchThat
  , tabulate
  , vectorOf
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- * Property tests for trivial aggregate vote signature verification

-- | Test that trivial aggregate vote signature verification succeeds when all
-- individual signatures are valid against the correct verification keys.
prop_trivialVerifyAggregateVoteSignature_valid :: Property
prop_trivialVerifyAggregateVoteSignature_valid =
  forAll genElectionId $ \electionId ->
    forAll genVoteCandidate $ \candidate ->
      forAll (choose (1, 10)) $ \numKeys ->
        forAll (vectorOf numKeys genKeyPair) $ \keyPairs -> do
          let (voteSigningKeys, voteVerificationKeys) =
                munzip $
                  bimap
                    (getVoteSigningKey (Proxy @TestCrypto))
                    (getVoteVerificationKey (Proxy @TestCrypto))
                    <$> NonEmpty.fromList keyPairs
          let aggVoteSignature =
                sconcat
                  . fmap (trivialLiftVoteSignature (Proxy @TestCrypto))
                  . fmap (\sk -> signVote @TestCrypto sk electionId candidate)
                  $ voteSigningKeys
          let aggVoteVerificationKey =
                sconcat
                  . fmap (trivialLiftVoteVerificationKey (Proxy @TestCrypto))
                  $ voteVerificationKeys
          tabulate "Number of keys" [show numKeys] $
            case ( trivialVerifyAggregateVoteSignature
                     (Proxy @TestCrypto)
                     aggVoteVerificationKey
                     electionId
                     candidate
                     aggVoteSignature
                 ) of
              Left err ->
                counterexample
                  ("Trivial aggregate vote verification failed: " <> err)
                  $ property False
              Right () ->
                property True

-- | Test that trivial aggregate vote signature verification fails when the some
-- of the individual verification keys or signatures are missing.
--
-- NOTE: this is trivially satisfied for aggregate keys/signatures with a single
-- element, as removing it would break the Semigroup-based aggregation step.
prop_trivialVerifyAggregateVoteSignature_lengthMismatch :: Property
prop_trivialVerifyAggregateVoteSignature_lengthMismatch =
  forAll genElectionId $ \electionId ->
    forAll genVoteCandidate $ \candidate ->
      forAll (choose (2, 10)) $ \numKeys ->
        forAll (vectorOf numKeys genKeyPair) $ \keyPairs -> do
          forAll
            ( removeOneFromEither $
                munzip $
                  bimap
                    (getVoteSigningKey (Proxy @TestCrypto))
                    (getVoteVerificationKey (Proxy @TestCrypto))
                    <$> NonEmpty.fromList keyPairs
            )
            $ \(voteSigningKeys, voteVerificationKeys) -> do
              let aggVoteSignature =
                    sconcat
                      . fmap (trivialLiftVoteSignature (Proxy @TestCrypto))
                      . fmap (\sk -> signVote @TestCrypto sk electionId candidate)
                      $ voteSigningKeys
              let aggVoteVerificationKey =
                    sconcat
                      . fmap (trivialLiftVoteVerificationKey (Proxy @TestCrypto))
                      $ voteVerificationKeys
              tabulate "Number of vote signers" [show numKeys] $
                case ( trivialVerifyAggregateVoteSignature
                         (Proxy @TestCrypto)
                         aggVoteVerificationKey
                         electionId
                         candidate
                         aggVoteSignature
                     ) of
                  Left _ ->
                    property True
                  Right () ->
                    counterexample
                      "Trivial aggregate vote verification should have failed, but it didn't"
                      $ property False

-- | Test that trivial aggregate vote signature verification fails when at
-- least one signature is invalid (i.e., it is a signature for something else).
prop_trivialVerifyAggregateVoteSignature_invalidSig :: Property
prop_trivialVerifyAggregateVoteSignature_invalidSig =
  forAll genElectionId $ \electionId ->
    forAll TestCrypto.genVoteCandidate $ \candidate ->
      forAll (choose (1, 10)) $ \numKeys ->
        forAll (vectorOf numKeys genKeyPair) $ \keyPairs ->
          forAll (choose (0, numKeys - 1)) $ \corruptedIdx ->
            forAll (TestCrypto.genVoteCandidate `suchThat` (/= candidate)) $ \corruptedCandidate -> do
              let (voteSigningKeys, voteVerificationKeys) =
                    munzip $
                      bimap
                        (getVoteSigningKey (Proxy @TestCrypto))
                        (getVoteVerificationKey (Proxy @TestCrypto))
                        <$> NonEmpty.fromList keyPairs
              let signatures =
                    NonEmpty.fromList
                      . zipWith
                        ( \idx sk ->
                            if idx == corruptedIdx
                              then signVote @TestCrypto sk electionId corruptedCandidate
                              else signVote @TestCrypto sk electionId candidate
                        )
                        [0 ..]
                      . NonEmpty.toList
                      $ voteSigningKeys
              let aggVoteSignature =
                    sconcat
                      . fmap (trivialLiftVoteSignature (Proxy @TestCrypto))
                      $ signatures
              let aggVoteVerificationKey =
                    sconcat
                      . fmap (trivialLiftVoteVerificationKey (Proxy @TestCrypto))
                      $ voteVerificationKeys
              tabulate "Number of keys" [show numKeys] $
                case ( trivialVerifyAggregateVoteSignature
                         (Proxy @TestCrypto)
                         aggVoteVerificationKey
                         electionId
                         candidate
                         aggVoteSignature
                     ) of
                  Left _ ->
                    property True
                  Right () ->
                    counterexample
                      "Trivial aggregate vote verification should have failed due to invalid signature"
                      $ property False

-- * Property tests for trivial aggregate VRF output verification

-- | Test that trivial aggregate VRF output verification succeeds when all
-- individual VRF outputs are valid against the correct verification keys.
prop_trivialVerifyAggregateVRFOutput_valid :: Property
prop_trivialVerifyAggregateVRFOutput_valid =
  forAll genElectionId $ \electionId ->
    forAll genEpochNonce $ \epochNonce ->
      forAll (choose (1, 10)) $ \numKeys ->
        forAll (vectorOf numKeys genKeyPair) $ \keyPairs -> do
          let (vrfSigningKeys, vrfVerificationKeys) =
                munzip $
                  bimap
                    (getVRFSigningKey (Proxy @TestCrypto))
                    (getVRFVerificationKey (Proxy @TestCrypto))
                    <$> NonEmpty.fromList keyPairs
          let input = mkVRFElectionInput @TestCrypto epochNonce electionId
          tabulate "Number of keys" [show numKeys] $
            case ( traverse
                     (\sk -> evalVRF @TestCrypto (VRFSignContext sk) input)
                     vrfSigningKeys
                 ) of
              Left err ->
                counterexample
                  ("VRF evaluation failed: " <> err)
                  $ property False
              Right outputs -> do
                let aggVRFVerificationKey =
                      sconcat
                        . fmap (trivialLiftVRFVerificationKey (Proxy @TestCrypto))
                        $ vrfVerificationKeys
                let aggOutput =
                      sconcat
                        . fmap (trivialLiftVRFOutput (Proxy @TestCrypto))
                        $ outputs
                case ( trivialVerifyAggregateVRFOutput @TestCrypto
                         aggVRFVerificationKey
                         input
                         aggOutput
                     ) of
                  Left err ->
                    counterexample
                      ("Trivial aggregate VRF verification failed: " <> err)
                      $ property False
                  Right () ->
                    property True

-- | Test that trivial aggregate VRF output verification fails when some of the
-- individual verification keys or VRF outputs are missing.
--
-- NOTE: this is trivially satisfied for aggregate keys/outputs with a single
-- element, as removing it would break the Semigroup-based aggregation step.
prop_trivialVerifyAggregateVRFOutput_lengthMismatch :: Property
prop_trivialVerifyAggregateVRFOutput_lengthMismatch =
  forAll genElectionId $ \electionId ->
    forAll genEpochNonce $ \epochNonce ->
      forAll (choose (2, 10)) $ \numKeys ->
        forAll (vectorOf numKeys genKeyPair) $ \keyPairs -> do
          forAll
            ( removeOneFromEither $
                munzip $
                  bimap
                    (getVRFSigningKey (Proxy @TestCrypto))
                    (getVRFVerificationKey (Proxy @TestCrypto))
                    <$> NonEmpty.fromList keyPairs
            )
            $ \(vrfSigningKeys, vrfVerificationKeys) -> do
              let input = mkVRFElectionInput @TestCrypto epochNonce electionId
              tabulate "Number of keys" [show numKeys] $
                case ( traverse
                         (\sk -> evalVRF @TestCrypto (VRFSignContext sk) input)
                         vrfSigningKeys
                     ) of
                  Left err ->
                    counterexample
                      ("VRF evaluation failed: " <> err)
                      $ property False
                  Right outputs -> do
                    let aggVRFVerificationKey =
                          sconcat
                            . fmap (trivialLiftVRFVerificationKey (Proxy @TestCrypto))
                            $ vrfVerificationKeys
                    let aggOutput =
                          sconcat
                            . fmap (trivialLiftVRFOutput (Proxy @TestCrypto))
                            $ outputs
                    case ( trivialVerifyAggregateVRFOutput @TestCrypto
                             aggVRFVerificationKey
                             input
                             aggOutput
                         ) of
                      Left _ ->
                        property True
                      Right () ->
                        counterexample
                          "Trivial aggregate VRF verification should have failed, but it didn't"
                          $ property False

-- | Test that trivial aggregate VRF output verification fails when at least
-- one VRF output is invalid (i.e., it is a VRF output for something else).
prop_trivialVerifyAggregateVRFOutput_invalidOutput :: Property
prop_trivialVerifyAggregateVRFOutput_invalidOutput =
  forAll genElectionId $ \electionId ->
    forAll genEpochNonce $ \epochNonce ->
      forAll (choose (1, 10)) $ \numKeys ->
        forAll (vectorOf numKeys genKeyPair) $ \keyPairs ->
          forAll (choose (0, numKeys - 1)) $ \corruptedIdx ->
            forAll (genElectionId `suchThat` (/= electionId)) $ \corruptedElectionId -> do
              let (vrfSigningKeys, vrfVerificationKeys) =
                    munzip $
                      bimap
                        (getVRFSigningKey (Proxy @TestCrypto))
                        (getVRFVerificationKey (Proxy @TestCrypto))
                        <$> NonEmpty.fromList keyPairs
              let input =
                    mkVRFElectionInput @TestCrypto epochNonce electionId
              let corruptedInput =
                    mkVRFElectionInput @TestCrypto epochNonce corruptedElectionId
              tabulate "Number of keys" [show numKeys] $
                case ( sequence
                         $ NonEmpty.zipWith
                           ( \idx sk ->
                               if idx == corruptedIdx
                                 then evalVRF @TestCrypto (VRFSignContext sk) corruptedInput
                                 else evalVRF @TestCrypto (VRFSignContext sk) input
                           )
                           (NonEmpty.fromList [0 ..])
                         $ vrfSigningKeys
                     ) of
                  Left err ->
                    counterexample
                      ("VRF evaluation failed: " <> err)
                      $ property False
                  Right verifiedOutputs -> do
                    let aggVRFVerificationKey =
                          sconcat
                            . fmap (trivialLiftVRFVerificationKey (Proxy @TestCrypto))
                            $ vrfVerificationKeys
                    let aggOutput =
                          sconcat
                            . fmap (trivialLiftVRFOutput (Proxy @TestCrypto))
                            $ verifiedOutputs
                    case ( trivialVerifyAggregateVRFOutput @TestCrypto
                             aggVRFVerificationKey
                             input
                             aggOutput
                         ) of
                      Left _ ->
                        property True
                      Right () ->
                        counterexample
                          "Trivial aggregate VRF verification should have failed due to invalid output"
                          $ property False

-- * Helpers

-- | Remove an element in either the left or right non-empty lists.
--
-- PRECONDITION: both lists must have at least two elements each.
removeOneFromEither :: (NE [a], NE [b]) -> Gen (NE [a], NE [b])
removeOneFromEither (xs, ys)
  | length xs < 2 || length ys < 2 =
      error "removeOneFromEither: both lists must have at least two elements"
  | otherwise =
      oneof
        [ (,) <$> removeOne xs <*> pure ys
        , (,) <$> pure xs <*> removeOne ys
        ]
 where
  removeOne zs = do
    idx <- choose (0, length zs - 1)
    pure $
      NonEmpty.fromList $
        NonEmpty.take idx zs
          <> NonEmpty.drop (idx + 1) zs

-- * Tests

tests :: TestTree
tests =
  testGroup
    "Trivial aggregate verification helpers"
    [ testGroup
        "trivialVerifyAggregateVoteSignature"
        [ testProperty
            "valid signatures"
            prop_trivialVerifyAggregateVoteSignature_valid
        , testProperty
            "length mismatch"
            prop_trivialVerifyAggregateVoteSignature_lengthMismatch
        , testProperty
            "invalid signature"
            prop_trivialVerifyAggregateVoteSignature_invalidSig
        ]
    , testGroup
        "trivialVerifyAggregateVRFOutput"
        [ testProperty
            "valid outputs"
            prop_trivialVerifyAggregateVRFOutput_valid
        , testProperty
            "length mismatch"
            prop_trivialVerifyAggregateVRFOutput_lengthMismatch
        , testProperty
            "invalid output"
            prop_trivialVerifyAggregateVRFOutput_invalidOutput
        ]
    ]
