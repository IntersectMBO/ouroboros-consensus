{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Test crypto for voting committee tests, based on BLS signatures.
module Test.Consensus.Committee.TestCrypto
  ( -- * Test crypto based on BLS signatures
    TestCrypto
  , VoteSignature (TestVoteSignature, unTestVoteSignature)
  , VRFElectionInput (TestVRFElectionInput, unTestVRFElectionInput)
  , VRFOutput (TestVRFOutput, unTestVRFOutput)

    -- * QuickCheck helpers
  , genElectionId
  , genVoteCandidate
  , genKeyPair

    -- * Smoke test properties
  , prop_SignAndVerifyVote
  , prop_SignAndVerifyAggregateVote
  , prop_EvalAndVerifyVRFOutput
  , prop_EvalAndVerifyAggregateVRFOutput
  , tests
  ) where

import Cardano.Crypto.DSIGN
  ( BLS12381MinSigDSIGN
  , DSIGNAlgorithm (..)
  )
import Cardano.Crypto.Hash (ByteString, Hash)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.BaseTypes (Nonce (..))
import Cardano.Ledger.Binary (runByteBuilder)
import Cardano.Ledger.Hashes (HASH)
import Control.Monad (when)
import Control.Monad.Zip (MonadZip (..))
import Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import Data.Containers.NonEmpty (HasNonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import Data.Word (Word64)
import GHC.Word (Word8)
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsAggregateVRF (..)
  , CryptoSupportsAggregateVoteSigning (..)
  , CryptoSupportsVRF (..)
  , CryptoSupportsVoteSigning (..)
  , ElectionId
  , PrivateKey
  , PublicKey
  , TrivialAggregateVRFOutput (..)
  , TrivialAggregateVRFVerificationKey (..)
  , TrivialAggregateVoteSignature (..)
  , TrivialAggregateVoteVerificationKey (..)
  , VRFPoolContext (..)
  , VoteCandidate
  , trivialLiftVRFOutput
  , trivialLiftVRFVerificationKey
  , trivialLiftVoteSignature
  , trivialLiftVoteVerificationKey
  )
import Ouroboros.Consensus.Committee.Crypto.BLS (KeyRole (..))
import qualified Ouroboros.Consensus.Committee.Crypto.BLS as BLS
import Test.Consensus.Committee.Utils (genEpochNonce)
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Property
  , Small (..)
  , Testable (..)
  , choose
  , counterexample
  , forAll
  , suchThat
  , tabulate
  , vectorOf
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.TestEnv (adjustQuickCheckTests)

-- * Test crypto based on BLS signatures

data TestCrypto

type instance ElectionId TestCrypto = Word64
type instance VoteCandidate TestCrypto = ByteString -- 32 bytes long

type instance PrivateKey TestCrypto = (BLS.PrivateKey SIGN, BLS.PrivateKey VRF)
type instance PublicKey TestCrypto = (BLS.PublicKey SIGN, BLS.PublicKey VRF)

-- | Hash the message of a Peras vote
hashVoteSignature ::
  ElectionId TestCrypto ->
  VoteCandidate TestCrypto ->
  Hash HASH (SigDSIGN BLS12381MinSigDSIGN)
hashVoteSignature electionId candidate =
  Hash.castHash
    . Hash.hashWith id
    . runByteBuilder (8 + 32)
    $ electionIdBytes <> candidateBytes
 where
  electionIdBytes =
    BS.word64BE electionId
  candidateBytes =
    BS.byteStringCopy candidate

-- | Hash the input for the VRF used in Peras elections
hashVRFInput ::
  ElectionId TestCrypto ->
  Nonce ->
  Hash HASH (SigDSIGN BLS12381MinSigDSIGN)
hashVRFInput electionId epochNonce =
  Hash.castHash
    . Hash.hashWith id
    . runByteBuilder (8 + 32)
    $ electionIdBytes <> epochNonceBytes
 where
  electionIdBytes =
    BS.word64BE electionId
  epochNonceBytes =
    case epochNonce of
      NeutralNonce -> mempty
      Nonce h -> BS.byteStringCopy (Hash.hashToBytes h)

-- * Crypto instances

instance CryptoSupportsVoteSigning TestCrypto where
  type VoteSigningKey TestCrypto = BLS.PrivateKey SIGN
  type VoteVerificationKey TestCrypto = BLS.PublicKey SIGN

  newtype VoteSignature TestCrypto
    = TestVoteSignature
    { unTestVoteSignature :: BLS.Signature SIGN
    }
    deriving newtype (Show, Eq)

  getVoteSigningKey _ = fst
  getVoteVerificationKey _ = fst

  signVote sk electionId candidate =
    TestVoteSignature
      . BLS.signWithRole @SIGN sk
      $ hashVoteSignature electionId candidate

  verifyVoteSignature pk electionId candidate (TestVoteSignature sig) =
    BLS.verifyWithRole @SIGN
      pk
      (hashVoteSignature electionId candidate)
      sig

instance CryptoSupportsAggregateVoteSigning TestCrypto where
  type AggregateVoteVerificationKey TestCrypto = TrivialAggregateVoteVerificationKey TestCrypto
  type AggregateVoteSignature TestCrypto = TrivialAggregateVoteSignature TestCrypto

  liftVoteVerificationKey = trivialLiftVoteVerificationKey
  liftVoteSignature = trivialLiftVoteSignature

  verifyAggregateVoteSignature
    _
    (TrivialAggregateVoteVerificationKey pks)
    electionId
    candidate
    (TrivialAggregateVoteSignature sigs) = do
      aggPk <- BLS.aggregatePublicKeys @SIGN pks
      aggSig <- BLS.aggregateSignatures @SIGN (unTestVoteSignature <$> sigs)
      BLS.verifyWithRole @SIGN
        aggPk
        (hashVoteSignature electionId candidate)
        aggSig

instance CryptoSupportsVRF TestCrypto where
  type VRFSigningKey TestCrypto = BLS.PrivateKey VRF
  type VRFVerificationKey TestCrypto = BLS.PublicKey VRF

  newtype VRFElectionInput TestCrypto
    = TestVRFElectionInput
    { unTestVRFElectionInput ::
        Hash HASH (SigDSIGN BLS12381MinSigDSIGN)
    }
    deriving newtype (Show, Eq)

  newtype VRFOutput TestCrypto
    = TestVRFOutput
    { unTestVRFOutput ::
        BLS.Signature VRF
    }
    deriving newtype (Show, Eq)

  getVRFSigningKey _ = snd
  getVRFVerificationKey _ = snd

  mkVRFElectionInput epochNonce electionId =
    TestVRFElectionInput $
      hashVRFInput electionId epochNonce

  evalVRF context (TestVRFElectionInput input) =
    case context of
      VRFSignContext sk -> do
        let sig = BLS.signWithRole @VRF (BLS.coercePrivateKey @VRF sk) input
        pure $ TestVRFOutput sig
      VRFVerifyContext pk (TestVRFOutput sig) -> do
        BLS.verifyWithRole @VRF (BLS.coercePublicKey @VRF pk) input sig
        pure $ TestVRFOutput sig

  normalizeVRFOutput (TestVRFOutput sig) =
    BLS.toNormalizedVRFOutput sig

instance CryptoSupportsAggregateVRF TestCrypto where
  type AggregateVRFVerificationKey TestCrypto = TrivialAggregateVRFVerificationKey TestCrypto
  type AggregateVRFOutput TestCrypto = TrivialAggregateVRFOutput TestCrypto

  liftVRFVerificationKey = trivialLiftVRFVerificationKey
  liftVRFOutput = trivialLiftVRFOutput

  verifyAggregateVRFOutput
    (TrivialAggregateVRFVerificationKey pks)
    (TestVRFElectionInput input)
    (TrivialAggregateVRFOutput sigs) = do
      BLS.linearizeAndVerifyVRFs
        pks
        input
        (fmap unTestVRFOutput sigs)

-- * QuickCheck helpers

genElectionId :: Gen (ElectionId TestCrypto)
genElectionId =
  getSmall <$> arbitrary

genVoteCandidate :: Gen (VoteCandidate TestCrypto)
genVoteCandidate =
  BS.pack <$> vectorOf 32 (arbitrary @Word8)

genKeyPair :: Gen (PrivateKey TestCrypto, PublicKey TestCrypto)
genKeyPair = do
  voteSigningKey <- genPrivateKey @SIGN
  vrfSigningKey <- genPrivateKey @VRF
  let voteVerificationKey = BLS.derivePublicKey voteSigningKey
  let vrfVerificationKey = BLS.derivePublicKey vrfSigningKey
  return
    ( (voteSigningKey, vrfSigningKey)
    , (voteVerificationKey, vrfVerificationKey)
    )
 where
  genPrivateKey :: forall r. Gen (BLS.PrivateKey r)
  genPrivateKey = do
    fromMaybe (error "Failed to generate BLS private key")
      . BLS.rawDeserialisePrivateKey @r "TEST"
      . BS.pack
      <$> vectorOf 32 (arbitrary @Word8)

-- | Swap two distinct elements in a non-empty list.
--
-- PRECONDITION: the input list must have at least two elements.
swapTwoElements :: NE [a] -> Gen (Int, Int, NE [a])
swapTwoElements xs = do
  let len = NonEmpty.length xs
  when (len < 2) $
    error "swapTwoElements requires a list with at least two elements"

  i <- choose (0, len - 1)
  j <- choose (0, len - 1) `suchThat` (/= i)
  let maybeSwap k
        | k == i = (NonEmpty.!!) xs j
        | k == j = (NonEmpty.!!) xs i
        | otherwise = (NonEmpty.!!) xs k
  let xs' =
        NonEmpty.fromList
          [ maybeSwap k
          | k <- [0 .. len - 1]
          ]
  return (i, j, xs')

-- * Smoke test properties

-- | Round trip test for vote signatures
prop_SignAndVerifyVote :: Property
prop_SignAndVerifyVote =
  forAll genElectionId $ \electionId ->
    forAll genVoteCandidate $ \candidate ->
      forAll genKeyPair $ \(sk, pk) -> do
        let voteSigningKey = getVoteSigningKey (Proxy @TestCrypto) sk
        let voteVerificationKey = getVoteVerificationKey (Proxy @TestCrypto) pk
        let sig = signVote @TestCrypto voteSigningKey electionId candidate
        case ( verifyVoteSignature @TestCrypto
                 voteVerificationKey
                 electionId
                 candidate
                 sig
             ) of
          Left err ->
            counterexample
              ("Round trip verification failed: " <> err)
              $ property False
          Right () ->
            property True

-- | Round trip test for aggregate vote signatures
prop_SignAndVerifyAggregateVote :: Property
prop_SignAndVerifyAggregateVote =
  forAll genElectionId $ \electionId ->
    forAll genVoteCandidate $ \candidate ->
      forAll (choose (1, 10)) $ \numSigners ->
        forAll (vectorOf numSigners genKeyPair) $ \keyPairs -> do
          let (voteSigningKeys, voteVerificationKeys) =
                munzip
                  . NonEmpty.fromList
                  . fmap (first (getVoteSigningKey (Proxy @TestCrypto)))
                  . fmap (second (getVoteVerificationKey (Proxy @TestCrypto)))
                  $ keyPairs
          let aggVoteSignature =
                sconcat
                  . fmap (liftVoteSignature (Proxy @TestCrypto))
                  . fmap (\sk -> signVote @TestCrypto sk electionId candidate)
                  $ voteSigningKeys
          let aggVoteVerificationKey =
                sconcat
                  . fmap (liftVoteVerificationKey (Proxy @TestCrypto))
                  $ voteVerificationKeys
          tabulate "Number of vote signers" [show numSigners] $
            case ( verifyAggregateVoteSignature
                     (Proxy @TestCrypto)
                     aggVoteVerificationKey
                     electionId
                     candidate
                     aggVoteSignature
                 ) of
              Left err ->
                counterexample
                  ("Aggregate vote verification failed: " <> err)
                  $ property False
              Right () ->
                property True

-- | Round trip test for VRF evaluation
prop_EvalAndVerifyVRFOutput :: Property
prop_EvalAndVerifyVRFOutput =
  forAll genElectionId $ \electionId ->
    forAll genEpochNonce $ \epochNonce ->
      forAll genKeyPair $ \(sk, pk) -> do
        let vrfSigningKey = getVRFSigningKey (Proxy @TestCrypto) sk
        let vrfVerificationKey = getVRFVerificationKey (Proxy @TestCrypto) pk
        let input = mkVRFElectionInput @TestCrypto epochNonce electionId
        case ( evalVRF @TestCrypto
                 (VRFSignContext vrfSigningKey)
                 input
             ) of
          Left err ->
            counterexample
              ("VRF evaluation failed: " <> err)
              $ property False
          Right output ->
            case ( evalVRF @TestCrypto
                     (VRFVerifyContext vrfVerificationKey output)
                     input
                 ) of
              Left err ->
                counterexample
                  ("VRF verification failed: " <> err)
                  $ property False
              Right output' ->
                counterexample
                  "VRF output mismatch"
                  $ output === output'

-- | Round trip test for aggregate VRF evaluation
prop_EvalAndVerifyAggregateVRFOutput :: Property
prop_EvalAndVerifyAggregateVRFOutput =
  forAll genElectionId $ \electionId ->
    forAll genEpochNonce $ \epochNonce ->
      forAll (choose (1, 10)) $ \numSigners ->
        forAll (vectorOf numSigners genKeyPair) $ \keyPairs -> do
          let (vrfSigningKeys, vrfVerificationKeys) =
                munzip
                  . NonEmpty.fromList
                  . fmap (first (getVRFSigningKey (Proxy @TestCrypto)))
                  . fmap (second (getVRFVerificationKey (Proxy @TestCrypto)))
                  $ keyPairs
          let input = mkVRFElectionInput @TestCrypto epochNonce electionId
          tabulate "Number of VRF signers" [show numSigners] $
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
                        . fmap (liftVRFVerificationKey (Proxy @TestCrypto))
                        $ vrfVerificationKeys
                let aggOutput =
                      sconcat
                        . fmap (liftVRFOutput (Proxy @TestCrypto))
                        $ outputs
                case ( verifyAggregateVRFOutput @TestCrypto
                         aggVRFVerificationKey
                         input
                         aggOutput
                     ) of
                  Left err ->
                    counterexample
                      ("Aggregate VRF verification failed: " <> err)
                      $ property False
                  Right () ->
                    property True

-- | Test resilience against swap attacks on aggregate VRF verification
prop_SwapAttackOnAggregateVRF :: Property
prop_SwapAttackOnAggregateVRF =
  forAll genElectionId $ \electionId ->
    forAll genEpochNonce $ \epochNonce ->
      forAll (choose (2, 10)) $ \numSigners ->
        forAll (vectorOf numSigners genKeyPair) $ \keyPairs -> do
          let (vrfSigningKeys, vrfVerificationKeys) =
                munzip
                  . NonEmpty.fromList
                  . fmap (first (getVRFSigningKey (Proxy @TestCrypto)))
                  . fmap (second (getVRFVerificationKey (Proxy @TestCrypto)))
                  $ keyPairs
          let input = mkVRFElectionInput @TestCrypto epochNonce electionId
          tabulate "Number of VRF signers" [show numSigners] $
            case ( traverse
                     (\sk -> evalVRF @TestCrypto (VRFSignContext sk) input)
                     vrfSigningKeys
                 ) of
              Left err ->
                counterexample
                  ("VRF evaluation failed: " <> err)
                  $ property False
              Right outputs -> do
                forAll (swapTwoElements outputs) $ \(i, j, swappedOutputs) -> do
                  let aggVRFVerificationKey =
                        sconcat
                          . fmap (liftVRFVerificationKey (Proxy @TestCrypto))
                          $ vrfVerificationKeys
                  let aggOutput =
                        sconcat
                          . fmap (liftVRFOutput (Proxy @TestCrypto))
                          $ swappedOutputs
                  case ( verifyAggregateVRFOutput @TestCrypto
                           aggVRFVerificationKey
                           input
                           aggOutput
                       ) of
                    Left _ ->
                      property True
                    Right () ->
                      counterexample
                        ( "Aggregate VRF verification should have failed when "
                            <> "swapping outputs at indices "
                            <> show i
                            <> " and "
                            <> show j
                        )
                        $ property False

tests :: TestTree
tests =
  testGroup
    "TestCrypto properties"
    [ adjustQuickCheckTests (* 10) $
        testProperty
          "prop_SignAndVerifyVote"
          prop_SignAndVerifyVote
    , adjustQuickCheckTests (* 10) $
        testProperty
          "prop_SignAndVerifyAggregateVote"
          prop_SignAndVerifyAggregateVote
    , adjustQuickCheckTests (* 10) $
        testProperty
          "prop_EvalAndVerifyVRFOutput"
          prop_EvalAndVerifyVRFOutput
    , adjustQuickCheckTests (* 10) $
        testProperty
          "prop_EvalAndVerifyAggregateVRFOutput"
          prop_EvalAndVerifyAggregateVRFOutput
    , adjustQuickCheckTests (* 10) $
        testProperty
          "prop_SwapAttackOnAggregateVRF"
          prop_SwapAttackOnAggregateVRF
    ]
