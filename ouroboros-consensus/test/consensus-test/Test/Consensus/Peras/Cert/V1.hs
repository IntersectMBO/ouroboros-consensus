{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Unit tests for properties of the V1 Peras cert
module Test.Consensus.Peras.Cert.V1 (tests) where

import Cardano.Binary (serialize)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.NonEmpty as NEMap
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import qualified Ouroboros.Consensus.Committee.Crypto.BLS as BLS
import Ouroboros.Consensus.Peras.Cert.V1
  ( PerasCert (..)
  , PerasCertVoters (..)
  , numberOfNonPersistentVoters
  , numberOfVoters
  , perasCertSizeUpperBound
  )
import Ouroboros.Consensus.Peras.Crypto.BLS (VRFOutput (..))
import Ouroboros.Consensus.Peras.Types
  ( PerasCertSize (..)
  , PerasSeatIndex (..)
  )
import Ouroboros.Consensus.Peras.Vote.V1
  ( PerasVoteEligibilityProof (..)
  )
import Test.QuickCheck.Gen (chooseInt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Gen
  , Property
  , counterexample
  , forAll
  , testProperty
  )
import Test.Util.Peras.V1 (genPerasCert)
import Test.Util.TestEnv (adjustQuickCheckTests)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests =
  adjustQuickCheckTests (* 1000) $
    testGroup
      "V1 Peras certificate"
      [ testProperty "has accurate upper bound" prop_accurateUpperBound
      ]

-- | Test that the upper bound on the size of Peras certificates is
-- indeed an upper bound, and does not over-approximate more than 15%.
prop_accurateUpperBound :: Property
prop_accurateUpperBound = forAll genPerasCertWithCustomVoters $ \cert -> do
  let
    upperBound = perasCertSizeUpperBound cert
    lowerBound = (upperBound * 85) `div` 100
    actualSize = serializedSize cert
    numVoters = numberOfVoters cert
    numNonPersistentVoters = numberOfNonPersistentVoters cert
    counterexampleMsg =
      unlines
        [ "Number of voters: " <> show numVoters
        , "Number of non-persistent voters: " <> show numNonPersistentVoters
        , "Upper bound: " <> show upperBound
        , "Lower bound: " <> show lowerBound
        , "Actual size: " <> show actualSize
        ]
  counterexample counterexampleMsg $
    upperBound >= actualSize
      && ( actualSize >= lowerBound
             -- For small certificates, we allow some leeway for the upper bound
             || upperBound <= 512
         )

serializedSize :: PerasCert () -> PerasCertSize
serializedSize = fromIntegral . LazyByteString.length . serialize

{-------------------------------------------------------------------------------
  Arbitrary helpers
-------------------------------------------------------------------------------}

-- * Peras cert

-- We don't care about the phantom type here, so we arbitrarily use ().
genPerasCertWithCustomVoters :: Gen (PerasCert ())
genPerasCertWithCustomVoters = do
  baseCert <- genPerasCert False
  persistentVoters <- chooseInt (1, 1000)
  nonPersistentVoters <- chooseInt (0, 3000)
  pure $
    baseCert
      { pcVoters =
          PerasCertVoters
            . NEMap.fromList
            . fromMaybe (error "test certificates require at least one voter")
            . NonEmpty.nonEmpty
            $ [ (seatIndex voter, PersistentPerasVoteEligibilityProof)
              | voter <- [0 .. persistentVoters - 1]
              ]
              <> [ ( seatIndex voter
                   , NonPersistentPerasVoteEligibilityProof proof
                   )
                 | voter <-
                     [ persistentVoters
                     .. persistentVoters + nonPersistentVoters - 1
                     ]
                 ]
      }
 where
  proof =
    PerasBLSCryptoVRFOutput $
      BLS.signWithRole
        (privateKey (Proxy @BLS.VRF))
        ("proof" :: ByteString)
  privateKey _ =
    fromMaybe (error "invalid BLS private key") $
      BLS.rawDeserialisePrivateKey
        ("TEST" :: ByteString)
        (ByteString.replicate 32 1)
  seatIndex i = PerasSeatIndex (fromIntegral i)
