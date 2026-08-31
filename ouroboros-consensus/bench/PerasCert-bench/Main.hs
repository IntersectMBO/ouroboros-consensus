{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Measure the serialized size of V1 Peras certificates.
--
-- The certificates made here are serialization-valid fixtures only: their BLS
-- signatures are not constructed to be consensus-valid for the synthetic
-- voters. Each reported size comes from serializing a constructed certificate,
-- rather than from a size formula.
module Main (main) where

import Cardano.Binary (serialize)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.NonEmpty as NEMap
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import qualified Ouroboros.Consensus.Committee.Crypto.BLS as BLS
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import Ouroboros.Consensus.Peras.Crypto.BLS (VRFOutput (..))
import Ouroboros.Consensus.Peras.Types (PerasSeatIndex (..))
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Test.Util.Peras (genPerasCert)

maxNonPersistentVoters :: Int
maxNonPersistentVoters = 3000

outputFile :: FilePath
outputFile = "peras_cert_sizes.csv"

main :: IO ()
main = do
  putStrLn $ "Writing Peras certificate sizes to " <> outputFile
  writeFile outputFile $
    unlines
      [ "non_persistent_voters,size"
      , unlines
          [ show n <> "," <> show size
          | n <- [0 .. maxNonPersistentVoters]
          , let size = measure n
          ]
      ]

measure :: Int -> Int
measure = fromIntegral . LazyByteString.length . serialize . mkCert

mkCert :: Int -> V1.PerasCert ()
mkCert nonPersistentVoters =
  fixture
    { V1.pcVoters =
        V1.PerasCertVoters
          . NEMap.fromList
          . NonEmpty.fromList
          $ [ ( PerasSeatIndex (fromIntegral voter)
              , V1.NonPersistentPerasVoteEligibilityProof proof
              )
            | voter <- [0 .. nonPersistentVoters]
            ]
    }
 where
  fixture =
    unGen (genPerasCert False) (mkQCGen 0) 100
  proof =
    PerasBLSCryptoVRFOutput $
      BLS.signWithRole
        (privateKey (Proxy @BLS.VRF))
        ("proof" :: ByteString)
  privateKey _ =
    fromMaybe (error "invalid BLS private key") $
      BLS.rawDeserialisePrivateKey
        "BENCH"
        (ByteString.replicate 32 1)
