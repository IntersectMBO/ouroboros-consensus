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
import Data.Word (Word16)
import qualified Ouroboros.Consensus.Committee.Crypto.BLS as BLS
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import Ouroboros.Consensus.Peras.Crypto.BLS (VRFOutput (..))
import Ouroboros.Consensus.Peras.Types (PerasSeatIndex (..))
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Test.Util.Peras (genPerasCert)

persistentVoterCounts :: [Int]
persistentVoterCounts = 0 : [100, 200 .. 800]

maxNonPersistentVoters :: Int
maxNonPersistentVoters = 3000

maxVoters :: Int
maxVoters = maximum persistentVoterCounts + maxNonPersistentVoters

outputFile :: FilePath
outputFile = "peras_cert_sizes.csv"

main :: IO ()
main = do
  putStrLn $ "Writing Peras certificate sizes to " <> outputFile
  writeFile outputFile $
    unlines $
      "persistent_voters,non_persistent_voters,size"
        : [ show persistent
              <> ","
              <> show nonPersistent
              <> ","
              <> show (measure persistent nonPersistent)
          | persistent <- persistentVoterCounts
          , nonPersistent <- [0 .. maxNonPersistentVoters]
          , persistent + nonPersistent > 0
          ]

measure :: Int -> Int -> Int
measure persistentVoters nonPersistentVoters =
  fromIntegral . LazyByteString.length . serialize $
    mkCert persistentVoters nonPersistentVoters

mkCert :: Int -> Int -> V1.PerasCert ()
mkCert persistentVoters nonPersistentVoters =
  fixture
    { V1.pcVoters =
        V1.PerasCertVoters
          . NEMap.fromList
          . fromMaybe (error "benchmark certificates require at least one voter")
          . NonEmpty.nonEmpty
          $ [ (seatIndex voter, V1.PersistentPerasVoteEligibilityProof)
            | voter <- [0 .. persistentVoters - 1]
            ]
            <> [ ( seatIndex voter
                 , V1.NonPersistentPerasVoteEligibilityProof proof
                 )
               | voter <-
                   [ persistentVoters
                   .. persistentVoters + nonPersistentVoters - 1
                   ]
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

seatIndex :: Int -> PerasSeatIndex
seatIndex voter
  | voter < 0 || voter >= maxVoters =
      error $ "Peras seat index outside benchmark bounds: " <> show voter
  | voter > fromIntegral (maxBound :: Word16) =
      error $ "Peras seat index does not fit in Word16: " <> show voter
  | otherwise = PerasSeatIndex (fromIntegral voter)
