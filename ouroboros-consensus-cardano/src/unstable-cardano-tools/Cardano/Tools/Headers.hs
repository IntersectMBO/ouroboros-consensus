{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Tooling to generate and validate (Praos) headers.
module Cardano.Tools.Headers where

import           Cardano.Crypto.DSIGN (deriveVerKeyDSIGN)
import           Cardano.Crypto.Hash (Blake2b_256, hashToBytes)
import           Cardano.Crypto.VRF
                     (VRFAlgorithm (deriveVerKeyVRF, hashVerKeyVRF))
import           Cardano.Ledger.Api (ConwayEra, StandardCrypto, VRF)
import           Cardano.Ledger.BaseTypes (BoundedRational (boundRational),
                     PositiveUnitInterval, mkActiveSlotCoeff)
import           Cardano.Ledger.Coin (Coin (..))
import           Cardano.Ledger.Compactible (toCompact)
import           Cardano.Ledger.Keys (VKey (..), hashKey)
import           Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import           Cardano.Prelude (ExitCode (..), exitWith, forM_, hPutStrLn,
                     stderr)
import           Control.Monad.Except (runExcept)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Data.Text (unpack)
import           Data.Text.Encoding (decodeUtf8)
import           Debug.Trace (trace)
import           Ouroboros.Consensus.Block (validateView)
import           Ouroboros.Consensus.Protocol.Praos (Praos,
                     doValidateKESSignature, doValidateVRFSignature)
import qualified Ouroboros.Consensus.Protocol.Praos.Views as Views
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     mkShelleyHeader)
import           Ouroboros.Consensus.Shelley.Protocol.Praos ()
import           Test.Ouroboros.Consensus.Protocol.Praos.Header
                     (GeneratorContext (..), MutatedHeader (..), Mutation (..),
                     Sample (..), expectedError, generateSamples, header,
                     mutation)

type ConwayBlock = ShelleyBlock (Praos StandardCrypto) (ConwayEra StandardCrypto)

-- * Running Generator
data Options
    = Generate Int
    | Validate

run :: Options -> IO ()
run = \case
    Generate n -> do
        sample <- generateSamples n
        LBS.putStr $ Json.encode sample <> "\n"
    Validate ->
        Json.eitherDecode <$> LBS.getContents >>= \case
            Left err -> hPutStrLn stderr err >> exitWith (ExitFailure 1)
            Right Sample{sample} ->
                forM_ sample $ \(context, mutatedHeader) -> do
                    print $ validate context mutatedHeader

data ValidationResult = Valid !Mutation | Invalid !Mutation !String
    deriving (Eq, Show)

validate :: GeneratorContext -> MutatedHeader -> ValidationResult
validate context MutatedHeader{header, mutation} =
    case (runExcept $ validateKES >> validateVRF, mutation) of
        (Left err, mut) | expectedError mut err -> Valid mut
        (Left err, mut) -> Invalid mut (show err)
        (Right _, NoMutation) -> Valid NoMutation
        (Right _, mut) -> Invalid mut $ "Expected error from mutation " <> show mut <> ", but validation succeeded"
  where
    GeneratorContext{praosSlotsPerKESPeriod, nonce, coldSignKey, vrfSignKey, ocertCounters, activeSlotCoeff} = context
    -- TODO: get these from the context
    maxKESEvo = 62
    coin = fromJust . toCompact . Coin
    ownsAllStake vrfKey = IndividualPoolStake 1 (coin 1) vrfKey
    poolDistr = Map.fromList [(poolId, ownsAllStake hashVRFKey)]
    poolId = hashKey $ VKey $ deriveVerKeyDSIGN coldSignKey
    hashVRFKey = hashVerKeyVRF $ deriveVerKeyVRF vrfSignKey

    headerView = validateView @ConwayBlock undefined (mkShelleyHeader header)
    validateKES = doValidateKESSignature maxKESEvo praosSlotsPerKESPeriod poolDistr ocertCounters headerView
    validateVRF = doValidateVRFSignature nonce poolDistr activeSlotCoeff headerView
