{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Temporary hack for retrieving BLS public keys from a JSON file specified
-- in the environment variable 'PERAS_PUBLIC_KEY_FILE'.
--
-- WARNING: this is a temporary hack for testing purposes, and should not be
-- used under any circumstances in production. This will be replaced with proper
-- on-chain key registration in the future.
module Ouroboros.Consensus.Peras.Crypto.BLS.Unsafe
  ( unsafeExtendPerasStakeDistrWithPublicKeysFromEnv
  , unsafePerasBLSPublicKeysFromEnv
  ) where

import Cardano.Ledger.State (IndividualPoolStake (..), PoolDistr (..))
import Data.Aeson (eitherDecodeFileStrict')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString (..))
import qualified Ouroboros.Consensus.Committee.Crypto.BLS as BLS
import Ouroboros.Consensus.Committee.Types (LedgerStake (..), PoolId (..))
import Ouroboros.Consensus.Peras.Crypto.BLS (PerasPublicKey (..))
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- | Extend a given 'PoolDistr' with the corresponding BLS public keys for each
-- pool retrieved from the JSON file specified in the environment variable
-- 'PERAS_PUBLIC_KEY_FILE'.
--
-- WARNING: this is a temporary hack for testing purposes, and should not be
-- used under any circumstances in production. This will be replaced with proper
-- on-chain key registration in the future.
unsafeExtendPerasStakeDistrWithPublicKeysFromEnv ::
  PoolDistr ->
  Either
    String
    (Map PoolId (LedgerStake, PerasPublicKey))
unsafeExtendPerasStakeDistrWithPublicKeysFromEnv poolDistr = do
  publicKeys <- unsafePerasBLSPublicKeysFromEnv -- uses 'unsafePerformIO'
  Map.traverseWithKey
    ( \poolId stake -> do
        let ledgerStake =
              LedgerStake (individualPoolStake stake)
        case Map.lookup poolId publicKeys of
          Nothing ->
            Left ("Public key not found for pool: " <> show poolId)
          Just publicKey ->
            Right (ledgerStake, publicKey)
    )
    . Map.mapKeysMonotonic PoolId
    . unPoolDistr
    $ poolDistr

-- | Retrieve the BLS public keys for the Peras committee members from a JSON
-- file specified in the environment variable 'PERAS_PUBLIC_KEY_FILE'.
--
-- WARNING: this is a temporary hack for testing purposes, and should not be
-- used under any circumstances in production. This will be replaced with proper
-- on-chain key registration in the future.
unsafePerasBLSPublicKeysFromEnv :: Either String (Map PoolId PerasPublicKey)
unsafePerasBLSPublicKeysFromEnv =
  unsafePerformIO $
    lookupEnv envVar >>= \case
      Nothing -> do
        pure $ Left $ "Environment variable " <> envVar <> " not set."
      Just keysFile -> do
        eitherDecodeFileStrict' keysFile >>= \case
          Left err -> do
            pure $ Left $ "Failed to parse public keys from file: " <> err
          Right rawKeys -> do
            pure $ traverse decodeKey (Map.mapKeysMonotonic PoolId rawKeys)
 where
  envVar =
    "PERAS_PUBLIC_KEY_FILE"

  keyScope =
    "TESTNET"

  decodeKey key =
    case BLS.rawDeserialisePublicKey keyScope (fromString key) of
      Nothing ->
        Left $ "Invalid public key format: " <> key
      Just pk ->
        Right $ PerasPublicKey pk
{-# NOINLINE unsafePerasBLSPublicKeysFromEnv #-}
