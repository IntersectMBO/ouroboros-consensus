{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Voting interface for Peras derived from the Praos ledger view.
module Ouroboros.Consensus.Protocol.Praos.Peras
  ( PerasSupportedVotingCommittee (..)
  , PerasVotingError (..)
  , PerasVotingCommittee
  , mkPerasVotingCommittee
  , perasForgeVoteIfEligible
  )
where

import qualified Cardano.Ledger.Shelley.State as SL
import Control.Exception (Exception)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString.Char8 as ByteString
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.Block.SupportsPeras (PerasBoostedBlock, PerasRoundNo)
import Ouroboros.Consensus.Committee.Class (CryptoSupportsVotingCommittee (..))
import qualified Ouroboros.Consensus.Committee.Class as Committee
import Ouroboros.Consensus.Committee.Crypto (PrivateKey, PublicKey)
import qualified Ouroboros.Consensus.Committee.Crypto.BLS as BLS
import Ouroboros.Consensus.Committee.EveryoneVotes
  ( EveryoneVotes
  , VotingCommitteeInput (..)
  )
import Ouroboros.Consensus.Committee.Types
  ( LedgerStake (..)
  , PoolId (..)
  , TargetCommitteeSize
  )
import Ouroboros.Consensus.Committee.WFA
  ( WFAError
  , mkExtWFAStakeDistr
  , wFATiebreakerWithEpochNonce
  )
import Ouroboros.Consensus.Committee.WFALS
  ( VotingCommitteeInput (..)
  , WFALS
  )
import Ouroboros.Consensus.Peras.Crypto.BLS
  ( PerasBLSCrypto
  , PerasPublicKey (..)
  )
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1
import Ouroboros.Consensus.Peras.Voting.Adapter
  ( PerasConversionError
  , PerasVoteCompatibleWithVotingCommittee (..)
  )
import Ouroboros.Consensus.Protocol.Praos
  ( PraosState (..)
  , Ticked (..)
  )
import Ouroboros.Consensus.Protocol.Praos.Views (LedgerView (..))
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- | Voting committee schemes supported by Peras.
--
-- NOTE: this is used internally as a singleton to determine which voting
-- committee scheme to use when constructing votes, certificates, and errors.
data PerasSupportedVotingCommittee committee where
  PerasWFALSVotingCommittee ::
    TargetCommitteeSize ->
    PerasSupportedVotingCommittee WFALS
  PerasEveryoneVotesVotingCommittee ::
    PerasSupportedVotingCommittee EveryoneVotes

-- | Collection of voting-related errors for Peras
data PerasVotingError
  = PerasVotingWFAError
      WFAError
  | PerasVotingWFALSError
      (VotingCommitteeError PerasBLSCrypto WFALS)
  | PerasVotingEveryoneVotesError
      (VotingCommitteeError PerasBLSCrypto EveryoneVotes)
  | PerasVotingConversionError
      PerasConversionError
  | PerasVotingPublicKeyError
      PerasPublicKeyError
  deriving (Show, Exception)

-- | Opaque voting committee for Peras
data PerasVotingCommittee where
  PerasVotingCommittee ::
    CryptoSupportsVotingCommittee PerasBLSCrypto committee =>
    PerasSupportedVotingCommittee committee ->
    VotingCommittee PerasBLSCrypto committee ->
    PerasVotingCommittee

-- | Construct a 'PerasVotingCommittee' from a 'Ticked PraosState'.
--
-- NOTE: the Praos 'LedgerView' needs to be extended with the public keys of the
-- pools in the stake distribution. These are needed to validate Peras votes and
-- certificates.
--
-- FIXME: for now, public keys are read from a JSON file specified by an
-- environment variable using 'unsafePerformIO'. This is a temporary hack until
-- we have a proper solution to retrieve public keys from the ledger state.
mkPerasVotingCommittee ::
  PerasSupportedVotingCommittee committee ->
  Ticked PraosState ->
  Either PerasVotingError PerasVotingCommittee
mkPerasVotingCommittee
  committeeType
  tickedPraosState = do
    let epochNonce =
          praosStateEpochNonce
            . tickedPraosStateChainDepState
            $ tickedPraosState
    let wFATiebreaker =
          wFATiebreakerWithEpochNonce epochNonce
    stakeDistrWithPublicKeys <-
      bimap PerasVotingPublicKeyError id $
        getStakeDistrWithPublicKeys tickedPraosState
    extWFAStakeDistr <-
      bimap PerasVotingWFAError id $
        mkExtWFAStakeDistr
          wFATiebreaker
          stakeDistrWithPublicKeys
    case committeeType of
      PerasWFALSVotingCommittee targetCommitteeSize ->
        bimap PerasVotingWFALSError (PerasVotingCommittee committeeType)
          . Committee.mkVotingCommittee
          $ WFALSVotingCommitteeInput
            epochNonce
            targetCommitteeSize
            extWFAStakeDistr
      PerasEveryoneVotesVotingCommittee ->
        bimap PerasVotingEveryoneVotesError (PerasVotingCommittee committeeType)
          . Committee.mkVotingCommittee
          $ EveryoneVotesVotingCommitteeInput
            extWFAStakeDistr

getStakeDistrWithPublicKeys ::
  Ticked PraosState ->
  Either
    PerasPublicKeyError
    (Map PoolId (LedgerStake, PublicKey PerasBLSCrypto))
getStakeDistrWithPublicKeys tickedPraosState = do
  let stakeDistr =
        Map.mapKeysMonotonic PoolId
          . Map.map (LedgerStake . SL.individualPoolStake)
          . SL.unPoolDistr
          . lvPoolDistr
          . tickedPraosStateLedgerView
          $ tickedPraosState

  publicKeys <- perasPublicKeysFromEnv -- Uses 'unsafePerformIO'
  Map.traverseWithKey (addPublicKey publicKeys) stakeDistr
 where
  addPublicKey publicKeys poolId stake =
    case Map.lookup poolId publicKeys of
      Nothing ->
        failWith $ "Public key not found for pool: " <> show poolId
      Just pk ->
        pure (stake, pk)

  failWith msg =
    Left (PerasPublicKeyError msg)

-- * Retrieveing public keys from a JSON file (temporary)

data PerasPublicKeyError
  = PerasPublicKeyError String
  deriving (Show, Exception)

perasPublicKeysFromEnv :: Either PerasPublicKeyError (Map PoolId PerasPublicKey)
perasPublicKeysFromEnv =
  unsafePerformIO $ do
    lookupEnv envVar >>= \case
      Nothing -> do
        pure $ failWith $ "Environment variable " <> envVar <> " not set."
      Just keysFile -> do
        eitherDecodeFileStrict' keysFile >>= \case
          Left err -> do
            pure $ failWith $ "Failed to parse public keys from file: " <> err
          Right rawKeys -> do
            pure $ decodeKeys rawKeys
 where
  envVar =
    "PERAS_PUBLIC_KEY_FILE"

  keyScope =
    "TESTNET"

  decodeKeys =
    fmap (Map.mapKeysMonotonic PoolId)
      . traverse decodeKey

  decodeKey key =
    case BLS.rawDeserialisePublicKey keyScope (ByteString.pack key) of
      Nothing ->
        failWith $ "Invalid public key format: " <> key
      Just pk ->
        Right $
          PerasPublicKey
            { perasVoteVerKey = BLS.coercePublicKey @BLS.SIGN pk
            , perasVRFVerKey = BLS.coercePublicKey @BLS.VRF pk
            }

  failWith msg =
    Left (PerasPublicKeyError msg)

-- * Partially applied 'CryptoSupportsVotingCommittee' interface

perasForgeVoteIfEligible ::
  PerasVotingCommittee ->
  PoolId ->
  PrivateKey PerasBLSCrypto ->
  PerasRoundNo ->
  PerasBoostedBlock ->
  proxy blk ->
  Either PerasVotingError (Maybe (V1.PerasVote blk))
perasForgeVoteIfEligible
  (PerasVotingCommittee committeeType votingCommittee)
  ourId
  ourPrivateKey
  roundNo
  boostedBlock
  _ =
    case ( Committee.checkShouldVote
             votingCommittee
             ourId
             ourPrivateKey
             roundNo
         ) of
      Left err ->
        case committeeType of
          PerasWFALSVotingCommittee _ ->
            Left (PerasVotingWFALSError err)
          PerasEveryoneVotesVotingCommittee ->
            Left (PerasVotingEveryoneVotesError err)
      Right Nothing ->
        Right Nothing
      Right (Just witness) -> do
        let abstractVote =
              Committee.forgeVote
                witness
                ourPrivateKey
                roundNo
                boostedBlock
        case committeeType of
          PerasWFALSVotingCommittee _ ->
            case toPerasVote abstractVote of
              Left err ->
                Left $ PerasVotingConversionError err
              Right perasVote ->
                Right (Just perasVote)
          PerasEveryoneVotesVotingCommittee ->
            case toPerasVote abstractVote of
              Left err ->
                Left $ PerasVotingConversionError err
              Right perasVote ->
                Right (Just perasVote)
