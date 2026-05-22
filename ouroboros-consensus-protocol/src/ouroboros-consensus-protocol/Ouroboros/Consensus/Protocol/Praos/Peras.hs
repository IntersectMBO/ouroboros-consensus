{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Voting interface for Peras derived from the Praos ledger view.
module Ouroboros.Consensus.Protocol.Praos.Peras
  ( PraosStateSupportsPerasVoting (..)
  , getStakeDistrWithBLSPublicKeys
  , perasBLSPublicKeysFromEnv
  ) where

import qualified Cardano.Ledger.Shelley.State as SL
import Data.Aeson (eitherDecodeFileStrict')
import Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Short (ShortByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.Block.Abstract (HeaderHash, StandardHash)
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , PerasCommitteeScheme
  , PerasCrypto
  , PerasParams
  , PerasVotingCommittee
  , injectCommitteeError
  )
import Ouroboros.Consensus.Committee.Class (CryptoSupportsVotingCommittee (..))
import qualified Ouroboros.Consensus.Committee.Class as Committee
import Ouroboros.Consensus.Committee.Crypto (PublicKey)
import qualified Ouroboros.Consensus.Committee.Crypto.BLS as BLS
import Ouroboros.Consensus.Committee.Types
  ( LedgerStake (..)
  , PoolId (..)
  , TargetCommitteeSize (..)
  )
import Ouroboros.Consensus.Committee.WFA
  ( mkExtWFAStakeDistr
  , wFATiebreakerWithEpochNonce
  )
import Ouroboros.Consensus.Committee.WFALS
  ( VotingCommitteeInput (..)
  , WFALS
  )
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
import qualified Ouroboros.Consensus.Peras.Error.V1 as V1
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1
import Ouroboros.Consensus.Protocol.Praos
  ( PraosState (..)
  , Ticked (..)
  )
import Ouroboros.Consensus.Protocol.Praos.Views (LedgerView (..))
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- This is a mocked up instance

data RealBlock

type instance PerasCrypto RealBlock = BLS.PerasBLSCrypto
type instance PerasCommitteeScheme RealBlock = WFALS
type instance HeaderHash RealBlock = ShortByteString

instance StandardHash RealBlock

instance BlockSupportsPeras RealBlock where
  type PerasVote RealBlock = V1.PerasVote RealBlock
  type PerasCert RealBlock = V1.PerasCert RealBlock
  type PerasError RealBlock = V1.PerasError RealBlock

  -- TODO: uncomment as soon as we add this method to 'BlockSupportsPeras'
  -- forgePerasVoteIfEligible = implPerasForgeVoteIfEligible
  forgePerasCert = undefined
  validatePerasVote = undefined
  validatePerasCert = undefined

instance PraosStateSupportsPerasVoting RealBlock where
  praosStatePerasVotingCommitteeInput _ _perasParams tickedPraosState = do
    let epochNonce =
          praosStateEpochNonce
            . tickedPraosStateChainDepState
            $ tickedPraosState
    let wFATiebreaker =
          wFATiebreakerWithEpochNonce epochNonce
    stakeDistrWithPublicKeys <-
      first V1.PerasTemporaryPublicKeyHackError $
        getStakeDistrWithBLSPublicKeys tickedPraosState
    extWFAStakeDistr <-
      first V1.PerasVotingWFAError $
        mkExtWFAStakeDistr
          wFATiebreaker
          stakeDistrWithPublicKeys
    let targetCommitteeSize = TargetCommitteeSize 100 -- TODO: use perams params to get this value instead
    pure $
      WFALSVotingCommitteeInput
        epochNonce
        targetCommitteeSize
        extWFAStakeDistr

--------------------------------------------------------------------------------
-- Helpers to deal with public keys and stake
--------------------------------------------------------------------------------

getStakeDistrWithBLSPublicKeys ::
  Ticked PraosState ->
  Either
    String
    (Map PoolId (LedgerStake, PublicKey BLS.PerasBLSCrypto))
getStakeDistrWithBLSPublicKeys tickedPraosState = do
  let stakeDistr =
        Map.mapKeysMonotonic PoolId
          . Map.map (LedgerStake . SL.individualPoolStake)
          . SL.unPoolDistr
          . lvPoolDistr
          . tickedPraosStateLedgerView
          $ tickedPraosState

  publicKeys <- perasBLSPublicKeysFromEnv -- Uses 'unsafePerformIO'
  Map.traverseWithKey (addPublicKey publicKeys) stakeDistr
 where
  addPublicKey publicKeys poolId stake =
    case Map.lookup poolId publicKeys of
      Nothing ->
        Left $ "Public key not found for pool: " <> show poolId
      Just pk ->
        pure (stake, pk)

-- * Retrieveing public keys from a JSON file (temporary)

perasBLSPublicKeysFromEnv :: Either String (Map PoolId BLS.PerasPublicKey)
{-# NOINLINE perasBLSPublicKeysFromEnv #-}
perasBLSPublicKeysFromEnv =
  unsafePerformIO $
    lookupEnv envVar >>= \case
      Nothing -> do
        pure $ Left $ "Environment variable " <> envVar <> " not set."
      Just keysFile -> do
        eitherDecodeFileStrict' keysFile >>= \case
          Left err -> do
            pure $ Left $ "Failed to parse public keys from file: " <> err
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
        Left $ "Invalid public key format: " <> key
      Just pk ->
        Right $
          BLS.PerasPublicKey
            { BLS.perasVoteVerKey = BLS.coercePublicKey @BLS.SIGN pk
            , BLS.perasVRFVerKey = BLS.coercePublicKey @BLS.VRF pk
            }

--------------------------------------------------------------------------------

class
  ( BlockSupportsPeras blk
  , CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasCommitteeScheme blk) -- TODO remove this constraint when it becomes a superclass constraint of 'BlockSupportsPeras'
  ) =>
  PraosStateSupportsPerasVoting blk
  where
  -- | How to extract a 'PerasVotingCommitteeInput' from a 'Ticked PraosState'.
  -- This is used to construct the 'PerasVotingCommittee' used for voting at a given ledger/praos state.
  praosStatePerasVotingCommitteeInput ::
    proxy blk ->
    PerasParams ->
    Ticked PraosState ->
    Either
      (PerasError blk)
      (VotingCommitteeInput (PerasCrypto blk) (PerasCommitteeScheme blk))

  -- | How to build a new 'PerasVotingCommittee' from a 'Ticked PraosState'. The implementation provided here relies on 'praosStatePerasVotingCommitteeInput'.
  praosStateGetPerasVotingCommittee ::
    proxy blk ->
    PerasParams ->
    Ticked PraosState ->
    Either
      (PerasError blk)
      (PerasVotingCommittee blk)
  praosStateGetPerasVotingCommittee p perasParams tickedPraosState = do
    committeeInput <-
      praosStatePerasVotingCommitteeInput p perasParams tickedPraosState
    first injectCommitteeError $
      Committee.mkVotingCommittee committeeInput
