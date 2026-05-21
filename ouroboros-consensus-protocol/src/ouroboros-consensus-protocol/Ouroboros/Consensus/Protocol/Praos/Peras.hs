{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Voting interface for Peras derived from the Praos ledger view.
module Ouroboros.Consensus.Protocol.Praos.Peras where

import qualified Cardano.Ledger.Shelley.State as SL
import Control.Exception (Exception)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Short (ShortByteString)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (HeaderHash, StandardHash)
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , PerasBoostedBlock
  , PerasCommitteeScheme
  , PerasCrypto
  , PerasParams
  , PerasRoundNo
  )
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
  , TargetCommitteeSize (..)
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
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
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
  type PerasError RealBlock = V1PerasError RealBlock
  forgePerasCert = undefined
  validatePerasVote = undefined
  validatePerasCert = undefined

-- | Collection of voting-related errors for Peras
data V1PerasError blk
  = PerasVotingWFAError
      WFAError
  | PerasVotingCommitteeError
      (VotingCommitteeError (PerasCrypto blk) (PerasCommitteeScheme blk))
  | PerasVotingConversionError
      PerasConversionError
  | PerasVotingPublicKeyError
      PerasPublicKeyError

deriving instance
  Show (VotingCommitteeError (PerasCrypto blk) (PerasCommitteeScheme blk)) =>
  Show (V1PerasError blk)
deriving instance
  Eq (VotingCommitteeError (PerasCrypto blk) (PerasCommitteeScheme blk)) =>
  Eq (V1PerasError blk)
deriving instance
  NoThunks (VotingCommitteeError (PerasCrypto blk) (PerasCommitteeScheme blk)) =>
  NoThunks (V1PerasError blk)
deriving instance
  Generic (V1PerasError blk)

--
-- class
--   BlockSupportsPeras blk =>
--   PraosStateSupportsPerasVoting blk
--   where
--   praosStatePerasVotingCommitteeInput ::
--     proxy blk ->
--     PerasParams ->
--     Ticked PraosState ->
--     Either
--       (PerasError blk)
--       (VotingCommitteeInput (PerasCrypto blk) (PerasCommitteeScheme blk))
--
--   praosStateGetPerasVotingCommittee ::
--     proxy blk ->
--     PerasParams ->
--     Ticked PraosState ->
--     Either
--       (PerasError blk)
--       (PerasVotingCommittee blk)
--   praosStateGetPerasVotingCommittee p perasParams tickedPraosState = do
--     committeeInput <-
--       praosStatePerasVotingCommitteeInput p perasParams tickedPraosState
--     bimap PerasVotingCommitteeError $
--       Committee.mkVotingCommittee committeeInput
--
-- instance PraosStateSupportsPerasVoting RealBlock where
--   praosStatePerasVotingCommitteeInput _ perasParams tickedPraosState = do
--     let epochNonce =
--           praosStateEpochNonce
--             . tickedPraosStateChainDepState
--             $ tickedPraosState
--     let wFATiebreaker =
--           wFATiebreakerWithEpochNonce epochNonce
--     stakeDistrWithPublicKeys <-
--       bimap PerasVotingPublicKeyError id $
--         getStakeDistrWithPublicKeys tickedPraosState
--     extWFAStakeDistr <-
--       bimap PerasVotingWFAError id $
--         mkExtWFAStakeDistr
--           wFATiebreaker
--           stakeDistrWithPublicKeys
--     let targetCommitteeSize = TargetCommitteeSize 100 -- hack
--     pure $
--       WFALSVotingCommitteeInput
--         epochNonce
--         targetCommitteeSize
--         extWFAStakeDistr
--
-- getStakeDistrWithPublicKeys ::
--   Ticked PraosState ->
--   Either
--     PerasPublicKeyError
--     (Map PoolId (LedgerStake, PublicKey PerasBLSCrypto))
-- getStakeDistrWithPublicKeys tickedPraosState = do
--   let stakeDistr =
--         Map.mapKeysMonotonic PoolId
--           . Map.map (LedgerStake . SL.individualPoolStake)
--           . SL.unPoolDistr
--           . lvPoolDistr
--           . tickedPraosStateLedgerView
--           $ tickedPraosState
--
--   publicKeys <- perasPublicKeysFromEnv -- Uses 'unsafePerformIO'
--   Map.traverseWithKey (addPublicKey publicKeys) stakeDistr
--  where
--   addPublicKey publicKeys poolId stake =
--     case Map.lookup poolId publicKeys of
--       Nothing ->
--         failWith $ "Public key not found for pool: " <> show poolId
--       Just pk ->
--         pure (stake, pk)
--
--   failWith msg =
--     Left (PerasPublicKeyError msg)
--

-- * Retrieveing public keys from a JSON file (temporary)

data PerasPublicKeyError
  = PerasPublicKeyError String
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

perasPublicKeysFromEnv :: Either PerasPublicKeyError (Map PoolId BLS.PerasPublicKey)
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
          BLS.PerasPublicKey
            { BLS.perasVoteVerKey = BLS.coercePublicKey @BLS.SIGN pk
            , BLS.perasVRFVerKey = BLS.coercePublicKey @BLS.VRF pk
            }

  failWith msg =
    Left (PerasPublicKeyError msg)

--------------------------------------------------------------------------------
-- This needs to stay in this file

-- | Construct a 'PerasVotingCommittee' from a 'Ticked PraosState'.
--
-- NOTE: the Praos 'LedgerView' needs to be extended with the public keys of the
-- pools in the stake distribution. These are needed to validate Peras votes and
-- certificates.
--
-- FIXME: for now, public keys are read from a JSON file specified by an
-- environment variable using 'unsafePerformIO'. This is a temporary hack until
-- we have a proper solution to retrieve public keys from the ledger state.
-- mkPerasVotingCommittee ::
--   CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasCommitteeScheme blk) =>
--   VotingCommitteeInput (PerasCrypto blk) (PerasCommitteeScheme blk) ->
--   Ticked PraosState ->
--   Either
--     (PerasVotingError blk)
--     (PerasVotingCommittee blk)
-- mkPerasVotingCommittee
--   committeeInput
--   tickedPraosState = do
--     let epochNonce =
--           praosStateEpochNonce
--             . tickedPraosStateChainDepState
--             $ tickedPraosState
--     let wFATiebreaker =
--           wFATiebreakerWithEpochNonce epochNonce
--     stakeDistrWithPublicKeys <-
--       bimap PerasVotingPublicKeyError id $
--         getStakeDistrWithPublicKeys tickedPraosState
--     extWFAStakeDistr <-
--       bimap PerasVotingWFAError id $
--         mkExtWFAStakeDistr
--           wFATiebreaker
--           stakeDistrWithPublicKeys
--     bimap PerasVotingCommitteeError id
--       . Committee.mkVotingCommittee
--       $ WFALSVotingCommitteeInput
--         epochNonce
--         targetCommitteeSize
--         extWFAStakeDistr

-- * Partially applied 'CryptoSupportsVotingCommittee' interface

-- perasForgeVoteIfEligible ::
--   PerasVotingCommittee ->
--   PoolId ->
--   PrivateKey PerasBLSCrypto ->
--   PerasRoundNo ->
--   PerasBoostedBlock ->
--   proxy blk ->
--   Either PerasVotingError (Maybe (V1.PerasVote blk))
-- perasForgeVoteIfEligible
--   (PerasVotingCommittee committeeType votingCommittee)
--   ourId
--   ourPrivateKey
--   roundNo
--   boostedBlock
--   _ =
--     case ( Committee.checkShouldVote
--              votingCommittee
--              ourId
--              ourPrivateKey
--              roundNo
--          ) of
--       Left err ->
--         case committeeType of
--           PerasWFALSVotingCommittee _ ->
--             Left (PerasVotingWFALSError err)
--           PerasEveryoneVotesVotingCommittee ->
--             Left (PerasVotingEveryoneVotesError err)
--       Right Nothing ->
--         Right Nothing
--       Right (Just witness) -> do
--         let abstractVote =
--               Committee.forgeVote
--                 witness
--                 ourPrivateKey
--                 roundNo
--                 boostedBlock
--         case committeeType of
--           PerasWFALSVotingCommittee _ ->
--             case toPerasVote abstractVote of
--               Left err ->
--                 Left (PerasVotingConversionError err)
--               Right perasVote ->
--                 Right (Just perasVote)
--           PerasEveryoneVotesVotingCommittee ->
--             case toPerasVote abstractVote of
--               Left err ->
--                 Left (PerasVotingConversionError err)
--               Right perasVote ->
--                 Right (Just perasVote)
