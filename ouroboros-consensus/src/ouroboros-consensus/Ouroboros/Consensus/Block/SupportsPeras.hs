{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Block.SupportsPeras
  ( -- * Voting committee types for Peras
    PerasVotingCommittee
  , PerasVotingCommitteeError
  , PerasVotingCommitteeInput

    -- * Epoch-dependent context for Peras
  , PerasEpochContext (..)

    -- * BlockSupportsPeras class
  , BlockSupportsPeras (..)

    -- * Helpers to derive @BlockSupportsPeras@ for block types without Peras support
  , VoidPerasVote (..)
  , VoidPerasCert (..)
  , VoidPerasError (..)
  , VoidPerasVotingCommitteeScheme
  , absurdVoidPerasVotingCommitteeError
  , absurdVoidPerasCert

    -- * Validated types
  , ValidatedPerasVote (..)
  , ValidatedPerasCert (..)

    -- * Convenience projection/injection classes
  , IsPerasVote (..)
  , getPerasVoteId
  , getPerasVoteTarget
  , IsPerasCert (..)
  , IsPerasError (..)

    -- * Types and functions related to Peras vote collection and quorum checking
  , PerasVoteCollection
    ( pvcTarget
    , pvcVotes
    , pvcTotalWeight
    )
  , perasVoteCollectionSingleton
  , perasVoteCollectionAddVote
  , unsafePerasVoteCollection
  , PerasVoteCollectionWithQuorum
    ( forgetQuorum
    )
  , unsafeAssumeQuorum
  , perasVoteCollectionCheckQuorum
  , toUniqueVotesWithSameTarget

    -- * Helpers
  , weightAboveThreshold

    -- * Convenience re-exports
  , module Ouroboros.Consensus.Peras.Params
  , module Ouroboros.Consensus.Peras.Types
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeListLenOf, encodeListLen)
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Hashes (KeyHash (..))
import Control.Exception (assert)
import Control.Exception.Base (Exception)
import Data.Bifunctor (bimap)
import Data.Containers.NonEmpty (NE)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.NonEmpty as NEMap
import Data.Map.Strict (Map)
import Data.Traversable (for)
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (Point, StandardHash)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime (..))
import Ouroboros.Consensus.Committee.Class
  ( CryptoSupportsVotingCommittee (..)
  , UniqueVotesWithSameTarget
  , VotingCommittee
  , getRawVotes
  , unsafeUniqueVotesWithSameTarget
  )
import qualified Ouroboros.Consensus.Committee.Class as Committee
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsVoteSigning (..)
  , ElectionId
  , PrivateKey
  , PublicKey
  , VoteCandidate
  )
import Ouroboros.Consensus.Committee.Types (PoolId (..))
import Ouroboros.Consensus.Peras.Cert.Class (IsPerasCert (..))
import Ouroboros.Consensus.Peras.Params
import Ouroboros.Consensus.Peras.Types
import Ouroboros.Consensus.Peras.Vote.Class
  ( IsPerasVote (..)
  , getPerasVoteId
  , getPerasVoteTarget
  )
import Ouroboros.Consensus.Peras.Voting.Adapter
  ( PerasCertCompatibleWithVotingCommittee (..)
  , PerasConversionError
  , PerasVoteCompatibleWithVotingCommittee (..)
  )
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.Orphans ()
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- * Voting committee types for Peras

-- | Voting committee for Peras indexed by block type
type PerasVotingCommittee blk =
  VotingCommittee
    (PerasCrypto blk)
    (PerasVotingCommitteeScheme blk)

-- | Error type for Peras voting committee errors
type PerasVotingCommitteeError blk =
  VotingCommitteeError
    (PerasCrypto blk)
    (PerasVotingCommitteeScheme blk)

-- | Input needed to build a Peras voting committee
type PerasVotingCommitteeInput blk =
  VotingCommitteeInput
    (PerasCrypto blk)
    (PerasVotingCommitteeScheme blk)

-- * Epoch-dependent context for Peras

-- | Epoch-dependent context used for forging and validation of objects.
data PerasEpochContext blk
  = PerasEpochContext
  { pecCommittee :: PerasVotingCommittee blk
  , pecParams :: PerasParams blk
  }

instance
  ( Typeable blk
  , FromCBOR (PerasVotingCommittee blk)
  ) =>
  FromCBOR (PerasEpochContext blk)
  where
  fromCBOR = do
    decodeListLenOf 2
    pecCommittee <- fromCBOR
    pecParams <- fromCBOR
    pure
      PerasEpochContext
        { pecCommittee
        , pecParams
        }

instance
  ( Typeable blk
  , ToCBOR (PerasVotingCommittee blk)
  ) =>
  ToCBOR (PerasEpochContext blk)
  where
  toCBOR
    PerasEpochContext
      { pecCommittee
      , pecParams
      } =
      encodeListLen 2
        <> toCBOR pecCommittee
        <> toCBOR pecParams

deriving instance
  Show (PerasVotingCommittee blk) =>
  Show (PerasEpochContext blk)
deriving instance
  Eq (PerasVotingCommittee blk) =>
  Eq (PerasEpochContext blk)
deriving instance
  NoThunks (PerasVotingCommittee blk) =>
  NoThunks (PerasEpochContext blk)
deriving instance
  Generic (PerasEpochContext blk)

-- * BlockSupportsPeras class

class
  ( -- Basic block constraints
    StandardHash blk
  , Typeable blk
  , -- PerasVote constraints
    Typeable (PerasVote blk)
  , Show (PerasVote blk)
  , Eq (PerasVote blk)
  , NoThunks (PerasVote blk)
  , IsPerasVote (PerasVote blk) blk
  , Typeable (BoostedBlock (PerasVote blk))
  , Show (BoostedBlock (PerasVote blk))
  , Eq (BoostedBlock (PerasVote blk))
  , NoThunks (BoostedBlock (PerasVote blk))
  , -- PerasCert constraints
    Typeable (PerasCert blk)
  , Show (PerasCert blk)
  , Eq (PerasCert blk)
  , NoThunks (PerasCert blk)
  , IsPerasCert (PerasCert blk) blk
  , Typeable (BoostedBlock (PerasCert blk))
  , Show (BoostedBlock (PerasCert blk))
  , Eq (BoostedBlock (PerasCert blk))
  , NoThunks (BoostedBlock (PerasCert blk))
  , -- PerasError constraints
    Typeable (PerasError blk)
  , Show (PerasError blk)
  , Eq (PerasError blk)
  , NoThunks (PerasError blk)
  , IsPerasError (PerasError blk) blk
  , Exception (PerasError blk)
  , -- PerasVotingCommittee constraints
    Typeable (PerasVotingCommittee blk)
  , Show (PerasVotingCommittee blk)
  , Eq (PerasVotingCommittee blk)
  , NoThunks (PerasVotingCommittee blk)
  , -- PerasEpochContext constraints
    Typeable (PerasEpochContext blk)
  , Show (PerasEpochContext blk)
  , Eq (PerasEpochContext blk)
  , NoThunks (PerasEpochContext blk)
  , -- Compatiblity with committee/crypto
    Show (PerasCrypto blk)
  , Eq (PerasCrypto blk)
  , Typeable (PerasCrypto blk)
  , NoThunks (PerasCrypto blk)
  , Show (PerasVotingCommitteeScheme blk)
  , Eq (PerasVotingCommitteeScheme blk)
  , Typeable (PerasVotingCommitteeScheme blk)
  , NoThunks (PerasVotingCommitteeScheme blk)
  , ElectionId (PerasCrypto blk) ~ PerasRoundNo
  , VoteCandidate (PerasCrypto blk) ~ BoostedBlock (PerasVote blk)
  , VoteCandidate (PerasCrypto blk) ~ BoostedBlock (PerasCert blk)
  ) =>
  BlockSupportsPeras blk
  where
  -- | The concrete Peras vote type for this block type.
  type PerasVote blk = (vote :: Type) | vote -> blk

  type PerasVote blk = VoidPerasVote blk

  -- | The concrete Peras certificate type for this block type.
  type PerasCert blk = (cert :: Type) | cert -> blk

  type PerasCert blk = VoidPerasCert blk

  -- | The concrete Peras error type for this block type.
  type PerasError blk = (err :: Type) | err -> blk

  type PerasError blk = VoidPerasError blk

  -- | The crypto scheme used for Peras votes and certificates.
  --
  -- Used to dispatch a block type to a its corresponding voting crypto scheme.
  type PerasCrypto blk :: Type

  type PerasCrypto blk = VoidPerasCrypto blk

  -- | The voting committee scheme used for Peras.
  --
  -- Used to dispatch a block type to a its corresponding voting committee scheme.
  type PerasVotingCommitteeScheme blk :: Type

  type PerasVotingCommitteeScheme blk = VoidPerasVotingCommitteeScheme

  -- | Forge a Peras vote if the given pool is eligible to vote in the given round.
  forgePerasVoteIfEligible ::
    PerasEpochContext blk ->
    PoolId ->
    PrivateKey (PerasCrypto blk) ->
    PerasRoundNo ->
    Point blk ->
    Either (PerasError blk) (Maybe (ValidatedPerasVote blk))
  default forgePerasVoteIfEligible ::
    ( CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)
    , PerasVoteCompatibleWithVotingCommittee
        (PerasVote blk)
        (PerasCrypto blk)
        (PerasVotingCommitteeScheme blk)
    ) =>
    PerasEpochContext blk ->
    PoolId ->
    PrivateKey (PerasCrypto blk) ->
    PerasRoundNo ->
    Point blk ->
    Either (PerasError blk) (Maybe (ValidatedPerasVote blk))
  forgePerasVoteIfEligible context ourId ourPrivateKey roundNo point = do
    let committee = pecCommittee context
    mbWitness <-
      bimap injectVotingCommitteeError id $
        Committee.checkShouldVote committee ourId ourPrivateKey roundNo
    for mbWitness $ \witness -> do
      let voteWeight = eligiblePartyVoteWeight committee witness
      let boostedBlock = pointToBoostedBlock point
      let abstractVote = Committee.forgeVote witness ourPrivateKey roundNo boostedBlock
      concreteVote <-
        bimap injectConversionError id $
          toPerasVote @(PerasVote blk) abstractVote
      pure $
        ValidatedPerasVote
          { vpvVote = concreteVote
          , vpvVoteWeight = voteWeight
          }

  -- | Verify a Peras vote and return its weight if valid.
  verifyPerasVote ::
    PerasEpochContext blk ->
    PerasVote blk ->
    Either (PerasError blk) (ValidatedPerasVote blk)
  default verifyPerasVote ::
    ( CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)
    , PerasVoteCompatibleWithVotingCommittee
        (PerasVote blk)
        (PerasCrypto blk)
        (PerasVotingCommitteeScheme blk)
    ) =>
    PerasEpochContext blk ->
    PerasVote blk ->
    Either (PerasError blk) (ValidatedPerasVote blk)
  verifyPerasVote context vote = do
    let committee = pecCommittee context
    -- NOTE: checking that the voted point is not from the future w.r.t. the
    -- starting slot of the 'PerasRoundNo' will have to be done at the HFC level
    -- since here we don't have 'PerasRoundNo' -> 'SlotNo' resolution device.
    abstractVote <-
      bimap injectConversionError id $
        fromPerasVote @(PerasVote blk) vote
    witness <-
      bimap injectVotingCommitteeError id $
        Committee.verifyVote committee abstractVote
    let voteWeight = eligiblePartyVoteWeight committee witness
    pure $
      ValidatedPerasVote
        { vpvVote = vote
        , vpvVoteWeight = voteWeight
        }

  -- | Forge a Peras certificate from a collection of votes reaching quorum.
  forgePerasCert ::
    PerasEpochContext blk ->
    PerasVoteCollectionWithQuorum blk ->
    Either (PerasError blk) (ValidatedPerasCert blk)
  default forgePerasCert ::
    ( CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)
    , PerasVoteCompatibleWithVotingCommittee
        (PerasVote blk)
        (PerasCrypto blk)
        (PerasVotingCommitteeScheme blk)
    , PerasCertCompatibleWithVotingCommittee
        (PerasCert blk)
        (PerasCrypto blk)
        (PerasVotingCommitteeScheme blk)
    ) =>
    PerasEpochContext blk ->
    PerasVoteCollectionWithQuorum blk ->
    Either (PerasError blk) (ValidatedPerasCert blk)
  forgePerasCert context voteCollection = do
    let params = pecParams context
    abstractVoteCollection <-
      bimap injectConversionError id $
        toUniqueVotesWithSameTarget voteCollection
    abstractCert <-
      bimap injectVotingCommitteeError id $
        Committee.forgeCert abstractVoteCollection
    concreteCert <-
      bimap injectConversionError id $
        toPerasCert abstractCert
    pure $
      ValidatedPerasCert
        { vpcCert = concreteCert
        , vpcCertBoost = perasWeight params
        }

  -- | Verify a Peras certificate and return its boost if valid.
  verifyPerasCert ::
    PerasEpochContext blk ->
    PerasCert blk ->
    Either (PerasError blk) (ValidatedPerasCert blk)
  default verifyPerasCert ::
    ( CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)
    , PerasCertCompatibleWithVotingCommittee
        (PerasCert blk)
        (PerasCrypto blk)
        (PerasVotingCommitteeScheme blk)
    ) =>
    PerasEpochContext blk ->
    PerasCert blk ->
    Either (PerasError blk) (ValidatedPerasCert blk)
  verifyPerasCert context cert = do
    let committee = pecCommittee context
    let params = pecParams context
    -- NOTE: checking that the voted point is not from the future w.r.t. the
    -- starting slot of the 'PerasRoundNo' will have to be done at the HFC level
    -- since here we don't have 'PerasRoundNo' -> 'SlotNo' resolution device.
    abstractCert <-
      bimap injectConversionError id $
        fromPerasCert @(PerasCert blk) cert
    witnesses <-
      bimap injectVotingCommitteeError id $
        Committee.verifyCert committee abstractCert
    let totalVoteWeight = sum (eligiblePartyVoteWeight committee <$> witnesses)
    if weightAboveThreshold params totalVoteWeight
      then
        Right $
          ValidatedPerasCert
            { vpcCert = cert
            , vpcCertBoost = perasWeight params
            }
      else
        Left $
          injectQuorumNotReachedError totalVoteWeight

  -- | Extract a Peras certificate optionally stored in a block.
  --
  -- Returns 'Nothing' if the block does not contain a Peras certificate, or
  -- if the block is from an era that does not support Peras certificates.
  getPerasCertInBlock ::
    blk ->
    Maybe (PerasCert blk)
  getPerasCertInBlock _ =
    Nothing

  -- | Read the private key for Peras voting from the env vars.
  --
  -- NOTE: this is a temporary workaround for testnet, this is supposed to be
  -- replaced for Peras-to-mainnet with proper key registration and retrieval
  -- mechanisms.
  readPerasPrivateKeyFromEnv ::
    proxy blk ->
    Either String (PrivateKey (PerasCrypto blk))
  default readPerasPrivateKeyFromEnv ::
    PerasCrypto blk ~ VoidPerasCrypto blk =>
    proxy blk ->
    Either String (PrivateKey (PerasCrypto blk))
  readPerasPrivateKeyFromEnv _ =
    Right ()

  -- | Read the PoolId from the environment variable 'PERAS_POOL_ID'.
  --
  -- NOTE: this is a temporary workaround for testnet, we still need to figure
  -- out how to properly thread the PoolId throughout a node creation for
  -- Peras-to-mainnet.
  readPerasPoolIdFromEnv ::
    proxy blk ->
    Either String PoolId
  default readPerasPoolIdFromEnv ::
    proxy blk ->
    Either String PoolId
  readPerasPoolIdFromEnv _ =
    unsafePerformIO $
      lookupEnv envVar >>= \case
        Nothing -> do
          pure $ Left $ "Environment variable " <> envVar <> "not set."
        Just rawKey -> do
          pure $ decodeKey rawKey
   where
    envVar =
      "PERAS_POOL_ID"

    decodeKey key =
      case Hash.hashFromStringAsHex key of
        Just hash -> Right $ PoolId (KeyHash hash)
        Nothing -> Left $ "failed to decode PoolId, invalid hash bytes: " <> show key
  {-# NOINLINE readPerasPoolIdFromEnv #-}

-- * Helpers to derive @BlockSupportsPeras@ for block types without Peras support

-- | Imposible Peras vote for @blk@.
--
-- NOTE: the phantom @blk@ is used to keep the 'PerasVote' type family injective.
newtype VoidPerasVote blk
  = VoidPerasVote
  { unVoidPerasVote :: Void
  }
  deriving newtype (Show, Eq, NoThunks, ShowProxy)

-- | Imposible Peras certificate for @blk@.
--
-- NOTE: the phantom @blk@ is used to keep the 'PerasCert' type family injective.
newtype VoidPerasCert blk
  = VoidPerasCert
  { unVoidPerasCert :: Void
  }
  deriving newtype (Show, Eq, NoThunks, ShowProxy)

type instance BoostedBlock (VoidPerasVote blk) = Point blk
type instance BoostedBlock (VoidPerasCert blk) = Point blk

instance IsPerasVote (VoidPerasVote blk) blk where
  getPerasVoteRound = absurd . unVoidPerasVote
  getPerasVoteBlock = absurd . unVoidPerasVote
  getPerasVoteSeatIndex = absurd . unVoidPerasVote

instance IsPerasCert (VoidPerasCert blk) blk where
  getPerasCertRound = absurd . unVoidPerasCert
  getPerasCertBlock = absurd . unVoidPerasCert

-- | Void Peras error for @blk@.
--
-- NOTE: the phantom @blk@ is used to keep the 'PerasError' type family injective.
newtype VoidPerasError blk
  = VoidPerasError
  { unVoidPerasError :: Void
  }
  deriving newtype (Show, Eq, NoThunks, Generic, ShowProxy, Exception)

instance IsPerasError (VoidPerasError blk) blk where
  injectVotingCommitteeError _ =
    error "injectVotingCommitteeError: VoidPerasError cannot be inhabited"
  injectConversionError _ =
    error "injectConversionError: VoidPerasError cannot be inhabited"

  injectQuorumNotReachedError _ =
    error "injectQuorumNotReachedError: VoidPerasError cannot be inhabited"

-- | Void Peras committee for @blk@.
data VoidPerasVotingCommitteeScheme
  deriving (Show, Eq, Generic, NoThunks)

data VoidPerasCrypto blk
  deriving (Show, Eq, Generic, NoThunks)

type instance ElectionId (VoidPerasCrypto blk) = PerasRoundNo
type instance VoteCandidate (VoidPerasCrypto blk) = Point blk

type instance PrivateKey (VoidPerasCrypto blk) = ()
type instance PublicKey (VoidPerasCrypto blk) = Void

instance CryptoSupportsVoteSigning (VoidPerasCrypto blk) where
  type VoteSigningKey (VoidPerasCrypto blk) = ()
  type VoteVerificationKey (VoidPerasCrypto blk) = Void
  data VoteSignature (VoidPerasCrypto blk) = VoidVoteSignature ()
  getVoteSigningKey _proxy _privateKey = ()
  getVoteVerificationKey _proxy publicKey = absurd publicKey
  signVote _signingKey _ _ = VoidVoteSignature ()
  verifyVoteSignature verificationKey _ _ _ = absurd verificationKey

newtype instance VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme
  = VoidPerasVotingCommittee Void

instance CryptoSupportsVotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme where
  newtype VotingCommitteeError (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme
    = VoidPerasVotingCommitteeError Void
  newtype VotingCommitteeInput (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme
    = VoidPerasVotingCommitteeInput Void
  newtype EligibilityWitness (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme
    = VoidPerasEligibilityWitness Void
  newtype Cert (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme
    = VoidPerasCert' {unCommitteeCert :: VoidPerasCert blk}
  newtype Vote (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme
    = VoidPerasVote' {unCommitteeVote :: VoidPerasVote blk}

  mkVotingCommittee (VoidPerasVotingCommitteeInput void) = absurd void
  checkShouldVote (VoidPerasVotingCommittee void) _ _ _ = absurd void
  forgeVote (VoidPerasEligibilityWitness void) _ _ _ = absurd void
  verifyVote (VoidPerasVotingCommittee void) _ = absurd void
  eligiblePartyVoteWeight (VoidPerasVotingCommittee void) _ = absurd void
  forgeCert = absurd . telescope
   where
    telescope = unVoidPerasVote . unCommitteeVote . NonEmpty.head . getRawVotes
  verifyCert (VoidPerasVotingCommittee void) _ = absurd void
  voteTarget (VoidPerasVote' (VoidPerasVote void)) = absurd void
  compareVotesById (VoidPerasVote' (VoidPerasVote void)) _ = absurd void

-- | Exists soley to silence a 'defined-but-not-used' warning on the
-- 'VoidPerasVotingCommitteeError' constructor.
absurdVoidPerasVotingCommitteeError ::
  VotingCommitteeError (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme ->
  a
absurdVoidPerasVotingCommitteeError (VoidPerasVotingCommitteeError void) =
  absurd void

-- | Exists soley to silence a 'defined-but-not-used' warning on the
-- 'VoidPerasCert\'' constructor.
absurdVoidPerasCert ::
  Cert (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme ->
  a
absurdVoidPerasCert (VoidPerasCert' (VoidPerasCert void)) =
  absurd void

deriving newtype instance
  Show (VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme)
deriving newtype instance
  Eq (VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme)
deriving newtype instance
  NoThunks (VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme)
deriving newtype instance
  Generic (VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme)

deriving newtype instance
  Typeable blk =>
  FromCBOR (VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme)
deriving newtype instance
  Typeable blk =>
  ToCBOR (VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme)

instance
  PerasVoteCompatibleWithVotingCommittee
    (VoidPerasVote blk)
    (VoidPerasCrypto blk)
    VoidPerasVotingCommitteeScheme
  where
  toPerasVote = absurd . unVoidPerasVote . unCommitteeVote
  fromPerasVote = absurd . unVoidPerasVote

instance
  PerasCertCompatibleWithVotingCommittee
    (VoidPerasCert blk)
    (VoidPerasCrypto blk)
    VoidPerasVotingCommitteeScheme
  where
  toPerasCert = absurd . unVoidPerasCert . unCommitteeCert
  fromPerasCert = absurd . unVoidPerasCert

-- * Validated types

data ValidatedPerasVote blk
  = ValidatedPerasVote
  { vpvVote :: !(PerasVote blk)
  , vpvVoteWeight :: !VoteWeight
  }

deriving instance Show (PerasVote blk) => Show (ValidatedPerasVote blk)
deriving instance Eq (PerasVote blk) => Eq (ValidatedPerasVote blk)
deriving instance Ord (PerasVote blk) => Ord (ValidatedPerasVote blk)
deriving instance NoThunks (PerasVote blk) => NoThunks (ValidatedPerasVote blk)
deriving instance Generic (ValidatedPerasVote blk)

data ValidatedPerasCert blk
  = ValidatedPerasCert
  { vpcCert :: !(PerasCert blk)
  , vpcCertBoost :: !PerasWeight
  }

deriving instance Show (PerasCert blk) => Show (ValidatedPerasCert blk)
deriving instance Eq (PerasCert blk) => Eq (ValidatedPerasCert blk)
deriving instance Ord (PerasCert blk) => Ord (ValidatedPerasCert blk)
deriving instance NoThunks (PerasCert blk) => NoThunks (ValidatedPerasCert blk)
deriving instance Generic (ValidatedPerasCert blk)

-- * Convenience projection/injection instances

type instance BoostedBlock (ValidatedPerasVote blk) = BoostedBlock (PerasVote blk)

instance
  ( IsPerasVote (PerasVote blk) blk
  , BoostedBlockCompatibleWithPoint (BoostedBlock (PerasVote blk)) blk
  ) =>
  IsPerasVote (ValidatedPerasVote blk) blk
  where
  getPerasVoteRound = getPerasVoteRound . vpvVote
  getPerasVoteBlock = getPerasVoteBlock . vpvVote
  getPerasVoteSeatIndex = getPerasVoteSeatIndex . vpvVote

type instance BoostedBlock (ValidatedPerasCert blk) = BoostedBlock (PerasCert blk)

instance
  ( IsPerasCert (PerasCert blk) blk
  , BoostedBlockCompatibleWithPoint (BoostedBlock (PerasCert blk)) blk
  ) =>
  IsPerasCert (ValidatedPerasCert blk) blk
  where
  getPerasCertRound = getPerasCertRound . vpcCert
  getPerasCertBlock = getPerasCertBlock . vpcCert

-- | Error types that support injecting certain types of Peras errors
class
  IsPerasError err blk
    | err -> blk
  where
  injectVotingCommitteeError :: PerasVotingCommitteeError blk -> err
  injectConversionError :: PerasConversionError -> err
  injectQuorumNotReachedError :: VoteWeight -> err

-- * Types and functions related to Peras vote collection and quorum checking

-- | Collection of Peras votes for a given target.
--
-- NOTE: votes in this collection are uniquely identified by their vote ID.
data PerasVoteCollection blk
  = PerasVoteCollection
  { pvcTarget :: !(PerasVoteTarget blk)
  -- ^ The target of the votes in this collection
  , pvcVotes :: !(NE (Map (PerasVoteId) (WithArrivalTime (ValidatedPerasVote blk))))
  -- ^ Votes received for this target, indexed by vote ID
  , pvcTotalWeight :: !VoteWeight
  -- ^ Total weight of the votes received for this target
  }

deriving instance
  ( StandardHash blk
  , Show (PerasVote blk)
  , Show (PerasCert blk)
  ) =>
  Show (PerasVoteCollection blk)
deriving instance
  ( StandardHash blk
  , Eq (PerasVote blk)
  , Eq (PerasCert blk)
  ) =>
  Eq (PerasVoteCollection blk)
deriving instance
  ( StandardHash blk
  , NoThunks (PerasVote blk)
  , NoThunks (PerasCert blk)
  ) =>
  NoThunks (PerasVoteCollection blk)
deriving instance
  Generic (PerasVoteCollection blk)

-- | Construct a 'PerasVoteCollection' with a single vote.
perasVoteCollectionSingleton ::
  IsPerasVote (PerasVote blk) blk =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasVoteCollection blk
perasVoteCollectionSingleton vote =
  PerasVoteCollection
    { pvcTarget = getPerasVoteTarget vote
    , pvcVotes = NEMap.singleton (getPerasVoteId vote) vote
    , pvcTotalWeight = vpvVoteWeight (forgetArrivalTime vote)
    }

-- | Add a vote to an existing vote collection if it isn't already present, and
-- update the total weight accordingly.
--
-- PRECONDITION: the vote's target must match the collection's target.
perasVoteCollectionAddVote ::
  ( StandardHash blk
  , IsPerasVote (PerasVote blk) blk
  ) =>
  WithArrivalTime (ValidatedPerasVote blk) ->
  PerasVoteCollection blk ->
  PerasVoteCollection blk
perasVoteCollectionAddVote vote pvc =
  assert (getPerasVoteTarget vote == pvcTarget pvc) $
    pvc
      { pvcVotes = pvcVotes'
      , pvcTotalWeight = pvcTotalWeight'
      }
 where
  swapVote =
    NEMap.insertLookupWithKey
      (\_k old _new -> old)
      (getPerasVoteId vote)

  (pvcVotes', pvcTotalWeight')
    -- key WAS NOT present → vote inserted and weight updated
    | (Nothing, votes') <- swapVote vote (pvcVotes pvc) =
        ( votes'
        , pvcTotalWeight pvc + vpvVoteWeight (forgetArrivalTime vote)
        )
    -- key WAS already present → votes and weight unchanged
    | otherwise =
        ( pvcVotes pvc
        , pvcTotalWeight pvc
        )

-- | Unsafe constructor for 'PerasVoteCollection' with asserts to check invariants.
--
-- The only recorded use at the moment is in the HFC implementation, to turn an
-- existing 'PerasVoteCollection' for the HardForkBlock into a
-- 'PerasVoteCollectionWithQuorum' of a concrete era.
unsafePerasVoteCollection ::
  (IsPerasVote (PerasVote blk) blk, StandardHash blk) =>
  (NE (Map (PerasVoteId) (WithArrivalTime (ValidatedPerasVote blk)))) ->
  PerasVoteCollection blk
unsafePerasVoteCollection votes =
  -- NOTE: no need to check for ID uniqueness since the votes are stored in a
  -- map keyed by vote ID.
  assert
    ( all
        (\vote -> getPerasVoteTarget vote == firstVoteTarget)
        (NEMap.elems votes)
    )
    $ PerasVoteCollection
      { pvcTarget = firstVoteTarget
      , pvcVotes = votes
      , pvcTotalWeight = totalWeight
      }
 where
  ((_, firstVote) :| _) = NEMap.toList votes
  firstVoteTarget = getPerasVoteTarget firstVote
  totalWeight = sum (vpvVoteWeight . forgetArrivalTime <$> NEMap.elems votes)

-- | A collection of Peras votes for a given target that has reached quorum
newtype PerasVoteCollectionWithQuorum blk
  = PerasVoteCollectionWithQuorum
  { forgetQuorum :: PerasVoteCollection blk
  }

deriving newtype instance
  ( StandardHash blk
  , Show (PerasVote blk)
  , Show (PerasCert blk)
  ) =>
  Show (PerasVoteCollectionWithQuorum blk)
deriving newtype instance
  ( StandardHash blk
  , Eq (PerasVote blk)
  , Eq (PerasCert blk)
  ) =>
  Eq (PerasVoteCollectionWithQuorum blk)
deriving newtype instance
  ( StandardHash blk
  , NoThunks (PerasVote blk)
  , NoThunks (PerasCert blk)
  ) =>
  NoThunks (PerasVoteCollectionWithQuorum blk)
deriving newtype instance
  Generic (PerasVoteCollectionWithQuorum blk)

-- | Transforms a 'PerasVoteCollection' into a 'PerasVoteCollectionWithQuorum'
-- without actually checking the quorum condition.
--
-- NOTE: the only recorded use at the moment is in the HFC implementation, to
-- turn an existing 'PerasVoteCollectionWithQuorum' for the HardForkBlock into
-- a 'PerasVoteCollectionWithQuorum' of a concrete era.
unsafeAssumeQuorum ::
  PerasVoteCollection blk ->
  PerasVoteCollectionWithQuorum blk
unsafeAssumeQuorum =
  PerasVoteCollectionWithQuorum

-- | Smart constructor for 'PerasVoteCollectionWithQuorum'
perasVoteCollectionCheckQuorum ::
  PerasEpochContext blk ->
  PerasVoteCollection blk ->
  Maybe (PerasVoteCollectionWithQuorum blk)
perasVoteCollectionCheckQuorum epochContext pvc =
  case weightAboveThreshold (pecParams epochContext) (pvcTotalWeight pvc) of
    True -> Just (PerasVoteCollectionWithQuorum pvc)
    False -> Nothing

-- | Convert a collection of Peras votes that has reached quorum into the
-- corresponding abstract representation of votes used by the voting committee
-- to forge certificates.
--
-- 'UniqueVotesWithSameTarget' and 'PerasVoteCollection' enforce the same
-- invariants, which are:
-- - The collection is not empty
-- - All votes have the same target
-- - All votes have a unique vote ID (or unique seat index, which is equivalent
--   assuming they also have the same target, see second point)
-- In addition to that, 'PerasVoteCollectionWithQuorum' guarantees that the
-- total weight of the votes is above the threshold.
toUniqueVotesWithSameTarget ::
  ( vote ~ PerasVote blk
  , crypto ~ PerasCrypto blk
  , committee ~ PerasVotingCommitteeScheme blk
  , ElectionId crypto ~ PerasRoundNo
  , PerasVoteCompatibleWithVotingCommittee vote crypto committee
  , CryptoSupportsVotingCommittee crypto committee
  , Eq (VoteCandidate crypto)
  ) =>
  PerasVoteCollectionWithQuorum blk ->
  Either
    PerasConversionError
    (UniqueVotesWithSameTarget (PerasCrypto blk) (PerasVotingCommitteeScheme blk))
toUniqueVotesWithSameTarget (PerasVoteCollectionWithQuorum pvc) = do
  fmap unsafeUniqueVotesWithSameTarget -- Skip redundant checks in production
    . traverse fromPerasVote
    . fmap (vpvVote . forgetArrivalTime)
    . NEMap.elems
    . pvcVotes
    $ pvc

-- * Helpers

-- | Check whether a given vote weight is above the quorum threshold.
--
-- NOTE: this function assumes that the 'VoteWeight' and the quorum
-- threshold used in 'PerasParams' are expressed in the same units. That is,
-- both are either absolute or relative (normalized) values. Under the current
-- current implementation of 'PerasParams', this function only makes sense when
-- both values are relative (normalized) values.
weightAboveThreshold :: PerasParams blk -> VoteWeight -> Bool
weightAboveThreshold params voteWeight =
  weight >= quorumThreshold + safetyMargin
 where
  weight =
    unVoteWeight voteWeight
  quorumThreshold =
    unPerasQuorumWeightThreshold
      (perasQuorumWeightThreshold params)
  safetyMargin =
    unPerasQuorumWeightThresholdSafetyMargin
      (perasQuorumWeightThresholdSafetyMargin params)
