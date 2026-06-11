{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Ouroboros.Consensus.Block.SupportsPeras
  ( PerasVotingCommittee
  , PerasVotingCommitteeError
  , PerasVotingCommitteeInput
  , DefaultPerasEpochContext (..)
  , BlockSupportsPeras (..)
  , PerasVoteCompatibleWithVotingCommittee (..)
  , PerasCertCompatibleWithVotingCommittee (..)
  , VoidPerasVotingCommitteeScheme
  , VoidPerasVote (..)
  , VoidPerasCert (..)
  , VoidPerasError (..)
  , ValidatedPerasCert (..)
  , ValidatedPerasVote (..)
  , IsPerasVote (..)
  , getPerasVoteId
  , getPerasVoteTarget
  , IsPerasCert (..)
  , IsPerasError (..)

    -- * Types and functions related to Peras vote collection and quorum checking
  , PerasVoteCollectionWithQuorum (forgetQuorum)
  , PerasVoteCollection
    ( pvcTarget
    , pvcVotes
    , pvcTotalWeight
    )
  , perasVoteCollectionSingleton
  , perasVoteCollectionAddVote
  , perasVoteCollectionCheckQuorum
  , toUniqueVotesWithSameTarget

    -- * Convenience re-exports
  , module Ouroboros.Consensus.Peras.Params
  , module Ouroboros.Consensus.Peras.Types
  ) where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Hashes (KeyHash (..))
import Codec.Serialise (Serialise)
import Control.Exception (assert)
import Control.Exception.Base (Exception)
import Data.Bifunctor (bimap)
import Data.Containers.NonEmpty (NE)
import Data.Kind (Type)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map.NonEmpty as NEMap
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
import Ouroboros.Consensus.Peras.Params
import Ouroboros.Consensus.Peras.Types
import Ouroboros.Consensus.Util (ShowProxy)
import Ouroboros.Consensus.Util.Orphans ()
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

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

-- | Default epoch context for Peras
data DefaultPerasEpochContext blk
  = DefaultPerasEpochContext
  { dpecCommittee :: PerasVotingCommittee blk
  , dpecParams :: PerasParams blk
  }

deriving instance Show (PerasVotingCommittee blk) => Show (DefaultPerasEpochContext blk)
deriving instance Eq (PerasVotingCommittee blk) => Eq (DefaultPerasEpochContext blk)
deriving instance NoThunks (PerasVotingCommittee blk) => NoThunks (DefaultPerasEpochContext blk)
deriving instance Serialise (PerasVotingCommittee blk) => Serialise (DefaultPerasEpochContext blk)
deriving instance Generic (DefaultPerasEpochContext blk)

-- * BlockSupportsPeras class

-- TODO: Add CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk) as a superclass constraint of 'BlockSupportsPeras'

class
  ( -- blk related constraints
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
    Typeable (PerasCrypto blk)
  , Typeable (PerasVotingCommitteeScheme blk)
  , CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)
  , ElectionId (PerasCrypto blk) ~ PerasRoundNo
  , VoteCandidate (PerasCrypto blk) ~ BoostedBlock (PerasVote blk)
  , VoteCandidate (PerasCrypto blk) ~ BoostedBlock (PerasCert blk)
  , PerasVoteCompatibleWithVotingCommittee
      (PerasVote blk)
      (PerasCrypto blk)
      (PerasVotingCommitteeScheme blk)
  , PerasCertCompatibleWithVotingCommittee
      (PerasCert blk)
      (PerasCrypto blk)
      (PerasVotingCommitteeScheme blk)
  ) =>
  BlockSupportsPeras blk
  where
  type PerasVote blk = (vote :: Type) | vote -> blk
  type PerasVote blk = VoidPerasVote blk

  type PerasCert blk = (cert :: Type) | cert -> blk
  type PerasCert blk = VoidPerasCert blk

  type PerasError blk = (err :: Type) | err -> blk
  type PerasError blk = VoidPerasError blk

  -- | The crypto scheme used for Peras votes and certificates
  --
  -- Used to dispatch a block type to a its corresponding voting crypto scheme.
  type PerasCrypto blk :: Type

  type PerasCrypto blk = VoidPerasCrypto blk

  -- | The voting committee scheme used for Peras.
  --
  -- Used to dispatch a block type to a its corresponding voting committee scheme.
  type PerasVotingCommitteeScheme blk :: Type

  type PerasVotingCommitteeScheme blk = VoidPerasVotingCommitteeScheme

  type PerasEpochContext blk = (context :: Type) | context -> blk
  type PerasEpochContext blk = DefaultPerasEpochContext blk

  pecPerasParams :: PerasEpochContext blk -> PerasParams blk
  default pecPerasParams ::
    PerasEpochContext blk ~ DefaultPerasEpochContext blk =>
    PerasEpochContext blk ->
    PerasParams blk
  pecPerasParams = dpecParams

  forgePerasVoteIfEligible ::
    PerasEpochContext blk ->
    PoolId ->
    PrivateKey (PerasCrypto blk) ->
    PerasRoundNo ->
    Point blk ->
    Either (PerasError blk) (Maybe (ValidatedPerasVote blk))
  default forgePerasVoteIfEligible ::
    PerasEpochContext blk ~ DefaultPerasEpochContext blk =>
    PerasEpochContext blk ->
    PoolId ->
    PrivateKey (PerasCrypto blk) ->
    PerasRoundNo ->
    Point blk ->
    Either (PerasError blk) (Maybe (ValidatedPerasVote blk))
  forgePerasVoteIfEligible context ourId ourPrivateKey roundNo point = do
    let committee = dpecCommittee context
    mWitness <-
      bimap injectVotingCommitteeError id $
        Committee.checkShouldVote committee ourId ourPrivateKey roundNo
    for mWitness $ \witness -> do
      let voteWeight = eligiblePartyVoteWeight committee witness
          abstractVote = Committee.forgeVote witness ourPrivateKey roundNo (pointToBoostedBlock point)
      concreteVote <-
        bimap injectConversionError id $
          toPerasVote @(PerasVote blk) abstractVote
      pure $
        ValidatedPerasVote
          { vpvVote = concreteVote
          , vpvVoteWeight = voteWeight
          }

  verifyPerasVote ::
    PerasEpochContext blk ->
    PerasVote blk ->
    Either (PerasError blk) (ValidatedPerasVote blk)
  default verifyPerasVote ::
    PerasEpochContext blk ~ DefaultPerasEpochContext blk =>
    PerasEpochContext blk ->
    PerasVote blk ->
    Either (PerasError blk) (ValidatedPerasVote blk)
  verifyPerasVote context vote = do
    let committee = dpecCommittee context
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

  forgePerasCert ::
    PerasEpochContext blk ->
    PerasVoteCollectionWithQuorum blk ->
    Either (PerasError blk) (ValidatedPerasCert blk)
  default forgePerasCert ::
    PerasEpochContext blk ~ DefaultPerasEpochContext blk =>
    PerasEpochContext blk ->
    PerasVoteCollectionWithQuorum blk ->
    Either (PerasError blk) (ValidatedPerasCert blk)
  forgePerasCert context voteCollection = do
    let params = dpecParams context
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

  verifyPerasCert ::
    PerasEpochContext blk ->
    PerasCert blk ->
    Either (PerasError blk) (ValidatedPerasCert blk)
  default verifyPerasCert ::
    PerasEpochContext blk ~ DefaultPerasEpochContext blk =>
    PerasEpochContext blk ->
    PerasCert blk ->
    Either (PerasError blk) (ValidatedPerasCert blk)
  verifyPerasCert context cert = do
    let committee = dpecCommittee context
    let params = dpecParams context
    -- NOTE: checking that the voted point is not from the future w.r.t. the
    -- starting slot of the 'PerasRoundNo' will have to be done at the HFC level
    -- since here we don't have 'PerasRoundNo' -> 'SlotNo' resolution device.
    abstractCert <-
      bimap injectConversionError id $
        fromPerasCert @(PerasCert blk) cert
    witnesses <-
      bimap injectVotingCommitteeError id $
        Committee.verifyCert committee abstractCert
    let totalVoteWeight = sum (fmap (eligiblePartyVoteWeight committee) witnesses)
    if weightAboveThreshold params totalVoteWeight
      then
        pure $
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

  blockDoesReallySupportsPeras ::
    proxy blk ->
    Bool
  default blockDoesReallySupportsPeras ::
    PerasCrypto blk ~ VoidPerasCrypto blk =>
    proxy blk ->
    Bool
  blockDoesReallySupportsPeras _ = False

  -- | Read the private key for Peras voting from the env vars
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
    Left "VoidPerasCrypto has no way to instantiate a private key"

  -- | Read the PoolId from the environment variable 'PERAS_POOL_ID'
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

-- * Conversion between concrete Peras types and abstract committee types

-- | Conversion between (concrete) Peras votes and (abstract) committee votes.
--
-- NOTE: the functional dependency @vote -> crypto@ explicitly ties each
-- concrete Peras vote type to a specific crypto scheme.
class
  PerasVoteCompatibleWithVotingCommittee vote crypto committee
    | vote -> crypto
  where
  toPerasVote ::
    Committee.Vote crypto committee ->
    Either PerasConversionError vote
  fromPerasVote ::
    vote ->
    Either PerasConversionError (Committee.Vote crypto committee)

-- | Conversion between (concrete) Peras certificates and (abstract) committee
-- certificates.
--
-- NOTE: the functional dependency @cert -> crypto@ explicitly ties each
-- concrete Peras certificate type to a specific crypto scheme.
class
  PerasCertCompatibleWithVotingCommittee cert crypto committee
    | cert -> crypto
  where
  toPerasCert ::
    Committee.Cert crypto committee ->
    Either PerasConversionError cert
  fromPerasCert ::
    cert ->
    Either PerasConversionError (Committee.Cert crypto committee)

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

data VoidPerasCrypto blk

type instance ElectionId (VoidPerasCrypto blk) = PerasRoundNo
type instance VoteCandidate (VoidPerasCrypto blk) = Point blk

type instance PrivateKey (VoidPerasCrypto blk) = Void
type instance PublicKey (VoidPerasCrypto blk) = Void

instance CryptoSupportsVoteSigning (VoidPerasCrypto blk) where
  type VoteSigningKey (VoidPerasCrypto blk) = Void
  type VoteVerificationKey (VoidPerasCrypto blk) = Void
  data VoteSignature (VoidPerasCrypto blk) = VoidVoteSignature {unVoidVoteSignature :: Void}
  getVoteSigningKey _proxy privateKey = absurd privateKey
  getVoteVerificationKey _proxy publicKey = absurd publicKey
  signVote signingKey _ _ = absurd signingKey
  verifyVoteSignature verificationKey _ _ _ = absurd verificationKey

instance CryptoSupportsVotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme where
  data VotingCommitteeError (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme = VoidPerasVotingCommitteeError {unVoidPerasVotingCommitteeError :: Void}
  data VotingCommitteeInput (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme = VoidPerasVotingCommitteeInput {unVoidPerasVotingCommitteeInput :: Void}
  data VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme = VoidPerasVotingCommittee {unVoidPerasVotingCommittee :: Void}
  data EligibilityWitness (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme = VoidPerasEligibilityWitness {unVoidPerasEligibilityWitness :: Void}
  data Cert (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme = Cert {unCommitteeCert :: VoidPerasCert blk}
  data Vote (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme = Vote {unCommitteeVote :: VoidPerasVote blk}

  mkVotingCommittee (VoidPerasVotingCommitteeInput void) = absurd void
  checkShouldVote (VoidPerasVotingCommittee void) _ _ _ = absurd void
  forgeVote (VoidPerasEligibilityWitness void) _ _ _ = absurd void
  verifyVote (VoidPerasVotingCommittee void) _ = absurd void
  eligiblePartyVoteWeight (VoidPerasVotingCommittee void) _ = absurd void
  forgeCert uniqueVotes = absurd . unVoidPerasVote . unCommitteeVote . NonEmpty.head . getRawVotes $ uniqueVotes
  verifyCert (VoidPerasVotingCommittee void) _ = absurd void
  voteTarget (Vote (VoidPerasVote void)) = absurd void
  compareVotesById (Vote (VoidPerasVote void)) _ = absurd void

deriving instance Show (VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme)
deriving instance Eq (VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme)
deriving instance NoThunks (VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme)
deriving instance Serialise (VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme)
deriving instance Generic (VotingCommittee (VoidPerasCrypto blk) VoidPerasVotingCommitteeScheme)

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

-- * Convenience projection/injection classes

-- | Types that support being treated as Peras votes
class
  BoostedBlockCompatibleWithPoint (BoostedBlock vote) blk =>
  IsPerasVote vote blk
    | vote -> blk
  where
  getPerasVoteRound :: vote -> PerasRoundNo
  getPerasVoteBlock :: vote -> BoostedBlock vote
  getPerasVoteSeatIndex :: vote -> PerasSeatIndex

  getPerasVotePoint :: vote -> Point blk
  getPerasVotePoint = boostedBlockToPoint . getPerasVoteBlock

-- | Extract the vote ID from a Peras vote container
getPerasVoteId :: IsPerasVote vote blk => vote -> PerasVoteId blk
getPerasVoteId vote =
  PerasVoteId
    { pviRoundNo = getPerasVoteRound vote
    , pviSeatIndex = getPerasVoteSeatIndex vote
    }

-- | Extract the vote target from a Peras vote container
getPerasVoteTarget :: IsPerasVote vote blk => vote -> PerasVoteTarget blk
getPerasVoteTarget vote =
  PerasVoteTarget
    { pvtRoundNo = getPerasVoteRound vote
    , pvtBlock = getPerasVotePoint vote
    }

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

instance
  IsPerasVote vote blk =>
  IsPerasVote (WithArrivalTime vote) blk
  where
  getPerasVoteRound = getPerasVoteRound . forgetArrivalTime
  getPerasVoteBlock = getPerasVoteBlock . forgetArrivalTime
  getPerasVoteSeatIndex = getPerasVoteSeatIndex . forgetArrivalTime

-- | Types that support being treated as Peras certificates
class
  BoostedBlockCompatibleWithPoint (BoostedBlock cert) blk =>
  IsPerasCert cert blk
    | cert -> blk
  where
  getPerasCertRound :: cert -> PerasRoundNo
  getPerasCertBlock :: cert -> BoostedBlock cert

  getPerasCertPoint :: cert -> Point blk
  getPerasCertPoint = boostedBlockToPoint . getPerasCertBlock

type instance BoostedBlock (ValidatedPerasCert blk) = BoostedBlock (PerasCert blk)

instance
  ( IsPerasCert (PerasCert blk) blk
  , BoostedBlockCompatibleWithPoint (BoostedBlock (PerasCert blk)) blk
  ) =>
  IsPerasCert (ValidatedPerasCert blk) blk
  where
  getPerasCertRound = getPerasCertRound . vpcCert
  getPerasCertBlock = getPerasCertBlock . vpcCert

type instance BoostedBlock (WithArrivalTime voteOrCert) = BoostedBlock voteOrCert

instance
  IsPerasCert cert blk =>
  IsPerasCert (WithArrivalTime cert) blk
  where
  getPerasCertRound = getPerasCertRound . forgetArrivalTime
  getPerasCertBlock = getPerasCertBlock . forgetArrivalTime

-- | Error types that support injecting certain types of Peras errors
class
  IsPerasError err blk
    | err -> blk
  where
  injectVotingCommitteeError :: PerasVotingCommitteeError blk -> err
  injectConversionError :: PerasConversionError -> err
  injectQuorumNotReachedError :: VoteWeight -> err

-------------------------------------------------------------------------------è

-- | Collection of Peras votes for a given target.
--
-- NOTE: votes in this collection are uniquely identified by their vote ID.
data PerasVoteCollection blk
  = PerasVoteCollection
  { pvcTarget :: !(PerasVoteTarget blk)
  -- ^ The target of the votes in this collection
  , pvcVotes :: !(NE (Map (PerasVoteId blk) (WithArrivalTime (ValidatedPerasVote blk))))
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

-- | Smart constructor for 'PerasVoteCollection' from a single vote.
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

-- | A collection of Peras votes for a given target that has reached quorum
newtype PerasVoteCollectionWithQuorum blk
  = PerasVoteCollectionWithQuorum {forgetQuorum :: PerasVoteCollection blk}

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

-- | Smart constructor for 'PerasVoteCollectionWithQuorum'
perasVoteCollectionCheckQuorum ::
  BlockSupportsPeras blk =>
  PerasEpochContext blk ->
  PerasVoteCollection blk ->
  Maybe (PerasVoteCollectionWithQuorum blk)
perasVoteCollectionCheckQuorum epochContext pvc =
  case weightAboveThreshold (pecPerasParams epochContext) (pvcTotalWeight pvc) of
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
  let concreteVoteNeList = (vpvVote . forgetArrivalTime) <$> NEMap.elems (pvcVotes pvc)
  abstractVoteNeList <- traverse fromPerasVote concreteVoteNeList
  pure $
    unsafeUniqueVotesWithSameTarget -- Skip redundant checks in production
      abstractVoteNeList
