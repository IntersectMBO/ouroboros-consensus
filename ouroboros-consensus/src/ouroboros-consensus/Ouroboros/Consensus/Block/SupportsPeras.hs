{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- Disabled until we get rid of the degenerate 'BlockSupportsPeras' instance.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ouroboros.Consensus.Block.SupportsPeras
  ( -- * Voting committee types for Peras
    PerasVotingCommittee
  , PerasVotingCommitteeError
  , PerasVotingCommitteeInput

    -- * Epoch-dependent context for Peras
  , PerasEpochContext (..)

    -- * BlockSupportsPeras class
  , BlockSupportsPeras (..)
  , defaultForgePerasVoteIfEligible
  , defaultVerifyPerasVote
  , defaultForgePerasCert
  , defaultVerifyPerasCert

    -- * To be removed in favor of using per-blk definitions
  , PerasCert' (..)
  , PerasVote' (..)

    -- * To be removed in favor of using a 'PerasEpochContext' directly
  , PerasVoteStakeDistr (..)

    -- * Validated types
  , ValidatedPerasCert (..)
  , ValidatedPerasVote (..)

    -- * Peras error types
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
  , module Ouroboros.Consensus.Peras.Cert.Class
  , module Ouroboros.Consensus.Peras.Params
  , module Ouroboros.Consensus.Peras.Types
  , module Ouroboros.Consensus.Peras.Void
  , module Ouroboros.Consensus.Peras.Vote.Class
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding (decodeListLenOf)
import Codec.Serialise.Encoding (encodeListLen)
import Control.Exception (assert)
import Control.Monad.Error.Class (MonadError (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Containers.NonEmpty (HasNonEmpty (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.NonEmpty as NEMap
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (..))
import Data.Traversable (for)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime (..))
import Ouroboros.Consensus.Committee.Class
  ( CryptoSupportsVotingCommittee (..)
  , UniqueVotesWithSameTarget
  , VotingCommittee
  , unsafeUniqueVotesWithSameTarget
  )
import qualified Ouroboros.Consensus.Committee.Class as Committee
import Ouroboros.Consensus.Committee.Crypto (PrivateKey, VoteCandidate)
import Ouroboros.Consensus.Committee.Types (PoolId)
import Ouroboros.Consensus.Peras.Cert.Class
import Ouroboros.Consensus.Peras.Params
import Ouroboros.Consensus.Peras.Types
import Ouroboros.Consensus.Peras.Void
import Ouroboros.Consensus.Peras.Vote.Class
import Ouroboros.Consensus.Peras.Voting.Adapter
import Ouroboros.Consensus.Util

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

-- * Peras types

-- TODO: to be removed in favor of using a 'PerasEpochContext' directly.
newtype PerasVoteStakeDistr = PerasVoteStakeDistr
  { unPerasVoteStakeDistr :: Map PerasSeatIndex VoteWeight
  }
  deriving newtype NoThunks
  deriving stock (Show, Eq, Generic)

-- * BlockSupportsPeras class

class
  ( StandardHash blk
  , Show (PerasParams blk)
  , NoThunks (PerasCert blk)
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

  validatePerasCert ::
    PerasParams blk ->
    PerasCert blk ->
    Either (PerasError blk) (ValidatedPerasCert blk)

  validatePerasVote ::
    PerasParams blk ->
    PerasVoteStakeDistr ->
    PerasVote blk ->
    Either (PerasError blk) (ValidatedPerasVote blk)

  forgePerasCert ::
    PerasParams blk ->
    PerasVoteCollectionWithQuorum blk ->
    Either (PerasError blk) (ValidatedPerasCert blk)

  -- | Extract a Peras certificate optionally stored in a block.
  --
  -- Returns 'Nothing' if the block does not contain a Peras certificate, or
  -- if the block is from an era that does not support Peras certificates.
  getPerasCertInBlock ::
    blk ->
    Maybe (PerasCert blk)

-- | Forge a Peras vote if the given pool is eligible to vote in the given round.
defaultForgePerasVoteIfEligible ::
  forall blk.
  ( BlockSupportsPeras blk
  , CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)
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
defaultForgePerasVoteIfEligible context ourId ourPrivateKey roundNo point = do
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
defaultVerifyPerasVote ::
  forall blk.
  ( BlockSupportsPeras blk
  , CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)
  , PerasVoteCompatibleWithVotingCommittee
      (PerasVote blk)
      (PerasCrypto blk)
      (PerasVotingCommitteeScheme blk)
  ) =>
  PerasEpochContext blk ->
  PerasVote blk ->
  Either (PerasError blk) (ValidatedPerasVote blk)
defaultVerifyPerasVote context vote = do
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
  pure
    ValidatedPerasVote
      { vpvVote = vote
      , vpvVoteWeight = voteWeight
      }

-- | Forge a Peras certificate from a collection of votes reaching quorum.
defaultForgePerasCert ::
  forall blk.
  ( BlockSupportsPeras blk
  , CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)
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
defaultForgePerasCert context voteCollection = do
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
  pure
    ValidatedPerasCert
      { vpcCert = concreteCert
      , vpcCertBoost = perasWeight params
      }

-- | Verify a Peras certificate and return its boost if valid.
defaultVerifyPerasCert ::
  forall blk.
  ( BlockSupportsPeras blk
  , CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)
  , PerasCertCompatibleWithVotingCommittee
      (PerasCert blk)
      (PerasCrypto blk)
      (PerasVotingCommitteeScheme blk)
  ) =>
  PerasEpochContext blk ->
  PerasCert blk ->
  Either (PerasError blk) (ValidatedPerasCert blk)
defaultVerifyPerasCert context cert = do
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
      pure
        ValidatedPerasCert
          { vpcCert = cert
          , vpcCertBoost = perasWeight params
          }
    else
      throwError (injectQuorumNotReachedError totalVoteWeight)

-- TODO: degenerate instance for all blks to get things to compile
-- see https://github.com/tweag/cardano-peras/issues/73
instance StandardHash blk => BlockSupportsPeras blk where
  type PerasCrypto blk = VoidPerasCrypto blk
  type PerasVotingCommitteeScheme blk = VoidPerasVotingCommitteeScheme
  type PerasError blk = VoidPerasError blk

  type PerasCert blk = PerasCert' blk
  type PerasVote blk = PerasVote' blk

  validatePerasCert params cert =
    Right
      ValidatedPerasCert
        { vpcCert = cert
        , vpcCertBoost = perasWeight params
        }

  validatePerasVote _params _stakeDistr vote =
    Right
      ValidatedPerasVote
        { vpvVote = vote
        , vpvVoteWeight = VoteWeight 0
        }

  forgePerasCert params votes =
    Right $
      ValidatedPerasCert
        { vpcCert =
            PerasCert
              { pcCertRound = pvtRoundNo (pvcTarget (forgetQuorum votes))
              , pcCertBoostedBlock = pvtBlock (pvcTarget (forgetQuorum votes))
              }
        , vpcCertBoost = perasWeight params
        }

  getPerasCertInBlock _ = Nothing

-- | NOTE: to be removed in favor of using per-blk definitions.
data PerasCert' blk
  = PerasCert
  { pcCertRound :: PerasRoundNo
  , pcCertBoostedBlock :: Point blk
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

-- | NOTE: to be removed in favor of using per-blk definitions.
data PerasVote' blk
  = PerasVote
  { pvVoteRound :: PerasRoundNo
  , pvVoteBlock :: Point blk
  , pvVoteVoterId :: PerasSeatIndex
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

instance ShowProxy blk => ShowProxy (PerasCert' blk) where
  showProxy _ = "PerasCert " <> showProxy (Proxy @blk)

instance ShowProxy blk => ShowProxy (PerasVote' blk) where
  showProxy _ = "PerasVote " <> showProxy (Proxy @blk)

instance Serialise (HeaderHash blk) => Serialise (PerasCert' blk) where
  encode PerasCert{pcCertRound, pcCertBoostedBlock} =
    encodeListLen 2
      <> encode pcCertRound
      <> encode pcCertBoostedBlock
  decode = do
    decodeListLenOf 2
    pcCertRound <- decode
    pcCertBoostedBlock <- decode
    pure $ PerasCert{pcCertRound, pcCertBoostedBlock}

instance Serialise (HeaderHash blk) => Serialise (PerasVote' blk) where
  encode PerasVote{pvVoteRound, pvVoteBlock, pvVoteVoterId} =
    encodeListLen 3
      <> encode pvVoteRound
      <> encode pvVoteBlock
      <> toCBOR pvVoteVoterId
  decode = do
    decodeListLenOf 3
    pvVoteRound <- decode
    pvVoteBlock <- decode
    pvVoteVoterId <- fromCBOR
    pure $ PerasVote{pvVoteRound, pvVoteBlock, pvVoteVoterId}

type instance BoostedBlock (PerasCert' blk) = Point blk
type instance BoostedBlock (PerasVote' blk) = Point blk

instance IsPerasCert (PerasCert' blk) blk where
  getPerasCertRound = pcCertRound
  getPerasCertBlock = pcCertBoostedBlock

instance IsPerasVote (PerasVote' blk) blk where
  getPerasVoteRound = pvVoteRound
  getPerasVoteBlock = pvVoteBlock
  getPerasVoteSeatIndex = pvVoteVoterId

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

deriving instance Show (PerasCert blk) => Show (ValidatedPerasCert blk)
deriving instance Eq (PerasCert blk) => Eq (ValidatedPerasCert blk)
deriving instance Ord (PerasCert blk) => Ord (ValidatedPerasCert blk)
deriving instance NoThunks (PerasCert blk) => NoThunks (ValidatedPerasCert blk)
deriving instance Generic (ValidatedPerasCert blk)

type instance BoostedBlock (ValidatedPerasCert blk) = BoostedBlock (PerasCert blk)

instance
  ( IsPerasCert (PerasCert blk) blk
  , BoostedBlockCompatibleWithPoint (BoostedBlock (PerasCert blk)) blk
  ) =>
  IsPerasCert (ValidatedPerasCert blk) blk
  where
  getPerasCertRound = getPerasCertRound . vpcCert
  getPerasCertBlock = getPerasCertBlock . vpcCert

--- * Peras error types

-- | Error types that support injecting certain types of Peras errors
class IsPerasError err blk | err -> blk where
  injectVotingCommitteeError :: PerasVotingCommitteeError blk -> err
  injectConversionError :: PerasConversionError -> err
  injectQuorumNotReachedError :: VoteWeight -> err

instance IsPerasError (VoidPerasError blk) blk where
  injectVotingCommitteeError _ =
    error "injectVotingCommitteeError: VoidPerasError cannot be inhabited"
  injectConversionError _ =
    error "injectConversionError: VoidPerasError cannot be inhabited"
  injectQuorumNotReachedError _ =
    error "injectQuorumNotReachedError: VoidPerasError cannot be inhabited"

-- * Types and functions related to Peras vote collection and quorum checking

-- | Collection of Peras votes for a given target.
--
-- NOTE: votes in this collection are uniquely identified by their vote ID.
data PerasVoteCollection blk
  = PerasVoteCollection
  { pvcTarget :: !(PerasVoteTarget blk)
  -- ^ The target of the votes in this collection
  , pvcVotes :: !(NE (Map PerasVoteId (WithArrivalTime (ValidatedPerasVote blk))))
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

-- | Unsafe constructor for 'PerasVoteCollection'.
--
-- The only recorded use at the moment is in the HFC implementation, to turn an
-- existing 'PerasVoteCollection' for the HardForkBlock into a
-- 'PerasVoteCollection' of a concrete era.
unsafePerasVoteCollection ::
  ( IsPerasVote (PerasVote blk) blk
  , StandardHash blk
  ) =>
  (NE (Map PerasVoteId (WithArrivalTime (ValidatedPerasVote blk)))) ->
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
  PerasParams blk ->
  PerasVoteCollection blk ->
  Maybe (PerasVoteCollectionWithQuorum blk)
perasVoteCollectionCheckQuorum params pvc =
  case weightAboveThreshold params (pvcTotalWeight pvc) of
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
  , PerasVoteCompatibleWithVotingCommittee vote crypto committee
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
