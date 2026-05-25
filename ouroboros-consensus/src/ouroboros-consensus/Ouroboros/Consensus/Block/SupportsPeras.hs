{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Block.SupportsPeras
  ( PerasCrypto
  , PerasVotingCommitteeScheme
  , PerasVotingCommittee
  , PerasVotingCommitteeError
  , PerasVotingCommitteeInput
  , BlockSupportsPeras (..)
  , PerasVoteCompatibleWithVotingCommittee (..)
  , PerasCertCompatibleWithVotingCommittee (..)
  , implPerasForgeVoteIfEligible
  , VoidPerasVote (..)
  , VoidPerasCert (..)
  , EmptyPerasError (..)
  , ValidatedPerasCert (..)
  , ValidatedPerasVote (..)
  , ValidatedPerasVotesWithQuorum
    ( vpvqTarget
    , vpvqVotes
    , vpvqPerasParams
    )
  , votesReachQuorum
  , IsPerasVote (..)
  , getPerasVoteId
  , getPerasVoteTarget
  , IsPerasCert (..)
  , IsPerasError (..)

    -- * Convenience re-exports
  , module Ouroboros.Consensus.Peras.Params
  , module Ouroboros.Consensus.Peras.Types
  ) where

import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime (..))
import Ouroboros.Consensus.Committee.Class (CryptoSupportsVotingCommittee (..))
import qualified Ouroboros.Consensus.Committee.Class as Committee
import Ouroboros.Consensus.Committee.Crypto (ElectionId, PrivateKey, VoteCandidate)
import Ouroboros.Consensus.Committee.Types (PoolId)
import Ouroboros.Consensus.Peras.Params
import Ouroboros.Consensus.Peras.Types
import Ouroboros.Consensus.Util (ShowProxy)

-- | The crypto scheme used for Peras votes and certificates
--
-- Used to dispatch a block type to a its corresponding voting crypto scheme.
--
-- TODO: maybe move this inside 'BlockSupportsPeras'.
-- TODO: add eq constraints ( ElectionId (PerasCrypto blk) ~ PerasRoundNo
--                          , VoteCandidate (PerasCrypto blk) ~ Point blk)
-- in BlockSupportsPeras
type family PerasCrypto blk :: Type

-- | The voting committee scheme used for Peras.
--
-- Used to dispatch a block type to a its corresponding voting committee scheme.
--
-- TODO: maybe move this inside 'BlockSupportsPeras'.
type family PerasVotingCommitteeScheme blk :: Type


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

-- * BlockSupportsPeras class

-- TODO: Add CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk) as a superclass constraint of 'BlockSupportsPeras'

class
  ( StandardHash blk
  , Typeable blk
  , Typeable (PerasVote blk)
  , Typeable (PerasCert blk)
  , Typeable (PerasError blk)
  , Show (PerasVote blk)
  , Show (PerasCert blk)
  , Show (PerasError blk)
  , Eq (PerasVote blk)
  , Eq (PerasCert blk)
  , Eq (PerasError blk)
  , NoThunks (PerasVote blk)
  , NoThunks (PerasCert blk)
  , NoThunks (PerasError blk)
  , IsPerasVote (PerasVote blk) blk
  , IsPerasCert (PerasCert blk) blk
  , IsPerasError (PerasError blk) blk
  ) =>
  BlockSupportsPeras blk
  where
  type PerasVote blk = (vote :: Type) | vote -> blk
  type PerasVote blk = VoidPerasVote blk

  type PerasCert blk = (cert :: Type) | cert -> blk
  type PerasCert blk = VoidPerasCert blk

  type PerasError blk = (err :: Type) | err -> blk
  type PerasError blk = EmptyPerasError blk

  validatePerasVote ::
    PerasParams ->
    VoteWeightDistr ->
    PerasVote blk ->
    Either (PerasError blk) (ValidatedPerasVote blk)
  default validatePerasVote ::
    PerasVote blk ~ VoidPerasVote blk =>
    PerasParams ->
    VoteWeightDistr ->
    PerasVote blk ->
    Either (PerasError blk) (ValidatedPerasVote blk)
  validatePerasVote _ _ vote =
    absurd (unVoidPerasVote vote)

  validatePerasCert ::
    PerasParams ->
    PerasCert blk ->
    Either (PerasError blk) (ValidatedPerasCert blk)
  default validatePerasCert ::
    PerasCert blk ~ VoidPerasCert blk =>
    PerasParams ->
    PerasCert blk ->
    Either (PerasError blk) (ValidatedPerasCert blk)
  validatePerasCert _ cert =
    absurd (unVoidPerasCert cert)

  forgePerasCert ::
    PerasParams ->
    ValidatedPerasVotesWithQuorum blk ->
    Either (PerasError blk) (ValidatedPerasCert blk)
  default forgePerasCert ::
    PerasVote blk ~ VoidPerasVote blk =>
    PerasParams ->
    ValidatedPerasVotesWithQuorum blk ->
    Either (PerasError blk) (ValidatedPerasCert blk)
  forgePerasCert _ votes =
    absurd (unVoidPerasVote (vpvVote (NonEmpty.head (vpvqVotes votes))))

  -- | Extract a Peras certificate optionally stored in a block.
  --
  -- Returns 'Nothing' if the block does not contain a Peras certificate, or
  -- if the block is from an era that does not support Peras certificates.
  getPerasCertInBlock ::
    blk ->
    Maybe (PerasCert blk)
  getPerasCertInBlock _ =
    Nothing

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

implPerasForgeVoteIfEligible ::
  forall blk.
  ( ElectionId (PerasCrypto blk) ~ PerasRoundNo -- TODO Remove later
  , VoteCandidate (PerasCrypto blk) ~ Point blk -- TODO Remove later
  , IsPerasError (PerasError blk) blk
  , CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk) -- TODO maybe remove this once part of 'BlockSupportsPeras' constraints
  , PerasVoteCompatibleWithVotingCommittee
      (PerasVote blk)
      (PerasCrypto blk)
      (PerasVotingCommitteeScheme blk)
  ) =>
  PerasVotingCommittee blk ->
  PoolId ->
  PrivateKey (PerasCrypto blk) ->
  PerasRoundNo ->
  Point blk ->
  Either (PerasError blk) (Maybe (ValidatedPerasVote blk))
implPerasForgeVoteIfEligible
  committee
  ourId
  ourPrivateKey
  roundNo
  boostedBlock =
    do
      mWitness <-
        bimap injectVotingCommitteeError id $
          Committee.checkShouldVote
            committee
            ourId
            ourPrivateKey
            roundNo
      case mWitness of
        Nothing ->
          Right Nothing
        Just witness ->
          Just <$> do
            let voteWeight = eligiblePartyVoteWeight committee witness
                abstractVote =
                  Committee.forgeVote
                    witness
                    ourPrivateKey
                    roundNo
                    (boostedBlockToPoint boostedBlock)
            concreteVote <-
              bimap injectConversionError id $
                toPerasVote @(PerasVote blk) abstractVote
            pure $
              ValidatedPerasVote
                { vpvVote = concreteVote
                , vpvVoteWeight = coerce voteWeight
                }

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
  getPerasVoteVoterId = absurd . unVoidPerasVote

instance IsPerasCert (VoidPerasCert blk) blk where
  getPerasCertRound = absurd . unVoidPerasCert
  getPerasCertBlock = absurd . unVoidPerasCert

-- | Empty Peras error for @blk@.
--
-- NOTE: the phantom @blk@ is used to keep the 'PerasError' type family injective.
--
-- NOTE: in contrast to 'VoidPerasVote' and 'VoidPerasCert', this type cannot be
-- uninhabited, or we would otherwise have to construct a `Void` when injecting
-- errors into this type, which would be impossible.
newtype EmptyPerasError blk
  = EmptyPerasError
  { unEmptyPerasError :: ()
  }
  deriving newtype (Show, Eq, NoThunks, ShowProxy)

instance IsPerasError (EmptyPerasError blk) blk where
  injectVotingCommitteeError _ = EmptyPerasError ()
  injectConversionError _ = EmptyPerasError ()

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

-- | A collection of validated Peras votes that:
-- 1. are all for the same target, and
-- 2. have total stake above the quorum threshold for a given 'PerasCfg'.
data ValidatedPerasVotesWithQuorum blk
  = ValidatedPerasVotesWithQuorum
  { vpvqTarget :: !(PerasVoteTarget blk)
  -- ^ The target that all the votes are for
  , vpvqVotes :: !(NonEmpty (ValidatedPerasVote blk))
  -- ^ The votes that reached quorum for the given target
  , vpvqPerasParams :: !PerasParams
  -- ^ The Peras parameters used to validate that the votes reach quorum
  }

deriving instance
  ( StandardHash blk
  , Show (PerasVote blk)
  ) =>
  Show (ValidatedPerasVotesWithQuorum blk)
deriving instance
  ( StandardHash blk
  , Eq (PerasVote blk)
  ) =>
  Eq (ValidatedPerasVotesWithQuorum blk)
deriving instance
  ( StandardHash blk
  , NoThunks (PerasVote blk)
  ) =>
  NoThunks (ValidatedPerasVotesWithQuorum blk)
deriving instance
  Generic (ValidatedPerasVotesWithQuorum blk)

-- | Smart constructor for 'ValidatedPerasVotesReachingQuorum'.
--
-- This function checks that all votes are for the same target, and that their
-- total stake is above the quorum threshold defined in the given 'PerasCfg'.
-- It returns 'Nothing' if either of these conditions is not met.
votesReachQuorum ::
  ( StandardHash blk
  , IsPerasVote (PerasVote blk) blk
  ) =>
  PerasParams ->
  [ValidatedPerasVote blk] ->
  Maybe (ValidatedPerasVotesWithQuorum blk)
votesReachQuorum params votes =
  case votes of
    -- We need at least one vote to determine who these votes are for, so we
    -- can't vacuously reach a quorum, even if the quorum threshold is 0.
    [] -> Nothing
    -- If we have at least one vote, we must check that all votes are for the
    -- same target, and that their total weight is above the quorum threshold.
    (v0 : vs)
      | not (allVotesMatchTarget v0 vs) ->
          Nothing
      | not votesHaveEnoughWeight ->
          Nothing
      | otherwise ->
          Just
            ValidatedPerasVotesWithQuorum
              { vpvqTarget = getPerasVoteTarget v0
              , vpvqVotes = v0 :| vs
              , vpvqPerasParams = params
              }
 where
  totalVoteWeight =
    mconcat (vpvVoteWeight <$> votes)
  votesHaveEnoughWeight =
    weightAboveThreshold params totalVoteWeight
  allVotesMatchTarget target =
    all ((== (getPerasVoteTarget target)) . getPerasVoteTarget)

-- * Convenience projection/injection classes

-- | Types that support being treated as Peras votes
class
  BoostedBlockCompatibleWithPoint (BoostedBlock vote) blk =>
  IsPerasVote vote blk
    | vote -> blk
  where
  getPerasVoteRound :: vote -> PerasRoundNo
  getPerasVoteBlock :: vote -> BoostedBlock vote
  getPerasVoteVoterId :: vote -> PerasVoterId

  getPerasVotePoint :: vote -> Point blk
  getPerasVotePoint = boostedBlockToPoint . getPerasVoteBlock

-- | Extract the vote ID from a Peras vote container
getPerasVoteId :: IsPerasVote vote blk => vote -> PerasVoteId blk
getPerasVoteId vote =
  PerasVoteId
    { pviRoundNo = getPerasVoteRound vote
    , pviVoterId = getPerasVoteVoterId vote
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
  getPerasVoteVoterId = getPerasVoteVoterId . vpvVote

instance
  IsPerasVote vote blk =>
  IsPerasVote (WithArrivalTime vote) blk
  where
  getPerasVoteRound = getPerasVoteRound . forgetArrivalTime
  getPerasVoteBlock = getPerasVoteBlock . forgetArrivalTime
  getPerasVoteVoterId = getPerasVoteVoterId . forgetArrivalTime

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
