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

    -- * To be removed in favor of using per-blk definitions
  , PerasCert' (..)
  , PerasVote' (..)

    -- * Types and functions related to Peras vote collection and quorum checking
  , PerasVoteStakeDistr (..)
  , ValidatedPerasVotesWithQuorum
    ( vpvqTarget
    , vpvqVotes
    , vpvqPerasParams
    )
  , votesReachQuorum

    -- * Validated types
  , ValidatedPerasCert (..)
  , ValidatedPerasVote (..)

    -- * Peras error types
  , IsPerasError (..)

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
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Committee.Class
  ( CryptoSupportsVotingCommittee (..)
  , VotingCommittee
  )
import Ouroboros.Consensus.Peras.Cert.Class
import Ouroboros.Consensus.Peras.Params
import Ouroboros.Consensus.Peras.Types
import Ouroboros.Consensus.Peras.Void
import Ouroboros.Consensus.Peras.Vote.Class
import Ouroboros.Consensus.Peras.Voting.Adapter (PerasConversionError)
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

{-------------------------------------------------------------------------------
-- * Peras types
-------------------------------------------------------------------------------}

-- TODO: to be removed in favor of using a 'PerasEpochContext' directly.
newtype PerasVoteStakeDistr = PerasVoteStakeDistr
  { unPerasVoteStakeDistr :: Map PerasSeatIndex VoteWeight
  }
  deriving newtype NoThunks
  deriving stock (Show, Eq, Generic)

-- ** Votes with enough stake to reach quorum for a given target

-- | A collection of validated Peras votes that:
-- 1. are all for the same target, and
-- 2. have total stake above the quorum threshold for a given 'PerasParams'.
data ValidatedPerasVotesWithQuorum blk = ValidatedPerasVotesWithQuorum
  { vpvqTarget :: !(PerasVoteTarget blk)
  -- ^ The target that all the votes are for
  , vpvqVotes :: !(NonEmpty (ValidatedPerasVote blk))
  -- ^ The votes that reached quorum for the given target
  , vpvqPerasParams :: !(PerasParams blk)
  -- ^ The Peras configuration used to validate that the votes reach quorum
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

-- | Smart constructor for 'ValidatedPerasVotesReachingQuorum'.
--
-- This function checks that all votes are for the same target, and that their
-- total stake is above the quorum threshold defined in the given 'PerasParams'.
-- It returns 'Nothing' if either of these conditions is not met.
votesReachQuorum ::
  ( StandardHash blk
  , IsPerasVote (PerasVote blk) blk
  ) =>
  PerasParams blk ->
  [ValidatedPerasVote blk] ->
  Maybe (ValidatedPerasVotesWithQuorum blk)
votesReachQuorum params votes =
  case votes of
    -- We need at least one vote to determine who these votes are for, so we
    -- can't vacuously reach a quorum, even if the quorum threshold is 0.
    [] -> Nothing
    -- If we have at least one vote, we must check that all votes are for the
    -- same target, and that their total stake of is above the quorum threshold.
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
  totalVoteStake =
    mconcat (vpvVoteWeight <$> votes)
  votesHaveEnoughWeight =
    weightAboveThreshold params totalVoteStake
  allVotesMatchTarget target =
    all ((== (getPerasVoteTarget target)) . getPerasVoteTarget)

{-------------------------------------------------------------------------------
-- * BlockSupportsPeras class
-------------------------------------------------------------------------------}

class
  ( Show (PerasParams blk)
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
    ValidatedPerasVotesWithQuorum blk ->
    Either (PerasError blk) (ValidatedPerasCert blk)

  -- | Extract a Peras certificate optionally stored in a block.
  --
  -- Returns 'Nothing' if the block does not contain a Peras certificate, or
  -- if the block is from an era that does not support Peras certificates.
  getPerasCertInBlock ::
    blk ->
    Maybe (PerasCert blk)

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
              { pcCertRound = pvtRoundNo (vpvqTarget votes)
              , pcCertBoostedBlock = pvtBlock (vpvqTarget votes)
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
