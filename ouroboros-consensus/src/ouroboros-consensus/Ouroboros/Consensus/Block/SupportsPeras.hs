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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , VoidPerasVote (..)
  , VoidPerasCert (..)
  , VoidPerasError (..)
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

    -- * Convenience re-exports
  , module Ouroboros.Consensus.Peras.Params
  , module Ouroboros.Consensus.Peras.Types
  ) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime (..))
import Ouroboros.Consensus.Peras.Params
import Ouroboros.Consensus.Peras.Types
import Ouroboros.Consensus.Util (ShowProxy)

-- * BlockSupportsPeras class

class
  ( StandardHash blk
  , Typeable blk
  , Typeable (PerasVote blk)
  , Typeable (PerasCert blk)
  , IsPerasVote (PerasVote blk) blk
  , IsPerasCert (PerasCert blk) blk
  , Show (PerasVote blk)
  , Show (PerasCert blk)
  , Show (PerasError blk)
  , Eq (PerasVote blk)
  , Eq (PerasCert blk)
  , Eq (PerasError blk)
  , NoThunks (PerasVote blk)
  , NoThunks (PerasCert blk)
  , NoThunks (PerasError blk)
  ) =>
  BlockSupportsPeras blk
  where
  type PerasVote blk = (vote :: Type) | vote -> blk
  type PerasVote blk = VoidPerasVote blk

  type PerasCert blk = (cert :: Type) | cert -> blk
  type PerasCert blk = VoidPerasCert blk

  type PerasError blk = (err :: Type) | err -> blk
  type PerasError blk = VoidPerasError blk

  validatePerasVote ::
    PerasParams ->
    PerasVoteStakeDistr ->
    PerasVote blk ->
    Either (PerasError blk) (ValidatedPerasVote blk)
  default validatePerasVote ::
    PerasVote blk ~ VoidPerasVote blk =>
    PerasParams ->
    PerasVoteStakeDistr ->
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

instance IsPerasVote (VoidPerasVote blk) blk where
  getPerasVoteRound = absurd . unVoidPerasVote
  getPerasVoteBlock = absurd . unVoidPerasVote
  getPerasVoteVoterId = absurd . unVoidPerasVote

instance IsPerasCert (VoidPerasCert blk) blk where
  getPerasCertRound = absurd . unVoidPerasCert
  getPerasCertBlock = absurd . unVoidPerasCert

-- | Imposible Peras error for @blk@.
--
-- NOTE: the phantom @blk@ is used to keep the 'PerasError' type family injective.
newtype VoidPerasError blk
  = VoidPerasError
  { unVoidPerasError :: Void
  }
  deriving newtype (Show, Eq, NoThunks, ShowProxy)

-- * Validated types

data ValidatedPerasVote blk
  = ValidatedPerasVote
  { vpvVote :: !(PerasVote blk)
  , vpvVoteStake :: !PerasVoteStake
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
    -- same target, and that their total stake of is above the quorum threshold.
    (v0 : vs)
      | not (allVotesMatchTarget v0 vs) ->
          Nothing
      | not votesHaveEnoughStake ->
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
    mconcat (vpvVoteStake <$> votes)
  votesHaveEnoughStake =
    stakeAboveThreshold params totalVoteStake
  allVotesMatchTarget target =
    all ((== (getPerasVoteTarget target)) . getPerasVoteTarget)

-- * Convenience projection classes

-- | Types that support being treated as Peras votes
class IsPerasVote vote blk | vote -> blk where
  getPerasVoteRound :: vote -> PerasRoundNo
  getPerasVoteBlock :: vote -> Point blk
  getPerasVoteVoterId :: vote -> PerasVoterId

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
    , pvtBlock = getPerasVoteBlock vote
    }

instance
  IsPerasVote (PerasVote blk) blk =>
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
class IsPerasCert cert blk | cert -> blk where
  getPerasCertRound :: cert -> PerasRoundNo
  getPerasCertBlock :: cert -> Point blk

instance
  IsPerasCert (PerasCert blk) blk =>
  IsPerasCert (ValidatedPerasCert blk) blk
  where
  getPerasCertRound = getPerasCertRound . vpcCert
  getPerasCertBlock = getPerasCertBlock . vpcCert

instance IsPerasCert cert blk => IsPerasCert (WithArrivalTime cert) blk where
  getPerasCertRound = getPerasCertRound . forgetArrivalTime
  getPerasCertBlock = getPerasCertBlock . forgetArrivalTime
