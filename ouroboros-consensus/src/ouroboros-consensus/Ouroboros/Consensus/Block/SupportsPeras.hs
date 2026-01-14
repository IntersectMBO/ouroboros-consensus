{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Block.SupportsPeras
  ( -- * BlockSupportsPeras class
    PerasCert
  , PerasVote
  , BlockSupportsPeras (..)

    -- * Validated types
  , ValidatedPerasCert (..)
  , ValidatedPerasVote (..)

    -- * Error types
  , PerasValidationErr (..)
  , PerasForgeErr (..)

    -- * Utility functions
  , lookupPerasVoteStake

    -- * Field accessors
  , HasPerasCert (..)
  , HasPerasCertBoost (..)
  , HasPerasVote (..)
  , HasPerasVoteStake (..)
  ) where

import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Ouroboros.Consensus.Block.Abstract (Point, StandardHash)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime (..))
import qualified Ouroboros.Consensus.Peras.Cert as Base (PerasCert (..))
import Ouroboros.Consensus.Peras.Params (PerasWeight (..))
import Ouroboros.Consensus.Peras.Round (PerasRoundNo)
import Ouroboros.Consensus.Peras.Vote
  ( PerasVoteId (..)
  , PerasVoteStake (..)
  , PerasVoteStakeDistr (..)
  , PerasVoteTarget (..)
  , PerasVoterId (..)
  )
import qualified Ouroboros.Consensus.Peras.Vote as Base (PerasVote (..))

{-------------------------------------------------------------------------------
-- * BlockSupportsPeras class
-------------------------------------------------------------------------------}

data family PerasCert blk :: Type

data family PerasVote blk :: Type

class
  ( StandardHash blk
  , Typeable blk
  , Eq (PerasCert blk)
  , Eq (PerasVote blk)
  , Ord (PerasCert blk)
  , Ord (PerasVote blk)
  , Show (PerasCfg blk)
  , Show (PerasCert blk)
  , Show (PerasVote blk)
  , Show (PerasValidationErr blk)
  , Show (PerasForgeErr blk)
  , NoThunks (PerasCfg blk)
  , NoThunks (PerasCert blk)
  , NoThunks (PerasVote blk)
  , NoThunks (PerasValidationErr blk)
  , NoThunks (PerasForgeErr blk)
  , HasPerasCert (PerasCert blk)
  , HasPerasVote (PerasVote blk)
  , CertBoostedBlock (PerasCert blk) ~ blk
  , VoteBlock (PerasVote blk) ~ blk
  ) =>
  BlockSupportsPeras blk
  where
  type PerasCfg blk :: Type

  mkPerasCert ::
    PerasRoundNo ->
    Point blk ->
    PerasCert blk

  mkPerasVote ::
    PerasVoterId ->
    PerasRoundNo ->
    Point blk ->
    PerasVote blk

  validatePerasCert ::
    PerasCfg blk ->
    PerasCert blk ->
    Either (PerasValidationErr blk) (ValidatedPerasCert blk)

  validatePerasVote ::
    PerasCfg blk ->
    PerasVoteStakeDistr ->
    PerasVote blk ->
    Either (PerasValidationErr blk) (ValidatedPerasVote blk)

  forgePerasCert ::
    PerasCfg blk ->
    PerasVoteTarget blk ->
    [ValidatedPerasVote blk] ->
    Either (PerasForgeErr blk) (ValidatedPerasCert blk)

-- * Error types

-- TODO: enrich with actual error types
-- see https://github.com/tweag/cardano-peras/issues/120
data PerasValidationErr blk
  = PerasValidationErr
  deriving stock (Generic, Show, Eq)
  deriving anyclass NoThunks

-- TODO: enrich with actual error types
-- see https://github.com/tweag/cardano-peras/issues/120
data PerasForgeErr blk
  = PerasForgeErrInsufficientVotes
  | PerasForgeErrTargetMismatch
  deriving stock (Generic, Show, Eq)
  deriving anyclass NoThunks

-- * Validated types

data ValidatedPerasCert blk = ValidatedPerasCert
  { vpcCert :: !(PerasCert blk)
  , vpcCertBoost :: !PerasWeight
  }

deriving instance Show (PerasCert blk) => Show (ValidatedPerasCert blk)
deriving instance Eq (PerasCert blk) => Eq (ValidatedPerasCert blk)
deriving instance Ord (PerasCert blk) => Ord (ValidatedPerasCert blk)
deriving instance Generic (ValidatedPerasCert blk)
deriving instance NoThunks (PerasCert blk) => NoThunks (ValidatedPerasCert blk)

data ValidatedPerasVote blk = ValidatedPerasVote
  { vpvVote :: !(PerasVote blk)
  , vpvVoteStake :: !PerasVoteStake
  }

deriving instance Show (PerasVote blk) => Show (ValidatedPerasVote blk)
deriving instance Eq (PerasVote blk) => Eq (ValidatedPerasVote blk)
deriving instance Ord (PerasVote blk) => Ord (ValidatedPerasVote blk)
deriving instance Generic (ValidatedPerasVote blk)
deriving instance NoThunks (PerasVote blk) => NoThunks (ValidatedPerasVote blk)

-- * Utility functions

-- | Lookup the stake of a vote cast by a member of a given stake distribution
lookupPerasVoteStake ::
  HasPerasVote vote =>
  vote ->
  PerasVoteStakeDistr ->
  Maybe PerasVoteStake
lookupPerasVoteStake vote distr =
  Map.lookup
    (getPerasVoteVoterId vote)
    (unPerasVoteStakeDistr distr)

-- | Lookup the stake of a vote cast by a member of a given stake distribution

-- * Field accessors

-- | Extract fields from a Peras certificate
class HasPerasCert cert where
  type CertBoostedBlock cert :: Type
  getPerasCertRound :: cert -> PerasRoundNo
  getPerasCertBoostedBlock :: cert -> Point (CertBoostedBlock cert)

instance HasPerasCert (Base.PerasCert blk) where
  type CertBoostedBlock (Base.PerasCert blk) = blk
  getPerasCertRound = Base.pcCertRound
  getPerasCertBoostedBlock = Base.pcCertBoostedBlock

instance
  HasPerasCert (PerasCert blk) =>
  HasPerasCert (ValidatedPerasCert blk)
  where
  type CertBoostedBlock (ValidatedPerasCert blk) = CertBoostedBlock (PerasCert blk)
  getPerasCertRound = getPerasCertRound . vpcCert
  getPerasCertBoostedBlock = getPerasCertBoostedBlock . vpcCert

instance
  HasPerasCert cert =>
  HasPerasCert (WithArrivalTime cert)
  where
  type CertBoostedBlock (WithArrivalTime cert) = CertBoostedBlock cert
  getPerasCertRound = getPerasCertRound . forgetArrivalTime
  getPerasCertBoostedBlock = getPerasCertBoostedBlock . forgetArrivalTime

-- | Extract fields from a Peras vote
class HasPerasVote vote where
  type VoteBlock vote :: Type
  getPerasVoteRound :: vote -> PerasRoundNo
  getPerasVoteVoterId :: vote -> PerasVoterId
  getPerasVoteTarget :: vote -> PerasVoteTarget (VoteBlock vote)
  getPerasVoteId :: vote -> PerasVoteId (VoteBlock vote)

instance HasPerasVote (Base.PerasVote blk) where
  type VoteBlock (Base.PerasVote blk) = blk
  getPerasVoteRound = Base.pvVoteRound
  getPerasVoteVoterId = Base.pvVoteVoterId
  getPerasVoteTarget vote =
    PerasVoteTarget
      { pvtRoundNo = Base.pvVoteRound vote
      , pvtBlock = Base.pvVoteBlock vote
      }
  getPerasVoteId vote =
    PerasVoteId
      { pviRoundNo = Base.pvVoteRound vote
      , pviVoterId = Base.pvVoteVoterId vote
      }

instance
  HasPerasVote (PerasVote blk) =>
  HasPerasVote (ValidatedPerasVote blk)
  where
  type VoteBlock (ValidatedPerasVote blk) = VoteBlock (PerasVote blk)
  getPerasVoteRound = getPerasVoteRound . vpvVote
  getPerasVoteVoterId = getPerasVoteVoterId . vpvVote
  getPerasVoteTarget = getPerasVoteTarget . vpvVote
  getPerasVoteId = getPerasVoteId . vpvVote

instance
  HasPerasVote vote =>
  HasPerasVote (WithArrivalTime vote)
  where
  type VoteBlock (WithArrivalTime vote) = VoteBlock vote
  getPerasVoteRound = getPerasVoteRound . forgetArrivalTime
  getPerasVoteVoterId = getPerasVoteVoterId . forgetArrivalTime
  getPerasVoteTarget = getPerasVoteTarget . forgetArrivalTime
  getPerasVoteId = getPerasVoteId . forgetArrivalTime

-- | Extract the certificate boost from a Peras certificate container
class HasPerasCertBoost cert where
  getPerasCertBoost :: cert -> PerasWeight

instance HasPerasCertBoost (ValidatedPerasCert blk) where
  getPerasCertBoost = vpcCertBoost

instance
  HasPerasCertBoost cert =>
  HasPerasCertBoost (WithArrivalTime cert)
  where
  getPerasCertBoost = getPerasCertBoost . forgetArrivalTime

-- | Extract the vote stake from a validated Peras vote container
class HasPerasVoteStake vote where
  getPerasVoteStake :: vote -> PerasVoteStake

instance HasPerasVoteStake (ValidatedPerasVote blk) where
  getPerasVoteStake = vpvVoteStake

instance
  HasPerasVoteStake vote =>
  HasPerasVoteStake (WithArrivalTime vote)
  where
  getPerasVoteStake = getPerasVoteStake . forgetArrivalTime
