{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Block.SupportsPeras
  ( -- * BlockSupportsPeras class
    BlockSupportsPeras (..)

    -- * Validated types
  , ValidatedPerasCert (..)
  , ValidatedPerasVote (..)

    -- * Error types
  , PerasValidationErr (..)
  , PerasForgeErr (..)

    -- * Utility functions
  , lookupPerasVoteStake

    -- * Field accessors
  , HasPerasCertRound (..)
  , HasPerasCertBoostedBlock (..)
  , HasPerasCertBoost (..)
  , HasPerasVoteRound (..)
  , HasPerasVoteBlock (..)
  , HasPerasVoteVoterId (..)
  , HasPerasVoteStake (..)
  , HasPerasVoteTarget (..)
  , HasPerasVoteId (..)
  ) where

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
  ) =>
  BlockSupportsPeras blk
  where
  type PerasCfg blk

  data PerasCert blk

  data PerasVote blk

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
  HasPerasVoteVoterId (PerasVote blk) =>
  PerasVote blk ->
  PerasVoteStakeDistr ->
  Maybe PerasVoteStake
lookupPerasVoteStake vote distr =
  Map.lookup
    (getPerasVoteVoterId vote)
    (unPerasVoteStakeDistr distr)

-- | Lookup the stake of a vote cast by a member of a given stake distribution

-- * Field accessors

-- | Extract the certificate round from a Peras certificate container
class HasPerasCertRound cert where
  getPerasCertRound :: cert -> PerasRoundNo

instance HasPerasCertRound (Base.PerasCert blk) where
  getPerasCertRound = Base.pcCertRound

instance
  HasPerasCertRound (PerasCert blk) =>
  HasPerasCertRound (ValidatedPerasCert blk)
  where
  getPerasCertRound = getPerasCertRound . vpcCert

instance
  HasPerasCertRound cert =>
  HasPerasCertRound (WithArrivalTime cert)
  where
  getPerasCertRound = getPerasCertRound . forgetArrivalTime

-- | Extract the boosted block point from a Peras certificate container
class HasPerasCertBoostedBlock cert blk | cert -> blk where
  getPerasCertBoostedBlock :: cert -> Point blk

instance HasPerasCertBoostedBlock (Base.PerasCert blk) blk where
  getPerasCertBoostedBlock = Base.pcCertBoostedBlock

instance
  HasPerasCertBoostedBlock (PerasCert blk) blk =>
  HasPerasCertBoostedBlock (ValidatedPerasCert blk) blk
  where
  getPerasCertBoostedBlock = getPerasCertBoostedBlock . vpcCert

instance
  HasPerasCertBoostedBlock cert blk =>
  HasPerasCertBoostedBlock (WithArrivalTime cert) blk
  where
  getPerasCertBoostedBlock = getPerasCertBoostedBlock . forgetArrivalTime

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

-- | Extract the vote round from a Peras vote container
class HasPerasVoteRound vote where
  getPerasVoteRound :: vote -> PerasRoundNo

instance HasPerasVoteRound (Base.PerasVote blk) where
  getPerasVoteRound = Base.pvVoteRound

instance
  HasPerasVoteRound (PerasVote blk) =>
  HasPerasVoteRound (ValidatedPerasVote blk)
  where
  getPerasVoteRound = getPerasVoteRound . vpvVote

instance
  HasPerasVoteRound vote =>
  HasPerasVoteRound (WithArrivalTime vote)
  where
  getPerasVoteRound = getPerasVoteRound . forgetArrivalTime

-- | Extract the vote block point from a Peras vote container
class HasPerasVoteBlock vote blk | vote -> blk where
  getPerasVoteBlock :: vote -> Point blk

instance HasPerasVoteBlock (Base.PerasVote blk) blk where
  getPerasVoteBlock = Base.pvVoteBlock

instance
  HasPerasVoteBlock (PerasVote blk) blk =>
  HasPerasVoteBlock (ValidatedPerasVote blk) blk
  where
  getPerasVoteBlock = getPerasVoteBlock . vpvVote

instance
  HasPerasVoteBlock vote blk =>
  HasPerasVoteBlock (WithArrivalTime vote) blk
  where
  getPerasVoteBlock = getPerasVoteBlock . forgetArrivalTime

-- | Extract the stake pool ID from a Peras vote container
class HasPerasVoteVoterId vote where
  getPerasVoteVoterId :: vote -> PerasVoterId

instance HasPerasVoteVoterId (Base.PerasVote blk) where
  getPerasVoteVoterId = Base.pvVoteVoterId

instance
  HasPerasVoteVoterId (PerasVote blk) =>
  HasPerasVoteVoterId (ValidatedPerasVote blk)
  where
  getPerasVoteVoterId = getPerasVoteVoterId . vpvVote

instance
  HasPerasVoteVoterId vote =>
  HasPerasVoteVoterId (WithArrivalTime vote)
  where
  getPerasVoteVoterId = getPerasVoteVoterId . forgetArrivalTime

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

-- | Extract the vote target from a Peras vote container
class HasPerasVoteTarget vote blk | vote -> blk where
  getPerasVoteTarget :: vote -> PerasVoteTarget blk

instance HasPerasVoteTarget (Base.PerasVote blk) blk where
  getPerasVoteTarget vote =
    PerasVoteTarget
      { pvtRoundNo = Base.pvVoteRound vote
      , pvtBlock = Base.pvVoteBlock vote
      }

instance
  HasPerasVoteTarget (PerasVote blk) blk =>
  HasPerasVoteTarget (ValidatedPerasVote blk) blk
  where
  getPerasVoteTarget = getPerasVoteTarget . vpvVote

instance
  HasPerasVoteTarget vote blk =>
  HasPerasVoteTarget (WithArrivalTime vote) blk
  where
  getPerasVoteTarget = getPerasVoteTarget . forgetArrivalTime

-- | Extract the vote ID from a Peras vote container
class HasPerasVoteId vote blk | vote -> blk where
  getPerasVoteId :: vote -> PerasVoteId blk

instance HasPerasVoteId (Base.PerasVote blk) blk where
  getPerasVoteId vote =
    PerasVoteId
      { pviRoundNo = Base.pvVoteRound vote
      , pviVoterId = Base.pvVoteVoterId vote
      }

instance
  HasPerasVoteId (PerasVote blk) blk =>
  HasPerasVoteId (ValidatedPerasVote blk) blk
  where
  getPerasVoteId = getPerasVoteId . vpvVote

instance
  HasPerasVoteId vote blk =>
  HasPerasVoteId (WithArrivalTime vote) blk
  where
  getPerasVoteId = getPerasVoteId . forgetArrivalTime
