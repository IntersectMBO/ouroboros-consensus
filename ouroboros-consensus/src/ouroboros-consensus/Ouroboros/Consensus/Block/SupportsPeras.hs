{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Ouroboros.Consensus.Block.SupportsPeras
  ( -- * BlockSupportsPeras class
    BlockSupportsPeras (..)

    -- * Utility functions
  , lookupPerasVoteStake

    -- * Field accessors
  , IsPerasCert (..)
  , IsValidatedPerasCert (..)
  , IsPerasVote (..)
  , IsValidatedPerasVote (..)
  ) where

import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import NoThunks.Class (NoThunks (..))
import Ouroboros.Consensus.Block.Abstract (Point, StandardHash)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime (..))
import qualified Ouroboros.Consensus.Peras.Cert as Base (PerasCert (..), ValidatedPerasCert (..))
import Ouroboros.Consensus.Peras.Params (PerasWeight (..))
import Ouroboros.Consensus.Peras.Round (PerasRoundNo)
import Ouroboros.Consensus.Peras.Vote
  ( PerasVoteId (..)
  , PerasVoteStake (..)
  , PerasVoteStakeDistr (..)
  , PerasVoteTarget (..)
  , PerasVoterId (..)
  )
import qualified Ouroboros.Consensus.Peras.Vote as Base (PerasVote (..), ValidatedPerasVote (..))

import Control.Exception (Exception)

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
  , Show (PerasCertValidationErr blk)
  , Show (PerasCertForgeErr blk)
  , Show (PerasVote blk)
  , Show (PerasVoteValidationErr blk)
  , Show (PerasVoteForgeErr blk)
  , NoThunks (PerasCfg blk)
  , NoThunks (PerasCert blk)
  , NoThunks (PerasCertForgeErr blk)
  , NoThunks (PerasCertValidationErr blk)
  , NoThunks (PerasVote blk)
  , NoThunks (PerasVoteValidationErr blk)
  , NoThunks (PerasVoteForgeErr blk)
  , IsPerasCert blk (PerasCert blk)
  , IsValidatedPerasCert (ValidatedPerasCert blk) blk
  , IsPerasVote blk (PerasVote blk)
  , IsValidatedPerasVote (ValidatedPerasVote blk) blk
  , Exception (PerasCertValidationErr blk)
  , Exception (PerasCertForgeErr blk)
  , Exception (PerasVoteValidationErr blk)
  , Exception (PerasVoteForgeErr blk)
  ) =>
  BlockSupportsPeras blk
  where
  type PerasCfg blk

  type PerasVote blk
  type ValidatedPerasVote blk
  type PerasVoteForgeErr blk
  type PerasVoteValidationErr blk

  forgePerasVote ::
    PerasCfg blk ->
    PerasVoteStakeDistr ->
    PerasVoterId ->
    PerasRoundNo ->
    Point blk ->
    Either (PerasVoteForgeErr blk) (ValidatedPerasVote blk)

  validatePerasVote ::
    PerasCfg blk ->
    PerasVoteStakeDistr ->
    PerasVote blk ->
    Either (PerasVoteValidationErr blk) (ValidatedPerasVote blk)

  type PerasCert blk
  type ValidatedPerasCert blk
  type PerasCertForgeErr blk
  type PerasCertValidationErr blk

  forgePerasCert ::
    PerasCfg blk ->
    PerasRoundNo ->
    Point blk ->
    [ValidatedPerasVote blk] ->
    Either (PerasCertForgeErr blk) (ValidatedPerasCert blk)

  validatePerasCert ::
    PerasCfg blk ->
    PerasCert blk ->
    Either (PerasCertValidationErr blk) (ValidatedPerasCert blk)

-- * Utility functions

-- | Lookup the stake of a vote cast by a member of a given stake distribution
lookupPerasVoteStake ::
  IsPerasVote blk vote =>
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
class IsPerasCert blk cert | cert -> blk where
  getPerasCertRound :: cert -> PerasRoundNo
  getPerasCertBoostedBlock :: cert -> Point blk

instance IsPerasCert blk (Base.PerasCert blk) where
  getPerasCertRound = Base.pcCertRound
  getPerasCertBoostedBlock = Base.pcCertBoostedBlock

instance
  IsPerasCert blk (Base.ValidatedPerasCert blk)
  where
  getPerasCertRound = getPerasCertRound . Base.vpcCert
  getPerasCertBoostedBlock = getPerasCertBoostedBlock . Base.vpcCert

instance
  IsPerasCert blk cert =>
  IsPerasCert blk (WithArrivalTime cert)
  where
  getPerasCertRound = getPerasCertRound . forgetArrivalTime
  getPerasCertBoostedBlock = getPerasCertBoostedBlock . forgetArrivalTime

class IsPerasCert blk cert => IsValidatedPerasCert blk cert | cert -> blk where
  getPerasCertBoost :: cert -> PerasWeight

instance IsValidatedPerasCert blk (Base.ValidatedPerasCert blk) where
  getPerasCertBoost = Base.vpcCertBoost

instance
  IsValidatedPerasCert blk cert =>
  IsValidatedPerasCert blk (WithArrivalTime cert)
  where
  getPerasCertBoost = getPerasCertBoost . forgetArrivalTime

-- | Extract fields from a Peras vote
class IsPerasVote blk vote | vote -> blk where
  getPerasVoteRound :: vote -> PerasRoundNo
  getPerasVoteVoterId :: vote -> PerasVoterId
  getPerasVoteTarget :: vote -> PerasVoteTarget blk
  getPerasVoteId :: vote -> PerasVoteId blk

instance IsPerasVote blk (Base.PerasVote blk) where
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
  IsPerasVote blk (Base.ValidatedPerasVote blk)
  where
  getPerasVoteRound = getPerasVoteRound . Base.vpvVote
  getPerasVoteVoterId = getPerasVoteVoterId . Base.vpvVote
  getPerasVoteTarget = getPerasVoteTarget . Base.vpvVote
  getPerasVoteId = getPerasVoteId . Base.vpvVote

instance
  IsPerasVote blk vote =>
  IsPerasVote blk (WithArrivalTime vote)
  where
  getPerasVoteRound = getPerasVoteRound . forgetArrivalTime
  getPerasVoteVoterId = getPerasVoteVoterId . forgetArrivalTime
  getPerasVoteTarget = getPerasVoteTarget . forgetArrivalTime
  getPerasVoteId = getPerasVoteId . forgetArrivalTime

class IsPerasVote blk vote => IsValidatedPerasVote blk vote | vote -> blk where
  getPerasVoteStake :: vote -> PerasVoteStake

instance IsValidatedPerasVote blk (Base.ValidatedPerasVote blk) where
  getPerasVoteStake = Base.vpvVoteStake

instance
  IsValidatedPerasVote blk vote =>
  IsValidatedPerasVote blk (WithArrivalTime vote)
  where
  getPerasVoteStake = getPerasVoteStake . forgetArrivalTime
