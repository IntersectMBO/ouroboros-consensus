{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Block.SupportsPeras
  ( -- * BlockSupportsPeras class
    BlockSupportsPeras (..)

    -- * Utility functions
  , lookupPerasVoteStake

    -- * Validated votes reaching quorum
  , ValidatedPerasVotesReachingQuorum (..)
  , votesReachQuorum

    -- * Field accessors
  , IsPerasCert (..)
  , IsValidatedPerasCert (..)
  , IsPerasVote (..)
  , IsValidatedPerasVote (..)
  ) where

import Control.Exception (Exception)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
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
import Ouroboros.Consensus.Ledger.Abstract (LedgerState)

{-------------------------------------------------------------------------------
-- * BlockSupportsPeras class
-------------------------------------------------------------------------------}

class
  ( StandardHash blk
  , Typeable blk
  -- , Eq (PerasConfig blk): Actually, we don't need it, and it's hard to implement for PerasConfig for HFC block
  , Eq (PerasErr blk)
  , Eq (PerasCert blk)
  , Eq (ValidatedPerasCert blk)
  , Eq (PerasVote blk)
  , Eq (ValidatedPerasVote blk)
  , Ord (PerasCert blk)
  , Ord (ValidatedPerasCert blk)
  , Ord (PerasVote blk)
  , Ord (ValidatedPerasVote blk)
  , Show (PerasConfig blk)
  , Show (PerasErr blk)
  , Show (PerasCert blk)
  , Show (ValidatedPerasCert blk)
  , Show (PerasVote blk)
  , Show (ValidatedPerasVote blk)
  , NoThunks (PerasConfig blk)
  , NoThunks (PerasErr blk)
  , NoThunks (PerasCert blk)
  , NoThunks (ValidatedPerasCert blk)
  , NoThunks (PerasVote blk)
  , NoThunks (ValidatedPerasVote blk)
  , IsPerasCert blk (PerasCert blk)
  , IsPerasCert blk (ValidatedPerasCert blk)
  , IsValidatedPerasCert blk (ValidatedPerasCert blk)
  , IsPerasVote blk (PerasVote blk)
  , IsPerasVote blk (ValidatedPerasVote blk)
  , IsValidatedPerasVote blk (ValidatedPerasVote blk)
  , Exception (PerasErr blk)
  ) =>
  BlockSupportsPeras blk
  where
  type PerasConfig blk
  type PerasErr blk

  type PerasVote blk
  type ValidatedPerasVote blk

  forgePerasVote ::
    PerasConfig blk ->
    LedgerState blk mk ->
    PerasVoterId ->
    PerasRoundNo ->
    Point blk ->
    Either (PerasErr blk) (ValidatedPerasVote blk)

  validatePerasVote ::
    PerasConfig blk ->
    LedgerState blk mk ->
    PerasVote blk ->
    Either (PerasErr blk) (ValidatedPerasVote blk)

  type PerasCert blk
  type ValidatedPerasCert blk

  forgePerasCert ::
    ValidatedPerasVotesReachingQuorum blk ->
    Either (PerasErr blk) (ValidatedPerasCert blk)

  validatePerasCert ::
    PerasConfig blk ->
    PerasCert blk ->
    Either (PerasErr blk) (ValidatedPerasCert blk)

-- * Utility functions

-- | Lookup the stake of a vote cast by a member of a given stake distribution
lookupPerasVoteStake ::
  IsPerasVote blk vote =>
  vote ->
  LedgerState blk mk ->
  Maybe PerasVoteStake
lookupPerasVoteStake _vote _distr = undefined

-- ** Votes with enough stake to reach quorum for a given target

-- | A collection of validated Peras votes that:
-- 1. are all for the same target, and
-- 2. have total stake above the quorum threshold for a given 'PerasConfig'.
data ValidatedPerasVotesReachingQuorum blk = ValidatedPerasVotesReachingQuorum
  { vpvqTarget :: !(PerasVoteTarget blk)
  -- ^ The target that all the votes are for
  , vpvqVotes :: !(NonEmpty (ValidatedPerasVote blk))
  -- ^ The votes that reached quorum for the given target
  , vpvqPerasConfig :: !(PerasConfig blk)
  -- ^ The Peras configuration used to validate that the votes reach quorum
  }
  deriving stock Generic

deriving stock instance BlockSupportsPeras blk => Show (ValidatedPerasVotesReachingQuorum blk)
deriving anyclass instance
  BlockSupportsPeras blk => NoThunks (ValidatedPerasVotesReachingQuorum blk)

-- | Smart constructor for 'ValidatedPerasVotesReachingQuorum'.
--
-- This function checks that all votes are for the same target, and that their
-- total stake is above the quorum threshold defined in the given 'PerasConfig'.
-- It returns 'Nothing' if either of these conditions is not met.
votesReachQuorum ::
  StandardHash blk =>
  PerasConfig blk ->
  [ValidatedPerasVote blk] ->
  Maybe (ValidatedPerasVotesReachingQuorum blk)
votesReachQuorum cfg votes = undefined

--   case votes of
--     -- We need at least one vote to determine who these votes are for, so we
--     -- can't vacuously reach a quorum, even if the quorum threshold is 0.
--     [] -> Nothing
--     -- If we have at least one vote, we must check that all votes are for the
--     -- same target, and that their total stake of is above the quorum threshold.
--     (v0 : vs)
--       | not (allVotesMatchTarget v0 vs) ->
--           Nothing
--       | not votesHaveEnoughStake ->
--           Nothing
--       | otherwise ->
--           Just
--             ValidatedPerasVotesReachingQuorum
--               { vpvqTarget = getPerasVoteTarget v0
--               , vpvqVotes = v0 :| vs
--               , vpvqPerasConfig = cfg
--               }
--  where
--   totalVoteStake =
--     mconcat (vpvVoteStake <$> votes)
--   votesHaveEnoughStake =
--     stakeAboveThreshold cfg totalVoteStake
--   allVotesMatchTarget target =
--     all ((== (getPerasVoteTarget target)) . getPerasVoteTarget)

-- * Field accessors

-- | Extract fields from a Peras certificate
class IsPerasCert blk cert | cert -> blk where
  getPerasCertRound :: cert -> PerasRoundNo
  getPerasCertBoostedBlock :: cert -> Point blk

instance IsPerasCert blk (Base.PerasCert blk) where
  getPerasCertRound :: Base.PerasCert blk -> PerasRoundNo
  getPerasCertRound = Base.pcCertRound
  getPerasCertBoostedBlock = Base.pcCertBoostedBlock

instance IsPerasCert blk (Base.ValidatedPerasCert blk) where
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
  getPerasVoteBlock :: vote -> Point blk

  getPerasVoteId :: vote -> PerasVoteId blk
  getPerasVoteId vote =
    PerasVoteId
      { pviRoundNo = getPerasVoteRound vote
      , pviVoterId = getPerasVoteVoterId vote
      }

  getPerasVoteTarget :: vote -> PerasVoteTarget blk
  getPerasVoteTarget vote =
    PerasVoteTarget
      { pvtRoundNo = getPerasVoteRound vote
      , pvtBlock = getPerasVoteBlock vote
      }

instance IsPerasVote blk (Base.PerasVote blk) where
  getPerasVoteRound = Base.pvVoteRound
  getPerasVoteVoterId = Base.pvVoteVoterId

  getPerasVoteBlock = Base.pvVoteBlock

instance IsPerasVote blk (Base.ValidatedPerasVote blk) where
  getPerasVoteRound = getPerasVoteRound . Base.vpvVote
  getPerasVoteVoterId = getPerasVoteVoterId . Base.vpvVote

  getPerasVoteBlock = getPerasVoteBlock . Base.vpvVote

instance
  IsPerasVote blk vote =>
  IsPerasVote blk (WithArrivalTime vote)
  where
  getPerasVoteRound = getPerasVoteRound . forgetArrivalTime
  getPerasVoteVoterId = getPerasVoteVoterId . forgetArrivalTime

  getPerasVoteBlock = getPerasVoteBlock . forgetArrivalTime

class IsPerasVote blk vote => IsValidatedPerasVote blk vote | vote -> blk where
  getPerasVoteStake :: vote -> PerasVoteStake

instance IsValidatedPerasVote blk (Base.ValidatedPerasVote blk) where
  getPerasVoteStake = Base.vpvVoteStake

instance
  IsValidatedPerasVote blk vote =>
  IsValidatedPerasVote blk (WithArrivalTime vote)
  where
  getPerasVoteStake = getPerasVoteStake . forgetArrivalTime
