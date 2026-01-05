{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Node.Peras
  (
  ) where

import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , HasPerasCertBoost
  , HasPerasCertBoostedBlock
  , HasPerasCertRound (..)
  , HasPerasVoteBlock
  , HasPerasVoteId
  , HasPerasVoteRound
  , HasPerasVoteStake
  , HasPerasVoteTarget (..)
  , HasPerasVoteVoterId
  , PerasForgeErr (..)
  , PerasValidationErr (..)
  , ValidatedPerasCert (..)
  , ValidatedPerasVote (..)
  , lookupPerasVoteStake
  )
import Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork (CanHardFork)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
  ( OneEraPerasCert
  , OneEraPerasVote
  )
import Ouroboros.Consensus.HardFork.Combinator.Basics (HardForkBlock)
import Ouroboros.Consensus.HardFork.Combinator.Block ()
import Ouroboros.Consensus.Peras.Params (PerasParams (..))
import Ouroboros.Consensus.Peras.Vote (PerasVoteTarget (..), stakeAboveThreshold)

{-------------------------------------------------------------------------------
  Peras
-------------------------------------------------------------------------------}

instance CanHardFork xs => BlockSupportsPeras (HardForkBlock xs) where
  type PerasCfg (HardForkBlock xs) = PerasParams

  newtype PerasCert (HardForkBlock xs) = HardForkPerasCert
    { getHardForkPerasCert :: OneEraPerasCert xs
    }
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass NoThunks

  newtype PerasVote (HardForkBlock xs) = HardForkPerasVote
    { getHardForkPerasVote :: OneEraPerasVote xs
    }
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass NoThunks

  mkPerasCert =
    error "mkPerasCert: not implemented for HardForkBlock"

  mkPerasVote =
    error "mkPerasVote: not implemented for HardForkBlock"

  -- TODO: perform actual validation against all
  -- possible 'PerasValidationErr' variants
  -- see https://github.com/tweag/cardano-peras/issues/120
  validatePerasCert params cert =
    Right
      ValidatedPerasCert
        { vpcCert = cert
        , vpcCertBoost = perasWeight params
        }

  -- TODO: perform actual validation against all
  -- possible 'PerasValidationErr' variants
  -- see https://github.com/tweag/cardano-peras/issues/120
  validatePerasVote _params stakeDistr vote
    | Just stake <- lookupPerasVoteStake vote stakeDistr =
        Right
          ValidatedPerasVote
            { vpvVote = vote
            , vpvVoteStake = stake
            }
    | otherwise =
        Left PerasValidationErr

  -- TODO: perform actual validation against all
  -- possible 'PerasForgeErr' variants
  -- see https://github.com/tweag/cardano-peras/issues/120
  forgePerasCert params target votes
    | not allVotersMatchTarget =
        Left PerasForgeErrTargetMismatch
    | not votesHaveEnoughStake =
        Left PerasForgeErrInsufficientVotes
    | otherwise =
        return $
          ValidatedPerasCert
            { vpcCert =
                mkPerasCert
                  (pvtRoundNo target)
                  (pvtBlock target)
            , vpcCertBoost = perasWeight params
            }
   where
    totalVotesStake =
      mconcat (vpvVoteStake <$> votes)

    votesHaveEnoughStake =
      stakeAboveThreshold params totalVotesStake

    allVotersMatchTarget =
      all ((target ==) . getPerasVoteTarget) votes

instance HasPerasCertRound (PerasCert (HardForkBlock xs))
instance HasPerasCertBoostedBlock (PerasCert (HardForkBlock xs)) (HardForkBlock xs)
instance HasPerasCertBoost (PerasCert (HardForkBlock xs))
instance HasPerasVoteRound (PerasVote (HardForkBlock xs))
instance HasPerasVoteBlock (PerasVote (HardForkBlock xs)) (HardForkBlock xs)
instance HasPerasVoteVoterId (PerasVote (HardForkBlock xs))
instance HasPerasVoteStake (PerasVote (HardForkBlock xs))
instance HasPerasVoteTarget (PerasVote (HardForkBlock xs)) (HardForkBlock xs)
instance HasPerasVoteId (PerasVote (HardForkBlock xs)) (HardForkBlock xs)
