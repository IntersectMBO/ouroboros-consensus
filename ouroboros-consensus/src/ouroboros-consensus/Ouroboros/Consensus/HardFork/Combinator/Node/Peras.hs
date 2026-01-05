{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Node.Peras
  (
  ) where

import Data.Proxy (Proxy (..))
import Data.SOP (K (..))
import Data.SOP.Strict (HCollapse (..), hcmap)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..), Point (..))
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , HasPerasCert (..)
  , HasPerasVote (..)
  , PerasCert
  , PerasForgeErr (..)
  , PerasValidationErr (..)
  , PerasVote
  , ValidatedPerasCert (..)
  , ValidatedPerasVote (..)
  , lookupPerasVoteStake
  )
import Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork (CanHardFork)
import Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
  ( SingleEraBlock
  , proxySingle
  )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
  ( OneEraHash (..)
  , OneEraPerasCert (..)
  , OneEraPerasVote (..)
  )
import Ouroboros.Consensus.HardFork.Combinator.Basics (HardForkBlock (..))
import Ouroboros.Consensus.HardFork.Combinator.Block ()
import Ouroboros.Consensus.Peras.Params (PerasParams (..))
import Ouroboros.Consensus.Peras.Vote
  ( PerasVoteId (..)
  , PerasVoteTarget (..)
  , stakeAboveThreshold
  )
import Ouroboros.Consensus.TypeFamilyWrappers
  ( WrapPerasCert (..)
  , WrapPerasVote (..)
  )

{-------------------------------------------------------------------------------
  Peras
-------------------------------------------------------------------------------}

newtype instance PerasCert (HardForkBlock xs) = HardForkPerasCert
  { getHardForkPerasCert :: OneEraPerasCert xs
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype NoThunks

newtype instance PerasVote (HardForkBlock xs) = HardForkPerasVote
  { getHardForkPerasVote :: OneEraPerasVote xs
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype NoThunks

instance CanHardFork xs => BlockSupportsPeras (HardForkBlock xs) where
  type PerasCfg (HardForkBlock xs) = PerasParams

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
            , vpcCertBoost =
                perasWeight params
            }
   where
    totalVotesStake =
      mconcat (vpvVoteStake <$> votes)

    votesHaveEnoughStake =
      stakeAboveThreshold params totalVotesStake

    allVotersMatchTarget =
      all ((target ==) . getPerasVoteTarget) votes

instance CanHardFork xs => HasPerasCert (PerasCert (HardForkBlock xs)) where
  type CertBoostedBlock (PerasCert (HardForkBlock xs)) = HardForkBlock xs

  getPerasCertRound (HardForkPerasCert (OneEraPerasCert hcert)) =
    hcollapse
      $ hcmap
        proxySingle
        ( K
            . getPerasCertRound
            . unwrapPerasCert
        )
      $ hcert

  getPerasCertBoostedBlock (HardForkPerasCert (OneEraPerasCert hcert)) =
    hcollapse
      $ hcmap
        proxySingle
        ( K
            . injectHardForkPoint
            . getPerasCertBoostedBlock
            . unwrapPerasCert
        )
      $ hcert

instance CanHardFork xs => HasPerasVote (PerasVote (HardForkBlock xs)) where
  type VoteBlock (PerasVote (HardForkBlock xs)) = HardForkBlock xs

  getPerasVoteRound (HardForkPerasVote (OneEraPerasVote hvote)) =
    hcollapse
      $ hcmap
        proxySingle
        ( K
            . getPerasVoteRound
            . unwrapPerasVote
        )
      $ hvote

  getPerasVoteVoterId (HardForkPerasVote (OneEraPerasVote hvote)) =
    hcollapse
      $ hcmap
        proxySingle
        ( K
            . getPerasVoteVoterId
            . unwrapPerasVote
        )
      $ hvote

  getPerasVoteTarget (HardForkPerasVote (OneEraPerasVote hvote)) =
    PerasVoteTarget
      { pvtRoundNo =
          hcollapse
            $ hcmap
              proxySingle
              ( K
                  . getPerasVoteRound
                  . unwrapPerasVote
              )
            $ hvote
      , pvtBlock =
          hcollapse
            $ hcmap
              proxySingle
              ( K
                  . injectHardForkPoint
                  . pvtBlock
                  . getPerasVoteTarget
                  . unwrapPerasVote
              )
            $ hvote
      }

  getPerasVoteId (HardForkPerasVote (OneEraPerasVote hvote)) =
    PerasVoteId
      { pviRoundNo =
          hcollapse
            $ hcmap
              proxySingle
              ( K
                  . getPerasVoteRound
                  . unwrapPerasVote
              )
            $ hvote
      , pviVoterId =
          hcollapse
            $ hcmap
              proxySingle
              ( K
                  . getPerasVoteVoterId
                  . unwrapPerasVote
              )
            $ hvote
      }

-- | Inject a 'Point' from a single era into a 'Point' of the hard fork block.
injectHardForkPoint ::
  forall blk xs.
  SingleEraBlock blk =>
  Point blk ->
  Point (HardForkBlock xs)
injectHardForkPoint = \case
  GenesisPoint ->
    GenesisPoint
  BlockPoint s h ->
    BlockPoint s (OneEraHash (toShortRawHash (Proxy @blk) h))
