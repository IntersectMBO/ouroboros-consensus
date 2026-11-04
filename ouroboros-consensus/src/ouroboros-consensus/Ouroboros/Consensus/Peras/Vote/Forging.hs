{-# LANGUAGE FlexibleContexts #-}

module Ouroboros.Consensus.Peras.Vote.Forging
  ( PerasVoteForging (..)
  )
where

import Control.Monad.Class.MonadSTM (STM)
import Ouroboros.Consensus.Block.Abstract (Point)
import Ouroboros.Consensus.Block.SupportsPeras (PerasRoundNo, ValidatedPerasVote)
import Ouroboros.Consensus.Peras.Context
  ( PerasEpochContextResolverHandle
  )

-- | Stateful wrapper around vote forging.
--
-- Provides a closure around the voter's pool identity and private key.
newtype PerasVoteForging m blk
  = PerasVoteForging
  { forgeVoteIfEligible ::
      PerasEpochContextResolverHandle m blk ->
      PerasRoundNo ->
      Point blk ->
      STM m (Maybe (ValidatedPerasVote blk))
  }
