{-# LANGUAGE FlexibleContexts #-}

module Ouroboros.Consensus.Peras.Vote.Forging
  ( PerasVoteForging (..)
  , mkPerasVoteForging
  )
where

import Control.Monad.Class.MonadSTM (STM)
import Ouroboros.Consensus.Block.Abstract (Point)
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras
  , PerasCrypto
  , PerasRoundNo
  , ValidatedPerasVote
  )
import Ouroboros.Consensus.Committee.Crypto (PrivateKey)
import Ouroboros.Consensus.Committee.Types (PoolId)
import Ouroboros.Consensus.Peras.Context
  ( PerasEpochContextResolverHandle
  , forgePerasVoteIfEligibleWithHandle
  )
import Ouroboros.Consensus.Util.IOLike (MonadSTM, MonadThrow)

-- | Stateful wrapper around vote forging.
--
-- Provides a closure around the voter's pool identity and private key.
newtype PerasVoteForging m blk = PerasVoteForging
  { forgeVoteIfEligible ::
      PerasEpochContextResolverHandle m blk ->
      PerasRoundNo ->
      Point blk ->
      STM m (Maybe (ValidatedPerasVote blk))
  }

-- | Construct a 'PerasVoteForging' given a pool identity and private key.
mkPerasVoteForging ::
  ( MonadSTM m
  , MonadThrow (STM m)
  , BlockSupportsPeras blk
  ) =>
  PoolId ->
  PrivateKey (PerasCrypto blk) ->
  PerasVoteForging m blk
mkPerasVoteForging poolId privateKey =
  PerasVoteForging
    { forgeVoteIfEligible = \resolverHandle roundNo point ->
        forgePerasVoteIfEligibleWithHandle
          resolverHandle
          poolId
          privateKey
          roundNo
          point
    }
