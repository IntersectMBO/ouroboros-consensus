{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cardano-specific Peras vote-forging helpers.
module Ouroboros.Consensus.Cardano.Peras
  ( mkCardanoPerasVoteForging
  )
where

import Data.SOP.Strict (NP (..))
import Ouroboros.Consensus.Cardano.Block
  ( CardanoBlock
  , CardanoEras
  )
import Ouroboros.Consensus.Cardano.CanHardFork (CardanoHardForkConstraints)
import Ouroboros.Consensus.Committee.Crypto (PrivateKey)
import Ouroboros.Consensus.Committee.Types (PoolId)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
  ( OneEraPerasCrypto
  , PerEraPerasPrivateKey (..)
  )
import Ouroboros.Consensus.Peras.Context (forgePerasVoteIfEligibleWithHandle)
import qualified Ouroboros.Consensus.Peras.Crypto.BLS as BLS
import Ouroboros.Consensus.Peras.Vote.Forging (PerasVoteForging (..))
import Ouroboros.Consensus.TypeFamilyWrappers (WrapPerasPrivateKey (..))
import Ouroboros.Consensus.Util.IOLike (MonadThrow)
import Ouroboros.Consensus.Util.MonadSTM.NormalForm (MonadSTM, STM)

-- | Construct Cardano vote forging from only a Dijkstra BLS private key.
--
-- The resolved round's committee chooses the applicable era. The supplied key
-- is used only when that context is tagged as Dijkstra; other era contexts
-- produce 'MissingPerasPrivateKeyForEra'.
mkCardanoPerasVoteForging ::
  forall m c.
  ( MonadSTM m
  , MonadThrow (STM m)
  , CardanoHardForkConstraints c
  ) =>
  PoolId ->
  BLS.PerasPrivateKey ->
  PerasVoteForging m (CardanoBlock c)
mkCardanoPerasVoteForging poolId blsPrivateKey =
  PerasVoteForging
    { forgeVoteIfEligible = \resolverHandle roundNo point -> do
        forgePerasVoteIfEligibleWithHandle
          resolverHandle
          poolId
          cardanoPrivateKey
          roundNo
          point
    }
 where
  cardanoPrivateKey :: PrivateKey (OneEraPerasCrypto (CardanoEras c))
  cardanoPrivateKey =
    PerEraPerasPrivateKey
      ( WrapPerasPrivateKey ()
          :* WrapPerasPrivateKey ()
          :* WrapPerasPrivateKey ()
          :* WrapPerasPrivateKey ()
          :* WrapPerasPrivateKey ()
          :* WrapPerasPrivateKey ()
          :* WrapPerasPrivateKey ()
          :* WrapPerasPrivateKey blsPrivateKey
          :* Nil
      )
