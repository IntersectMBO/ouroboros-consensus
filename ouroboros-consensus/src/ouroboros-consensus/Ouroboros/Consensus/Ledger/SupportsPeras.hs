{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Ledger.SupportsPeras
  ( ALedgerStateSupportsPeras (..)
  )
where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Coin (Coin (..), compactCoinOrError, knownNonZeroCoin)
import Cardano.Ledger.Keys (KeyHash (..), toVRFVerKeyHash)
import Cardano.Ledger.State (IndividualPoolStake (..), PoolDistr (..))
import qualified Data.Map as Map
import Ouroboros.Consensus.Block.SupportsPeras (PerasParams, defaultPerasParams)

-- | Extract Peras information stored in the ledger state
class ALedgerStateSupportsPeras ledger where
  -- | Extract the stake distribution from the given ledger state.
  -- PRECONDITION: this function will only return a meaningful result if the
  -- ledger state is from a block that supports Peras
  getPoolDistr :: ledger -> PoolDistr
  default getPoolDistr :: ledger -> PoolDistr
  -- NOTE: this is a bit of a hack for blocks that do not really support Peras.
  -- We return a single dummy stake pool holding all of the active stake, so
  -- that consumers relying on a non-empty stake distribution (e.g. the mock
  -- voting committee) do not fail.
  getPoolDistr _ =
    PoolDistr
      { unPoolDistr =
          Map.singleton
            dummyPoolId
            IndividualPoolStake
              { individualPoolStake = 1
              , individualTotalPoolStake = compactCoinOrError (Coin 1)
              , individualPoolStakeVrf = dummyPoolVrf
              }
      , pdTotalActiveStake = knownNonZeroCoin @1
      }
   where
    dummyPoolId = KeyHash (Hash.castHash (Hash.hashWith id "peras-mock-pool"))
    dummyPoolVrf = toVRFVerKeyHash (Hash.castHash (Hash.hashWith id "peras-mock-pool-vrf"))

  -- | TODO: when Peras params go on chain, update this
  getPerasParams :: proxy blk -> ledger -> PerasParams blk
  default getPerasParams :: proxy blk -> ledger -> PerasParams blk
  getPerasParams _ _ = defaultPerasParams
