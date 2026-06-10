{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Ledger.SupportsPeras
  ( ALedgerStateSupportsPeras (..)
  )
where

import Cardano.Ledger.Coin (knownNonZeroCoin)
import Cardano.Ledger.State (PoolDistr (..))
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
  getPoolDistr _ =
    PoolDistr
      { unPoolDistr = Map.empty
      , pdTotalActiveStake = knownNonZeroCoin @1
      }

  -- | TODO: when Peras params go on chain, update this
  getPerasParams :: proxy blk -> ledger -> PerasParams blk
  default getPerasParams :: proxy blk -> ledger -> PerasParams blk
  getPerasParams _ _ = defaultPerasParams
