{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Ouroboros.Consensus.Ledger.SupportsPeras
  ( LedgerSupportsPeras (..)
  )
where

import Ouroboros.Consensus.Block.SupportsPeras (PerasRoundNo)
import Ouroboros.Consensus.Ledger.Abstract (LedgerState)
import Cardano.Ledger.State (PoolDistr (..))
import Cardano.Ledger.Coin (knownNonZeroCoin)
import qualified Data.Map as Map

-- | Extract Peras information stored in the ledger state
class LedgerSupportsPeras blk where
  -- | Extract the round number of the latest Peras certificate stored in the
  -- given ledger state (if any). This is needed to coordinate the end of a
  -- cooldown period.
  getLatestPerasCertRound :: LedgerState blk mk -> Maybe PerasRoundNo
  default getLatestPerasCertRound :: LedgerState blk mk -> Maybe PerasRoundNo
  getLatestPerasCertRound _ = Nothing

  -- | Extract the stake distribution from the given ledger state.
  -- PRECONDITION: this function will only return a meaningful result if the
  -- ledger state is from a block that supports Peras
  getStakeDistr :: LedgerState blk mk -> PoolDistr
  default getStakeDistr :: LedgerState blk mk -> PoolDistr
    -- NOTE: this is a bit of a hack for blocks that do not really support Peras.
  getStakeDistr _ = PoolDistr {
    unPoolDistr = Map.empty,
    pdTotalActiveStake = knownNonZeroCoin @1
  }
