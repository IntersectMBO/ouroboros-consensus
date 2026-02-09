{-# LANGUAGE DefaultSignatures #-}

module Ouroboros.Consensus.Ledger.SupportsPeras
  ( LedgerSupportsPeras (..)
  )
where

import Ouroboros.Consensus.Block.SupportsPeras (PerasRoundNo)
import Ouroboros.Consensus.Ledger.Abstract (LedgerState)

-- | Extract Peras information stored in the ledger state
class LedgerSupportsPeras blk where
  -- | Extract the round number of the latest Peras certificate stored in the
  -- given ledger state (if any). This is needed to coordinate the end of a
  -- cooldown period.
  getLatestPerasCertRound :: LedgerState blk mk -> Maybe PerasRoundNo
  default getLatestPerasCertRound :: LedgerState blk mk -> Maybe PerasRoundNo
  getLatestPerasCertRound _ = Nothing
