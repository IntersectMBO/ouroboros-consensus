{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.SupportsPeras () where

import Data.Maybe.Strict (strictMaybeToMaybe)
import Ouroboros.Consensus.Ledger.SupportsPeras (LedgerSupportsPeras (..))
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import Ouroboros.Consensus.Shelley.Ledger.Ledger (LedgerState (..))

instance LedgerSupportsPeras (ShelleyBlock proto era) where
  getLatestPerasCertRound =
    strictMaybeToMaybe
      . shelleyLedgerLatestPerasCertRound
