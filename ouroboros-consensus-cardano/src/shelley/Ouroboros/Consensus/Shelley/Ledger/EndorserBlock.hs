{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Shelley.Ledger.EndorserBlock (EndorserBlock (..), mkEndorserBlock) where

import qualified Ouroboros.Consensus.Block as Core
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Core
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)

data EndorserBlock proto era = EndorserBlock
  { endorserBlockTxs :: [Core.Validated (Core.GenTx (ShelleyBlock proto era))] -- TODO(bladyjoker): This should be endorserBlockTxRefs and be GenTxHashes
  }

mkEndorserBlock ::
  [Core.Validated (Core.GenTx (ShelleyBlock proto era))] -> EndorserBlock proto era
mkEndorserBlock = EndorserBlock

type instance Core.EndorserBlock (ShelleyBlock proto era) = EndorserBlock proto era
