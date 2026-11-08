module Test.Consensus.Shelley.MempoolTx (mkTestShelleyTx) where

import Cardano.Ledger.Core (TopTx, Tx)
import Cardano.Ledger.MempoolTx.Internal (MempoolTx (UnsafeMkMempoolTx))
import Ouroboros.Consensus.Ledger.SupportsMempool (GenTx)
import Ouroboros.Consensus.Shelley.Eras (ShelleyBasedEra)
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock, mkShelleyTx)

-- | A transaction the test built, submitted as a wallet would.
--
-- The ledger has no public way to turn a transaction built locally into a
-- 'MempoolTx'.
mkTestShelleyTx :: ShelleyBasedEra era => Tx TopTx era -> GenTx (ShelleyBlock proto era)
mkTestShelleyTx = mkShelleyTx . UnsafeMkMempoolTx
