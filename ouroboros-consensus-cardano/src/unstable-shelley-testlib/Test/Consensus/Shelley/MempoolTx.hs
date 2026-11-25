module Test.Consensus.Shelley.MempoolTx (mkTestMempoolTx, mkTestShelleyTx) where

import Cardano.Ledger.Core (TopTx, Tx)
import Cardano.Ledger.MempoolTx.Internal (MempoolTx (UnsafeMkMempoolTx))
import Ouroboros.Consensus.Ledger.SupportsMempool (GenTx)
import Ouroboros.Consensus.Shelley.Eras (ShelleyBasedEra)
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock, mkShelleyTx)

-- | Wrap a transaction built by a test without running submission decoding.
--
-- The ledger has no public way to turn a transaction built locally into a
-- 'MempoolTx'.
-- This preserves the supplied validity flag, including flags that Dijkstra's
-- mempool decoder would reject.
mkTestMempoolTx :: Tx TopTx era -> MempoolTx era
mkTestMempoolTx = UnsafeMkMempoolTx

-- | Wrap a transaction built by a test for the consensus mempool.
mkTestShelleyTx :: ShelleyBasedEra era => Tx TopTx era -> GenTx (ShelleyBlock proto era)
mkTestShelleyTx = mkShelleyTx . mkTestMempoolTx
