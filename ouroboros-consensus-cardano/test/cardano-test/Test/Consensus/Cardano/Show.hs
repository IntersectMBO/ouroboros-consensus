module Test.Consensus.Cardano.Show () where

import           Ouroboros.Consensus.Cardano ()
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()

-- | This definition exists solely to ensure that 'CardanoLedgerConfig' has a working 'Show' instance that isn't accidentally removed or broken in future
_showCardanoLedgerConfig :: CardanoLedgerConfig StandardCrypto -> String
_showCardanoLedgerConfig = show
