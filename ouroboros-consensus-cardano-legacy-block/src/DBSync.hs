{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module DBSync (
    initLedger
  , reapplyBlock
  ) where

import           Legacy.Cardano ()
import           Legacy.Cardano.Block
import           Legacy.Cardano.CanHardFork
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Node.ProtocolInfo

initLedger ::
     ProtocolInfo (LegacyCardanoBlock StandardCrypto)
  -> ExtLedgerState (LegacyCardanoBlock StandardCrypto) EmptyMK
initLedger = stowLedgerTables . pInfoInitLedger

reapplyBlock ::
     LegacyCardanoHardForkConstraints StandardCrypto
  => ExtLedgerCfg (LegacyCardanoBlock StandardCrypto)
  -> LegacyCardanoBlock StandardCrypto
  -> ExtLedgerState (LegacyCardanoBlock StandardCrypto) EmptyMK
  -> LedgerResult
       (ExtLedgerState (LegacyCardanoBlock StandardCrypto))
       (ExtLedgerState (LegacyCardanoBlock StandardCrypto) EmptyMK)
reapplyBlock cfg block lsb =
    fmap (stowLedgerTables . applyDiffs tables) res
  where
    unstowedLedger = unstowLedgerTables lsb
    tables = projectLedgerTables unstowedLedger
    res = tickThenReapplyLedgerResult cfg block unstowedLedger
