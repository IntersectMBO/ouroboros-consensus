{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | See 'ApplyEndorserBlock'.
module Ouroboros.Consensus.Ledger.SupportsLeios
  ( ApplyEndorserBlock (..)
  ) where

import Ouroboros.Consensus.Block.SupportsLeios (BlockSupportsLeios (..))
import Ouroboros.Consensus.Config (configLedger)
import Ouroboros.Consensus.Ledger.Abstract
  ( ApplyBlock
  , DiffMK
  , LedgerCfg
  , LedgerState
  , ValuesMK
  )
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerCfg (..), ExtLedgerState (..))
import Ouroboros.Consensus.Leios.Types (LeiosPoint)

-- | Apply endorser blocks to a ledger, as 'ApplyBlock' applies blocks.
class (ApplyBlock l blk, BlockSupportsLeios blk) => ApplyEndorserBlock l blk where
  -- | The endorser block the tip of the state announced, if any.
  announcedEndorserBlock :: l blk mk -> Maybe LeiosPoint

  -- | Apply an endorser block to the state whose tip announced it, without
  -- validating its transactions: the certificate of the next block vouches
  -- for them.
  applyEndorserBlock :: LedgerCfg l blk -> LedgerEndorserBlock blk -> l blk ValuesMK -> l blk DiffMK

instance (ApplyBlock LedgerState blk, BlockSupportsLeios blk) => ApplyEndorserBlock LedgerState blk where
  announcedEndorserBlock = ledgerAnnouncedEb
  applyEndorserBlock = applyLedgerEndorserBlock

instance
  (ApplyBlock ExtLedgerState blk, BlockSupportsLeios blk) =>
  ApplyEndorserBlock ExtLedgerState blk
  where
  announcedEndorserBlock = ledgerAnnouncedEb . ledgerState
  applyEndorserBlock cfg eb st =
    st{ledgerState = applyLedgerEndorserBlock (configLedger (getExtLedgerCfg cfg)) eb (ledgerState st)}
