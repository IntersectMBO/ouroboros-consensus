{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Ouroboros.Consensus.Legacy.Util (
    castExtLedgerCfg
  , castExtLedgerState
  ) where

import           Data.Coerce (Coercible)
import           Ouroboros.Consensus.Block.Abstract (BlockProtocol)
import           Ouroboros.Consensus.Config (TopLevelConfig)
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip (..),
                     castHeaderState)
import           Ouroboros.Consensus.Ledger.Basics (LedgerState)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerCfg (..),
                     ExtLedgerState (..))
import           Ouroboros.Consensus.Protocol.Abstract (ConsensusProtocol (..))

castExtLedgerState ::
     ( Coercible (ChainDepState (BlockProtocol blk ))
                 (ChainDepState (BlockProtocol blk'))
     , TipInfo blk ~ TipInfo blk'
     )
  => (LedgerState blk mk -> LedgerState blk' mk)
  -> ExtLedgerState blk mk
  -> ExtLedgerState blk' mk
castExtLedgerState f (ExtLedgerState st chaindep) =
  ExtLedgerState (f st) (castHeaderState chaindep)

castExtLedgerCfg ::
     (TopLevelConfig blk -> TopLevelConfig blk')
  -> ExtLedgerCfg blk
  -> ExtLedgerCfg blk'
castExtLedgerCfg f (ExtLedgerCfg t) = ExtLedgerCfg $ f t
