{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Ledger.Inspect (
    ShelleyLedgerUpdate (..)
  , pparamsUpdate
  ) where

import           Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.Core as Core
import qualified Cardano.Ledger.Shelley.Governance as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import           Control.Monad
import           Data.Void
import           Lens.Micro ((^.))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Ledger
import           Ouroboros.Consensus.Util.Condense

data ShelleyLedgerUpdate era =
  ShelleyUpdatedPParams
    !(StrictMaybe (Core.PParams era))
    !EpochNo

deriving instance Eq (Core.PParams era) => Eq (ShelleyLedgerUpdate era)
deriving instance Show (Core.PParams era) => Show (ShelleyLedgerUpdate era)

instance Show (Core.PParams era) => Condense (ShelleyLedgerUpdate era) where
  condense = show

instance ShelleyBasedEra era => InspectLedger (ShelleyBlock proto era) where
  type LedgerWarning (ShelleyBlock proto era) = Void
  type LedgerUpdate  (ShelleyBlock proto era) = ShelleyLedgerUpdate era

  inspectLedger _tlc before after = do
      guard $ updatesBefore /= updatesAfter
      return $ LedgerUpdate updatesAfter
    where

      updatesBefore, updatesAfter :: ShelleyLedgerUpdate era
      updatesBefore = pparamsUpdate before
      updatesAfter  = pparamsUpdate after

pparamsUpdate ::
       forall era proto mk. ShelleyBasedEra era
    => LedgerState (ShelleyBlock proto era) mk
    -> ShelleyLedgerUpdate era
pparamsUpdate st =
    let nes = shelleyLedgerState st
     in ShelleyUpdatedPParams
          (SL.nextEpochUpdatedPParams (nes ^. SL.newEpochStateGovStateL))
          (succ (SL.nesEL nes))
