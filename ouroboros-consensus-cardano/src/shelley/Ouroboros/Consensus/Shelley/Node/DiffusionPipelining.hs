{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Node.DiffusionPipelining () where

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Protocol ()

deriving via SelectViewDiffusionPipelining (ShelleyBlock proto era)
  instance ShelleyCompatible proto era
  => BlockSupportsDiffusionPipelining (ShelleyBlock proto era)
