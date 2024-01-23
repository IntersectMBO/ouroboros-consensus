-- | Consolidated protocol versions for cardano eras.
module Ouroboros.Consensus.Cardano.Node.ProtocolVersions (
    -- * Cardano protocol versions
    CardanoProtocolVersions (CardanoProtocolVersions, protVerByron, protVerShelley, protVerAllegra, protVerMary, protVerAlonzo, protVerBabbage, protVerConway)
  , cardanoMainnetProtocolVersions
  , cardanoMaxProtocolVersion
    -- * HasProtocolVersion
  , HasProtocolVersion (..)
  , ProtocolVersion
  ) where

import           Ouroboros.Consensus.Cardano.Node.ProtocolVersions.Internal
