module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Policy
  ( DecisionPolicy (..)
  , defaultDecisionPolicy
  ) where

import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types (ObjectMultiplicity)
import Ouroboros.Network.Protocol.ObjectDiffusion.Type
  ( NumObjectIdsReq (..)
  , NumObjectsOutstanding
  , NumObjectsReq (..)
  )

-- | Policy for making decisions
data DecisionPolicy = DecisionPolicy
  { dpMaxNumObjectIdsReq :: !NumObjectIdsReq
  -- ^ a maximal number of objectIds requested at once.
  , dpMaxNumObjectsOutstanding :: !NumObjectsOutstanding
  -- ^ maximal number of objects in the outstanding FIFO.
  , dpMaxNumObjectsInflightPerPeer :: !NumObjectsReq
  -- ^ a limit of objects in-flight from a single peer.
  , dpMaxNumObjectsInflightTotal :: !NumObjectsReq
  -- ^ a limit of objects in-flight from all peers for this node.
  , dpMaxObjectInflightMultiplicity :: !ObjectMultiplicity
  -- ^ from how many peers download the `objectId` simultaneously
  }
  deriving Show

defaultDecisionPolicy :: DecisionPolicy
defaultDecisionPolicy =
  DecisionPolicy
    { dpMaxNumObjectIdsReq = 3
    , dpMaxNumObjectsOutstanding = 10 -- must be the same as the outbound peer's value
    , dpMaxNumObjectsInflightPerPeer = 6
    , dpMaxNumObjectsInflightTotal = 20
    , dpMaxObjectInflightMultiplicity = 2
    }
