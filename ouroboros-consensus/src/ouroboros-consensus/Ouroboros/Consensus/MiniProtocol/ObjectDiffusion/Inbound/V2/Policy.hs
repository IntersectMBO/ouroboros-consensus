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
  -- ^ maximal number of dpsOutstandingFifo.
  , dpMaxNumObjectsInflightPerPeer :: !NumObjectsReq
  -- ^ a limit of objects in-flight from a single peer, plus or minus 1.
  , dpMaxNumObjectsInflightTotal :: !NumObjectsReq
  -- ^ a limit of object size in-flight from all peers, plus or minus 1
  , dpMaxObjectInflightMultiplicity :: !ObjectMultiplicity
  -- ^ from how many peers download the `objectId` simultaneously
  }
  deriving Show

defaultDecisionPolicy :: DecisionPolicy
defaultDecisionPolicy =
  DecisionPolicy
    { dpMaxNumObjectIdsReq = 3
    , dpMaxNumObjectsOutstanding = 10 -- must be the same as objectDiffusionMaxUnacked
    , dpMaxNumObjectsInflightPerPeer = 6
    , dpMaxNumObjectsInflightTotal = 20
    , dpMaxObjectInflightMultiplicity = 2
    }
