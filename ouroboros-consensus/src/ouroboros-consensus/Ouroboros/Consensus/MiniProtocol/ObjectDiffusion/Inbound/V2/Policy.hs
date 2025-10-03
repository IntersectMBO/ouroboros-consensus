{-# LANGUAGE ImportQualifiedPost #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Policy
  ( DecisionPolicy (..)
  , defaultDecisionPolicy

    -- * Re-exports
  , NumObjectIdsReq (..)
  ) where

import Control.Monad.Class.MonadTime.SI
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (NumObjectIdsReq (..), NumObjectsOutstanding, NumObjectsReq (..))

-- | Policy for making decisions
data DecisionPolicy = DecisionPolicy
  { dpMaxNumObjectIdsReq :: !NumObjectIdsReq
  -- ^ a maximal number of objectIds requested at once.
  , dpMaxNumObjectsOutstanding :: !NumObjectsOutstanding
  -- ^ maximal number of dpsOutstandingFifo.
  ,  dpMaxNumObjectsInflightPerPeer :: !NumObjectsReq
  -- ^ a limit of objects in-flight from a single peer, plus or minus 1.
  , dpMaxNumObjectsInflightTotal :: !NumObjectsReq
  -- ^ a limit of object size in-flight from all peers, plus or minus 1
  , dpMaxObjectInflightMultiplicity :: !Int
  -- ^ from how many peers download the `objectId` simultaneously
  , dpMinObtainedButNotAckedObjectsLifetime :: !DiffTime
  -- ^ how long objects that have been added to the objectpool will be
  -- kept in the `dgsObjectsPending` cache.
  , dpScoreDrainRate :: !Double
  -- ^ rate at which "rejected" objects drain. Unit: object/seconds.
  -- TODO: still relevant?
  , dpScoreMaxRejections :: !Double
  -- ^ Maximum number of "rejections". Unit: seconds
  -- TODO: still relevant?
  }
  deriving Show

defaultDecisionPolicy :: DecisionPolicy
defaultDecisionPolicy =
  DecisionPolicy
    { dpMaxNumObjectIdsReq = 3
    , dpMaxNumObjectsOutstanding = 10 -- must be the same as objectDiffusionMaxUnacked
    , dpMaxNumObjectsInflightPerPeer = NumObjectsReq 6
    , dpMaxNumObjectsInflightTotal = NumObjectsReq 20
    , dpMaxObjectInflightMultiplicity = 2
    , dpMinObtainedButNotAckedObjectsLifetime = 2
    , dpScoreDrainRate = 0.1
    , dpScoreMaxRejections = 15 * 60
    }
