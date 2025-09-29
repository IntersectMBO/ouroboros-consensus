{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Policy
  ( ObjectDecisionPolicy (..)
  , defaultObjectDecisionPolicy
  , max_OBJECT_SIZE

    -- * Re-exports
  , NumObjectIdsToReq (..)
  ) where

import Control.Monad.Class.MonadTime.SI
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API (SizeInBytes (..))
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (NumObjectIdsToReq (..))

-- | Maximal object size.
--
-- Affects:
--
-- * `ObjectDecisionPolicy`
-- * `maximumIngressQueue` for `object-submission` mini-protocol, see
--   `Ouroboros.Consensus.MiniProtocol.NodeToNode.objectDiffusionProtocolLimits`
max_OBJECT_SIZE :: SizeInBytes
max_OBJECT_SIZE = 65_540

-- | Policy for making decisions
data ObjectDecisionPolicy = ObjectDecisionPolicy
  { maxNumObjectIdsToRequest :: !NumObjectIdsToReq
  -- ^ a maximal number of objectIds requested at once.
  , maxUnacknowledgedObjectIds :: !NumObjectIdsToReq
  -- ^ maximal number of unacknowledgedObjectIds.  Measured in `NumObjectIdsToReq`
  -- since we enforce this policy by requesting not more objectIds than what
  -- this limit allows.
  , --
    -- Configuration of object decision logic.
    --

    objectsSizeInflightPerPeer :: !SizeInBytes
  -- ^ a limit of object size in-flight from a single peer.
  -- It can be exceed by max object size.
  , maxObjectsSizeInflight :: !SizeInBytes
  -- ^ a limit of object size in-flight from all peers.
  -- It can be exceed by max object size.
  , objectInflightMultiplicity :: !Int
  -- ^ from how many peers download the `objectId` simultaneously
  , bufferedObjectsMinLifetime :: !DiffTime
  -- ^ how long OBJECTs that have been added to the objectpool will be
  -- kept in the `bufferedObjects` cache.
  , scoreRate :: !Double
  -- ^ rate at which "rejected" OBJECTs drain. Unit: OBJECT/seconds.
  , scoreMax :: !Double
  -- ^ Maximum number of "rejections". Unit: seconds
  }
  deriving Show

defaultObjectDecisionPolicy :: ObjectDecisionPolicy
defaultObjectDecisionPolicy =
  ObjectDecisionPolicy
    { maxNumObjectIdsToRequest = 3
    , maxUnacknowledgedObjectIds = 10 -- must be the same as objectDiffusionMaxUnacked
    , objectsSizeInflightPerPeer = max_OBJECT_SIZE * 6
    , maxObjectsSizeInflight = max_OBJECT_SIZE * 20
    , objectInflightMultiplicity = 2
    , bufferedObjectsMinLifetime = 2
    , scoreRate = 0.1
    , scoreMax = 15 * 60
    }
