{-# LANGUAGE ImportQualifiedPost #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Policy
  ( PeerDecisionPolicy (..)
  , defaultPeerDecisionPolicy

    -- * Re-exports
  , NumObjectIdsReq (..)
  ) where

import Control.Monad.Class.MonadTime.SI
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (NumObjectIdsReq (..))

newtype NumObjects = NumObjects Int
  deriving Show

-- | Policy for making decisions
data PeerDecisionPolicy = PeerDecisionPolicy
  { maxNumObjectIdsRequest :: !NumObjectIdsReq
  -- ^ a maximal number of objectIds requested at once.
  , maxUnacknowledgedObjectIds :: !NumObjectIdsReq
  -- ^ maximal number of outstandingFifo.  Measured in `NumObjectIdsReq`
  -- since we enforce this policy by requesting not more objectIds than what
  -- this limit allows.
  , --
    -- Configuration of object decision logic.
    --

    objectsSizeInflightPerPeer :: !NumObjects
  -- ^ a limit of objects in-flight from a single peer, plus or minus 1.
  , maxObjectsSizeInflight :: !NumObjects
  -- ^ a limit of object size in-flight from all peers, plus or minus 1
  , objectInflightMultiplicity :: !Int
  -- ^ from how many peers download the `objectId` simultaneously
  , globalObtainedButNotAckedObjectsMinLifetime :: !DiffTime
  -- ^ how long OBJECTs that have been added to the objectpool will be
  -- kept in the `globalObtainedButNotAckedObjects` cache.
  , scoreRate :: !Double
  -- ^ rate at which "rejected" OBJECTs drain. Unit: OBJECT/seconds.
  -- TODO: still relevant?
  , scoreMax :: !Double
  -- ^ Maximum number of "rejections". Unit: seconds
  -- TODO: still relevant?
  }
  deriving Show

defaultPeerDecisionPolicy :: PeerDecisionPolicy
defaultPeerDecisionPolicy =
  PeerDecisionPolicy
    { maxNumObjectIdsRequest = 3
    , maxUnacknowledgedObjectIds = 10 -- must be the same as objectDiffusionMaxUnacked
    , objectsSizeInflightPerPeer = NumObjects 6
    , maxObjectsSizeInflight = NumObjects 20
    , objectInflightMultiplicity = 2
    , globalObtainedButNotAckedObjectsMinLifetime = 2
    , scoreRate = 0.1
    , scoreMax = 15 * 60
    }
