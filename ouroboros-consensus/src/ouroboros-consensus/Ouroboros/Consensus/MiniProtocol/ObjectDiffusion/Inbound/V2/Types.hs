{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
  ( -- * DecisionPeerState
    DecisionPeerState (..)

    -- * DecisionGlobalState
  , DecisionGlobalState (..)

    -- * Decisions
  , PeerDecision (..)
  , mempty
  , TraceDecisionLogic (..)
  , ObjectMultiplicity (..)

    -- * Reporting
  , ObjectDiffusionCounters (..)
  , makeObjectDiffusionCounters

    -- * Init delay
  , ObjectDiffusionInitDelay (..)
  , defaultObjectDiffusionInitDelay

    -- * Copied from V1
  , NumObjectsProcessed (..)
  , TraceObjectDiffusionInbound (..)
  , ObjectDiffusionInboundError (..)
  ) where

import Control.Exception (Exception (..))
import Control.Monad.Class.MonadTime.SI
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Ouroboros.Network.Protocol.ObjectDiffusion.Type
import System.Random (StdGen)
import Data.Word (Word64)
import Ouroboros.Network.ControlMessage (ControlMessage)
import Control.DeepSeq (NFData)
import Quiet (Quiet (..))

--
-- DecisionPeerState, DecisionGlobalState
--


-- | In all the fields' names,
-- If "Ids" appears at the beginning of a name field, it means we refer to IDs
-- specifically (i.e. before the corresponding object is in flight).
-- On the other hand, a field name of the form "Objects...Ids" means we are
-- speaking of objects (i.e. after they have been requested) but identify them
-- by their IDs for this field purpose.
data DecisionPeerState objectId object = DecisionPeerState
  { dpsOutstandingFifo :: !(StrictSeq objectId)
  -- ^ Those objects (by their identifier) that the client has told
  -- us about, and which we have not yet acknowledged. This is kept in
  -- the order in which the client gave them to us. This is the same order
  -- in which we submit them to the objectpool. It is also the order
  -- in which we acknowledge them.
  , dpsIdsAvailable :: !(Set objectId)
  -- ^ Set of known object ids which can be requested from this peer.
  , dpsNumIdsInflight :: !NumObjectIdsReq
  -- ^ The number of object identifiers that we have requested but
  -- which have not yet been replied to. We need to track this it keep
  -- our requests within the limit on the number of unacknowledged objectIds.
  , dpsObjectsInflightIds :: !(Set objectId)
  -- ^ The set of requested objects (by their ids).
  -- , dpsObjectsRequestedButNotReceivedIds :: !(Set objectId)
  -- ^ A subset of `dpsOutstandingFifo` which were unknown to the peer
  -- (i.e. requested but not received). We need to track these `objectId`s
  -- since they need to be acknowledged.
  , dpsObjectsPending :: !(Map objectId object)
  -- ^ A set of objects downloaded from the peer. They are not yet
  -- acknowledged and haven't been sent to the objectpool yet.
  --
  -- Life cycle of entries:
  -- * added when a object is downloaded in `handleReceivedObjectsImpl`
  -- * removed by `acknowledgeObjectIds` (to properly follow `dpsOutstandingFifo`)
  , dpsObjectsOwtPool :: !(Map objectId object)
  -- ^ A set of objects on their way to the objectpool.
  -- Tracked here so that we can cleanup `dgsObjectsOwtPoolMultiplicities` if the
  -- peer dies.
  --
  -- Life cycle of entries:
  -- * added by `acknowledgeObjectIds` (where decide which objects can be
  --   submitted to the objectpool)
  -- * removed by `withObjectPoolSem`
  }
  deriving (Eq, Show, Generic)

instance
  ( NoThunks objectId
  , NoThunks object
  ) =>
  NoThunks (DecisionPeerState objectId object)

-- | Shared state of all `ObjectDiffusion` clients.
--
-- New `objectId` enters `dpsOutstandingFifo` it is also added to `dpsIdsAvailable`
-- and `dgsObjectsLiveMultiplicities` (see `acknowledgeObjectIdsImpl`).
--
-- When the requested object arrives, the corresponding entry is removed from `dgsObjectsInflightMultiplicities`.
--
-- Whenever we choose an `objectId` to acknowledge (either in `acknowledObjectsIds`,
-- `handleReceivedObjectsImpl` or
-- `pickObjectsToDownload`, we also
-- recalculate `dgsObjectsLiveMultiplicities` and only keep live `objectId`s in other maps (e.g.
-- `dpsIdsAvailable`).
data DecisionGlobalState peerAddr objectId object = DecisionGlobalState
  { dgsPeerStates :: !(Map peerAddr (DecisionPeerState objectId object))
  -- ^ Map of peer states.
  --
  -- /Invariant:/ for peerAddr's which are registered using `withPeer`,
  -- there's always an entry in this map even if the set of `objectId`s is
  -- empty.
  , dgsObjectsInflightMultiplicities :: !(Map objectId ObjectMultiplicity)
  -- ^ Map from object ids of objects which are in-flight (have already been
  -- requested) to their multiplicities (from how many peers it is
  -- currently in-flight)
  --
  -- This can intersect with `dpsIdsAvailable`.
  , dgsObjectsLiveMultiplicities :: !(Map objectId ObjectMultiplicity)
  -- ^ We track counts of live objects.
  -- An object is added to the live map when it is inflight, and is only removed
  -- after the retention timeout expires.
  --
  -- The value for any key must be always non-zero (strictly positive).
  --
  -- The `dgsObjectsOwtPoolMultiplicities` map contains a subset of `dgsObjectsLiveMultiplicities`.
  , dgsRententionTimeouts :: !(Map Time [objectId])
  -- ^ Objects are kept live for a bit longer after having been added to the objectpool.
  --
  -- We need these short timeouts to avoid re-downloading a `object`. We could
  -- acknowledge this `objectId` to all peers, when a peer from another
  -- continent presents us it again.
  --
  -- Every objectId entry has a reference count in `dgsObjectsLiveMultiplicities`.
  , dgsObjectsOwtPoolMultiplicities :: !(Map objectId ObjectMultiplicity)
  -- ^ A set of objectIds that have been downloaded by a peer and are on their
  -- way to the objectpool. We won't issue further fetch-requests for objects in
  -- this state.  We track these objects to not re-download them from another
  -- peer.
  --
  -- * We subtract from the counter when a given object is added or rejected by
  --   the objectpool or do that for all objects in `dpsObjectsOwtPool` when a peer is
  --   unregistered.
  -- * We add to the counter when a given object is selected to be added to the
  --   objectpool in `pickObjectsToDownload`.
  , dgsRng :: !StdGen
  -- ^ Rng used to randomly order peers
  }
  deriving (Eq, Show, Generic)

instance
  ( NoThunks peerAddr
  , NoThunks object
  , NoThunks objectId
  , NoThunks StdGen
  ) =>
  NoThunks (DecisionGlobalState peerAddr objectId object)

-- | Merge dpsIdsAvailable from all peers of the global state.
dgsIdsAvailable :: Ord objectId => DecisionGlobalState peerAddr objectId object -> Set objectId
dgsIdsAvailable DecisionGlobalState{dgsPeerStates} =
  Set.unions (dpsIdsAvailable <$> (Map.elems dgsPeerStates))

-- | Merge dpsObjectsPending from all peers of the global state.
dgsObjectsPending :: Ord objectId => DecisionGlobalState peerAddr objectId object -> Map objectId object
dgsObjectsPending DecisionGlobalState{dgsPeerStates} =
  Map.unions (dpsObjectsPending <$> (Map.elems dgsPeerStates))

--
-- Decisions
--

-- | Decision made by the decision logic.  Each peer will receive a 'Decision'.
--
-- /note:/ it is rather non-standard to represent a choice between requesting
-- `objectId`s and `object`'s as a product rather than a sum type.  The client will
-- need to download `object`s first and then send a request for more objectIds (and
-- acknowledge some `objectId`s).   Due to pipelining each client will request
-- decision from the decision logic quite often (every two pipelined requests),
-- but with this design a decision once taken will make the peer non-active
-- (e.g. it won't be returned by `filterActivePeers`) for longer, and thus the
-- expensive `makeDecision` computation will not need to take that peer into
-- account.
data PeerDecision objectId object = PeerDecision
  { pdIdsToAck :: !NumObjectIdsAck
  -- ^ objectId's to acknowledge
  , pdIdsToReq :: !NumObjectIdsReq
  -- ^ number of objectId's to request
  , pdCanPipelineIdsReq :: !Bool
  -- ^ the object-submission protocol only allows to pipeline `objectId`'s requests
  -- if we have non-acknowledged `objectId`s.
  , pdObjectsToReqIds :: !(Set objectId)
  -- ^ objectId's to download.
  , pdObjectsToPool :: !(Set objectId)
  -- ^ list of `object`s to submit to the objectpool.
  }
  deriving (Show, Eq)

-- | A non-commutative semigroup instance.
--
-- /note:/ this instance must be consistent with `pickObjectsToDownload` and how
-- `DecisionPeerState` is updated.  It is designed to work with `TMergeVar`s.
instance Ord objectId => Semigroup (PeerDecision objectId object) where
  PeerDecision
    { pdIdsToAck
    , pdIdsToReq
    , pdCanPipelineIdsReq = _ignored
    , pdObjectsToReqIds
    , pdObjectsToPool
    }
    <> PeerDecision
      { pdIdsToAck = pdIdsToAck'
      , pdIdsToReq = pdIdsToReq'
      , pdCanPipelineIdsReq = pdCanPipelineIdsReq'
      , pdObjectsToReqIds = pdObjectsToReqIds'
      , pdObjectsToPool = pdObjectsToPool'
      } =
      PeerDecision
        { pdIdsToAck = pdIdsToAck + pdIdsToAck'
        , pdIdsToReq = pdIdsToReq + pdIdsToReq'
        , pdCanPipelineIdsReq = pdCanPipelineIdsReq'
        , pdObjectsToReqIds = pdObjectsToReqIds <> pdObjectsToReqIds'
        , pdObjectsToPool = pdObjectsToPool <> pdObjectsToPool'
        }
instance Ord objectId => Monoid (PeerDecision objectId object) where
  mempty = PeerDecision
    { pdIdsToAck = 0
    , pdIdsToReq = 0
    , pdCanPipelineIdsReq = False
    , pdObjectsToReqIds = Set.empty
    , pdObjectsToPool = Set.empty
    }

-- | ObjectLogic tracer.
data TraceDecisionLogic peerAddr objectId object
  = TraceDecisionLogicGlobalStateUpdated String (DecisionGlobalState peerAddr objectId object)
  | TraceDecisionLogicDecisionsMade (Map peerAddr (PeerDecision objectId object))
  deriving Show

data ObjectDiffusionCounters
  = ObjectDiffusionCounters
  { odcDistinctNumObjectsAvailable :: Int
  -- ^ objectIds which are not yet downloaded.
  , odcNumDistinctObjectsLive :: Int
  -- ^ number of distinct live objects
  , odcNumDistinctObjectsInflight :: Int
  -- ^ number of distinct in-flight objects.
  , odcNumTotalObjectsInflight :: Int
  -- ^ number of all in-flight objects.
  , odcNumDistinctObjectsPending :: Int
  -- ^ number of distinct pending objects (downloaded but not acked)
  , odcNumDistinctObjectsOwtPool :: Int
  -- ^ number of distinct objects which are waiting to be added to the
  -- objectpool (each peer need to acquire the semaphore to effectively add
  -- them to the pool)
  }
  deriving (Eq, Show)

makeObjectDiffusionCounters ::
  Ord objectId =>
  DecisionGlobalState peerAddr objectId object ->
  ObjectDiffusionCounters
makeObjectDiffusionCounters
  dgs@DecisionGlobalState
    { dgsObjectsInflightMultiplicities
    , dgsObjectsLiveMultiplicities
    , dgsObjectsOwtPoolMultiplicities
    } =
    ObjectDiffusionCounters
      { odcDistinctNumObjectsAvailable = Set.size $ dgsIdsAvailable dgs
      , odcNumDistinctObjectsLive = Map.size dgsObjectsLiveMultiplicities
      , odcNumDistinctObjectsInflight = Map.size dgsObjectsInflightMultiplicities
      , odcNumTotalObjectsInflight = fromIntegral $ mconcat (Map.elems dgsObjectsInflightMultiplicities)
      , odcNumDistinctObjectsPending = Map.size $ dgsObjectsPending dgs
      , odcNumDistinctObjectsOwtPool = Map.size dgsObjectsOwtPoolMultiplicities
      }

data ObjectDiffusionInitDelay
  = ObjectDiffusionInitDelay DiffTime
  | NoObjectDiffusionInitDelay
  deriving (Eq, Show)

defaultObjectDiffusionInitDelay :: ObjectDiffusionInitDelay
defaultObjectDiffusionInitDelay = ObjectDiffusionInitDelay 60

-- Copied from V1:

newtype NumObjectsProcessed
  = NumObjectsProcessed
  { getNumObjectsProcessed :: Word64
  }
  deriving (Eq, Ord, NFData, Generic)
  deriving newtype (Num, Enum, Real, Integral, Bounded, NoThunks)
  deriving (Semigroup) via (Sum Word64)
  deriving (Monoid)    via (Sum Word64)
  deriving (Show)      via (Quiet NumObjectsProcessed)

newtype ObjectMultiplicity
  = ObjectMultiplicity
  { getObjectMultiplicity :: Word64
  }
  deriving (Eq, Ord, NFData, Generic)
  deriving newtype (Num, Enum, Real, Integral, Bounded, NoThunks)
  deriving (Semigroup) via (Sum Word64)
  deriving (Monoid)    via (Sum Word64)
  deriving (Show)      via (Quiet ObjectMultiplicity)

data TraceObjectDiffusionInbound objectId object
  = -- | Number of objects just about to be inserted.
    TraceObjectDiffusionInboundCollectedObjects Int
  | -- | Just processed object pass/fail breakdown.
    TraceObjectDiffusionInboundAddedObjects Int
  | -- | Received a 'ControlMessage' from the outbound peer governor, and about
    -- to act on it.
    TraceObjectDiffusionInboundRecvControlMessage ControlMessage
  | TraceObjectDiffusionInboundCanRequestMoreObjects Int
  | TraceObjectDiffusionInboundCannotRequestMoreObjects Int
  | TraceObjectDiffusionInboundDecisionReceived (PeerDecision objectId object)
  deriving (Eq, Show)

data ObjectDiffusionInboundError
  = ProtocolErrorObjectNotRequested
  | ProtocolErrorObjectIdsNotRequested
  | ProtocolErrorObjectIdAlreadyKnown
  | ProtocolErrorObjectIdsDuplicate
  deriving Show

instance Exception ObjectDiffusionInboundError where
  displayException ProtocolErrorObjectNotRequested =
    "The peer replied with a object we did not ask for."
  displayException ProtocolErrorObjectIdsNotRequested =
    "The peer replied with more objectIds than we asked for."
  displayException ProtocolErrorObjectIdAlreadyKnown =
    "The peer replied with an objectId that it has already sent us previously."
  displayException ProtocolErrorObjectIdsDuplicate =
    "The peer replied with a batch of objectIds containing a duplicate."