{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
  ( -- * DecisionPeerState
    DecisionPeerState (..)

    -- * DecisionGlobalState
  , DecisionGlobalState (..)
  , dgsObjectsAvailableMultiplicities
  , dgsObjectsInflightMultiplicities
  , dgsObjectsOwtPoolMultiplicities
  , DecisionGlobalStateVar
  , newDecisionGlobalStateVar

    -- * Decisions
  , PeerDecision (..)
  , PeerDecisionStatus (..)
  , unavailableDecision

    -- * Tracing
  , mempty
  , TraceDecisionLogic (..)
  , ObjectMultiplicity (..)

    -- * Reporting
  , ObjectDiffusionCounters (..)
  , makeObjectDiffusionCounters

    -- * Copied from V1
  , NumObjectsProcessed (..)
  , TraceObjectDiffusionInbound (..)
  , ObjectDiffusionInboundError (..)

    -- * Object pool semaphore
  , ObjectPoolSem (..)
  , newObjectPoolSem
  ) where

import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM, StrictTVar, atomically, newTVarIO)
import Control.Concurrent.Class.MonadSTM.TSem (TSem, newTSem)
import Control.DeepSeq (NFData (..))
import Control.Exception (Exception (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Data.Sequence.Strict (StrictSeq, fromList)
import Data.Set (Set)
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import NoThunks.Class (NoThunks (..))
import Ouroboros.Network.ControlMessage (ControlMessage)
import Ouroboros.Network.Protocol.ObjectDiffusion.Type
import Quiet (Quiet (..))
import Test.QuickCheck (Arbitrary (..), elements)

-- | Semaphore to guard access to the ObjectPool
newtype ObjectPoolSem m = ObjectPoolSem (TSem m)

newObjectPoolSem :: MonadSTM m => m (ObjectPoolSem m)
newObjectPoolSem = ObjectPoolSem <$> atomically (newTSem 1)

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
  { dpsNumIdsInflight :: !NumObjectIdsReq
  -- ^ The number of object identifiers that we have requested but
  -- which have not yet been replied to. We need to track this it keep
  -- our requests within the limit on the number of unacknowledged objectIds.
  , dpsOutstandingFifo :: !(StrictSeq objectId)
  -- ^ Those objects (by their identifier) that the client has told
  -- us about, and which we have not yet acknowledged. This is kept in
  -- the order in which the client gave them to us. This is the same order
  -- in which we submit them to the objectpool. It is also the order
  -- in which we acknowledge them.
  , dpsObjectsAvailableIds :: !(Set objectId)
  -- ^ Set of known object ids which can be requested from this peer.
  , dpsObjectsInflightIds :: !(Set objectId)
  -- ^ The set of requested objects (by their ids).
  -- , dpsObjectsRequestedButNotReceivedIds :: !(Set objectId)
  -- ^ A subset of `dpsOutstandingFifo` which were unknown to the peer
  -- (i.e. requested but not received). We need to track these `objectId`s
  -- since they need to be acknowledged.
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
  ( Arbitrary objectId
  , Arbitrary object
  , Ord objectId
  ) =>
  Arbitrary (DecisionPeerState objectId object)
  where
    arbitrary = DecisionPeerState
      <$> (NumObjectIdsReq <$> arbitrary)
      <*> (fromList <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  ( NoThunks objectId
  , NoThunks object
  ) =>
  NoThunks (DecisionPeerState objectId object)

-- | Shared state of all `ObjectDiffusion` clients.
data DecisionGlobalState peerAddr objectId object = DecisionGlobalState
  { dgsPeerStates :: !(Map peerAddr (DecisionPeerState objectId object))
  -- ^ Map of peer states.
  --
  -- /Invariant:/ for peerAddr's which are registered using `withPeer`,
  -- there's always an entry in this map even if the set of `objectId`s is
  -- empty.
  }
  deriving (Eq, Show, Generic)

instance
  ( Arbitrary peerAddr
  , Arbitrary object
  , Arbitrary objectId
  , Ord peerAddr
  , Ord objectId
  ) =>
  Arbitrary (DecisionGlobalState peerAddr objectId object) where
    arbitrary = DecisionGlobalState <$> arbitrary

instance
  ( NoThunks peerAddr
  , NoThunks object
  , NoThunks objectId
  ) =>
  NoThunks (DecisionGlobalState peerAddr objectId object)

-- | Merge dpsObjectsAvailableIds from all peers of the global state.
dgsObjectsAvailableMultiplicities ::
  Ord objectId => DecisionGlobalState peerAddr objectId object -> Map objectId ObjectMultiplicity
dgsObjectsAvailableMultiplicities DecisionGlobalState{dgsPeerStates} =
  Map.unionsWith
    (+)
    (Map.fromSet (const 1) . dpsObjectsAvailableIds <$> Map.elems dgsPeerStates)

dgsObjectsInflightMultiplicities ::
  Ord objectId => DecisionGlobalState peerAddr objectId object -> Map objectId ObjectMultiplicity
dgsObjectsInflightMultiplicities DecisionGlobalState{dgsPeerStates} =
  Map.unionsWith
    (+)
    (Map.fromSet (const 1) . dpsObjectsInflightIds <$> Map.elems dgsPeerStates)

dgsObjectsOwtPoolMultiplicities ::
  Ord objectId => DecisionGlobalState peerAddr objectId object -> Map objectId ObjectMultiplicity
dgsObjectsOwtPoolMultiplicities DecisionGlobalState{dgsPeerStates} =
  Map.unionsWith
    (+)
    (Map.fromSet (const 1) . Map.keysSet . dpsObjectsOwtPool <$> Map.elems dgsPeerStates)

type DecisionGlobalStateVar m peerAddr objectId object =
  StrictTVar m (DecisionGlobalState peerAddr objectId object)

newDecisionGlobalStateVar ::
  MonadSTM m =>
  m (DecisionGlobalStateVar m peerAddr objectId object)
newDecisionGlobalStateVar =
  newTVarIO
    DecisionGlobalState
      { dgsPeerStates = Map.empty
      }

--
-- Decisions
--

-- | Decision made by the decision logic.  Each peer will receive a 'Decision'.
--
-- /note:/ it is rather non-standard to represent a choice between requesting
-- `objectId`s and `object`'s as a product rather than a sum type. The client will
-- need to download `object`s first and then send a request for more objectIds (and
-- acknowledge some `objectId`s). Due to pipelining each client will request
-- decision from the decision logic quite often (every two pipelined requests).
--
-- TODO: in the previous design, we prefiltered active peers before calling
-- `makeDecision`, so that a decision once taken would make the peer non-active
-- (e.g. it won't be returned by `filterActivePeers`) for longer, and thus the
-- expensive `makeDecision` computation would not need to take that peer into
-- account. This is no longer the case, but we could reintroduce this optimization
-- if needed.
data PeerDecision objectId object = PeerDecision
  { pdNumIdsToAck :: !NumObjectIdsAck
  -- ^ objectId's to acknowledge
  , pdNumIdsToReq :: !NumObjectIdsReq
  -- ^ number of objectId's to request
  , pdCanPipelineIdsRequests :: !Bool
  -- ^ the object-submission protocol only allows to pipeline `objectId`'s requests
  -- if we have non-acknowledged `objectId`s.
  , pdObjectsToReqIds :: !(Set objectId)
  -- ^ objectId's to download.
  , pdStatus :: !PeerDecisionStatus
  -- ^ Whether the peer is actually executing the said decision
  }
  deriving (Show, Eq)

instance
  ( Arbitrary objectId
  , Ord objectId
  ) =>
  Arbitrary (PeerDecision objectId object) where
    arbitrary = PeerDecision
      <$> (NumObjectIdsAck <$> arbitrary)
      <*> (NumObjectIdsReq <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  ( NFData objectId
  , NFData object
  ) =>
  NFData (PeerDecision objectId object) where
    rnf = undefined

data PeerDecisionStatus
  = DecisionUnread
  | DecisionBeingActedUpon
  | DecisionCompleted
  deriving (Show, Eq)

instance Arbitrary PeerDecisionStatus where
  arbitrary = elements
    [ DecisionUnread
    , DecisionBeingActedUpon
    , DecisionCompleted
    ]

-- | A placeholder when no decision has been made, at the beginning of a loop.
-- Nothing should be read from it except its status.
unavailableDecision :: HasCallStack => PeerDecision objectId object
unavailableDecision =
  PeerDecision
    { pdStatus = DecisionCompleted
    , pdObjectsToReqIds = error "This decision is not available yet"
    , pdNumIdsToAck = error "This decision is not available yet"
    , pdNumIdsToReq = error "This decision is not available yet"
    , pdCanPipelineIdsRequests = error "This decision is not available yet"
    }

-- | ObjectLogic tracer.
data TraceDecisionLogic peerAddr objectId object
  = TraceDecisionLogicGlobalStateUpdated String (DecisionGlobalState peerAddr objectId object)
  | TraceDecisionLogicDecisionsMade (Map peerAddr (PeerDecision objectId object))
  deriving Show

data ObjectDiffusionCounters
  = ObjectDiffusionCounters
  { odcNumDistinctObjectsAvailable :: Int
  -- ^ objectIds which are not yet downloaded.
  , odcNumDistinctObjectsInflight :: Int
  -- ^ number of distinct in-flight objects.
  , odcNumTotalObjectsInflight :: Int
  -- ^ number of all in-flight objects.
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
  dgs =
    ObjectDiffusionCounters
      { odcNumDistinctObjectsAvailable = Map.size $ dgsObjectsAvailableMultiplicities dgs
      , odcNumDistinctObjectsInflight = Map.size $ dgsObjectsInflightMultiplicities dgs
      , odcNumTotalObjectsInflight =
          fromIntegral . mconcat . Map.elems $ dgsObjectsInflightMultiplicities dgs
      , odcNumDistinctObjectsOwtPool = Map.size $ dgsObjectsOwtPoolMultiplicities dgs
      }

-- Copied from V1:

newtype NumObjectsProcessed
  = NumObjectsProcessed
  { getNumObjectsProcessed :: Word64
  }
  deriving (Eq, Ord, NFData, Generic)
  deriving newtype (Num, Enum, Real, Integral, Bounded, NoThunks)
  deriving Semigroup via (Sum Word64)
  deriving Monoid via (Sum Word64)
  deriving Show via (Quiet NumObjectsProcessed)

newtype ObjectMultiplicity
  = ObjectMultiplicity
  { getObjectMultiplicity :: Word64
  }
  deriving (Eq, Ord, NFData, Generic)
  deriving newtype (Num, Enum, Real, Integral, Bounded, NoThunks)
  deriving Semigroup via (Sum Word64)
  deriving Monoid via (Sum Word64)
  deriving Show via (Quiet ObjectMultiplicity)

data TraceObjectDiffusionInbound objectId object
  = TraceObjectDiffusionInboundRequestedIds Int
  | TraceObjectDiffusionInboundRequestedObjects Int
  | TraceObjectDiffusionInboundReceivedIds Int
  | TraceObjectDiffusionInboundReceivedObjects Int
  | TraceObjectDiffusionInboundAddedObjects Int
  | -- | Received a 'ControlMessage' from the outbound peer governor, and about
    -- to act on it.
    TraceObjectDiffusionInboundReceivedControlMessage ControlMessage
  | TraceObjectDiffusionInboundReceivedDecision (PeerDecision objectId object)
  deriving (Eq, Show)

data ObjectDiffusionInboundError
  = ProtocolErrorObjectNotRequested
  | ProtocolErrorObjectIdsNotRequested
  | ProtocolErrorObjectIdAlreadyKnown
  | ProtocolErrorObjectIdsDuplicate
  | ProtocolErrorObjectMissing
  deriving Show

instance Exception ObjectDiffusionInboundError where
  displayException ProtocolErrorObjectNotRequested =
    "The peer replied with an object we did not ask for."
  displayException ProtocolErrorObjectIdsNotRequested =
    "The peer replied with more objectIds than we asked for."
  displayException ProtocolErrorObjectIdAlreadyKnown =
    "The peer replied with an objectId that it has already sent us previously."
  displayException ProtocolErrorObjectIdsDuplicate =
    "The peer replied with a batch of objectIds containing a duplicate."
  displayException ProtocolErrorObjectMissing =
    "The peer did not deliver an object for which it claimed to have an id."
