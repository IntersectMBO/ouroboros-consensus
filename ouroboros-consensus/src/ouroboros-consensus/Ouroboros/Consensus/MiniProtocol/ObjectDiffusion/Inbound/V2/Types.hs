{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
  ( -- * State
    PeerState (..)
  , pssObjectsAvailableMultiplicities
  , pssObjectsInflightMultiplicities
  , pssObjectsOwtPoolMultiplicities
  , PeerStatesVar
  , newPeerStatesVar

    -- * Decisions
  , DecisionContext (..)
  , DecisionPolicy (..)
  , PeerDecision (..)
  , PeerDecisionStatus (..)
  , ReqObjectsDecision (..)
  , ReqIdsDecision (..)

    -- * Tracing
  , mempty
  , TraceDecisionLogic (..)
  , ObjectMultiplicity (..)

    -- * Reporting
  , ObjectDiffusionCounters (..)
  , makeObjectDiffusionCounters

    -- * Error and Tracing
  , TraceObjectDiffusionInbound (..)
  , ObjectDiffusionInboundError (..)

    -- * Object pool semaphore
  , ObjectPoolSem (..)
  , newObjectPoolSem
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
  ( MonadSTM
  , StrictTVar
  , atomically
  , newTVarIO
  )
import Control.Concurrent.Class.MonadSTM.TSem (TSem, newTSem)
import Control.DeepSeq (NFData (..))
import Control.Exception (Exception (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Time (DiffTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Ouroboros.Network.ControlMessage (ControlMessage)
import Ouroboros.Network.Protocol.ObjectDiffusion.Type
import Quiet (Quiet (..))
import System.Random (StdGen)

-- | Semaphore to guard access to the ObjectPool
newtype ObjectPoolSem m = ObjectPoolSem (TSem m)

newObjectPoolSem :: MonadSTM m => m (ObjectPoolSem m)
newObjectPoolSem = ObjectPoolSem <$> atomically (newTSem 1)

-- | In all the fields' names,
-- If "Ids" appears at the beginning of a name field, it means we refer to IDs
-- specifically (i.e. before the corresponding object is in flight).
-- On the other hand, a field name of the form "Objects...Ids" means we are
-- speaking of objects (i.e. after they have been requested) but identify them
-- by their IDs for this field purpose.
data PeerState objectId object = PeerState
  { psNumIdsInflight :: !NumObjectIdsReq
  -- ^ The number of object identifiers that we have requested but
  -- which have not yet been replied to. We need to track this it keep
  -- our requests within the limit on the number of unacknowledged objectIds.
  , psOutstandingFifo :: !(StrictSeq objectId)
  -- ^ Sequence of objects (by their id) that the client has told
  -- us about, and which we have not yet acknowledged. This is kept in
  -- the order in which the client gave them to us. It is also the order
  -- in which we acknowledge them.
  , psObjectsAvailableIds :: !(Set objectId)
  -- ^ Set of objects (by their ids) that can be requested from the outbound peer.
  , psObjectsInflightIds :: !(Set objectId)
  -- ^ The set of requested objects (by their ids) that haven't been received yet.
  , psObjectsOwtPool :: !(Map objectId object)
  -- ^ Received objects that are on their way to the objectpool.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, NoThunks)

countMultiplicities ::
  Ord objectId =>
  (PeerState objectId object -> Set objectId) ->
  Map peerAddr (PeerState objectId object) ->
  Map objectId ObjectMultiplicity
countMultiplicities selector peerStates =
  Map.unionsWith
    (+)
    (Map.fromSet (const 1) . selector <$> Map.elems peerStates)

pssObjectsAvailableMultiplicities ::
  Ord objectId =>
  Map peerAddr (PeerState objectId object) ->
  Map objectId ObjectMultiplicity
pssObjectsAvailableMultiplicities = countMultiplicities psObjectsAvailableIds

pssObjectsInflightMultiplicities ::
  Ord objectId =>
  Map peerAddr (PeerState objectId object) ->
  Map objectId ObjectMultiplicity
pssObjectsInflightMultiplicities = countMultiplicities psObjectsInflightIds

pssObjectsOwtPoolMultiplicities ::
  Ord objectId =>
  Map peerAddr (PeerState objectId object) ->
  Map objectId ObjectMultiplicity
pssObjectsOwtPoolMultiplicities = countMultiplicities (Map.keysSet . psObjectsOwtPool)

type PeerStatesVar m peerAddr objectId object =
  StrictTVar m (Map peerAddr (PeerState objectId object))

newPeerStatesVar ::
  MonadSTM m =>
  m (PeerStatesVar m peerAddr objectId object)
newPeerStatesVar =
  newTVarIO Map.empty

--
-- Decisions
--

data DecisionContext peerAddr objectId object = DecisionContext
  { dcRng :: StdGen
  , dcHasObject :: (objectId -> Bool)
  , dcDecisionPolicy :: DecisionPolicy
  , dcPeerStates :: Map peerAddr (PeerState objectId object)
  , dcPrevDecisions :: Map peerAddr (PeerDecisionStatus objectId object)
  }
  deriving stock Generic
  deriving anyclass NFData

-- | Policy for making decisions
data DecisionPolicy = DecisionPolicy
  { dpMaxNumObjectIdsReq :: !NumObjectIdsReq
  -- ^ a maximal number of objectIds requested at once.
  , dpMaxNumObjectsOutstanding :: !NumObjectsOutstanding
  -- ^ maximal number of objects in the outstanding FIFO.
  -- Must be the same as the outbound peer's value.
  , dpMaxNumObjectsInflightPerPeer :: !NumObjectsReq
  -- ^ a limit of objects in-flight from a single peer.
  , dpMaxNumObjectsInflightTotal :: !NumObjectsReq
  -- ^ a limit of objects in-flight from all peers for this node.
  , dpTargetObjectRedundancy :: !ObjectMultiplicity
  -- ^ from how many peers download the `objectId` simultaneously
  , dpDecisionThreadSleepDelay :: !DiffTime
  -- ^ delay (in seconds) between two decision making rounds
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, NoThunks)

-- | Decision made by the decision logic.  Each peer will receive a 'Decision'.
--
-- /note:/ it is rather non-standard to represent a choice between requesting
-- `objectId`s and `object`'s as a product rather than a sum type. The client will
-- need to download `object`s first and then send a request for more objectIds (and
-- acknowledge some `objectId`s). Due to pipelining each client will request
-- decision from the decision logic quite often (every two pipelined requests).
--
-- TODO: in the previous design, we prefiltered pending peers before calling
-- `makeDecision`, so that a decision once taken would make the peer non-pending
-- (e.g. it won't be returned by `filterPendingPeers`) for longer, and thus the
-- expensive `makeDecision` computation would not need to take that peer into
-- account. This is no longer the case, but we could reintroduce this optimization
-- if needed.
data ReqIdsDecision objectId object = ReqIdsDecision
  { ridNumIdsToAck :: !NumObjectIdsAck
  -- ^ objectId's to acknowledge
  , ridNumIdsToReq :: !NumObjectIdsReq
  -- ^ number of objectId's to request
  , ridCanPipelineIdsRequests :: !Bool
  -- ^ the object-submission protocol only allows to pipeline `objectId`'s requests
  -- if we have non-acknowledged `objectId`s.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, NoThunks)

newtype ReqObjectsDecision objectId object = ReqObjectsDecision
  { rodObjectsToReqIds :: Set objectId
  -- ^ objectId's to request
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, NoThunks)

data PeerDecision objectId object
  = PeerDecision
  { pdReqObjects :: ReqObjectsDecision objectId object
  , pdReqIds :: ReqIdsDecision objectId object
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, NoThunks)

data PeerDecisionStatus objectId object
  = PeerDecisionUnread !(PeerDecision objectId object)
  | PeerDecisionBeingActedUpon !(PeerDecision objectId object)
  | PeerDecisionCompleted
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, NoThunks)

-- | ObjectLogic tracer.
data TraceDecisionLogic peerAddr objectId object
  = TraceDecisionLogicPeerStatesUpdated String (Map peerAddr (PeerState objectId object))
  | TraceDecisionLogicDecisionsMade (Map peerAddr (PeerDecision objectId object))
  deriving stock (Show, Eq, Generic)

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
  deriving stock (Show, Eq, Generic)

makeObjectDiffusionCounters ::
  Ord objectId =>
  Map peerAddr (PeerState objectId object) ->
  ObjectDiffusionCounters
makeObjectDiffusionCounters
  peerStates =
    ObjectDiffusionCounters
      { odcNumDistinctObjectsAvailable =
          Map.size $ pssObjectsAvailableMultiplicities peerStates
      , odcNumDistinctObjectsInflight =
          Map.size $ pssObjectsInflightMultiplicities peerStates
      , odcNumTotalObjectsInflight =
          fromIntegral . mconcat . Map.elems $ pssObjectsInflightMultiplicities peerStates
      , odcNumDistinctObjectsOwtPool =
          Map.size $ pssObjectsOwtPoolMultiplicities peerStates
      }

newtype ObjectMultiplicity
  = ObjectMultiplicity
  { getObjectMultiplicity :: Word64
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (NFData, NoThunks, Num, Enum, Real, Integral, Bounded)
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
  | TraceObjectDiffusionInboundTerminated
  | TraceObjectDiffusionInboundReceivedDecision (PeerDecision objectId object)
  deriving stock (Show, Eq, Generic)

data ObjectDiffusionInboundError
  = ProtocolErrorObjectNotRequested
  | ProtocolErrorObjectIdsNotRequested
  | ProtocolErrorObjectIdAlreadyKnown
  | ProtocolErrorObjectIdsDuplicate
  | ProtocolErrorObjectMissing
  deriving stock (Show, Eq, Generic)

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
