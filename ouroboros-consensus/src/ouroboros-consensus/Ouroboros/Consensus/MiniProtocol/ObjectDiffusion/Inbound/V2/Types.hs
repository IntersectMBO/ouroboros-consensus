{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
  ( -- * DecisionPeerState
    DecisionPeerState (..)

    -- * DecisionGlobalState
  , DecisionGlobalState (..)

    -- * Decisions
  , ObjectsToObjectPool (..)
  , PeerDecision (..)
  , emptyPeerDecision
  , TraceObjectLogic (..)
  , ObjectDiffusionInitDelay (..)
  , defaultObjectDiffusionInitDelay

    -- * Types shared with V1

    -- ** Various
  , ProcessedObjectCount (..)
  , ObjectDiffusionLogicVersion (..)

    -- ** ObjectPool API
  , ObjectDiffusionObjectPoolWriter (..)

    -- ** Traces
  , TraceObjectDiffusionInbound (..)
  , ObjectDiffusionCounters (..)
  , mkObjectDiffusionCounters

    -- ** Protocol Error
  , ObjectDiffusionProtocolError (..)
  ) where

import Control.Exception (Exception (..))
import Control.Monad.Class.MonadTime.SI
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable, eqT, (:~:) (Refl))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Ouroboros.Network.Protocol.ObjectDiffusion.Type
import System.Random (StdGen)

-- | Flag to enable/disable the usage of the new object-submission logic.
data ObjectDiffusionLogicVersion
  = -- | the legacy `Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V1`
    ObjectDiffusionLogicV1
  | -- | the new `Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2`
    ObjectDiffusionLogicV2
  deriving (Eq, Show, Enum, Bounded)

--
-- DecisionPeerState, DecisionGlobalState
--

data DecisionPeerState objectId object = DecisionPeerState
  { outstandingFifo :: !(StrictSeq objectId)
  -- ^ Those transactions (by their identifier) that the client has told
  -- us about, and which we have not yet acknowledged. This is kept in
  -- the order in which the client gave them to us. This is the same order
  -- in which we submit them to the objectpool (or for this example, the final
  -- result order). It is also the order we acknowledge in.
  , availableObjectIds :: !(Set objectId)
  -- ^ Set of known transaction ids which can be requested from this peer.
  , numIdsInFlight :: !NumObjectIdsReq
  -- ^ The number of transaction identifiers that we have requested but
  -- which have not yet been replied to. We need to track this it keep
  -- our requests within the limit on the number of unacknowledged objectIds.
  , inFlight :: !(Set objectId)
  -- ^ The set of requested `objectId`s.
  , requestedButNotReceived :: !(Set objectId)
  -- ^ A subset of `outstandingFifo` which were unknown to the peer
  -- (i.e. requested but not received). We need to track these `objectId`s
  -- since they need to be acknowledged.
  --
  -- We track these `objectId` per peer, rather than in `globalObtainedButNotAckedObjects` map,
  -- since that could potentially lead to corrupting the node, not being
  -- able to download a `object` which is needed & available from other nodes.
  , score :: !Double
  -- ^ Score is a metric that tracks how usefull a peer has been.
  -- The larger the value the less usefull peer. It slowly decays towards
  -- zero.
  , scoreTs :: !Time
  -- ^ Timestamp for the last time `score` was drained.
  , pendingObjects :: !(Map objectId object)
  -- ^ A set of OBJECTs downloaded from the peer. They are not yet
  -- acknowledged and haven't been sent to the objectpool yet.
  --
  -- Life cycle of entries:
  -- * added when a object is downloaded (see `collectObjectsImpl`)
  -- * follows `outstandingFifo` (see `acknowledgeObjectIds`)
  , toPoolObjects :: !(Map objectId object)
  -- ^ A set of OBJECTs on their way to the objectpool.
  -- Tracked here so that we can cleanup `globalToPoolObjects` if the
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
-- New `objectId` enters `outstandingFifo` it is also added to `availableObjectIds`
-- and `referenceCounts` (see `acknowledgeObjectIdsImpl`).
--
-- When a `objectId` id is selected to be downloaded, it's added to
-- `inFlightSize` (see
-- `Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.Decision.pickObjectsToDownload`).
--
-- When the request arrives, the `objectId` is removed from `globalInFlightObjects`.  It
-- might be added to `requestedButNotReceived` if the server didn't have that `objectId`, or
-- it's added to `globalObtainedButNotAckedObjects` (see `collectObjectsImpl`).
--
-- Whenever we choose `objectId` to acknowledge (either in `acknowledobjectsIdsImpl`,
-- `collectObjectsImpl` or
-- `Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.Decision.pickObjectsToDownload`, we also
-- recalculate `referenceCounts` and only keep live `objectId`s in other maps (e.g.
-- `availableObjectIds`, `globalObtainedButNotAckedObjects`, `requestedButNotReceived`).
data DecisionGlobalState peerAddr objectId object = DecisionGlobalState
  { peerStates :: !(Map peerAddr (DecisionPeerState objectId object))
  -- ^ Map of peer states.
  --
  -- /Invariant:/ for peerAddr's which are registered using `withPeer`,
  -- there's always an entry in this map even if the set of `objectId`s is
  -- empty.
  , globalInFlightObjects :: !(Map objectId Int)
  -- ^ Set of transactions which are in-flight (have already been
  -- requested) together with multiplicities (from how many peers it is
  -- currently in-flight)
  --
  -- This set can intersect with `availableObjectIds`.
  , globalObtainedButNotAckedObjects :: !(Map objectId (Maybe object))
  -- ^ Map of `object` which:
  --
  --    * were downloaded and added to the objectpool,
  --    * are already in the objectpool (`Nothing` is inserted in that case),
  --
  -- We only keep live `objectId`, e.g. ones which `objectId` is unacknowledged by
  -- at least one peer or has a `globalRententionTimeouts` entry.
  --
  -- /Note:/ `objectId`s which `object` were unknown by a peer are tracked
  -- separately in `requestedButNotReceived`.
  --
  -- /Note:/ previous implementation also needed to explicitly track
  -- `objectId`s which were already acknowledged, but are still unacknowledged.
  -- In this implementation, this is done using reference counting.
  --
  -- This map is useful to acknowledge `objectId`s, it's basically taking the
  -- longest prefix which contains entries in `globalObtainedButNotAckedObjects` or `requestedButNotReceived`.
  , referenceCounts :: !(Map objectId Int)
  -- ^ We track reference counts of all unacknowledged and globalRententionTimeouts objectIds.
  -- Once the count reaches 0, a object is removed from `globalObtainedButNotAckedObjects`.
  --
  -- The `bufferedObject` map contains a subset of `objectId` which
  -- `referenceCounts` contains.
  --
  -- /Invariants:/
  --
  --    * the objectId count is equal to multiplicity of objectId in all
  --      `outstandingFifo` sequences;
  --    * @Map.keysSet globalObtainedButNotAckedObjects `Set.isSubsetOf` Map.keysSet referenceCounts@;
  --    * all counts are positive integers.
  , globalRententionTimeouts :: !(Map Time [objectId])
  -- ^ A set of timeouts for objectIds that have been added to globalObtainedButNotAckedObjects after being
  -- inserted into the objectpool.
  --
  -- We need these short timeouts to avoid re-downloading a `object`.  We could
  -- acknowledge this `objectId` to all peers, when a peer from another
  -- continent presents us it again.
  --
  -- Every objectId entry has a reference count in `referenceCounts`.
  , globalToPoolObjects :: !(Map objectId Int)
  -- ^ A set of objectIds that have been downloaded by a peer and are on their
  -- way to the objectpool. We won't issue further fetch-requests for OBJECTs in
  -- this state.  We track these objects to not re-download them from another
  -- peer.
  --
  -- * We subtract from the counter when a given object is added or rejected by
  --   the objectpool or do that for all objects in `toPoolObjects` when a peer is
  --   unregistered.
  -- * We add to the counter when a given object is selected to be added to the
  --   objectpool in `pickObjectsToDownload`.
  , orderRng :: !StdGen
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

--
-- Decisions
--

newtype ObjectsToObjectPool objectId object = ObjectsToObjectPool {listOfObjectsToObjectPool :: [(objectId, object)]}
  deriving newtype (Eq, Show, Semigroup, Monoid)

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
  { objectIdsToAcknowledge :: !NumObjectIdsAck
  -- ^ objectId's to acknowledge
  , objectIdsToRequest :: !NumObjectIdsReq
  -- ^ number of objectId's to request
  , objectPipelineObjectIds :: !Bool
  -- ^ the object-submission protocol only allows to pipeline `objectId`'s requests
  -- if we have non-acknowledged `objectId`s.
  , objectsToRequest :: !(Map objectId SizeInBytes)
  -- ^ objectId's to download.
  , objectsToObjectPool :: !(ObjectsToObjectPool objectId object)
  -- ^ list of `object`s to submit to the objectpool.
  }
  deriving (Show, Eq)

-- | A non-commutative semigroup instance.
--
-- /note:/ this instance must be consistent with `pickObjectsToDownload` and how
-- `DecisionPeerState` is updated.  It is designed to work with `TMergeVar`s.
instance Ord objectId => Semigroup (PeerDecision objectId object) where
  PeerDecision
    { objectIdsToAcknowledge
    , objectIdsToRequest
    , objectPipelineObjectIds = _ignored
    , objectsToRequest
    , objectsToObjectPool
    }
    <> PeerDecision
      { objectIdsToAcknowledge = objectIdsToAcknowledge'
      , objectIdsToRequest = objectIdsToRequest'
      , objectPipelineObjectIds = objectPipelineObjectIds'
      , objectsToRequest = objectsToRequest'
      , objectsToObjectPool = objectsToObjectPool'
      } =
      PeerDecision
        { objectIdsToAcknowledge = objectIdsToAcknowledge + objectIdsToAcknowledge'
        , objectIdsToRequest = objectIdsToRequest + objectIdsToRequest'
        , objectPipelineObjectIds = objectPipelineObjectIds'
        , objectsToRequest = objectsToRequest <> objectsToRequest'
        , objectsToObjectPool = objectsToObjectPool <> objectsToObjectPool'
        }

-- | A no-op decision.
emptyPeerDecision :: PeerDecision objectId object
emptyPeerDecision =
  PeerDecision
    { objectIdsToAcknowledge = 0
    , objectIdsToRequest = 0
    , objectPipelineObjectIds = False
    , objectsToRequest = Map.empty
    , objectsToObjectPool = mempty
    }

-- | ObjectLogic tracer.
data TraceObjectLogic peerAddr objectId object
  = TraceDecisionGlobalState String (DecisionGlobalState peerAddr objectId object)
  | TracePeerDecisions (Map peerAddr (PeerDecision objectId object))
  deriving Show

data ProcessedObjectCount = ProcessedObjectCount
  { pobjectcAccepted :: Int
  -- ^ Just accepted this many transactions.
  , pobjectcRejected :: Int
  -- ^ Just rejected this many transactions.
  , pobjectcScore :: Double
  }
  deriving (Eq, Show)

-- | The consensus layer functionality that the inbound side of the object
-- submission logic requires.
--
-- This is provided to the object submission logic by the consensus layer.
data ObjectDiffusionObjectPoolWriter objectId object ticketNo m
  = ObjectDiffusionObjectPoolWriter
  { objectId :: object -> objectId
  -- ^ Compute the transaction id from a transaction.
  --
  -- This is used in the protocol handler to verify a full transaction
  -- matches a previously given transaction id.
  , objectpoolAddObjects :: [object] -> m [objectId]
  -- ^ Supply a batch of transactions to the objectpool. They are either
  -- accepted or rejected individually, but in the order supplied.
  --
  -- The 'objectId's of all transactions that were added successfully are
  -- returned.
  }

data TraceObjectDiffusionInbound objectId object
  = -- | Number of transactions just about to be inserted.
    TraceObjectDiffusionCollected [objectId]
  | -- | Just processed transaction pass/fail breakdown.
    TraceObjectDiffusionProcessed ProcessedObjectCount
  | TraceObjectInboundCanRequestMoreObjects Int
  | TraceObjectInboundCannotRequestMoreObjects Int
  | TraceObjectInboundAddedToObjectPool [objectId] DiffTime
  | TraceObjectInboundRejectedFromObjectPool [objectId] DiffTime
  | TraceObjectInboundError ObjectDiffusionProtocolError
  | --
    -- messages emitted by the new implementation of the server in
    -- "Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.Server"; some of them are also
    -- used in this module.
    --

    -- | Server received 'MsgDone'
    TraceObjectInboundTerminated
  | TraceObjectInboundDecision (PeerDecision objectId object)
  deriving (Eq, Show)

data ObjectDiffusionCounters
  = ObjectDiffusionCounters
  { numOfOutstandingObjectIds :: Int
  -- ^ objectIds which are not yet downloaded.  This is a diff of keys sets of
  -- `referenceCounts` and a sum of `globalObtainedButNotAckedObjects` and
  -- `inbubmissionToObjectPoolObjects` maps.
  , numOfBufferedObjects :: Int
  -- ^ number of all buffered objects (downloaded or not available)
  , numOfInSubmissionToObjectPoolObjects :: Int
  -- ^ number of all object's which were submitted to the objectpool
  , numOfObjectIdsInflight :: Int
  -- ^ number of all in-flight objectId's.
  }
  deriving (Eq, Show)

mkObjectDiffusionCounters ::
  Ord objectId =>
  DecisionGlobalState peerAddr objectId object ->
  ObjectDiffusionCounters
mkObjectDiffusionCounters
  DecisionGlobalState
    { globalInFlightObjects
    , globalObtainedButNotAckedObjects
    , referenceCounts
    , globalToPoolObjects
    } =
    ObjectDiffusionCounters
      { numOfOutstandingObjectIds =
          Set.size $
            Map.keysSet referenceCounts
              Set.\\ Map.keysSet globalObtainedButNotAckedObjects
              Set.\\ Map.keysSet globalToPoolObjects
      , numOfBufferedObjects = Map.size globalObtainedButNotAckedObjects
      , numOfInSubmissionToObjectPoolObjects = Map.size globalToPoolObjects
      , numOfObjectIdsInflight = getSum $ foldMap Sum globalInFlightObjects
      }

data ObjectDiffusionProtocolError
  = ProtocolErrorObjectNotRequested
  | ProtocolErrorObjectIdsNotRequested
  | -- | a list of objectId for which the received size and advertised size didn't
    -- match.
    forall objectId.
    (Typeable objectId, Show objectId, Eq objectId) =>
    ProtocolErrorObjectSizeError [(objectId, SizeInBytes, SizeInBytes)]

instance Eq ObjectDiffusionProtocolError where
  ProtocolErrorObjectNotRequested == ProtocolErrorObjectNotRequested = True
  ProtocolErrorObjectNotRequested == _ = False
  ProtocolErrorObjectIdsNotRequested == ProtocolErrorObjectIdsNotRequested = True
  ProtocolErrorObjectIdsNotRequested == _ = True
  ProtocolErrorObjectSizeError (as :: [(a, SizeInBytes, SizeInBytes)])
    == ProtocolErrorObjectSizeError (as' :: [(a', SizeInBytes, SizeInBytes)]) =
      case eqT @a @a' of
        Nothing -> False
        Just Refl -> as == as'
  ProtocolErrorObjectSizeError{} == _ = False

deriving instance Show ObjectDiffusionProtocolError

instance Exception ObjectDiffusionProtocolError where
  displayException ProtocolErrorObjectNotRequested =
    "The peer replied with a transaction we did not ask for."
  displayException ProtocolErrorObjectIdsNotRequested =
    "The peer replied with more objectIds than we asked for."
  displayException (ProtocolErrorObjectSizeError objectIds) =
    "The peer received objects with wrong sizes " ++ show objectIds

data ObjectDiffusionInitDelay
  = ObjectDiffusionInitDelay DiffTime
  | NoObjectDiffusionInitDelay
  deriving (Eq, Show)

defaultObjectDiffusionInitDelay :: ObjectDiffusionInitDelay
defaultObjectDiffusionInitDelay = ObjectDiffusionInitDelay 60
