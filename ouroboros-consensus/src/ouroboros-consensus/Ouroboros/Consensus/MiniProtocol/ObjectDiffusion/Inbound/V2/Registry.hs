{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Registry
  ( ObjectChannels (..)
  , ObjectChannelsVar
  , ObjectObjectPoolSem
  , SharedObjectStateVar
  , newSharedObjectStateVar
  , newObjectChannelsVar
  , newObjectObjectPoolSem
  , PeerObjectAPI (..)
  , decisionLogicThreads
  , withPeer
  ) where

import Control.Concurrent.Class.MonadMVar.Strict
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Concurrent.Class.MonadSTM.TSem
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)
import Data.Foldable as Foldable (foldl', traverse_)
import Data.Hashable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import Data.Void (Void)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Decision
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Policy
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.State
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Network.Protocol.ObjectDiffusion.Type

-- | Communication channels between `ObjectDiffusion` client mini-protocol and
-- decision logic.
newtype ObjectChannels m peeraddr objectId object = ObjectChannels
  { objectChannelMap :: Map peeraddr (StrictMVar m (ObjectDecision objectId object))
  }

type ObjectChannelsVar m peeraddr objectId object =
  StrictMVar m (ObjectChannels m peeraddr objectId object)

newObjectChannelsVar :: MonadMVar m => m (ObjectChannelsVar m peeraddr objectId object)
newObjectChannelsVar = newMVar (ObjectChannels Map.empty)

newtype ObjectObjectPoolSem m = ObjectObjectPoolSem (TSem m)

newObjectObjectPoolSem :: MonadSTM m => m (ObjectObjectPoolSem m)
newObjectObjectPoolSem = ObjectObjectPoolSem <$> atomically (newTSem 1)

-- | API to access `PeerObjectState` inside `PeerObjectStateVar`.
data PeerObjectAPI m objectId object = PeerObjectAPI
  { readObjectDecision :: m (ObjectDecision objectId object)
  -- ^ a blocking action which reads `ObjectDecision`
  , handleReceivedObjectIds ::
      NumObjectIdsToReq ->
      StrictSeq objectId ->
      -- \^ received objectIds
      Map objectId SizeInBytes ->
      -- \^ received sizes of advertised object's
      m ()
  -- ^ handle received objectIds
  , handleReceivedObjects ::
      Map objectId SizeInBytes ->
      -- \^ requested objectIds
      Map objectId object ->
      -- \^ received objects
      m (Maybe ObjectDiffusionProtocolError)
  -- ^ handle received objects
  , submitObjectToObjectPool ::
      Tracer m (TraceObjectDiffusionInbound objectId object) ->
      objectId ->
      object ->
      m ()
  -- ^ submit the given (objectId, object) to the objectpool.
  }

data ObjectObjectPoolResult = ObjectAccepted | ObjectRejected

-- | A bracket function which registers / de-registers a new peer in
-- `SharedObjectStateVar` and `PeerObjectStateVar`s,  which exposes `PeerObjectStateAPI`.
-- `PeerObjectStateAPI` is only safe inside the  `withPeer` scope.
withPeer ::
  forall object peeraddr objectId idx m a.
  ( MonadMask m
  , MonadMVar m
  , MonadSTM m
  , MonadMonotonicTime m
  , Ord objectId
  , Show objectId
  , Typeable objectId
  , Ord peeraddr
  , Show peeraddr
  ) =>
  Tracer m (TraceObjectLogic peeraddr objectId object) ->
  ObjectChannelsVar m peeraddr objectId object ->
  ObjectObjectPoolSem m ->
  ObjectDecisionPolicy ->
  SharedObjectStateVar m peeraddr objectId object ->
  ObjectDiffusionObjectPoolReader objectId object idx m ->
  ObjectDiffusionObjectPoolWriter objectId object idx m ->
  (object -> SizeInBytes) ->
  peeraddr ->
  --  ^ new peer

  -- | callback which gives access to `PeerObjectStateAPI`
  (PeerObjectAPI m objectId object -> m a) ->
  m a
withPeer
  tracer
  channelsVar
  (ObjectObjectPoolSem objectpoolSem)
  policy@ObjectDecisionPolicy{bufferedObjectsMinLifetime}
  sharedStateVar
  ObjectDiffusionObjectPoolReader{objectpoolGetSnapshot}
  ObjectDiffusionObjectPoolWriter{objectpoolAddObjects}
  objectSize
  peeraddr
  io =
    bracket
      ( do
          -- create a communication channel
          !peerObjectAPI <-
            modifyMVar
              channelsVar
              \ObjectChannels{objectChannelMap} -> do
                chann <- newEmptyMVar
                let (chann', objectChannelMap') =
                      Map.alterF
                        ( \mbChann ->
                            let !chann'' = fromMaybe chann mbChann
                             in (chann'', Just chann'')
                        )
                        peeraddr
                        objectChannelMap
                return
                  ( ObjectChannels{objectChannelMap = objectChannelMap'}
                  , PeerObjectAPI
                      { readObjectDecision = takeMVar chann'
                      , handleReceivedObjectIds
                      , handleReceivedObjects
                      , submitObjectToObjectPool
                      }
                  )

          atomically $ modifyTVar sharedStateVar registerPeer
          return peerObjectAPI
      )
      -- the handler is a short blocking operation, thus we need to use
      -- `uninterruptibleMask_`
      ( \_ -> uninterruptibleMask_ do
          atomically $ modifyTVar sharedStateVar unregisterPeer
          modifyMVar_
            channelsVar
            \ObjectChannels{objectChannelMap} ->
              return ObjectChannels{objectChannelMap = Map.delete peeraddr objectChannelMap}
      )
      io
   where
    registerPeer ::
      SharedObjectState peeraddr objectId object ->
      SharedObjectState peeraddr objectId object
    registerPeer st@SharedObjectState{peerObjectStates} =
      st
        { peerObjectStates =
            Map.insert
              peeraddr
              PeerObjectState
                { availableObjectIds = Map.empty
                , requestedObjectIdsInflight = 0
                , requestedObjectsInflightSize = 0
                , requestedObjectsInflight = Set.empty
                , unacknowledgedObjectIds = StrictSeq.empty
                , unknownObjects = Set.empty
                , score = 0
                , scoreTs = Time 0
                , downloadedObjects = Map.empty
                , toObjectPoolObjects = Map.empty
                }
              peerObjectStates
        }

    -- TODO: this function needs to be tested!
    -- Issue: https://github.com/IntersectMBO/ouroboros-network/issues/5151
    unregisterPeer ::
      SharedObjectState peeraddr objectId object ->
      SharedObjectState peeraddr objectId object
    unregisterPeer
      st@SharedObjectState
        { peerObjectStates
        , bufferedObjects
        , referenceCounts
        , inflightObjects
        , inflightObjectsSize
        , inSubmissionToObjectPoolObjects
        } =
        st
          { peerObjectStates = peerObjectStates'
          , bufferedObjects = bufferedObjects'
          , referenceCounts = referenceCounts'
          , inflightObjects = inflightObjects'
          , inflightObjectsSize = inflightObjectsSize'
          , inSubmissionToObjectPoolObjects = inSubmissionToObjectPoolObjects'
          }
       where
        ( PeerObjectState
            { unacknowledgedObjectIds
            , requestedObjectsInflight
            , requestedObjectsInflightSize
            , toObjectPoolObjects
            }
          , peerObjectStates'
          ) =
            Map.alterF
              ( \case
                  Nothing -> error ("ObjectDiffusion.withPeer: invariant violation for peer " ++ show peeraddr)
                  Just a -> (a, Nothing)
              )
              peeraddr
              peerObjectStates

        referenceCounts' =
          Foldable.foldl'
            ( flip $ Map.update \cnt ->
                if cnt > 1
                  then Just $! pred cnt
                  else Nothing
            )
            referenceCounts
            unacknowledgedObjectIds

        liveSet = Map.keysSet referenceCounts'

        bufferedObjects' =
          bufferedObjects
            `Map.restrictKeys` liveSet

        inflightObjects' = Foldable.foldl' purgeInflightObjects inflightObjects requestedObjectsInflight
        inflightObjectsSize' = inflightObjectsSize - requestedObjectsInflightSize

        -- When we unregister a peer, we need to subtract all objects in the
        -- `toObjectPoolObjects`, as they will not be submitted to the objectpool.
        inSubmissionToObjectPoolObjects' =
          Foldable.foldl'
            ( flip $ Map.update \cnt ->
                if cnt > 1
                  then Just $! pred cnt
                  else Nothing
            )
            inSubmissionToObjectPoolObjects
            (Map.keysSet toObjectPoolObjects)

        purgeInflightObjects m objectId = Map.alter fn objectId m
         where
          fn (Just n) | n > 1 = Just $! pred n
          fn _ = Nothing

    --
    -- PeerObjectAPI
    --

    submitObjectToObjectPool ::
      Tracer m (TraceObjectDiffusionInbound objectId object) -> objectId -> object -> m ()
    submitObjectToObjectPool objectTracer objectId object =
      bracket_
        (atomically $ waitTSem objectpoolSem)
        (atomically $ signalTSem objectpoolSem)
        $ do
          start <- getMonotonicTime
          res <- addObject
          end <- getMonotonicTime
          atomically $ modifyTVar sharedStateVar (updateBufferedObject end res)
          let duration = end `diffTime` start
          case res of
            ObjectAccepted -> traceWith objectTracer (TraceObjectInboundAddedToObjectPool [objectId] duration)
            ObjectRejected -> traceWith objectTracer (TraceObjectInboundRejectedFromObjectPool [objectId] duration)
     where
      -- add the object to the objectpool
      addObject :: m ObjectObjectPoolResult
      addObject = do
        mpSnapshot <- atomically objectpoolGetSnapshot

        -- Note that checking if the objectpool contains a OBJECT before
        -- spending several ms attempting to add it to the pool has
        -- been judged immoral.
        if objectpoolHasObject mpSnapshot objectId
          then do
            !now <- getMonotonicTime
            !s <- countRejectedObjects now 1
            traceWith objectTracer $
              TraceObjectDiffusionProcessed
                ProcessedObjectCount
                  { pobjectcAccepted = 0
                  , pobjectcRejected = 1
                  , pobjectcScore = s
                  }
            return ObjectRejected
          else do
            acceptedObjects <- objectpoolAddObjects [object]
            end <- getMonotonicTime
            if null acceptedObjects
              then do
                !s <- countRejectedObjects end 1
                traceWith objectTracer $
                  TraceObjectDiffusionProcessed
                    ProcessedObjectCount
                      { pobjectcAccepted = 0
                      , pobjectcRejected = 1
                      , pobjectcScore = s
                      }
                return ObjectRejected
              else do
                !s <- countRejectedObjects end 0
                traceWith objectTracer $
                  TraceObjectDiffusionProcessed
                    ProcessedObjectCount
                      { pobjectcAccepted = 1
                      , pobjectcRejected = 0
                      , pobjectcScore = s
                      }
                return ObjectAccepted

      updateBufferedObject ::
        Time ->
        ObjectObjectPoolResult ->
        SharedObjectState peeraddr objectId object ->
        SharedObjectState peeraddr objectId object
      updateBufferedObject
        _
        ObjectRejected
        st@SharedObjectState
          { peerObjectStates
          , inSubmissionToObjectPoolObjects
          } =
          st
            { peerObjectStates = peerObjectStates'
            , inSubmissionToObjectPoolObjects = inSubmissionToObjectPoolObjects'
            }
         where
          inSubmissionToObjectPoolObjects' =
            Map.update
              (\case 1 -> Nothing; n -> Just $! pred n)
              objectId
              inSubmissionToObjectPoolObjects

          peerObjectStates' = Map.update fn peeraddr peerObjectStates
           where
            fn ps = Just $! ps{toObjectPoolObjects = Map.delete objectId (toObjectPoolObjects ps)}
      updateBufferedObject
        now
        ObjectAccepted
        st@SharedObjectState
          { peerObjectStates
          , bufferedObjects
          , referenceCounts
          , timedObjects
          , inSubmissionToObjectPoolObjects
          } =
          st
            { peerObjectStates = peerObjectStates'
            , bufferedObjects = bufferedObjects'
            , timedObjects = timedObjects'
            , referenceCounts = referenceCounts'
            , inSubmissionToObjectPoolObjects = inSubmissionToObjectPoolObjects'
            }
         where
          inSubmissionToObjectPoolObjects' =
            Map.update
              (\case 1 -> Nothing; n -> Just $! pred n)
              objectId
              inSubmissionToObjectPoolObjects

          timedObjects' = Map.alter fn (addTime bufferedObjectsMinLifetime now) timedObjects
           where
            fn :: Maybe [objectId] -> Maybe [objectId]
            fn Nothing = Just [objectId]
            fn (Just objectIds) = Just $! (objectId : objectIds)

          referenceCounts' = Map.alter fn objectId referenceCounts
           where
            fn :: Maybe Int -> Maybe Int
            fn Nothing = Just 1
            fn (Just n) = Just $! succ n

          bufferedObjects' = Map.insert objectId (Just object) bufferedObjects

          peerObjectStates' = Map.update fn peeraddr peerObjectStates
           where
            fn ps = Just $! ps{toObjectPoolObjects = Map.delete objectId (toObjectPoolObjects ps)}

    handleReceivedObjectIds ::
      NumObjectIdsToReq ->
      StrictSeq objectId ->
      Map objectId SizeInBytes ->
      m ()
    handleReceivedObjectIds numObjectIdsToReq objectIdsSeq objectIdsMap =
      receivedObjectIds
        tracer
        sharedStateVar
        objectpoolGetSnapshot
        peeraddr
        numObjectIdsToReq
        objectIdsSeq
        objectIdsMap

    handleReceivedObjects ::
      Map objectId SizeInBytes ->
      -- \^ requested objectIds with their announced size
      Map objectId object ->
      -- \^ received objects
      m (Maybe ObjectDiffusionProtocolError)
    handleReceivedObjects objectIds objects =
      collectObjects tracer objectSize sharedStateVar peeraddr objectIds objects

    -- Update `score` & `scoreTs` fields of `PeerObjectState`, return the new
    -- updated `score`.
    --
    -- PRECONDITION: the `Double` argument is non-negative.
    countRejectedObjects ::
      Time ->
      Double ->
      m Double
    countRejectedObjects _ n
      | n < 0 =
          error ("ObjectDiffusion.countRejectedObjects: invariant violation for peer " ++ show peeraddr)
    countRejectedObjects now n = atomically $ stateTVar sharedStateVar $ \st ->
      let (result, peerObjectStates') = Map.alterF fn peeraddr (peerObjectStates st)
       in (result, st{peerObjectStates = peerObjectStates'})
     where
      fn :: Maybe (PeerObjectState objectId object) -> (Double, Maybe (PeerObjectState objectId object))
      fn Nothing = error ("ObjectDiffusion.withPeer: invariant violation for peer " ++ show peeraddr)
      fn (Just ps) = (score ps', Just $! ps')
       where
        ps' = updateRejects policy now n ps

updateRejects ::
  ObjectDecisionPolicy ->
  Time ->
  Double ->
  PeerObjectState objectId object ->
  PeerObjectState objectId object
updateRejects _ now 0 pts | score pts == 0 = pts{scoreTs = now}
updateRejects
  ObjectDecisionPolicy{scoreRate, scoreMax}
  now
  n
  pts@PeerObjectState{score, scoreTs} =
    let duration = diffTime now scoreTs
        !drain = realToFrac duration * scoreRate
        !drained = max 0 $ score - drain
     in pts
          { score = min scoreMax $ drained + n
          , scoreTs = now
          }

drainRejectionThread ::
  forall m peeraddr objectId object.
  ( MonadDelay m
  , MonadSTM m
  , MonadThread m
  , Ord objectId
  ) =>
  Tracer m (TraceObjectLogic peeraddr objectId object) ->
  ObjectDecisionPolicy ->
  SharedObjectStateVar m peeraddr objectId object ->
  m Void
drainRejectionThread tracer policy sharedStateVar = do
  labelThisThread "object-rejection-drain"
  now <- getMonotonicTime
  go $ addTime drainInterval now
 where
  drainInterval :: DiffTime
  drainInterval = 7

  go :: Time -> m Void
  go !nextDrain = do
    threadDelay 1

    !now <- getMonotonicTime
    st'' <- atomically $ do
      st <- readTVar sharedStateVar
      let ptss =
            if now > nextDrain
              then Map.map (updateRejects policy now 0) (peerObjectStates st)
              else peerObjectStates st
          st' =
            tickTimedObjects
              now
              st
                { peerObjectStates = ptss
                }
      writeTVar sharedStateVar st'
      return st'
    traceWith tracer (TraceSharedObjectState "drainRejectionThread" st'')

    if now > nextDrain
      then go $ addTime drainInterval now
      else go nextDrain

decisionLogicThread ::
  forall m peeraddr objectId object.
  ( MonadDelay m
  , MonadMVar m
  , MonadSTM m
  , MonadMask m
  , MonadFork m
  , Ord peeraddr
  , Ord objectId
  , Hashable peeraddr
  ) =>
  Tracer m (TraceObjectLogic peeraddr objectId object) ->
  Tracer m ObjectDiffusionCounters ->
  ObjectDecisionPolicy ->
  ObjectChannelsVar m peeraddr objectId object ->
  SharedObjectStateVar m peeraddr objectId object ->
  m Void
decisionLogicThread tracer counterTracer policy objectChannelsVar sharedStateVar = do
  labelThisThread "object-decision"
  go
 where
  go :: m Void
  go = do
    -- We rate limit the decision making process, it could overwhelm the CPU
    -- if there are too many inbound connections.
    threadDelay _DECISION_LOOP_DELAY

    (decisions, st) <- atomically do
      sharedObjectState <- readTVar sharedStateVar
      let activePeers = filterActivePeers policy sharedObjectState

      -- block until at least one peer is active
      check (not (Map.null activePeers))

      let (sharedState, decisions) = makeDecisions policy sharedObjectState activePeers
      writeTVar sharedStateVar sharedState
      return (decisions, sharedState)
    traceWith tracer (TraceSharedObjectState "decisionLogicThread" st)
    traceWith tracer (TraceObjectDecisions decisions)
    ObjectChannels{objectChannelMap} <- readMVar objectChannelsVar
    traverse_
      (\(mvar, d) -> modifyMVarWithDefault_ mvar d (\d' -> pure (d' <> d)))
      ( Map.intersectionWith
          (,)
          objectChannelMap
          decisions
      )
    traceWith counterTracer (mkObjectDiffusionCounters st)
    go

  -- Variant of modifyMVar_ that puts a default value if the MVar is empty.
  modifyMVarWithDefault_ :: StrictMVar m a -> a -> (a -> m a) -> m ()
  modifyMVarWithDefault_ m d io =
    mask $ \restore -> do
      mbA <- tryTakeMVar m
      case mbA of
        Just a -> do
          a' <- restore (io a) `onException` putMVar m a
          putMVar m a'
        Nothing -> putMVar m d

-- | Run `decisionLogicThread` and `drainRejectionThread`.
decisionLogicThreads ::
  forall m peeraddr objectId object.
  ( MonadDelay m
  , MonadMVar m
  , MonadMask m
  , MonadAsync m
  , MonadFork m
  , Ord peeraddr
  , Ord objectId
  , Hashable peeraddr
  ) =>
  Tracer m (TraceObjectLogic peeraddr objectId object) ->
  Tracer m ObjectDiffusionCounters ->
  ObjectDecisionPolicy ->
  ObjectChannelsVar m peeraddr objectId object ->
  SharedObjectStateVar m peeraddr objectId object ->
  m Void
decisionLogicThreads tracer counterTracer policy objectChannelsVar sharedStateVar =
  uncurry (<>)
    <$> drainRejectionThread tracer policy sharedStateVar
      `concurrently` decisionLogicThread tracer counterTracer policy objectChannelsVar sharedStateVar

-- `5ms` delay
_DECISION_LOOP_DELAY :: DiffTime
_DECISION_LOOP_DELAY = 0.005
