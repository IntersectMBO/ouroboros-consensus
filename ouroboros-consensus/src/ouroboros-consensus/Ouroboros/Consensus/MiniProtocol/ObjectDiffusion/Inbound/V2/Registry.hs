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
  , DecisionGlobalStateVar
  , newDecisionGlobalStateVar
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
newtype ObjectChannels m peerAddr objectId object = ObjectChannels
  { objectChannelMap :: Map peerAddr (StrictMVar m (PeerDecision objectId object))
  }

type ObjectChannelsVar m peerAddr objectId object =
  StrictMVar m (ObjectChannels m peerAddr objectId object)

newObjectChannelsVar :: MonadMVar m => m (ObjectChannelsVar m peerAddr objectId object)
newObjectChannelsVar = newMVar (ObjectChannels Map.empty)

newtype ObjectObjectPoolSem m = ObjectObjectPoolSem (TSem m)

newObjectObjectPoolSem :: MonadSTM m => m (ObjectObjectPoolSem m)
newObjectObjectPoolSem = ObjectObjectPoolSem <$> atomically (newTSem 1)

-- | API to access `DecisionPeerState` inside `DecisionPeerStateVar`.
data PeerObjectAPI m objectId object = PeerObjectAPI
  { readPeerDecision :: m (PeerDecision objectId object)
  -- ^ a blocking action which reads `PeerDecision`
  , handleReceivedObjectIds ::
      NumObjectIdsReq ->
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
      m (Maybe ObjectDiffusionInboundError)
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
-- `DecisionGlobalStateVar` and `DecisionPeerStateVar`s,  which exposes `DecisionPeerStateAPI`.
-- `DecisionPeerStateAPI` is only safe inside the  `withPeer` scope.
withPeer ::
  forall object peerAddr objectId ticketNo m a.
  ( MonadMask m
  , MonadMVar m
  , MonadSTM m
  , MonadMonotonicTime m
  , Ord objectId
  , Show objectId
  , Typeable objectId
  , Ord peerAddr
  , Show peerAddr
  ) =>
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  ObjectChannelsVar m peerAddr objectId object ->
  ObjectObjectPoolSem m ->
  DecisionPolicy ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  ObjectDiffusionObjectPoolReader objectId object ticketNo m ->
  ObjectDiffusionObjectPoolWriter objectId object ticketNo m ->
  (object -> SizeInBytes) ->
  peerAddr ->
  --  ^ new peer

  -- | callback which gives access to `DecisionPeerStateAPI`
  (PeerObjectAPI m objectId object -> m a) ->
  m a
withPeer
  tracer
  channelsVar
  (ObjectObjectPoolSem objectpoolSem)
  policy@DecisionPolicy{dpMinObtainedButNotAckedObjectsLifetime}
  sharedStateVar
  ObjectDiffusionObjectPoolReader{objectpoolGetSnapshot}
  ObjectDiffusionObjectPoolWriter{objectpoolAddObjects}
  objectSize
  peerAddr
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
                        peerAddr
                        objectChannelMap
                return
                  ( ObjectChannels{objectChannelMap = objectChannelMap'}
                  , PeerObjectAPI
                      { readPeerDecision = takeMVar chann'
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
              return ObjectChannels{objectChannelMap = Map.delete peerAddr objectChannelMap}
      )
      io
   where
    registerPeer ::
      DecisionGlobalState peerAddr objectId object ->
      DecisionGlobalState peerAddr objectId object
    registerPeer st@DecisionGlobalState{dgsPeerStates} =
      st
        { dgsPeerStates =
            Map.insert
              peerAddr
              DecisionPeerState
                { dpsIdsAvailable = Map.empty
                , dpsNumIdsInflight = 0
                , dpsObjectsInflightIdsSize = 0
                , dpsObjectsInflightIds = Set.empty
                , dpsOutstandingFifo = StrictSeq.empty
                , dpsObjectsRequestedButNotReceivedIds = Set.empty
                , dpsScore = 0
                , dpsScoreLastUpdatedAt = Time 0
                , dpsObjectsPending = Map.empty
                , dpsObjectsOwtPool = Map.empty
                }
              dgsPeerStates
        }

    -- TODO: this function needs to be tested!
    -- Issue: https://github.com/IntersectMBO/ouroboros-network/issues/5151
    unregisterPeer ::
      DecisionGlobalState peerAddr objectId object ->
      DecisionGlobalState peerAddr objectId object
    unregisterPeer
      st@DecisionGlobalState
        { dgsPeerStates
        , dgsObjectsPending
        , dgsObjectReferenceCounts
        , dgsObjectsInflightMultiplicities
        , dgsObjectsInflightMultiplicitiesSize
        , dgsObjectsOwtPool
        } =
        st
          { dgsPeerStates = dgsPeerStates'
          , dgsObjectsPending = dgsObjectsPending'
          , dgsObjectReferenceCounts = dgsObjectReferenceCounts'
          , dgsObjectsInflightMultiplicities = dgsObjectsInflightMultiplicities'
          , dgsObjectsInflightMultiplicitiesSize = dgsObjectsInflightMultiplicitiesSize'
          , dgsObjectsOwtPool = dgsObjectsOwtPool'
          }
       where
        ( DecisionPeerState
            { dpsOutstandingFifo
            , dpsObjectsInflightIds
            , dpsObjectsInflightIdsSize
            , dpsObjectsOwtPool
            }
          , dgsPeerStates'
          ) =
            Map.alterF
              ( \case
                  Nothing -> error ("ObjectDiffusion.withPeer: invariant violation for peer " ++ show peerAddr)
                  Just a -> (a, Nothing)
              )
              peerAddr
              dgsPeerStates

        dgsObjectReferenceCounts' =
          Foldable.foldl'
            ( flip $ Map.update \cnt ->
                if cnt > 1
                  then Just $! pred cnt
                  else Nothing
            )
            dgsObjectReferenceCounts
            dpsOutstandingFifo

        liveSet = Map.keysSet dgsObjectReferenceCounts'

        dgsObjectsPending' =
          dgsObjectsPending
            `Map.restrictKeys` liveSet

        dgsObjectsInflightMultiplicities' = Foldable.foldl' purgeInflightObjects dgsObjectsInflightMultiplicities dpsObjectsInflightIds
        dgsObjectsInflightMultiplicitiesSize' = dgsObjectsInflightMultiplicitiesSize - dpsObjectsInflightIdsSize

        -- When we unregister a peer, we need to subtract all objects in the
        -- `dpsObjectsOwtPool`, as they will not be submitted to the objectpool.
        dgsObjectsOwtPool' =
          Foldable.foldl'
            ( flip $ Map.update \cnt ->
                if cnt > 1
                  then Just $! pred cnt
                  else Nothing
            )
            dgsObjectsOwtPool
            (Map.keysSet dpsObjectsOwtPool)

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

        -- Note that checking if the objectpool contains a object before
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
        DecisionGlobalState peerAddr objectId object ->
        DecisionGlobalState peerAddr objectId object
      updateBufferedObject
        _
        ObjectRejected
        st@DecisionGlobalState
          { dgsPeerStates
          , dgsObjectsOwtPool
          } =
          st
            { dgsPeerStates = dgsPeerStates'
            , dgsObjectsOwtPool = dgsObjectsOwtPool'
            }
         where
          dgsObjectsOwtPool' =
            Map.update
              (\case 1 -> Nothing; n -> Just $! pred n)
              objectId
              dgsObjectsOwtPool

          dgsPeerStates' = Map.update fn peerAddr dgsPeerStates
           where
            fn ps = Just $! ps{dpsObjectsOwtPool = Map.delete objectId (dpsObjectsOwtPool ps)}
      updateBufferedObject
        now
        ObjectAccepted
        st@DecisionGlobalState
          { dgsPeerStates
          , dgsObjectsPending
          , dgsObjectReferenceCounts
          , dgsRententionTimeouts
          , dgsObjectsOwtPool
          } =
          st
            { dgsPeerStates = dgsPeerStates'
            , dgsObjectsPending = dgsObjectsPending'
            , dgsRententionTimeouts = dgsRententionTimeouts'
            , dgsObjectReferenceCounts = dgsObjectReferenceCounts'
            , dgsObjectsOwtPool = dgsObjectsOwtPool'
            }
         where
          dgsObjectsOwtPool' =
            Map.update
              (\case 1 -> Nothing; n -> Just $! pred n)
              objectId
              dgsObjectsOwtPool

          dgsRententionTimeouts' = Map.alter fn (addTime dpMinObtainedButNotAckedObjectsLifetime now) dgsRententionTimeouts
           where
            fn :: Maybe [objectId] -> Maybe [objectId]
            fn Nothing = Just [objectId]
            fn (Just objectIds) = Just $! (objectId : objectIds)

          dgsObjectReferenceCounts' = Map.alter fn objectId dgsObjectReferenceCounts
           where
            fn :: Maybe Int -> Maybe Int
            fn Nothing = Just 1
            fn (Just n) = Just $! succ n

          dgsObjectsPending' = Map.insert objectId (Just object) dgsObjectsPending

          dgsPeerStates' = Map.update fn peerAddr dgsPeerStates
           where
            fn ps = Just $! ps{dpsObjectsOwtPool = Map.delete objectId (dpsObjectsOwtPool ps)}

    handleReceivedObjectIds ::
      NumObjectIdsReq ->
      StrictSeq objectId ->
      Map objectId SizeInBytes ->
      m ()
    handleReceivedObjectIds numObjectIdsToReq objectIdsSeq objectIdsMap =
      receivedObjectIds
        tracer
        sharedStateVar
        objectpoolGetSnapshot
        peerAddr
        numObjectIdsToReq
        objectIdsSeq
        objectIdsMap

    handleReceivedObjects ::
      Map objectId SizeInBytes ->
      -- \^ requested objectIds with their announced size
      Map objectId object ->
      -- \^ received objects
      m (Maybe ObjectDiffusionInboundError)
    handleReceivedObjects objectIds objects =
      collectObjects tracer objectSize sharedStateVar peerAddr objectIds objects

    -- Update `dpsScore` & `dpsScoreLastUpdatedAt` fields of `DecisionPeerState`, return the new
    -- updated `dpsScore`.
    --
    -- PRECONDITION: the `Double` argument is non-negative.
    countRejectedObjects ::
      Time ->
      Double ->
      m Double
    countRejectedObjects _ n
      | n < 0 =
          error ("ObjectDiffusion.countRejectedObjects: invariant violation for peer " ++ show peerAddr)
    countRejectedObjects now n = atomically $ stateTVar sharedStateVar $ \st ->
      let (result, dgsPeerStates') = Map.alterF fn peerAddr (dgsPeerStates st)
       in (result, st{dgsPeerStates = dgsPeerStates'})
     where
      fn :: Maybe (DecisionPeerState objectId object) -> (Double, Maybe (DecisionPeerState objectId object))
      fn Nothing = error ("ObjectDiffusion.withPeer: invariant violation for peer " ++ show peerAddr)
      fn (Just ps) = (dpsScore ps', Just $! ps')
       where
        ps' = updateRejects policy now n ps

updateRejects ::
  DecisionPolicy ->
  Time ->
  Double ->
  DecisionPeerState objectId object ->
  DecisionPeerState objectId object
updateRejects _ now 0 pts | dpsScore pts == 0 = pts{dpsScoreLastUpdatedAt = now}
updateRejects
  DecisionPolicy{dpScoreDrainRate, dpScoreMaxRejections}
  now
  n
  pts@DecisionPeerState{dpsScore, dpsScoreLastUpdatedAt} =
    let duration = diffTime now dpsScoreLastUpdatedAt
        !drain = realToFrac duration * dpScoreDrainRate
        !drained = max 0 $ dpsScore - drain
     in pts
          { dpsScore = min dpScoreMaxRejections $ drained + n
          , dpsScoreLastUpdatedAt = now
          }

drainRejectionThread ::
  forall m peerAddr objectId object.
  ( MonadDelay m
  , MonadSTM m
  , MonadThread m
  , Ord objectId
  ) =>
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  DecisionPolicy ->
  DecisionGlobalStateVar m peerAddr objectId object ->
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
              then Map.map (updateRejects policy now 0) (dgsPeerStates st)
              else dgsPeerStates st
          st' =
            tickTimedObjects
              now
              st
                { dgsPeerStates = ptss
                }
      writeTVar sharedStateVar st'
      return st'
    traceWith tracer (TraceDecisionGlobalState "drainRejectionThread" st'')

    if now > nextDrain
      then go $ addTime drainInterval now
      else go nextDrain

decisionLogicThread ::
  forall m peerAddr objectId object.
  ( MonadDelay m
  , MonadMVar m
  , MonadSTM m
  , MonadMask m
  , MonadFork m
  , Ord peerAddr
  , Ord objectId
  , Hashable peerAddr
  ) =>
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  Tracer m ObjectDiffusionCounters ->
  DecisionPolicy ->
  ObjectChannelsVar m peerAddr objectId object ->
  DecisionGlobalStateVar m peerAddr objectId object ->
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
    traceWith tracer (TraceDecisionGlobalState "decisionLogicThread" st)
    traceWith tracer (TracePeerDecisions decisions)
    ObjectChannels{objectChannelMap} <- readMVar objectChannelsVar
    traverse_
      (\(mvar, d) -> modifyMVarWithDefault_ mvar d (\d' -> pure (d' <> d)))
      ( Map.intersectionWith
          (,)
          objectChannelMap
          decisions
      )
    traceWith counterTracer (makeObjectDiffusionCounters st)
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
  forall m peerAddr objectId object.
  ( MonadDelay m
  , MonadMVar m
  , MonadMask m
  , MonadAsync m
  , MonadFork m
  , Ord peerAddr
  , Ord objectId
  , Hashable peerAddr
  ) =>
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  Tracer m ObjectDiffusionCounters ->
  DecisionPolicy ->
  ObjectChannelsVar m peerAddr objectId object ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  m Void
decisionLogicThreads tracer counterTracer policy objectChannelsVar sharedStateVar =
  uncurry (<>)
    <$> drainRejectionThread tracer policy sharedStateVar
      `concurrently` decisionLogicThread tracer counterTracer policy objectChannelsVar sharedStateVar

-- `5ms` delay
_DECISION_LOOP_DELAY :: DiffTime
_DECISION_LOOP_DELAY = 0.005
