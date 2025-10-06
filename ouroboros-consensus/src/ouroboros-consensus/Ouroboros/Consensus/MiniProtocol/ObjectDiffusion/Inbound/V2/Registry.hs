{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Registry
  ( PeerDecisionChannels (..)
  , PeerDecisionChannelsVar
  , ObjectPoolSem
  , DecisionGlobalStateVar
  , newDecisionGlobalStateVar
  , newPeerDecisionChannelsVar
  , newObjectPoolSem
  , InboundPeerAPI (..)
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

-- | Communication channels between `ObjectDiffusion` mini-protocol inbound side
-- and decision logic.
type PeerDecisionChannels m peerAddr objectId object =
  Map peerAddr (StrictMVar m (PeerDecision objectId object))

type PeerDecisionChannelsVar m peerAddr objectId object =
  StrictMVar m (PeerDecisionChannels m peerAddr objectId object)

newPeerDecisionChannelsVar ::
  MonadMVar m => m (PeerDecisionChannelsVar m peerAddr objectId object)
newPeerDecisionChannelsVar = newMVar (PeerDecisionChannels Map.empty)

-- | Semaphore to guard access to the ObjectPool
newtype ObjectPoolSem m = ObjectPoolSem (TSem m)

newObjectPoolSem :: MonadSTM m => m (ObjectPoolSem m)
newObjectPoolSem = ObjectPoolSem <$> atomically (newTSem 1)

data InboundPeerAPI m objectId object = InboundPeerAPI
  { readPeerDecision :: m (PeerDecision objectId object)
  -- ^ a blocking action which reads `PeerDecision`
  , handleReceivedIds :: [objectId] -> m ()
  , handleReceivedObjects :: [object] -> m ()
  , submitObjectsToPool :: [object] -> m ()
  }

-- | A bracket function which registers / de-registers a new peer in
-- `DecisionGlobalStateVar` and `PeerDecisionChannelsVar`s, which exposes `InboundPeerAPI`.
-- `InboundPeerAPI` is only safe inside the `withPeer` scope.
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
  PeerDecisionChannelsVar m peerAddr objectId object ->
  ObjectPoolSem m ->
  DecisionPolicy ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  ObjectPoolReader objectId object ticketNo m ->
  ObjectPoolWriter objectId object ticketNo m ->
  -- | new peer
  peerAddr ->
  -- | callback which gives access to `InboundPeerAPI`
  (InboundPeerAPI m objectId object -> m a) ->
  m a
withPeer
  decisionTracer
  decisionChannelsVar
  (ObjectPoolSem poolSem)
  policy@DecisionPolicy{dpMinObtainedButNotAckedObjectsLifetime}
  globalStateVar
  ObjectPoolReader{}
  ObjectPoolWriter{opwAddObjects}
  peerAddr
  withApi =
    bracket registerPeerAndCreateAPI unregisterPeer withAPI
  where
    registerPeerAndCreateAPI :: m (InboundPeerAPI m objectId object)
    registerPeerAndCreateAPI = do
          -- create the API for this peer, obtaining a channel for it in the process
          !inboundPeerAPI <-
            modifyMVar
              decisionChannelsVar
              \peerToChannel -> do
                -- We get a channel for this peer, and register it in peerToChannel.
                (chan', peerToChannel') <-
                  case peerToChannel Map.!? peerAddr of
                    -- Checks if a channel already exists for this peer, in case we reuse it
                    Just chan -> return (chan, peerToChannel)
                    -- Otherwise create a new channel and register it
                    Nothing -> do
                      chan <- newEmptyMVar
                      return (chan, Map.insert peerAddr chan peerToChannel)
                return
                  ( peerToChannel'
                  , InboundPeerAPI
                      { readPeerDecision = takeMVar chan'
                      , handleReceivedIds =
                          collectIds
                            decisionTracer
                            globalStateVar
                            objectpoolGetSnapshot
                            peerAddr
                            numObjectIdsToReq
                            objectIdsSeq
                            objectIdsMap
                      , handleReceivedObjects =
                          collectObjects
                            decisionTracer
                            objectSize
                            globalStateVar
                            peerAddr
                            objectIds
                            objects
                      , submitObjectsToPool
                      }
                  )
          -- register the peer in the global state now
          atomically $ modifyTVar globalStateVar registerPeerGlobalState
          -- initialization is complete for this peer, it can proceed and
          -- interact through its given API
          return inboundPeerAPI
      where

    unregisterPeer :: InboundPeerAPI m objectId object -> m ()
    unregisterPeer _ =
      -- the handler is a short blocking operation, thus we need to use
      -- `uninterruptibleMask_`
      uninterruptibleMask_ do
          -- unregister the peer from the global state
          atomically $ modifyTVar globalStateVar unregisterPeerGlobalState
          -- remove the channel of this peer from the global channel map
          modifyMVar_
            decisionChannelsVar
            \peerToChannel ->
              return $ Map.delete peerAddr peerToChannel

    registerPeerGlobalState ::
      DecisionGlobalState peerAddr objectId object ->
      DecisionGlobalState peerAddr objectId object
    registerPeerGlobalState st@DecisionGlobalState{dgsPeerStates} =
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
    unregisterPeerGlobalState ::
      DecisionGlobalState peerAddr objectId object ->
      DecisionGlobalState peerAddr objectId object
    unregisterPeerGlobalState
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
countRejectedObjects now n = atomically $ stateTVar globalStateVar $ \st ->
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
drainRejectionThread decisionTracer policy globalStateVar = do
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
      st <- readTVar globalStateVar
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
      writeTVar globalStateVar st'
      return st'
    traceWith decisionTracer (TraceDecisionGlobalState "drainRejectionThread" st'')

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
  PeerDecisionChannelsVar m peerAddr objectId object ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  m Void
decisionLogicThread decisionTracer counterTracer policy objectChannelsVar globalStateVar = do
  labelThisThread "object-decision"
  go
 where
  go :: m Void
  go = do
    -- We rate limit the decision making process, it could overwhelm the CPU
    -- if there are too many inbound connections.
    threadDelay _DECISION_LOOP_DELAY

    (decisions, st) <- atomically do
      sharedObjectState <- readTVar globalStateVar
      let activePeers = filterActivePeers policy sharedObjectState

      -- block until at least one peer is active
      check (not (Map.null activePeers))

      let (sharedState, decisions) = makeDecisions policy sharedObjectState activePeers
      writeTVar globalStateVar sharedState
      return (decisions, sharedState)
    traceWith decisionTracer (TraceDecisionGlobalState "decisionLogicThread" st)
    traceWith decisionTracer (TracePeerDecisions decisions)
    PeerDecisionChannels{peerToChannel} <- readMVar objectChannelsVar
    traverse_
      (\(mvar, d) -> modifyMVarWithDefault_ mvar d (\d' -> pure (d' <> d)))
      ( Map.intersectionWith
          (,)
          peerToChannel
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
  PeerDecisionChannelsVar m peerAddr objectId object ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  m Void
decisionLogicThreads decisionTracer counterTracer policy objectChannelsVar globalStateVar =
  uncurry (<>)
    <$> drainRejectionThread decisionTracer policy globalStateVar
      `concurrently` decisionLogicThread decisionTracer counterTracer policy objectChannelsVar globalStateVar

-- `5ms` delay
_DECISION_LOOP_DELAY :: DiffTime
_DECISION_LOOP_DELAY = 0.005
