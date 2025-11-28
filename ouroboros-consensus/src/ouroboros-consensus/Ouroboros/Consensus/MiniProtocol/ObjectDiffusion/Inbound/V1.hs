{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Defines the inbound side of the ObjectDiffusion mini-protocol.
-- This protocol is inspired from TxSubmission and aims to provide a generic
-- way to diffuse objects for Peras, i.e. Peras votes and certificates.
--
-- See the design document here: https://tweag.github.io/cardano-peras/peras-design.pdf
module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V1
  ( objectDiffusionInbound
  , TraceObjectDiffusionInbound (..)
  , ObjectDiffusionInboundError (..)
  , NumObjectsProcessed (..)
  ) where

import Cardano.Prelude (catMaybes, (&))
import Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked
import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer, traceWith)
import Data.Data (Typeable)
import Data.Foldable as Foldable (foldl', toList)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import Network.TypedProtocol.Core (N (Z), Nat (..), natToInt)
import NoThunks.Class (NoThunks (..))
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Consensus.Util.NormalForm.Invariant (noThunksInvariant)
import Ouroboros.Network.ControlMessage
import Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion)
import Ouroboros.Network.Protocol.ObjectDiffusion.Inbound
import Ouroboros.Network.Protocol.ObjectDiffusion.Type

newtype NumObjectsProcessed
  = NumObjectsProcessed
  { getNumObjectsProcessed :: Word64
  }
  deriving (Eq, Show)

data TraceObjectDiffusionInbound objectId object
  = -- | Number of objects just about to be inserted.
    TraceObjectDiffusionInboundCollectedObjects Int
  | -- | Just processed object pass/fail breakdown.
    TraceObjectDiffusionInboundAddedObjects NumObjectsProcessed
  | -- | Received a 'ControlMessage' from the outbound peer governor, and about
    -- to act on it.
    TraceObjectDiffusionInboundRecvControlMessage ControlMessage
  | TraceObjectDiffusionInboundCanRequestMoreObjects Int
  | TraceObjectDiffusionInboundCannotRequestMoreObjects Int
  deriving (Eq, Show)

data ObjectDiffusionInboundError objectId object
  = ProtocolErrorObjectsDifferentThanRequested (Set objectId) (Set objectId)
  | ProtocolErrorObjectIdsNotRequested Int Int
  | ProtocolErrorObjectIdsAlreadyKnown (Set objectId)
  | ProtocolErrorObjectIdsDuplicate (Map objectId Int)
  deriving Show

instance
  (Show objectId, Typeable object, Typeable objectId) =>
  Exception (ObjectDiffusionInboundError objectId object)
  where
  displayException (ProtocolErrorObjectsDifferentThanRequested reqButNotRecvd recvButNotReq) =
    "The peer replied with different objects than those we asked for: "
      ++ "requested but didn't receive "
      ++ show reqButNotRecvd
      ++ "; received but didn't requested "
      ++ show recvButNotReq
  displayException (ProtocolErrorObjectIdsNotRequested expected actual) =
    "The peer replied with more objectIds than we asked for: expected "
      ++ show expected
      ++ " , received "
      ++ show actual
  displayException (ProtocolErrorObjectIdsAlreadyKnown offenders) =
    "The peer replied with some objectIds that it has already sent us previously: "
      ++ show offenders
  displayException (ProtocolErrorObjectIdsDuplicate offenders) =
    "The peer replied with a batch of objectIds containing duplicates: "
      ++ show offenders

-- | Information maintained internally in the 'objectDiffusionInbound'
-- implementation.
data InboundSt objectId object = InboundSt
  { numIdsInFlight :: !NumObjectIdsReq
  -- ^ The number of object identifiers that we have requested but
  -- which have not yet been replied to. We need to track this to keep
  -- our requests within the limit on the 'outstandingFifo' size.
  , outstandingFifo :: !(StrictSeq objectId)
  -- ^ This mirrors the queue of objects that the outbound peer has available
  -- for us. Objects are kept in the order in which the outbound peer
  -- advertised them to us. This is the same order in which we submit them to
  -- the objectPool. It is also the order we acknowledge them.
  , canRequestNext :: !(Set objectId)
  -- ^ The objectIds that we can request. These are a subset of the
  -- 'outstandingFifo' that we have not yet requested or not have in the pool
  -- already. This is not ordered to illustrate the fact that we can
  -- request objects out of order.
  , pendingObjects :: !(Map objectId (Maybe object))
  -- ^ Objects we have successfully downloaded (or decided intentionally to
  -- skip download) but have not yet added to the objectPool or acknowledged.
  --
  -- Object IDs in this 'Map' are mapped to 'Nothing' if we notice that
  -- they are already in the objectPool. That way we can skip requesting them
  -- from the outbound peer, but still acknowledge them when the time comes.
  , numToAckOnNextReq :: !NumObjectIdsAck
  -- ^ The number of objects we can acknowledge on our next request
  -- for more object IDs. Their corresponding IDs have already been removed
  -- from 'outstandingFifo'.
  }
  deriving stock (Show, Generic)
  deriving anyclass NoThunks

initialInboundSt :: InboundSt objectId object
initialInboundSt = InboundSt 0 Seq.empty Set.empty Map.empty 0

objectDiffusionInbound ::
  forall objectId object m.
  ( Ord objectId
  , Show objectId
  , Typeable objectId
  , Typeable object
  , NoThunks objectId
  , NoThunks object
  , MonadSTM m
  , MonadThrow m
  ) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  -- | Maximum values for outstanding FIFO length, number of IDs to request,
  -- and number of objects to request
  (NumObjectsOutstanding, NumObjectIdsReq, NumObjectsReq) ->
  ObjectPoolWriter objectId object m ->
  NodeToNodeVersion ->
  ControlMessageSTM m ->
  ObjectDiffusionInboundPipelined objectId object m ()
objectDiffusionInbound
  tracer
  (maxFifoLength, maxNumIdsToReq, maxNumObjectsToReq)
  ObjectPoolWriter{..}
  _version
  controlMessageSTM =
    ObjectDiffusionInboundPipelined $!
      checkState initialInboundSt & go Zero
   where
    canRequestMoreObjects :: InboundSt k object -> Bool
    canRequestMoreObjects !st =
      not (Set.null (canRequestNext st))

    -- Computes how many new IDs we can request so that receiving all of them
    -- won't make 'outstandingFifo' exceed 'maxFifoLength'.
    numIdsToReq :: InboundSt objectId object -> NumObjectIdsReq
    numIdsToReq !st =
      maxNumIdsToReq
        `min` ( fromIntegral maxFifoLength
                  - (fromIntegral $ Seq.length $ outstandingFifo st)
                  - numIdsInFlight st
              )

    -- Updates 'InboundSt' with new object IDs and return the updated 'InboundSt'.
    --
    -- Collected object IDs that are already in the objectPool are pre-emptively
    -- acknowledged so that we don't need to bother requesting them from the
    -- outbound peer.
    preAcknowledge ::
      InboundSt objectId object ->
      (objectId -> Bool) ->
      [objectId] ->
      InboundSt objectId object
    preAcknowledge !st _ collectedIds | null collectedIds = st
    preAcknowledge !st poolHasObject collectedIds =
      let
        -- Divide the collected IDs in two parts: those that are already in the
        -- objectPool and those that are not.
        (alreadyObtained, notYetObtained) =
          List.partition
            poolHasObject
            collectedIds

        -- The objects that we intentionally don't request, because they are
        -- already in the objectPool, will need to be acknowledged.
        -- So we extend 'pendingObjects' with those objects (so of course they
        -- have no corresponding reply).
        pendingObjects' =
          foldl'
            (\accMap objectId -> Map.insert objectId Nothing accMap)
            (pendingObjects st)
            alreadyObtained

        -- We initially extend 'outstandingFifo' with the all the collected IDs
        -- (to properly mirror the server state).
        outstandingFifo' = outstandingFifo st <> Seq.fromList collectedIds

        -- Now check if the update of 'pendingObjects' let us acknowledge a prefix
        -- of the 'outstandingFifo', as we do in 'goCollect' -> 'CollectObjects'.
        (objectIdsToAck, outstandingFifo'') =
          Seq.spanl (`Map.member` pendingObjects') outstandingFifo'

        -- If so we can remove them from the 'pendingObjects' structure.
        --
        -- Note that unlike in TX-Submission, we made sure the outstanding FIFO
        -- couldn't have duplicate IDs, so we don't have to worry about re-adding
        -- the duplicate IDs to 'pendingObjects' for future acknowledgment.
        pendingObjects'' =
          Foldable.foldl'
            (flip Map.delete)
            pendingObjects'
            objectIdsToAck

        !st' =
          st
            { canRequestNext = canRequestNext st <> (Set.fromList notYetObtained)
            , pendingObjects = pendingObjects''
            , outstandingFifo = outstandingFifo''
            , numToAckOnNextReq =
                numToAckOnNextReq st
                  + fromIntegral (Seq.length objectIdsToAck)
            }
       in
        st'

    go ::
      forall (n :: N).
      Nat n ->
      InboundSt objectId object ->
      InboundStIdle n objectId object m ()
    go n !st = WithEffect $ do
      -- Check whether we should continue engaging in the protocol.
      ctrlMsg <- atomically controlMessageSTM
      traceWith tracer $
        TraceObjectDiffusionInboundRecvControlMessage ctrlMsg
      case ctrlMsg of
        -- The peer selection governor is asking us to terminate the connection.
        Terminate ->
          pure $! terminateAfterDrain n
        -- Otherwise, we can continue the protocol normally.
        _continue -> case n of
          -- We didn't pipeline any requests, so there are no replies in flight
          -- (nothing to collect)
          Zero -> do
            if canRequestMoreObjects st
              then do
                -- There are no replies in flight, but we do know some more objects
                -- we can ask for, so lets ask for them and more objectIds in a
                -- pipelined way.
                traceWith tracer $
                  TraceObjectDiffusionInboundCanRequestMoreObjects (natToInt n)
                pure $! checkState st & goReqObjectsAndObjectIdsPipelined Zero
              else do
                -- There's no replies in flight, and we have no more objects we can
                -- ask for so the only remaining thing to do is to ask for more
                -- objectIds. Since this is the only thing to do now, we make this a
                -- blocking call.
                traceWith tracer $
                  TraceObjectDiffusionInboundCannotRequestMoreObjects (natToInt n)
                pure $! checkState st & goReqObjectIdsBlocking

          -- We have pipelined some requests, so there are some replies in flight.
          Succ n' ->
            if canRequestMoreObjects st
              then do
                -- We have replies in flight and we should eagerly collect them if
                -- available, but there are objects to request too so we
                -- should *not* block waiting for replies.
                -- So we ask for new objects and objectIds in a pipelined way.
                traceWith tracer $
                  TraceObjectDiffusionInboundCanRequestMoreObjects (natToInt n)
                pure $!
                  CollectPipelined
                    (Just (checkState st & goReqObjectsAndObjectIdsPipelined (Succ n')))
                    (\collected -> checkState st & goCollect n' collected)
              else do
                traceWith tracer $
                  TraceObjectDiffusionInboundCannotRequestMoreObjects (natToInt n)
                -- In this case we can theoretically only collect replies or request
                -- new object IDs.
                --
                -- But it's important not to pipeline more requests for objectIds now
                -- because if we did, then immediately after sending the request (but
                -- having not yet received a response to either this or the other
                -- pipelined requests), we would directly re-enter this code path,
                -- resulting us in filling the pipeline with an unbounded number of
                -- requests.
                --
                -- So we instead block until we collect a reply.
                pure $!
                  CollectPipelined
                    Nothing
                    (\collected -> checkState st & goCollect n' collected)

    goCollect ::
      forall (n :: N).
      Nat n ->
      Collect objectId object ->
      InboundSt objectId object ->
      InboundStIdle n objectId object m ()
    goCollect n collect !st = case collect of
      CollectObjectIds numIdsRequested collectedIds -> WithEffect $ do
        let numCollectedIds = length collectedIds
            collectedIdsMap = Map.fromListWith (+) [(x, 1 :: Int) | x <- collectedIds]

        -- Check they didn't send more than we asked for. We don't need to
        -- check for a minimum: the blocking case checks for non-zero
        -- elsewhere, and for the non-blocking case it is quite normal for
        -- them to send us none.
        when (numCollectedIds > fromIntegral numIdsRequested) $
          throwIO
            ( ProtocolErrorObjectIdsNotRequested @objectId @object
                (fromIntegral numIdsRequested)
                numCollectedIds
            )

        -- Check that the server didn't send duplicate IDs in its response
        when (Map.size collectedIdsMap /= numCollectedIds) $
          throwIO
            ( ProtocolErrorObjectIdsDuplicate @objectId @object $
                Map.filter (> 1) $
                  collectedIdsMap
            )

        -- Check that the server didn't send IDs that were already in the
        -- outstanding FIFO
        let alreadyKnownIds = Seq.filter (`Map.member` collectedIdsMap) (outstandingFifo st)
        when (not (null alreadyKnownIds)) $
          throwIO
            ( ProtocolErrorObjectIdsAlreadyKnown @objectId @object $
                Set.fromList (toList alreadyKnownIds)
            )

        -- We extend our outstanding FIFO with the newly received objectIds by
        -- calling 'preAcknowledge' which will also pre-emptively acknowledge the
        -- objectIds that we already have in the pool and thus don't need to
        -- request.
        let !st' = st{numIdsInFlight = numIdsInFlight st - numIdsRequested}
        poolHasObject <- atomically $ opwHasObject
        let !st'' = preAcknowledge st' poolHasObject collectedIds
        pure $! checkState st'' & go n
      CollectObjects requestedIds collectedObjects -> WithEffect $ do
        let requestedIdsSet = Set.fromList requestedIds
            obtainedIdsSet = Set.fromList (opwObjectId <$> collectedObjects)

        -- To start with we have to verify that the objects they have sent us are
        -- exactly the objects we asked for, not more, not less.
        when (requestedIdsSet /= obtainedIdsSet) $
          let reqButNotRecvd = requestedIdsSet `Set.difference` obtainedIdsSet
              recvButNotReq = obtainedIdsSet `Set.difference` requestedIdsSet
           in throwIO
                ( ProtocolErrorObjectsDifferentThanRequested @objectId @object
                    reqButNotRecvd
                    recvButNotReq
                )

        traceWith tracer $
          TraceObjectDiffusionInboundCollectedObjects (length collectedObjects)

        -- We update 'pendingObjects' with the newly obtained objects
        let pendingObjects' =
              foldl'
                (\accMap object -> Map.insert (opwObjectId object) (Just object) accMap)
                (pendingObjects st)
                collectedObjects

            -- We then find the longest prefix of 'outstandingFifo' for which we have
            -- all the corresponding IDs in 'pendingObjects'.
            -- We remove this prefix from 'outstandingFifo'.
            (objectIdsToAck, outstandingFifo') =
              Seq.spanl (`Map.member` pendingObjects') (outstandingFifo st)

            -- And also remove these entries from 'pendingObjects'.
            --
            -- Note that unlike in TX-Submission, we made sure the outstanding FIFO
            -- couldn't have duplicate IDs, so we don't have to worry about re-adding
            -- the duplicate IDs to 'pendingObjects' for future acknowledgment.
            pendingObjects'' =
              Foldable.foldl'
                (flip Map.delete)
                pendingObjects'
                objectIdsToAck

            -- These are the objects we need to submit to the object pool
            objectsToAck =
              catMaybes $
                (((Map.!) pendingObjects') <$> toList objectIdsToAck)

        opwAddObjects objectsToAck
        traceWith tracer $
          TraceObjectDiffusionInboundAddedObjects
            (NumObjectsProcessed (fromIntegral $ length objectsToAck))

        let !st' =
              st
                { pendingObjects = pendingObjects''
                , outstandingFifo = outstandingFifo'
                , numToAckOnNextReq =
                    numToAckOnNextReq st
                      + fromIntegral (Seq.length objectIdsToAck)
                }
        pure $! checkState st' & go n

    goReqObjectIdsBlocking ::
      InboundSt objectId object ->
      InboundStIdle 'Z objectId object m ()
    goReqObjectIdsBlocking !st =
      let numIdsToRequest = numIdsToReq st
          -- We should only request new object IDs in a blocking way if we have
          -- absolutely nothing else we can do.
          !st' =
            st
              { numToAckOnNextReq = 0
              , numIdsInFlight = numIdsToRequest
              }
       in assert
            ( numIdsInFlight st == 0
                && Seq.null (outstandingFifo st)
                && Set.null (canRequestNext st)
                && Map.null (pendingObjects st)
            )
            $ SendMsgRequestObjectIdsBlocking
              (numToAckOnNextReq st)
              numIdsToRequest
              ( \neCollectedIds ->
                  checkState st' & goCollect Zero (CollectObjectIds numIdsToRequest (NonEmpty.toList neCollectedIds))
              )

    goReqObjectsAndObjectIdsPipelined ::
      forall (n :: N).
      Nat n ->
      InboundSt objectId object ->
      InboundStIdle n objectId object m ()
    goReqObjectsAndObjectIdsPipelined n !st =
      -- TODO: This implementation is deliberately naive, we pick in an
      -- arbitrary order. We may want to revisit this later.
      let (toRequest, canRequestNext') =
            Set.splitAt (fromIntegral maxNumObjectsToReq) (canRequestNext st)
          !st' = st{canRequestNext = canRequestNext'}
       in SendMsgRequestObjectsPipelined
            (toList toRequest)
            (checkState st' & goReqObjectIdsPipelined (Succ n))

    goReqObjectIdsPipelined ::
      forall (n :: N).
      Nat n ->
      InboundSt objectId object ->
      InboundStIdle n objectId object m ()
    goReqObjectIdsPipelined n !st =
      let numIdsToRequest = numIdsToReq st
       in if numIdsToRequest <= 0
            then checkState st & go n
            else
              let !st' =
                    st
                      { numIdsInFlight =
                          numIdsInFlight st
                            + numIdsToRequest
                      , numToAckOnNextReq = 0
                      }
               in SendMsgRequestObjectIdsPipelined
                    (numToAckOnNextReq st)
                    numIdsToRequest
                    (checkState st' & go (Succ n))

    -- Ignore all outstanding replies to messages we pipelined ("drain"), and then
    -- terminate.
    terminateAfterDrain ::
      Nat n -> InboundStIdle n objectId object m ()
    terminateAfterDrain = \case
      Zero -> SendMsgDone ()
      Succ n -> CollectPipelined Nothing $ \_ignoredMsg -> terminateAfterDrain n

-- | Helper to ensure that the `InboundSt` is free of unexpected thunks and
-- stays strict during the whole process
checkState :: NoThunks s => s -> s
checkState !st = checkInvariant (noThunksInvariant st) st
