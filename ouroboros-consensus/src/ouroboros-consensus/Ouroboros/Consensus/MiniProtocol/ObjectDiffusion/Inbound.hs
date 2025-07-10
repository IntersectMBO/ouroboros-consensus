{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}

{-# OPTIONS_GHC -Wno-partial-fields #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound
  ( objectDiffusionInbound
  , MockMempoolWriter (..)
  , TraceObjectDiffusionInbound (..)
  , ObjectDiffusionProtocolError (..)
  , ProcessedObjectCount (..)
  ) where

import Data.Foldable as Foldable (foldl', toList)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as Seq
import Data.Set qualified as Set
import Data.Word (Word16)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..), unsafeNoThunks)

import Cardano.Prelude (forceElemsToWHNF)

import Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked
import Control.Exception (assert)
import Control.Monad (unless)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer, traceWith)

import Network.TypedProtocol.Core (N, Nat (..), natToInt)

import Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion)
import Ouroboros.Network.Protocol.ObjectDiffusion.Server
import Ouroboros.Network.Protocol.ObjectDiffusion.Type

import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.MockMempoolReader

-- TODO: This is a copy of
-- `Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.TxSubmissionMempool.Writer`
-- `ouroboros-network`, brought here to make things compile. We might want a
-- different interface at some point.
--
data MockMempoolWriter objectId object index m =
     MockMempoolWriter {

       -- | Compute the transaction id from a transaction.
       --
       -- This is used in the protocol handler to verify a full transaction
       -- matches a previously given transaction id.
       --
       getObjectId          :: object -> objectId,

       -- | Supply a batch of transactions to the mempool. They are either
       -- accepted or rejected individually, but in the order supplied.
       --
       -- The 'objectId's of all transactions that were added successfully are
       -- returned.
       mempoolAddObjects :: [object] -> m [objectId]
    }

data ProcessedObjectCount = ProcessedObjectCount {
      -- | Just accepted this many transactions.
      pobjectcAccepted :: Int
      -- | Just rejected this many transactions.
    , pobjectcRejected :: Int
    }
  deriving (Eq, Show)

data TraceObjectDiffusionInbound objectId object =
    -- | Number of transactions just about to be inserted.
    TraceObjectDiffusionCollected Int
    -- | Just processed transaction pass/fail breakdown.
  | TraceObjectDiffusionProcessed ProcessedObjectCount
    -- | Server received 'MsgDone'
  | TraceObjectInboundTerminated
  | TraceObjectInboundCanRequestMoreObjects Int
  | TraceObjectInboundCannotRequestMoreObjects Int
  deriving (Eq, Show)

data ObjectDiffusionProtocolError =
       ProtocolErrorObjectNotRequested
     | ProtocolErrorObjectIdsNotRequested
  deriving Show

instance Exception ObjectDiffusionProtocolError where
  displayException ProtocolErrorObjectNotRequested =
      "The peer replied with a transaction we did not ask for."
  displayException ProtocolErrorObjectIdsNotRequested =
      "The peer replied with more objectIds than we asked for."


-- | Information maintained internally in the 'objectDiffusionInbound' server
-- implementation.
--
data ServerState objectId object = ServerState {
       -- | The number of transaction identifiers that we have requested but
       -- which have not yet been replied to. We need to track this it keep
       -- our requests within the limit on the number of unacknowledged objectIds.
       --
       requestedObjectIdsInFlight :: !Word16,

       -- | Those transactions (by their identifier) that the client has told
       -- us about, and which we have not yet acknowledged. This is kept in
       -- the order in which the client gave them to us. This is the same order
       -- in which we submit them to the mempool (or for this example, the final
       -- result order). It is also the order we acknowledge in.
       unacknowledgedObjectIds    :: !(StrictSeq objectId),

       -- | Those transactions (by their identifier) that we can request. These
       -- are a subset of the 'unacknowledgedObjectIds' that we have not yet
       -- requested. This is not ordered to illustrate the fact that we can
       -- request objects out of order. We also remember the size.
       availableObjectids         :: !(Map objectId SizeInBytes),

       -- | Transactions we have successfully downloaded but have not yet added
       -- to the mempool or acknowledged. This needed because we can request
       -- transactions out of order but must use the original order when adding
       -- to the mempool or acknowledging transactions.
       --
       -- However, it's worth noting that, in a few situations, some of the
       -- transaction IDs in this 'Map' may be mapped to 'Nothing':
       --
       -- * Transaction IDs mapped to 'Nothing' can represent transaction IDs
       --   that were requested, but not received. This can occur because the
       --   client will not necessarily send all of the transactions that we
       --   asked for, but we still need to acknowledge those transactions.
       --
       --   For example, if we request a transaction that no longer exists in
       --   the client's mempool, the client will just exclude it from the
       --   response. However, we still need to acknowledge it (i.e. remove it
       --   from the 'unacknowledgedObjectIds') in order to note that we're no
       --   longer awaiting receipt of that transaction.
       --
       -- * Transaction IDs mapped to 'Nothing' can represent transactions
       --   that were not requested from the client because they're already
       --   in the mempool.
       --
       --   For example, if we request some transaction IDs and notice that
       --   some subset of them have are already in the mempool, we wouldn't
       --   want to bother asking for those specific transactions. Therefore,
       --   we would just insert those transaction IDs mapped to 'Nothing' to
       --   the 'bufferedObjects' such that those transactions are acknowledged,
       --   but never actually requested.
       --
       bufferedObjects            :: !(Map objectId (Maybe object)),

       -- | The number of transactions we can acknowledge on our next request
       -- for more transactions. The number here have already been removed from
       -- 'unacknowledgedObjectIds'.
       --
       numObjectsToAcknowledge    :: !Word16
     }
  deriving (Show, Generic)

instance ( NoThunks objectId
         , NoThunks object
         ) => NoThunks (ServerState objectId object)

initialServerState :: ServerState objectId object
initialServerState = ServerState 0 Seq.empty Map.empty Map.empty 0


objectDiffusionInbound
  :: forall objectId object index m.
     ( Ord objectId
     , NoThunks objectId
     , NoThunks object
     , MonadSTM m
     , MonadThrow m
     )
  => Tracer m (TraceObjectDiffusionInbound objectId object)
  -> NumObjectIdsToAck  -- ^ Maximum number of unacknowledged objectIds allowed
  -> MockMempoolReader objectId object index m
  -> MockMempoolWriter objectId object index m
  -> NodeToNodeVersion
  -> ObjectDiffusionServerPipelined objectId object m ()
objectDiffusionInbound tracer (NumObjectIdsToAck maxUnacked) mpReader mpWriter _version =
    ObjectDiffusionServerPipelined $ do
#ifdef OBJECTSUBMISSION_DELAY
      -- make the client linger before asking for object's and expending
      -- our resources as well, as he may disconnect for some reason
      threadDelay (fromMaybe (-1) longWait)
#endif
      continueWithStateM (serverIdle Zero) initialServerState
  where
    -- TODO #1656: replace these fixed limits by policies based on
    -- SizeInBytes and delta-Q and the bandwidth/delay product.
    -- These numbers are for demo purposes only, the throughput will be low.
    maxObjectIdsToRequest = 3 :: Word16
    maxObjectToRequest    = 2 :: Word16

    MockMempoolReader{mempoolGetSnapshot} = mpReader

    MockMempoolWriter
      { getObjectId
      , mempoolAddObjects
      } = mpWriter

    serverIdle :: forall (n :: N).
                  Nat n
               -> StatefulM (ServerState objectId object) n objectId object m
    serverIdle n = StatefulM $ \st -> case n of
        Zero -> do
          if canRequestMoreObjects st
          then do
            -- There are no replies in flight, but we do know some more objects we
            -- can ask for, so lets ask for them and more objectIds.
            traceWith tracer (TraceObjectInboundCanRequestMoreObjects (natToInt n))
            pure $ continueWithState (serverReqObjects Zero) st

          else do
            traceWith tracer (TraceObjectInboundCannotRequestMoreObjects (natToInt n))
            -- There's no replies in flight, and we have no more objects we can
            -- ask for so the only remaining thing to do is to ask for more
            -- objectIds. Since this is the only thing to do now, we make this a
            -- blocking call.
            let numObjectIdsToRequest = maxObjectIdsToRequest `min` maxUnacked
            assert (requestedObjectIdsInFlight st == 0
                  && Seq.null (unacknowledgedObjectIds st)
                  && Map.null (availableObjectids st)
                  && Map.null (bufferedObjects st)) $
              pure $
              SendMsgRequestObjectIdsBlocking
                (NumObjectIdsToAck (numObjectsToAcknowledge st))
                (NumObjectIdsToReq numObjectIdsToRequest)
                -- Our result if the client terminates the protocol
                (traceWith tracer TraceObjectInboundTerminated)
                ( collectAndContinueWithState (handleReply Zero) st {
                    numObjectsToAcknowledge    = 0,
                    requestedObjectIdsInFlight = numObjectIdsToRequest
                  }
                . CollectObjectIds (NumObjectIdsToReq numObjectIdsToRequest)
                . NonEmpty.toList)

        Succ n' -> if canRequestMoreObjects st
          then do
            -- We have replies in flight and we should eagerly collect them if
            -- available, but there are transactions to request too so we
            -- should not block waiting for replies.
            --
            -- Having requested more transactions, we opportunistically ask
            -- for more objectIds in a non-blocking way. This is how we pipeline
            -- asking for both objects and objectIds.
            --
            -- It's important not to pipeline more requests for objectIds when we
            -- have no objects to ask for, since (with no other guard) this will
            -- put us into a busy-polling loop.
            --
            traceWith tracer (TraceObjectInboundCanRequestMoreObjects (natToInt n))
            pure $ CollectPipelined
              (Just (continueWithState (serverReqObjects (Succ n')) st))
              (collectAndContinueWithState (handleReply n') st)

          else do
            traceWith tracer (TraceObjectInboundCannotRequestMoreObjects (natToInt n))
            -- In this case there is nothing else to do so we block until we
            -- collect a reply.
            pure $ CollectPipelined
              Nothing
              (collectAndContinueWithState (handleReply n') st)
      where
        canRequestMoreObjects :: ServerState k object -> Bool
        canRequestMoreObjects st =
            not (Map.null (availableObjectids st))

    handleReply :: forall (n :: N).
                   Nat n
                -> StatefulCollect (ServerState objectId object) n objectId object m
    handleReply n = StatefulCollect $ \st collect -> case collect of
      CollectObjectIds (NumObjectIdsToReq reqNo) objectIds -> do
        -- Check they didn't send more than we asked for. We don't need to
        -- check for a minimum: the blocking case checks for non-zero
        -- elsewhere, and for the non-blocking case it is quite normal for
        -- them to send us none.
        let objectIdsSeq = Seq.fromList (map fst objectIds)
            objectIdsMap = Map.fromList objectIds

        unless (Seq.length objectIdsSeq <= fromIntegral reqNo) $
          throwIO ProtocolErrorObjectIdsNotRequested

        -- Upon receiving a batch of new objectIds we extend our available set,
        -- and extended the unacknowledged sequence.
        --
        -- We also pre-emptively acknowledge those objectIds that are already in
        -- the mempool. This prevents us from requesting their corresponding
        -- transactions again in the future.
        let st' = st {
          requestedObjectIdsInFlight = requestedObjectIdsInFlight st - reqNo
        }
        mpSnapshot <- atomically mempoolGetSnapshot
        continueWithStateM
          (serverIdle n)
          (acknowledgeObjectIds st' objectIdsSeq objectIdsMap mpSnapshot)

      CollectObjects objectIds objects -> do
        -- To start with we have to verify that the objects they have sent us do
        -- correspond to the objects we asked for. This is slightly complicated by
        -- the fact that in general we get a subset of the objects that we asked
        -- for. We should never get a object we did not ask for. We take a strict
        -- approach to this and check it.
        --
        let objectsMap :: Map objectId object
            objectsMap = Map.fromList [ (getObjectId object, object) | object <- objects ]

            objectIdsReceived  = Map.keysSet objectsMap
            objectIdsRequested = Set.fromList objectIds

        unless (objectIdsReceived `Set.isSubsetOf` objectIdsRequested) $
          throwIO ProtocolErrorObjectNotRequested

            -- We can match up all the objectIds we requested, with those we
            -- received.
        let objectIdsRequestedWithObjectsReceived :: Map objectId (Maybe object)
            objectIdsRequestedWithObjectsReceived =
                Map.map Just objectsMap
             <> Map.fromSet (const Nothing) objectIdsRequested

            -- We still have to acknowledge the objectIds we were given. This
            -- combined with the fact that we request objects out of order means
            -- our bufferedObjects has to track all the objectIds we asked for, even
            -- though not all have replies.
            bufferedObjects1 = bufferedObjects st <> objectIdsRequestedWithObjectsReceived

            -- We have to update the unacknowledgedObjectIds here eagerly and not
            -- delay it to serverReqObjects, otherwise we could end up blocking in
            -- serverIdle on more pipelined results rather than being able to
            -- move on.

            -- Check if having received more objects we can now confirm any (in
            -- strict order in the unacknowledgedObjectIds sequence).
            (acknowledgedObjectIds, unacknowledgedObjectIds') =
              Seq.spanl (`Map.member` bufferedObjects1) (unacknowledgedObjectIds st)

            -- If so we can submit the acknowledged objects to our local mempool
            objectsReady = foldr (\objectId r -> maybe r (:r) (bufferedObjects1 Map.! objectId))
                             [] acknowledgedObjectIds

            -- And remove acknowledged objects from our buffer
            bufferedObjects2 = Foldable.foldl' (flip Map.delete)
                                   bufferedObjects1 acknowledgedObjectIds

            -- If we are acknowledging transactions that are still in
            -- unacknowledgedObjectIds' we need to re-add them so that we also can
            -- acknowledge them again later. This will happen in case of
            -- duplicate objectIds within the same window.
            live = filter (`elem` unacknowledgedObjectIds') $ toList acknowledgedObjectIds
            bufferedObjects3 = forceElemsToWHNF $ bufferedObjects2 <>
                               Map.fromList (zip live (repeat Nothing))

        let !collected = length objects
        traceWith tracer $
          TraceObjectDiffusionCollected collected

        objectIdsAccepted <- mempoolAddObjects objectsReady

        let !accepted = length objectIdsAccepted

        traceWith tracer $ TraceObjectDiffusionProcessed ProcessedObjectCount {
            pobjectcAccepted = accepted
          , pobjectcRejected = collected - accepted
          }

        continueWithStateM (serverIdle n) st {
          bufferedObjects         = bufferedObjects3,
          unacknowledgedObjectIds = unacknowledgedObjectIds',
          numObjectsToAcknowledge = numObjectsToAcknowledge st
                              + fromIntegral (Seq.length acknowledgedObjectIds)
        }

    -- Pre-emptively acknowledge those of the available transaction IDs that
    -- are already in the mempool and return the updated 'ServerState'.
    --
    -- This enables us to effectively filter out transactions that we don't
    -- need to bother requesting from the client since they're already in the
    -- mempool.
    --
    acknowledgeObjectIds :: ServerState objectId object
                     -> StrictSeq objectId
                     -> Map objectId SizeInBytes
                     -> MockMempoolSnapshot objectId object index
                     -> ServerState objectId object
    acknowledgeObjectIds st objectIdsSeq _ _ | Seq.null objectIdsSeq  = st
    acknowledgeObjectIds st objectIdsSeq objectIdsMap MockMempoolSnapshot{mempoolHasObject} =
        -- Return the next 'ServerState'
        st {
          availableObjectids      = availableObjectids',
          bufferedObjects         = bufferedObjects'',
          unacknowledgedObjectIds = unacknowledgedObjectIds'',
          numObjectsToAcknowledge = numObjectsToAcknowledge st
                              + fromIntegral (Seq.length acknowledgedObjectIds)
        }
      where

        -- Divide the new objectIds in two: those that are already in the
        -- mempool or in flight and those that are not. We'll request some objects from the
        -- latter.
        (ignoredObjectids, availableObjectidsMp) =
              Map.partitionWithKey
                (\objectId _ -> mempoolHasObject objectId)
                objectIdsMap

        availableObjectidsU =
              Map.filterWithKey
                (\objectId _ -> notElem objectId (unacknowledgedObjectIds st))
                objectIdsMap

        availableObjectids' = availableObjectids st <> Map.intersection availableObjectidsMp availableObjectidsU

        -- The objects that we intentionally don't request, because they are
        -- already in the mempool, need to be acknowledged.
        --
        -- So we extend bufferedObjects with those objects (so of course they have
        -- no corresponding reply).
        bufferedObjects' = bufferedObjects st
                    <> Map.map (const Nothing) ignoredObjectids

        unacknowledgedObjectIds' = unacknowledgedObjectIds st <> objectIdsSeq

        -- Check if having decided not to request more objects we can now
        -- confirm any objectIds (in strict order in the unacknowledgedObjectIds
        -- sequence). This is used in the 'numObjectsToAcknowledge' below
        -- which will then be used next time we SendMsgRequestObjectIds.
        --
        (acknowledgedObjectIds, unacknowledgedObjectIds'') =
          Seq.spanl (`Map.member` bufferedObjects') unacknowledgedObjectIds'


        -- If so we can remove acknowledged objects from our buffer provided that they
        -- are not still in unacknowledgedObjectIds''. This happens in case of duplicate
        -- objectIds.
        bufferedObjects'' = forceElemsToWHNF $ Foldable.foldl' (\m objectId -> if elem objectId unacknowledgedObjectIds''
                                              then m
                                              else Map.delete objectId m)
                                bufferedObjects' acknowledgedObjectIds

    serverReqObjects :: forall (n :: N).
                    Nat n
                 -> Stateful (ServerState objectId object) n objectId object m
    serverReqObjects n = Stateful $ \st -> do
        -- TODO: This implementation is deliberately naive, we pick in an
        -- arbitrary order and up to a fixed limit. This is to illustrate
        -- that we can request objects out of order. In the final version we will
        -- try to pick in-order and only pick out of order when we have to.
        -- We will also uses the size of objects in bytes as our limit for
        -- upper and lower watermarks for pipelining. We'll also use the
        -- amount in flight and delta-Q to estimate when we're in danger of
        -- becoming idle, and need to request stalled objects.
        --
        let (objectsToRequest, availableObjectids') =
              Map.splitAt (fromIntegral maxObjectToRequest) (availableObjectids st)

        SendMsgRequestObjectsPipelined
          (Map.keys objectsToRequest)
          (continueWithStateM (serverReqObjectIds (Succ n)) st {
             availableObjectids = availableObjectids'
           })

    serverReqObjectIds :: forall (n :: N).
                      Nat n
                   -> StatefulM (ServerState objectId object) n objectId object m
    serverReqObjectIds n = StatefulM $ \st -> do
          -- This definition is justified by the fact that the
          -- 'numObjectsToAcknowledge' are not included in the
          -- 'unacknowledgedObjectIds'.
      let numObjectIdsToRequest =
                  (maxUnacked
                    - fromIntegral (Seq.length (unacknowledgedObjectIds st))
                    - requestedObjectIdsInFlight st)
            `min` maxObjectIdsToRequest

      if numObjectIdsToRequest > 0
        then pure $ SendMsgRequestObjectIdsPipelined
          (NumObjectIdsToAck (numObjectsToAcknowledge st))
          (NumObjectIdsToReq numObjectIdsToRequest)
          (continueWithStateM (serverIdle (Succ n)) st {
                requestedObjectIdsInFlight = requestedObjectIdsInFlight st
                                       + numObjectIdsToRequest,
                numObjectsToAcknowledge    = 0
              })
        else continueWithStateM (serverIdle n) st

newtype Stateful s n objectId object m = Stateful (s -> ServerStIdle n objectId object m ())

newtype StatefulM s n objectId object m
  = StatefulM (s -> m (ServerStIdle n objectId object m ()))

newtype StatefulCollect s n objectId object m
  = StatefulCollect (s -> Collect objectId object -> m (ServerStIdle n objectId object m ()))

-- | After checking that there are no unexpected thunks in the provided state,
-- pass it to the provided function.
--
-- See 'checkInvariant' and 'unsafeNoThunks'.
continueWithState :: NoThunks s
                  => Stateful s n objectId object m
                  -> s
                  -> ServerStIdle n objectId object m ()
continueWithState (Stateful f) !st =
    checkInvariant (show <$> unsafeNoThunks st) (f st)

-- | A variant of 'continueWithState' to be more easily utilized with
-- 'serverIdle' and 'serverReqObjectIds'.
continueWithStateM :: NoThunks s
                   => StatefulM s n objectId object m
                   -> s
                   -> m (ServerStIdle n objectId object m ())
continueWithStateM (StatefulM f) !st =
    checkInvariant (show <$> unsafeNoThunks st) (f st)
{-# NOINLINE continueWithStateM #-}

-- | A variant of 'continueWithState' to be more easily utilized with
-- 'handleReply'.
collectAndContinueWithState :: NoThunks s
                            => StatefulCollect s n objectId object m
                            -> s
                            -> Collect objectId object
                            -> m (ServerStIdle n objectId object m ())
collectAndContinueWithState (StatefulCollect f) !st c =
    checkInvariant (show <$> unsafeNoThunks st) (f st c)
{-# NOINLINE collectAndContinueWithState #-}
