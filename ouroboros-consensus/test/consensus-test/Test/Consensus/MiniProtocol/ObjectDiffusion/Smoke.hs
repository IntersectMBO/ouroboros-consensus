{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

-- | Smoke tests for the object diffusion protocol. This uses a trivial object
-- pool and checks that a few objects can indeed be transferred from the
-- outbound to the inbound peer.
module Test.Consensus.MiniProtocol.ObjectDiffusion.Smoke
  ( tests
  , prop_smoke_object_diffusion
  , ProtocolConstants
  , genProtocolConstants
  ) where

import Cardano.Network.NodeToNode.Version (NodeToNodeVersion (..))
import Control.Monad.Class.MonadTimer.SI (timeout)
import Control.Monad.IOSim (IOSim, runSimStrictShutdown)
import Control.ResourceRegistry (forkLinkedThread, withRegistry)
import Control.Tracer (Tracer, mkTracer, nullTracer, traceWith)
import Data.Data (Typeable)
import Data.Functor.Contravariant (contramap)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Network.TypedProtocol.Channel (Channel, createConnectedChannels)
import Network.TypedProtocol.Codec (AnyMessage)
import Network.TypedProtocol.Driver.Simple (runPeer, runPipelinedPeer)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound
  ( TraceObjectDiffusionInbound (TraceObjectDiffusionInboundServerIdle)
  , objectDiffusionInbound
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
  ( ObjectPoolReader (..)
  , ObjectPoolWriter (..)
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Outbound (objectDiffusionOutbound)
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , MonadDelay (..)
  , MonadSTM (..)
  , StrictTVar
  , modifyTVar
  , readTVar
  , uncheckedNewTVarM
  , writeTVar
  )
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Protocol.ObjectDiffusion.Codec (codecObjectDiffusionId)
import Ouroboros.Network.Protocol.ObjectDiffusion.Inbound
  ( ObjectDiffusionInboundPipelined
  , objectDiffusionInboundPeerPipelined
  )
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound
  ( ObjectDiffusionOutbound
  , objectDiffusionOutboundPeer
  )
import Ouroboros.Network.Protocol.ObjectDiffusion.Type
  ( NumObjectIdsReq (..)
  , NumObjectsReq (..)
  , NumObjectsUnacknowledged (..)
  , ObjectDiffusion
  )
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.Orphans.Arbitrary ()
import Test.Util.Orphans.IOLike ()
import Test.Util.Peras (ListWithUniqueIds (..), genListWithUniqueIds)

tests :: TestTree
tests =
  testGroup
    "ObjectDiffusion.Smoke"
    [ testProperty
        "ObjectDiffusion smoke test with mock objects"
        prop_smoke
    , testProperty
        "ObjectDiffusion delivers an object after an idle response"
        prop_object_after_idle
    , testProperty
        "ObjectDiffusion reports idle only after committing prior objects"
        prop_idle_after_commit
    ]

{-------------------------------------------------------------------------------
  Mock objectPools
-------------------------------------------------------------------------------}

newtype SmokeObjectId = SmokeObjectId Int
  deriving (Eq, Ord, Show, NoThunks)

newtype SmokeObject = SmokeObject {getSmokeObjectId :: SmokeObjectId}
  deriving (Eq, Ord, Show, NoThunks)

genSmokeObjectId :: Gen SmokeObjectId
genSmokeObjectId = SmokeObjectId <$> arbitrary

genSmokeObject :: Gen SmokeObject
genSmokeObject = SmokeObject <$> genSmokeObjectId

newtype SmokeObjectPool m = SmokeObjectPool (StrictTVar m [SmokeObject])

newObjectPool :: MonadSTM m => [SmokeObject] -> m (SmokeObjectPool m)
newObjectPool initialPoolContent = SmokeObjectPool <$> uncheckedNewTVarM initialPoolContent

makeObjectPoolReader ::
  MonadSTM m => SmokeObjectPool m -> ObjectPoolReader SmokeObjectId SmokeObject Int m
makeObjectPoolReader (SmokeObjectPool poolContentTvar) =
  ObjectPoolReader
    { oprObjectId = getSmokeObjectId
    , oprObjectsAfter = \minTicketNo limit -> do
        poolContent <- readTVar poolContentTvar
        let items =
              take (fromIntegral limit) $
                drop (minTicketNo + 1) $
                  zip [(0 :: Int) ..] poolContent
        if null items
          then pure Nothing
          else pure $ Just $ pure $ Map.fromList items
    , oprZeroTicketNo = -1 :: Int -- objectPoolObjectIdsAfter uses strict comparison, and first ticketNo is 0.
    }

makeObjectPoolWriter ::
  MonadSTM m => SmokeObjectPool m -> ObjectPoolWriter SmokeObjectId SmokeObject m
makeObjectPoolWriter (SmokeObjectPool poolContentTvar) =
  ObjectPoolWriter
    { opwObjectId = getSmokeObjectId
    , opwAddObjects = \objects -> do
        atomically $ modifyTVar poolContentTvar (++ objects)
        return ()
    , opwHasObject = do
        poolContent <- readTVar poolContentTvar
        pure $ \objectId -> any (\obj -> getSmokeObjectId obj == objectId) poolContent
    }

mkMockPoolInterfaces ::
  MonadSTM m =>
  [SmokeObject] ->
  m
    ( ObjectPoolReader SmokeObjectId SmokeObject Int m
    , ObjectPoolWriter SmokeObjectId SmokeObject m
    , m [SmokeObject]
    )
mkMockPoolInterfaces objects = do
  outboundPool <- newObjectPool objects
  inboundPool@(SmokeObjectPool tvar) <- newObjectPool []

  let outboundPoolReader = makeObjectPoolReader outboundPool
      inboundPoolWriter = makeObjectPoolWriter inboundPool

  return (outboundPoolReader, inboundPoolWriter, atomically $ readTVar tvar)

{-------------------------------------------------------------------------------
  Protocol constants
-------------------------------------------------------------------------------}

newtype ProtocolConstants
  = ProtocolConstants (NumObjectsUnacknowledged, NumObjectIdsReq, NumObjectsReq)
  deriving Show

genProtocolConstants :: Gen ProtocolConstants
genProtocolConstants = do
  maxFifoSize <- choose (5, 20)
  maxIdsToReq <- choose (3, maxFifoSize)
  maxObjectsToReq <- choose (2, maxIdsToReq)
  pure $
    ProtocolConstants
      ( NumObjectsUnacknowledged maxFifoSize
      , NumObjectIdsReq maxIdsToReq
      , NumObjectsReq maxObjectsToReq
      )

nodeToNodeVersion :: NodeToNodeVersion
nodeToNodeVersion = NodeToNodeV_14

{-------------------------------------------------------------------------------
  Main properties
-------------------------------------------------------------------------------}

prop_smoke :: Property
prop_smoke =
  forAll genProtocolConstants $ \protocolConstants ->
    forAll (genListWithUniqueIds getSmokeObjectId genSmokeObject) $
      \(ListWithUniqueIds objects) ->
        prop_smoke_object_diffusion
          protocolConstants
          objects
          runOutboundPeer
          runInboundPeer
          (mkMockPoolInterfaces objects)
 where
  runOutboundPeer outbound outboundChannel tracer =
    runPeer
      ((\x -> "Outbound (Server): " ++ show x) `contramap` tracer)
      codecObjectDiffusionId
      outboundChannel
      (objectDiffusionOutboundPeer outbound)
      >> pure ()

  runInboundPeer inbound inboundChannel tracer =
    runPipelinedPeer
      ((\x -> "Inbound (Client): " ++ show x) `contramap` tracer)
      codecObjectDiffusionId
      inboundChannel
      (objectDiffusionInboundPeerPipelined inbound)
      >> pure ()

-- | Exercise the important distinction from client-side polling: once the
-- server has returned an idle response, the client immediately starts another
-- blocking request. An object added between idle responses must wake that
-- request immediately rather than waiting for another client-side delay.
prop_object_after_idle :: Property
prop_object_after_idle =
  case runSimStrictShutdown simulation of
    Right (mIdle, mDelivered, mTerminated, inboundObjects) ->
      counterexample "the server did not return client agency while idle" (isJust mIdle)
        .&&. counterexample "object added after idle was not delivered promptly" (isJust mDelivered)
        .&&. counterexample "peers did not terminate after delivery" (isJust mTerminated)
        .&&. inboundObjects === [object]
    Left err -> counterexample (show err) $ property False
 where
  object = SmokeObject (SmokeObjectId 42)

  simulation :: forall s. IOSim s (Maybe (), Maybe (), Maybe (), [SmokeObject])
  simulation = do
    let maxFifoSize = NumObjectsUnacknowledged 5
        maxIdsToReq = NumObjectIdsReq 3
        maxObjectsToReq = NumObjectsReq 2

    outboundPool@(SmokeObjectPool outboundObjectsVar) <- newObjectPool []
    inboundPool@(SmokeObjectPool inboundObjectsVar) <- newObjectPool []
    controlMessage <- uncheckedNewTVarM Continue
    idleSeen <- uncheckedNewTVarM False

    let inboundTracer = mkTracer $ \event -> case event of
          TraceObjectDiffusionInboundServerIdle ->
            atomically $ writeTVar idleSeen True
          _ -> pure ()
        inbound =
          objectDiffusionInbound
            inboundTracer
            (maxFifoSize, maxIdsToReq, maxObjectsToReq)
            (makeObjectPoolWriter inboundPool)
            nodeToNodeVersion
            (readTVar controlMessage)
        outbound =
          objectDiffusionOutbound
            nullTracer
            maxFifoSize
            1
            (makeObjectPoolReader outboundPool)
            nodeToNodeVersion

    withRegistry $ \reg -> do
      (outboundChannel, inboundChannel) <- createConnectedChannels
      peersDone <- uncheckedNewTVarM (0 :: Int)
      let trackDone action = do
            _ <- action
            atomically $ modifyTVar peersDone (+ 1)

      _outboundThread <-
        forkLinkedThread reg "ObjectDiffusion post-idle outbound peer" $
          trackDone $
            runPeer
              nullTracer
              codecObjectDiffusionId
              outboundChannel
              (objectDiffusionOutboundPeer outbound)
      _inboundThread <-
        forkLinkedThread reg "ObjectDiffusion post-idle inbound peer" $
          trackDone $
            runPipelinedPeer
              nullTracer
              codecObjectDiffusionId
              inboundChannel
              (objectDiffusionInboundPeerPipelined inbound)

      mIdle <- timeout 1.25 $ atomically $ readTVar idleSeen >>= check

      -- Add an object after the idle response. A polling client that slept for
      -- another idle interval would miss the deliberately shorter deadline;
      -- the immediately reissued blocking request is woken at once.
      atomically $ modifyTVar outboundObjectsVar (++ [object])

      mDelivered <- timeout 0.25 $ atomically $ do
        inboundObjects <- readTVar inboundObjectsVar
        check (inboundObjects == [object])

      atomically $ writeTVar controlMessage Terminate
      mTerminated <- timeout 3 $ atomically $ do
        n <- readTVar peersDone
        check (n == 2)

      inboundObjects <- atomically $ readTVar inboundObjectsVar
      pure (mIdle, mDelivered, mTerminated, inboundObjects)

-- | Receiving 'MsgServerIdle' is a synchronization point for the inbound
-- peer: every object advertised before it must already have been committed to
-- the local pool. Hold a commit open and verify that no idle event can overtake
-- it.
prop_idle_after_commit :: Property
prop_idle_after_commit =
  case runSimStrictShutdown simulation of
    Right (mCommitStarted, mEarlyIdle, mCommitted, mIdle, mTerminated, inboundObjects) ->
      counterexample "the inbound peer never started committing the object" (isJust mCommitStarted)
        .&&. counterexample
          "the inbound peer reported idle before the object commit completed"
          (not $ isJust mEarlyIdle)
        .&&. counterexample "the inbound peer did not commit the object" (isJust mCommitted)
        .&&. counterexample "the server did not report idle after the commit" (isJust mIdle)
        .&&. counterexample "peers did not terminate after the idle response" (isJust mTerminated)
        .&&. inboundObjects === [object]
    Left err -> counterexample (show err) $ property False
 where
  object = SmokeObject (SmokeObjectId 42)

  simulation :: forall s. IOSim s (Maybe (), Maybe (), Maybe (), Maybe (), Maybe (), [SmokeObject])
  simulation = do
    let maxFifoSize = NumObjectsUnacknowledged 5
        maxIdsToReq = NumObjectIdsReq 3
        maxObjectsToReq = NumObjectsReq 2

    outboundPool <- newObjectPool [object]
    SmokeObjectPool inboundObjectsVar <- newObjectPool []
    controlMessage <- uncheckedNewTVarM Continue
    commitStarted <- uncheckedNewTVarM False
    allowCommit <- uncheckedNewTVarM False
    idleSeen <- uncheckedNewTVarM False

    let inboundTracer = mkTracer $ \event -> case event of
          TraceObjectDiffusionInboundServerIdle ->
            atomically $ writeTVar idleSeen True
          _ -> pure ()
        inboundWriter =
          ObjectPoolWriter
            { opwObjectId = getSmokeObjectId
            , opwAddObjects = \objects -> do
                atomically $ writeTVar commitStarted True
                atomically $ readTVar allowCommit >>= check
                atomically $ modifyTVar inboundObjectsVar (++ objects)
            , opwHasObject = do
                inboundObjects <- readTVar inboundObjectsVar
                pure $ \objectId ->
                  any ((== objectId) . getSmokeObjectId) inboundObjects
            }
        inbound =
          objectDiffusionInbound
            inboundTracer
            (maxFifoSize, maxIdsToReq, maxObjectsToReq)
            inboundWriter
            nodeToNodeVersion
            (readTVar controlMessage)
        outbound =
          objectDiffusionOutbound
            nullTracer
            maxFifoSize
            1
            (makeObjectPoolReader outboundPool)
            nodeToNodeVersion

    withRegistry $ \reg -> do
      (outboundChannel, inboundChannel) <- createConnectedChannels
      peersDone <- uncheckedNewTVarM (0 :: Int)
      let trackDone action = do
            _ <- action
            atomically $ modifyTVar peersDone (+ 1)

      _outboundThread <-
        forkLinkedThread reg "ObjectDiffusion commit-order outbound peer" $
          trackDone $
            runPeer
              nullTracer
              codecObjectDiffusionId
              outboundChannel
              (objectDiffusionOutboundPeer outbound)
      _inboundThread <-
        forkLinkedThread reg "ObjectDiffusion commit-order inbound peer" $
          trackDone $
            runPipelinedPeer
              nullTracer
              codecObjectDiffusionId
              inboundChannel
              (objectDiffusionInboundPeerPipelined inbound)

      mCommitStarted <- timeout 1 $ atomically $ readTVar commitStarted >>= check
      mEarlyIdle <- timeout 1.25 $ atomically $ readTVar idleSeen >>= check

      atomically $ writeTVar allowCommit True
      mCommitted <- timeout 0.25 $ atomically $ do
        inboundObjects <- readTVar inboundObjectsVar
        check (inboundObjects == [object])
      mIdle <- timeout 1.25 $ atomically $ readTVar idleSeen >>= check

      atomically $ writeTVar controlMessage Terminate
      mTerminated <- timeout 3 $ atomically $ do
        n <- readTVar peersDone
        check (n == 2)

      inboundObjects <- atomically $ readTVar inboundObjectsVar
      pure (mCommitStarted, mEarlyIdle, mCommitted, mIdle, mTerminated, inboundObjects)

--- The core logic of the smoke test is shared between the generic smoke tests for ObjectDiffusion, and the ones specialised to PerasCert/PerasVote diffusion
prop_smoke_object_diffusion ::
  ( Eq object
  , Show object
  , Ord objectId
  , Typeable objectId
  , Typeable object
  , NoThunks objectId
  , Show objectId
  , NoThunks object
  ) =>
  ProtocolConstants ->
  [object] ->
  ( forall m.
    IOLike m =>
    ObjectDiffusionOutbound objectId object m () ->
    Channel m (AnyMessage (ObjectDiffusion objectId object)) ->
    (Tracer m String) ->
    m ()
  ) ->
  ( forall m.
    IOLike m =>
    ObjectDiffusionInboundPipelined objectId object m () ->
    (Channel m (AnyMessage (ObjectDiffusion objectId object))) ->
    (Tracer m String) ->
    m ()
  ) ->
  ( forall m.
    IOLike m =>
    m
      ( ObjectPoolReader objectId object ticketNo m
      , ObjectPoolWriter objectId object m
      , m [object]
      )
  ) ->
  Property
prop_smoke_object_diffusion
  (ProtocolConstants (maxFifoSize, maxIdsToReq, maxObjectsToReq))
  objects
  runOutboundPeer
  runInboundPeer
  mkPoolInterfaces =
    let
      simulationResult = runSimStrictShutdown $ do
        let tracer = nullTracer

        traceWith tracer "========== [ Starting ObjectDiffusion smoke test ] =========="
        traceWith tracer (show objects)

        (outboundPoolReader, inboundPoolWriter, getAllInboundPoolContent) <- mkPoolInterfaces
        controlMessage <- uncheckedNewTVarM Continue

        let
          inbound =
            objectDiffusionInbound
              tracer
              ( maxFifoSize
              , maxIdsToReq
              , maxObjectsToReq
              )
              inboundPoolWriter
              nodeToNodeVersion
              (readTVar controlMessage)

          outbound =
            objectDiffusionOutbound
              tracer
              maxFifoSize
              1
              outboundPoolReader
              nodeToNodeVersion

        mTerminated <- withRegistry $ \reg -> do
          (outboundChannel, inboundChannel) <- createConnectedChannels
          peersDone <- uncheckedNewTVarM (0 :: Int)
          let trackDone action = do
                _ <- action
                atomically $ modifyTVar peersDone (+ 1)
          _outboundThread <-
            forkLinkedThread reg "ObjectDiffusion Outbound peer thread" $
              trackDone $
                runOutboundPeer outbound outboundChannel tracer
          _inboundThread <-
            forkLinkedThread reg "ObjectDiffusion Inbound peer thread" $
              trackDone $
                runInboundPeer inbound inboundChannel tracer
          _controlMessageThread <- forkLinkedThread reg "ObjectDiffusion Control thread" $ do
            threadDelay 1 -- give the peers time to transfer all initial objects
            atomically $ writeTVar controlMessage Terminate

          -- Once caught up, the server returns agency at least once per idle
          -- interval. The client must then observe 'Terminate', send 'MsgDone',
          -- and let both peer threads finish. The timeout covers the one-second
          -- head start plus one one-second idle interval and scheduling margin.
          timeout 3 $ atomically $ do
            n <- readTVar peersDone
            check (n == 2)

        traceWith tracer "========== [ ObjectDiffusion smoke test finished ] =========="
        poolContent <- getAllInboundPoolContent

        traceWith tracer "inboundPoolContent:"
        traceWith tracer (show poolContent)
        traceWith tracer "========== ======================================= =========="
        pure (mTerminated, poolContent)
     in
      case simulationResult of
        Right (mTerminated, inboundPoolContent) ->
          counterexample
            "peers did not terminate after the Terminate control message"
            (isJust mTerminated)
            .&&. inboundPoolContent === objects
        Left msg -> counterexample (show msg) $ property False
