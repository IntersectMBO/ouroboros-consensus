{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Smoke tests for the object diffusion protocol. This uses a trivial object
-- pool and checks that a few objects can indeed be transferred from the
-- outbound to the inbound peer.
module Test.Consensus.MiniProtocol.ObjectDiffusion.Smoke
  ( tests
  , WithId (..)
  , ListWithUniqueIds (..)
  , ProtocolConstants
  , prop_smoke_object_diffusion
  ) where

import Control.Monad.IOSim (runSimStrictShutdown)
import Control.ResourceRegistry (forkLinkedThread, waitAnyThread, withRegistry)
import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Functor.Contravariant (contramap)
import Network.TypedProtocol.Channel (Channel, createConnectedChannels)
import Network.TypedProtocol.Codec (AnyMessage)
import Network.TypedProtocol.Driver.Simple (runPeer, runPipelinedPeer)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V1
  ( objectDiffusionInbound
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V1.State
  ( ObjectDiffusionInboundStateView (..)
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
  ( ObjectPoolReader (..)
  , ObjectPoolWriter (..)
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Outbound (objectDiffusionOutbound)
import qualified Ouroboros.Consensus.MiniProtocol.Util.Idling as Idling
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
import Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion (..))
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
  , NumObjectsOutstanding (..)
  , NumObjectsReq (..)
  , ObjectDiffusion
  )
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.Orphans.Arbitrary ()
import Test.Util.Orphans.IOLike ()

tests :: TestTree
tests =
  testGroup
    "ObjectDiffusion.Smoke"
    [ testProperty
        "ObjectDiffusion smoke test with mock objects"
        prop_smoke
    ]

{-------------------------------------------------------------------------------
  Provides a way to generate lists composed of objects with no duplicate ids,
  with an Arbitrary instance
-------------------------------------------------------------------------------}

class WithId a idTy | a -> idTy where
  getId :: a -> idTy

newtype ListWithUniqueIds a idTy = ListWithUniqueIds [a]
  deriving (Eq, Show, Ord)

instance (Ord idTy, WithId a idTy, Arbitrary a) => Arbitrary (ListWithUniqueIds a idTy) where
  arbitrary = ListWithUniqueIds . nubOrdOn getId <$> arbitrary

instance WithId SmokeObject SmokeObjectId where getId = getSmokeObjectId

{-------------------------------------------------------------------------------
  Mock objectPools
-------------------------------------------------------------------------------}

newtype SmokeObjectId = SmokeObjectId Int
  deriving (Eq, Ord, Show, NoThunks, Arbitrary)

newtype SmokeObject = SmokeObject {getSmokeObjectId :: SmokeObjectId}
  deriving (Eq, Ord, Show, NoThunks, Arbitrary)

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
        pure $
          take (fromIntegral limit) $
            drop (minTicketNo + 1) $
              ( (\(ticketNo, smokeObject) -> (ticketNo, getSmokeObjectId smokeObject, pure smokeObject))
                  <$> zip [(0 :: Int) ..] poolContent
              )
    , oprZeroTicketNo = -1 -- objectPoolObjectIdsAfter uses strict comparison, and first ticketNo is 0.
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
  Main properties
-------------------------------------------------------------------------------}

-- Protocol constants

newtype ProtocolConstants
  = ProtocolConstants (NumObjectsOutstanding, NumObjectIdsReq, NumObjectsReq)
  deriving Show

instance Arbitrary ProtocolConstants where
  arbitrary = do
    maxFifoSize <- choose (5, 20)
    maxIdsToReq <- choose (3, maxFifoSize)
    maxObjectsToReq <- choose (2, maxIdsToReq)
    pure $
      ProtocolConstants
        ( NumObjectsOutstanding maxFifoSize
        , NumObjectIdsReq maxIdsToReq
        , NumObjectsReq maxObjectsToReq
        )

nodeToNodeVersion :: NodeToNodeVersion
nodeToNodeVersion = NodeToNodeV_14

prop_smoke :: ProtocolConstants -> ListWithUniqueIds SmokeObject idTy -> Property
prop_smoke protocolConstants (ListWithUniqueIds objects) =
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

--- The core logic of the smoke test is shared between the generic smoke tests for ObjectDiffusion, and the ones specialised to PerasCert/PerasVote diffusion
prop_smoke_object_diffusion ::
  ( Eq object
  , Show object
  , Ord objectId
  , NoThunks objectId
  , Show objectId
  , NoThunks object
  , Ord ticketNo
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
          inboundState =
            ObjectDiffusionInboundStateView
              { odisvIdling = Idling.noIdling
              }

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
              inboundState

          outbound =
            objectDiffusionOutbound
              tracer
              maxFifoSize
              outboundPoolReader
              nodeToNodeVersion

        withRegistry $ \reg -> do
          (outboundChannel, inboundChannel) <- createConnectedChannels
          outboundThread <-
            forkLinkedThread reg "ObjectDiffusion Outbound peer thread" $
              runOutboundPeer outbound outboundChannel tracer
          inboundThread <-
            forkLinkedThread reg "ObjectDiffusion Inbound peer thread" $
              runInboundPeer inbound inboundChannel tracer
          controlMessageThread <- forkLinkedThread reg "ObjectDiffusion Control thread" $ do
            threadDelay 1000 -- give a head start to the other threads
            atomically $ writeTVar controlMessage Terminate
            threadDelay 1000 -- wait for the other threads to finish
          waitAnyThread [outboundThread, inboundThread, controlMessageThread]

        traceWith tracer "========== [ ObjectDiffusion smoke test finished ] =========="
        poolContent <- getAllInboundPoolContent

        traceWith tracer "inboundPoolContent:"
        traceWith tracer (show poolContent)
        traceWith tracer "========== ======================================= =========="
        pure poolContent
     in
      case simulationResult of
        Right inboundPoolContent -> inboundPoolContent === objects
        Left msg -> counterexample (show msg) $ property False
