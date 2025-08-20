{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Smoke tests for the object diffusion protocol
module Test.Consensus.MiniProtocol.ObjectDiffusion.Smoke (tests, WithId (..), ListWithUniqueIds (..), prop_smoke_object_diffusion) where

import Control.Monad.IOSim (IOSim, runSimStrictShutdown)
import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Functor.Contravariant (contramap)
import Network.TypedProtocol.Channel (Channel, createConnectedChannels)
import Network.TypedProtocol.Codec (AnyMessage)
import Network.TypedProtocol.Driver.Simple (runPeer, runPipelinedPeer)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound
  ( objectDiffusionInbound
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
  ( ObjectPoolReader (..)
  , ObjectPoolWriter (..)
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Outbound (objectDiffusionOutbound)
import Ouroboros.Consensus.Util.IOLike
  ( MonadAsync (..)
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
  , objectDiffusionInboundClientPeerPipelined
  , objectDiffusionInboundServerPeerPipelined
  )
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound
  ( ObjectDiffusionOutbound
  , objectDiffusionOutboundClientPeer
  , objectDiffusionOutboundServerPeer
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
        "ObjectDiffusion smoke test with mock objects (client inbound, server outbound)"
        prop_smoke_init_inbound
    , testProperty
        "ObjectDiffusion smoke test with mock objects (client outbound, server inbound)"
        prop_smoke_init_outbound
    ]

{-------------------------------------------------------------------------------
  Provides a way to generate lists composed of objects with no duplicate ids,
  with an Arbitrary instance
-------------------------------------------------------------------------------}

class WithId a idTy | a -> idTy where
  getId :: a -> idTy

newtype ListWithUniqueIds a idTy = ListWithUniqueIds [a]
  deriving (Eq, Show, Ord)

instance (Eq idTy, WithId a idTy, Arbitrary a) => Arbitrary (ListWithUniqueIds a idTy) where
  arbitrary =
    ListWithUniqueIds <$> do
      (objects :: [a]) <- arbitrary
      return $
        foldr
          (\obj acc -> if any (\obj' -> getId obj == getId obj') acc then acc else obj : acc)
          []
          objects

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
        poolContent <- atomically $ readTVar poolContentTvar
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

maxFifoSize :: NumObjectsOutstanding
maxFifoSize = NumObjectsOutstanding 10

maxIdsToReq :: NumObjectIdsReq
maxIdsToReq = NumObjectIdsReq 3

maxObjectsToReq :: NumObjectsReq
maxObjectsToReq = NumObjectsReq 2

nodeToNodeVersion :: NodeToNodeVersion
nodeToNodeVersion = NodeToNodeV_14

prop_smoke_init_inbound :: ListWithUniqueIds SmokeObject idTy -> Property
prop_smoke_init_inbound (ListWithUniqueIds objects) =
  prop_smoke_object_diffusion objects mkOutboundAsync mkInboundAsync (mkMockPoolInterfaces objects)
 where
  mkOutboundAsync outbound outboundChannel tracer =
    async $
      ( ()
          <$ runPeer
            ((\x -> "Outbound (Server): " ++ show x) `contramap` tracer)
            codecObjectDiffusionId
            outboundChannel
            (objectDiffusionOutboundServerPeer outbound)
      )

  mkInboundAsync inbound inboundChannel tracer =
    async $
      ( ()
          <$ runPipelinedPeer
            ((\x -> "Inbound (Client): " ++ show x) `contramap` tracer)
            codecObjectDiffusionId
            inboundChannel
            (objectDiffusionInboundClientPeerPipelined inbound)
      )

prop_smoke_init_outbound :: ListWithUniqueIds SmokeObject SmokeObjectId -> Property
prop_smoke_init_outbound (ListWithUniqueIds objects) =
  prop_smoke_object_diffusion objects mkOutboundAsync mkInboundAsync (mkMockPoolInterfaces objects)
 where
  mkOutboundAsync outbound outboundChannel tracer =
    async $
      ( ()
          <$ runPeer
            ((\x -> "Outbound (Client): " ++ show x) `contramap` tracer)
            codecObjectDiffusionId
            outboundChannel
            (objectDiffusionOutboundClientPeer outbound)
      )

  mkInboundAsync inbound inboundChannel tracer =
    async $
      ( ()
          <$ runPipelinedPeer
            ((\x -> "Inbound (Server): " ++ show x) `contramap` tracer)
            codecObjectDiffusionId
            inboundChannel
            (objectDiffusionInboundServerPeerPipelined inbound)
      )

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
  [object] ->
  ( forall s.
    ObjectDiffusionOutbound objectId object (IOSim s) () ->
    ( Channel
        (IOSim s)
        ( AnyMessage
            (ObjectDiffusion initAgency objectId object)
        )
    ) ->
    (Tracer (IOSim s) String) ->
    IOSim s (Async (IOSim s) ())
  ) ->
  ( forall s.
    ObjectDiffusionInboundPipelined
      objectId
      object
      (IOSim s)
      () ->
    ( Channel
        (IOSim s)
        ( AnyMessage
            (ObjectDiffusion initAgency objectId object)
        )
    ) ->
    (Tracer (IOSim s) String) ->
    IOSim s (Async (IOSim s) ())
  ) ->
  ( forall s.
    IOSim
      s
      ( ObjectPoolReader objectId object ticketNo (IOSim s)
      , ObjectPoolWriter objectId object (IOSim s)
      , (IOSim s) [object]
      )
  ) ->
  Property
prop_smoke_object_diffusion objects mkOutboundAsync mkInboundAsync mkPoolInterfaces =
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

        outbound =
          objectDiffusionOutbound
            tracer
            maxFifoSize
            outboundPoolReader
            nodeToNodeVersion
            (readTVar controlMessage)

      (outboundChannel, inboundChannel) <- createConnectedChannels
      outboundAsync <- mkOutboundAsync outbound outboundChannel tracer
      inboundAsync <- mkInboundAsync inbound inboundChannel tracer

      controlMessageAsync <- async $ do
        threadDelay 1000 -- give a head start to the other threads
        atomically $ writeTVar controlMessage Terminate
        threadDelay 1000 -- wait for the other threads to finish
      _ <- waitAnyCancel [outboundAsync, inboundAsync, controlMessageAsync]

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
