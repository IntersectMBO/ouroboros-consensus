{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Smoke tests for the object diffusion protocol. This uses a trivial object
-- pool and checks that a few objects can indeed be transferred from the
-- outbound to the inbound peer.
module Test.Consensus.MiniProtocol.ObjectDiffusion.Smoke
  ( tests
  , WithId (..)
  , ListWithUniqueIds (..)
  , ProtocolConstants
  , mockSystemTime
  , prop_smoke_object_diffusion
  , genSmokeObjectId
  , genSmokeObject
  , genListWithUniqueIds
  , genProtocolConstants
  , genRelativeTime
  , genWithArrivalTime
  , genPointTestBlock
  ) where

import Cardano.Network.NodeToNode.Version (NodeToNodeVersion (..))
import Control.Monad.IOSim (runSimStrictShutdown)
import Control.ResourceRegistry (forkLinkedThread, waitAnyThread, withRegistry)
import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Data (Typeable)
import Data.Functor.Contravariant (contramap)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Network.TypedProtocol.Channel (Channel, createConnectedChannels)
import Network.TypedProtocol.Codec (AnyMessage)
import Network.TypedProtocol.Driver.Simple (runPeer, runPipelinedPeer)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  , SystemTime (..)
  , WithArrivalTime (..)
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound
  ( objectDiffusionInbound
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
import Ouroboros.Network.Block (Point (..), SlotNo (SlotNo))
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Point (Block (Block), WithOrigin (..))
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
import Test.Util.TestBlock

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

genListWithUniqueIds :: (Ord idTy, WithId a idTy) => Gen a -> Gen (ListWithUniqueIds a idTy)
genListWithUniqueIds genObject = ListWithUniqueIds . nubOrdOn getId <$> listOf genObject

instance WithId SmokeObject SmokeObjectId where getId = getSmokeObjectId

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
  Main properties
-------------------------------------------------------------------------------}

-- Protocol constants

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
  Shared generators for Peras smoke tests
-------------------------------------------------------------------------------}

genRelativeTime :: Gen RelativeTime
genRelativeTime = RelativeTime . fromIntegral <$> arbitrary @Word64

genWithArrivalTime :: Gen a -> Gen (WithArrivalTime a)
genWithArrivalTime genA = WithArrivalTime <$> genRelativeTime <*> genA

genPointTestBlock :: Gen (Point TestBlock)
genPointTestBlock =
  -- Sometimes pick the genesis point
  frequency
    [ (1, pure $ Point Origin)
    ,
      ( 50
      , do
          slotNo <- SlotNo <$> arbitrary
          hash <- TestHash . NE.fromList . getNonEmpty <$> arbitrary
          pure $ Point (At (Block slotNo hash))
      )
    ]

-- | A static 'SystemTime' returning a constant time. The canonical mock
-- system time lives in 'Test.Util.LogicalClock.mockSystemTime', but it
-- is a field of 'LogicalClock' which requires a 'ResourceRegistry' and
-- a background tick thread — too heavyweight for simple property tests
-- that don't need time progression.
mockSystemTime :: Applicative m => SystemTime m
mockSystemTime =
  SystemTime
    { systemTimeCurrent = pure (RelativeTime 0)
    , systemTimeWait = pure ()
    }

prop_smoke :: Property
prop_smoke =
  forAll genProtocolConstants $ \protocolConstants ->
    forAll (genListWithUniqueIds genSmokeObject) $ \(ListWithUniqueIds objects) ->
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

          -- 'outboundThread' and 'inputThread' will run indefinitely, at least
          -- until we send the 'Terminate' control message through the
          -- 'controlMessageThread'.
          -- \* If 'inputThread' supports graceful termination, it will react to
          -- the 'Terminate' message in a timely manner, send 'MsgDone' to the
          -- 'outboundThread', and both threads should terminate shortly after
          -- (before the expiration of the second 'threadDelay' in
          -- 'controlMessageThread').
          -- \* If 'inputThread' does not support graceful termination (which is
          -- the case in the initial Peras implementation), it will probably be
          -- stuck waiting for a response to a blocking `ReqIds` request when no
          -- new data is available on the 'outboundThread' side. So the
          -- 'Terminate' message will have no effect, and 'controlMessageThread'
          -- will actually be the first thread to finish (with the expiration of
          -- the second 'threadDelay') after which we will finish the test by
          -- comparing received data.
          -- But this isn't really an issue, because the 'inputThread' blocks on
          -- `ReqIds` only when it is caught-up, i.e. when all possible data has
          -- already been transferred from the 'outboundThread'. So even without
          -- graceful termination, the test should still work as intended.
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
