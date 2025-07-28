{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Smoke tests for the object diffusion protocol
module Test.Consensus.MiniProtocol.ObjectDiffusion.Smoke (tests) where

import Control.Monad (guard)
import Control.Monad.IOSim (runSimStrictShutdown)
import Control.Tracer (nullTracer, traceWith)
import Data.Functor.Contravariant (contramap)
import Data.List (foldl')
import Data.Sequence (fromList, (!?))
import Network.TypedProtocol.Channel (createConnectedChannels)
import Network.TypedProtocol.Driver.Simple (runPeer, runPipelinedPeer)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound
  ( ObjectPoolWriter (..)
  , objectDiffusionInbound
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPoolReader
  ( ObjectPoolReader (..)
  , ObjectPoolSnapshot (..)
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Outbound (objectDiffusionOutbound)
import Ouroboros.Consensus.Util.IOLike
  ( MonadAsync (..)
  , MonadDelay (..)
  , MonadSTM (..)
  , StrictTVar
  , modifyTVar
  , readTVar
  , readTVarIO
  , uncheckedNewTVarM
  , writeTVar
  )
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion (..))
import Ouroboros.Network.Protocol.ObjectDiffusion.Inbound (objectDiffusionClientInboundPeerPipelined)
import Ouroboros.Network.Protocol.ObjectDiffusion.Codec (codecObjectDiffusionId)
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound (objectDiffusionServerOutboundPeer)
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (NumObjectIdsToAck (..))
import Ouroboros.Network.SizeInBytes (SizeInBytes (SizeInBytes))
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.Orphans.Arbitrary ()
import Test.Util.Orphans.IOLike ()

tests :: TestTree
tests =
  testGroup
    "ObjectDiffusion.Smoke"
    [ testProperty "smoke" prop_smoke
    ]

{-------------------------------------------------------------------------------
  Mock objectPools
-------------------------------------------------------------------------------}

newtype SmokeObjectId = SmokeObjectId Int
  deriving (Eq, Ord, Show, NoThunks, Arbitrary)

newtype SmokeObject = SmokeObject {smokeObjectId :: SmokeObjectId}
  deriving (Eq, Ord, Show, NoThunks, Arbitrary)

newtype SmokeObjectPool m = SmokeObjectPool (StrictTVar m [SmokeObject])

newObjectPool :: MonadSTM m => [SmokeObject] -> m (SmokeObjectPool m)
newObjectPool pool = SmokeObjectPool <$> uncheckedNewTVarM pool

-- REVIEW: this is cheating; it would be better to get everything from the inbound' reader
everythingInPool :: MonadSTM m => SmokeObjectPool m -> m [SmokeObject]
everythingInPool (SmokeObjectPool pool) = readTVarIO pool

makeObjectPoolReader ::
  MonadSTM m => SmokeObjectPool m -> ObjectPoolReader SmokeObjectId SmokeObject Int m
makeObjectPoolReader (SmokeObjectPool pool) =
  ObjectPoolReader
    { objectPoolGetSnapshot = poolToSnapshot <$> readTVar pool
    , objectPoolZeroIndex = -1 -- objectPoolObjectIdsAfter is strict, and first index is 0.
    }

poolToSnapshot :: [SmokeObject] -> ObjectPoolSnapshot SmokeObjectId SmokeObject Int
poolToSnapshot pool =
  ObjectPoolSnapshot
    { objectPoolObjectIdsAfter = \minIndex -> do
        (index, SmokeObject{smokeObjectId}) <- zip [0 ..] pool
        guard (index > minIndex)
        return (smokeObjectId, index, SizeInBytes 0) -- REVIEW: 0?
    , objectPoolLookupObject = (fromList pool !?)
    , objectPoolHasObject = \objectId -> any ((== objectId) . smokeObjectId) pool
    }

makeObjectPoolWriter ::
  MonadSTM m => SmokeObjectPool m -> ObjectPoolWriter SmokeObjectId SmokeObject Int m
makeObjectPoolWriter (SmokeObjectPool pool) =
  ObjectPoolWriter
    { getObjectId = smokeObjectId
    , objectPoolAddObjects = \objects -> do
        atomically $ modifyTVar pool (++ objects)
        return $ map smokeObjectId objects
    }

{-------------------------------------------------------------------------------
  Main property
-------------------------------------------------------------------------------}

prop_smoke :: ListWithUniqueIds SmokeObject SmokeObjectId -> Property
prop_smoke (ListWithUniqueIds objects) =
  case simulationResult of
    Right inboundPoolContent -> inboundPoolContent === objects
    Left msg -> counterexample (show msg) $ property False
 where
  simulationResult = runSimStrictShutdown $ do
    traceWith nullTracer "========== [ Starting test ] =========="
    traceWith nullTracer (show objects)

    inboundPool <- newObjectPool []
    outboundPool <- newObjectPool objects

    controlMessage <- uncheckedNewTVarM Continue

    let
      server =
        objectDiffusionInbound
          nullTracer
          numObjectIdsToAck
          (makeObjectPoolReader inboundPool)
          (makeObjectPoolWriter inboundPool)
          nodeToNodeVersion

      client =
        objectDiffusionOutbound
          nullTracer
          numObjectIdsToAck
          (makeObjectPoolReader outboundPool)
          nodeToNodeVersion
          (readTVar controlMessage)

    (outboundChannel, inboundChannel) <- createConnectedChannels
    outboundAsync <-
      async $
        ( ()
            <$ runPeer
              ((\x -> "Outbound " ++ show x) `contramap` nullTracer)
              codecObjectDiffusionId
              outboundChannel
              (objectDiffusionServerOutboundPeer client)
        )

    inboundAsync <-
      async $
        ( ()
            <$ runPipelinedPeer
              ((\x -> "Inbound " ++ show x) `contramap` nullTracer)
              codecObjectDiffusionId
              inboundChannel
              (objectDiffusionClientInboundPeerPipelined server)
        )

    controlMessageAsync <- async $ do
      threadDelay 1000 -- give a head start to the other threads
      atomically $ writeTVar controlMessage Terminate
      threadDelay 1000 -- wait for the other threads to finish
    _ <- waitAnyCancel [outboundAsync, inboundAsync, controlMessageAsync]

    traceWith nullTracer "========== [ Test finished ] =========="

    everythingInPool inboundPool

  numObjectIdsToAck = NumObjectIdsToAck 10
  nodeToNodeVersion = NodeToNodeV_14

{-------------------------------------------------------------------------------
  Arbitrary lists with unique ids
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
        foldl'
          (\acc obj -> if any (\obj' -> getId obj == getId obj') acc then acc else obj : acc)
          []
          objects

instance WithId SmokeObject SmokeObjectId where getId = smokeObjectId
