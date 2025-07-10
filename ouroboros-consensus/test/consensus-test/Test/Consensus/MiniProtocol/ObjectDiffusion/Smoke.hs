{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Smoke tests for the object diffusion protocol
--
module Test.Consensus.MiniProtocol.ObjectDiffusion.Smoke (tests) where

import Cardano.Crypto.DSIGN.Mock
import Cardano.Ledger.BaseTypes (nonZero, unNonZero)
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Class.MonadThrow (Handler (..), catches)
import Control.Monad.Class.MonadTime (MonadTime, getCurrentTime)
import Control.Monad.Class.MonadTimer (MonadTimer)
import Control.Monad.IOSim (runSimOrThrow)
import Control.ResourceRegistry
import Control.Tracer (contramap, contramapM, nullTracer)
import Data.DerivingVia (InstantiatedAt (InstantiatedAt))
import Data.List as List (foldl', intercalate)
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Semigroup (Max (Max), getMax)
import qualified Data.Set as Set
import Data.Time (NominalDiffTime, diffUTCTime)
import Data.Typeable
import GHC.Generics (Generic)
import Network.TypedProtocol.Channel
import Network.TypedProtocol.Driver.Simple
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.HeaderStateHistory
  ( HeaderStateHistory (..)
  )
import qualified Ouroboros.Consensus.HeaderStateHistory as HeaderStateHistory
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended hiding (ledgerState)
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( CSJConfig (..)
  , ChainDbView (..)
  , ChainSyncClientException
  , ChainSyncClientHandleCollection (..)
  , ChainSyncClientResult (..)
  , ChainSyncLoPBucketConfig (..)
  , ChainSyncState (..)
  , ChainSyncStateView (..)
  , ConfigEnv (..)
  , Consensus
  , DynamicEnv (..)
  , Our (..)
  , Their (..)
  , TraceChainSyncClientEvent (..)
  , bracketChainSyncClient
  , chainSyncClient
  , chainSyncStateFor
  , newChainSyncClientHandleCollection
  , viewChainSyncState
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.HistoricityCheck
  ( HistoricityCheck
  , HistoricityCutoff (..)
  )
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.HistoricityCheck as HistoricityCheck
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck
  ( ClockSkew
  , clockSkewInSeconds
  , unClockSkew
  )
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import Ouroboros.Consensus.Node.GsmState (GsmState (Syncing))
import Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( NodeToNodeVersion
  )
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.NodeId
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Consensus.Util (lastMaybe, whenJust)
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM
  ( Fingerprint (..)
  , WithFingerprint (..)
  )
import Ouroboros.Consensus.Util.Time
  ( multipleNominalDelay
  , nominalDelay
  )
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (getTipPoint)
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Mock.Chain (Chain (Genesis))
import qualified Ouroboros.Network.Mock.Chain as Chain
import Ouroboros.Network.Mock.ProducerState
  ( chainState
  , initChainProducerState
  )
import qualified Ouroboros.Network.Mock.ProducerState as CPS
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
import Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSyncId)
import Ouroboros.Network.Protocol.ChainSync.Examples
import Ouroboros.Network.Protocol.ChainSync.PipelineDecision
  ( pipelineDecisionLowHighMark
  )
import Ouroboros.Network.Protocol.ChainSync.Server
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import Quiet (Quiet (..))
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.ChainUpdates
  ( ChainUpdate (..)
  , UpdateBehavior (..)
  , genChainUpdates
  , toChainUpdates
  )
import Test.Util.Header (dropTimeFromFragment)
import Test.Util.LogicalClock (Tick (..))
import Test.Util.Orphans.Arbitrary ()
import Test.Util.Orphans.IOLike ()
import Test.Util.Schedule
  ( Schedule (..)
  , genSchedule
  , joinSchedule
  , lastTick
  , shrinkSchedule
  )
import Test.Util.TestBlock
import qualified Test.Util.TestBlock as TestBlock
import Test.Util.Tracer (recordingTracerTVar)

tests :: TestTree
tests =
  testGroup
    "ObjectDiffusion.Smoke"
    [ testProperty "smoke" prop_smoke
    ]

{-------------------------------------------------------------------------------
  Main property
-------------------------------------------------------------------------------}

prop_smoke :: Property
prop_smoke =
  ()

  where
    server =
      objectDiffusionInbound
        nullTracer
        numObjectIdsToAck
        mempoolReader
        mempoolWriter
        (NodeToNodeVersion 1)

    client =
      objectDiffusionOutbound
        nullTracer
        numObjectIdsToAck
        mempoolReader
        (NodeToNodeVersion 1)
        fixmeControlMessageStm

    numObjectIdsToAck = NumObjectIdsToAck 10

    mempoolReader = ()

    mempoolWriter = ()
