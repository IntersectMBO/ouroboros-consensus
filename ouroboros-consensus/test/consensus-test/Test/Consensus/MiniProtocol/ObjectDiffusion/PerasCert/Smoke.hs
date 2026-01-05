{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.MiniProtocol.ObjectDiffusion.PerasCert.Smoke (tests) where

import Control.Tracer (contramap, nullTracer)
import Data.Functor.Identity (Identity (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Network.TypedProtocol.Driver.Simple (runPeer, runPipelinedPeer)
import Ouroboros.Consensus.Block.SupportsPeras
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  , SystemTime (..)
  , WithArrivalTime (..)
  , addArrivalTime
  , systemTimeCurrent
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.PerasCert
import Ouroboros.Consensus.Peras.Params (PerasParams (..), mkPerasParams)
import Ouroboros.Consensus.Peras.Round (PerasRoundNo (..))
import Ouroboros.Consensus.Storage.PerasCertDB.API
  ( AddPerasCertResult (..)
  , PerasCertDB
  , PerasCertTicketNo
  )
import qualified Ouroboros.Consensus.Storage.PerasCertDB.API as PerasCertDB
import qualified Ouroboros.Consensus.Storage.PerasCertDB.Impl as PerasCertDB
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , atomically
  , stateTVar
  , throwIO
  , uncheckedNewTVarM
  )
import Ouroboros.Network.Block (Point (..), SlotNo (SlotNo))
import Ouroboros.Network.Point (Block (Block), WithOrigin (..))
import Ouroboros.Network.Protocol.ObjectDiffusion.Codec
import Ouroboros.Network.Protocol.ObjectDiffusion.Inbound
  ( objectDiffusionInboundPeerPipelined
  )
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound (objectDiffusionOutboundPeer)
import System.Random (mkStdGen, uniform)
import Test.Consensus.MiniProtocol.ObjectDiffusion.Smoke
  ( ListWithUniqueIds (..)
  , WithId
  , genListWithUniqueIds
  , genProtocolConstants
  , getId
  , prop_smoke_object_diffusion
  )
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.TestBlock

tests :: TestTree
tests =
  testGroup
    "ObjectDiffusion.PerasCert.Smoke"
    [ testProperty
        "PerasCertDiffusion smoke test"
        prop_smoke
    ]

perasTestCfg :: PerasCfg TestBlock
perasTestCfg = mkPerasParams

genPoint :: Gen (Point (TestBlock))
genPoint =
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

genPerasCert :: Gen (PerasCert TestBlock)
genPerasCert = do
  tpcCertRound <- PerasRoundNo <$> arbitrary
  tpcCertBoostedBlock <- genPoint
  pure $
    TestPerasCert
      { tpcCertRound
      , tpcCertBoostedBlock
      }

instance WithId (PerasCert TestBlock) PerasRoundNo where
  getId = tpcCertRound

mockSystemTime :: IOLike m => Int -> m (SystemTime m)
mockSystemTime seed = do
  varGen <- uncheckedNewTVarM (mkStdGen seed)
  return $
    SystemTime
      { systemTimeCurrent =
          RelativeTime . realToFrac @Int <$> atomically (stateTVar varGen uniform)
      , systemTimeWait =
          pure ()
      }

newCertDB ::
  ( IOLike m
  , BlockSupportsPeras blk
  , PerasCfg blk ~ PerasParams
  ) =>
  PerasCfg blk ->
  SystemTime m ->
  [PerasCert blk] ->
  m (PerasCertDB m blk)
newCertDB perasParams systemTime certs = do
  db <- PerasCertDB.openDB (PerasCertDB.PerasCertDbArgs @Identity nullTracer)
  mapM_
    ( \cert -> do
        let validatedCert =
              ValidatedPerasCert
                { vpcCert = cert
                , vpcCertBoost = perasWeight perasParams
                }
        result <- PerasCertDB.addCert db =<< addArrivalTime systemTime validatedCert
        case result of
          AddedPerasCertToDB -> pure ()
          PerasCertAlreadyInDB -> throwIO (userError "Expected AddedPerasCertToDB, but cert was already in DB")
    )
    certs
  pure db

prop_smoke :: Property
prop_smoke =
  forAll genProtocolConstants $ \protocolConstants ->
    forAll (genListWithUniqueIds genPerasCert) $ \(ListWithUniqueIds certs) ->
      forAll arbitrary $ \systemTimeSeed ->
        let
          runOutboundPeer outbound outboundChannel tracer =
            runPeer
              ((\x -> "Outbound (Client): " ++ show x) `contramap` tracer)
              codecObjectDiffusionId
              outboundChannel
              (objectDiffusionOutboundPeer outbound)
              >> pure ()
          runInboundPeer inbound inboundChannel tracer =
            runPipelinedPeer
              ((\x -> "Inbound (Server): " ++ show x) `contramap` tracer)
              codecObjectDiffusionId
              inboundChannel
              (objectDiffusionInboundPeerPipelined inbound)
              >> pure ()
          mkPoolInterfaces ::
            forall m.
            IOLike m =>
            m
              ( ObjectPoolReader PerasRoundNo (PerasCert TestBlock) PerasCertTicketNo m
              , ObjectPoolWriter PerasRoundNo (PerasCert TestBlock) m
              , m [PerasCert TestBlock]
              )
          mkPoolInterfaces = do
            systemTime <- mockSystemTime systemTimeSeed
            outboundPool <- newCertDB perasTestCfg systemTime certs
            inboundPool <- newCertDB perasTestCfg systemTime []

            let outboundPoolReader = makePerasCertPoolReaderFromCertDB outboundPool
                inboundPoolWriter = makePerasCertPoolWriterFromCertDB perasTestCfg systemTime inboundPool
                getAllInboundPoolContent = atomically $ do
                  snap <- PerasCertDB.getCertSnapshot inboundPool
                  let rawContent =
                        Map.toAscList $
                          PerasCertDB.getCertsAfter snap (PerasCertDB.zeroPerasCertTicketNo)
                  pure $ vpcCert . forgetArrivalTime . snd <$> rawContent

            return (outboundPoolReader, inboundPoolWriter, getAllInboundPoolContent)
         in
          prop_smoke_object_diffusion
            protocolConstants
            certs
            runOutboundPeer
            runInboundPeer
            mkPoolInterfaces
