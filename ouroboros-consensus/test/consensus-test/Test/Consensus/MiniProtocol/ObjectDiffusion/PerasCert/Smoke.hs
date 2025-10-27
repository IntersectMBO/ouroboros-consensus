{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.MiniProtocol.ObjectDiffusion.PerasCert.Smoke (tests) where

import Control.Tracer (contramap, nullTracer)
import Data.Functor.Identity (Identity (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Network.TypedProtocol.Driver.Simple (runPeer, runPipelinedPeer)
import Ouroboros.Consensus.Block.SupportsPeras
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.PerasCert
import Ouroboros.Consensus.Storage.PerasCertDB.API
  ( AddPerasCertResult (..)
  , PerasCertDB
  , PerasCertTicketNo
  )
import qualified Ouroboros.Consensus.Storage.PerasCertDB.API as PerasCertDB
import qualified Ouroboros.Consensus.Storage.PerasCertDB.Impl as PerasCertDB
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Block (Point (..), SlotNo (SlotNo), StandardHash)
import Ouroboros.Network.Point (Block (Block), WithOrigin (..))
import Ouroboros.Network.Protocol.ObjectDiffusion.Codec
import Ouroboros.Network.Protocol.ObjectDiffusion.Inbound
  ( objectDiffusionInboundPeerPipelined
  )
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound (objectDiffusionOutboundPeer)
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
  pcCertRound <- PerasRoundNo <$> arbitrary
  pcCertBoostedBlock <- genPoint
  pure $ PerasCert{pcCertRound, pcCertBoostedBlock}

instance WithId (PerasCert blk) PerasRoundNo where
  getId = pcCertRound

newCertDB ::
  (IOLike m, StandardHash blk) =>
  PerasCfg blk ->
  [PerasCert blk] ->
  m (PerasCertDB m blk)
newCertDB perasParams certs = do
  db <- PerasCertDB.openDB (PerasCertDB.PerasCertDbArgs @Identity nullTracer)
  mapM_
    ( \cert -> do
        let validatedCert =
              ValidatedPerasCert
                { vpcCert = cert
                , vpcCertBoost = perasWeight perasParams
                }
        result <- PerasCertDB.addCert db validatedCert
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
          outboundPool <- newCertDB perasTestCfg certs
          inboundPool <- newCertDB perasTestCfg []

          let outboundPoolReader = makePerasCertPoolReaderFromCertDB outboundPool
              inboundPoolWriter = makePerasCertPoolWriterFromCertDB inboundPool
              getAllInboundPoolContent = atomically $ do
                snap <- PerasCertDB.getCertSnapshot inboundPool
                let rawContent =
                      Map.toAscList $
                        PerasCertDB.getCertsAfter snap (PerasCertDB.zeroPerasCertTicketNo)
                pure $ getPerasCert . snd <$> rawContent

          return (outboundPoolReader, inboundPoolWriter, getAllInboundPoolContent)
       in
        prop_smoke_object_diffusion
          protocolConstants
          certs
          runOutboundPeer
          runInboundPeer
          mkPoolInterfaces
