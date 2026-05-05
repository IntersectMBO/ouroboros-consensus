{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.MiniProtocol.ObjectDiffusion.PerasCert.Smoke
  ( tests
  , genPerasCert
  , genValidatedPerasCert
  ) where

import Control.Monad (join)
import Control.Tracer (contramap, nullTracer)
import Data.Functor.Identity (Identity (..))
import qualified Data.Map as Map
import Network.TypedProtocol.Driver.Simple (runPeer, runPipelinedPeer)
import Ouroboros.Consensus.Block.SupportsPeras
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( WithArrivalTime (..)
  , forgetArrivalTime
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.PerasCert
import Ouroboros.Consensus.Peras.Params (mkPerasParams, perasWeight)
import Ouroboros.Consensus.Peras.Types (PerasRoundNo (..))
import Ouroboros.Consensus.Storage.PerasCertDB.API
  ( AddPerasCertResult (..)
  , PerasCertDB
  , PerasCertTicketNo
  )
import qualified Ouroboros.Consensus.Storage.PerasCertDB.API as PerasCertDB
import qualified Ouroboros.Consensus.Storage.PerasCertDB.Impl as PerasCertDB
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Block (StandardHash)
import Ouroboros.Network.Protocol.ObjectDiffusion.Codec
import Ouroboros.Network.Protocol.ObjectDiffusion.Inbound
  ( objectDiffusionInboundPeerPipelined
  )
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound (objectDiffusionOutboundPeer)
import Test.Consensus.MiniProtocol.ObjectDiffusion.Smoke
  ( ListWithUniqueIds (..)
  , WithId
  , genListWithUniqueIds
  , genPointTestBlock
  , genProtocolConstants
  , genWithArrivalTime
  , getId
  , mockSystemTime
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
    [ testProperty "PerasCertDiffusion smoke test" prop_smoke
    ]

genPerasCert :: Gen (PerasCert TestBlock)
genPerasCert = do
  pcCertRound <- PerasRoundNo <$> arbitrary
  pcCertBoostedBlock <- genPointTestBlock
  pure $ PerasCert{pcCertRound, pcCertBoostedBlock}

instance WithId (PerasCert blk) PerasRoundNo where
  getId = pcCertRound

instance WithId (WithArrivalTime (ValidatedPerasCert blk)) PerasRoundNo where
  getId = pcCertRound . vpcCert . forgetArrivalTime

genValidatedPerasCert :: Gen (ValidatedPerasCert TestBlock)
genValidatedPerasCert =
  ValidatedPerasCert
    <$> genPerasCert
    <*> pure (perasWeight mkPerasParams)

newCertDB ::
  (IOLike m, StandardHash blk) => [WithArrivalTime (ValidatedPerasCert blk)] -> m (PerasCertDB m blk)
newCertDB certs = do
  db <- PerasCertDB.createDB (PerasCertDB.PerasCertDbArgs @Identity nullTracer)
  mapM_
    ( \cert -> do
        result <- join $ atomically $ PerasCertDB.addCert db cert
        case result of
          AddedPerasCertToDB -> pure ()
          PerasCertAlreadyInDB -> throwIO (userError "Expected AddedPerasCertToDB, but cert was already in DB")
    )
    certs
  pure db

prop_smoke :: Property
prop_smoke =
  forAll genProtocolConstants $ \protocolConstants ->
    forAll (genListWithUniqueIds (genWithArrivalTime genValidatedPerasCert)) $
      \(ListWithUniqueIds watValidatedCerts) ->
        let
          mkPoolInterfaces ::
            forall m.
            IOLike m =>
            m
              ( ObjectPoolReader PerasRoundNo (PerasCert TestBlock) PerasCertTicketNo m
              , ObjectPoolWriter PerasRoundNo (PerasCert TestBlock) m
              , m [PerasCert TestBlock]
              )
          mkPoolInterfaces = do
            outboundPool <- newCertDB watValidatedCerts
            inboundPool <- newCertDB []

            let outboundPoolReader = makePerasCertPoolReaderFromCertDB outboundPool
                inboundPoolWriter = makePerasCertPoolWriterFromCertDB mockSystemTime inboundPool
                getAllInboundPoolContent = do
                  certsMap <-
                    atomically $
                      PerasCertDB.getCertsAfter inboundPool (PerasCertDB.zeroPerasCertTicketNo)
                  certs' <- sequence (Map.elems certsMap)
                  pure $ vpcCert . forgetArrivalTime <$> certs'

            return (outboundPoolReader, inboundPoolWriter, getAllInboundPoolContent)
         in
          prop_smoke_object_diffusion
            protocolConstants
            (map (vpcCert . forgetArrivalTime) watValidatedCerts)
            runOutboundPeer
            runInboundPeer
            mkPoolInterfaces
 where
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
