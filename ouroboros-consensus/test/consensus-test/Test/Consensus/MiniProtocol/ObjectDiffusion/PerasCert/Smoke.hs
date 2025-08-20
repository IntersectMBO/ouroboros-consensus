{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Consensus.MiniProtocol.ObjectDiffusion.PerasCert.Smoke (tests) where

import Control.Monad.IOSim (IOSim)
import Control.Tracer (Tracer (..), contramap, nullTracer)
import Data.Functor.Identity (Identity (..))
import qualified Data.List.NonEmpty as NE
import Debug.Trace (traceM)
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
  ( objectDiffusionInboundServerPeerPipelined
  )
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound (objectDiffusionOutboundClientPeer)
import Test.Consensus.MiniProtocol.ObjectDiffusion.Smoke
  ( ListWithUniqueIds (..)
  , WithId
  , getId
  , prop_smoke_object_diffusion
  )
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.TestBlock

-- When wanting to debug a part of the test:
debugTracer :: Monad m => Tracer m String
debugTracer = Tracer traceM

tests :: TestTree
tests =
  testGroup
    "ObjectDiffusion.PerasCert.Smoke"
    [ testProperty "PerasCertDiffusion smoke test" prop_smoke
    ]

instance Arbitrary (Point TestBlock) where
  arbitrary =
    -- Sometimes pick the genesis point
    frequency
      [ (1, pure $ Point Origin)
      ,
        ( 4
        , do
            slotNo <- SlotNo <$> arbitrary
            hash <- TestHash . NE.fromList . getNonEmpty <$> arbitrary
            pure $ Point (At (Block slotNo hash))
        )
      ]

instance Arbitrary (Point blk) => Arbitrary (PerasCert blk) where
  arbitrary = do
    pcCertRound <- PerasRoundNo <$> arbitrary
    pcCertBoostedBlock <- arbitrary
    pure $ PerasCert{pcCertRound, pcCertBoostedBlock}

instance WithId (PerasCert blk) PerasRoundNo where
  getId = pcCertRound

newCertDB :: (IOLike m, StandardHash blk) => [PerasCert blk] -> m (PerasCertDB m blk)
newCertDB certs = do
  db <- PerasCertDB.openDB (PerasCertDB.PerasCertDbArgs @Identity nullTracer)
  mapM_
    ( \cert -> do
        result <- PerasCertDB.addCert db cert
        case result of
          AddedPerasCertToDB -> pure ()
          PerasCertAlreadyInDB -> throwIO (userError "Expected AddedPerasCertToDB, but cert was already in DB")
    )
    certs
  pure db

prop_smoke :: ListWithUniqueIds (PerasCert TestBlock) PerasRoundNo -> Property
prop_smoke (ListWithUniqueIds certs) =
  prop_smoke_object_diffusion certs mkOutboundAsync mkInboundAsync mkPoolInterfaces
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
  mkPoolInterfaces ::
    forall s.
    IOSim
      s
      ( ObjectPoolReader PerasRoundNo (PerasCert TestBlock) PerasCertTicketNo (IOSim s)
      , ObjectPoolWriter PerasRoundNo (PerasCert TestBlock) (IOSim s)
      , (IOSim s) [PerasCert TestBlock]
      )
  mkPoolInterfaces = do
    outboundPool <- newCertDB certs
    inboundPool <- newCertDB []

    let outboundPoolReader = makePerasCertPoolReaderFromCertDB outboundPool
        inboundPoolWriter = makePerasCertPoolWriterFromCertDB inboundPool
        getAllInboundPoolContent = do
          snap <- atomically $ PerasCertDB.getCertSnapshot inboundPool
          let rawContent = PerasCertDB.getCertsAfter snap (PerasCertDB.zeroPerasCertTicketNo)
          pure $ fst <$> rawContent

    return (outboundPoolReader, inboundPoolWriter, getAllInboundPoolContent)
