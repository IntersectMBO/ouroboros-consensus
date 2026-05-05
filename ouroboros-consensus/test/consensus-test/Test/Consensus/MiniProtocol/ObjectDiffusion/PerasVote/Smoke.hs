{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.MiniProtocol.ObjectDiffusion.PerasVote.Smoke
  ( tests
  ) where

import Control.Monad (join)
import Control.Tracer (contramap, nullTracer)
import qualified Data.Map as Map
import Network.TypedProtocol.Driver.Simple (runPeer, runPipelinedPeer)
import Ouroboros.Consensus.Block.SupportsPeras
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( WithArrivalTime (..)
  , forgetArrivalTime
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.PerasVote
import Ouroboros.Consensus.Peras.Context
  ( PerasEpochContextResolverHandle
  , mockPerasEpochContextResolverHandle
  )
import Ouroboros.Consensus.Storage.PerasVoteDB
  ( AddPerasVoteResult (..)
  , PerasVoteDB
  , PerasVoteTicketNo
  , zeroPerasVoteTicketNo
  )
import qualified Ouroboros.Consensus.Storage.PerasVoteDB as PerasVoteDB
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Protocol.ObjectDiffusion.Codec
import Ouroboros.Network.Protocol.ObjectDiffusion.Inbound
  ( objectDiffusionInboundPeerPipelined
  )
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound (objectDiffusionOutboundPeer)
import Test.Consensus.MiniProtocol.ObjectDiffusion.Smoke
  ( ListWithUniqueIds (..)
  , genProtocolConstants
  , prop_smoke_object_diffusion
  )
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Peras
  ( genListWithUniqueIds
  , genMockPerasEpochContext
  , genMockValidatedPerasVote
  , genWithArrivalTime
  , mockSystemTime
  )
import Test.Util.TestBlock

tests :: TestTree
tests =
  testGroup
    "ObjectDiffusion.PerasVote.Smoke"
    [ testProperty "PerasVoteDiffusion smoke test" prop_smoke
    ]

newVoteDB ::
  ( IOLike m
  , BlockSupportsPeras blk
  ) =>
  PerasEpochContextResolverHandle m blk ->
  [WithArrivalTime (ValidatedPerasVote blk)] ->
  m (PerasVoteDB m blk)
newVoteDB resolverHandle votes = do
  db <- PerasVoteDB.createDB (PerasVoteDB.PerasVoteDbArgs nullTracer resolverHandle)
  mapM_
    ( \vote -> do
        result <- join $ atomically $ PerasVoteDB.addVote db vote
        case result of
          PerasVoteAlreadyInDB -> throwIO (userError "Expected AddedPerasVote..., but vote was already in DB")
          AddedPerasVoteButDidntGenerateNewCert -> pure ()
          AddedPerasVoteAndGeneratedNewCert _ -> pure ()
    )
    votes
  pure db

prop_smoke :: Property
prop_smoke =
  forAll genProtocolConstants $ \protocolConstants ->
    forAll genMockPerasEpochContext $ \epochContext ->
      forAll
        (genListWithUniqueIds getPerasVoteRound (genWithArrivalTime (genMockValidatedPerasVote epochContext)))
        $ \(ListWithUniqueIds watValidatedVotes) ->
          let
            mkPoolInterfaces ::
              IOLike m =>
              m
                ( ObjectPoolReader PerasVoteId (PerasVote TestBlock) PerasVoteTicketNo m
                , ObjectPoolWriter PerasVoteId (PerasVote TestBlock) m
                , m [PerasVote TestBlock]
                )
            mkPoolInterfaces = do
              epochContextResolverHandle <- mockPerasEpochContextResolverHandle epochContext

              outboundPool <- newVoteDB epochContextResolverHandle watValidatedVotes
              inboundPool <- newVoteDB epochContextResolverHandle []

              let outboundPoolReader = makePerasVotePoolReaderFromVoteDB outboundPool
                  inboundPoolWriter = makePerasVotePoolWriterFromVoteDB mockSystemTime inboundPool epochContextResolverHandle
                  getAllInboundPoolContent = do
                    votesMap <-
                      atomically $
                        PerasVoteDB.getVotesAfter inboundPool zeroPerasVoteTicketNo
                    pure $ vpvVote . forgetArrivalTime <$> Map.elems votesMap

              return (outboundPoolReader, inboundPoolWriter, getAllInboundPoolContent)
           in
            prop_smoke_object_diffusion
              protocolConstants
              (map (vpvVote . forgetArrivalTime) watValidatedVotes)
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
