{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.MiniProtocol.ObjectDiffusion.PerasVote.Smoke
  ( tests
  , genPerasSeatIndex
  , genVoteWeight
  , genPerasVote
  , genValidatedPerasVote
  ) where

import Control.Monad (join)
import Control.Tracer (contramap, nullTracer)
import qualified Data.Map as Map
import Data.Ratio ((%))
import Network.TypedProtocol.Driver.Simple (runPeer, runPipelinedPeer)
import Ouroboros.Consensus.Block.SupportsPeras
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( WithArrivalTime (..)
  , forgetArrivalTime
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.PerasVote
import Ouroboros.Consensus.Peras.Vote.Mock (MockPerasVote (..))
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
    "ObjectDiffusion.PerasVote.Smoke"
    [ testProperty "PerasVoteDiffusion smoke test" prop_smoke
    ]

genPerasSeatIndex :: Gen PerasSeatIndex
genPerasSeatIndex = PerasSeatIndex <$> arbitrary

genVoteWeight :: Gen VoteWeight
genVoteWeight = do
  weight <- (1 %) <$> choose (2, 10)
  pure (VoteWeight weight)

genPerasVote :: Gen (PerasVote TestBlock)
genPerasVote = do
  mockVoteRound <- PerasRoundNo <$> arbitrary
  mockVoteBlock <- genPointTestBlock
  mockVoteSeatIndex <- genPerasSeatIndex
  mockVoteWeight <- genVoteWeight
  pure $
    MockPerasVote
      { mockVoteRound
      , mockVoteBlock
      , mockVoteSeatIndex
      , mockVoteWeight
      }

instance WithId (MockPerasVote blk) (PerasVoteId blk) where
  getId = getPerasVoteId

instance
  IsPerasVote (PerasVote blk) blk =>
  WithId (WithArrivalTime (ValidatedPerasVote blk)) (PerasVoteId blk)
  where
  getId = getPerasVoteId . vpvVote . forgetArrivalTime

genValidatedPerasVote :: Gen (ValidatedPerasVote TestBlock)
genValidatedPerasVote = do
  mockVote <- genPerasVote
  pure
    ValidatedPerasVote
      { vpvVote = mockVote
      , vpvVoteWeight = mockVoteWeight mockVote
      }

newVoteDB ::
  ( IOLike m
  , BlockSupportsPeras blk
  ) =>
  [WithArrivalTime (ValidatedPerasVote blk)] -> m (PerasVoteDB m blk)
newVoteDB votes = do
  db <- PerasVoteDB.createDB (PerasVoteDB.PerasVoteDbArgs nullTracer mkPerasParams)
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
    forAll (genListWithUniqueIds (genWithArrivalTime genValidatedPerasVote)) $
      \(ListWithUniqueIds watValidatedVotes) ->
        let
          mkPoolInterfaces ::
            IOLike m =>
            m
              ( ObjectPoolReader (PerasVoteId TestBlock) (PerasVote TestBlock) PerasVoteTicketNo m
              , ObjectPoolWriter (PerasVoteId TestBlock) (PerasVote TestBlock) m
              , m [PerasVote TestBlock]
              )
          mkPoolInterfaces = do
            outboundPool <- newVoteDB watValidatedVotes
            inboundPool <- newVoteDB []

            let outboundPoolReader = makePerasVotePoolReaderFromVoteDB outboundPool
                inboundPoolWriter =
                  makePerasVotePoolWriterFromVoteDB
                    mockSystemTime
                    (pure (VoteWeightDistr mempty)) -- mocked votes are self-validating
                    inboundPool
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
