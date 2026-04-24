{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.MiniProtocol.ObjectDiffusion.PerasVote.Smoke
  ( tests
  , genPerasVoterId
  , genPerasVoteStake
  , genPerasVote
  , genValidatedPerasVote
  ) where

import qualified Cardano.Crypto.DSIGN.Class as SL
import qualified Cardano.Crypto.Seed as SL
import qualified Cardano.Ledger.Keys as SL
import Control.Monad (join)
import Control.Tracer (contramap, nullTracer)
import Data.Data (Typeable)
import qualified Data.Map as Map
import Data.Ratio ((%))
import Data.String (IsString (..))
import Network.TypedProtocol.Driver.Simple (runPeer, runPipelinedPeer)
import Ouroboros.Consensus.Block.SupportsPeras
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( WithArrivalTime (..)
  , forgetArrivalTime
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.PerasVote
import Ouroboros.Consensus.Storage.PerasVoteDB
  ( AddPerasVoteResult (..)
  , PerasVoteDB
  , PerasVoteTicketNo
  , zeroPerasVoteTicketNo
  )
import qualified Ouroboros.Consensus.Storage.PerasVoteDB as PerasVoteDB
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
    "ObjectDiffusion.PerasVote.Smoke"
    [ testProperty "PerasVoteDiffusion smoke test" prop_smoke
    ]

genPerasVoterId :: Gen PerasVoterId
genPerasVoterId = do
  bytes <- fromString <$> vectorOf 32 arbitrary
  let signKey = SL.genKeyDSIGN (SL.mkSeedFromBytes bytes)
      verKey = SL.deriveVerKeyDSIGN signKey
      keyHash = SL.hashKey (SL.VKey verKey)
  pure (PerasVoterId keyHash)

genPerasVoteStake :: Gen PerasVoteStake
genPerasVoteStake = do
  stake <- (1 %) <$> choose (2, 10)
  pure (PerasVoteStake stake)

genPerasVote :: Gen (PerasVote TestBlock)
genPerasVote = do
  pvVoteRound <- PerasRoundNo <$> arbitrary
  pvVoteBlock <- genPointTestBlock
  pvVoteVoterId <- genPerasVoterId
  pvVoteStake <- genPerasVoteStake
  pure $ PerasVote{pvVoteRound, pvVoteBlock, pvVoteVoterId, pvVoteStake}

instance WithId (PerasVote blk) (PerasVoteId blk) where
  getId = getPerasVoteId

instance WithId (WithArrivalTime (ValidatedPerasVote blk)) (PerasVoteId blk) where
  getId = getPerasVoteId . vpvVote . forgetArrivalTime

genValidatedPerasVote :: Gen (ValidatedPerasVote TestBlock)
genValidatedPerasVote = do
  vote <- genPerasVote
  pure $
    ValidatedPerasVote
      { vpvVote = vote
      , vpvVoteStake = pvVoteStake vote
      }

newVoteDB ::
  (IOLike m, StandardHash blk, Typeable blk) =>
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
            forall m.
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
