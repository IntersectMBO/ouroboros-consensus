{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.MiniProtocol.ObjectDiffusion.PerasCert.Smoke
  ( tests
  ) where

import Control.Monad (join)
import Control.Monad.Class.MonadTimer.SI (timeout)
import Control.Monad.IOSim (IOSim, runSimStrictShutdown)
import Control.ResourceRegistry (withRegistry)
import Control.Tracer (contramap, nullTracer)
import Data.Functor.Identity (Identity (..))
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Network.TypedProtocol.Driver.Simple (runPeer, runPipelinedPeer)
import Ouroboros.Consensus.Block.SupportsPeras
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( WithArrivalTime (..)
  , forgetArrivalTime
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.PerasCert
import Ouroboros.Consensus.Peras.Context
  ( BoundedPerasEpochContext (BoundedPerasEpochContext)
  , PerasEpochContextResolver (..)
  , PerasEpochContextResolverHandle (..)
  , mockPerasEpochContextResolverHandle
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDBImpl
import Ouroboros.Consensus.Storage.PerasCertDB.API
  ( AddPerasCertResult (..)
  , PerasCertDB
  )
import qualified Ouroboros.Consensus.Storage.PerasCertDB.API as PerasCertDB
import qualified Ouroboros.Consensus.Storage.PerasCertDB.Impl as PerasCertDB
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Protocol.ObjectDiffusion.Codec
import Ouroboros.Network.Protocol.ObjectDiffusion.Inbound
  ( objectDiffusionInboundPeerPipelined
  )
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound (objectDiffusionOutboundPeer)
import Test.Consensus.MiniProtocol.ObjectDiffusion.Smoke
  ( genProtocolConstants
  , prop_smoke_object_diffusion
  )
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.ChainDB
import Test.Util.Peras
import Test.Util.TestBlock

tests :: TestTree
tests =
  testGroup
    "ObjectDiffusion.PerasCert.Smoke"
    [ testProperty "PerasCertDiffusion smoke test" prop_smoke
    , testProperty
        "production writer skips obsolete certificates, waits for processing and handles closure"
        prop_writer_waits_for_processing
    ]

-- | Use the production writer with controlled ChainDB completion promises.
-- The real database supplies the rest of the API and its resource lifecycle.
-- The resolver can exclude the oldest certificate to represent its context
-- becoming obsolete between the download request and the writer call.
prop_writer_waits_for_processing :: Property
prop_writer_waits_for_processing =
  forAll genMockPerasEpochContext $ \epochContext ->
    forAll
      ( resize 5 (genListWithUniqueIds getPerasCertRound (genMockValidatedPerasCert epochContext))
          `suchThat` (\(ListWithUniqueIds certs) -> length certs >= 2)
      )
      $ \(ListWithUniqueIds certs) ->
        forAll
          ( elements
              [ (outcome, skipOldest)
              | outcome <-
                  [ ChainDB.PerasCertProcessed AddedPerasCertToDB
                  , ChainDB.PerasCertIgnoredTooOld
                  , ChainDB.PerasCertNotProcessedClosing
                  ]
              , skipOldest <- [False, True]
              ]
          )
          $ \(outcome, skipOldest) ->
            let lowerBound = if skipOldest then minimum (map getPerasCertRound certs) + 1 else minBound
                expectedRounds = filter (>= lowerBound) (map getPerasCertRound certs)
                simulation :: forall s. IOSim s (Maybe (), Maybe (), Maybe (Either IOError ()))
                simulation = withRegistry $ \reg -> do
                  nodeDBs <- emptyNodeDBs
                  let cfg = singleNodeTestConfig
                      args =
                        fromMinimalChainDbArgs
                          MinimalChainDbArgs
                            { mcdbTopLevelConfig = cfg
                            , mcdbChunkInfo = mkTestChunkInfo cfg
                            , mcdbInitLedger = testInitExtLedger
                            , mcdbRegistry = reg
                            , mcdbNodeDBs = nodeDBs
                            }
                  bracket
                    (ChainDBImpl.openDBInternal args False)
                    (ChainDB.closeDB . fst)
                    $ \(db, _) -> do
                      let resolver =
                            PerasEpochContextResolverHandle $
                              pure $
                                PerasEpochContextResolver
                                  (PerasEnabled (BoundedPerasEpochContext lowerBound maxBound epochContext))
                                  NoPerasEnabled
                      queued <- newTVarIO []
                      allowCompletion <- newTVarIO False
                      -- IOException has no NoThunks instance.
                      result <- uncheckedNewTVarM Nothing
                      let controlledDb =
                            db
                              { ChainDB.getPerasEpochContextResolverHandle = resolver
                              , ChainDB.getPerasCertIds = pure mempty
                              , ChainDB.addPerasCertAsync = \cert -> do
                                  atomically $ modifyTVar queued (++ [getPerasCertRound (forgetArrivalTime cert)])
                                  pure $ ChainDB.AddPerasCertPromise $ do
                                    atomically $ readTVar allowCompletion >>= check
                                    pure outcome
                              }
                          writer = makePerasCertPoolWriterFromChainDB mockSystemTime controlledDb
                          waitReturned = atomically $ do
                            returned <- readTVar result
                            maybe retry pure returned
                      withAsync
                        (try (opwAddObjects writer (vpcCert <$> certs)) >>= atomically . writeTVar result . Just)
                        $ \_ -> do
                          allQueued <- timeout 1 $ atomically $ do
                            rounds <- readTVar queued
                            check (rounds == expectedRounds)
                          returnedEarly <- timeout 0.25 (waitReturned >> pure ())
                          atomically $ writeTVar allowCompletion True
                          returned <- timeout 1 waitReturned
                          pure (allQueued, returnedEarly, returned)
             in case runSimStrictShutdown simulation of
                  Right (allQueued, returnedEarly, returned) ->
                    counterexample "the writer did not enqueue its whole batch before waiting" (isJust allQueued)
                      .&&. counterexample "the writer returned before ChainDB processed the batch" (not (isJust returnedEarly))
                      .&&. counterexample
                        "incorrect handling of ChainDB completion"
                        ( case returned of
                            Just (Left _) -> outcome == ChainDB.PerasCertNotProcessedClosing
                            Just (Right ()) -> outcome /= ChainDB.PerasCertNotProcessedClosing
                            Nothing -> False
                        )
                  Left err -> counterexample (show err) $ property False

newCertDB ::
  ( IOLike m
  , BlockSupportsPeras blk
  ) =>
  [WithArrivalTime (ValidatedPerasCert blk)] ->
  m (PerasCertDB m blk)
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
    forAll genMockPerasEpochContext $ \epochContext ->
      forAll
        (genListWithUniqueIds getPerasCertRound (genWithArrivalTime (genMockValidatedPerasCert epochContext)))
        $ \(ListWithUniqueIds watValidatedCerts) ->
          let
            mkPoolInterfaces ::
              forall m.
              IOLike m =>
              m
                ( ObjectPoolReader PerasRoundNo (PerasCert TestBlock) PerasRoundNo m
                , ObjectPoolWriter PerasRoundNo (PerasCert TestBlock) m
                , m [PerasCert TestBlock]
                , m [PerasCert TestBlock]
                )
            mkPoolInterfaces = do
              epochContextResolverHandle <- mockPerasEpochContextResolverHandle epochContext

              outboundPool <- newCertDB watValidatedCerts
              inboundPool <- newCertDB []

              let outboundPoolReader = makeTestPerasCertPoolReaderFromCertDB outboundPool
                  inboundPoolWriter = makeTestPerasCertPoolWriterFromCertDB mockSystemTime inboundPool epochContextResolverHandle
                  getAllPoolContent pool = do
                    certsMap <-
                      atomically $
                        PerasCertDB.getCertsAfter pool zeroPerasRoundNo
                    certs' <- sequence (Map.elems certsMap)
                    pure $ vpcCert . forgetArrivalTime <$> certs'

              return
                ( outboundPoolReader
                , inboundPoolWriter
                , getAllPoolContent outboundPool
                , getAllPoolContent inboundPool
                )
           in
            prop_smoke_object_diffusion
              protocolConstants
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
