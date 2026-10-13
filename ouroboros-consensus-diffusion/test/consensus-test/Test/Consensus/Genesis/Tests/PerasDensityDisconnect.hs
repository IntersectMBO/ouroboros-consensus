{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Reproduce the unresolved interaction between Peras boosts and Genesis LoE.
-- The expected failure must be removed when the syncing path is fixed.
-- The separate fix also needs certificate-weighted GDD and a certificate
-- completeness condition before disconnecting peers; see Peras design §A.2.3.
module Test.Consensus.Genesis.Tests.PerasDensityDisconnect (tests) where

import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import Control.Monad (void)
import Control.Monad.IOSim (runSimStrictShutdown)
import Control.ResourceRegistry
import Control.Tracer (nullTracer)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Set.NonEmpty as NESet
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  , WithArrivalTime (..)
  )
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Genesis.Governor (gddWatcher)
import Ouroboros.Consensus.HeaderValidation (HeaderWithTime)
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( ChainSyncClientHandle (..)
  , ChainSyncClientHandleCollection (..)
  , ChainSyncState (..)
  , newChainSyncClientHandleCollection
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.State
  ( ChainSyncJumpingState (..)
  , DisengagedInitState (..)
  )
import Ouroboros.Consensus.Node.GsmState (GsmState (Syncing))
import Ouroboros.Consensus.NodeId (CoreNodeId (..))
import Ouroboros.Consensus.Peras.Cert.Mock (MockPerasCert (..))
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as Punishment
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB.Impl
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as ChainDB
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM (forkLinkedWatcher)
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Test.Consensus.Genesis.Setup.GenChains (IssueTestBlock (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (counterexample, expectFailure, once, (===))
import Test.Util.ChainDB
import Test.Util.Header (attachSlotTimeToFragment)
import Test.Util.Orphans.IOLike ()
import Test.Util.TestBlock

tests :: TestTree
tests =
  testGroup
    "Peras boost during Genesis syncing"
    [ testCase "scenario selects the certified sparse fork" $
        case runSimStrictShutdown scenario of
          Left err -> error (show err)
          Right (ordinaryTip, syncingTip, loeTip, _) -> do
            ordinaryTip @?= blockPoint a2
            syncingTip @?= blockPoint a2
            loeTip @?= blockPoint common
    , testProperty "KNOWN FAILURE: certificate advances the immutable point past LoE" $
        once $ expectFailure $
          case runSimStrictShutdown scenario of
            Left err -> counterexample (show err) False
            Right (_, _, _, anchor) ->
              counterexample "the shared point must remain reversible until GDD resolves the peers" $
                anchor === blockPoint common
    ]

scenario :: forall m. IOLike m => m (Point TestBlock, Point TestBlock, Point TestBlock, Point TestBlock)
scenario = do
  ordinaryTip <- withRegistry $ \registry -> do
    db <- openChainDB registry (pure ChainDB.LoEDisabled)
    addBlocks db [common, h1, h2, a1, a2]
    void $ ChainDB.addPerasCertSync db cert
    addBlocks db [h3, h4]
    selectedTip db

  (syncingTip, loeTip, anchorBeforeGDD) <- withRegistry $ \registry -> do
    varLoE <- newTVarIO $ AF.Empty AF.AnchorGenesis
    db <- openChainDB registry (atomically $ ChainDB.LoEEnabled <$> readTVar varLoE)
    handles <- atomically newChainSyncClientHandleCollection
    denserHandle <- atomically $ mkHandle denserHeaders
    certifiedHandle <- atomically $ mkHandle certifiedHeaders
    atomically $ do
      cschcAddHandle handles denserPeer denserHandle
      cschcAddHandle handles certifiedPeer certifiedHandle
    void $ forkLinkedWatcher registry "Peras GDD" $
      gddWatcher cfg nullTracer db 0 (pure Syncing) (cschcMap handles) varLoE

    -- Wait until GDD has supplied ChainDB with the peers' shared prefix.
    loeTip <- atomically $ do
      loe <- readTVar varLoE
      check $ AF.castPoint (AF.headPoint loe) == blockPoint common
      pure $ AF.castPoint $ AF.headPoint loe

    addBlocks db [common, h1, h2, a1, a2, h3, h4]
    void $ ChainDB.addPerasCertSync db cert
    syncingTip <- selectedTip db
    chain <- atomically $ ChainDB.getCurrentChain db
    let anchorBeforeGDD = castPoint (AF.anchorPoint chain) :: Point TestBlock
    pure (syncingTip, loeTip, anchorBeforeGDD)

  pure (ordinaryTip, syncingTip, loeTip, anchorBeforeGDD)
 where
  denserPeer, certifiedPeer :: CoreNodeId
  denserPeer = CoreNodeId 1
  certifiedPeer = CoreNodeId 2

  denserHeaders, certifiedHeaders :: AnchoredFragment (HeaderWithTime TestBlock)
  denserHeaders = headers [common, h1, h2, h3, h4]
  certifiedHeaders = headers [common, a1, a2]

  headers = attachSlotTimeToFragment cfg . foldl (AF.:>) (AF.Empty AF.AnchorGenesis) . map getHeader

  addBlocks db =
    mapM_ (ChainDB.addBlock_ db Punishment.noPunishment)

cfg :: TopLevelConfig TestBlock
cfg =
  singleNodeTestConfigWith
    TestBlockCodecConfig
    TestBlockStorageConfig
    (SecurityParam $ knownNonZeroBounded @2)
    (GenesisWindow 120)

common, h1, h2, h3, h4, a1, a2 :: TestBlock
common = issueFirstBlock 0 0
h1 = issueSuccessorBlock Nothing 10 common
h2 = issueSuccessorBlock Nothing 20 h1
h3 = issueSuccessorBlock Nothing 20 h2
h4 = issueSuccessorBlock Nothing 30 h3
a1 = issueSuccessorBlock (Just 2) 10 common
a2 = issueSuccessorBlock Nothing 10 a1

cert :: WithArrivalTime (ValidatedPerasCert TestBlock)
cert =
  WithArrivalTime (RelativeTime 0) $
    ValidatedPerasCert
      { vpcCert =
          MockPerasCert
            { mockCertRound = PerasRoundNo 1
            , mockCertBlock = blockPoint a1
            , mockCertVoters = NESet.fromList (PerasSeatIndex 0 :| [])
            }
      , vpcCertBoost = PerasWeight 3
      }

selectedTip :: IOLike m => ChainDB.ChainDB m TestBlock -> m (Point TestBlock)
selectedTip db = atomically $ AF.castPoint . AF.headPoint <$> ChainDB.getCurrentChain db

mkHandle ::
  IOLike m =>
  AnchoredFragment (HeaderWithTime TestBlock) ->
  STM m (ChainSyncClientHandle m TestBlock)
mkHandle frag = do
  varState <-
    newTVar
      ChainSyncState
        { csCandidate = frag
        , csIdling = False
        , csLatestSlot = pure $ AF.headSlot frag
        }
  varJumping <- newTVar $ Disengaged DisengagedDone
  varJumpInfo <- newTVar Nothing
  pure
    ChainSyncClientHandle
      { cschState = varState
      , cschOnGsmStateChanged = \_ _ -> pure ()
      , cschGDDKill = pure ()
      , cschJumping = varJumping
      , cschJumpInfo = varJumpInfo
      }

openChainDB ::
  IOLike m =>
  ResourceRegistry m ->
  ChainDB.GetLoEFragment m TestBlock ->
  m (ChainDB.ChainDB m TestBlock)
openChainDB registry getLoEFragment = do
  mcdbNodeDBs <- emptyNodeDBs
  let args =
        fromMinimalChainDbArgs
          MinimalChainDbArgs
            { mcdbChunkInfo = mkTestChunkInfo cfg
            , mcdbInitLedger = testInitExtLedger
            , mcdbRegistry = registry
            , mcdbTopLevelConfig = cfg
            , mcdbNodeDBs
            }
      chainDbArgs =
        args
          { ChainDB.cdbsArgs =
              (ChainDB.cdbsArgs args){ChainDB.cdbsLoE = getLoEFragment}
          }
  (_, (db, ChainDB.Impl.Internal{ChainDB.Impl.intAddBlockRunner})) <-
    allocate
      registry
      (\_ -> ChainDB.Impl.openDBInternal chainDbArgs False)
      (ChainDB.closeDB . fst)
  void $ forkLinkedThread registry "AddBlockRunner" intAddBlockRunner
  pure db
