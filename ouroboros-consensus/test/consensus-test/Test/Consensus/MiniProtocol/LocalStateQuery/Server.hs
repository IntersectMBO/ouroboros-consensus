{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Tests for the local state query server.
--
-- The local state query protocol allows clients such as wallets to query the
-- state of the ledger at any point within @k@ blocks from the tip. The test for
-- this is quite minimal at present: it prepopulates a ledger DB with a bunch of
-- blocks, and then verifies that requesting the ledger tip corresponding to the
-- these blocks gives the right answers, and that asking for blocks not on the
-- chain results in the right error message.
--
-- Note that the query protocol is abstract in the ledger, and the query
-- /language/ we offer (the kinds of queries that can be asked) of course
-- depends on the ledger. The tests use a mock ledger for this purpose.
module Test.Consensus.MiniProtocol.LocalStateQuery.Server (tests) where

import Cardano.Crypto.DSIGN.Mock
import Cardano.Ledger.BaseTypes (knownNonZeroBounded, nonZero, unNonZero)
import Control.Concurrent.Class.MonadSTM.Strict.TMVar
import Control.Monad (join)
import Control.Monad.IOSim (runSimOrThrow)
import Control.ResourceRegistry
import Control.Tracer
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Network.TypedProtocol.Stateful.Proofs (connect)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Ledger.Basics
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server
import Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import Ouroboros.Consensus.NodeId
import Ouroboros.Consensus.Peras.Weight (emptyPerasWeightSnapshot)
import Ouroboros.Consensus.Protocol.BFT
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import Ouroboros.Consensus.Storage.ImmutableDB.Stream hiding
  ( streamAPI
  )
import Ouroboros.Consensus.Storage.LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Backend as V2
import Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory
import Ouroboros.Consensus.Util.IOLike hiding (newTVarIO)
import Ouroboros.Network.Mock.Chain (Chain (..))
import qualified Ouroboros.Network.Mock.Chain as Chain
import Ouroboros.Network.Protocol.LocalStateQuery.Client
import Ouroboros.Network.Protocol.LocalStateQuery.Examples
  ( localStateQueryClient
  )
import Ouroboros.Network.Protocol.LocalStateQuery.Server
import Ouroboros.Network.Protocol.LocalStateQuery.Type
  ( AcquireFailure (..)
  , State (..)
  , Target (..)
  )
import System.FS.API (SomeHasFS (..))
import qualified System.FS.Sim.MockFS as MockFS
import System.FS.Sim.STM
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.QuickCheck hiding (Result)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.Orphans.IOLike ()
import Test.Util.TestBlock

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests =
  testGroup
    "LocalStateQueryServer"
    [ testProperty "localStateQueryServer" prop_localStateQueryServer
    , testProperty "acquireFailsDuringLedgerReplay" prop_acquireFailsDuringLedgerReplay
    , testProperty "acquireSucceedsAfterLedgerReady" prop_acquireSucceedsAfterLedgerReady
    ]

{-------------------------------------------------------------------------------
  Main property
-------------------------------------------------------------------------------}

-- | Plan:
-- * Preseed the LedgerDB of the server with the preferred chain of the
--  'BlockTree'.
-- * Acquire for each block in the 'BlockTree', including the ones not on the
--   chain, a state and send the 'QueryLedgerTip'. Collect these results.
-- * Check that when acquiring failed, it rightfully failed. Otherwise, check
--   whether the returned tip matches the block.
prop_localStateQueryServer ::
  SecurityParam ->
  BlockTree ->
  Permutation ->
  Positive (Small Int) ->
  Property
prop_localStateQueryServer k bt p (Positive (Small n)) = checkOutcome k chain actualOutcome
 where
  chain :: Chain TestBlock
  chain = treePreferredChain emptyPerasWeightSnapshot bt

  points :: [Target (Point TestBlock)]
  points =
    permute p $
      replicate n VolatileTip
        ++ (SpecificPoint . blockPoint <$> treeToBlocks bt)

  actualOutcome :: [(Target (Point TestBlock), Either AcquireFailure (Point TestBlock))]
  actualOutcome = runSimOrThrow $ withRegistry $ \rr -> do
    let client = mkClient points
    server <- mkServer rr k chain
    (\(a, _, _) -> a)
      <$> connect
        StateIdle
        (localStateQueryClientPeer client)
        (localStateQueryServerPeer server)

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

-- | Checks whether the given outcome is correct: in case of an
-- 'AcquireFailure', we check whether it was warranted. Otherwise we check
-- whether the results are correct.
--
-- NOTE: when we don't get an 'AcquireFailure', even though we expected it, we
-- accept it. This is because the LedgerDB may contain snapshots for blocks on
-- the current chain older than @k@, but we do not want to imitate such
-- implementation details.
--
-- Additionally, this function labels the test results.
checkOutcome ::
  SecurityParam ->
  Chain TestBlock ->
  [(Target (Point TestBlock), Either AcquireFailure (Point TestBlock))] ->
  Property
checkOutcome k chain = conjoin . map (uncurry checkResult)
 where
  immutableSlot :: WithOrigin SlotNo
  immutableSlot =
    Chain.headSlot $
      Chain.drop (fromIntegral $ unNonZero (maxRollbacks k)) chain

  checkResult ::
    Target (Point TestBlock) ->
    Either AcquireFailure (Point TestBlock) ->
    Property
  checkResult (SpecificPoint pt) = \case
    Right result ->
      tabulate "Acquired" ["Success"] $ result === pt
    Left AcquireFailurePointNotOnChain
      | Chain.pointOnChain pt chain ->
          counterexample
            ( "Point "
                <> show pt
                <> " on chain, but got AcquireFailurePointNotOnChain"
            )
            (property False)
      | otherwise ->
          tabulate "Acquired" ["AcquireFailurePointNotOnChain"] $ property True
    Left AcquireFailurePointTooOld
      | pointSlot pt >= immutableSlot ->
          counterexample
            ( "Point "
                <> show pt
                <> " newer or equal than the immutable tip "
                <> show immutableSlot
                <> ", but got AcquireFailurePointTooOld"
            )
            (property False)
      | otherwise ->
          tabulate "Acquired" ["AcquireFailurePointTooOld"] $ property True
  checkResult VolatileTip = \case
    Right _result -> tabulate "Acquired" ["Success"] True
    Left failure -> counterexample ("acquire tip point resulted in " ++ show failure) False
  checkResult ImmutableTip = \case
    Right _result -> tabulate "Acquired" ["Success"] True
    Left failure -> counterexample ("acquire tip point resulted in " ++ show failure) False

mkClient ::
  Monad m =>
  [Target (Point TestBlock)] ->
  LocalStateQueryClient
    TestBlock
    (Point TestBlock)
    (Query TestBlock)
    m
    [(Target (Point TestBlock), Either AcquireFailure (Point TestBlock))]
mkClient points = localStateQueryClient [(pt, BlockQuery QueryLedgerTip) | pt <- points]

mkServer ::
  IOLike m =>
  ResourceRegistry m ->
  SecurityParam ->
  Chain TestBlock ->
  m (LocalStateQueryServer TestBlock (Point TestBlock) (Query TestBlock) m ())
mkServer rr k chain = do
  lgrDB <- initLedgerDB k chain
  return $
    localStateQueryServer
      cfg
      ( \t -> do
          (rk, res) <-
            allocate
              rr
              (\_ -> LedgerDB.openReadOnlyForker lgrDB t)
              ( \case
                  Left{} -> pure ()
                  Right v -> roforkerClose v
              )
          case res of
            Left err -> release rk >> pure (Left err)
            Right v -> pure (Right (rk, v))
      )
 where
  cfg = ExtLedgerCfg $ testCfg k

streamAPI :: forall m. IOLike m => StreamAPI m TestBlock TestBlock
streamAPI = StreamAPI{streamAfter}
 where
  streamAfter ::
    Point TestBlock ->
    (Either (RealPoint TestBlock) (m (NextItem TestBlock)) -> m a) ->
    m a
  streamAfter _ k = do
    k (Right (pure NoMoreItems))

-- | Initialise a 'LedgerDB' with the given chain.
initLedgerDB ::
  IOLike m =>
  SecurityParam ->
  Chain TestBlock ->
  m (LedgerDB' m TestBlock)
initLedgerDB s c = do
  fs <- newTMVarIO MockFS.empty
  let args =
        LedgerDbArgs
          { lgrSnapshotPolicyArgs = defaultSnapshotPolicyArgs
          , lgrHasFS = SomeHasFS $ simHasFS fs
          , lgrGenesis = return testInitExtLedger
          , lgrTracer = nullTracer
          , lgrBackendArgs = LedgerDbBackendArgsV2 $ V2.SomeBackendArgs InMemArgs
          , lgrConfig = LedgerDB.configLedgerDb (testCfg s) OmitLedgerEvents
          , lgrQueryBatchSize = DefaultQueryBatchSize
          , lgrStartSnapshot = Nothing
          }
  ldb <-
    runWithTempRegistry
      ( do
          db <-
            LedgerDB.openDB
              args
              streamAPI
              (Chain.headPoint c)
              (\rpt -> pure $ fromMaybe (error "impossible") $ Chain.findBlock ((rpt ==) . blockRealPoint) c)
              (LedgerDB.praosGetVolatileSuffix s)
          pure (db, ())
      )

  case NE.nonEmpty $ Chain.toOldestFirst c of
    Nothing -> pure ()
    Just chain -> do
      result <-
        LedgerDB.validateFork
          ldb
          (const $ pure ())
          BlockCache.empty
          0
          (NE.map getHeader chain)
          (MkSuccessForkerAction $ join . atomically . LedgerDB.forkerCommit)
      case result of
        LedgerDB.ValidateSuccessful -> do
          pure ()
        LedgerDB.ValidateExceededRollBack _ ->
          error "impossible: rollback was 0"
        LedgerDB.ValidateLedgerError _ ->
          error "impossible: there were no invalid blocks"

  pure ldb

testCfg :: SecurityParam -> TopLevelConfig TestBlock
testCfg securityParam =
  TopLevelConfig
    { topLevelConfigProtocol =
        BftConfig
          { bftParams =
              BftParams
                { bftSecurityParam = securityParam
                , bftNumNodes = numCoreNodes
                }
          , bftSignKey = SignKeyMockDSIGN 0
          , bftVerKeys = Map.singleton (CoreId (CoreNodeId 0)) (VerKeyMockDSIGN 0)
          }
    , topLevelConfigLedger = testBlockLedgerConfigFrom eraParams
    , topLevelConfigBlock = TestBlockConfig numCoreNodes
    , topLevelConfigCodec = TestBlockCodecConfig
    , topLevelConfigStorage = TestBlockStorageConfig
    , topLevelConfigCheckpoints = emptyCheckpointsMap
    }
 where
  slotLength :: SlotLength
  slotLength = slotLengthFromSec 20

  numCoreNodes :: NumCoreNodes
  numCoreNodes = NumCoreNodes 1

  eraParams :: HardFork.EraParams
  eraParams = HardFork.defaultEraParams securityParam slotLength

{-------------------------------------------------------------------------------
  Option A: LSQ Acquire fail-fast during LedgerDB replay
-------------------------------------------------------------------------------}

-- | When the LedgerDB is still replaying, the production
-- 'openReadOnlyForkerAtPoint' returns @Left LedgerNotReady@. The LSQ server
-- must translate that into @MsgFailure AcquireFailurePointTooOld@ regardless
-- of which 'Target' the client requested.
prop_acquireFailsDuringLedgerReplay :: Property
prop_acquireFailsDuringLedgerReplay = once $
  conjoin
    [ counterexample ("target " ++ show t ++ ", got: " ++ show res) $
        case res of
          Left AcquireFailurePointTooOld -> property True
          _ -> property False
    | (t, res) <- outcomes
    ]
 where
  outcomes :: [(Target (Point TestBlock), Either AcquireFailure (Point TestBlock))]
  outcomes = runSimOrThrow $ do
    let getView _ = pure (Left LedgerNotReady)
        server = localStateQueryServer cfg getView
        points = [VolatileTip, ImmutableTip, SpecificPoint GenesisPoint]
        client = mkClient points
    (\(a, _, _) -> a)
      <$> connect
        StateIdle
        (localStateQueryClientPeer client)
        (localStateQueryServerPeer server)

  cfg :: ExtLedgerCfg TestBlock
  cfg = ExtLedgerCfg $ testCfg (SecurityParam $ knownNonZeroBounded @2)

-- | Once the LedgerDB transitions from replaying to ready, subsequent
-- @MsgAcquire@s must succeed. We simulate the transition by flipping a
-- 'TVar' between two acquires in the same session: the first call to
-- 'getView' returns @Left LedgerNotReady@, the second falls through to
-- the real LedgerDB-backed forker.
prop_acquireSucceedsAfterLedgerReady ::
  SecurityParam ->
  BlockTree ->
  Property
prop_acquireSucceedsAfterLedgerReady k bt = once $
  counterexample ("outcomes: " ++ show outcomes) $
    case outcomes of
      [(_, Left AcquireFailurePointTooOld), (_, Right _)] -> property True
      _ -> property False
 where
  chain :: Chain TestBlock
  chain = treePreferredChain emptyPerasWeightSnapshot bt

  outcomes :: [(Target (Point TestBlock), Either AcquireFailure (Point TestBlock))]
  outcomes = runSimOrThrow $ withRegistry $ \rr -> do
    firstCall <- atomically $ newTVar True
    lgrDB <- initLedgerDB k chain
    let getView t = do
          isFirst <- atomically $ do
            b <- readTVar firstCall
            writeTVar firstCall False
            pure b
          if isFirst
            then pure (Left LedgerNotReady)
            else do
              (rk, res) <-
                allocate
                  rr
                  (\_ -> LedgerDB.openReadOnlyForker lgrDB t)
                  ( \case
                      Left{} -> pure ()
                      Right v -> roforkerClose v
                  )
              case res of
                Left err -> release rk >> pure (Left err)
                Right v -> pure (Right (rk, v))
        server = localStateQueryServer cfg getView
        client = mkClient [VolatileTip, VolatileTip]
    (\(a, _, _) -> a)
      <$> connect
        StateIdle
        (localStateQueryClientPeer client)
        (localStateQueryServerPeer server)

  cfg :: ExtLedgerCfg TestBlock
  cfg = ExtLedgerCfg $ testCfg k

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

instance Arbitrary SecurityParam where
  arbitrary = SecurityParam <$> choose (1, 100) `suchThatMap` nonZero
  shrink (SecurityParam k) = [SecurityParam k' | k' <- shrink k, unNonZero k' > 0]
