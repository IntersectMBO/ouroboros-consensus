{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
--
module Test.Consensus.MiniProtocol.LocalStateQuery.Server (tests) where

import           Cardano.Crypto.DSIGN.Mock
import           Control.Concurrent.Class.MonadSTM.Strict.TVar
import           Control.Monad.Base
import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Tracer
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Network.TypedProtocol.Proofs (connect)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query (Query (..))
import           Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.BFT
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Stream hiding
                     (streamAPI)
import           Ouroboros.Consensus.Storage.LedgerDB (LedgerDB')
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Args
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Snapshots
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Args
import           Ouroboros.Consensus.Util.IOLike hiding (newTVarIO)
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Network.Mock.Chain (Chain (..))
import qualified Ouroboros.Network.Mock.Chain as Chain
import           Ouroboros.Network.Protocol.LocalStateQuery.Client
import           Ouroboros.Network.Protocol.LocalStateQuery.Examples
                     (localStateQueryClient)
import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (AcquireFailure (..), Target (..))
import           System.FS.API (SomeHasFS (..))
import qualified System.FS.Sim.MockFS as MockFS
import           System.FS.Sim.STM
import           Test.QuickCheck hiding (Result)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "LocalStateQueryServer"
    [ testProperty "localStateQueryServer" prop_localStateQueryServer
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
prop_localStateQueryServer
  :: SecurityParam
  -> BlockTree
  -> Permutation
  -> Positive (Small Int)
  -> Property
prop_localStateQueryServer k bt p (Positive (Small n)) = checkOutcome k chain actualOutcome
  where
    chain :: Chain TestBlock
    chain = treePreferredChain bt

    points :: [Target (Point TestBlock)]
    points = permute p $
         replicate n VolatileTip
      ++ (SpecificPoint . blockPoint <$> treeToBlocks bt)


    actualOutcome :: [(Target (Point TestBlock), Either AcquireFailure (Point TestBlock))]
    actualOutcome = runSimOrThrow $ withRegistry $ \rr ->do
      let client = mkClient points
      server <- mkServer rr k chain
      (\(a, _, _) -> a) <$>
        connect
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
     SecurityParam
  -> Chain TestBlock
  -> [(Target (Point TestBlock), Either AcquireFailure (Point TestBlock))]
  -> Property
checkOutcome k chain = conjoin . map (uncurry checkResult)
  where
    immutableSlot :: WithOrigin SlotNo
    immutableSlot = Chain.headSlot $
      Chain.drop (fromIntegral (maxRollbacks k)) chain

    checkResult
      :: Target (Point TestBlock)
      -> Either AcquireFailure (Point TestBlock)
      -> Property
    checkResult (SpecificPoint pt) = \case
      Right result
        -> tabulate "Acquired" ["Success"] $ result === pt
      Left AcquireFailurePointNotOnChain
        | Chain.pointOnChain pt chain
        -> counterexample
           ("Point " <> show pt <>
            " on chain, but got AcquireFailurePointNotOnChain")
           (property False)
        | otherwise
        -> tabulate "Acquired" ["AcquireFailurePointNotOnChain"] $ property True
      Left AcquireFailurePointTooOld
        | pointSlot pt >= immutableSlot
        -> counterexample
           ("Point " <> show pt <>
            " newer or equal than the immutable tip " <> show immutableSlot <>", but got AcquireFailurePointTooOld")
           (property False)
        | otherwise
        -> tabulate "Acquired" ["AcquireFailurePointTooOld"] $ property True
    checkResult VolatileTip = \case
      Right _result -> tabulate "Acquired" ["Success"] True
      Left  failure -> counterexample ("acquire tip point resulted in " ++ show failure) False
    checkResult ImmutableTip = \case
      Right _result -> tabulate "Acquired" ["Success"] True
      Left  failure -> counterexample ("acquire tip point resulted in " ++ show failure) False

mkClient ::
     Monad m
  => [Target (Point TestBlock)]
  -> LocalStateQueryClient
       TestBlock
       (Point TestBlock)
       (Query TestBlock)
       m
       [(Target (Point TestBlock), Either AcquireFailure (Point TestBlock))]
mkClient points = localStateQueryClient [(pt, BlockQuery QueryLedgerTip) | pt <- points]

mkServer ::
     (IOLike m, MonadBase m m)
  => ResourceRegistry m
  -> SecurityParam
  -> Chain TestBlock
  -> m (LocalStateQueryServer TestBlock (Point TestBlock) (Query TestBlock) m ())
mkServer rr k chain = do
    lgrDB <- initLedgerDB k chain
    return $
      localStateQueryServer
        cfg
        (LedgerDB.getReadOnlyForker lgrDB rr)
  where
    cfg = ExtLedgerCfg $ testCfg k

streamAPI :: forall m. IOLike m => StreamAPI m TestBlock TestBlock
streamAPI = StreamAPI {..}
  where
    streamAfter ::
         Point TestBlock
      -> (Either (RealPoint TestBlock) (m (NextItem TestBlock)) -> m a)
      -> m a
    streamAfter _ k = do
        k (Right (pure NoMoreItems))

-- | Initialise a 'LedgerDB' with the given chain.
initLedgerDB ::
     (IOLike m, MonadBase m m)
  => SecurityParam
  -> Chain TestBlock
  -> m (LedgerDB' m TestBlock)
initLedgerDB s c = do
  reg <- unsafeNewRegistry
  fs <- newTVarIO MockFS.empty
  let snapshotPolicyArgs = SnapshotPolicyArgs
        { spaInterval = DefaultSnapshotInterval
        , spaNum = DefaultNumOfDiskSnapshots
        }
      args = LedgerDbArgs
        { lgrSnapshotPolicyArgs = snapshotPolicyArgs
        , lgrHasFS              = SomeHasFS $ simHasFS fs
        , lgrSSDHasFS           = SomeHasFS $ simHasFS fs
        , lgrSnapshotTablesSSD  = False
        , lgrSnapshotStateSSD   = False
        , lgrGenesis            = return testInitExtLedger
        , lgrTracer             = nullTracer
        , lgrFlavorArgs         = LedgerDbFlavorArgsV1 $ V1Args DefaultFlushFrequency DefaultQueryBatchSize InMemoryBackingStoreArgs
        , lgrConfig             = LedgerDB.configLedgerDb $ testCfg s
        , lgrRegistry           = reg
        , lgrStartSnapshot      = Nothing
        }
  ldb <- fst <$> LedgerDB.openDB
    args
    streamAPI
    (Chain.headPoint c)
    (\rpt -> pure $ fromMaybe (error "impossible") $ Chain.findBlock ((rpt ==) . blockRealPoint) c)

  result <- LedgerDB.validate ldb reg (const $ pure ()) BlockCache.empty 0 (map getHeader $ Chain.toOldestFirst c)
  case result of
    LedgerDB.ValidateSuccessful forker -> do
      atomically $ LedgerDB.forkerCommit forker
      LedgerDB.forkerClose forker
    LedgerDB.ValidateExceededRollBack _ ->
      error "impossible: rollback was 0"
    LedgerDB.ValidateLedgerError _ ->
      error "impossible: there were no invalid blocks"

  pure ldb

testCfg :: SecurityParam -> TopLevelConfig TestBlock
testCfg securityParam = TopLevelConfig {
      topLevelConfigProtocol = BftConfig {
          bftParams  = BftParams { bftSecurityParam = securityParam
                                 , bftNumNodes      = numCoreNodes
                                 }
        , bftSignKey = SignKeyMockDSIGN 0
        , bftVerKeys = Map.singleton (CoreId (CoreNodeId 0)) (VerKeyMockDSIGN 0)
        }
    , topLevelConfigLedger  = eraParams
    , topLevelConfigBlock   = TestBlockConfig numCoreNodes
    , topLevelConfigCodec   = TestBlockCodecConfig
    , topLevelConfigStorage = TestBlockStorageConfig
    }
  where
    slotLength :: SlotLength
    slotLength = slotLengthFromSec 20

    numCoreNodes :: NumCoreNodes
    numCoreNodes = NumCoreNodes 1

    eraParams :: HardFork.EraParams
    eraParams = HardFork.defaultEraParams securityParam slotLength

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

instance Arbitrary SecurityParam where
  arbitrary = SecurityParam <$> choose (1, 100)
  shrink (SecurityParam k) = [SecurityParam k' |  k' <- shrink k, k' > 0]
