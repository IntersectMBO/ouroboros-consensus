{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
import           Cardano.Ledger.BaseTypes (nonZero, unNonZero)
import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Tracer (nullTracer)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Network.TypedProtocol.Stateful.Proofs (connect)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query (Query (..))
import           Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.BFT
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import           Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB (LgrDB,
                     LgrDbArgs (..), mkLgrDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB as LgrDB
import           Ouroboros.Consensus.Storage.LedgerDB (configLedgerDb,
                     defaultDiskPolicyArgs)
import qualified Ouroboros.Consensus.Storage.LedgerDB as LgrDB (ledgerDbPast,
                     ledgerDbTip, ledgerDbWithAnchor)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Mock.Chain (Chain (..))
import qualified Ouroboros.Network.Mock.Chain as Chain
import           Ouroboros.Network.Protocol.LocalStateQuery.Client
import           Ouroboros.Network.Protocol.LocalStateQuery.Examples
                     (localStateQueryClient)
import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (AcquireFailure (..), State (..), Target (..))
import           System.FS.API (HasFS, SomeHasFS (..))
import           Test.Cardano.Ledger.Core.Arbitrary ()
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
-- * Preseed the LgrDB of the server with the preferred chain of the
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
    actualOutcome = runSimOrThrow $ do
      let client = mkClient points
      server <- mkServer k chain
      (\(a, _, _) -> a) <$>
        connect
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
-- accept it. This is because the LgrDB may contain snapshots for blocks on
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
      Chain.drop (fromIntegral $ unNonZero (maxRollbacks k)) chain

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
            " newer than the immutable tip, but got AcquireFailurePointTooOld")
           (property False)
        | otherwise
        -> tabulate "Acquired" ["AcquireFailurePointTooOld"] $ property True
    checkResult VolatileTip = \case
      Right _result -> tabulate "Acquired" ["Success"] True
      Left  failure -> counterexample ("acquire tip point resulted in " ++ show failure) False
    checkResult ImmutableTip = \case
      Right _result -> tabulate "Acquired" ["Success"] True
      Left  failure -> counterexample ("acquire tip point resulted in " ++ show failure) False

mkClient
  :: Monad m
  => [Target (Point TestBlock)]
  -> LocalStateQueryClient
       TestBlock
       (Point TestBlock)
       (Query TestBlock)
       m
       [(Target (Point TestBlock), Either AcquireFailure (Point TestBlock))]
mkClient points = localStateQueryClient [(pt, BlockQuery QueryLedgerTip) | pt <- points]

mkServer ::
     IOLike m
  => SecurityParam
  -> Chain TestBlock
  -> m (LocalStateQueryServer TestBlock (Point TestBlock) (Query TestBlock) m ())
mkServer k chain = do
    lgrDB <- initLgrDB k chain
    return $
      localStateQueryServer
        cfg
        (castPoint . LgrDB.ledgerDbTip <$> LgrDB.getCurrent lgrDB)
        (\pt -> LgrDB.ledgerDbPast pt <$> LgrDB.getCurrent lgrDB)
        getImmutablePoint
  where
    cfg = ExtLedgerCfg $ testCfg k
    getImmutablePoint = return $ Chain.headPoint $
      Chain.drop (fromIntegral $ unNonZero (maxRollbacks k)) chain

-- | Initialise a 'LgrDB' with the given chain.
initLgrDB ::
     forall m. IOLike m
  => SecurityParam
  -> Chain TestBlock
  -> m (LgrDB m TestBlock)
initLgrDB k chain = do
    varDB          <- newTVarIO genesisLedgerDB
    varPrevApplied <- newTVarIO mempty
    let lgrDB = mkLgrDB varDB varPrevApplied resolve args k
    LgrDB.validate lgrDB genesisLedgerDB BlockCache.empty 0 noopTrace
      (map getHeader (Chain.toOldestFirst chain)) >>= \case
        LgrDB.ValidateExceededRollBack _ ->
          error "impossible: rollback was 0"
        LgrDB.ValidateLedgerError _ ->
          error "impossible: there were no invalid blocks"
        LgrDB.ValidateSuccessful ledgerDB' -> do
          atomically $ LgrDB.setCurrent lgrDB ledgerDB'
          return lgrDB
  where
    resolve :: RealPoint TestBlock -> m TestBlock
    resolve = return . (blockMapping Map.!)

    blockMapping :: Map (RealPoint TestBlock) TestBlock
    blockMapping = Map.fromList
      [(blockRealPoint b, b) | b <- Chain.toOldestFirst chain]

    cfg = configLedgerDb (testCfg k) OmitLedgerEvents

    genesisLedgerDB = LgrDB.ledgerDbWithAnchor testInitExtLedger

    noopTrace :: blk -> m ()
    noopTrace = const $ pure ()

    args = LgrDbArgs
      { lgrConfig               = cfg
      , lgrHasFS                = SomeHasFS (error "lgrHasFS" :: HasFS m ())
      , lgrDiskPolicyArgs       = defaultDiskPolicyArgs
      , lgrGenesis              = return testInitExtLedger
      , lgrTracer               = nullTracer
      }

testCfg :: SecurityParam -> TopLevelConfig TestBlock
testCfg securityParam = TopLevelConfig {
      topLevelConfigProtocol = BftConfig {
          bftParams  = BftParams { bftSecurityParam = securityParam
                                 , bftNumNodes      = numCoreNodes
                                 }
        , bftSignKey = SignKeyMockDSIGN 0
        , bftVerKeys = Map.singleton (CoreId (CoreNodeId 0)) (VerKeyMockDSIGN 0)
        }
    , topLevelConfigLedger      = testBlockLedgerConfigFrom eraParams
    , topLevelConfigBlock       = TestBlockConfig numCoreNodes
    , topLevelConfigCodec       = TestBlockCodecConfig
    , topLevelConfigStorage     = TestBlockStorageConfig
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
  Orphans
-------------------------------------------------------------------------------}

instance Arbitrary SecurityParam where
  arbitrary = SecurityParam <$> choose (1, 100) `suchThatMap` nonZero
  shrink (SecurityParam k) = [SecurityParam k' |  k' <- shrink k, unNonZero k' > 0]
