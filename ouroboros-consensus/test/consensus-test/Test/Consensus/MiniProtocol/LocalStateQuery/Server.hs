{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
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
import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Tracer (nullTracer, traceWith)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Network.TypedProtocol.Proofs (connect)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Ledger.Basics (getTip)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query (DiskLedgerView (..),
                     Query (..))
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.BFT
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import           Ouroboros.Consensus.Storage.LedgerDB.API (LedgerDB (..),
                     LedgerDBView (viewHandle))
import qualified Ouroboros.Consensus.Storage.LedgerDB.API as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.Args as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore.Init as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.Config as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.DbChangelog as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.DbChangelog.Query as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.Impl as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.Lock as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.Update as LedgerDB
                     (ValidateResult (..))
import           Ouroboros.Consensus.Util (StaticEither (..), fromStaticLeft)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Mock.Chain (Chain (..))
import qualified Ouroboros.Network.Mock.Chain as Chain
import           Ouroboros.Network.Protocol.LocalStateQuery.Client
import           Ouroboros.Network.Protocol.LocalStateQuery.Examples
                     (localStateQueryClient)
import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (AcquireFailure (..))
import           System.FS.API (HasFS, SomeHasFS (..))
import qualified System.FS.Sim.MockFS as Mock
import           System.FS.Sim.STM (simHasFS)
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

    points :: [Maybe (Point TestBlock)]
    points = permute p $
         replicate n Nothing
      ++ (Just . blockPoint <$> treeToBlocks bt)

    actualOutcome = runSimOrThrow $ do
      let client = mkClient points
      server <- mkServer k chain
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
checkOutcome
  :: SecurityParam
  -> Chain TestBlock
  -> [(Maybe (Point TestBlock), Either AcquireFailure (Point TestBlock))]
  -> Property
checkOutcome k chain = conjoin . map (uncurry checkResult)
  where
    immutableSlot :: WithOrigin SlotNo
    immutableSlot = Chain.headSlot $
      Chain.drop (fromIntegral (maxRollbacks k)) chain

    checkResult
      :: Maybe (Point TestBlock)
      -> Either AcquireFailure (Point TestBlock)
      -> Property
    checkResult (Just pt) = \case
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
    checkResult Nothing = \case
      Right _result -> tabulate "Acquired" ["Success"] True
      Left  failure -> counterexample ("acuire tip point resulted in " ++ show failure) False

mkClient
  :: Monad m
  => [Maybe (Point TestBlock)]
  -> LocalStateQueryClient
       TestBlock
       (Point TestBlock)
       (Query TestBlock)
       m
       [(Maybe (Point TestBlock), Either AcquireFailure (Point TestBlock))]
mkClient points = localStateQueryClient [(pt, BlockQuery QueryLedgerTip) | pt <- points]

mkServer
  :: IOLike m
  => SecurityParam
  -> Chain TestBlock
  -> m (LocalStateQueryServer TestBlock (Point TestBlock) (Query TestBlock) m ())
mkServer k chain = do
    lgrDB <- initLedgerDB k chain
    return $
      localStateQueryServer
        cfg
        (\mpt -> do
           ldb0 <- LedgerDB.anchorlessChangelog <$> atomically (LedgerDB.getCurrent lgrDB)
           pure $ case mpt of
             Nothing -> Right $ mkDLV ldb0
             Just pt -> case LedgerDB.rollback pt ldb0 of
               Nothing  -> Left  $ castPoint $ getTip $ LedgerDB.anchor ldb0
               Just ldb -> Right $ mkDLV ldb
        )
  where
    cfg = ExtLedgerCfg $ testCfg k

    mkDLV ldb =
      DiskLedgerView
        (LedgerDB.current ldb)
        (\_ -> pure trivialLedgerTables)
        (\_rq -> pure trivialLedgerTables)
        (pure ())
        1

-- | Initialise a 'LedgerDB' with the given chain.
initLedgerDB
  :: forall m. IOLike m
  => SecurityParam
  -> Chain TestBlock
  -> m (LedgerDB.LedgerDB m TestBlock)
initLedgerDB k chain = do
    varDB          <- newTVarIO genesisLedgerDB
    varPrevApplied <- newTVarIO mempty
    let
      backingStoreInitialiser =
        LedgerDB.newBackingStoreInitialiser
        mempty
        LedgerDB.InMemoryBackingStore
    rawLock <- LedgerDB.mkLedgerDBLock
    ledgerDB <- do
      v <- uncheckedNewTVarM Mock.empty
      bstore <- LedgerDB.newBackingStore
        backingStoreInitialiser
        (SomeHasFS (simHasFS v))
        trivialLedgerTables
      let st = LedgerDB.LedgerDBState varDB bstore rawLock varPrevApplied
      LedgerDB.mkLedgerDB st args resolve

    ((), seP) <- LedgerDB.acquireLDBReadView ledgerDB (StaticLeft ()) (pure ())
    let vh = fromStaticLeft seP
    LedgerDB.validate ledgerDB (viewHandle vh) (LedgerDB.anchorlessChangelog $ genesisLedgerDB) BlockCache.empty 0 (traceWith nullTracer)
      (map getHeader (Chain.toOldestFirst chain)) >>= \case
        LedgerDB.ValidateExceededRollBack _ ->
          error "impossible: rollback was 0"
        LedgerDB.ValidateLedgerError _ ->
          error "impossible: there were no invalid blocks"
        LedgerDB.ValidateSuccessful ledgerDB' -> do
          atomically $ LedgerDB.setCurrent ledgerDB ledgerDB'
          tryFlush ledgerDB
          return ledgerDB
  where
    resolve :: RealPoint TestBlock -> m TestBlock
    resolve = return . (blockMapping Map.!)

    blockMapping :: Map (RealPoint TestBlock) TestBlock
    blockMapping = Map.fromList
      [(blockRealPoint b, b) | b <- Chain.toOldestFirst chain]

    cfg = testCfg k

    genesisLedgerDB = LedgerDB.empty (convertMapKind testInitExtLedger)

    args = LedgerDB.LedgerDBArgs
      { LedgerDB.lgrTopLevelConfig       = cfg
      , LedgerDB.lgrHasFS                = SomeHasFS (error "lgrHasFS" :: HasFS m ())
      , LedgerDB.lgrDiskPolicy           = LedgerDB.defaultDiskPolicy k LedgerDB.DefaultSnapshotInterval
      , LedgerDB.lgrGenesis              = return testInitExtLedger
      , LedgerDB.lgrTracer               = nullTracer
      , LedgerDB.lgrTraceLedger          = nullTracer
      , LedgerDB.lgrBackingStoreSelector = LedgerDB.InMemoryBackingStore
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
