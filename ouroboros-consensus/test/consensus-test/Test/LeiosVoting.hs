{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Unit tests for 'validateEbClosure', the part of the Leios voting thread
-- that decides whether an EB's endorsed transactions actually apply.
--
-- Mock blocks stand in for a real era: the interesting behaviour here is the
-- orchestration -- which ledger rule each tx goes through, and what ends up
-- recorded in the LeiosTxCache -- not the ledger rules themselves.
module Test.LeiosVoting (tests) where

import qualified Codec.Serialise as Serialise
import Control.Monad (foldM, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy
import Data.Function ((&))
import qualified Data.Vector.Strict as V
import LeiosDemoDb
  ( LeiosDbConnection (..)
  , LeiosDbHandle
  , newLeiosDBInMemory
  , withLeiosDb
  )
import LeiosDemoTypes
  ( BytesSize
  , LeiosEb (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , RbHash (..)
  , SerializedEbBody
  , TxHash
  , hashLeiosEb
  , hashLeiosTx
  , leiosEbBytesSize
  , serializeEbBody
  )
import LeiosTxCache
  ( LeiosTxCache (..)
  , newPureLeiosTxCache
  )
import LeiosVoting (EbClosureVerdict (..), validateEbClosure)
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Ledger.Basics (LedgerState)
import Ouroboros.Consensus.Ledger.Tables (projectLedgerTables)
import Ouroboros.Consensus.Ledger.Tables.MapKind (EmptyMK, ValuesMK)
import Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables)
import Test.Consensus.Mempool.Util
  ( TestBlock
  , TestTx
  , genInvalidTx
  , genValidTxs
  , testInitLedger
  , testLedgerConfigNoSizeLimits
  )
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Orphans.IOLike ()

tests :: TestTree
tests =
  testGroup
    "LeiosVoting"
    [ testGroup
        "validateEbClosure"
        [ testProperty "a fresh closure is validated in full" prop_freshClosureFullyApplied
        , testProperty "a second look reapplies instead" prop_secondLookReapplies
        , testProperty "records the txs validated before a failure" prop_recordsValidatedBeforeFailure
        , testProperty "a tx after the failure is not recorded" prop_stopsAtFirstFailure
        ]
    ]

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | Nothing in the cache yet, so every tx goes through the full 'applyTx'.
prop_freshClosureFullyApplied :: Property
prop_freshClosureFullyApplied =
  forAllValidClosure 4 $ \txs -> ioProperty $ do
    (verdict, tagged) <- runValidate txs
    pure $
      conjoin
        [ verdict === Valid 0 (length txs)
        , tagged === map (const True) txs
            & counterexample "every validated tx should be tagged"
        ]

-- | The tags the first pass left behind send the second pass down 'reapplyTx',
-- which is the whole point of consulting the cache.
prop_secondLookReapplies :: Property
prop_secondLookReapplies =
  forAllValidClosure 4 $ \txs -> ioProperty $ do
    (_, _, second) <- runValidateTwice txs
    pure $ second === Valid (length txs) 0

-- | A closure that fails part-way must still leave everything it validated
-- before the failure recorded: that work was done against a real ledger state
-- and holds regardless of the tx that sank the EB.
prop_recordsValidatedBeforeFailure :: Property
prop_recordsValidatedBeforeFailure =
  forAllValidThenInvalid 3 $ \good bad -> ioProperty $ do
    (verdict, tagged) <- runValidate (good <> [bad])
    pure $
      conjoin
        [ isInvalid verdict
            & counterexample ("expected an invalid verdict, got " <> show verdict)
        , take (length good) tagged === map (const True) good
            & counterexample "txs validated before the failure should be tagged"
        ]

-- | The failing tx itself is not recorded -- it never validated.
prop_stopsAtFirstFailure :: Property
prop_stopsAtFirstFailure =
  forAllValidThenInvalid 2 $ \good bad -> ioProperty $ do
    (_, tagged) <- runValidate (good <> [bad])
    pure $
      last tagged === False
        & counterexample "the failing tx must not be tagged validated"

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

forAllValidClosure :: Int -> ([TestTx] -> Property) -> Property
forAllValidClosure n k =
  forAll (fst <$> genValidTxs n testInitLedger) $ \txs ->
    length txs === n .&&. k txs

-- | @n@ txs that apply, then one that does not.
forAllValidThenInvalid :: Int -> ([TestTx] -> TestTx -> Property) -> Property
forAllValidThenInvalid n k =
  forAll (genValidTxs n testInitLedger) $ \(good, ledger') ->
    forAll (genInvalidTx ledger') $ \bad ->
      length good === n .&&. k good bad

{-------------------------------------------------------------------------------
  Harness
-------------------------------------------------------------------------------}

-- | 'EbClosureVerdict' is not 'Eq'/'Show' (its error type need not be), so
-- compare a projection of it.
data Verdict
  = -- | Reapplied and applied counts, in that order.
    Valid Int Int
  | Invalid
  deriving (Eq, Show)

summarise :: EbClosureVerdict blk -> Verdict
summarise = \case
  EbClosureValid r a -> Valid r a
  EbClosureInvalid _ -> Invalid

isInvalid :: Verdict -> Bool
isInvalid = \case
  Invalid -> True
  Valid{} -> False

-- | Run 'validateEbClosure' over a closure, returning its verdict and, per tx
-- in closure order, whether the cache now reports it validated.
runValidate :: [TestTx] -> IO (Verdict, [Bool])
runValidate txs = do
  withHarness txs $ \h -> validateOnce h txs

-- | As 'runValidate', but validates the same closure twice against the same
-- cache, so the second pass sees the first pass's tags.
runValidateTwice :: [TestTx] -> IO (Verdict, [Bool], Verdict)
runValidateTwice txs = withHarness txs $ \h -> do
  (first', tagged) <- validateOnce h txs
  (second', _) <- validateOnce h txs
  pure (first', tagged, second')

data Harness = Harness
  { hConn :: LeiosDbConnection IO
  , hCache :: LeiosTxCache IO () () SerializedEbBody
  , hPoint :: LeiosPoint
  }

validateOnce :: Harness -> [TestTx] -> IO (Verdict, [Bool])
validateOnce Harness{hConn, hCache, hPoint} txs = do
  verdict <-
    validateEbClosure
      testLedgerConfigNoSizeLimits
      hConn
      hCache
      (\_keys -> pure (projectLedgerTables testInitLedger))
      hPoint
      baseLedger
  tagged <- withLookupTx hCache $ \look ->
    mapM (\tx -> isValidated <$> look (txHashOf tx)) txs
  pure (summarise verdict, tagged)
 where
  isValidated = \case
    Just (Right ()) -> True
    _ -> False

-- | An in-memory LeiosDb holding the closure, and a cache that has seen the
-- announcement and body -- the state LeiosFetch would have left behind, since
-- 'withLockedInsertAppliedTx' only upgrades entries a body already created.
withHarness :: [TestTx] -> (Harness -> IO a) -> IO a
withHarness txs k = do
  db :: LeiosDbHandle IO <- newLeiosDBInMemory
  withLeiosDb db $ \conn -> do
    leiosDbInsertEbPoint conn point (leiosEbBytesSize eb)
    void $ leiosDbInsertEbBody conn point eb
    void $ leiosDbInsertTxs conn [(txHashOf tx, txBytes tx) | tx <- txs]

    cache <- newPureLeiosTxCache
    void $ insertAnnouncement cache (pointSlotNo point) rbHash (pointEbHash point)
    -- The fold over the not-yet-acquired txs is what a fetch would use to build
    -- its request; here only the refcount bump matters, so it folds into ().
    void $ insertBody cache (pointEbHash point) (serializeEbBody eb) () (\() _ _ _ -> ())
    -- Mark them acquired, as a fetch would: only then can they be upgraded.
    void $ withLockedInsertUnappliedTx cache $ \w0 step ->
      foldM (\w tx -> step w (txHashOf tx) (txBytesSize tx) ()) w0 txs

    k Harness{hConn = conn, hCache = cache, hPoint = point}
 where
  eb = ebOf txs
  point = MkLeiosPoint (SlotNo 1) (hashLeiosEb eb)
  rbHash = MkRbHash (BS.replicate 32 0)

baseLedger :: LedgerState TestBlock EmptyMK
baseLedger = forgetLedgerTables (testInitLedger :: LedgerState TestBlock ValuesMK)

-- | The LeiosDb keys txs by the hash of their stored bytes, so hash the same
-- bytes we store.
txBytes :: TestTx -> ByteString
txBytes = Lazy.toStrict . Serialise.serialise

txHashOf :: TestTx -> TxHash
txHashOf = hashLeiosTx . MkLeiosTx . txBytes

txBytesSize :: TestTx -> BytesSize
txBytesSize = fromIntegral . BS.length . txBytes

ebOf :: [TestTx] -> LeiosEb
ebOf txs =
  MkLeiosEb $
    V.fromList [(txHashOf tx, txBytesSize tx) | tx <- txs]
