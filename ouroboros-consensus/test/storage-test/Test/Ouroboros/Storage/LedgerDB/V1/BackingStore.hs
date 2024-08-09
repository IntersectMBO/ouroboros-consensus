{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{- HLINT ignore "Use camelCase" -}

module Test.Ouroboros.Storage.LedgerDB.V1.BackingStore (
    labelledExamples
  , tests
  ) where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Slotting.Slot
import           Control.Concurrent.Class.MonadMVar.Strict
import           Control.Concurrent.Class.MonadSTM.Strict.TVar
import           Control.Monad (void)
import           Control.Monad.Class.MonadThrow (Handler (..), catches)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.IOSim
import           Control.Monad.Reader (runReaderT)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.SOP.Dict as Dict
import           Data.Typeable
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Ledger.Tables.Utils
import qualified Ouroboros.Consensus.Ledger.Tables.UtxoDiff as Diff
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Args as BS
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore as BS
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.InMemory as InMemory
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB as LMDB
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.IOLike hiding (MonadMask (..),
                     newMVar, newTVarIO, readMVar)
import qualified System.Directory as Dir
import           System.FS.API hiding (Handle)
import           System.FS.IO (ioHasFS)
import qualified System.FS.Sim.MockFS as MockFS
import           System.FS.Sim.STM
import           System.IO.Temp (createTempDirectory)
import           Test.Ouroboros.Storage.LedgerDB.V1.BackingStore.Lockstep
import qualified Test.Ouroboros.Storage.LedgerDB.V1.BackingStore.Mock as Mock
import           Test.Ouroboros.Storage.LedgerDB.V1.BackingStore.Registry
import qualified Test.QuickCheck as QC
import           Test.QuickCheck (Arbitrary (..), Property, Testable)
import           "quickcheck-dynamic" Test.QuickCheck.Extras
import           Test.QuickCheck.Gen.Unsafe
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Monadic (PropertyM)
import           Test.QuickCheck.StateModel as StateModel
import           Test.QuickCheck.StateModel.Lockstep as Lockstep
import           Test.QuickCheck.StateModel.Lockstep.Run as Lockstep
import           Test.Tasty
import           Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)
import           Test.Util.LedgerStateOnlyTables
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Orphans.ToExpr ()
import           Test.Util.UtxoDiff ()

{-------------------------------------------------------------------------------
  Main test tree
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "BackingStore" [
    adjustOption (scaleQuickCheckTests 10) $
      testProperty "InMemory IOSim SimHasFS" testWithIOSim
  , adjustOption (scaleQuickCheckTests 10) $
      testProperty "InMemory IO SimHasFS" $ testWithIO $
        setupBSEnv BS.InMemoryBackingStoreArgs setupSimHasFS (pure ())
  , adjustOption (scaleQuickCheckTests 10) $
      testProperty "InMemory IO IOHasFS" $ testWithIO $ do
        (fp, cleanup) <- setupTempDir
        setupBSEnv BS.InMemoryBackingStoreArgs (setupIOHasFS fp) cleanup
  , adjustOption (scaleQuickCheckTests 2) $
      testProperty "LMDB IO IOHasFS" $ testWithIO $ do
        (fp, cleanup) <- setupTempDir
        setupBSEnv (BS.LMDBBackingStoreArgs testLMDBLimits Dict.Dict) (setupIOHasFS fp) cleanup
  ]

scaleQuickCheckTests :: Int -> QuickCheckTests -> QuickCheckTests
scaleQuickCheckTests c (QuickCheckTests n) = QuickCheckTests $ c * n

testLMDBLimits :: LMDB.LMDBLimits
testLMDBLimits = LMDB.LMDBLimits
  { -- 100 MiB should be more than sufficient for the tests we're running here.
    -- If the database were to grow beyond 100 Mebibytes, resulting in a test
    -- error, then something in the LMDB backing store or tests has changed and
    -- we should reconsider this value.
    LMDB.lmdbMapSize = 100 * 1024 * 1024
    -- 3 internal databases: 1 for the settings, 1 for the state, and 1 for the
    -- ledger tables.
  , LMDB.lmdbMaxDatabases = 3

  , LMDB.lmdbMaxReaders = maxOpenValueHandles
  }

testWithIOSim :: Actions (Lockstep (BackingStoreState K V D)) -> Property
testWithIOSim acts = monadicSim $ do
  BSEnv {bsRealEnv, bsCleanup} <-
    QC.run (setupBSEnv BS.InMemoryBackingStoreArgs setupSimHasFS (pure ()))
  void $
    runPropertyIOLikeMonad $
      runPropertyReaderT (StateModel.runActions acts) bsRealEnv
  QC.run bsCleanup
  pure True

testWithIO::
     IO (BSEnv IO K V D)
  -> Actions (Lockstep T) -> Property
testWithIO mkBSEnv = runActionsBracket pT mkBSEnv bsCleanup runner

runner ::
     RealMonad m ks vs d a
  -> BSEnv m ks vs d
  -> m a
runner c r = unIOLikeMonad . runReaderT c $ bsRealEnv r

-- | Generate minimal examples for each label.
labelledExamples :: IO ()
labelledExamples = do
  -- TODO: the thread delay ensures that we do not start printing labelled
  -- exampes throughout other test output, but it is not a very nice solution.
  -- We should find a better alternative.
  threadDelay 1
  QC.labelledExamples $ tagActions pT

{-------------------------------------------------------------------------------
  Resources
-------------------------------------------------------------------------------}

data BSEnv m ks vs d = BSEnv {
    bsRealEnv :: RealEnv m ks vs d
  , bsCleanup :: m ()
  }

-- | Set up a simulated @'HasFS'@.
setupSimHasFS :: IOLike m => m (SomeHasFS m)
setupSimHasFS = SomeHasFS . simHasFS <$> newTVarIO MockFS.empty

-- | Set up a @'HasFS'@ for @'IO'@.
setupIOHasFS :: MonadIO m => FilePath -> m (SomeHasFS m)
setupIOHasFS = pure . SomeHasFS . ioHasFS . MountPoint

-- | In case we are running tests in @'IO'@, we must do some temporary directory
-- management.
setupTempDir :: MonadIO m => m (FilePath, m ())
setupTempDir = do
  sysTmpDir <- liftIO Dir.getTemporaryDirectory
  qsmTmpDir <- liftIO $ createTempDirectory sysTmpDir "BS_QSM"
  pure (qsmTmpDir, liftIO $ Dir.removeDirectoryRecursive qsmTmpDir)

setupBSEnv ::
     IOLike m
  => Complete BS.BackingStoreArgs m
  -> m (SomeHasFS m)
  -> m ()
  -> m (BSEnv m K V D)
setupBSEnv bss mkSfhs cleanup = do
  sfhs@(SomeHasFS hfs) <- mkSfhs

  createDirectory hfs (mkFsPath ["copies"])

  let bsi = BS.newBackingStoreInitialiser mempty bss

  bsVar <- newMVar =<< bsi sfhs sfhs (BS.InitFromValues Origin emptyLedgerTables)

  rr <- initHandleRegistry

  let
    bsCleanup = do
      bs <- readMVar bsVar
      catches (BS.bsClose bs) closeHandlers
      cleanup

  pure BSEnv {
      bsRealEnv = RealEnv {
          reSomeHasFS = sfhs
        , reBackingStoreInit = bsi
        , reBackingStore = bsVar
        , reRegistry = rr
        }
    , bsCleanup
    }

-- | A backing store will throw an error on close if it has already been closed,
-- which we ignore if we are performing a close as part of resource cleanup.
closeHandlers :: IOLike m => [Handler m ()]
closeHandlers = [
    Handler $ \case
      InMemory.InMemoryBackingStoreClosedExn -> pure ()
      e                                  -> throwIO e
  , Handler $ \case
      LMDB.LMDBErrClosed -> pure ()
      e                -> throwIO e
  ]

{-------------------------------------------------------------------------------
  Types under test
-------------------------------------------------------------------------------}

type T = BackingStoreState K V D

pT :: Proxy T
pT = Proxy

type K = LedgerTables (OTLedgerState (Fixed Word) (Fixed Word)) KeysMK
type V = LedgerTables (OTLedgerState (Fixed Word) (Fixed Word)) ValuesMK
type D = LedgerTables (OTLedgerState (Fixed Word) (Fixed Word)) DiffMK

{-------------------------------------------------------------------------------
  @'HasOps'@ instances
-------------------------------------------------------------------------------}

instance Mock.EmptyValues V where
  emptyValues = emptyLedgerTables

instance Mock.ApplyDiff V D where
  applyDiff = applyDiffs'

instance Mock.LookupKeysRange K V where
  lookupKeysRange = \prev n vs ->
      case prev of
        Nothing ->
          ltmap (rangeRead n) vs
        Just ks ->
          ltliftA2 (rangeRead' n) ks vs
    where
      rangeRead :: Int -> ValuesMK k v -> ValuesMK k v
      rangeRead n (ValuesMK vs) =
        ValuesMK $ Map.take n vs

      rangeRead' ::
          Ord k
        => Int
        -> KeysMK k v
        -> ValuesMK k v
        -> ValuesMK k v
      rangeRead' n ksmk vsmk =
          case Set.lookupMax ks of
            Nothing -> ValuesMK Map.empty
            Just  k -> ValuesMK $
              Map.take n $ snd $ Map.split k vs
        where
          KeysMK ks   = ksmk
          ValuesMK vs = vsmk

instance Mock.LookupKeys K V where
  lookupKeys = ltliftA2 readKeys
    where
      readKeys ::
          Ord k
        => KeysMK k v
        -> ValuesMK k v
        -> ValuesMK k v
      readKeys (KeysMK ks) (ValuesMK vs) =
        ValuesMK $ Map.restrictKeys vs ks

instance Mock.ValuesLength V where
  valuesLength (LedgerTables (ValuesMK m)) =
    Map.size m

instance Mock.MakeDiff V D where
  diff t1 t2 = forgetTrackingValues $ calculateDifference t1 t2

instance Mock.DiffSize D where
  diffSize (LedgerTables (DiffMK d)) = Diff.size d

instance Mock.KeysSize K where
  keysSize (LedgerTables (KeysMK s)) = Set.size s

instance Mock.HasOps K V D

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

runPropertyIOLikeMonad ::
     IOLikeMonadC m
  => PropertyM (IOLikeMonad m) a
  -> PropertyM m a
runPropertyIOLikeMonad p = QC.MkPropertyM $ \k -> do
  m <- QC.unPropertyM p $ fmap ioLikeMonad . k
  return $ unIOLikeMonad m

-- | Copied from @Ouroboros.Network.Testing.QuickCheck@.
runSimGen :: (forall s. QC.Gen (IOSim s a)) -> QC.Gen a
runSimGen f = do
    Capture eval <- capture
    return $ runSimOrThrow (eval f)

-- | Copied from @Ouroboros.Network.Testing.QuickCheck@.
monadicSim :: Testable a => (forall s. PropertyM (IOSim s) a) -> Property
monadicSim m = QC.property (runSimGen (QC.monadic' m))

{-------------------------------------------------------------------------------
  Orphan Arbitrary instances
-------------------------------------------------------------------------------}

deriving newtype instance QC.Arbitrary (mk k v)
                       => QC.Arbitrary (OTLedgerTables k v mk)

instance (Ord k, QC.Arbitrary k)
      => QC.Arbitrary (KeysMK k v) where
  arbitrary = KeysMK <$> QC.arbitrary
  shrink (KeysMK ks) = KeysMK <$> QC.shrink ks

instance (Ord k, QC.Arbitrary k, QC.Arbitrary v)
      => QC.Arbitrary (DiffMK k v) where
  arbitrary = DiffMK <$> QC.arbitrary
  shrink (DiffMK d) = DiffMK <$> QC.shrink d

instance (Ord k, QC.Arbitrary k, QC.Arbitrary v)
      => QC.Arbitrary (ValuesMK k v) where
  arbitrary = ValuesMK <$> QC.arbitrary
  shrink (ValuesMK vs) = ValuesMK <$> QC.shrink vs

instance QC.Arbitrary ks => QC.Arbitrary (BS.RangeQuery ks) where
  arbitrary = BS.RangeQuery <$> QC.arbitrary <*> QC.arbitrary
  shrink (BS.RangeQuery x y) = BS.RangeQuery <$> QC.shrink x <*> QC.shrink y

newtype Fixed a = Fixed a
  deriving newtype (Show, Eq, Ord)
  deriving newtype (NoThunks, ToCBOR, FromCBOR)

deriving via QC.Fixed a instance QC.Arbitrary a => QC.Arbitrary (Fixed a)
