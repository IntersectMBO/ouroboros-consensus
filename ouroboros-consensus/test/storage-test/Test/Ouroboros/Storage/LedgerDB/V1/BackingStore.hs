{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{- HLINT ignore "Use camelCase" -}

module Test.Ouroboros.Storage.LedgerDB.V1.BackingStore (
    labelledExamples
  , tests
  ) where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Slotting.Slot
import           Control.Concurrent.Class.MonadMVar.Strict
import           Control.Concurrent.Class.MonadSTM.Strict.TMVar
import           Control.Monad.Class.MonadThrow (Handler (..), catches)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader (runReaderT)
import qualified Data.Map.Strict as Map
import           Data.MemPack
import qualified Data.Set as Set
import qualified Data.SOP.Dict as Dict
import           Data.Typeable
import           Ouroboros.Consensus.Ledger.Tables
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import           Ouroboros.Consensus.Ledger.Tables.Utils
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.Args as BS
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore as BS
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.InMemory as InMemory
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB as LMDB
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.IOLike hiding (MonadMask (..),
                     newMVar, newTVarIO, readMVar)
import qualified System.Directory as Dir
import qualified System.FilePath as FilePath
import           System.FS.API hiding (Handle)
import           System.FS.IO (ioHasFS)
import qualified System.FS.Sim.MockFS as MockFS
import           System.FS.Sim.STM
import           System.IO.Temp (createTempDirectory)
import           Test.Ouroboros.Storage.LedgerDB.V1.BackingStore.Lockstep
import qualified Test.Ouroboros.Storage.LedgerDB.V1.BackingStore.Mock as Mock
import           Test.Ouroboros.Storage.LedgerDB.V1.LMDB (testLMDBLimits)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck (Arbitrary (..), Property)
import           Test.QuickCheck.StateModel as StateModel
import           Test.QuickCheck.StateModel.Lockstep as Lockstep
import           Test.QuickCheck.StateModel.Lockstep.Run as Lockstep
import           Test.Tasty
import           Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)
import           Test.Util.LedgerStateOnlyTables
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Orphans.ToExpr ()

{-------------------------------------------------------------------------------
  Main test tree
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "BackingStore" [
    adjustOption (scaleQuickCheckTests 10) $
      testProperty "InMemory IO SimHasFS" $ testWithIO $
        setupBSEnv BS.InMemoryBackingStoreArgs setupSimHasFS (pure ())
  , adjustOption (scaleQuickCheckTests 10) $
      testProperty "InMemory IO IOHasFS" $ testWithIO $ do
        (fp, cleanup) <- setupTempDir
        setupBSEnv BS.InMemoryBackingStoreArgs (setupIOHasFS fp) cleanup
  , adjustOption (scaleQuickCheckTests 2) $
      testProperty "LMDB IO IOHasFS" $ testWithIO $ do
        (fp, cleanup) <- setupTempDir
        lmdbTmpDir <- (FilePath.</> "BS_LMDB") <$> Dir.getTemporaryDirectory
        setupBSEnv (BS.LMDBBackingStoreArgs lmdbTmpDir (testLMDBLimits maxOpenValueHandles) Dict.Dict) (setupIOHasFS fp) (cleanup >> Dir.removeDirectoryRecursive lmdbTmpDir)
  ]

scaleQuickCheckTests :: Int -> QuickCheckTests -> QuickCheckTests
scaleQuickCheckTests c (QuickCheckTests n) = QuickCheckTests $ c * n

testWithIO ::
     IO (BSEnv IO K V D)
  -> Actions (Lockstep T) -> Property
testWithIO mkBSEnv = runActionsBracket pT mkBSEnv bsCleanup runner

runner ::
     RealMonad m ks vs d (OTLedgerState (QC.Fixed Word) (QC.Fixed Word)) a
  -> BSEnv m ks vs d
  -> m a
runner c r = runReaderT c $ bsRealEnv r

-- | Generate minimal examples for each label.
labelledExamples :: IO ()
labelledExamples = QC.labelledExamples $ tagActions pT

{-------------------------------------------------------------------------------
  Resources
-------------------------------------------------------------------------------}

data BSEnv m ks vs d = BSEnv {
    bsRealEnv :: RealEnv m ks vs d (OTLedgerState (QC.Fixed Word) (QC.Fixed Word))
  , bsCleanup :: m ()
  }

-- | Set up a simulated @'HasFS'@.
setupSimHasFS :: IOLike m => m (SomeHasFS m)
setupSimHasFS = SomeHasFS . simHasFS <$> newTMVarIO MockFS.empty

-- | Set up a @'HasFS'@ for @'IO'@.
setupIOHasFS :: (PrimState m ~ PrimState IO, MonadIO m) => FilePath -> m (SomeHasFS m)
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
setupBSEnv mkBsArgs mkShfs cleanup = do
  shfs@(SomeHasFS hfs) <- mkShfs

  createDirectory hfs (mkFsPath ["copies"])

  let bsi = BS.newBackingStoreInitialiser mempty mkBsArgs (BS.SnapshotsFS shfs)

  bsVar <- newMVar =<< bsi (BS.InitFromValues Origin (OTLedgerState emptyMK emptyLedgerTables) emptyLedgerTables)

  let
    bsCleanup = do
      bs <- readMVar bsVar
      catches (BS.bsClose bs) closeHandlers
      cleanup

  pure BSEnv {
      bsRealEnv = RealEnv {
          reBackingStoreInit = bsi
        , reBackingStore = bsVar
        }
    , bsCleanup
    }

-- | A backing store will throw an error on close if it has already been closed,
-- which we ignore if we are performing a close as part of resource cleanup.
closeHandlers :: IOLike m => [Handler m ()]
closeHandlers = [
    Handler $ \case
      InMemory.InMemoryBackingStoreClosedExn -> pure ()
      e                                      -> throwIO e
  , Handler $ \case
      LMDB.LMDBErrClosed -> pure ()
      e                  -> throwIO e
  ]

{-------------------------------------------------------------------------------
  Types under test
-------------------------------------------------------------------------------}

type T = BackingStoreState K V D (OTLedgerState (QC.Fixed Word) (QC.Fixed Word))

pT :: Proxy T
pT = Proxy

type K = LedgerTables (OTLedgerState (QC.Fixed Word) (QC.Fixed Word)) KeysMK
type V = LedgerTables (OTLedgerState (QC.Fixed Word) (QC.Fixed Word)) ValuesMK
type D = LedgerTables (OTLedgerState (QC.Fixed Word) (QC.Fixed Word)) DiffMK

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
  diff t1 t2 = trackingToDiffs $ calculateDifference t1 t2

instance Mock.DiffSize D where
  diffSize (LedgerTables (DiffMK (Diff.Diff m))) = Map.size m

instance Mock.KeysSize K where
  keysSize (LedgerTables (KeysMK s)) = Set.size s

instance Mock.HasOps K V D

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

deriving newtype instance (Ord k, QC.Arbitrary k, QC.Arbitrary v)
                       => QC.Arbitrary (Diff.Diff k v)
instance QC.Arbitrary v => QC.Arbitrary (Diff.Delta v) where
  arbitrary =
    QC.oneof [
        Diff.Insert <$> QC.arbitrary
      , pure Diff.Delete
      ]

instance QC.Arbitrary ks => QC.Arbitrary (BS.RangeQuery ks) where
  arbitrary = BS.RangeQuery <$> QC.arbitrary <*> QC.arbitrary
  shrink (BS.RangeQuery x y) = BS.RangeQuery <$> QC.shrink x <*> QC.shrink y

instance NoThunks a => NoThunks (QC.Fixed a) where
  wNoThunks ctxt = wNoThunks ctxt . QC.getFixed
  showTypeOf _ = "Fixed " ++ showTypeOf (Proxy @a)


deriving newtype instance MemPack a => MemPack (QC.Fixed a)
deriving newtype instance FromCBOR a => FromCBOR (QC.Fixed a)
deriving newtype instance ToCBOR a => ToCBOR (QC.Fixed a)
