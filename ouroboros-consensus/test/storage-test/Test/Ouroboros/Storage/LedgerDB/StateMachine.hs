{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

-- | On-disk ledger DB tests.
--
-- This is a state-machine based test. The commands here are
--
-- * Get the current volatile and immutable tip
-- * Switch to a fork (possibly rolling back 0 blocks, so equivalent to a push)
-- * Write a snapshot to disk
-- * Restore the ledger DB from the snapshots on disk
-- * Model disk corruption (truncate or delete latest snapshot)
--
-- The model here is satisfyingly simple: just a map from blocks to their
-- corresponding ledger state modelling the whole block chain since genesis.
module Test.Ouroboros.Storage.LedgerDB.StateMachine (tests) where

import qualified Cardano.Ledger.BaseTypes as BT
import qualified Control.Monad as Monad
import Control.Monad.Except
import Control.Monad.State hiding (state)
import Control.ResourceRegistry
import Control.Tracer (Tracer (..))
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.SOP.Dict as Dict
import Data.Word
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Tables.Utils
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import Ouroboros.Consensus.Storage.ImmutableDB.Stream
import Ouroboros.Consensus.Storage.LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.V1 as V1
import Ouroboros.Consensus.Storage.LedgerDB.V1.Args hiding
  ( LedgerDbFlavorArgs
  )
import Ouroboros.Consensus.Storage.LedgerDB.V2 as V2
import Ouroboros.Consensus.Storage.LedgerDB.V2.Args hiding
  ( LedgerDbFlavorArgs
  )
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Args as V2
import Ouroboros.Consensus.Util hiding (Some)
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Network.AnchoredSeq as AS
import qualified System.Directory as Dir
import System.FS.API
import qualified System.FS.IO as FSIO
import qualified System.FS.Sim.MockFS as MockFS
import System.FS.Sim.STM
import qualified System.FilePath as FilePath
import qualified System.IO.Temp as Temp
import Test.Ouroboros.Storage.LedgerDB.StateMachine.TestBlock
import Test.Ouroboros.Storage.LedgerDB.V1.LMDB
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import Test.QuickCheck.StateModel
import Test.Tasty
import Test.Tasty.QuickCheck hiding (Some (..))
import Test.Util.TestBlock hiding
  ( TestBlock
  , TestBlockCodecConfig
  , TestBlockStorageConfig
  )
import "quickcheck-dynamic" Test.QuickCheck.Extras

tests :: TestTree
tests =
  testGroup
    "StateMachine"
    [ testProperty "InMemV1" $
        prop_sequential 100000 inMemV1TestArguments noFilePath simulatedFS
    , testProperty "InMemV2" $
        prop_sequential 100000 inMemV2TestArguments noFilePath simulatedFS
    , testProperty "LMDB" $
        prop_sequential 1000 lmdbTestArguments realFilePath realFS
    ]

prop_sequential ::
  Int ->
  (SecurityParam -> FilePath -> TestArguments IO) ->
  IO (FilePath, IO ()) ->
  IO (SomeHasFS IO, IO ()) ->
  Actions Model ->
  QC.Property
prop_sequential maxSuccess mkTestArguments getLmdbDir fsOps as = QC.withMaxSuccess maxSuccess $
  QC.monadicIO $ do
    ref <- lift $ initialEnvironment fsOps getLmdbDir mkTestArguments =<< initChainDB
    (_, env@(Environment _ testInternals _ _ _ _ clean)) <- runPropertyStateT (runActions as) ref
    checkNoLeakedHandles env
    QC.run $ closeLedgerDB testInternals >> clean
    QC.assert True

-- | The initial environment is mostly undefined because it will be initialized
-- by the @Init@ command. We are forced to provide this dummy implementation
-- because some parts of it are static (which we can provide now) and also the
-- empty sequence of commands must still run the cleanup functions, which here
-- are trivial, but nevertheless they have to exist.
initialEnvironment ::
  IO (SomeHasFS IO, IO ()) ->
  IO (FilePath, IO ()) ->
  (SecurityParam -> FilePath -> TestArguments IO) ->
  ChainDB IO ->
  IO Environment
initialEnvironment fsOps getLmdbDir mkTestArguments cdb = do
  (sfs, cleanupFS) <- fsOps
  (lmdbDir, cleanupLMDB) <- getLmdbDir
  pure $
    Environment
      undefined
      (TestInternals undefined undefined undefined undefined undefined (pure ()) (pure 0))
      cdb
      (flip mkTestArguments lmdbDir)
      sfs
      (pure $ NumOpenHandles 0)
      (cleanupFS >> cleanupLMDB)

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

data TestArguments m = TestArguments
  { argFlavorArgs :: !(Complete LedgerDbFlavorArgs m)
  , argLedgerDbCfg :: !(LedgerDbCfg (ExtLedgerState TestBlock))
  }

noFilePath :: IO (FilePath, IO ())
noFilePath = pure ("Bogus", pure ())

realFilePath :: IO (FilePath, IO ())
realFilePath = liftIO $ do
  tmpdir <- (FilePath.</> "test_lmdb") <$> Dir.getTemporaryDirectory
  pure
    ( tmpdir
    , do
        exists <- Dir.doesDirectoryExist tmpdir
        Monad.when exists $ Dir.removeDirectoryRecursive tmpdir
    )

simulatedFS :: IO (SomeHasFS IO, IO ())
simulatedFS = do
  fs <- simHasFS' MockFS.empty
  pure (SomeHasFS fs, pure ())

realFS :: IO (SomeHasFS IO, IO ())
realFS = liftIO $ do
  systmpdir <- Dir.getTemporaryDirectory
  tmpdir <- Temp.createTempDirectory systmpdir "init_standalone_db"
  pure (SomeHasFS $ FSIO.ioHasFS $ MountPoint tmpdir, Dir.removeDirectoryRecursive tmpdir)

inMemV1TestArguments ::
  SecurityParam ->
  FilePath ->
  TestArguments IO
inMemV1TestArguments secParam _ =
  TestArguments
    { argFlavorArgs = LedgerDbFlavorArgsV1 $ V1Args DisableFlushing InMemoryBackingStoreArgs
    , argLedgerDbCfg = extLedgerDbConfig secParam
    }

inMemV2TestArguments ::
  SecurityParam ->
  FilePath ->
  TestArguments IO
inMemV2TestArguments secParam _ =
  TestArguments
    { argFlavorArgs = LedgerDbFlavorArgsV2 $ V2Args InMemoryHandleArgs
    , argLedgerDbCfg = extLedgerDbConfig secParam
    }

lmdbTestArguments ::
  SecurityParam ->
  FilePath ->
  TestArguments IO
lmdbTestArguments secParam fp =
  TestArguments
    { argFlavorArgs =
        LedgerDbFlavorArgsV1 $
          V1Args DisableFlushing $
            LMDBBackingStoreArgs fp (testLMDBLimits 16) Dict.Dict
    , argLedgerDbCfg = extLedgerDbConfig secParam
    }

{-------------------------------------------------------------------------------
 Model
-------------------------------------------------------------------------------}

type TheBlockChain =
  AS.AnchoredSeq
    (WithOrigin SlotNo)
    (ExtLedgerState TestBlock ValuesMK)
    (TestBlock, ExtLedgerState TestBlock ValuesMK)

data Model
  = UnInit
  | Model
      TheBlockChain
      SecurityParam
  deriving (Generic, Show)

instance
  AS.Anchorable
    (WithOrigin SlotNo)
    (ExtLedgerState TestBlock ValuesMK)
    (TestBlock, ExtLedgerState TestBlock ValuesMK)
  where
  asAnchor = snd
  getAnchorMeasure _ = getTipSlot

instance HasVariables TheBlockChain where
  getAllVariables _ = mempty

modelUpdateLedger ::
  StateT
    TheBlockChain
    (Except (ExtValidationError TestBlock))
    a ->
  Model ->
  Model
modelUpdateLedger f model@(Model chain secParam) =
  case runExcept (runStateT f chain) of
    Left{} -> model
    Right (_, ledger') -> Model ledger' secParam
modelUpdateLedger _ _ = error "Uninitialized model tried to apply blocks!"

modelRollback :: Word64 -> Model -> Model
modelRollback n (Model chain secParam) =
  Model (AS.dropNewest (fromIntegral n) chain) secParam
modelRollback _ UnInit = error "Uninitialized model can't rollback!"

{-------------------------------------------------------------------------------
  StateModel
-------------------------------------------------------------------------------}

deriving instance Show (Action Model a)
deriving instance Eq (Action Model a)

instance HasVariables (Action Model a) where
  getAllVariables _ = mempty

instance HasVariables (BT.NonZero Word64) where
  getAllVariables _ = mempty

instance StateModel Model where
  data Action Model a where
    WipeLedgerDB :: Action Model ()
    TruncateSnapshots :: Action Model ()
    DropAndRestore :: Word64 -> Action Model ()
    ForceTakeSnapshot :: Action Model ()
    GetState ::
      Action Model (ExtLedgerState TestBlock EmptyMK, ExtLedgerState TestBlock EmptyMK)
    Init :: SecurityParam -> Action Model ()
    ValidateAndCommit :: Word64 -> [TestBlock] -> Action Model ()

  actionName WipeLedgerDB{} = "WipeLedgerDB"
  actionName TruncateSnapshots{} = "TruncateSnapshots"
  actionName DropAndRestore{} = "DropAndRestore"
  actionName ForceTakeSnapshot = "TakeSnapshot"
  actionName GetState{} = "GetState"
  actionName Init{} = "Init"
  actionName ValidateAndCommit{} = "ValidateAndCommit"

  arbitraryAction _ UnInit = Some . Init <$> QC.arbitrary
  arbitraryAction _ model@(Model chain secParam) =
    frequency $
      [ (2, pure $ Some GetState)
      , (2, pure $ Some ForceTakeSnapshot)
      , (1, Some . DropAndRestore <$> QC.choose (0, fromIntegral $ AS.length chain))
      ,
        ( 4
        , Some <$> do
            let maxRollback =
                  min
                    (fromIntegral . AS.length $ chain)
                    (BT.unNonZero $ maxRollbacks secParam)
            numRollback <- QC.choose (0, maxRollback)
            numNewBlocks <- QC.choose (numRollback, numRollback + 2)
            let
              chain' = case modelRollback numRollback model of
                UnInit -> error "Impossible"
                Model ch _ -> ch
              blocks =
                genBlocks
                  numNewBlocks
                  (lastAppliedPoint . ledgerState . either id snd . AS.head $ chain')
            return $ ValidateAndCommit numRollback blocks
        )
      , (1, pure $ Some WipeLedgerDB)
      , (1, pure $ Some TruncateSnapshots)
      ]

  initialState = UnInit

  nextState _ (Init secParam) _var = Model (AS.Empty genesis) secParam
  nextState state GetState _var = state
  nextState state ForceTakeSnapshot _var = state
  nextState state@(Model _ secParam) (ValidateAndCommit n blks) _var =
    modelUpdateLedger switch state
   where
    push ::
      TestBlock ->
      StateT
        ( AS.AnchoredSeq
            (WithOrigin SlotNo)
            (ExtLedgerState TestBlock ValuesMK)
            (TestBlock, ExtLedgerState TestBlock ValuesMK)
        )
        (Except (ExtValidationError TestBlock))
        ()
    push b = do
      ls <- get
      let tip = either id snd $ AS.head ls
      l' <- lift $ tickThenApply OmitLedgerEvents (ledgerDbCfg $ extLedgerDbConfig secParam) b tip
      put (ls AS.:> (b, applyDiffs tip l'))

    switch ::
      StateT
        ( AS.AnchoredSeq
            (WithOrigin SlotNo)
            (ExtLedgerState TestBlock ValuesMK)
            (TestBlock, ExtLedgerState TestBlock ValuesMK)
        )
        (Except (ExtValidationError TestBlock))
        ()
    switch = do
      modify $ AS.dropNewest (fromIntegral n)
      mapM_ push blks
  nextState state WipeLedgerDB _var = state
  nextState state TruncateSnapshots _var = state
  nextState state (DropAndRestore n) _var = modelRollback n state
  nextState UnInit _ _ = error "Uninitialized model created a command different than Init"

  precondition UnInit Init{} = True
  precondition UnInit _ = False
  precondition (Model chain secParam) (ValidateAndCommit n blks) =
    n <= min (BT.unNonZero $ maxRollbacks secParam) (fromIntegral $ AS.length chain)
      && case blks of
        [] -> True
        (b : _) -> tbSlot b == 1 + fromWithOrigin 0 (getTipSlot (AS.headAnchor chain'))
         where
          chain' = AS.dropNewest (fromIntegral n) chain
  precondition _ Init{} = False
  precondition _ _ = True

{-------------------------------------------------------------------------------
  Mocked ChainDB
-------------------------------------------------------------------------------}

-- | Mocked chain db
data ChainDB m = ChainDB
  { dbBlocks :: StrictTVar m (Map (RealPoint TestBlock) TestBlock)
  -- ^ Block storage
  , dbChain :: StrictTVar m [RealPoint TestBlock]
  -- ^ Current chain and corresponding ledger state
  }

initChainDB ::
  forall m.
  (MonadIO m, IOLike m) =>
  m (ChainDB m)
initChainDB = do
  dbBlocks <- uncheckedNewTVarM Map.empty
  dbChain <- uncheckedNewTVarM []
  return $ ChainDB dbBlocks dbChain

dbStreamAPI ::
  forall m.
  IOLike m =>
  SecurityParam ->
  ChainDB m ->
  m (StreamAPI m TestBlock TestBlock, [TestBlock])
dbStreamAPI secParam chainDb =
  atomically $ do
    points <- reverse . take (fromIntegral $ BT.unNonZero $ maxRollbacks secParam) <$> readTVar dbChain
    blks <- readTVar dbBlocks
    pure $ (StreamAPI streamAfter, map (blks Map.!) points)
 where
  ChainDB
    { dbBlocks
    , dbChain
    } = chainDb

  streamAfter ::
    Point TestBlock ->
    (Either (RealPoint TestBlock) (m (NextItem TestBlock)) -> m a) ->
    m a
  streamAfter tip k = do
    pts <-
      atomically $
        reverse . drop (fromIntegral $ BT.unNonZero $ maxRollbacks secParam) <$> readTVar dbChain
    case tip' of
      NotOrigin pt
        | pt `L.notElem` pts ->
            k $ Left pt
      _otherwise ->
        do
          toStream <- uncheckedNewTVarM (blocksToStream tip' pts)
          k (Right (getNext toStream))
   where
    tip' = pointToWithOriginRealPoint tip

  -- Blocks to stream
  --
  -- Precondition: tip must be on the current chain
  blocksToStream ::
    WithOrigin (RealPoint TestBlock) ->
    [RealPoint TestBlock] ->
    [RealPoint TestBlock]
  blocksToStream Origin = id
  blocksToStream (NotOrigin r) = tail . dropWhile (/= r)

  getNext :: StrictTVar m [RealPoint TestBlock] -> m (NextItem TestBlock)
  getNext toStream = do
    mr <- atomically $ do
      rs <- readTVar toStream
      case rs of
        [] -> return Nothing
        r : rs' -> writeTVar toStream rs' >> return (Just r)
    case mr of
      Nothing -> return NoMoreItems
      Just r -> do
        mb <- atomically $ Map.lookup r <$> readTVar dbBlocks
        case mb of
          Just b -> return $ NextItem b
          Nothing -> error blockNotFound

blockNotFound :: String
blockNotFound =
  concat
    [ "dbStreamAPI: "
    , "invariant violation: "
    , "block in dbChain not present in dbBlocks"
    ]

{-------------------------------------------------------------------------------
  New SUT
-------------------------------------------------------------------------------}

openLedgerDB ::
  Complete LedgerDbFlavorArgs IO ->
  ChainDB IO ->
  LedgerDbCfg (ExtLedgerState TestBlock) ->
  SomeHasFS IO ->
  IO (LedgerDB' IO TestBlock, TestInternals' IO TestBlock, IO NumOpenHandles)
openLedgerDB flavArgs env cfg fs = do
  (stream, volBlocks) <- dbStreamAPI (ledgerDbCfgSecParam cfg) env
  let getBlock f = Map.findWithDefault (error blockNotFound) f <$> readTVarIO (dbBlocks env)
  replayGoal <- fmap (realPointToPoint . last . Map.keys) . atomically $ readTVar (dbBlocks env)
  rr <- unsafeNewRegistry
  (tracer, getNumOpenHandles) <- mkTrackOpenHandles
  let args =
        LedgerDbArgs
          (SnapshotPolicyArgs DisableSnapshots DefaultNumOfDiskSnapshots)
          (pure genesis)
          fs
          cfg
          tracer
          flavArgs
          rr
          DefaultQueryBatchSize
          Nothing
  (ldb, _, od) <- case flavArgs of
    LedgerDbFlavorArgsV1 bss ->
      let initDb =
            V1.mkInitDb
              args
              bss
              getBlock
       in openDBInternal args initDb stream replayGoal
    LedgerDbFlavorArgsV2 bss ->
      let initDb =
            V2.mkInitDb
              args
              bss
              getBlock
       in openDBInternal args initDb stream replayGoal
  withRegistry $ \reg -> do
    vr <- validateFork ldb reg (const $ pure ()) BlockCache.empty 0 (map getHeader volBlocks)
    case vr of
      ValidateSuccessful forker -> do
        atomically (forkerCommit forker)
        forkerClose forker
      _ -> error "Couldn't restart the chain, failed to apply volatile blocks!"
  pure (ldb, od, getNumOpenHandles)

{-------------------------------------------------------------------------------
  RunModel
-------------------------------------------------------------------------------}

-- | The environment for the monad in which we will run the test
data Environment
  = Environment
      (LedgerDB' IO TestBlock)
      (TestInternals' IO TestBlock)
      (ChainDB IO)
      (SecurityParam -> TestArguments IO)
      (SomeHasFS IO)
      (IO NumOpenHandles)
      (IO ())

instance RunModel Model (StateT Environment IO) where
  perform _ (Init secParam) _ = do
    Environment _ _ chainDb mkArgs fs _ cleanup <- get
    (ldb, testInternals, getNumOpenHandles) <- lift $ do
      let args = mkArgs secParam
      openLedgerDB (argFlavorArgs args) chainDb (argLedgerDbCfg args) fs
    put (Environment ldb testInternals chainDb mkArgs fs getNumOpenHandles cleanup)
  perform _ WipeLedgerDB _ = do
    Environment _ testInternals _ _ _ _ _ <- get
    lift $ wipeLedgerDB testInternals
  perform _ GetState _ = do
    Environment ldb _ _ _ _ _ _ <- get
    lift $ atomically $ (,) <$> getImmutableTip ldb <*> getVolatileTip ldb
  perform _ ForceTakeSnapshot _ = do
    Environment _ testInternals _ _ _ _ _ <- get
    lift $ takeSnapshotNOW testInternals TakeAtImmutableTip Nothing
  perform _ (ValidateAndCommit n blks) _ = do
    Environment ldb _ chainDb _ _ _ _ <- get
    lift $ do
      atomically $
        modifyTVar (dbBlocks chainDb) $
          repeatedly (uncurry Map.insert) (map (\b -> (blockRealPoint b, b)) blks)
      withRegistry $ \rr -> do
        vr <- validateFork ldb rr (const $ pure ()) BlockCache.empty n (map getHeader blks)
        case vr of
          ValidateSuccessful forker -> do
            atomically $
              modifyTVar (dbChain chainDb) $
                (reverse (map blockRealPoint blks) ++) . drop (fromIntegral n)
            atomically (forkerCommit forker)
            forkerClose forker
          ValidateExceededRollBack{} -> error "Unexpected Rollback"
          ValidateLedgerError (AnnLedgerError forker _ _) -> forkerClose forker >> error "Unexpected ledger error"
  perform state@(Model _ secParam) (DropAndRestore n) lk = do
    Environment _ testInternals chainDb _ _ _ _ <- get
    lift $ do
      atomically $ modifyTVar (dbChain chainDb) (drop (fromIntegral n))
      closeLedgerDB testInternals
    perform state (Init secParam) lk
  perform _ TruncateSnapshots _ = do
    Environment _ testInternals _ _ _ _ _ <- get
    lift $ truncateSnapshots testInternals
  perform UnInit _ _ = error "Uninitialized model created a command different than Init"

  monitoring _ (ValidateAndCommit n _) _ _ = tabulate "Rollback depths" [show n]
  monitoring _ _ _ _ = id

  -- NOTE
  --
  -- In terms of postcondition, we only need to check that the immutable and
  -- volatile tip are the right ones. By the blocks validating one on top of
  -- each other it already implies that having the right volatile tip means that
  -- we have the right whole chain.
  postcondition (Model chain secParam, _) GetState _ (imm, vol) =
    let volSt = either forgetLedgerTables (forgetLedgerTables . snd) (AS.head chain)
        immSt =
          either
            forgetLedgerTables
            (forgetLedgerTables . snd)
            (AS.head (AS.dropNewest (fromIntegral $ BT.unNonZero $ maxRollbacks secParam) chain))
     in do
          counterexamplePost $
            unlines
              [ "VolSt: "
              , show volSt
              , "VolSut: "
              , show vol
              , "ImmSt: "
              , show immSt
              , "ImmSut: "
              , show imm
              ]
          pure $ volSt == vol && immSt == imm
  postcondition _ _ _ _ = pure True

{-------------------------------------------------------------------------------
  Additional checks
-------------------------------------------------------------------------------}

newtype NumOpenHandles = NumOpenHandles Word64
  deriving newtype (Show, Eq, Enum)

mkTrackOpenHandles :: IO (Tracer IO (TraceEvent TestBlock), IO NumOpenHandles)
mkTrackOpenHandles = do
  varOpen <- uncheckedNewTVarM (NumOpenHandles 0)
  let tracer = Tracer $ \case
        LedgerDBFlavorImplEvent (FlavorImplSpecificTraceV2 ev) ->
          atomically $ modifyTVar varOpen $ case ev of
            V2.TraceLedgerTablesHandleCreate -> succ
            V2.TraceLedgerTablesHandleClose -> pred
        _ -> pure ()
  pure (tracer, readTVarIO varOpen)

-- | Check that we didn't leak any 'LedgerTablesHandle's (with V2 only).
checkNoLeakedHandles :: Environment -> QC.PropertyM IO ()
checkNoLeakedHandles (Environment _ testInternals _ _ _ getNumOpenHandles _) = do
  expected <- liftIO $ NumOpenHandles <$> LedgerDB.getNumLedgerTablesHandles testInternals
  actual <- liftIO getNumOpenHandles
  QC.assertWith (actual == expected) $
    "leaked handles, expected " <> show expected <> ", but actual " <> show actual
