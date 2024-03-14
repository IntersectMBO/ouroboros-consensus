{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

-- | On-disk ledger DB tests.
--
-- This is a model based test. The commands here are
--
-- * Get the current ledger state
-- * Push a block, or switch to a fork
-- * Write a snapshot to disk
-- * Restore the ledger DB from the snapshots on disk
-- * Model disk corruption
--
-- The model here is satisfyingly simple: just a map from blocks to their
-- corresponding ledger state.
--
module Test.Ouroboros.Storage.LedgerDB.StateMachine (tests) where

import           Control.Monad (void)
import           Control.Monad.Except
import           Control.Monad.State hiding (state)
import           Control.Tracer (nullTracer)
import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.SOP.Dict
import           Data.Word
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Tables.Utils
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Stream
import           Ouroboros.Consensus.Storage.LedgerDB.API as LedgerDB
import           Ouroboros.Consensus.Storage.LedgerDB.API.Config
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Args as Args
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Init
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Snapshots
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Args
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Init as V1
import           Ouroboros.Consensus.Storage.LedgerDB.V2.Init as V2
import           Ouroboros.Consensus.Storage.LedgerDB.V2.Args
import           Ouroboros.Consensus.Util hiding (Some)
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import qualified Ouroboros.Network.AnchoredSeq as AS
import qualified System.Directory as Dir
import           System.FS.API
import qualified System.FS.IO as FSIO
import qualified System.FS.Sim.MockFS as MockFS
import           System.FS.Sim.STM
import qualified System.IO.Temp as Temp
import           Test.Ouroboros.Storage.LedgerDB.StateMachine.TestBlock
import qualified Test.QuickCheck as QC
import           "quickcheck-dynamic" Test.QuickCheck.Extras
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.StateModel
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.TestBlock hiding (TestBlock, TestBlockCodecConfig,
                     TestBlockStorageConfig)

tests :: TestTree
tests = testGroup "StateMachine" [
      testProperty "InMemV1" $
          prop_sequential 100000 inMemV1TestArguments
    , testProperty "InMemV2" $
          prop_sequential 100000 inMemV2TestArguments
    , testProperty "LMDB" $
          prop_sequential 1000 lmdbTestArguments
    ]

prop_sequential ::
     Int
  -> (SecurityParam -> IO (TestArguments IO))
  -> Actions Model
  -> QC.Property
prop_sequential maxSuccess mkTestArguments as = QC.withMaxSuccess maxSuccess $
  QC.monadicIO $ do
    ref <- lift $ do
      cdb <- initChainDB
      pure $ Environment undefined (OnDemandActions undefined undefined undefined undefined undefined (pure ())) cdb mkTestArguments (pure ())
    (_, Environment _ od _ _ clean) <- runPropertyStateT (runActions as) ref
    QC.run $ closeLedgerDB od
    QC.run $ clean
    QC.assert True

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

data TestArguments m = TestArguments {
      argHasFS       :: !(SomeHasFS m)
    , argFlavorArgs  :: !(Complete Args.LedgerDbFlavorArgs m)
    , argLedgerDbCfg :: !(LedgerDbCfg (ExtLedgerState TestBlock))
    , cleanup        :: !(m ())
    }

inMemV1TestArguments :: MonadIO m
  => SecurityParam
  -> m (TestArguments IO)
inMemV1TestArguments secParam = liftIO $ do
  fs <- unsafeToUncheckedStrictTVar <$> uncheckedNewTVarM MockFS.empty
  pure TestArguments {
      argHasFS = SomeHasFS $ simHasFS fs
    , argFlavorArgs = LedgerDbFlavorArgsV1 $ V1Args DisableFlushing DisableQuerySize InMemoryBackingStoreArgs
    , argLedgerDbCfg = extLedgerDbConfig secParam
    , cleanup = pure ()
    }

inMemV2TestArguments :: MonadIO m
  => SecurityParam
  -> m (TestArguments IO)
inMemV2TestArguments secParam = liftIO $ do
  fs <- unsafeToUncheckedStrictTVar <$> uncheckedNewTVarM MockFS.empty
  pure TestArguments {
      argHasFS = SomeHasFS $ simHasFS fs
    , argFlavorArgs = LedgerDbFlavorArgsV2 $ V2Args InMemoryHandleArgs -- DisableFlushing DisableQuerySize InMemoryBackingStoreArgs
    , argLedgerDbCfg = extLedgerDbConfig secParam
    , cleanup = pure ()
    }

testLMDBLimits :: LMDBLimits
testLMDBLimits = LMDBLimits
  { -- 100 MiB should be more than sufficient for the tests we're running here.
    -- If the database were to grow beyond 100 Mebibytes, resulting in a test
    -- error, then something in the LMDB backing store or tests has changed and
    -- we should reconsider this value.
    lmdbMapSize = 100 * 1024 * 1024
    -- 3 internal databases: 1 for the settings, 1 for the state, and 1 for the
    -- ledger tables.
  , lmdbMaxDatabases = 3
  , lmdbMaxReaders = 16
  }

lmdbTestArguments :: MonadIO m
  => SecurityParam
  -> m (TestArguments IO)
lmdbTestArguments secParam = do
  (hfs, cu) <- liftIO $ do
      systmpdir <- Dir.getTemporaryDirectory
      tmpdir <- Temp.createTempDirectory systmpdir "init_standalone_db"
      pure (SomeHasFS $ FSIO.ioHasFS $ MountPoint tmpdir, Dir.removeDirectoryRecursive tmpdir)
  pure TestArguments {
      argHasFS = hfs
    , argFlavorArgs = LedgerDbFlavorArgsV1 $ V1Args DisableFlushing DisableQuerySize $ LMDBBackingStoreArgs testLMDBLimits Dict
    , argLedgerDbCfg = extLedgerDbConfig secParam
    , cleanup = cu
    }

{-------------------------------------------------------------------------------
 Model
-------------------------------------------------------------------------------}

type TheBlockChain =
  AS.AnchoredSeq
    (WithOrigin SlotNo)
    (ExtLedgerState TestBlock ValuesMK)
    (TestBlock, ExtLedgerState TestBlock ValuesMK)

data Model =
    UnInit
  | Model
      TheBlockChain
      [Var DiskSnapshot]
      SecurityParam
  deriving (Generic, Show)

instance AS.Anchorable
           (WithOrigin SlotNo)
           (ExtLedgerState TestBlock ValuesMK)
           (TestBlock, ExtLedgerState TestBlock ValuesMK) where
  asAnchor = snd
  getAnchorMeasure _ = getTipSlot

instance HasVariables TheBlockChain where
  getAllVariables _ = mempty

modelUpdateLedger ::
     StateT
       TheBlockChain
       (Except (ExtValidationError TestBlock)) a
  -> Model
  -> Model
modelUpdateLedger f model@(Model chain corrupted secParam) =
    case runExcept (runStateT f chain) of
      Left{}             -> model
      Right (_, ledger') -> Model ledger' corrupted secParam
modelUpdateLedger _ _ = error "Uninitialized model tried to apply blocks!"

modelRollback :: Word64 -> Model -> Model
modelRollback n (Model chain corrupted secParam) =
   Model (AS.dropNewest (fromIntegral n) chain) corrupted secParam
modelRollback _ UnInit = error "Uninitialized model can't rollback!"

{-------------------------------------------------------------------------------
  StateModel
-------------------------------------------------------------------------------}

deriving instance Show (Action Model a)
deriving instance Eq (Action Model a)

instance HasVariables (Action Model a) where
  getAllVariables (DoDelete s)   = getAllVariables s
  getAllVariables (DoTruncate s) = getAllVariables s
  getAllVariables _              = mempty

instance StateModel Model where
  data Action Model a where
    DoDelete          :: Var DiskSnapshot -> Action Model ()
    DoTruncate        :: Var DiskSnapshot -> Action Model ()
    DropAndRestore    :: Word64 -> Action Model ()
    ForceTakeSnapshot :: Action Model (Maybe DiskSnapshot)
    GetState          :: Action Model (ExtLedgerState TestBlock EmptyMK, ExtLedgerState TestBlock EmptyMK)
    Init              :: SecurityParam -> Action Model ()
    ValidateAndCommit :: Word64 -> [TestBlock] -> Action Model ()

  actionName DoDelete{}          = "DoDelete"
  actionName DoTruncate{}        = "DoTruncate"
  actionName DropAndRestore{}    = "DropAndRestore"
  actionName ForceTakeSnapshot   = "TakeSnapshot"
  actionName GetState{}          = "GetState"
  actionName Init{}              = "Init"
  actionName ValidateAndCommit{} = "ValidateAndCommit"

  arbitraryAction _ UnInit = Some . Init <$> QC.arbitrary
  arbitraryAction vctxt model@(Model chain corrupted secParam) =
    oneof [ pure $ Some GetState
          , pure $ Some ForceTakeSnapshot
          , Some . DropAndRestore <$> QC.choose (0, fromIntegral $ AS.length chain)
          , Some . DoDelete       <$> arbitraryVar vctxt `suchThat` (`notElem` corrupted)
          , Some . DoTruncate     <$> arbitraryVar vctxt `suchThat` (`notElem` corrupted)
          , Some <$> do
              let maxRollback = minimum [
                      fromIntegral . AS.length $ chain
                    , maxRollbacks secParam
                    ]
              numRollback  <- QC.choose (0, maxRollback)
              numNewBlocks <- QC.choose (numRollback, numRollback + 2)
              let
                chain' = case modelRollback numRollback model of
                  UnInit       -> error "Impossible"
                  Model ch _ _ -> ch
                blocks = genBlocks
                           numNewBlocks
                           (lastAppliedPoint . ledgerState . either id snd . AS.head $ chain')
              return $ ValidateAndCommit numRollback blocks
          ]

--  shrinkAction = defaulted

  initialState = UnInit

  nextState _     (Init secParam)            _var = Model (AS.Empty genesis) [] secParam
  nextState state GetState                   _var = state
  -- Here we can ignore the _var because as all vars will be snapshots this is
  -- implicitly accounted in the Q-D vars
  nextState state ForceTakeSnapshot          _var = state
  nextState state@(Model _ _ secParam) (ValidateAndCommit n blks) _var =
      modelUpdateLedger switch state
    where
      push :: TestBlock -> StateT (AS.AnchoredSeq (WithOrigin SlotNo) (ExtLedgerState TestBlock ValuesMK) (TestBlock, ExtLedgerState TestBlock ValuesMK)) (Except (ExtValidationError TestBlock)) ()
      push b = do
        ls <- get
        let tip = either id snd $ AS.head ls
        l' <- lift $ tickThenApply (ledgerDbCfg $ extLedgerDbConfig secParam) b tip
        put (ls AS.:> (b, applyDiffs tip l'))

      switch :: StateT (AS.AnchoredSeq (WithOrigin SlotNo) (ExtLedgerState TestBlock ValuesMK) (TestBlock, ExtLedgerState TestBlock ValuesMK)) (Except (ExtValidationError TestBlock)) ()
      switch = do
          modify $ AS.dropNewest (fromIntegral n)
          mapM_ push blks

  nextState (Model chain corrupted secParam) (DoDelete ss) _var =
    Model chain (ss : corrupted) secParam
  nextState (Model chain corrupted secParam) (DoTruncate ss) _var =
    Model chain (ss : corrupted) secParam
  nextState state (DropAndRestore n)         _var = modelRollback n state
  nextState UnInit _ _ = error "Uninitialized model created a command different than Init"

  precondition UnInit               Init{}                  = True
  precondition UnInit _                                     = False
  precondition (Model chain _ secParam) (ValidateAndCommit n blks) =
       n <= min (maxRollbacks secParam) (fromIntegral $ AS.length chain)
    && case blks of
        [] -> True
        (b:_) -> tbSlot b == 1 + withOrigin 0 id (getTipSlot (AS.headAnchor chain))
  precondition _                    Init{}                  = False
  precondition _ _                                          = True

{-------------------------------------------------------------------------------
  Mocked ChainDB
-------------------------------------------------------------------------------}

-- | Mocked chain db
data ChainDB m = ChainDB {
      -- | Block storage
      dbBlocks  :: StrictTVar m (Map (RealPoint TestBlock) TestBlock)

      -- | Current chain and corresponding ledger state
      --
      -- Invariant: all references @r@ here must be present in 'dbBlocks'.
    , dbChain   :: StrictTVar m [RealPoint TestBlock]
    }

initChainDB ::
     forall m. (MonadIO m, IOLike m)
  => m (ChainDB m)
initChainDB = do
    dbBlocks <- uncheckedNewTVarM Map.empty
    dbChain  <- uncheckedNewTVarM []
    return $ ChainDB dbBlocks dbChain

dbStreamAPI ::
     forall m. IOLike m
  => SecurityParam
  -> ChainDB m
  -> m (StreamAPI m TestBlock TestBlock, [TestBlock])
dbStreamAPI secParam chainDb =
    atomically $ do
      points <- reverse . take (fromIntegral $ maxRollbacks secParam) <$> readTVar dbChain
      blks <- readTVar dbBlocks
      pure $ (StreamAPI streamAfter, map (blks Map.!) points)
  where
    ChainDB {
        dbBlocks
      , dbChain
      } = chainDb

    streamAfter ::
         Point TestBlock
      -> (Either (RealPoint TestBlock) (m (NextItem TestBlock)) -> m a)
      -> m a
    streamAfter tip k = do
        pts <- atomically $ reverse . drop (fromIntegral $ maxRollbacks secParam) <$> readTVar dbChain
        case tip' of
          NotOrigin pt
            | pt `L.notElem` pts
            -> k $ Left pt
          _otherwise
            -> do toStream <- uncheckedNewTVarM (blocksToStream tip' pts)
                  k (Right (getNext toStream))
     where
       tip' = pointToWithOriginRealPoint tip

    -- Blocks to stream
    --
    -- Precondition: tip must be on the current chain
    blocksToStream ::
         WithOrigin (RealPoint TestBlock)
      -> [RealPoint TestBlock] -> [RealPoint TestBlock]
    blocksToStream Origin        = id
    blocksToStream (NotOrigin r) = tail . dropWhile (/= r)

    getNext :: StrictTVar m [RealPoint TestBlock] -> m (NextItem TestBlock)
    getNext toStream = do
        mr <- atomically $ do
                rs <- readTVar toStream
                case rs of
                  []    -> return Nothing
                  r:rs' -> writeTVar toStream rs' >> return (Just r)
        case mr of
          Nothing -> return NoMoreItems
          Just r  -> do mb <- atomically $ Map.lookup r <$> readTVar dbBlocks
                        case mb of
                          Just b  -> return $ NextItem b
                          Nothing -> error blockNotFound

blockNotFound :: String
blockNotFound = concat [
          "dbStreamAPI: "
        , "invariant violation: "
        , "block in dbChain not present in dbBlocks"
        ]

{-------------------------------------------------------------------------------
  New SUT
-------------------------------------------------------------------------------}

openLedgerDB ::
     Complete Args.LedgerDbFlavorArgs IO
  -> ChainDB IO
  -> LedgerDbCfg (ExtLedgerState TestBlock)
  -> SomeHasFS IO
  -> IO (LedgerDB' IO TestBlock, OnDemandActions' IO TestBlock)
openLedgerDB flavArgs env cfg fs = do
  (stream, volBlocks) <- dbStreamAPI (ledgerDbCfgSecParam cfg) env
  let getBlock f = Map.findWithDefault (error blockNotFound) f <$> readTVarIO (dbBlocks env)
  replayGoal <- fmap (realPointToPoint . last . Map.keys) . atomically $ readTVar (dbBlocks env)
  rr <- unsafeNewRegistry
  let args = LedgerDbArgs
               (SnapshotPolicyArgs DisableSnapshots DefaultNumOfDiskSnapshots)
               (pure genesis)
               fs fs False False
               cfg
               nullTracer
               flavArgs
               rr
  (ldb, _, od) <- case flavArgs of
    LedgerDbFlavorArgsV1 bss ->
      let initDb = V1.mkInitDb
                       args
                       bss
                       getBlock
        in
          openDBInternal args initDb stream replayGoal
    LedgerDbFlavorArgsV2 bss ->
        let initDb = V2.mkInitDb
                       args
                       bss
                       getBlock
        in
          openDBInternal args initDb stream replayGoal
  withRegistry $ \reg -> do
    vr <- validate ldb reg (const $ pure ()) BlockCache.empty 0 (map getHeader volBlocks)
    case vr of
      ValidateSuccessful forker -> do
        atomically (forkerCommit forker)
        forkerClose forker
      _ -> error "Couldn't restart the chain, failed to apply volatile blocks!"
  pure (ldb, od)

{-------------------------------------------------------------------------------
  RunModel
-------------------------------------------------------------------------------}

-- | The environment for the monad in which we will run the test
data Environment =
  Environment
    (LedgerDB' IO TestBlock)
    (OnDemandActions' IO TestBlock)
    (ChainDB IO)
    (SecurityParam -> IO (TestArguments IO))
    (IO ())

instance RunModel Model (StateT Environment IO) where

  perform _ (Init secParam) _ = do
    Environment _ _ chainDb mkArgs _ <- get
    (ldb, od, clean) <- lift $ do
      args <- mkArgs secParam
      (ldb, od) <- openLedgerDB (argFlavorArgs args) chainDb (extLedgerDbConfig secParam) (argHasFS args)
      pure (ldb, od, cleanup args)
    put (Environment ldb od chainDb mkArgs (closeLedgerDB od >> clean))
  perform _ (DoTruncate s) lk = do
    Environment _ od _ _ _ <- get
    lift $ truncateSnapshot od (lk s)
  perform _ (DoDelete s) lk = do
    Environment _ od _ _ _ <- get
    lift $ destroySnapshot od (lk s)
  perform _ GetState _ = do
    Environment ldb _ _ _ _ <- get
    lift $ atomically $ (,) <$> getImmutableTip ldb <*> getVolatileTip ldb
  perform _ ForceTakeSnapshot _ = do
    Environment ldb od _ _ _ <- get
    lift $ do
      void $ flushNOW od >> takeSnapshotNOW od Nothing
      imm <- atomically $ getImmutableTip ldb
      let pt = pointToWithOriginRealPoint $ castPoint @(ExtLedgerState TestBlock) @TestBlock $ getTip imm
      pure
         $ withOrigin Nothing (\p -> Just (DiskSnapshot (unSlotNo $ realPointSlot p) Nothing))
         $ pt
  perform _ (ValidateAndCommit n blks) _ = do
      Environment ldb _ chainDb _ _ <- get
      lift $ do
        atomically $ modifyTVar (dbBlocks chainDb) $
           repeatedly (uncurry Map.insert) (map (\b -> (blockRealPoint b, b)) blks)
        withRegistry $ \rr -> do
          vr <- validate ldb rr (const $ pure ()) BlockCache.empty n (map getHeader blks)
          case vr of
            ValidateSuccessful forker -> do
              atomically $ modifyTVar (dbChain chainDb) (reverse (map blockRealPoint blks) ++)
              atomically (forkerCommit forker)
              forkerClose forker
            ValidateExceededRollBack{} -> error "Unexpected Rollback"
            ValidateLedgerError (AnnLedgerError forker _ _) -> forkerClose forker >> error "Unexpected ledger error"
  perform state@(Model _ _ secParam) (DropAndRestore n) lk = do
    Environment _ od chainDb _ clean <- get
    lift $ do
      atomically $ modifyTVar (dbChain chainDb) (drop (fromIntegral n))
      closeLedgerDB od
      clean
    perform state (Init secParam) lk
  perform UnInit _ _ = error "Uninitialized model created a command different than Init"


  postcondition (Model chain _ secParam, _) GetState _ (imm, vol) =
    let volSt = either forgetLedgerTables (forgetLedgerTables . snd) (AS.head chain)
        immSt = either forgetLedgerTables (forgetLedgerTables . snd) (AS.head (AS.dropNewest (fromIntegral $ maxRollbacks secParam) chain))
    in do
       counterexamplePost $ unlines [ "VolSt: ", show volSt
                                    , "VolSut: ", show vol
                                    , "ImmSt: ", show immSt
                                    , "ImmSut: ", show imm
                                    ]
       pure $ volSt == vol && immSt == imm
  postcondition _ _ _ _ = pure True
