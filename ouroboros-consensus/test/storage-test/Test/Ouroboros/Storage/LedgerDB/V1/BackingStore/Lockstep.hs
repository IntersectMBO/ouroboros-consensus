{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Ouroboros.Storage.LedgerDB.V1.BackingStore.Lockstep (
    BackingStoreState (..)
  , RealEnv (..)
  , RealMonad
  , maxOpenValueHandles
  ) where

import           Cardano.Slotting.Slot
import           Control.Concurrent.Class.MonadMVar.Strict
import           Control.Monad
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Constraint
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Typeable
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore as BS
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.InMemory as BS
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB as LMDB
                     (LMDBErr (..))
import           Ouroboros.Consensus.Util.IOLike hiding (MonadMask (..),
                     StrictMVar, handle, readMVar, swapMVar)
import           System.FS.API hiding (Handle)
import qualified System.FS.API.Types as FS
import           Test.Cardano.Ledger.Binary.Arbitrary ()
import qualified Test.Ouroboros.Storage.LedgerDB.V1.BackingStore.Mock as Mock
import           Test.Ouroboros.Storage.LedgerDB.V1.BackingStore.Mock (Err (..),
                     Mock (..), ValueHandle (..), runMockMonad)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck (Gen)
import           Test.QuickCheck.StateModel
import           Test.QuickCheck.StateModel.Lockstep as Lockstep
import           Test.QuickCheck.StateModel.Lockstep.Defaults as Lockstep
import           Test.QuickCheck.StateModel.Lockstep.Op.SumProd as Lockstep
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.ToExpr ()

{-------------------------------------------------------------------------------
  @'Values'@ wrapper
-------------------------------------------------------------------------------}

-- | Wrapper to prevent ambiguity in pattern matches.
newtype Values vs = Values {unValues :: vs}
  deriving stock (Show, Eq, Ord)
  deriving newtype QC.Arbitrary

{-------------------------------------------------------------------------------
  Model state
-------------------------------------------------------------------------------}

data BackingStoreState ks vs d = BackingStoreState {
    bssMock  :: Mock vs
  , bssStats :: Stats ks vs d
  }
  deriving (Show, Eq)

initState :: Mock.EmptyValues vs  => BackingStoreState ks vs d
initState = BackingStoreState {
    bssMock  = Mock.emptyMock
  , bssStats = initStats
  }

-- | Maximum number of LMDB readers that can be active at a time.
--
-- 32 is an arbitrary number of readers. We can increase or decrease this at
-- will.
maxOpenValueHandles :: Int
maxOpenValueHandles = 32

{-------------------------------------------------------------------------------
  @'StateModel'@ and @'RunModel'@ instances
-------------------------------------------------------------------------------}

type BackingStoreInitializer m ks vs d =
     BS.InitFrom vs
  -> m (BS.BackingStore m ks vs d)

data RealEnv m ks vs d = RealEnv {
    reBackingStoreInit :: BackingStoreInitializer m ks vs d
  , reBackingStore     :: StrictMVar m (BS.BackingStore m ks vs d)
  }

type RealMonad m ks vs d = ReaderT (RealEnv m ks vs d) m

type BSAct ks vs d a =
  Action
    (Lockstep (BackingStoreState ks vs d))
    (Either Err a)
type BSVar ks vs d a =
  ModelVar (BackingStoreState ks vs d) a

instance ( Show ks, Show vs, Show d
         , Show (BS.InitHint vs), Show (BS.WriteHint d), Show (BS.ReadHint vs)
         , Eq ks, Eq vs, Eq d
         , Eq (BS.InitHint vs), Eq (BS.WriteHint d), Eq (BS.ReadHint vs)
         , Typeable ks, Typeable vs, Typeable d, Typeable (BS.WriteHint d)
         , QC.Arbitrary ks, QC.Arbitrary vs, QC.Arbitrary d
         , QC.Arbitrary (BS.RangeQuery ks)
         , Mock.HasOps ks vs d
         ) => StateModel (Lockstep (BackingStoreState ks vs d)) where
  data Action (Lockstep (BackingStoreState ks vs d)) a where
    -- Reopen a backing store by intialising from values.
    BSInitFromValues :: WithOrigin SlotNo
                     -> BS.InitHint vs
                     -> Values vs
                     -> BSAct ks vs d ()
    -- Reopen a backing store by initialising from a copy.
    BSInitFromCopy   :: BS.InitHint vs
                     -> FS.FsPath
                     -> BSAct ks vs d ()
    BSClose          :: BSAct ks vs d ()
    BSCopy           :: FS.FsPath
                     -> BSAct ks vs d ()
    BSValueHandle    :: BSAct ks vs d (BS.BackingStoreValueHandle IO ks vs)
    BSWrite          :: SlotNo
                     -> BS.WriteHint d
                     -> d
                     -> BSAct ks vs d ()
    BSVHClose        :: BSVar ks vs d (BS.BackingStoreValueHandle IO ks vs)
                     -> BSAct ks vs d ()
    BSVHRangeRead    :: BSVar ks vs d (BS.BackingStoreValueHandle IO ks vs)
                     -> BS.ReadHint vs
                     -> BS.RangeQuery ks
                     -> BSAct ks vs d (Values vs)
    BSVHRead         :: BSVar ks vs d (BS.BackingStoreValueHandle IO ks vs)
                     -> BS.ReadHint vs
                     -> ks
                     -> BSAct ks vs d (Values vs)
    BSVHAtSlot       :: BSVar ks vs d (BS.BackingStoreValueHandle IO ks vs)
                     -> BSAct ks vs d (WithOrigin SlotNo)
    -- | Corresponds to 'bsvhStat'
    BSVHStat         :: BSVar ks vs d (BS.BackingStoreValueHandle IO ks vs)
                     -> BSAct ks vs d BS.Statistics

  initialState        = Lockstep.initialState initState
  nextState           = Lockstep.nextState
  precondition st act = Lockstep.precondition st act
                        && modelPrecondition (getModel st) act
  arbitraryAction     = Lockstep.arbitraryAction
  shrinkAction        = Lockstep.shrinkAction

deriving stock instance ( Show ks, Show vs, Show d
                        , Show (BS.InitHint vs), Show (BS.WriteHint d), Show (BS.ReadHint vs)
                        ) => Show (LockstepAction (BackingStoreState ks vs d) a)
deriving stock instance ( Eq ks, Eq vs, Eq d
                        , Eq (BS.InitHint vs), Eq (BS.WriteHint d), Eq (BS.ReadHint vs)
                        ) => Eq (LockstepAction (BackingStoreState ks vs d) a)

instance ( Show ks, Show vs, Show d
         , Show (BS.InitHint vs), Show (BS.WriteHint d), Show (BS.ReadHint vs)
         , Eq ks, Eq vs, Eq d
         , Eq (BS.InitHint vs), Eq (BS.WriteHint d), Eq (BS.ReadHint vs)
         , Typeable ks, Typeable vs, Typeable d, Typeable (BS.WriteHint d)
         , QC.Arbitrary ks, QC.Arbitrary vs, QC.Arbitrary d
         , QC.Arbitrary (BS.RangeQuery ks)
         , Mock.HasOps ks vs d
         ) => RunModel
                (Lockstep (BackingStoreState ks vs d))
                (RealMonad IO ks vs d) where
  perform       = \_st -> runIO
  postcondition = Lockstep.postcondition
  monitoring    = Lockstep.monitoring (Proxy @(RealMonad IO ks vs d))

-- | Custom precondition that prevents errors in the @'LMDB'@ backing store due
-- to exceeding the maximum number of LMDB readers.
--
-- See @'maxOpenValueHandles'@.
modelPrecondition ::
       BackingStoreState ks vs d
    -> LockstepAction (BackingStoreState ks vs d) a
    -> Bool
modelPrecondition (BackingStoreState mock _stats) action = case action of
    BSInitFromValues _ _ _ -> isClosed mock
    BSInitFromCopy _ _     -> isClosed mock
    BSCopy _               -> canOpenReader
    BSValueHandle          -> canOpenReader
    _                      -> True
  where
    canOpenReader         = Map.size openValueHandles < maxOpenValueHandles
    openValueHandles      = Map.filter (==Mock.Open) (valueHandles mock)

{-------------------------------------------------------------------------------
  @'InLockstep'@ instance
-------------------------------------------------------------------------------}

type BSVal ks vs d a = ModelValue (BackingStoreState ks vs d) a
type BSObs ks vs d a = Observable (BackingStoreState ks vs d) a

instance ( Show ks, Show vs, Show d
         , Show (BS.InitHint vs), Show (BS.WriteHint d), Show (BS.ReadHint vs)
         , Eq ks, Eq vs, Eq d
         , Eq (BS.InitHint vs), Eq (BS.WriteHint d), Eq (BS.ReadHint vs)
         , Typeable ks, Typeable vs, Typeable d, Typeable (BS.WriteHint d)
         , QC.Arbitrary ks, QC.Arbitrary vs, QC.Arbitrary d
         , QC.Arbitrary (BS.RangeQuery ks)
         , Mock.HasOps ks vs d
         ) => InLockstep (BackingStoreState ks vs d) where

  data instance ModelValue (BackingStoreState ks vs d) a where
    MValueHandle :: ValueHandle vs -> BSVal ks vs d (BS.BackingStoreValueHandle IO ks vs)

    MErr        :: Err
                -> BSVal ks vs d Err
    MSlotNo     :: WithOrigin SlotNo
                -> BSVal ks vs d (WithOrigin SlotNo)
    MValues     :: vs
                -> BSVal ks vs d (Values vs)
    MUnit       :: ()
                -> BSVal ks vs d ()
    MStatistics :: BS.Statistics
                -> BSVal ks vs d BS.Statistics

    MEither :: Either (BSVal ks vs d a) (BSVal ks vs d b)
            -> BSVal ks vs d (Either a b)
    MPair   :: (BSVal ks vs d a, BSVal ks vs d b)
            -> BSVal ks vs d (a, b)

  data instance Observable (BackingStoreState ks vs d) a where
    OValueHandle :: BSObs ks vs d (BS.BackingStoreValueHandle IO ks vs)
    OValues :: (Show a, Eq a, Typeable a) => a -> BSObs ks vs d (Values a)
    OId     :: (Show a, Eq a, Typeable a) => a -> BSObs ks vs d a
    OEither :: Either (BSObs ks vs d a) (BSObs ks vs d b)
            -> BSObs ks vs d (Either a b)
    OPair   :: (BSObs ks vs d a, BSObs ks vs d b) -> BSObs ks vs d (a, b)

  observeModel :: BSVal ks vs d a -> BSObs ks vs d a
  observeModel = \case
    MValueHandle _ -> OValueHandle
    MErr x         -> OId x
    MSlotNo x      -> OId x
    MValues x      -> OValues x
    MUnit x        -> OId x
    MStatistics x  -> OId x
    MEither x      -> OEither $ bimap observeModel observeModel x
    MPair x        -> OPair   $ bimap observeModel observeModel x

  modelNextState :: forall a.
       LockstepAction (BackingStoreState ks vs d) a
    -> ModelVarContext (BackingStoreState ks vs d)
    -> BackingStoreState ks vs d -> (BSVal ks vs d a, BackingStoreState ks vs d)
  modelNextState action lookUp (BackingStoreState mock stats) =
      auxStats $ runMock lookUp action mock
    where
      auxStats ::
           (BSVal ks vs d a, Mock vs)
        -> (BSVal ks vs d a, BackingStoreState ks vs d)
      auxStats (result, state') =
          ( result
          , BackingStoreState state' $ updateStats action lookUp result stats
          )

  type ModelOp (BackingStoreState ks vs d) = Op

  usedVars ::
       LockstepAction (BackingStoreState ks vs d) a
    -> [AnyGVar (ModelOp (BackingStoreState ks vs d))]
  usedVars = \case
    BSInitFromValues _ _ _ -> []
    BSInitFromCopy _ _     -> []
    BSClose                -> []
    BSCopy _               -> []
    BSValueHandle          -> []
    BSWrite _ _ _          -> []
    BSVHClose h            -> [SomeGVar h]
    BSVHRangeRead h _ _    -> [SomeGVar h]
    BSVHRead h _ _         -> [SomeGVar h]
    BSVHAtSlot h           -> [SomeGVar h]
    BSVHStat h             -> [SomeGVar h]

  arbitraryWithVars ::
       ModelVarContext (BackingStoreState ks vs d)
    -> BackingStoreState ks vs d
    -> Gen (Any (LockstepAction (BackingStoreState ks vs d)))
  arbitraryWithVars = arbitraryBackingStoreAction

  shrinkWithVars ::
       ModelVarContext (BackingStoreState ks vs d)
    -> BackingStoreState ks vs d
    -> LockstepAction (BackingStoreState ks vs d) a
    -> [Any (LockstepAction (BackingStoreState ks vs d))]
  shrinkWithVars = shrinkBackingStoreAction

  tagStep ::
       (BackingStoreState ks vs d, BackingStoreState ks vs d)
    -> LockstepAction (BackingStoreState ks vs d) a
    -> BSVal ks vs d a
    -> [String]
  tagStep (BackingStoreState _ before, BackingStoreState _ after) action val =
    map show $ tagBSAction before after action val

deriving stock instance ( Show ks, Show vs, Show d
                        , Show (BS.WriteHint d), Show (BS.ReadHint vs)
                        ) => Show (BSVal ks vs d a)

deriving stock instance (Show ks, Show vs, Show d
                        , Show (BS.WriteHint d), Show (BS.ReadHint vs)
                        ) => Show (BSObs ks vs d a)

deriving stock instance (Eq ks, Eq vs, Eq d
                        , Eq (BS.WriteHint d), Eq (BS.ReadHint vs)
                        ) => Eq (BSObs ks vs d a)

{-------------------------------------------------------------------------------
  @'RunLockstep'@ instance
-------------------------------------------------------------------------------}

instance ( Show ks, Show vs, Show d
         , Show (BS.InitHint vs), Show (BS.WriteHint d), Show (BS.ReadHint vs)
         , Eq ks, Eq vs, Eq d
         , Eq (BS.InitHint vs), Eq (BS.WriteHint d), Eq (BS.ReadHint vs)
         , Typeable ks, Typeable vs, Typeable d, Typeable (BS.WriteHint d)
         , QC.Arbitrary ks, QC.Arbitrary vs, QC.Arbitrary d
         , QC.Arbitrary (BS.RangeQuery ks)
         , Mock.HasOps ks vs d
         ) => RunLockstep (BackingStoreState ks vs d) (RealMonad IO ks vs d) where
  observeReal ::
       Proxy (RealMonad IO ks vs d)
    -> LockstepAction (BackingStoreState ks vs d) a
    -> Realized (RealMonad IO ks vs d) a
    -> BSObs ks vs d a
  observeReal _proxy = \case
    BSInitFromValues _ _ _ -> OEither . bimap OId OId
    BSInitFromCopy _ _     -> OEither . bimap OId OId
    BSClose                -> OEither . bimap OId OId
    BSCopy _               -> OEither . bimap OId OId
    BSValueHandle          -> OEither . bimap OId (const OValueHandle)
    BSWrite _ _ _          -> OEither . bimap OId OId
    BSVHClose _            -> OEither . bimap OId OId
    BSVHRangeRead _ _ _    -> OEither . bimap OId (OValues . unValues)
    BSVHRead _ _ _         -> OEither . bimap OId (OValues . unValues)
    BSVHAtSlot _           -> OEither . bimap OId OId
    BSVHStat _             -> OEither . bimap OId OId

  showRealResponse ::
       Proxy (RealMonad IO ks vs d)
    -> LockstepAction (BackingStoreState ks vs d) a
    -> Maybe (Dict (Show (Realized (RealMonad IO ks vs d) a)))
  showRealResponse _proxy = \case
    BSInitFromValues _ _ _ -> Just Dict
    BSInitFromCopy _ _     -> Just Dict
    BSClose                -> Just Dict
    BSCopy _               -> Just Dict
    BSValueHandle          -> Nothing
    BSWrite _ _ _          -> Just Dict
    BSVHClose _            -> Just Dict
    BSVHRangeRead _ _ _    -> Just Dict
    BSVHRead _ _ _         -> Just Dict
    BSVHAtSlot _           -> Just Dict
    BSVHStat _             -> Just Dict

{-------------------------------------------------------------------------------
  Interpreter against the model
-------------------------------------------------------------------------------}

runMock ::
     forall ks vs d a. (Mock.HasOps ks vs d, QC.Arbitrary ks, QC.Arbitrary vs, QC.Arbitrary d, QC.Arbitrary (BS.RangeQuery ks))
  => ModelVarContext (BackingStoreState ks vs d)
  -> Action (Lockstep (BackingStoreState ks vs d)) a
  -> Mock vs
  -> ( BSVal ks vs d a
     , Mock vs
     )
runMock lookUp = \case
    BSInitFromValues sl h (Values vs) ->
      wrap MUnit . runMockMonad (Mock.mBSInitFromValues sl h vs)
    BSInitFromCopy h bsp ->
      wrap MUnit . runMockMonad (Mock.mBSInitFromCopy h bsp)
    BSClose            ->
      wrap MUnit . runMockMonad Mock.mBSClose
    BSCopy bsp         ->
      wrap MUnit . runMockMonad (Mock.mBSCopy bsp)
    BSValueHandle      ->
      wrap MValueHandle . runMockMonad Mock.mBSValueHandle
    BSWrite sl whint d    ->
      wrap MUnit . runMockMonad (Mock.mBSWrite sl whint d)
    BSVHClose h        ->
      wrap MUnit . runMockMonad (Mock.mBSVHClose (getHandle $ lookupVar lookUp h))
    BSVHRangeRead h rhint rq ->
      wrap MValues . runMockMonad (Mock.mBSVHRangeRead (getHandle $ lookupVar lookUp h) rhint rq)
    BSVHRead h rhint ks      ->
      wrap MValues . runMockMonad (Mock.mBSVHRead (getHandle $ lookupVar lookUp h) rhint ks)
    BSVHAtSlot h       ->
      wrap MSlotNo . runMockMonad (Mock.mBSVHAtSlot (getHandle $ lookupVar lookUp h))
    BSVHStat h         ->
      wrap MStatistics . runMockMonad (Mock.mBSVHStat (getHandle $ lookupVar lookUp h))
  where
    wrap f = first (MEither . bimap MErr f)

    getHandle :: BSVal ks vs d (BS.BackingStoreValueHandle IO ks vs) -> ValueHandle vs
    getHandle (MValueHandle h) = h

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

arbitraryBackingStoreAction ::
     forall ks vs d.
     ( Mock.HasOps ks vs d
     , QC.Arbitrary ks, QC.Arbitrary vs, QC.Arbitrary d
     , QC.Arbitrary (BS.RangeQuery ks)
     )
  => ModelVarContext (BackingStoreState ks vs d)
  -> BackingStoreState ks vs d
  -> Gen (Any (LockstepAction (BackingStoreState ks vs d)))
arbitraryBackingStoreAction fv (BackingStoreState mock _stats) =
    QC.frequency $
         withoutVars
      ++ case findVars fv (Proxy @(Either Err (BS.BackingStoreValueHandle IO ks vs))) of
          []   -> []
          vars -> withVars (QC.elements vars)
  where
    withoutVars :: [(Int, Gen (Any (LockstepAction (BackingStoreState ks vs d))))]
    withoutVars = [
        (5, fmap Some $ BSInitFromValues <$> QC.arbitrary <*>
              pure (Mock.makeInitHint (Proxy @vs)) <*> (Values <$> QC.arbitrary))
      , (5, fmap Some $ BSInitFromCopy <$>
              pure (Mock.makeInitHint (Proxy @vs)) <*> genBackingStorePath)
      , (2, pure $ Some BSClose)
      , (5, fmap Some $ BSCopy <$> genBackingStorePath)
      , (5, pure $ Some BSValueHandle)
      , (5, fmap Some $ BSWrite <$> genSlotNo <*>
              pure (Mock.makeWriteHint (Proxy @d)) <*> genDiff)
      ]

    withVars ::
         Gen (BSVar ks vs d (Either Err (BS.BackingStoreValueHandle IO ks vs)))
      -> [(Int, Gen (Any (LockstepAction (BackingStoreState ks vs d))))]
    withVars genVar = [
          (5, fmap Some $ BSVHClose <$> (opFromRight <$> genVar))
        , (5, fmap Some $ BSVHRangeRead <$> (opFromRight <$> genVar) <*>
                pure (Mock.makeReadHint (Proxy @vs)) <*> QC.arbitrary)
        , (5, fmap Some $ BSVHRead <$> (opFromRight <$> genVar) <*>
                pure (Mock.makeReadHint (Proxy @vs)) <*> QC.arbitrary)
        , (5, fmap Some $ BSVHAtSlot <$> (opFromRight <$> genVar))
        , (5, fmap Some $ BSVHStat <$> (opFromRight <$> genVar))
        ]
      where
        opFromRight :: forall a. GVar Op (Either Err a) -> GVar Op a
        opFromRight = mapGVar (\op -> OpRight `OpComp` op)

    genBackingStorePath :: Gen FS.FsPath
    genBackingStorePath = do
      file <- genBSPFile
      pure . mkFsPath $ ["copies", file]

    -- Generate a file name for a copy of the backing store contents. We keep
    -- the set of possible file names small, such that errors (i.e., file alread
    -- exists) occur most of the time.
    genBSPFile :: Gen String
    genBSPFile = QC.elements [show x | x <- [1 :: Int .. 10]]

    -- Generate a slot number that is close before, at, or after the backing
    -- store's current slot number. A
    genSlotNo :: Gen SlotNo
    genSlotNo = do
        n :: Int <- QC.choose (-5, 5)
        pure $ maybe 0 (+ fromIntegral n) (withOriginToMaybe seqNo)
      where
        seqNo = backingSeqNo mock

    -- Generate valid diffs most of the time, and generate fully arbitrary
    -- (probably invalid) diffs some of the time.
    genDiff :: Gen d
    genDiff = QC.frequency [
        (9, Mock.diff (backingValues mock) <$> QC.arbitrary)
      , (1, QC.arbitrary)
      ]

{-------------------------------------------------------------------------------
  Shrinker
-------------------------------------------------------------------------------}

shrinkBackingStoreAction ::
       forall ks vs d a.
       ( Typeable vs, Eq ks, Eq vs, Eq d
       , Eq (BS.InitHint vs), Eq (BS.WriteHint d), Eq (BS.ReadHint vs)
       , QC.Arbitrary d, QC.Arbitrary (BS.RangeQuery ks), QC.Arbitrary ks
       )
    => ModelVarContext (BackingStoreState ks vs d)
    -> BackingStoreState ks vs d
    -> LockstepAction (BackingStoreState ks vs d) a
    -> [Any (LockstepAction (BackingStoreState ks vs d))]
shrinkBackingStoreAction _findVars (BackingStoreState _mock _) = \case
  BSWrite sl st d ->
       [Some $ BSWrite sl  st d' | d' <- QC.shrink d]
    ++ [Some $ BSWrite sl' st d  | sl' <- QC.shrink sl]
  BSVHRangeRead h rhint rq ->
    [Some $ BSVHRangeRead h rhint rq' | rq' <- QC.shrink rq]
  BSVHRead h rhint ks ->
    [Some $ BSVHRead h rhint ks' | ks' <- QC.shrink ks]
  _ -> []

{-------------------------------------------------------------------------------
  Interpret @'Op'@ against @'ModelValue'@
-------------------------------------------------------------------------------}

instance InterpretOp Op (ModelValue (BackingStoreState ks vs d)) where
  intOp OpId         = Just
  intOp OpFst        = \case MPair   x -> Just (fst x)
  intOp OpSnd        = \case MPair   x -> Just (snd x)
  intOp OpLeft       = \case MEither x -> either Just (const Nothing) x
  intOp OpRight      = \case MEither x -> either (const Nothing) Just x
  intOp (OpComp g f) = intOp g <=< intOp f

{-------------------------------------------------------------------------------
  Interpreter for implementation (@'RealMonad'@)
-------------------------------------------------------------------------------}

runIO ::
     forall ks vs d a.
     LockstepAction (BackingStoreState ks vs d) a
  -> LookUp (RealMonad IO ks vs d)
  -> RealMonad IO ks vs d (Realized (RealMonad IO ks vs d) a)
runIO action lookUp = ReaderT $ \renv ->
    aux renv action
  where
    aux ::
         RealEnv IO ks vs d
      -> LockstepAction (BackingStoreState ks vs d) a
      -> IO a
    aux renv = \case
        BSInitFromValues sl h (Values vs) -> catchErr $ do
          bs <- bsi (BS.InitFromValues sl h vs)
          void $ swapMVar bsVar bs
        BSInitFromCopy h bsp -> catchErr $ do
          bs <- bsi (BS.InitFromCopy h bsp)
          void $ swapMVar bsVar bs
        BSClose            -> catchErr $
          readMVar bsVar >>= BS.bsClose
        BSCopy bsp         -> catchErr $
          readMVar bsVar >>= \bs -> BS.bsCopy bs bsp
        BSValueHandle      -> catchErr $
          readMVar bsVar >>= BS.bsValueHandle
        BSWrite sl whint d    -> catchErr $
          readMVar bsVar >>= \bs -> BS.bsWrite bs sl whint d
        BSVHClose var        -> catchErr $
          BS.bsvhClose (lookUp' var)
        BSVHRangeRead var rhint rq -> catchErr $ Values <$>
          BS.bsvhRangeRead (lookUp' var) rhint rq
        BSVHRead var rhint ks      -> catchErr $ Values <$>
          BS.bsvhRead (lookUp' var) rhint ks
        BSVHAtSlot var -> catchErr $
          pure (BS.bsvhAtSlot (lookUp' var))
        BSVHStat var         -> catchErr $
          BS.bsvhStat (lookUp' var)
      where
        RealEnv{
            reBackingStoreInit = bsi
          , reBackingStore     = bsVar
          } = renv

        lookUp' :: BSVar ks vs d x -> Realized (RealMonad IO ks vs d) x
        lookUp' = lookUpGVar (Proxy @(RealMonad IO ks vs d)) lookUp

catchErr :: forall m a. IOLike m => m a -> m (Either Err a)
catchErr act = catches (Right <$> act)
    [mkHandler fromTVarExn, mkHandler fromTVarExn', mkHandler fromDbErr]

{-------------------------------------------------------------------------------
  Statistics and tagging
-------------------------------------------------------------------------------}

data Stats ks vs d = Stats {
    -- | Slots that value handles were created in
    handleSlots         :: Map (ValueHandle vs) (WithOrigin SlotNo)
    -- | Slots in which writes were performed
  , writeSlots          :: Map SlotNo Int
    -- | A value handle was created before a write, and read after the write
  , readAfterWrite      :: Bool
    -- | A value handle was created before a write, and range read after the
    -- write
  , rangeReadAfterWrite :: Bool
  }
  deriving stock (Show, Eq)


initStats :: Stats ks vs d
initStats = Stats {
    handleSlots         = Map.empty
  , writeSlots          = Map.empty
  , readAfterWrite      = False
  , rangeReadAfterWrite = False
  }

updateStats ::
     forall ks vs d a. (Mock.HasOps ks vs d, QC.Arbitrary ks, QC.Arbitrary vs, QC.Arbitrary d, QC.Arbitrary (BS.RangeQuery ks))
  => LockstepAction (BackingStoreState ks vs d) a
  -> ModelVarContext (BackingStoreState ks vs d)
  -> BSVal ks vs d a
  -> Stats ks vs d
  -> Stats ks vs d
updateStats action lookUp result stats@Stats{handleSlots, writeSlots} =
      updateHandleSlots
    . updateWriteSlots
    . updateReadAfterWrite
    . updateRangeReadAfterWrite
    $ stats
  where
    getHandle :: BSVal ks vs d (BS.BackingStoreValueHandle IO ks vs) -> ValueHandle vs
    getHandle (MValueHandle h) = h

    updateHandleSlots :: Stats ks vs d -> Stats ks vs d
    updateHandleSlots s = case (action, result) of
      (BSValueHandle, MEither (Right (MValueHandle h)))
        -> s {handleSlots = Map.insert h (seqNo h) handleSlots}
      (BSClose, MEither (Right _))
        -> s {handleSlots = Map.empty}
      (BSVHClose h, MEither (Right _))
        -> s {handleSlots = Map.delete (getHandle $ lookupVar lookUp h) handleSlots}
      _ -> s

    updateWriteSlots :: Stats ks vs d -> Stats ks vs d
    updateWriteSlots s = case (action, result) of
      (BSWrite sl _ d, MEither (Right (MUnit ())))
        | 1 <= Mock.diffSize d
        -> s {writeSlots = Map.insert sl (Mock.diffSize d) writeSlots}
      (BSClose, MEither (Right _))
        -> s {writeSlots = Map.empty}
      _ -> s

    updateReadAfterWrite :: Stats ks vs d -> Stats ks vs d
    updateReadAfterWrite s = case (action, result) of
      (BSVHRead h _ _, MEither (Right (MValues vs)))
        | h' <- getHandle $ lookupVar lookUp h
        , Just wosl <- Map.lookup h' handleSlots
        , Just (sl, _) <- Map.lookupMax writeSlots
        , wosl < at sl
        , 1 <= Mock.valuesLength vs
        -> s {readAfterWrite = True}
      _ -> s

    updateRangeReadAfterWrite :: Stats ks vs d -> Stats ks vs d
    updateRangeReadAfterWrite s = case (action, result) of
      (BSVHRangeRead h _ _, MEither (Right (MValues vs)))
        | h' <- getHandle $ lookupVar lookUp h
        , Just wosl <- Map.lookup h' handleSlots
        , Just (sl, _) <- Map.lookupMax writeSlots
        , wosl < at sl
        , 1 <= Mock.valuesLength vs
        -> s {rangeReadAfterWrite = True}
      _ -> s

data TagAction =
    TBSInitFromValues
  | TBSInitFromCopy
  | TBSClose
  | TBSCopy
  | TBSValueHandle
  | TBSWrite
  | TBSVHClose
  | TBSVHRangeRead
  | TBSVHRead
  | TBSVHAtSlot
  | TBSVHStat
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Identify actions by their constructor.
tAction :: LockstepAction (BackingStoreState ks vs d) a -> TagAction
tAction = \case
  BSInitFromValues _ _ _ -> TBSInitFromValues
  BSInitFromCopy _ _     -> TBSInitFromCopy
  BSClose                -> TBSClose
  BSCopy _               -> TBSCopy
  BSValueHandle          -> TBSValueHandle
  BSWrite _ _ _          -> TBSWrite
  BSVHClose _            -> TBSVHClose
  BSVHRangeRead _ _ _    -> TBSVHRangeRead
  BSVHRead _ _ _         -> TBSVHRead
  BSVHAtSlot _           -> TBSVHAtSlot
  BSVHStat _             -> TBSVHStat

data Tag =
    -- | A value handle is created before a write, and read after the write. The
    -- write should not affect the result of the read.
    ReadAfterWrite
    -- | A value handle is created before a write, and read after the write. The
    -- write should not affect the result of the read.
  | RangeReadAfterWrite
  | ErrorBecauseBackingStoreIsClosed TagAction
  | ErrorBecauseBackingStoreValueHandleIsClosed TagAction
  deriving (Show)

tagBSAction ::
     Stats ks vs d
  -> Stats ks vs d
  -> LockstepAction (BackingStoreState ks vs d) a
  -> BSVal ks vs d a
  -> [Tag]
tagBSAction before after action result =
    globalTags ++ case (action, result) of
      (_, MEither (Left (MErr ErrBackingStoreClosed))) ->
        [ErrorBecauseBackingStoreIsClosed (tAction action)]
      (_, MEither (Left (MErr ErrBackingStoreValueHandleClosed))) ->
        [ErrorBecauseBackingStoreValueHandleIsClosed (tAction action)]
      _ -> []
  where
    globalTags = mconcat [
        [ ReadAfterWrite
        | not (readAfterWrite before)
        , readAfterWrite after
        ]
      , [ RangeReadAfterWrite
        | not (rangeReadAfterWrite before)
        , rangeReadAfterWrite after
        ]
      ]

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

mkHandler ::
     (IOLike m, Exception e)
  => (e -> Maybe Err)
  -> Handler m (Either Err a)
mkHandler fhandler = Handler $
  \e -> maybe (throwIO e) (return . Left) (fhandler e)

-- | Map LMDB errors to mock errors.
fromDbErr :: LMDB.LMDBErr -> Maybe Err
fromDbErr = \case
  LMDBErrNoDbSeqNo                   -> Nothing
  LMDBErrNonMonotonicSeq wo wo'      -> Just $ ErrNonMonotonicSeqNo wo wo'
  LMDBErrInitialisingNonEmpty _      -> Nothing
  LMDBErrNoValueHandle _             -> Just ErrBackingStoreValueHandleClosed
  LMDBErrBadRead                     -> Nothing
  LMDBErrBadRangeRead                -> Nothing
  LMDBErrDirExists _                 -> Just ErrCopyPathAlreadyExists
  LMDBErrDirDoesntExist _            -> Just ErrCopyPathDoesNotExist
  LMDBErrDirIsNotLMDB _              -> Nothing
  LMDBErrClosed                      -> Just ErrBackingStoreClosed
  LMDBErrInitialisingAlreadyHasState -> Nothing
  LMDBErrUnableToReadSeqNo           -> Nothing
  LMDBErrNotADir _                   -> Nothing

-- | Map InMemory (i.e., @TVarBackingStore@) errors to mock errors.
fromTVarExn :: BS.InMemoryBackingStoreExn -> Maybe Err
fromTVarExn = \case
  BS.InMemoryBackingStoreClosedExn              -> Just ErrBackingStoreClosed
  BS.InMemoryBackingStoreValueHandleClosedExn   -> Just ErrBackingStoreValueHandleClosed
  BS.InMemoryBackingStoreDirectoryExists        -> Just ErrCopyPathAlreadyExists
  BS.InMemoryBackingStoreNonMonotonicSeq wo wo' -> Just $ ErrNonMonotonicSeqNo wo wo'
  BS.InMemoryBackingStoreDeserialiseExn _       -> Nothing
  BS.InMemoryIncompleteDeserialiseExn           -> Nothing

fromTVarExn' :: BS.InMemoryBackingStoreInitExn -> Maybe Err
fromTVarExn' = \case
  BS.StoreDirIsIncompatible _ -> Just ErrCopyPathDoesNotExist
