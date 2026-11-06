{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Controlled local-source, ownership, wakeup and shutdown schedules for E22.
module Test.Consensus.Leios.LeiosFetch.LocalReuse (tests) where

import Cardano.Crypto.Hash (castHash)
import Control.Monad (forM_, void, when)
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.Monad.IOSim (runSimOrThrow)
import qualified Data.ByteString as BS
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Leios.LocalStore
import Ouroboros.Consensus.Leios.TxCache
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Handoff as H
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.LocalReuse
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Wait as W
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity
import Ouroboros.Consensus.Storage.LeiosDB
import Ouroboros.Consensus.Util.IOLike
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Util.Orphans.IOLike ()

-- Sources and target references are deliberately separate: the scalar index
-- cannot substitute membership or a claimed size for verified serialized bytes.
data Fixture m = Fixture
  { handoff :: H.StorageHandoff m Int Int
  , cache :: LeiosTxCache m
  , writer :: LeiosDbWriter m
  , database :: LeiosDbHandle m
  , source :: LocalSource m BS.ByteString
  , index :: StrictTVar m (Natural, Map.Map TxHash (BytesSize, BS.ByteString))
  , ready :: W.Readiness m
  , closing :: StrictTVar m Bool
  , generation :: AcquisitionGeneration
  , target :: LeiosPoint
  , expected :: [ExpectedTx]
  }

payload :: Int -> BS.ByteString
payload n = BS.replicate 6 (fromIntegral n)

hashOf :: Int -> TxHash
hashOf = hashLeiosTx . MkLeiosTx . payload

right :: Show e => Either e a -> a
right = either (error . show) id

withFixture :: IOLike m => H.LocalStorageLimits -> (Fixture m -> m a) -> m a
withFixture limits action = bracket newLeiosDBInMemory closeLeiosDbHandle $ \raw -> do
  cache <- newReferenceCache (CacheConfig 2 16)
  let database = observeLeiosDB cache raw
  withWriter database $ \writer -> do
    handoff <- right <$> H.newStorageHandoffWithLocal (H.StorageLimits 4096 32 8 8 6) (Just limits)
    let body = mkEndorserBlock [(hashOf 1, 6), (hashOf 2, 6)]
        target = MkLeiosPoint 10 (endorserBlockHash body)
        expected = [ExpectedTx 0 (hashOf 1) 6, ExpectedTx 1 (hashOf 2) 6]
    generation <- right <$> atomically (H.beginAcquisition handoff target)
    recordAnnouncement cache 10 (MkRankingBlockHash (castHash (hashOf 10))) (pointEbHash target)
    held <- await =<< writeEbBody writer target body
    void $ H.registerDurableBody handoff (const (pure ())) target generation body (IntSet.fromList held)
    index <- uncheckedNewTVarM (0, Map.empty)
    ready <- atomically W.newReadiness
    closing <- uncheckedNewTVarM False
    let source = LocalSource (readTVar index) (pure . Just . MkLeiosTx)
    action
      Fixture
        { handoff
        , cache
        , writer
        , database
        , source
        , index
        , ready
        , closing
        , generation
        , target
        , expected
        }

setSource :: MonadSTM m => Fixture m -> [Int] -> STM m ()
setSource f txs = modifyTVar (index f) $ \(epoch, _) -> (epoch + 1, Map.fromList [(hashOf n, (6, payload n)) | n <- txs])

snapshot :: MonadSTM m => Fixture m -> STM m H.AcquisitionSnapshot
snapshot f = fromJust <$> H.readAcquisition (handoff f) (target f)

notify :: MonadSTM m => Fixture m -> H.StorageEvent -> STM m ()
notify f _ = W.notifyReadiness (ready f)

stop :: MonadSTM m => Fixture m -> STM m ()
stop f = writeTVar (closing f) True >> W.notifyReadiness (ready f)

runResolver :: IOLike m => Fixture m -> m ()
runResolver f =
  runLocalResolver
    (LocalResolverConfig 6 1)
    (handoff f)
    (cache f)
    (source f)
    (ready f)
    (readTVar (closing f))
    (pure ())

withResolver :: IOLike m => Fixture m -> m a -> m a
withResolver f action = withAsync (runResolver f) $ \resolver -> do
  link resolver
  result <- action `finally` atomically (stop f)
  wait resolver
  pure result

awaitCondition :: (IOLike m, SI.MonadTimer m) => STM m Bool -> m ()
awaitCondition condition =
  SI.timeout 5 (atomically (condition >>= check))
    >>= maybe (error "local fixture failed to progress") pure

slice :: IOLike m => Fixture m -> [ExpectedTx] -> m IntSet.IntSet
slice f es =
  resolveLocalSlice
    (handoff f)
    (cache f)
    (source f)
    (readTVar (closing f))
    (pure ())
    (target f)
    (generation f)
    True
    IntSet.empty
    IntSet.empty
    es

seedCache :: IOLike m => Fixture m -> m TxLocation
seedCache f = do
  let body = mkEndorserBlock [(hashOf 2, 6)]
      point = MkLeiosPoint 3 (endorserBlockHash body)
  recordAnnouncement (cache f) 3 (MkRankingBlockHash (castHash (hashOf 3))) (pointEbHash point)
  void $ await =<< writeEbBody (writer f) point body
  void $ await =<< writeTxs (writer f) point [(0, payload 2)]
  pure (MkTxLocation point 0)

tests :: TestTree
tests =
  testGroup
    "local transaction reuse"
    [ testCase "R11 unrelated source churn cannot starve another acquisition's network miss" $
        assertBool "second acquisition is classified while local capacity stays full" $
          runSimOrThrow $
            withFixture (H.LocalStorageLimits 6 1) $ \f -> do
              reservation <-
                right
                  <$> atomically (H.reserveLocalTransaction (handoff f) (target f) (generation f) (head (expected f)))
              atomically $ setSource f [2]
              let body2 = mkEndorserBlock [(hashOf 3, 6)]
                  point2 = MkLeiosPoint 11 (endorserBlockHash body2)
              gen2 <- right <$> atomically (H.beginAcquisition (handoff f) point2)
              _ <- await =<< writeEbBody (writer f) point2 body2
              _ <- H.registerDurableBody (handoff f) (const (pure ())) point2 gen2 body2 IntSet.empty
              result <- withResolver f
                $ withAsync
                  ( forM_ [1 .. 100 :: Int] $ \_ -> do
                      atomically $ setSource f [2]
                      yield
                  )
                $ \churn -> do
                  awaitCondition $ do
                    second <- H.readAcquisition (handoff f) point2
                    pure (maybe False ((== IntSet.singleton 0) . H.snapshotFetchable) second)
                  wait churn
                  usage <- atomically $ H.readStorageSnapshot (handoff f)
                  pure (H.snapshotLocalReservedBytes usage == 6)
              atomically $ H.dropLocalReservation (handoff f) reservation
              pure result
    , testCase "R10 GC releases the cache lock before invoking database GC" $ do
        withFixture H.defaultLocalStorageLimits $ \f -> do
          _ <- seedCache f
          let raw = (database f){garbageCollect = \_ -> void (readCacheStats (cache f))}
          result <- SI.timeout 1 (garbageCollect (observeLeiosDB (cache f) raw) 11)
          result @?= Just ()
    , testCase "R10 a late receipt cannot republish a pruned source" $ do
        withFixture H.defaultLocalStorageLimits $ \f -> do
          _ <- seedCache f
          gate <- newEmptyTMVarIO
          let db =
                (database f)
                  { openWriter = do
                      w <- openWriter (database f)
                      pure
                        w
                          { writeTxs = \p txs -> do
                              promise <- writeTxs w p txs
                              pure $ Promise $ atomically (readTMVar gate) >> await promise
                          }
                  }
          withWriter (observeLeiosDB (cache f) db) $ \w -> do
            let b = mkEndorserBlock [(hashOf 2, 6)]; point = MkLeiosPoint 3 (endorserBlockHash b)
            pending <- writeTxs w point [(0, payload 2)]
            garbageCollect (database f) 4
            atomically $ putTMVar gate ()
            _ <- await pending
            (_, hits) <- lookupLocations (cache f) [(0, hashOf 2)]
            hits @?= [(0, Nothing)]
    , testCase "R14 local storage failure wakes the owner instead of hanging drain" $ do
        withFixture H.defaultLocalStorageLimits $ \f -> do
          atomically $ setSource f [1]
          let failed =
                (writer f){writeTxs = \_ _ -> pure $ Promise $ throwIO (userError "injected local receipt failure")}
          outcome <-
            try
              ( SI.timeout 1 $ H.withStorageHandoff (handoff f) failed (notify f) $ \_ -> do
                  _ <- slice f [head (expected f)]
                  atomically retry
              ) ::
              IO (Either SomeException (Maybe ()))
          case outcome of
            Left _ -> pure ()
            Right _ -> assertFailure "local writer error did not reach the node owner"
    , testCase "all mempool TXs become durable with no network offer/session" $
        assertBool "local closure acquired" $
          runSimOrThrow $
            withFixture H.defaultLocalStorageLimits $ \f -> do
              atomically $ setSource f [1, 2]
              H.withStorageHandoff (handoff f) (writer f) (notify f) $ \_ -> withResolver f $ do
                awaitCondition ((== IntSet.fromList [0, 1]) . H.snapshotHeld <$> snapshot f)
                usage <- atomically $ H.readStorageSnapshot (handoff f)
                pure (H.snapshotReservedBytes usage == 0 && H.snapshotSessions usage == 0)
    , testCase "all cache copies and mixed cache/mempool acquire exact target bytes" $
        assertBool "mixed local closure acquired" $
          runSimOrThrow $
            withFixture H.defaultLocalStorageLimits $ \f -> do
              _ <- seedCache f
              atomically $ setSource f [1]
              H.withStorageHandoff (handoff f) (writer f) (notify f) $ \_ -> withResolver f $ do
                awaitCondition ((== IntSet.fromList [0, 1]) . H.snapshotHeld <$> snapshot f)
                withReader (database f) $ \reader -> do
                  rows <- batchRetrieveTxs reader (target f) [0, 1]
                  pure (rows == [(0, hashOf 1, Just (payload 1)), (1, hashOf 2, Just (payload 2))])
    , testCase "no local hits classify every missing TX for network service" $
        assertBool "eligible, not held" $
          runSimOrThrow $
            withFixture H.defaultLocalStorageLimits $ \f -> withResolver f $ do
              awaitCondition ((== IntSet.fromList [0, 1]) . H.snapshotFetchable <$> snapshot f)
              IntSet.null . H.snapshotHeld <$> atomically (snapshot f)
    , testCase "R01 source removal alone releases a deferred candidate" $
        assertBool "source-only wake" $
          runSimOrThrow $
            withFixture (H.LocalStorageLimits 6 1) $ \f -> do
              reservation <-
                right
                  <$> atomically (H.reserveLocalTransaction (handoff f) (target f) (generation f) (head (expected f)))
              atomically $ setSource f [2]
              withResolver f $ do
                awaitCondition (IntSet.member 1 . H.snapshotLocalClassified <$> snapshot f)
                before <- atomically $ H.readLocalRevision (handoff f)
                atomically $ setSource f []
                awaitCondition (IntSet.member 1 . H.snapshotFetchable <$> snapshot f)
                after <- atomically $ H.readLocalRevision (handoff f)
                atomically $ H.dropLocalReservation (handoff f) reservation
                pure (before == after)
    , testCase "R02 cache change after lookup is not swallowed at pass completion" $ do
        result <- SI.timeout 5 $ withFixture (H.LocalStorageLimits 6 1) $ \f -> do
          location <- seedCache f
          reservation <-
            right
              <$> atomically (H.reserveLocalTransaction (handoff f) (target f) (generation f) (head (expected f)))
          entered <- newEmptyTMVarIO
          release <- newEmptyTMVarIO
          calls <- uncheckedNewTVarM (0 :: Int)
          let original = cache f
              gated =
                original
                  { lookupLocations = \requested -> do
                      result <- lookupLocations original requested
                      n <- atomically $ do n <- readTVar calls; writeTVar calls (n + 1); pure n
                      when (n == 0) $ atomically (putTMVar entered ()) >> atomically (readTMVar release)
                      pure result
                  }
          withResolver f{cache = gated} $ do
            atomically $ readTMVar entered
            invalidateLocationIfMatches original (hashOf 2) location
            atomically $ putTMVar release ()
            awaitCondition (IntSet.member 1 . H.snapshotFetchable <$> snapshot f)
          atomically $ H.dropLocalReservation (handoff f) reservation
        result @?= Just ()
    , testCase "R04 cancel during extraction refunds the uncommitted reservation" $ do
        withFixture H.defaultLocalStorageLimits $ \f -> do
          atomically $ setSource f [1]
          entered <- newEmptyTMVarIO
          release <- newEmptyTMVarIO
          let gated =
                (source f)
                  { localSourceBytes = \bytes -> do
                      atomically $ putTMVar entered ()
                      _ <- atomically $ readTMVar release
                      pure (Just (MkLeiosTx bytes))
                  }
          withAsync (slice f{source = gated} [head (expected f)]) $ \resolver -> do
            atomically $ readTMVar entered
            charged <- atomically $ H.readStorageSnapshot (handoff f)
            H.snapshotLocalReservedBytes charged @?= 6
            void $ cancel resolver
          after <- atomically $ H.readStorageSnapshot (handoff f)
          H.snapshotLocalReservedBytes after @?= 0
          H.snapshotClaims after @?= 0
    , testCase "R04 retirement during extraction cannot attach to the new generation" $
        withFixture H.defaultLocalStorageLimits $ \f -> do
          atomically $ setSource f [1]
          entered <- newEmptyTMVarIO
          release <- newEmptyTMVarIO
          let gated =
                (source f)
                  { localSourceBytes = \bytes -> do
                      atomically $ putTMVar entered ()
                      _ <- atomically $ readTMVar release
                      pure (Just (MkLeiosTx bytes))
                  }
          withAsync (slice f{source = gated} [head (expected f)]) $ \resolver -> do
            atomically $ readTMVar entered
            atomically $ H.retireAcquisition (handoff f) (target f) (generation f)
            newGen <- right <$> atomically (H.beginAcquisition (handoff f) (target f))
            atomically $ putTMVar release ()
            _ <- wait resolver
            snapshotNow <- atomically $ snapshot f
            H.snapshotGeneration snapshotNow @?= newGen
            H.snapshotPending snapshotNow @?= IntSet.empty
            H.snapshotHeld snapshotNow @?= IntSet.empty
    , testCase "R12 size mismatch never materializes an uncharged large source" $
        withFixture H.defaultLocalStorageLimits $ \f -> do
          atomically $ writeTVar (index f) (1, Map.singleton (hashOf 1) (1000000, payload 1))
          let forbidden = (source f){localSourceBytes = const (error "wrong-size source was materialized")}
          _ <- slice f{source = forbidden} [head (expected f)]
          H.snapshotFetchable <$> atomically (snapshot f) >>= (@?= IntSet.singleton 0)
          H.snapshotLocalReservedBytes <$> atomically (H.readStorageSnapshot (handoff f)) >>= (@?= 0)
    , testCase "R14 closing escapes an unavailable source STM condition" $
        assertBool "resolver stopped" $
          runSimOrThrow $
            withFixture H.defaultLocalStorageLimits $ \f -> do
              started <- newEmptyTMVarIO
              unavailable <- uncheckedNewTVarM False
              let blocked = (source f){localSourceIndex = readTVar unavailable >>= check >> localSourceIndex (source f)}
              withAsync (atomically (putTMVar started ()) >> runResolver f{source = blocked}) $ \resolver -> do
                atomically $ readTMVar started
                yield
                atomically $ stop f
                wait resolver
                pure True
    , testCase "R11 unchanged full capacity parks instead of re-probing" $
        assertBool "bounded memory passes" $
          runSimOrThrow $
            withFixture (H.LocalStorageLimits 6 1) $ \f -> do
              reservation <-
                right
                  <$> atomically (H.reserveLocalTransaction (handoff f) (target f) (generation f) (head (expected f)))
              atomically $ setSource f [2]
              calls <- uncheckedNewTVarM (0 :: Int)
              let original = cache f
                  counted =
                    original
                      { lookupLocations = \requested -> do
                          atomically $ modifyTVar calls (+ 1)
                          lookupLocations original requested
                      }
              withResolver f{cache = counted} $ do
                awaitCondition (IntSet.member 1 . H.snapshotLocalClassified <$> snapshot f)
                -- Simulated time advances only after runnable work has parked.
                threadDelay 0.1
                before <- readTVarIO calls
                threadDelay 0.1
                after <- readTVarIO calls
                atomically $ H.dropLocalReservation (handoff f) reservation
                pure (before == after && before <= 3)
    ]
