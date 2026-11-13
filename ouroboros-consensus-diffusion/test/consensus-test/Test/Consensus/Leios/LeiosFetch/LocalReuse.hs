{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bounded local discovery and node-owned receipt lifecycle regressions.
module Test.Consensus.Leios.LeiosFetch.LocalReuse (tests) where

import qualified Control.Concurrent.Class.MonadSTM.Strict as S
import Control.Monad (forM_, void, when)
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.Tracer (mkTracer)
import qualified Data.ByteString as BS
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Strict as V
import qualified Ouroboros.Consensus.Leios.TxCache as Cache
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies hiding
  ( processLeiosBlock
  , processLeiosBlockTxs
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.LocalReuse
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.ReceiptTasks as Receipts
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
import Ouroboros.Consensus.Storage.LeiosDB
import Ouroboros.Consensus.Util.IOLike
import Test.Consensus.Leios.LeiosFetch.Replies
  ( awaitNotes
  , fixture
  , newContext
  , newContextHolding
  , processLeiosBlock
  , processLeiosBlockTxs
  )
import Test.Tasty
import Test.Tasty.HUnit

bounded :: IO () -> IO ()
bounded action = SI.timeout 5 action >>= maybe (assertFailure "bounded local/receipt test did not finish") pure

sourceOf :: [LeiosTx] -> LocalSource IO LeiosTx
sourceOf txs =
  LocalSource
    { localLookup = \requested ->
        pure $!
          Map.fromList
            [ (h, tx)
            | (h, size) <- requested
            , Just tx <- [Map.lookup h byHash]
            , BS.length (leiosTxBytes tx) == fromIntegral size
            ]
    , localBytes = pure . Just
    }
 where
  byHash = Map.fromList [(hashLeiosTx tx, tx) | tx <- txs]

withLocal ::
  [LeiosTx] -> (LeiosPoint -> EndorserBlock -> LeiosDbHandle IO -> LeiosDbWriter IO -> IO ()) -> IO ()
withLocal txs action = bracket newLeiosDBInMemory closeLeiosDbHandle $ \db -> withWriter db $ \writer -> do
  let eb =
        endorserBlockFromTxBytes
          (case map leiosTxBytes txs of x : xs -> x :| xs; [] -> error "empty fixture")
      point = MkLeiosPoint 5 (endorserBlockHash eb)
  _ <- await =<< writeEbBody writer point eb []
  action point eb db writer

resolve :: LocalReuse IO -> LeiosPoint -> EndorserBlock -> IntSet.IntSet -> IO IntSet.IntSet
resolve local point eb held = reuseMempool local (pure True) point held (zip [0 ..] (endorserBlockReferences eb))

tests :: TestTree
tests =
  testGroup
    "bounded local reuse and receipts"
    [ testCase "DB-filled closures do not wait on a missing mempool" $ bounded $ withLocal txs $ \p eb _ writer -> do
        _ <- await =<< writeTxs writer p [(i, leiosTxBytes tx) | (i, tx) <- zip [0 ..] txs]
        local <- newLocalReuse (pure False) (LocalSource (const retry) (\() -> pure Nothing)) writer
        resolve local p eb (IntSet.fromList [0, 1]) >>= (@?= IntSet.fromList [0, 1])
    , testCase "DB-filled closures bypass an occupied materialization gate" testHeldBypassesGate
    , testCase "exact mempool bytes fill target rows" $ bounded $ withLocal txs $ \p eb db writer -> do
        local <- newLocalReuse (pure False) (sourceOf txs) writer
        resolve local p eb IntSet.empty >>= (@?= IntSet.fromList [0, 1])
        withReader db $ \reader ->
          lookupEbClosure reader p >>= \found ->
            fmap (map snd) found @?= Just (map leiosTxBytes txs)
    , testCase "wrong hash, size or missing projection stays fetchable" $ bounded $ withLocal txs $ \p eb _ writer -> do
        forM_ [pure (Just (MkLeiosTx (BS.replicate 7 0))), pure Nothing] $ \project -> do
          local <- newLocalReuse (pure False) ((sourceOf txs){localBytes = const project}) writer
          resolve local p eb IntSet.empty >>= (@?= IntSet.empty)
    , testCase "unconfirmed imports do not suppress work" $ bounded $ withLocal txs $ \p eb _ writer -> do
        let unconfirmed = writer{writeTxs = \_ _ -> pure (Promise (pure (TxWriteReceipt IntSet.empty [])))}
        local <- newLocalReuse (pure False) (sourceOf txs) unconfirmed
        resolve local p eb IntSet.empty >>= (@?= IntSet.empty)
    , testCase "entry cap splits tiny transactions" $
        bounded $
          checkSlices [MkLeiosTx (BS.singleton i) | i <- [0 .. 128]] [128, 1]
    , testCase "byte cap allows one indivisible overhang" $
        bounded $
          checkSlices [MkLeiosTx (BS.replicate 19000 i) | i <- [0 .. 2]] [2, 1]
    , testCase "retired points stop before source lookup" $ bounded $ withLocal txs $ \p eb _ writer -> do
        local <- newLocalReuse (pure False) (LocalSource (const retry) (\() -> pure Nothing)) writer
        reuseMempool local (pure False) p IntSet.empty (zip [0 ..] (endorserBlockReferences eb))
          >>= (@?= IntSet.empty)
    , testCase "closing wakes a retrying mempool source" $ bounded $ withLocal txs $ \p eb _ writer -> do
        closing <- S.newTVarIO False
        local <- newLocalReuse (S.readTVar closing) (LocalSource (const retry) (\() -> pure Nothing)) writer
        withAsync (resolve local p eb IntSet.empty) $ \task -> do
          yield
          atomically (S.writeTVar closing True)
          wait task >>= (@?= IntSet.empty)
    , testCase "cancelled materialization releases the permit" testCancelProjection
    , testCase "closing wakes a queued import without stealing the active permit" testClosingGate
    , testCase "cancelled queued import cannot release the active permit" testCancelQueued
    , testCase "normal drain joins late accepted callbacks" testReceiptDrain
    , testCase "failure during normal drain is supervised" testFailureDuringDrain
    , testCase "abort quiesces pre-acceptance producers" testProducerAbort
    , testCase "abort cancels and joins accepted callbacks" testReceiptAbort
    , testCase "writer failure aborts source owner and queued imports" testWriterFailure
    , testCase "body settlement omits locally held transactions" testBodyIntegration
    , testCase "cached bodies use the same bounded local pass" testCachedBody
    , testCase "local receipt survives peer removal" testPeerRemoval
    , testCase "retirement between slices stops further optional imports" testRetireBetweenSlices
    , testCase "accepted TX settlement survives post-acceptance tracing failure" testTraceFailure
    , testCase "interrupted forge TX submission still owns its accepted body" testForgeBodyOwner
    ]
 where
  (_, _, txs) = fixture

checkSlices :: [LeiosTx] -> [Int] -> IO ()
checkSlices txs sizes = withLocal txs $ \p eb _ writer -> do
  calls <- S.newTVarIO []
  let counted =
        writer
          { writeTxs = \point payload -> do
              atomically $ S.modifyTVar calls (<> [length payload])
              writeTxs writer point payload
          }
  local <- newLocalReuse (pure False) (sourceOf txs) counted
  found <- resolve local p eb IntSet.empty
  IntSet.size found @?= length txs
  S.readTVarIO calls >>= (@?= sizes)

-- A blocked first projection holds the sole active permit. All observations
-- come from source operations and task completion, never production hooks.
blockedSource :: [LeiosTx] -> IO (LocalSource IO LeiosTx, S.StrictTMVar IO (), S.StrictTMVar IO ())
blockedSource txs = do
  first <- S.newTVarIO True
  entered <- S.newEmptyTMVarIO
  release <- S.newEmptyTMVarIO
  let original = sourceOf txs
      source =
        original
          { localBytes = \tx -> do
              block <- atomically $ do a <- S.readTVar first; S.writeTVar first False; pure a
              when block $ atomically (S.putTMVar entered ()) >> atomically (S.readTMVar release)
              localBytes original tx
          }
  pure (source, entered, release)

testCancelProjection :: IO ()
testCancelProjection = bounded $ withLocal txs $ \p eb _ writer -> do
  (source, entered, _) <- blockedSource txs
  local <- newLocalReuse (pure False) source writer
  withAsync (resolve local p eb IntSet.empty) $ \active -> do
    atomically (S.readTMVar entered)
    cancel active
  resolve local p eb IntSet.empty >>= (@?= IntSet.fromList [0, 1])
 where
  (_, _, txs) = fixture

testClosingGate :: IO ()
testClosingGate = bounded $ withLocal txs $ \p eb _ writer -> do
  (source, entered, release) <- blockedSource txs
  closing <- S.newTVarIO False
  local <- newLocalReuse (S.readTVar closing) source writer
  withAsync (resolve local p eb IntSet.empty) $ \active -> do
    atomically (S.readTMVar entered)
    withAsync (resolve local p eb IntSet.empty) $ \queued -> do
      atomically (S.writeTVar closing True)
      wait queued >>= (@?= IntSet.empty)
      atomically (S.putTMVar release ())
      wait active >>= (@?= IntSet.empty)
 where
  (_, _, txs) = fixture

testCancelQueued :: IO ()
testCancelQueued = bounded $ withLocal txs $ \p eb _ writer -> do
  (source, entered, release) <- blockedSource txs
  local <- newLocalReuse (pure False) source writer
  withAsync (resolve local p eb IntSet.empty) $ \active -> do
    atomically (S.readTMVar entered)
    withAsync (resolve local p eb IntSet.empty) cancel
    withAsync (resolve local p eb IntSet.empty) $ \third -> do
      SI.timeout 0.02 (wait third) >>= (@?= Nothing)
      atomically (S.putTMVar release ())
      wait active >>= (@?= IntSet.fromList [0, 1])
      wait third >>= (@?= IntSet.fromList [0, 1])
 where
  (_, _, txs) = fixture

testReceiptDrain :: IO ()
testReceiptDrain = bounded $ do
  tasks <- Receipts.newReceiptTasks
  entered <- newEmptyTMVarIO
  accepted <- newEmptyTMVarIO
  durable <- newEmptyTMVarIO
  callback <- uncheckedNewTVarM False
  withAsync
    ( Receipts.withReceiptSubmission tasks $ do
        atomically $ putTMVar entered ()
        atomically $ readTMVar accepted
        Receipts.forkReceipt tasks $ do
          atomically $ readTMVar durable
          atomically $ writeTVar callback True
    )
    $ \producer -> do
      atomically $ readTMVar entered
      withAsync (Receipts.withReceiptTasks tasks (pure ())) $ \scope -> do
        atomically $ Receipts.receiptClosing tasks >>= check
        poll scope >>= \status -> case status of
          Nothing -> pure ()
          _ -> assertFailure "drain ignored an admitted producer"
        atomically $ putTMVar accepted ()
        wait producer
        poll scope >>= \status -> case status of
          Nothing -> pure ()
          _ -> assertFailure "drain ignored an accepted receipt"
        atomically $ putTMVar durable ()
        wait scope
  readTVarIO callback >>= (@?= True)
  Receipts.withReceiptSubmission tasks (assertFailure "submission admitted after shutdown")

testReceiptAbort :: IO ()
testReceiptAbort = bounded $ do
  tasks <- Receipts.newReceiptTasks
  entered <- newEmptyTMVarIO
  finished <- uncheckedNewTVarM False
  result <- try $ Receipts.withReceiptTasks tasks $ do
    Receipts.withReceiptSubmission tasks $
      Receipts.forkReceipt tasks $
        (atomically (putTMVar entered ()) >> atomically retry)
          `finally` atomically (writeTVar finished True)
    atomically $ readTMVar entered
    throwIO (userError "injected node failure")
  case (result :: Either SomeException ()) of
    Left _ -> pure ()
    Right () -> assertFailure "node failure disappeared"
  readTVarIO finished >>= (@?= True)
  atomically (Receipts.receiptClosing tasks) >>= (@?= True)

testFailureDuringDrain :: IO ()
testFailureDuringDrain = bounded $ do
  tasks <- Receipts.newReceiptTasks
  release <- newEmptyTMVarIO
  entered <- newEmptyTMVarIO
  finished <- uncheckedNewTVarM False
  withAsync
    ( Receipts.withReceiptTasks tasks $
        Receipts.withReceiptSubmission tasks $
          Receipts.forkReceipt tasks $
            ( do
                atomically $ putTMVar entered ()
                atomically $ readTMVar release
                throwIO (userError "receipt failed during normal drain")
            )
              `finally` atomically (writeTVar finished True)
    )
    $ \scope -> do
      atomically $ readTMVar entered
      atomically $ Receipts.receiptClosing tasks >>= check
      atomically $ putTMVar release ()
      outcome <- waitCatch scope
      case outcome of
        Left _ -> pure ()
        Right () -> assertFailure "normal drain ignored an accepted write failure"
  readTVarIO finished >>= (@?= True)

testProducerAbort :: IO ()
testProducerAbort = bounded $ do
  tasks <- Receipts.newReceiptTasks
  entered <- newEmptyTMVarIO
  finished <- uncheckedNewTVarM False
  withAsync
    ( Receipts.withReceiptSubmission tasks $
        (atomically (putTMVar entered ()) >> atomically retry)
          `finally` atomically (writeTVar finished True)
    )
    $ \producer -> do
      result <- try $ Receipts.withReceiptTasks tasks $ do
        atomically $ readTMVar entered
        throwIO (userError "node stopped before writer acceptance")
      case (result :: Either SomeException ()) of
        Left _ -> pure ()
        Right () -> assertFailure "node failure disappeared"
      readTVarIO finished >>= (@?= True)
      waitCatch producer >>= \outcome -> case outcome of
        Left _ -> pure ()
        Right () -> assertFailure "blocked submission was not interrupted"

testBodyIntegration :: IO ()
testBodyIntegration = bounded $ forM_ [[], take 1 txs, txs] $ \localTxs -> do
  (ctx, notes) <- newContext
  local <-
    newLocalReuse (Receipts.receiptClosing (lfcReceiptTasks ctx)) (sourceOf localTxs) (lfcWriter ctx)
  let current = ctx{lfcLocalReuse = local}
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ do
    processLeiosBlock current 0 request eb
    void $ awaitNotes notes (if length localTxs == length txs then 2 else 1)
  outstanding <- readMVar (lfcOutstanding ctx)
  case Map.lookup point (ebState outstanding) of
    Just (MkEbState _ (BodyAcquired pool)) -> do
      if length localTxs == length txs
        then Jobs.nullJobPool pool @?= True
        else case Jobs.lookupJob (Jobs.MkLeiosJobId 0) pool of
          Just (Jobs.MkLeiosJob offsets _ _) -> offsets @?= IntSet.fromList [length localTxs .. 1]
          Nothing -> assertFailure "missing network job"
    _ -> assertFailure "body settlement did not finish"
 where
  (point, eb, txs) = fixture
  request =
    MkLeiosBlockRequest
      point
      (encodeEndorserBlockSize eb)
      (MkLeiosEbByteSizeBounds maxBound maxBound)

testTraceFailure :: IO ()
testTraceFailure = bounded $ do
  (ctx, notes) <- newContext
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ do
    processLeiosBlock ctx 0 bodyRequest eb
    void $ awaitNotes notes 1
    outstanding <- readMVar (lfcOutstanding ctx)
    let request = case Map.lookup point (ebState outstanding) of
          Just (MkEbState _ (BodyAcquired pool)) -> case Jobs.lookupJob (Jobs.MkLeiosJobId 0) pool of
            Just job -> MkLeiosBlockTxsRequest point (NEIntMap.singleton 0 job)
            _ -> error "missing request job"
          _ -> error "missing acquired body"
        failing =
          ctx
            { lfcTracer = mkTracer $ \case
                TraceLeiosFetchClientTxsArrived{} -> throwIO (userError "injected post-acceptance trace failure")
                _ -> pure ()
            }
    result <- try (processLeiosBlockTxs failing 0 request (V.fromList txs))
    case result :: Either SomeException () of
      Left _ -> pure ()
      Right () -> assertFailure "trace failure disappeared"
    void $ awaitNotes notes 2
 where
  (point, eb, txs) = fixture
  bodyRequest =
    MkLeiosBlockRequest
      point
      (encodeEndorserBlockSize eb)
      (MkLeiosEbByteSizeBounds maxBound maxBound)

testForgeBodyOwner :: IO ()
testForgeBodyOwner = bounded $ do
  release <- S.newEmptyTMVarIO
  entered <- S.newEmptyTMVarIO
  (ctx, _) <- newContextHolding (atomically (S.readTMVar release))
  let badWriter = (lfcWriter ctx){writeTxs = \_ _ -> atomically (S.putTMVar entered ()) >> atomically retry}
      current = ctx{lfcWriter = badWriter}
      (point, _, txs) = fixture
      forged = forgeLeiosEb EvenProtVer (case map leiosTxBytes txs of x : xs -> x :| xs; [] -> error "fixture")
  withAsync
    ( Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ do
        withAsync (acquireForgedEb current point forged) $ \producer -> do
          atomically (S.readTMVar entered)
          cancel producer
    )
    $ \scope -> do
      atomically (Receipts.receiptClosing (lfcReceiptTasks ctx) >>= check)
      poll scope >>= \case
        Nothing -> pure ()
        _ -> assertFailure "accepted body lost its receipt owner"
      atomically (S.putTMVar release ())
      wait scope

testWriterFailure :: IO ()
testWriterFailure = bounded $ withLocal txs $ \p eb _ writer -> do
  tasks <- Receipts.newReceiptTasks
  entered <- S.newEmptyTMVarIO
  failWrite <- S.newEmptyTMVarIO
  let failing =
        writer
          { writeTxs = \_ _ -> pure $ Promise $ do
              atomically $ void $ S.tryPutTMVar entered ()
              atomically $ S.readTMVar failWrite
              throwIO (userError "injected writer failure")
          }
  local <- newLocalReuse (Receipts.receiptClosing tasks) (sourceOf txs) failing
  result <- try $ Receipts.withReceiptTasks tasks $ do
    Receipts.withReceiptSubmission tasks $ do
      Receipts.forkReceipt tasks (void (resolve local p eb IntSet.empty))
      Receipts.forkReceipt tasks (void (resolve local p eb IntSet.empty))
    atomically $ S.readTMVar entered
    atomically $ S.putTMVar failWrite ()
    atomically retry
  case result :: Either SomeException () of
    Left _ -> pure ()
    Right () -> assertFailure "writer failure disappeared"
  atomically (Receipts.receiptClosing tasks) >>= (@?= True)
 where
  (_, _, txs) = fixture

testHeldBypassesGate :: IO ()
testHeldBypassesGate = bounded $ withLocal txs $ \p eb _ writer -> do
  let other = p{pointSlotNo = 6}
  _ <- await =<< writeEbBody writer other eb []
  receipt <- await =<< writeTxs writer other [(i, leiosTxBytes tx) | (i, tx) <- zip [0 ..] txs]
  (source, entered, release) <- blockedSource txs
  local <- newLocalReuse (pure False) source writer
  withAsync (resolve local p eb IntSet.empty) $ \active -> do
    atomically (S.readTMVar entered)
    resolve local other eb (txWriteConfirmed receipt) >>= (@?= IntSet.fromList [0, 1])
    atomically (S.putTMVar release ())
    void (wait active)
 where
  (_, _, txs) = fixture

testCachedBody :: IO ()
testCachedBody = bounded $ do
  (ctx, notes) <- newContext
  let (point, eb, txs) = fixture
  _ <-
    Cache.insertBody
      (lfcTxCache ctx)
      (pointEbHash point)
      (Cache.packEndorserBlock eb)
      ()
      (\w _ _ _ _ -> w)
  local <-
    newLocalReuse (Receipts.receiptClosing (lfcReceiptTasks ctx)) (sourceOf txs) (lfcWriter ctx)
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ do
    acquireCachedBody ctx{lfcLocalReuse = local} point maxBound
    void $ awaitNotes notes 2

testPeerRemoval :: IO ()
testPeerRemoval = bounded $ do
  (ctx, notes) <- newContext
  entered <- S.newEmptyTMVarIO
  release <- S.newEmptyTMVarIO
  let (point, eb, txs) = fixture
      writer =
        (lfcWriter ctx)
          { writeTxs = \p payloads -> do
              pending <- writeTxs (lfcWriter ctx) p payloads
              pure $ Promise $ do
                atomically $ S.putTMVar entered ()
                atomically $ S.readTMVar release
                await pending
          }
  local <- newLocalReuse (Receipts.receiptClosing (lfcReceiptTasks ctx)) (sourceOf txs) writer
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ do
    processLeiosBlock
      ctx{lfcLocalReuse = local}
      0
      (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
      eb
    atomically $ S.readTMVar entered
    modifyMVar_ (lfcOutstanding ctx) (pure . removePeerFromOutstanding 0)
    atomically $ S.putTMVar release ()
    void $ awaitNotes notes 2

testRetireBetweenSlices :: IO ()
testRetireBetweenSlices = bounded $ withLocal txs $ \point eb _ writer -> do
  entered <- S.newEmptyTMVarIO
  release <- S.newEmptyTMVarIO
  wanted <- S.newTVarIO True
  calls <- S.newTVarIO (0 :: Int)
  let gated =
        writer
          { writeTxs = \p payloads -> do
              pending <- writeTxs writer p payloads
              atomically $ S.modifyTVar calls (+ 1)
              pure $ Promise $ do
                atomically $ S.putTMVar entered ()
                atomically $ S.readTMVar release
                await pending
          }
  local <- newLocalReuse (pure False) (sourceOf txs) gated
  withAsync
    ( reuseMempool
        local
        (S.readTVarIO wanted)
        point
        IntSet.empty
        (zip [0 ..] (endorserBlockReferences eb))
    )
    $ \task -> do
      atomically $ S.readTMVar entered
      atomically $ S.writeTVar wanted False >> S.putTMVar release ()
      wait task >>= (@?= IntSet.fromList [0, 1])
  S.readTVarIO calls >>= (@?= 1)
 where
  txs = [MkLeiosTx (BS.replicate 19000 i) | i <- [0 .. 2]]
