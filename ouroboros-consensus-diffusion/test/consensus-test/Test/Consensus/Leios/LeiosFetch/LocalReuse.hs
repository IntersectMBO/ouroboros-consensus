{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bounded local discovery and node-owned receipt lifecycle regressions.
module Test.Consensus.Leios.LeiosFetch.LocalReuse (tests) where

import qualified Control.Concurrent.Class.MonadSTM.Strict as S
import Control.Monad (forM_, void)
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.Monad.IOSim (runSimOrThrow)
import Control.Tracer (mkTracer)
import qualified Data.ByteString as BS
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
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
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Runtime as R
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
import Ouroboros.Consensus.Storage.LeiosDB
import Ouroboros.Consensus.Util.IOLike
import Test.Consensus.Leios.LeiosFetch.Replies
  ( awaitNotes
  , fixture
  , newContext
  , newContextAt
  , newContextHolding
  , processLeiosBlock
  , processLeiosBlockTxs
  , submitFixtureForge
  )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.Orphans.IOLike ()

bounded :: IO () -> IO ()
bounded action = SI.timeout 5 action >>= maybe (assertFailure "bounded local/receipt test did not finish") pure

sourceOf :: [LeiosTx] -> LocalSource IO LeiosTx
sourceOf txs =
  LocalSource
    { localLookup = \requested -> pure (Map.fromList [(h, tx) | (h, _) <- requested, Just tx <- [Map.lookup h byHash]], Nothing)
    , localBytes = pure . Just
    }
 where
  byHash = Map.fromList [(hashLeiosTx tx, tx) | tx <- txs]

tests :: TestTree
tests =
  testGroup
    "whole-EB preparation and receipts"
    [ testCase "node-owned source wait closes before receipt supervision" testSourceClose
    , testCase "receipt waits retain capacity and retirement wakes a third peer" testSlotBound
    , testCase "body admission progress comparison fixture" testBodyAdmissionComparison
    , testCase "whole EB preparation comparison fixture" testPreparationComparison
    , testCase "body settlement omits locally held transactions" testBodyIntegration
    , testCase "cached body uses the same whole-EB pass" testCachedBody
    , testCase "normal drain joins late accepted callbacks" testReceiptDrain
    , testCase "failure during normal drain is supervised" testFailureDuringDrain
    , testCase "abort quiesces pre-acceptance producers" testProducerAbort
    , testCase "abort cancels and joins accepted callbacks" testReceiptAbort
    , testCase "accepted TX survives later tracing failure" testTraceFailure
    , testCase "interrupted forge caller retains combined receipt owner" testForgeBodyOwner
    ]

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
        transferred <- Receipts.withReceiptTransfer tasks (pure ()) $
          Receipts.forkReceipt tasks $ do
            atomically $ readTMVar durable
            atomically $ writeTVar callback True
        transferred @?= Just ()
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

testTraceFailure :: IO ()
testTraceFailure = bounded $ do
  (ctx, notes) <- newContext
  withBodyWorkers ctx $ do
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
      (MkLeiosEbByteSizes maxBound maxBound)

testForgeBodyOwner :: IO ()
testForgeBodyOwner = bounded $ do
  release <- S.newEmptyTMVarIO
  entered <- S.newEmptyTMVarIO
  (ctx, _) <- newContextHolding (atomically (S.readTMVar release))
  withBodyWorkers ctx $ do
    let (point, _, txs) = fixture
        forged = forgeLeiosEb EvenProtVer (case map leiosTxBytes txs of x : xs -> x :| xs; [] -> error "fixture")
    withAsync
      ( Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ do
          withAsync
            ( do
                submitFixtureForge ctx point forged
                atomically (S.putTMVar entered ())
                atomically retry
            )
            $ \producer -> do
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

testBodyIntegration :: IO ()
testBodyIntegration = bounded $ forM_ [[], take 1 txs, txs] $ \localTxs -> do
  (ctx, notes) <- newContext
  withBodyWorkers ctx $ do
    let local = newLocalReuse (sourceOf localTxs)
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
      (MkLeiosEbByteSizes maxBound maxBound)

testCachedBody :: IO ()
testCachedBody = bounded $ do
  (ctx, notes) <- newContext
  withBodyWorkers ctx $ do
    let (point, eb, txs) = fixture
    _ <-
      Cache.insertBody
        (lfcTxCache ctx)
        point
        (Cache.packEndorserBlock eb)
        ()
        (\w _ _ _ _ -> w)
    let local = newLocalReuse (sourceOf txs)
    Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ do
      atomically $ queueCachedBody ctx{lfcLocalReuse = local} point maxBound
      void $ awaitNotes notes 2

testPreparationComparison :: IO ()
testPreparationComparison = bounded $ forM_ [0, 100, 200] $ \overlap -> do
  let txs = [MkLeiosTx (BS.replicate 3000 i) | i <- [0 .. 199]]
      eb = endorserBlockFromTxBytes (case map leiosTxBytes txs of x : xs -> x :| xs; [] -> error "fixture")
      point = MkLeiosPoint 5 (endorserBlockHash eb)
  (base, notes) <- newContextAt point (pure ())
  withBodyWorkers base $ do
    counts <- S.newTVarIO (0 :: Int, 0 :: Int, 0 :: Int)
    let original = lfcWriter base
        writer =
          original
            { writeEbBody = \p body fills given -> do
                atomically $ S.modifyTVar counts (\(b, t, l) -> (b + 1, t, l))
                writeEbBody original p body fills given
            , writeTxs = \p given -> do
                atomically $ S.modifyTVar counts (\(b, t, l) -> (b, t + 1, l))
                writeTxs original p given
            }
        source =
          (sourceOf (take overlap txs))
            { localLookup = \requested -> do
                S.modifyTVar counts (\(b, t, l) -> (b, t, l + 1))
                localLookup (sourceOf (take overlap txs)) requested
            }
    let local = newLocalReuse source
    let ctx = base{lfcWriter = writer, lfcLocalReuse = local}
    Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ do
      processLeiosBlock ctx 0 (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb
      void $ awaitNotes notes (if overlap == 200 then 2 else 1)
    result <- S.readTVarIO counts
    putStrLn ("PREPARATION overlap=" ++ show overlap ++ " body,tx,lookup=" ++ show result)
    result @?= (1, 0, 1)

-- Hold writer admission (rather than receipt completion) and observe whether
-- the peer handler can return. Both versions must eventually acquire the body.

testBodyAdmissionComparison :: IO ()
testBodyAdmissionComparison = do
  let finished = runSimOrThrow $ do
        (base, notes) <- newContext
        withBodyWorkers base $ do
          entered <- S.newEmptyTMVarIO
          release <- S.newEmptyTMVarIO
          let original = lfcWriter base
              writer =
                original
                  { writeEbBody = \p body fills given -> do
                      atomically (S.putTMVar entered ())
                      atomically (S.readTMVar release)
                      writeEbBody original p body fills given
                  }
              ctx = base{lfcWriter = writer}
              (point, eb, _) = fixture
          Receipts.withReceiptTasks (lfcReceiptTasks ctx)
            $ bracket_ (pure ()) (atomically $ void (S.tryPutTMVar release ()))
            $ withAsync
              (processLeiosBlock ctx 0 (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb)
            $ \handler -> do
              atomically (S.readTMVar entered)
              outcome <- SI.timeout 0.1 (wait handler)
              atomically (S.putTMVar release ())
              wait handler
              atomically $ S.readTVar notes >>= check . (>= 1) . length
              pure (outcome == Just ())
  putStrLn ("PREPARATION handler_waits_for_writer=" ++ show (not finished))
  assertBool "node-owned body handler returns while writer admission is held" finished

-- A node-owned body does not borrow its peer handler for a mempool wait, and
-- normal close can skip that optional source before the outer receipts close.
testSourceClose :: IO ()
testSourceClose = bounded $ do
  (base, notes) <- newContext
  closing <- S.newTVarIO False
  let rt = (lfcRuntime base){R.runtimeClosing = S.readTVar closing}
      ctx =
        base
          { lfcRuntime = rt
          , lfcLocalReuse = newLocalReuse (LocalSource (const retry) (\() -> pure Nothing))
          }
      (point, eb, _) = fixture
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $
    withBodyWorkers ctx $
      ( do
          result <-
            SI.timeout
              0.1
              (processLeiosBlock ctx 0 (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb)
          result @?= Just ()
          atomically (Map.size . R.pending <$> readTVar (R.runtimeFacts rt)) >>= (@?= 1)
          atomically $ R.closePeer rt 0
          atomically (Map.size . R.pending <$> readTVar (R.runtimeFacts rt)) >>= (@?= 1)
      )
        `finally` atomically (S.writeTVar closing True)
  void $ awaitNotes notes 1
  atomically (Map.null . R.pending <$> readTVar (R.runtimeFacts rt)) >>= (@?= True)

-- Both accepted-but-unsettled writes still occupy the two-entry pool. A third
-- received body waits outside the logical lock and can retire without waiting
-- for either unrelated writer receipt to complete.
testSlotBound :: IO ()
testSlotBound = bounded $ do
  (base, notes) <- newContext
  release <- S.newEmptyTMVarIO
  submitted <- S.newTVarIO (0 :: Int)
  retired <- S.newTVarIO False
  let (first, eb, _) = fixture
      second = first{pointSlotNo = 6}
      third = first{pointSlotNo = 7}
      original = lfcWriter base
      ctx =
        base
          { lfcBodyRetired = \point -> if point == third then S.readTVar retired else pure False
          , lfcWriter =
              original
                { writeEbBody = \point body fills given -> do
                    promise <- writeEbBody original point body fills given
                    atomically $ S.modifyTVar submitted (+ 1)
                    pure promise{await = atomically (S.readTMVar release) >> await promise}
                }
          }
      receive point = processLeiosBlock ctx 0 (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb
  modifyMVar_ (lfcOutstanding ctx) $ \out ->
    pure
      out
        { ebState =
            foldr (\point -> Map.insert point (MkEbState SNothing NoBody)) (ebState out) [second, third]
        }
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ withBodyWorkers ctx $ do
    receive first
    receive second
    atomically $ S.readTVar submitted >>= check . (== 2)
    withAsync (receive third) $ \waiting -> do
      SI.timeout 0.1 (wait waiting) >>= (@?= Nothing)
      SI.timeout
        0.1
        (modifyMVar_ (lfcOutstanding ctx) (pure . \out -> out{ebState = Map.delete third (ebState out)}))
        >>= (@?= Just ())
      atomically $ S.writeTVar retired True
      wait waiting
      S.readTVarIO submitted >>= (@?= 2)
    atomically $ S.putTMVar release ()
    void $ awaitNotes notes 2
