{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.Leios.LeiosFetch.Wire (tests) where

import Cardano.Slotting.Slot (SlotNo (..))
import qualified Codec.CBOR.Read
import qualified Codec.CBOR.Write as CBOR
import qualified Control.Concurrent.Class.MonadSTM.Strict as S
import Control.Exception (AsyncException (ThreadKilled))
import Control.Monad (unless, void, when)
import Control.Monad.Class.MonadTest (exploreRaces)
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.Monad.IOSim (IOSim, exploreSimTrace, runSimOrThrow, traceResult)
import Control.Tracer (mkTracer, nullTracer)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft, isRight)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector.Strict as V
import qualified Network.TypedProtocol.Codec
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.ReceiptTasks as Receipts
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState as L
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Runtime as R
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Server
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Timing as T
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Wait as W
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
  ( FetchRoutine (..)
  , TraceLeiosFetchClientEvent (..)
  , fetchArrivalEvicted
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
import Ouroboros.Consensus.Network.NodeToNode.LeiosFetch (runLeiosFetchClient)
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbWriter (..)
  , Promise (..)
  , TxWriteReceipt (..)
  )
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Channel
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Driver.Limits (ProtocolLimitFailure (..), runPeerWithLimits)
import Test.Consensus.Leios.LeiosFetch.Replies
  ( assignRequests
  , awaitNotes
  , expireRuntimeHeads
  , fixture
  , newContext
  , tryEnqueueRequest
  )
import qualified Test.Consensus.Leios.LeiosFetch.Replies as Fixtures
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (Property, counterexample, property, testProperty)
import Test.Util.Orphans.IOLike ()

bounded :: IO () -> IO ()
bounded action = SI.timeout 10 action >>= maybe (assertFailure "ordinary wire test did not finish") pure

tests :: TestTree
tests =
  testGroup
    "ordinary observed wire"
    [ testCase "logical discard still wakes after physical credit already disappeared" $
        runSimOrThrow logicalDiscardReadiness @?= True
    , testCase "body framing and inner payload limits are independent" testBodyFraming
    , testCase "already received input cannot earn a stall while our decoder is paused" testDecodePause
    , testCase
        "needing more input after decoding retains the original absolute deadline"
        testDecodeNeedsInput
    , testProperty "accepted ownership survives removal and stale settlement under POR" propReceiptRemoval
    , testCase
        "silent source hits the original 60-second guard and releases its registration"
        testSilentGuard
    , testCase
        "a winner makes a dequeued command obsolete during a full pipeline wait"
        testObsoleteAtDepth
    , testCase "shutdown refunds an already-dequeued unsent command" testClosing
    , testCase
        "early body reply precedes send return; individual jobs pipeline through unchanged codec/server"
        testPipeline
    , testCase "one accepted body owner suppresses competing delivery through disconnect" testPendingBody
    , testCase
        "body-pool closure discards a reply and drains without a peer failure"
        testBodyPoolClosure
    , testGroup
        "discarded TX submission"
        [ testCase label (runSimOrThrow (discardedTx mode) @?= True)
        | (label, mode) <-
            [ ("completed work bypasses blocked writer admission", TxCompleted)
            , ("pruned work bypasses blocked writer admission", TxPruned)
            , ("forge-owned work bypasses blocked writer admission", TxForged)
            , ("completed work still checks reply count", TxBadCount)
            , ("completed work still checks job size", TxBadSize)
            ]
        ]
    , testCase "discarded copy can retry after its unsubmitted winner is cancelled" $
        runSimOrThrow retryDiscardedCopy @?= True
    , testCase "an early receipt and late trace failure preserve the next logical assignment" $
        runSimOrThrow earlyReceiptTraceFailure @?= True
    , testCase "accepted transfer failure closes admission before exact-owner cleanup" $
        runSimOrThrow (failedReceiptTransfer InstallFault) @?= True
    , testCase "peer cancellation before receipt installation is supervised" $
        runSimOrThrow (failedReceiptTransfer PeerCancelled) @?= True
    , testProperty
        "aborting a pre-install receipt transfer retains the scope failure under POR"
        propAbortReceiptTransfer
    ]

codec :: MonadST m => NetworkCodec m
codec =
  codecLeiosFetch
    20000
    encodeLeiosPoint
    decodeLeiosPoint
    encodeSerialisedEndorserBlock
    decodeSerialisedEndorserBlock
    encodeLeiosTx
    decodeLeiosTx

-- Keep the inferred codec's concrete failure type at its use sites.
type NetworkCodec m =
  Network.TypedProtocol.Codec.Codec
    (LeiosFetch LeiosPoint (Serialised EndorserBlock) LeiosTx)
    Codec.CBOR.Read.DeserialiseFailure
    m
    BL.ByteString

open :: IOLike m => LeiosFetchClientContext m Int -> Int -> m (R.Client m Int)
open ctx peer = do
  queue <- uncheckedNewTVarM Seq.empty
  atomically (R.openClient (lfcRuntime ctx) peer False queue (pure Nothing))

request :: R.Client IO Int -> LeiosFetchRequest -> IO (L.AttemptKey Int)
request client req = do
  now <- getMonotonicTime
  atomically (tryEnqueueRequest client now req)
    >>= maybe (assertFailure "request rejected" >> fail "unreachable") pure

type TxCopy m = (R.Client m Int, L.AttemptKey Int, LeiosBlockTxsRequest)

ensure :: MonadFail m => String -> Bool -> m ()
ensure message ok = unless ok (fail message)

-- Use actual planner picks and runtime leases; the second copy is admitted only
-- after the first source's head expires. No logical multiplicities are invented.
withTxCopies ::
  (LeiosFetchClientContext (IOSim s) Int -> TxCopy (IOSim s) -> TxCopy (IOSim s) -> IOSim s a) ->
  IOSim s a
withTxCopies action = do
  (ctx, notes) <- newContext
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ withBodyWorkers ctx $ do
    let (point, eb, _) = fixture
        bodyRequest = MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound
    Fixtures.processLeiosBlock ctx 0 bodyRequest eb
    atomically $ S.readTVar notes >>= check . (not . null)
    a <- open ctx 0
    b <- open ctx 1
    ( do
        first <- enqueueTx ctx a
        dispatchTx first
        threadDelay 10
        now <- getMonotonicTime
        atomically $
          modifyTVar (R.runtimeFacts (lfcRuntime ctx)) (expireRuntimeHeads now (const True))
        second <- enqueueTx ctx b
        dispatchTx second
        action ctx first second
      )
      `finally` atomically (R.closeClient a >> R.closeClient b)

enqueueTx :: IOLike m => LeiosFetchClientContext m Int -> R.Client m Int -> m (TxCopy m)
enqueueTx ctx client = do
  now <- getMonotonicTime
  let (point, _, _) = fixture
      offer =
        Map.singleton (R.clientPeer client) $
          Map.singleton point (MkPeerOffer SNothing OfferedClosure)
  mask_ $ modifyMVar_ (lfcOutstanding ctx) $
    assignRequests (lfcRuntime ctx) now (const (Right maxBound)) Nothing Map.empty offer
  next <- atomically $ R.takeRequest client
  case next of
    Just (key, LeiosBlockTxsRequest req) -> pure (client, key, req)
    _ -> throwIO (userError "planner did not assign the expected TX job")

dispatchTx :: IOLike m => TxCopy m -> m ()
dispatchTx (client, key, _) = do
  now <- getMonotonicTime
  atomically $ do
    R.commitRequest client key >>= check
    R.observeSend client now
    R.installReceiver client key

deliverTx ::
  IOLike m => LeiosFetchClientContext m Int -> TxCopy m -> V.Vector LeiosTx -> m ()
deliverTx ctx (client, key, req) txs = do
  now <- getMonotonicTime
  atomically $ R.completeWire client key now
  processLeiosBlockTxs
    (requestReplyOwnership ctx key (LeiosBlockTxsRequest req)) ctx (R.clientPeer client) req txs
    `finally` do
      at <- getMonotonicTime
      atomically (R.finishHandler client key (R.workOf (LeiosBlockTxsRequest req)) at)

data TxDiscardCase = TxCompleted | TxPruned | TxForged | TxBadCount | TxBadSize
  deriving Eq

discardedTx :: TxDiscardCase -> IOSim s Bool
discardedTx mode = withTxCopies $ \ctx first second@(_, key, _) -> do
  let (point, _, txs) = fixture
      rt = lfcRuntime ctx
  case mode of
    TxPruned -> modifyMVar_ (lfcOutstanding ctx) (pure . pruneOutstandingToImmTip (SlotNo 6))
    TxForged -> modifyMVar_ (lfcOutstanding ctx) (pure . markReceivingFromForge point)
    _ -> do
      deliverTx ctx first (V.fromList txs)
      atomically $ readTVar (R.runtimeFacts rt) >>= check . Map.null . R.pending
  calls <- uncheckedNewTVarM (0 :: Int)
  let blocked =
        ctx
          { lfcWriter = (lfcWriter ctx)
              { writeTxs = \_ _ -> do
                  atomically $ modifyTVar calls (+ 1)
                  atomically retry
              }
          }
      payload = case mode of
        TxBadCount -> V.empty
        TxBadSize -> V.fromList (MkLeiosTx (BS.snoc (leiosTxBytes (head txs)) 0) : tail txs)
        _ -> V.fromList txs
  result <- try $ deliverTx blocked second payload
  correct <- case (mode, result) of
    (TxBadCount, Left ExnLeiosBlockTxsWrongCount{}) -> pure True
    (TxBadSize, Left ExnLeiosBlockTxsJobMismatch{}) -> pure True
    (_, Right ()) -> pure (mode /= TxBadCount && mode /= TxBadSize)
    _ -> pure False
  count <- readTVarIO calls
  out <- readMVar (lfcOutstanding ctx)
  facts <- readTVarIO (R.runtimeFacts rt)
  pure
    ( correct && count == 0 && Map.notMember 1 (requestedJobsPerPeer out)
        && Map.notMember key (L.leases (R.ledger facts))
    )

logicalDiscardReadiness :: forall s. IOSim s Bool
logicalDiscardReadiness = withTxCopies $ \ctx (a, key, req) _ -> do
  let takeReady = atomically $
        orElse (W.awaitReadiness (lfcReady ctx) >> pure True) (pure False)
  atomically (R.closeClient a)
  _ <- takeReady
  before <- readMVar (lfcOutstanding ctx)
  -- Bypass the outer protocol handler's finalizer to isolate the real
  -- submission-scope unpick signal. No lease or pending owner can wake it.
  outcome <-
    try
      (processLeiosBlockTxs
        (requestReplyOwnership ctx key (LeiosBlockTxsRequest req)) ctx 0 req V.empty)
      :: IOSim s (Either SomeException ())
  dirty <- takeReady
  following <- readMVar (lfcOutstanding ctx)
  pure $
    isLeft outcome && dirty
      && Map.member 0 (requestedJobsPerPeer before)
      && Map.notMember 0 (requestedJobsPerPeer following)
      && Map.member 1 (requestedJobsPerPeer following)

retryDiscardedCopy :: IOSim s Bool
retryDiscardedCopy = withTxCopies $ \ctx first@(_, winnerKey, req) second@(b, loserKey, _) -> do
  entered <- newEmptyTMVarIO
  calls <- uncheckedNewTVarM (0 :: Int)
  let writer = lfcWriter ctx
      gated =
        ctx
          { lfcWriter = writer
              { writeTxs = \_ _ -> do
                  atomically $ putTMVar entered ()
                  atomically retry
              }
          }
      blocked =
        ctx
          { lfcWriter = writer
              { writeTxs = \_ _ -> do
                  atomically $ modifyTVar calls (+ 1)
                  atomically retry
              }
          }
      (point, _, txs) = fixture
      rt = lfcRuntime ctx
  withAsync (deliverTx gated first (V.fromList txs)) $ \winner -> do
    atomically $ readTMVar entered
    deliverTx blocked second (V.fromList txs)
    out <- readMVar (lfcOutstanding ctx)
    facts <- readTVarIO (R.runtimeFacts rt)
    ensure "loser erased the winning claim" $
      Map.lookup (R.workOf (LeiosBlockTxsRequest req)) (R.pending facts)
        == Just (R.NetworkOwner winnerKey, False)
    ensure "loser retained its assignment or physical credit" $
      Map.notMember 1 (requestedJobsPerPeer out)
        && Map.notMember loserKey (L.leases (R.ledger facts))
    cancel winner
  fresh <- enqueueTx ctx b
  dispatchTx fresh
  deliverTx ctx fresh (V.fromList txs)
  atomically $ readTVar (R.runtimeFacts rt) >>= check . Map.null . R.pending
  out <- readMVar (lfcOutstanding ctx)
  count <- readTVarIO calls
  pure $ count == 0 && case Map.lookup point (ebState out) of
    Just (MkEbState _ (BodyAcquired pool)) -> Jobs.nullJobPool pool
    _ -> False

-- The first receipt intentionally confirms no offsets, exercising the documented
-- retry path. Its accepted pending owner is gone before a new planner pick, while
-- the old handler is still blocked in an application tracer.
earlyReceiptTraceFailure :: IOSim s Bool
earlyReceiptTraceFailure = withTxCopies $ \ctx first@(a, _, _) second -> do
  receiptRelease <- newEmptyTMVarIO
  traceEntered <- newEmptyTMVarIO
  traceRelease <- newEmptyTMVarIO
  let writer = lfcWriter ctx
      partial = writer
        { writeTxs = \p payload -> do
            accepted <- writeTxs writer p payload
            pure $ Promise $ do
              atomically $ readTMVar receiptRelease
              void $ await accepted
              pure (TxWriteReceipt Set.empty Set.empty True)
        }
      failing =
        ctx
          { lfcWriter = partial
          , lfcTracer = mkTracer $ \case
              TraceLeiosFetchClientTxsArrived{} -> do
                atomically $ putTMVar traceEntered ()
                atomically $ readTMVar traceRelease
                throwIO (userError "late TX trace failure")
              _ -> pure ()
          }
      (point, _, txs) = fixture
      rt = lfcRuntime ctx
  withAsync (try (deliverTx failing first (V.fromList txs))) $ \old -> do
    atomically $ readTMVar traceEntered
    deliverTx ctx second (V.fromList txs)
    atomically $ putTMVar receiptRelease ()
    atomically $ readTVar (R.runtimeFacts rt) >>= check . Map.null . R.pending
    fresh <- enqueueTx ctx a
    atomically $ putTMVar traceRelease ()
    outcome <- wait old
    ensure "post-handoff trace failure vanished" (isLeft (outcome :: Either SomeException ()))
    out <- readMVar (lfcOutstanding ctx)
    ensure "old cleanup erased the next logical pick" $
      Map.member 0 (requestedJobsPerPeer out) && case Map.lookup point (ebState out) of
        Just (MkEbState _ (BodyAcquired pool)) ->
          Jobs.jobMultiplicities pool == IntMap.singleton 0 1
        _ -> False
    dispatchTx fresh
    deliverTx ctx fresh (V.fromList txs)
  atomically $ readTVar (R.runtimeFacts rt) >>= check . Map.null . R.pending
  pure True

data TransferFault = TransferFault
  deriving (Eq, Show)

instance Exception TransferFault

data TransferFailure = InstallFault | PeerCancelled | ScopeAborted

-- Submit a real write and exercise the shared pre-install failure boundary.
-- This fails the installation action, not allocation inside 'forkReceipt'. The
-- other cases interrupt that action before installation. All must close
-- admission before dropping the accepted owner, without inventing a receipt.
failedReceiptTransfer :: TransferFailure -> IOSim s Bool
failedReceiptTransfer failure = do
  cleaned <- uncheckedNewTVarM False
  start <- newEmptyTMVarIO
  entered <- newEmptyTMVarIO
  -- Keep the peer's enclosing async scope outside the receipt scope, so its
  -- automatic cancellation cannot pre-empt the node's own abort ordering.
  withAsync (atomically (takeTMVar start) >>= id) $ \producer -> do
    outcome <- try $ withTxCopies $ \ctx (a, key, req) (b, _, _) -> do
      let rt = lfcRuntime ctx
          tasks = lfcReceiptTasks ctx
          ownership = requestReplyOwnership ctx key (LeiosBlockTxsRequest req)
          (point, _, txs) = fixture
      -- Remove the other physical copy, so admission would succeed if closing
      -- and accepted-owner cleanup were wrongly separated.
      atomically $ R.closeClient b
      candidate <- open ctx 2
      let transfer = Receipts.withReceiptSubmission tasks $ do
            now <- getMonotonicTime
            claimed <- atomically $ do
              R.completeWire a key now
              claimUsefulReply ownership
            ensure "useful transfer never claimed its job" claimed
            _ <- writeTxs (lfcWriter ctx) point (Map.fromList (zip [0 ..] (map leiosTxBytes txs)))
            let abandon = do
                  closing <- Receipts.receiptClosing tasks
                  before <- readTVar (R.runtimeFacts rt)
                  abandonAccepted ownership
                  replacement <- tryEnqueueRequest candidate now (LeiosBlockTxsRequest req)
                  facts <- readTVar (R.runtimeFacts rt)
                  writeTVar cleaned $
                    closing && replacement == Nothing && Map.null (R.pending facts)
                      && Map.lookup (R.workOf (LeiosBlockTxsRequest req)) (R.pending before)
                        == Just (R.NetworkOwner key, True)
            transferred <- Receipts.withReceiptTransfer tasks abandon $ do
              void $ atomically (acceptedReply ownership now)
              atomically $ putTMVar entered ()
              case failure of
                InstallFault -> throwIO TransferFault
                _ -> atomically retry
            ensure "failed transfer acquired a receipt owner" (transferred == (Nothing :: Maybe ()))
      ( do
          atomically $ putTMVar start transfer
          atomically $ readTMVar entered
          case failure of
            InstallFault -> atomically retry
            PeerCancelled -> throwTo (asyncThreadId producer) ThreadKilled >> atomically retry
            ScopeAborted -> throwIO TransferFault
        )
        `finally` atomically (R.closeClient candidate)
    original <- case outcome of
      Left err -> pure $ case failure of
        PeerCancelled -> fromException err == Just ThreadKilled
        _ -> fromException err == Just TransferFault
      Right () -> pure False
    joined <- isRight <$> waitCatch producer
    released <- readTVarIO cleaned
    pure (original && joined && released)

propAbortReceiptTransfer :: Property
propAbortReceiptTransfer =
  exploreSimTrace id (exploreRaces >> failedReceiptTransfer ScopeAborted) $ \_ tr ->
    case traceResult False tr of
      Left err -> counterexample (show err) False
      Right ok -> property ok

-- A late complete reply can reach local admission after body workers close.
-- Exercise the actual client/codec/server and its physical-request finalizers.
testBodyPoolClosure :: Assertion
testBodyPoolClosure = bounded $ do
  (base, notes) <- newContext
  withBodyWorkers base (pure ())
  control <- uncheckedNewTVarM Continue
  closed <- newEmptyTMVarIO
  let (point, eb, _) = fixture
      ctx =
        base
          { lfcTracer = mkTracer $ \case
              TraceLeiosFetchClientBlockArrived _ arrived bytes -> do
                arrived @?= point
                bytes @?= fetchArrivalEvicted (encodeEndorserBlockSize eb)
                atomically $ writeTVar control Terminate
              TraceLeiosFetchClientPolicy _ _ (FetchInvocationClosed _ failed) ->
                atomically $ putTMVar closed failed
              _ -> pure ()
          }
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ do
    client <- open ctx 0
    void $ request client $
      LeiosBlockRequest (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
    (local, remote) <- createConnectedBufferedChannelsUnbounded
    let server :: LeiosFetchServerPeer LeiosPoint (Serialised EndorserBlock) LeiosTx IO ()
        server = leiosFetchServerPeer $ MkLeiosFetchRequestHandler $ \case
          MsgLeiosBlockRequest _ -> pure (MsgLeiosBlock (serialiseEndorserBlock eb))
          MsgLeiosBlockTxsRequest{} -> fail "closed body pool requested closure TXs"
        peer =
          toLeiosFetchClientPeerPipelined $
            leiosFetchClientPeerPipelined
              150
              (awaitTerminate (readTVar control)) (nextLeiosFetchJob ctx client)
    void $
      concurrently
        ( runPeerWithLimits nullTracer codec (byteLimitsLeiosFetch (fromIntegral . BL.length))
            timeLimitsLeiosFetch remote server
        )
        ( runLeiosFetchClient nullTracer codec (byteLimitsLeiosFetch (fromIntegral . BL.length))
            ctx client peer local
        )
    atomically (readTMVar closed) >>= (@?= False)
    facts <- readTVarIO (R.runtimeFacts (lfcRuntime ctx))
    assertBool "closed reply retained its original credit or local owner" $
      Map.null (L.leases (R.ledger facts)) && Map.null (R.pending facts)
        && Map.notMember 0 (R.sessions facts)
    S.readTVarIO notes >>= (@?= [])

testPipeline :: Assertion
testPipeline = bounded $ do
  (base, notes) <- newContext
  withBodyWorkers base $ do
    let cfg = (lfcStaticEnv base){maxJobBytesSize = 6}
    runtime <- R.newRuntime cfg (lfcReady base) (Receipts.receiptClosing (lfcReceiptTasks base))
    let ctx = base{lfcStaticEnv = cfg, lfcRuntime = runtime}
        (point, eb, txs) = fixture
    Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ do
      client <- open ctx 0
      control <- uncheckedNewTVarM Continue
      firstSend <- newEmptyTMVarIO
      firstTx <- newEmptyTMVarIO :: IO (StrictTMVar IO ())
      releaseTx <- newEmptyTMVarIO
      secondTxSent <- newEmptyTMVarIO
      sentCount <- uncheckedNewTVarM (0 :: Int)
      requested <- uncheckedNewTVarM []
      (local, remote) <- createConnectedBufferedChannelsUnbounded
      let channel =
            local
              { send = \bytes -> do
                  send local bytes
                  n <- atomically $ do old <- readTVar sentCount; writeTVar sentCount (old + 1); pure old
                  when (n == 0) (atomically (readTMVar firstSend))
                  when (n == 2) (atomically (putTMVar secondTxSent ()))
              }
          server = leiosFetchServerPeer $ MkLeiosFetchRequestHandler $ \case
            MsgLeiosBlockRequest _ -> pure (MsgLeiosBlock (serialiseEndorserBlock eb))
            MsgLeiosBlockTxsRequest _ bitmaps -> do
              let offsets = bitmapToOffsets bitmaps
              first <- atomically $ do
                old <- readTVar requested
                modifyTVar requested (offsets :)
                pure (null old)
              when first $ do
                atomically (putTMVar firstTx ())
                atomically (readTMVar releaseTx)
              pure (MsgLeiosBlockTxs (V.fromList [txs !! off | off <- offsets]))
          peer =
            toLeiosFetchClientPeerPipelined
              (leiosFetchClientPeerPipelined 150 (awaitTerminate (readTVar control)) (nextLeiosFetchJob ctx client))
      void
        (request client (LeiosBlockRequest (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)))
      bracket_
        (pure ())
        (atomically $ void (tryPutTMVar firstSend ()) >> void (tryPutTMVar releaseTx ()))
        $ withAsync
          ( runPeerWithLimits
              nullTracer
              codec
              (byteLimitsLeiosFetch (fromIntegral . BL.length))
              timeLimitsLeiosFetch
              remote
              server
          )
        $ \upstream ->
          withAsync
            ( runLeiosFetchClient
                nullTracer
                codec
                (byteLimitsLeiosFetch (fromIntegral . BL.length))
                ctx
                client
                peer
                channel
            )
            $ \downstream -> do
              void (awaitNotes notes 1)
              before <- atomically (readTVar (R.runtimeFacts runtime))
              Map.size (R.attempts before) @?= 1
              Map.null (L.leases (R.ledger before)) @?= True
              atomically (putTMVar firstSend ())
              now <- getMonotonicTime
              mask_ $
                modifyMVar_ (lfcOutstanding ctx) $
                  assignRequests
                    runtime
                    now
                    (const (Right maxBound))
                    Nothing
                    Map.empty
                    (Map.singleton 0 (Map.singleton point (MkPeerOffer SNothing OfferedClosure)))
              -- The FIFO server cannot handle TX2 while TX1's reply is held.
              -- Observe the actual client send, not eventual server batches.
              overlap <- SI.timeout 1 (atomically (readTMVar firstTx >> readTMVar secondTxSent))
              atomically $ void (tryPutTMVar releaseTx ())
              overlap @?= Just ()
              void (awaitNotes notes 2)
              atomically $ do
                state <- readTVar (R.runtimeFacts runtime)
                check (Map.null (R.attempts state) && Map.null (R.pending state))
              batches <- readTVarIO requested
              length batches @?= 2
              reverse batches @?= [[0], [1]]
              IntSet.fromList (concat batches) @?= IntSet.fromList [0, 1]
              atomically (writeTVar control Terminate)
              void (wait downstream)
              void (wait upstream)

testPendingBody :: Assertion
testPendingBody = bounded $ do
  (ctx, notes) <- newContext
  withBodyWorkers ctx $ do
    let rt = lfcRuntime ctx
        (point, eb, _) = fixture
        req = MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound
        descriptor = LeiosBlockRequest req
    a <- open ctx 0
    b <- open ctx 1
    keyA <- request a descriptor
    atomically $ R.commitRequest a keyA >>= check >> R.observeSend a (Time 0)
    atomically (modifyTVar (R.runtimeFacts rt) (expireRuntimeHeads (Time 10) (const True)))
    keyB <- request b descriptor
    gate <- newEmptyTMVarIO
    accepted <- newEmptyTMVarIO
    let writer = lfcWriter ctx
        gated =
          ctx
            { lfcWriter =
                writer
                  { writeEbBody = \p body fills txs -> do
                      promise <- writeEbBody writer p body fills txs
                      atomically (putTMVar accepted ())
                      pure promise{await = atomically (readTMVar gate) >> await promise}
                  }
            }
    Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ do
      processLeiosBlock
        (requestReplyOwnership gated keyA descriptor)
        gated
        0
        req
        (serialiseEndorserBlock eb)
      atomically (readTMVar accepted)
      atomically $ R.closePeer rt 0 >> R.closePeer rt 0
      -- B already physically owes a response, but cannot become another local
      -- writer while A's accepted receipt owns this body.
      processLeiosBlock (requestReplyOwnership ctx keyB descriptor) ctx 1 req (serialiseEndorserBlock eb)
      atomically (Map.size . R.pending <$> readTVar (R.runtimeFacts rt)) >>= (@?= 1)
      atomically (putTMVar gate ())
      void (awaitNotes notes 1)
      atomically $ do
        state <- readTVar (R.runtimeFacts rt)
        check (Map.null (R.pending state))
      atomically (R.closeClient b)

-- Closing is observed again at dispatch, after dequeue and pipeline-slot waits.
testClosing :: Assertion
testClosing = bounded $ do
  (ctx, _) <- newContext
  withBodyWorkers ctx $ do
    client <- open ctx 0
    let (point, eb, _) = fixture
    key <-
      request client (LeiosBlockRequest (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound))
    command <- atomically (nextLeiosFetchJob ctx client)
    Receipts.withReceiptTasks (lfcReceiptTasks ctx) (pure ())
    case command of
      MkSomeLeiosFetchJob actions _ _ -> beforeDispatch actions >>= (@?= False)
    state <- atomically (readTVar (R.runtimeFacts (lfcRuntime ctx)))
    Map.member key (L.leases (R.ledger state)) @?= False
    Map.null (R.attempts state) @?= True

-- Exercise the actual ordinary client, annotated codec and observed driver in
-- virtual time. Soft recovery does not replace the independent hard guard.
testSilentGuard :: Assertion
testSilentGuard = runSimOrThrow silent @?= (True, Time 60, True)
 where
  silent :: forall s. IOSim s (Bool, Time, Bool)
  silent = do
    (ctx, _) <- newContext
    withBodyWorkers ctx $ do
      client <- open ctx 0
      let (point, eb, _) = fixture
      key <-
        atomically
          ( tryEnqueueRequest
              client
              (Time 0)
              (LeiosBlockRequest (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound))
          )
      case key of { Nothing -> error "body credit"; Just _ -> pure () }
      let channel = Channel{send = const (pure ()), recv = atomically retry}
          peer =
            toLeiosFetchClientPeerPipelined
              (leiosFetchClientPeerPipelined 150 (awaitTerminate (pure Continue)) (nextLeiosFetchJob ctx client))
      outcome <-
        try
          ( runLeiosFetchClient
              nullTracer
              codec
              (byteLimitsLeiosFetch (fromIntegral . BL.length))
              ctx
              client
              peer
              channel
          )
      now <- getMonotonicTime
      state <- atomically (readTVar (R.runtimeFacts (lfcRuntime ctx)))
      let timedOut = case outcome of Left (ExceededTimeLimit _) -> True; _ -> False
      pure
        ( timedOut
        , now
        , Map.null (R.sessions state) && Map.null (L.leases (R.ledger state)) && Map.null (R.attempts state)
        )

testObsoleteAtDepth :: Assertion
testObsoleteAtDepth = bounded $ do
  (ctx, notes) <- newContext
  withBodyWorkers ctx $ do
    let (point, eb, _) = fixture
        other = point{pointSlotNo = SlotNo 6}
        describe p = LeiosBlockRequest (MkLeiosBlockRequest p (encodeEndorserBlockSize eb) maxBound)
    modifyMVar_ (lfcOutstanding ctx) $ \out ->
      pure $
        (recordReferencedPoint other SNothing out)
          { elFocus = Map.union (Map.mapKeys (\e -> e{electionSlot = SlotNo 6}) (elFocus out)) (elFocus out)
          }
    client <- open ctx 0
    _ <- request client (describe point)
    obsolete <- request client (describe other)
    gate <- newEmptyTMVarIO
    served <- uncheckedNewTVarM []
    control <- uncheckedNewTVarM Continue
    (local, remote) <- createConnectedBufferedChannelsUnbounded
    let server = leiosFetchServerPeer $ MkLeiosFetchRequestHandler $ \case
          MsgLeiosBlockRequest p -> do
            atomically (modifyTVar served (p :))
            atomically (readTMVar gate)
            pure (MsgLeiosBlock (serialiseEndorserBlock eb))
          MsgLeiosBlockTxsRequest{} -> error "unexpected TX request"
        peer =
          toLeiosFetchClientPeerPipelined
            (leiosFetchClientPeerPipelined 1 (awaitTerminate (readTVar control)) (nextLeiosFetchJob ctx client))
    Receipts.withReceiptTasks (lfcReceiptTasks ctx)
      $ withAsync
        ( runPeerWithLimits
            nullTracer
            codec
            (byteLimitsLeiosFetch (fromIntegral . BL.length))
            timeLimitsLeiosFetch
            remote
            server
        )
      $ \upstream ->
        withAsync
          ( runLeiosFetchClient
              nullTracer
              codec
              (byteLimitsLeiosFetch (fromIntegral . BL.length))
              ctx
              client
              peer
              local
          )
          $ \downstream -> do
            -- The second immutable command has left the queue while the first fills
            -- the sole wire slot. No instrumentation in the production planner.
            atomically $ do
              state <- readTVar (R.runtimeFacts (lfcRuntime ctx))
              case Map.lookup 0 (R.sessions state) of
                Nothing -> retry
                Just session -> readTVar (R.queued session) >>= check . Seq.null
              readTVar served >>= check . (== [point])
            Fixtures.processLeiosBlock
              ctx
              1
              (MkLeiosBlockRequest other (encodeEndorserBlockSize eb) maxBound)
              eb
            _ <- awaitNotes notes 1
            atomically (putTMVar gate ())
            atomically $ do
              state <- readTVar (R.runtimeFacts (lfcRuntime ctx))
              check (Map.notMember obsolete (R.attempts state))
            atomically (writeTVar control Terminate)
            void (wait downstream)
            void (wait upstream)
            readTVarIO served >>= (@?= [point])
            atomically (Map.null . L.leases . R.ledger <$> readTVar (R.runtimeFacts (lfcRuntime ctx)))
              >>= (@?= True)

-- Explore the acceptance/removal ordering without adding production test gates.
-- An old receipt also runs after the replacement owns the same logical work.
propReceiptRemoval :: Property
propReceiptRemoval = exploreSimTrace id (exploreRaces >> scenario) $ \_ tr ->
  case traceResult False tr of
    Left err -> counterexample (show err) False
    Right ok -> property ok
 where
  scenario :: IOSim s Bool
  scenario = do
    (ctx, _) <- newContext
    withBodyWorkers ctx $ do
      original <- open ctx 0
      let rt = lfcRuntime ctx
          (point, eb, _) = fixture
          req = LeiosBlockRequest (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
      Just old <- atomically (tryEnqueueRequest original (Time 0) req)
      claimed <- atomically $ do
        R.commitRequest original old >>= check
        R.observeSend original (Time 0)
        R.installReceiver original old
        R.completeWire original old (Time 0)
        R.claimUseful rt old
      withAsync (atomically (R.acceptReply rt old (R.workOf req))) $ \accepted ->
        withAsync (atomically (R.closePeer rt 0)) $ \removed ->
          wait accepted >> wait removed
      replacement <- open ctx 0
      blocked <- atomically (tryEnqueueRequest replacement (Time 0) req)
      atomically (R.settleReceipt rt old (R.workOf req))
      Just fresh <- atomically (tryEnqueueRequest replacement (Time 0) req)
      followingClaim <- atomically (R.claimUseful rt fresh)
      atomically (R.settleReceipt rt old (R.workOf req))
      final <- atomically (readTVar (R.runtimeFacts rt))
      pure
        ( claimed
            && blocked == Nothing
            && old /= fresh
            && followingClaim
            && Map.lookup (L.Body point) (R.pending final) == Just (R.NetworkOwner fresh, False)
        )

-- Delay the real codec after its channel input has arrived, without modifying
-- production scheduling. A local decoder pause is not new peer-service time.
testDecodePause :: Assertion
testDecodePause = runSimOrThrow (decodePause True) @?= (False, False)

testDecodeNeedsInput :: Assertion
testDecodeNeedsInput = runSimOrThrow (decodePause False) @?= (False, True)

decodePause :: forall s. Bool -> IOSim s (Bool, Bool)
decodePause completeInput = do
  (ctx, _) <- newContext
  withBodyWorkers ctx $ do
    client <- open ctx 0
    let rt = lfcRuntime ctx
        (point, eb, _) = fixture
    Just key <-
      atomically
        ( tryEnqueueRequest
            client
            (Time 0)
            (LeiosBlockRequest (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound))
        )
    control <- uncheckedNewTVarM Continue
    used <- uncheckedNewTVarM False
    let baseCodec :: NetworkCodec (IOSim s)
        baseCodec = codec
        encoded = Network.TypedProtocol.Codec.encode baseCodec (MsgLeiosBlock (serialiseEndorserBlock eb))
        payload = if completeInput then encoded else BL.take 2 encoded
        channel =
          Channel
            { send = const (pure ())
            , recv = do
                sent <- atomically (readTVar used)
                if sent
                  then atomically retry
                  else do
                    SI.threadDelay 0.1
                    atomically (writeTVar used True)
                    pure (Just payload)
            }
        slowCodec :: NetworkCodec (IOSim s)
        slowCodec =
          baseCodec
            { Network.TypedProtocol.Codec.decode = \tok -> pause <$> Network.TypedProtocol.Codec.decode baseCodec tok
            }
        pause ::
          Network.TypedProtocol.Codec.DecodeStep BL.ByteString failure (IOSim s) a ->
          Network.TypedProtocol.Codec.DecodeStep BL.ByteString failure (IOSim s) a
        pause (Network.TypedProtocol.Codec.DecodePartial k) = Network.TypedProtocol.Codec.DecodePartial $ \bytes -> SI.threadDelay 2 >> k bytes
        pause step = step
        peer =
          toLeiosFetchClientPeerPipelined
            (leiosFetchClientPeerPipelined 150 (awaitTerminate (readTVar control)) (nextLeiosFetchJob ctx client))
    Receipts.withReceiptTasks (lfcReceiptTasks ctx)
      $ withAsync
        ( runLeiosFetchClient
            nullTracer
            slowCodec
            (byteLimitsLeiosFetch (fromIntegral . BL.length))
            ctx
            client
            peer
            channel
        )
      $ \downstream -> do
        SI.threadDelay 1.1
        state <- atomically (readTVar (R.runtimeFacts rt))
        let expired = expireRuntimeHeads (Time 1.1) (const True) state
            fired = maybe False (T.clockFired . R.attemptClock) (Map.lookup key (R.attempts expired))
        SI.threadDelay 1.1
        following <- atomically (readTVar (R.runtimeFacts rt))
        let next = expireRuntimeHeads (Time 2.2) (const True) following
            firedAfter = maybe False (T.clockFired . R.attemptClock) (Map.lookup key (R.attempts next))
        atomically (writeTVar control Terminate)
        if completeInput then void (wait downstream) else cancel downstream
        pure (fired, firedAfter)

-- The outer reply accepts its CBOR-in-CBOR wrapper, while the inner decoder
-- still enforces the original payload ceiling independently of that framing.
testBodyFraming :: Assertion
testBodyFraming = do
  let body n = Serialised (BL.fromStrict (BS.replicate n 0))
      maximumBody = fromIntegral maxEndorserBlockBytesSize
      raw = body maximumBody
      wrapped = Network.TypedProtocol.Codec.encode (codec :: NetworkCodec IO) (MsgLeiosBlock raw)
      inner = CBOR.toLazyByteString . encodeSerialisedEndorserBlock
  leiosBlockMessageOverhead @?= 10
  assertBool "maximum payload plus its wrapper fits the outer bound" $
    BL.length wrapped > fromIntegral maximumBody
      && BL.length wrapped <= fromIntegral (maxEndorserBlockBytesSize + leiosBlockMessageOverhead)
  assertBool "inner ceiling is accepted" $
    isRight (Codec.CBOR.Read.deserialiseFromBytes decodeSerialisedEndorserBlock (inner raw))
  assertBool "extra framing allowance does not enlarge inner ceiling" $
    isLeft
      (Codec.CBOR.Read.deserialiseFromBytes decodeSerialisedEndorserBlock (inner (body (maximumBody + 1))))
