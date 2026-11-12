{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Test.Consensus.Leios.LeiosFetch.Wire (tests) where

import qualified Codec.CBOR.Read
import Control.Monad (void, when)
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence as Seq
import qualified Data.Vector.Strict as V
import qualified Network.TypedProtocol.Codec
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.ReceiptTasks as Receipts
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState as L
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Runtime as R
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Server
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
import Ouroboros.Consensus.Network.NodeToNode.LeiosFetch (runLeiosFetchClient)
import Ouroboros.Consensus.Storage.LeiosDB.API (LeiosDbWriter (..), Promise (..))
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Channel
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Driver.Limits (runAnnotatedPeerWithLimits)
import Test.Consensus.Leios.LeiosFetch.Replies (awaitNotes, fixture, newContext)
import Test.Tasty
import Test.Tasty.HUnit

bounded :: IO () -> IO ()
bounded action = SI.timeout 10 action >>= maybe (assertFailure "ordinary wire test did not finish") pure

tests :: TestTree
tests =
  testGroup
    "ordinary observed wire"
    [ testCase "shutdown refunds an already-dequeued unsent command" testClosing
    , testCase
        "early body reply precedes send return; individual jobs pipeline through unchanged codec/server"
        testPipeline
    , testCase "one accepted body owner suppresses competing delivery through disconnect" testPendingBody
    ]

codec :: MonadST m => NetworkCodec m
codec =
  codecLeiosFetch
    20000
    encodeLeiosPoint
    decodeLeiosPoint
    encodeEndorserBlock
    decodeEndorserBlock
    encodeLeiosTx
    decodeLeiosTx

-- Keep the inferred codec's concrete failure type at its use sites.
type NetworkCodec m =
  Network.TypedProtocol.Codec.AnnotatedCodec
    (LeiosFetch LeiosPoint EndorserBlock LeiosTx)
    Codec.CBOR.Read.DeserialiseFailure
    m
    BL.ByteString

open :: LeiosFetchClientContext IO Int -> Int -> IO (R.Client IO Int)
open ctx peer = do
  queue <- uncheckedNewTVarM Seq.empty
  atomically (R.openClient (lfcRuntime ctx) peer False queue (pure Nothing))

request :: R.Client IO Int -> LeiosFetchRequest -> IO (L.AttemptKey Int)
request client req = do
  now <- getMonotonicTime
  atomically (R.enqueueRequest client now req)
    >>= maybe (assertFailure "request rejected" >> fail "unreachable") pure

testPipeline :: Assertion
testPipeline = bounded $ do
  (base, notes) <- newContext
  let cfg = (lfcStaticEnv base){maxJobBytesSize = 6}
  runtime <- R.newRuntime cfg (lfcReady base) (Receipts.receiptClosing (lfcReceiptTasks base))
  let ctx = base{lfcStaticEnv = cfg, lfcRuntime = runtime}
      (point, eb, txs) = fixture
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ do
    client <- open ctx 0
    control <- uncheckedNewTVarM Continue
    firstSend <- newEmptyTMVarIO
    sentCount <- uncheckedNewTVarM (0 :: Int)
    requested <- uncheckedNewTVarM []
    (local, remote) <- createConnectedBufferedChannelsUnbounded
    let channel =
          local
            { send = \bytes -> do
                send local bytes
                n <- atomically $ do old <- readTVar sentCount; writeTVar sentCount (old + 1); pure old
                when (n == 0) (atomically (readTMVar firstSend))
            }
        server = leiosFetchServerPeer $ MkLeiosFetchRequestHandler $ \case
          MsgLeiosBlockRequest _ -> pure (MsgLeiosBlock eb)
          MsgLeiosBlockTxsRequest _ bitmaps -> do
            let offsets = bitmapToOffsets bitmaps
            atomically (modifyTVar requested (offsets :))
            pure (MsgLeiosBlockTxs (V.fromList [txs !! off | off <- offsets]))
        peer =
          toLeiosFetchClientPeerPipelined
            (leiosFetchClientPeerPipelined 150 (nextLeiosFetchClientCommand ctx (readTVar control) client))
    void
      (request client (LeiosBlockRequest (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)))
    withAsync
      ( runAnnotatedPeerWithLimits
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
            void (awaitNotes notes 2)
            atomically $ do
              state <- readTVar (R.runtimeFacts runtime)
              check (Map.null (R.attempts state) && Map.null (R.pending state))
            batches <- readTVarIO requested
            length batches @?= 2
            IntSet.fromList (concat batches) @?= IntSet.fromList [0, 1]
            atomically (writeTVar control Terminate)
            void (wait downstream)
            void (wait upstream)

testPendingBody :: Assertion
testPendingBody = bounded $ do
  (ctx, notes) <- newContext
  let rt = lfcRuntime ctx
      (point, eb, _) = fixture
      req = MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound
      descriptor = LeiosBlockRequest req
  a <- open ctx 0
  b <- open ctx 1
  keyA <- request a descriptor
  atomically $ R.commitRequest a keyA >>= check >> R.observeSend a (Time 0)
  atomically (modifyTVar (R.runtimeFacts rt) (R.expireHeads (Time 10) (const True)))
  keyB <- request b descriptor
  gate <- newEmptyTMVarIO
  accepted <- newEmptyTMVarIO
  let writer = lfcWriter ctx
      gated =
        ctx
          { lfcWriter =
              writer
                { writeEbBody = \p body fills -> do
                    promise <- writeEbBody writer p body fills
                    atomically (putTMVar accepted ())
                    pure promise{await = atomically (readTMVar gate) >> await promise}
                }
          }
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ do
    processLeiosBlock (requestReplyOwnership rt keyA) gated 0 req eb
    atomically (readTMVar accepted)
    atomically $ R.closePeer rt 0 >> R.closePeer rt 0
    -- B already physically owes a response, but cannot become another local
    -- writer while A's accepted receipt owns this body.
    processLeiosBlock (requestReplyOwnership rt keyB) ctx 1 req eb
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
  client <- open ctx 0
  let (point, eb, _) = fixture
  key <-
    request client (LeiosBlockRequest (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound))
  command <- nextLeiosFetchClientCommand ctx (pure Continue) client
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) (pure ())
  case command of
    Right (Right (MkSomeLeiosFetchJob actions _ _)) -> beforeDispatch actions >>= (@?= False)
    _ -> assertFailure "expected dequeued command"
  state <- atomically (readTVar (R.runtimeFacts (lfcRuntime ctx)))
  Map.member key (L.leases (R.ledger state)) @?= False
  Map.null (R.attempts state) @?= True
