{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests of how the node's LeiosFetch client takes in replies, against an
-- in-memory LeiosDB.
module Test.Consensus.Leios.LeiosFetch.Replies (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (BlockIssuer))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Class.MonadSTM.Strict
  ( StrictTVar
  , atomically
  , check
  , modifyTVar
  , newEmptyTMVarIO
  , newTVarIO
  , putTMVar
  , readTMVar
  , readTVar
  , readTVarIO
  )
import Control.Exception (SomeException, try)
import Control.Tracer (mkTracer, nullTracer)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Short as SBS
import Data.Foldable (toList)
import qualified Data.IntMap.NonEmpty as NEIntMap
import Data.List (isInfixOf, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Vector.Strict as V
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  , SystemTime (..)
  )
import Ouroboros.Consensus.Leios.Acquisitions (LeiosEbNotification (..))
import Ouroboros.Consensus.Leios.Types
  ( ElectionId (..)
  , EndorserBlock
  , LeiosEbByteSizeBounds (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , Serialised (..)
  , encodeEndorserBlockSize
  , endorserBlockFromTxBytes
  , endorserBlockHash
  , hashLeiosTx
  , hashSerialisedEndorserBlock
  , mkEndorserBlock
  , serialiseEndorserBlock
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
  ( ExnLeiosFetchInvalidReply (..)
  , JobMismatch (..)
  , LeiosFetchClientContext (..)
  , processLeiosBlock
  , processLeiosBlockTxs
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( EbFetchState (..)
  , EbState (..)
  , LeiosBlockRequest (..)
  , LeiosBlockTxsRequest (..)
  , LeiosFetchRequest (..)
  , LeiosOutstanding (..)
  , OfferedHalves (..)
  , PeerOffer (..)
  , defaultLeiosFetchStaticEnv
  , emptyLeiosOutstanding
  , focusElectionIfUnfocused
  , leiosFetchLogicIteration
  , pruneOutstandingToImmTip
  , recordReferencedPoint
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs (nullJobPool)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
  ( FetchArrivalBytes
  , TraceLeiosFetchClientEvent (..)
  , fetchArrivalEvicted
  , fetchArrivalExtra
  , fetchArrivalGood
  )
import Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbHandle (..)
  , newLeiosDBInMemory
  )
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbWriter (..)
  , Promise (..)
  )
import Ouroboros.Consensus.Util.IOLike (modifyMVar, modifyMVar_, readMVar, takeMVar)
import Ouroboros.Consensus.Util.NormalForm.StrictMVar (uncheckedNewEmptyMVar, uncheckedNewMVar)
import System.Random (mkStdGen)
import qualified System.Timeout as Timeout
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "replies"
    [ testCase "the client emits a body's acquisition, then its closure's, once each is durable" $
        test_acquisitions
    , testCase "the client disconnects a peer whose body has another hash" $
        test_wrongHash
    , testCase
        "the client acquires a body only at its own point, even if the endorser block was announced at a newer point while it was being written"
        test_announcedAgainWhileWriting
    , testCase
        "the client acquires nothing for a body whose point was pruned while it was being written, and fetches the endorser block again at a newer point"
        test_prunedWhileWriting
    , testCase
        "the client acquires no closure whose transactions are not the sizes its endorser block declares, fetches no more of it, and disconnects the peer that served them"
        test_wrongSizedTxs
    , testCase
        "the client takes a body of the right hash but not the offered size, emits its acquisition, and then disconnects the peer"
        test_wrongOfferedSize
    , testCase
        "the client disconnects a peer whose body is bigger than its point allows, without writing it, even if the peer also misreported its size"
        test_bodyTooBig
    , testCase
        "the client disconnects a peer whose body references more transaction bytes than its point allows, without writing it"
        test_closureTooBig
    , testCase
        "the client disconnects a peer whose body has the requested hash but does not decode, without writing it"
        test_undecodable
    , testCase "the client disconnects a peer that sends fewer transactions than requested" $
        test_wrongTxCount
    , testCase
        "the client disconnects a peer whose transactions for a job do not add up to that job's size"
        test_jobBytesSizeMismatch
    , testCase
        "the client disconnects a peer whose transactions for a job do not hash to that job's commitment"
        test_jobCommitmentMismatch
    , testCase
        "of two deliveries of a body whose writes overlap, the client counts the bytes as needed only for the one that settles first"
        test_overlappingDeliveries
    , testCase "the client counts a body that arrives once its point holds it as redundant" $
        test_deliveryAfterSettle
    , testCase "the client counts transactions for a point pruned meanwhile as evicted" $
        test_txsForPrunedPoint
    ]

-- | What a notification says, comparably.
data Acquired = AcquiredBody LeiosPoint | AcquiredClosure LeiosPoint
  deriving (Eq, Show)

-- | An endorser block of two transactions, announced in slot 5.
fixture :: (LeiosPoint, EndorserBlock, [LeiosTx])
fixture = (MkLeiosPoint (SlotNo 5) (endorserBlockHash eb), eb, txs)
 where
  tx0 = MkLeiosTx (BS8.pack "tx zero")
  tx1 = MkLeiosTx (BS8.pack "tx one")
  txs = [tx0, tx1]
  eb = endorserBlockFromTxBytes (leiosTxBytes tx0 :| [leiosTxBytes tx1])

peer :: Int
peer = 0

otherPeer :: Int
otherPeer = 1

-- | Whom every election in these tests elects.
electee :: KeyHash BlockIssuer
electee = KeyHash (UnsafeHash (SBS.pack (replicate 28 0)))

-- | A client context over an in-memory LeiosDB and fresh state that is
-- fetching the 'fixture', with its notifications collected, newest first.
newContext :: IO (LeiosFetchClientContext IO Int, StrictTVar IO [Acquired])
newContext = newContextHolding (pure ())

-- | 'newContext', whose body writes are not done until this returns.
newContextHolding :: IO () -> IO (LeiosFetchClientContext IO Int, StrictTVar IO [Acquired])
newContextHolding = let (point, _eb, _txs) = fixture in newContextAt point

-- | 'newContextHolding', fetching the endorser block at this point instead.
newContextAt ::
  LeiosPoint -> IO () -> IO (LeiosFetchClientContext IO Int, StrictTVar IO [Acquired])
newContextAt point release = newContextGated point (pure release)

-- | 'newContextAt', each of whose body writes is not done until what this
-- gives at that write returns.
newContextGated ::
  LeiosPoint -> IO (IO ()) -> IO (LeiosFetchClientContext IO Int, StrictTVar IO [Acquired])
newContextGated point gate = do
  let MkLeiosPoint slot ebHash = point
  db <- newLeiosDBInMemory
  writer <- holdingBodies <$> openWriter db
  outstanding <-
    uncheckedNewMVar $
      focusElectionIfUnfocused (ElectionId slot electee) ebHash $
        recordReferencedPoint point SNothing $
          emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0)
  ready <- uncheckedNewEmptyMVar
  notes <- newTVarIO []
  let note = \case
        AcquiredEb p _bodySize -> AcquiredBody p
        AcquiredEbTxs p -> AcquiredClosure p
  pure
    ( MkLeiosFetchClientContext
        { lfcStaticEnv = defaultLeiosFetchStaticEnv maxBound
        , lfcOutstanding = outstanding
        , lfcReady = ready
        , lfcWriter = writer
        , lfcNotify = modifyTVar notes . (:) . note
        , lfcSystemTime = SystemTime (pure (RelativeTime 0)) (pure ())
        , lfcTracer = nullTracer
        }
    , notes
    )
 where
  holdingBodies writer =
    writer
      { writeEbBody = \p eb fills -> do
          release <- gate
          written <- writeEbBody writer p eb fills
          pure $ Promise $ release >> await written
      }

-- | Wait until this many notifications have arrived, and return them, oldest
-- first.
awaitNotes :: StrictTVar IO [Acquired] -> Int -> IO [Acquired]
awaitNotes notes n =
  Timeout.timeout
    5000000
    (atomically $ readTVar notes >>= \ns -> reverse ns <$ check (length ns >= n))
    >>= maybe (assertFailure "timed out waiting for notifications") pure

test_acquisitions :: Assertion
test_acquisitions = do
  let (point, eb, txs) = fixture
  (ctx, notes) <- newContext
  processLeiosBlock
    ctx
    peer
    (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
    (serialiseEndorserBlock eb)
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])
  closureRequest ctx point >>= \req -> processLeiosBlockTxs ctx peer req (V.fromList txs)
  awaitNotes notes 2 >>= (@?= [AcquiredBody point, AcquiredClosure point])

-- | The one request the decision logic makes of a peer that offers the closure
-- of the endorser block at this point, whose body the client holds.
closureRequest :: LeiosFetchClientContext IO Int -> LeiosPoint -> IO LeiosBlockTxsRequest
closureRequest ctx point = do
  reqs <-
    modifyMVar (lfcOutstanding ctx) $ \outstanding -> do
      let (outstanding', decisions, _drops) =
            leiosFetchLogicIteration
              (lfcStaticEnv ctx)
              (\_slot -> Right maxBound)
              (Just (SlotNo 6, 10))
              (Map.singleton peer (Map.singleton point (MkPeerOffer SNothing OfferedClosure)))
              Map.empty
              outstanding
      pure (outstanding', [r | LeiosBlockTxsRequest r <- foldMap toList decisions])
  case reqs of
    [req] -> pure req
    _ -> assertFailure $ "expected one request, got " <> show (length reqs)

-- | An endorser block that misstates the sizes of its first two transactions,
-- by amounts that cancel out, so that their job's count, total size and hashes
-- all check out. Its third transaction is too big to share their job. The
-- LeiosDB drops the first two, so the client acquires no closure, fetches no
-- more of it, not even the third's job, and its settlement throws, through its
-- link to the client's thread.
test_wrongSizedTxs :: Assertion
test_wrongSizedTxs = do
  let tx0 = MkLeiosTx (BS8.pack "tx zero")
      tx1 = MkLeiosTx (BS8.pack "tx one")
      tx2 = MkLeiosTx (BS8.replicate (64 * 1024) 'x')
      eb = mkEndorserBlock [(hashLeiosTx tx0, 6), (hashLeiosTx tx1, 7), (hashLeiosTx tx2, 64 * 1024)]
      point = MkLeiosPoint (SlotNo 5) (endorserBlockHash eb)
      expected = ExnLeiosBlockTxsWrongSize point [0, 1]
  (ctx, notes) <- newContextAt point (pure ())
  processLeiosBlock
    ctx
    peer
    (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
    (serialiseEndorserBlock eb)
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])
  MkLeiosBlockTxsRequest _point jobs <- closureRequest ctx point
  NEIntMap.size jobs @?= 2
  result <-
    try $ do
      processLeiosBlockTxs
        ctx
        peer
        (MkLeiosBlockTxsRequest point (uncurry NEIntMap.singleton (NEIntMap.findMin jobs)))
        (V.fromList [tx0, tx1])
      -- Until the settlement's exception interrupts this.
      threadDelay 2000000
  case result of
    Left (exn :: SomeException)
      | show expected `isInfixOf` show exn -> pure ()
      | otherwise -> assertFailure $ "unexpected exception: " <> show exn
    Right () -> assertFailure "the client settled the transactions without disconnecting the peer"
  readTVarIO notes >>= (@?= [AcquiredBody point])
  readMVar (lfcOutstanding ctx) >>= assertNoJobLeft point

test_wrongHash :: Assertion
test_wrongHash = do
  let (point, _eb, _txs) = fixture
      other = endorserBlockFromTxBytes (BS8.pack "another tx" :| [])
  (ctx, notes) <- newContext
  result <-
    try $
      processLeiosBlock
        ctx
        peer
        (MkLeiosBlockRequest point (encodeEndorserBlockSize other) maxBound)
        (serialiseEndorserBlock other)
  case result of
    Left (ExnLeiosBlockWrongHash p h) -> (p, h) @?= (point, endorserBlockHash other)
    Left exn -> assertFailure $ "unexpected exception: " <> show exn
    Right () -> assertFailure "the client accepted a body with another hash"
  atomically (readTVar notes) >>= (@?= [])

-- | An election focuses the 'fixture' at a newer point while its body is being
-- written.
test_announcedAgainWhileWriting :: Assertion
test_announcedAgainWhileWriting = do
  let (point, eb, _txs) = fixture
      ebHash = pointEbHash point
      newer = MkLeiosPoint (SlotNo 7) ebHash
  written <- newEmptyTMVarIO
  (ctx, notes) <- newContextHolding (atomically (readTMVar written))
  processLeiosBlock
    ctx
    peer
    (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
    (serialiseEndorserBlock eb)
  modifyMVar_ (lfcOutstanding ctx) $
    pure
      . focusElectionIfUnfocused (ElectionId (pointSlotNo newer) electee) ebHash
      . recordReferencedPoint newer SNothing
  atomically $ putTMVar written ()
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])
  outstanding <- readMVar (lfcOutstanding ctx)
  assertNoBody newer outstanding

-- | While the 'fixture''s body is being written, pruning passes its point, and
-- an election focuses it anew at a newer point.
test_prunedWhileWriting :: Assertion
test_prunedWhileWriting = do
  let (point, eb, _txs) = fixture
      ebHash = pointEbHash point
      newer = MkLeiosPoint (SlotNo 8) ebHash
  written <- newEmptyTMVarIO
  (ctx, notes) <- newContextHolding (atomically (readTMVar written))
  processLeiosBlock
    ctx
    peer
    (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
    (serialiseEndorserBlock eb)
  -- What 'processLeiosBlock' itself signals, so that what is left to await is
  -- the body's settlement.
  takeMVar (lfcReady ctx)
  modifyMVar_ (lfcOutstanding ctx) $ pure . pruneOutstandingToImmTip (SlotNo 6)
  modifyMVar_ (lfcOutstanding ctx) $
    pure . recordReferencedPoint newer SNothing
  atomically $ putTMVar written ()
  Timeout.timeout 5000000 (takeMVar (lfcReady ctx))
    >>= maybe (assertFailure "timed out waiting for the body to settle") pure
  readTVarIO notes >>= (@?= [])
  outstanding <- readMVar (lfcOutstanding ctx)
  assertNoBody newer outstanding

assertNoBody :: LeiosPoint -> LeiosOutstanding peer -> Assertion
assertNoBody point outstanding = case Map.lookup point (ebState outstanding) of
  Just (MkEbState _onset NoBody) -> pure ()
  Just (MkEbState _onset BodyAcquired{}) -> assertFailure "the point holds a body"
  Nothing -> assertFailure "the point has no entry"

assertNoJobLeft :: LeiosPoint -> LeiosOutstanding peer -> Assertion
assertNoJobLeft point outstanding = case Map.lookup point (ebState outstanding) of
  Just (MkEbState _onset (BodyAcquired jobPool)) ->
    assertBool "the point has jobs left" (nullJobPool jobPool)
  Just (MkEbState _onset NoBody) -> assertFailure "the point holds no body"
  Nothing -> assertFailure "the point has no entry"

test_wrongOfferedSize :: Assertion
test_wrongOfferedSize = do
  let (point, eb, _txs) = fixture
      size = encodeEndorserBlockSize eb
  (ctx, notes) <- newContext
  result <-
    try $
      processLeiosBlock
        ctx
        peer
        (MkLeiosBlockRequest point (size + 1) maxBound)
        (serialiseEndorserBlock eb)
  case result of
    Left (ExnLeiosBlockWrongSize p offered actual) -> (p, offered, actual) @?= (point, size + 1, size)
    Left exn -> assertFailure $ "unexpected exception: " <> show exn
    Right () -> assertFailure "the client kept a peer that misreported the body's size"
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])

test_bodyTooBig :: Assertion
test_bodyTooBig = do
  let (point, eb, _txs) = fixture
      size = encodeEndorserBlockSize eb
      bounds = MkLeiosEbByteSizeBounds{lebMaxBodySize = size - 1, lebMaxTxsSize = maxBound}
  (ctx, notes) <- newContext
  result <-
    try $
      processLeiosBlock ctx peer (MkLeiosBlockRequest point (size + 1) bounds) (serialiseEndorserBlock eb)
  case result of
    Left (ExnLeiosBlockTooBig p actual most) -> (p, actual, most) @?= (point, size, size - 1)
    Left exn -> assertFailure $ "unexpected exception: " <> show exn
    Right () -> assertFailure "the client kept a peer that sent a body too big"
  readTVarIO notes >>= (@?= [])
  readMVar (lfcOutstanding ctx) >>= assertNoJobLeft point

test_closureTooBig :: Assertion
test_closureTooBig = do
  let (point, eb, txs) = fixture
      closure = sum [fromIntegral (BS8.length (leiosTxBytes tx)) | tx <- txs]
      bounds = MkLeiosEbByteSizeBounds{lebMaxBodySize = maxBound, lebMaxTxsSize = closure - 1}
  (ctx, notes) <- newContext
  result <-
    try $
      processLeiosBlock
        ctx
        peer
        (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) bounds)
        (serialiseEndorserBlock eb)
  case result of
    Left (ExnLeiosClosureTooBig p actual most) ->
      (p, actual, most) @?= (point, fromIntegral closure, closure - 1)
    Left exn -> assertFailure $ "unexpected exception: " <> show exn
    Right () -> assertFailure "the client kept a peer that sent a body referencing too many bytes"
  readTVarIO notes >>= (@?= [])
  readMVar (lfcOutstanding ctx) >>= assertNoJobLeft point

test_undecodable :: Assertion
test_undecodable = do
  let bytes = Serialised (BSL8.pack "not an endorser block")
      point = MkLeiosPoint (SlotNo 5) (hashSerialisedEndorserBlock bytes)
  (ctx, notes) <- newContextAt point (pure ())
  result <-
    try $
      processLeiosBlock
        ctx
        peer
        (MkLeiosBlockRequest point (fromIntegral (BSL8.length (unSerialised bytes))) maxBound)
        bytes
  case result of
    Left (ExnLeiosBlockUndecodable p _err) -> p @?= point
    Left exn -> assertFailure $ "unexpected exception: " <> show exn
    Right () -> assertFailure "the client kept a peer that sent a body that does not decode"
  readTVarIO notes >>= (@?= [])
  readMVar (lfcOutstanding ctx) >>= assertNoJobLeft point

test_wrongTxCount :: Assertion
test_wrongTxCount = do
  let (point, _eb, txs) = fixture
  replyToClosure (take 1 txs) >>= \case
    Left (ExnLeiosBlockTxsWrongCount p requested arrived) -> (p, requested, arrived) @?= (point, 2, 1)
    Left exn -> assertFailure $ "unexpected exception: " <> show exn
    Right () -> assertFailure "the client took too few transactions"

test_jobBytesSizeMismatch :: Assertion
test_jobBytesSizeMismatch = do
  let (point, _eb, txs) = fixture
  replyToClosure (take 1 txs <> [MkLeiosTx (BS8.pack "tx one!")]) >>= \case
    Left (ExnLeiosBlockTxsJobMismatch p _job JobBytesSizeMismatch) -> p @?= point
    Left exn -> assertFailure $ "unexpected exception: " <> show exn
    Right () -> assertFailure "the client took transactions of the wrong size"

test_jobCommitmentMismatch :: Assertion
test_jobCommitmentMismatch = do
  let (point, _eb, txs) = fixture
  replyToClosure (take 1 txs <> [MkLeiosTx (BS8.pack "tx onf")]) >>= \case
    Left (ExnLeiosBlockTxsJobMismatch p _job JobCommitmentMismatch) -> p @?= point
    Left exn -> assertFailure $ "unexpected exception: " <> show exn
    Right () -> assertFailure "the client took transactions other than the endorser block's"

-- | What the client makes of these transactions as the reply to its request
-- for the 'fixture''s closure.
replyToClosure :: [LeiosTx] -> IO (Either ExnLeiosFetchInvalidReply ())
replyToClosure txs = do
  let (point, eb, _txs) = fixture
  (ctx, notes) <- newContext
  processLeiosBlock
    ctx
    peer
    (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
    (serialiseEndorserBlock eb)
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])
  req <- closureRequest ctx point
  try $ processLeiosBlockTxs ctx peer req (V.fromList txs)

-- | Two peers deliver the 'fixture''s body before either write settles.
test_overlappingDeliveries :: Assertion
test_overlappingDeliveries = do
  let (point, eb, _txs) = fixture
      size = encodeEndorserBlockSize eb
      req = MkLeiosBlockRequest point size maxBound
  written <- newEmptyTMVarIO
  (ctx, notes, traces) <- withTraces $ newContextHolding (atomically (readTMVar written))
  processLeiosBlock ctx peer req (serialiseEndorserBlock eb)
  processLeiosBlock ctx otherPeer req (serialiseEndorserBlock eb)
  atomically $ putTMVar written ()
  arrivals <- awaitBlockArrivals traces 2
  [p | (p, _point, _bytes) <- arrivals] @?= [peer, otherPeer]
  let byPeer = [bytes | (_peer, _point, bytes) <- arrivals]
      needed = fetchArrivalGood size
      redundant = fetchArrivalExtra size
  assertBool
    ("the bytes were not needed once and redundant once: " <> show byPeer)
    (byPeer == [needed, redundant] || byPeer == [redundant, needed])
  readTVarIO notes >>= (@?= [AcquiredBody point])

test_deliveryAfterSettle :: Assertion
test_deliveryAfterSettle = do
  let (point, eb, _txs) = fixture
      size = encodeEndorserBlockSize eb
      req = MkLeiosBlockRequest point size maxBound
  (ctx, notes, traces) <- withTraces newContext
  processLeiosBlock ctx peer req (serialiseEndorserBlock eb)
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])
  processLeiosBlock ctx otherPeer req (serialiseEndorserBlock eb)
  awaitBlockArrivals traces 2
    >>= (@?= [(peer, point, fetchArrivalGood size), (otherPeer, point, fetchArrivalExtra size)])

test_txsForPrunedPoint :: Assertion
test_txsForPrunedPoint = do
  let (point, eb, txs) = fixture
      closure = sum [fromIntegral (BS8.length (leiosTxBytes tx)) | tx <- txs]
  (ctx, notes, traces) <- withTraces newContext
  processLeiosBlock
    ctx
    peer
    (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
    (serialiseEndorserBlock eb)
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])
  req <- closureRequest ctx point
  modifyMVar_ (lfcOutstanding ctx) $ pure . pruneOutstandingToImmTip (SlotNo 6)
  processLeiosBlockTxs ctx peer req (V.fromList txs)
  events <- readTVarIO traces
  [(p, pt, bytes) | TraceLeiosFetchClientTxsArrived p pt bytes <- events]
    @?= [(peer, point, fetchArrivalEvicted closure)]

-- | The context, its notifications, and its traces, newest first.
withTraces ::
  IO (LeiosFetchClientContext IO Int, StrictTVar IO [Acquired]) ->
  IO
    ( LeiosFetchClientContext IO Int
    , StrictTVar IO [Acquired]
    , StrictTVar IO [TraceLeiosFetchClientEvent Int]
    )
withTraces mk = do
  (ctx, notes) <- mk
  traces <- newTVarIO []
  pure (ctx{lfcTracer = mkTracer $ atomically . modifyTVar traces . (:)}, notes, traces)

-- | Wait until this many endorser block arrivals are traced, and return them,
-- ordered by peer.
awaitBlockArrivals ::
  StrictTVar IO [TraceLeiosFetchClientEvent Int] ->
  Int ->
  IO [(Int, LeiosPoint, FetchArrivalBytes)]
awaitBlockArrivals traces n =
  Timeout.timeout
    5000000
    ( atomically $ do
        events <- readTVar traces
        let arrivals = [(p, pt, bytes) | TraceLeiosFetchClientBlockArrived p pt bytes <- events]
        sortOn (\(p, _pt, _bytes) -> p) arrivals <$ check (length arrivals >= n)
    )
    >>= maybe (assertFailure "timed out waiting for arrivals") pure
