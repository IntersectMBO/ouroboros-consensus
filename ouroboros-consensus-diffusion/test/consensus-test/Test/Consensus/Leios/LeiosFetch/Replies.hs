{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests of how the node's LeiosFetch client takes in replies, against an
-- in-memory LeiosDB.
module Test.Consensus.Leios.LeiosFetch.Replies (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (BlockIssuer))
import Cardano.Slotting.Slot (SlotNo (..))
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
import Control.Exception (try)
import Control.Monad.Class.MonadAsync (wait, withAsync)
import Control.Tracer (mkTracer, nullTracer)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Short as SBS
import Data.Foldable (toList)
import qualified Data.IntMap.NonEmpty as NEIntMap
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Vector.Strict as V
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  , SystemTime (..)
  )
import Ouroboros.Consensus.Leios.Acquisitions (LeiosAcquisition (..))
import Ouroboros.Consensus.Leios.TxCache
  ( defaultLeiosTxCacheShift
  , newPureLeiosTxCache
  )
import Ouroboros.Consensus.Leios.Types
  ( ElectionId (..)
  , EndorserBlock
  , ForgedEndorserBlock
  , LeiosEbByteSizes (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , ProtVerParity (..)
  , Serialised (..)
  , encodeEndorserBlockSize
  , endorserBlockFromTxBytes
  , endorserBlockHash
  , forgeLeiosEb
  , hashLeiosTx
  , hashSerialisedEndorserBlock
  , idLeiosEbByteSizesLimit
  , mkEndorserBlock
  , serialiseEndorserBlock
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
  ( ExnLeiosFetchInvalidReply (..)
  , JobMismatch (..)
  , LeiosFetchClientContext (..)
  , acquireForgedEb
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
        "the client disconnects a peer whose transactions are not the sizes the endorser block declares, even if they add up to their job's size, without writing them"
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
        "of two replies with a body whose writes overlap, the client counts the bytes as needed only for the one that settles first"
        test_overlappingReplies
    , testCase "the client counts a body that arrives once its point holds it as redundant" $
        test_replyAfterSettle
    , testCase
        "of two replies with a closure's transactions whose writes overlap, the client counts the bytes as needed only for the one that settles first"
        test_overlappingTxsReplies
    , testCase "the client counts transactions for a point pruned meanwhile as evicted" $
        test_txsForPrunedPoint
    , testCase
        "the client acquires the closure of an endorser block the node forged at a point whose body it already held, without acquiring the body again"
        test_forgedAfterBody
    , testCase
        "the client emits nothing more for an endorser block the node forged again at a point whose closure it already acquired"
        test_forgedAgain
    , testCase
        "the client counts transactions a peer sends after the node's own forge acquired them as redundant, and acquires nothing more"
        test_txsAfterForged
    ]

-- | What a notification says, comparably.
data Acquired = AcquiredBody LeiosPoint | AcquiredClosure LeiosPoint
  deriving (Eq, Show)

-- | An endorser block of two transactions, announced in slot 5.
fixture :: (LeiosPoint, EndorserBlock, [LeiosTx])
fixture = (MkLeiosPoint (SlotNo 5) (endorserBlockHash eb), eb, toList fixtureTxs)
 where
  eb = endorserBlockFromTxBytes (fmap leiosTxBytes fixtureTxs)

-- | The 'fixture''s transactions.
fixtureTxs :: NonEmpty LeiosTx
fixtureTxs = MkLeiosTx (BS8.pack "tx zero") :| [MkLeiosTx (BS8.pack "tx one")]

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
  txCache <- newPureLeiosTxCache defaultLeiosTxCacheShift
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
        , lfcTxCache = txCache
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
      { writeEbBody = \p eb fills txs -> do
          release <- gate
          written <- writeEbBody writer p eb fills txs
          pure $ Promise $ release >> await written
      }

-- | Writes that hold until released: what to run in a held write, waiting
-- until this many writers are held, and releasing them all.
newWriteHold :: IO (IO (), Int -> IO (), IO ())
newWriteHold = do
  written <- newEmptyTMVarIO
  held <- newTVarIO (0 :: Int)
  pure
    ( atomically (modifyTVar held (+ 1)) >> atomically (readTMVar written)
    , \n -> atomically $ readTVar held >>= check . (>= n)
    , atomically $ putTMVar written ()
    )

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
-- by amounts that cancel out, so that their job's count and total size check
-- out. Its third transaction is too big to share their job.
test_wrongSizedTxs :: Assertion
test_wrongSizedTxs = do
  let tx0 = MkLeiosTx (BS8.pack "tx zero")
      tx1 = MkLeiosTx (BS8.pack "tx one")
      tx2 = MkLeiosTx (BS8.replicate (64 * 1024) 'x')
      eb = mkEndorserBlock [(hashLeiosTx tx0, 6), (hashLeiosTx tx1, 7), (hashLeiosTx tx2, 64 * 1024)]
      point = MkLeiosPoint (SlotNo 5) (endorserBlockHash eb)
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
    try $
      processLeiosBlockTxs
        ctx
        peer
        (MkLeiosBlockTxsRequest point (uncurry NEIntMap.singleton (NEIntMap.findMin jobs)))
        (V.fromList [tx0, tx1])
  case result of
    Left (ExnLeiosBlockTxsJobMismatch p _job JobCommitmentMismatch) -> p @?= point
    Left exn -> assertFailure $ "unexpected exception: " <> show exn
    Right () -> assertFailure "the client took transactions of other sizes than the endorser block declares"
  readTVarIO notes >>= (@?= [AcquiredBody point])

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
  (hold, awaitHeld, release) <- newWriteHold
  (ctx, notes) <- newContextHolding hold
  withAsync
    ( processLeiosBlock
        ctx
        peer
        (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
        (serialiseEndorserBlock eb)
    )
    $ \client -> do
      awaitHeld 1
      modifyMVar_ (lfcOutstanding ctx) $
        pure
          . focusElectionIfUnfocused (ElectionId (pointSlotNo newer) electee) ebHash
          . recordReferencedPoint newer SNothing
      release
      wait client
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
  (hold, awaitHeld, release) <- newWriteHold
  (ctx, notes) <- newContextHolding hold
  withAsync
    ( processLeiosBlock
        ctx
        peer
        (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
        (serialiseEndorserBlock eb)
    )
    $ \client -> do
      awaitHeld 1
      -- What 'processLeiosBlock' itself signals, so that what is left to await
      -- is the body's settlement.
      takeMVar (lfcReady ctx)
      modifyMVar_ (lfcOutstanding ctx) $ pure . pruneOutstandingToImmTip (SlotNo 6)
      modifyMVar_ (lfcOutstanding ctx) $
        pure . recordReferencedPoint newer SNothing
      release
      wait client
  Timeout.timeout 5000000 (takeMVar (lfcReady ctx))
    >>= maybe (assertFailure "timed out waiting for the body to settle") pure
  readTVarIO notes >>= (@?= [])
  outstanding <- readMVar (lfcOutstanding ctx)
  assertNoBody newer outstanding

-- | A peer replies with the 'fixture''s body, and then the node's own forge
-- produces the same endorser block at the same point, while the closure is
-- still to fetch.
test_forgedAfterBody :: Assertion
test_forgedAfterBody = do
  let (point, eb, _txs) = fixture
  (ctx, notes) <- newContext
  processLeiosBlock
    ctx
    peer
    (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
    (serialiseEndorserBlock eb)
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])
  acquireForgedEb ctx (fixtureElection point) fixtureForged
  reverse <$> readTVarIO notes >>= (@?= [AcquiredBody point, AcquiredClosure point])

-- | The node's own forge produces the 'fixture''s endorser block, and once its
-- writes have settled, another of the node's forges produces the same endorser
-- block at the same point.
test_forgedAgain :: Assertion
test_forgedAgain = do
  let (point, _eb, _txs) = fixture
  (ctx, notes) <- newContext
  acquireForgedEb ctx (fixtureElection point) fixtureForged
  reverse <$> readTVarIO notes >>= (@?= [AcquiredBody point, AcquiredClosure point])
  acquireForgedEb ctx (fixtureElection point) fixtureForged
  reverse <$> readTVarIO notes >>= (@?= [AcquiredBody point, AcquiredClosure point])
  readMVar (lfcOutstanding ctx) >>= assertNoJobLeft point

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
      bounds = idLeiosEbByteSizesLimit MkLeiosEbByteSizes{lebBodySize = size - 1, lebTxsSize = maxBound}
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
      bounds = idLeiosEbByteSizesLimit MkLeiosEbByteSizes{lebBodySize = maxBound, lebTxsSize = closure - 1}
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
      (p, actual, most) @?= (point, closure, closure - 1)
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

-- | Two peers reply with the 'fixture''s body before either write settles.
test_overlappingReplies :: Assertion
test_overlappingReplies = do
  let (point, eb, _txs) = fixture
      size = encodeEndorserBlockSize eb
      req = MkLeiosBlockRequest point size maxBound
  (hold, awaitHeld, release) <- newWriteHold
  (ctx, notes, traces) <- withTraces $ newContextHolding hold
  withAsync (processLeiosBlock ctx peer req (serialiseEndorserBlock eb)) $ \client ->
    withAsync (processLeiosBlock ctx otherPeer req (serialiseEndorserBlock eb)) $ \otherClient -> do
      awaitHeld 2
      release
      wait client >> wait otherClient
  arrivals <- awaitArrivals blockArrivals traces 2
  [p | (p, _point, _bytes) <- arrivals] @?= [peer, otherPeer]
  let byPeer = [bytes | (_peer, _point, bytes) <- arrivals]
      needed = fetchArrivalGood size
      redundant = fetchArrivalExtra size
  assertBool
    ("the bytes were not needed once and redundant once: " <> show byPeer)
    (byPeer == [needed, redundant] || byPeer == [redundant, needed])
  readTVarIO notes >>= (@?= [AcquiredBody point])

test_replyAfterSettle :: Assertion
test_replyAfterSettle = do
  let (point, eb, _txs) = fixture
      size = encodeEndorserBlockSize eb
      req = MkLeiosBlockRequest point size maxBound
  (ctx, notes, traces) <- withTraces newContext
  processLeiosBlock ctx peer req (serialiseEndorserBlock eb)
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])
  processLeiosBlock ctx otherPeer req (serialiseEndorserBlock eb)
  awaitArrivals blockArrivals traces 2
    >>= (@?= [(peer, point, fetchArrivalGood size), (otherPeer, point, fetchArrivalExtra size)])

-- | Two peers reply with the 'fixture''s transactions before either write
-- settles.
test_overlappingTxsReplies :: Assertion
test_overlappingTxsReplies = do
  let (point, eb, txs) = fixture
      closure = sum [fromIntegral (BS8.length (leiosTxBytes tx)) | tx <- txs]
  (hold, awaitHeld, release) <- newWriteHold
  (ctx0, notes, traces) <- withTraces newContext
  let ctx = ctx0{lfcWriter = holdingTxs hold (lfcWriter ctx0)}
  processLeiosBlock
    ctx
    peer
    (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
    (serialiseEndorserBlock eb)
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])
  req <- closureRequest ctx point
  withAsync (processLeiosBlockTxs ctx peer req (V.fromList txs)) $ \client ->
    withAsync (processLeiosBlockTxs ctx otherPeer req (V.fromList txs)) $ \otherClient -> do
      awaitHeld 2
      release
      wait client >> wait otherClient
  arrivals <- awaitArrivals txsArrivals traces 2
  [p | (p, _point, _bytes) <- arrivals] @?= [peer, otherPeer]
  let byPeer = [bytes | (_peer, _point, bytes) <- arrivals]
      needed = fetchArrivalGood closure
      redundant = fetchArrivalExtra closure
  assertBool
    ("the bytes were not needed once and redundant once: " <> show byPeer)
    (byPeer == [needed, redundant] || byPeer == [redundant, needed])
  readTVarIO notes >>= (@?= [AcquiredClosure point, AcquiredBody point])

-- | This writer, whose transaction writes are not done until this returns.
holdingTxs :: IO () -> LeiosDbWriter IO -> LeiosDbWriter IO
holdingTxs release writer =
  writer
    { writeTxs = \p txs -> do
        written <- writeTxs writer p txs
        pure $ Promise $ release >> await written
    }

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
  txsArrivals events @?= [(peer, point, fetchArrivalEvicted closure)]

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

-- | Wait until this many of these arrivals are traced, and return them,
-- ordered by peer.
awaitArrivals ::
  ([TraceLeiosFetchClientEvent Int] -> [(Int, LeiosPoint, FetchArrivalBytes)]) ->
  StrictTVar IO [TraceLeiosFetchClientEvent Int] ->
  Int ->
  IO [(Int, LeiosPoint, FetchArrivalBytes)]
awaitArrivals select traces n =
  Timeout.timeout
    5000000
    ( atomically $ do
        arrivals <- select <$> readTVar traces
        sortOn (\(p, _pt, _bytes) -> p) arrivals <$ check (length arrivals >= n)
    )
    >>= maybe (assertFailure "timed out waiting for arrivals") pure

blockArrivals
  , txsArrivals ::
    [TraceLeiosFetchClientEvent Int] -> [(Int, LeiosPoint, FetchArrivalBytes)]
blockArrivals events = [(p, pt, bytes) | TraceLeiosFetchClientBlockArrived p pt bytes <- events]
txsArrivals events = [(p, pt, bytes) | TraceLeiosFetchClientTxsArrived p pt bytes <- events]

-- | A peer sends the 'fixture''s body, the node asks it for the closure, and
-- the node's own forge acquires the same endorser block at that point before
-- the peer's txs arrive.
test_txsAfterForged :: Assertion
test_txsAfterForged = do
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
  acquireForgedEb ctx (fixtureElection point) fixtureForged
  processLeiosBlockTxs ctx peer req (V.fromList txs)
  awaitArrivals txsArrivals traces 1 >>= (@?= [(peer, point, fetchArrivalExtra closure)])
  reverse <$> readTVarIO notes >>= (@?= [AcquiredBody point, AcquiredClosure point])

-- | The 'fixture''s endorser block, as the node's own forge produces it.
fixtureForged :: ForgedEndorserBlock
fixtureForged = forgeLeiosEb EvenProtVer (fmap leiosTxBytes fixtureTxs)

-- | The election announcing the endorser block at this point.
fixtureElection :: LeiosPoint -> ElectionId
fixtureElection point = ElectionId (pointSlotNo point) electee
