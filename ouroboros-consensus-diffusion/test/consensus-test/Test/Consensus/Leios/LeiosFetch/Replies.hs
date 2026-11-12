{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests of how the node's LeiosFetch client takes in replies, against an
-- in-memory LeiosDB.
module Test.Consensus.Leios.LeiosFetch.Replies
  ( tests
  , fixture
  , newContext
  , newContextHolding
  , awaitNotes
  , processLeiosBlock
  , processLeiosBlockTxs
  , tryEnqueueRequest
  , assignRequests
  , expireRuntimeHeads
  ) where

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
import Control.Exception (try)
import Control.Monad (forM_)
import Control.Monad.Class.MonadAsync (wait, withAsync)
import Control.Tracer (mkTracer, nullTracer)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as SBS
import Data.Foldable (toList)
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Set as Set
import qualified Data.Vector.Strict as V
import Data.Word (Word64)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  , SystemTime (..)
  )
import Ouroboros.Consensus.Forecast (OutsideForecastRange)
import Ouroboros.Consensus.Ledger.SupportsMempool (VersionWord16 (..))
import Ouroboros.Consensus.Leios.Acquisitions (LeiosAcquisition (..))
import Ouroboros.Consensus.Leios.TxCache
  ( LandedTx (..)
  , LeiosTxCache (..)
  , TxState (..)
  , defaultLeiosTxCacheShift
  , evictOlderThan
  , insertBody
  , newPureLeiosTxCache
  , packEndorserBlock
  )
import Ouroboros.Consensus.Leios.Types
  ( ElectionId (..)
  , EndorserBlock
  , LeiosEbByteSizes (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , encodeEndorserBlockSize
  , endorserBlockFromTxBytes
  , endorserBlockHash
  , forgeLeiosEb
  , hashLeiosTx
  , mkEndorserBlock
  , serialiseEndorserBlock
  )
import qualified Ouroboros.Consensus.Leios.Types as Leios
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
  ( ExnLeiosFetchInvalidReply (..)
  , JobMismatch (..)
  , LeiosFetchClientContext (..)
  , ReplyOwnership (..)
  , acquireForgedEb
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies as Replies
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Coverage (ClosureSummary (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( ClosureOfferTimes
  , EbFetchState (..)
  , EbState (..)
  , LeiosBlockRequest (..)
  , LeiosBlockTxsRequest (..)
  , LeiosOutstanding (..)
  , PeerOffer
  , assignRequestsObserved
  , defaultLeiosFetchStaticEnv
  , emptyLeiosOutstanding
  , focusElectionIfUnfocused
  , markReceivingFromForge
  , pruneOutstandingToImmTip
  , recordReferencedPoint
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs (nullJobPool)
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.LocalReuse (LocalSource (..), newLocalReuse)
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.ReceiptTasks as Receipts
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Request (LeiosFetchRequest)
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState as L
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Runtime as R
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
  ( FetchArrivalBytes
  , TraceLeiosFetchClientEvent (..)
  , fetchArrivalEvicted
  , fetchArrivalExtra
  , fetchArrivalGood
  , fetchArrivalInvalid
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Wait as W
import Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbHandle (..)
  , newLeiosDBInMemory
  )
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( BodyWriteReceipt (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , TxWriteReceipt (..)
  )
import Ouroboros.Consensus.Util.IOLike (IOLike, modifyMVar_, readMVar)
import qualified Ouroboros.Consensus.Util.IOLike as IOLike
import Ouroboros.Consensus.Util.NormalForm.StrictMVar (uncheckedNewMVar)
import System.Random (mkStdGen)
import qualified System.Timeout as Timeout
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

-- | Projection for lifecycle fixtures that only distinguish success/refusal.
-- Admission-reason regressions call the production API directly.
tryEnqueueRequest ::
  (IOLike.IOLike m, Ord peer) =>
  R.Client m peer -> IOLike.Time -> LeiosFetchRequest -> IOLike.STM m (Maybe (L.AttemptKey peer))
tryEnqueueRequest client at request =
  either (const Nothing) Just <$> R.enqueueRequest client at request

-- | Ignore detached reports when a scenario checks assignment state directly.
assignRequests ::
  (IOLike.IOLike m, Ord peer) =>
  R.Runtime m peer ->
  IOLike.Time ->
  (SlotNo -> Either OutsideForecastRange (LeiosEbByteSizes Leios.SizeLimit)) ->
  Maybe (SlotNo, Word64) ->
  ClosureOfferTimes peer ->
  Map.Map peer (Map.Map LeiosPoint PeerOffer) ->
  LeiosOutstanding peer ->
  m (LeiosOutstanding peer)
assignRequests rt at forecast horizon times offers out =
  fst <$> assignRequestsObserved rt at forecast horizon times offers out

-- | Keep the real expiry transition when its reports are not the test's subject.
expireRuntimeHeads ::
  Ord peer =>
  IOLike.Time ->
  (L.Work LeiosPoint -> Bool) ->
  R.RuntimeState m peer ->
  R.RuntimeState m peer
expireRuntimeHeads now wanted = fst . R.expireHeadsObserved now wanted

tests :: TestTree
tests =
  testGroup
    "replies"
    [ testCase "retired forged callbacks cannot recreate coverage" test_forgePrunedCoverage
    , testCase
        "incomplete combined forge confirmation cannot announce a phantom closure"
        test_forgeIncompleteConfirmation
    , testCase "closure summaries count confirmed network payload once" test_summary
    , testCase "forge summaries keep locally produced bytes local" test_forgeSummary
    , testCase "the client emits a body's acquisition, then its closure's, once each is durable" $
        test_acquisitions
    , testCase
        "cache eviction cannot invalidate a positive body receipt"
        test_cacheEvictionDuringBodyReceipt
    , testCase
        "duplicate hashes are classified without mutation and duplicate reply entries are rejected"
        test_repeatedArrivalSnapshot
    , testCase "an evicted point still settles a positive TX receipt" (test_evictedTxReceipt False)
    , testCase
        "a younger shared reference cannot redirect an evicted point receipt"
        (test_evictedTxReceipt True)
    , testCase "only confirmed TX offsets become held cache entries" test_partialTxConfirmation
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
        "overlapping transaction replies that both observe missing data each count those arrival bytes as needed"
        test_overlappingTxsReplies
    , testCase "the client counts transactions for a point pruned meanwhile as evicted" $
        test_txsForPrunedPoint
    , testCase
        "the client copies into a body's write the transactions another point holds, and fetches only the rest"
        test_fillsFromAnotherPoint
    , testCase
        "the client fetches all of a body's transactions once the point holding some of them leaves the LeiosTxCache"
        test_noFillsFromPrunedPoint
    , testCase
        "the client acquires the closure of an endorser block the node forged at a point whose body it already held, without acquiring the body again"
        test_forgedAfterBody
    , testCase
        "the client emits nothing more for an endorser block the node forged again at a point whose closure it already acquired"
        test_forgedAgain
    , testCase
        "the client counts a body sent by a peer while the node's own forge writes it as redundant, whether it arrives or settles meanwhile"
        test_bodyWhileForging
    , testCase
        "the client counts transactions sent by a peer while the node's own forge writes them as redundant"
        test_txsWhileForging
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
newContext :: IOLike m => m (LeiosFetchClientContext m Int, StrictTVar m [Acquired])
newContext = newContextHolding (pure ())

-- | 'newContext', whose body writes are not done until this returns.
newContextHolding :: IOLike m => m () -> m (LeiosFetchClientContext m Int, StrictTVar m [Acquired])
newContextHolding = let (point, _eb, _txs) = fixture in newContextAt point

-- | 'newContextHolding', fetching the endorser block at this point instead.
newContextAt ::
  IOLike m => LeiosPoint -> m () -> m (LeiosFetchClientContext m Int, StrictTVar m [Acquired])
newContextAt point release = newContextGated point (pure release)

-- | 'newContextAt', each of whose body writes is not done until what this
-- gives at that write returns.
newContextGated ::
  IOLike m => LeiosPoint -> m (m ()) -> m (LeiosFetchClientContext m Int, StrictTVar m [Acquired])
newContextGated point gate = do
  db <- newLeiosDBInMemory
  newContextOver db point gate

newContextOver ::
  IOLike m =>
  LeiosDbHandle m ->
  LeiosPoint ->
  m (m ()) ->
  m (LeiosFetchClientContext m Int, StrictTVar m [Acquired])
newContextOver db point gate = do
  let MkLeiosPoint slot ebHash = point
  writer <- holdingBodies <$> openWriter db
  receiptTasks <- Receipts.newReceiptTasks
  localReuse <-
    newLocalReuse
      (Receipts.receiptClosing receiptTasks)
      (LocalSource (const (pure Map.empty)) (\() -> pure Nothing))
      writer
  txCache <- newPureLeiosTxCache defaultLeiosTxCacheShift
  outstanding <-
    uncheckedNewMVar $
      focusElectionIfUnfocused (ElectionId slot electee) ebHash $
        recordReferencedPoint point SNothing $
          emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0)
  ready <- atomically W.newReadiness
  runtime <-
    R.newRuntime (defaultLeiosFetchStaticEnv) ready (Receipts.receiptClosing receiptTasks)
  notes <- newTVarIO []
  let note = \case
        AcquiredEb p _bodySize -> AcquiredBody p
        AcquiredEbTxs p -> AcquiredClosure p
  pure
    ( MkLeiosFetchClientContext
        { lfcStaticEnv = defaultLeiosFetchStaticEnv
        , lfcOutstanding = outstanding
        , lfcReady = ready
        , lfcRuntime = runtime
        , lfcTxCache = txCache
        , lfcReceiptTasks = receiptTasks
        , lfcLocalReuse = localReuse
        , lfcWriter = writer
        , lfcNotify = modifyTVar notes . (:) . note
        , lfcSystemTime = SystemTime (pure (RelativeTime 0)) (pure ())
        , lfcDetailTracer = nullTracer
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
  processLeiosBlock ctx peer (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])
  closureRequest ctx point >>= \req -> processLeiosBlockTxs ctx peer req (V.fromList txs)
  awaitNotes notes 2 >>= (@?= [AcquiredBody point, AcquiredClosure point])

-- | Cache lifetime does not decide whether a committed target was acquired.
test_cacheEvictionDuringBodyReceipt :: Assertion
test_cacheEvictionDuringBodyReceipt = do
  let (point, eb, txs) = fixture
  (hold, awaitHeld, release) <- newWriteHold
  (ctx0, notes) <- newContextHolding hold
  reuse <-
    newLocalReuse
      (Receipts.receiptClosing (lfcReceiptTasks ctx0))
      (LocalSource (const (pure (Map.fromList [(hashLeiosTx tx, tx) | tx <- txs]))) (pure . Just))
      (lfcWriter ctx0)
  let ctx = ctx0{lfcLocalReuse = reuse}
  withAsync
    (processLeiosBlock ctx peer (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb)
    $ \client -> do
      awaitHeld 1
      evictOlderThan (lfcTxCache ctx) (pointSlotNo point + 1)
      release
      wait client
  awaitNotes notes 2 >>= (@?= [AcquiredBody point, AcquiredClosure point])
  readMVar (lfcOutstanding ctx) >>= assertNoJobLeft point
  forM_ txs $ \tx ->
    withLookupTx (lfcTxCache ctx) (\lookupTx -> lookupTx (hashLeiosTx tx)) >>= (@?= SNothing)

-- | The one request the decision logic makes of a peer that offers the closure
-- of the endorser block at this point, whose body the client holds.
closureRequest :: LeiosFetchClientContext IO Int -> LeiosPoint -> IO LeiosBlockTxsRequest
closureRequest ctx point = do
  out <- readMVar (lfcOutstanding ctx)
  case Map.lookup point (ebState out) of
    Just (MkEbState _ (BodyAcquired pool)) -> case [ (jid, job)
                                                   | jid <- IntMap.keys (Jobs.jobMultiplicities pool)
                                                   , Just job <- [Jobs.lookupJob (Jobs.MkLeiosJobId jid) pool]
                                                   ] of
      first : rest -> pure (MkLeiosBlockTxsRequest point (NEIntMap.fromList (first :| rest)))
      [] -> assertFailure "no pending fixture jobs" >> fail "unreachable"
    _ -> assertFailure "fixture body not acquired" >> fail "unreachable"

-- | Wrong individual sizes must fail the commitment even when their errors
-- cancel in the aggregate. No TX write is submitted and the job stays pending.
test_wrongSizedTxs :: Assertion
test_wrongSizedTxs = do
  let tx0 = MkLeiosTx (BS8.pack "tx zero")
      tx1 = MkLeiosTx (BS8.pack "tx one")
      tx2 = MkLeiosTx (BS8.replicate (64 * 1024) 'x')
      eb = mkEndorserBlock [(hashLeiosTx tx0, 6), (hashLeiosTx tx1, 7), (hashLeiosTx tx2, 64 * 1024)]
      point = MkLeiosPoint (SlotNo 5) (endorserBlockHash eb)
  submitted <- newTVarIO False
  (ctx0, notes) <- newContextAt point (pure ())
  let writer = lfcWriter ctx0
      ctx =
        ctx0
          { lfcWriter =
              writer
                { writeTxs = \p txs -> do
                    atomically (modifyTVar submitted (const True))
                    writeTxs writer p txs
                }
          }
  processLeiosBlock ctx peer (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb
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
    Right () -> assertFailure "incorrect individual sizes passed validation"
  readTVarIO submitted >>= (@?= False)
  readTVarIO notes >>= (@?= [AcquiredBody point])
  MkLeiosBlockTxsRequest _ remaining <- closureRequest ctx point
  NEIntMap.size remaining @?= 2

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
        other
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
        eb
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
        eb
    )
    $ \client -> do
      awaitHeld 1
      -- What 'processLeiosBlock' itself signals, so that what is left to await
      -- is the body's settlement.
      atomically (W.awaitReadiness (lfcReady ctx))
      modifyMVar_ (lfcOutstanding ctx) $ pure . pruneOutstandingToImmTip (SlotNo 6)
      modifyMVar_ (lfcOutstanding ctx) $
        pure . recordReferencedPoint newer SNothing
      release
      wait client
  Timeout.timeout 5000000 (atomically (W.awaitReadiness (lfcReady ctx)))
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
  processLeiosBlock ctx peer (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])
  modifyMVar_ (lfcOutstanding ctx) (pure . markReceivingFromForge point)
  acquireForgedEb ctx point (forgeLeiosEb minBound (fmap leiosTxBytes fixtureTxs))
  awaitNotes notes 2 >>= (@?= [AcquiredBody point, AcquiredClosure point])
  -- Long enough for any further emission to have landed.
  threadDelay 100000
  reverse <$> readTVarIO notes >>= (@?= [AcquiredBody point, AcquiredClosure point])

-- | The node's own forge produces the 'fixture''s endorser block, and once its
-- writes have settled, another of the node's forges produces the same endorser
-- block at the same point.
test_forgedAgain :: Assertion
test_forgedAgain = do
  let (point, _eb, _txs) = fixture
  (ctx, notes) <- newContext
  let forgeAndSettle = do
        modifyMVar_ (lfcOutstanding ctx) (pure . markReceivingFromForge point)
        acquireForgedEb ctx point (forgeLeiosEb minBound (fmap leiosTxBytes fixtureTxs))
        Timeout.timeout 5000000 (atomically (W.awaitReadiness (lfcReady ctx)))
          >>= maybe (assertFailure "timed out waiting for the forge's writes to settle") pure
  forgeAndSettle
  reverse <$> readTVarIO notes >>= (@?= [AcquiredBody point, AcquiredClosure point])
  forgeAndSettle
  reverse <$> readTVarIO notes >>= (@?= [AcquiredBody point, AcquiredClosure point])
  readMVar (lfcOutstanding ctx) >>= assertNoJobLeft point

assertNoBody :: LeiosPoint -> LeiosOutstanding peer -> Assertion
assertNoBody point outstanding = case Map.lookup point (ebState outstanding) of
  Just (MkEbState _onset NoBody) -> pure ()
  Just (MkEbState _onset ReceivingFromForge{}) -> assertFailure "the point is being received from the forge"
  Just (MkEbState _onset BodyAcquired{}) -> assertFailure "the point holds a body"
  Nothing -> assertFailure "the point has no entry"

assertNoJobLeft :: LeiosPoint -> LeiosOutstanding peer -> Assertion
assertNoJobLeft point outstanding = case Map.lookup point (ebState outstanding) of
  Just (MkEbState _onset (BodyAcquired jobPool)) ->
    assertBool "the point has jobs left" (nullJobPool jobPool)
  Just (MkEbState _onset ReceivingFromForge{}) -> assertFailure "the point is being received from the forge"
  Just (MkEbState _onset NoBody) -> assertFailure "the point holds no body"
  Nothing -> assertFailure "the point has no entry"

test_wrongOfferedSize :: Assertion
test_wrongOfferedSize = do
  let (point, eb, _txs) = fixture
      size = encodeEndorserBlockSize eb
  (ctx, notes) <- newContext
  result <- try $ processLeiosBlock ctx peer (MkLeiosBlockRequest point (size + 1) maxBound) eb
  case result of
    Left (ExnLeiosBlockWrongSize p offered actual) -> (p, offered, actual) @?= (point, size + 1, size)
    Left exn -> assertFailure $ "unexpected exception: " <> show exn
    Right () -> assertFailure "the client kept a peer that misreported the body's size"
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])

test_bodyTooBig :: Assertion
test_bodyTooBig = do
  let (point, eb, _txs) = fixture
      size = encodeEndorserBlockSize eb
      bounds = Leios.idLeiosEbByteSizesLimit MkLeiosEbByteSizes{lebBodySize = size - 1, lebTxsSize = maxBound}
  (ctx, notes) <- newContext
  result <- try $ processLeiosBlock ctx peer (MkLeiosBlockRequest point (size + 1) bounds) eb
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
      bounds =
        Leios.idLeiosEbByteSizesLimit MkLeiosEbByteSizes{lebBodySize = maxBound, lebTxsSize = closure - 1}
  (ctx, notes) <- newContext
  result <-
    try $ processLeiosBlock ctx peer (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) bounds) eb
  case result of
    Left (ExnLeiosClosureTooBig p actual most) ->
      (p, actual, most) @?= (point, closure, closure - 1)
    Left exn -> assertFailure $ "unexpected exception: " <> show exn
    Right () -> assertFailure "the client kept a peer that sent a body referencing too many bytes"
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
  processLeiosBlock ctx peer (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb
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
  withAsync (processLeiosBlock ctx peer req eb) $ \client ->
    withAsync (processLeiosBlock ctx otherPeer req eb) $ \otherClient -> do
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
  processLeiosBlock ctx peer req eb
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])
  processLeiosBlock ctx otherPeer req eb
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
    eb
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])
  req <- closureRequest ctx point
  withAsync (processLeiosBlockTxs ctx peer req (V.fromList txs)) $ \client ->
    withAsync (processLeiosBlockTxs ctx otherPeer req (V.fromList txs)) $ \otherClient -> do
      awaitHeld 2
      release
      wait client >> wait otherClient
  arrivals <- awaitArrivals txsArrivals traces 2
  [p | (p, _point, _bytes) <- arrivals] @?= [peer, otherPeer]
  -- Both handlers reached the held receipt after classifying missing TXs.
  -- Their read-only snapshots must not mark either pending write as held.
  [bytes | (_peer, _point, bytes) <- arrivals]
    @?= [fetchArrivalGood closure, fetchArrivalGood closure]
  awaitNotes notes 2 >>= (@?= [AcquiredBody point, AcquiredClosure point])

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
  processLeiosBlock ctx peer (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])
  req <- closureRequest ctx point
  modifyMVar_ (lfcOutstanding ctx) $ pure . pruneOutstandingToImmTip (SlotNo 6)
  processLeiosBlockTxs ctx peer req (V.fromList txs)
  events <- readTVarIO traces
  txsArrivals events @?= [(peer, point, fetchArrivalEvicted closure)]

-- | An endorser block at a newer point than the 'fixture''s, referencing the
-- 'fixture''s second transaction and one of its own.
sharing :: (LeiosPoint, EndorserBlock, LeiosTx, LeiosTx)
sharing = (MkLeiosPoint (SlotNo 6) (endorserBlockHash eb), eb, shared, own)
 where
  shared = MkLeiosTx (BS8.pack "tx one")
  own = MkLeiosTx (BS8.pack "tx two")
  eb = endorserBlockFromTxBytes (fmap leiosTxBytes (shared :| [own]))

-- | The client acquires the 'fixture''s closure, then this runs, then the
-- client takes in the body of 'sharing' and fetches its closure, which a peer
-- serves with these transactions. The notifications, oldest first.
acquireSharing :: (LeiosFetchClientContext IO Int -> IO ()) -> [LeiosTx] -> IO [Acquired]
acquireSharing between sent = do
  let (point, eb, txs) = fixture
      (pointB, ebB, _shared, _own) = sharing
  (ctx, notes) <- newContext
  modifyMVar_ (lfcOutstanding ctx) $
    pure
      . focusElectionIfUnfocused (fixtureElection pointB) (pointEbHash pointB)
      . recordReferencedPoint pointB SNothing
  processLeiosBlock
    ctx
    peer
    (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
    eb
  _ <- awaitNotes notes 1
  closureRequest ctx point >>= \req -> processLeiosBlockTxs ctx peer req (V.fromList txs)
  _ <- awaitNotes notes 2
  between ctx
  processLeiosBlock
    ctx
    peer
    (MkLeiosBlockRequest pointB (encodeEndorserBlockSize ebB) maxBound)
    ebB
  _ <- awaitNotes notes 3
  closureRequest ctx pointB >>= \req -> processLeiosBlockTxs ctx peer req (V.fromList sent)
  awaitNotes notes 4

test_fillsFromAnotherPoint :: Assertion
test_fillsFromAnotherPoint = do
  let (point, _eb, _txs) = fixture
      (pointB, _ebB, _shared, own) = sharing
  acquireSharing (\_ctx -> pure ()) [own]
    >>= (@?= [AcquiredBody point, AcquiredClosure point, AcquiredBody pointB, AcquiredClosure pointB])

test_noFillsFromPrunedPoint :: Assertion
test_noFillsFromPrunedPoint = do
  let (point, _eb, _txs) = fixture
      (pointB, _ebB, shared, own) = sharing
  acquireSharing (\ctx -> evictOlderThan (lfcTxCache ctx) (pointSlotNo pointB)) [shared, own]
    >>= (@?= [AcquiredBody point, AcquiredClosure point, AcquiredBody pointB, AcquiredClosure pointB])

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

-- | One peer's reply with the 'fixture''s body is being written when the
-- node's own forge produces the same endorser block at that point, and another
-- peer's arrives after.
test_bodyWhileForging :: Assertion
test_bodyWhileForging = do
  let (point, eb, _txs) = fixture
      size = encodeEndorserBlockSize eb
      req = MkLeiosBlockRequest point size maxBound
  (hold, awaitHeld, release) <- newWriteHold
  (ctx, notes, traces) <- withTraces $ newContextHolding hold
  withAsync (processLeiosBlock ctx peer req eb) $ \client -> do
    awaitHeld 1
    modifyMVar_ (lfcOutstanding ctx) (pure . markReceivingFromForge point)
    release
    wait client
  processLeiosBlock ctx otherPeer req eb
  awaitArrivals blockArrivals traces 2
    >>= (@?= [(peer, point, fetchArrivalExtra size), (otherPeer, point, fetchArrivalExtra size)])
  readTVarIO notes >>= (@?= [])

test_txsWhileForging :: Assertion
test_txsWhileForging = do
  let (point, eb, txs) = fixture
      closure = sum [fromIntegral (BS8.length (leiosTxBytes tx)) | tx <- txs]
  (ctx, notes, traces) <- withTraces newContext
  processLeiosBlock ctx peer (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])
  req <- closureRequest ctx point
  modifyMVar_ (lfcOutstanding ctx) (pure . markReceivingFromForge point)
  processLeiosBlockTxs ctx peer req (V.fromList txs)
  events <- readTVarIO traces
  [(p, pt, bytes) | TraceLeiosFetchClientTxsArrived p pt bytes <- events]
    @?= [(peer, point, fetchArrivalExtra closure)]

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
    eb
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])
  req <- closureRequest ctx point
  modifyMVar_ (lfcOutstanding ctx) (pure . markReceivingFromForge point)
  acquireForgedEb ctx point (forgeLeiosEb minBound (fmap leiosTxBytes fixtureTxs))
  _ <- awaitNotes notes 2
  processLeiosBlockTxs ctx peer req (V.fromList txs)
  awaitArrivals txsArrivals traces 1 >>= (@?= [(peer, point, fetchArrivalExtra closure)])
  reverse <$> readTVarIO notes >>= (@?= [AcquiredBody point, AcquiredClosure point])

-- | The election announcing the endorser block at this point.
fixtureElection :: LeiosPoint -> ElectionId
fixtureElection point = ElectionId (pointSlotNo point) electee

-- Unit fixtures isolate payload validation/storage. Runtime ownership and actual
-- client/driver interactions are exercised separately by lifecycle tests.
unitOwnership :: ReplyOwnership IO
unitOwnership = ReplyOwnership (pure True) (\_ _ -> pure (pure ())) (\_ -> pure (pure ())) (pure ())

processLeiosBlock ::
  LeiosFetchClientContext IO Int -> Int -> LeiosBlockRequest -> EndorserBlock -> IO ()
processLeiosBlock ctx source request body = Replies.processLeiosBlock unitOwnership ctx source request (serialiseEndorserBlock body)

processLeiosBlockTxs ::
  LeiosFetchClientContext IO Int -> Int -> LeiosBlockTxsRequest -> V.Vector LeiosTx -> IO ()
processLeiosBlockTxs = Replies.processLeiosBlockTxs unitOwnership

-- Real writer receipts and the normal settlement path, including a duplicate
-- response after semantic completion. Arrival counters are deliberately unused.
test_summary :: Assertion
test_summary = do
  (base, notes) <- newContext
  summaries <- newTVarIO []
  let (point, eb, txs) = fixture
      ctx =
        base
          { lfcTracer = mkTracer $ \case
              TraceLeiosFetchClientClosureSummary p value -> atomically (modifyTVar summaries ((p, value) :))
              _ -> pure ()
          }
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ do
    processLeiosBlock ctx peer (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb
    _ <- awaitNotes notes 1
    req <- closureRequest ctx point
    processLeiosBlockTxs ctx peer req (V.fromList txs)
    _ <- awaitNotes notes 2
    processLeiosBlockTxs ctx otherPeer req (V.fromList txs)
  let total = fromIntegral (sum (map (BS8.length . leiosTxBytes) txs))
  readTVarIO summaries >>= (@?= [(point, ClosureSummary total 0 total 2 (Just total) False)])

test_forgeSummary :: Assertion
test_forgeSummary = do
  (base, _) <- newContext
  summaries <- newTVarIO []
  let (point, _, txs) = fixture
      ctx =
        base
          { lfcTracer = mkTracer $ \case
              TraceLeiosFetchClientClosureSummary _ value -> atomically (modifyTVar summaries (value :))
              _ -> pure ()
          }
  modifyMVar_ (lfcOutstanding ctx) (pure . markReceivingFromForge point)
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $
    acquireForgedEb ctx point (forgeLeiosEb minBound (fmap leiosTxBytes fixtureTxs))
  let total = fromIntegral (sum (map (BS8.length . leiosTxBytes) txs))
  readTVarIO summaries >>= (@?= [ClosureSummary total total 0 0 Nothing True])

-- A combined forge write must confirm every requested transaction.
test_forgeIncompleteConfirmation :: Assertion
test_forgeIncompleteConfirmation = do
  let (point, _, _) = fixture
  db <- newLeiosDBInMemory
  (base, notes) <- newContextOver db point (pure (pure ()))
  let original = lfcWriter base
      ctx =
        base{lfcWriter = original{writeEbBody = \_ _ _ _ -> pure (Promise (pure (BodyPresent Set.empty)))}}
  modifyMVar_ (lfcOutstanding ctx) (pure . markReceivingFromForge point)
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $
    acquireForgedEb ctx point (forgeLeiosEb minBound (fmap leiosTxBytes fixtureTxs))
  readTVarIO notes >>= (@?= [])

test_forgePrunedCoverage :: Assertion
test_forgePrunedCoverage = do
  let (point, _, _) = fixture
  gate <- newEmptyTMVarIO
  (ctx, notes) <- newContextHolding (atomically (readTMVar gate))
  modifyMVar_ (lfcOutstanding ctx) (pure . markReceivingFromForge point)
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ do
    acquireForgedEb ctx point (forgeLeiosEb minBound (fmap leiosTxBytes fixtureTxs))
    modifyMVar_ (lfcOutstanding ctx) (pure . pruneOutstandingToImmTip (SlotNo 6))
    atomically (putTMVar gate ())
  readTVarIO notes >>= (@?= [])
  out <- readMVar (lfcOutstanding ctx)
  Map.null (closureCoverage out) @?= True
  Map.null (ebState out) @?= True

-- The ledger reference map canonicalizes repeated hashes. The batch cache API
-- must still account for repeated occurrences without mutation; a wire reply
-- supplying an extra occurrence fails count validation before writer admission.
test_repeatedArrivalSnapshot :: Assertion
test_repeatedArrivalSnapshot = do
  let (_, _, txs) = fixture
      tx = head txs
      eb = endorserBlockFromTxBytes (leiosTxBytes tx :| [leiosTxBytes tx])
      point = MkLeiosPoint 5 (endorserBlockHash eb)
      size = fromIntegral (BS8.length (leiosTxBytes tx))
  (ctx, notes, traces) <- withTraces (newContextAt point (pure ()))
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ do
    processLeiosBlock ctx peer (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb
    _ <- awaitNotes notes 1
    req <- closureRequest ctx point
    classifyTxs (lfcTxCache ctx) point [(hashLeiosTx tx, size), (hashLeiosTx tx, size)]
      >>= (@?= fetchArrivalGood (2 * size))
    result <- try (processLeiosBlockTxs ctx peer req (V.fromList [tx, tx]))
    case result of
      Left (ExnLeiosBlockTxsWrongCount p expected received) -> (p, expected, received) @?= (point, 1, 2)
      Left other -> assertFailure (show other)
      Right () -> assertFailure "unsolicited duplicate transaction was accepted"
  reverse <$> readTVarIO notes >>= (@?= [AcquiredBody point])
  withLookupTx (lfcTxCache ctx) (\lookupTx -> lookupTx (hashLeiosTx tx)) >>= (@?= SNothing)
  txsArrivals <$> readTVarIO traces >>= (@?= [(peer, point, fetchArrivalInvalid (2 * size))])

-- Eviction is a cache-capacity event, not storage failure or logical pruning.
-- The original immutable arrival counters survive a subsequent cache change.
test_evictedTxReceipt :: Bool -> Assertion
test_evictedTxReceipt shared = do
  let (point, eb, txs) = fixture
      younger = point{pointSlotNo = pointSlotNo point + 1}
      bytes = sum [fromIntegral (BS8.length (leiosTxBytes tx)) | tx <- txs]
      x = head txs
      y = last txs
  (base, notes, traces) <- withTraces newContext
  (hold, awaitHeld, release) <- newWriteHold
  let ctx = base{lfcWriter = holdingTxs hold (lfcWriter base)}
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ do
    processLeiosBlock ctx peer (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb
    _ <- awaitNotes notes 1
    req <- closureRequest ctx point
    processLeiosBlockTxs ctx peer req (V.fromList txs)
    awaitHeld 1
    awaitArrivals txsArrivals traces 1 >>= (@?= [(peer, point, fetchArrivalGood bytes)])
    if shared
      then do
        _ <- await =<< writeEbBody (lfcWriter base) younger eb Map.empty Map.empty
        _ <- await =<< writeTxs (lfcWriter base) younger (Map.singleton 0 (leiosTxBytes x))
        _ <- insertBody (lfcTxCache ctx) younger (packEndorserBlock eb)
        _ <-
          insertLandedTxs
            (lfcTxCache ctx)
            younger
            [ MkLandedTx
                0
                (hashLeiosTx x)
                (fromIntegral (BS8.length (leiosTxBytes x)))
                (TxValidated (MkVersionWord16 10))
            ]
        pure ()
      else pure ()
    evictOlderThan (lfcTxCache ctx) (pointSlotNo younger)
    release
  awaitNotes notes 2 >>= (@?= [AcquiredBody point, AcquiredClosure point])
  txsArrivals <$> readTVarIO traces >>= (@?= [(peer, point, fetchArrivalGood bytes)])
  readMVar (lfcOutstanding ctx) >>= assertNoJobLeft point
  withLookupTx (lfcTxCache ctx) (\lookupTx -> lookupTx (hashLeiosTx y)) >>= (@?= SNothing)
  withLookupTx (lfcTxCache ctx) (\lookupTx -> lookupTx (hashLeiosTx x))
    >>= (@?= if shared then SJust (TxValidated (MkVersionWord16 10)) else SNothing)
  classifyTxs
    (lfcTxCache ctx)
    point
    [(hashLeiosTx tx, fromIntegral (BS8.length (leiosTxBytes tx))) | tx <- txs]
    >>= (@?= fetchArrivalEvicted bytes)

-- Storage may confirm fewer offsets than were submitted. Unconfirmed entries
-- cannot acquire a closure or become reusable via another retained point.
test_partialTxConfirmation :: Assertion
test_partialTxConfirmation = do
  let (point, eb, txs) = fixture
  (base, notes) <- newContext
  let writer = lfcWriter base
      partial =
        writer
          { writeTxs = \p payloads -> do
              pending <- writeTxs writer p payloads
              pure $ Promise $ do
                receipt <- await pending
                pure receipt{txWriteConfirmed = Set.intersection (Set.singleton 0) (txWriteConfirmed receipt)}
          }
      ctx = base{lfcWriter = partial}
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ do
    processLeiosBlock ctx peer (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb
    _ <- awaitNotes notes 1
    req <- closureRequest ctx point
    processLeiosBlockTxs ctx peer req (V.fromList txs)
  reverse <$> readTVarIO notes >>= (@?= [AcquiredBody point])
  _ <- closureRequest ctx point
  withLookupTx (lfcTxCache ctx) (\lookupTx -> lookupTx (hashLeiosTx (head txs)))
    >>= (@?= SJust (TxInserted ()))
  withLookupTx (lfcTxCache ctx) (\lookupTx -> lookupTx (hashLeiosTx (last txs))) >>= (@?= SNothing)
