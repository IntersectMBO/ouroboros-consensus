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
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.Tracer (nullTracer)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short as SBS
import Data.Foldable (toList)
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntSet as IntSet
import Data.List (isInfixOf)
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
  , LeiosPoint (..)
  , LeiosTx (..)
  , encodeEndorserBlockSize
  , endorserBlockFromTxBytes
  , endorserBlockHash
  , hashLeiosTx
  , mkEndorserBlock
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
  ( ExnLeiosFetchInvalidReply (..)
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
  , removePeerFromOutstanding
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs (LeiosJob (..), nullLeiosJobPool)
import Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbHandle (..)
  , newLeiosDBInMemory
  )
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbWriter (..)
  , Promise (..)
  , TxWriteReceipt (..)
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
    , testCase "an unconfirmed write stays fetchable and emits no closure acquisition" $
        test_unconfirmedTxs
    , testCase "a durable write can settle after the serving peer was removed" $
        test_txsAfterPeerRemoval
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
      { writeEbBody = \p eb -> do
          release <- gate
          written <- writeEbBody writer p eb
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
  processLeiosBlock ctx peer (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])
  closureRequest ctx point >>= \req -> processLeiosBlockTxs ctx peer req (V.fromList txs)
  awaitNotes notes 2 >>= (@?= [AcquiredBody point, AcquiredClosure point])

test_unconfirmedTxs :: Assertion
test_unconfirmedTxs = do
  let (point, eb, txs) = fixture
  (ctx0, notes) <- newContext
  let writer =
        (lfcWriter ctx0)
          { writeTxs = \_ submitted ->
              pure $
                Promise $
                  pure (TxWriteReceipt IntSet.empty (IntSet.fromList (map fst submitted)) [])
          }
      ctx = ctx0{lfcWriter = writer}
  processLeiosBlock ctx peer (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb
  _ <- awaitNotes notes 1
  req@(MkLeiosBlockTxsRequest _ jobs) <- closureRequest ctx point
  processLeiosBlockTxs ctx peer req (V.fromList txs)
  -- Poll the ownership transition rather than readiness, which can coalesce
  -- with the earlier arrival notification.
  settled <-
    SI.timeout 5 $
      let
        wait = do
          outstanding <- readMVar (lfcOutstanding ctx)
          if maybe False (Map.member point) (Map.lookup peer (requestedJobsPerPeer outstanding))
            then SI.threadDelay 0.001 >> wait
            else pure ()
       in
        wait
  settled @?= Just ()
  readTVarIO notes >>= (@?= [AcquiredBody point])
  MkLeiosBlockTxsRequest _ retry <- closureRequest ctx point
  let offsets = foldMap (\(MkLeiosJob offs _ _) -> offs)
  offsets retry @?= offsets jobs

test_txsAfterPeerRemoval :: Assertion
test_txsAfterPeerRemoval = do
  let (point, eb, txs) = fixture
  (ctx0, notes) <- newContext
  gate <- newEmptyTMVarIO
  let original = lfcWriter ctx0
      writer =
        original
          { writeTxs = \p submitted -> do
              written <- writeTxs original p submitted
              pure $ Promise $ atomically (readTMVar gate) >> await written
          }
      ctx = ctx0{lfcWriter = writer}
  processLeiosBlock ctx peer (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb
  _ <- awaitNotes notes 1
  req <- closureRequest ctx point
  processLeiosBlockTxs ctx peer req (V.fromList txs)
  modifyMVar_ (lfcOutstanding ctx) (pure . removePeerFromOutstanding peer)
  atomically $ putTMVar gate ()
  awaitNotes notes 2 >>= (@?= [AcquiredBody point, AcquiredClosure point])
  outstanding <- readMVar (lfcOutstanding ctx)
  Map.member peer (requestedJobsPerPeer outstanding) @?= False
  assertNoJobLeft point outstanding

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
  processLeiosBlock ctx peer (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb
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
  written <- newEmptyTMVarIO
  (ctx, notes) <- newContextHolding (atomically (readTMVar written))
  processLeiosBlock ctx peer (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb
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
  processLeiosBlock ctx peer (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound) eb
  -- What 'processLeiosBlock' itself signals, so that what is left to await is
  -- the body's settlement.
  takeMVar (lfcReady ctx)
  modifyMVar_ (lfcOutstanding ctx) $ pure . snd . pruneOutstandingToImmTip (SlotNo 6)
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
    assertBool "the point has jobs left" (nullLeiosJobPool jobPool)
  Just (MkEbState _onset NoBody) -> assertFailure "the point holds no body"
  Nothing -> assertFailure "the point has no entry"
