{-# LANGUAGE LambdaCase #-}

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
import Control.Monad (forM_)
import Control.Tracer (nullTracer)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.Foldable (toList)
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Vector.Strict as V
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  , SystemTime (..)
  )
import Ouroboros.Consensus.Leios.Acquisitions (LeiosEbNotification (..))
import Ouroboros.Consensus.Leios.TxCache
  ( LeiosTxCache (..)
  , defaultLeiosTxCacheShift
  , newPureLeiosTxCache
  , packTxReferences
  )
import Ouroboros.Consensus.Leios.Types
  ( ElectionId (..)
  , EndorserBlock
  , EndorserBlockEnvelope (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , RankingBlockHash (..)
  , encodeEndorserBlockSize
  , endorserBlockFromTxBytes
  , endorserBlockHash
  , hashEndorserBlockBytes
  , hashLeiosTx
  , mkEndorserBlockEnvelope
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
  ( ExnLeiosFetchInvalidReply (..)
  , LeiosFetchClientContext (..)
  , processLeiosBlock
  , processLeiosBlockTxs
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( LeiosBlockRequest (..)
  , LeiosBlockTxsRequest (..)
  , LeiosFetchRequest (..)
  , LeiosOutstanding (..)
  , PeerOffer (..)
  , WhetherTxsClosureOffered (..)
  , defaultLeiosFetchStaticEnv
  , ebStateHasBody
  , emptyLeiosOutstanding
  , focusElectionIfUnfocused
  , leiosFetchLogicIteration
  , pruneOutstandingToImmTip
  , recordMaxAnnouncementSlot
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs (LeiosJob (..))
import Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbHandle (..)
  , newLeiosDBInMemory
  )
import Ouroboros.Consensus.Storage.LeiosDB.API
  ( LeiosDbReader (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , withReader
  )
import Ouroboros.Consensus.Util.IOLike (modifyMVar, modifyMVar_, readMVar, takeMVar)
import Ouroboros.Consensus.Util.NormalForm.StrictMVar (uncheckedNewEmptyMVar, uncheckedNewMVar)
import Ouroboros.Network.Block (Serialised (..))
import System.Random (mkStdGen)
import qualified System.Timeout as Timeout
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "replies"
    [ testCase "the client announces a body, then its closure, once each is durable" $
        test_acquisitions
    , testCase "the client disconnects a peer whose body has another hash" $
        test_wrongHash
    , testCase "the client disconnects a peer whose body has another hash, without decoding it" $
        test_wrongHashUndecodable
    , testCase "the client disconnects a peer whose body has the hash but does not decode" $
        test_undecodable
    , testCase
        "the client announces a body at the newer slot an election focused it at while it was being written, which the LeiosDB then holds"
        test_refocusedWhileWriting
    , testCase
        "the client announces nothing for a body the LeiosDB collected while it was being written, which it then fetches again"
        test_collectedWhileWriting
    , testCase
        "the client fetches again both txs a body's write filled, if meanwhile the LeiosDB collected them and another delivery wrote the body again without them"
        $ test_collectedAndRewrittenWhileWriting 2
    , testCase
        "the client fetches again the one tx a body's write filled, if meanwhile the LeiosDB collected it and another delivery wrote the body again without it"
        $ test_collectedAndRewrittenWhileWriting 1
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
newContextHolding release = do
  let (point, _eb, _txs) = fixture
      MkLeiosPoint slot ebHash = point
  db <- newLeiosDBInMemory
  writer <- holdingBodies <$> openWriter db
  txCache <- newPureLeiosTxCache defaultLeiosTxCacheShift
  _ <-
    insertAnnouncement txCache slot (MkRankingBlockHash (UnsafeHash (SBS.pack (replicate 32 1)))) ebHash
  outstanding <-
    uncheckedNewMVar $
      focusElectionIfUnfocused (ElectionId slot electee) ebHash $
        recordMaxAnnouncementSlot ebHash slot SNothing $
          emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0)
  ready <- uncheckedNewEmptyMVar
  notes <- newTVarIO []
  let note = \case
        AcquiredEb p _sizes -> AcquiredBody p
        AcquiredEbTxs p _sizes -> AcquiredClosure p
  pure
    ( MkLeiosFetchClientContext
        { lfcStaticEnv = defaultLeiosFetchStaticEnv maxBound
        , lfcOutstanding = outstanding
        , lfcReady = ready
        , lfcTxCache = txCache
        , lfcWriter = writer
        , lfcLeiosDb = db
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
    (mkEndorserBlockEnvelope eb)
  awaitNotes notes 1 >>= (@?= [AcquiredBody point])
  -- The peer offers the closure, so the decision logic asks it for the jobs.
  reqs <-
    modifyMVar (lfcOutstanding ctx) $ \outstanding -> do
      let (outstanding', decisions, _drops) =
            leiosFetchLogicIteration
              (lfcStaticEnv ctx)
              (\_slot -> Right maxBound)
              (Just (SlotNo 6))
              (Map.singleton peer (Map.singleton point (MkPeerOffer SNothing SNothing TxsClosureOffered)))
              Map.empty
              outstanding
      pure (outstanding', [r | LeiosBlockTxsRequest r <- foldMap toList decisions])
  case reqs of
    [req] -> processLeiosBlockTxs ctx peer req (V.fromList txs)
    _ -> assertFailure $ "expected one request, got " <> show (length reqs)
  awaitNotes notes 2 >>= (@?= [AcquiredBody point, AcquiredClosure point])

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
        (mkEndorserBlockEnvelope other)
  case result of
    Left (ExnLeiosBlockWrongHash p h) -> (p, h) @?= (point, endorserBlockHash other)
    Left exn -> assertFailure $ "unexpected exception: " <> show exn
    Right () -> assertFailure "the client accepted a body with another hash"
  atomically (readTVar notes) >>= (@?= [])

-- | Bytes that are no endorser block.
notAnEndorserBlock :: BS8.ByteString
notAnEndorserBlock = BS8.pack "not an endorser block"

-- | These bytes in the envelope of version 0, as an endorser block's.
enveloped :: BS8.ByteString -> EndorserBlockEnvelope
enveloped bytes = MkEndorserBlockEnvelope 0 (Serialised (BSL.fromStrict bytes))

test_wrongHashUndecodable :: Assertion
test_wrongHashUndecodable = do
  let (point, _eb, _txs) = fixture
  (ctx, notes) <- newContext
  result <-
    try $
      processLeiosBlock
        ctx
        peer
        (MkLeiosBlockRequest point (fromIntegral (BS8.length notAnEndorserBlock)) maxBound)
        (enveloped notAnEndorserBlock)
  case result of
    Left (ExnLeiosBlockWrongHash p h) -> (p, h) @?= (point, hashEndorserBlockBytes notAnEndorserBlock)
    Left exn -> assertFailure $ "unexpected exception: " <> show exn
    Right () -> assertFailure "the client accepted a body with another hash"
  atomically (readTVar notes) >>= (@?= [])

test_undecodable :: Assertion
test_undecodable = do
  let point = MkLeiosPoint (SlotNo 5) (hashEndorserBlockBytes notAnEndorserBlock)
  (ctx, notes) <- newContext
  result <-
    try $
      processLeiosBlock
        ctx
        peer
        (MkLeiosBlockRequest point (fromIntegral (BS8.length notAnEndorserBlock)) maxBound)
        (enveloped notAnEndorserBlock)
  case result of
    Left (ExnLeiosBlockUndecodable p _err) -> p @?= point
    Left exn -> assertFailure $ "unexpected exception: " <> show exn
    Right () -> assertFailure "the client accepted bytes that are no endorser block"
  atomically (readTVar notes) >>= (@?= [])

-- | An election focuses the 'fixture' at a newer slot while its body is being
-- written.
test_refocusedWhileWriting :: Assertion
test_refocusedWhileWriting = do
  let (point, eb, _txs) = fixture
      newer = MkLeiosPoint (SlotNo 7) (pointEbHash point)
  written <- newEmptyTMVarIO
  (ctx, notes) <- newContextHolding (atomically (readTMVar written))
  processLeiosBlock
    ctx
    peer
    (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
    (mkEndorserBlockEnvelope eb)
  modifyMVar_ (lfcOutstanding ctx) $
    pure . recordMaxAnnouncementSlot (pointEbHash point) (pointSlotNo newer) SNothing
  atomically $ putTMVar written ()
  awaitNotes notes 1 >>= (@?= [AcquiredBody newer])
  points <- withReader (lfcLeiosDb ctx) testScanEbPoints
  (pointSlotNo newer, pointEbHash newer) `elem` points @?= True

-- | While the 'fixture''s body is being written, pruning passes its point, the
-- LeiosDB collects it, and an election focuses it anew at a newer slot, whose
-- point is written.
test_collectedWhileWriting :: Assertion
test_collectedWhileWriting = do
  let (point, eb, _txs) = fixture
      ebHash = pointEbHash point
      newer = MkLeiosPoint (SlotNo 8) ebHash
  written <- newEmptyTMVarIO
  (ctx, notes) <- newContextHolding (atomically (readTMVar written))
  processLeiosBlock
    ctx
    peer
    (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
    (mkEndorserBlockEnvelope eb)
  -- What 'processLeiosBlock' itself signals, so that what is left to await is
  -- the body's settlement.
  takeMVar (lfcReady ctx)
  modifyMVar_ (lfcOutstanding ctx) $ pure . snd . pruneOutstandingToImmTip (SlotNo 6)
  garbageCollect (lfcLeiosDb ctx) (SlotNo 6)
  await =<< writeEbPoint (lfcWriter ctx) newer (encodeEndorserBlockSize eb)
  modifyMVar_ (lfcOutstanding ctx) $
    pure . recordMaxAnnouncementSlot ebHash (pointSlotNo newer) SNothing
  atomically $ putTMVar written ()
  Timeout.timeout 5000000 (takeMVar (lfcReady ctx))
    >>= maybe (assertFailure "timed out waiting for the body to settle") pure
  readTVarIO notes >>= (@?= [])
  outstanding <- readMVar (lfcOutstanding ctx)
  (ebStateHasBody <$> Map.lookup ebHash (ebState outstanding)) @?= Just False

-- | While the 'fixture''s body is being written, filling this many of its txs
-- from another endorser block, pruning passes both endorser blocks' points and the
-- LeiosDB collects them. An election then focuses the 'fixture' anew at a
-- newer slot, and another delivery writes its point and body, without any
-- txs. Once the first write settles, the peer offers the closure.
test_collectedAndRewrittenWhileWriting :: Int -> Assertion
test_collectedAndRewrittenWhileWriting filled = do
  let (point, eb, txs) = fixture
      ebHash = pointEbHash point
      newer = MkLeiosPoint (SlotNo 8) ebHash
  written <- newEmptyTMVarIO
  (ctx, notes) <- newContextHolding (atomically (readTMVar written))
  holdSource ctx (take filled txs)
  processLeiosBlock
    ctx
    peer
    (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
    (mkEndorserBlockEnvelope eb)
  takeMVar (lfcReady ctx)
  modifyMVar_ (lfcOutstanding ctx) $ pure . snd . pruneOutstandingToImmTip (SlotNo 6)
  garbageCollect (lfcLeiosDb ctx) (SlotNo 6)
  modifyMVar_ (lfcOutstanding ctx) $
    pure
      . focusElectionIfUnfocused (ElectionId (pointSlotNo newer) electee) ebHash
      . recordMaxAnnouncementSlot ebHash (pointSlotNo newer) SNothing
  writer <- openWriter (lfcLeiosDb ctx)
  await =<< writeEbPoint writer newer (encodeEndorserBlockSize eb)
  _ <- await =<< writeEbBody writer newer eb []
  atomically $ putTMVar written ()
  Timeout.timeout 5000000 (takeMVar (lfcReady ctx))
    >>= maybe (assertFailure "timed out waiting for the body to settle") pure
  reqs <-
    modifyMVar (lfcOutstanding ctx) $ \outstanding -> do
      let (outstanding', decisions, _drops) =
            leiosFetchLogicIteration
              (lfcStaticEnv ctx)
              (\_slot -> Right maxBound)
              (Just (SlotNo 9))
              (Map.singleton peer (Map.singleton newer (MkPeerOffer SNothing SNothing TxsClosureOffered)))
              Map.empty
              outstanding
      pure (outstanding', [r | LeiosBlockTxsRequest r <- foldMap toList decisions])
  forM_ reqs $ \req@(MkLeiosBlockTxsRequest _point jobs) ->
    processLeiosBlockTxs ctx peer req $
      V.fromList [txs !! off | off <- IntSet.toAscList (foldMap (\(MkLeiosJob offs _ _) -> offs) jobs)]
  awaitNotes notes 2 >>= (@?= [AcquiredBody newer, AcquiredClosure newer])
  closure <- withReader (lfcLeiosDb ctx) $ \reader -> lookupEbClosure reader ebHash
  closure @?= Just (map leiosTxBytes txs)

-- | Another endorser block, announced in slot 3, holding these txs after one
-- of its own: the LeiosDB holds its closure, and the tx cache knows where.
holdSource :: LeiosFetchClientContext IO Int -> [LeiosTx] -> IO ()
holdSource ctx held = do
  writer <- openWriter (lfcLeiosDb ctx)
  await =<< writeEbPoint writer point (encodeEndorserBlockSize eb)
  _ <- await =<< writeEbBody writer point eb []
  _ <- await =<< writeTxs writer point (zip [0 ..] (map leiosTxBytes txs))
  _ <-
    insertAnnouncement
      (lfcTxCache ctx)
      (pointSlotNo point)
      (MkRankingBlockHash (UnsafeHash (SBS.pack (replicate 32 2))))
      ebHash
  _ <- insertBody (lfcTxCache ctx) ebHash (packTxReferences eb) () (\w _off _txh _sz _loc -> w)
  setTxLocations (lfcTxCache ctx) ebHash (zip [0 ..] (map hashLeiosTx txs))
 where
  own = MkLeiosTx (BS8.pack "a tx of its own")
  txs = own : held
  eb = endorserBlockFromTxBytes (leiosTxBytes own :| map leiosTxBytes held)
  ebHash = endorserBlockHash eb
  point = MkLeiosPoint (SlotNo 3) ebHash
