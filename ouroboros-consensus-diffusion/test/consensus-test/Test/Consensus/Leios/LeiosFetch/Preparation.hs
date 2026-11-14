{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Whole-EB selection, queue and failure interactions use the real production
-- workers. Gates live in the supplied source/cache/writer, never in production.
module Test.Consensus.Leios.LeiosFetch.Preparation (tests) where

import qualified Control.Concurrent.Class.MonadSTM.Strict as S
import Control.Monad (unless, void)
import Control.Monad.Class.MonadTest (exploreRaces)
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.Monad.IOSim (IOSim, exploreSimTrace, traceResult)
import Control.Tracer (mkTracer)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Ouroboros.Consensus.Leios.TxCache as Cache
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.LocalReuse
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.ReceiptTasks as Receipts
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Runtime as R
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace
import Ouroboros.Consensus.Storage.LeiosDB.API
import Ouroboros.Consensus.Util.IOLike
import qualified Test.Consensus.Leios.LeiosFetch.Replies as Fixtures
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Util.Orphans.IOLike ()

bounded :: IO () -> IO ()
bounded action = SI.timeout 5 action >>= maybe (assertFailure "whole-EB scenario stranded") pure

ownership :: IOLike m => LeiosFetchClientContext m peer -> LeiosPoint -> ReplyOwnership m
ownership ctx point =
  ReplyOwnership
    { claimUsefulReply = pure True
    , claimBodyReply = R.claimLocalBody (lfcRuntime ctx) point
    , acceptedReply = const (pure (pure ()))
    , bodyWon = const (pure ())
    , settledReceipt = const (pure (pure ()))
    , abandonUnsubmitted = pure ()
    }

receive :: IOLike m => LeiosFetchClientContext m peer -> peer -> LeiosPoint -> EndorserBlock -> m ()
receive ctx peer point eb =
  processLeiosBlock
    (ownership ctx point)
    ctx
    peer
    (MkLeiosBlockRequest point (encodeEndorserBlockSize eb) maxBound)
    (serialiseEndorserBlock eb)

sourceOf :: MonadSTM m => [LeiosTx] -> LocalSource m LeiosTx
sourceOf txs =
  LocalSource
    { localLookup = const (pure (Map.fromList [(hashLeiosTx tx, tx) | tx <- txs], Just EvenProtVer))
    , localBytes = pure . Just
    }

-- Generated cases include exact hits, absent hashes, same-length wrong bytes,
-- wrong advertised sizes and unavailable projections. The oracle comes from the
-- generated case tag, rather than calling the selection implementation again.
propSelection :: [Word] -> Property
propSelection tags = checkCoverage $
  cover 60 (any ((== 0) . (`mod` 5)) tags) "exact hit" $
    cover 60 (any ((== 1) . (`mod` 5)) tags) "missing hash" $
      cover 60 (any ((== 2) . (`mod` 5)) tags) "wrong hash" $
        cover 60 (any ((== 3) . (`mod` 5)) tags) "wrong size" $
          cover 60 (any ((== 4) . (`mod` 5)) tags) "no projection" $
            ioProperty $ do
              let entries =
                    [ ( off
                      , tag `mod` 5
                      , MkLeiosTx (BS.pack [fromIntegral off, fromIntegral (off `div` 256), fromIntegral tag])
                      )
                    | (off, tag) <- zip [0 ..] (take 200 tags)
                    ]
                  refs = [(off, (hashLeiosTx tx, if kind == 3 then 4 else 3)) | (off, kind, tx) <- entries]
                  table =
                    Map.fromList
                      [ ( hashLeiosTx tx
                        , if kind == 4
                            then Nothing
                            else Just (if kind == 2 then MkLeiosTx (BS.map (`xor` 128) (leiosTxBytes tx)) else tx)
                        )
                      | (_, kind, tx) <- entries
                      , kind /= 1
                      ]
                  expected = [(off, leiosTxBytes tx) | (off, kind, tx) <- entries, kind == 0]
              calls <- S.newTVarIO (0 :: Int)
              let source =
                    LocalSource
                      { localLookup = const $ S.modifyTVar calls (+ 1) >> pure (table, Just OddProtVer)
                      , localBytes = pure
                      }
              PreparedTxs bytes validated <- prepareBodyTxs (newLocalReuse source) (pure False) refs
              count <- S.readTVarIO calls
              pure $
                counterexample (show (entries, bytes)) $
                  sortOn fst bytes == expected
                    && sortOn fst validated
                      == sortOn fst [(hashLeiosTx (MkLeiosTx raw), OddProtVer) | (_, raw) <- expected]
                    && count == 1

testCapturedSource :: Assertion
testCapturedSource = bounded $ do
  let (_, eb, txs) = Fixtures.fixture
      refs = zip [0 ..] (endorserBlockReferences eb)
      table = Map.fromList [(hashLeiosTx tx, tx) | tx <- txs]
  live <- S.newTVarIO table
  version <- S.newTVarIO EvenProtVer
  let source =
        LocalSource
          { localLookup = const $ (,) <$> S.readTVar live <*> (Just <$> S.readTVar version)
          , localBytes = \tx -> do
              atomically $ S.writeTVar live Map.empty >> S.writeTVar version OddProtVer
              pure (Just tx)
          }
  PreparedTxs bytes validated <- prepareBodyTxs (newLocalReuse source) (pure False) refs
  sortOn fst bytes @?= zip [0 ..] (leiosTxBytes <$> txs)
  sortOn fst validated @?= sortOn fst [(hashLeiosTx tx, EvenProtVer) | tx <- txs]

-- A queued cache candidate has not proved useful possession. Its lookup miss
-- must wake the waiting valid network delivery, rather than discard that body.
testCacheCandidate :: Bool -> Assertion
testCacheCandidate invalid = bounded $ do
  (base, notes) <- Fixtures.newContext
  entered <- S.newEmptyTMVarIO
  release <- S.newEmptyTMVarIO
  let (point, eb, _) = Fixtures.fixture
      cache = lfcTxCache base
      wrong = endorserBlockFromTxBytes (BS.singleton 99 :| [])
      ctx =
        base
          { lfcTxCache =
              cache
                { Cache.lookupBody = \_ -> do
                    atomically $ S.putTMVar entered ()
                    atomically $ S.readTMVar release
                    pure (if invalid then Just (Cache.packEndorserBlock wrong) else Nothing)
                }
          }
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ withBodyWorkers ctx $ do
    acquireCachedBody ctx point maxBound
    atomically $ S.readTMVar entered
    withAsync (receive ctx 0 point eb) $ \handler -> do
      SI.timeout 0.05 (wait handler) >>= (@?= Nothing)
      atomically (Map.null . R.pending <$> readTVar (R.runtimeFacts (lfcRuntime ctx))) >>= (@?= True)
      atomically $ S.putTMVar release ()
      wait handler
    void $ Fixtures.awaitNotes notes 1
  -- No local closure is fabricated by an unverified cached body.
  out <- readMVar (lfcOutstanding ctx)
  case Map.lookup point (ebState out) of
    Just (MkEbState _ (BodyAcquired jobs)) -> Jobs.nullJobPool jobs @?= False
    _ -> assertFailure "valid network body was lost"

-- One pool entry owns the whole operation while a non-strict writer is pending.
-- A bad second projection must fail before any writer call, even after a good
-- first projection has already produced reusable bytes.
testVerifyBeforeWriter :: Assertion
testVerifyBeforeWriter = bounded $ do
  (base, _) <- Fixtures.newContext
  called <- S.newTVarIO False
  let (point, eb, txs) = Fixtures.fixture
      source =
        (sourceOf txs)
          { localBytes = \tx -> pure (Just (if tx == last txs then MkLeiosTx (error "invalid projection") else tx))
          }
      original = lfcWriter base
      ctx =
        base
          { lfcLocalReuse = newLocalReuse source
          , lfcWriter =
              original{writeEbBody = \_ _ _ _ -> atomically (S.writeTVar called True) >> pure (Promise (pure (BodyPresent Set.empty)))}
          }
  result <-
    try @_ @SomeException $
      Receipts.withReceiptTasks (lfcReceiptTasks ctx) $
        withBodyWorkers ctx $
          receive ctx 0 point eb >> atomically retry
  case result of { Left _ -> pure (); Right () -> assertFailure "projection failure disappeared" }
  S.readTVarIO called >>= (@?= False)
  atomically (Map.null . R.pending <$> readTVar (R.runtimeFacts (lfcRuntime ctx))) >>= (@?= True)

-- Competing source failures share one supervised worker group; both operations
-- must finish cleanup and no second linked exception may arrive after teardown.
pairFailure :: IOSim s Bool
pairFailure = do
  (base, _) <- Fixtures.newContext
  entered <- S.newTVarIO (0 :: Int)
  joined <- S.newTVarIO (0 :: Int)
  gate <- S.newEmptyTMVarIO
  let (first, eb, txs) = Fixtures.fixture
      second = first{pointSlotNo = 6}
      source =
        (sourceOf txs)
          { localBytes = \_ ->
              ( do
                  exploreRaces
                  atomically $ S.modifyTVar entered (+ 1)
                  atomically $ S.readTMVar gate
                  throwIO (userError "preparation failed")
              )
                `finally` atomically (S.modifyTVar joined (+ 1))
          }
      ctx = base{lfcLocalReuse = newLocalReuse source}
  modifyMVar_ (lfcOutstanding ctx) $ \out -> pure out{ebState = Map.insert second (MkEbState SNothing NoBody) (ebState out)}
  result <- try @_ @SomeException $ Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ withBodyWorkers ctx $ do
    receive ctx 0 first eb
    receive ctx 1 second eb
    atomically $ S.readTVar entered >>= check . (== 2)
    atomically $ S.putTMVar gate ()
    atomically retry
  threadDelay 0.001
  stopped <- S.readTVarIO joined
  pending <- atomically $ R.pending <$> readTVar (R.runtimeFacts (lfcRuntime ctx))
  pure (either (const True) (const False) result && stopped == 2 && Map.null pending)

propPairFailure :: Property
propPairFailure = exploreSimTrace id (exploreRaces >> pairFailure) $ \_ trace ->
  case traceResult False trace of
    Left err -> counterexample (show err) False
    Right ok -> property ok

tests :: TestTree
tests =
  testGroup
    "whole-EB integration"
    [ testCase "understated offer preserves useful body before disconnect" testUnderdeclared
    , testCase "verified mempool hit replaces its DB fill" (testFillPriority False False)
    , testCase "unverified hit preserves a usable DB fill" (testFillPriority False True)
    , testCase "stale fill is a miss, distinct from chosen-source counts" (testFillPriority True False)
    , testProperty "exact selected bytes and parity under generated source cases" propSelection
    , testCase "selected TXs and parity survive later source changes" testCapturedSource
    , testCase "cache miss does not consume a valid network body" (testCacheCandidate False)
    , testCase "invalid cache candidate does not consume a valid network body" (testCacheCandidate True)
    , testCase "all projections are verified before writer invocation" testVerifyBeforeWriter
    , testProperty "simultaneous preparation failures quiesce across interleavings" propPairFailure
    ]

-- Verify mempool hits before dropping their DB fill alternatives. With a stale
-- DB source, chosen-source counts must remain distinct from durable coverage.
testFillPriority :: Bool -> Bool -> Assertion
testFillPriority stale badProjection = bounded $ do
  (base, notes) <- Fixtures.newContext
  choices <- S.newTVarIO []
  writes <- S.newTVarIO []
  let (point, eb, txs) = Fixtures.fixture
      prior = point{pointSlotNo = 4}
      original = lfcWriter base
      oldCache = lfcTxCache base
      source =
        (sourceOf (take 1 txs))
          { localBytes = \tx -> pure (Just (if badProjection then MkLeiosTx (BS.singleton 0) else tx))
          }
      cache =
        oldCache
          { Cache.insertBody = \h packed nil step ->
              Cache.insertBody
                oldCache
                h
                packed
                nil
                (\acc off txh size _ -> step acc off txh size (Just (MkTxLocation prior off)))
          }
      writer =
        original
          { writeEbBody = \p body fills given -> do
              atomically $ S.modifyTVar writes ((Map.keys fills, Map.keys given) :)
              writeEbBody original p body fills given
          }
      ctx =
        base
          { lfcWriter = writer
          , lfcTxCache = cache
          , lfcLocalReuse = newLocalReuse source
          , lfcTracer = mkTracer $ \case
              TraceLeiosFetchClientBlockTxSources _ selected _ ->
                atomically $
                  S.modifyTVar choices ((btsLocalFills selected, btsMempool selected, btsRemaining selected) :)
              _ -> pure ()
          }
      complete = not stale
  unless stale $ void $ await =<< writeEbBody original prior eb Map.empty (Map.fromList (zip [0 ..] (leiosTxBytes <$> txs)))
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ withBodyWorkers ctx $ do
    receive ctx 0 point eb
    void $ Fixtures.awaitNotes notes (if complete then 2 else 1)
  S.readTVarIO writes >>= (@?= [if badProjection then ([1, 0], []) else ([1], [0])])
  S.readTVarIO choices >>= (@?= [if badProjection then (2, 0, 0) else (1, 1, 0)])
  out <- readMVar (lfcOutstanding ctx)
  case Map.lookup point (ebState out) of
    Just (MkEbState _ (BodyAcquired jobs)) -> Jobs.nullJobPool jobs @?= complete
    _ -> assertFailure "body acquisition was lost"

-- A false small offer is a connection fault, but useful complete bytes transfer
-- first. Disconnect must not erase the preparation that outlives that handler.
testUnderdeclared :: Assertion
testUnderdeclared = bounded $ do
  (base, notes) <- Fixtures.newContext
  let (point, eb, txs) = Fixtures.fixture
      actual = encodeEndorserBlockSize eb
      ctx = base{lfcLocalReuse = newLocalReuse (sourceOf txs)}
  Receipts.withReceiptTasks (lfcReceiptTasks ctx) $ withBodyWorkers ctx $ do
    result <-
      try $
        processLeiosBlock (ownership ctx point) ctx 0 (MkLeiosBlockRequest point (actual - 1) maxBound) (serialiseEndorserBlock eb)
    case result of
      Left (ExnLeiosBlockWrongSize p offered arrived) -> (p, offered, arrived) @?= (point, actual - 1, actual)
      Left exn -> assertFailure (show (exn :: ExnLeiosFetchInvalidReply))
      Right () -> assertFailure "understated offer was accepted"
    atomically $ R.closePeer (lfcRuntime ctx) 0
    void $ Fixtures.awaitNotes notes 2
