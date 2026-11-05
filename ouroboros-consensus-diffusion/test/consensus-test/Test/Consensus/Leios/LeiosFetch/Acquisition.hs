{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.Leios.LeiosFetch.Acquisition (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Codec.CBOR.Read as CBOR
import Control.Concurrent.Class.MonadSTM.Strict
  ( STM
  , StrictTVar
  , atomically
  , check
  , modifyTVar
  , newTVarIO
  , readTVarIO
  )
import Control.Exception (bracket)
import Control.Monad.Class.MonadAsync (concurrently)
import Control.Monad.Class.MonadTest (MonadTest (exploreRaces))
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.Monad.IOSim (IOSim, exploreSimTrace, traceResult)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import Data.Foldable (toList)
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Vector.Strict as V
import Data.Word (Word32)
import Network.TypedProtocol.Codec (AnnotatedCodec, CodecF (..))
import Ouroboros.Consensus.Leios.Types
  ( ElectionId (..)
  , EndorserBlock
  , LeiosPoint (..)
  , LeiosTx (..)
  , decodeEndorserBlock
  , decodeLeiosPoint
  , decodeLeiosTx
  , encodeEndorserBlock
  , encodeLeiosPoint
  , encodeLeiosTx
  , endorserBlockFromTxBytes
  , endorserBlockHash
  , endorserBlockReferences
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Acquisition
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Handoff as H
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Type as Wire
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Consensus.Storage.LeiosDB
  ( LeiosDbHandle (..)
  , LeiosDbWriter (..)
  , Promise (..)
  , TxWriteReceipt (..)
  , newLeiosDBInMemory
  , withWriter
  )
import System.Random (mkStdGen)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck
  ( chooseInt
  , counterexample
  , forAll
  , once
  , property
  , testProperty
  , vectorOf
  )

tests :: TestTree
tests =
  testGroup
    "atomic acquisition"
    [ testCase "target crossing uses whole TXs and exact actual bytes" testOverhang
    , testCase "entry, payload and encoded caps stop before a whole TX" testHardLimits
    , testCase "an indivisible TX exceeding a hard cap fails without skipping it" testOversized
    , testCase "default checkpoint is independent of publication geometry" testDefaultTarget
    , testCase "sparse eligibility spans offsets and excludes local/pending data" testSparse
    , testCase "unknown body, invalid selections and invalid limits are rejected" testInvalid
    , testCase "independent wire encoding agrees at list and byte-string boundaries" testEncoding
    , testProperty "checkpoint partition covers each eligible TX once, in order" $
        forAll (chooseInt (1, 120)) $ \count ->
          forAll (vectorOf count (chooseInt (1, 1000))) $ \sizes ->
            let snap = modelSnapshot sizes
                chunks = partitionMissing (limits 2000 100000 1000 110000) snap
                offsets = concatMap offsetsOf chunks
                bytes = map payloadOf chunks
             in counterexample (show (offsets, bytes)) $
                  offsets == [0 .. count - 1]
                    && all (>= 2000) (init bytes)
                    && all (< 3000) bytes
    , testCase "verified body metadata enables checkpoints before body durability" testBodyReadiness
    , testCase "full-closure offer and current focus gate ordinary assignment" testEligibility
    , testCase "concurrent ordinary assignment never owns the same TX twice" testConcurrent
    , testProperty "IOSimPOR explores assignment and obsolete-release races" $
        once $
          exploreSimTrace id assignmentRaces $ \_ trace -> case traceResult False trace of
            Left failure -> counterexample (show failure) False
            Right result -> property result
    , testCase "failed handoff admission leaves no ordinary ownership behind" testAdmissionRollback
    , testCase "release affects exact ownership and retains old immutable descriptors" testRelease
    , testCase "pending writes suppress new work without completing legacy jobs" testPendingProjection
    , testCase "an unconfirmed write re-enables projected legacy work" testUnconfirmed
    , testCase "legacy projection preserves jobs, priorities and peer accounting" testLegacyProjection
    , testCase "whole legacy descriptors retain original membership after projection" testLegacyDescriptor
    , testCase
        "new prefixes and legacy vectors share pending claims and closure completion"
        testMixedDelivery
    , testCase "durable legacy receipts reconcile acquisition without new admission" testLegacyReceipt
    , testCase "local possession and zero missing TXs do not request the closure" testLocalPossession
    , testCase "source removal cannot release a later registration's ownership" testPeerRemoval
    , testCase "pruning/refocus fences old assignments, projections and receipts" testGeneration
    ]

right :: Show e => Either e a -> a
right = either (error . show) id

limits :: Word32 -> Word32 -> Word32 -> Word32 -> CheckpointLimits
limits a b c d = right (mkCheckpointLimits a b c d)

small :: CheckpointLimits
small = limits 10 65536 32 65536

sampleTxs :: [Int] -> [LeiosTx]
sampleTxs = zipWith (\n size -> MkLeiosTx (BS.replicate size n)) [1 ..]

sampleBody :: [Int] -> EndorserBlock
sampleBody sizes = endorserBlockFromTxBytes (fmap leiosTxBytes (fromJust (NE.nonEmpty (sampleTxs sizes))))

pointOf :: EndorserBlock -> LeiosPoint
pointOf = MkLeiosPoint (SlotNo 5) . endorserBlockHash

modelSnapshot :: [Int] -> H.AcquisitionSnapshot
modelSnapshot sizes =
  H.AcquisitionSnapshot
    { H.snapshotGeneration = AcquisitionGeneration 0
    , H.snapshotReferences = Just refs
    , H.snapshotBodyDurable = True
    , H.snapshotHeld = IntSet.empty
    , H.snapshotPending = IntSet.empty
    , H.snapshotMissing = IntMap.keysSet refs
    }
 where
  refs = IntMap.fromDistinctAscList (zip [0 ..] (endorserBlockReferences (sampleBody sizes)))

build ::
  CheckpointLimits ->
  H.AcquisitionSnapshot ->
  IntSet.IntSet ->
  Either AcquisitionError (Maybe PreparedRequest)
build config snap = buildCheckpoint config (pointOf (sampleBody [1])) snap

selected :: CheckpointLimits -> H.AcquisitionSnapshot -> PreparedRequest
selected config snap = fromJust (right (build config snap (H.snapshotMissing snap)))

offsetsOf :: PreparedRequest -> [Int]
offsetsOf request = case preparedDescriptor request of
  BatchRequest batch -> map expectedOffset (NE.toList (batchExpected batch))
  BodyRequest{} -> error "expected transaction batch"

payloadOf :: PreparedRequest -> Word32
payloadOf request = fromIntegral $ sum [n | PayloadBytes n <- toList (preparedElements request)]

partitionMissing :: CheckpointLimits -> H.AcquisitionSnapshot -> [PreparedRequest]
partitionMissing config snap = case right (build config snap (H.snapshotMissing snap)) of
  Nothing -> []
  Just req ->
    req
      : partitionMissing
        config
        snap{H.snapshotMissing = H.snapshotMissing snap `IntSet.difference` IntSet.fromList (offsetsOf req)}

testOverhang :: Assertion
testOverhang = do
  let chunks = partitionMissing small (modelSnapshot [6, 6, 6])
  map offsetsOf chunks @?= [[0, 1], [2]]
  map payloadOf chunks @?= [12, 6]
  map preparedResponseBytes chunks @?= [EncodedBytes 21, EncodedBytes 12]

testHardLimits :: Assertion
testHardLimits = do
  let snap = modelSnapshot [6, 6, 6]
  map
    (offsetsOf . (`selected` snap))
    [limits 10 10 8 100, limits 10 100 1 100, limits 10 100 8 12]
    @?= replicate 3 [0]

testOversized :: Assertion
testOversized = do
  let snap = modelSnapshot [11, 1]
  build (limits 10 10 8 100) snap (H.snapshotMissing snap) @?= Left TransactionExceedsLimits
  build (limits 10 100 8 10) snap (H.snapshotMissing snap) @?= Left TransactionExceedsLimits

testDefaultTarget :: Assertion
testDefaultTarget = do
  let snap = modelSnapshot [16384, 2000, 4000, 100]
      req = selected defaultCheckpointLimits snap
  offsetsOf req @?= [0, 1, 2]
  payloadOf req @?= 22384

testSparse :: Assertion
testSparse = do
  let snap =
        (modelSnapshot [4, 4, 4, 4, 4, 4])
          { H.snapshotHeld = IntSet.singleton 1
          , H.snapshotPending = IntSet.singleton 2
          , H.snapshotMissing = IntSet.fromList [0, 3, 4, 5]
          }
      req = fromJust (right (build small snap (IntSet.fromList [0, 3, 5])))
  offsetsOf req @?= [0, 3, 5]
  build small snap (IntSet.singleton 1) @?= Left InvalidSelection
  build small snap (IntSet.singleton 2) @?= Left InvalidSelection

testInvalid :: Assertion
testInvalid = do
  let snap = modelSnapshot [1]
  build small snap{H.snapshotReferences = Nothing} IntSet.empty @?= Left BodyUnknown
  build small snap (IntSet.singleton 1) @?= Left InvalidSelection
  build small snap{H.snapshotMissing = IntSet.empty} IntSet.empty @?= Right Nothing
  map
    (\(a, b, c, d) -> mkCheckpointLimits a b c d)
    [(0, 10, 1, 100), (10, 5, 1, 100), (10, 10, 0, 100), (10, 500001, 1, 100), (10, 10, 1, 550001)]
    @?= replicate 5 (Left InvalidCheckpointLimits)

codec ::
  AnnotatedCodec
    (Wire.LeiosFetchAtomic LeiosPoint EndorserBlock LeiosTx)
    CBOR.DeserialiseFailure
    IO
    BL.ByteString
codec =
  Wire.codecLeiosFetchAtomic
    2
    encodeLeiosPoint
    decodeLeiosPoint
    encodeEndorserBlock
    decodeEndorserBlock
    encodeLeiosTx
    decodeLeiosTx

testEncoding :: Assertion
testEncoding =
  mapM_
    checkEncoding
    [replicate 23 1, replicate 24 24, replicate 256 1, [23, 24, 255, 256, 65535, 65536]]
 where
  checkEncoding sizes = do
    let snap = modelSnapshot sizes
        req = selected (limits 500000 500000 1000 550000) snap
        bytes = BL.length (encode codec (Wire.MsgLeiosBlockTxs (V.fromList (sampleTxs sizes))))
    preparedResponseBytes req @?= EncodedBytes (fromIntegral bytes)

-- Actual storage/decision fixtures keep global acquisition and per-peer wire
-- state separate; storage gates make pending/durable transitions observable.
data Fixture = Fixture
  { handoff :: !(H.StorageHandoff IO Int Int)
  , assignments :: !(OrdinaryAssignments IO Int)
  , generation :: !AcquisitionGeneration
  , point :: !LeiosPoint
  , body :: !EndorserBlock
  , txs :: ![LeiosTx]
  , writer :: !(LeiosDbWriter IO)
  , events :: !(StrictTVar IO [H.StorageEvent])
  }

withFixture :: [Int] -> [Int] -> (Fixture -> IO a) -> IO a
withFixture sizes held action = bracket newLeiosDBInMemory closeLeiosDbHandle $ \db -> withWriter db $ \writer -> do
  let body = sampleBody sizes; point = pointOf body; txs = sampleTxs sizes
  handoff <- right <$> H.newStorageHandoff (H.StorageLimits 131072 512 16 16 1000)
  assignments <- newOrdinaryAssignments
  generation <- right <$> atomically (H.beginAcquisition handoff point)
  events <- newTVarIO []
  let env = Fixture{handoff, assignments, generation, point, body, txs, writer, events}
  _ <- await =<< writeEbBody writer point body
  _ <- await =<< writeTxs writer point [(off, leiosTxBytes (txs !! off)) | off <- held]
  H.registerDurableBody handoff (notify env) point generation body (IntSet.fromList held)
    >>= (@?= Right ())
  action env

notify :: Fixture -> H.StorageEvent -> STM IO ()
notify env ev = modifyTVar (events env) (ev :)

currentSnapshot :: Fixture -> IO H.AcquisitionSnapshot
currentSnapshot env = fromJust <$> atomically (H.readAcquisition (handoff env) (point env))

newSession :: Fixture -> Int -> Int -> IO (H.StorageSession Int)
newSession env peer d =
  right
    <$> atomically
      (H.openStorageSession (handoff env) peer (MkLeiosPeerLifetimeId peer 0) (fromIntegral d) 65536)

focused :: Fixture -> LeiosOutstanding Int
focused = focusedPoint . point

focusedPoint :: LeiosPoint -> LeiosOutstanding Int
focusedPoint point =
  focusElectionIfUnfocused election (pointEbHash point) $
    recordReferencedPoint point SNothing (emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0))
 where
  election = ElectionId (pointSlotNo point) (KeyHash (UnsafeHash (SBS.pack (replicate 28 0))))

closureOffer :: PeerOffer
closureOffer = MkPeerOffer SNothing OfferedClosure

tryAssign ::
  Fixture -> H.StorageSession Int -> IO (Either AcquisitionError (Maybe (H.StorageRequest Int)))
tryAssign env session =
  atomically $
    assignCheckpoint
      (assignments env)
      (handoff env)
      small
      session
      (point env)
      (generation env)
      closureOffer
      (focused env)

assignedRequest :: Fixture -> H.StorageSession Int -> IO (H.StorageRequest Int)
assignedRequest env session = do
  req <- fromJust . right <$> tryAssign env session
  atomically (H.dispatchStorageRequest (handoff env) (H.storageRequestKey req)) >>= (@?= Right ())
  pure req

assigned :: Fixture -> IO IntSet.IntSet
assigned env = atomically $ readAssignedOffsets (assignments env) (point env) (generation env)

awaitSTM :: STM IO a -> IO a
awaitSTM stm = SI.timeout 5 (atomically stm) >>= maybe (assertFailure "storage transition timed out") pure

awaitHeld :: Fixture -> IntSet.IntSet -> IO ()
awaitHeld env held = awaitSTM $ do
  current <- H.readAcquisition (handoff env) (point env)
  check (maybe False ((== held) . H.snapshotHeld) current)

envLegacy :: LeiosFetchStaticEnv
envLegacy = (defaultLeiosFetchStaticEnv 60000000){maxJobBytesSize = 1000}

project :: Fixture -> H.AcquisitionSnapshot -> LeiosOutstanding Int -> LeiosOutstanding Int
project env = projectAcquisition envLegacy (point env) (generation env)

poolAt :: Fixture -> LeiosOutstanding peer -> Jobs.LeiosJobPool
poolAt env outstanding = case Map.lookup (point env) (ebState outstanding) of
  Just (MkEbState _ (BodyAcquired pool)) -> pool
  _ -> error "body not acquired"

legacyRequest :: Fixture -> H.AcquisitionSnapshot -> LeiosBlockTxsRequest
legacyRequest env snap =
  MkLeiosBlockTxsRequest (point env) $
    fromJust $
      NEIntMap.nonEmptyMap $
        IntMap.singleton 0 $
          fromJust $
            Jobs.lookupJob (Jobs.MkLeiosJobId 0) $
              poolAt env (project env snap (focused env))

testEligibility :: Assertion
testEligibility = withFixture [6, 6, 6] [] $ \env -> do
  a <- newSession env 1 8
  let attempt offer outstanding =
        atomically $
          assignCheckpoint
            (assignments env)
            (handoff env)
            small
            a
            (point env)
            (generation env)
            offer
            outstanding
  attempt (MkPeerOffer SNothing (OfferedBody 100)) (focused env) >>= (@?= Left ClosureNotOffered)
  attempt closureOffer (focused env){elFocus = Map.empty} >>= (@?= Left PointNotFocused)
  assigned env >>= (@?= IntSet.empty)
  _ <- assignedRequest env a
  pure ()

testConcurrent :: Assertion
testConcurrent = withFixture (replicate 8 6) [] $ \env -> do
  a <- newSession env 1 8
  b <- newSession env 2 8
  (ra, rb) <- concurrently (assignedRequest env a) (assignedRequest env b)
  let os = offsetsOf . H.storageRequestPrepared
  assertBool
    "ordinary batches are disjoint"
    (IntSet.disjoint (IntSet.fromList (os ra)) (IntSet.fromList (os rb)))
  assigned env >>= (@?= IntSet.fromList [0, 1, 2, 3])

testAdmissionRollback :: Assertion
testAdmissionRollback = withFixture [6, 6, 6] [] $ \env -> do
  a <- newSession env 1 1
  _ <- assignedRequest env a
  before <- assigned env
  failed <- tryAssign env a
  case failed of
    Left StorageAdmissionError{} -> pure ()
    _ -> assertFailure (show failed)
  assigned env >>= (@?= before)
  b <- newSession env 2 1
  rb <- assignedRequest env b
  offsetsOf (H.storageRequestPrepared rb) @?= [2]

testRelease :: Assertion
testRelease = withFixture [6, 6, 6] [] $ \env -> do
  a <- newSession env 1 8
  b <- newSession env 2 8
  ra <- assignedRequest env a
  let old = H.storageRequestPrepared ra
  atomically $ releaseCheckpoint (assignments env) (H.storageRequestKey ra)
  rb <- assignedRequest env b
  atomically $ releaseCheckpoint (assignments env) (H.storageRequestKey ra)
  assigned env >>= (@?= IntSet.fromList [0, 1])
  offsetsOf (H.storageRequestPrepared rb) @?= [0, 1]
  H.storageRequestPrepared ra @?= old
  H.acceptTransaction (handoff env) ra 0 (head (txs env)) >>= (@?= Right H.Useful)

testPendingProjection :: Assertion
testPendingProjection = withFixture [6, 6] [] $ \env -> do
  a <- newSession env 1 8
  ra <- assignedRequest env a
  snap0 <- currentSnapshot env
  let original = legacyRequest env snap0
      originalPool = poolAt env (project env snap0 (focused env))
  H.acceptLegacyBatch (handoff env) ra (txs env) >>= (@?= Right [H.Useful, H.Useful])
  pending <- currentSnapshot env
  let pool = poolAt env (project env pending (project env snap0 (focused env)))
  assertBool "pending is not durable closure" (not (Jobs.nullLeiosJobPool pool))
  assertBool
    "pending-only job has no scheduling candidate"
    (not (isJust (Jobs.pickLeastRequestedJobExcept (mkStdGen 0) IntSet.empty pool)))
  Jobs.lookupJob (Jobs.MkLeiosJobId 0) pool @?= Jobs.lookupJob (Jobs.MkLeiosJobId 0) originalPool
  assertBool
    "unpick does not reinsert a pending job"
    ( not
        ( isJust
            ( Jobs.pickLeastRequestedJobExcept
                (mkStdGen 0)
                IntSet.empty
                (Jobs.unpickJob (Jobs.MkLeiosJobId 0) pool)
            )
        )
    )
  prepareLegacyBatch pending original @?= prepareLegacyBatch snap0 original
  atomically $ releaseCheckpoint (assignments env) (H.storageRequestKey ra)
  b <- newSession env 2 8
  tryAssign env b >>= (@?= Right Nothing)

testUnconfirmed :: Assertion
testUnconfirmed = withFixture [6, 6] [] $ \env -> do
  a <- newSession env 1 8
  ra <- assignedRequest env a
  snap0 <- currentSnapshot env
  H.acceptLegacyBatch (handoff env) ra (txs env) >>= (@?= Right [H.Useful, H.Useful])
  pending <- currentSnapshot env
  let waiting = project env pending (project env snap0 (focused env))
      storage = (writer env){writeTxs = \_ xs -> pure (Promise (pure (receipt xs)))}
      receipt xs = TxWriteReceipt IntSet.empty (IntSet.fromList (map fst xs)) []
  -- The actual worker reconciles an explicit unconfirmed receipt.
  H.withStorageHandoff (handoff env) storage (notify env) $ \_ -> awaitSTM $ do
    snap <- H.readAcquisition (handoff env) (point env)
    check (maybe False ((== IntSet.fromList [0, 1]) . H.snapshotMissing) snap)
  missing <- currentSnapshot env
  assertBool
    "unconfirmed data is schedulable again"
    ( isJust
        ( Jobs.pickLeastRequestedJobExcept
            (mkStdGen 0)
            IntSet.empty
            (poolAt env (project env missing waiting))
        )
    )
  assertBool
    "still incomplete"
    (not (Jobs.nullLeiosJobPool (poolAt env (project env missing waiting))))

testLegacyProjection :: Assertion
testLegacyProjection = withFixture [6, 6, 6] [] $ \env -> do
  snap0 <- currentSnapshot env
  let base = project env snap0 (focused env)
      offers = Map.singleton (1 :: Int) (Map.singleton (point env) closureOffer)
      (inflight, requests, _) =
        leiosFetchLogicIteration
          envLegacy
          (const (Right (MkLeiosEbBounds 600000 500000)))
          Nothing
          offers
          Map.empty
          base
      received = snap0{H.snapshotHeld = IntSet.fromList [0, 1, 2], H.snapshotMissing = IntSet.empty}
      completed = project env received inflight
  assertBool "legacy still assigns its original requests" (not (Map.null requests))
  assertBool "only durable offsets complete the pool" (Jobs.nullLeiosJobPool (poolAt env completed))
  requestedBytesSizePerPeer completed @?= requestedBytesSizePerPeer inflight
  requestedJobsPerPeer completed @?= requestedJobsPerPeer inflight
  elFocus completed @?= elFocus inflight

testLegacyDescriptor :: Assertion
testLegacyDescriptor = withFixture [6, 6, 6] [] $ \env -> do
  before <- currentSnapshot env
  let original = legacyRequest env before
      partial = before{H.snapshotHeld = IntSet.singleton 0, H.snapshotMissing = IntSet.fromList [1, 2]}
      prepared = right (prepareLegacyBatch partial original)
  offsetsOf prepared @?= [0, 1, 2]
  prepareLegacyBatch partial original @?= prepareLegacyBatch before original
  let bad = partial{H.snapshotReferences = H.snapshotReferences (modelSnapshot [7, 6, 6])}
  prepareLegacyBatch bad original @?= Left InvalidLegacyCommitment

testMixedDelivery :: Assertion
testMixedDelivery = withFixture [6, 6, 6] [] $ \env -> do
  a <- newSession env 1 8
  b <- newSession env 2 8
  ra <- assignedRequest env a
  before <- currentSnapshot env
  let original = legacyRequest env before
      prepared = right (prepareLegacyBatch before original)
  rb <- right <$> atomically (H.admitStorageRequest (handoff env) b prepared)
  atomically (H.dispatchStorageRequest (handoff env) (H.storageRequestKey rb)) >>= (@?= Right ())
  H.acceptTransaction (handoff env) ra 0 (head (txs env)) >>= (@?= Right H.Useful)
  H.acceptLegacyBatch (handoff env) rb [head (txs env), head (txs env), last (txs env)]
    >>= (@?= Left H.InvalidTransaction)
  H.acceptLegacyBatch (handoff env) rb (txs env)
    >>= (@?= Right [H.PendingDuplicate, H.Useful, H.Useful])
  H.withStorageHandoff (handoff env) (writer env) (notify env) $ \_ -> awaitHeld env (IntSet.fromList [0, 1, 2])
  after <- currentSnapshot env
  assertBool
    "legacy projection sees new-prefix/legacy completion"
    (Jobs.nullLeiosJobPool (poolAt env (project env after (project env before (focused env)))))
  evs <- readTVarIO (events env)
  length [() | H.ClosureDurable{} <- evs] @?= 1

testLegacyReceipt :: Assertion
testLegacyReceipt = withFixture [6, 6] [] $ \env -> do
  a <- newSession env 1 8
  ra <- assignedRequest env a
  H.acceptTransaction (handoff env) ra 0 (head (txs env)) >>= (@?= Right H.Useful)
  _ <- await =<< writeTxs (writer env) (point env) [(0, leiosTxBytes (head (txs env)))]
  atomically
    ( H.registerDurableTransactions
        (handoff env)
        (notify env)
        (point env)
        (generation env)
        (IntSet.singleton 0)
    )
    >>= (@?= Right ())
  snap <- currentSnapshot env
  H.snapshotHeld snap @?= IntSet.singleton 0
  atomically
    ( H.registerDurableTransactions
        (handoff env)
        (notify env)
        (point env)
        (generation env)
        (IntSet.singleton 10)
    )
    >>= (@?= Left H.InvalidDescriptor)
  -- No new request was admitted or settled by that receipt bridge.
  assigned env >>= (@?= IntSet.fromList [0, 1])
  H.withStorageHandoff (handoff env) (writer env) (notify env) $ \_ -> awaitSTM $ do
    usage <- H.readStorageSnapshot (handoff env)
    check (H.snapshotClaims usage == 0)

testLocalPossession :: Assertion
testLocalPossession = withFixture [6, 6, 6] [0, 2] $ \env -> do
  a <- newSession env 1 8
  ra <- assignedRequest env a
  offsetsOf (H.storageRequestPrepared ra) @?= [1]
  _ <- await =<< writeTxs (writer env) (point env) [(1, leiosTxBytes (txs env !! 1))]
  atomically
    ( H.registerDurableTransactions
        (handoff env)
        (notify env)
        (point env)
        (generation env)
        (IntSet.singleton 1)
    )
    >>= (@?= Right ())
  b <- newSession env 2 8
  tryAssign env b >>= (@?= Right Nothing)

testPeerRemoval :: Assertion
testPeerRemoval = withFixture [6, 6, 6, 6] [] $ \env -> do
  a <- newSession env 1 8
  ra <- assignedRequest env a
  atomically $ do
    H.closeStorageSession (handoff env) a
    releasePeerAssignments (assignments env) (MkLeiosPeerLifetimeId 1 0)
  b <- right <$> atomically (H.openStorageSession (handoff env) 1 (MkLeiosPeerLifetimeId 1 1) 8 65536)
  rb <- assignedRequest env b
  atomically $ releasePeerAssignments (assignments env) (MkLeiosPeerLifetimeId 1 0)
  assigned env >>= (@?= IntSet.fromList [0, 1])
  assertBool "different full identities" (H.storageRequestKey ra /= H.storageRequestKey rb)

testGeneration :: Assertion
testGeneration = withFixture [6, 6, 6] [] $ \env -> do
  a <- newSession env 1 8
  _ <- assignedRequest env a
  old <- currentSnapshot env
  atomically $ do
    H.retireAcquisition (handoff env) (point env) (generation env)
    releaseAcquisitionAssignments (assignments env) (point env) (generation env)
  fresh <- right <$> atomically (H.beginAcquisition (handoff env) (point env))
  let env' = env{generation = fresh}
  H.registerDurableBody (handoff env) (notify env) (point env) fresh (body env) IntSet.empty
    >>= (@?= Right ())
  b <- newSession env 2 8
  _ <- assignedRequest env' b
  atomically $ releaseAcquisitionAssignments (assignments env) (point env) (generation env)
  assigned env' >>= (@?= IntSet.fromList [0, 1])
  atomically
    ( H.registerDurableTransactions
        (handoff env)
        (notify env)
        (point env)
        (generation env)
        (IntSet.singleton 0)
    )
    >>= (@?= Left H.StaleAcquisition)
  let base = focused env'
      untouched = projectAcquisition envLegacy (point env) fresh old base
      (_, pruned) = pruneOutstandingToImmTip (SlotNo 6) base
  ebState untouched @?= ebState base
  ebState (project env old pruned) @?= ebState pruned

-- Borrow the concurrent schedule exploration used by typed-protocols' lookahead
-- tests. This fixture seeds a model's durable metadata without doing filesystem
-- I/O, so every relevant STM ordering can be explored by IOSimPOR.
assignmentRaces :: IOSim s Bool
assignmentRaces = do
  exploreRaces
  handoff <- right <$> H.newStorageHandoff (H.StorageLimits 4096 32 4 4 10)
  let body = sampleBody [6, 6, 6, 6]
      point = pointOf body
      peer n = MkLeiosPeerLifetimeId (n :: Int) 0
  gen <- right <$> atomically (H.beginAcquisition handoff point)
  _ <- H.registerDurableBody handoff (const (pure ())) point gen body IntSet.empty
  assignments <- newOrdinaryAssignments
  a <- right <$> atomically (H.openStorageSession handoff (1 :: Int) (peer 1) 8 4096)
  b <- right <$> atomically (H.openStorageSession handoff (2 :: Int) (peer 2) 8 4096)
  let assign session =
        fromJust . right
          <$> atomically
            (assignCheckpoint assignments handoff small session point gen closureOffer (focusedPoint point))
      offsets = IntSet.fromList . offsetsOf . H.storageRequestPrepared
  (ra, rb) <- concurrently (assign a) (assign b)
  atomically $ releaseCheckpoint assignments (H.storageRequestKey ra)
  (replacement, ()) <-
    concurrently (assign b) (atomically (releaseCheckpoint assignments (H.storageRequestKey ra)))
  owned <- atomically (readAssignedOffsets assignments point gen)
  pure $
    IntSet.disjoint (offsets ra) (offsets rb)
      && offsets replacement == offsets ra
      && owned == IntSet.fromList [0, 1, 2, 3]

-- New-version requests may use verified metadata while its body write is pending;
-- legacy publication/assignment still waits for the durable body milestone.
testBodyReadiness :: Assertion
testBodyReadiness = do
  let body = sampleBody [6, 6]
      point = pointOf body
  handoff <- right <$> H.newStorageHandoff (H.StorageLimits 131072 512 4 4 10)
  gen <- right <$> atomically (H.beginAcquisition handoff point)
  assignments <- newOrdinaryAssignments
  session <-
    right
      <$> atomically (H.openStorageSession handoff (1 :: Int) (MkLeiosPeerLifetimeId (1 :: Int) 0) 8 65536)
  let attempt =
        atomically $
          assignCheckpoint assignments handoff small session point gen closureOffer (focusedPoint point)
  attempt >>= (@?= Left BodyUnknown)
  let prepared = right $ prepareBody (BodyDescriptor point gen 1 (MkLeiosEbBounds 4096 65536)) (EncodedBytes 4099)
  req <- right <$> atomically (H.admitStorageRequest handoff session prepared)
  atomically (H.dispatchStorageRequest handoff (H.storageRequestKey req)) >>= (@?= Right ())
  accepted <- H.acceptBody handoff req body
  assertBool
    "whole body is accepted despite an understated offer"
    (either (const False) H.bodySizeMisstated accepted)
  snapshot <- fromJust <$> atomically (H.readAcquisition handoff point)
  H.snapshotBodyDurable snapshot @?= False
  request <- fromJust . right <$> attempt
  offsetsOf (H.storageRequestPrepared request) @?= [0, 1]
  let projected = projectAcquisition envLegacy point gen snapshot (focusedPoint point)
  case Map.lookup point (ebState projected) of
    Just (MkEbState _ NoBody) -> pure ()
    _ -> assertFailure "pending body must not become legacy BodyAcquired"
