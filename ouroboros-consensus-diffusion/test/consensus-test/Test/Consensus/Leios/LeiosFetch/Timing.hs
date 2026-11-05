module Test.Consensus.Leios.LeiosFetch.Timing (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (foldM)
import qualified Data.ByteString.Short as SBS
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ratio ((%))
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import Data.Word (Word32, Word64)
import Ouroboros.Consensus.Leios.Types (LeiosEbBounds (..), LeiosPoint (..), mkEndorserBlock)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Deadline
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Timing
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity
import Ouroboros.Consensus.Util.IOLike (Time (Time), addTime)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Property
  , chooseInt
  , counterexample
  , elements
  , forAll
  , property
  , shuffle
  , testProperty
  , (.&&.)
  )

tests :: TestTree
tests =
  testGroup
    "atomic timing"
    [ testCase "KeepAlive freshness, fallback and cap use completed measurements" testRtt
    , testCase "chain-following calibration uses trusted byte totals and safety factor" testCalibration
    , testCase "startup rounds count cumulative full initial windows" testRounds
    , testCase "20kB pilot and higher-rate examples retain the general startup rule" testService
    , testCase "early and late appends absorb only remaining transit" testAnchors
    , testCase
        "late appends and ACK-only backlog are fresh without inferring TCP state"
        testClassification
    , testCase "body timer input is separate from its trusted reservation" testBody
    , testCase "actual whole-TX overhang enlarges service allowance" testOverhang
    , testCase "hedge ceiling preserves a large body's base and clips huge factors" testHedge
    , testCase "invalid settings and fatal-guard incompatibility are rejected" testConfig
    , testCase "queued requests have no clock until predecessor wire retirement" testFIFO
    , testCase "ACK guard starts at head and does not charge data failure" testAck
    , testCase "logical completion retains the head clock and permits queued rescue" testWithdrawal
    , testCase "previously handled failure is not charged again" testFailureOnce
    , testCase "complete on the deadline wins; late completion records one expiry" testTie
    , testCase "pre-deadline input awaits validation without moving its deadline" testValidation
    , testCase "partial input does not complete or renew a checkpoint" testPartial
    , testCase "post-deadline input cannot defer expiry" testLateInput
    , testCase "send notification can follow a fast reply without reviving it" testFastReply
    , testCase "late coordinator handling retains predecessor receipt timestamps" testDelayedHandling
    , testCase "progress, new offers and appends do not renew an active head" testNoRenewal
    , testCase "stale timer/validation tokens cannot affect a new head or session" testStale
    , testCase "invalid event times and lost observation boundaries are explicit errors" testEventErrors
    , testProperty "generated sizes preserve transfer floors, startup and hedge bounds" propArithmetic
    , testProperty "notification delivery order does not change chronological outcomes" propEventOrder
    , testProperty "fired timers do not repeat penalties or reschedule past deadlines" propOneShot
    ]

type State = TimingState Int
type Key = RequestKey Int

right :: Show e => Either e a -> a
right = either (error . show) id

at :: Rational -> Time
at = Time . fromRational

up :: Rational -> DiffTime
up x = picosecondsToDiffTime (ceiling (x * 1000000000000))

config :: Rational -> TimingConfig
config rate = right (mkTimingConfig rate 14600 0.5 2 60 (PayloadBytes 1048576))

sample :: Maybe RttObservation
sample = Just (RttObservation 0.1 (at 0))

point :: Word -> LeiosPoint
point n = MkLeiosPoint (SlotNo (fromIntegral n)) (UnsafeHash (SBS.pack (replicate 32 (fromIntegral n))))

body :: Word32 -> PreparedRequest
body n =
  right
    ( prepareBody
        (BodyDescriptor (point 0) (AcquisitionGeneration 0) n (MkLeiosEbBounds 600000 1048576))
        (EncodedBytes 600016)
    )

batch :: [Word32] -> PreparedRequest
batch sizes =
  right $
    prepareBatch
      (point 1)
      (AcquisitionGeneration 0)
      ( mkEndorserBlock
          [(UnsafeHash (SBS.pack (replicate 32 (fromIntegral i))), n) | (i, n) <- zip [0 :: Int ..] sizes]
      )
      (0 :| [1 .. length sizes - 1])
      (EncodedBytes (sum (map fromIntegral sizes) + 16))

snapshot :: Rational -> Rational -> PreparedRequest -> TimingSnapshot
snapshot rate factor = right . snapshotTiming (config rate) (at 0) sample factor

standard :: TimingKind
standard = TrackData (snapshot 1500000 1 (batch [20000]))

ack :: TimingKind
ack = TrackAck (conservativeRtt (at 0) sample)

key :: Word64 -> Key
key n = RequestKey (MkLeiosPeerLifetimeId 7 0) (FetchSessionToken 0) (WireOrdinal n)

initial :: State
initial = newTimingState (MkLeiosPeerLifetimeId 7 0) (FetchSessionToken 0)

registered :: Word64 -> TimingKind -> State -> State
registered n kind = right . registerTiming (key n) kind

sent :: Word64 -> Rational -> State -> State
sent n t = right . recordSend (key n) (at t)

terminal :: Word64 -> Rational -> State -> State
terminal n t = right . recordTerminal (key n) (at t)

poll :: Rational -> State -> ([Expiry Int], State)
poll t = right . reconcileTiming (at t)

noExpiry :: Rational -> State -> State
noExpiry t s = case poll t s of
  ([], next) -> next
  (events, _) -> error ("unexpected expiries: " ++ show events)

oneExpiry :: Rational -> State -> (Expiry Int, State)
oneExpiry t s = case poll t s of
  ([event], next) -> (event, next)
  (events, _) -> error ("expected one expiry: " ++ show events)

headAt :: State -> Time
headAt = maybe (error "no active head") snd . activeDeadline

token :: State -> TimerToken Int
token = maybe (error "no active timer") fst . activeDeadline

testRtt :: Assertion
testRtt = do
  let r raw completed = getRttAllowance (conservativeRtt (at 40) (Just (RttObservation raw (at completed))))
  r 0.1 10 @?= 0.15
  r 0.1 (10 - 1 % 1000000000000) @?= 0.5
  r 0.1 41 @?= 0.5
  r 0 40 @?= 0.5
  r (-0.1) 40 @?= 0.5
  r 1 40 @?= 0.5
  getRttAllowance (conservativeRtt (at 40) Nothing) @?= 0.5

testCalibration :: Assertion
testCalibration = do
  chainFollowingRate (PayloadBytes 90000) (PayloadBytes 262144) (PayloadBytes 6624) 20 2
    @?= Right (1435072 % 5)
  chainFollowingRate (PayloadBytes 90000) (PayloadBytes 262144) (PayloadBytes 6624) 0 2
    @?= Left InvalidCalibration
  chainFollowingRate (PayloadBytes 90000) (PayloadBytes 262144) (PayloadBytes 6624) 20 0
    @?= Left InvalidCalibration

testRounds :: Assertion
testRounds = do
  mapM_
    (\(size, k) -> initialRounds 14600 size @?= Right k)
    [(0, 1), (14600, 1), (14601, 2), (43800, 2), (43801, 3), (219000, 4), (262800, 5)]
  initialRounds 0 20000 @?= Left InvalidInitialWindow
  initialRounds 1 (2 ^ (80 :: Int)) @?= Right 81

testService :: Assertion
testService = do
  let low = snapshot 300000 1 (batch [20000])
      high = snapshot 1500000 1 (batch [20000])
      fresh s = dataDeadline s (at 0) Nothing Nothing
      warm s = dataDeadline s (at (1 % 20)) (Just (at (1 % 5))) (Just (at (1 % 5)))
  transferAllowance low @?= up (8 % 15)
  coldAllowance low @?= 0.3
  deadlineExpiresAt (fresh low) @?= addTime (up (8 % 15)) (at (3 % 20))
  deadlineAllowance (fresh high) @?= 0.45
  deadlineExpiresAt (warm high) @?= at (7 % 20)
  deadlinePipeline (warm high) @?= Continuous

testAnchors :: Assertion
testAnchors = do
  let near100ms = Just (RttObservation (picosecondsToDiffTime 66666666667) (at 0))
      s = right (snapshotTiming (config 1500000) (at 0) near100ms 1 (batch [75000]))
      d sentAt = dataDeadline s (at sentAt) (Just (at (1 % 5))) (Just (at (1 % 5)))
  deadlineExpiresAt (d (18 % 100)) @?= addTime (picosecondsToDiffTime 1) (at (68 % 100))
  deadlineExpiresAt (d (5 % 100)) @?= at (6 % 10)

testClassification :: Assertion
testClassification = do
  let s = snapshot 1500000 1 (batch [20000])
      late = dataDeadline s (at (3 % 10)) (Just (at (1 % 5))) (Just (at (1 % 5)))
      ackOnly = dataDeadline s (at (1 % 20)) (Just (at 2)) Nothing
  deadlinePipeline late @?= FreshOrDrained
  deadlineHeadAt late @?= at (3 % 10)
  deadlineExpiresAt late @?= at (75 % 100)
  deadlinePipeline ackOnly @?= FreshOrDrained
  deadlineBase ackOnly @?= 0.3

testBody :: Assertion
testBody = do
  let s = snapshot 300000 1 (body 12000)
  snapshotPayload s @?= PayloadBytes 12000
  preparedElements (body 12000) @?= PayloadBytes 600000 :| []
  transferAllowance s @?= 0.32
  snapshotPayload (snapshot 300000 1 (body 0)) @?= PayloadBytes 0

testOverhang :: Assertion
testOverhang = do
  let s = snapshot 300000 1 (batch [14600, 16384])
  snapshotPayload s @?= PayloadBytes 30984
  transferAllowance s @?= 0.82624

testHedge :: Assertion
testHedge = do
  hedgeAllowance 0.7 4 2 @?= 2
  hedgeAllowance 3 4 2 @?= 3
  hedgeAllowance 0.7 (10 ^ (100 :: Int)) 2 @?= 2
  let noSample = right (snapshotTiming (config 300000) (at 0) Nothing 16 (body 600000))
      d = dataDeadline noSample (at 0) Nothing Nothing
  deadlineBase d @?= 16.5
  deadlineAllowance d @?= 16.5

testConfig :: Assertion
testConfig = do
  let cfg rate window cold hedgeCap guard bytes = mkTimingConfig rate window cold hedgeCap guard (PayloadBytes bytes)
  cfg 0 14600 0.5 2 60 600000 @?= Left InvalidServiceRate
  cfg 300000 0 0.5 2 60 600000 @?= Left InvalidInitialWindow
  cfg 300000 14600 (-1) 2 60 600000 @?= Left InvalidDuration
  cfg 300000 14600 0.5 2 60 0 @?= Left InvalidMaximumPayload
  cfg 10000 14600 0.5 2 60 600000 @?= Left IncompatibleFatalGuard
  cfg 300000 14600 0.5 60 60 600000 @?= Left IncompatibleFatalGuard
  cfg (4800000 / (119 % 2)) 14600 0.5 2 60 600000 @?= Left IncompatibleFatalGuard
  snapshotTiming (config 300000) (at 0) sample (1 % 2) (body 1000) @?= Left InvalidBackoffFactor
  snapshotTiming (config 300000) (at 0) sample 1 (body 1048577) @?= Left PayloadBeyondMaximum

two :: State
two = sent 1 (1 % 20) (sent 0 0 (registered 1 standard (registered 0 standard initial)))

testFIFO :: Assertion
testFIFO = do
  activeDeadline two @?= Nothing
  let s = noExpiry 0.1 two
  headAt s @?= at 0.45
  nextDeadline s @?= Just (token s, at 0.45)
  let t = noExpiry 0.2 (terminal 0 (1 % 5) s)
  token t @?= TimerToken (key 1) 1
  headAt t @?= at 0.35

testAck :: Assertion
testAck = do
  let s =
        sent
          2
          (1 % 10)
          ( sent
              1
              (1 % 20)
              (sent 0 0 (registered 2 standard (registered 1 ack (registered 0 standard initial))))
          )
      (_, blockedData) = poll 1 s
  token blockedData @?= TimerToken (key 0) 0
  let (events, t) = poll 2 (terminal 0 2 blockedData)
  events @?= []
  headAt t @?= at 2.15
  let (expiry, u) = oneExpiry 2.15 t
  chargeDataFailure expiry @?= False
  rescuableRequests u @?= [key 2]
  let v = noExpiry 2.2 (terminal 1 (22 % 10) u)
  assertBool "ACK retirement unblocks this upstream" (not (upstreamBlocked v))
  token v @?= TimerToken (key 2) 2

testWithdrawal :: Assertion
testWithdrawal = do
  let (_, s) = poll 0.1 two
      withdrawn = right (withdrawTiming (key 0) s)
      (expiry, t) = oneExpiry 0.45 withdrawn
  headAt withdrawn @?= headAt s
  chargeDataFailure expiry @?= False
  assertBool "head still blocks" (upstreamBlocked t)
  rescuableRequests t @?= [key 1]
  nextDeadline t @?= Nothing
  let u = noExpiry 0.46 (terminal 0 (46 % 100) t)
  assertBool "wire retirement resumes scheduling" (not (upstreamBlocked u))
  token u @?= TimerToken (key 1) 1

testFailureOnce :: Assertion
testFailureOnce = do
  let (_, s) = poll 0.1 two
      (expiry, t) = oneExpiry 0.45 (right (noteFailureHandled (key 0) s))
  chargeDataFailure expiry @?= False
  fst (poll 1 t) @?= []
  nextDeadline t @?= Nothing

testTie :: Assertion
testTie = do
  let s = sent 0 0 (registered 0 standard initial)
  fst (poll 1 (terminal 0 (45 % 100) s)) @?= []
  let (expiry, t) = oneExpiry 1 (terminal 0 (46 % 100) s)
  expiryAt expiry @?= at 0.45
  chargeDataFailure expiry @?= True
  activeDeadline t @?= Nothing
  assertBool "late completed response no longer blocks" (not (upstreamBlocked t))

testValidation :: Assertion
testValidation = do
  let (_, s) = poll 0.1 (sent 0 0 (registered 0 standard initial))
      (v, pending) = right (beginValidation (key 0) (at 0.45) s)
      waiting = noExpiry 1 pending
  headAt waiting @?= at 0.45
  nextDeadline waiting @?= Nothing
  assertBool "local processing is not an upstream failure" (not (upstreamBlocked waiting))
  let finished = right (finishValidation v (ValidTerminal (at 0.45)) waiting)
      (events, done) = poll 1 finished
  events @?= []
  activeDeadline done @?= Nothing

  let late = right (finishValidation v (ValidTerminal (at 0.46)) waiting)
      (lateExpiry, lateDone) = oneExpiry 1 late
  chargeDataFailure lateExpiry @?= True
  activeDeadline lateDone @?= Nothing

testPartial :: Assertion
testPartial = do
  let (_, s) = poll 0.1 (sent 0 0 (registered 0 standard initial))
      (v, p) = right (beginValidation (key 0) (at 0.4) s)
      w = noExpiry 1 p
      partial = right (finishValidation v NeedMoreInput w)
      (expiry, t) = oneExpiry 1 partial
  headAt t @?= at 0.45
  expiryAt expiry @?= at 0.45
  chargeDataFailure expiry @?= True

testLateInput :: Assertion
testLateInput = do
  let (_, s) = poll 0.1 (sent 0 0 (registered 0 standard initial))
      (_, p) = right (beginValidation (key 0) (at 0.46) s)
      (expiry, t) = oneExpiry 1 p
  chargeDataFailure expiry @?= True
  assertBool "late input cannot postpone expiry" (upstreamBlocked t)

testFastReply :: Assertion
testFastReply = do
  let received = terminal 0 (1 % 10) (registered 0 standard initial)
      s = noExpiry 0.2 received
  activeDeadline s @?= Nothing
  let done = noExpiry 0.3 (sent 0 (1 % 5) s)
  activeDeadline done @?= Nothing
  recordSend (key 0) (at 0.2) done @?= Left UnknownTimingKey
  registerTiming (key 0) standard done @?= Left NonConsecutiveTimingKey

testDelayedHandling :: Assertion
testDelayedHandling = do
  let (events, s) = poll 0.34 (terminal 0 (1 % 5) two)
  events @?= []
  headAt s @?= at 0.35
  let after = sent 1 (3 % 10) (sent 0 0 (registered 1 standard (registered 0 standard initial)))
      t = noExpiry 0.4 (terminal 0 (1 % 5) after)
  headAt t @?= at 0.75

testNoRenewal :: Assertion
testNoRenewal = do
  let (_, s) = poll 0.1 two
      (v, p) = right (beginValidation (key 0) (at 0.2) s)
      partial = right (finishValidation v NeedMoreInput p)
      future = TrackData (right (snapshotTiming (config 300000) (at 0.3) Nothing 16 (body 600000)))
      appended = sent 2 (3 % 10) (registered 2 future partial)
      (_, t) = poll 0.3 appended
  activeDeadline t @?= activeDeadline s

testStale :: Assertion
testStale = do
  let (_, s) = poll 0.1 two
      oldTimer = token s
      (_, t) = poll 0.2 (terminal 0 (1 % 5) s)
  wakeTimer oldTimer (at 1) t @?= Right ([], t)
  let (v, p) = right (beginValidation (key 1) (at 0.25) t)
      settled = right (finishValidation v NeedMoreInput p)
  finishValidation v NeedMoreInput settled @?= Left UnknownValidation
  let (_, pendingAgain) = right (beginValidation (key 1) (at 0.26) settled)
  finishValidation v NeedMoreInput pendingAgain @?= Left UnknownValidation
  let replacement :: State
      replacement = newTimingState (MkLeiosPeerLifetimeId 7 1) (FetchSessionToken 0)
  wakeTimer oldTimer (at 1) replacement @?= Right ([], replacement)
  recordTerminal (key 0) (at 0.1) replacement @?= Left WrongTimingSession
  finishValidation v NeedMoreInput replacement @?= Left WrongTimingSession
  recordSend (key 0) (at 1) (retireTimingSession s) @?= Left ClosedTimingSession

testEventErrors :: Assertion
testEventErrors = do
  let s = sent 0 0 (registered 0 standard initial)
      (_, t) = poll 0.1 s
  recordSend (key 0) (at (-1)) (registered 0 standard initial) @?= Left InvalidObservationTime
  reconcileTiming (at 0.05) t @?= Left TimeWentBackwards
  reconcileTiming (at 0.05) (terminal 0 (1 % 10) s) @?= Left FutureObservation
  let (_, expired) = poll 1 t
  recordTerminal (key 0) (at 0.45) expired @?= Left MissedObservationBoundary
  let (v, pending) = right (beginValidation (key 0) (at 0.2) t)
  finishValidation v (ValidTerminal (at 0.1)) pending @?= Left InvalidObservationTime

propArithmetic :: Property
propArithmetic = forAll
  ((,,) <$> chooseInt (1, 600000) <*> elements [300000, 1500000, 200000000] <*> chooseInt (1, 16))
  $ \(bytes, rate, factor) ->
    let s = snapshot rate (fromIntegral factor) (batch [fromIntegral bytes])
        fresh = dataDeadline s (at 0) Nothing Nothing
        warm = dataDeadline s (at 0) (Just (at 1)) (Just (at 1))
        small = snapshot rate 1 (batch [fromIntegral (max 1 (bytes - 1))])
     in counterexample (show (s, fresh, warm)) $
          property (toRational (transferAllowance s) >= 8 * fromIntegral bytes / rate)
            .&&. property (deadlineAllowance fresh >= deadlineBase fresh)
            .&&. property (deadlineAllowance fresh <= max 2 (deadlineBase fresh))
            .&&. property (coldAllowance s >= getRttAllowance (snapshotRtt s))
            .&&. property (transferAllowance small <= transferAllowance s)
            .&&. property (deadlineBase warm >= transferAllowance s)

propEventOrder :: Property
propEventOrder = forAll (shuffle [0 :: Int, 1, 2, 3]) $ \order ->
  let start = registered 1 standard (registered 0 standard initial)
      apply s i = case i of
        0 -> recordSend (key 0) (at 0) s
        1 -> recordTerminal (key 0) (at 0.1) s
        2 -> recordSend (key 1) (at 0.05) s
        _ -> recordTerminal (key 1) (at 0.6) s
      result = foldM apply start order >>= reconcileTiming (at 1)
   in counterexample (show result) $ case result of
        Right ([expiry], done) ->
          property
            ( expiryToken expiry == TimerToken (key 1) 1
                && expiryAt expiry == at 0.35
                && chargeDataFailure expiry
                && activeDeadline done == Nothing
                && not (upstreamBlocked done)
            )
        _ -> property False

propOneShot :: Property
propOneShot = forAll (chooseInt (1, 50)) $ \later ->
  let (events, s) = poll 1 (sent 0 0 (registered 0 standard initial))
      again = wakeTimer (token s) (at (fromIntegral later + 1)) s
   in counterexample (show again) $ case again of
        Right ([], done) -> property (length events == 1 && upstreamBlocked done && nextDeadline done == Nothing)
        _ -> property False
