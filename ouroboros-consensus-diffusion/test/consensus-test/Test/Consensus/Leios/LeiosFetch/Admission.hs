{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.Leios.LeiosFetch.Admission (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (foldM)
import qualified Data.ByteString.Short as SBS
import Data.Either (isRight)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word16, Word64)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Leios.Types
  ( LeiosEbBounds (..)
  , LeiosPoint (..)
  , mkEndorserBlock
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Admission
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Arbitrary (..)
  , Property
  , chooseInt
  , counterexample
  , frequency
  , property
  , testProperty
  , vectorOf
  , (.&&.)
  )

tests :: TestTree
tests =
  testGroup
    "atomic admission"
    [ testCase "preparation allocates nothing and validates trusted offsets" testPreparation
    , testCase "D data records retain the other D slots for cancellation" testCapacity
    , testCase "a crossed normal response retains its record until the cancel ACK" testCrossed
    , testCase "cancellation of A preserves B and has its own FIFO slot" testFIFO
    , testCase "response, payload and entry capacity are separate and shared across EBs" testBudgets
    , testCase "body reserves trusted upper bound but can retain a larger-than-announced body" testBody
    , testCase "accepted TX prefix survives disconnect until its exact storage receipt" testDisconnect
    , testCase "cancelled terminal cannot truncate committed normal data" testCommitted
    , testCase "replacement registration/session cannot accept stale callbacks" testStale
    , testCase "tiny ordinal spaces reserve future cancels without gaps" testExhaustion
    , testCase "Word64 boundary does not overflow the available-ordinal calculation" testWord64
    , testCase "local identity counters can advance beyond Word64" testLocalTokens
    , testProperty "generated transition traces preserve admission invariants" propTransitions
    ]

type State = AdmissionState Int
type Key = RequestKey Int

right :: Show e => Either e a -> a
right = either (error . show) id

limits :: Word16 -> Natural -> Natural -> Natural -> Word64 -> AdmissionLimits
limits d wire payload entries lastId =
  right (mkAdmissionLimits d (EncodedBytes wire) (PayloadBytes payload) (EntryCount entries) lastId)

initial :: State
initial =
  newAdmissionState (limits 3 100 100 20 maxBound) (MkLeiosPeerLifetimeId 7 0) (FetchSessionToken 0)

key :: Word64 -> Key
key = RequestKey (MkLeiosPeerLifetimeId 7 0) (FetchSessionToken 0) . WireOrdinal

point :: Word -> LeiosPoint
point n = MkLeiosPoint (SlotNo (fromIntegral n)) (UnsafeHash (SBS.pack (replicate 32 (fromIntegral n))))

batch :: Word -> [Word] -> PreparedRequest
batch eb sizes =
  right $
    prepareBatch
      (point eb)
      (AcquisitionGeneration 0)
      ( mkEndorserBlock
          [ (UnsafeHash (SBS.pack (replicate 32 (fromIntegral i))), fromIntegral size)
          | (i, size) <- zip [0 :: Int ..] sizes
          ]
      )
      (0 :| [1 .. length sizes - 1])
      (EncodedBytes (fromIntegral (sum sizes) + 2))

small :: PreparedRequest
small = batch 0 [2]

admitted :: PreparedRequest -> State -> (Key, State)
admitted p = right . admitData p

sent :: Key -> State -> State
sent k = right . dispatchNext k

observed :: Key -> Natural -> State -> (HandoffKey Int, State)
observed k size s = case right (observeElement k (PayloadBytes size) True s) of
  (Just ticket, next) -> (ticket, next)
  _ -> error "useful element did not create a ticket"

expectError :: (Eq a, Show a) => AdmissionError -> Either AdmissionError a -> Assertion
expectError err result = result @?= Left err

testPreparation :: Assertion
testPreparation = do
  nextOrdinal initial @?= 0
  liveRequests initial @?= []
  let body = mkEndorserBlock [(UnsafeHash (SBS.pack (replicate 32 0)), 2)]
      prep offsets bytes = prepareBatch (point 0) (AcquisitionGeneration 0) body offsets (EncodedBytes bytes)
  prep (0 :| [0]) 4 @?= Left InvalidOffsets
  prep ((-1) :| []) 4 @?= Left OffsetOutsideBody
  prep (1 :| []) 4 @?= Left OffsetOutsideBody
  prep (0 :| []) 1 @?= Left InsufficientFraming
  mkAdmissionLimits 0 (EncodedBytes 1) (PayloadBytes 1) (EntryCount 1) maxBound @?= Left InvalidLimits

testCapacity :: Assertion
testCapacity = do
  let (ks, full) =
        foldl
          (\(keys, s) _ -> let (k, t) = admitted small s in (keys ++ [k], t))
          ([], initial)
          [1 .. 3 :: Int]
      cancelled = right $ foldM (\s k -> snd <$> admitCancel k s) full ks
  expectError DataCapacity (admitData small full)
  usedExchanges (admissionUsage full) @?= 3
  usedExchanges (admissionUsage cancelled) @?= 6
  assertBool "full D/W invariant" (admissionInvariant cancelled)
  expectError AlreadyCancelled (admitCancel (head ks) cancelled)
  expectError DataCapacity (admitData small cancelled)

testCrossed :: Assertion
testCrossed = do
  let (a, s1) = admitted small initial
      (c, s2) = right (admitCancel a s1)
      (_, s3) = observed a 2 (sent c (sent a s2))
      s4 = right (finishHead a NormalTerminal s3)
  liveRequests s4 @?= [a]
  usedResponseBytes (admissionUsage s4) @?= 0
  usedHandoffBytes (admissionUsage s4) @?= 2
  expectError ReplyOrder (finishHead a NormalTerminal s4)
  let s5 = right (finishHead c CancelAcknowledgement s4)
  liveRequests s5 @?= []
  map fst (pendingHandoffs s5) @?= [HandoffKey a 0]
  let s6 = right (releaseHandoff (HandoffKey a 0) s5)
  admissionUsage s6 @?= Usage 0 0 0 0 0
  expectError UnknownHandoff (releaseHandoff (HandoffKey a 0) s6)

testFIFO :: Assertion
testFIFO = do
  let (a, s1) = admitted small initial
      (b, s2) = admitted small s1
      (c, s3) = right (admitCancel a s2)
  expectError DispatchOrder (dispatchNext b s3)
  expectError NotDispatched (finishHead a CancelledTerminal s3)
  let s4 = sent c (sent b (sent a s3))
  expectError ReplyOrder (finishHead c CancelAcknowledgement s4)
  let s5 = right (finishHead a CancelledTerminal s4)
  outstandingExchanges s5 @?= [DataExchange b, CancelExchange c a]
  let (_, s6) = observed b 2 s5
      s7 = right (finishHead b NormalTerminal s6)
      s8 = right (finishHead c CancelAcknowledgement s7)
  liveRequests s8 @?= []
  assertBool "B is independently delivered" (not (null (pendingHandoffs s8)))

testBudgets :: Assertion
testBudgets = do
  let state wire payload entries =
        newAdmissionState
          (limits 3 wire payload entries maxBound)
          (MkLeiosPeerLifetimeId 7 0)
          (FetchSessionToken 0)
      (_, wireFull) = admitted small (state 4 100 10)
      (_, handoffFull) = admitted small (state 100 2 10)
      (_, entryFull) = admitted small (state 100 100 1)
  expectError ResponseCapacity (admitData (batch 1 [2]) wireFull)
  expectError HandoffCapacity (admitData (batch 1 [2]) handoffFull)
  expectError EntryCapacity (admitData (batch 1 [2]) entryFull)
  expectError ResponseCapacity (admitData (batch 1 [1000]) initial)

testBody :: Assertion
testBody = do
  let body =
        right
          ( prepareBody
              (BodyDescriptor (point 0) (AcquisitionGeneration 0) 1 (MkLeiosEbBounds 60 100))
              (EncodedBytes 62)
          )
      (a, s1) = admitted body initial
  usedResponseBytes (admissionUsage s1) @?= 62
  usedHandoffBytes (admissionUsage s1) @?= 60
  expectError ResponseCapacity (admitData body s1)
  expectError InvalidElement (observeElement a (PayloadBytes 61) True (sent a s1))
  let (ticket, s2) = observed a 40 (sent a s1)
      s3 = right (finishHead a NormalTerminal s2)
  usedHandoffBytes (admissionUsage s3) @?= 40
  admissionUsage (right (releaseHandoff ticket s3)) @?= Usage 0 0 0 0 0

testDisconnect :: Assertion
testDisconnect = do
  let (a, s1) = admitted (batch 0 [2, 3]) initial
      (ticket, s2) = observed a 2 (sent a s1)
      s3 = retireSession s2
  admissionUsage s3 @?= Usage 0 0 0 2 1
  expectError SessionClosed (admitData small s3)
  expectError SessionClosed (observeElement a (PayloadBytes 3) True s3)
  let s4 = right (releaseHandoff ticket s3)
  admissionUsage s4 @?= Usage 0 0 0 0 0
  retireSession s4 @?= s4
  expectError UnknownHandoff (releaseHandoff ticket s4)

testCommitted :: Assertion
testCommitted = do
  let (a, s1) = admitted (batch 0 [2, 3]) initial
      (c, s2) = right (admitCancel a s1)
      (_, s3) = observed a 2 (sent c (sent a s2))
  expectError InvalidTerminal (finishHead a NormalTerminal s3)
  expectError InvalidTerminal (finishHead a CancelledTerminal s3)
  expectError InvalidElement (observeElement a (PayloadBytes 2) True s3)
  let (_, s4) = right (observeElement a (PayloadBytes 3) False s3)
      s5 = right (finishHead a NormalTerminal s4)
  usedHandoffBytes (admissionUsage s5) @?= 2
  assertBool "normal response drains after cancel" (admissionInvariant s5)

testStale :: Assertion
testStale = do
  let (a, _) = admitted small initial
      changedSession =
        newAdmissionState (limits 3 100 100 20 maxBound) (MkLeiosPeerLifetimeId 7 0) (FetchSessionToken 1)
      changedLifetime =
        newAdmissionState (limits 3 100 100 20 maxBound) (MkLeiosPeerLifetimeId 7 1) (FetchSessionToken 0)
  mapM_
    ( \s -> do
        expectError StaleSession (admitCancel a s)
        expectError StaleSession (dispatchNext a s)
        expectError StaleSession (observeElement a (PayloadBytes 2) True s)
        expectError StaleSession (finishHead a NormalTerminal s)
        expectError StaleSession (releaseHandoff (HandoffKey a 0) s)
    )
    [changedSession, changedLifetime]

testExhaustion :: Assertion
testExhaustion = do
  let start = newAdmissionState (limits 3 100 100 20 2) (MkLeiosPeerLifetimeId 7 0) (FetchSessionToken 0)
      (a, s1) = admitted small start
  a @?= key 0
  expectError OrdinalExhausted (admitData small s1)
  nextOrdinal s1 @?= 1
  let (c, s2) = right (admitCancel a s1)
      s3 = right (finishHead a CancelledTerminal (sent c (sent a s2)))
      s4 = right (finishHead c CancelAcknowledgement s3)
  c @?= key 1
  expectError OrdinalExhausted (admitData small s4)
  assertBool "last ID remains unavailable for uncancellable data" (admissionInvariant s4)
  let zero :: State
      zero = newAdmissionState (limits 1 100 100 20 0) (MkLeiosPeerLifetimeId 7 0) (FetchSessionToken 0)
  expectError OrdinalExhausted (admitData small zero)
  let start4 = newAdmissionState (limits 2 100 100 20 3) (MkLeiosPeerLifetimeId 7 0) (FetchSessionToken 0)
      (a4, t1) = admitted small start4
      (b4, t2) = admitted small t1
      (_, t3) = right (admitCancel a4 t2)
      (lastCancel, t4) = right (admitCancel b4 t3)
  lastCancel @?= key 3
  nextOrdinal t4 @?= 4
  assertBool "final wire ID is usable for a promised cancel" (admissionInvariant t4)

testWord64 :: Assertion
testWord64 = do
  let (a, s) = admitted small initial
  a @?= key 0
  nextOrdinal s @?= 1
  assertBool "maxBound + 1 is promoted before arithmetic" (admissionInvariant s)

testLocalTokens :: Assertion
testLocalTokens = do
  let n = fromIntegral (maxBound :: Word64) + 1
      lifetime = MkLeiosPeerLifetimeId 7 n
      s = newAdmissionState (limits 1 100 100 20 maxBound) lifetime (FetchSessionToken n)
      (a, _) = admitted small s
  a @?= RequestKey lifetime (FetchSessionToken n) (WireOrdinal 0)
  assertBool "registration remains distinct" (lifetime /= MkLeiosPeerLifetimeId 7 0)

data Action = Add Int | Cancel Int | Send | Observe Bool | Reply Bool | Ack | Release Int | Disconnect
  deriving Show

newtype Trace = Trace [Action] deriving Show

instance Arbitrary Trace where
  arbitrary =
    (\actions -> Trace (actions ++ [Disconnect, Release 0]))
      <$> vectorOf
        200
        ( frequency
            [ (5, Add <$> chooseInt (0, 2))
            , (3, Cancel <$> chooseInt (0, 6))
            , (5, pure Send)
            , (4, Observe <$> arbitrary)
            , (3, Reply <$> arbitrary)
            , (2, pure Ack)
            , (2, Release <$> chooseInt (0, 6))
            ]
        )
  shrink (Trace xs) = [Trace ys | ys <- shrinkList xs]
   where
    shrinkList [] = []
    shrinkList ys = [take (length ys `div` 2) ys, drop 1 ys]

propTransitions :: Trace -> Property
propTransitions (Trace actions) =
  foldr
    (.&&.)
    (property True)
    [ walk
        ( newAdmissionState
            (limits d wire payload entries lastId)
            (MkLeiosPeerLifetimeId 7 0)
            (FetchSessionToken 0)
        )
        actions
    | (d, wire, payload, entries, lastId) <-
        [(1, 12, 10, 3, 3), (2, 20, 10, 4, 7), (3, 40, 30, 8, 15), (3, 100, 100, 20, maxBound)]
    ]
 where
  walk s [] = counterexample (show s) (admissionInvariant s)
  walk s (action : rest) =
    let result = transition action s
        next = either (const s) id result
     in counterexample
          (show action ++ "\n" ++ show result)
          (property (admissionInvariant next) .&&. walk next rest)

transition :: Action -> State -> Either AdmissionError State
transition action s = case action of
  Add i -> snd <$> admitData (case i of 0 -> small; 1 -> batch 1 [2, 3]; _ -> batch 2 [3]) s
  Cancel i -> case liveRequests s of
    [] -> snd <$> admitCancel (key (fromIntegral i)) s
    xs -> snd <$> admitCancel (xs !! (i `mod` length xs)) s
  Send -> case [exchangeKey e | e <- outstandingExchanges s, isRight (dispatchNext (exchangeKey e) s)] of
    k : _ -> dispatchNext k s
    [] -> Left DispatchOrder
  Observe useful -> case outstandingExchanges s of
    DataExchange k : _ -> case observeElement k (PayloadBytes 2) useful s of
      Right (_, t) -> Right t
      Left _ -> snd <$> observeElement k (PayloadBytes 3) useful s
    _ -> Left InvalidElement
  Reply cancelled -> case outstandingExchanges s of
    DataExchange k : _ -> finishHead k (if cancelled then CancelledTerminal else NormalTerminal) s
    _ -> Left ReplyOrder
  Ack -> case outstandingExchanges s of
    CancelExchange k _ : _ -> finishHead k CancelAcknowledgement s
    _ -> Left ReplyOrder
  Release i -> case pendingHandoffs s of
    [] -> Left UnknownHandoff
    xs -> releaseHandoff (fst (xs !! (i `mod` length xs))) s
  Disconnect -> Right (retireSession s)
 where
  exchangeKey (DataExchange k) = k
  exchangeKey (CancelExchange k _) = k
