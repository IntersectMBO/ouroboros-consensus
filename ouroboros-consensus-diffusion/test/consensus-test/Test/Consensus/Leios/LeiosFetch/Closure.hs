module Test.Consensus.Leios.LeiosFetch.Closure (tests) where

import Control.Monad.Class.MonadTime.SI (Time (..))
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Closure
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

type State = ClosureState Int ()

point :: LeiosPoint
point = MkLeiosPoint (SlotNo 1) (endorserBlockHash (mkEndorserBlock []))

gen :: AcquisitionGeneration
gen = AcquisitionGeneration 0

peer :: Int -> LeiosPeerLifetimeId Int
peer n = MkLeiosPeerLifetimeId n 0

key :: Int -> Int -> RequestKey Int
key p n = RequestKey (peer p) (FetchSessionToken 0) (WireOrdinal (fromIntegral n))

batch :: [Int] -> BatchDescriptor
batch offsets =
  BatchDescriptor
    point
    gen
    ( NE.fromList
        [ ExpectedTx
            off
            (hashLeiosTx (MkLeiosTx (BS.pack [fromIntegral off])))
            (fromIntegral (1000 * (off + 1)))
        | off <- offsets
        ]
    )

right :: Show e => Either e a -> a
right = either (error . show) id

initial :: State
initial =
  refreshClosure
    point
    gen
    (IntSet.fromList [0 .. 7])
    (ensureClosureAcquisition point gen emptyClosureState)

primary :: State
primary = right (admitClosureAttempt ClosurePrimary (key 1 0) () (batch [0, 1]) initial)

stalled :: State
stalled = snd (expireClosureAttempt (key 1 0) True primary)

hedged :: State
hedged = right (admitClosureAttempt (ClosureRescue (key 1 0)) (key 2 0) () (batch [0, 1]) stalled)

failed :: State
failed = snd (expireClosureAttempt (key 2 0) True hedged)

snapshot :: State -> ClosureSnapshot Int
snapshot = fromJust . Map.lookup point . closureSnapshots

remaining :: [Int] -> State -> State
remaining offsets = refreshClosure point gen (IntSet.fromList offsets)

withTail :: State
withTail = right (admitClosureAttempt ClosurePrimary (key 1 1) () (batch [2, 3]) hedged)

tests :: TestTree
tests =
  testGroup
    "closure policy"
    [ testCase "primary expires once and remains wanted without cancellation" $ do
        fst (expireClosureAttempt (key 1 0) True primary) @?= ClosurePrimaryStalled
        closureStalled (snapshot stalled) @?= [key 1 0]
        closureFailures (peer 1) stalled @?= 1
        pendingClosureCancels stalled @?= []
        expireClosureAttempt (key 1 0) True stalled @?= (NoClosureChange, stalled)
    , testCase "only one extra batch per EB; no third wanted TX copy" $ do
        admitClosureAttempt (ClosureRescue (key 1 0)) (key 3 0) () (batch [0]) hedged
          @?= Left InvalidClosureAttempt
        IntMap.elems (closureCopies (snapshot hedged)) @?= [2, 2]
        closureRescuePlans hedged @?= []
    , testCase "rescue failure frees the logical slot, retaining the original debt" $ do
        closureRescue (snapshot failed) @?= Nothing
        closureDraining (snapshot failed) @?= 1
        closureFailures (peer 2) failed @?= 1
        pendingClosureCancels failed @?= [(key 2 0, ())]
        closureRescueCandidates (key 1 0) (map peer [1, 2, 3]) failed @?= [peer 3]
    , testCase "original before ACK cannot make the canceled peer reusable" $ do
        let canceled = right (recordClosureCancel (key 2 0) (key 2 1) failed)
            done = finishClosureOriginal (key 2 0) canceled
        closureRescueCandidates (key 1 0) (map peer [2, 3]) done @?= [peer 3]
        closureRescueCandidates (key 1 0) (map peer [2, 3]) (finishClosureCancel (key 2 1) done)
          @?= [peer 3, peer 2]
    , testCase "ACK before original also retains debt" $ do
        let canceled = right (recordClosureCancel (key 2 0) (key 2 1) failed)
            done = finishClosureCancel (key 2 1) canceled
        closureRescueCandidates (key 1 0) [peer 2] done @?= []
        closureRescueCandidates (key 1 0) [peer 2] (finishClosureOriginal (key 2 0) done) @?= [peer 2]
    , testCase "blocked queued primary gets rescue eligibility without failure" $ do
        let queued = right (admitClosureAttempt ClosurePrimary (key 1 1) () (batch [2, 3]) primary)
            blocked = rescueBlockedClosure (key 1 1) (rescueBlockedClosure (key 1 0) queued)
        map (\(k, _, _, _) -> k) (closureRescuePlans blocked) @?= [key 1 0, key 1 1]
        closureFailures (peer 1) blocked @?= 0
    , testCase "blocked queued rescue releases its slot without a fabricated service failure" $ do
        let blocked = rescueBlockedClosure (key 2 0) hedged
        closureFailures (peer 2) blocked @?= 0
        closureRescue (snapshot blocked) @?= Nothing
        pendingClosureCancels blocked @?= [(key 2 0, ())]
    , testCase "multiple primaries rotate FIFO after a partial rescue opportunity" $ do
        let both = rescueBlockedClosure (key 1 1) withTail
            rotated = snd (expireClosureAttempt (key 2 0) True both)
        closureStalled (snapshot rotated) @?= [key 1 1, key 1 0]
    , testCase "subset rescue requests only remaining offered TXs" $ do
        let partial = remaining [1 .. 7] stalled
            selected = right (admitClosureAttempt (ClosureRescue (key 1 0)) (key 2 0) () (batch [1]) partial)
        IntMap.lookup 0 (closureCopies (snapshot selected)) @?= Nothing
        IntMap.lookup 1 (closureCopies (snapshot selected)) @?= Just 2
        admitClosureAttempt (ClosureRescue (key 1 0)) (key 3 0) () (batch [2]) partial
          @?= Left InvalidClosureAttempt
    , testCase "partly redundant original keeps its membership and is not canceled" $ do
        let partial = remaining [1 .. 7] hedged
        pendingClosureCancels partial @?= []
        closureRescue (snapshot partial) @?= Just (key 2 0)
    , testCase "verified fulfillment prevents later timeout from charging either copy" $ do
        let fulfilled = remaining [2 .. 7] hedged
        fst (expireClosureAttempt (key 1 0) True fulfilled) @?= NoClosureChange
        fst (expireClosureAttempt (key 2 0) True fulfilled) @?= NoClosureChange
        closureRescue (snapshot fulfilled) @?= Nothing
        map fst (pendingClosureCancels fulfilled) @?= [key 1 0, key 2 0]
    , testCase "successful rescue enables whole-batch useful-tail handoff" $ do
        let serviced = noteClosureService (key 2 0) 1 (Time 1) withTail
            complete = remaining [2 .. 7] serviced
        closureTailPlans complete @?= [(key 1 1, peer 2, IntSet.fromList [2, 3])]
        closureTailPlans (abandonClosureAttempt (key 1 1) complete) @?= []
    , testCase "productive suppliers are ranked by actual bytes, then service time" $ do
        let one = noteClosureService (key 2 0) 0 (Time 1) withTail
            rotated = snd (expireClosureAttempt (key 2 0) True (remaining [1 .. 7] one))
            second = right (admitClosureAttempt (ClosureRescue (key 1 0)) (key 3 0) () (batch [1]) rotated)
            done = remaining [2 .. 7] (noteClosureService (key 3 0) 0 (Time 2) second)
        map (\(_, p, _) -> p) (closureTailPlans done) @?= map peer [3, 2]
    , testCase "handoff never includes another EB's or another session's batches" $ do
        let other = RequestKey (peer 1) (FetchSessionToken 1) (WireOrdinal 9)
            mixed = right (admitClosureAttempt ClosurePrimary other () (batch [4, 5]) withTail)
            done = remaining [2 .. 7] (noteClosureService (key 2 0) 0 (Time 1) mixed)
        map (\(k, _, _) -> k) (closureTailPlans done) @?= [key 1 1]
    , testCase "terminal originals are never canceled, even before durable storage" $ do
        let done = finishClosureOriginal (key 2 0) hedged
        map fst (pendingClosureCancels done) @?= []
        closureRescue (snapshot done) @?= Nothing
    , testCase "unconfirmed storage can restore missing work without renewing old attempts" $ do
        let verified = remaining [2 .. 7] hedged
            retry = remaining [0 .. 7] verified
        IntMap.elems (closureCopies (snapshot retry)) @?= []
        closureRescuePlans retry @?= []
        assertBool "fresh assignment allowed" $ case admitClosureAttempt ClosurePrimary (key 3 0) () (batch [0, 1]) retry of
          Right _ -> True
          _ -> False
    , testCase "withdrawal is published once while cancel capacity is unavailable" $ do
        unwithdrawnClosureAttempts failed @?= [key 2 0]
        unwithdrawnClosureAttempts (recordClosureWithdrawal (key 2 0) failed) @?= []
    , testCase "retirement preserves cancelable originals but rejects stale admission" $ do
        let retired = retireClosureAcquisition point gen hedged
        closureSnapshots retired @?= Map.empty
        length (pendingClosureCancels retired) @?= 2
        admitClosureAttempt ClosurePrimary (key 3 0) () (batch [0]) retired @?= Left UnknownClosure
    , testCase "disconnect removes only exact registration evidence and debt" $ do
        let gone = removeClosurePeer (peer 2) failed
        closureFailures (peer 2) gone @?= 0
        pendingClosureCancels gone @?= []
        closureFailures (peer 1) gone @?= 1
        map (\(k, _, _, _) -> k) (closureRescuePlans gone) @?= [key 1 0]
    , testCase "concurrent EBs have independent single rescue slots" $ do
        let other = point{pointSlotNo = SlotNo 2}
            otherBatch = (batch [2, 3]){batchPoint = other}
            ready = refreshClosure other gen (IntSet.fromList [2, 3]) (ensureClosureAcquisition other gen hedged)
            owned = right (admitClosureAttempt ClosurePrimary (key 1 1) () otherBatch ready)
            expired = snd (expireClosureAttempt (key 1 1) True owned)
            rescued = right (admitClosureAttempt (ClosureRescue (key 1 1)) (key 3 0) () otherBatch expired)
        map closureRescue (Map.elems (closureSnapshots rescued)) @?= [Just (key 2 0), Just (key 3 0)]
    , testCase "old-generation expiry and refresh cannot affect a refocused acquisition" $ do
        let newGen = AcquisitionGeneration 1
            ready =
              refreshClosure
                point
                newGen
                (IntSet.fromList [0 .. 7])
                (ensureClosureAcquisition point newGen (retireClosureAcquisition point gen hedged))
            owned =
              right
                (admitClosureAttempt ClosurePrimary (key 3 0) () (batch [0, 1]){batchGeneration = newGen} ready)
        expireClosureAttempt (key 1 0) True owned @?= (NoClosureChange, owned)
        refreshClosure point gen IntSet.empty owned @?= owned
        closureGeneration (snapshot owned) @?= newGen
        IntMap.elems (closureCopies (snapshot owned)) @?= [1, 1]
    , testProperty "generated admission, expiry, fulfillment and drain schedules respect copy bounds" $ \events ->
        let states = scanl step (hedged, [key 1 0, key 2 0]) (zip [2 ..] (take 100 (events :: [Int])))
         in counterexample
              (show states)
              ( all
                  (all (<= 2) . concatMap (IntMap.elems . closureCopies) . Map.elems . closureSnapshots . fst)
                  states
              )
    ]
 where
  step (s, known) (ordinal, event) =
    let selected = known !! (event `mod` length known)
        nextKey p = key p ordinal
        add kind p offsets = case admitClosureAttempt kind (nextKey p) () (batch offsets) s of
          Right next -> (next, known ++ [nextKey p])
          Left _ -> (s, known)
        unchanged next = (next, known)
     in case event `mod` 9 of
          0 -> add ClosurePrimary (1 + event `mod` 4) [event `mod` 8]
          1 -> case closureRescuePlans s of
            (parent, _, _, eligible) : _ -> case closureRescueCandidates parent (map peer [1 .. 4]) s of
              MkLeiosPeerLifetimeId p _ : _ -> add (ClosureRescue parent) p (take 2 (IntSet.toList eligible))
              _ -> (s, known)
            _ -> (s, known)
          2 -> unchanged (snd (expireClosureAttempt selected True s))
          3 -> unchanged (remaining [event `mod` 8 .. 7] s)
          4 -> unchanged (finishClosureOriginal selected s)
          5 -> unchanged (rescueBlockedClosure selected s)
          6 -> case pendingClosureCancels s of
            (original@(RequestKey lifetime token _), _) : _ ->
              let ack = RequestKey lifetime token (WireOrdinal (fromIntegral ordinal))
               in case recordClosureCancel original ack s of
                    Right canceled -> unchanged (finishClosureCancel ack canceled)
                    Left _ -> (s, known)
            _ -> (s, known)
          7 -> unchanged (removeClosurePeer (peer (1 + event `mod` 4)) s)
          _ -> unchanged (foldl' (flip recordClosureWithdrawal) s (unwithdrawnClosureAttempts s))
