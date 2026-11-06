module Test.Consensus.Leios.LeiosFetch.Body (tests) where

import Control.Monad.IOSim (runSimOrThrow)
import qualified Data.ByteString as BS
import qualified Data.IntSet as IntSet
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.Types
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Body
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Deadline as D
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
  ( BodyDescriptor (BodyDescriptor)
  , EncodedBytes (..)
  , PayloadBytes (..)
  , prepareBody
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Handoff as H
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity
import Ouroboros.Consensus.Util.IOLike (atomically)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Util.Orphans.IOLike ()

type State = BodyState Int ()

point :: LeiosPoint
point = MkLeiosPoint (SlotNo 1) (endorserBlockHash (mkEndorserBlock []))

gen :: AcquisitionGeneration
gen = AcquisitionGeneration 0

key :: Int -> RequestKey Int
key n = RequestKey (MkLeiosPeerLifetimeId n 0) (FetchSessionToken 0) (WireOrdinal 0)

ack :: Int -> RequestKey Int
ack n = RequestKey (MkLeiosPeerLifetimeId n 0) (FetchSessionToken 0) (WireOrdinal 1)

peer :: Int -> LeiosPeerLifetimeId Int
peer n = MkLeiosPeerLifetimeId n 0

initial :: State
initial =
  registerBodyOffers
    point
    (map peer [1 .. 4])
    (ensureBodyAcquisition point gen (Just (MkLeiosEbBounds 600000 12000000)) emptyBodyState)

primary :: State
primary = right (admitBodyAttempt point gen BodyPrimary (key 1) () initial)

triggered :: State
triggered = snd (expireBodyAttempt (key 1) True primary)

hedged :: State
hedged = right (admitBodyAttempt point gen BodyHedge (key 2) () triggered)

failed :: State
failed = snd (expireBodyAttempt (key 2) True hedged)

snapshot :: State -> BodySnapshot Int
snapshot = fromJust . bodySnapshot point

right :: Show e => Either e a -> a
right result = either (error . show) id result

tests :: TestTree
tests =
  testGroup
    "body policy"
    [ testCase "primary expiry preserves the original and opens just the hedge slot" $ do
        let (change, next) = expireBodyAttempt (key 1) True primary
        change @?= BodyPrimaryTriggered
        bodyPrimary (snapshot next) @?= Just (key 1)
        bodyFailures (snapshot next) @?= 0
        bodyWanted (snapshot next) @?= 1
        pendingBodyCancels next @?= []
        nextBodyRole point next @?= Just BodyHedge
    , testCase "an occupied hedge slot rejects a third wanted copy" $ do
        admitBodyAttempt point gen BodyHedge (key 3) () hedged @?= Left BodySlotOccupied
        bodyWanted (snapshot hedged) @?= 2
    , testCase "hedge expiry releases logical interest but preserves draining originals" $ do
        bodyWanted (snapshot failed) @?= 1
        bodyDraining (snapshot failed) @?= 1
        bodyFailures (snapshot failed) @?= 1
        pendingBodyCancels failed @?= [(key 2, ())]
        bodyCandidates point failed @?= map peer [3, 4]
    , testCase "duplicate, withdrawn and acknowledgment expiry cannot add another failure" $ do
        expireBodyAttempt (key 2) True failed @?= (NoBodyChange, failed)
        expireBodyAttempt (key 1) True triggered @?= (NoBodyChange, triggered)
        expireBodyAttempt (ack 2) False failed @?= (NoBodyChange, failed)
    , testCase "logical timing withdrawal is published only once while cancellation is blocked" $ do
        unwithdrawnBodyAttempts failed @?= [key 2]
        unwithdrawnBodyAttempts (recordBodyWithdrawal (key 2) failed) @?= []
        pendingBodyCancels (recordBodyWithdrawal (key 2) failed) @?= [(key 2, ())]
    , testCase "an original retiring first cannot free the same peer before its ACK" $ do
        let canceled = right (recordBodyCancel (key 2) (ack 2) failed)
            originalDone = finishBodyOriginal (key 2) canceled
        bodyCandidates point originalDone @?= map peer [3, 4]
        bodyCandidates point (finishBodyCancel (ack 2) originalDone) @?= map peer [3, 4, 2]
    , testCase "an ACK retiring first cannot free the same peer before its original" $ do
        let canceled = right (recordBodyCancel (key 2) (ack 2) failed)
            ackDone = finishBodyCancel (ack 2) canceled
        bodyCandidates point ackDone @?= map peer [3, 4]
        bodyCandidates point (finishBodyOriginal (key 2) ackDone) @?= map peer [3, 4, 2]
    , testCase "repeated offers neither duplicate the queue nor replace an unexpired hedge" $ do
        let repeated = registerBodyOffers point (map peer [1, 2, 3, 4, 3, 2, 1]) hedged
        repeated @?= hedged
        nextBodyRole point repeated @?= Nothing
    , testCase "a late canceled winner cancels the other wanted copies without undoing its failure" $ do
        let canceled = right (recordBodyCancel (key 2) (ack 2) failed)
            next = right (admitBodyAttempt point gen BodyHedge (key 3) () canceled)
            won = fulfillBody point gen (Just (key 2)) next
        bodyWanted (snapshot won) @?= 0
        bodyFailures (snapshot won) @?= 1
        bodyFulfilled (snapshot won) @?= True
        map fst (pendingBodyCancels won) @?= [key 1, key 3]
    , testCase "metadata retirement does not cancel its already verified winning original" $ do
        let won = fulfillBody point gen (Just (key 1)) hedged
            duplicate = fulfillBody point gen (Just (key 2)) won
            retired = retireBodyAcquisition point gen duplicate
        duplicate @?= won
        map fst (pendingBodyCancels retired) @?= [key 2]
        bodyAttemptPayload (key 1) retired @?= Just ()
        bodyAttemptPayload (key 1) (finishBodyOriginal (key 1) retired) @?= Nothing
    , testCase "source removal promotes a surviving hedge without renewing its request" $ do
        let promoted = removeBodyPeer (peer 1) hedged
        bodyPrimary (snapshot promoted) @?= Just (key 2)
        bodyHedge (snapshot promoted) @?= Nothing
        bodyWanted (snapshot promoted) @?= 1
        bodyAttemptPayload (key 2) promoted @?= Just ()
        fst (expireBodyAttempt (key 2) True promoted) @?= NoBodyChange
        bodyFailures (snapshot promoted) @?= 0
    , testCase "old generation fulfillment and failure cannot revive a pruned body" $ do
        let retired = retireBodyAcquisition point gen hedged
            replacement = ensureBodyAcquisition point (AcquisitionGeneration 1) Nothing retired
            oldWinner = fulfillBody point gen (Just (key 2)) replacement
        bodyGeneration (snapshot oldWinner) @?= AcquisitionGeneration 1
        bodyFulfilled (snapshot oldWinner) @?= False
        bodyFailures (snapshot oldWinner) @?= 0
        fst (expireBodyAttempt (key 2) True oldWinner) @?= NoBodyChange
    , testCase "different EBs have independent primary triggers and hedge failure counters" $ do
        let another = point{pointSlotNo = SlotNo 2}
            both = ensureBodyAcquisition another (AcquisitionGeneration 1) Nothing failed
        bodyFailures (fromJust (bodySnapshot another both)) @?= 0
        bodyTriggered (fromJust (bodySnapshot another both)) @?= False
        Map.size (bodySnapshots both) @?= 2
    , testCase "first verified body provenance survives disconnect/durability and is fenced by generation" $ do
        let actual = runSimOrThrow $ do
              h <- right <$> H.newStorageHandoff (H.StorageLimits 2097152 2048 4 4 20000)
              generation <- atomically (H.beginAcquisition h point) >>= pure . right
              session <- atomically (H.openStorageSession h (1 :: Int) (peer 1) 2 1048576) >>= pure . right
              let tx = MkLeiosTx (BS.pack [1, 2, 3, 4])
                  body = mkEndorserBlock [(hashLeiosTx tx, 4)]
                  bodyPoint = MkLeiosPoint (SlotNo 1) (endorserBlockHash body)
              actualGen <- atomically (H.beginAcquisition h bodyPoint) >>= pure . right
              let prepared =
                    right
                      ( prepareBody
                          (BodyDescriptor bodyPoint actualGen (encodeEndorserBlockSize body) (MkLeiosEbBounds 600000 12000000))
                          (EncodedBytes 600003)
                      )
              request <- atomically (H.admitStorageRequest h session prepared) >>= pure . right
              _ <- atomically (H.dispatchStorageRequest h (H.storageRequestKey request))
              _ <- H.acceptBody h request body
              before <- atomically (H.readVerifiedBodySource h bodyPoint actualGen)
              atomically (H.closeStorageSession h session)
              _ <- H.registerDurableBody h (const (pure ())) bodyPoint actualGen body IntSet.empty
              durable <- atomically (H.readVerifiedBodySource h bodyPoint actualGen)
              atomically (H.retireAcquisition h bodyPoint actualGen)
              replacement <- atomically (H.beginAcquisition h bodyPoint) >>= pure . right
              stale <- atomically (H.readVerifiedBodySource h bodyPoint actualGen)
              _ <- H.registerDurableBody h (const (pure ())) bodyPoint replacement body IntSet.empty
              bridged <- atomically (H.readVerifiedBodySource h bodyPoint replacement)
              pure
                ( generation == gen
                    && before == Just (H.BodyFromRequest (H.storageRequestKey request))
                    && before == durable
                    && stale == Nothing
                    && bridged == Just H.BodyFromStorage
                )
        actual @?= True
    , testCase "blocked FIFO rescue starts no queued timer and charges no body hedge failure" $ do
        let queued = rescueBlockedBody (key 2) hedged
            primaryRescue = rescueBlockedBody (key 1) primary
        bodyFailures (snapshot queued) @?= 0
        bodyHedge (snapshot queued) @?= Nothing
        pendingBodyCancels queued @?= [(key 2, ())]
        bodyTriggered (snapshot primaryRescue) @?= True
        bodyPrimary (snapshot primaryRescue) @?= Just (key 1)
        bodyFailures (snapshot primaryRescue) @?= 0
    , testCase "body backoff is 1.5^k until the safe ceiling-saturation bound" $ do
        let config = right (D.mkTimingConfig 1500000 14600 0.5 2 60 (PayloadBytes 600000))
        map (D.bodyBackoffFactor config) [0, 1, 2, 3] @?= [1, 3 / 2, 9 / 4, 27 / 8]
        D.hedgeAllowance 1 (D.bodyBackoffFactor config maxBound) 2 @?= 2
        D.hedgeAllowance 3 (D.bodyBackoffFactor config maxBound) 2 @?= 3
    , testProperty
        "arbitrary admissions, repeated expiry and late settlement keep at most two wanted copies"
        $ \events ->
          let states = scanl step initial (take 250 (events :: [Word8]))
           in conjoin [counterexample (show (snapshot st)) (bodyWanted (snapshot st) <= 2) | st <- states]
    ]
 where
  step state event = case event `mod` 6 of
    0 -> case (nextBodyRole point state, bodyCandidates point state) of
      (Just role, candidate : _) ->
        let MkLeiosPeerLifetimeId n _ = candidate
         in right (admitBodyAttempt point gen role (key n) () state)
      _ -> state
    1 ->
      maybe
        state
        (\original -> snd (expireBodyAttempt original True state))
        (bodyPrimary (snapshot state))
    2 ->
      maybe state (\original -> snd (expireBodyAttempt original True state)) (bodyHedge (snapshot state))
    3 ->
      foldl'
        ( \st (original, _) -> case recordBodyCancel original (nextAck original) st of Right next -> next; Left _ -> st
        )
        state
        (pendingBodyCancels state)
    4 -> foldl' (flip finishBodyOriginal) state (map key [1 .. 4])
    _ -> foldl' (flip finishBodyCancel) state (map ack [1 .. 4])
  nextAck (RequestKey lifetime session _) = RequestKey lifetime session (WireOrdinal 1)
