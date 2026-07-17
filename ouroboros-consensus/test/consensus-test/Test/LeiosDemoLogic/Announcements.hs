{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the block-agnostic announcement logic in
-- 'LeiosDemoLogic.Announcements' and 'LeiosDemoLogic.Announcements.ElBimap':
-- per-peer equivocation counting ('extendLive'\/'onAnnouncement'), the
-- node-wide dedup + relay ('onAnnouncementCentral'), pruning, and the
-- bidirectional election map.
--
-- Everything here is exercised through a trivial mock announcement type
-- ('Anc'), so the tests depend on none of the block\/ledger machinery — which
-- is the point of keeping that logic polymorphic.
module Test.LeiosDemoLogic.Announcements (tests) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict.TVar
  ( StrictTVar
  , newTVarIO
  , readTVarIO
  )
import Control.Monad.Except (runExceptT)
import Control.Tracer (nullTracer)
import qualified Data.ByteString.Short as SBS
import Data.IORef
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Data.Word (Word64, Word8)
import LeiosDemoLogic.Announcements
import LeiosDemoLogic.Announcements.ElBimap
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Gen
  , Property
  , chooseInt
  , counterexample
  , forAll
  , listOf
  , oneof
  , testProperty
  , (===)
  )

tests :: TestTree
tests =
  testGroup
    "Announcements"
    [ extendLiveTests
    , onAnnouncementTests
    , centralTests
    , pruneTests
    , elBimapTests
    ]

{-------------------------------------------------------------------------------
  Mock announcement type
-------------------------------------------------------------------------------}

-- | A mock announcement: its election plus a tag distinguishing announcements
-- for the same election. Two 'Anc's with the same election but different tags
-- are an equivocation; identical 'Anc's are a repeat.
data Anc = Anc !ElId !Int
  deriving (Eq, Show)

ancEl :: Anc -> ElId
ancEl (Anc e _) = e

ancTag :: Anc -> Int
ancTag (Anc _ i) = i

-- | An election at the given slot for the given (one-byte) pool id.
mkEl :: Word64 -> Word8 -> ElId
mkEl slot poolId = MkElId (SlotNo slot) (SBS.pack [poolId])

elSlot :: ElId -> Word64
elSlot (MkElId (SlotNo s) _) = s

{-------------------------------------------------------------------------------
  extendLive
-------------------------------------------------------------------------------}

-- | A comparable summary of an 'extendLive' outcome. 'St' lists the tags of the
-- election's stored announcements (in order).
data Ext = ErrR | ErrT | St [Int]
  deriving (Eq, Show)

step :: PeerState Anc -> Anc -> Either (ErrAnnouncement Void) (ElState Anc, PeerState Anc)
step st a = extendLive (ancEl a) a st

classifyExt :: Either (ErrAnnouncement Void) (ElState Anc, PeerState Anc) -> Ext
classifyExt = \case
  Left ErrRepeat -> ErrR
  Left ErrThird -> ErrT
  Right (elSt, _) -> St (elStateTags elSt)

elStateTags :: ElState Anc -> [Int]
elStateTags elSt =
  ancTag (firstAnnouncement elSt) : maybe [] (pure . ancTag) (secondAnnouncement elSt)

-- | Fold announcements into a 'PeerState', keeping successful extensions and
-- silently dropping rejected ones.
buildState :: [Anc] -> PeerState Anc
buildState = foldl (\st a -> either (const st) snd (step st a)) emptyPeerState

el0 :: ElId
el0 = mkEl 10 0

extendLiveTests :: TestTree
extendLiveTests =
  testGroup
    "extendLive"
    [ testCase "first announcement is accepted" $
        classifyExt (step emptyPeerState (Anc el0 0)) @?= St [0]
    , testCase "identical repeat is ErrRepeat" $
        classifyExt (step (buildState [Anc el0 0]) (Anc el0 0)) @?= ErrR
    , testCase "second distinct announcement equivocates (kept as two)" $
        classifyExt (step (buildState [Anc el0 0]) (Anc el0 1)) @?= St [0, 1]
    , testCase "third announcement is ErrThird" $
        classifyExt (step (buildState [Anc el0 0, Anc el0 1]) (Anc el0 2)) @?= ErrT
    , testCase "repeat of one of two is still ErrThird" $
        classifyExt (step (buildState [Anc el0 0, Anc el0 1]) (Anc el0 0)) @?= ErrT
    , testCase "distinct elections are independent" $
        Set.fromList (map elSlot (Map.keys (live (buildState [Anc (mkEl 10 0) 0, Anc (mkEl 20 0) 0]))))
          @?= Set.fromList [10, 20]
    , testProperty "stored announcements are the first <=2 distinct tags, in order" $
        prop_extendLiveModel
    ]

-- | Folding a random announcement stream leaves, for each election, exactly the
-- first at-most-two distinct tags in order of first appearance.
prop_extendLiveModel :: Property
prop_extendLiveModel = forAll genStream $ \ops ->
  let actual = Map.map elStateTags (live (buildState [Anc e t | (e, t) <- ops]))
      model = Map.map (take 2 . nub) (Map.fromListWith (\new old -> old ++ new) [(e, [t]) | (e, t) <- ops])
   in actual === model

genElId :: Gen ElId
genElId = mkEl <$> (fromIntegral <$> chooseInt (1, 3)) <*> (fromIntegral <$> chooseInt (0, 1))

genStream :: Gen [(ElId, Int)]
genStream = listOf ((,) <$> genElId <*> chooseInt (0, 3))

{-------------------------------------------------------------------------------
  onAnnouncement (per-peer wrapper: count, then validate, then process)
-------------------------------------------------------------------------------}

onAnnouncementTests :: TestTree
onAnnouncementTests =
  testGroup
    "onAnnouncement"
    [ testCase "invalid announcement surfaces as ErrInvalid" test_oaInvalid
    , testCase "valid announcement runs process and returns new state" test_oaProcess
    , testCase "process does not run for an invalid announcement" test_oaNoProcessOnInvalid
    , testCase "repeat is rejected without running process" test_oaRepeat
    ]

test_oaInvalid :: Assertion
test_oaInvalid = do
  res <-
    runExceptT $
      onAnnouncement nullTracer ancEl (\_ -> pure (Left "bad") :: IO (Either String ())) (\_ _ -> pure ()) emptyPeerState (Anc el0 0)
  case res of
    Left (ErrInvalid inv) -> inv @?= "bad"
    _ -> assertFailure "expected ErrInvalid"

test_oaProcess :: Assertion
test_oaProcess = do
  ref <- newIORef []
  res <-
    runExceptT $
      onAnnouncement nullTracer ancEl (\_ -> pure (Right ())) (\a () -> modifyIORef' ref (a :)) emptyPeerState (Anc el0 0)
  readIORef ref >>= (@?= [Anc el0 0])
  case res of
    Right _ -> pure ()
    Left _ -> assertFailure "expected success"

test_oaNoProcessOnInvalid :: Assertion
test_oaNoProcessOnInvalid = do
  ref <- newIORef (0 :: Int)
  _ <-
    runExceptT $
      onAnnouncement nullTracer ancEl (\_ -> pure (Left ()) :: IO (Either () ())) (\_ _ -> modifyIORef' ref (+ 1)) emptyPeerState (Anc el0 0)
  readIORef ref >>= (@?= 0)

test_oaRepeat :: Assertion
test_oaRepeat = do
  ref <- newIORef (0 :: Int)
  let validate = \_ -> pure (Right ())
      process = \_ _ -> modifyIORef' ref (+ 1) :: IO ()
      go st = runExceptT $ onAnnouncement nullTracer ancEl validate process st (Anc el0 0)
  Right st1 <- go emptyPeerState
  res2 <- go st1
  case res2 of
    Left ErrRepeat -> pure ()
    _ -> assertFailure "expected ErrRepeat"
  readIORef ref >>= (@?= 1) -- process ran once (the first, not the repeat)

{-------------------------------------------------------------------------------
  onAnnouncementCentral (node-wide dedup + relay)
-------------------------------------------------------------------------------}

-- | A downstream peer's queue: a credit counter and a FIFO of relayed
-- announcements, plus readers for both.
data TestQueue = TestQueue
  { tqView :: QueueAnnouncementView IO Anc
  , tqRead :: IO [Anc]
  , tqCredits :: IO Int
  }

newQueue :: Int -> IO TestQueue
newQueue credits = do
  free <- newTVarIO credits :: IO (StrictTVar IO Int)
  q <- newTVarIO [] :: IO (StrictTVar IO [Anc])
  pure
    TestQueue
      { tqView = MkQueueAnnouncementView free (\xs a -> xs ++ [a]) q
      , tqRead = readTVarIO q
      , tqCredits = readTVarIO free
      }

-- | Central-state helper: relay from a given source with a given 'ShouldRelay'.
central ::
  Maybe Int ->
  ShouldRelay ->
  Anc ->
  CentralState IO Int Anc ->
  IO (CentralState IO Int Anc)
central src rel anc st =
  onAnnouncementCentral nullTracer ancEl (\_ -> pure ()) st src rel anc

centralTests :: TestTree
centralTests =
  testGroup
    "onAnnouncementCentral"
    [ testCase "new announcement is relayed, credit spent, peer gated" test_relay
    , testCase "duplicate announcement is a no-op" test_dedup
    , testCase "DoNotRelay skips the queue but still dedups" test_noRelay
    , testCase "no relay when the peer has no credits" test_noCredit
    , testCase "equivocation goes only to peers already sent the first" test_equivocation
    , testCase "publishLocally fires once per new announcement" test_publish
    ]

test_relay :: Assertion
test_relay = do
  tq <- newQueue 5
  let st0 = insertPeerCentral 1 (tqView tq) emptyCentralState
  st1 <- central (Just 2) DoRelay (Anc el0 0) st0
  tqRead tq >>= (@?= [Anc el0 0])
  tqCredits tq >>= (@?= 4)
  Set.toList (lookupElBimapL el0 (gate st1)) @?= [1]

test_dedup :: Assertion
test_dedup = do
  tq <- newQueue 5
  let st0 = insertPeerCentral 1 (tqView tq) emptyCentralState
  st1 <- central (Just 2) DoRelay (Anc el0 0) st0
  _ <- central (Just 2) DoRelay (Anc el0 0) st1 -- same announcement again
  tqRead tq >>= (@?= [Anc el0 0]) -- not relayed twice
  tqCredits tq >>= (@?= 4) -- credit spent only once

test_noRelay :: Assertion
test_noRelay = do
  tq <- newQueue 5
  let st0 = insertPeerCentral 1 (tqView tq) emptyCentralState
  st1 <- central Nothing DoNotRelay (Anc el0 0) st0 -- e.g. self-forged, too old to relay
  tqRead tq >>= (@?= []) -- nothing sent
  _st2 <- central (Just 2) DoRelay (Anc el0 0) st1 -- same announcement, now DoRelay
  tqRead tq >>= (@?= []) -- still nothing: DoNotRelay had recorded it in selfPeer

test_noCredit :: Assertion
test_noCredit = do
  tq <- newQueue 0
  let st0 = insertPeerCentral 1 (tqView tq) emptyCentralState
  st1 <- central (Just 2) DoRelay (Anc el0 0) st0
  tqRead tq >>= (@?= []) -- dropped for lack of credits
  Set.toList (lookupElBimapL el0 (gate st1)) @?= [] -- and not gated

test_equivocation :: Assertion
test_equivocation = do
  tqA <- newQueue 5
  tqB <- newQueue 5
  let stA = insertPeerCentral 1 (tqView tqA) emptyCentralState
  st1 <- central (Just 9) DoRelay (Anc el0 0) stA -- peer 1 gets the first
  let stAB = insertPeerCentral 2 (tqView tqB) st1 -- peer 2 joins afterwards
  _ <- central (Just 9) DoRelay (Anc el0 1) stAB -- the equivocating second
  tqRead tqA >>= (@?= [Anc el0 0, Anc el0 1]) -- peer 1: first + equivocation proof
  tqRead tqB >>= (@?= []) -- peer 2: never saw the first, so not the proof

test_publish :: Assertion
test_publish = do
  ref <- newIORef (0 :: Int)
  tq <- newQueue 5
  let st0 = insertPeerCentral 'A' (tqView tq) emptyCentralState
      pub = \_ -> modifyIORef' ref (+ 1)
  st1 <- onAnnouncementCentral nullTracer ancEl pub st0 (Just 'B') DoRelay (Anc el0 0)
  _ <- onAnnouncementCentral nullTracer ancEl pub st1 (Just 'B') DoRelay (Anc el0 0) -- duplicate
  readIORef ref >>= (@?= 1)

{-------------------------------------------------------------------------------
  pruning
-------------------------------------------------------------------------------}

pruneTests :: TestTree
pruneTests =
  testGroup
    "prunePeerState"
    [ testCase "drops elections strictly below the immutable tip" $
        let st = buildState [Anc (mkEl s 0) 0 | s <- [10, 20, 30]]
         in Set.fromList (map elSlot (Map.keys (live (prunePeerState (SlotNo 20) st))))
              @?= Set.fromList [20, 30]
    ]

{-------------------------------------------------------------------------------
  ElBimap
-------------------------------------------------------------------------------}

elBimapTests :: TestTree
elBimapTests =
  testGroup
    "ElBimap"
    [ testCase "insert then look up both directions" test_bimapRoundtrip
    , testCase "deleteElBimapR removes the right key from both halves" test_bimapDeleteR
    , testCase "pruneElBimap drops old elections from both halves" test_bimapPrune
    , testProperty "the two halves always agree" prop_bimapConsistent
    ]

test_bimapRoundtrip :: Assertion
test_bimapRoundtrip = do
  let bm =
        insertElBimap (mkEl 1 0) (10 :: Int) $
          insertElBimap (mkEl 1 0) 11 $
            insertElBimap (mkEl 2 0) 10 emptyElBimap
  lookupElBimapL (mkEl 1 0) bm @?= Set.fromList [10, 11]
  lookupElBimapR 10 bm @?= Set.fromList [mkEl 1 0, mkEl 2 0]
  lookupElBimapR 11 bm @?= Set.fromList [mkEl 1 0]

test_bimapDeleteR :: Assertion
test_bimapDeleteR = do
  let bm =
        deleteElBimapR 10 $
          insertElBimap (mkEl 1 0) (10 :: Int) $
            insertElBimap (mkEl 1 0) 11 emptyElBimap
  lookupElBimapL (mkEl 1 0) bm @?= Set.fromList [11]
  lookupElBimapR 10 bm @?= Set.empty

test_bimapPrune :: Assertion
test_bimapPrune = do
  let bm =
        pruneElBimap (SlotNo 10) $
          insertElBimap (mkEl 5 0) (1 :: Int) $
            insertElBimap (mkEl 15 0) 1 emptyElBimap
  lookupElBimapL (mkEl 5 0) bm @?= Set.empty
  lookupElBimapL (mkEl 15 0) bm @?= Set.fromList [1]
  lookupElBimapR 1 bm @?= Set.fromList [mkEl 15 0]

data BOp = Ins ElId Int | DelL ElId | DelR Int | Prune Word64
  deriving Show

applyBOp :: ElBimap Int -> BOp -> ElBimap Int
applyBOp bm = \case
  Ins l r -> insertElBimap l r bm
  DelL l -> deleteElBimapL l bm
  DelR r -> deleteElBimapR r bm
  Prune s -> pruneElBimap (SlotNo s) bm

-- | @r@ is in the forward image of @l@ iff @l@ is in the inverse image of @r@.
prop_bimapConsistent :: Property
prop_bimapConsistent = forAll (listOf genBOp) $ \ops ->
  let bm = foldl applyBOp emptyElBimap ops
      fwdOK =
        all
          (\l -> all (\r -> l `Set.member` lookupElBimapR r bm) (lookupElBimapL l bm))
          (Map.keys (forwardHalf bm))
      invOK =
        all
          (\r -> all (\l -> r `Set.member` lookupElBimapL l bm) (lookupElBimapR r bm))
          (Map.keys (inverseHalf bm))
   in counterexample "halves disagree" (fwdOK && invOK)

genBOp :: Gen BOp
genBOp =
  oneof
    [ Ins <$> genElId <*> chooseInt (0, 3)
    , DelL <$> genElId
    , DelR <$> chooseInt (0, 3)
    , Prune . fromIntegral <$> chooseInt (0, 4)
    ]
