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
--
-- The third announcement module, 'LeiosDemoLogic.Announcements.Validate', is
-- deliberately out of scope for this specific test suite:
-- 'validateAnnouncementHeader' needs a concrete block (@LedgerSupportsProtocol@
-- + @ResolveLeiosBlock@, plus forecasting and header-protocol validation), so
-- it is exercised by the proto-devnet rather than here.
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
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Data.Void (Void)
import Data.Word (Word64, Word8)
import LeiosDemoLogic.Announcements
import LeiosDemoLogic.Announcements.ElBimap
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck
  ( Gen
  , Property
  , QuickCheckTests (..)
  , chooseInt
  , forAll
  , listOf
  , oneof
  , testProperty
  , (===)
  )

tests :: TestTree
tests =
  -- These properties are cheap; run 1000x the usual repetitions.
  adjustOption (\(QuickCheckTests n) -> QuickCheckTests (n * 1000)) $
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
    , testCase "DoNotRelay skips the queue and subsequent DoRelay can't retcon that" test_noRelay
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
    [ testProperty "keeps exactly the elections at or above the immutable tip" prop_prune
    ]

-- | 'prunePeerState' keeps precisely the elections whose slot is at or above the
-- immutable tip, leaving each such election's announcements untouched.
prop_prune :: Property
prop_prune = forAll genStream $ \ops -> forAll genSlot $ \s ->
  let st = buildState [Anc e t | (e, t) <- ops]
      tagsOf ps = Map.map elStateTags (live ps)
   in tagsOf (prunePeerState (SlotNo s) st)
        === Map.filterWithKey (\e _ -> elSlot e >= s) (tagsOf st)

{-------------------------------------------------------------------------------
  ElBimap
-------------------------------------------------------------------------------}

elBimapTests :: TestTree
elBimapTests =
  testGroup
    "ElBimap"
    [ testProperty "forward half is a plain Map ElId (NESet a)" prop_bimapForwardModel
    , testProperty "inverse half is a plain Map a (NESet ElId)" prop_bimapInverseModel
    ]

-- | An immutable-tip slot spanning the range of generated election slots (so
-- prune boundaries are exercised). Used by 'prop_prune'.
genSlot :: Gen Word64
genSlot = fromIntegral <$> chooseInt (0, 4)

-- | The forward half's semantics, implemented directly on a plain 'Map'.
applyFwd :: Map.Map ElId (NESet Int) -> BOp -> Map.Map ElId (NESet Int)
applyFwd m = \case
  Ins l r -> Map.insertWith NESet.union l (NESet.singleton r) m
  DelL l -> Map.delete l m
  DelR r -> Map.mapMaybe (NESet.nonEmptySet . NESet.delete r) m
  Prune s -> Map.filterWithKey (\l _ -> elSlot l >= s) m

-- | The inverse half's semantics, implemented directly on a plain 'Map'.
applyInv :: Map.Map Int (NESet ElId) -> BOp -> Map.Map Int (NESet ElId)
applyInv m = \case
  Ins l r -> Map.insertWith NESet.union r (NESet.singleton l) m
  DelL l -> Map.mapMaybe (NESet.nonEmptySet . NESet.delete l) m
  DelR r -> Map.delete r m
  Prune s -> Map.mapMaybe (NESet.nonEmptySet . NESet.filter (\l -> elSlot l >= s)) m

-- | The forward half is indistinguishable from a plain @Map ElId (NESet a)@
-- maintained directly, under any mix of inserts, deletes, and prunes.
prop_bimapForwardModel :: Property
prop_bimapForwardModel = forAll (listOf genBOp) $ \ops ->
  forwardHalf (foldl applyBOp emptyElBimap ops) === foldl applyFwd Map.empty ops

-- | The inverse half is indistinguishable from a plain @Map a (NESet ElId)@
-- maintained directly, under any mix of inserts, deletes, and prunes.
prop_bimapInverseModel :: Property
prop_bimapInverseModel = forAll (listOf genBOp) $ \ops ->
  inverseHalf (foldl applyBOp emptyElBimap ops) === foldl applyInv Map.empty ops

data BOp = Ins ElId Int | DelL ElId | DelR Int | Prune Word64
  deriving Show

applyBOp :: ElBimap Int -> BOp -> ElBimap Int
applyBOp bm = \case
  Ins l r -> insertElBimap l r bm
  DelL l -> deleteElBimapL l bm
  DelR r -> deleteElBimapR r bm
  Prune s -> pruneElBimap (SlotNo s) bm

genBOp :: Gen BOp
genBOp =
  oneof
    [ Ins <$> genElId <*> chooseInt (0, 3)
    , DelL <$> genElId
    , DelR <$> chooseInt (0, 3)
    , Prune . fromIntegral <$> chooseInt (0, 4)
    ]
