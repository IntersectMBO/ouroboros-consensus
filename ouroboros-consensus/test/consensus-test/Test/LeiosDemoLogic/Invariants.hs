{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Sequence-level invariant tests for the Leios fetch state.
--
-- The sibling "Test.LeiosDemoLogic" checks that the /pure/ decision function
-- makes the right choice at a single instant. This module instead drives the
-- /real, effectful/ handlers ('processLeiosBlock', 'processLeiosBlockTxs',
-- 'recordAnnouncedEb', 'leiosFetchLogicIteration') over sequences of interleaved
-- message arrivals and decisions, in 'IOSim' against an in-memory 'LeiosDb', a
-- 'nullLeiosTxCache', and plain 'MVar's — then asserts that a state invariant
-- holds after every step.
--
-- NOTE. The EbTxs side of the fetch logic is being rewritten from scratch, so the
-- old missing-tx \/ reverse-index regression is gone. What remains checks the
-- EB-body side: after each command the 'ebState' reverse-index invariant must
-- hold, and — belt and suspenders — a 'Decide' is forced through the real fetch
-- logic so a stray @impossible!@ surfaces.
--
-- NOTE. A second regression lives here too: the fetch logic must never request
-- an EB body it already holds. That storm — a held body being re-listed and
-- re-requested — is what 'prop_neverRefetchesHeldBody' guards against; after each
-- 'Decide' it checks that no body just requested is one we already hold.
-- Phrasing it as "already held" rather than a request count keeps it correct if
-- 'maxRequestsPerEb' rises above 1: requesting a not-yet-held body from several
-- peers is fine; re-requesting a held one is not.
module Test.LeiosDemoLogic.Invariants (tests) where

import Cardano.Slotting.Slot (SlotNo (SlotNo))
import Control.Concurrent.Class.MonadMVar
  ( MVar
  , modifyMVar_
  , newEmptyMVar
  , newMVar
  , readMVar
  )
import Control.Monad.Class.MonadAsync (concurrently_)
import Control.Monad.Class.MonadTest (exploreRaces)
import Control.Monad.Class.MonadThrow (SomeException, try)
import Control.Monad.IOSim (IOSim, exploreSimTrace, runSimOrThrow, traceResult)
import Control.Tracer (nullTracer)
import qualified Data.ByteString as BS
import qualified Data.DList as DList
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Vector.Strict as V
import Data.Void (Void, absurd)
import LeiosDemoDb (withLeiosDb)
import qualified LeiosDemoDb as LeiosDb
import LeiosDemoLogic
  ( AlsoOfferedTxsClosure (..)
  , LeiosBlockSource (..)
  , LeiosBlockTxsSource (..)
  , LeiosFetchDecisions (..)
  , leiosFetchLogicIteration
  , processLeiosBlock
  , processLeiosBlockTxs
  , recordAnnouncedEb
  , recordEbBodyOffer
  )
import LeiosDemoTypes
  ( BytesSize
  , EbHash
  , LeiosBlockRequest (..)
  , LeiosEb (..)
  , LeiosOutstanding (..)
  , LeiosPeerVars
  , LeiosPoint (..)
  , LeiosTx (..)
  , PeerId (..)
  , TxHash
  , demoLeiosFetchStaticEnv
  , emptyLeiosOutstanding
  , hashLeiosEb
  , hashLeiosTx
  , leiosEbBytesSize
  , newLeiosPeerVars
  )
import qualified LeiosDemoTypes as Leios
import qualified LeiosDemoTypes.LeiosJobs as Jobs
import LeiosTxCache (LeiosTxCache, newPureLeiosTxCache, nullLeiosTxCache)
import Ouroboros.Consensus.Util.IOLike (IOLike, evaluate)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Orphans.IOLike ()
import Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  -- 10x whatever '--quickcheck-tests' supplies, for every property below.
  adjustQuickCheckTests (* 10) $ testGroup
    "LeiosDemoLogic.Invariants"
    [ testGroup
        "curated sequences"
        [ testCase "forge purges a body it already holds (offered first)" $
            runCmdsReFetchViolations reproForgeAfterOffer @?= Right []
        ]
    , testCase "acquired EB kept until its greatest slot is below the immutable tip" $ do
        let eb = ebOf [0, 1]
            h = hashLeiosEb eb
            pool = Jobs.mkLeiosJobPool 1000 10 mempty -- an empty pool suffices here
            -- announce at slot 5, then again at the smaller slot 3, and acquire
            o =
              Leios.insertAcquiredEbBody h eb pool $
                Leios.recordMaxAnnouncementSlot h (SlotNo 3) $
                  Leios.recordMaxAnnouncementSlot h (SlotNo 5) $
                    (emptyLeiosOutstanding (SlotNo 0) :: LeiosOutstanding Int)
        -- the greater slot is retained, not the last-recorded one
        Map.lookup h (Leios.ebState o) @?= Just (Leios.MkEbState (SlotNo 5) (Leios.BodyAcquired eb pool))
        -- kept while the greatest slot (5) is at/above the immutable tip (4)
        Map.lookup h (Leios.ebState (Leios.pruneOutstandingToImmTip (SlotNo 4) o))
          @?= Just (Leios.MkEbState (SlotNo 5) (Leios.BodyAcquired eb pool))
        -- dropped once the greatest slot (5) is below the immutable tip (6)
        Map.lookup h (Leios.ebState (Leios.pruneOutstandingToImmTip (SlotNo 6) o))
          @?= Nothing
    , testCase "prune drops below-tip missing-body points and keeps the reverse index in sync" $ do
        let hA = hashLeiosEb (ebOf [0, 1]) -- to be listed at slots 3 and 10
            hB = hashLeiosEb (ebOf [2, 3]) -- to be listed at slot 3 only
            pointAt slot h = MkLeiosPoint (SlotNo slot) h
            o0 :: LeiosOutstanding Int
            o0 =
              (emptyLeiosOutstanding (SlotNo 0))
                { Leios.missingEbBodies =
                    Map.fromList [(pointAt 3 hA, 10), (pointAt 10 hA, 10), (pointAt 3 hB, 20)]
                , Leios.reverseSlotIndexByEbHash =
                    Map.fromList
                      [ (hA, NESet.insert (SlotNo 3) (NESet.singleton (SlotNo 10)))
                      , (hB, NESet.singleton (SlotNo 3))
                      ]
                }
            o = Leios.pruneOutstandingToImmTip (SlotNo 5) o0
        -- hA's slot-3 point is dropped, its slot-10 point kept
        Map.lookup (pointAt 3 hA) (Leios.missingEbBodies o) @?= Nothing
        Map.lookup (pointAt 10 hA) (Leios.missingEbBodies o) @?= Just 10
        -- hB was listed only at slot 3, so it drops out entirely
        Map.lookup (pointAt 3 hB) (Leios.missingEbBodies o) @?= Nothing
        Map.size (Leios.missingEbBodies o) @?= 1
        -- the reverse index stays the exact inverse: hA at slot 10 only, hB gone
        Map.lookup hA (Leios.reverseSlotIndexByEbHash o) @?= Just (NESet.singleton (SlotNo 10))
        Map.lookup hB (Leios.reverseSlotIndexByEbHash o) @?= Nothing
    , testProperty
        "ebState stays in sync with ebsPerMaxAnnouncementSlot across arbitrary sequences"
        prop_invariants
    , testProperty
        "the fetch logic never requests an already-held EB body"
        prop_neverRefetchesHeldBody
    , testProperty
        "a concurrent offer and body arrival never leave a held EB body listed (IOSimPOR)"
        prop_neverRefetchesHeldBodyConcurrent
    ]

------------------------------------------------------------
-- Commands
------------------------------------------------------------

-- | A test EB is a list of (globally distinct) tx ids; the same list means the
-- same 'LeiosEb', hence the same 'EbHash' — so the same EB announced at two
-- slots is genuinely one hash at two 'LeiosPoint's (the crash arming).
type TestEb = [Int]

data Cmd
  = -- | @recordAnnouncedEb@: announce this EB at this slot.
    Announce TestEb Word
  | -- | @recordEbBodyOffer@: a peer offers this EB body at this slot.
    Offer TestEb Word
  | -- | @processLeiosBlock@: the EB body arrives for that point.
    ArriveBody TestEb Word
  | -- | Disarmed: the EbTxs side is being rewritten from scratch, so tx delivery
    -- has no command for now (uninhabited).
    ArriveTx Void
  | -- | @leiosFetchLogicIteration@ at this current slot.
    Decide Word
  | -- | The forge produces this EB: drives 'processLeiosBlock'/'processLeiosBlockTxs'
    -- with 'ForgedBlock'/'ForgedTxs' (as 'onForgedLeiosEb' does), reconciling the
    -- outstanding state exactly as a remote acquisition would.
    Forge TestEb Word
  deriving (Eq, Show)

------------------------------------------------------------
-- Self-consistent EB/tx construction
--
-- The handlers validate @hashLeiosEb eb == ebHash@ and @hashLeiosTx tx ==
-- txHash@, so we derive hashes from the bytes rather than inventing them.
------------------------------------------------------------

txBytesOf :: Int -> BS.ByteString
txBytesOf i = BS.pack (fromIntegral (i + 1) : replicate 31 0)

leiosTxOf :: Int -> LeiosTx
leiosTxOf = MkLeiosTx . txBytesOf

txHashOf :: Int -> TxHash
txHashOf = hashLeiosTx . leiosTxOf

txSizeOf :: Int -> BytesSize
txSizeOf = fromIntegral . BS.length . txBytesOf

ebOf :: TestEb -> LeiosEb
ebOf ids = MkLeiosEb (V.fromList [(txHashOf i, txSizeOf i) | i <- ids])

pointOf :: TestEb -> Word -> LeiosPoint
pointOf ids slot = MkLeiosPoint (fromIntegral slot) (hashLeiosEb (ebOf ids))

------------------------------------------------------------
-- Harness
------------------------------------------------------------

-- | Run a command sequence in 'IOSim' against in-memory dependencies, checking
-- the invariant after each command. 'Left' names the first failing command.
runCmds :: [Cmd] -> Either String ()
runCmds = (() <$) . runCmdsReFetchViolations

-- | Like 'runCmds', but on success also return the EB bodies that the fetch
-- logic requested despite already holding them (i.e. despite 'ebStateHasBody'),
-- gathered across all 'Decide's. That list is the
-- re-fetch-storm regression signal: it must be empty. See
-- 'prop_neverRefetchesHeldBody'.
runCmdsReFetchViolations :: [Cmd] -> Either String [EbHash]
runCmdsReFetchViolations cmds = runSimOrThrow (go cmds)
 where
  go :: forall s. [Cmd] -> IOSim s (Either String [EbHash])
  go cs0 = do
    dbHandle <- LeiosDb.newLeiosDBInMemory
    withLeiosDb dbHandle $ \conn -> do
      outstandingVar <- newMVar (emptyLeiosOutstanding (SlotNo 0))
      readyVar <- newEmptyMVar
      peerVars <- newLeiosPeerVars
      let kv = (outstandingVar, readyVar)
          txCache = nullLeiosTxCache
          peerId = MkPeerId (0 :: Int)
          loop acc [] = pure (Right acc)
          loop acc (c : cs) = do
            r <-
              try (applyCmd conn txCache kv peerVars peerId c)
                :: IOSim s (Either SomeException [EbHash])
            case r of
              Left e -> pure (Left ("exception on " <> show c <> ": " <> show e))
              Right violations -> do
                outstanding <- readMVar outstandingVar
                case checkInvariant outstanding of
                  Left msg -> pure (Left (msg <> " (after " <> show c <> ")"))
                  Right () -> loop (acc <> violations) cs
      loop [] cs0

-- | Apply a command, returning any EB bodies it requested that are already held
-- (per 'ebStateHasBody') — the re-fetch-storm violation. Empty for everything
-- but a misbehaving 'Decide'.
applyCmd ::
  forall s.
  LeiosDb.LeiosDbConnection (IOSim s) ->
  LeiosTxCache (IOSim s) () () Leios.SerializedEbBody ->
  (MVar (IOSim s) (LeiosOutstanding Int), MVar (IOSim s) ()) ->
  LeiosPeerVars (IOSim s) ->
  PeerId Int ->
  Cmd ->
  IOSim s [EbHash]
applyCmd conn txCache kv peerVars peerId = \case
  Announce ids slot -> do
    recordAnnouncedEb kv (pointOf ids slot, leiosEbBytesSize (ebOf ids))
    pure []
  Offer ids slot -> do
    recordEbBodyOffer kv peerVars TxsClosureNotAlsoOffered (pointOf ids slot, leiosEbBytesSize (ebOf ids))
    pure []
  ArriveBody ids slot -> do
    let eb = ebOf ids
        req = MkLeiosBlockRequest (pointOf ids slot) (leiosEbBytesSize eb)
    processLeiosBlock nullTracer nullTracer kv txCache conn (ReceivedBlockFrom peerId req) eb
    pure []
  ArriveTx v -> absurd v
  Forge ids slot -> do
    let eb = ebOf ids
        point = pointOf ids slot
    -- The outstanding-state half of 'onForgedLeiosEb'; the announcement it also
    -- makes doesn't touch 'outstanding' for a 'ForgedLocally' source.
    processLeiosBlock nullTracer nullTracer kv txCache conn (ForgedBlock point) eb
    processLeiosBlockTxs nullTracer nullTracer kv txCache conn (ForgedTxs point eb) (V.fromList (map leiosTxOf ids))
    pure []
  Decide slot -> do
    outstanding <- readMVar (fst kv)
    let ebs = referencedEbs outstanding
        offerings = Map.singleton peerId (ebs, ebs)
        (out', decs) =
          leiosFetchLogicIteration demoLeiosFetchStaticEnv (Just (fromIntegral slot)) offerings outstanding
    -- Force the fetch logic so any 'impossible!' surfaces (caught by 'go').
    -- Forcing @out'@ to WHNF drives 'go1' to completion (its reverse lookups);
    -- 'forceDecisions' additionally forces the per-request offset lookups.
    _ <- evaluate out'
    _ <- evaluate (forceDecisions decs)
    modifyMVar_ (fst kv) (\_ -> pure out')
    -- Regression: the fetch logic must not request a body we already hold.
    -- Return any it did (empty when well-behaved).
    let held = Map.keysSet (Map.filter Leios.ebStateHasBody (Leios.ebState outstanding))
    pure (filter (\h -> Set.member h held) (ebBodyRequestHashes decs))

-- | Every EbHash currently referenced by the outstanding state (bodies only, now
-- that the EbTxs side is disarmed), as an all-offering peer's body\/closure sets.
referencedEbs :: LeiosOutstanding Int -> Set.Set EbHash
referencedEbs o =
  Set.fromList $
    map (.pointEbHash) (Map.keys (Leios.missingEbBodies o))

-- | Force the decision structure, including each tx request's resolved offset
-- (the @goTx2@ lookup), to a scalar.
forceDecisions :: LeiosFetchDecisions pid -> Int
forceDecisions (MkLeiosFetchDecisions m) =
  sum
    [ offset + fromIntegral sz
    | slotMap <- Map.elems m
    , (txs, _ebs) <- Map.elems slotMap
    , (_txHash, sz, _ebHash, offset) <- DList.toList txs
    ]

-- | The 'EbHash'es a decision set issues an EB-body fetch request for (one entry
-- per request; the fetch logic caps this at 'maxRequestsPerEb' per EB).
ebBodyRequestHashes :: LeiosFetchDecisions pid -> [EbHash]
ebBodyRequestHashes (MkLeiosFetchDecisions m) =
  [ ebHash
  | slotMap <- Map.elems m
  , (_txs, ebReqs) <- Map.elems slotMap
  , (ebHash, _sz) <- DList.toList ebReqs
  ]

------------------------------------------------------------
-- The invariant
------------------------------------------------------------

-- | 'ebsPerMaxAnnouncementSlot' must be the exact inverse of the greatest-slot
-- field of 'ebState' (the reverse index 'pruneOutstandingToImmTip' prunes by).
--
-- (The old missing-tx \/ reverse-index invariant is gone with the EbTxs rewrite.)
checkInvariant :: LeiosOutstanding Int -> Either String ()
checkInvariant o =
  if Leios.ebsPerMaxAnnouncementSlot o == inverseOfMax
    then Right ()
    else
      Left
        ( "ebsPerMaxAnnouncementSlot desynced from ebState: "
            <> show (Leios.ebsPerMaxAnnouncementSlot o, inverseOfMax)
        )
 where
  inverseOfMax =
    Map.fromListWith
      NESet.union
      [ (Leios.ebStateMaxSlot s, NESet.singleton h)
      | (h, s) <- Map.toList (Leios.ebState o)
      ]

------------------------------------------------------------
-- Curated repros
------------------------------------------------------------

-- | A peer offers an EB body; we forge the same EB before the offered body
-- arrives. Forging must purge the offered body from 'missingEbBodies' (its
-- 'ebState' now reads 'BodyAcquired'), so the fetch logic never re-requests a body
-- we already hold. Pre-fix the forge recorded the body as acquired without purging,
-- so the 'Decide' re-fetched it.
reproForgeAfterOffer :: [Cmd]
reproForgeAfterOffer =
  [ Offer [0, 1] 10
  , Forge [0, 1] 12
  , Decide 13
  ]

------------------------------------------------------------
-- Property
------------------------------------------------------------

worldEbs :: [TestEb]
worldEbs = [[0, 1], [1, 2], [0], [2, 3, 4]]

worldSlots :: [Word]
worldSlots = [10, 11, 12]

genCmd :: Gen Cmd
genCmd = do
  ids <- elements worldEbs
  slot <- elements worldSlots
  oneof
    [ pure (Announce ids slot)
    , pure (Offer ids slot)
    , pure (ArriveBody ids slot)
    , pure (Forge ids slot)
    , Decide <$> elements worldSlots
    ]

------------------------------------------------------------
-- Coverage
------------------------------------------------------------

isForge :: Cmd -> Bool
isForge Forge{} = True
isForge _ = False

cmdName :: Cmd -> String
cmdName = \case
  Announce{} -> "Announce"
  Offer{} -> "Offer"
  ArriveBody{} -> "ArriveBody"
  ArriveTx{} -> "ArriveTx"
  Forge{} -> "Forge"
  Decide{} -> "Decide"

-- | An EB made known (offer \/ announce \/ body arrival) and later forged: the
-- body forge hazard, where forging must purge the earlier listing.
listedThenForged :: [Cmd] -> Bool
listedThenForged cmds =
  or
    [ Just ids `elem` map listing (take i cmds)
    | (i, Forge ids _) <- zip [0 :: Int ..] cmds
    ]
 where
  listing = \case
    Offer x _ -> Just x
    Announce x _ -> Just x
    ArriveBody x _ -> Just x
    _ -> Nothing

-- | Coverage shared by the generated properties: the command mix, and whether
-- the two forge hazards were actually generated -- so the properties are visibly
-- non-vacuous.
coverage :: Testable prop => [Cmd] -> prop -> Property
coverage cmds prop =
  tabulate "commands" (map cmdName cmds) $
    classify (any isForge cmds) "has a Forge" $
      cover 15 (listedThenForged cmds) "listed then forged (body hazard)" $
        property prop

prop_invariants :: Property
prop_invariants =
  forAllShrink (listOf genCmd) (shrinkList (const [])) $ \cmds ->
    coverage cmds (runCmds cmds === Right ())

-- | Regression for the EB-body re-fetch storm: over any interleaving of
-- announces, offers, and body/tx arrivals, the fetch logic must never request an
-- EB body it already holds (one whose 'ebState' reads 'BodyAcquired'). The storm was precisely
-- this — a held body re-listed and re-requested indefinitely.
--
-- Stated as "already held" rather than a request count, so it stays correct if
-- 'maxRequestsPerEb' rises above 1: requesting a not-yet-held body from several
-- peers is fine; re-requesting a held one is not. (With 'nullLeiosTxCache', the
-- old LeiosTxCache-based "do we have it?" check would see nothing held and
-- re-list/re-request endlessly; the 'ebStateHasBody' check is cache-independent.)
prop_neverRefetchesHeldBody :: Property
prop_neverRefetchesHeldBody =
  forAllShrink (listOf genCmd) (shrinkList (const [])) $ \cmds ->
    coverage cmds $
      case runCmdsReFetchViolations cmds of
        Left msg -> counterexample msg (property False)
        Right violations ->
          counterexample
            ("fetch requested already-held EB bodies: " ++ show violations)
            (null violations)

------------------------------------------------------------
-- Concurrent (IOSimPOR) regression
------------------------------------------------------------

-- Unlike the rest of this module, this scenario calls the handlers directly
-- rather than through the 'Cmd' interpreter ('applyCmd'). Two reasons: a race
-- has no use for 'Decide' -- we assert on the state directly -- and 'applyCmd'
-- runs 'Decide' as a read-then-blind-overwrite that is only sound
-- single-threaded (a concurrent write would be silently clobbered); and spelling
-- the handlers out keeps the lock structure this test exists to probe -- the body
-- arrival's purge-then-acquire, which must be a single 'outstandingVar' critical
-- section even though it also touches the separate cache lock -- in plain view at
-- the race site.

-- | The sequential 'prop_neverRefetchesHeldBody' generates event /sequences/ but
-- runs each handler to completion, so it can't reproduce an interleaving that
-- splits one handler's critical section around a concurrent update to the shared
-- state. This scenario runs three handlers for the /same EB hash at three
-- different slots/ as genuinely concurrent threads over the shared MVars — an
-- offer (slot 10), an announcement (slot 11), and a body arrival (slot 12, which
-- inserts the body into the pure 'newPureLeiosTxCache' -- a lock distinct from the
-- outstanding lock -- while holding the outstanding lock) — and uses IOSimPOR to
-- explore every interleaving.
--
-- An 'EbHash' is not 1-to-1 with slots, so this is exactly the shape that armed
-- the storm: whichever listing wins is recorded at its own slot, and the arrival
-- (at yet another slot) must clear it /by hash/, not by point. In every
-- interleaving the state invariant "a held EB body is never still listed for
-- fetching" must hold, which is what stops a later decision from re-requesting
-- it.
--
-- With the shipped fix each handler's "held?"/"listed?" test and its state update
-- are one 'outstandingVar' critical section, and acquisition purges every point
-- sharing the hash via 'reverseSlotIndexByEbHash', so no interleaving can violate
-- this; the test guards against regressing either half (moving a check back out
-- of the lock, or reverting to a delete-by-point that misses the other slots).
prop_neverRefetchesHeldBodyConcurrent :: Property
prop_neverRefetchesHeldBodyConcurrent =
  exploreSimTrace id (exploreRaces *> raceSameHashMultiSlot) $ \_ tr ->
    case traceResult False tr of
      Right prop -> prop
      Left e -> counterexample ("Failure: " <> show e) False

-- | An offer, an announcement, and a body arrival walk into a bar...
--
-- All for the same EB hash but at three distinct slots, run concurrently over
-- shared state; the returned 'Property' is the invariant "no held EB body is
-- still listed for fetching".
raceSameHashMultiSlot :: forall m. IOLike m => m Property
raceSameHashMultiSlot = do
  dbHandle <- LeiosDb.newLeiosDBInMemory
  withLeiosDb dbHandle $ \conn -> do
    outstandingVar <- newMVar (emptyLeiosOutstanding (SlotNo 0))
    readyVar <- newEmptyMVar
    peerVars <- newLeiosPeerVars
    txCache <- newPureLeiosTxCache
    let kv = (outstandingVar, readyVar)
        peerId = MkPeerId (0 :: Int)
        ids = [0, 1] :: TestEb
        eb = ebOf ids
        ebBytesSize = leiosEbBytesSize eb
        -- One hash (same ids), three different slots.
        offerPoint = pointOf ids 10
        announcePoint = pointOf ids 11
        arrivalPoint = pointOf ids 12
    concurrently_
      (recordEbBodyOffer kv peerVars TxsClosureNotAlsoOffered (offerPoint, ebBytesSize))
      ( concurrently_
          (recordAnnouncedEb kv (announcePoint, ebBytesSize))
          ( processLeiosBlock
              nullTracer
              nullTracer
              kv
              txCache
              conn
              (ReceivedBlockFrom peerId (MkLeiosBlockRequest arrivalPoint ebBytesSize))
              eb
          )
      )
    outstanding <- readMVar outstandingVar
    let held = Map.keysSet (Map.filter Leios.ebStateHasBody (Leios.ebState outstanding))
        listed =
          Set.fromList (map (.pointEbHash) (Map.keys (Leios.missingEbBodies outstanding)))
        heldAndListed = Set.toList (Set.intersection held listed)
    pure $
      counterexample
        ("held EB body still listed for fetching: " <> show heldAndListed)
        (null heldAndListed)
