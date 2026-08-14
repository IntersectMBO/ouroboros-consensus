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
-- NOTE. This test suite initially exists as a specific regression tests: every
-- tx tracked in 'Leios.missingEbTxs' must still be resolvable in
-- 'Leios.reverseEbIndexByTx' (same keying 'leiosFetchLogicIteration' relies
-- on). We check it structurally after each command, and — belt and suspenders —
-- force a 'Decide' through the real fetch logic so a stray @impossible!@
-- surfaces even if the structural check missed a shape.
--
-- NOTE. A second regression lives here too: the fetch logic must never request
-- an EB body it already holds. That storm — a held body being re-listed and
-- re-requested — is what 'prop_neverRefetchesHeldBody' guards against; after each
-- 'Decide' it checks that no body just requested is already in 'acquiredEbBodies'.
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
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.DList as DList
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Vector.Strict as V
import Data.Word (Word16, Word64)
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
  , LeiosBlockTxsRequest (..)
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
        [ testCase "same EB hash at two slots: delivery clears both" $
            runCmds reproMultiSlot @?= Right ()
        , testCase "tx shared across two EBs: delivery discharges both" $
            runCmds reproSharedTx @?= Right ()
        , testCase "forge purges a body it already holds (offered first)" $
            runCmdsReFetchViolations reproForgeAfterOffer @?= Right []
        , testCase "forge discharges a tx a peer EB still needs" $
            runCmds reproForgeSharedTx @?= Right ()
        ]
    , testProperty
        "missingEbTxs stays in sync with reverseEbIndexByTx across arbitrary sequences"
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
  | -- | @processLeiosBlockTxs@: deliver the tx at this /index within the EB/.
    ArriveTx TestEb Word Int
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
-- logic requested despite already holding them (i.e. despite being in
-- 'acquiredEbBodies'), gathered across all 'Decide's. That list is the
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
-- (in 'acquiredEbBodies') — the re-fetch-storm violation. Empty for everything
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
  ArriveTx ids slot idx -> do
    let txId = ids !! idx
        req =
          MkLeiosBlockTxsRequest
            (pointOf ids slot)
            (offsetsToBitmaps [idx])
            (V.singleton (txHashOf txId))
    processLeiosBlockTxs nullTracer nullTracer kv txCache conn (ReceivedTxsFrom peerId req) (V.singleton (leiosTxOf txId))
    pure []
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
    -- Regression: the fetch logic must not request a body already in
    -- 'acquiredEbBodies'. Return any it did (empty when well-behaved).
    let held = Leios.acquiredEbBodies outstanding
    pure (filter (\h -> Map.member h held) (ebBodyRequestHashes decs))

-- | Every EbHash currently referenced by the outstanding state (bodies + txs),
-- as an all-offering peer's body\/closure sets.
referencedEbs :: LeiosOutstanding Int -> Set.Set EbHash
referencedEbs o =
  Set.fromList $
    map (.pointEbHash) (Map.keys (Leios.missingEbBodies o))
      <> map (.pointEbHash) (Map.keys (Leios.missingEbTxs o))

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

-- | Every tx tracked as missing must be resolvable in the reverse index at the
-- exact (EbHash, slot, offset) 'go1'/'goTx2' will look it up by. Its violation
-- is what @impossible! leiosFetchLogicIteration go1@ reports.
checkInvariant :: LeiosOutstanding Int -> Either String ()
checkInvariant o =
  case
    [ msg
    | (p, txs) <- Map.toList (Leios.missingEbTxs o)
    , (off, (txHash, _sz)) <- IntMap.toList txs
    , Left msg <- [resolvable p off txHash]
    ] of
    [] -> Right ()
    (msg : _) -> Left msg
 where
  rev = Leios.reverseEbIndexByTx o
  resolvable p off txHash =
    case Map.lookup txHash rev of
      Nothing ->
        Left ("missingEbTxs tx absent from reverseEbIndexByTx: " <> show (p.pointSlotNo, off))
      Just ebm -> case Map.lookup (pointEbHash p) ebm of
        Nothing -> Left ("reverseEbIndexByTx lacks this EB for a missing tx: " <> show p.pointSlotNo)
        Just (slots, off', _sz')
          | p.pointSlotNo `NESet.member` slots && off' == off -> Right ()
          | otherwise -> Left ("reverseEbIndexByTx slot/offset mismatch at " <> show p.pointSlotNo)

------------------------------------------------------------
-- Curated repros
------------------------------------------------------------

-- | The same EB (hash) bodied at two slots; delivering its tx for one slot must
-- clear it for the other too. Pre-fix, the second 'Decide' hits @impossible!@.
reproMultiSlot :: [Cmd]
reproMultiSlot =
  [ ArriveBody [0] 10
  , ArriveBody [0] 11
  , Decide 11
  , ArriveTx [0] 10 0
  , Decide 11
  ]

-- | A tx shared by two distinct EBs; delivering it via one must discharge it
-- for the other (the deduping-LeiosDb behaviour the fix relies on).
reproSharedTx :: [Cmd]
reproSharedTx =
  [ ArriveBody [0, 1] 10
  , ArriveBody [1, 2] 11
  , Decide 12
  , ArriveTx [0, 1] 10 1 -- deliver the shared tx (id 1)
  , Decide 12
  ]

-- | A peer offers an EB body; we forge the same EB before the offered body
-- arrives. Forging must purge the offered body from 'missingEbBodies' (it now
-- lives in 'acquiredEbBodies'), so the fetch logic never re-requests a body we
-- already hold. Pre-fix the forge recorded the body as acquired without purging,
-- so the 'Decide' re-fetched it.
reproForgeAfterOffer :: [Cmd]
reproForgeAfterOffer =
  [ Offer [0, 1] 10
  , Forge [0, 1] 12
  , Decide 13
  ]

-- | A peer's EB still needs a tx that our own forged EB's closure supplies.
-- Forging must discharge it from that EB's 'missingEbTxs' (as delivering it via
-- 'ArriveTx' would), keeping the missing sets consistent.
reproForgeSharedTx :: [Cmd]
reproForgeSharedTx =
  [ ArriveBody [1, 2] 10
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
    , ArriveTx ids slot <$> choose (0, length ids - 1)
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

-- | A peer EB body arrived, then a /different/ EB sharing one of its txs is
-- forged: the tx forge hazard, where forging must discharge the shared tx.
arrivedThenForgedSharingTx :: [Cmd] -> Bool
arrivedThenForgedSharingTx cmds =
  or
    [ arrivedIds /= ids && any (`elem` ids) arrivedIds
    | (i, Forge ids _) <- zip [0 :: Int ..] cmds
    , ArriveBody arrivedIds _ <- take i cmds
    ]

-- | Coverage shared by the generated properties: the command mix, and whether
-- the two forge hazards were actually generated -- so the properties are visibly
-- non-vacuous.
coverage :: Testable prop => [Cmd] -> prop -> Property
coverage cmds prop =
  tabulate "commands" (map cmdName cmds) $
    classify (any isForge cmds) "has a Forge" $
      cover 15 (listedThenForged cmds) "listed then forged (body hazard)" $
        cover 10 (arrivedThenForgedSharingTx cmds) "arrived then forged, shared tx (tx hazard)" $
          property prop

prop_invariants :: Property
prop_invariants =
  forAllShrink (listOf genCmd) (shrinkList (const [])) $ \cmds ->
    coverage cmds (runCmds cmds === Right ())

-- | Regression for the EB-body re-fetch storm: over any interleaving of
-- announces, offers, and body/tx arrivals, the fetch logic must never request an
-- EB body it already holds (one in 'acquiredEbBodies'). The storm was precisely
-- this — a held body re-listed and re-requested indefinitely.
--
-- Stated as "already held" rather than a request count, so it stays correct if
-- 'maxRequestsPerEb' rises above 1: requesting a not-yet-held body from several
-- peers is fine; re-requesting a held one is not. (With 'nullLeiosTxCache', the
-- old LeiosTxCache-based "do we have it?" check would see nothing held and
-- re-list/re-request endlessly; the 'acquiredEbBodies' check is cache-independent.)
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
-- the handlers out keeps the two-lock structure this test exists to probe -- the
-- shared cache, and the announcement path's cross-lock 'lookupBody' -- in plain
-- view at the race site.

-- | The sequential 'prop_neverRefetchesHeldBody' generates event /sequences/ but
-- runs each handler to completion, so it can't reproduce a cross-lock race
-- straddling a concurrent body insert. This scenario runs three handlers for the
-- /same EB hash at three different slots/ as genuinely concurrent threads over
-- the shared MVars — an offer (slot 10), an announcement (slot 11, whose "do we
-- already hold it?" read hits the pure 'newPureLeiosTxCache', a lock distinct
-- from the outstanding lock, before it touches the outstanding state), and a body
-- arrival (slot 12, different from both) — and uses IOSimPOR to explore every
-- interleaving.
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
    let held = Map.keysSet (Leios.acquiredEbBodies outstanding)
        listed =
          Set.fromList (map (.pointEbHash) (Map.keys (Leios.missingEbBodies outstanding)))
        heldAndListed = Set.toList (Set.intersection held listed)
    pure $
      counterexample
        ("held EB body still listed for fetching: " <> show heldAndListed)
        (null heldAndListed)

offsetsToBitmaps :: [Int] -> [(Word16, Word64)]
offsetsToBitmaps offs =
  [ (fromIntegral q, bm)
  | (q, bm) <-
      IntMap.toAscList $
        foldr
          (\o -> let (q, r) = o `divMod` 64 in IntMap.insertWith (Bits..|.) q (Bits.bit (63 - r)))
          IntMap.empty
          offs
  ]
