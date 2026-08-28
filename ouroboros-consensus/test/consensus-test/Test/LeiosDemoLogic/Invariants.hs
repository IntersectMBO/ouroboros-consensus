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
-- Phrasing it as "already held" rather than a request count is what makes it
-- correct given there is no per-EB request cap: requesting a not-yet-held body
-- from several peers is fine; re-requesting a held one is not.
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
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Vector.Strict as V
import Data.Void (Void, absurd)
import LeiosDemoDb (withLeiosDb)
import qualified LeiosDemoDb as LeiosDb
import LeiosDemoLogic
  ( LeiosBlockSource (..)
  , LeiosBlockTxsSource (..)
  , leiosFetchLogicIteration
  , noMempoolPull
  , processLeiosBlock
  , processLeiosBlockTxs
  , recordAnnouncedEb
  , recordEbBodyOffer
  )
import LeiosDemoTypes
  ( AlsoOfferedTxsClosure (..)
  , BytesSize
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
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  , SystemTime (..)
  )
import Ouroboros.Consensus.Util.IOLike (IOLike, evaluate)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
  ( IsBigLedgerPeer (..)
  )
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Orphans.IOLike ()
import Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  -- 10x whatever '--quickcheck-tests' supplies, for every property below.
  adjustQuickCheckTests (* 10) $
    testGroup
      "LeiosDemoLogic.Invariants"
      [ testGroup
          "curated sequences"
          [ testCase "forge purges a body it already holds (offered first)" $
              runCmdsReFetchViolations reproForgeAfterOffer @?= Right []
          , testCase "an offer of a self-forged EB is not re-fetched (forged first)" $
              runCmdsReFetchViolations reproForgeThenOffer @?= Right []
          ]
      , testCase "acquired EB kept until its greatest slot is below the immutable tip" $ do
          let eb = ebOf [0, 1]
              h = hashLeiosEb eb
              -- an empty job pool suffices here
              jobPool = Jobs.mkLeiosJobPool 1000 10 mempty
              -- announce at slot 5, then again at the smaller slot 3, and acquire
              o =
                Leios.insertAcquiredEbBody h jobPool $
                  Leios.recordMaxAnnouncementSlot h (SlotNo 3) SNothing $
                    Leios.recordMaxAnnouncementSlot h (SlotNo 5) SNothing $
                      (emptyLeiosOutstanding (SlotNo 0) :: LeiosOutstanding Int)
          -- the greater slot is retained, not the last-recorded one
          Map.lookup h (Leios.ebState o)
            @?= Just (Leios.MkEbState (SlotNo 5) SNothing (Leios.BodyAcquired jobPool))
          -- kept while the greatest slot (5) is at/above the immutable tip (4)
          Map.lookup h (Leios.ebState (snd (Leios.pruneOutstandingToImmTip (SlotNo 4) o)))
            @?= Just (Leios.MkEbState (SlotNo 5) SNothing (Leios.BodyAcquired jobPool))
          -- dropped once the greatest slot (5) is below the immutable tip (6)
          Map.lookup h (Leios.ebState (snd (Leios.pruneOutstandingToImmTip (SlotNo 6) o)))
            @?= Nothing
      , testCase "an announcement raises a forged EB's max slot (so it isn't pruned early)" $ do
          let h = hashLeiosEb (ebOf [0, 1])
              -- forge at slot 5, then a peer announces the same EB at the later slot 10
              o =
                Leios.recordMaxAnnouncementSlot h (SlotNo 10) SNothing $
                  Leios.markBodyImminent h (SlotNo 5) $
                    (emptyLeiosOutstanding (SlotNo 0) :: LeiosOutstanding Int)
          -- the announcement raised the slot to 10, keeping the forged state
          Map.lookup h (Leios.ebState o) @?= Just (Leios.MkEbState (SlotNo 10) SNothing Leios.BodyImminent)
          -- so it survives pruning up to slot 9, and is dropped only past slot 10
          Map.member h (Leios.ebState (snd (Leios.pruneOutstandingToImmTip (SlotNo 9) o))) @?= True
          Map.member h (Leios.ebState (snd (Leios.pruneOutstandingToImmTip (SlotNo 11) o))) @?= False
      , testCase "an announcement's onset is recorded (earliest kept); an offer never clobbers it" $ do
          let h = hashLeiosEb (ebOf [0, 1])
              t3 = RelativeTime 3
              t5 = RelativeTime 5
              base = emptyLeiosOutstanding (SlotNo 0) :: LeiosOutstanding Int
              onsetOf o = Leios.ebStateOnset <$> Map.lookup h (Leios.ebState o)
          -- an announcement records its slot's onset
          onsetOf (Leios.recordMaxAnnouncementSlot h (SlotNo 5) (SJust t5) base)
            @?= Just (SJust t5)
          -- a later announcement (greater slot, earlier onset) keeps the earlier onset
          onsetOf
            ( Leios.recordMaxAnnouncementSlot h (SlotNo 8) (SJust t3) $
                Leios.recordMaxAnnouncementSlot h (SlotNo 5) (SJust t5) base
            )
            @?= Just (SJust t3)
          -- an offer (no onset) bumps the slot but never clobbers a recorded onset
          onsetOf
            ( Leios.recordMaxAnnouncementSlot h (SlotNo 9) SNothing $
                Leios.recordMaxAnnouncementSlot h (SlotNo 5) (SJust t5) base
            )
            @?= Just (SJust t5)
          -- a self-forged EB records no onset (kept out of the age panels)
          onsetOf (Leios.markBodyImminent h (SlotNo 5) base)
            @?= Just SNothing
      , testCase "start-up seeding marks each completed EB held, with an empty pool" $ do
          let ebA = [0, 1] :: TestEb
              ebB = [2, 3] :: TestEb
              hA = hashLeiosEb (ebOf ebA)
              hB = hashLeiosEb (ebOf ebB)
              -- The complete-closure scan yields points: ebA listed at two slots (5
              -- and 8), ebB at slot 6.
              points = [pointOf ebA 5, pointOf ebA 8, pointOf ebB 6]
              immTipSlot = SlotNo 4
              o = Leios.initializeLeiosOutstanding points immTipSlot :: LeiosOutstanding Int
          -- each completed EB is held with an empty job pool: nothing left to fetch
          Map.lookup hB (Leios.ebState o)
            @?= Just (Leios.MkEbState (SlotNo 6) SNothing (Leios.BodyAcquired Jobs.emptyLeiosJobPool))
          -- and when one EB is listed at several points, its greatest slot wins (8, not 5)
          Map.lookup hA (Leios.ebState o)
            @?= Just (Leios.MkEbState (SlotNo 8) SNothing (Leios.BodyAcquired Jobs.emptyLeiosJobPool))
          -- so every seeded EB reports as held ...
          all Leios.ebStateHasBody (Map.elems (Leios.ebState o)) @?= True
          -- ... nothing is listed for fetch (empty pools, no missing bodies) ...
          Leios.missingEbBodies o @?= Map.empty
          Leios.reverseSlotIndexByEbHash o @?= Map.empty
          -- ... no requests are outstanding (there are no connections at start-up) ...
          Leios.requestedBytesSizePerPeer o @?= Map.empty
          Leios.requestedEbPeers o @?= Map.empty
          Leios.requestedJobsPerPeer o @?= Map.empty
          -- ... and the pruning watermark is seeded from the immutable tip
          Leios.acquiredEbBodiesPrunedSlot o @?= immTipSlot
      , testCase "start-up seeding: a peer's offer of a seeded EB is not re-fetched" $ do
          let ebA = [0, 1] :: TestEb
              ebB = [2, 3] :: TestEb
              points = [pointOf ebA 8, pointOf ebB 6]
              o = Leios.initializeLeiosOutstanding points (SlotNo 4) :: LeiosOutstanding Int
              peerId = MkPeerId (0 :: Int)
              -- a peer offers every seeded EB, body and closure
              offerings = Map.singleton peerId (referencedOffers o)
              (_out', decs, _drops) =
                leiosFetchLogicIteration demoLeiosFetchStaticEnv (Just (SlotNo 10)) offerings Map.empty o
          -- no body is re-requested (the whole point of the seed) ...
          ebBodyRequestHashes decs @?= []
          -- ... and with empty pools there is nothing at all to request
          Map.null decs @?= True
      , testCase "a big-ledger peer has a larger, but still finite, closure budget" $ do
          let ids = [0, 1, 2, 3, 4] :: TestEb
              h = hashLeiosEb (ebOf ids)
              point = pointOf ids 10
              misses = IntMap.fromList [(off, (txHashOf i, txSizeOf i)) | (off, i) <- zip [0 ..] ids]
              jobPool =
                Jobs.mkLeiosJobPool
                  (Leios.maxJobBytesSize demoLeiosFetchStaticEnv)
                  (Leios.maxJobTxCount demoLeiosFetchStaticEnv)
                  misses
              peerId = MkPeerId (0 :: Int)
              offers = Map.singleton peerId (Map.singleton point TxsClosureAlsoOffered)
              ordinaryCap = Leios.maxRequestedBytesSizePerPeer demoLeiosFetchStaticEnv
              bigLedgerCap = Leios.maxRequestedBytesSizePerBigLedgerPeer demoLeiosFetchStaticEnv
              -- hold the body (so the pool is live), with the peer's in-flight bytes
              -- preloaded to 'used'
              run bigLedgerPeers used =
                let outstanding =
                      (\o -> o{Leios.requestedBytesSizePerPeer = Map.singleton peerId used}) $
                        Leios.insertAcquiredEbBody h jobPool $
                          Leios.recordMaxAnnouncementSlot h (SlotNo 10) SNothing $
                            (emptyLeiosOutstanding (SlotNo 0) :: LeiosOutstanding Int)
                    (_o, reqs, _d) =
                      leiosFetchLogicIteration
                        demoLeiosFetchStaticEnv
                        (Just (SlotNo 11))
                        offers
                        bigLedgerPeers
                        outstanding
                 in requestedOffsets reqs
              ordinary = Map.empty
              bigLedger = Map.singleton peerId IsBigLedgerPeer
          -- past the ordinary cap, an ordinary peer is asked for nothing ...
          run ordinary (ordinaryCap + 1) @?= IntSet.empty
          -- ... but a big-ledger peer still has budget for the whole pool at once
          run bigLedger (ordinaryCap + 1) @?= IntSet.fromList ids
          -- past even the big-ledger cap, though, a big-ledger peer is bounded too
          run bigLedger (bigLedgerCap + 1) @?= IntSet.empty
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
              o = snd (Leios.pruneOutstandingToImmTip (SlotNo 5) o0)
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

-- | A stub clock for the handlers: the suite never asserts on EB age, and these
-- EBs are never heralded, so the age always comes out 'Nothing' regardless.
dummySystemTime :: Applicative m => SystemTime m
dummySystemTime =
  SystemTime
    { systemTimeCurrent = pure (RelativeTime 0)
    , systemTimeWait = pure ()
    }

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
      peerVars <- newLeiosPeerVars IsNotBigLedgerPeer
      let kv = (outstandingVar, readyVar)
          txCache = nullLeiosTxCache
          peerId = MkPeerId (0 :: Int)
          loop acc [] = pure (Right acc)
          loop acc (c : cs) = do
            r <-
              try (applyCmd conn txCache kv peerVars peerId c) ::
                IOSim s (Either SomeException [EbHash])
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
    -- These invariants are about the fetch bookkeeping, which never reads the
    -- onset; only the voting path needs it.
    recordAnnouncedEb kv SNothing (pointOf ids slot, leiosEbBytesSize (ebOf ids))
    pure []
  Offer ids slot -> do
    recordEbBodyOffer
      kv
      peerVars
      TxsClosureNotAlsoOffered
      (pointOf ids slot, leiosEbBytesSize (ebOf ids))
    pure []
  ArriveBody ids slot -> do
    let eb = ebOf ids
        req = MkLeiosBlockRequest (pointOf ids slot) (leiosEbBytesSize eb)
    processLeiosBlock
      nullTracer
      nullTracer
      kv
      txCache
      conn
      dummySystemTime
      noMempoolPull
      (ReceivedBlockFrom peerId req)
      eb
    pure []
  ArriveTx v -> absurd v
  Forge ids slot -> do
    let eb = ebOf ids
        point = pointOf ids slot
    -- Replicate 'onForgedLeiosEb''s effect on 'outstanding': its 'ForgedLocally'
    -- announcement marks the EB 'BodyImminent' (via 'markBodyImminent'), then the
    -- body and closure arrive. That mark is what stops a later peer offer of our
    -- own EB from being re-fetched -- without it a forge-first sequence would leave
    -- 'ebState' untouched (as it did pre-fix, causing the crash).
    --
    -- We can't just call 'onForgedLeiosEb' because it needs a concrete @blk@ with a
    -- real 'AnnouncingHeader' and a 'CentralState' -- the whole announcement stack
    -- this suite deliberately avoids.
    --
    -- WARNING: this hand-replicates 'onForgedLeiosEb'; if that function's effect on
    -- 'outstanding' changes, mirror it here or this regression coverage goes stale
    -- silently.
    modifyMVar_ (fst kv) (pure . Leios.markBodyImminent point.pointEbHash point.pointSlotNo)
    processLeiosBlock
      nullTracer
      nullTracer
      kv
      txCache
      conn
      dummySystemTime
      noMempoolPull
      (ForgedBlock point)
      eb
    processLeiosBlockTxs
      nullTracer
      nullTracer
      kv
      txCache
      conn
      dummySystemTime
      (ForgedTxs point eb $ V.fromList $ map leiosTxOf ids)
    pure []
  Decide slot -> do
    outstanding <- readMVar (fst kv)
    let offerings = Map.singleton peerId (referencedOffers outstanding)
        -- The generated peer is not a big-ledger peer; the aggressive-fetch path
        -- has its own dedicated test below.
        (out', decs, _drops) =
          leiosFetchLogicIteration
            demoLeiosFetchStaticEnv
            (Just (fromIntegral slot))
            offerings
            Map.empty
            outstanding
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

-- | Offer every EB the outstanding state tracks, at its 'ebStateMaxSlot' and as
-- 'TxsClosureAlsoOffered' (which implies the body too) -- an all-offering peer, so
-- the fetch logic can act on whichever half each EB still needs.
referencedOffers :: LeiosOutstanding Int -> Map.Map Leios.LeiosPoint Leios.AlsoOfferedTxsClosure
referencedOffers o =
  Map.fromList
    [ (Leios.MkLeiosPoint (Leios.ebStateMaxSlot s) h, Leios.TxsClosureAlsoOffered)
    | (h, s) <- Map.toList (Leios.ebState o)
    ]

-- | Force the requests to a scalar, so any @impossible!@ hidden in a thunk
-- surfaces when the caller 'evaluate's it. Touches each tx request's covered
-- job offsets and each EB request's size.
forceDecisions :: Map.Map peer (NESeq Leios.LeiosFetchRequest) -> Int
forceDecisions m =
  sum [reqScore req | reqs <- Map.elems m, req <- toList reqs]
 where
  reqScore = \case
    Leios.LeiosBlockRequest (Leios.MkLeiosBlockRequest _p sz) -> fromIntegral sz
    Leios.LeiosBlockTxsRequest (Leios.MkLeiosBlockTxsRequest _p jobs) ->
      sum
        [ off
        | Jobs.MkLeiosJob offs _bytes _root <- toList jobs
        , off <- IntSet.toList offs
        ]

-- | The 'EbHash'es the requests fetch an EB body for (one entry per request; with
-- no per-EB cap, an EB may appear once per offering peer).
ebBodyRequestHashes :: Map.Map peer (NESeq Leios.LeiosFetchRequest) -> [EbHash]
ebBodyRequestHashes m =
  [ p.pointEbHash
  | reqs <- Map.elems m
  , Leios.LeiosBlockRequest (Leios.MkLeiosBlockRequest p _sz) <- toList reqs
  ]

-- | The union of every tx offset the requests fetch, across all peers.
requestedOffsets :: Map.Map peer (NESeq Leios.LeiosFetchRequest) -> IntSet.IntSet
requestedOffsets m =
  IntSet.unions
    [ offs
    | reqs <- Map.elems m
    , Leios.LeiosBlockTxsRequest (Leios.MkLeiosBlockTxsRequest _p jobs) <- toList reqs
    , Jobs.MkLeiosJob offs _bytes _root <- toList jobs
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

-- | The devnet crash order: we forge an EB, then a peer offers that same EB back
-- (e.g. relaying our own announcement). The forge marked it 'BodyImminent' (the
-- body arrival then makes it 'BodyAcquired'), so the offer must be dropped, never
-- re-fetched. Pre-fix the re-fetch re-acquired the closure, emitting a duplicate
-- 'AcquiredEbTxs' that killed 'runLeiosVoting' with 'AlreadyKnown'.
reproForgeThenOffer :: [Cmd]
reproForgeThenOffer =
  [ Forge [0, 1] 12
  , Offer [0, 1] 12
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
-- Stated as "already held" rather than a request count, since there is no per-EB
-- request cap: requesting a not-yet-held body from several peers is fine;
-- re-requesting a held one is not. (With 'nullLeiosTxCache', the
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
    peerVars <- newLeiosPeerVars IsNotBigLedgerPeer
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
          (recordAnnouncedEb kv SNothing (announcePoint, ebBytesSize))
          ( processLeiosBlock
              nullTracer
              nullTracer
              kv
              txCache
              conn
              dummySystemTime
              noMempoolPull
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
