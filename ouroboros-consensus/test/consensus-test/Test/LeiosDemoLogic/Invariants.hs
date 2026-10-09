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
import Control.Concurrent.Class.MonadSTM.Strict (atomically, tryReadTChan)
import Control.Monad (forever)
import Control.Monad.Class.MonadAsync (concurrently_)
import Control.Monad.Class.MonadTest (exploreRaces)
import Control.Monad.Class.MonadThrow (SomeException, try)
import Control.Monad.Class.MonadTimer (threadDelay)
import Control.Monad.IOSim (IOSim, exploreSimTrace, runSimOrThrow, traceResult)
import Control.Tracer (nullTracer)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Foldable (foldl', toList)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Vector.Strict as V
import Data.Void (Void, absurd)
import Data.Word (Word64)
import LeiosDemoDb (withWriter)
import qualified LeiosDemoDb as LeiosDb
import LeiosDemoLogic
  ( AnnouncingHeader
  , ExnLeiosWellHashedBodyRejected (..)
  , LeiosBlockSource (..)
  , LeiosBlockTxsSource (..)
  , leiosFetchLogicIteration
  , mkAnnouncingHeader
  , noMempoolPull
  , processAnnouncementCentrally
  , processLeiosBlock
  , processLeiosBlockTxs
  , recordAnnouncedEb
  , recordEbBodyOffer
  , removePeerFromOutstanding
  )
import qualified LeiosDemoLogic.Announcements as Announcements
import LeiosDemoLogic.Announcements.ElBimap (ElId (MkElId))
import LeiosDemoTypes
  ( AnnouncementSource (..)
  , BytesSize
  , ClosureOffer (..)
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
  , encodeLeiosEbSize
  , hashLeiosEb
  , hashLeiosTx
  , newLeiosPeerVars
  )
import qualified LeiosDemoTypes as Leios
import qualified LeiosDemoTypes.LeiosJobs as Jobs
import LeiosTxCache (LeiosTxCache, defaultLeiosTxCacheShift, newPureLeiosTxCache, nullLeiosTxCache)
import Ouroboros.Consensus.Block (getHeader)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  , SystemTime (..)
  )
import Ouroboros.Consensus.Forecast (OutsideForecastRange)
import Ouroboros.Consensus.Util.IOLike (IOLike, evaluate)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
  ( IsBigLedgerPeer (..)
  )
import System.Random (mkStdGen)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.LeiosTestBlock
  ( LeiosTestBlock
  , announcing
  , firstLeiosBlock
  , issuedBy
  , successorLeiosBlock
  )
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
      , testCase "a body is claimed acquired only by a settled write" $ do
          let eb = ebOf [0, 1]
              h = hashLeiosEb eb
              jobPool = Jobs.mkLeiosJobPool 1000 10 V.empty mempty
              fetchStateOf o =
                (\(Leios.MkEbState _ _ fs) -> fs) <$> Map.lookup h (Leios.ebState o)
              announced =
                Leios.recordMaxAnnouncementSlot h (SlotNo 5) SNothing $
                  (emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0) :: LeiosOutstanding Int)
          -- Only an announcement so far: the body is not held, so a fetch is due.
          -- There is no in-flight state; the body write is enqueued before any
          -- claim, and the claim ('BodyAcquired') is made only once it is durable.
          fetchStateOf announced @?= Just Leios.NoBody
          -- The write landed: claim 'BodyAcquired' with the settled write's pool,
          -- keeping the recorded max slot.
          Map.lookup h (Leios.ebState (Leios.acquireEbBody h jobPool announced))
            @?= Just (Leios.MkEbState (SlotNo 5) SNothing (Leios.BodyAcquired jobPool))
          -- Idempotent: a concurrent redundant delivery does not re-claim it.
          fetchStateOf (Leios.acquireEbBody h jobPool (Leios.acquireEbBody h jobPool announced))
            @?= Just (Leios.BodyAcquired jobPool)
      , testCase "acquired EB kept until its greatest slot is below the immutable tip" $ do
          let eb = ebOf [0, 1]
              h = hashLeiosEb eb
              -- an empty job pool suffices here
              jobPool = Jobs.mkLeiosJobPool 1000 10 V.empty mempty
              -- announce at slot 5, then again at the smaller slot 3, and acquire
              o =
                Leios.acquireEbBody h jobPool $
                  Leios.recordMaxAnnouncementSlot h (SlotNo 3) SNothing $
                    Leios.recordMaxAnnouncementSlot h (SlotNo 5) SNothing $
                      (emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0) :: LeiosOutstanding Int)
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
                    (emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0) :: LeiosOutstanding Int)
          -- the announcement raised the slot to 10, keeping the forged state
          Map.lookup h (Leios.ebState o) @?= Just (Leios.MkEbState (SlotNo 10) SNothing Leios.BodyImminent)
          -- so it survives pruning up to slot 9, and is dropped only past slot 10
          Map.member h (Leios.ebState (snd (Leios.pruneOutstandingToImmTip (SlotNo 9) o))) @?= True
          Map.member h (Leios.ebState (snd (Leios.pruneOutstandingToImmTip (SlotNo 11) o))) @?= False
      , testCase "an announcement's onset is recorded (earliest kept); an offer never clobbers it" $ do
          let h = hashLeiosEb (ebOf [0, 1])
              t3 = RelativeTime 3
              t5 = RelativeTime 5
              base = emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0) :: LeiosOutstanding Int
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
              o = Leios.initializeLeiosOutstanding (mkStdGen 0) points immTipSlot :: LeiosOutstanding Int
          -- each completed EB is held with an empty job pool: nothing left to fetch
          Map.lookup hB (Leios.ebState o)
            @?= Just (Leios.MkEbState (SlotNo 6) SNothing (Leios.BodyAcquired Jobs.emptyLeiosJobPool))
          -- and when one EB is listed at several points, its greatest slot wins (8, not 5)
          Map.lookup hA (Leios.ebState o)
            @?= Just (Leios.MkEbState (SlotNo 8) SNothing (Leios.BodyAcquired Jobs.emptyLeiosJobPool))
          -- so every seeded EB reports as held ...
          all Leios.ebStateHasBody (Map.elems (Leios.ebState o)) @?= True
          -- ... nothing is listed for fetch (empty pools, no missing bodies) ...
          -- ... no requests are outstanding (there are no connections at start-up) ...
          Leios.requestedBytesSizePerPeer o @?= Map.empty
          Leios.requestedEbPeers o @?= Map.empty
          Leios.requestedJobsPerPeer o @?= Map.empty
          -- ... and the pruning watermark is seeded from the immutable tip
          Leios.outstandingPrunedSlot o @?= immTipSlot
      , testCase "start-up seeding: a peer's offer of a seeded EB is not re-fetched" $ do
          let ebA = [0, 1] :: TestEb
              ebB = [2, 3] :: TestEb
              points = [pointOf ebA 8, pointOf ebB 6]
              o = Leios.initializeLeiosOutstanding (mkStdGen 0) points (SlotNo 4) :: LeiosOutstanding Int
              peerId = MkPeerId (0 :: Int)
              -- a peer offers every seeded EB, body and closure
              offerings = Map.singleton peerId (referencedOffers o)
              (_out', decs, _drops) =
                leiosFetchLogicIteration
                  demoLeiosFetchStaticEnv
                  anyClosureSize
                  (Just (SlotNo 10))
                  offerings
                  Map.empty
                  o
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
                  (V.fromList (map txSizeOf ids))
                  misses
              peerId = MkPeerId (0 :: Int)
              -- The body is already held in these runs, so the offer names no
              -- size; only its closure half is consulted.
              offers =
                Map.singleton peerId $
                  Map.singleton point (Leios.MkPeerOffer SNothing SNothing Leios.wholeClosureOffer)
              ordinaryCap = Leios.maxRequestedBytesSizePerPeer demoLeiosFetchStaticEnv
              bigLedgerCap = Leios.maxRequestedBytesSizePerBigLedgerPeer demoLeiosFetchStaticEnv
              -- hold the body (so the pool is live), with the peer's in-flight bytes
              -- preloaded to 'used'
              run bigLedgerPeers used =
                let outstanding =
                      (\o -> o{Leios.requestedBytesSizePerPeer = Map.singleton peerId used})
                        $
                        -- the election fetching it, without which nothing is requested
                        Leios.focusElectionIfUnfocused
                          (Leios.announcementElection (announcementOf point 0))
                          h
                        $ Leios.acquireEbBody h jobPool
                        $ Leios.recordMaxAnnouncementSlot h (SlotNo 10) SNothing
                        $ (emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0) :: LeiosOutstanding Int)
                    (_o, reqs, _d) =
                      leiosFetchLogicIteration
                        demoLeiosFetchStaticEnv
                        anyClosureSize
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
      , testCase "job assignment draws within the least-requested bucket, at random, respecting exclusions" $ do
          let misses = IntMap.fromList [(off, (txHashOf off, txSizeOf off)) | off <- [0 .. 5]]
              -- 'maxJobTxCount' 1 makes each tx its own job, so job ids 0..5 all
              -- start at multiplicity 0 (one bucket).
              pool0 = Jobs.mkLeiosJobPool 1000000 1 (V.fromList [txSizeOf off | off <- [0 .. 5]]) misses
              pickId pool excluded s =
                case Jobs.pickLeastRequestedJobExcept (mkStdGen s) excluded pool of
                  Just (Jobs.MkLeiosJobId i, _job, _pool', _prng') -> Just i
                  Nothing -> Nothing
          -- across seeds the draw isn't pinned to the lowest id, and every draw is
          -- a real job id.
          let drawn = Set.fromList [i | s <- [0 .. 99 :: Int], Just i <- [pickId pool0 IntSet.empty s]]
          assertBool "shuffles: more than one distinct job is drawn" (Set.size drawn > 1)
          assertBool "only ever draws real job ids" (drawn `Set.isSubsetOf` Set.fromList [0 .. 5])
          -- an excluded job is never drawn: exclude all but job 5.
          Set.fromList
            [i | s <- [0 .. 50 :: Int], Just i <- [pickId pool0 (IntSet.fromList [0, 1, 2, 3, 4]) s]]
            @?= Set.singleton 5
          -- the least-requested bucket wins regardless of the draw: force job 0 to
          -- multiplicity 1 (by excluding the rest), then an unrestricted draw comes
          -- only from the still-least-requested jobs 1..5, never job 0.
          let pool1 = case Jobs.pickLeastRequestedJobExcept (mkStdGen 0) (IntSet.fromList [1, 2, 3, 4, 5]) pool0 of
                Just (_jid, _job, p, _prng') -> p
                Nothing -> error "forced pick of the sole non-excluded job failed"
              drawnFromPool1 = Set.fromList [i | s <- [0 .. 50 :: Int], Just i <- [pickId pool1 IntSet.empty s]]
          assertBool
            "least-requested bucket wins: job 0 (multiplicity 1) is not drawn"
            (not (0 `Set.member` drawnFromPool1))
          assertBool "still shuffles among the least-requested jobs" (Set.size drawnFromPool1 > 1)
      , testCase "only the announcement that takes an election's focus is tracked" $ do
          -- The announcement path contributes at most one 'ebState' entry per
          -- election: the one that takes the election's focus. A second
          -- announcement for an already-focused election is an equivocation,
          -- and tracking its endorser block too would let a pool double what
          -- its won slots cost us. Should that one turn out to be the certified
          -- one, 'trackCertifiedEb' gives it an entry then --- which this says
          -- nothing about, and is the other way an election can reach
          -- 'ebState'.
          let hFocused = hashLeiosEb (ebOf [0, 1])
              hRival = hashLeiosEb (ebOf [2, 3])
              elId = MkElId (SlotNo 5) (SBS.pack [1])
              fieldsFor h = Leios.MkAnnouncementFields elId h 99
              o = runSimOrThrow $ do
                outstandingVar <-
                  newMVar (emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0) :: LeiosOutstanding Int)
                readyVar <- newEmptyMVar
                recordAnnouncedEb (outstandingVar, readyVar) SNothing (fieldsFor hFocused)
                recordAnnouncedEb (outstandingVar, readyVar) SNothing (fieldsFor hRival)
                readMVar outstandingVar
          Map.member hFocused (Leios.ebState o) @?= True
          Map.member hRival (Leios.ebState o) @?= False
          Map.lookup elId (Leios.elFocus o) @?= Just hFocused
      , testGroup
          "EB-hash collision (ported from #2309)"
          -- 'announcementOf' derives the election from (slot, hash), so the same
          -- EB at two slots is two elections. Only 'Forge' completes a closure
          -- here (no tx-delivery command, no mempool pull), so notifications are
          -- only asserted in the 'Forge' scenarios.
          [ testCase "forged, then the same EB announced at a new slot: both points registered and notified" $ do
              let (points, notified) = runCmdsCollision [Forge collisionEb 5, Announce collisionEb 8]
              points @?= [(SlotNo 5, collisionHash), (SlotNo 8, collisionHash)]
              notified @?= [pointOf collisionEb 5, pointOf collisionEb 8]
          , testCase "body held, then the same EB announced at a new slot: both points registered" $ do
              let (points, _) =
                    runCmdsCollision
                      [Announce collisionEb 5, Decide 5, ArriveBody collisionEb 5, Announce collisionEb 8]
              points @?= [(SlotNo 5, collisionHash), (SlotNo 8, collisionHash)]
          , testCase "both announced before the fetch: both points registered" $ do
              let (points, _) =
                    runCmdsCollision
                      [Announce collisionEb 5, Announce collisionEb 8, Decide 8, ArriveBody collisionEb 8]
              points @?= [(SlotNo 5, collisionHash), (SlotNo 8, collisionHash)]
          , testCase "forged, then the same EB merely offered at a new slot: the offer registers nothing" $ do
              -- An offer is an unverified claim (#2309's 9c8e5d9aa).
              let (points, notified) = runCmdsCollision [Forge collisionEb 5, Offer collisionEb 8]
              points @?= [(SlotNo 5, collisionHash)]
              notified @?= [pointOf collisionEb 5]
          ]
      , testCase "a certificate tracks an endorser block no announcement did" $ do
          -- A certificate can reach us for an announcement we never processed:
          -- if the announcing block has been on our selection since initial
          -- chain selection, ChainSync intersects at or after it, so its header
          -- never rolls forward and nothing announces it to us. Then only the
          -- certificate gives that endorser block an 'ebState' entry, without
          -- which the decision logic skips every offer of it.
          let eb = ebOf [0, 1]
              h = hashLeiosEb eb
              elCertified = MkElId (SlotNo 7) (SBS.pack [2])
              o =
                Leios.focusCertifiedEb (Leios.MkAnnouncementFields elCertified h 99) $
                  (emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0) :: LeiosOutstanding Int)
          Map.lookup h (Leios.ebState o)
            @?= Just (Leios.MkEbState (SlotNo 7) SNothing Leios.NoBody)
          Map.lookup elCertified (Leios.elFocus o) @?= Just h
      , testCase "an endorser block that references too many tx bytes is dropped, not fetched" $ do
          -- The references are what the closure costs, and bounding the encoded
          -- body does not bound them: a reference is charged the CBOR digits of
          -- the size it claims. So the arriving body is weighed against the
          -- bound its request carried, before any closure job exists.
          let ids = [0 .. 3]
              eb = ebOf ids
              point = pointOf ids 5
              h = Leios.pointEbHash point
              referenced = sum (map (fromIntegral . txSizeOf) ids) :: Word64
              -- One byte under what this endorser block references.
              bound = fromIntegral referenced - 1
              run = runSimOrThrow $ do
                dbHandle <- LeiosDb.newLeiosDBInMemory
                withWriter dbHandle $ \conn -> do
                  outstandingVar <- newMVar (emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0))
                  readyVar <- newEmptyMVar
                  let kv = (outstandingVar, readyVar)
                  recordAnnouncedEb kv SNothing $
                    announcementOf point (encodeLeiosEbSize eb)
                  r <-
                    try $
                      processLeiosBlock
                        nullTracer
                        nullTracer
                        kv
                        nullLeiosTxCache
                        conn
                        dummySystemTime
                        noMempoolPull
                        (ReceivedBlockFrom (MkPeerId (0 :: Int)) (MkLeiosBlockRequest point (encodeLeiosEbSize eb) bound))
                        eb
                  outstanding <- readMVar outstandingVar
                  pure (r :: Either ExnLeiosWellHashedBodyRejected (), outstanding)
              (thrown, o) = run
          -- The peer answers for it.
          case thrown of
            Left (ExnLeiosClosureTooBig p referenced' bound') ->
              (p, referenced', bound') @?= (point, referenced, bound)
            other -> assertFailure ("expected ExnLeiosClosureTooBig, got " <> show other)
          -- Recorded with an empty job pool: no closure to fetch, and the next
          -- peer to serve the same bytes does not cost us the work again.
          Map.lookup h (Leios.ebState o)
            @?= Just (Leios.MkEbState (SlotNo 5) SNothing (Leios.BodyAcquired Jobs.emptyLeiosJobPool))
          Leios.numMissingBodies o @?= 0
      , testCase "an endorser block at exactly its bound is fetched" $ do
          let ids = [0 .. 3]
              eb = ebOf ids
              point = pointOf ids 5
              h = Leios.pointEbHash point
              bound = sum (map txSizeOf ids)
              o = runSimOrThrow $ do
                dbHandle <- LeiosDb.newLeiosDBInMemory
                withWriter dbHandle $ \conn -> do
                  outstandingVar <- newMVar (emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0))
                  readyVar <- newEmptyMVar
                  let kv = (outstandingVar, readyVar)
                  recordAnnouncedEb kv SNothing $
                    announcementOf point (encodeLeiosEbSize eb)
                  processLeiosBlock
                    nullTracer
                    nullTracer
                    kv
                    nullLeiosTxCache
                    conn
                    dummySystemTime
                    noMempoolPull
                    (ReceivedBlockFrom (MkPeerId (0 :: Int)) (MkLeiosBlockRequest point (encodeLeiosEbSize eb) bound))
                    eb
                  readMVar outstandingVar
          -- Accepted, so its closure became jobs rather than an empty pool.
          case Map.lookup h (Leios.ebState o) of
            Just (Leios.MkEbState _ _ (Leios.BodyAcquired jobPool)) ->
              assertBool "expected a non-empty job pool" (jobPool /= Jobs.emptyLeiosJobPool)
            other -> assertFailure ("expected BodyAcquired, got " <> show other)
      , testCase "a certificate leaves a body we already hold held" $ do
          let eb = ebOf [0, 1]
              h = hashLeiosEb eb
              elCertified = MkElId (SlotNo 7) (SBS.pack [2])
              o =
                Leios.focusCertifiedEb (Leios.MkAnnouncementFields elCertified h 99) $
                  Leios.acquireEbBody h (Jobs.mkLeiosJobPool 1000 10 V.empty mempty) $
                    -- the announcement that got us the body in the first place;
                    -- without it 'acquireEbBody' has no entry to update
                    Leios.recordMaxAnnouncementSlot h (SlotNo 7) SNothing $
                      (emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0) :: LeiosOutstanding Int)
          maybe False Leios.ebStateHasBody (Map.lookup h (Leios.ebState o)) @?= True
      , testCase
          "a closure-prefix offer is served only inside the prefix, and pruned once that is exhausted"
          $ do
            let ids = [0 .. 5] :: TestEb
                h = hashLeiosEb (ebOf ids)
                point = pointOf ids 10
                sizes = V.fromList (map txSizeOf ids)
                misses = IntMap.fromList [(off, (txHashOf i, txSizeOf i)) | (off, i) <- zip [0 ..] ids]
                -- one job per tx, so a prefix boundary falls between jobs
                jobPool = Jobs.mkLeiosJobPool 1000000 1 sizes misses
                peerId = MkPeerId (0 :: Int)
                -- exactly the first three txs
                prefix = sum (map txSizeOf (take 3 ids))
                ordinaryCap = Leios.maxRequestedBytesSizePerPeer demoLeiosFetchStaticEnv
                run offer used =
                  let outstanding =
                        (\o -> o{Leios.requestedBytesSizePerPeer = Map.singleton peerId used})
                          $
                          -- the election fetching it, without which nothing is requested
                          Leios.focusElectionIfUnfocused
                            (Leios.announcementElection (announcementOf point 0))
                            h
                          $ Leios.acquireEbBody h jobPool
                          $ Leios.recordMaxAnnouncementSlot h (SlotNo 10) SNothing
                          $ (emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0) :: LeiosOutstanding Int)
                      (_o, reqs, drops) =
                        leiosFetchLogicIteration
                          demoLeiosFetchStaticEnv
                          anyClosureSize
                          (Just (SlotNo 11))
                          (Map.singleton peerId (Map.singleton point (Leios.MkPeerOffer SNothing SNothing offer)))
                          Map.empty
                          outstanding
                   in (requestedOffsets reqs, Map.keys drops)
            -- only the jobs inside the prefix are requested, and with nothing else
            -- it can serve the offer is pruned
            run (MkClosureOffer (Map.singleton 0 prefix)) 0 @?= (IntSet.fromList [0, 1, 2], [peerId])
            -- a prefix short of the first tx serves nothing, and so does a body-only offer
            run (MkClosureOffer (Map.singleton 0 (txSizeOf 0 - 1))) 0 @?= (IntSet.empty, [peerId])
            run (MkClosureOffer Map.empty) 0 @?= (IntSet.empty, [peerId])
            -- the whole closure serves every job
            run (MkClosureOffer (Map.singleton 0 maxBound)) 0 @?= (IntSet.fromList ids, [peerId])
            -- a run offered out of order serves exactly the jobs inside it
            let mid = sum (map txSizeOf (take 2 ids))
                end5 = sum (map txSizeOf (take 5 ids))
            run (MkClosureOffer (Map.singleton mid end5)) 0 @?= (IntSet.fromList [2, 3, 4], [peerId])
            -- with budget for a single job the offer still has jobs to give, so it stays
            let (one, kept) = run (MkClosureOffer (Map.singleton 0 prefix)) (ordinaryCap - 1)
            IntSet.size one @?= 1
            assertBool "the one job is inside the prefix" (one `IntSet.isSubsetOf` IntSet.fromList [0, 1, 2])
            kept @?= []
      , testProperty
          "outstanding-state invariants hold across arbitrary sequences"
          prop_invariants
      , testProperty
          "the fetch logic never requests an already-held EB body"
          prop_neverRefetchesHeldBody
      , testProperty
          "a concurrent offer, announcement and body arrival keep the reverse index in sync (IOSimPOR)"
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
  | -- | The peer disconnects: @removePeerFromOutstanding@. Subsequent commands
    -- reuse the same peer id, so this also covers reconnection.
    Disconnect
  | -- | The EB body arrives, but its LeiosDb body write never lands -- what a
    -- peer killed while parked on a full writer queue leaves behind: the point
    -- row written, the body not. The state still reads 'BodyAcquired'.
    ArriveBodyLostWrite TestEb Word
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

-- | The announcing header a 'Announce' stands for: a block in that slot, by
-- 'issuerOf', announcing that EB.
announcingHeaderOf :: TestEb -> Word -> AnnouncingHeader LeiosTestBlock
announcingHeaderOf ids slot =
  case mkAnnouncingHeader (getHeader blk) of
    Nothing -> error "announcingHeaderOf: header announces nothing"
    Just anc -> anc
 where
  blk =
    announcing (pointOf ids slot) (encodeLeiosEbSize (ebOf ids)) $
      issuedBy (issuerOf ids) $
        iterate successorLeiosBlock (firstLeiosBlock 9) !! (fromIntegral slot - 1)

-- | A distinct issuer per 'TestEb', so that two endorser blocks announced in
-- one slot are two elections rather than one pool equivocating. 'headerElId'
-- keeps only the low byte, and the four 'worldEbs' map to 9, 17, 1 and 180.
issuerOf :: TestEb -> Word64
issuerOf = fromIntegral . foldl' (\acc i -> (acc * 7 + i + 1) `mod` 256) 0

------------------------------------------------------------
-- Harness
------------------------------------------------------------

-- | Run a command sequence in 'IOSim' against in-memory dependencies, checking
-- the invariant after each command. 'Left' names the first failing command.
runCmds :: [Cmd] -> Either String ()
runCmds = (() <$) . runCmdsReFetchViolations

collisionEb :: TestEb
collisionEb = [0, 1]

collisionHash :: EbHash
collisionHash = hashLeiosEb (ebOf collisionEb)

-- | Run a command sequence against a fresh in-memory LeiosDb, then report the
-- EB points it has registered (sorted) and every 'AcquiredEbTxs' it emitted, in
-- order (duplicates kept, so "exactly once" is checkable).
--
-- The in-memory writer performs each write at submission, so everything the
-- commands wrote has landed by the time 'withWriter' returns.
runCmdsCollision :: [Cmd] -> ([(SlotNo, EbHash)], [LeiosPoint])
runCmdsCollision cmds = runSimOrThrow go
 where
  go :: forall s. IOSim s ([(SlotNo, EbHash)], [LeiosPoint])
  go = do
    dbHandle <- LeiosDb.newLeiosDBInMemory
    chan <- LeiosDb.subscribeEbNotifications dbHandle
    withWriter dbHandle $ \conn -> do
      outstandingVar <- newMVar (emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0))
      readyVar <- newEmptyMVar
      peerVars <- newLeiosPeerVars IsNotBigLedgerPeer
      let kv = (outstandingVar, readyVar)
      centralVar <- newMVar Announcements.emptyCentralState
      mapM_
        (applyCmd centralVar conn nullLeiosTxCache kv peerVars (MkPeerId (0 :: Int)))
        cmds
    -- The plain selector: record dot cannot select a 'HasCallStack =>' field.
    points <- LeiosDb.withReader dbHandle LeiosDb.scanEbPoints
    let drain acc =
          atomically (tryReadTChan chan) >>= \case
            Nothing -> pure (reverse acc)
            Just n -> drain (n : acc)
    notifs <- drain []
    pure (sort points, [p | LeiosDb.AcquiredEbTxs p <- notifs])

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
    withWriter dbHandle $ \conn -> do
      outstandingVar <- newMVar (emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0))
      readyVar <- newEmptyMVar
      peerVars <- newLeiosPeerVars IsNotBigLedgerPeer
      centralVar <- newMVar Announcements.emptyCentralState
      let kv = (outstandingVar, readyVar)
          txCache = nullLeiosTxCache
          peerId = MkPeerId (0 :: Int)
          loop acc [] = pure (Right acc)
          loop acc (c : cs) = do
            r <-
              try (applyCmd centralVar conn txCache kv peerVars peerId c) ::
                IOSim s (Either SomeException [EbHash])
            case r of
              Left e -> pure (Left ("exception on " <> show c <> ": " <> show e))
              Right violations -> do
                outstanding <- readMVar outstandingVar
                -- Which bodies the LeiosDb really holds, for the no-absorbing-
                -- 'BodyAcquired' half of the invariant.
                dbBodies <-
                  LeiosDb.withReader dbHandle $ \rdr ->
                    fmap (Set.fromList . map fst . filter (not . null . snd)) $
                      mapM (\h -> (,) h <$> LeiosDb.lookupEbBody rdr h) $
                        Map.keys (Leios.ebState outstanding)
                case checkInvariant dbBodies outstanding of
                  Left msg -> pure (Left (msg <> " (after " <> show c <> ")"))
                  Right () -> loop (acc <> violations) cs
      loop [] cs0

-- | Apply a command, returning any EB bodies it requested that are already held
-- (per 'ebStateHasBody') — the re-fetch-storm violation. Empty for everything
-- but a misbehaving 'Decide'.
applyCmd ::
  forall s.
  MVar
    (IOSim s)
    (Announcements.CentralState (IOSim s) (PeerId Int) (AnnouncingHeader LeiosTestBlock)) ->
  LeiosDb.LeiosDbWriter (IOSim s) ->
  LeiosTxCache (IOSim s) () () Leios.SerializedEbBody ->
  (MVar (IOSim s) (LeiosOutstanding Int), MVar (IOSim s) ()) ->
  LeiosPeerVars (IOSim s) ->
  PeerId Int ->
  Cmd ->
  IOSim s [EbHash]
applyCmd centralVar conn txCache kv peerVars peerId = \case
  Announce ids slot -> do
    -- Through the same entry point the node uses, so that whatever a validated
    -- announcement is defined to do --- today that includes registering the
    -- point with the LeiosDb --- these commands do too, without this harness
    -- having to know what that is.
    --
    -- These invariants are about the fetch bookkeeping, which never reads the
    -- onset; only the voting path needs it.
    processAnnouncementCentrally
      nullTracer
      centralVar
      kv
      txCache
      conn
      (Just peerId)
      ReceivedViaLeiosNotify
      Announcements.DoRelay
      SNothing
      Nothing
      (announcingHeaderOf ids slot)
    pure []
  Offer ids slot -> do
    recordEbBodyOffer
      (snd kv)
      peerVars
      (pointOf ids slot, encodeLeiosEbSize (ebOf ids))
    pure []
  ArriveBody ids slot -> do
    let eb = ebOf ids
        req = MkLeiosBlockRequest (pointOf ids slot) (encodeLeiosEbSize eb) maxBound
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
  Disconnect -> do
    modifyMVar_ (fst kv) (pure . removePeerFromOutstanding peerId)
    pure []
  ArriveBodyLostWrite ids slot -> do
    let eb = ebOf ids
        req = MkLeiosBlockRequest (pointOf ids slot) (encodeLeiosEbSize eb) maxBound
        -- The body write is never enqueued, so its promise never resolves and
        -- the acquisition is never confirmed.
        lostConn = conn{LeiosDb.writeEbBody = \_ _ _ -> pure (LeiosDb.Promise (forever (threadDelay 1000000)))}
    processLeiosBlock
      nullTracer
      nullTracer
      kv
      txCache
      lostConn
      dummySystemTime
      noMempoolPull
      (ReceivedBlockFrom peerId req)
      eb
    pure []
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
            anyClosureSize
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

-- | Offer every EB the outstanding state tracks, at its 'ebStateMaxSlot',
-- offering both its body and its closure -- an all-offering peer, so the fetch
-- logic can act on whichever of the two each EB still needs.
--
-- Offering unconditionally is the point: declining to fetch must be the fetch
-- logic's own doing, not something this fixture arranged by withholding the
-- offer. One byte for the same reason: the size a peer claims is what its
-- request spends against the per-peer byte budget, so the smallest claim is
-- the one least likely to end an iteration before it has assigned everything
-- it would.
referencedOffers :: LeiosOutstanding Int -> Map.Map Leios.LeiosPoint Leios.PeerOffer
referencedOffers o =
  Map.fromList
    [ ( Leios.MkLeiosPoint (Leios.ebStateMaxSlot s) h
      , Leios.MkPeerOffer SNothing (SJust 1) Leios.wholeClosureOffer
      )
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
    Leios.LeiosBlockRequest r -> fromIntegral (Leios.lbrOfferedSize r)
    Leios.LeiosBlockTxsRequest (Leios.MkLeiosBlockTxsRequest _p jobs) ->
      sum
        [ off
        | Jobs.MkLeiosJob offs _bytes _start _end _root <- toList jobs
        , off <- IntSet.toList offs
        ]

-- | The 'EbHash'es the requests fetch an EB body for (one entry per request; with
-- no per-EB cap, an EB may appear once per offering peer).
ebBodyRequestHashes :: Map.Map peer (NESeq Leios.LeiosFetchRequest) -> [EbHash]
ebBodyRequestHashes m =
  [ p.pointEbHash
  | reqs <- Map.elems m
  , Leios.LeiosBlockRequest (Leios.MkLeiosBlockRequest p _sz _) <- toList reqs
  ]

-- | The union of every tx offset the requests fetch, across all peers.
requestedOffsets :: Map.Map peer (NESeq Leios.LeiosFetchRequest) -> IntSet.IntSet
requestedOffsets m =
  IntSet.unions
    [ offs
    | reqs <- Map.elems m
    , Leios.LeiosBlockTxsRequest (Leios.MkLeiosBlockTxsRequest _p jobs) <- toList reqs
    , Jobs.MkLeiosJob offs _bytes _start _end _root <- toList jobs
    ]

------------------------------------------------------------
-- The invariant
------------------------------------------------------------

-- | The closure bound these tests forecast: large enough that nothing here
-- trips it. 'Test.Consensus.Leios.RecoveryPath' is where the bound itself is
-- exercised, against a real node.
anyClosureSize :: SlotNo -> Either OutsideForecastRange Leios.BytesSize
anyClosureSize _slot = Right maxBound

-- | Three invariants:
--
-- * 'ebsPerMaxAnnouncementSlot' must be the exact inverse of the greatest-slot
--   field of 'ebState' (the reverse index 'pruneOutstandingToImmTip' prunes by).
--
-- * 'numMissingBodies' must be the count 'ebState' would give if we walked it
--   (which nothing does at run time, which is why this checks it).
--
-- * No absorbing 'BodyAcquired': see below.
--
-- (The old missing-tx \/ reverse-index invariant is gone with the EbTxs rewrite.)
--
-- @dbBodies@: the EBs the LeiosDb actually holds a body for. Passed in because
-- the central invariant is a claim about the database, not about the state alone.
checkInvariant :: Set.Set EbHash -> LeiosOutstanding Int -> Either String ()
checkInvariant dbBodies o
  | Leios.ebsPerMaxAnnouncementSlot o /= inverseOfMax =
      Left
        ( "ebsPerMaxAnnouncementSlot desynced from ebState: "
            <> show (Leios.ebsPerMaxAnnouncementSlot o, inverseOfMax)
        )
  | Leios.numMissingBodies o /= countedMissing =
      Left
        ( "numMissingBodies desynced from ebState: "
            <> show (Leios.numMissingBodies o, countedMissing)
        )
  -- 'BodyAcquired' asserts the LeiosDb holds the body, and the fetch logic
  -- retires a peer's offer for good on the strength of it. So the state may only
  -- ever be reached through a durable write ('acquireEbBody'): anything else
  -- strands the EB.
  | not (null stranded) =
      Left ("BodyAcquired but absent from the LeiosDb: " <> show stranded)
  | otherwise = Right ()
 where
  acquired =
    Map.keysSet $
      flip Map.filter (Leios.ebState o) $ \(Leios.MkEbState _ _ fs) -> case fs of
        Leios.BodyAcquired{} -> True
        _ -> False
  stranded = Set.toList (acquired `Set.difference` dbBodies)
  inverseOfMax =
    Map.fromListWith
      NESet.union
      [ (Leios.ebStateMaxSlot s, NESet.singleton h)
      | (h, s) <- Map.toList (Leios.ebState o)
      ]
  countedMissing = sum (map Leios.wantsBody (Map.elems (Leios.ebState o)))

------------------------------------------------------------
-- Curated repros
------------------------------------------------------------

-- | A peer offers an EB body; we forge the same EB before the offered body
-- arrives. Forging must leave that endorser block's 'ebState' reading
-- 'BodyAcquired', so the fetch logic never re-requests a body we already hold.
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
    , pure Disconnect
    , pure (ArriveBodyLostWrite ids slot)
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
  Disconnect -> "Disconnect"
  ArriveBodyLostWrite{} -> "ArriveBodyLostWrite"

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
-- the storm the fetch bookkeeping used to be vulnerable to: three handlers
-- writing about one endorser block at three slots at once. What they write now
-- is 'ebState', keyed by hash, and 'ebsPerMaxAnnouncementSlot', its reverse
-- index --- so what is left to violate is 'checkInvariant'.
--
-- Each handler's read and its state update are one 'outstandingVar' critical
-- section, so no interleaving can desync the two; this guards against moving a
-- check back out of the lock.
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
  withWriter dbHandle $ \conn -> do
    outstandingVar <- newMVar (emptyLeiosOutstanding (mkStdGen 0) (SlotNo 0))
    readyVar <- newEmptyMVar
    peerVars <- newLeiosPeerVars IsNotBigLedgerPeer
    txCache <- newPureLeiosTxCache defaultLeiosTxCacheShift
    let kv = (outstandingVar, readyVar)
        peerId = MkPeerId (0 :: Int)
        ids = [0, 1] :: TestEb
        eb = ebOf ids
        ebBytesSize = encodeLeiosEbSize eb
        -- One hash (same ids), three different slots.
        offerPoint = pointOf ids 10
        announcePoint = pointOf ids 11
        arrivalPoint = pointOf ids 12
    concurrently_
      (recordEbBodyOffer (snd kv) peerVars (offerPoint, ebBytesSize))
      ( concurrently_
          (recordAnnouncedEb kv SNothing (announcementOf announcePoint ebBytesSize))
          ( processLeiosBlock
              nullTracer
              nullTracer
              kv
              txCache
              conn
              dummySystemTime
              noMempoolPull
              (ReceivedBlockFrom peerId (MkLeiosBlockRequest arrivalPoint ebBytesSize maxBound))
              eb
          )
      )
    outstanding <- readMVar outstandingVar
    -- Which bodies the LeiosDb really holds, for the no-absorbing-
    -- 'BodyAcquired' half of the invariant.
    dbBodies <-
      LeiosDb.withReader dbHandle $ \rdr ->
        fmap (Set.fromList . map fst . filter (not . null . snd)) $
          mapM (\h -> (,) h <$> LeiosDb.lookupEbBody rdr h) $
            Map.keys (Leios.ebState outstanding)
    -- Three handlers wrote about one endorser block at three slots at once; the
    -- reverse index must still be the exact inverse of 'ebState', whichever
    -- order they ran in.
    pure $ case checkInvariant dbBodies outstanding of
      Right () -> counterexample "" True
      Left why -> counterexample why False

-- | An announcement of this endorser block, in an election of its own.
--
-- These tests are about the fetch bookkeeping rather than about which
-- announcement an election is fetching, so the election is derived from the
-- endorser block: no two announcements here ever compete for one election.
announcementOf :: LeiosPoint -> Leios.BytesSize -> Leios.AnnouncementFields
announcementOf point size =
  Leios.MkAnnouncementFields
    (MkElId (Leios.pointSlotNo point) (SBS.toShort (Leios.ebHashBytes ebHash)))
    ebHash
    size
 where
  ebHash = Leios.pointEbHash point
