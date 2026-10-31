{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | What the node's LeiosFetch clients request, decided for all of them at
-- once: the node-wide state of what is outstanding, and the pure decision
-- that assigns the peers' offers to requests.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( -- * Requests
    LeiosFetchRequest (..)
  , LeiosBlockRequest (..)
  , LeiosBlockTxsRequest (..)
  , LeiosEbBounds (..)
  , offsetsToBitmap

    -- * Offers
  , WhetherTxsClosureOffered (..)
  , PeerOffer (..)

    -- * Outstanding state
  , LeiosOutstanding (..)
  , EbState (..)
  , EbFetchState (..)
  , BodySource (..)
  , ebStateMaxSlot
  , ebStateOnset
  , ebStateHasBody
  , heldAcquisitionsAt
  , emptyLeiosOutstanding
  , initializeLeiosOutstanding
  , newLeiosJobPool
  , acquireEbBody
  , recordMaxAnnouncementSlot
  , focusElectionIfUnfocused
  , focusCertifiedEb
  , trackCertifiedEb
  , focusElection
  , isFocusedEb
  , pruneOutstandingToImmTip

    -- * Settled requests
  , removePeerFromOutstanding
  , refundEbRequest
  , refundTxRequest
  , completeTxRequest

    -- * Deciding
  , LeiosFetchStaticEnv (..)
  , defaultLeiosFetchStaticEnv
  , leiosFetchLogicIteration
  , fetchPriorityTiers
  , peerBudget
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.Bits as Bits
import qualified Data.Foldable as F
import Data.IntMap.NonEmpty (NEIntMap)
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.IntSet.NonEmpty (NEIntSet)
import qualified Data.IntSet.NonEmpty as NEIntSet
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Ord (Down (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Sequence.NonEmpty as NESeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Data.Word (Word64)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime)
import Ouroboros.Consensus.Forecast (OutsideForecastRange)
import Ouroboros.Consensus.Leios.Acquisitions (LeiosEbNotification (..))
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , ElectionId (..)
  , EndorserBlockHash
  , LeiosPoint (..)
  , TxHash
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
  ( TxBitmaps
  , maxLeiosTxsRequestBytesSize
  )
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (IsBigLedgerPeer (..))
import System.Random (StdGen)

{-------------------------------------------------------------------------------
  Requests
-------------------------------------------------------------------------------}

data LeiosFetchRequest
  = LeiosBlockRequest LeiosBlockRequest
  | LeiosBlockTxsRequest LeiosBlockTxsRequest

-- | A request for an endorser block's body.
data LeiosBlockRequest = MkLeiosBlockRequest
  { lbrPoint :: !LeiosPoint
  , lbrOfferedSize :: !BytesSize
  -- ^ The size the peer offered this endorser block at; not sent, but used by
  -- the reply handler.
  , lbrEbBounds :: !LeiosEbBounds
  -- ^ The ledger's bounds on this block; not sent, but used by the reply
  -- handler.
  }

-- | The ledger's bounds on an endorser block, as of its slot.
data LeiosEbBounds = MkLeiosEbBounds
  { lebMaxBodySize :: !BytesSize
  -- ^ the largest body the ledger admits
  , lebMaxTxsSize :: !BytesSize
  -- ^ the largest total size of the transactions a body may reference
  }
  deriving (Eq, Show, Bounded)

data LeiosBlockTxsRequest
  = -- | A request for some of an EB's txs: its point and the 'Jobs.LeiosJob's it
    -- covers, keyed by job id (so a request can't list a job twice), each with its
    -- full commitment. Everything needed to validate the response is carried here,
    -- so the reply handler needs no lookup into the (body-less) job pool -- and a
    -- redundant response for a job that has since completed still validates. On
    -- the wire only the offset bitmap goes to the peer; it is derived from the
    -- union of the jobs' offsets (see 'offsetsToBitmap'), not stored.
    MkLeiosBlockTxsRequest
      !LeiosPoint
      !(NEIntMap Jobs.LeiosJob)

-- | The offset set as the wire bitmap (chunk index, 64-bit mask).
offsetsToBitmap :: IntSet.IntSet -> TxBitmaps
offsetsToBitmap offsets =
  [ (fromIntegral q, bm)
  | (q, bm) <- IntMap.toAscList chunks
  ]
 where
  chunks =
    IntSet.foldr
      (\off -> let (q, r) = off `divMod` 64 in IntMap.insertWith (Bits..|.) q (Bits.bit (63 - r)))
      IntMap.empty
      offsets

{-------------------------------------------------------------------------------
  Offers
-------------------------------------------------------------------------------}

-- | Whether a peer has offered an endorser block's tx-closure.
data WhetherTxsClosureOffered = TxsClosureOffered | TxsClosureNotOffered
  deriving (Eq, Show)

-- | Offered if either says so.
instance Semigroup WhetherTxsClosureOffered where
  TxsClosureOffered <> _ = TxsClosureOffered
  TxsClosureNotOffered <> y = y

instance Monoid WhetherTxsClosureOffered where
  mempty = TxsClosureNotOffered

-- | What one peer has offered for one endorser block point.
--
-- The two LeiosNotify offers are independent messages: either can arrive
-- first, or alone. A CertRB roll-forward makes both at once.
--
-- The body offer carries a size and the closure offer does not, and that
-- asymmetry is not an oversight. We hold nothing to size a body request by, so
-- the peer's claim is both what the request asks for and what it spends
-- against that peer's byte budget. Closure jobs carry their own byte counts,
-- taken from the body we already hold by the time 'assignClosure' runs, so
-- there is nothing there for a peer to claim.
data PeerOffer = MkPeerOffer
  { poEbBounds :: !(StrictMaybe LeiosEbBounds)
  -- ^ The ledger's bounds on this offered EB. It's 'SNothing' if
  -- the offer arrived via LeiosNotify, and 'SJust' if it arrived via ChainSync.
  --
  -- The decision logic uses forecasting in the 'SNothing' case. We avoid
  -- forecasting in the ChainSync case so that the Leios Recovery Path can
  -- handle sparse chains.
  , poOfferedBody :: !(StrictMaybe BytesSize)
  -- ^ The size this peer offered the body at, if it has offered the body.
  --
  -- The peer's own claim, and the only size that decides anything: no
  -- announcement gets a say (see 'assignBody'), and what the endorser block
  -- really weighs is settled by hashing the bytes that arrive.
  --
  -- Offering one point twice costs the peer its connection, but one hash at
  -- two /points/ does not: those are separate entries, each needing its own
  -- announcement, and nothing here arbitrates between the sizes they name.
  --
  -- TODO disconnect a peer that ever offers one 'EndorserBlockHash' at two
  -- sizes, whichever messages the two arrived on. That is proof it is lying,
  -- and today we keep both and carry on.
  , poClosure :: !WhetherTxsClosureOffered
  }
  deriving (Eq, Show)

-- | Each field on its own terms: the leftmost size wins, which for
-- 'Map.insertWith' is the newer offer's, and a peer that makes that choice
-- matter has already lost our trust (see 'poOfferedBody').
--
-- The bound is not a claim of the peer's but a value we computed, so the two
-- sides agree whenever both have one and either will do.
instance Semigroup PeerOffer where
  MkPeerOffer b1 sz1 c1 <> MkPeerOffer b2 sz2 c2 =
    MkPeerOffer (pickSize b1 b2) (pickSize sz1 sz2) (c1 <> c2)
   where
    pickSize SNothing y = y
    pickSize x@SJust{} _ = x

instance Monoid PeerOffer where
  mempty = MkPeerOffer SNothing SNothing mempty

{-------------------------------------------------------------------------------
  Outstanding state
-------------------------------------------------------------------------------}

-- | Main data structure used in the Leios fetching logic.
--
-- Tracks both EB-level state (what EBs we have/need) and TX-level state
-- (what TXs we need for each EB), along with request tracking for bandwidth
-- management.
--
-- TODO: Potential simplifications once we have better test coverage:
--
-- 1. Consider separating "offer tracking" from "request tracking" into distinct
--    data structures for clarity.
data LeiosOutstanding peer = MkLeiosOutstanding
  { -- EB-level tracking
    ebState :: !(Map EndorserBlockHash EbState)
  -- ^ Per-EB state for every EB this node is tracking. Three things add an
  -- entry: the announcement that takes an election's focus, a verified
  -- certificate ('trackCertifiedEb'), and the start-up seeding
  -- ('initializeLeiosOutstanding'), which restores what the LeiosDb already
  -- holds. An offer never does, and is only ever acted on for an endorser
  -- block already here.
  , ebsPerMaxAnnouncementSlot :: !(Map SlotNo (NESet EndorserBlockHash))
  -- ^ Slot-keyed reverse index of 'ebStateMaxSlot' on 'ebState'
  --
  -- Used to accelerate pruning.
  , elFocus :: !(Map ElectionId EndorserBlockHash)
  -- ^ The endorser block each election is fetching: the first announcement we
  -- processed for that election, until a certificate names one, which wins.
  --
  -- Two activities decide it. Processing an announcement fills an empty slot
  -- and nothing more ('focusElectionIfUnfocused'). Processing a certificate
  -- overrides whatever is there ('focusElection') --- the CertRB itself, when
  -- its certificate is verified, not its header, which only records an offer.
  -- Pruning is the only other writer, and it just drops the elections below
  -- the immutable tip.
  , ebFocusingElections :: !(Map EndorserBlockHash (NESet ElectionId))
  -- ^ The inverse of 'elFocus': the elections fetching each endorser block.
  -- 'fetchPriorityTiers' ranks an endorser block by their slots.
  --
  -- Unless an endorser block is in here, the decision logic asks no peer for
  -- anything about it: neither its body nor any of its closure's jobs, since
  -- this is what filters a peer's offers before either is considered. A body
  -- we already hold stays held and its job pool stays as it is, so an election
  -- that focuses it later resumes rather than restarts.
  , outstandingPrunedSlot :: !SlotNo
  -- ^ The slot this whole 'LeiosOutstanding' has most recently been pruned up
  -- to: 'pruneOutstandingToImmTip' moves it in the same record it prunes
  -- 'ebState' and 'elFocus' in, so it says how far both of them have got.
  --
  -- Used to robustly prevent re-inserting what has already been pruned out.
  , numMissingBodies :: !Int
  -- ^ How many 'ebState' entries have no body: the body-fetch backlog,
  -- maintained as 'ebState' changes rather than counted, so reading it stays
  -- O(1) however often the fetch loop runs. 'wantsBody' is the per-entry
  -- contribution.
  , -- Request tracking
    requestedEbPeers :: !(Map EndorserBlockHash (Set peer))
  -- ^ Which peers we've requested each EB from
  --
  -- TODO add requestedEbsPerPeer :: !(Map peer (NESet EndorserBlockHash)) to
  -- avoid linear scan
  , requestedBytesSizePerPeer :: !(Map peer BytesSize)
  -- ^ Running total of bytes requested from each peer. This is the only
  -- outstanding-byte limit: there is no global cap (the global bound falls out
  -- as this per-peer cap times the peer count).
  , requestedJobsPerPeer :: !(Map peer (Map EndorserBlockHash NEIntSet))
  -- ^ Per peer, per EB, the job ids it currently has in flight -- for
  -- decrementing those multiplicities on disconnect
  , leiosFetchPrng :: !StdGen
  -- ^ The LeiosFetch decision loop's own PRNG, threaded through each iteration.
  -- Used to shuffle which job is drawn when assigning tx-closure work to a peer
  -- (a uniform pick within the least-requested multiplicity bucket), so job ids
  -- aren't consumed in a fixed order. Seeded once at start-up.
  , nextLeiosJobId :: !Jobs.LeiosJobId
  -- ^ The first job id that no pool has used yet; see 'newLeiosJobPool'.
  }

-- | The empty outstanding state, given the slot it has already been pruned up
-- to. The caller supplies the immutable-tip slot at startup so that a body at
-- or below it reads as too old from the outset (see
-- 'outstandingPrunedSlot' / 'pruneOutstandingToImmTip'), and the seed for
-- the decision loop's PRNG (see 'leiosFetchPrng').
emptyLeiosOutstanding :: StdGen -> SlotNo -> LeiosOutstanding peer
emptyLeiosOutstanding prng prunedSlot =
  MkLeiosOutstanding
    { ebState = Map.empty
    , ebsPerMaxAnnouncementSlot = Map.empty
    , elFocus = Map.empty
    , ebFocusingElections = Map.empty
    , outstandingPrunedSlot = prunedSlot
    , numMissingBodies = 0
    , requestedEbPeers = Map.empty
    , requestedBytesSizePerPeer = Map.empty
    , requestedJobsPerPeer = Map.empty
    , leiosFetchPrng = prng
    , nextLeiosJobId = Jobs.MkLeiosJobId 0
    }

-- | Per-EB state tracked in 'ebState'
data EbState
  = -- | The greatest slot at which the EB has been announced, the wall-clock
    -- onset of its earliest announcement slot, and the current progress of
    -- fetching it.
    --
    -- The acquisition traces subtract that onset from the arrival time, so
    -- they say how long after this EB was first announced we got it. It is
    -- 'SNothing' when nothing supplied an onset, as for a verified certificate
    -- or the start-up seeding. See 'recordMaxAnnouncementSlot' for why the slot
    -- is the greatest of them and the onset the earliest.
    MkEbState !SlotNo !(StrictMaybe RelativeTime) !EbFetchState
  deriving (Eq, Show)

-- | Whether we hold an EB's body.
data EbFetchState
  = -- | We've only ever received an announcement.
    NoBody
  | -- | The job pool: the jobs whose txs are not yet durably written.
    --
    -- The body itself is /not/ retained -- it lives in the LeiosDb, and each job
    -- carries a 'Jobs.JobRootHash' commitment sufficient to validate its
    -- response. Retaining up to ~10k bodies (each up to ~600 kB) would cost
    -- gigabytes.
    --
    -- TODO the 'Jobs.LeiosJobPool' could be an 'MVar m LeiosJobPool' for per-EB
    -- locking, at the cost of an 'm' parameter on
    -- EbFetchState\/EbState\/LeiosOutstanding and a monadic body-acquire;
    -- deferred.
    BodyAcquired !BodySource !Jobs.LeiosJobPool
  deriving (Eq, Show)

-- | Where a body we count as held came from, which decides what the node may
-- announce about it ('heldAcquisitionsAt').
data BodySource
  = -- | Fetched and durably written during this run, at this size.
    FetchedBody !BytesSize
  | -- | Restored from the LeiosDb at start-up ('initializeLeiosOutstanding'), at
    -- a size we do not know.
    RestoredBody
  | -- | Fetched, but with a closure too big to fetch, so never written.
    OversizedBody
  deriving (Eq, Show)

ebStateMaxSlot :: EbState -> SlotNo
ebStateMaxSlot (MkEbState slot _onset _fetchState) = slot

-- | The recorded onset of the EB's oldest announcement slot, if any (see
-- 'MkEbState'); the arrival handlers use it to report the EB's age on arrival.
ebStateOnset :: EbState -> StrictMaybe RelativeTime
ebStateOnset (MkEbState _slot onset _fetchState) = onset

-- | Whether we already hold the EB's body (the "do we have it?" test that the
-- offer/announcement/arrival paths consult before fetching).
ebStateHasBody :: EbState -> Bool
ebStateHasBody (MkEbState _slot _onset fetchState) = case fetchState of
  NoBody -> False
  BodyAcquired{} -> True

-- | What to announce again when an election newly focuses this point's
-- endorser block and we hold it: its body, if we know the body's size, and its
-- closure, if we hold every transaction. See 'LeiosEbNotification'.
heldAcquisitionsAt :: LeiosPoint -> LeiosOutstanding peer -> [LeiosEbNotification]
heldAcquisitionsAt point outstanding = case Map.lookup (pointEbHash point) (ebState outstanding) of
  Just (MkEbState _slot _onset (BodyAcquired source jobPool)) ->
    let closure = [AcquiredEbTxs point | Jobs.nullLeiosJobPool jobPool]
     in case source of
          FetchedBody size -> AcquiredEb point size : closure
          RestoredBody -> closure
          OversizedBody -> []
  Just (MkEbState _slot _onset NoBody) -> []
  Nothing -> []

-- | A pool of jobs to fetch these missing txs of an EB's closure, given as in
-- 'Jobs.mkLeiosJobPool', and the state with its jobs' ids taken.
newLeiosJobPool ::
  LeiosFetchStaticEnv ->
  IntMap.IntMap (TxHash, BytesSize) ->
  LeiosOutstanding peer ->
  (Jobs.LeiosJobPool, LeiosOutstanding peer)
newLeiosJobPool env misses outstanding =
  (jobPool, outstanding{nextLeiosJobId = nextId})
 where
  (jobPool, nextId) =
    Jobs.mkLeiosJobPool
      (maxJobBytesSize env)
      (maxJobTxCount env)
      (nextLeiosJobId outstanding)
      misses

-- | The body's LeiosDb write is durable, so claim 'BodyAcquired' with the pool
-- of jobs the settled write still leaves to fetch.
--
-- There is no intermediate "write in flight" state. During the (brief, async)
-- write window the EB reads as not-held ('ebStateHasBody' is 'False'), so a
-- concurrent offer may redundantly re-fetch the body -- cheaper than tracking an
-- in-flight state and reverting it on a lost write, which with the single writer
-- cannot happen without bringing the node down anyway. The body write is only
-- enqueued before this is reached, and the enqueue only parks on a free queue
-- slot, so a cancellation there leaves no claim to strand.
acquireEbBody ::
  EndorserBlockHash ->
  BodySource ->
  Jobs.LeiosJobPool ->
  LeiosOutstanding peer ->
  LeiosOutstanding peer
acquireEbBody ebHash source jobPool =
  alterEbState ebHash $ \case
    -- Pruned before the write landed (we could not have requested it
    -- without an announcement, and that was since evicted): ignore.
    Nothing -> Nothing
    Just (MkEbState slot onset fetchState) -> case fetchState of
      NoBody -> Just $ MkEbState slot onset (BodyAcquired source jobPool)
      -- Already acquired -- e.g. a concurrent redundant delivery landed
      -- first; keep the existing pool rather than clobbering it.
      BodyAcquired{} -> Nothing

-- | Record that the EB with this hash is referenced --- announced, or named by
-- a verified certificate --- at this slot, along with that slot's wall-clock
-- onset if known.
--
-- The same EB (hash) can be referenced by several points; we keep the
-- /greatest/ such slot, so the EB's state isn't pruned prematurely. The onset,
-- in contrast, is kept as the /minimum/ (oldest announcement), so the arrival
-- handlers report the age since the EB was first heralded. An offer carries no
-- onset ('SNothing') and so never overrides one already recorded by an
-- announcement.
recordMaxAnnouncementSlot ::
  EndorserBlockHash ->
  SlotNo ->
  StrictMaybe RelativeTime ->
  LeiosOutstanding peer ->
  LeiosOutstanding peer
recordMaxAnnouncementSlot ebHash slot onset =
  alterEbState ebHash $ \case
    Nothing -> Just $ MkEbState slot onset NoBody
    Just (MkEbState oldSlot oldOnset fetchState) ->
      let newSlot = max slot oldSlot
          newOnset = minOnset onset oldOnset
       in if newSlot == oldSlot && newOnset == oldOnset
            then Nothing
            else Just $ MkEbState newSlot newOnset fetchState

-- | Combine two onsets, keeping the earlier and treating 'SNothing' as absent.
minOnset :: StrictMaybe RelativeTime -> StrictMaybe RelativeTime -> StrictMaybe RelativeTime
minOnset SNothing y = y
minOnset x SNothing = x
minOnset (SJust a) (SJust b) = SJust (min a b)

-- | Initialize the outstanding state
--
-- Its contents are only @'BodyAcquired' 'Jobs.emptyLeiosJobPool'@ for the EBs
-- the LeiosDb already holds /in full/: a complete tx closure whose announcer is
-- no older than the immutable tip. Each is marked 'BodyAcquired' with an empty
-- job pool so we neither re-fetch nor re-process (eg emit 'AcquiredEb') an EB
-- whose closure we already have at start-up time.
--
-- Everything else in the LeiosDb is deliberately ignored here, and it's
-- safe to do so:
--
--   * A merely body-held EB with a /partial/ closure is not seeded: an
--     empty pool would strand its missing txs. Left absent, it is
--     re-derived from a fresh announcement/offer, and the redundant body
--     re-fetch + re-insert is idempotent.
--
--   * Complete closures at or below the immutable tip are already final;
--     they would read as too old anyway, so there is nothing to track.
--
-- The announcements on the initially selected chain are likewise not replayed
-- here, though those blocks never roll forward and so are never announced to
-- us this run. Only two things list an endorser block to fetch --- the
-- announcement that takes an election's focus, and 'focusCertifiedEb' on a
-- verified certificate --- and between them they reach every endorser block we
-- could come to need. A CertRB on the initial chain has its endorser block
-- already, since it could not have been selected otherwise; one that is not on
-- the initial chain is re-fetched if some chain we would select needs it, and
-- verifying its certificate then sets the focus and lists it.
--
-- The per-peer request-tracking fields must start empty regardless: there are
-- no connections yet and nothing is in flight.
initializeLeiosOutstanding :: StdGen -> [LeiosPoint] -> SlotNo -> LeiosOutstanding peer
initializeLeiosOutstanding prng points immTipSlot =
  F.foldl' (flip seed1) (emptyLeiosOutstanding prng immTipSlot) points
 where
  -- These points were read back out of the LeiosDb, so the body is durable
  -- already: settle it at once.
  seed1 (MkLeiosPoint slot ebHash) =
    acquireEbBody ebHash RestoredBody Jobs.emptyLeiosJobPool
      . recordMaxAnnouncementSlot ebHash slot SNothing

-- | Make this endorser block the one this election is fetching, unless the
-- election is already fetching one.
--
-- The first announcement we see for an election wins, which is what an honest
-- electee's single announcement gives; only a certificate ('focusElection')
-- overrules it.
focusElectionIfUnfocused ::
  ElectionId -> EndorserBlockHash -> LeiosOutstanding peer -> LeiosOutstanding peer
focusElectionIfUnfocused elId ebHash outstanding
  | Map.member elId (elFocus outstanding) = outstanding
  | otherwise = focusElection elId ebHash outstanding

-- | A verified certificate moves its election's focus onto the endorser block
-- it names, and lists that body as one to fetch.
--
-- The endorser block this names may never have been listed. The announcement
-- that took the election's focus listed its own; this one's may have lost that
-- race, or may never have reached us --- and nothing lists it in the meantime,
-- because an unverified claim must not be able to make us track an endorser
-- block.
--
-- It does nothing for an election older than what has been pruned: its
-- certificate may have been read before the pruning, and focusing it now would
-- track, and announce held acquisitions at, what the pruning dropped.
focusCertifiedEb ::
  ElectionId -> EndorserBlockHash -> LeiosOutstanding peer -> LeiosOutstanding peer
focusCertifiedEb elId ebHash outstanding
  | electionSlot elId < outstandingPrunedSlot outstanding = outstanding
  | otherwise =
      focusElection elId ebHash $
        trackCertifiedEb (MkLeiosPoint (electionSlot elId) ebHash) outstanding

-- | Start tracking a certified endorser block, unless it is too old.
--
-- An 'ebState' entry is what makes a body fetchable at all: the decision logic
-- skips any offer of an endorser block it has no entry for. The announcement
-- this certificate names may have no entry, in two ways. Its announcing block
-- may have been on our selection since start-up, so its header never rolled
-- forward and nothing announced it to us. Or it may have been a second
-- announcement for an election already focused elsewhere, which is
-- deliberately not tracked.
--
-- Already holding the body is not a special case: 'recordMaxAnnouncementSlot'
-- leaves the fetch state alone, so nothing is re-fetched.
trackCertifiedEb :: LeiosPoint -> LeiosOutstanding peer -> LeiosOutstanding peer
trackCertifiedEb point outstanding
  | ebSlot < outstandingPrunedSlot outstanding = outstanding
  | otherwise = recordMaxAnnouncementSlot ebHash ebSlot SNothing outstanding
 where
  MkLeiosPoint ebSlot ebHash = point

-- | Make this endorser block the one this election is fetching, whatever it was
-- fetching before.
--
-- Whatever it was fetching stops being asked for, unless another election is
-- also fetching it. Nothing else about that endorser block is disturbed: we
-- keep the body if we acquired it, and we still take whatever was already
-- requested for it, since those bytes are paid for either way.
--
-- TODO One election, one certified endorser block --- which holds only while
-- enough of the committee's weight is honest, since two valid certificates for
-- one election means the committee equivocated. Should that ever happen, this
-- is last-claim-wins: the focus lands on whichever certificate we validated
-- most recently, so the node can be left fetching the endorser block of one
-- fork while the chain it would select needs the other, and it stays stuck
-- there until something moves the focus again. The recourse is to restart the
-- node and hope the certificates arrive in the other order, since this state is
-- in memory only and the order they come back in is the network's to decide. We
-- deliberately do not spend complexity on that case here, since it requires
-- catastrophically buggy nodes and/or an amount of adversarial stake the
-- protocol itself is not designed to resist.
focusElection ::
  ElectionId -> EndorserBlockHash -> LeiosOutstanding peer -> LeiosOutstanding peer
focusElection elId ebHash outstanding = case Map.lookup elId (elFocus outstanding) of
  Just oldEbHash | oldEbHash == ebHash -> outstanding
  mbOldEbHash ->
    outstanding
      { elFocus = Map.insert elId ebHash (elFocus outstanding)
      , ebFocusingElections =
          Map.insertWith NESet.union ebHash (NESet.singleton elId) $
            maybe id (unfocusing elId) mbOldEbHash (ebFocusingElections outstanding)
      }

-- | Remove an election from those fetching this endorser block.
unfocusing ::
  ElectionId ->
  EndorserBlockHash ->
  Map EndorserBlockHash (NESet ElectionId) ->
  Map EndorserBlockHash (NESet ElectionId)
unfocusing elId = Map.update (NESet.nonEmptySet . NESet.delete elId)

-- | Whether some election is currently fetching this endorser block.
isFocusedEb :: EndorserBlockHash -> LeiosOutstanding peer -> Bool
isFocusedEb ebHash = Map.member ebHash . ebFocusingElections

-- | Whether this EB's body is one we are still trying to fetch: the per-entry
-- contribution to 'numMissingBodies'.
wantsBody :: EbState -> Int
wantsBody (MkEbState _slot _onset fetchState) = case fetchState of
  NoBody -> 1
  BodyAcquired{} -> 0

-- | Upsert an EB's 'ebState' entry, keeping 'ebsPerMaxAnnouncementSlot' in step
-- whenever the entry's max slot moves. The supplied function must be
-- slot-monotonic (never lower the greatest slot), which both callers are.
alterEbState ::
  EndorserBlockHash ->
  -- | REQUIREMENT: must not reduce 'ebStateMaxSlot'
  (Maybe EbState -> Maybe EbState) ->
  LeiosOutstanding peer ->
  LeiosOutstanding peer
alterEbState ebHash f outstanding =
  case Map.alterF upsert1 ebHash (ebState outstanding) of
    (Nothing, _) -> outstanding
    (Just (mbOldSlot, newSlot, deltaMissing), ebState') ->
      outstanding
        { ebState = ebState'
        , numMissingBodies = numMissingBodies outstanding + deltaMissing
        , ebsPerMaxAnnouncementSlot =
            if mbOldSlot == Just newSlot
              then ebsPerMaxAnnouncementSlot outstanding -- max slot unchanged
              else Map.insertWith NESet.union newSlot (NESet.singleton ebHash) $
                case mbOldSlot of
                  Nothing ->
                    ebsPerMaxAnnouncementSlot outstanding
                  Just oldSlot ->
                    Map.update
                      (NESet.nonEmptySet . NESet.delete ebHash)
                      oldSlot
                      (ebsPerMaxAnnouncementSlot outstanding)
        }
 where
  -- One traversal of 'ebState': the pair functor carries whether the entry
  -- changed at all and, if so, the prior and new greatest slots for the
  -- reverse-index update and this entry's contribution to 'numMissingBodies'.
  upsert1 mbOld = case f mbOld of
    Nothing -> (Nothing, mbOld)
    Just new ->
      ( Just
          ( ebStateMaxSlot <$> mbOld
          , ebStateMaxSlot new
          , wantsBody new - maybe 0 wantsBody mbOld
          )
      , Just new
      )

-- | Prune 'LeiosOutstanding' to the immutable tip, returning the EB hashes it
-- dropped (so the caller can drop those same hashes from the peers' offers).
--
-- Uses the 'ebsPerMaxAnnouncementSlot' reverse index to drop the below-tip prefix
-- directly (@spanAntitone@), rather than scanning the whole map.
--
-- TODO still more it could prune (e.g. abandoned in-flight EB requests).
pruneOutstandingToImmTip ::
  SlotNo -> LeiosOutstanding peer -> (Set EndorserBlockHash, LeiosOutstanding peer)
pruneOutstandingToImmTip immTipSlot outstanding =
  ( prunedHashes
  , outstanding
      { ebState = ebState outstanding `Map.withoutKeys` prunedHashes
      , ebsPerMaxAnnouncementSlot = atOrAbove
      , elFocus = elFocusAtOrAbove
      , ebFocusingElections = ebFocusingElections'
      , outstandingPrunedSlot = max (outstandingPrunedSlot outstanding) immTipSlot
      , numMissingBodies = numMissingBodies outstanding - prunedMissing
      }
  )
 where
  (below, atOrAbove) =
    Map.spanAntitone (< immTipSlot) (ebsPerMaxAnnouncementSlot outstanding)
  prunedHashes = Set.unions (map NESet.toSet (Map.elems below))

  -- 'ElectionId' orders slot-first, so the below-tip elections are a prefix. A
  -- pruned election releases whatever it was fetching, which stops being
  -- fetched only if no election above the tip is fetching it too.
  (staleFocus, elFocusAtOrAbove) =
    Map.spanAntitone
      (\elId -> electionSlot elId < immTipSlot)
      (elFocus outstanding)
  -- O(pruned), which pruning already is.
  prunedMissing =
    F.foldl'
      (\n h -> n + maybe 0 wantsBody (Map.lookup h (ebState outstanding)))
      0
      prunedHashes
  ebFocusingElections' =
    Map.foldlWithKey'
      (\m elId ebHash -> unfocusing elId ebHash m)
      (ebFocusingElections outstanding)
      staleFocus

{-------------------------------------------------------------------------------
  Settled requests
-------------------------------------------------------------------------------}

-- | 'Nothing' if the predicate holds.
delIf :: (a -> Bool) -> a -> Maybe a
delIf predicate x = if predicate x then Nothing else Just x

-- | Cancel all of a peer's outstanding fetch requests in bulk, e.g. when it
-- disconnects: refund its share of the request budget, drop it from the per-EB
-- body request set ('requestedEbPeers'), and -- via the per-peer
-- 'requestedJobsPerPeer' index -- decrement the multiplicity of every job it
-- had in flight, so those bodies and jobs become re-requestable from other
-- peers.
--
-- TODO eliminate the linear scans
removePeerFromOutstanding ::
  Ord peer =>
  peer ->
  LeiosOutstanding peer ->
  LeiosOutstanding peer
removePeerFromOutstanding peer o =
  o
    { requestedBytesSizePerPeer = Map.delete peer (requestedBytesSizePerPeer o)
    , requestedEbPeers =
        Map.mapMaybe (delIf Set.null . Set.delete peer) (requestedEbPeers o)
    , requestedJobsPerPeer = Map.delete peer (requestedJobsPerPeer o)
    , ebState =
        Map.foldrWithKey
          (\ebHash jobIds -> Map.adjust (releaseJobs jobIds) ebHash)
          (ebState o)
          (Map.findWithDefault Map.empty peer (requestedJobsPerPeer o))
    }
 where
  -- Decrement, in that EB's jobPool, the multiplicity of each job this peer held.
  releaseJobs jobIds (MkEbState slot onset fetchState) =
    MkEbState slot onset $ case fetchState of
      NoBody -> NoBody
      BodyAcquired source jobPool -> BodyAcquired source $! release jobPool
   where
    release jobPool =
      NEIntSet.foldl' (flip $ Jobs.unpickJob . Jobs.MkLeiosJobId) jobPool jobIds

-- | Reverse this peer's per-request accounting for a received EB body, but only
-- if the peer is still tracked.
--
-- If the peer has already been cancelled in bulk (e.g. it disconnected and its
-- requests were refunded en masse via its 'requestedBytesSizePerPeer' total),
-- that entry is gone; re-applying the per-request refund here would
-- double-subtract 'requestedBytesSizePerPeer' and underflow. So we gate on the peer
-- still being present. The membership check, the refund, and the bulk
-- cancellation all run within the same critical section, so whichever happens
-- first claims the refund and the other no-ops.
refundEbRequest ::
  Ord peer =>
  peer ->
  EndorserBlockHash ->
  BytesSize ->
  LeiosOutstanding peer ->
  LeiosOutstanding peer
refundEbRequest peer ebHash ebBytesSize o
  | Map.member peer (requestedBytesSizePerPeer o) =
      o
        { requestedBytesSizePerPeer =
            Map.update (\x -> delIf (== 0) (x - ebBytesSize)) peer (requestedBytesSizePerPeer o)
        , requestedEbPeers =
            Map.update (delIf Set.null . Set.delete peer) ebHash (requestedEbPeers o)
        }
  | otherwise = o

-- | Like 'refundEbRequest', but for a received batch of EB txs: refunds the
-- bytes, gated on the peer still being tracked (see 'refundEbRequest'). The job
-- bookkeeping (jobPool + this peer's in-flight set) is handled by
-- 'completeTxRequest'.
refundTxRequest ::
  Ord peer =>
  peer ->
  BytesSize ->
  LeiosOutstanding peer ->
  LeiosOutstanding peer
refundTxRequest peer txsBytesSize o
  | Map.member peer (requestedBytesSizePerPeer o) =
      o
        { requestedBytesSizePerPeer =
            Map.update (\x -> delIf (== 0) (x - txsBytesSize)) peer (requestedBytesSizePerPeer o)
        }
  | otherwise = o

-- | On a received tx batch, remove its now-fetched jobs: delete them from the
-- EB's jobPool (they are done for /every/ peer) and from this peer's in-flight set,
-- so neither this peer nor any other is asked for them again. Complements
-- 'refundTxRequest', which handles only the per-peer byte accounting.
completeTxRequest ::
  Ord peer =>
  peer ->
  LeiosBlockTxsRequest ->
  LeiosOutstanding peer ->
  LeiosOutstanding peer
completeTxRequest peer (MkLeiosBlockTxsRequest point jobs) o =
  o
    { ebState = Map.adjust completeInJobPool ebHash (ebState o)
    , requestedJobsPerPeer =
        Map.update (nonEmptyMap . Map.update dropJobs ebHash) peer (requestedJobsPerPeer o)
    }
 where
  ebHash = pointEbHash point
  jobIds = NEIntMap.keysSet jobs
  completeInJobPool (MkEbState slot onset fetchState) =
    MkEbState slot onset $ case fetchState of
      NoBody -> NoBody
      BodyAcquired source jobPool -> BodyAcquired source $! complete jobPool
   where
    complete jobPool =
      NEIntSet.foldl' (flip $ Jobs.completeJob . Jobs.MkLeiosJobId) jobPool jobIds
  dropJobs held =
    NEIntSet.nonEmptySet (IntSet.difference (NEIntSet.toSet held) (NEIntSet.toSet jobIds))
  nonEmptyMap m = if Map.null m then Nothing else Just m

{-------------------------------------------------------------------------------
  Deciding
-------------------------------------------------------------------------------}

-- TODO which of these limits are allowed to be exceeded by at most one
-- request?
data LeiosFetchStaticEnv = MkLeiosFetchStaticEnv
  { maxRequestedBytesSizePerPeer :: BytesSize
  -- ^ At most this many outstanding bytes requested from each non-big-ledger
  -- peer
  , maxRequestedBytesSizePerBigLedgerPeer :: BytesSize
  -- ^ At most this many outstanding bytes requested from each big-ledger peer.
  --
  -- At most the LeiosFetch ingress queue's size: the scheduler must never
  -- leave more requested-but-unconsumed response bytes outstanding than that
  -- queue can hold, else the mux tears the connection down (see
  -- @Network.Mux.Ingress@'s @IngressQueueOverRun@).
  --
  -- Larger than 'maxRequestedBytesSizePerPeer' so a high-stake peer can be asked
  -- for multiple whole EB closures at once, but still bounded so an adversarial
  -- peer can't drown us.
  , maxRequestBytesSize :: BytesSize
  -- ^ At most this many outstanding bytes per request
  , maxJobBytesSize :: BytesSize
  -- ^ At most this many bytes of txs per job
  , maxJobTxCount :: Int
  -- ^ At most this many txs per job
  , fetchPriorityWindowSlots :: Word64
  -- ^ @L = 3*L_hdr + L_vote + L_diff@ (in slots): the window, ending at the
  -- current slot, of EBs still worth voting on. Fetch prioritisation inverts to
  -- oldest-first within it (see 'fetchPriorityTiers'). ~14s on mainnet, ~10 on
  -- the testnet.
  --
  -- TODO these are Leios protocol parameters (@L_hdr@/@L_vote@/@L_diff@) that
  -- should be read from the ledger state and can change via on-chain governance;
  -- static stub for now.
  }

-- | The prototype's settings, given 'maxRequestedBytesSizePerBigLedgerPeer'.
defaultLeiosFetchStaticEnv :: BytesSize -> LeiosFetchStaticEnv
defaultLeiosFetchStaticEnv maxPerBigLedgerPeer =
  MkLeiosFetchStaticEnv
    { maxRequestedBytesSizePerPeer = 5 * million
    , maxRequestedBytesSizePerBigLedgerPeer = maxPerBigLedgerPeer
    , maxRequestBytesSize = maxLeiosTxsRequestBytesSize
    , maxJobBytesSize = 64 * thousandBase2
    , maxJobTxCount = 20000 -- TODO do we want this to be low enough to matter?
    , fetchPriorityWindowSlots = 10 -- TODO read dynamically from ledger state
    }
 where
  million :: Num a => a
  million = 10 ^ (6 :: Int)
  thousandBase2 :: Num a => a
  thousandBase2 = 2 ^ (10 :: Int)

-- | Decide what to request from each peer right now
--
-- A big-ledger peer is fetched from aggressively: it has a larger per-peer
-- byte budget, enough that a closure it offers is requested in full (the whole
-- remaining job pool) at once.
--
-- NOTE that this does not read txs from the LeiosTxCache nor from the Mempool;
-- that happened when the EB body arrived. (TODO the LeiosFetch client could
-- also check the LeiosTxCache and the Mempool just before it sends the
-- request? The major cost is that doing so requires retaining the individual
-- txs' hashes in memory and/or fetching them from disk, which adds complexity
-- and\/or latency. At least with the /current/ SQLite-based LeiosDb backend,
-- that complexity and\/or latency is not the responsibility of the LeiosFetch
-- logic.)
leiosFetchLogicIteration ::
  Ord peer =>
  LeiosFetchStaticEnv ->
  -- | The ledger's bounds on an EB, forecast from the immutable tip to a slot.
  --
  -- Only an offer that brought no bound of its own needs this, and for those it
  -- cannot fail: the announcement backing such an offer was accepted by
  -- forecasting from the immutable tip to that same slot. See
  -- 'poEbBounds'.
  (SlotNo -> Either OutsideForecastRange LeiosEbBounds) ->
  -- | The current slot, or 'Nothing' when it is not yet known (i.e. we are
  -- syncing), in which case we fetch freshest-last instead of freshest-first.
  Maybe SlotNo ->
  Map peer (Map LeiosPoint PeerOffer) ->
  -- | Which peers are big-ledger peers (a peer absent from this map is treated as
  -- 'IsNotBigLedgerPeer').
  Map peer IsBigLedgerPeer ->
  LeiosOutstanding peer ->
  -- | The new outstanding state, the requests to send, and the offers to prune
  ( LeiosOutstanding peer
  , Map peer (NESeq LeiosFetchRequest)
  , Map peer (NESet LeiosPoint)
  )
leiosFetchLogicIteration env forecastEbBounds mbCurrentSlot offerings bigLedgerPeers = \acc0 ->
  -- One pass per peer. Bodies and tx-closure jobs compete on equal footing,
  -- ranked by the slots of the elections fetching each EB (see
  -- 'fetchPriorityTiers'), whether it is the body or the closure they still
  -- need.
  -- Each peer's 'assignPeer' yields only its own requests and dead offers; fold
  -- those into the per-peer maps here.
  Map.foldlWithKey'
    ( \(acc, reqs, drops) peer offers ->
        let isBig = Map.findWithDefault IsNotBigLedgerPeer peer bigLedgerPeers
            (acc', peerReqs, peerDrops) =
              assignPeer env forecastEbBounds mbCurrentSlot isBig peer offers acc
         in ( acc'
            , case NESeq.nonEmptySeq peerReqs of
                Nothing -> reqs
                Just neReqs -> Map.insert peer neReqs reqs
            , case NESet.nonEmptySet peerDrops of
                Nothing -> drops
                Just nes -> Map.insert peer nes drops
            )
    )
    (acc0, Map.empty, Map.empty)
    offerings

-- | A peer's remaining outstanding-byte budget. The only "global limit" falls
-- out as this per-peer cap multiplied by the peer count. That's good so that
-- an adversarial peer can't occupy "too much" of some fixed global budget,
-- thereby starving honest peers.
--
-- Big-ledger peers get a larger cap (so they can be asked for a whole EB closure
-- at once), but still a bounded one -- even a stake-based peer might be adversarial.
peerBudget ::
  Ord peer => LeiosFetchStaticEnv -> IsBigLedgerPeer -> LeiosOutstanding peer -> peer -> Int
peerBudget env isBig acc peer =
  fromIntegral cap
    - fromIntegral (Map.findWithDefault 0 peer (requestedBytesSizePerPeer acc))
 where
  cap = case isBig of
    IsBigLedgerPeer -> maxRequestedBytesSizePerBigLedgerPeer env
    IsNotBigLedgerPeer -> maxRequestedBytesSizePerPeer env

-- | Prioritize a peer's Leios offers, each given with the elections fetching
-- its EB
--
-- The offers are categorized into two tiers. The high-priority tier is the EBs
-- still worth voting on: those that an election no older than L = 3*L_hdr +
-- L_vote + L_diff is fetching (ie whose slot is @>= currentSlot - L@). It
-- prioritizes /staler/ EBs, by the oldest such election. The low-priority tier
-- is the rest, and it prioritizes /fresher/ EBs, by the youngest election.
--
-- 'assignPeer' processes the high-priority tier first and then it carries over
-- the resulting accumulator in order to process the low-priority tier.
--
-- When the node's ledger state is too old to know what the current slot is, all
-- EBs are categorized as the high-priority tier, by the oldest election, and
-- the low-priority tier is empty.
fetchPriorityTiers :: Maybe SlotNo -> Word64 -> [(NESet ElectionId, v)] -> ([v], [v])
fetchPriorityTiers mbCurrentSlot l offers =
  ( map snd $ sortOn fst [(slot, v) | (HighTier slot, v) <- tiered]
  , map snd $ sortOn (Down . fst) [(slot, v) | (LowTier slot, v) <- tiered]
  )
 where
  tiered = [(tierOf (NESet.toSet elIds), v) | (elIds, v) <- offers]

  -- 'ElectionId' orders slot-first, so each lookup is logarithmic.
  tierOf elIds = case mbCurrentSlot of
    Nothing -> HighTier $ electionSlot $ Set.findMin elIds
    Just (SlotNo s) ->
      -- @a + l < s@ (i.e. @a < S - L@) is stale, guarding underflow when
      -- @S < L@.
      let stale elId = case electionSlot elId of SlotNo a -> a + l < s
       in case Set.lookupMin (Set.dropWhileAntitone stale elIds) of
            Just elId -> HighTier $ electionSlot elId
            Nothing -> LowTier $ electionSlot $ Set.findMax elIds

-- | Which of 'fetchPriorityTiers' an offer falls in, and the slot it is ranked
-- by there
data FetchPriorityTier = HighTier SlotNo | LowTier SlotNo

-- | Walk this peer's offered points in priority order (see
-- 'fetchPriorityTiers'), assigning requests to the peer until it's saturated at
-- 'maxRequestedBytesSizePerPeer'. A big-ledger peer saturates instead at
-- the larger 'maxRequestedBytesSizePerBigLedgerPeer', enough that a
-- couple closures it offers can be entirely inflight at the same time (see
-- 'assignClosure').
--
-- Offers beyond the saturation point are never visited, so aren't pruned this
-- pass; that's fine because it's ephemeral and/or the other prune based on the
-- imm-tip advancing is a backstop.
assignPeer ::
  Ord peer =>
  LeiosFetchStaticEnv ->
  (SlotNo -> Either OutsideForecastRange LeiosEbBounds) ->
  Maybe SlotNo ->
  IsBigLedgerPeer ->
  peer ->
  Map LeiosPoint PeerOffer ->
  LeiosOutstanding peer ->
  (LeiosOutstanding peer, Seq LeiosFetchRequest, Set LeiosPoint)
assignPeer env forecastEbBounds mbCurrentSlot isBig peer offers acc =
  -- Walk the high-priority tier, then the low, threading the accumulator; the
  -- second walk immediately short-circuits if the first already saturated the
  -- peer.
  go (go (acc, Seq.empty, Set.empty) highTier) lowTier
 where
  (highTier, lowTier) =
    fetchPriorityTiers
      mbCurrentSlot
      (fetchPriorityWindowSlots env)
      -- Only what some election is currently fetching. An offer of anything
      -- else is skipped rather than dropped, so it comes back into play if some
      -- election later fetches that endorser block --- because a certificate
      -- moved an election onto it, or because another election announced it.
      [ (elIds, (point, offer))
      | (point, offer) <- Map.toList offers
      , Just elIds <- [Map.lookup point.pointEbHash (ebFocusingElections acc)]
      ]

  go st@(acc', _dec, _drops) = \case
    [] -> st
    (point, offerKind) : rest
      | peerBudget env isBig acc' peer <= 0 -> st
      | otherwise -> go (classify point offerKind st) rest

  classify point offerKind (acc1, dec1, drops) =
    case Map.lookup ebHash (ebState acc1) of
      Nothing ->
        -- We are no longer tracking this EB (pruned off below the
        -- imm-tip). This is an ephemeral state, mid prune, but go ahead and
        -- prune it now.
        pruneThisOffer
      Just (MkEbState slot _onset fetchState) -> case (fetchState, poClosure offerKind) of
        (NoBody, TxsClosureNotOffered) ->
          -- Body-only offer: request the body. If that's all that was
          -- offered, prune it.
          let (acc2, dec2) = assignBody forecastEbBounds peer ebHash slot offerKind (acc1, dec1)
           in (acc2, dec2, Set.insert point drops)
        (NoBody, TxsClosureOffered) ->
          -- Request the body now, but keep the offer: we will request the
          -- closure from this peer once we hold the body.
          let (acc2, dec2) = assignBody forecastEbBounds peer ebHash slot offerKind (acc1, dec1)
           in (acc2, dec2, drops)
        (BodyAcquired _source _jobPool, TxsClosureNotOffered) ->
          -- We hold the body and the peer never offered the closure, so it can
          -- no longer help.
          pruneThisOffer
        (BodyAcquired _source jobPool, TxsClosureOffered)
          | Jobs.nullLeiosJobPool jobPool ->
              -- Nothing left to fetch --- the whole datum is in hand, or we
              -- rejected the endorser block on arrival --- so the closure offer
              -- is useless now too.
              pruneThisOffer
          | otherwise ->
              -- Still need the txs, and the peer offered the closure. If we just
              -- now assign all remaining jobs to the peer, prune its offer.
              let ((acc2, dec2), exhausted) = assignClosure env isBig peer ebHash (acc1, dec1)
               in ( acc2
                  , dec2
                  , case exhausted of
                      PeerEbNotExhausted -> drops
                      PeerEbExhausted -> Set.insert point drops
                  )
   where
    ebHash = pointEbHash point

    pruneThisOffer = (acc1, dec1, Set.insert point drops)

-- | Request the EB body from this peer, at the size the peer offered it at.
--
-- The offered size is the only size with any say here; an announcement's has
-- none, since at most one announcement naming a hash is honest and nothing
-- tells us which. So we ask whoever says they have it, for as many bytes as
-- they say. The offer was bounded by the ledger's maximum endorser block size
-- when it arrived, and what the bytes really are is settled on arrival by
-- hashing them: a peer that misrepresented either the block or its length
-- loses the connection then.
assignBody ::
  Ord peer =>
  -- | The ledger's bounds on an EB, forecast for a slot. See 'poEbBounds'.
  (SlotNo -> Either OutsideForecastRange LeiosEbBounds) ->
  peer ->
  EndorserBlockHash ->
  SlotNo ->
  PeerOffer ->
  (LeiosOutstanding peer, Seq LeiosFetchRequest) ->
  (LeiosOutstanding peer, Seq LeiosFetchRequest)
assignBody forecastEbBounds peer ebHash slot offer st@(acc, dec)
  | peer `Set.member` Map.findWithDefault Set.empty ebHash (requestedEbPeers acc) =
      -- unless we've already requested it from them
      st
  | otherwise = case (eEbBounds, poOfferedBody offer) of
      -- Beyond our horizon and the offer brought no bound of its own, so we
      -- have nothing to hold the body to. Ignoring it this iteration costs
      -- nothing: a later one forecasts again, from a tip that has moved.
      (Left _outsideRange, _) -> st
      -- the peer offered the closure but not the body
      (Right _, SNothing) -> st
      (Right bounds, SJust size) ->
        let acc' =
              acc
                { requestedEbPeers =
                    Map.insertWith Set.union ebHash (Set.singleton peer) (requestedEbPeers acc)
                , requestedBytesSizePerPeer =
                    Map.insertWith (+) peer size (requestedBytesSizePerPeer acc)
                }
         in ( acc'
            , dec
                Seq.|> LeiosBlockRequest
                  MkLeiosBlockRequest
                    { lbrPoint = MkLeiosPoint slot ebHash
                    , lbrOfferedSize = size
                    , lbrEbBounds = bounds
                    }
            )
 where
  -- What the offer brought, else what we can forecast for its slot.
  eEbBounds = case poEbBounds offer of
    SJust x -> Right x
    SNothing -> forecastEbBounds slot

-- | Whether all jobs matching a peer's offers are already inflight
data WhetherPeerEbExhausted = PeerEbExhausted | PeerEbNotExhausted

-- | Request tx-closure jobs from this peer, the least-requested ones we haven't
-- already requested from it. Keep adding jobs until the peer is saturated or
-- there are no jobs left. Also returns whether there are no jobs left.
assignClosure ::
  Ord peer =>
  LeiosFetchStaticEnv ->
  IsBigLedgerPeer ->
  peer ->
  EndorserBlockHash ->
  (LeiosOutstanding peer, Seq LeiosFetchRequest) ->
  ((LeiosOutstanding peer, Seq LeiosFetchRequest), WhetherPeerEbExhausted)
assignClosure env isBig peer ebHash st@(acc, dec) =
  case Map.lookup ebHash (ebState acc) of
    Nothing -> (st, PeerEbNotExhausted)
    Just (MkEbState _slot _onset NoBody) -> (st, PeerEbNotExhausted)
    Just (MkEbState slot onset (BodyAcquired source jobPool)) ->
      let inflightJobs =
            maybe IntSet.empty NEIntSet.toSet $
              Map.lookup ebHash =<< Map.lookup peer (requestedJobsPerPeer acc)
          -- A big-ledger peer gets a larger budget ('peerBudget'), enough for
          -- multiple full EB closures at once, but still bounded.
          --
          -- There are no more than 184 jobs per EB, so picked can't be a /long/ list.
          --
          -- 'pickJobs' draws from the decision loop's own PRNG ('leiosFetchPrng');
          -- its advanced state is written back below (unchanged when nothing is
          -- picked, so the 'Nothing' branch's 'st' is correct as-is).
          (picked, jobPool', prng', exhausted) =
            pickJobs (leiosFetchPrng acc) inflightJobs jobPool (peerBudget env isBig acc peer)
       in flip (,) exhausted $ case nonEmpty picked of
            Nothing -> st
            Just nePicked ->
              let acc' =
                    acc
                      { ebState =
                          Map.insert
                            ebHash
                            (MkEbState slot onset (BodyAcquired source jobPool'))
                            (ebState acc)
                      , requestedJobsPerPeer =
                          Map.insertWith
                            (Map.unionWith NEIntSet.union)
                            peer
                            ( Map.singleton ebHash $
                                NEIntSet.fromList $
                                  fmap (\(Jobs.MkLeiosJobId i, _) -> i) nePicked
                            )
                            (requestedJobsPerPeer acc)
                      , requestedBytesSizePerPeer =
                          Map.insertWith
                            (+)
                            peer
                            (sum $ fmap (\(_, Jobs.MkLeiosJob _ bytes _) -> bytes) nePicked)
                            (requestedBytesSizePerPeer acc)
                      , leiosFetchPrng = prng'
                      }
                  reqs = batchTxsRequests env (MkLeiosPoint slot ebHash) nePicked
               in (acc', dec <> Seq.fromList reqs)

-- | Take least-requested-available jobs until the budget is spent or
-- there are no more jobs that aren't already assigned to this peer. Also
-- returns 'PeerEbExhausted', in the latter case. Each pick carries the whole
-- 'Jobs.LeiosJob' (id + commitment) so the request can validate its own
-- response. The supplied PRNG shuffles which job is drawn within the
-- least-requested bucket; its advanced state is returned so the caller can
-- persist it.
pickJobs ::
  StdGen ->
  IntSet.IntSet ->
  Jobs.LeiosJobPool ->
  Int ->
  ([(Jobs.LeiosJobId, Jobs.LeiosJob)], Jobs.LeiosJobPool, StdGen, WhetherPeerEbExhausted)
pickJobs prng0 inflightJobs0 jobPool0 budget0 =
  go prng0 inflightJobs0 jobPool0 budget0 []
 where
  go prng inflightJobs jobPool budget acc
    | budget <= 0 = (reverse acc, jobPool, prng, PeerEbNotExhausted)
    | otherwise = case Jobs.pickLeastRequestedJobExcept prng inflightJobs jobPool of
        Nothing -> (reverse acc, jobPool, prng, PeerEbExhausted)
        Just (jid@(Jobs.MkLeiosJobId i), job@(Jobs.MkLeiosJob _offsets bytes _root), jobPool', prng') ->
          go
            prng'
            (IntSet.insert i inflightJobs)
            jobPool'
            (budget - fromIntegral bytes)
            ((jid, job) : acc)

-- | Partition the picked jobs into requests, each within 'maxRequestBytesSize'
-- (a lone job above the cap simply forms its own request). Each request carries
-- the jobs it covers with their commitments; the wire bitmap is derived from the
-- union of their offsets at send time. Order within a request is irrelevant
-- (union offsets, set of ids, independent per-job validation).
batchTxsRequests ::
  LeiosFetchStaticEnv ->
  LeiosPoint ->
  NonEmpty (Jobs.LeiosJobId, Jobs.LeiosJob) ->
  [LeiosFetchRequest]
batchTxsRequests env point (j0 :| rest0) =
  go j0 [] (jobBytes j0) rest0
 where
  cap = fromIntegral (maxRequestBytesSize env) :: Int
  jobBytes (_jid, Jobs.MkLeiosJob _offs bytes _root) = fromIntegral bytes :: Int
  -- 'accRev' are the batch's jobs after its seed; a batch is always non-empty.
  -- 'NEIntMap.fromList' keys by the raw job id; the picks are distinct ids, so no
  -- merge.
  flush seed accRev =
    LeiosBlockTxsRequest $
      MkLeiosBlockTxsRequest
        point
        (NEIntMap.fromList (fmap (\(Jobs.MkLeiosJobId i, job) -> (i, job)) (seed :| reverse accRev)))
  go seed accRev _curBytes [] = [flush seed accRev]
  go seed accRev curBytes (j : rest)
    | curBytes + jobBytes j > cap = flush seed accRev : go j [] (jobBytes j) rest
    | otherwise = go seed (j : accRev) (curBytes + jobBytes j) rest
