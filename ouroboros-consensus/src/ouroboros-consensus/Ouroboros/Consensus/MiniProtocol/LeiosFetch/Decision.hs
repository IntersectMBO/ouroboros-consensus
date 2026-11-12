{-# LANGUAGE LambdaCase #-}

-- | What the node's LeiosFetch clients request, decided for all of them at
-- once: the node-wide state of what is outstanding, and the decision
-- that assigns the peers' offers to requests, sharing STM admission facts.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Decision
  ( -- * Requests
    LeiosFetchRequest (..)
  , LeiosBlockRequest (..)
  , LeiosBlockTxsRequest (..)
  , LeiosEbByteSizeBounds (..)

    -- * Offers
  , OfferedHalves (..)
  , PeerOffer (..)

    -- * Outstanding state
  , LeiosOutstanding (..)
  , EbState (..)
  , EbFetchState (..)
  , ebStateOnset
  , emptyLeiosOutstanding
  , initializeLeiosOutstanding
  , newLeiosJobPool
  , acquireEbBody
  , AlreadyEmitted (..)
  , markReceivingFromForge
  , settleReceivingFromForge
  , recordReferencedPoint
  , focusElectionIfUnfocused
  , focusCertifiedEb
  , trackCertifiedEb
  , focusElection
  , isFocusedPoint
  , pruneOutstandingToImmTip

    -- * Settled requests
  , removePeerFromOutstanding
  , settleTxRequest
  , abandonClosure

    -- * Deciding
  , LeiosFetchStaticEnv (..)
  , defaultLeiosFetchStaticEnv
  , assignRequests
  , ClosureOfferTimes
  , orderedClosureSources
  , workStillNeeded
  , fetchPriorityTiers
  ) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (foldM)
import qualified Data.Foldable as F
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.IntSet.NonEmpty (NEIntSet)
import qualified Data.IntSet.NonEmpty as NEIntSet
import Data.List (partition, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Ord (Down (..))
import Data.Word (Word64)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime)
import Ouroboros.Consensus.Forecast (OutsideForecastRange)
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , ElectionId (..)
  , EndorserBlockHash
  , LeiosEbByteSizeBounds (..)
  , LeiosPoint (..)
  , TxHash
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Config
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Jobs as Jobs
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.PeerPenalty as P
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Request
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState as L
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Runtime as R
import Ouroboros.Consensus.Util.IOLike (IOLike, Time, atomically, readTVar)
import System.Random (StdGen)

{-------------------------------------------------------------------------------
  Requests
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Offers
-------------------------------------------------------------------------------}

-- | Which halves of an endorser block one peer has offered at one point. A peer
-- that offered neither has no offer there.
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
--
-- The size the peer offered the body at is its own claim, and the only size
-- that decides anything: no announcement gets a say (see 'assignBody'), and
-- what the endorser block really weighs is settled by hashing the bytes that
-- arrive.
--
-- Offering one point twice costs the peer its connection, but one hash at two
-- /points/ does not: those are separate entries, each needing its own
-- announcement, and nothing here arbitrates between the sizes they name.
--
-- TODO disconnect a peer that ever offers one 'EndorserBlockHash' at two
-- sizes, whichever messages the two arrived on. That is proof it is lying, and
-- today we keep both and carry on.
data OfferedHalves
  = -- | The body, at this size
    OfferedBody !BytesSize
  | -- | The closure
    OfferedClosure
  | -- | Both, the body at this size
    OfferedBodyAndClosure !BytesSize
  deriving (Eq, Show)

-- | What either offered. Of two body sizes, the leftmost wins, which for
-- 'Map.insertWith' is the newer offer's, and a peer that makes that choice
-- matter has already lost our trust.
instance Semigroup OfferedHalves where
  OfferedBody size <> OfferedBody _ = OfferedBody size
  OfferedBody size <> OfferedClosure = OfferedBodyAndClosure size
  OfferedBody size <> OfferedBodyAndClosure _ = OfferedBodyAndClosure size
  OfferedClosure <> OfferedBody size = OfferedBodyAndClosure size
  OfferedClosure <> OfferedClosure = OfferedClosure
  OfferedClosure <> OfferedBodyAndClosure size = OfferedBodyAndClosure size
  OfferedBodyAndClosure size <> OfferedBody _ = OfferedBodyAndClosure size
  OfferedBodyAndClosure size <> OfferedClosure = OfferedBodyAndClosure size
  OfferedBodyAndClosure size <> OfferedBodyAndClosure _ = OfferedBodyAndClosure size

-- | What one peer has offered for one endorser block point.
data PeerOffer = MkPeerOffer
  { poEbBounds :: !(StrictMaybe LeiosEbByteSizeBounds)
  -- ^ The ledger's bounds on this offered EB. It's 'SNothing' if
  -- the offer arrived via LeiosNotify, and 'SJust' if it arrived via ChainSync.
  --
  -- The decision logic uses forecasting in the 'SNothing' case. We avoid
  -- forecasting in the ChainSync case so that the Leios Recovery Path can
  -- handle sparse chains.
  , poOffered :: !OfferedHalves
  }
  deriving (Eq, Show)

-- | Each field on its own terms (see 'OfferedHalves'). The bound is not a claim
-- of the peer's but a value we computed, so the two sides agree whenever both
-- have one and either will do.
instance Semigroup PeerOffer where
  MkPeerOffer b1 o1 <> MkPeerOffer b2 o2 =
    MkPeerOffer (pickBounds b1 b2) (o1 <> o2)
   where
    pickBounds SNothing y = y
    pickBounds x@SJust{} _ = x

{-------------------------------------------------------------------------------
  Outstanding state
-------------------------------------------------------------------------------}

-- | Main data structure used in the Leios fetching logic.
--
-- Tracks both EB-level state (what EBs we have/need) and TX-level state
-- (what TXs we need for each EB), along with request tracking for bandwidth
-- management.
data LeiosOutstanding peer = MkLeiosOutstanding
  { ebState :: !(Map LeiosPoint EbState)
  -- ^ Per-point state for every endorser block point this node is tracking.
  -- Three things add an entry: the announcement that takes an election's
  -- focus, a verified certificate ('trackCertifiedEb'), and the start-up
  -- seeding ('initializeLeiosOutstanding') of the endorser blocks whose
  -- closures the LeiosDb already holds (merely to avoid refetching them). An
  -- offer never does, and is only ever acted on for a point already here.
  --
  -- Each point is fetched on its own, even when another point names the same
  -- endorser block, so the ledger's bounds an entry is held to never change:
  -- they are those of its slot.
  , elFocus :: !(Map ElectionId EndorserBlockHash)
  -- ^ The endorser block each election is fetching.
  --
  -- Two activities decide it. Processing an announcement focuses an election
  -- that has no focus yet, and nothing more ('focusElectionIfUnfocused').
  -- Processing a certificate overrides whatever is there ('focusCertifiedEb')
  -- --- the CertRB itself, when its certificate is verified, not its header,
  -- which only records an offer.
  -- Pruning is the only other writer, and it just drops the elections below
  -- the immutable tip.
  --
  -- A point is focused when an election in its slot is fetching its endorser
  -- block ('isFocusedPoint'). Unless a point is focused, the decision logic
  -- asks no peer for anything about it: neither its body nor any of its
  -- closure's jobs, since this is what filters a peer's offers before either is
  -- considered. A body we already hold stays held and its job pool stays as it
  -- is, so an election that focuses it later resumes rather than restarts.
  , outstandingPrunedSlot :: !SlotNo
  -- ^ The slot this whole 'LeiosOutstanding' has most recently been pruned up
  -- to: 'pruneOutstandingToImmTip' moves it in the same record it prunes
  -- 'ebState' and 'elFocus' in, so it says how far both of them have got.
  --
  -- Used to robustly prevent re-inserting what has already been pruned out.
  , requestedJobsPerPeer :: !(Map peer (Map LeiosPoint NEIntSet))
  -- ^ Per peer, per point, the job ids it currently has in flight -- for
  -- decrementing those multiplicities on disconnect
  , leiosFetchPrng :: !StdGen
  -- ^ The LeiosFetch decision loop's own PRNG, threaded through each iteration.
  -- Used to shuffle which job is drawn when assigning tx-closure work to a peer
  -- (a uniform pick within the least-requested multiplicity bucket), so job ids
  -- aren't consumed in a fixed order. Seeded once at start-up.
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
    , elFocus = Map.empty
    , outstandingPrunedSlot = prunedSlot
    , requestedJobsPerPeer = Map.empty
    , leiosFetchPrng = prng
    }

-- | Per-point state tracked in 'ebState'
data EbState
  = MkEbState
      -- | The wall-clock onset of the point's slot, if known. The acquisition
      -- traces subtract it from the arrival time, so they say how long after
      -- this point was announced we got it. It is 'SNothing' when nothing
      -- supplied an onset, as for a verified certificate or the start-up
      -- seeding.
      !(StrictMaybe RelativeTime)
      -- | The current progress of fetching it
      !EbFetchState
  deriving (Eq, Show)

-- | Whether we hold an EB's body.
data EbFetchState
  = -- | We've only ever received an announcement.
    NoBody
  | -- | Our own forge produced this endorser block, and its body and closure
    -- are being written. Nothing is requested at this point, every offer of it
    -- is pruned, and every reply for it is discarded.
    --
    -- It overrides whatever the point was in ('markReceivingFromForge'), recording
    -- what of its acquisition the node had already emitted, and only the
    -- forge's writes settle it ('settleReceivingFromForge'), into 'BodyAcquired' with
    -- no job left, once the whole closure is durable. So there is no state in
    -- which the body is held and its txs are still to fetch.
    ReceivingFromForge !AlreadyEmitted
  | -- | The job pool: the jobs whose txs are not yet durably written.
    --
    -- A body too big for the point's bounds is held with an empty pool too. It
    -- is never written, and nothing more is fetched or emitted for it at this
    -- point. So is a body whose closure was abandoned ('abandonClosure'),
    -- though it is written and emitted.
    --
    -- The body itself is /not/ retained -- it lives in the LeiosDb, and each job
    -- carries a 'Jobs.JobCommitment' sufficient to validate its response.
    -- Retaining up to ~10k bodies (each up to ~600 kB) would cost gigabytes.
    --
    -- TODO the 'Jobs.LeiosJobPool' could be an 'MVar m LeiosJobPool' for per-EB
    -- locking, at the cost of an 'm' parameter on
    -- EbFetchState\/EbState\/LeiosOutstanding and a monadic body-acquire;
    -- deferred.
    BodyAcquired !Jobs.LeiosJobPool
  deriving (Eq, Show)

-- | What of a point's acquisition the node had emitted when its own forge
-- produced the endorser block there.
--
-- It may have emitted some already: another node can announce the exact same
-- point, and if that announcement arrived early but within the clock skew, the
-- node might process it, and fetch from that node, before its own forge wakes
-- up or finishes.
data AlreadyEmitted
  = -- | Neither 'AcquiredEb' nor 'AcquiredEbTxs'
    EmittedNothing
  | -- | 'AcquiredEb'
    EmittedBody
  | -- | 'AcquiredEb' and 'AcquiredEbTxs', or neither for a body too big for
    -- the point's bounds, which is never emitted
    EmittedBodyAndClosure
  deriving (Eq, Show)

-- | The recorded onset of the point's slot, if any (see 'MkEbState'); the
-- arrival handlers use it to report the EB's age on arrival.
ebStateOnset :: EbState -> StrictMaybe RelativeTime
ebStateOnset (MkEbState onset _fetchState) = onset

-- | A pool of jobs to fetch these missing txs of an EB's closure, given as in
-- 'Jobs.mkJobPool'.
newLeiosJobPool ::
  LeiosFetchStaticEnv ->
  IntMap.IntMap (TxHash, BytesSize) ->
  Jobs.LeiosJobPool
newLeiosJobPool env = Jobs.mkJobPool (maxJobBytesSize env) (maxJobTxCount env)

-- | The body's LeiosDb write is durable, so record 'BodyAcquired' with the pool
-- of jobs the settled write still leaves to fetch.
--
-- There is no intermediate "write in flight" state. During the (brief, async)
-- write window the entry is still 'NoBody', so a concurrent offer may
-- redundantly re-fetch the body -- cheaper than tracking an in-flight state and
-- reverting it on a lost write, which with the single writer cannot happen
-- without bringing the node down anyway.
acquireEbBody ::
  LeiosPoint ->
  Jobs.LeiosJobPool ->
  LeiosOutstanding peer ->
  LeiosOutstanding peer
acquireEbBody point jobPool =
  alterEbState point $ \case
    -- Pruned before the write landed (we could not have requested it
    -- without an announcement, and that was since evicted): ignore.
    Nothing -> Nothing
    Just st@(MkEbState onset fetchState) -> case fetchState of
      NoBody -> Just $ MkEbState onset (BodyAcquired jobPool)
      -- Only the forge's writes settle it.
      ReceivingFromForge{} -> Just st
      -- Already acquired -- e.g. a concurrent redundant delivery landed
      -- first; keep the existing pool rather than clobbering it.
      BodyAcquired{} -> Just st

-- | Record that our own forge produced the endorser block at this point, whose
-- body and closure it is about to write, so that nothing is fetched there,
-- whatever the point was in.
--
-- If another forge of this node already produced it (testnet nodes sometimes
-- run the forges of several stake pools), the point stays as that forge marked
-- it, and whichever forge's writes are durable first settle it.
markReceivingFromForge :: LeiosPoint -> LeiosOutstanding peer -> LeiosOutstanding peer
markReceivingFromForge point =
  alterEbState point $ \case
    -- A forged endorser block records no onset: its age on arrival says nothing
    -- about diffusion.
    Nothing -> Just $ MkEbState SNothing (ReceivingFromForge EmittedNothing)
    Just st@(MkEbState onset fetchState) -> case fetchState of
      NoBody -> Just $ MkEbState onset (ReceivingFromForge EmittedNothing)
      ReceivingFromForge{} -> Just st
      BodyAcquired jobPool
        -- An empty pool means the body and closure were emitted, except for a
        -- body too big for its bounds or a closure no peer can complete
        -- ('abandonClosure'). Our forge produces neither such endorser block:
        -- it fits each one to its slot's bounds, and states each tx's own size.
        | Jobs.nullJobPool jobPool -> Just $ MkEbState onset (ReceivingFromForge EmittedBodyAndClosure)
        | otherwise -> Just $ MkEbState onset (ReceivingFromForge EmittedBody)

-- | The forge's writes of the endorser block at this point are durable, so hold
-- it with no job left, and say what of its acquisition is still to emit:
-- 'Nothing' unless the point is 'ReceivingFromForge', as when another forge's writes
-- settled it first or the point was pruned.
settleReceivingFromForge ::
  LeiosPoint -> LeiosOutstanding peer -> (LeiosOutstanding peer, Maybe AlreadyEmitted)
settleReceivingFromForge point outstanding =
  case Map.lookup point (ebState outstanding) of
    Just (MkEbState onset (ReceivingFromForge emitted)) ->
      ( alterEbState
          point
          (\_ -> Just $ MkEbState onset (BodyAcquired Jobs.emptyJobPool))
          outstanding
      , Just emitted
      )
    _ -> (outstanding, Nothing)

-- | Record that this point is referenced --- announced, or named by a verified
-- certificate --- along with its slot's wall-clock onset if known. A
-- reference that carries no onset ('SNothing') never overrides one already
-- recorded.
recordReferencedPoint ::
  LeiosPoint ->
  StrictMaybe RelativeTime ->
  LeiosOutstanding peer ->
  LeiosOutstanding peer
recordReferencedPoint point onset =
  alterEbState point $ \case
    Nothing -> Just $ MkEbState onset NoBody
    Just (MkEbState SNothing fetchState) | SJust{} <- onset -> Just $ MkEbState onset fetchState
    Just st@MkEbState{} -> Just st

-- | Initialize the outstanding state
--
-- Its contents are only @'BodyAcquired' 'Jobs.emptyJobPool'@ for the EBs
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
initializeLeiosOutstanding ::
  StdGen -> [LeiosPoint] -> SlotNo -> LeiosOutstanding peer
initializeLeiosOutstanding prng points immTipSlot =
  F.foldl' (flip seed1) (emptyLeiosOutstanding prng immTipSlot) points
 where
  -- These points were read back out of the LeiosDb, so the body is durable
  -- already: settle it at once.
  seed1 point =
    acquireEbBody point Jobs.emptyJobPool
      . recordReferencedPoint point SNothing

-- | Make this endorser block the one this election is fetching, unless the
-- election is already fetching one.
--
-- The first announcement we see for an election wins, which is what an honest
-- electee's single announcement gives; only a certificate ('focusCertifiedEb')
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
-- track what the pruning dropped.
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
-- Already holding the body is not a special case: 'recordReferencedPoint'
-- leaves the fetch state alone, so nothing is re-fetched.
trackCertifiedEb :: LeiosPoint -> LeiosOutstanding peer -> LeiosOutstanding peer
trackCertifiedEb point outstanding
  | pointSlotNo point < outstandingPrunedSlot outstanding = outstanding
  | otherwise = recordReferencedPoint point SNothing outstanding

-- | Make this endorser block the one this election is fetching, whatever it was
-- fetching before.
--
-- Whatever it was fetching stops being asked for, unless another election is
-- also fetching it. Nothing else about that endorser block is disturbed: we
-- keep the body if we acquired it, and we still take whatever was already
-- requested for it, since those bytes are paid for either way.
--
-- NOTE One election, one certified endorser block --- which holds only while
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
  _ -> outstanding{elFocus = Map.insert elId ebHash (elFocus outstanding)}

-- | Whether some election is currently fetching this point: one in the point's
-- slot that focuses its endorser block. 'ElectionId' orders slot-first, so the
-- elections of a slot are a range of 'elFocus'. A slot has at most one election
-- per leader of that slot, and rarely has more than one leader, so the range is
-- short.
isFocusedPoint :: LeiosPoint -> LeiosOutstanding peer -> Bool
isFocusedPoint (MkLeiosPoint slot ebHash) =
  elem ebHash
    . Map.takeWhileAntitone ((== slot) . electionSlot)
    . Map.dropWhileAntitone ((< slot) . electionSlot)
    . elFocus

-- | 'Map.alter' a point's 'ebState' entry.
alterEbState ::
  LeiosPoint ->
  (Maybe EbState -> Maybe EbState) ->
  LeiosOutstanding peer ->
  LeiosOutstanding peer
alterEbState point f outstanding =
  outstanding{ebState = Map.alter f point (ebState outstanding)}

-- | Prune 'LeiosOutstanding' to the immutable tip.
--
-- 'LeiosPoint' and 'ElectionId' both order slot-first, so what is below the tip
-- is a prefix of each map, dropped with @dropWhileAntitone@.
--
-- TODO still more it could prune (e.g. abandoned in-flight EB requests).
pruneOutstandingToImmTip :: SlotNo -> LeiosOutstanding peer -> LeiosOutstanding peer
pruneOutstandingToImmTip immTipSlot outstanding =
  outstanding
    { ebState = Map.dropWhileAntitone ((< immTipSlot) . pointSlotNo) (ebState outstanding)
    , elFocus = Map.dropWhileAntitone ((< immTipSlot) . electionSlot) (elFocus outstanding)
    , outstandingPrunedSlot = max (outstandingPrunedSlot outstanding) immTipSlot
    }

{-------------------------------------------------------------------------------
  Settled requests
-------------------------------------------------------------------------------}

-- | Release this registration's logical job picks once on disconnect.
-- Physical leases and body pending ownership are independently retired by
-- the runtime; accepted receipts can still complete globally relevant jobs.
removePeerFromOutstanding ::
  Ord peer =>
  peer ->
  LeiosOutstanding peer ->
  LeiosOutstanding peer
removePeerFromOutstanding peer o =
  o
    { requestedJobsPerPeer = Map.delete peer (requestedJobsPerPeer o)
    , ebState =
        Map.foldrWithKey
          (\point jobIds -> Map.adjust (releaseJobs jobIds) point)
          (ebState o)
          (Map.findWithDefault Map.empty peer (requestedJobsPerPeer o))
    }
 where
  -- Decrement, in that EB's jobPool, the multiplicity of each job this peer held.
  releaseJobs jobIds (MkEbState onset fetchState) =
    MkEbState onset $ case fetchState of
      NoBody -> NoBody
      imminent@ReceivingFromForge{} -> imminent
      BodyAcquired jobPool -> BodyAcquired $! release jobPool
   where
    release jobPool =
      NEIntSet.foldl' (flip $ Jobs.unpickJob . Jobs.MkLeiosJobId) jobPool jobIds

-- | Settle only fully confirmed immutable jobs. Retry unconfirmed jobs only
-- while this original peer still owns their logical assignment; disconnect may
-- already have unpicked them. Presence may still complete jobs after disconnect.
settleTxRequest ::
  Ord peer =>
  peer ->
  LeiosBlockTxsRequest ->
  IntSet.IntSet ->
  LeiosOutstanding peer ->
  LeiosOutstanding peer
settleTxRequest peer (MkLeiosBlockTxsRequest point jobs) confirmed o =
  o
    { ebState = Map.adjust completeInJobPool point (ebState o)
    , requestedJobsPerPeer =
        Map.update (nonEmptyMap . Map.update dropJobs point) peer (requestedJobsPerPeer o)
    }
 where
  requested = NEIntMap.toMap jobs
  jobIds = IntMap.keysSet requested
  confirmedIds =
    IntMap.keysSet $
      IntMap.filter
        (\(Jobs.MkLeiosJob offsets _ _) -> offsets `IntSet.isSubsetOf` confirmed)
        requested
  owned =
    maybe IntSet.empty NEIntSet.toSet $
      Map.lookup peer (requestedJobsPerPeer o) >>= Map.lookup point
  retryIds = IntSet.intersection owned (IntSet.difference jobIds confirmedIds)
  completeInJobPool (MkEbState onset fetchState) =
    MkEbState onset $ case fetchState of
      NoBody -> NoBody
      imminent@ReceivingFromForge{} -> imminent
      BodyAcquired jobPool ->
        BodyAcquired $!
          IntSet.foldl'
            (\pool jid -> Jobs.unpickJob (Jobs.MkLeiosJobId jid) pool)
            (IntSet.foldl' (\pool jid -> Jobs.completeJob (Jobs.MkLeiosJobId jid) pool) jobPool confirmedIds)
            retryIds
  dropJobs held =
    NEIntSet.nonEmptySet (IntSet.difference (NEIntSet.toSet held) jobIds)
  nonEmptyMap m = if Map.null m then Nothing else Just m

-- | Fetch no more of the closure of the endorser block at this point, though
-- jobs of it remain: it references a tx with a size other than the tx's own,
-- so no peer can complete it.
abandonClosure :: LeiosPoint -> LeiosOutstanding peer -> LeiosOutstanding peer
abandonClosure point o =
  o{ebState = Map.adjust abandon point (ebState o)}
 where
  abandon (MkEbState onset fetchState) =
    MkEbState onset $ case fetchState of
      NoBody -> NoBody
      imminent@ReceivingFromForge{} -> imminent
      BodyAcquired _jobPool -> BodyAcquired Jobs.emptyJobPool

{-------------------------------------------------------------------------------
  Deciding
-------------------------------------------------------------------------------}

-- | Prioritize a peer's Leios offers, each given with its point's slot
--
-- The offers are categorized into two tiers. The high-priority tier is the
-- points still worth voting on: those no older than @L = 3*L_hdr + L_vote +
-- L_diff@, in slots, as of the ledger at the current slot (ie whose slot is
-- @>= currentSlot - L@). It prioritizes /staler/ points. The low-priority tier
-- is the rest, and it prioritizes /fresher/ points.
--
-- 'assignRequests' completes the high-priority tier before the low one.
--
-- When the node's ledger state is too old to know the current slot or @L@ there,
-- all points are categorized as the high-priority tier, and the low-priority
-- tier is empty.
--
-- TODO rank first the points a verified certificate certifies (see
-- 'focusCertifiedEb'): ChainSel cannot select the block certifying one until
-- the node holds its closure.
fetchPriorityTiers :: Maybe (SlotNo, Word64) -> [(SlotNo, v)] -> ([v], [v])
fetchPriorityTiers mbHorizon offers =
  ( map snd $ sortOn fst high
  , map snd $ sortOn (Down . fst) low
  )
 where
  (high, low) = partition (not . stale . fst) offers

  -- @a + l < s@ (i.e. @a < S - L@) is stale, guarding underflow when @S < L@.
  stale (SlotNo a) = case mbHorizon of
    Nothing -> False
    Just (SlotNo s, l) -> a < s && l < s - a

-- | Semantic work relevance, separately from its original physical reservation.
workStillNeeded :: L.Work LeiosPoint -> LeiosOutstanding peer -> Bool
workStillNeeded work out =
  isFocusedPoint point out && case (work, Map.lookup point (ebState out)) of
    (L.Body{}, Just (MkEbState _ NoBody)) -> True
    (L.Job _ jid, Just (MkEbState _ (BodyAcquired pool))) -> maybe False (const True) (Jobs.lookupJob (Jobs.MkLeiosJobId jid) pool)
    _ -> False
 where
  point = case work of L.Body p -> p; L.Job p _ -> p

-- | Admit and publish whole immutable jobs under the coordinator/removal lock.
-- STM reservation rechecks progressive eligibility; rejected speculative picks
-- never enter the shared pool. Physical credit is owned solely by the runtime.
assignRequests ::
  (IOLike m, Ord peer) =>
  R.Runtime m peer ->
  Time ->
  (SlotNo -> Either OutsideForecastRange LeiosEbByteSizeBounds) ->
  Maybe (SlotNo, Word64) ->
  ClosureOfferTimes peer ->
  Map peer (Map LeiosPoint PeerOffer) ->
  LeiosOutstanding peer ->
  m (LeiosOutstanding peer)
assignRequests rt now forecast horizon times offers out0 = do
  clients <- atomically (R.sessions <$> readTVar (R.runtimeFacts rt))
  foldM (visitPoint clients) out0 points
 where
  (urgent, other) = fetchPriorityTiers horizon [(pointSlotNo point, point) | point <- Map.keys (ebState out0)]
  points = urgent ++ other
  visitPoint clients out point
    | not (isFocusedPoint point out) = pure out
    | otherwise = case Map.lookup point (ebState out) of
        Just (MkEbState _ BodyAcquired{}) ->
          foldM
            (visitSource point)
            out
            [ (peer, session)
            | (peer, _) <- orderedClosureSources point (Map.map (R.closureScore now) clients) times offers
            , Just session <- [Map.lookup peer clients]
            ]
        _ -> foldM (visitSource point) out (Map.toAscList clients)
  visitSource point out (peer, session) = case Map.lookup peer offers >>= Map.lookup point of
    Nothing -> pure out
    Just offer -> do
      let client = R.Client rt peer (R.invocation session)
      case Map.lookup point (ebState out) of
        Just (MkEbState _ NoBody) -> case (bodySize (poOffered offer), bounds point offer) of
          (Just size, Just trusted) ->
            atomically
              (R.enqueueRequest client now (LeiosBlockRequest (MkLeiosBlockRequest point size trusted)))
              >> pure out
          _ -> pure out
        Just (MkEbState onset (BodyAcquired pool))
          | hasClosure (poOffered offer) ->
              assignJobs client point onset initialExcluded pool (leiosFetchPrng out) out
         where
          initialExcluded =
            maybe IntSet.empty NEIntSet.toSet (Map.lookup peer (requestedJobsPerPeer out) >>= Map.lookup point)
        _ -> pure out
  assignJobs client point onset excluded pool prng out =
    case Jobs.pickLeastRequestedJobExcept prng excluded pool of
      Nothing -> pure out{leiosFetchPrng = prng}
      Just (Jobs.MkLeiosJobId jid, job, picked, nextPrng) -> do
        let req = LeiosBlockTxsRequest (MkLeiosBlockTxsRequest point (NEIntMap.singleton jid job))
        admitted <- atomically (R.enqueueRequest client now req)
        let next = case admitted of
              Nothing -> out
              Just _ ->
                out
                  { ebState = Map.insert point (MkEbState onset (BodyAcquired picked)) (ebState out)
                  , requestedJobsPerPeer =
                      Map.insertWith
                        (Map.unionWith NEIntSet.union)
                        (R.clientPeer client)
                        (Map.singleton point (NEIntSet.singleton jid))
                        (requestedJobsPerPeer out)
                  }
        assignJobs
          client
          point
          onset
          (IntSet.insert jid excluded)
          (case admitted of Nothing -> pool; Just _ -> picked)
          nextPrng
          next
  bodySize (OfferedBody n) = Just n
  bodySize (OfferedBodyAndClosure n) = Just n
  bodySize OfferedClosure = Nothing
  hasClosure OfferedBody{} = False
  hasClosure _ = True
  bounds point offer = case poEbBounds offer of
    SJust trusted -> Just trusted
    SNothing -> either (const Nothing) Just (forecast (pointSlotNo point))

-- | First full-closure arrivals, independently scoped to each live registration.
-- Missing metadata remains eligible and ranks after known times at equal score.
type ClosureOfferTimes peer = Map peer (Map LeiosPoint Time)

-- | Rank eligible closure sources by aged stall evidence, then this EB's first
-- closure offer, then original registration identity. Body-only announcements
-- cannot lend their earlier timestamp to a later closure offer.
orderedClosureSources ::
  Ord peer =>
  LeiosPoint ->
  Map peer P.Score ->
  ClosureOfferTimes peer ->
  Map peer (Map LeiosPoint PeerOffer) ->
  [(peer, P.Score)]
orderedClosureSources point scores times offers =
  [ (peer, score)
  | (score, _, peer) <- sortOn (\(score, at, peer) -> (score, at == Nothing, at, peer)) candidates
  ]
 where
  candidates =
    [ (score, Map.lookup peer times >>= Map.lookup point, peer)
    | (peer, score) <- Map.toAscList scores
    , Just offer <- [Map.lookup peer offers >>= Map.lookup point]
    , case poOffered offer of OfferedBody{} -> False; _ -> True
    ]
