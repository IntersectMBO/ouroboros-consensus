{-# LANGUAGE LambdaCase #-}

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
  , OfferedHalves (..)
  , PeerOffer (..)
  , OfferPrune (..)
  , pruneOffers

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
  , refundEbRequest
  , refundTxRequest
  , completeTxRequest
  , abandonClosure

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
import Data.List (partition, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
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
import Ouroboros.Consensus.Leios.Types
  ( BytesSize
  , ElectionId (..)
  , EndorserBlockHash
  , LeiosEbBounds (..)
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
  x <> y = case (x, y) of
    (OfferedBody size, OfferedBody _) -> OfferedBody size
    (OfferedBody size, _) -> OfferedBodyAndClosure size
    (OfferedBodyAndClosure size, _) -> OfferedBodyAndClosure size
    (OfferedClosure, OfferedClosure) -> OfferedClosure
    (OfferedClosure, OfferedBody size) -> OfferedBodyAndClosure size
    (OfferedClosure, OfferedBodyAndClosure size) -> OfferedBodyAndClosure size

-- | What one peer has offered for one endorser block point.
data PeerOffer = MkPeerOffer
  { poEbBounds :: !(StrictMaybe LeiosEbBounds)
  -- ^ The ledger's bounds on this offered EB. It's 'SNothing' if
  -- the offer arrived via LeiosNotify, and 'SJust' if it arrived via ChainSync.
  --
  -- The decision logic uses forecasting in the 'SNothing' case. We avoid
  -- forecasting in the ChainSync case so that the Leios Recovery Path can
  -- handle sparse chains.
  , poOffered :: !OfferedHalves
  }
  deriving (Eq, Show)

-- | What of a peer's offer of a point the decision logic found can no longer
-- help.
--
-- The decision logic reads the peers' offers in one transaction and prunes them
-- in a later one, so meanwhile a peer's closure offer may merge into its body
-- offer. Pruning the whole offer would lose that closure offer, so
-- 'PruneOffer' is decided only where no closure offer could help either.
data OfferPrune
  = -- | The whole offer.
    PruneOffer
  | -- | Its body half, and the whole offer unless it offers the closure: the
    -- body is held, or was requested of this peer, which is asked for it at
    -- most once.
    PruneOfferedBody
  deriving (Eq, Show)

-- | Prune a peer's offers as 'leiosFetchLogicIteration' decided.
pruneOffers :: Map LeiosPoint OfferPrune -> Map LeiosPoint PeerOffer -> Map LeiosPoint PeerOffer
pruneOffers prunes offers = Map.foldlWithKey' prune offers prunes
 where
  prune acc point = \case
    PruneOffer -> Map.delete point acc
    PruneOfferedBody -> Map.update withoutBody point acc
  withoutBody offer = case poOffered offer of
    OfferedBody _size -> Nothing
    OfferedClosure -> Just offer
    OfferedBodyAndClosure _size -> Just offer{poOffered = OfferedClosure}

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
  { -- EB-level tracking
    ebState :: !(Map LeiosPoint EbState)
  -- ^ Per-point state for every endorser block point this node is tracking.
  -- Three things add an entry: the announcement that takes an election's
  -- focus, a verified certificate ('trackCertifiedEb'), and the start-up
  -- seeding ('initializeLeiosOutstanding') of the endorser blocks whose
  -- closures the LeiosDb already holds (merely to avoid refetching them). An
  -- offer never does, and is only ever
  -- acted on for a point already here.
  --
  -- Each point is fetched on its own, even when another point names the same
  -- endorser block, so the ledger's bounds an entry is held to never change:
  -- they are those of its slot.
  , elFocus :: !(Map ElectionId EndorserBlockHash)
  -- ^ The endorser block each election is fetching.
  --
  -- Two activities decide it. Processing an announcement focuses an election
  -- that has no focus yet, and nothing more ('focusElectionIfUnfocused').
  -- Processing a certificate
  -- overrides whatever is there ('focusElection') --- the CertRB itself, when
  -- its certificate is verified, not its header, which only records an offer.
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
  , numMissingBodies :: !Int
  -- ^ How many 'ebState' entries have no body: the body-fetch backlog,
  -- maintained as 'ebState' changes rather than counted, so reading it stays
  -- O(1) however often the fetch loop runs. 'wantsBody' is the per-entry
  -- contribution.
  , -- Request tracking
    requestedEbPeers :: !(Map LeiosPoint (Set peer))
  -- ^ Which peers we've requested each point's body from
  , requestedEbsPerPeer :: !(Map peer (NESet LeiosPoint))
  -- ^ The inverse of 'requestedEbPeers': the points whose body each peer has
  -- been requested, so that cancelling a peer's requests touches only those
  , requestedBytesSizePerPeer :: !(Map peer BytesSize)
  -- ^ Running total of bytes requested from each peer. This is the only
  -- outstanding-byte limit: there is no global cap (the global bound falls out
  -- as this per-peer cap times the peer count).
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
    , numMissingBodies = 0
    , requestedEbPeers = Map.empty
    , requestedEbsPerPeer = Map.empty
    , requestedBytesSizePerPeer = Map.empty
    , requestedJobsPerPeer = Map.empty
    , leiosFetchPrng = prng
    }

-- | Per-point state tracked in 'ebState'
data EbState
  = -- | The wall-clock onset of the point's slot, if known, and the current
    -- progress of fetching it.
    --
    -- The acquisition traces subtract that onset from the arrival time, so
    -- they say how long after this point was announced we got it. It is
    -- 'SNothing' when nothing supplied an onset, as for a verified certificate
    -- or the start-up seeding.
    MkEbState !(StrictMaybe RelativeTime) !EbFetchState
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
    -- point. So is a body whose closure no peer can complete
    -- ('abandonClosure'), though it is written and emitted.
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
    BodyAcquired !Jobs.LeiosJobPool
  deriving (Eq, Show)

-- | What of a point's acquisition the node had emitted when its own forge
-- produced the endorser block there.
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
-- 'Jobs.mkLeiosJobPool'.
newLeiosJobPool ::
  LeiosFetchStaticEnv ->
  IntMap.IntMap (TxHash, BytesSize) ->
  Jobs.LeiosJobPool
newLeiosJobPool env = Jobs.mkLeiosJobPool (maxJobBytesSize env) (maxJobTxCount env)

-- | The body's LeiosDb write is durable, so record 'BodyAcquired' with the pool
-- of jobs the settled write still leaves to fetch.
--
-- There is no intermediate "write in flight" state. During the (brief, async)
-- write window the entry is still 'NoBody', so a
-- concurrent offer may redundantly re-fetch the body -- cheaper than tracking an
-- in-flight state and reverting it on a lost write, which with the single writer
-- cannot happen without bringing the node down anyway. The body write is only
-- enqueued before this is reached, and the enqueue only parks on a free queue
-- slot, so a cancellation there leaves no claim to strand.
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
    Just (MkEbState onset fetchState) -> case fetchState of
      NoBody -> Just $ MkEbState onset (BodyAcquired jobPool)
      -- Only the forge's writes settle it.
      ReceivingFromForge{} -> Nothing
      -- Already acquired -- e.g. a concurrent redundant delivery landed
      -- first; keep the existing pool rather than clobbering it.
      BodyAcquired{} -> Nothing

-- | Record that our own forge produced the endorser block at this point, whose
-- body and closure it is about to write, so that nothing is fetched there,
-- whatever the point was in.
--
-- If another forge of this node already produced it, the point stays as that
-- forge marked it, and whichever forge's writes are durable first settle it.
markReceivingFromForge :: LeiosPoint -> LeiosOutstanding peer -> LeiosOutstanding peer
markReceivingFromForge point =
  alterEbState point $ \case
    -- A forged endorser block records no onset: its age on arrival says nothing
    -- about diffusion.
    Nothing -> Just $ MkEbState SNothing (ReceivingFromForge EmittedNothing)
    Just (MkEbState onset fetchState) -> case fetchState of
      NoBody -> Just $ MkEbState onset (ReceivingFromForge EmittedNothing)
      ReceivingFromForge{} -> Nothing
      BodyAcquired jobPool
        -- An empty pool means the body and closure were emitted, except for a
        -- body too big for its bounds or a closure no peer can complete
        -- ('abandonClosure'). Our forge produces neither such endorser block:
        -- it fits each one to its slot's bounds, and states each tx's own size.
        | Jobs.nullLeiosJobPool jobPool -> Just $ MkEbState onset (ReceivingFromForge EmittedBodyAndClosure)
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
          (\_ -> Just $ MkEbState onset (BodyAcquired Jobs.emptyLeiosJobPool))
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
    Just MkEbState{} -> Nothing

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
initializeLeiosOutstanding ::
  StdGen -> [LeiosPoint] -> SlotNo -> LeiosOutstanding peer
initializeLeiosOutstanding prng points immTipSlot =
  F.foldl' (flip seed1) (emptyLeiosOutstanding prng immTipSlot) points
 where
  -- These points were read back out of the LeiosDb, so the body is durable
  -- already: settle it at once.
  seed1 point =
    acquireEbBody point Jobs.emptyLeiosJobPool
      . recordReferencedPoint point SNothing

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

-- | Whether this EB's body is one we are still trying to fetch: the per-entry
-- contribution to 'numMissingBodies'.
wantsBody :: EbState -> Int
wantsBody (MkEbState _onset fetchState) = case fetchState of
  NoBody -> 1
  ReceivingFromForge{} -> 0
  BodyAcquired{} -> 0

-- | Upsert a point's 'ebState' entry, keeping 'numMissingBodies' in step. The
-- supplied function returns 'Nothing' to leave the entry as it is.
alterEbState ::
  LeiosPoint ->
  (Maybe EbState -> Maybe EbState) ->
  LeiosOutstanding peer ->
  LeiosOutstanding peer
alterEbState point f outstanding =
  case Map.alterF upsert1 point (ebState outstanding) of
    (Nothing, _) -> outstanding
    (Just deltaMissing, ebState') ->
      outstanding
        { ebState = ebState'
        , numMissingBodies = numMissingBodies outstanding + deltaMissing
        }
 where
  -- One traversal of 'ebState': the pair functor carries whether the entry
  -- changed at all and, if so, its change to 'numMissingBodies'.
  upsert1 mbOld = case f mbOld of
    Nothing -> (Nothing, mbOld)
    Just new -> (Just (wantsBody new - maybe 0 wantsBody mbOld), Just new)

-- | Prune 'LeiosOutstanding' to the immutable tip, returning the points it
-- dropped.
--
-- 'LeiosPoint' and 'ElectionId' both order slot-first, so what is below the tip
-- is a prefix of each map, dropped with @spanAntitone@.
--
-- TODO still more it could prune (e.g. abandoned in-flight EB requests).
pruneOutstandingToImmTip ::
  SlotNo -> LeiosOutstanding peer -> (Set LeiosPoint, LeiosOutstanding peer)
pruneOutstandingToImmTip immTipSlot outstanding =
  ( Map.keysSet below
  , outstanding
      { ebState = atOrAbove
      , elFocus = Map.dropWhileAntitone ((< immTipSlot) . electionSlot) (elFocus outstanding)
      , outstandingPrunedSlot = max (outstandingPrunedSlot outstanding) immTipSlot
      , numMissingBodies = numMissingBodies outstanding - F.foldl' (\n st -> n + wantsBody st) 0 below
      }
  )
 where
  (below, atOrAbove) = Map.spanAntitone ((< immTipSlot) . pointSlotNo) (ebState outstanding)

{-------------------------------------------------------------------------------
  Settled requests
-------------------------------------------------------------------------------}

-- | 'Nothing' if the predicate holds.
delIf :: (a -> Bool) -> a -> Maybe a
delIf predicate x = if predicate x then Nothing else Just x

-- | Cancel all of a peer's outstanding fetch requests in bulk, e.g. when it
-- disconnects: refund its share of the request budget, drop it from the per-EB
-- body request set ('requestedEbPeers'), and -- via the per-peer
-- 'requestedEbsPerPeer' and 'requestedJobsPerPeer' indices -- decrement the
-- multiplicity of every job it had in flight, so those bodies and jobs become
-- re-requestable from other peers.
removePeerFromOutstanding ::
  Ord peer =>
  peer ->
  LeiosOutstanding peer ->
  LeiosOutstanding peer
removePeerFromOutstanding peer o =
  o
    { requestedBytesSizePerPeer = Map.delete peer (requestedBytesSizePerPeer o)
    , requestedEbPeers =
        F.foldl'
          (flip $ Map.update (delIf Set.null . Set.delete peer))
          (requestedEbPeers o)
          (maybe Set.empty NESet.toSet (Map.lookup peer (requestedEbsPerPeer o)))
    , requestedEbsPerPeer = Map.delete peer (requestedEbsPerPeer o)
    , requestedJobsPerPeer = Map.delete peer (requestedJobsPerPeer o)
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
  LeiosPoint ->
  BytesSize ->
  LeiosOutstanding peer ->
  LeiosOutstanding peer
refundEbRequest peer point ebBytesSize o
  | Map.member peer (requestedBytesSizePerPeer o) =
      o
        { requestedBytesSizePerPeer =
            Map.update (\x -> delIf (== 0) (x - ebBytesSize)) peer (requestedBytesSizePerPeer o)
        , requestedEbPeers =
            Map.update (delIf Set.null . Set.delete peer) point (requestedEbPeers o)
        , requestedEbsPerPeer =
            Map.update (NESet.nonEmptySet . NESet.delete point) peer (requestedEbsPerPeer o)
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
    { ebState = Map.adjust completeInJobPool point (ebState o)
    , requestedJobsPerPeer =
        Map.update (nonEmptyMap . Map.update dropJobs point) peer (requestedJobsPerPeer o)
    }
 where
  jobIds = NEIntMap.keysSet jobs
  completeInJobPool (MkEbState onset fetchState) =
    MkEbState onset $ case fetchState of
      NoBody -> NoBody
      imminent@ReceivingFromForge{} -> imminent
      BodyAcquired jobPool -> BodyAcquired $! complete jobPool
   where
    complete jobPool =
      NEIntSet.foldl' (flip $ Jobs.completeJob . Jobs.MkLeiosJobId) jobPool jobIds
  dropJobs held =
    NEIntSet.nonEmptySet (IntSet.difference (NEIntSet.toSet held) (NEIntSet.toSet jobIds))
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
      BodyAcquired _jobPool -> BodyAcquired Jobs.emptyLeiosJobPool

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
  }

-- | The settings the node runs with, given 'maxRequestedBytesSizePerBigLedgerPeer'.
defaultLeiosFetchStaticEnv :: BytesSize -> LeiosFetchStaticEnv
defaultLeiosFetchStaticEnv maxPerBigLedgerPeer =
  MkLeiosFetchStaticEnv
    { maxRequestedBytesSizePerPeer = 5 * million
    , maxRequestedBytesSizePerBigLedgerPeer = maxPerBigLedgerPeer
    , maxRequestBytesSize = maxLeiosTxsRequestBytesSize
    , maxJobBytesSize = 64 * thousandBase2
    , maxJobTxCount = 20000 -- TODO do we want this to be low enough to matter?
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
-- NOTE that which txs a point still lacks was settled when its body's write
-- landed, so a job is requested even if the node has come to hold its txs
-- elsewhere since. (TODO the LeiosFetch client could check again just before
-- it sends the request? That would need the txs' hashes, retained in memory or
-- read from disk.)
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
  -- | The current slot, and how many slots before it a point is still worth
  -- voting on, as of the ledger there (see 'fetchPriorityTiers'); or 'Nothing'
  -- when either is not yet known (i.e. we are syncing), in which case we fetch
  -- freshest-last instead of freshest-first.
  Maybe (SlotNo, Word64) ->
  Map peer (Map LeiosPoint PeerOffer) ->
  -- | Which peers are big-ledger peers (a peer absent from this map is treated as
  -- 'IsNotBigLedgerPeer').
  Map peer IsBigLedgerPeer ->
  LeiosOutstanding peer ->
  -- | The new outstanding state, the requests to send, and what of the offers
  -- to prune ('pruneOffers')
  ( LeiosOutstanding peer
  , Map peer (NESeq LeiosFetchRequest)
  , Map peer (NEMap LeiosPoint OfferPrune)
  )
leiosFetchLogicIteration env forecastEbBounds mbHorizon offerings bigLedgerPeers = \acc0 ->
  -- One pass per peer. Bodies and tx-closure jobs compete on equal footing,
  -- ranked by their points' slots (see 'fetchPriorityTiers'), whether it is
  -- the body or the closure they still
  -- need.
  -- Each peer's 'assignPeer' yields only its own requests and dead offers; fold
  -- those into the per-peer maps here.
  Map.foldlWithKey'
    ( \(acc, reqs, drops) peer offers ->
        let isBig = Map.findWithDefault IsNotBigLedgerPeer peer bigLedgerPeers
            (acc', peerReqs, peerDrops) =
              assignPeer env forecastEbBounds mbHorizon isBig peer offers acc
         in ( acc'
            , case NESeq.nonEmptySeq peerReqs of
                Nothing -> reqs
                Just neReqs -> Map.insert peer neReqs reqs
            , case NEMap.nonEmptyMap peerDrops of
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

-- | Prioritize a peer's Leios offers, each given with its point's slot
--
-- The offers are categorized into two tiers. The high-priority tier is the
-- points still worth voting on: those no older than @L = 3*L_hdr + L_vote +
-- L_diff@, in slots, as of the ledger at the current slot (ie whose slot is
-- @>= currentSlot - L@). It prioritizes /staler/ points. The low-priority tier
-- is the rest, and it prioritizes /fresher/ points.
--
-- 'assignPeer' processes the high-priority tier first and then it carries over
-- the resulting accumulator in order to process the low-priority tier.
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
    Just (SlotNo s, l) -> a + l < s

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
  Maybe (SlotNo, Word64) ->
  IsBigLedgerPeer ->
  peer ->
  Map LeiosPoint PeerOffer ->
  LeiosOutstanding peer ->
  (LeiosOutstanding peer, Seq LeiosFetchRequest, Map LeiosPoint OfferPrune)
assignPeer env forecastEbBounds mbHorizon isBig peer offers acc =
  -- Walk the high-priority tier, then the low, threading the accumulator; the
  -- second walk immediately short-circuits if the first already saturated the
  -- peer.
  go (go (acc, Seq.empty, Map.empty) highTier) lowTier
 where
  (highTier, lowTier) =
    fetchPriorityTiers
      mbHorizon
      -- Only what some election is currently fetching. An offer of anything
      -- else is skipped rather than dropped, so it comes back into play if some
      -- election later fetches that point, because a certificate moved an
      -- election onto it.
      [ (pointSlotNo point, (point, offer))
      | (point, offer) <- Map.toList offers
      , isFocusedPoint point acc
      ]

  go st@(acc', _dec, _drops) = \case
    [] -> st
    (point, offerKind) : rest
      | peerBudget env isBig acc' peer <= 0 -> st
      | otherwise -> go (classify point offerKind st) rest

  classify point offerKind (acc1, dec1, drops) =
    case Map.lookup point (ebState acc1) of
      Nothing ->
        -- We are no longer tracking this point (pruned off below the
        -- imm-tip). This is an ephemeral state, mid prune, but go ahead and
        -- prune it now.
        pruneThisOffer
      Just (MkEbState _onset fetchState) -> case (fetchState, poOffered offerKind) of
        (ReceivingFromForge{}, _) ->
          -- Our own forge produced it, so there is nothing to fetch, and no
          -- offer of it can help.
          pruneThisOffer
        (NoBody, OfferedBody size) ->
          -- Body-only offer: request the body, and prune it.
          let (acc2, dec2) = fromMaybe (acc1, dec1) $ requestBody size
           in (acc2, dec2, Map.insert point PruneOfferedBody drops)
        (NoBody, OfferedClosure) ->
          -- Nothing to request until we hold the body.
          (acc1, dec1, drops)
        (NoBody, OfferedBodyAndClosure size) ->
          -- Request the body now, and prune only the body half of the offer:
          -- we will request the closure from this peer once we hold the body,
          -- but never ask it for the body again.
          case requestBody size of
            Nothing -> (acc1, dec1, drops)
            Just (acc2, dec2) -> (acc2, dec2, Map.insert point PruneOfferedBody drops)
        (BodyAcquired _jobPool, OfferedBody _size) ->
          -- We hold the body, and the peer offered only the body.
          (acc1, dec1, Map.insert point PruneOfferedBody drops)
        (BodyAcquired jobPool, OfferedClosure) -> requestClosure jobPool
        (BodyAcquired jobPool, OfferedBodyAndClosure _size) -> requestClosure jobPool
   where
    pruneThisOffer = (acc1, dec1, Map.insert point PruneOffer drops)

    requestBody size =
      assignBody forecastEbBounds peer point (poEbBounds offerKind) size (acc1, dec1)

    requestClosure jobPool
      | Jobs.nullLeiosJobPool jobPool =
          -- Nothing left to fetch --- the whole datum is in hand, or we
          -- rejected the endorser block on arrival or abandoned its closure ---
          -- so the closure offer is useless now too.
          pruneThisOffer
      | otherwise =
          -- Still need the txs, and the peer offered the closure. If we just
          -- now assign all remaining jobs to the peer, prune its offer.
          let ((acc2, dec2), exhausted) = assignClosure env isBig peer point (acc1, dec1)
           in ( acc2
              , dec2
              , case exhausted of
                  PeerEbNotExhausted -> drops
                  PeerEbExhausted -> Map.insert point PruneOffer drops
              )

-- | Request the EB body from this peer, at the size the peer offered it at.
-- 'Nothing' if it does not.
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
  LeiosPoint ->
  -- | The offer's own bounds, if any
  StrictMaybe LeiosEbBounds ->
  -- | The size the peer offered the body at
  BytesSize ->
  (LeiosOutstanding peer, Seq LeiosFetchRequest) ->
  Maybe (LeiosOutstanding peer, Seq LeiosFetchRequest)
assignBody forecastEbBounds peer point offerBounds size (acc, dec)
  | peer `Set.member` Map.findWithDefault Set.empty point (requestedEbPeers acc) =
      -- unless we've already requested it from them
      Nothing
  | otherwise = case eEbBounds of
      -- Beyond our horizon and the offer brought no bound of its own, so we
      -- have nothing to hold the body to. Ignoring it this iteration costs
      -- nothing: a later one forecasts again, from a tip that has moved.
      Left _outsideRange -> Nothing
      Right bounds ->
        let acc' =
              acc
                { requestedEbPeers =
                    Map.insertWith Set.union point (Set.singleton peer) (requestedEbPeers acc)
                , requestedEbsPerPeer =
                    Map.insertWith NESet.union peer (NESet.singleton point) (requestedEbsPerPeer acc)
                , requestedBytesSizePerPeer =
                    Map.insertWith (+) peer size (requestedBytesSizePerPeer acc)
                }
         in Just
              ( acc'
              , dec
                  Seq.|> LeiosBlockRequest
                    MkLeiosBlockRequest
                      { lbrPoint = point
                      , lbrOfferedSize = size
                      , lbrEbBounds = bounds
                      }
              )
 where
  -- The bounds a request made for this offer holds the endorser block to: the
  -- offer's own, else what we can forecast for the slot.
  eEbBounds = case offerBounds of
    SJust x -> Right x
    SNothing -> forecastEbBounds (pointSlotNo point)

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
  LeiosPoint ->
  (LeiosOutstanding peer, Seq LeiosFetchRequest) ->
  ((LeiosOutstanding peer, Seq LeiosFetchRequest), WhetherPeerEbExhausted)
assignClosure env isBig peer point st@(acc, dec) =
  case Map.lookup point (ebState acc) of
    Nothing -> (st, PeerEbNotExhausted)
    Just (MkEbState _onset NoBody) -> (st, PeerEbNotExhausted)
    Just (MkEbState _onset ReceivingFromForge{}) -> (st, PeerEbNotExhausted)
    Just (MkEbState onset (BodyAcquired jobPool)) ->
      let inflightJobs =
            maybe IntSet.empty NEIntSet.toSet $
              Map.lookup point =<< Map.lookup peer (requestedJobsPerPeer acc)
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
                            point
                            (MkEbState onset (BodyAcquired jobPool'))
                            (ebState acc)
                      , requestedJobsPerPeer =
                          Map.insertWith
                            (Map.unionWith NEIntSet.union)
                            peer
                            ( Map.singleton point $
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
                  reqs = batchTxsRequests env point nePicked
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
