{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module LeiosDemoLogic (module LeiosDemoLogic) where

import Cardano.Slotting.Slot (SlotNo (..), withOrigin)
import Control.Concurrent.Class.MonadMVar (MVar)
import qualified Control.Concurrent.Class.MonadMVar as MVar
import Control.Concurrent.Class.MonadSTM.Strict (StrictTVar)
import qualified Control.Concurrent.Class.MonadSTM.Strict as StrictSTM
import Control.Monad (foldM, forM_, unless, when)
import Control.Monad.Class.MonadThrow (Exception, catch, throwIO)
import Control.Monad.Except (runExcept)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Tracer (Tracer, contramap, nullTracer, traceWith)
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import Data.Foldable (fold)
import Data.Functor (void, (<&>))
import qualified Data.IntMap as IntMap
import qualified Data.IntMap.NonEmpty as NEIntMap
import qualified Data.IntSet as IntSet
import Data.IntSet.NonEmpty (NEIntSet)
import qualified Data.IntSet.NonEmpty as NEIntSet
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Maybe.Strict (StrictMaybe (..), strictMaybeToMaybe)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Sequence.NonEmpty as NESeq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import Data.Time.Clock (NominalDiffTime)
import qualified Data.Vector.Strict as V
import qualified Data.Vector.Strict.Mutable as MV
import Data.Word (Word16, Word64)
import LeiosDemoDb
  ( LeiosDbReader
  , LeiosDbWriter (..)
  , Promise (..)
  , batchRetrieveTxs
  , lookupEbBody
  )
import LeiosDemoException (throwLeiosDbException)
import LeiosDemoLogic.Announcements
  ( AnnouncementVerdict (..)
  , ElState (..)
  , ErrAnnouncement
  , PeerState
  , ShouldRelay (..)
  , TraceLeiosNotifyEvent (..)
  , TraceLeiosNotifyPeerEvent (..)
  , announcementsInSlot
  , emptyPeerState
  , prunePeerState
  )
import qualified LeiosDemoLogic.Announcements as Announcements
import LeiosDemoLogic.Announcements.ElBimap (ElId (..))
import LeiosDemoLogic.Announcements.Validate
  ( AnnouncementInvalidity
  , validateAnnouncementHeader
  )
import qualified LeiosDemoOnlyTestFetch as LF
import LeiosDemoTypes
  ( AnnouncementEquivocation (..)
  , AnnouncementFields (..)
  , AnnouncementSource (..)
  , BytesSize
  , EbHash (..)
  , LeiosBlockRequest (..)
  , LeiosBlockTxsRequest (..)
  , LeiosEb (..)
  , LeiosFetchRequest (..)
  , LeiosFetchStaticEnv
  , LeiosOutstanding
  , LeiosPeerVars
  , LeiosPoint (..)
  , LeiosTx (..)
  , PeerId (..)
  , RbHash (..)
  , SerializedEbBody
  , TraceLeiosKernel (..)
  , TraceLeiosPeer (..)
  , TxHash (..)
  , WhetherTxsClosureOffered (..)
  , announcementLeiosPoint
  , encodeLeiosEbSize
  , fetchArrivalEvicted
  , fetchArrivalExtra
  , fetchArrivalGood
  , fetchArrivalInvalid
  , hashLeiosEb
  , hashLeiosTx
  , leiosEbTxs
  , maxTxsPerEb
  )
import qualified LeiosDemoTypes as Leios
import qualified LeiosDemoTypes.LeiosJobs as Jobs
import LeiosTxCache (LeiosTxCache (..))
import Ouroboros.Consensus.Block
  ( BlockProtocol
  , ConvertRawHash
  , HasHeader
  , Header
  , WithOrigin (NotOrigin)
  , blockSlot
  , headerHash
  , toRawHash
  )
import Ouroboros.Consensus.BlockchainTime
  ( CurrentSlot (CurrentSlot, CurrentSlotUnknown)
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime
  , SystemTime
  , diffRelTime
  , systemTimeCurrent
  )
import Ouroboros.Consensus.Config (TopLevelConfig, configLedger)
import Ouroboros.Consensus.Forecast (OutsideForecastRange, forecastFor)
import Ouroboros.Consensus.Ledger.Abstract (getTipSlot)
import Ouroboros.Consensus.Ledger.Basics (EmptyMK)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState, ledgerState)
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol
  , ledgerViewForecastAt
  )
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import Ouroboros.Consensus.Protocol.Abstract (ChainDepState, LedgerView)
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( OCINStaleness (..)
  , ResolveLeiosBlock (..)
  )
import Ouroboros.Consensus.Util.IOLike (IOLike, async, link)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
  ( IsBigLedgerPeer (..)
  )
import System.Random (StdGen)

-- | Wrap an action with exception tracing. Catches the exception,
-- traces it using the provided handler, and re-throws.
traceException :: (IOLike m, Exception e) => Tracer m a -> (e -> a) -> m b -> m b
traceException tracer toTrace action =
  action `catch` \e -> traceWith tracer (toTrace e) >> throwIO e

{-------------------------------------------------------------------------------
  Shadow LeiosTxCache wiring

  The 'LeiosTxCache' handle is maintained (announcements, bodies, and txs inserted
  at the same sites as the LeiosDb) but not yet consulted, so it changes no
  observable behavior. The node's handle is
  @'LeiosTxCache' m () () 'SerializedEbBody'@: only presence (@()@) is recorded
  per tx, and the serialized body is the @b@.
-------------------------------------------------------------------------------}

-- | Insert an EB announcement into the tx-cache index, keyed by the announced
-- slot, the announcing RB header's hash, and the announced EB hash. Evicted
-- bodies\/txs are discarded; they can be useful for debugging/etc.
recordAnnouncementInTxCache ::
  IOLike m =>
  LeiosTxCache m () () SerializedEbBody ->
  -- | The announcing block's hash.
  RbHash ->
  LeiosPoint ->
  m ()
recordAnnouncementInTxCache txCache rbh point =
  void $ txCache.insertAnnouncement point.pointSlotNo rbh point.pointEbHash

-- | Register a locally-forged EB in the tx-cache: its announcement, its
-- body, and each of its txs as already-applied (the forger drew them from its
-- validated mempool, so they are known-valid). This mirrors the receive side,
-- which splits the same inserts between announcement handling
-- ('recordAnnouncementInTxCache') and body acquisition. The applied-tagging must
-- follow 'insertBody', which creates the per-tx entries that the tagging upgrades.
recordForgedEbAndClosureInTxCache ::
  Monad m =>
  Tracer m TraceLeiosKernel ->
  LeiosTxCache m () () SerializedEbBody ->
  RbHash ->
  Leios.ForgedLeiosEb ->
  m ()
recordForgedEbAndClosureInTxCache tracer txCache rbh forgedEb = do
  _ <- txCache.insertAnnouncement point.pointSlotNo rbh point.pointEbHash
  -- The forge path does not fetch, so it discards the miss set: a unit
  -- accumulator and a no-op snoc.
  mbSummary <-
    fmap (fmap @Maybe (\(x, ()) -> x)) $
      insertBody txCache point.pointEbHash (Leios.serializeEbBody eb) () (\() _ _ _ _ -> ())
  -- A forged body holds its whole closure locally: every tx not already in the
  -- cache came from our own mempool (that is where the forge selected them). There
  -- is no actual mempool-pull stage, so attribute those txs -- @txsInEb - acquired@
  -- -- to mempool hits directly, making the combined cache+mempool hit rate 100%.
  forM_ mbSummary $ \summary ->
    traceWith tracer $
      TraceLeiosBodyHits point summary (Leios.ibsTxsInEb summary - Leios.ibsAcquired summary) 0
  withLockedInsertAppliedTx txCache $ \w0 step ->
    foldM (\w (txh, _sz) -> step w txh ()) w0 (leiosEbTxs eb)
 where
  point = forgedEb.point
  eb = forgedEb.body

-----

data SomeLeiosFetchContext m
  = MkSomeLeiosFetchContext !(LeiosFetchContext m)

data LeiosFetchContext m = MkLeiosFetchContext
  { leiosDbReader :: !(LeiosDbReader m)
  , leiosEbBuffer :: !(MV.MVector (PrimState m) (TxHash, BytesSize))
  , leiosEbTxsBuffer :: !(MV.MVector (PrimState m) LeiosTx)
  }

-- | Build a per-instance fetch context around an already-opened DB connection.
--
-- The connection is owned by the caller: SQLite connections must not be
-- shared across threads, and each LeiosFetch client/server instance runs on
-- its own thread, so the caller is expected to bracket a fresh 'open' /
-- 'close' pair for the lifetime of that instance (see 'withReader').
newLeiosFetchContext ::
  PrimMonad m =>
  LeiosDbReader m ->
  m (LeiosFetchContext m)
newLeiosFetchContext leiosDbReader = do
  leiosEbBuffer <- MV.new maxTxsPerEb
  leiosEbTxsBuffer <- MV.new maxTxsPerEb
  pure
    MkLeiosFetchContext{leiosDbReader, leiosEbBuffer, leiosEbTxsBuffer}

-----

leiosFetchHandler ::
  IOLike m =>
  Tracer m TraceLeiosPeer ->
  LeiosFetchContext m ->
  LF.LeiosFetchRequestHandler LeiosPoint LeiosEb LeiosTx m
leiosFetchHandler tracer leiosContext = LF.MkLeiosFetchRequestHandler $ \case
  LF.MsgLeiosBlockRequest p -> do
    traceWith tracer $ MkTraceLeiosPeer $ "[start] MsgLeiosBlockRequest " <> Leios.prettyLeiosPoint p
    x <- msgLeiosBlockRequest tracer leiosContext p
    traceWith tracer $ MkTraceLeiosPeer $ "[done] MsgLeiosBlockRequest " <> Leios.prettyLeiosPoint p
    pure $ LF.MsgLeiosBlock x
  LF.MsgLeiosBlockTxsRequest p bitmaps -> traceException tracer TraceLeiosPeerDbException $ do
    traceWith tracer $ MkTraceLeiosPeer $ "[start] MsgLeiosBlockTxsRequest " <> Leios.prettyLeiosPoint p
    x <- msgLeiosBlockTxsRequest tracer leiosContext p bitmaps
    traceWith tracer $ MkTraceLeiosPeer $ "[done] MsgLeiosBlockTxsRequest " <> Leios.prettyLeiosPoint p
    pure $ LF.MsgLeiosBlockTxs p bitmaps x

msgLeiosBlockRequest ::
  IOLike m =>
  Tracer m TraceLeiosPeer ->
  LeiosFetchContext m ->
  LeiosPoint ->
  m LeiosEb
msgLeiosBlockRequest tracer leiosContext point@MkLeiosPoint{pointEbHash} = do
  let MkLeiosFetchContext{leiosDbReader, leiosEbBuffer = buf} = leiosContext
  n <- traceException tracer TraceLeiosPeerDbException $ do
    -- get the EB items using new db
    items <- lookupEbBody leiosDbReader pointEbHash
    when (null items) $ throwIO $ ExnLeiosUnknownBlockRequested point
    -- A database written by an older build can hold a body with more rows
    -- than the buffer has room for.
    let rowCount = length items
    when (rowCount > maxTxsPerEb) $
      throwLeiosDbException $
        "EB "
          <> Leios.prettyEbHash pointEbHash
          <> " has "
          <> show rowCount
          <> " rows, more than maxTxsPerEb = "
          <> show maxTxsPerEb
    let loop !i [] = pure i
        loop !i ((txHash, txBytesSize) : rest) = do
          MV.write buf i (txHash, txBytesSize)
          loop (i + 1) rest
    loop 0 items
  v <- V.freeze $ MV.slice 0 n buf
  pure $ MkLeiosEb v

msgLeiosBlockTxsRequest ::
  IOLike m =>
  Tracer m TraceLeiosPeer ->
  LeiosFetchContext m ->
  LeiosPoint ->
  [(Word16, Word64)] ->
  m (V.Vector LeiosTx)
msgLeiosBlockTxsRequest _tracer leiosContext point bitmaps = do
  let MkLeiosFetchContext{leiosDbReader, leiosEbTxsBuffer = buf} = leiosContext
  let txOffsets = bitmapOffsets bitmaps
  n <- do
    -- Use new db to batch retrieve transactions
    results <- batchRetrieveTxs leiosDbReader point.pointEbHash txOffsets
    -- See the same check in 'msgLeiosBlockRequest'.
    let rowCount = length results
    when (rowCount > maxTxsPerEb) $
      throwLeiosDbException $
        "EB "
          <> Leios.prettyEbHash point.pointEbHash
          <> " has "
          <> show rowCount
          <> " requested rows, more than maxTxsPerEb = "
          <> show maxTxsPerEb
    -- Process results and write to buffer
    -- REVIEW: why a mutable vector?
    -- Every requested offset must come back, or the request named one this
    -- endorser block does not have.
    when (length results /= length txOffsets) $
      throwIO $
        ExnLeiosUnknownTxsRequested point txOffsets
    let loop !i [] = pure i
        loop !i ((offset, _txHash, mbTxBytes) : rest) = do
          case mbTxBytes of
            Nothing -> throwIO $ ExnLeiosUnknownTxsRequested point [offset]
            Just txBytes -> do
              -- NOTE: We do not need to decode the stored bytes into a proper
              -- 'Tx era' in order to serve them through the mini-protocols.
              MV.write buf i (MkLeiosTx txBytes)
              loop (i + 1) rest
    loop 0 results
  V.freeze $ MV.slice 0 n buf

-- | For example
-- @
--   print $ unfoldr popLeftmostOffset 0
--   print $ unfoldr popLeftmostOffset 1
--   print $ unfoldr popLeftmostOffset (2^(34 :: Int))
--   print $ unfoldr popLeftmostOffset (2^(63 :: Int) + 2^(62 :: Int) + 8)
--   []
--   [63]
--   [29]
--   [0,1,60]
-- @
popLeftmostOffset :: Word64 -> Maybe (Int, Word64)
{-# INLINE popLeftmostOffset #-}
popLeftmostOffset = \case
  0 -> Nothing
  w ->
    let zs = Bits.countLeadingZeros w
     in Just (zs, Bits.clearBit w (63 - zs))

-----

-- | Decide what to request from each peer right now
--
-- A big-ledger peer (per 'bigLedgerPeers') is fetched from aggressively: it has a
-- larger per-peer byte budget, enough that a closure it offers is requested in
-- full (the whole remaining job pool) at once.
--
-- NOTE that this does not read txs from the LeiosTxCache nor from the Mempool;
-- that happened when the EB body arrived, in 'processLeiosBlock'. (TODO the
-- LeiosFetch client could also check the LeiosTxCache and the Mempool just
-- before it sends the request? The major cost is that doing so requires
-- retaining the individual txs' hashes in memory and/or fetching them from
-- disk, which adds complexity and\/or latency. At least with the /current/
-- SQLite-based LeiosDb backend, that complexity and\/or latency is not the
-- responsibility of the LeiosFetch logic.)
leiosFetchLogicIteration ::
  forall pid.
  Ord pid =>
  LeiosFetchStaticEnv ->
  -- | The maximum EB closure size forecast from the immutable tip to a slot.
  --
  -- Only an offer that brought no bound of its own needs this, and for those it
  -- cannot fail: the announcement backing such an offer was accepted by
  -- forecasting from the immutable tip to that same slot. See
  -- 'Leios.poMaxEbTxsSize'.
  (SlotNo -> Either OutsideForecastRange BytesSize) ->
  -- | The current slot, or 'Nothing' when it is not yet known (i.e. we are
  -- syncing), in which case we fetch freshest-last instead of freshest-first.
  Maybe SlotNo ->
  Map (PeerId pid) (Map LeiosPoint Leios.PeerOffer) ->
  -- | Which peers are big-ledger peers (a peer absent from this map is treated as
  -- 'IsNotBigLedgerPeer').
  Map (PeerId pid) IsBigLedgerPeer ->
  LeiosOutstanding pid ->
  -- | The new outstanding state, the requests to send, and the offers to prune
  ( LeiosOutstanding pid
  , Map (PeerId pid) (NESeq LeiosFetchRequest)
  , Map (PeerId pid) (NESet.NESet LeiosPoint)
  )
leiosFetchLogicIteration env forecastMaxEbTxsSize mbCurrentSlot offerings bigLedgerPeers = \acc0 ->
  -- One pass per peer. Bodies and tx-closure jobs compete on equal footing,
  -- ranked by each EB's slot in 'ebState' (its greatest announcement slot), so
  -- the freshest EBs are fetched first whether it is the body or the closure
  -- they still need.
  -- Each peer's 'assignPeer' yields only its own requests and dead offers; fold
  -- those into the per-peer maps here.
  Map.foldlWithKey'
    ( \(acc, reqs, drops) peerId offers ->
        let isBig = Map.findWithDefault IsNotBigLedgerPeer peerId bigLedgerPeers
            (acc', peerReqs, peerDrops) =
              assignPeer env forecastMaxEbTxsSize mbCurrentSlot isBig peerId offers acc
         in ( acc'
            , case NESeq.nonEmptySeq peerReqs of
                Nothing -> reqs
                Just neReqs -> Map.insert peerId neReqs reqs
            , case NESet.nonEmptySet peerDrops of
                Nothing -> drops
                Just nes -> Map.insert peerId nes drops
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
  Ord pid => LeiosFetchStaticEnv -> IsBigLedgerPeer -> LeiosOutstanding pid -> PeerId pid -> Int
peerBudget env isBig acc peerId =
  fromIntegral cap
    - fromIntegral (Map.findWithDefault 0 peerId (Leios.requestedBytesSizePerPeer acc))
 where
  cap = case isBig of
    IsBigLedgerPeer -> Leios.maxRequestedBytesSizePerBigLedgerPeer env
    IsNotBigLedgerPeer -> Leios.maxRequestedBytesSizePerPeer env

-- | Prioritize a peer's Leios offers
--
-- The offers are categorized into two tiers. The high-priority tier prioritizes
-- /staler/ EBs (those with lesser slot numbers). The low-priority tier
-- prioritizes /fresher/ EBs (those with greater slot numbers).
--
-- 'assignPeer' processes the high-priority tier first and then it carries over
-- the resulting accumulator in order to process the low-priority tier.
--
-- When the node's ledger state is too old to know what the current slot is, all
-- EBs are categorized as the high-priority tier, and the low-priority tier is
-- empty.
--
-- When the current slot is known, the high-priority tier is only the freshest
-- EBs, those no older than L = 3*L_hdr + L_vote + L_diff (ie whose slot is @>=
-- currentSlot - L@). All EBs older than that are categorized as the
-- low-priority tier.
fetchPriorityTiers ::
  Maybe SlotNo -> Word64 -> Map LeiosPoint v -> ([(LeiosPoint, v)], [(LeiosPoint, v)])
fetchPriorityTiers mbCurrentSlot l offers =
  (Map.toAscList highTier, Map.toDescList lowTier)
 where
  (highTier, lowTier) = case mbCurrentSlot of
    Nothing -> (offers, Map.empty)
    Just (SlotNo s) ->
      -- @a + l < s@ (i.e. @a < S - L@) is the low tier, guarding underflow when
      -- @S < L@; 'spanAntitone' relies on it being false-suffixed in slot order.
      let (stale, fresh) = Map.spanAntitone (\p -> case p.pointSlotNo of SlotNo a -> a + l < s) offers
       in (fresh, stale)

-- | Walk this peer's offered points in priority order (see
-- 'fetchPriorityTiers'), assigning requests to the peer until it's saturated at
-- 'Leios.maxRequestedBytesSizePerPeer'. A big-ledger peer saturates instead at
-- the larger 'Leios.maxRequestedBytesSizePerBigLedgerPeer', enough that a
-- couple closures it offers can be entirely inflight at the same time (see
-- 'assignClosure').
--
-- Offers beyond the saturation point are never visited, so aren't pruned this
-- pass; that's fine because it's ephemeral and/or the other prune based on the
-- imm-tip advancing is a backstop.
assignPeer ::
  Ord pid =>
  LeiosFetchStaticEnv ->
  (SlotNo -> Either OutsideForecastRange BytesSize) ->
  Maybe SlotNo ->
  IsBigLedgerPeer ->
  PeerId pid ->
  Map LeiosPoint Leios.PeerOffer ->
  LeiosOutstanding pid ->
  (LeiosOutstanding pid, Seq LeiosFetchRequest, Set LeiosPoint)
assignPeer env forecastMaxEbTxsSize mbCurrentSlot isBig peerId offers acc =
  -- Walk the high-priority tier, then the low, threading the accumulator; the
  -- second walk immediately short-circuits if the first already saturated the
  -- peer.
  go (go (acc, Seq.empty, Set.empty) highTier) lowTier
 where
  (highTier, lowTier) =
    fetchPriorityTiers
      mbCurrentSlot
      (Leios.fetchPriorityWindowSlots env)
      -- Only what some election is currently fetching. An offer of anything
      -- else is skipped rather than dropped, so it comes back into play if some
      -- election later fetches that endorser block --- because a certificate
      -- moved an election onto it, or because another election announced it.
      (Map.filterWithKey (\point _ -> Leios.isFocusedEb point.pointEbHash acc) offers)

  go st@(acc', _dec, _drops) = \case
    [] -> st
    (point, offerKind) : rest
      | peerBudget env isBig acc' peerId <= 0 -> st
      | otherwise -> go (classify point offerKind st) rest

  classify point offerKind (acc1, dec1, drops) =
    case Map.lookup ebHash (Leios.ebState acc1) of
      Nothing ->
        -- We are no longer tracking this EB (pruned off below the
        -- imm-tip). This is an ephemeral state, mid prune, but go ahead and
        -- prune it now.
        pruneThisOffer
      Just (Leios.MkEbState slot _onset fetchState) -> case (fetchState, Leios.poClosure offerKind) of
        (Leios.BodyImminent, _) ->
          -- Our forge is producing this EB, so we hold the whole datum (even
          -- though it might not be inserted yet): never request it, and the
          -- peer's offer is dead.
          pruneThisOffer
        (Leios.NoBody, TxsClosureNotOffered) ->
          -- Body-only offer: request the body. If that's all that was
          -- offered, prune it.
          let (acc2, dec2) = assignBody forecastMaxEbTxsSize peerId ebHash slot offerKind (acc1, dec1)
           in (acc2, dec2, Set.insert point drops)
        (Leios.NoBody, TxsClosureOffered) ->
          -- Request the body now, but keep the offer: we will request the
          -- closure from this peer once we hold the body.
          let (acc2, dec2) = assignBody forecastMaxEbTxsSize peerId ebHash slot offerKind (acc1, dec1)
           in (acc2, dec2, drops)
        (Leios.BodyAcquired _jobPool, TxsClosureNotOffered) ->
          -- We hold the body and the peer never offered the closure, so it can
          -- no longer help.
          pruneThisOffer
        (Leios.BodyAcquired jobPool, TxsClosureOffered)
          | Jobs.nullLeiosJobPool jobPool ->
              -- Nothing left to fetch --- the whole datum is in hand, or we
              -- rejected the endorser block on arrival --- so the closure offer
              -- is useless now too.
              pruneThisOffer
          | otherwise ->
              -- Still need the txs, and the peer offered the closure. If we just
              -- now assign all remaining jobs to the peer, prune its offer.
              let ((acc2, dec2), MkWhetherPeerEbExhausted exhausted) =
                    assignClosure env isBig peerId ebHash (acc1, dec1)
               in (acc2, dec2, if not exhausted then drops else Set.insert point drops)
   where
    ebHash = point.pointEbHash

    pruneThisOffer = (acc1, dec1, Set.insert point drops)

-- | Request the EB body from this peer, at the size the peer offered it at.
--
-- The offered size is the only size with any say here; an announcement's has
-- none, since at most one announcement naming a hash is honest and nothing
-- tells us which. So we ask whoever says they have it, for as many bytes as
-- they say. The offer was bounded by 'Leios.maxLeiosEbBytesSize' when it
-- arrived ('checkLeiosBlockOffer'), and what the bytes really are is settled on
-- arrival by hashing them ('processLeiosBlock'): a peer that misrepresented
-- either the block or its length loses the connection then.
assignBody ::
  Ord pid =>
  -- | The maximum EB closure size forecast for a slot. See
  -- 'Leios.poMaxEbTxsSize'.
  (SlotNo -> Either OutsideForecastRange BytesSize) ->
  PeerId pid ->
  EbHash ->
  SlotNo ->
  Leios.PeerOffer ->
  (LeiosOutstanding pid, Seq LeiosFetchRequest) ->
  (LeiosOutstanding pid, Seq LeiosFetchRequest)
assignBody forecastMaxEbTxsSize peerId ebHash slot offer st@(acc, dec)
  | peerId `Set.member` Map.findWithDefault Set.empty ebHash (Leios.requestedEbPeers acc) =
      -- unless we've already requested it from them
      st
  | otherwise = case (eMaxEbTxsSize, Leios.poOfferedBody offer) of
      -- Beyond our horizon and the offer brought no bound of its own, so we
      -- have nothing to hold the body to. Ignoring it this iteration costs
      -- nothing: a later one forecasts again, from a tip that has moved.
      (Left _outsideRange, _) -> st
      -- the peer offered the closure but not the body
      (_, SNothing) -> st
      (Right maxEbTxsSize, SJust size) ->
        let acc' =
              acc
                { Leios.requestedEbPeers =
                    Map.insertWith Set.union ebHash (Set.singleton peerId) (Leios.requestedEbPeers acc)
                , Leios.requestedBytesSizePerPeer =
                    Map.insertWith (+) peerId size (Leios.requestedBytesSizePerPeer acc)
                }
         in ( acc'
            , dec
                Seq.|> LeiosBlockRequest
                  MkLeiosBlockRequest
                    { lbrPoint = MkLeiosPoint slot ebHash
                    , lbrOfferedSize = size
                    , lbrMaxEbTxsSize = maxEbTxsSize
                    }
            )
 where
  -- What the offer brought, else what we can forecast for its slot.
  eMaxEbTxsSize = case Leios.poMaxEbTxsSize offer of
    SJust x -> Right x
    SNothing -> forecastMaxEbTxsSize slot

-- | Flag indicating whether all jobs matching a peer's offers are already
-- inflight
newtype WhetherPeerEbExhausted = MkWhetherPeerEbExhausted Bool

-- | Request tx-closure jobs from this peer, the least-requested ones we haven't
-- already requested from it. Keep adding jobs until the peer is saturated or
-- there are no jobs left. Also returns whether there are no jobs left.
assignClosure ::
  Ord pid =>
  LeiosFetchStaticEnv ->
  IsBigLedgerPeer ->
  PeerId pid ->
  EbHash ->
  (LeiosOutstanding pid, Seq LeiosFetchRequest) ->
  ((LeiosOutstanding pid, Seq LeiosFetchRequest), WhetherPeerEbExhausted)
assignClosure env isBig peerId ebHash st@(acc, dec) =
  case Map.lookup ebHash (Leios.ebState acc) of
    Nothing -> (st, MkWhetherPeerEbExhausted False)
    Just (Leios.MkEbState _slot _onset Leios.NoBody) -> (st, MkWhetherPeerEbExhausted False)
    Just (Leios.MkEbState _slot _onset Leios.BodyImminent) -> (st, MkWhetherPeerEbExhausted False)
    Just (Leios.MkEbState slot onset (Leios.BodyAcquired jobPool)) ->
      assignInto slot onset jobPool Leios.BodyAcquired
 where
  assignInto slot onset jobPool rebuild =
    let inflightJobs =
          maybe IntSet.empty NEIntSet.toSet $
            Map.lookup ebHash =<< Map.lookup peerId (Leios.requestedJobsPerPeer acc)
        -- A big-ledger peer gets a larger budget ('peerBudget'), enough for multiple
        -- full EB closures at once, but still bounded.
        --
        -- There are no more than 184 jobs per EB, so picked can't be a /long/ list.
        --
        -- 'pickJobs' draws from the decision loop's own PRNG ('leiosFetchPrng');
        -- its advanced state is written back below (unchanged when nothing is
        -- picked, so the 'Nothing' branch's 'st' is correct as-is).
        (picked, jobPool', prng', exhausted) =
          pickJobs (Leios.leiosFetchPrng acc) inflightJobs jobPool (peerBudget env isBig acc peerId)
     in flip (,) exhausted $ case nonEmpty picked of
          Nothing -> st
          Just nePicked ->
            let acc' =
                  acc
                    { Leios.ebState =
                        Map.insert
                          ebHash
                          (Leios.MkEbState slot onset (rebuild jobPool'))
                          (Leios.ebState acc)
                    , Leios.requestedJobsPerPeer =
                        Map.insertWith
                          (Map.unionWith NEIntSet.union)
                          peerId
                          (Map.singleton ebHash $ NEIntSet.fromList $ fmap (\(Jobs.MkLeiosJobId i, _) -> i) nePicked)
                          (Leios.requestedJobsPerPeer acc)
                    , Leios.requestedBytesSizePerPeer =
                        Map.insertWith
                          (+)
                          peerId
                          (sum $ fmap (\(_, Jobs.MkLeiosJob _ bytes _) -> bytes) nePicked)
                          (Leios.requestedBytesSizePerPeer acc)
                    , Leios.leiosFetchPrng = prng'
                    }
                reqs = batchTxsRequests env (MkLeiosPoint slot ebHash) nePicked
             in (acc', dec <> Seq.fromList reqs)

-- | Take least-requested-available jobs until the budget is spent or
-- there are no more jobs that aren't already assigned to this peer. Also
-- returns true, in the latter case. Each pick carries the whole 'Jobs.LeiosJob'
-- (id + commitment) so the request can validate its own response. The supplied
-- PRNG shuffles which job is drawn within the least-requested bucket; its
-- advanced state is returned so the caller can persist it.
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
    | budget <= 0 = (reverse acc, jobPool, prng, MkWhetherPeerEbExhausted False)
    | otherwise = case Jobs.pickLeastRequestedJobExcept prng inflightJobs jobPool of
        Nothing -> (reverse acc, jobPool, prng, MkWhetherPeerEbExhausted True)
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
  cap = fromIntegral (Leios.maxRequestBytesSize env) :: Int
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

-- | The offset set as the wire bitmap (chunk index, 64-bit mask).
offsetsToBitmap :: IntSet.IntSet -> [(Word16, Word64)]
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

-----

nextLeiosFetchClientCommand ::
  forall pid m.
  ( Ord pid
  , IOLike m
  ) =>
  Tracer m TraceLeiosKernel ->
  Tracer m TraceLeiosPeer ->
  StrictSTM.STM m Bool ->
  ( MVar m (LeiosOutstanding pid)
  , MVar m ()
  ) ->
  LeiosTxCache m () () SerializedEbBody ->
  LeiosDbWriter m ->
  -- | For reporting each arriving EB's age (see 'processLeiosBlock').
  SystemTime m ->
  -- | Pull EB-body misses out of the local mempool; see 'processLeiosBlock'.
  ( IntMap.IntMap (TxHash, BytesSize) ->
    m (IntMap.IntMap (TxHash, BytesSize), Map TxHash BS.ByteString)
  ) ->
  PeerId pid ->
  StrictTVar m (Seq LeiosFetchRequest) ->
  m
    ( Either
        (m (Either () (LF.SomeLeiosFetchJob LeiosPoint LeiosEb LeiosTx m)))
        (Either () (LF.SomeLeiosFetchJob LeiosPoint LeiosEb LeiosTx m))
    )
nextLeiosFetchClientCommand ktracer tracer stopSTM kernelVars txCache writer systemTime pullFromMempool peerId reqsVar = do
  StrictSTM.atomically checkOrBlock >>= \case
    Right result -> pure $ Right result
    Left () -> pure $ Left (StrictSTM.atomically awaitStopOrRequest)
 where
  -- Non-blocking: return 'Right result' if stop or a request is available,
  -- or 'Left ()' if we'd have to block (caller returns the blocking STM).
  checkOrBlock ::
    StrictSTM.STM m (Either () (Either () (LF.SomeLeiosFetchJob LeiosPoint LeiosEb LeiosTx m)))
  checkOrBlock =
    stopSTM >>= \case
      True -> pure $ Right $ Left ()
      False ->
        StrictSTM.readTVar reqsVar >>= \case
          req Seq.:<| reqs -> do
            StrictSTM.writeTVar reqsVar reqs
            pure $ Right $ Right $ g req
          Seq.Empty -> pure $ Left ()

  awaitStopOrRequest ::
    StrictSTM.STM m (Either () (LF.SomeLeiosFetchJob LeiosPoint LeiosEb LeiosTx m))
  awaitStopOrRequest =
    stopSTM >>= \case
      True -> pure $ Left ()
      False ->
        StrictSTM.readTVar reqsVar >>= \case
          req Seq.:<| reqs -> do
            StrictSTM.writeTVar reqsVar reqs
            pure $ Right $ g req
          Seq.Empty -> StrictSTM.retry

  -- Responses are processed right on the pipelined-peer collector thread:
  -- everything they touch (the 'LeiosDbWriter' submission point, the locked
  -- tx cache, the kernel MVars) is thread-safe.
  g = \case
    LeiosBlockRequest req ->
      LF.MkSomeLeiosFetchJob
        (LF.MsgLeiosBlockRequest (lbrPoint req))
        ( pure $ \(LF.MsgLeiosBlock eb) ->
            processLeiosBlock
              ktracer
              tracer
              kernelVars
              txCache
              writer
              systemTime
              pullFromMempool
              (ReceivedBlockFrom peerId req)
              eb
        )
    LeiosBlockTxsRequest req@(MkLeiosBlockTxsRequest p jobs) ->
      -- The wire request is just the point + bitmap; the bitmap is the union of
      -- the covered jobs' offsets (the jobs and their commitments stay local).
      let bitmaps = offsetsToBitmap (foldMap (\(Jobs.MkLeiosJob offs _ _) -> offs) jobs)
       in LF.MkSomeLeiosFetchJob
            (LF.MsgLeiosBlockTxsRequest p bitmaps)
            ( pure $ \(LF.MsgLeiosBlockTxs _ _ txs) ->
                processLeiosBlockTxs
                  ktracer
                  tracer
                  kernelVars
                  txCache
                  writer
                  systemTime
                  (ReceivedTxsFrom peerId req txs)
            )

-----

-- | Where an EB body being ingested came from. 'processLeiosBlock' and
-- 'processLeiosBlockTxs' serve both a fetch response from a peer (carrying the
-- request we are fulfilling) and our own forge; the arrival-specific behaviour a
-- local forge skips is: refunding the peer's request budget, classifying/listing
-- the missing txs (a forge holds its whole closure, so nothing is missing), and
-- emitting fetch-arrival telemetry (which would otherwise pollute the metrics
-- with self-produced data).
data LeiosBlockSource pid
  = ReceivedBlockFrom (PeerId pid) LeiosBlockRequest
  | -- | A locally-forged EB, carrying the point the forge assigned it.
    ForgedBlock !LeiosPoint

-- | Like 'LeiosBlockSource', for a batch of EB txs. Each constructor carries its
-- own tx bytes.
data LeiosBlockTxsSource pid
  = ReceivedTxsFrom (PeerId pid) LeiosBlockTxsRequest !(V.Vector LeiosTx)
  | -- | Carries the forged EB's point and body (so the tx hashes come from the
    -- body's 'leiosEbTxs', aligned by position with the closure bytes, rather
    -- than being re-derived) and the closure bytes.
    ForgedTxs !LeiosPoint !LeiosEb !(V.Vector LeiosTx)
  | -- | Carries an EB's txs that 'processLeiosBlock' found in our local mempool
    -- (so it removed them from the fetch job set), already paired with their
    -- (known) tx hashes, to be ingested applied.
    MempoolTxs !LeiosPoint !(IntMap.IntMap (TxHash, BS.ByteString))

-- | The age of an EB on arrival: the wall-clock elapsed from its recorded oldest
-- announcement-slot onset (see 'Leios.ebStateOnset') to @now@, or 'Nothing' if
-- the EB was never heralded by an announcement (an offer-only or self-forged
-- body).
ebPointAge :: RelativeTime -> Map EbHash Leios.EbState -> LeiosPoint -> Maybe NominalDiffTime
ebPointAge now ebStates p = do
  st <- Map.lookup p.pointEbHash ebStates
  onset <- strictMaybeToMaybe (Leios.ebStateOnset st)
  Just (diffRelTime now onset)

processLeiosBlock ::
  ( Ord pid
  , IOLike m
  ) =>
  Tracer m TraceLeiosKernel ->
  Tracer m TraceLeiosPeer ->
  ( MVar m (LeiosOutstanding pid)
  , MVar m ()
  ) ->
  LeiosTxCache m () () SerializedEbBody ->
  LeiosDbWriter m ->
  -- | For reporting the EB's age on arrival (now minus its recorded onset).
  SystemTime m ->
  -- | Pull the txs we already hold in our local mempool out of the given misses
  -- (offset -> (tx hash, size)): returns the misses still to fetch from peers,
  -- plus the mempool-found txs' bytes (which 'processLeiosBlock' ingests itself,
  -- as its last step). See 'noMempoolPull' for the forge/test no-op.
  ( IntMap.IntMap (TxHash, BytesSize) ->
    m (IntMap.IntMap (TxHash, BytesSize), Map TxHash BS.ByteString)
  ) ->
  LeiosBlockSource pid ->
  LeiosEb ->
  m ()
processLeiosBlock ktracer tracer (outstandingVar, readyVar) txCache writer systemTime pullFromMempool source eb = do
  now <- systemTimeCurrent systemTime
  -- validate it
  let (mbPeer, point, ebBytesSize, mbMaxEbTxsSize) = case source of
        ReceivedBlockFrom peerId req ->
          (Just peerId, lbrPoint req, lbrOfferedSize req, Just (lbrMaxEbTxsSize req))
        ForgedBlock p -> (Nothing, p, encodeLeiosEbSize eb, Nothing)
  traceWith tracer $ MkTraceLeiosPeer $ "[start] MsgLeiosBlock " <> Leios.prettyLeiosPoint point
  let MkLeiosPoint _ebSlot ebHash = point
  let ebBytesSize' = encodeLeiosEbSize eb
  let closureBytesSize :: Word64
      closureBytesSize =
        V.foldl' (\acc (_txh, sz) -> acc + fromIntegral sz) 0 (Leios.leiosEbTxs eb)
      mbTooBig = case mbMaxEbTxsSize of
        Just maxEbTxsSize
          | closureBytesSize > fromIntegral maxEbTxsSize -> Just maxEbTxsSize
        _ -> Nothing
  -- A failed-validation body: attribute the whole body to 'fabInvalid'.
  --
  -- TODO throw a proper exception type rather than 'error', which this module
  -- otherwise keeps for what cannot happen --- a peer earning a disconnect is
  -- routine and should not read as a bug in this node. The mirror of
  -- 'ExnLeiosInvalidRequest' is what is missing. 'processLeiosBlockTxs' has the
  -- same helper, with the same gap.
  let invalidReply reason =
        traceWith ktracer (TraceLeiosFetchBodyArrival (fetchArrivalInvalid ebBytesSize'))
          >> error reason
  -- Whether the peer sent a different number of bytes than its offer promised.
  --
  -- That costs it the connection, but only once we have taken the body: the
  -- hash is what says these are the right bytes, and if they are, throwing them
  -- away would let a peer deny us an endorser block just by lowballing its own
  -- offer. So this is settled at the very end of this function.
  --
  -- An honest peer cannot be caught by this, because the voting logic checks
  -- sizes (TODO it doesn't yet; see the related @FIXME@ in
  -- 'LeiosVoting.runLeiosVoting'): only our interpretation of CertRB
  -- roll-forward as an EB body offer interprets the issuer's claimed size as
  -- the peer's claimed size. When that peer isn't also the issuer, they'd lose
  -- their connection to us if the issuer lied about the size. However, an
  -- honest peer only sends that CertRB after validating the (or an equivalent)
  -- certificate. So, there is actually no such risk, because Leios committee is
  -- assumed to be honest.
  let wrongLength = case source of
        ForgedBlock{} -> False
        ReceivedBlockFrom{} -> ebBytesSize' /= ebBytesSize
  case source of
    -- A forge's body is self-produced; never validate it (so no 'error' path is
    -- ever reachable for a locally-forged EB).
    ForgedBlock{} -> pure ()
    ReceivedBlockFrom{} -> do
      -- The hash is the whole of it: these bytes either are the endorser block
      -- we asked for or they are not, and no announcement gets a say.
      let ebHash' = hashLeiosEb eb
      when (ebHash' /= ebHash) $ do
        invalidReply $ "MsgLeiosBlock hash mismatch: " <> show (ebHash', ebHash)
      -- Reject an EB that lists the same tx hash at two offsets: malformed, and
      -- it would otherwise have the tx fetched (and cache-counted) once per offset.
      let MkLeiosEb v = eb
          duplicateTxHashes =
            Map.keys $
              Map.filter (> (1 :: Int)) $
                Map.fromListWith (+) [(txh, 1) | (txh, _) <- V.toList v]
      when (not (null duplicateTxHashes)) $ do
        invalidReply $ "MsgLeiosBlock duplicate tx hashes: " <> show duplicateTxHashes
  -- ingest it. The lock decides 'shouldPersist' (a genuinely novel, still-relevant
  -- body) under exclusion and hands it to 'persistAndIngest' below. That is the
  -- only novelty check we need: keyed off the lock, two peers delivering the same
  -- body cannot both write it (the second sees 'novel = False'), so we neither
  -- re-pay the ~16k-row write nor emit a storm of 'LeiosDbInsertCollision's.
  (shouldPersist, bodyClass, mempoolIngest, missedBoth, fills) <- MVar.modifyMVar outstandingVar $ \outstanding -> do
    let tooOld = point.pointSlotNo < Leios.outstandingPrunedSlot outstanding
        novel = not $ maybe False Leios.ebStateHasBody (Map.lookup ebHash (Leios.ebState outstanding))
        -- Always: this request is no longer in flight and we now have the body,
        -- so drop the body-fetch bookkeeping ('refundEbRequest' reverses the
        -- per-request accounting -- skipped if a disconnect already cancelled it
        -- in bulk); and unless the EB is too old to matter, remember we have it
        -- so we neither re-fetch nor re-offer it.
        !outstandingCleaned =
          ( case mbPeer of
              Just peerId -> refundEbRequest peerId ebHash ebBytesSize
              Nothing -> id
          )
            outstanding
    -- Persist and classify only a genuinely novel, still-relevant body. A
    -- duplicate (already held) or a too-old arrival (its slot is below pruned
    -- watermark, so 'novel' can't be trusted) is left at the bookkeeping above
    -- -- in particular no second 'writeEbBody', hence no duplicate
    -- 'AcquiredEb'/re-offer. So is one over its closure bound, which is
    -- additionally recorded as held-with-nothing-to-fetch, so that neither its
    -- closure nor a re-send of the same bytes costs us anything more. The peer
    -- answers for that one below, once the lock is released --- throwing here
    -- would roll the record back.
    --
    -- TODO the LeiosDb has no way to record that we are done with an endorser
    -- block whose closure we never fetched. Writing the body would say the
    -- wrong thing --- 'writeEbBody' notifies 'AcquiredEb', and we must never
    -- offer this one onward --- so the verdict lives only in 'ebState' and a
    -- restart within the pruning window forfeits it, costing one more body
    -- fetch from the next peer to serve it.
    if tooOld || not novel || isJust mbTooBig
      then
        pure
          ( if isJust mbTooBig
              then Leios.acquireEbBody ebHash Jobs.emptyLeiosJobPool outstandingCleaned
              else outstandingCleaned
          ,
            ( False
            , case (tooOld, mbTooBig) of
                (True, _) -> fetchArrivalEvicted ebBytesSize'
                (_, Just{}) -> fetchArrivalInvalid ebBytesSize'
                _ -> fetchArrivalExtra ebBytesSize'
            , IntMap.empty
            , IntMap.empty
            , []
            )
          )
      else do
        -- Register the body's entries in the in-memory tx cache and classify the
        -- fetch set -- all in-memory, no disk IO under the lock. Persistence and
        -- the mempool-tx copy happen after this lock (forked for received bodies;
        -- see 'persistAndIngest' below).
        -- The body-insert pass also hands back, per referenced tx, where its
        -- bytes durably live if some recent EB holds them ('locatedTxs') -- the
        -- cross-EB fill sources, read in the same locked pass that bumps the
        -- refcounts rather than in a second consultation.
        mbInsertBody <-
          insertBody
            txCache
            ebHash
            (Leios.serializeEbBody eb)
            []
            ( \acc off _txh _sz -> \case
                Just loc -> (off, loc) : acc
                Nothing -> acc
            )
        let locatedTxs = maybe [] snd mbInsertBody
        (bodyClass, mbBodyTxCacheSummary) <- case source of
          -- A forge holds its whole closure, so nothing is missing. Its txs are
          -- inserted (applied) by the subsequent 'processLeiosBlockTxs' call; the
          -- 'insertBody' above only served to register the cache entries.
          ForgedBlock{} -> pure (fetchArrivalGood ebBytesSize', Nothing)
          ReceivedBlockFrom{} -> case mbInsertBody of
            -- 'BodyNotYetInserted': the announcement was present and we filled it.
            Just (txCacheSummary, _located) -> pure (fetchArrivalGood ebBytesSize', Just txCacheSummary)
            -- Announcement absent (assumed present once, since evicted): the
            -- cache insert was a no-op, and there is no cache summary, so no
            -- 'TraceLeiosBodyHits'.
            Nothing -> pure (fetchArrivalEvicted ebBytesSize', Nothing)
        -- Look the /full/ tx set up in our local mempool (not just the cache
        -- misses), so txs in BOTH the mempool and the cache surface here: we prefer
        -- to (re-)apply those from the mempool, since that is what sets their
        -- Applied flag in the cache. All mempool hits are ingested below (the last
        -- thing this function does); none of them become fetch jobs.
        let MkLeiosEb ebTxs = eb
            fullTxSet = IntMap.fromList (zip [0 ..] (V.toList ebTxs))
        (notInMempool, mempoolHits) <- pullFromMempool fullTxSet
        -- The fetch set is everything the mempool cannot supply. The cache is
        -- NOT consulted for it: it records tx hashes we have seen, but bytes
        -- are owned per (ebHash, txOffset) now, so a hash seen in another EB
        -- says nothing about whether THIS EB's row is filled. Letting it
        -- suppress a fetch is how a closure never completes.
        let mempoolIngest =
              IntMap.mapMaybeWithKey
                (\_ (txh, _sz) -> (,) txh <$> Map.lookup txh mempoolHits)
                (IntMap.difference fullTxSet notInMempool)
            missedBoth = notInMempool
        -- Report the body's cache+mempool hit picture: the cache summary, the full
        -- mempool-resident count, and how many txs were in neither (so the combined
        -- hit rate is @(txsInEb - missedBoth) / txsInEb@, avoiding double counting).
        forM_ mbBodyTxCacheSummary $ \txCacheSummary ->
          traceWith ktracer $
            TraceLeiosBodyHits point txCacheSummary (Map.size mempoolHits) (IntMap.size missedBoth)
        -- Misses whose bytes some recent EB durably holds: the body write
        -- copies them locally ('cross-EB fill') instead of fetching them again.
        -- 'locatedTxs' came from the body-insert pass above; keep only the ones
        -- the mempool cannot supply. Optimistic -- a source swept in the meantime
        -- fills nothing -- so the fetch set is decided at settle time from what
        -- actually filled, not promised here.
        let fills = [ol | ol@(off, _loc) <- locatedTxs, off `IntMap.member` missedBoth]
        -- The body is not marked held here: the state is claimed 'BodyAcquired'
        -- only once the write is durable (see 'settle' below), after the write
        -- has been enqueued. Until then the body reads as not-held, so a
        -- concurrent offer may redundantly re-fetch it -- accepted, and simpler
        -- than an in-flight state that a lost/cancelled write would have to undo.
        pure (outstandingCleaned, (True, bodyClass, mempoolIngest, missedBoth, fills))
  void $ MVar.tryPutMVar readyVar ()
  case source of
    ForgedBlock{} -> pure () -- self-produced: not a fetch arrival
    ReceivedBlockFrom{} -> traceWith ktracer $ TraceLeiosFetchBodyArrival bodyClass
  traceWith tracer $ MkTraceLeiosPeer $ "[done] MsgLeiosBlock " <> Leios.prettyLeiosPoint point
  -- Last: ingest the txs we found in our own mempool (they were removed from the
  -- fetch job set above)
  when shouldPersist $
    traceException tracer TraceLeiosPeerDbException $ do
      -- TODO remove the 'writeEbPoint' call below once no important node's
      -- VolatileDB still holds an announcing RB that it fetched without this
      -- patch. 'processAnnouncementCentrally' records the point now, and that
      -- write is persistent, so the only endorser blocks still reaching here
      -- without one are those announced by an RB this node selected before it
      -- ran this code.
      --
      -- The trace below is unconditional, so it fires for every body and means
      -- nothing. Conditioning it on the point being absent would make it
      -- report exactly the endorser blocks described above.
      traceWith ktracer $ TraceLeiosBlockPointMissing point
      -- Enqueue the write first, then claim the body. 'writeEbBody' only parks
      -- on a free writer-queue slot (bounded backpressure), so a cancellation
      -- there -- e.g. the peer going away while the queue is full -- happens
      -- before any state is touched: nothing to strand, no abandon path. Once the
      -- write is durable, 'settle' claims 'BodyAcquired' with the pool the write
      -- still leaves to fetch. During the write window the body reads as not-held,
      -- so a concurrent offer may redundantly re-fetch it -- accepted.
      --
      -- A failed write is fatal: the single writer rethrows and is 'link'ed to the
      -- node, so there is no lost-write case to recover from here.
      --
      -- The size written is the actual size of the received body, regardless of
      -- announcements' or offers' claims.
      pointWritten <- writeEbPoint writer point ebBytesSize'
      bodyWritten <- writeEbBody writer point eb fills
      let settle = do
            completedByPoint <- await pointWritten
            (completedByBody, filledOffs) <- await bodyWritten
            -- The fetch set is what the settled write still misses: the fills
            -- that landed are durable rows, everything else -- fills whose source
            -- vanished included -- stays fetchable. Each job commits to its
            -- covered tx hashes, so a response validates without the body.
            let !jobPool =
                  Jobs.mkLeiosJobPool
                    -- TODO thread the real 'LeiosFetchStaticEnv' rather than the demo one
                    (Leios.maxJobBytesSize Leios.demoLeiosFetchStaticEnv)
                    (Leios.maxJobTxCount Leios.demoLeiosFetchStaticEnv)
                    (IntMap.withoutKeys missedBoth (IntSet.fromList filledOffs))
            MVar.modifyMVar_ outstandingVar $
              pure . Leios.acquireEbBody ebHash jobPool
            -- Point the cache at THIS EB as the durable holder of the txs it just
            -- filled locally, so a re-endorsement fills from the youngest holder
            -- (evicted last) rather than the older source that goes first. Fetched
            -- txs are recorded by 'ingestAcquiredTxs'; together every durable
            -- holder points the cache at itself.
            let MkLeiosEb ebv = eb
            setTxLocations txCache ebHash [(off, fst (ebv V.! off)) | off <- filledOffs]
            void $ MVar.tryPutMVar readyVar ()
            st <- Leios.ebState <$> MVar.readMVar outstandingVar
            traceWith ktracer $ TraceLeiosBlockAcquired point (ebPointAge now st point)
            forM_ (completedByPoint <> completedByBody) $ \p ->
              traceWith ktracer $ TraceLeiosBlockTxsAcquired p (ebPointAge now st p)
      case source of
        -- The forge must not advertise what it has not stored, so it waits.
        ForgedBlock{} -> settle
        ReceivedBlockFrom{} ->
          -- Off this thread, but 'link'ed to it, so a failed write still brings
          -- the node down rather than being silently dropped.
          link =<< async (traceException tracer TraceLeiosPeerDbException settle)
  -- Every mempool hit is ingested for THIS EB: bytes are per (ebHash, offset),
  -- so "the tx is already persisted" (for some other EB) no longer excuses
  -- skipping the write. Also what marks them Applied in the cache.
  unless (IntMap.null mempoolIngest) $
    processLeiosBlockTxs
      ktracer
      tracer
      (outstandingVar, readyVar)
      txCache
      writer
      systemTime
      (MempoolTxs point mempoolIngest)
  -- The correctly-hashed EB body has been processed by this point. So now we
  -- can punish the peer if it the EB isn't what it offered or should not have
  -- been offered.
  when wrongLength $
    throwIO $
      ExnLeiosBlockWrongSize point ebBytesSize ebBytesSize'
  forM_ mbTooBig $ \maxEbTxsSize ->
    throwIO $ ExnLeiosClosureTooBig point closureBytesSize maxEbTxsSize

-- | The 'processLeiosBlock' mempool-pull for paths that never pull from the
-- mempool (the forge, which already holds the whole closure, and tests): keep
-- every miss and find nothing locally.
noMempoolPull ::
  Applicative m =>
  IntMap.IntMap (TxHash, BytesSize) ->
  m (IntMap.IntMap (TxHash, BytesSize), Map TxHash BS.ByteString)
noMempoolPull misses = pure (misses, Map.empty)

-- | Build a 'processLeiosBlock' mempool-pull from a read of the mempool's Leios
-- tx index (keyed by 'TxHash') and an era-specific conversion of a found tx to
-- its 'LeiosTx' bytes. A miss is removed from the still-to-fetch set only if it
-- is present in the index /and/ converts (so a tx we can't turn into bytes is
-- still fetched from peers, never lost). Polymorphic in the index's value type so
-- this stays blk-agnostic.
mkMempoolPull ::
  Monad m =>
  -- | Read the mempool's current Leios tx index.
  m (Map TxHash vtx) ->
  -- | The 'LeiosTx' bytes of a found tx, if it has them.
  (vtx -> Maybe BS.ByteString) ->
  IntMap.IntMap (TxHash, BytesSize) ->
  m (IntMap.IntMap (TxHash, BytesSize), Map TxHash BS.ByteString)
mkMempoolPull readIndex toBytes misses = do
  idx <- readIndex
  let missHashes = Set.fromList (map fst (IntMap.elems misses))
      hits = Map.mapMaybe toBytes (Map.restrictKeys idx missHashes)
      hitHashes = Map.keysSet hits
      stillMissing = IntMap.filter (\(h, _sz) -> not (Set.member h hitHashes)) misses
  pure (stillMissing, hits)

-----

delIf :: (a -> Bool) -> a -> Maybe a
delIf predicate x = if predicate x then Nothing else Just x

-----

-- | Cancel all of a peer's outstanding fetch requests in bulk, e.g. when it
-- disconnects: refund its share of the request budget, drop it from the per-EB
-- body request set ('requestedEbPeers'), and -- via the per-peer
-- 'requestedJobsPerPeer' index -- decrement the multiplicity of every job it
-- had in flight, so those bodies and jobs become re-requestable from other
-- peers.
--
-- TODO eliminate the linear scans
removePeerFromOutstanding ::
  Ord pid =>
  PeerId pid ->
  LeiosOutstanding pid ->
  LeiosOutstanding pid
removePeerFromOutstanding peerId o =
  o
    { Leios.requestedBytesSizePerPeer = Map.delete peerId (Leios.requestedBytesSizePerPeer o)
    , Leios.requestedEbPeers =
        Map.mapMaybe (delIf Set.null . Set.delete peerId) (Leios.requestedEbPeers o)
    , Leios.requestedJobsPerPeer = Map.delete peerId (Leios.requestedJobsPerPeer o)
    , Leios.ebState =
        Map.foldrWithKey
          (\ebHash jobIds -> Map.adjust (releaseJobs jobIds) ebHash)
          (Leios.ebState o)
          (Map.findWithDefault Map.empty peerId (Leios.requestedJobsPerPeer o))
    }
 where
  -- Decrement, in that EB's jobPool, the multiplicity of each job this peer held.
  releaseJobs jobIds (Leios.MkEbState slot onset fetchState) =
    Leios.MkEbState slot onset $ case fetchState of
      Leios.NoBody -> Leios.NoBody
      Leios.BodyImminent -> Leios.BodyImminent
      Leios.BodyAcquired jobPool -> Leios.BodyAcquired $! release jobPool
   where
    release jobPool =
      NEIntSet.foldl' (flip $ Jobs.unpickJob . Jobs.MkLeiosJobId) jobPool jobIds

-----

-- | Reverse this peer's per-request accounting for a received EB body, but only
-- if the peer is still tracked.
--
-- If the peer has already been cancelled in bulk (e.g. it disconnected and its
-- requests were refunded en masse via its 'requestedBytesSizePerPeer' total),
-- that entry is gone; re-applying the per-request refund here would
-- double-subtract 'requestedBytesSizePerPeer' and underflow. So we gate on the peer
-- still being present. The membership check, the refund, and the bulk
-- cancellation all run within the same 'outstandingVar' critical section, so
-- whichever happens first claims the refund and the other no-ops.
refundEbRequest ::
  Ord pid =>
  PeerId pid ->
  EbHash ->
  BytesSize ->
  LeiosOutstanding pid ->
  LeiosOutstanding pid
refundEbRequest peerId ebHash ebBytesSize o
  | Map.member peerId (Leios.requestedBytesSizePerPeer o) =
      o
        { Leios.requestedBytesSizePerPeer =
            Map.update (\x -> delIf (== 0) (x - ebBytesSize)) peerId (Leios.requestedBytesSizePerPeer o)
        , Leios.requestedEbPeers =
            Map.update (delIf Set.null . Set.delete peerId) ebHash (Leios.requestedEbPeers o)
        }
  | otherwise = o

-----

-- | Like 'refundEbRequest', but for a received batch of EB txs: refunds the
-- bytes, gated on the peer still being tracked (see 'refundEbRequest'). The job
-- bookkeeping (jobPool + this peer's in-flight set) is handled by 'completeTxRequest'.
refundTxRequest ::
  Ord pid =>
  PeerId pid ->
  BytesSize ->
  LeiosOutstanding pid ->
  LeiosOutstanding pid
refundTxRequest peerId txsBytesSize o
  | Map.member peerId (Leios.requestedBytesSizePerPeer o) =
      o
        { Leios.requestedBytesSizePerPeer =
            Map.update (\x -> delIf (== 0) (x - txsBytesSize)) peerId (Leios.requestedBytesSizePerPeer o)
        }
  | otherwise = o

-----

-- | On a received tx batch, remove its now-fetched jobs: delete them from the
-- EB's jobPool (they are done for /every/ peer) and from this peer's in-flight set,
-- so neither this peer nor any other is asked for them again. Complements
-- 'refundTxRequest', which handles only the per-peer byte accounting.
completeTxRequest ::
  Ord pid =>
  PeerId pid ->
  LeiosBlockTxsRequest ->
  LeiosOutstanding pid ->
  LeiosOutstanding pid
completeTxRequest peerId (MkLeiosBlockTxsRequest point jobs) =
  adjustOutstandingTxRequest Jobs.completeJob peerId point.pointEbHash (NEIntMap.keysSet jobs)

adjustOutstandingTxRequest ::
  Ord pid =>
  (Jobs.LeiosJobId -> Jobs.LeiosJobPool -> Jobs.LeiosJobPool) ->
  PeerId pid ->
  EbHash ->
  NEIntSet ->
  LeiosOutstanding pid ->
  LeiosOutstanding pid
adjustOutstandingTxRequest onJob peerId ebHash jobIds o =
  o
    { Leios.ebState = Map.adjust completeInJobPool ebHash (Leios.ebState o)
    , Leios.requestedJobsPerPeer =
        Map.update (nonEmptyMap . Map.update dropJobs ebHash) peerId (Leios.requestedJobsPerPeer o)
    }
 where
  completeInJobPool (Leios.MkEbState slot onset fetchState) =
    Leios.MkEbState slot onset $ case fetchState of
      Leios.NoBody -> Leios.NoBody
      Leios.BodyImminent -> Leios.BodyImminent
      Leios.BodyAcquired jobPool -> Leios.BodyAcquired $! complete jobPool
   where
    complete jobPool =
      NEIntSet.foldl' (flip $ onJob . Jobs.MkLeiosJobId) jobPool jobIds
  dropJobs held =
    NEIntSet.nonEmptySet (IntSet.difference (NEIntSet.toSet held) (NEIntSet.toSet jobIds))
  nonEmptyMap m = if Map.null m then Nothing else Just m

-- | Decode a tx-offset bitmap (@[(chunk index, 64-bit mask)]@) to ascending body
-- offsets: the inverse of the fetch logic's 'offsetsToBitmap', and the exact
-- decode the fetch server uses to pick which txs to send -- so the arrival
-- handler derives its validation hashes in the peer's send order.
bitmapOffsets :: [(Word16, Word64)] -> [Int]
bitmapOffsets = unfoldr nextOffset
 where
  nextOffset = \case
    [] -> Nothing
    (idx, bitmap) : k -> case popLeftmostOffset bitmap of
      Nothing -> nextOffset k
      Just (i, bitmap') -> Just (64 * fromIntegral idx + i, (idx, bitmap') : k)

-- | Cheap validation of one covered job against the commitment the request
-- carries for it: its arriving txs match its offset count (popcount) and its
-- total byte size.
--
-- Does /no/ hashing so that redundant\/"hedge" requests doesn't contend for
-- CPU. A peer that over-sends to create extra work is punished even if we
-- already did the (right amount of) CPU work for a peer that replied earlier,
-- without pointlessly repeating that CPU work.
checkJobSize ::
  IntMap.IntMap (LeiosTx, BS.ByteString) ->
  Jobs.LeiosJobId ->
  Jobs.LeiosJob ->
  Either String ()
checkJobSize aligned (Jobs.MkLeiosJobId jid) (Jobs.MkLeiosJob offs expectedBytes _root)
  | IntMap.size sub /= IntSet.size offs =
      Left $ "MsgLeiosBlockTxs job " ++ show jid ++ " count mismatch"
  | fromIntegral (sum [BS.length bs | (_tx, bs) <- IntMap.elems sub]) /= expectedBytes =
      Left $ "MsgLeiosBlockTxs job " ++ show jid ++ " byte-size mismatch"
  | otherwise = Right ()
 where
  -- just the txs from /this/ job
  sub = IntMap.restrictKeys aligned offs

-- | Content validation of one /pending/ job we intend to ingest: hash its
-- arriving txs and check their root hash against the request's commitment,
--
-- Only runs for the first reply for a job. Runs /in addition to/
-- 'checkJobSize'.
ingestJob ::
  IntMap.IntMap (LeiosTx, BS.ByteString) ->
  Jobs.LeiosJobId ->
  Jobs.LeiosJob ->
  Either String [(Int, TxHash, BS.ByteString)]
ingestJob aligned (Jobs.MkLeiosJobId jid) (Jobs.MkLeiosJob offs _expectedBytes expectedRoot)
  | Jobs.jobRootHashOfTxHashes [h | (_, h, _) <- hashed] /= expectedRoot =
      Left $ "MsgLeiosBlockTxs job " ++ show jid ++ " root-hash mismatch"
  | otherwise = Right hashed
 where
  -- 'IntMap.toAscList' is ascending by offset -- the order the root hash
  -- commits to, and the key the LeiosDb stores the bytes under.
  hashed =
    [ (off, hashLeiosTx tx, bs)
    | (off, (tx, bs)) <- IntMap.toAscList (IntMap.restrictKeys aligned offs)
    ]

-----

processLeiosBlockTxs ::
  forall pid m.
  ( Ord pid
  , IOLike m
  ) =>
  Tracer m TraceLeiosKernel ->
  Tracer m TraceLeiosPeer ->
  ( MVar m (LeiosOutstanding pid)
  , MVar m ()
  ) ->
  LeiosTxCache m () () SerializedEbBody ->
  LeiosDbWriter m ->
  -- | For reporting each completed closure's age on arrival.
  SystemTime m ->
  LeiosBlockTxsSource pid ->
  m ()
processLeiosBlockTxs ktracer tracer (outstandingVar, readyVar) txCache writer systemTime source = case source of
  ForgedTxs _point eb txs -> do
    now <- systemTimeCurrent systemTime
    -- Ingest the whole closure (TODO even though we might already have some of
    -- it).
    --
    -- No peer accounting, no arrival telemetry.
    -- The forge holds no fetch jobs for its own EB, so there is nothing to retire
    -- or hand back on either outcome.
    _ <-
      ingestAcquiredTxs
        now
        [ (off, txh, bs)
        | (off, (txh, _sz), bs) <-
            zip3 [0 ..] (V.toList (leiosEbTxs eb)) (V.toList (V.map cbor txs))
        ]
    void $ MVar.tryPutMVar readyVar ()
  MempoolTxs _point hits -> do
    now <- systemTimeCurrent systemTime
    -- Txs found in our local mempool (so already-known-valid): ingest applied,
    -- using the hashes we already have. No peer accounting, no arrival telemetry.
    -- Mempool-sourced: likewise no fetch jobs of ours.
    _ <-
      ingestAcquiredTxs
        now
        [(off, txh, bs) | (off, (txh, bs)) <- IntMap.toAscList hits]
    void $ MVar.tryPutMVar readyVar ()
  ReceivedTxsFrom peerId req@(MkLeiosBlockTxsRequest point jobs) txs -> do
    now <- systemTimeCurrent systemTime
    traceWith tracer $ MkTraceLeiosPeer $ "[start] " ++ Leios.prettyLeiosBlockTxsRequest req
    let txBytess = V.map cbor txs
        batchBytes = V.sum (V.map BS.length txBytess)
        invalidReply :: String -> m a
        invalidReply reason =
          traceWith ktracer (TraceLeiosFetchTxsArrival (fetchArrivalInvalid (fromIntegral batchBytes)))
            >> error reason
        -- The union of the covered jobs' offsets, ascending -- the order the peer
        -- decoded our bitmap into, so it aligns position-wise with the arriving
        -- txs. No hashing here: 'aligned' is just @offset -> (tx, tx bytes)@.
        offsetsSet = foldMap (\(Jobs.MkLeiosJob offs _ _) -> offs) jobs
    when (V.length txs /= IntSet.size offsetsSet) $
      invalidReply $
        "MsgLeiosBlockTxs count mismatch: " ++ show (V.length txs, IntSet.size offsetsSet)
    let aligned :: IntMap.IntMap (LeiosTx, BS.ByteString)
        aligned = IntMap.fromList $ zip (IntSet.toAscList offsetsSet) (zip (V.toList txs) (V.toList txBytess))
    -- Cheap checks (count + total bytes, no hashing) for every covered job, so an
    -- over-send is caught and punished even for a job we have since completed.
    -- 'foldrWithKey' short-circuits on the first rejection.
    either invalidReply pure $
      NEIntMap.foldrWithKey
        (\i job acc -> checkJobSize aligned (Jobs.MkLeiosJobId i) job >> acc)
        (Right ())
        jobs
    -- Only jobs still pending in the jobPool are content-validated (root hash) and
    -- ingested; a redundant delivery of a completed job -- or a response for an EB
    -- pruned mid-flight -- is discarded without hashing. Read the jobPool once; the
    -- read-then-complete race is benign (completion is monotonic, and re-ingest is
    -- idempotent).
    outstanding0 <- MVar.readMVar outstandingVar
    let pendingJobs = case Map.lookup point.pointEbHash (Leios.ebState outstanding0) of
          Nothing ->
            IntMap.empty
          Just (Leios.MkEbState _slot _onset Leios.NoBody) ->
            IntMap.empty
          Just (Leios.MkEbState _slot _onset Leios.BodyImminent) ->
            IntMap.empty
          Just (Leios.MkEbState _slot _onset (Leios.BodyAcquired jobPool)) ->
            Jobs.restrictToPending (NEIntMap.toMap jobs) jobPool
        -- The covered jobs we won't ingest -- an earlier delivery already
        -- completed them (or the EB was pruned). Their txs did arrive, and being
        -- from a completed job they are already held, so account their (committed,
        -- 'checkJobSize'-verified) bytes as 'fetchArrivalExtra'. This mirrors how a
        -- concurrent duplicate delivery already lands in that bucket via the cache.
        redundantExtra =
          fetchArrivalExtra $
            IntMap.foldr
              (\(Jobs.MkLeiosJob _ bytes _) acc -> bytes + acc)
              0
              (IntMap.difference (NEIntMap.toMap jobs) pendingJobs)
    toIngest <-
      either invalidReply (pure . fold) $
        IntMap.traverseWithKey (\i job -> ingestJob aligned (Jobs.MkLeiosJobId i) job) pendingJobs
    -- ingest the validated txs (unapplied). 'txArrival' covers those; add the
    -- redundant arrivals the cache never saw, so the trace reflects everything
    -- that came off the wire.
    -- Retiring the jobs waits for the write: 'completeJob' drops them from the
    -- pool for every peer, so doing it on arrival leaves the closure permanently
    -- incomplete if the write never lands -- the LeiosDb is what emits the
    -- closure-acquired notification, and no later offer would be acted on.
    -- Until then they stay picked by this peer, which is already re-requestable
    -- by others and released wholesale on its disconnect.
    txArrival <- ingestAcquiredTxs now toIngest
    traceWith ktracer $ TraceLeiosFetchTxsArrival (txArrival <> redundantExtra)
    -- 'refundTxRequest' reverses this peer's per-request byte accounting (but skips
    -- it if the peer was already cancelled in bulk by a disconnect).
    adjustOutstanding (refundTxRequest peerId (fromIntegral batchBytes))
    void $ MVar.tryPutMVar readyVar ()
    traceWith tracer $ MkTraceLeiosPeer $ "[done] " ++ Leios.prettyLeiosBlockTxsRequest req
 where
  -- Shared ingest for both sources: write the txs to the LeiosDb (which owns the
  -- closure-acquired notification side-effect, and reports for the trace the EBs it
  -- newly completed), then to the tx-cache. A forge's txs are 'Applied'
  -- (known-valid, from a validated mempool); a peer's are 'Unapplied'. Returns the
  -- arrival-bytes tally -- 'mempty' on the applied path, which emits no
  -- fetch-arrival telemetry.
  --
  -- NB two peers delivering the same (redundantly-requested) job at once can both
  -- ingest it: the jobPool read and 'completeTxRequest' aren't atomic across
  -- threads. Harmless --- the DB insert is idempotent and the cache buckets each
  -- tx by its prior state in one locked pass, tolerating duplicates.
  --
  -- Everything source-specific -- applied vs unapplied, which fetch jobs to
  -- retire -- is read off 'source' rather than passed in: a peer's txs are
  -- unapplied and carry its picked jobs (retired by 'onDurable' /
  -- 'completeTxRequest'); forge and mempool txs are applied and hold no jobs of
  -- ours.
  ingestAcquiredTxs ::
    RelativeTime ->
    [(Int, TxHash, BS.ByteString)] ->
    m Leios.FetchArrivalBytes
  ingestAcquiredTxs now toIngest = do
    txsWritten <- writeTxs writer ingestPoint [(off, bs) | (off, _txh, bs) <- toIngest]
    let traceCompleted = do
          completed <- traceException tracer TraceLeiosPeerDbException $ await txsWritten
          onDurable
          -- These bytes are durable now: advertise them as fill sources for
          -- later EBs referencing the same txs.
          setTxLocations
            txCache
            ingestPoint.pointEbHash
            [(off, txh) | (off, txh, _bs) <- toIngest]
          ebStates <- Leios.ebState <$> MVar.readMVar outstandingVar
          forM_ completed $ \p ->
            traceWith ktracer $ TraceLeiosBlockTxsAcquired p (ebPointAge now ebStates p)
    case source of
      ForgedTxs{} -> traceCompleted -- synchronous
      -- Off the collector thread so the fetch pipeline keeps flowing; 'link'
      -- brings a failed write back to the node rather than letting it be taken as
      -- durable. We trust the write to land (or kill the node), so there is no
      -- loss to recover from here.
      --
      -- TODO: the worker's lifetime is unbounded -- 'link' propagates its
      -- exceptions but does not tie it to the peer, so on teardown it is
      -- orphaned and keeps touching shared state. Fork it in the peer's
      -- ResourceRegistry (or move to a writer-run completion callback) so it
      -- dies with the peer. Separate PR.
      _ -> link =<< async traceCompleted
    -- The cache update: the fetch logic consults it to decide what is still
    -- missing, so it cannot lag behind the caller.
    -- TODO: do this before the DB write (like in processLeiosBlock)
    case source of
      ReceivedTxsFrom{} ->
        withLockedInsertUnappliedTx txCache $ \w0 step ->
          foldM (\w (_off, txh, bs) -> step w txh (fromIntegral (BS.length bs)) ()) w0 toIngest
      _ -> do
        withLockedInsertAppliedTx txCache $ \w0 step ->
          foldM (\w (_off, txh, _bs) -> step w txh ()) w0 toIngest
        pure mempty

  onDurable = whenReceivedTxs $ \peerId req -> adjustOutstanding $ completeTxRequest peerId req

  whenReceivedTxs f = case source of
    ReceivedTxsFrom peerId req _ -> f peerId req
    _ -> pure ()

  adjustOutstanding f = MVar.modifyMVar_ outstandingVar (pure . f)

  -- The EB whose rows this call fills; every source names it.
  ingestPoint = case source of
    ForgedTxs point _ _ -> point
    MempoolTxs point _ -> point
    ReceivedTxsFrom _ (MkLeiosBlockTxsRequest point _) _ -> point

-----

-- | Record this peer's 'MsgLeiosBlockOffer': it can serve this endorser
-- block's body, at this size.
--
-- This does not list the body as one to fetch. The offer is already gated on
-- an announcement from this peer, so it says nothing about what exists that
-- the announcement did not; what we pursue is seeded by announcements and by
-- CertRB roll-forwards ('recordCertRbOffer').
recordEbBodyOffer ::
  IOLike m =>
  MVar m () ->
  LeiosPeerVars m ->
  -- | The offered EB: its point and on-the-wire body size.
  (LeiosPoint, BytesSize) ->
  m ()
recordEbBodyOffer readyVar peerVars (point, ebBytesSize) =
  recordOffer readyVar peerVars point $
    Leios.MkPeerOffer SNothing (SJust ebBytesSize) TxsClosureNotOffered

-- | Record this peer's 'MsgLeiosBlockTxsOffer': it can serve this endorser
-- block's tx closure.
--
-- Independent of the body offer, and carrying no size: closure jobs are per
-- endorser block and only assigned once we hold the body, so a bare point
-- names its closure unambiguously.
recordEbClosureOffer ::
  IOLike m => MVar m () -> LeiosPeerVars m -> LeiosPoint -> m ()
recordEbClosureOffer readyVar peerVars point =
  recordOffer readyVar peerVars point $
    Leios.MkPeerOffer SNothing SNothing TxsClosureOffered

-- | Record a CertRB roll-forward as an offer of both the body and the
-- closure.
--
-- Its point and size are read out of the announcing block's chain-dep state,
-- so the peer chose which block to roll forward but not what that block's
-- predecessor announced. It still only /offers/: the claim that this endorser
-- block is certified has not been verified yet, and an unverified claim must
-- not be able to make us track an endorser block. What we track comes from the
-- announcement that takes the election's focus, and from the focus moving once
-- the certificate is verified.
recordCertRbOffer ::
  IOLike m =>
  MVar m () ->
  LeiosPeerVars m ->
  -- | The offered EB: its point and on-the-wire body size.
  (LeiosPoint, BytesSize) ->
  -- | The maximum EB closure size, from the announcment's slot. See
  -- 'Leios.poMaxEbTxsSize'.
  BytesSize ->
  m ()
recordCertRbOffer readyVar peerVars (point, ebBytesSize) maxEbTxsSize =
  recordOffer readyVar peerVars point $
    Leios.MkPeerOffer (SJust maxEbTxsSize) (SJust ebBytesSize) TxsClosureOffered

recordOffer ::
  IOLike m => MVar m () -> LeiosPeerVars m -> LeiosPoint -> Leios.PeerOffer -> m ()
recordOffer readyVar peerVars point offer = do
  MVar.modifyMVar_ (Leios.offerings peerVars) $ \offers ->
    pure $! Map.insertWith (<>) point offer offers
  void $ MVar.tryPutMVar readyVar ()

-----

-- | The offer-side handling of a 'MsgRollForward': when the header is a CertRB
-- ('headerContainsLeiosCert'), record this peer as offering the EB it certifies
-- (via 'recordEbBodyOffer', offering both its body and tx-closure), reading
-- that EB from the predecessor's chain-dep
-- state ('chainDepStateLeiosAnnouncement'), which the CertRB's own transition
-- would overwrite. A no-op otherwise. The announcement-side handling of the same
-- header is separate; see the ChainSync client's 'leiosMsgRollForwardCallback'.
--
-- This execution of the node may never have processed the announcement this
-- offer is for. If the CertRB's predecessor has been on our selection since
-- before we started, then ChainSync intersects at or after it and its header
-- never rolls forward, so nothing announces it to us. No election is then
-- fetching that endorser block and the decision logic skips this offer --- but
-- the offer is still recorded. The claim this CertRB establishes names the
-- election, since the announcing block's 'VolatileDB.BlockInfo' carries it, so
-- verifying the certificate focuses that election on this announcement's EB and
-- now the decision logic can act on the offer.
checkMsgRollForwardForLeiosOffers ::
  forall blk pid m.
  (IOLike m, ResolveLeiosBlock blk) =>
  ( MVar m (LeiosOutstanding pid)
  , MVar m ()
  ) ->
  LeiosPeerVars m ->
  Header blk ->
  -- | The ledger view at this header's predecessor's slot
  LedgerView (BlockProtocol blk) ->
  ChainDepState (BlockProtocol blk) ->
  m ()
checkMsgRollForwardForLeiosOffers kernelVars peerVars hdr predLedgerView cds =
  when (headerContainsLeiosCert hdr) $
    forM_ (protocolStateLeiosAnnouncement @blk cds) $ \fields -> do
      noteCertificationClaim
        peerVars
        (announcementElection fields)
        (announcementEbHash fields)
      recordCertRbOffer
        (snd kernelVars)
        peerVars
        (announcementLeiosPoint fields, announcementEbBodySize fields)
        (getLeiosMaxEbTxsSizeFromView (Proxy @blk) predLedgerView)

-- | Count this peer's claim that an endorser block is certified for an
-- election, disconnecting if it contradicts a prior claim by this peer
--
-- One certified announcement per election is all an honest peer ever has
-- selected. Its roll-forwards follow its own selection, so a second certified
-- announcement would mean it followed a fork where a different endorser block
-- was certified for that election --- which takes two valid certificates to
-- exist at all, and that requires that /the committee/ equivocated, which can
-- only happen if the protocol itself is defeated.
--
-- This is deliberately stricter than the two announcements per election
-- 'LeiosDemoLogic.Announcements.extendLive' tolerates: that allowance exists so
-- equivocation proofs can spread, and nothing asks a peer to show us two
-- certificates.
--
-- Without this, rolling CertRBs forward is a door into this peer's 'offerings'
-- that the announcement cap does not guard: a pool can equivocate its own won
-- slots into arbitrarily many announcing blocks, put a cert-claiming header on
-- each --- the certificate is in the body, which we need never fetch --- and
-- roll them all forward, arbitrarily increasing the node's memory usage.
noteCertificationClaim :: IOLike m => LeiosPeerVars m -> ElId -> EbHash -> m ()
noteCertificationClaim peerVars elId ebHash =
  MVar.modifyMVar (Leios.certificationClaims peerVars) $ \claimed ->
    case Map.lookup elId claimed of
      Just alreadyClaimed
        | alreadyClaimed /= ebHash ->
            throwIO $ ExnLeiosTwoCertificationClaims elId alreadyClaimed ebHash
      _ -> pure (Map.insert elId ebHash claimed, ())

-- | Thrown when a peer's roll-forwards claim that two different endorser
-- blocks are certified for one election; the ensuing thread death disconnects
-- it. See 'noteCertificationClaim'.
data ExnLeiosTwoCertificationClaims = ExnLeiosTwoCertificationClaims !ElId !EbHash !EbHash
  deriving Show

instance Exception ExnLeiosTwoCertificationClaims

-----

-- The pure logic for handling an inbound 'MsgLeiosBlockAnnouncement'. The
-- effectful glue (reading the immutable tip, the 'PeerState' ref,
-- 'MVar' updates, tracing, and 'throwIO') lives in the NodeToNode client, which
-- invokes 'onAnnouncement' with these pieces.

-- | 'Header blk' as a relayed LeiosNotify announcement, paired with the
-- announcement data parsed from it (see 'mkAnnouncingHeader'). The 'Eq' instance
-- compares by header hash: that is the one identity used for announcement dedup
-- and equivocation counting (see 'onAnnouncement').
data AnnouncingHeader blk
  = -- | INVARIANT: 'ancHeader' includes an announcement whose fields are
    -- 'ancAnnouncementFields'
    UnsafeMkAnnouncingHeader
    { ancHeader :: !(Header blk)
    , ancAnnouncementFields :: !AnnouncementFields
    }

instance HasHeader (Header blk) => Eq (AnnouncingHeader blk) where
  a == b = headerHash (ancHeader a) == headerHash (ancHeader b)

-- | Interpret a header as a relayed EB announcement, or 'Nothing' if it carries
-- no announcement (so it should not have been relayed as one). Parsing the
-- announcement once here keeps it total for all later consumers (e.g. tracing).
mkAnnouncingHeader :: ResolveLeiosBlock blk => Header blk -> Maybe (AnnouncingHeader blk)
mkAnnouncingHeader h =
  headerLeiosAnnouncement h <&> \(MkLeiosPoint _ebSlot ebHash, ebBodySize) ->
    UnsafeMkAnnouncingHeader h (MkAnnouncementFields (headerElId h) ebHash ebBodySize)

-- | The other safe constructor of an 'AnnouncingHeader': for a header we already
-- know announces a specific EB because we forged it. Unlike 'mkAnnouncingHeader'
-- it is total -- no parse of the header's announcement is needed, since the
-- announcement fields come straight from the 'ForgedLeiosEb' whose EB the header
-- announces by construction.
mkForgedAnnouncingHeader ::
  ResolveLeiosBlock blk => Header blk -> Leios.ForgedLeiosEb -> AnnouncingHeader blk
mkForgedAnnouncingHeader h forgedEb =
  UnsafeMkAnnouncingHeader h $
    MkAnnouncementFields (headerElId h) forgedEb.point.pointEbHash (encodeLeiosEbSize forgedEb.body)

-- | The election of an 'AnnouncingHeader'.
ancElId :: AnnouncingHeader blk -> ElId
ancElId = announcementElection . ancAnnouncementFields

-- | The central-state handling shared by an incoming LeiosNotify
-- 'MsgLeiosBlockAnnouncement' and a ChainSync 'MsgRollForward' that announces an
-- EB: run 'Announcements.onAnnouncementCentral' (relay + dedup) and, for a
-- genuinely new announcement, record the EB as awaited ('recordAnnouncedEb') and
-- in the tx-cache ('recordAnnouncementInTxCache'). Central-only: no per-peer
-- state is touched.
processAnnouncementCentrally ::
  forall blk peer pid m.
  (IOLike m, ConvertRawHash blk, HasHeader (Header blk), Ord peer) =>
  Tracer m TraceLeiosKernel ->
  MVar m (Announcements.CentralState m peer (AnnouncingHeader blk)) ->
  (MVar m (LeiosOutstanding pid), MVar m ()) ->
  LeiosTxCache m () () SerializedEbBody ->
  LeiosDbWriter m ->
  Maybe peer ->
  AnnouncementSource ->
  ShouldRelay ->
  -- | This announcement slot's wall-clock onset, if known
  --
  -- Recorded so the body\/closure arrival handlers can report the EB's
  -- age. It's intentionally 'SNothing' for a self-forged EB, since these ages
  -- are relevant to /diffusion/.
  StrictMaybe RelativeTime ->
  Maybe NominalDiffTime ->
  AnnouncingHeader blk ->
  m ()
processAnnouncementCentrally
  kernelTracer
  centralVar
  kernelVars
  txCache
  writer
  source
  provenance
  shouldRelay
  onset
  age
  ancHdr =
    MVar.modifyMVar_ centralVar $ \cst ->
      Announcements.onAnnouncementCentral
        (contramap (traceNewAnnouncement provenance) kernelTracer)
        ancElId
        ( \_elSt -> do
            -- A received announcement lists the EB for fetching; one we forged is
            -- instead marked 'BodyImminent' in 'ebState' so the fetch logic never
            -- requests it -- even after a peer relays our own announcement back to
            -- us. Marking it here, at announcement time, closes the window before
            -- the body is persisted and before any such relay can arrive.
            case provenance of
              ForgedLocally -> markForged
              ReceivedViaChainSync -> recordAnnounced
              ReceivedViaLeiosNotify -> recordAnnounced
            recordAnnouncementInTxCache txCache announcerRbHash point
            -- If this new point's EB body and/or closure is already in the
            -- LeiosDb, it should also emit the corresponding events for this
            -- point. One crucial consequence is 'cdbAcquiredLeiosEbs' being
            -- informed that the EB won't be pruned until the new (often
            -- /younger/) point becomes immutable.
            void $ writeEbPoint writer point (announcementEbBodySize fields)
        )
        cst
        source
        shouldRelay
        age
        ancHdr
   where
    fields = ancAnnouncementFields ancHdr
    -- The announced EB's slot is the announcing header's own slot (see
    -- 'headerLeiosAnnouncement'); its ebHash is kept in 'ancAnnouncementFields'.
    point = MkLeiosPoint (blockSlot (ancHeader ancHdr)) (announcementEbHash fields)
    announcerRbHash = MkRbHash (toRawHash (Proxy @blk) (headerHash (ancHeader ancHdr)))
    recordAnnounced = recordAnnouncedEb kernelVars onset fields
    markForged =
      MVar.modifyMVar_ (fst kernelVars) $
        pure . Leios.markBodyImminent point.pointEbHash point.pointSlotNo

-- | Thrown when a peer misbehaves on the announcement protocol; the ensuing
-- thread death disconnects the peer. It carries the
-- 'ErrAnnouncement' verbatim (the @blk@ is existential); every
-- such error is a disconnect, since the only invalidities that used to be
-- tolerated — opcert issue numbers ahead of the immutable tip — are now
-- accepted outright by 'validateAnnouncementHeader'.
data ExnInvalidLeiosAnnouncement
  = forall blk.
    ReactToAnnouncementError (ErrAnnouncement (AnnouncementInvalidity blk))

deriving instance Show ExnInvalidLeiosAnnouncement

instance Exception ExnInvalidLeiosAnnouncement

-- | Thrown when a peer offers an endorser block over LeiosNotify that it never
-- announced, that it has already offered, that is bigger than any endorser
-- block may be, or that is too old to check; the ensuing thread death
-- disconnects it.
--
-- Without this requirement, peers could send bogus offers, and there are
-- infinitely many of those.
data ExnLeiosInvalidOffer
  = -- | A body offer of an endorser block this peer never announced, with the
    -- size it claimed.
    ExnLeiosBlockOfferWithoutAnnouncement !LeiosPoint !BytesSize
  | -- | A body offer claiming more bytes than any endorser block may have:
    -- the offered point, the size it claimed, and the bound it exceeded.
    ExnLeiosBlockOfferTooBig !LeiosPoint !BytesSize !BytesSize
  | -- | A closure offer for an endorser block this peer never announced.
    ExnLeiosClosureOfferWithoutAnnouncement !LeiosPoint
  | -- | A second offer of the body, or of the closure, this peer has already
    -- offered. The two are independent, so each may be offered once.
    ExnLeiosRepeatedOffer !LeiosPoint !OfferedBodyOrClosure
  | -- | An offer below the slot we have pruned this peer's announcements to,
    -- which is therefore unanswerable on its own terms: the offered point,
    -- and that slot. See 'leiosOfferRelayDecision' for why an honest peer
    -- does not reach it.
    ExnLeiosOfferTooOld !LeiosPoint !SlotNo
  deriving Show

instance Exception ExnLeiosInvalidOffer

-- | Thrown when a peer asks over LeiosFetch for something we do not have, or
-- asks for it in a way no honest peer would; the ensuing thread death
-- disconnects it.
--
-- An honest peer can still lose the connection here, by asking for something we
-- pruned between sending our offer and receiving their request. Nothing
-- prevents that, but it should be rare between healthy nodes on a healthy
-- connection (and especially so if the nearly-immutable Chain Growth was also
-- healthy).
--
-- Offered over LeiosNotify, the endorser block is at least 'LeiosMinOfferLead'
-- slots above our immutable tip, and is deleted no sooner than @cdbGcDelay@
-- after our immutable tip passes it: 60+1 minutes at the mainnet defaults. An
-- immutable tip can lurch --- while syncing, on escaping an eclipse, or over a
-- Chain-Growth gap replayed k blocks later --- which consumes a chunk of that
-- 60 minute buffer arbitrarily fast. @cdbGcDelay@ is wall clock, though, so the
-- peer keeps that last minute regardless.
--
-- Offered over ChainSync, by rolling a CertRB forward, there is no such lead:
-- only @cdbGcDelay@. However, it also takes two deep fork switches, one to roll
-- forward onto the CertRB near the frontier and another to roll back off it to
-- prevent it from becoming immutable (and hence always requestable).
--
-- The two do not compound under a ProtocolBurstAttack, where hours of suddenly
-- released endorser blocks queue ahead of ours and freshest-first leaves our
-- offer sitting for well over a minute. Withheld blocks are uncertified ---
-- certification needs a quorum of honest voters to have held the closure
-- /during the voting window/ --- and only a certified EB is offered (by an
-- honest server!) by rolling a CertRB forward. So that attack's EBs only reach
-- the path with the 61-minute margin, never the one with the 60-second
-- margin. That rests on @leiosQuorumStakeThreshold@ staying out of an
-- adversary's reach.
--
-- However, if a ProtocolBurstAttack consists of EBs /younger/ than one we just
-- offered (either via LeiosNotify or via ChainSync), then that might prevent
-- the honest downstream peer from sending a request in response to our offer
-- until "arbitrarily" later---it depends on how long it takes them to fetch the
-- ProtocolBurstAttack's EBs. If they finish acquiring the ProtocolBurstAttack
-- EBs just before we prune our EB/they prune their offers, then it's possible
-- this race condition will disconnect the two honest nodes. We're accepting
-- this risk for the MVP, since it seems quite difficult for the adversary to
-- arrange it: the ProtocolBurstAttack can't end too soon or too late---its
-- target moment does depend on some /known/ blocks' slots, but the actual
-- state/timings of the two nodes' connection is hard to predict.
--
-- TODO perhaps an analog of /MsgNoBlocks/ is worthwhile, only sent if the
-- request's slot is old enough for the EB to have been pruned out.
--
-- TODO check that we /offered/ what was asked for /to the peer that asked for
-- it/. We don't do that already because a) it requires some tedious
-- rearranging\/plumbing\/more complicated state to catch and b) it's /so far/,
-- at least, harmless to serve something we could have offered but didn't. If
-- the egress scheduler begins to rely on un-offered things being
-- un-requestable, then we'd have to fill this gap.
data ExnLeiosInvalidRequest
  = -- | An endorser block whose body we do not hold.
    ExnLeiosUnknownBlockRequested !LeiosPoint
  | -- | Offsets into an endorser block that it does not have, or whose
    -- transactions we do not hold.
    ExnLeiosUnknownTxsRequested !LeiosPoint ![Int]
  deriving Show

instance Exception ExnLeiosInvalidRequest

-- | Thrown when a peer's 'MsgLeiosBlock' hashes to the endorser block we asked
-- for and is still not one we can accept; the ensuing thread death disconnects
-- it.
--
-- Unlike the other invalid replies, the bytes are the endorser block we asked
-- for, so 'processLeiosBlock' settles what it owes the rest of the node before
-- throwing: the arrival is accounted for what it was, and 'ebState' records the
-- verdict so the next peer to serve the same bytes does not cost us the work
-- again. That is why neither of these goes through @invalidReply@, which writes
-- the whole body off as waste, and why both are thrown only once the
-- 'LeiosOutstanding' lock has been let go --- throwing under it would roll the
-- record back.
data ExnLeiosWellHashedBodyRejected
  = -- | Not the number of bytes it offered that endorser block at: the point,
    -- the size it offered, and the size it sent.
    ExnLeiosBlockWrongSize !LeiosPoint !BytesSize !BytesSize
  | -- | References more transaction bytes than the protocol allows the closure
    -- to amount to: the point, what it referenced, and the bound it broke.
    --
    -- No honest peer sends this. Such an endorser block can never be certified
    -- --- an honest committee member applies the same bound --- so an honest
    -- peer that fetched it rejected it here too and never stored or offered it.
    -- Only a peer that skipped the check can serve one.
    ExnLeiosClosureTooBig !LeiosPoint !Word64 !BytesSize
  deriving Show

instance Exception ExnLeiosWellHashedBodyRejected

-- | Which of a point's two independent offers a LeiosNotify message makes.
data OfferedBodyOrClosure = OfferedBody | OfferedClosure
  deriving (Eq, Ord, Show)

-- | Thrown when a peer relays a 'MsgLeiosBlockAnnouncement' whose header carries
-- no EB announcement (so 'mkAnnouncingHeader' returns 'Nothing'); the ensuing thread
-- death disconnects the peer.
data ExnLeiosBlockAnnouncementMissing = ExnLeiosBlockAnnouncementMissing
  deriving Show

instance Exception ExnLeiosBlockAnnouncementMissing

-- | Block until the immutable tip can forecast the ledger view to the current
-- slot
--
-- LeiosNotify replies are fresh, ie in slots near the wall clock, and
-- 'announcementValidity' judges them by forecasting from the immutable tip.
-- Until that tip reaches approximately /now/, a fresh reply would be
-- @PastHorizon@, and a reply we cannot judge is no grounds to punish the
-- peer. So we don't even send a request.
--
-- The current slot is read via the caller's 'CurrentSlot' action, which uses
-- the volatile tip; its horizon reaches further, so gating on it alone would
-- send requests too soon.
awaitImmTipCanForecastNow ::
  (IOLike m, LedgerSupportsProtocol blk) =>
  TopLevelConfig blk ->
  -- | the /immutable/ tip's ledger state
  StrictSTM.STM m (ExtLedgerState blk mk) ->
  StrictSTM.STM m CurrentSlot ->
  StrictSTM.STM m ()
awaitImmTipCanForecastNow cfg readImmutableLedger readCurrentSlot =
  readCurrentSlot >>= \case
    CurrentSlotUnknown -> StrictSTM.retry
    CurrentSlot slot -> do
      immLedger <- readImmutableLedger
      case runExcept
        ( forecastFor
            (ledgerViewForecastAt (configLedger cfg) (ledgerState immLedger))
            slot
        ) of
        Left _ -> StrictSTM.retry
        Right _ -> pure ()

-- | The @validate@ callback for 'onAnnouncement'.
--
-- First apply ChainSync's in-future check to the announced slot's wall-clock
-- onset (reusing the node's own 'InFutureCheck.SomeHeaderInFutureCheck'):
-- a far-future slot raises 'InFutureCheck.HeaderArrivalException' (disconnecting
-- the peer), a near-future slot blocks until the slot's onset (Ouroboros
-- Chronos) — blocking the per-peer handler is acceptable, as a (near-)future
-- announcement is the peer's fault.
--
-- If the announcement is valid and 'FreshOCIN', the verdict carries its data and
-- whether to relay it downstream (see 'ShouldRelay' and
-- 'maxAnnouncementAgeSend'). If it is valid but 'StaleOCIN' (its opcert counter
-- was revoked by /our/ immutable tip; see 'validateAnnouncementHeader'), the
-- verdict is 'VerdictIgnore', so that
-- 'onAnnouncement' accepts it from the peer without processing or
-- relaying it.
announcementValidity ::
  (IOLike m, LedgerSupportsProtocol blk, ResolveLeiosBlock blk) =>
  SystemTime m ->
  InFutureCheck.SomeHeaderInFutureCheck m blk ->
  TopLevelConfig blk ->
  -- | The immutable tip's ledger state, for the OCIN revocation check: a
  -- revocation only counts once it cannot be rolled back.
  ExtLedgerState blk EmptyMK ->
  Header blk ->
  m
    ( AnnouncementVerdict
        (AnnouncementInvalidity blk)
        (ShouldRelay, RelativeTime, NominalDiffTime, (LeiosPoint, BytesSize))
    )
announcementValidity systemTime futureCheck cfg immLedger hdr = do
  onset <- case futureCheck of
    InFutureCheck.SomeHeaderInFutureCheck hifc -> do
      arrival <- InFutureCheck.recordHeaderArrival hifc hdr
      judgment <-
        either throwIO pure $
          runExcept $
            InFutureCheck.judgeHeaderArrival
              hifc
              (configLedger cfg)
              (ledgerState immLedger)
              arrival
      arrivalResult <- InFutureCheck.handleHeaderArrival hifc judgment
      either throwIO pure (runExcept arrivalResult)
  -- The in-future check has delayed this thread until 'onset' if the
  -- slot was near-future, so 'now' is at or after 'onset' and the age
  -- is non-negative.
  now <- systemTimeCurrent systemTime
  let age = diffRelTime now onset
  pure $
    -- Only this function holds the wall clock, so it owns the too-old check.
    if age > maxAnnouncementAgeRecv
      then VerdictTooOld
      else
        let shouldRelay =
              if age <= maxAnnouncementAgeSend
                then DoRelay
                else DoNotRelay
         in case validateAnnouncementHeader cfg immLedger hdr of
              Left inv -> VerdictInvalid inv
              Right (StaleOCIN, _v) -> VerdictIgnore
              Right (FreshOCIN, v) -> VerdictProcess (shouldRelay, onset, age, v)

-- | Record a validated, newly-announced EB body as missing, unless its already
-- pruned\/tracked\/acquired
recordAnnouncedEb ::
  IOLike m =>
  ( MVar m (LeiosOutstanding pid)
  , MVar m ()
  ) ->
  -- | This announcement slot's wall-clock onset, if known.
  StrictMaybe RelativeTime ->
  AnnouncementFields ->
  m ()
recordAnnouncedEb (outstandingVar, readyVar) onset fields = do
  changed <- MVar.modifyMVar outstandingVar (pure . upd)
  when changed $ void $ MVar.tryPutMVar readyVar ()
 where
  -- The announced size is deliberately unused by fetching; nothing about
  -- fetching turns on it (see 'assignBody').
  MkAnnouncementFields elId ebHash _ebBytesSize = fields
  -- The announced EB's slot is its election's slot (see
  -- 'headerLeiosAnnouncement').
  MkElId ebSlot _poolId = elId

  -- The same in-lock guard as 'recordEbBodyOffer' (too old / already held). No
  -- cache lookup: 'ebState' is authoritative here.
  upd outstanding =
    let tooOld = ebSlot < Leios.outstandingPrunedSlot outstanding -- too old to fetch
    -- One entry per election: the announcement that takes the election's
    -- focus. A second announcement for an already-focused election is an
    -- equivocation, and tracking its endorser block too would let a pool
    -- double what its won slots cost us. If that one turns out to be the
    -- certified one, 'trackCertifiedEb' gives it an entry then.
        introducedFocus = not $ Map.member elId (Leios.elFocus outstanding)
        !outstanding'
          | tooOld || not introducedFocus = outstanding
          | otherwise =
              Leios.focusElectionIfUnfocused elId ebHash $
                Leios.recordMaxAnnouncementSlot ebHash ebSlot onset outstanding
        -- Whether this gives the fetch logic anything new to do, and so is
        -- worth waking it for.
        skip =
          tooOld
            || not introducedFocus
            || maybe False Leios.ebStateHasBody (Map.lookup ebHash (Leios.ebState outstanding)) -- already have it
     in (outstanding', not skip)

-- | What one LeiosNotify client remembers about its upstream peer.
--
-- The announcements and the offers are pruned together, by the announcement
-- handler, which is the only thing that ever adds an announcement --- and
-- offers are gated on announcements, so neither can grow between prunes.
data LeiosNotifyPeerState blk = MkLeiosNotifyPeerState
  { lnpsPruneSlot :: !SlotNo
  -- ^ The slot the announcements and the offers have been pruned up to.
  , lnpsAnnouncements :: !(PeerState (AnnouncingHeader blk))
  -- ^ What this peer has announced, for dedup and equivocation counting.
  , lnpsOffers :: !(Map LeiosPoint Leios.PeerOffer)
  -- ^ Whether this peer has offered the body, the closure, or both, for each
  -- point, so that repeating either costs it the connection.
  --
  -- Kept here rather than read back out of 'offerings', which the fetch logic
  -- evicts from as soon as it acts on an offer.
  }

emptyLeiosNotifyPeerState :: LeiosNotifyPeerState blk
emptyLeiosNotifyPeerState =
  MkLeiosNotifyPeerState
    { lnpsPruneSlot = SlotNo 0
    , lnpsAnnouncements = emptyPeerState
    , lnpsOffers = Map.empty
    }

pruneLeiosNotifyPeerStateToImmTip ::
  LedgerSupportsProtocol blk =>
  ExtLedgerState blk EmptyMK ->
  LeiosNotifyPeerState blk ->
  LeiosNotifyPeerState blk
pruneLeiosNotifyPeerStateToImmTip immLedger peerSt =
  case getTipSlot (ledgerState immLedger) of
    NotOrigin immTipSlot
      | lnpsPruneSlot peerSt < immTipSlot ->
          MkLeiosNotifyPeerState
            { lnpsPruneSlot = immTipSlot
            , lnpsAnnouncements = prunePeerState immTipSlot (lnpsAnnouncements peerSt)
            , lnpsOffers =
                -- 'LeiosPoint' orders slot-first, so the below-tip points are
                -- a prefix.
                snd $
                  Map.spanAntitone
                    (\(MkLeiosPoint slot _ebHash) -> slot < immTipSlot)
                    (lnpsOffers peerSt)
            }
    _ -> peerSt

-- | Accept a peer's 'MsgLeiosBlockOffer', or say why the peer must go.
--
-- A peer may offer only an endorser block it has itself announced. That is
-- what stops an offer from being an independent way to make us track an
-- endorser block, outside the two-per-election cap 'extendLive' puts on
-- announcements.
--
-- Only the point is held to the announcements; the size is free. An offer
-- claims to hold a body, and whoever holds a body knows its size, so a peer may
-- well state a size that no announcement we received claimed --- for one it
-- came by through the Recovery Path, say. Nothing downstream /trusts/ the size:
-- it only filters which offers are relevant to our acquisition of an EB of some
-- specific size (see 'assignBody').
--
-- Below the slot we have pruned this peer's announcements to there is nothing
-- left to check the offer against, and the offer is useless to us besides,
-- since the fetch logic prunes to the immutable tip too. See
-- 'leiosOfferRelayDecision' for why an honest peer won't send offers that are
-- older than our imm tip.
checkLeiosBlockOffer ::
  LeiosPoint ->
  BytesSize ->
  LeiosNotifyPeerState blk ->
  Either ExnLeiosInvalidOffer (LeiosNotifyPeerState blk)
checkLeiosBlockOffer point claimed peerSt
  | ebSlot < lnpsPruneSlot peerSt =
      Left $ ExnLeiosOfferTooOld point (lnpsPruneSlot peerSt)
  | SJust{} <- Leios.poOfferedBody seen = Left $ ExnLeiosRepeatedOffer point OfferedBody
  | claimed == 0 || claimed > Leios.maxLeiosEbBytesSize =
      -- An endorser block has at least one byte, and no endorser block may
      -- exceed this in any slot: that is the bound the guardrails script
      -- imposes, so the ledger parameter that actually applies can only be
      -- smaller. Since the offered size is what we go on to request (see
      -- 'assignBody'), this is the only thing bounding it.
      --
      -- TODO enforce that parameter instead of its ceiling. The announcement
      -- this offer rides on (see 'announcedIt') was validated against the
      -- ledger view of its own slot, so retaining that view's maximum endorser
      -- block size alongside the announcement would give the exact value here.
      Left $ ExnLeiosBlockOfferTooBig point claimed Leios.maxLeiosEbBytesSize
  | not (announcedIt point peerSt) =
      Left $ ExnLeiosBlockOfferWithoutAnnouncement point claimed
  | otherwise =
      Right
        peerSt
          { lnpsOffers =
              Map.insert
                point
                seen{Leios.poOfferedBody = SJust claimed}
                (lnpsOffers peerSt)
          }
 where
  ebSlot = pointSlotNo point
  seen = Map.findWithDefault mempty point (lnpsOffers peerSt)

-- | Whether this peer has announced this endorser block.
announcedIt :: LeiosPoint -> LeiosNotifyPeerState blk -> Bool
announcedIt point peerSt =
  any
    (\ancHdr -> announcementEbHash (ancAnnouncementFields ancHdr) == pointEbHash point)
    (announcementsInSlot (pointSlotNo point) (lnpsAnnouncements peerSt))

-- | Accept a peer's 'MsgLeiosBlockTxsOffer', or say why the peer must go.
--
-- Held to the same announcement as the body offer and otherwise independent of
-- it: either may arrive first, or alone. So a LeiosNotify server never has to
-- track what it has already offered a peer in order to synthesise a body offer
-- ahead of a closure offer.
checkLeiosClosureOffer ::
  LeiosPoint ->
  LeiosNotifyPeerState blk ->
  Either ExnLeiosInvalidOffer (LeiosNotifyPeerState blk)
checkLeiosClosureOffer point peerSt
  | pointSlotNo point < lnpsPruneSlot peerSt =
      Left $ ExnLeiosOfferTooOld point (lnpsPruneSlot peerSt)
  | TxsClosureOffered <- Leios.poClosure seen =
      Left $ ExnLeiosRepeatedOffer point OfferedClosure
  | not (announcedIt point peerSt) = Left $ ExnLeiosClosureOfferWithoutAnnouncement point
  | otherwise =
      Right
        peerSt
          { lnpsOffers =
              Map.insert
                point
                seen{Leios.poClosure = TxsClosureOffered}
                (lnpsOffers peerSt)
          }
 where
  seen = Map.findWithDefault mempty point (lnpsOffers peerSt)

-- | The just-counted announcement's fields, and whether it equivocates a prior
-- header announcing the same election.
announcementTraceFields ::
  ElState (AnnouncingHeader blk) ->
  (AnnouncementEquivocation, AnnouncementFields)
announcementTraceFields = \case
  OneAnnouncement a ->
    (NoEquivocation, ancAnnouncementFields a)
  TwoAnnouncements _a1 a2 ->
    (Equivocation, ancAnnouncementFields a2)

-- | Render an 'Announcements' per-peer announcement event as a 'TraceLeiosPeer'.
tracePeerAnnouncement ::
  TraceLeiosNotifyPeerEvent (AnnouncingHeader blk) ->
  TraceLeiosPeer
tracePeerAnnouncement (TracePeerAnnouncement elSt) =
  let (equivocation, fields) = announcementTraceFields elSt
   in TraceLeiosPeerAnnouncement equivocation fields

-- | Render an 'Announcements' node-wide announcement event as a
-- 'TraceLeiosKernel'. The 'AnnouncementSource' is supplied by the caller (only
-- it knows which path delivered the announcement); the event's own @mbPeer@
-- cannot distinguish LeiosNotify from ChainSync, as both carry a peer.
traceNewAnnouncement ::
  AnnouncementSource ->
  TraceLeiosNotifyEvent peer (AnnouncingHeader blk) ->
  TraceLeiosKernel
traceNewAnnouncement source (TraceNewAnnouncement _mbPeer _elId elSt age) =
  let (equivocation, fields) = announcementTraceFields elSt
   in TraceLeiosAnnouncementAccepted source equivocation fields age

-- | Do not relay (to downstream peers) an announcement whose slot's wall-clock
-- onset is older than this. See 'ShouldRelay'.
--
-- Must be comfortably less than 'maxAnnouncementAgeRecv', so that an
-- announcement an honest node relays just before this bound still arrives at
-- the downstream peer within that peer's larger receive bound, even after
-- transmission time and clock skew.
--
-- TODO magic number; should be a config/RunNode option
maxAnnouncementAgeSend :: NominalDiffTime
maxAnnouncementAgeSend = 300 -- 5 minutes

-- | Disconnect an upstream peer that relays an announcement whose slot's
-- wall-clock onset is older than this. See 'ErrTooOld'.
--
-- Comfortably greater than 'maxAnnouncementAgeSend', so that an honest peer
-- (which stops relaying at that smaller bound) is never disconnected on account
-- of transmission time or clock skew.
--
-- TODO magic number; should be a config/RunNode option... or even a protocol
-- parameter?
maxAnnouncementAgeRecv :: NominalDiffTime
maxAnnouncementAgeRecv = 600 -- 10 minutes

-- | Whether an endorser block of this slot is fresh enough to offer to our
-- peers: offer it only while its slot is this many slots younger than our own
-- immutable tip.
--
-- The LeiosNotify server consults this for each notification it is about to
-- turn into an offer. Reads the immutable tip, which moves, so the same slot
-- can answer differently over time.
--
-- The receiving peer disconnects us for offering an endorser block below /its/
-- immutable tip, and each side compares the offered slot against its own
-- immutable tip, so no clock enters into it. That leaves a whole
-- 'leiosMinOfferLead' of room: we only lose a peer once its immutable tip is
-- that far ahead of ours. Keeping back what our own tip has nearly reached is
-- what bounds the wavefront of an endorser block an adversary withheld and then
-- released at just the most dangerous moment that would cost honest relayers
-- the most connections.
--
-- The Recovery Path is the backstop, which allows a nodes to (eventually) offer
-- these otherwise-unofferable EBs (which were likely withheld by an adversarial
-- issuer, if they're still diffusing when /almost/ as old as the imm tip).
leiosOfferRelayDecision ::
  IOLike m =>
  LeiosMinOfferLead ->
  -- | the /immutable/ tip's slot
  StrictSTM.STM m (WithOrigin SlotNo) ->
  -- | the offered endorser block's slot
  SlotNo ->
  StrictSTM.STM m ShouldRelay
leiosOfferRelayDecision (MkLeiosMinOfferLead minLead) readImmTipSlot slot = do
  immTipSlot <- withOrigin (SlotNo 0) id <$> readImmTipSlot
  pure $ if unSlotNo slot < unSlotNo immTipSlot + minLead then DoNotRelay else DoRelay

-- | The threshold for 'leiosOfferRelayDecision'
newtype LeiosMinOfferLead
  = MkLeiosMinOfferLead {unLeiosMinOfferLead :: Word64}
  deriving newtype (Enum, Eq, Ord, Show)

-- | The default 'LeiosMinOfferLead': an hour of one-second slots.
--
-- Not a protocol parameter --- the two nodes need not agree on it --- so a
-- node may configure its own. Too small and a peer whose immutable tip is
-- merely a little ahead of ours disconnects us, so the risk of choosing one
-- too small falls on the node that chose it.
--
-- Too large and the node relays nothing at all, and this bound is the one to
-- watch: every endorser block worth relaying sits between the immutable tip
-- and the wall clock, so a lead approaching that distance suppresses the lot.
-- An hour leaves ample room on a network whose immutable tip trails by @k@
-- blocks of a chain that grows every twenty seconds, and none whatsoever on a
-- short-horizon test network --- which is why the test networks configure
-- their own.
defaultLeiosMinOfferLead :: LeiosMinOfferLead
defaultLeiosMinOfferLead = MkLeiosMinOfferLead 3600

-----

-- | The forge's counterpart to receiving an EB from an upstream peer: hand our
-- own freshly-forged EB to the same three handlers a remote acquisition uses---
-- announcement ('processAnnouncementCentrally', as 'ForgedLocally'), body
-- ('processLeiosBlock'), then closure ('processLeiosBlockTxs')---with no peer.
-- Keeping this similarity explicit is what makes forging an EB reconcile the
-- outstanding fetch state exactly as receiving one does.
--
-- WARNING: the @Forge@ command interpreter in "Test.LeiosDemoLogic.Invariants"
-- hand-replicates only this function's side-effects that alter the
-- 'LeiosOutstanding' state. If you change here, keep it in sync there.
onForgedLeiosEb ::
  ( IOLike m
  , ConvertRawHash blk
  , HasHeader (Header blk)
  , Ord pid
  ) =>
  Tracer m TraceLeiosKernel ->
  MVar m (Announcements.CentralState m pid (AnnouncingHeader blk)) ->
  ( MVar m (LeiosOutstanding pid)
  , MVar m ()
  ) ->
  LeiosTxCache m () () SerializedEbBody ->
  LeiosDbWriter m ->
  -- | Threaded through to the body/closure handlers for age reporting
  SystemTime m ->
  -- | Built by the caller (see 'mkForgedAnnouncingHeader'), at the call site
  -- nearest the forge where its correspondence to the closure is evident.
  AnnouncingHeader blk ->
  Leios.ForgedLeiosEb ->
  m ()
onForgedLeiosEb kernelTracer centralVar kv txCache writer systemTime anc forgedEb = do
  processAnnouncementCentrally
    kernelTracer
    centralVar
    kv
    txCache
    writer
    Nothing
    ForgedLocally
    Announcements.DoRelay
    SNothing -- a self-forged EB records no onset (kept out of the /diffusion/ events)
    Nothing
    anc
  processLeiosBlock
    kernelTracer
    nullTracer
    kv
    txCache
    writer
    systemTime
    noMempoolPull -- the forge holds the whole closure
    (ForgedBlock forgedEb.point)
    forgedEb.body
  processLeiosBlockTxs
    kernelTracer
    nullTracer
    kv
    txCache
    writer
    systemTime
    (ForgedTxs forgedEb.point forgedEb.body $ V.fromList $ map (MkLeiosTx . snd) $ forgedEb.txClosure)
  traceWith kernelTracer $
    TraceLeiosBlockStored{slot = forgedEb.point.pointSlotNo, eb = forgedEb.body}
