{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module LeiosDemoLogic (module LeiosDemoLogic) where

import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadMVar (MVar)
import qualified Control.Concurrent.Class.MonadMVar as MVar
import qualified Control.Concurrent.Class.MonadSTM as LazySTM
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
import qualified Data.IntSet as IntSet
import qualified Data.IntMap.NonEmpty as NEIntMap
import Data.IntSet.NonEmpty (NEIntSet)
import qualified Data.IntSet.NonEmpty as NEIntSet
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
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
  ( LeiosDbConnection
  , leiosDbBatchRetrieveTxs
  , leiosDbInsertEbBody
  , leiosDbInsertEbPoint
  , leiosDbInsertTxs
  , leiosDbLookupEbBody
  )
import LeiosDemoLogic.Announcements
  ( AnnouncementVerdict (..)
  , ElState (..)
  , ErrAnnouncement
  , PeerState
  , ShouldRelay (..)
  , TraceLeiosNotifyEvent (..)
  , TraceLeiosNotifyPeerEvent (..)
  , prunePeerState
  )
import qualified LeiosDemoLogic.Announcements as Announcements
import LeiosDemoLogic.Announcements.ElBimap (ElId)
import LeiosDemoLogic.Announcements.Validate
  ( AnnouncementInvalidity
  , validateAnnouncementHeader
  )
import qualified LeiosDemoOnlyTestFetch as LF
import LeiosDemoTypes
  ( AnnouncementEquivocation (..)
  , AnnouncementFields (..)
  , AnnouncementSource (..)
  , AlsoOfferedTxsClosure (..)
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
  , hashLeiosEb
  , hashLeiosTx
  , fetchArrivalEvicted
  , fetchArrivalExtra
  , fetchArrivalGood
  , fetchArrivalInvalid
  , leiosEbBytesSize
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
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( SystemTime
  , diffRelTime
  , systemTimeCurrent
  )
import Ouroboros.Consensus.Config (TopLevelConfig, configLedger)
import Ouroboros.Consensus.Ledger.Abstract (getTipSlot)
import Ouroboros.Consensus.Ledger.Basics (EmptyMK)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState, ledgerState)
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import Ouroboros.Consensus.Protocol.Abstract (ChainDepState)
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( OCINStaleness (..)
  , ResolveLeiosBlock (..)
  )
import Ouroboros.Consensus.Util.IOLike (IOLike)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
  ( IsBigLedgerPeer (..)
  )

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
  forall blk m.
  (ConvertRawHash blk, HasHeader (Header blk), IOLike m) =>
  LeiosTxCache m () () SerializedEbBody ->
  AnnouncingHeader blk ->
  LeiosPoint ->
  m ()
recordAnnouncementInTxCache txCache ancHdr point =
  void $ txCache.insertAnnouncement point.pointSlotNo rbh point.pointEbHash
 where
  rbh = MkRbHash (toRawHash (Proxy @blk) (headerHash (ancHeader ancHdr)))

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
      fmap (fmap @Maybe (\(x, ()) -> x))
    $ insertBody txCache point.pointEbHash (Leios.serializeEbBody eb) () (\() _ _ _ -> ())
  forM_ mbSummary $ traceWith tracer . TraceLeiosTxCacheEbBody point
  withLockedInsertAppliedTx txCache $ \w0 step ->
    foldM (\w (txh, _sz) -> step w txh ()) w0 (leiosEbTxs eb)
 where
  point = forgedEb.point
  eb = forgedEb.body

-----

data SomeLeiosFetchContext m
  = MkSomeLeiosFetchContext !(LeiosFetchContext m)

data LeiosFetchContext m = MkLeiosFetchContext
  { leiosDbConn :: !(LeiosDbConnection m)
  , leiosEbBuffer :: !(MV.MVector (PrimState m) (TxHash, BytesSize))
  , leiosEbTxsBuffer :: !(MV.MVector (PrimState m) LeiosTx)
  }

-- | Build a per-instance fetch context around an already-opened DB connection.
--
-- The connection is owned by the caller: SQLite connections must not be
-- shared across threads, and each LeiosFetch client/server instance runs on
-- its own thread, so the caller is expected to bracket a fresh 'open' /
-- 'close' pair for the lifetime of that instance (see 'withLeiosDb').
newLeiosFetchContext ::
  PrimMonad m =>
  LeiosDbConnection m ->
  m (LeiosFetchContext m)
newLeiosFetchContext leiosDbConn = do
  leiosEbBuffer <- MV.new maxTxsPerEb
  leiosEbTxsBuffer <- MV.new maxTxsPerEb
  pure
    MkLeiosFetchContext{leiosDbConn, leiosEbBuffer, leiosEbTxsBuffer}

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
msgLeiosBlockRequest tracer leiosContext MkLeiosPoint{pointEbHash} = do
  let MkLeiosFetchContext{leiosDbConn, leiosEbBuffer = buf} = leiosContext
  n <- traceException tracer TraceLeiosPeerDbException $ do
    -- get the EB items using new db
    items <- leiosDbLookupEbBody leiosDbConn pointEbHash
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
  let MkLeiosFetchContext{leiosDbConn, leiosEbTxsBuffer = buf} = leiosContext
  do
    let idxs = map fst bitmaps
    let idxLimit = maxTxsPerEb `div` 64
    when (any (== 0) $ map snd bitmaps) $ do
      error "A bitmap is zero"
    when (flip any idxs (> fromIntegral idxLimit)) $ do
      error $ "An offset exceeds the theoretical limit " <> show idxLimit
    when (not $ and $ zipWith (<) idxs (drop 1 idxs)) $ do
      error "Offsets not strictly ascending"
  let txOffsets = bitmapOffsets bitmaps
  n <- do
    -- Use new db to batch retrieve transactions
    results <- leiosDbBatchRetrieveTxs leiosDbConn point.pointEbHash txOffsets
    -- Process results and write to buffer
    -- REVIEW: why a mutable vector?
    let loop !i [] = pure i
        loop !i ((offset, _txHash, mbTxBytes) : rest) = do
          case mbTxBytes of
            Nothing -> error $ "Missing txBytes for offset " ++ show offset
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
-- TODO also pull txs from the Mempool
leiosFetchLogicIteration ::
  forall pid.
  Ord pid =>
  LeiosFetchStaticEnv ->
  -- | The current slot, or 'Nothing' when it is not yet known (i.e. we are
  -- syncing), in which case we fetch freshest-last instead of freshest-first.
  Maybe SlotNo ->
  Map (PeerId pid) (Map LeiosPoint AlsoOfferedTxsClosure) ->
  -- | Which peers are big-ledger peers (a peer absent from this map is treated as
  -- 'IsNotBigLedgerPeer').
  Map (PeerId pid) IsBigLedgerPeer ->
  LeiosOutstanding pid ->
  -- | The new outstanding state, the requests to send, and the offers to prune
  ( LeiosOutstanding pid
  , Map (PeerId pid) (NESeq LeiosFetchRequest)
  , Map (PeerId pid) (NESet.NESet LeiosPoint)
  )
leiosFetchLogicIteration env mbCurrentSlot offerings bigLedgerPeers = \acc0 ->
  -- One pass per peer. Bodies and tx-closure jobs compete on equal footing,
  -- ranked by each EB's slot in 'ebState' (its greatest announcement slot), so
  -- the freshest EBs are fetched first regardless of which half they still need.
  -- Each peer's 'assignPeer' yields only its own requests and dead offers; fold
  -- those into the per-peer maps here.
  Map.foldlWithKey'
    ( \(acc, reqs, drops) peerId offers ->
        let isBig = Map.findWithDefault IsNotBigLedgerPeer peerId bigLedgerPeers
            (acc', peerReqs, peerDrops) = assignPeer env mbCurrentSlot isBig peerId offers acc
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
peerBudget :: Ord pid => LeiosFetchStaticEnv -> IsBigLedgerPeer -> LeiosOutstanding pid -> PeerId pid -> Int
peerBudget env isBig acc peerId =
  fromIntegral cap
    - fromIntegral (Map.findWithDefault 0 peerId (Leios.requestedBytesSizePerPeer acc))
 where
  cap = case isBig of
    IsBigLedgerPeer -> Leios.maxRequestedBytesSizePerBigLedgerPeer env
    IsNotBigLedgerPeer -> Leios.maxRequestedBytesSizePerPeer env

-- | Walk this peer's offered points freshest-first (freshest-last while
-- syncing), assigning requests to the peer until it's saturated at
-- 'Leios.maxRequestedBytesSizePerPeer'. A big-ledger peer saturates at the larger
-- 'Leios.maxRequestedBytesSizePerBigLedgerPeer', enough that a closure it offers
-- is requested in full (see 'assignClosure').
--
-- Offered points below the saturation point are never visited, so aren't
-- pruned this pass; that's fine because it's ephemeral and/or the other prune
-- based on the imm-tip advancing is a backstop.
assignPeer ::
  Ord pid =>
  LeiosFetchStaticEnv ->
  Maybe SlotNo ->
  IsBigLedgerPeer ->
  PeerId pid ->
  Map LeiosPoint AlsoOfferedTxsClosure ->
  LeiosOutstanding pid ->
  (LeiosOutstanding pid, Seq LeiosFetchRequest, Set LeiosPoint)
assignPeer env mbCurrentSlot isBig peerId offers acc =
  go (acc, Seq.empty, Set.empty) prioritized
 where
  prioritized = case mbCurrentSlot of
    Nothing -> Map.toAscList offers -- syncing: freshest-last
    Just _currentSlot -> Map.toDescList offers -- freshest-first

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
      Just (Leios.MkEbState slot fetchState) -> case (fetchState, offerKind) of
        (Leios.BodyImminent, _) ->
          -- Our forge is producing this EB, so we hold the whole datum (even
          -- though it might not be inserted yet): never request it, and the
          -- peer's offer is dead.
          pruneThisOffer
        (Leios.NoBody, TxsClosureNotAlsoOffered) ->
          -- Body-only offer: request the body. If that's all that was
          -- offered, prune it.
          let (acc2, dec2) = assignBody peerId ebHash slot (acc1, dec1)
           in (acc2, dec2, Set.insert point drops)
        (Leios.NoBody, TxsClosureAlsoOffered) ->
          -- Request the body now, but keep the offer: we will request the
          -- closure from this peer once we hold the body.
          let (acc2, dec2) = assignBody peerId ebHash slot (acc1, dec1)
           in (acc2, dec2, drops)
        (Leios.BodyAcquired _jobPool, TxsClosureNotAlsoOffered) ->
          -- We hold the body and the peer never offered the closure, so it
          -- can no longer help.
          pruneThisOffer
        (Leios.BodyAcquired jobPool, TxsClosureAlsoOffered)
          | Jobs.nullLeiosJobPool jobPool ->
             -- whole datum in hand: the closure offer is useless now too
             pruneThisOffer
          | otherwise ->
              -- Still need the txs, and the peer offered the closure. If we
              -- just now assign all remaining jobs to the peer, prune its
              -- offer.
              let ((acc2, dec2), MkWhetherPeerEbExhausted exhausted) =
                    assignClosure env isBig peerId ebHash (acc1, dec1)
               in (acc2, dec2, if not exhausted then drops else Set.insert point drops)
   where
    ebHash = point.pointEbHash

    pruneThisOffer = (acc1, dec1, Set.insert point drops)

-- | Request the EB body from this peer
assignBody ::
  Ord pid =>
  PeerId pid -> EbHash ->
  SlotNo ->
  (LeiosOutstanding pid, Seq LeiosFetchRequest) ->
  (LeiosOutstanding pid, Seq LeiosFetchRequest)
assignBody peerId ebHash slot st@(acc, dec)
  | peerId `Set.member` Map.findWithDefault Set.empty ebHash (Leios.requestedEbPeers acc) =
      -- unless we've already requested it from them
      st
  | otherwise =
      case bodySize acc ebHash of
        Nothing ->
          -- another ephemeral case where 'ebState' has been pruned before the
          -- offers have
          st
        Just size ->
          let acc' =
                acc
                  { Leios.requestedEbPeers =
                      Map.insertWith Set.union ebHash (Set.singleton peerId) (Leios.requestedEbPeers acc)
                  , Leios.requestedBytesSizePerPeer =
                      Map.insertWith (+) peerId size (Leios.requestedBytesSizePerPeer acc)
                  }
           in (acc', dec Seq.|> LeiosBlockRequest (MkLeiosBlockRequest (MkLeiosPoint slot ebHash) size))

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
    Just (Leios.MkEbState _slot Leios.NoBody) -> (st, MkWhetherPeerEbExhausted False)
    Just (Leios.MkEbState _slot Leios.BodyImminent) -> (st, MkWhetherPeerEbExhausted False)
    Just (Leios.MkEbState slot (Leios.BodyAcquired jobPool)) ->
      let inflightJobs =
            maybe IntSet.empty NEIntSet.toSet $
              Map.lookup ebHash =<< Map.lookup peerId (Leios.requestedJobsPerPeer acc)
          -- A big-ledger peer gets a larger budget ('peerBudget'), enough for multiple
          -- full EB closures at once, but still bounded.
          --
          -- There are no more than 184 jobs per EB, so picked can't be a /long/ list.
          (picked, jobPool', exhausted) = pickJobs inflightJobs jobPool (peerBudget env isBig acc peerId)
       in flip (,) exhausted $ case nonEmpty picked of
            Nothing -> st
            Just nePicked ->
              let acc' =
                    acc
                      { Leios.ebState =
                          Map.insert
                            ebHash
                            (Leios.MkEbState slot (Leios.BodyAcquired jobPool'))
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
                      }
                  reqs = batchTxsRequests env (MkLeiosPoint slot ebHash) nePicked
               in (acc', dec <> Seq.fromList reqs)

-- | The announced body size of an EB we are still missing. All points of a hash
-- share the size, so any one still listed in 'missingEbBodies' serves.
bodySize :: LeiosOutstanding pid -> EbHash -> Maybe BytesSize
bodySize acc ebHash = do
  slots <- Map.lookup ebHash (Leios.reverseSlotIndexByEbHash acc)
  Map.lookup (MkLeiosPoint (NESet.findMin slots) ebHash) (Leios.missingEbBodies acc)

-- | Take least-requested-available jobs until the budget is spent or
-- there are no more jobs that aren't already assigned to this peer. Also
-- returns true, in the latter case. Each pick carries the whole 'Jobs.LeiosJob'
-- (id + commitment) so the request can validate its own response.
pickJobs ::
  IntSet.IntSet ->
  Jobs.LeiosJobPool ->
  Int ->
  ([(Jobs.LeiosJobId, Jobs.LeiosJob)], Jobs.LeiosJobPool, WhetherPeerEbExhausted)
pickJobs inflightJobs0 jobPool0 budget0 =
  go inflightJobs0 jobPool0 budget0 []
 where
  go inflightJobs jobPool budget acc
    | budget <= 0 = (reverse acc, jobPool, MkWhetherPeerEbExhausted False)
    | otherwise = case Jobs.pickLeastRequestedJobExcept inflightJobs jobPool of
        Nothing -> (reverse acc, jobPool, MkWhetherPeerEbExhausted True)
        Just (jid@(Jobs.MkLeiosJobId i), job@(Jobs.MkLeiosJob _offsets bytes _root), jobPool') ->
          go
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
  LeiosFetchStaticEnv -> LeiosPoint -> NonEmpty (Jobs.LeiosJobId, Jobs.LeiosJob) -> [LeiosFetchRequest]
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

-- | A response received by the pipelined-peer collector thread, deferred
-- for processing on the main peer thread. The collector must not touch
-- the 'LeiosDbConnection' — it belongs to the main peer thread.
data PendingResponse
  = PendingBlockResponse !LeiosBlockRequest !LeiosEb
  | PendingBlockTxsResponse !LeiosBlockTxsRequest !(V.Vector LeiosTx)

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
  LeiosDbConnection m ->
  -- | Pull EB-body misses out of the local mempool; see 'processLeiosBlock'.
  ( IntMap.IntMap (TxHash, BytesSize) ->
    m (IntMap.IntMap (TxHash, BytesSize), Map TxHash BS.ByteString)
  ) ->
  PeerId pid ->
  StrictTVar m (Seq LeiosFetchRequest) ->
  -- | Queue of responses received by the pipelined collector thread.
  -- The collector enqueues; this function (on the main peer thread)
  -- drains and processes, keeping all 'LeiosDbConnection' access on
  -- the main thread.
  LazySTM.TQueue m PendingResponse ->
  m
    ( Either
        (m (Either () (LF.SomeLeiosFetchJob LeiosPoint LeiosEb LeiosTx m)))
        (Either () (LF.SomeLeiosFetchJob LeiosPoint LeiosEb LeiosTx m))
    )
nextLeiosFetchClientCommand ktracer tracer stopSTM kernelVars txCache db pullFromMempool peerId reqsVar responseQ = do
  drainResponses
  StrictSTM.atomically checkOrPeek >>= \case
    Right result -> pure $ Right result
    Left () -> pure $ Left blockingLoop
 where
  -- Drain everything currently in the response queue and process on this thread.
  drainResponses :: m ()
  drainResponses = do
    pending <- StrictSTM.atomically $ LazySTM.flushTQueue responseQ
    forM_ pending $ \case
      PendingBlockResponse req eb ->
        processLeiosBlock ktracer tracer kernelVars txCache db pullFromMempool (ReceivedBlockFrom peerId req) eb
      PendingBlockTxsResponse req txs ->
        processLeiosBlockTxs ktracer tracer kernelVars txCache db (ReceivedTxsFrom peerId req txs)

  -- Non-blocking: return 'Right result' if stop or a request is available,
  -- or 'Left ()' if we'd have to block (caller returns Left blockingLoop).
  checkOrPeek ::
    StrictSTM.STM m (Either () (Either () (LF.SomeLeiosFetchJob LeiosPoint LeiosEb LeiosTx m)))
  checkOrPeek =
    stopSTM >>= \case
      True -> pure $ Right $ Left ()
      False ->
        StrictSTM.readTVar reqsVar >>= \case
          req Seq.:<| reqs -> do
            StrictSTM.writeTVar reqsVar reqs
            pure $ Right $ Right $ g req
          Seq.Empty -> pure $ Left ()

  -- Blocking path. Wake on stop, new request, or a response arriving
  -- (peek doesn't consume; caller re-drains). Ensures responses get
  -- processed even when there are no requests to send.
  blockingLoop :: m (Either () (LF.SomeLeiosFetchJob LeiosPoint LeiosEb LeiosTx m))
  blockingLoop = do
    step <-
      StrictSTM.atomically $
        (Right <$> awaitStopOrRequest)
          `LazySTM.orElse` (Left () <$ LazySTM.peekTQueue responseQ)
    case step of
      Right result -> pure result
      Left () -> drainResponses *> blockingLoop

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

  g = \case
    LeiosBlockRequest req@(MkLeiosBlockRequest p _ebBytesSize) ->
      LF.MkSomeLeiosFetchJob
        (LF.MsgLeiosBlockRequest p)
        ( pure $ \(LF.MsgLeiosBlock eb) ->
            StrictSTM.atomically $
              LazySTM.writeTQueue responseQ (PendingBlockResponse req eb)
        )
    LeiosBlockTxsRequest req@(MkLeiosBlockTxsRequest p jobs) ->
      -- The wire request is just the point + bitmap; the bitmap is the union of
      -- the covered jobs' offsets (the jobs and their commitments stay local).
      let bitmaps = offsetsToBitmap (foldMap (\(Jobs.MkLeiosJob offs _ _) -> offs) jobs)
       in LF.MkSomeLeiosFetchJob
            (LF.MsgLeiosBlockTxsRequest p bitmaps)
            ( pure $ \(LF.MsgLeiosBlockTxs _ _ txs) ->
                StrictSTM.atomically $
                  LazySTM.writeTQueue responseQ (PendingBlockTxsResponse req txs)
            )

-----

-- | Where an EB body being ingested came from. 'processLeiosBlock' and
-- 'processLeiosBlockTxs' serve both a fetch response from a peer (carrying the
-- request we are fulfilling) and our own forge; the arrival-specific behaviour a
-- local forge skips is: refunding the peer's request budget, classifying/listing
-- the missing txs (a forge holds its whole closure, so nothing is missing), and
-- emitting fetch-arrival telemetry (which would otherwise pollute the arrival
-- panels with self-produced data).
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
    MempoolTxs !LeiosPoint !(Map TxHash BS.ByteString)

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
  LeiosDbConnection m ->
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
processLeiosBlock ktracer tracer (outstandingVar, readyVar) txCache db pullFromMempool source eb = do
  -- validate it
  let (mbPeer, point, ebBytesSize) = case source of
        ReceivedBlockFrom peerId (MkLeiosBlockRequest p sz) -> (Just peerId, p, sz)
        ForgedBlock p -> (Nothing, p, leiosEbBytesSize eb)
  traceWith tracer $ MkTraceLeiosPeer $ "[start] MsgLeiosBlock " <> Leios.prettyLeiosPoint point
  let MkLeiosPoint _ebSlot ebHash = point
  let ebBytesSize' = leiosEbBytesSize eb
  -- A failed-validation body: attribute the whole body to 'fabInvalid'.
  let invalidReply reason =
        traceWith ktracer (TraceLeiosFetchBodyArrival (fetchArrivalInvalid ebBytesSize'))
          >> error reason
  case source of
    -- A forge's body is self-produced; never validate it (so no 'error' path is
    -- ever reachable for a locally-forged EB).
    ForgedBlock{} -> pure ()
    ReceivedBlockFrom{} -> do
      -- FIXME: 'ebBytesSize' here is the size we recorded from the peer
      -- offer at 'MsgLeiosBlockOffer' time (carried through the request),
      -- not the chain-authoritative 'leiosEbBytesSize' from the parent
      -- RB's 'headerLeiosAnnouncement'. EB announcements are not yet
      -- implemented; once they are, validate against the announced size
      -- so that a peer cannot poison this check by sending a bad-size
      -- offer first.
      when (ebBytesSize' /= ebBytesSize) $ do
        invalidReply $ "MsgLeiosBlock size mismatch: " <> show (ebBytesSize', ebBytesSize)
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
  -- ingest it
  (bodyClass, mempoolHits) <- MVar.modifyMVar outstandingVar $ \outstanding -> do
    let tooOld = point.pointSlotNo < Leios.acquiredEbBodiesPrunedSlot outstanding
        novel = not $ maybe False Leios.ebStateHasBody (Map.lookup ebHash (Leios.ebState outstanding))
        -- Always: this request is no longer in flight and we now have the body,
        -- so drop the body-fetch bookkeeping ('refundEbRequest' reverses the
        -- per-request accounting -- skipped if a disconnect already cancelled it
        -- in bulk -- and we delete every point listing this body from
        -- 'missingEbBodies'); and unless the EB is too old to matter, remember we
        -- have it so we neither re-fetch nor re-offer it.
        !outstandingCleaned =
          ( case mbPeer of
              Just peerId -> refundEbRequest peerId ebHash ebBytesSize
              Nothing -> id
          )
            $ outstanding
              { Leios.missingEbBodies =
                  case Map.lookup ebHash (Leios.reverseSlotIndexByEbHash outstanding) of
                        Nothing -> Leios.missingEbBodies outstanding
                        Just slots ->
                          foldr
                            (\slot -> Map.delete (MkLeiosPoint slot ebHash))
                            (Leios.missingEbBodies outstanding)
                            slots
              , Leios.reverseSlotIndexByEbHash =
                  Map.delete ebHash (Leios.reverseSlotIndexByEbHash outstanding)
              }
    -- Persist and classify only a genuinely novel, still-relevant body. A
    -- duplicate (already held) or a too-old arrival (its slot is below pruned
    -- watermark, so 'novel' can't be trusted) is left at the bookkeeping above
    -- -- in particular no second 'leiosDbInsertEbBody', hence no duplicate
    -- 'AcquiredEb'/re-offer.
    if tooOld || not novel
      then
        pure
          ( outstandingCleaned
          , ( (if tooOld then fetchArrivalEvicted else fetchArrivalExtra) $ ebBytesSize'
            , Map.empty
            )
          )
      else do
        -- TODO don't hold the outstanding mvar during this IO
        mbMissesFromBody <- traceException tracer TraceLeiosPeerDbException $ do
          -- FIXME: Once proper EB announcements are wired in, the point
          -- MUST already be present here (announcement handling inserts
          -- it) and this should become an assertion. Today we still tolerate
          -- receiving an EB body without a prior announcement, so we insert
          -- the point idempotently as a stop-gap and trace a warning.
          traceWith ktracer $ TraceLeiosBlockPointMissing point
          leiosDbInsertEbPoint db point ebBytesSize
          completedByBody <- leiosDbInsertEbBody db point eb
          mbSummaryMisses <-
            insertBody
              txCache
              ebHash
              (Leios.serializeEbBody eb)
              IntMap.empty
              (\acc i missingTxh sz -> IntMap.insert i (missingTxh, sz) acc)
          forM_ mbSummaryMisses $ traceWith ktracer . TraceLeiosTxCacheEbBody point . fst
          traceWith ktracer $ TraceLeiosBlockAcquired point
          forM_ completedByBody $ traceWith ktracer . TraceLeiosBlockTxsAcquired
          pure $ fmap snd mbSummaryMisses
        (bodyClass, misses) <- case source of
          -- A forge holds its whole closure, so nothing is missing. Its txs are
          -- inserted (applied) by the subsequent 'processLeiosBlockTxs' call; the
          -- 'insertBody' above only served to register the cache entries.
          ForgedBlock{} -> pure (fetchArrivalGood ebBytesSize', IntMap.empty)
          ReceivedBlockFrom{} -> case mbMissesFromBody of
            -- 'BodyNotYetInserted': the announcement was present and we filled it.
            Just ms -> pure (fetchArrivalGood ebBytesSize', ms)
            Nothing -> do
              -- Announcement absent (assumed present once, since evicted): the
              -- cache insert was a no-op. Backstop: classify the txs directly to
              -- build the misses.
              let MkLeiosEb v = eb
              ms <- withLookupTx txCache $ \look ->
                V.ifoldM
                  ( \acc i (txh, sz) -> do
                      r <- look txh
                      pure $ case r of
                        Just{} -> acc
                        Nothing -> IntMap.insert i (txh, sz) acc
                  )
                  IntMap.empty
                  v
              pure (fetchArrivalEvicted ebBytesSize', ms)
        -- Before allocating jobs, pull out the misses we already hold in our local
        -- mempool: they never become fetch jobs; instead we ingest them ourselves
        -- below (the last thing this function does).
        (stillMissing, mempoolHits) <- pullFromMempool misses
        let !jobPool =
              -- TODO should this calculation be deferred until the first offer
              -- arrives?
              -- each job commits to its covered tx hashes (so its response can be
              -- validated without the body); 'stillMissing' is offset -> (tx hash,
              -- on-wire size).
              Jobs.mkLeiosJobPool
                -- TODO thread the real 'LeiosFetchStaticEnv' rather than the demo one
                (Leios.maxJobBytesSize Leios.demoLeiosFetchStaticEnv)
                (Leios.maxJobTxCount Leios.demoLeiosFetchStaticEnv)
                stillMissing
            !outstanding' = Leios.insertAcquiredEbBody ebHash jobPool outstandingCleaned
        pure (outstanding', (bodyClass, mempoolHits))
  void $ MVar.tryPutMVar readyVar ()
  case source of
    ForgedBlock{} -> pure () -- self-produced: not a fetch arrival
    ReceivedBlockFrom{} -> traceWith ktracer $ TraceLeiosFetchBodyArrival bodyClass
  traceWith tracer $ MkTraceLeiosPeer $ "[done] MsgLeiosBlock " <> Leios.prettyLeiosPoint point
  -- Last: ingest the txs we found in our own mempool into the DB and cache (they
  -- were removed from the fetch job set above). This function already pays disk
  -- latency, so doing it synchronously here is fine.
  unless (Map.null mempoolHits) $
    processLeiosBlockTxs
      ktracer
      tracer
      (outstandingVar, readyVar)
      txCache
      db
      (MempoolTxs point mempoolHits)

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
  releaseJobs jobIds (Leios.MkEbState slot fetchState) =
    Leios.MkEbState slot $ case fetchState of
      Leios.NoBody -> Leios.NoBody
      Leios.BodyImminent -> Leios.BodyImminent
      Leios.BodyAcquired jobPool ->
        Leios.BodyAcquired $!
          NEIntSet.foldl'
            (flip $ Jobs.unpickJob . Jobs.MkLeiosJobId)
            jobPool
            jobIds

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
  EbHash ->
  NEIntSet ->
  LeiosOutstanding pid ->
  LeiosOutstanding pid
completeTxRequest peerId ebHash jobIds o =
  o
    { Leios.ebState = Map.adjust completeInJobPool ebHash (Leios.ebState o)
    , Leios.requestedJobsPerPeer =
        Map.update (nonEmptyMap . Map.update dropJobs ebHash) peerId (Leios.requestedJobsPerPeer o)
    }
 where
  completeInJobPool (Leios.MkEbState slot fetchState) =
    Leios.MkEbState slot $ case fetchState of
      Leios.NoBody -> Leios.NoBody
      Leios.BodyImminent -> Leios.BodyImminent
      Leios.BodyAcquired jobPool ->
        Leios.BodyAcquired $!
          NEIntSet.foldl' (flip $ Jobs.completeJob . Jobs.MkLeiosJobId) jobPool jobIds
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
  Either String [(TxHash, BS.ByteString)]
ingestJob aligned (Jobs.MkLeiosJobId jid) (Jobs.MkLeiosJob offs _expectedBytes expectedRoot)
  | Jobs.jobRootHashOfTxHashes (map fst hashed) /= expectedRoot =
      Left $ "MsgLeiosBlockTxs job " ++ show jid ++ " root-hash mismatch"
  | otherwise = Right hashed
 where
  -- 'IntMap.elems' is ascending by offset -- the order the root hash commits to.
  hashed = [(hashLeiosTx tx, bs) | (tx, bs) <- IntMap.elems (IntMap.restrictKeys aligned offs)]

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
  LeiosDbConnection m ->
  LeiosBlockTxsSource pid ->
  m ()
processLeiosBlockTxs ktracer tracer (outstandingVar, readyVar) txCache db source = case source of
  ForgedTxs _point eb txs -> do
    -- Ingest the whole closure (TODO even though we might already have some of
    -- it).
    --
    -- No peer accounting, no arrival telemetry.
    _ <- id $
        ingestAcquiredTxs
          Applied
      $ V.toList (V.map fst (leiosEbTxs eb)) `zip` V.toList (V.map cbor txs)
    void $ MVar.tryPutMVar readyVar ()
  MempoolTxs _point hits -> do
    -- Txs found in our local mempool (so already-known-valid): ingest applied,
    -- using the hashes we already have. No peer accounting, no arrival telemetry.
    _ <- ingestAcquiredTxs Applied (Map.toList hits)
    void $ MVar.tryPutMVar readyVar ()
  ReceivedTxsFrom peerId req@(MkLeiosBlockTxsRequest point jobs) txs -> do
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
      invalidReply $ "MsgLeiosBlockTxs count mismatch: " ++ show (V.length txs, IntSet.size offsetsSet)
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
          Just (Leios.MkEbState _slot Leios.NoBody) ->
            IntMap.empty
          Just (Leios.MkEbState _slot Leios.BodyImminent) ->
            IntMap.empty
          Just (Leios.MkEbState _slot (Leios.BodyAcquired jobPool)) ->
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
    txArrival <- ingestAcquiredTxs Unapplied toIngest
    traceWith ktracer $ TraceLeiosFetchTxsArrival (txArrival <> redundantExtra)
    -- 'refundTxRequest' reverses this peer's per-request byte accounting (but skips
    -- it if the peer was already cancelled in bulk by a disconnect);
    -- 'completeTxRequest' removes the now-fetched jobs from the EB's job pool and
    -- from this peer's in-flight set, so they are never re-requested.
    MVar.modifyMVar_ outstandingVar $
      pure
        . completeTxRequest peerId point.pointEbHash (NEIntMap.keysSet jobs)
        . refundTxRequest peerId (fromIntegral batchBytes)
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
  ingestAcquiredTxs :: WhetherApplied -> [(TxHash, BS.ByteString)] -> m Leios.FetchArrivalBytes
  ingestAcquiredTxs applied toIngest =
    traceException tracer TraceLeiosPeerDbException $ do
      completed <- leiosDbInsertTxs db toIngest
      forM_ completed $ traceWith ktracer . TraceLeiosBlockTxsAcquired
      case applied of
        Applied -> do
          withLockedInsertAppliedTx txCache $ \w0 step ->
            foldM (\w (txh, _bs) -> step w txh ()) w0 toIngest
          pure mempty
        Unapplied ->
          withLockedInsertUnappliedTx txCache $ \w0 step ->
            foldM (\w (txh, bs) -> step w txh (fromIntegral (BS.length bs)) ()) w0 toIngest

-- | Whether ingested txs are tagged applied (from our forge's validated mempool)
-- or unapplied (fetched from a peer).
data WhetherApplied = Applied | Unapplied

-----

-- | Record an offered EB body: mark it as something to fetch and mark the peer
-- as a serving candidate, then wake the fetch logic. Shared by the explicit
-- 'MsgLeiosBlockOffer' handler and by the CertRB roll-forward path in
-- 'checkMsgRollForwardForLeiosOffers'.
--
-- The body is /not/ added to 'missingEbBodies' if it is: too old (older than has already been pruned), already held (per
-- 'ebStateHasBody' — the only "do we have it" test now, read in-lock with no
-- cache lookup), already listed under this content hash, or zero-sized. Unless it
-- is too old or zero-sized, the offer slot is folded into 'ebState' regardless.
-- The offered size is not chain-authoritative (there are no EB announcements
-- yet), so refusing to overwrite an existing same-hash entry makes the first-seen
-- (slot, size) win, and a zero-sized offer — which no honest forger produces — is
-- dropped. The per-peer offerings are updated regardless, so the peer stays a
-- serving candidate.
recordEbBodyOffer ::
  IOLike m =>
  ( MVar m (LeiosOutstanding pid)
  , MVar m ()
  ) ->
  LeiosPeerVars m ->
  AlsoOfferedTxsClosure ->
  -- | The offered EB: its point and on-the-wire body size.
  (LeiosPoint, BytesSize) ->
  m ()
recordEbBodyOffer (outstandingVar, readyVar) peerVars offeredClosure (point, ebBytesSize) = do
  let MkLeiosPoint ebSlot ebHash = point
  MVar.modifyMVar_ outstandingVar $ \outstanding ->
    pure $!
      let tooOld = ebSlot < Leios.acquiredEbBodiesPrunedSlot outstanding -- too old to fetch
          malformed = ebBytesSize == 0 -- malformed offer
          -- Offers are currently trusted, so this is evidence that the EB is
          -- announced in this slot; fold it into 'ebState' regardless of whether
          -- we go on to list the body for fetching.
          --
          -- TODO stop that, once offers are no longer trusted
          outstanding'
            | tooOld || malformed = outstanding
            | otherwise = Leios.recordMaxAnnouncementSlot ebHash ebSlot outstanding
          skip =
            tooOld
            || malformed
            || maybe False Leios.ebStateHasBody (Map.lookup ebHash (Leios.ebState outstanding)) -- already have it
            || Map.member ebHash (Leios.reverseSlotIndexByEbHash outstanding) -- already listed
       in if skip then outstanding' else
          outstanding'
            { Leios.missingEbBodies =
                Map.insert point ebBytesSize (Leios.missingEbBodies outstanding')
            , Leios.reverseSlotIndexByEbHash =
                Map.insertWith
                  NESet.union
                  ebHash
                  (NESet.singleton ebSlot)
                  (Leios.reverseSlotIndexByEbHash outstanding')
            }
  MVar.modifyMVar_ (Leios.offerings peerVars) $ \offers ->
    -- store the offer as-is; 'mergeOffer' keeps the closure if either offer had it
    pure $! Map.insertWith Leios.mergeOffer point offeredClosure offers
  void $ MVar.tryPutMVar readyVar ()

-----

-- | The offer-side handling of a 'MsgRollForward': when the header is a CertRB
-- ('headerContainsLeiosCert'), record this peer as offering the EB it certifies
-- (via 'recordEbBodyOffer', offering both its body and tx-closure), reading
-- that EB from the predecessor's chain-dep
-- state ('chainDepStateLeiosAnnouncement'), which the CertRB's own transition
-- would overwrite. A no-op otherwise. The announcement-side handling of the same
-- header is separate; see the ChainSync client's 'leiosMsgRollForwardCallback'.
checkMsgRollForwardForLeiosOffers ::
  forall blk pid m.
  (IOLike m, ResolveLeiosBlock blk) =>
  ( MVar m (LeiosOutstanding pid)
  , MVar m ()
  ) ->
  LeiosPeerVars m ->
  Header blk ->
  ChainDepState (BlockProtocol blk) ->
  m ()
checkMsgRollForwardForLeiosOffers kernelVars peerVars hdr cds =
  when (headerContainsLeiosCert hdr) $
    forM_ (protocolStateLeiosAnnouncement @blk cds) $ \announcement ->
      recordEbBodyOffer kernelVars peerVars TxsClosureAlsoOffered announcement

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
    MkAnnouncementFields (headerElId h) forgedEb.point.pointEbHash (leiosEbBytesSize forgedEb.body)

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
  Maybe peer ->
  AnnouncementSource ->
  ShouldRelay ->
  Maybe NominalDiffTime ->
  AnnouncingHeader blk ->
  m ()
processAnnouncementCentrally
  kernelTracer
  centralVar
  kernelVars
  txCache
  source
  provenance
  shouldRelay
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
            recordAnnouncementInTxCache txCache ancHdr point
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
  recordAnnounced = recordAnnouncedEb kernelVars (point, Leios.announcementEbBodySize fields)
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

-- | Thrown when a peer relays a 'MsgLeiosBlockAnnouncement' whose header carries
-- no EB announcement (so 'mkAnnouncingHeader' returns 'Nothing'); the ensuing thread
-- death disconnects the peer.
data ExnLeiosBlockAnnouncementMissing = ExnLeiosBlockAnnouncementMissing
  deriving Show

instance Exception ExnLeiosBlockAnnouncementMissing

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
  ExtLedgerState blk EmptyMK ->
  Header blk ->
  m
    ( AnnouncementVerdict
        (AnnouncementInvalidity blk)
        (ShouldRelay, NominalDiffTime, (LeiosPoint, BytesSize))
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
              Right (FreshOCIN, v) -> VerdictProcess (shouldRelay, age, v)

-- | Record a validated, newly-announced EB body as missing, unless its already
-- pruned\/tracked\/acquired
recordAnnouncedEb ::
  IOLike m =>
  ( MVar m (LeiosOutstanding pid)
  , MVar m ()
  ) ->
  (LeiosPoint, BytesSize) ->
  m ()
recordAnnouncedEb (outstandingVar, readyVar) (point, ebBytesSize) = do
  changed <- MVar.modifyMVar outstandingVar (pure . upd)
  when changed $ void $ MVar.tryPutMVar readyVar ()
 where
  MkLeiosPoint ebSlot ebHash = point

  -- The same in-lock guard as 'recordEbBodyOffer' (too old / already held /
  -- already listed). No cache lookup: 'ebState' is authoritative here.
  upd outstanding =
    let tooOld = ebSlot < Leios.acquiredEbBodiesPrunedSlot outstanding -- too old to fetch
        !outstanding'
          | tooOld = outstanding
          | otherwise = Leios.recordMaxAnnouncementSlot ebHash ebSlot outstanding
        skip =
          tooOld
          || maybe False Leios.ebStateHasBody (Map.lookup ebHash (Leios.ebState outstanding)) -- already have it
          || Map.member ebHash (Leios.reverseSlotIndexByEbHash outstanding) -- already listed
        !outstanding''
          | skip = outstanding'
          | otherwise = outstanding'
              { Leios.missingEbBodies =
                  Map.insert point ebBytesSize (Leios.missingEbBodies outstanding')
              , Leios.reverseSlotIndexByEbHash =
                  Map.insertWith
                    NESet.union
                    ebHash
                    (NESet.singleton ebSlot)
                    (Leios.reverseSlotIndexByEbHash outstanding')
              }
     in (outstanding'', not skip)

prunePeerStateToImmTip ::
  LedgerSupportsProtocol blk =>
  ExtLedgerState blk EmptyMK ->
  SlotNo ->
  PeerState anc ->
  (SlotNo, PeerState anc)
prunePeerStateToImmTip immLedger latestPruneSlot peerSt =
  case getTipSlot (ledgerState immLedger) of
    NotOrigin immTipSlot
      | latestPruneSlot < immTipSlot -> (immTipSlot, prunePeerState immTipSlot peerSt)
    _ -> (latestPruneSlot, peerSt)

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
  LeiosDbConnection m ->
  -- | Built by the caller (see 'mkForgedAnnouncingHeader'), at the call site
  -- nearest the forge where its correspondence to the closure is evident.
  AnnouncingHeader blk ->
  Leios.ForgedLeiosEb ->
  m ()
onForgedLeiosEb kernelTracer centralVar kv txCache db anc forgedEb = do
  processAnnouncementCentrally
    kernelTracer
    centralVar
    kv
    txCache
    Nothing
    ForgedLocally
    Announcements.DoRelay
    Nothing
    anc
  processLeiosBlock
    kernelTracer
    nullTracer
    kv
    txCache
    db
    noMempoolPull -- the forge holds the whole closure
    (ForgedBlock forgedEb.point)
    forgedEb.body
  processLeiosBlockTxs
    kernelTracer
    nullTracer
    kv
    txCache
    db
    (ForgedTxs forgedEb.point forgedEb.body $ V.fromList $ map (MkLeiosTx . snd) $ forgedEb.txClosure)
  traceWith kernelTracer $
    TraceLeiosBlockStored{slot = forgedEb.point.pointSlotNo, eb = forgedEb.body}
