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
import Control.Monad (foldM, forM_, when)
import Control.Monad.Class.MonadThrow (Exception, catch, throwIO)
import Control.Monad.Except (runExcept)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Tracer (Tracer, contramap, traceWith)
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Functor (void, (<&>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.List (unfoldr)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
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
  , maxTxsPerEb
  , leiosEbTxs
  , RbHash (..)
  )
import qualified LeiosDemoTypes as Leios
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
import Ouroboros.Consensus.Storage.LedgerDB.Forker (ResolveLeiosBlock (..))
import Ouroboros.Consensus.Util.IOLike (IOLike)

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
  let nextOffset = \case
        [] -> Nothing
        (idx, bitmap) : k -> case popLeftmostOffset bitmap of
          Nothing -> nextOffset k
          Just (i, bitmap') ->
            Just (64 * fromIntegral idx + i, (idx, bitmap') : k)
      txOffsets = unfoldr nextOffset bitmaps
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

newtype LeiosFetchDecisions pid
  = MkLeiosFetchDecisions
      (Map (PeerId pid) (Map SlotNo (DList (TxHash, BytesSize, EbHash, Int), DList (EbHash, BytesSize))))

emptyLeiosFetchDecisions :: LeiosFetchDecisions pid
emptyLeiosFetchDecisions = MkLeiosFetchDecisions Map.empty

leiosFetchLogicIteration ::
  forall pid.
  Ord pid =>
  LeiosFetchStaticEnv ->
  -- | The current slot, or 'Nothing' when it is not yet known (i.e. we are
  -- syncing), in which case we fetch freshest-last instead of freshest-first.
  Maybe SlotNo ->
  Map (PeerId pid) (Set EbHash, Set EbHash) ->
  LeiosOutstanding pid ->
  (LeiosOutstanding pid, LeiosFetchDecisions pid)
leiosFetchLogicIteration env mbCurrentSlot offerings =
  \acc ->
    go1 acc emptyLeiosFetchDecisions $
      expand $
        prioritize $
          Map.map Left (Leios.missingEbBodies acc) `Map.union` Map.map Right (Leios.missingEbTxs acc)
 where
  -- Once we know the current slot we fetch freshest-first; until then we are
  -- syncing, so we fetch freshest-last (i.e. oldest-first) to make progress
  -- from the tip of our chain forward.
  prioritize m = case mbCurrentSlot of
    Nothing -> Map.toAscList m
    Just _currentSlot -> Map.toDescList m

  expand = \case
    [] -> []
    (point, Left ebBytesSize) : vs -> Left (point, ebBytesSize) : expand vs
    (point, Right v) : vs ->
      [Right (point, txBytesSize, txHash) | (_txOffset, (txHash, txBytesSize)) <- IntMap.toAscList v]
        <> expand vs

  go1 ::
    LeiosOutstanding pid ->
    LeiosFetchDecisions pid ->
    [Either (LeiosPoint, BytesSize) (LeiosPoint, BytesSize, TxHash)] ->
    (LeiosOutstanding pid, LeiosFetchDecisions pid)
  go1 !acc !accNew = \case
    [] ->
      (acc, accNew)
    Left (point, ebBytesSize) : targets
      | let peerIds :: Set (PeerId pid)
            peerIds = Map.findWithDefault Set.empty point.pointEbHash (Leios.requestedEbPeers acc) ->
          goEb2 acc accNew targets point ebBytesSize peerIds
    Right (point, txBytesSize, txHash) : targets ->
      let !txOffsets = case Map.lookup txHash (Leios.reverseEbIndexByTx acc) of
            Nothing -> error "impossible! leiosFetchLogicIteration go1"
            Just x -> x
          peerIds :: Set (PeerId pid)
          peerIds = Map.findWithDefault Set.empty txHash (Leios.requestedTxPeers acc)
       in goTx2 acc accNew targets point txBytesSize txHash txOffsets peerIds

  goEb2 !acc !accNew targets point ebBytesSize peerIds
    | Leios.requestedBytesSize acc >= Leios.maxRequestedBytesSize env -- we can't request anything
      =
        (acc, accNew)
    | Set.size peerIds < Leios.maxRequestsPerEb env -- we would like to request it from an additional peer
    , Just peerId <- choosePeerEb peerIds acc point.pointEbHash =
        -- there's a peer who offered it and we haven't already requested it from them
        let accNew' =
              MkLeiosFetchDecisions $
                Map.insertWith
                  (Map.unionWith (<>))
                  peerId
                  ( Map.singleton
                      point.pointSlotNo
                      (DList.empty, DList.singleton (point.pointEbHash, ebBytesSize))
                  )
                  (let MkLeiosFetchDecisions x = accNew in x)
            acc' =
              acc
                { Leios.requestedEbPeers =
                    Map.insertWith Set.union point.pointEbHash (Set.singleton peerId) (Leios.requestedEbPeers acc)
                , Leios.requestedBytesSizePerPeer =
                    Map.insertWith (+) peerId ebBytesSize (Leios.requestedBytesSizePerPeer acc)
                , Leios.requestedBytesSize = ebBytesSize + Leios.requestedBytesSize acc
                }
            peerIds' = Set.insert peerId peerIds
         in goEb2 acc' accNew' targets point ebBytesSize peerIds'
    | otherwise =
        go1 acc accNew targets

  choosePeerEb :: Set (PeerId pid) -> LeiosOutstanding pid -> EbHash -> Maybe (PeerId pid)
  choosePeerEb peerIds acc ebHash =
    foldr (\a _ -> Just a) Nothing $
      [ peerId
      | (peerId, (ebHashes, _ebHashes)) <-
          Map.toList $ -- TODO prioritize/shuffle?
            (`Map.withoutKeys` peerIds) $ -- not already requested from this peer
              offerings
      , Map.findWithDefault 0 peerId (Leios.requestedBytesSizePerPeer acc)
          <= Leios.maxRequestedBytesSizePerPeer env
      , -- peer can be sent more requests
      ebHash `Set.member` ebHashes -- peer has offered this EB body
      ]

  goTx2 ::
    LeiosOutstanding pid ->
    LeiosFetchDecisions pid ->
    [Either (LeiosPoint, BytesSize) (LeiosPoint, BytesSize, TxHash)] ->
    LeiosPoint ->
    BytesSize ->
    TxHash ->
    Map EbHash (NESet SlotNo, Int, BytesSize) ->
    Set (PeerId pid) ->
    (LeiosOutstanding pid, LeiosFetchDecisions pid)
  goTx2 !acc !accNew targets point txBytesSize txHash txOffsets peerIds
    | Leios.requestedBytesSize acc >= Leios.maxRequestedBytesSize env -- we can't request anything
      =
        (acc, accNew)
    | Set.size peerIds < Leios.maxRequestsPerTx env -- we would like to request it from an additional peer
    -- TODO if requests list priority, does this limit apply even if the
    -- tx has only been requested at lower priorities?
    , Just peerId <- choosePeerTx peerIds acc point.pointEbHash =
        -- there's a peer offering this EB's tx closure and we haven't already
        -- requested it from them
        let txOffset = case Map.lookup point.pointEbHash txOffsets of
              Just (_slots, o, _) -> o
              Nothing -> error "impossible! goTx2: target EB absent from its own reverse entry"
            accNew' =
              MkLeiosFetchDecisions $
                Map.insertWith
                  (Map.unionWith (<>))
                  peerId
                  (Map.singleton point.pointSlotNo (DList.singleton (txHash, txBytesSize, point.pointEbHash, txOffset), DList.empty))
                  (let MkLeiosFetchDecisions x = accNew in x)
            acc' =
              acc
                { Leios.requestedTxPeers =
                    Map.insertWith Set.union txHash (Set.singleton peerId) (Leios.requestedTxPeers acc)
                , Leios.requestedBytesSizePerPeer =
                    Map.insertWith (+) peerId txBytesSize (Leios.requestedBytesSizePerPeer acc)
                , Leios.requestedBytesSize = txBytesSize + Leios.requestedBytesSize acc
                }
            peerIds' = Set.insert peerId peerIds
         in goTx2 acc' accNew' targets point txBytesSize txHash txOffsets peerIds'
    | otherwise =
        go1 acc accNew targets

  choosePeerTx ::
    Set (PeerId pid) ->
    LeiosOutstanding pid ->
    EbHash ->
    Maybe (PeerId pid)
  choosePeerTx peerIds acc ebHash =
    foldr (\a _ -> Just a) Nothing $
      [ peerId
      | (peerId, (_bodies, closures)) <-
          Map.toList $ -- TODO prioritize/shuffle?
            (`Map.withoutKeys` peerIds) $ -- not already requested from this peer
              offerings
      , Map.findWithDefault 0 peerId (Leios.requestedBytesSizePerPeer acc)
          <= Leios.maxRequestedBytesSizePerPeer env
      , -- peer can be sent more requests
      ebHash `Set.member` closures -- peer has offered this EB's tx closure
      ]

packRequests ::
  LeiosFetchStaticEnv ->
  LeiosFetchDecisions pid ->
  Map (PeerId pid) (Seq LeiosFetchRequest)
packRequests env =
  \(MkLeiosFetchDecisions x) -> Map.map goPeer x
 where
  goPeer =
    Map.foldlWithKey
      (\acc prio (txs, ebs) -> goPrioTx prio txs <> goPrioEb prio ebs <> acc)
      -- TODO priority within same slot?
      Seq.empty

  goPrioEb prio ebs =
    DList.foldr (Seq.:<|) Seq.empty $
      DList.map
        ( \(ebHash, ebBytesSize) ->
            LeiosBlockRequest $ MkLeiosBlockRequest (MkLeiosPoint prio ebHash) ebBytesSize
        )
        ebs

  goPrioTx prio txs =
    Map.foldlWithKey
      ( \acc ebHash txs' ->
          goEb {- prio -}
            (MkLeiosPoint prio ebHash)
            0
            IntMap.empty
            0
            DList.empty
            (IntMap.toAscList txs')
            <> acc
      )
      Seq.empty
      -- group by EbHash, sort by offset ascending. 'prio' is the target point's
      -- own slot and 'ebHash' its own EbHash (both filed by 'goTx2' from the same
      -- point), so 'MkLeiosPoint prio ebHash' is a real point -- slot and hash
      -- from the same EB.
      $ Map.fromListWith IntMap.union
      $ [ (ebHash, IntMap.singleton txOffset (txHash, txBytesSize))
        | (txHash, txBytesSize, ebHash, txOffset) <- DList.toList txs
        ]

  goEb ::
    LeiosPoint ->
    BytesSize ->
    IntMap Word64 ->
    Int ->
    DList TxHash ->
    [(Int, (TxHash, BytesSize))] ->
    Seq LeiosFetchRequest
  -- TODO the incoming indexes are ascending, so the IntMap accumulator could
  -- be simplified away
  goEb p !accTxBytesSize !accBitmaps !accN !accHashes = \case
    [] -> if 0 < accN then Seq.singleton flush else Seq.empty
    txsAgain@((txOffset, (txHash, txBytesSize)) : txs)
      | Leios.maxRequestBytesSize env < accTxBytesSize' ->
          flush Seq.:<| goEb p 0 IntMap.empty 0 DList.empty txsAgain
      | otherwise
      , let (q, r) = txOffset `divMod` 64 ->
          goEb
            p
            accTxBytesSize'
            (IntMap.insertWith (Bits..|.) q (Bits.bit (63 - r)) accBitmaps)
            (accN + 1)
            (accHashes `DList.snoc` txHash)
            txs
     where
      accTxBytesSize' = accTxBytesSize + txBytesSize
   where
    flush =
      LeiosBlockTxsRequest $
        MkLeiosBlockTxsRequest
          {- prio -}
          p
          [(fromIntegral idx, bitmap) | (idx, bitmap) <- IntMap.toAscList accBitmaps]
          (V.fromListN accN $ DList.toList accHashes)

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
nextLeiosFetchClientCommand ktracer tracer stopSTM kernelVars txCache db peerId reqsVar responseQ = do
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
        msgLeiosBlock ktracer tracer kernelVars txCache db peerId req eb
      PendingBlockTxsResponse req txs ->
        msgLeiosBlockTxs ktracer tracer kernelVars txCache db peerId req txs

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
    LeiosBlockTxsRequest req@(MkLeiosBlockTxsRequest p bitmaps _txHashes) ->
      LF.MkSomeLeiosFetchJob
        (LF.MsgLeiosBlockTxsRequest p bitmaps)
        ( pure $ \(LF.MsgLeiosBlockTxs _ _ txs) ->
            StrictSTM.atomically $
              LazySTM.writeTQueue responseQ (PendingBlockTxsResponse req txs)
        )

-----

msgLeiosBlock ::
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
  PeerId pid ->
  LeiosBlockRequest ->
  LeiosEb ->
  m ()
msgLeiosBlock ktracer tracer (outstandingVar, readyVar) txCache db peerId req eb = do
  -- validate it
  let MkLeiosBlockRequest point ebBytesSize = req
  traceWith tracer $ MkTraceLeiosPeer $ "[start] MsgLeiosBlock " <> Leios.prettyLeiosPoint point
  let MkLeiosPoint _ebSlot ebHash = point
  let ebBytesSize' = leiosEbBytesSize eb
  -- A failed-validation body: attribute the whole body to 'fabInvalid'.
  let invalidReply reason =
        traceWith ktracer (TraceLeiosFetchBodyArrival (fetchArrivalInvalid ebBytesSize'))
          >> error reason
  do
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
    -- Every referenced tx must be unique: 'reverseEbIndexByTx' records one
    -- offset per (tx, EB), so a duplicate desyncs it from 'missingEbTxs'.
    let MkLeiosEb v = eb
        duplicateTxHashes =
          Map.keys $
            Map.filter (> (1 :: Int)) $
              Map.fromListWith (+) [(txh, 1) | (txh, _) <- V.toList v]
    when (not (null duplicateTxHashes)) $ do
      invalidReply $ "MsgLeiosBlock duplicate tx hashes: " <> show duplicateTxHashes
  -- ingest it
  bodyClass <- MVar.modifyMVar outstandingVar $ \outstanding -> do
    let tooOld = point.pointSlotNo < Leios.acquiredEbBodiesPrunedSlot outstanding
        novel = not $ Map.member ebHash (Leios.acquiredEbBodies outstanding)
        -- Always: this request is no longer in flight and we now have the body,
        -- so drop the body-fetch bookkeeping ('refundEbRequest' reverses the
        -- per-request accounting -- skipped if a disconnect already cancelled it
        -- in bulk -- and we delete the point from 'missingEbBodies'); and unless
        -- the EB is too old to matter, remember we have it so we neither
        -- re-fetch nor re-offer it.
        !outstandingCleaned =
          refundEbRequest peerId ebHash ebBytesSize $
            outstanding
              { Leios.missingEbBodies = Map.delete point (Leios.missingEbBodies outstanding)
              , Leios.acquiredEbBodies =
                  if tooOld
                    then Leios.acquiredEbBodies outstanding
                    else Map.insert ebHash point.pointSlotNo (Leios.acquiredEbBodies outstanding)
              }
    -- Persist and classify only a genuinely novel, still-relevant body. A
    -- duplicate (already in 'acquiredEbBodies') or a too-old arrival (its
    -- 'acquiredEbBodies' slot has been pruned, so 'novel' can't be trusted) is
    -- left at the bookkeeping above -- in particular no second
    -- 'leiosDbInsertEbBody', hence no duplicate 'AcquiredEb'/re-offer.
    if tooOld || not novel
      then
        pure
          ( outstandingCleaned
          , (if tooOld then fetchArrivalEvicted else fetchArrivalExtra) $ ebBytesSize'
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
        (bodyClass, misses) <- case mbMissesFromBody of
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
        let !outstanding' =
              outstandingCleaned
                { Leios.blockingPerEb =
                    Map.insert point (IntMap.size misses) (Leios.blockingPerEb outstandingCleaned)
                , Leios.missingEbTxs =
                    Map.insert point misses (Leios.missingEbTxs outstandingCleaned)
                , Leios.reverseEbIndexByTx =
                    IntMap.foldrWithKey
                      ( \i (txHash, txBytesSize) acc ->
                          Map.insertWith
                            (Map.unionWith (\(s1, i1, z1) (s2, _, _) -> (s1 <> s2, i1, z1)))
                            txHash
                            (Map.singleton ebHash (NESet.singleton point.pointSlotNo, i, txBytesSize))
                            acc
                      )
                      (Leios.reverseEbIndexByTx outstandingCleaned)
                      misses
                }
        pure (outstanding', bodyClass)
  void $ MVar.tryPutMVar readyVar ()
  traceWith ktracer $ TraceLeiosFetchBodyArrival bodyClass
  traceWith tracer $ MkTraceLeiosPeer $ "[done] MsgLeiosBlock " <> Leios.prettyLeiosPoint point

-----

delIf :: (a -> Bool) -> a -> Maybe a
delIf predicate x = if predicate x then Nothing else Just x

-----

-- | Cancel all of a peer's outstanding fetch requests in bulk, e.g. when it
-- disconnects: refund its share of the request budget and drop it from the
-- per-EB/per-tx request sets, so those items can be re-requested from other
-- peers.
--
-- Note this is O(size of the request maps): it scans 'requestedEbPeers' /
-- 'requestedTxPeers' for the peer rather than knowing its keys directly. A
-- future optimisation would track a per-peer in-flight set to make this
-- O(that peer's outstanding requests).
removePeerFromOutstanding ::
  Ord pid =>
  PeerId pid ->
  LeiosOutstanding pid ->
  LeiosOutstanding pid
removePeerFromOutstanding peerId o =
  o
    { Leios.requestedBytesSize =
        Leios.requestedBytesSize o - Map.findWithDefault 0 peerId (Leios.requestedBytesSizePerPeer o)
    , Leios.requestedBytesSizePerPeer = Map.delete peerId (Leios.requestedBytesSizePerPeer o)
    , Leios.requestedEbPeers =
        Map.mapMaybe (delIf Set.null . Set.delete peerId) (Leios.requestedEbPeers o)
    , Leios.requestedTxPeers =
        Map.mapMaybe (delIf Set.null . Set.delete peerId) (Leios.requestedTxPeers o)
    }

-----

-- | Reverse this peer's per-request accounting for a received EB body, but only
-- if the peer is still tracked.
--
-- If the peer has already been cancelled in bulk (e.g. it disconnected and its
-- requests were refunded en masse via its 'requestedBytesSizePerPeer' total),
-- that entry is gone; re-applying the per-request refund here would
-- double-subtract 'requestedBytesSize' and underflow. So we gate on the peer
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
        { Leios.requestedBytesSize = Leios.requestedBytesSize o - ebBytesSize
        , Leios.requestedBytesSizePerPeer =
            Map.update (\x -> delIf (== 0) (x - ebBytesSize)) peerId (Leios.requestedBytesSizePerPeer o)
        , Leios.requestedEbPeers =
            Map.update (delIf Set.null . Set.delete peerId) ebHash (Leios.requestedEbPeers o)
        }
  | otherwise = o

-----

-- | Like 'refundEbRequest', but for a received batch of EB txs: installs the
-- already-computed 'requestedTxPeers'' (this peer removed from each requested
-- tx) and refunds the bytes, gated on the peer still being tracked (see
-- 'refundEbRequest').
refundTxRequest ::
  Ord pid =>
  PeerId pid ->
  Map TxHash (Set (PeerId pid)) ->
  BytesSize ->
  LeiosOutstanding pid ->
  LeiosOutstanding pid
refundTxRequest peerId requestedTxPeers' txsBytesSize o
  | Map.member peerId (Leios.requestedBytesSizePerPeer o) =
      o
        { Leios.requestedBytesSize = Leios.requestedBytesSize o - txsBytesSize
        , Leios.requestedBytesSizePerPeer =
            Map.update (\x -> delIf (== 0) (x - txsBytesSize)) peerId (Leios.requestedBytesSizePerPeer o)
        , Leios.requestedTxPeers = requestedTxPeers'
        }
  | otherwise = o

-----

msgLeiosBlockTxs ::
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
  PeerId pid ->
  LeiosBlockTxsRequest ->
  V.Vector LeiosTx ->
  m ()
msgLeiosBlockTxs ktracer tracer (outstandingVar, readyVar) txCache db peerId req txs = do
  traceWith tracer $ MkTraceLeiosPeer $ "[start] " ++ Leios.prettyLeiosBlockTxsRequest req
  -- validate it
  -- TODO: could validate the returned point + bitmaps too (added to response recently)
  let MkLeiosBlockTxsRequest point bitmaps txHashes = req
  let txBytess = V.map cbor txs
  let batchBytes = V.sum (V.map BS.length txBytess)
  -- A failed-validation batch: attribute the whole batch to 'fabInvalid'.
  let invalidReply reason =
        traceWith ktracer (TraceLeiosFetchTxsArrival (fetchArrivalInvalid (fromIntegral batchBytes)))
          >> error reason
  do
    when (V.length txs /= V.length txHashes) $ do
      invalidReply $ "MsgLeiosBlockTxs length mismatch: " ++ show (V.length txs, V.length txHashes)
    let txHashes' = V.map hashLeiosTx txs
    when (txHashes' /= txHashes) $ do
      let mismatches =
            V.toList $
              V.findIndices id $
                V.zipWith (/=) txHashes txHashes'
      invalidReply $ "MsgLeiosBlockTxs hash mismatches: " ++ show mismatches
  let nextOffset = \case
        [] -> Nothing
        (idx, bitmap) : k -> case popLeftmostOffset bitmap of
          Nothing -> nextOffset k
          Just (i, bitmap') ->
            Just (64 * fromIntegral idx + i, (idx, bitmap') : k)
      offsets = unfoldr nextOffset bitmaps
  -- ingest
  txArrival <- traceException tracer TraceLeiosPeerDbException $ do
    completed <- leiosDbInsertTxs db (V.toList $ V.zip txHashes txBytess)
    forM_ completed $ traceWith ktracer . TraceLeiosBlockTxsAcquired
    -- crucially: insert the txs into the TxCacheIndex _after_ they've been
    -- written to the LeiosDb, since that's what the TxCacheIndex currently
    -- indexes. The handle buckets each tx's bytes by its prior state in the same
    -- locked pass -- coherent under concurrent duplicate deliveries; the returned
    -- partition sums to the batch size.
    withLockedInsertUnappliedTx txCache $ \w0 step ->
      V.foldM'
        (\w (txh, sz) -> step w txh sz ())
        w0
        (V.zip txHashes (V.map (fromIntegral . BS.length) txBytess))
  traceWith ktracer $ TraceLeiosFetchTxsArrival txArrival
  -- update NodeKernel state
  MVar.modifyMVar_ outstandingVar $ \outstanding -> do
    let removeTxFromMissing txHash mtxs =
          case Map.lookup txHash (Leios.reverseEbIndexByTx outstanding) of
            Nothing -> mtxs
            Just ebsWithThisTx ->
              Map.foldrWithKey
                ( \ebHash (slotsWithThisEb, offset, _sz) acc ->
                    foldr
                      (\ebSlot ->
                         Map.update
                           (delIf IntMap.null . IntMap.delete offset)
                           (MkLeiosPoint ebSlot ebHash)
                      )
                      acc
                      slotsWithThisEb
                )
                mtxs
                ebsWithThisTx
    let (requestedTxPeers', reverseEbIndexByTx', missingEbTxs', txsBytesSize) =
          V.foldl'
            ( \(!accReqs, !accRev, !accMtxs, !accSz) (txHash, txBytes) ->
                ( Map.update (delIf Set.null . Set.delete peerId) txHash accReqs
                , Map.delete txHash accRev   -- full delete from reverseEbIndexByTx
                , removeTxFromMissing txHash accMtxs   -- full delete from missingEbTxs
                , accSz + BS.length txBytes
                )
            )
            ( Leios.requestedTxPeers outstanding
            , Leios.reverseEbIndexByTx outstanding
            , Leios.missingEbTxs outstanding
            , 0
            )
            (txHashes `V.zip` txBytess)
    let offsetsSet = IntSet.fromList offsets
        -- the requests this MsgLeiosBlockTxs was the first to resolve for this
        -- point (kept only to keep the best-effort 'blockingPerEb' roughly current)
        beatOtherPeers =
          (`IntMap.restrictKeys` offsetsSet) $
            Map.findWithDefault IntMap.empty point (Leios.missingEbTxs outstanding)
    -- 'refundTxRequest' reverses this peer's per-request accounting (but skips
    -- it if the peer has already been cancelled in bulk by a disconnect); the
    -- global state below is updated unconditionally, since we did receive the
    -- txs.
    let !outstanding' =
          refundTxRequest peerId requestedTxPeers' (fromIntegral txsBytesSize) $
            outstanding
              { Leios.missingEbTxs = missingEbTxs'
              , Leios.reverseEbIndexByTx = reverseEbIndexByTx'
              , Leios.blockingPerEb =
                  if IntMap.null beatOtherPeers
                    then Leios.blockingPerEb outstanding
                    else
                      Map.alter
                        ( \case
                            Nothing -> Nothing
                            Just x -> delIf (== 0) $ x - IntMap.size beatOtherPeers
                        )
                        point
                        (Leios.blockingPerEb outstanding)
              }
    pure outstanding'
  void $ MVar.tryPutMVar readyVar ()
  traceWith tracer $ MkTraceLeiosPeer $ "[done] " ++ Leios.prettyLeiosBlockTxsRequest req

-----

-- | Whether an EB offer also implies its tx-closure is on offer. A CertRB does
-- (it certifies the whole EB); a bare 'MsgLeiosBlockOffer' does not — the closure
-- is offered separately, as a 'MsgLeiosBlockTxsOffer'.
data AlsoOfferedTxsClosure = TxsClosureAlsoOffered | TxsClosureNotAlsoOffered

-- | Record an offered EB body: mark it as something to fetch and mark the peer
-- as a serving candidate, then wake the fetch logic. Shared by the explicit
-- 'MsgLeiosBlockOffer' handler and by the CertRB roll-forward path in
-- 'checkMsgRollForwardForLeiosOffers'.
--
-- The body is /not/ added to 'missingEbBodies' if it is: too old (at or below
-- the slot 'acquiredEbBodies' has been pruned to), already recorded in
-- 'acquiredEbBodies' (received or forged — the only "do we have it" test now,
-- read in-lock with no cache lookup), already listed under this content hash, or
-- zero-sized.
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
    pure $
      if ebSlot < Leios.acquiredEbBodiesPrunedSlot outstanding -- too old to fetch
        || ebBytesSize == 0 -- malformed offer
        || Map.member ebHash (Leios.acquiredEbBodies outstanding) -- already have it
        || any ((== ebHash) . pointEbHash) (Map.keys (Leios.missingEbBodies outstanding)) -- already listed
        then outstanding
        else
          outstanding
            { Leios.missingEbBodies =
                Map.insert point ebBytesSize (Leios.missingEbBodies outstanding)
            }
  MVar.modifyMVar_ (Leios.offerings peerVars) $ \(offers1, offers2) -> do
    let !offers1' = Set.insert ebHash offers1
        !offers2' = case offeredClosure of
          TxsClosureAlsoOffered -> Set.insert ebHash offers2
          TxsClosureNotAlsoOffered -> offers2
    pure (offers1', offers2')
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
-- effectful glue (reading the immutable tip, the 'Announcements.PeerState' ref,
-- 'MVar' updates, tracing, and 'throwIO') lives in the NodeToNode client, which
-- invokes 'Announcements.onAnnouncement' with these pieces.

-- | 'Header blk' as a relayed LeiosNotify announcement, paired with the
-- announcement data parsed from it (see 'mkAnnouncingHeader'). The 'Eq' instance
-- compares by header hash: that is the one identity used for announcement dedup
-- and equivocation counting (see 'Announcements.onAnnouncement').
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
  Announcements.ShouldRelay ->
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
            recordAnnouncedEb txCache kernelVars (point, Leios.announcementEbBodySize fields)
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

-- | Process this node's own freshly-forged EB announcement: relay it centrally
-- (as 'ForgedLocally', before the forge inserts the EB body — writing the body
-- is what offers it, so a downstream peer never gets the offer before the
-- announcement) and record the EB as acquired, so we neither re-fetch nor
-- re-offer the body we just forged. A no-op for a forged block that announces no
-- EB. 'forge' invokes this right after forging and before adoption (its
-- 'afterForgeBeforeInsert' callback).
processForgedAnnouncement ::
  forall blk peer pid m.
  (IOLike m, ResolveLeiosBlock blk, HasHeader (Header blk), Ord peer) =>
  Tracer m TraceLeiosKernel ->
  MVar m (Announcements.CentralState m peer (AnnouncingHeader blk)) ->
  MVar m (LeiosOutstanding pid) ->
  Header blk ->
  m ()
processForgedAnnouncement kernelTracer centralVar outstandingVar forgedHeader =
  forM_ (mkAnnouncingHeader forgedHeader) $ \anc -> do
    MVar.modifyMVar_ centralVar $ \cst ->
      Announcements.onAnnouncementCentral
        (contramap (traceNewAnnouncement ForgedLocally) kernelTracer)
        ancElId
        (\_elSt -> pure ()) -- we forged the EB; nothing to fetch locally
        cst
        Nothing -- the source is this node, not an upstream peer
        Announcements.DoRelay -- our newly forged block can't be too old
        Nothing -- no wall-clock lateness for a locally-forged announcement
        anc
    -- Record the forged EB as acquired: don't fetch the body we're about to
    -- insert, and don't re-offer it when an offer or its CertRB comes back.
    MVar.modifyMVar_ outstandingVar $ \outstanding ->
      let ebSlot = blockSlot (ancHeader anc)
          ebHash = announcementEbHash (ancAnnouncementFields anc)
       in pure $
            outstanding
              { Leios.acquiredEbBodies =
                  Map.insert ebHash ebSlot (Leios.acquiredEbBodies outstanding)
              }

-- | Thrown when a peer misbehaves on the announcement protocol; the ensuing
-- thread death disconnects the peer. It carries the
-- 'Announcements.ErrAnnouncement' verbatim (the @blk@ is existential); every
-- such error is a disconnect, since the only invalidities that used to be
-- tolerated — opcert issue numbers ahead of the immutable tip — are now
-- accepted outright by 'validateAnnouncementHeader'.
data ExnInvalidLeiosAnnouncement
  = forall blk.
    ReactToAnnouncementError (Announcements.ErrAnnouncement (AnnouncementInvalidity blk))

deriving instance Show ExnInvalidLeiosAnnouncement

instance Exception ExnInvalidLeiosAnnouncement

-- | Thrown when a peer relays a 'MsgLeiosBlockAnnouncement' whose header carries
-- no EB announcement (so 'mkAnnouncingHeader' returns 'Nothing'); the ensuing thread
-- death disconnects the peer.
data ExnLeiosBlockAnnouncementMissing = ExnLeiosBlockAnnouncementMissing
  deriving Show

instance Exception ExnLeiosBlockAnnouncementMissing

-- | The @validate@ callback for 'Announcements.onAnnouncement'.
--
-- First apply ChainSync's in-future check to the announced slot's wall-clock
-- onset (reusing the node's own 'InFutureCheck.SomeHeaderInFutureCheck'):
-- a far-future slot raises 'InFutureCheck.HeaderArrivalException' (disconnecting
-- the peer), a near-future slot blocks until the slot's onset (Ouroboros
-- Chronos) — blocking the per-peer handler is acceptable, as a (near-)future
-- announcement is the peer's fault.
--
-- Returns the announcement's data and whether to relay it downstream (see
-- 'Announcements.ShouldRelay' and 'maxAnnouncementAgeSend'), if the announcement
-- is valid. Per the 'Announcements.onAnnouncement' contract, 'Left Nothing'
-- signals the too-old rejection (see 'maxAnnouncementAgeRecv') and 'Left Just'
-- any other invalidity.
announcementValidity ::
  (IOLike m, LedgerSupportsProtocol blk, ResolveLeiosBlock blk) =>
  SystemTime m ->
  InFutureCheck.SomeHeaderInFutureCheck m blk ->
  TopLevelConfig blk ->
  ExtLedgerState blk EmptyMK ->
  Header blk ->
  m
    ( Either
        (Maybe (AnnouncementInvalidity blk))
        (Announcements.ShouldRelay, NominalDiffTime, (LeiosPoint, BytesSize))
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
    -- 'Left Nothing' signals the too-old rejection to 'Announcements.onAnnouncement'
    -- (which raises 'Announcements.ErrTooOld'); only this function holds the wall
    -- clock, so it owns that check.
    if age > maxAnnouncementAgeRecv
      then Left Nothing
      else
        let shouldRelay =
              if age <= maxAnnouncementAgeSend
                then Announcements.DoRelay
                else Announcements.DoNotRelay
         in case validateAnnouncementHeader cfg immLedger hdr of
              Left inv -> Left (Just inv)
              Right v -> Right (shouldRelay, age, v)

-- | Record a validated, newly-announced EB body as missing, with its
-- authoritative (forger-signed) size. First-seen wins: a no-op if the body is
-- already acquired or already recorded.
recordAnnouncedEb ::
  IOLike m =>
  LeiosTxCache m () () SerializedEbBody ->
  ( MVar m (LeiosOutstanding pid)
  , MVar m ()
  ) ->
  (LeiosPoint, BytesSize) ->
  m ()
recordAnnouncedEb txCache (outstandingVar, readyVar) (point, ebBytesSize) =
  txCache.lookupBody ebHash >>= \case
    -- TODO once LeiosFetch is announcement-sensitive: a fresher announcement for
    -- an EB we already hold should still raise its freshest-first priority, which
    -- this branch drops.
    --
    -- Note that that priority applies to the diffusion of this EB's closure
    -- too.
    --
    -- We're accepting that infelicity for now; the imminent LeiosFetch rewrite
    -- will address this. But this handler will be what takes care of it:
    -- updating an EB closure's priority is the reponsibility of the
    -- announcement handler.
    Just{} -> pure () -- we already hold this EB's body; nothing to fetch
    Nothing -> do
      changed <- MVar.modifyMVar outstandingVar (pure . upd)
      when changed $ void $ MVar.tryPutMVar readyVar ()
 where
  MkLeiosPoint _ebSlot ebHash = point

  upd outstanding =
    if Map.member ebHash (Leios.acquiredEbBodies outstanding)
      || any ((== ebHash) . pointEbHash) (Map.keys (Leios.missingEbBodies outstanding))
      then (outstanding, False)
      else
        flip (,) True $
          outstanding
            { Leios.missingEbBodies =
                Map.insert point ebBytesSize (Leios.missingEbBodies outstanding)
            }

prunePeerStateToImmTip ::
  LedgerSupportsProtocol blk =>
  ExtLedgerState blk EmptyMK ->
  SlotNo ->
  Announcements.PeerState anc ->
  (SlotNo, Announcements.PeerState anc)
prunePeerStateToImmTip immLedger latestPruneSlot peerSt =
  case getTipSlot (ledgerState immLedger) of
    NotOrigin immTipSlot
      | latestPruneSlot < immTipSlot -> (immTipSlot, Announcements.prunePeerState immTipSlot peerSt)
    _ -> (latestPruneSlot, peerSt)

-- | The just-counted announcement's fields, and whether it equivocates a prior
-- header announcing the same election.
announcementTraceFields ::
  Announcements.ElState (AnnouncingHeader blk) ->
  (AnnouncementEquivocation, AnnouncementFields)
announcementTraceFields = \case
  Announcements.OneAnnouncement a ->
    (NoEquivocation, ancAnnouncementFields a)
  Announcements.TwoAnnouncements _a1 a2 ->
    (Equivocation, ancAnnouncementFields a2)

-- | Render an 'Announcements' per-peer announcement event as a 'TraceLeiosPeer'.
tracePeerAnnouncement ::
  Announcements.TraceLeiosNotifyPeerEvent (AnnouncingHeader blk) ->
  TraceLeiosPeer
tracePeerAnnouncement (Announcements.TracePeerAnnouncement elSt) =
  let (equivocation, fields) = announcementTraceFields elSt
   in TraceLeiosPeerAnnouncement equivocation fields

-- | Render an 'Announcements' node-wide announcement event as a
-- 'TraceLeiosKernel'. The 'AnnouncementSource' is supplied by the caller (only
-- it knows which path delivered the announcement); the event's own @mbPeer@
-- cannot distinguish LeiosNotify from ChainSync, as both carry a peer.
traceNewAnnouncement ::
  AnnouncementSource ->
  Announcements.TraceLeiosNotifyEvent peer (AnnouncingHeader blk) ->
  TraceLeiosKernel
traceNewAnnouncement source (Announcements.TraceNewAnnouncement _mbPeer _elId elSt age) =
  let (equivocation, fields) = announcementTraceFields elSt
   in TraceLeiosAnnouncementAccepted source equivocation fields age

-- | Do not relay (to downstream peers) an announcement whose slot's wall-clock
-- onset is older than this. See 'Announcements.ShouldRelay'.
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
-- wall-clock onset is older than this. See 'Announcements.ErrTooOld'.
--
-- Comfortably greater than 'maxAnnouncementAgeSend', so that an honest peer
-- (which stops relaying at that smaller bound) is never disconnected on account
-- of transmission time or clock skew.
--
-- TODO magic number; should be a config/RunNode option... or even a protocol
-- parameter?
maxAnnouncementAgeRecv :: NominalDiffTime
maxAnnouncementAgeRecv = 600 -- 10 minutes
