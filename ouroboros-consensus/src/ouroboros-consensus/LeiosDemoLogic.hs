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
import Data.Time.Clock (NominalDiffTime)
import qualified Data.Vector.Strict as V
import qualified Data.Vector.Strict.Mutable as MV
import Data.Word (Word16, Word64)
import LeiosDemoDb
  ( LeiosDbConnection
  , leiosDbBatchRetrieveTxs
  , leiosDbFilterMissingEbBodies
  , leiosDbFilterMissingTxs
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
import Ouroboros.Consensus.Storage.LedgerDB.Forker
  ( OCINStaleness (..)
  , ResolveLeiosBlock (..)
  )
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
  mSummary <- txCache.insertBody point.pointEbHash (Leios.serializeEbBody eb)
  forM_ mSummary $ traceWith tracer . TraceLeiosTxCacheEbBody point
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
      (Map (PeerId pid) (Map SlotNo (DList (TxHash, BytesSize, Map EbHash Int), DList (EbHash, BytesSize))))

emptyLeiosFetchDecisions :: LeiosFetchDecisions pid
emptyLeiosFetchDecisions = MkLeiosFetchDecisions Map.empty

-- | Filter outstanding work against the database.
-- Removes EB bodies and TXs that we already have in the DB.
-- This should be called before leiosFetchLogicIteration to avoid re-fetching.
--
-- NOTE: This is the minimal integration of DB filtering into the fetch logic.
-- The outstanding state tracks what we think is missing, but the DB is the
-- source of truth. This function reconciles the two by filtering out items
-- that have been acquired (possibly from other sources like forging).
filterMissingWork ::
  IOLike m =>
  LeiosDbConnection m ->
  LeiosOutstanding pid ->
  m (LeiosOutstanding pid)
filterMissingWork db outstanding = do
  -- Ask DB which of our "missing" EBs are actually still missing
  let ebPoints = Map.keys (Leios.missingEbBodies outstanding)
  stillMissingPoints <- leiosDbFilterMissingEbBodies db ebPoints
  let stillMissingPointSet = Set.fromList stillMissingPoints
      filteredMissingEbBodies = Map.restrictKeys (Leios.missingEbBodies outstanding) stillMissingPointSet
      acquiredEbHashes = [p.pointEbHash | p <- ebPoints, Set.notMember p stillMissingPointSet]
  -- Ask DB which of our "missing" TXs are actually still missing
  let allTxHashes =
        Set.toList $
          Set.fromList
            [ txHash
            | txs <- Map.elems (Leios.missingEbTxs outstanding)
            , (txHash, _) <- IntMap.elems txs
            ]
  stillMissingTxs <- leiosDbFilterMissingTxs db allTxHashes
  let stillMissingTxSet = Set.fromList stillMissingTxs
      filteredMissingEbTxs =
        Map.filter (not . IntMap.null) $
          Map.map
            (IntMap.filter (\(txHash, _) -> Set.member txHash stillMissingTxSet))
            (Leios.missingEbTxs outstanding)
      filteredReverseEbIndexByTx =
        Map.filterWithKey
          (\txHash _ -> Set.member txHash stillMissingTxSet)
          (Leios.reverseEbIndexByTx outstanding)
  pure $
    outstanding
      { Leios.missingEbBodies = filteredMissingEbBodies
      , Leios.missingEbTxs = filteredMissingEbTxs
      , Leios.reverseEbIndexByTx = filteredReverseEbIndexByTx
      , Leios.acquiredEbBodies =
          Leios.acquiredEbBodies outstanding `Set.union` Set.fromList acquiredEbHashes
      }

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
            Nothing -> error "impossible!"
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
    Map EbHash (Int, BytesSize) ->
    Set (PeerId pid) ->
    (LeiosOutstanding pid, LeiosFetchDecisions pid)
  goTx2 !acc !accNew targets point txBytesSize txHash txOffsets peerIds
    | Leios.requestedBytesSize acc >= Leios.maxRequestedBytesSize env -- we can't request anything
      =
        (acc, accNew)
    | Set.size peerIds < Leios.maxRequestsPerTx env -- we would like to request it from an additional peer
    -- TODO if requests list priority, does this limit apply even if the
    -- tx has only been requested at lower priorities?
    , Just (peerId, txOffsets') <- choosePeerTx peerIds acc txOffsets txBytesSize =
        -- there's a peer who offered it and we haven't already requested it from them
        let accNew' =
              MkLeiosFetchDecisions $
                Map.insertWith
                  (Map.unionWith (<>))
                  peerId
                  (Map.singleton point.pointSlotNo (DList.singleton (txHash, txBytesSize, txOffsets'), DList.empty))
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
    Map EbHash (Int, BytesSize) ->
    BytesSize ->
    Maybe (PeerId pid, Map EbHash Int)
  choosePeerTx peerIds acc txOffsets targetTxBytesSize =
    foldr (\a _ -> Just a) Nothing $
      [ (peerId, Map.map fst txOffsetsMatching)
      | (peerId, (_ebIds, ebIds)) <-
          Map.toList $ -- TODO prioritize/shuffle?
            (`Map.withoutKeys` peerIds) $ -- not already requested from this peer
              offerings
      , Map.findWithDefault 0 peerId (Leios.requestedBytesSizePerPeer acc)
          <= Leios.maxRequestedBytesSizePerPeer env
      , -- peer can be sent more requests
      let txOffsets' = txOffsets `Map.restrictKeys` ebIds
          -- Filter to entries whose recorded tx size matches the target's
          -- authority. The recorded size in 'reverseEbIndexByTx' can disagree
          -- across EBs (e.g. a malformed body delivered under a different EB
          -- hash); a single tx hash uniquely determines content, so any entry
          -- with a different size is bogus and must not carry the request.
          txOffsetsMatching =
            Map.filter (\(_, txBytesSize) -> txBytesSize == targetTxBytesSize) txOffsets'
      , -- peer has offered at least one EB closure recording this
      -- tx at the authoritative size
      not (Map.null txOffsetsMatching)
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
      -- group by EbId, sort by offset ascending
      $ Map.fromListWith IntMap.union
      $ [ (,) ebId $ IntMap.singleton txOffset (txHash, txBytesSize)
        | (txHash, txBytesSize, txOffsets) <- DList.toList txs
        , -- TODO somewhat arbitrarily choosing the freshest EbId here; merely
        -- something simple and sufficient for the demo
        let (ebId, txOffset) =
              case Map.lookupMax txOffsets of
                Nothing -> error "impossible!"
                Just x -> x
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
  do
    -- FIXME: 'ebBytesSize' here is the size we recorded from the peer
    -- offer at 'MsgLeiosBlockOffer' time (carried through the request),
    -- not the chain-authoritative 'leiosEbBytesSize' from the parent
    -- RB's 'headerLeiosAnnouncement'. EB announcements are not yet
    -- implemented; once they are, validate against the announced size
    -- so that a peer cannot poison this check by sending a bad-size
    -- offer first.
    let ebBytesSize' = leiosEbBytesSize eb
    when (ebBytesSize' /= ebBytesSize) $ do
      error $ "MsgLeiosBlock size mismatch: " <> show (ebBytesSize', ebBytesSize)
    let ebHash' = hashLeiosEb eb
    when (ebHash' /= ebHash) $ do
      error $ "MsgLeiosBlock hash mismatch: " <> show (ebHash', ebHash)
  -- ingest it
  MVar.modifyMVar_ outstandingVar $ \outstanding -> do
    let novel = not $ Set.member ebHash (Leios.acquiredEbBodies outstanding)
    when novel $ do
      -- TODO don't hold the outstanding mvar during this IO
      traceException tracer TraceLeiosPeerDbException $ do
        -- FIXME: Once proper EB announcements are wired in, the point
        -- MUST already be present here (announcement handling inserts
        -- it) and this should become an assertion. Today we still tolerate
        -- receiving an EB body without a prior announcement, so we insert
        -- the point idempotently as a stop-gap and trace a warning.
        traceWith ktracer $ TraceLeiosBlockPointMissing point
        leiosDbInsertEbPoint db point ebBytesSize
        completedByBody <- leiosDbInsertEbBody db point eb
        mSummary <- txCache.insertBody ebHash (Leios.serializeEbBody eb)
        forM_ mSummary $ traceWith ktracer . TraceLeiosTxCacheEbBody point
        traceWith ktracer $ TraceLeiosBlockAcquired point
        forM_ completedByBody $ traceWith ktracer . TraceLeiosBlockTxsAcquired
    -- update NodeKernel state
    --
    -- 'refundEbRequest' reverses this peer's per-request accounting (but skips
    -- it if the peer has already been cancelled in bulk by a disconnect); the
    -- global acquisition state below is updated unconditionally, since we did
    -- receive the EB.
    let !outstanding' =
          refundEbRequest peerId ebHash ebBytesSize $
            if novel
              then
                outstanding
                  { Leios.acquiredEbBodies = Set.insert ebHash (Leios.acquiredEbBodies outstanding)
                  , Leios.missingEbBodies = Map.delete point (Leios.missingEbBodies outstanding)
                  , Leios.blockingPerEb =
                      Map.insert
                        point
                        (let MkLeiosEb v = eb in V.length v)
                        (Leios.blockingPerEb outstanding)
                  , Leios.missingEbTxs =
                      Map.insert
                        point
                        ( V.ifoldl
                            (\acc i x -> IntMap.insert i x acc)
                            IntMap.empty
                            (let MkLeiosEb v = eb in v)
                        )
                        (Leios.missingEbTxs outstanding)
                  , Leios.reverseEbIndexByTx =
                      V.ifoldl
                        ( \acc i (txHash, txBytesSize) ->
                            Map.insertWith Map.union txHash (Map.singleton ebHash (i, txBytesSize)) acc
                        )
                        (Leios.reverseEbIndexByTx outstanding)
                        (let MkLeiosEb v = eb in v)
                  }
              else outstanding
    pure outstanding'
  void $ MVar.tryPutMVar readyVar ()
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
  let ebHash = point.pointEbHash
      txBytess = V.map cbor txs
  do
    when (V.length txs /= V.length txHashes) $ do
      error $ "MsgLeiosBlockTxs length mismatch: " ++ show (V.length txs, V.length txHashes)
    let txHashes' = V.map hashLeiosTx txs
    when (txHashes' /= txHashes) $ do
      let mismatches =
            V.toList $
              V.findIndices id $
                V.zipWith (/=) txHashes txHashes'
      error $ "MsgLeiosBlockTxs hash mismatches: " ++ show mismatches
  let nextOffset = \case
        [] -> Nothing
        (idx, bitmap) : k -> case popLeftmostOffset bitmap of
          Nothing -> nextOffset k
          Just (i, bitmap') ->
            Just (64 * fromIntegral idx + i, (idx, bitmap') : k)
      offsets = unfoldr nextOffset bitmaps
  -- ingest
  traceException tracer TraceLeiosPeerDbException $ do
    completed <- leiosDbInsertTxs db (V.toList $ V.zip txHashes txBytess)
    forM_ completed $ traceWith ktracer . TraceLeiosBlockTxsAcquired
    -- crucially: add the txs to the TxCacheIndex _after_ they've been written
    -- to the LeiosDb, since it's currently what the TxCacheIndex is indexing
    withLockedInsertUnappliedTx txCache $ \z step ->
      foldM (\acc txh -> step acc txh ()) z txHashes
  -- update NodeKernel state
  MVar.modifyMVar_ outstandingVar $ \outstanding -> do
    let (requestedTxPeers', reverseEbIndexByTx', txsBytesSize) =
          ( \f ->
              V.foldl
                f
                ( Leios.requestedTxPeers outstanding
                , Leios.reverseEbIndexByTx outstanding
                , 0
                )
                (txHashes `V.zip` txBytess)
          )
            $ \(!accReqs, !accOffsetss, !accSz) (txHash, txBytes) ->
              ( Map.update (delIf Set.null . Set.delete peerId) txHash accReqs
              , Map.update (delIf Map.null . Map.delete ebHash) txHash accOffsetss
              , accSz + BS.length txBytes
              )
    let offsetsSet = IntSet.fromList offsets
        -- the requests that this MsgLeiosBlockTxs was the first to resolve
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
              { Leios.missingEbTxs =
                  Map.update
                    (delIf IntMap.null . (`IntMap.withoutKeys` offsetsSet))
                    point
                    (Leios.missingEbTxs outstanding)
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

-- | Update a peer's LeiosFetch state as if its LeiosNotify client had offered
-- the given EB — i.e. as a 'MsgLeiosBlockOffer' (EB body) plus a
-- 'MsgLeiosBlockTxsOffer' (EB txs).
--
-- This is the LeiosFetch-side effect of a CertRB header arriving via ChainSync:
-- given the peer's (already-resolved) LeiosNotify vars and the EB the CertRB
-- certifies (the announcement recorded in the predecessor's chain-dep state,
-- plus the EB's on-the-wire body size), we record the body as missing and this
-- peer as offering both the body and its txs, then wake the fetch logic. (The
-- block-aware decision of /whether/ to call this — recognising the CertRB,
-- extracting its announcement, and waiting for the peer's LeiosNotify vars to
-- register — stays in 'checkMsgRollForwardForLeiosOffers'.)
leiosCertRbOffer ::
  IOLike m =>
  ( MVar m (LeiosOutstanding pid)
  , MVar m ()
  ) ->
  LeiosPeerVars m ->
  -- | The EB the CertRB certifies: its point and on-the-wire body size.
  (LeiosPoint, BytesSize) ->
  m ()
leiosCertRbOffer (outstandingVar, readyVar) peerVars (point, ebBytesSize) = do
  let MkLeiosPoint _ebSlot ebHash = point
  -- As if 'MsgLeiosBlockOffer': record the EB body as missing.
  MVar.modifyMVar_ outstandingVar $ \outstanding ->
    pure $
      if Set.member ebHash (Leios.acquiredEbBodies outstanding)
        then outstanding
        else
          outstanding
            { Leios.missingEbBodies =
                Map.insert point ebBytesSize (Leios.missingEbBodies outstanding)
            }
  -- As if 'MsgLeiosBlockOffer' (body) and 'MsgLeiosBlockTxsOffer' (txs): record
  -- this peer as offering both.
  MVar.modifyMVar_ (Leios.offerings peerVars) $ \(offers1, offers2) -> do
    let !offers1' = Set.insert ebHash offers1
        !offers2' = Set.insert ebHash offers2
    pure (offers1', offers2')
  void $ MVar.tryPutMVar readyVar ()

-----

-- | The offer-side handling of a 'MsgRollForward': when the header is a CertRB
-- ('headerContainsLeiosCert'), record this peer as offering the EB it certifies
-- (via 'leiosCertRbOffer'), reading that EB from the predecessor's chain-dep
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
      leiosCertRbOffer kernelVars peerVars announcement

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
            recordAnnouncedEb kernelVars (point, Leios.announcementEbBodySize fields)
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

-- | Record a validated, newly-announced EB body as missing, with its
-- authoritative (forger-signed) size. First-seen wins: a no-op if the body is
-- already acquired or already recorded.
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
  MkLeiosPoint _ebSlot ebHash = point

  upd outstanding =
    if Set.member ebHash (Leios.acquiredEbBodies outstanding)
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
