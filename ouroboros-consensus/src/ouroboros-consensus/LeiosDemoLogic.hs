{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LeiosDemoLogic (module LeiosDemoLogic) where

import Cardano.Binary (decodeFullDecoder', serialize')
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Prelude (first)
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadMVar (MVar)
import qualified Control.Concurrent.Class.MonadMVar as MVar
import Control.Concurrent.Class.MonadSTM.Strict (StrictTVar)
import qualified Control.Concurrent.Class.MonadSTM.Strict as StrictSTM
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Tracer (Tracer, traceWith)
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Foldable (forM_)
import Data.Functor (void)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.List (unfoldr)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Word (Word16, Word64)
import LeiosDemoDb (LeiosDbHandle (..))
import qualified LeiosDemoOnlyTestFetch as LF
import LeiosDemoTypes
  ( BytesSize
  , EbHash (..)
  , LeiosBlockRequest (..)
  , LeiosBlockTxsRequest (..)
  , LeiosEb (..)
  , LeiosEbBodies
  , LeiosFetchRequest (..)
  , LeiosFetchStaticEnv
  , LeiosNotification (..)
  , LeiosOutstanding
  , LeiosPoint (..)
  , LeiosTx (..)
  , PeerId (..)
  , TraceLeiosKernel (..)
  , TraceLeiosPeer (..)
  , TxHash (..)
  , maxTxsPerEb
  )
import qualified LeiosDemoTypes as Leios
import Ouroboros.Consensus.Util.IOLike (IOLike)

loadEbBodies :: IOLike m => LeiosDbHandle m -> m LeiosEbBodies
loadEbBodies db = do
  points <- leiosDbScanEbPoints db
  pure
    Leios.emptyLeiosEbBodies
      { Leios.ebPoints = IntMap.fromList $ map (first fromEnum) points
      }

-----

data SomeLeiosFetchContext m
  = MkSomeLeiosFetchContext !(LeiosFetchContext m)

data LeiosFetchContext m = MkLeiosFetchContext
  { leiosDb :: !(LeiosDbHandle m)
  , leiosEbBuffer :: !(MV.MVector (PrimState m) (TxHash, BytesSize))
  , leiosEbTxsBuffer :: !(MV.MVector (PrimState m) LeiosTx)
  , leiosWriteLock :: !(MVar m ())
  , readLeiosEbBodies :: !(m LeiosEbBodies)
  }

newLeiosFetchContext ::
  PrimMonad m =>
  MVar m () ->
  LeiosDbHandle m ->
  m LeiosEbBodies ->
  m (LeiosFetchContext m)
newLeiosFetchContext leiosWriteLock leiosDb readLeiosEbBodies = do
  -- each LeiosFetch server calls this when it initializes
  leiosEbBuffer <- MV.new maxTxsPerEb
  leiosEbTxsBuffer <- MV.new maxTxsPerEb
  pure
    MkLeiosFetchContext{leiosDb, leiosEbBuffer, leiosEbTxsBuffer, leiosWriteLock, readLeiosEbBodies}

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
  LF.MsgLeiosBlockTxsRequest p bitmaps -> do
    traceWith tracer $ MkTraceLeiosPeer $ "[start] MsgLeiosBlockTxsRequest " <> Leios.prettyLeiosPoint p
    x <- msgLeiosBlockTxsRequest tracer leiosContext p bitmaps
    traceWith tracer $ MkTraceLeiosPeer $ "[done] MsgLeiosBlockTxsRequest " <> Leios.prettyLeiosPoint p
    pure $ LF.MsgLeiosBlockTxs x

msgLeiosBlockRequest ::
  IOLike m =>
  Tracer m TraceLeiosPeer ->
  LeiosFetchContext m ->
  LeiosPoint ->
  m LeiosEb
msgLeiosBlockRequest _tracer leiosContext MkLeiosPoint{pointEbHash} = do
  let MkLeiosFetchContext{leiosDb = db, leiosEbBuffer = buf, leiosWriteLock} = leiosContext
  n <- MVar.withMVar leiosWriteLock $ \() -> do
    -- get the EB items using new db
    items <- leiosDbLookupEbBody db pointEbHash
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
  let MkLeiosFetchContext{leiosDb = db, leiosEbTxsBuffer = buf, leiosWriteLock} = leiosContext
  do
    let idxs = map fst bitmaps
    let idxLimit = maxTxsPerEb `div` 64
    when (any (== 0) $ map snd bitmaps) $ do
      error "A bitmap is zero"
    when (flip any idxs (> fromIntegral idxLimit)) $ do
      error $ "An offset exceeds the theoretical limit " <> show idxLimit
    when (not $ and $ zipWith (<) idxs (tail idxs)) $ do
      error "Offsets not strictly ascending"
  let nextOffset = \case
        [] -> Nothing
        (idx, bitmap) : k -> case popLeftmostOffset bitmap of
          Nothing -> nextOffset k
          Just (i, bitmap') ->
            Just (64 * fromIntegral idx + i, (idx, bitmap') : k)
      txOffsets = unfoldr nextOffset bitmaps
  n <- MVar.withMVar leiosWriteLock $ \() -> do
    -- Use new db to batch retrieve transactions
    results <- leiosDbBatchRetrieveTxs db point.pointEbHash txOffsets
    -- Process results and write to buffer
    let loop !i [] = pure i
        loop !i ((offset, _txHash, mbTxBytes) : rest) = do
          case mbTxBytes of
            Nothing -> error $ "Missing txBytes for offset " ++ show offset
            Just txBytes -> do
              tx <- case decodeFullDecoder' (fromString "txBytes column") Leios.decodeLeiosTx txBytes of
                Left err ->
                  error $
                    "Failed to deserialize txBytes column: "
                      ++ Leios.prettyLeiosPoint point
                      ++ " "
                      ++ show (offset, err)
                Right tx -> pure tx
              MV.write buf i tx
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

leiosFetchLogicIteration ::
  forall pid.
  Ord pid =>
  LeiosFetchStaticEnv ->
  (LeiosEbBodies, Map (PeerId pid) (Set EbHash, Set EbHash)) ->
  LeiosOutstanding pid ->
  (LeiosOutstanding pid, LeiosFetchDecisions pid)
leiosFetchLogicIteration env (ebBodies, offerings) =
  \acc ->
    go1 acc emptyLeiosFetchDecisions $
      expand $
        Map.toDescList $
          Map.map Left (Leios.missingEbBodies ebBodies) `Map.union` Map.map Right (Leios.missingEbTxs acc)
 where
  expand = \case
    [] -> []
    (point, Left ebBytesSize) : vs -> Left (point, ebBytesSize) : expand vs
    (point, Right v) : vs ->
      [Right (point, txOffset, txHash) | (txOffset, (txHash, _txBytesSize)) <- IntMap.toAscList v]
        <> expand vs

  go1 ::
    LeiosOutstanding pid ->
    LeiosFetchDecisions pid ->
    [Either (LeiosPoint, BytesSize) (LeiosPoint, IntSet.Key, TxHash)] ->
    (LeiosOutstanding pid, LeiosFetchDecisions pid)
  go1 !acc !accNew = \case
    [] ->
      (acc, accNew)
    Left (point, ebBytesSize) : targets
      | let peerIds :: Set (PeerId pid)
            peerIds = Map.findWithDefault Set.empty point.pointEbHash (Leios.requestedEbPeers acc) ->
          goEb2 acc accNew targets point ebBytesSize peerIds
    Right (point, txOffset, txHash) : targets
      | Just _ <- Map.lookup point (Leios.toCopy acc) >>= IntMap.lookup txOffset ->
          -- it's already scheduled to be copied from TxCache
          go1 acc accNew targets
      | Just txBytesSize <- Map.lookup txHash (Leios.cachedTxs acc) -> -- it's in the TxCache
          let full =
                Leios.toCopyBytesSize acc >= Leios.maxToCopyBytesSize env
                  || Leios.toCopyCount acc >= Leios.maxToCopyCount env
              acc' =
                if full
                  then acc
                  else
                    acc
                      { Leios.missingEbTxs =
                          Map.alter
                            ( \case
                                Nothing -> error "impossible!"
                                Just x -> delIf IntMap.null $ IntMap.delete txOffset x
                            )
                            point
                            (Leios.missingEbTxs acc)
                      , Leios.txOffsetss =
                          Map.alter
                            ( \case
                                Nothing -> error "impossible!"
                                Just x -> delIf Map.null $ Map.delete point.pointEbHash x
                            )
                            txHash
                            (Leios.txOffsetss acc)
                      , Leios.toCopy =
                          Map.insertWith
                            IntMap.union
                            point
                            (IntMap.singleton txOffset txBytesSize)
                            (Leios.toCopy acc)
                      , Leios.toCopyBytesSize = Leios.toCopyBytesSize acc + txBytesSize
                      , Leios.toCopyCount = Leios.toCopyCount acc + 1
                      }
           in go1 acc' accNew targets
      | otherwise ->
          let !txOffsets = case Map.lookup txHash (Leios.txOffsetss acc) of
                Nothing -> error "impossible!"
                Just x -> x
              peerIds :: Set (PeerId pid)
              peerIds = Map.findWithDefault Set.empty txHash (Leios.requestedTxPeers acc)
           in goTx2 acc accNew targets point txHash txOffsets peerIds

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

  goTx2 !acc !accNew targets point txHash txOffsets peerIds
    | Leios.requestedBytesSize acc >= Leios.maxRequestedBytesSize env -- we can't request anything
      =
        (acc, accNew)
    | Set.size peerIds < Leios.maxRequestsPerTx env -- we would like to request it from an additional peer
    -- TODO if requests list priority, does this limit apply even if the
    -- tx has only been requested at lower priorities?
    , Just (peerId, txOffsets') <- choosePeerTx peerIds acc txOffsets =
        -- there's a peer who offered it and we haven't already requested it from them
        let txBytesSize = case Map.lookupMax txOffsets' of
              Nothing -> error "impossible!"
              Just (_ebHash, txOffset) -> case Map.lookup point (Leios.missingEbTxs acc) of
                Nothing -> error "impossible!"
                Just v -> case IntMap.lookup txOffset v of
                  Nothing -> error "impossible!"
                  Just (_txHash, x) -> x
            accNew' =
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
         in goTx2 acc' accNew' targets point txHash txOffsets peerIds'
    | otherwise =
        go1 acc accNew targets

  choosePeerTx ::
    Set (PeerId pid) -> LeiosOutstanding pid -> Map EbHash Int -> Maybe (PeerId pid, Map EbHash Int)
  choosePeerTx peerIds acc txOffsets =
    foldr (\a _ -> Just a) Nothing $
      [ (peerId, txOffsets')
      | (peerId, (_ebIds, ebIds)) <-
          Map.toList $ -- TODO prioritize/shuffle?
            (`Map.withoutKeys` peerIds) $ -- not already requested from this peer
              offerings
      , Map.findWithDefault 0 peerId (Leios.requestedBytesSizePerPeer acc)
          <= Leios.maxRequestedBytesSizePerPeer env
      , -- peer can be sent more requests
      let txOffsets' = txOffsets `Map.restrictKeys` ebIds
      , not $ Map.null txOffsets' -- peer has offered an EB closure that includes this tx
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

nextLeiosFetchClientCommand ::
  forall pid m.
  ( Ord pid
  , IOLike m
  ) =>
  Tracer m TraceLeiosKernel ->
  Tracer m TraceLeiosPeer ->
  StrictSTM.STM m Bool ->
  ( MVar m ()
  , MVar m LeiosEbBodies
  , MVar m (LeiosOutstanding pid)
  , MVar m ()
  , MVar m (Map (PeerId pid) (StrictTVar m (Map SlotNo (Seq LeiosNotification))))
  ) ->
  LeiosDbHandle m ->
  PeerId pid ->
  StrictTVar m (Seq LeiosFetchRequest) ->
  m
    ( Either
        (m (Either () (LF.SomeLeiosFetchJob LeiosPoint LeiosEb LeiosTx m)))
        (Either () (LF.SomeLeiosFetchJob LeiosPoint LeiosEb LeiosTx m))
    )
nextLeiosFetchClientCommand ktracer tracer stopSTM kernelVars db peerId reqsVar = do
  f (pure Nothing) (pure . Just) >>= \case
    Just x -> pure $ Right x
    Nothing -> pure $ Left $ f StrictSTM.retry pure
 where
  f ::
    StrictSTM.STM m r ->
    (Either () (LF.SomeLeiosFetchJob LeiosPoint LeiosEb LeiosTx m) -> StrictSTM.STM m r) ->
    m r
  f retry_ pure_ = StrictSTM.atomically $ do
    stopSTM >>= \case
      True -> pure_ $ Left ()
      False ->
        StrictSTM.readTVar reqsVar >>= \case
          Seq.Empty -> retry_
          req Seq.:<| reqs -> do
            StrictSTM.writeTVar reqsVar reqs
            pure_ $ Right $ g req

  g = \case
    LeiosBlockRequest req@(MkLeiosBlockRequest p _ebBytesSize) ->
      LF.MkSomeLeiosFetchJob
        (LF.MsgLeiosBlockRequest p)
        ( pure $ \(LF.MsgLeiosBlock eb) ->
            msgLeiosBlock ktracer tracer kernelVars db peerId req eb
        )
    LeiosBlockTxsRequest req@(MkLeiosBlockTxsRequest p bitmaps _txHashes) ->
      LF.MkSomeLeiosFetchJob
        (LF.MsgLeiosBlockTxsRequest p bitmaps)
        ( pure $ \(LF.MsgLeiosBlockTxs txs) -> do
            msgLeiosBlockTxs ktracer tracer kernelVars db peerId req txs
        )

-----

msgLeiosBlock ::
  ( Ord pid
  , IOLike m
  ) =>
  Tracer m TraceLeiosKernel ->
  Tracer m TraceLeiosPeer ->
  ( MVar m ()
  , MVar m LeiosEbBodies
  , MVar m (LeiosOutstanding pid)
  , MVar m ()
  , MVar m (Map (PeerId pid) (StrictTVar m (Map SlotNo (Seq LeiosNotification))))
  ) ->
  LeiosDbHandle m ->
  PeerId pid ->
  LeiosBlockRequest ->
  LeiosEb ->
  m ()
msgLeiosBlock ktracer tracer (writeLock, ebBodiesVar, outstandingVar, readyVar, notificationVars) db peerId req eb = do
  -- validate it
  let MkLeiosBlockRequest point ebBytesSize = req
  traceWith tracer $ MkTraceLeiosPeer $ "[start] MsgLeiosBlock " <> Leios.prettyLeiosPoint point
  let MkLeiosPoint ebSlot ebHash = point
  do
    let ebBytes :: ByteString
        ebBytes = serialize' $ Leios.encodeLeiosEb eb
    -- REVIEW: use 'leiosEbBytesSize'?
    let ebBytesSize' = BS.length ebBytes
    when (ebBytesSize' /= fromIntegral ebBytesSize) $ do
      error $ "MsgLeiosBlock size mismatch: " <> show (ebBytesSize', ebBytesSize)
    -- REVIEW: use 'hashLeiosEb'?
    let ebHash' :: EbHash
        ebHash' = MkEbHash $ Hash.hashToBytes $ Hash.hashWith @Leios.HASH id ebBytes
    when (ebHash' /= ebHash) $ do
      error $ "MsgLeiosBlock hash mismatch: " <> show (ebHash', ebHash)
  -- ingest it
  novel <- MVar.modifyMVar ebBodiesVar $ \ebBodies -> do
    let novel = not $ Set.member ebHash (Leios.acquiredEbBodies ebBodies)
    when novel $ MVar.withMVar writeLock $ \() -> do
      traceWith ktracer $ TraceLeiosBlockAcquired point
      -- TODO don't hold the ebBodies mvar during this IO
      leiosDbInsertEbBody db point eb
    -- update NodeKernel state
    let !ebBodies' =
          if not novel
            then ebBodies
            else
              ebBodies
                { Leios.acquiredEbBodies = Set.insert ebHash (Leios.acquiredEbBodies ebBodies)
                , Leios.missingEbBodies = Map.delete point (Leios.missingEbBodies ebBodies)
                }
    pure (ebBodies', novel)
  MVar.modifyMVar_ outstandingVar $ \outstanding -> do
    let !outstanding' =
          outstanding
            { Leios.blockingPerEb =
                if not novel
                  then Leios.blockingPerEb outstanding
                  else
                    Map.insert
                      point
                      (let MkLeiosEb v = eb in V.length v)
                      (Leios.blockingPerEb outstanding)
            , Leios.missingEbTxs =
                if not novel
                  then Leios.missingEbTxs outstanding
                  else
                    Map.insert
                      point
                      ( V.ifoldl
                          (\acc i x -> IntMap.insert i x acc)
                          IntMap.empty
                          (let MkLeiosEb v = eb in v)
                      )
                      (Leios.missingEbTxs outstanding)
            , Leios.txOffsetss =
                if not novel
                  then Leios.txOffsetss outstanding
                  else
                    V.ifoldl
                      ( \acc i (txHash, _txBytesSize) ->
                          Map.insertWith Map.union txHash (Map.singleton ebHash i) acc
                      )
                      (Leios.txOffsetss outstanding)
                      (let MkLeiosEb v = eb in v)
            , Leios.requestedBytesSize = Leios.requestedBytesSize outstanding - ebBytesSize
            , Leios.requestedBytesSizePerPeer =
                Map.alter
                  ( \case
                      Nothing -> error "impossible!"
                      Just x -> delIf (== 0) $ x - ebBytesSize
                  )
                  peerId
                  (Leios.requestedBytesSizePerPeer outstanding)
            , Leios.requestedEbPeers =
                Map.update (delIf Set.null . Set.delete peerId) ebHash (Leios.requestedEbPeers outstanding)
            }
    pure outstanding'
  void $ MVar.tryPutMVar readyVar ()
  traceWith tracer $ MkTraceLeiosPeer $ "[done] MsgLeiosBlock " <> Leios.prettyLeiosPoint point

-----

delIf :: (a -> Bool) -> a -> Maybe a
delIf predicate x = if predicate x then Nothing else Just x

-----

msgLeiosBlockTxs ::
  ( Ord pid
  , IOLike m
  ) =>
  Tracer m TraceLeiosKernel ->
  Tracer m TraceLeiosPeer ->
  ( MVar m ()
  , MVar m LeiosEbBodies
  , MVar m (LeiosOutstanding pid)
  , MVar m ()
  , MVar m (Map (PeerId pid) (StrictTVar m (Map SlotNo (Seq LeiosNotification))))
  ) ->
  LeiosDbHandle m ->
  PeerId pid ->
  LeiosBlockTxsRequest ->
  V.Vector LeiosTx ->
  m ()
msgLeiosBlockTxs ktracer tracer (writeLock, _ebBodiesVar, outstandingVar, readyVar, notificationVars) db peerId req txs = do
  traceWith tracer $ MkTraceLeiosPeer $ "[start] " ++ Leios.prettyLeiosBlockTxsRequest req
  -- validate it
  let MkLeiosBlockTxsRequest point bitmaps txHashes = req
  let ebHash = point.pointEbHash
  --    forM_ txHashes $ \txHash -> do
  --        traceWith tracer $ MkTraceLeiosPeer $ "leiosRspTxHash: " ++ Leios.prettyTxHash txHash
  let txBytess :: V.Vector ByteString
      txBytess = V.map (serialize' . Leios.encodeLeiosTx) txs
  do
    when (V.length txs /= V.length txHashes) $ do
      error $ "MsgLeiosBlockTxs length mismatch: " ++ show (V.length txs, V.length txHashes)
    let rehash :: ByteString -> Hash.Hash Leios.HASH ByteString
        rehash = Hash.hashWith id
    let txHashes' = V.map (MkTxHash . Hash.hashToBytes . rehash) txBytess
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
  MVar.withMVar writeLock $ \() -> do
    -- REVIEW: These operations were previously wrapped in a single dbWithBEGIN
    -- transaction but are now separate db calls. For SQLite, each operation is
    -- transactional, but they're not in a single atomic transaction anymore.
    -- This may need to be fixed by adding a batch update operation to the db or
    -- ensuring transactional semantics. Use new db for both txCache and ebTxs
    -- updates
    leiosDbUpdateEbTx db ebHash (zip offsets (V.toList txBytess))
    -- FIXME: Why is tx cache separate? it results in the same bytes stored twice in the DB
    -- Insert into txCache (expiry set to 0 for now - TODO: use proper expiry)
    forM_ (txHashes `V.zip` txBytess) $ \(txHash, txBytes) -> do
      leiosDbInsertTxCache db txHash txBytes (fromIntegral $ BS.length txBytes) 0
  -- update NodeKernel state
  newNotifications <- MVar.modifyMVar outstandingVar $ \outstanding -> do
    let (requestedTxPeers', cachedTxs', txOffsetss', txsBytesSize) =
          ( \f ->
              V.foldl
                f
                ( Leios.requestedTxPeers outstanding
                , Leios.cachedTxs outstanding
                , Leios.txOffsetss outstanding
                , 0
                )
                (txHashes `V.zip` txBytess)
          )
            $ \(!accReqs, !accCache, !accOffsetss, !accSz) (txHash, txBytes) ->
              id $
                ( Map.update (delIf Set.null . Set.delete peerId) txHash accReqs
                , Map.insert txHash (fromIntegral (BS.length txBytes)) accCache
                , Map.update (delIf Map.null . Map.delete ebHash) txHash accOffsetss
                , accSz + BS.length txBytes
                )
    let offsetsSet = IntSet.fromList offsets
        checkBeat :: forall a. Map LeiosPoint (IntMap a) -> IntMap a
        checkBeat x =
          (`IntMap.restrictKeys` offsetsSet) $
            Map.findWithDefault
              IntMap.empty
              point
              x
        -- the requests that this MsgLeiosBlockTxs was the first to resolve
        beatOtherPeers = checkBeat $ Leios.missingEbTxs outstanding
        -- the currently scheduled 'toCopy' operations that this
        -- MsgLeiosBlockTxs just won the race against
        beatToCopy = checkBeat $ Leios.toCopy outstanding
    let !outstanding' =
          outstanding
            { Leios.cachedTxs = cachedTxs'
            , Leios.missingEbTxs =
                Map.update
                  (delIf IntMap.null . (`IntMap.withoutKeys` offsetsSet))
                  point
                  (Leios.missingEbTxs outstanding)
            , Leios.txOffsetss = txOffsetss'
            , Leios.blockingPerEb =
                if IntMap.null beatOtherPeers && IntMap.null beatToCopy
                  then Leios.blockingPerEb outstanding
                  else
                    Map.alter
                      ( \case
                          Nothing -> Nothing
                          Just x -> delIf (== 0) $ x - IntMap.size beatOtherPeers - IntMap.size beatToCopy
                      )
                      point
                      (Leios.blockingPerEb outstanding)
            , Leios.requestedBytesSize =
                Leios.requestedBytesSize outstanding - fromIntegral txsBytesSize
            , Leios.requestedBytesSizePerPeer =
                Map.alter
                  ( \case
                      Nothing -> error "impossible!"
                      Just x -> delIf (== 0) $ x - fromIntegral txsBytesSize
                  )
                  peerId
                  (Leios.requestedBytesSizePerPeer outstanding)
            , Leios.requestedTxPeers = requestedTxPeers'
            , Leios.toCopy =
                if IntMap.null beatToCopy
                  then Leios.toCopy outstanding
                  else
                    Map.alter
                      ( \case
                          Nothing -> Nothing
                          Just x ->
                            delIf IntMap.null $
                              x `IntMap.difference` beatToCopy
                      )
                      point
                      (Leios.toCopy outstanding)
            , Leios.toCopyBytesSize =
                Leios.toCopyBytesSize outstanding - sum beatToCopy
            , Leios.toCopyCount =
                Leios.toCopyCount outstanding - IntMap.size beatToCopy
            }
    let newNotifications =
          Map.keys $
            Leios.blockingPerEb outstanding `Map.difference` Leios.blockingPerEb outstanding'
    pure (outstanding', newNotifications)
  void $ MVar.tryPutMVar readyVar ()
  -- FIXME: this must happen in the LeiosDbHandle now
  -- when (not $ null newNotifications) $ do
  --   notifications <- fmap Map.fromList . forM newNotifications $ \completedPoint -> do
  --     traceWith ktracer $ TraceLeiosBlockTxsAcquired completedPoint
  --     pure (completedPoint.pointSlotNo, Seq.singleton (LeiosOfferBlockTxs completedPoint))

  --   vars <- MVar.readMVar notificationVars
  --   forM_ vars $ \var -> do
  --     traceWith tracer $
  --       MkTraceLeiosPeer $
  --         "leiosNotificationsBlockTxs!: " ++ unwords (map Leios.prettyLeiosPoint newNotifications)
  --     StrictSTM.atomically $ do
  --       x <- StrictSTM.readTVar var
  --       let !x' = Map.unionWith (<>) x notifications
  --       StrictSTM.writeTVar var x'
  traceWith tracer $ MkTraceLeiosPeer $ "[done] " ++ Leios.prettyLeiosBlockTxsRequest req

-----

doCacheCopy ::
  IOLike m =>
  Tracer m TraceLeiosKernel ->
  LeiosDbHandle m ->
  ( MVar m ()
  , MVar m (LeiosOutstanding pid)
  , MVar m (Map (PeerId pid) (StrictTVar m (Map SlotNo (Seq LeiosNotification))))
  ) ->
  BytesSize ->
  m Bool
doCacheCopy tracer db (writeLock, outstandingVar, notificationVars) bytesSize = do
  copied <- do
    outstanding <- MVar.readMVar outstandingVar
    MVar.withMVar writeLock $ \() -> do
      leiosDbCopyFromTxCacheBatch db (Leios.toCopy outstanding) bytesSize
  (moreTodo, newNotifications) <- MVar.modifyMVar outstandingVar $ \outstanding -> do
    let usefulCopied =
          -- @copied@ might contain elements that were already accounted
          -- for by a @MsgLeiosBlockTxs@ that won the race. This
          -- intersection discards those.
          Map.intersectionWith
            IntMap.restrictKeys
            (Leios.toCopy outstanding)
            copied
    let !outstanding' =
          outstanding
            { Leios.blockingPerEb =
                Map.differenceWithKey
                  ( \_point count copiedEbHash ->
                      delIf (== 0) $ count - IntMap.size copiedEbHash
                  )
                  (Leios.blockingPerEb outstanding)
                  usefulCopied
            , Leios.toCopy =
                Map.differenceWithKey
                  ( \_ebId toCopy copiedEbId ->
                      delIf IntMap.null $ toCopy `IntMap.difference` copiedEbId
                  )
                  (Leios.toCopy outstanding)
                  usefulCopied
            , Leios.toCopyBytesSize =
                Leios.toCopyBytesSize outstanding - sum (Map.map sum usefulCopied)
            , Leios.toCopyCount =
                Leios.toCopyCount outstanding - sum (Map.map IntMap.size usefulCopied)
            }
    let newNotifications =
          Map.keys $
            Leios.blockingPerEb outstanding `Map.difference` Leios.blockingPerEb outstanding'
    pure (outstanding', (0 /= Leios.toCopyCount outstanding', newNotifications))
  -- FIXME: this must happen in the LeiosDbHandle now
  -- when (not $ null newNotifications) $ do
  --   let notifications =
  --         Map.fromList $
  --           [ (p.pointSlotNo, Seq.singleton (LeiosOfferBlockTxs p))
  --           | p <- newNotifications
  --           ]
  --   traceWith tracer $
  --     MkTraceLeiosKernel $
  --       "leiosNotificationsCopy: " ++ unwords (map Leios.prettyLeiosPoint newNotifications)
  --   vars <- MVar.readMVar notificationVars
  --   forM_ vars $ \var -> do
  --     traceWith tracer $
  --       MkTraceLeiosKernel $
  --         "leiosNotificationsCopy!: " ++ unwords (map Leios.prettyLeiosPoint newNotifications)
  --     StrictSTM.atomically $ do
  --       x <- StrictSTM.readTVar var
  --       let !x' = Map.unionWith (<>) x notifications
  --       StrictSTM.writeTVar var x'
  pure moreTodo
