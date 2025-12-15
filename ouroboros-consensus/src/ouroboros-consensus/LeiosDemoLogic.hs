{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LeiosDemoLogic (module LeiosDemoLogic) where

import           Cardano.Binary (decodeFullDecoder', serialize')
import qualified Cardano.Crypto.Hash as Hash
import           Cardano.Slotting.Slot (SlotNo (..))
import           Control.Concurrent.Class.MonadMVar (MVar, MonadMVar)
import qualified Control.Concurrent.Class.MonadMVar as MVar
import           Control.Concurrent.Class.MonadSTM (MonadSTM)
import           Control.Concurrent.Class.MonadSTM.Strict (StrictTVar)
import qualified Control.Concurrent.Class.MonadSTM.Strict as StrictSTM
import           Control.Monad (foldM, when)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Tracer (Tracer, traceWith)
import qualified Data.Bits as Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.DList (DList)
import qualified Data.DList as DList
import           Data.Foldable (forM_)
import           Data.Functor (void, (<&>))
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.List (unfoldr)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (fromString)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Data.Word (Word16, Word64)
import qualified Database.SQLite3.Direct as DB
import qualified LeiosDemoOnlyTestFetch as LF
import qualified LeiosDemoOnlyTestNotify as LN
import           LeiosDemoTypes (BytesSize, EbHash (..), EbId (..),
                     LeiosBlockRequest (..), LeiosBlockTxsRequest (..),
                     LeiosDb (..), LeiosEb (..), LeiosEbBodies,
                     LeiosFetchRequest (..), LeiosFetchStaticEnv,
                     LeiosNotification (..), LeiosOutstanding, LeiosPoint (..),
                     LeiosTx (..), PeerId (..), TraceLeiosKernel (..),
                     TraceLeiosPeer (..), TxHash (..), dbBindBlob, dbBindInt64,
                     dbColumnBlob, dbColumnInt64, dbExec, dbReset, dbStep,
                     dbStep1, dbWithBEGIN, dbWithPrepare)
import qualified LeiosDemoTypes as Leios
import           Ouroboros.Consensus.Util.IOLike (IOLike)

ebIdSlot :: EbId -> SlotNo
ebIdSlot (MkEbId y) =
    SlotNo (fromIntegral y `div` 1000)

ebIdToPoint :: EbId -> LeiosEbBodies -> Maybe LeiosPoint
ebIdToPoint (MkEbId i) x =
        (\h -> MkLeiosPoint (ebIdSlot (MkEbId i)) h)
    <$>
        IntMap.lookup i (Leios.ebPointsInverse x)

ebIdToPointM :: MonadMVar m => MVar m LeiosEbBodies -> EbId -> m (Maybe LeiosPoint)
ebIdToPointM mvar ebId =
    MVar.readMVar mvar <&> ebIdToPoint ebId

ebIdFromPoint :: LeiosPoint -> LeiosEbBodies -> (EbId, Maybe LeiosEbBodies)
ebIdFromPoint p x =
    case IntMap.lookup islot (Leios.ebPoints x) of
        Just m -> case Map.lookup ebHash m of
            Just y  -> (y, Nothing)
            Nothing -> gen $ MkEbId $ zero + 99 - Map.size m
        Nothing -> gen $ MkEbId $ zero + 99
  where
    MkLeiosPoint ebSlot ebHash = p
    SlotNo wslot = ebSlot
    islot = fromIntegral (wslot :: Word64)

    zero = fromIntegral (wslot * 1000) :: Int

    gen y =
        let !x' = x {
                  Leios.ebPoints =
                      IntMap.insertWith
                          Map.union
                          islot
                          (Map.singleton ebHash y)
                          (Leios.ebPoints x)
               ,
                  Leios.ebPointsInverse =
                      let MkEbId z = y
                      in
                      IntMap.insert z ebHash (Leios.ebPointsInverse x)
               }
        in (y, Just x')

ebIdFromPointM :: MonadMVar m => MVar m LeiosEbBodies -> LeiosPoint -> m EbId
ebIdFromPointM mvar p =
    MVar.modifyMVar mvar $ \ebBodies -> do
        let (ebId, mbEbBodies') = ebIdFromPoint p ebBodies
        case mbEbBodies' of
            Nothing -> pure (ebBodies, ebId)
            Just ebBodies' -> do
                -- TODO when to INSERT INTO ebPoints?
                pure (ebBodies', ebId)

-----

loadEbBodies :: IOLike m => LeiosDb stmt m -> m LeiosEbBodies
loadEbBodies db = do
    (ps, qs) <- dbWithBEGIN db $ dbWithPrepare db (fromString sql_scan_ebId) $ \stmt -> do
        let loop !ps !qs =
                dbStep db stmt >>= \case
                    DB.Done -> pure (ps, qs)
                    DB.Row -> do
                        ebSlot <- fromIntegral <$> dbColumnInt64 db stmt 0
                        ebHash <- MkEbHash <$> dbColumnBlob db stmt 1
                        ebId <- fromIntegral <$> dbColumnInt64 db stmt 2
                        loop
                            (IntMap.insertWith Map.union ebSlot (Map.singleton ebHash (MkEbId ebId)) ps)
                            (IntMap.insert ebId ebHash qs)
        loop IntMap.empty IntMap.empty
    pure Leios.emptyLeiosEbBodies {
        Leios.ebPoints = ps
      ,
        Leios.ebPointsInverse = qs
      }

sql_scan_ebId :: String
sql_scan_ebId =
    "SELECT ebSlot, ebHashBytes, ebId\n\
    \FROM ebPoints\n\
    \ORDER BY ebId ASC\n\
    \"

-----

data SomeLeiosFetchContext m =
    forall stmt. MkSomeLeiosFetchContext !(LeiosFetchContext stmt m)

data LeiosFetchContext stmt m = MkLeiosFetchContext {
    leiosDb           :: !(LeiosDb stmt m)
  , leiosEbBuffer     :: !(MV.MVector (PrimState m) (TxHash, BytesSize))
  , leiosEbTxsBuffer  :: !(MV.MVector (PrimState m) LeiosTx)
  , leiosWriteLock    :: !(MVar m ())
  , readLeiosEbBodies :: !(m LeiosEbBodies)
  }

newLeiosFetchContext ::
    PrimMonad m
 =>
    MVar m ()
 ->
    LeiosDb stmt m
 ->
    m LeiosEbBodies
 ->
    m (LeiosFetchContext stmt m)
newLeiosFetchContext leiosWriteLock leiosDb readLeiosEbBodies = do
    -- each LeiosFetch server calls this when it initializes
    leiosEbBuffer <- MV.new Leios.maxEbItems
    leiosEbTxsBuffer <- MV.new Leios.maxEbItems
    pure MkLeiosFetchContext { leiosDb, leiosEbBuffer, leiosEbTxsBuffer, leiosWriteLock, readLeiosEbBodies}

-----

leiosFetchHandler ::
    IOLike m
 =>
    Tracer m TraceLeiosPeer
 ->
    LeiosFetchContext stmt m
 ->
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
    IOLike m
 =>
    Tracer m TraceLeiosPeer
 ->
    LeiosFetchContext stmt m
 ->
    LeiosPoint
 ->
    m LeiosEb
msgLeiosBlockRequest _tracer leiosContext p = do
    let MkLeiosFetchContext {leiosDb = db, leiosEbBuffer = buf, leiosWriteLock, readLeiosEbBodies} = leiosContext
    (ebId, mbLeiosEbBodies') <- readLeiosEbBodies <&> ebIdFromPoint p
    case mbLeiosEbBodies' of
        Nothing -> pure ()
        Just _  -> error "Unrecognized Leios point"
    n <- MVar.withMVar leiosWriteLock $ \() -> do
        -- get the EB items
        dbWithBEGIN db $ dbWithPrepare db (fromString sql_lookup_ebBodies) $ \stmt -> do
            dbBindInt64 db stmt 1 (Leios.fromIntegralEbId ebId)
            let loop !i =
                    dbStep db stmt >>= \case
                        DB.Done -> pure i
                        DB.Row -> do
                            txHashBytes <- dbColumnBlob db stmt 0
                            txBytesSize <- fromIntegral <$> dbColumnInt64 db stmt 1
                            MV.write buf i (MkTxHash txHashBytes, txBytesSize)
                            loop (i+1)
            loop 0
    v <- V.freeze $ MV.slice 0 n buf
    pure $ MkLeiosEb v

sql_lookup_ebBodies :: String
sql_lookup_ebBodies =
    "SELECT txHashBytes, txBytesSize FROM ebTxs\n\
    \WHERE ebId = ?\n\
    \ORDER BY txOffset ASC\n\
    \"

msgLeiosBlockTxsRequest ::
    IOLike m
 =>
    Tracer m TraceLeiosPeer
 ->
    LeiosFetchContext stmt m
 ->
    LeiosPoint
 ->
    [(Word16, Word64)]
 ->
    m (V.Vector LeiosTx)
msgLeiosBlockTxsRequest _tracer leiosContext p bitmaps = do
    let MkLeiosFetchContext {leiosDb = db, leiosEbTxsBuffer = buf, leiosWriteLock, readLeiosEbBodies} = leiosContext
    (ebId, mbLeiosEbBodies') <- readLeiosEbBodies <&> ebIdFromPoint p
    case mbLeiosEbBodies' of
        Nothing -> pure ()
        Just _  -> error "Unrecognized Leios point"
    do
        let idxs = map fst bitmaps
        let idxLimit = Leios.maxEbItems `div` 64
        when (any (== 0) $ map snd bitmaps) $ do
            error "A bitmap is zero"
        when (flip any idxs (> fromIntegral idxLimit)) $ do
            error $ "An offset exceeds the theoretical limit " <> show idxLimit
        when (not $ and $ zipWith (<) idxs (tail idxs)) $ do
            error "Offsets not strictly ascending"
    let nextOffset = \case
            [] -> Nothing
            (idx, bitmap) : k -> case popLeftmostOffset bitmap of
                Nothing           -> nextOffset k
                Just (i, bitmap') ->
                    Just (64 * fromIntegral idx + i, (idx, bitmap') : k)
        txOffsets = unfoldr nextOffset bitmaps
    n <- MVar.withMVar leiosWriteLock $ \() -> dbWithBEGIN db $ do
        -- fill in-memory table
        dbWithPrepare db (fromString sql_insert_memTxPoints) $ \stmt -> do
            dbBindInt64 db stmt 1 (Leios.fromIntegralEbId ebId)
            forM_ txOffsets $ \txOffset -> do
                dbBindInt64 db stmt 2 (fromIntegral txOffset)
                dbStep1 db stmt
                dbReset db stmt
        -- get txBytess
        n <- dbWithPrepare db (fromString sql_retrieve_from_ebTxs) $ \stmt -> do
            (\f -> foldM f 0 txOffsets) $ \i txOffset -> do
                dbStep db stmt >>= \case
                    DB.Done -> pure i
                    DB.Row -> do
                        txOffset' <- dbColumnInt64 db stmt 0
                        txBytes <- dbColumnBlob db stmt 1
                        when (fromIntegral txOffset /= txOffset') $ do
                            error $ "Missing offset " ++ show (txOffset, txOffset')
                        tx <- case decodeFullDecoder' (fromString "txBytes column") Leios.decodeLeiosTx txBytes of
                            Left err -> error $ "Failed to deserialize txBytes column: " ++ Leios.prettyLeiosPoint p ++ " " ++ show (txOffset', err)
                            Right tx -> pure tx
                        MV.write buf i tx
                        pure $! (i + 1)
        dbExec db (fromString sql_flush_memTxPoints)
        pure n
    V.freeze $ MV.slice 0 n buf

{- | For example
@
  print $ unfoldr popLeftmostOffset 0
  print $ unfoldr popLeftmostOffset 1
  print $ unfoldr popLeftmostOffset (2^(34 :: Int))
  print $ unfoldr popLeftmostOffset (2^(63 :: Int) + 2^(62 :: Int) + 8)
  []
  [63]
  [29]
  [0,1,60]
@
-}
popLeftmostOffset :: Word64 -> Maybe (Int, Word64)
{-# INLINE popLeftmostOffset #-}
popLeftmostOffset = \case
    0 -> Nothing
    w -> let zs = Bits.countLeadingZeros w
         in
         Just (zs, Bits.clearBit w (63 - zs))

_sql_detach_memTxPoints :: String
_sql_detach_memTxPoints =
    -- NB :memory: databases are discarded when detached
    "DETACH DATABASE mem;\n\
    \"

sql_flush_memTxPoints :: String
sql_flush_memTxPoints =
    "DELETE FROM mem.txPoints;\n\
    \"

sql_insert_memTxPoints :: String
sql_insert_memTxPoints =
    "INSERT INTO mem.txPoints (ebId, txOffset) VALUES (?, ?);\n\
    \"

sql_retrieve_from_ebTxs :: String
sql_retrieve_from_ebTxs =
    "SELECT x.txOffset, x.txBytes\n\
    \FROM ebTxs as x\n\
    \INNER JOIN mem.txPoints ON x.ebId = mem.txPoints.ebId AND x.txOffset = mem.txPoints.txOffset\n\
    \WHERE x.txBytes IS NOT NULL\n\
    \ORDER BY mem.txPoints.ebId ASC, mem.txPoints.txOffset ASC\n\
    \"

-----

newtype LeiosFetchDecisions pid =
    MkLeiosFetchDecisions
        (Map (PeerId pid) (Map SlotNo (DList (TxHash, BytesSize, Map EbId Int), DList (EbId , BytesSize))))

emptyLeiosFetchDecisions :: LeiosFetchDecisions pid
emptyLeiosFetchDecisions = MkLeiosFetchDecisions Map.empty

leiosFetchLogicIteration :: forall pid.
    Ord pid
 =>
    LeiosFetchStaticEnv
 ->
    (LeiosEbBodies, Map (PeerId pid) (Set EbId, Set EbId))
 ->
    LeiosOutstanding pid
 ->
    (LeiosOutstanding pid, LeiosFetchDecisions pid)
leiosFetchLogicIteration env (ebBodies, offerings) =
    \acc ->
        go1 acc emptyLeiosFetchDecisions
      $ expand
      $ Map.toDescList
      $ Map.map Left (Leios.missingEbBodies ebBodies) `Map.union` Map.map Right (Leios.missingEbTxs acc)
  where
    expand = \case
        [] -> []
        (ebId, Left ebBytesSize):vs -> Left (ebId, ebBytesSize) : expand vs
        (ebId, Right v):vs ->
            [ Right (ebId, txOffset, txHash) | (txOffset, (txHash, _txBytesSize)) <- IntMap.toAscList v ]
         <> expand vs
    go1 !acc !accNew = \case
        []
         -> (acc, accNew)

        Left (ebId, ebBytesSize) : targets
          | let peerIds :: Set (PeerId pid)
                peerIds = Map.findWithDefault Set.empty ebId (Leios.requestedEbPeers acc)
         -> goEb2 acc accNew targets ebId ebBytesSize peerIds

        Right (ebId, txOffset, txHash) : targets

          | Just _ <- Map.lookup ebId (Leios.toCopy acc) >>= IntMap.lookup txOffset
              -- it's already scheduled to be copied from TxCache
         -> go1 acc accNew targets

          | Just txBytesSize <- Map.lookup txHash (Leios.cachedTxs acc)   -- it's in the TxCache
         -> let full =
                    Leios.toCopyBytesSize acc >= Leios.maxToCopyBytesSize env
                 ||
                    Leios.toCopyCount acc >= Leios.maxToCopyCount env
                acc' =
                    if full then acc else
                    acc {
                        Leios.missingEbTxs =
                            Map.alter
                                (\case
                                    Nothing -> error "impossible!"
                                    Just x -> delIf IntMap.null $ IntMap.delete txOffset x
                                )
                                ebId
                                (Leios.missingEbTxs acc)
                      ,
                        Leios.txOffsetss =
                            Map.alter
                                (\case
                                    Nothing -> error "impossible!"
                                    Just x -> delIf Map.null $ Map.delete ebId x
                                )
                                txHash
                                (Leios.txOffsetss acc)
                      ,
                        Leios.toCopy =
                            Map.insertWith
                                IntMap.union
                                ebId
                                (IntMap.singleton txOffset txBytesSize)
                                (Leios.toCopy acc)
                      ,
                        Leios.toCopyBytesSize = Leios.toCopyBytesSize acc + txBytesSize
                      ,
                        Leios.toCopyCount = Leios.toCopyCount acc + 1
                      }
            in go1 acc' accNew targets

          | otherwise
         -> let !txOffsets = case Map.lookup txHash (Leios.txOffsetss acc) of
                    Nothing -> error "impossible!"
                    Just x  -> x
                peerIds :: Set (PeerId pid)
                peerIds = Map.findWithDefault Set.empty txHash (Leios.requestedTxPeers acc)
            in
            goTx2 acc accNew targets (ebIdSlot ebId) txHash txOffsets peerIds

    goEb2 !acc !accNew targets ebId ebBytesSize peerIds
      | Leios.requestedBytesSize acc >= Leios.maxRequestedBytesSize env   -- we can't request anything
      = (acc, accNew)

      | Set.size peerIds < Leios.maxRequestsPerEb env   -- we would like to request it from an additional peer
      , Just peerId <- choosePeerEb peerIds acc ebId
          -- there's a peer who offered it and we haven't already requested it from them
      = let accNew' =
                MkLeiosFetchDecisions
              $ Map.insertWith
                   (Map.unionWith (<>))
                   peerId
                   (Map.singleton (ebIdSlot ebId) (DList.empty, DList.singleton (ebId, ebBytesSize)))
                   (let MkLeiosFetchDecisions x = accNew in x)
            acc' = acc {
                Leios.requestedEbPeers = Map.insertWith Set.union ebId (Set.singleton peerId) (Leios.requestedEbPeers acc)
              ,
                Leios.requestedBytesSizePerPeer = Map.insertWith (+) peerId ebBytesSize (Leios.requestedBytesSizePerPeer acc)
              ,
                Leios.requestedBytesSize = ebBytesSize + Leios.requestedBytesSize acc
              }
            peerIds' = Set.insert peerId peerIds
        in
        goEb2 acc' accNew' targets ebId ebBytesSize peerIds'

      | otherwise
      = go1 acc accNew targets

    choosePeerEb :: Set (PeerId pid) -> LeiosOutstanding pid -> EbId -> Maybe (PeerId pid)
    choosePeerEb peerIds acc ebId =
        foldr (\a _ -> Just a) Nothing
      $ [ peerId
        | (peerId, (ebIds, _ebIds)) <-
              Map.toList   -- TODO prioritize/shuffle?
            $ (`Map.withoutKeys` peerIds)   -- not already requested from this peer
            $ offerings
        , Map.findWithDefault 0 peerId (Leios.requestedBytesSizePerPeer acc) <= Leios.maxRequestedBytesSizePerPeer env
            -- peer can be sent more requests
        , ebId `Set.member` ebIds   -- peer has offered this EB body
        ]

    goTx2 !acc !accNew targets ebSlot txHash txOffsets peerIds

      | Leios.requestedBytesSize acc >= Leios.maxRequestedBytesSize env   -- we can't request anything
      = (acc, accNew)

      | Set.size peerIds < Leios.maxRequestsPerTx env   -- we would like to request it from an additional peer
          -- TODO if requests list priority, does this limit apply even if the
          -- tx has only been requested at lower priorities?
      , Just (peerId, txOffsets') <- choosePeerTx peerIds acc txOffsets
          -- there's a peer who offered it and we haven't already requested it from them
      = let txBytesSize = case Map.lookupMax txOffsets' of
                Nothing -> error "impossible!"
                Just (ebId, txOffset) -> case Map.lookup ebId (Leios.missingEbTxs acc) of
                    Nothing -> error "impossible!"
                    Just v -> case IntMap.lookup txOffset v of
                        Nothing           -> error "impossible!"
                        Just (_txHash, x) -> x
            accNew' =
                MkLeiosFetchDecisions
              $ Map.insertWith
                    (Map.unionWith (<>))
                    peerId
                    (Map.singleton ebSlot (DList.singleton (txHash, txBytesSize, txOffsets'), DList.empty))
                    (let MkLeiosFetchDecisions x = accNew in x)
            acc' = acc {
                Leios.requestedTxPeers = Map.insertWith Set.union txHash (Set.singleton peerId) (Leios.requestedTxPeers acc)
              ,
                Leios.requestedBytesSizePerPeer = Map.insertWith (+) peerId txBytesSize (Leios.requestedBytesSizePerPeer acc)
              ,
                Leios.requestedBytesSize = txBytesSize + Leios.requestedBytesSize acc
              }
            peerIds' = Set.insert peerId peerIds
        in
        goTx2 acc' accNew' targets ebSlot txHash txOffsets peerIds'

      | otherwise
      = go1 acc accNew targets

    choosePeerTx :: Set (PeerId pid) -> LeiosOutstanding pid -> Map EbId Int -> Maybe (PeerId pid, Map EbId Int)
    choosePeerTx peerIds acc txOffsets =
        foldr (\a _ -> Just a) Nothing
      $ [ (peerId, txOffsets')
        | (peerId, (_ebIds, ebIds)) <-
              Map.toList   -- TODO prioritize/shuffle?
            $ (`Map.withoutKeys` peerIds)   -- not already requested from this peer
            $ offerings
        , Map.findWithDefault 0 peerId (Leios.requestedBytesSizePerPeer acc) <= Leios.maxRequestedBytesSizePerPeer env
            -- peer can be sent more requests
        , let txOffsets' = txOffsets `Map.restrictKeys` ebIds
        , not $ Map.null txOffsets'   -- peer has offered an EB closure that includes this tx
        ]

packRequests :: LeiosFetchStaticEnv -> LeiosEbBodies -> LeiosFetchDecisions pid -> Map (PeerId pid) (Seq LeiosFetchRequest)
packRequests env ebBodies =
    \(MkLeiosFetchDecisions x) -> Map.map goPeer x
  where
    goPeer =
         Map.foldlWithKey
            (\acc prio (txs, ebs) -> goPrioTx prio txs <> goPrioEb prio ebs <> acc)
                -- TODO priority within same slot?
            Seq.empty

    goPrioEb _prio ebs =
        DList.foldr (Seq.:<|) Seq.empty
      $ DList.map
            (\(ebId, ebBytesSize) -> case ebIdToPoint ebId ebBodies of
                Nothing -> error "impossible!"
                Just p  -> LeiosBlockRequest $ MkLeiosBlockRequest p ebBytesSize
            )
            ebs

    goPrioTx _prio txs =
        Map.foldlWithKey
            (\acc ebId txs' ->
                case ebIdToPoint ebId ebBodies of
                    Nothing -> error "impossible!"
                    Just p ->
                        goEb
                            {- prio -}
                            p
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
        -- TODO somewhat arbitrarily choosing the freshest EbId here; merely
        -- something simple and sufficient for the demo
        , let (ebId, txOffset) =
                  case Map.lookupMax txOffsets of
                      Nothing -> error "impossible!"
                      Just x  -> x
        ]

    goEb ::
        LeiosPoint
     ->
        BytesSize
     ->
        IntMap Word64
     ->
        Int
     ->
        DList TxHash
     ->
        [(Int, (TxHash, BytesSize))]
     ->
        Seq LeiosFetchRequest
    -- TODO the incoming indexes are ascending, so the IntMap accumulator could
    -- be simplified away
    goEb p !accTxBytesSize !accBitmaps !accN !accHashes = \case
        [] -> if 0 < accN then Seq.singleton flush else Seq.empty
        txsAgain@((txOffset, (txHash, txBytesSize)):txs)

          | Leios.maxRequestBytesSize env < accTxBytesSize'
         -> flush Seq.:<| goEb p 0 IntMap.empty 0 DList.empty txsAgain

          | otherwise
          , let (q, r) = txOffset `divMod` 64
         -> goEb
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
            LeiosBlockTxsRequest
          $ MkLeiosBlockTxsRequest
                {- prio -}
                p
                [ (fromIntegral idx, bitmap) | (idx, bitmap) <- IntMap.toAscList accBitmaps ]
                (V.fromListN accN $ DList.toList accHashes)

-----

nextLeiosFetchClientCommand :: forall pid stmt m.
  (
    Ord pid
  ,
    IOLike m
  )
 =>
    Tracer m TraceLeiosKernel
 ->
    Tracer m TraceLeiosPeer
 ->
    StrictSTM.STM m Bool
 ->
    (MVar m (), MVar m LeiosEbBodies, MVar m (LeiosOutstanding pid), MVar m (), MVar m (Map (PeerId pid) (StrictTVar m (Map SlotNo (Seq LeiosNotification)))))
 ->
    LeiosDb stmt m
 ->
    PeerId pid
 ->
    StrictTVar m (Seq LeiosFetchRequest)
 ->
    m (Either
        (m  (Either () (LF.SomeLeiosFetchJob LeiosPoint LeiosEb LeiosTx m)))
            (Either () (LF.SomeLeiosFetchJob LeiosPoint LeiosEb LeiosTx m))
      )
nextLeiosFetchClientCommand ktracer tracer stopSTM kernelVars db peerId reqsVar = do
    f (pure Nothing) (pure . Just) >>= \case
        Just x -> pure $ Right x
        Nothing -> pure $ Left $ f StrictSTM.retry pure
  where
    f ::
        StrictSTM.STM m r
     ->
        (Either () (LF.SomeLeiosFetchJob LeiosPoint LeiosEb LeiosTx m) -> StrictSTM.STM m r)
     ->
        m r
    f retry_ pure_ = StrictSTM.atomically $ do
        stopSTM >>= \case
            True -> pure_ $ Left ()
            False -> StrictSTM.readTVar reqsVar >>= \case
                Seq.Empty -> retry_
                req Seq.:<| reqs -> do
                    StrictSTM.writeTVar reqsVar reqs
                    pure_ $ Right $ g req

    g = \case
        LeiosBlockRequest req@(MkLeiosBlockRequest p _ebBytesSize) ->
            LF.MkSomeLeiosFetchJob
                (LF.MsgLeiosBlockRequest p)
                (pure $ \(LF.MsgLeiosBlock eb) ->
                    msgLeiosBlock ktracer tracer kernelVars db peerId req eb
                )
        LeiosBlockTxsRequest req@(MkLeiosBlockTxsRequest p bitmaps _txHashes) ->
            LF.MkSomeLeiosFetchJob
                (LF.MsgLeiosBlockTxsRequest p bitmaps)
                (pure $ \(LF.MsgLeiosBlockTxs txs) -> do
                    msgLeiosBlockTxs ktracer tracer kernelVars db peerId req txs
                )

-----

msgLeiosBlock ::
  (
    Ord pid
  ,
    IOLike m
  )
 =>
    Tracer m TraceLeiosKernel
 ->
    Tracer m TraceLeiosPeer
 ->
    (MVar m (), MVar m LeiosEbBodies, MVar m (LeiosOutstanding pid), MVar m (), MVar m (Map (PeerId pid) (StrictTVar m (Map SlotNo (Seq LeiosNotification)))))
 ->
    LeiosDb stmt m
 ->
    PeerId pid
 ->
    LeiosBlockRequest
 ->
    LeiosEb
 ->
    m ()
msgLeiosBlock ktracer tracer (writeLock, ebBodiesVar, outstandingVar, readyVar, notificationVars) db peerId req eb = do
    -- validate it
    let MkLeiosBlockRequest p ebBytesSize = req
    traceWith tracer $ MkTraceLeiosPeer $ "[start] MsgLeiosBlock " <> Leios.prettyLeiosPoint p
    do
        let MkLeiosPoint _ebSlot ebHash = p
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
    (ebId, novel) <- MVar.modifyMVar ebBodiesVar $ \ebBodies -> do
        ebId <- do
            let (x, mbLeiosEbBodies') = ebIdFromPoint p ebBodies
            case mbLeiosEbBodies' of
                Just _  -> error "Unrecognized Leios point"
                Nothing -> pure x
        let novel = not $ Set.member ebId (Leios.acquiredEbBodies ebBodies)
        when novel $ MVar.withMVar writeLock $ \() -> do   -- TODO don't hold the ebBodies mvar during this IO
            dbWithBEGIN db $ dbWithPrepare db (fromString sql_insert_ebBody) $ \stmt -> do
                -- INSERT INTO ebTxs
                dbBindInt64 db stmt 1 (Leios.fromIntegralEbId ebId)
                V.iforM_ (let MkLeiosEb v = eb in v) $ \txOffset (txHash, txBytesSize) -> do
                    dbBindInt64 db stmt 2 (fromIntegral txOffset)
                    dbBindBlob  db stmt 3 (let MkTxHash bytes = txHash in bytes)
                    dbBindInt64 db stmt 4 (fromIntegral txBytesSize)
                    dbStep1     db stmt
                    dbReset     db stmt
        -- update NodeKernel state
        let !ebBodies' = if not novel then ebBodies else ebBodies {
                Leios.acquiredEbBodies = Set.insert ebId (Leios.acquiredEbBodies ebBodies)
              ,
                Leios.missingEbBodies = Map.delete ebId (Leios.missingEbBodies ebBodies)
              }
        pure (ebBodies', (ebId, novel))
    MVar.modifyMVar_ outstandingVar $ \outstanding -> do
        let !outstanding' = outstanding {
                Leios.blockingPerEb =
                    if not novel then Leios.blockingPerEb outstanding else
                    Map.insert
                        ebId
                        (let MkLeiosEb v = eb in V.length v)
                        (Leios.blockingPerEb outstanding)
              ,
                Leios.missingEbTxs =
                    if not novel then Leios.missingEbTxs outstanding else
                    Map.insert
                        ebId
                        (V.ifoldl
                            (\acc i x -> IntMap.insert i x acc)
                            IntMap.empty
                            (let MkLeiosEb v = eb in v)
                        )
                        (Leios.missingEbTxs outstanding)
              ,
                Leios.txOffsetss =
                    if not novel then Leios.txOffsetss outstanding else
                    V.ifoldl
                        (\acc i (txHash, _txBytesSize) ->
                             Map.insertWith Map.union txHash (Map.singleton ebId i) acc
                        )
                        (Leios.txOffsetss outstanding)
                        (let MkLeiosEb v = eb in v)
              ,
                Leios.requestedBytesSize = Leios.requestedBytesSize outstanding - ebBytesSize
              ,
                Leios.requestedBytesSizePerPeer =
                    Map.alter
                        (\case
                            Nothing -> error "impossible!"
                            Just x -> delIf (==0) $ x - ebBytesSize
                        )
                        peerId
                        (Leios.requestedBytesSizePerPeer outstanding)
              ,
                Leios.requestedEbPeers =
                    Map.update (delIf Set.null . Set.delete peerId) ebId (Leios.requestedEbPeers outstanding)
              }
        pure outstanding'
    void $ MVar.tryPutMVar readyVar ()
    when novel $ do
        traceWith ktracer $ TraceLeiosBlockAcquired p
        vars <- MVar.readMVar notificationVars
        forM_ vars $ \var -> do
            traceWith tracer $ MkTraceLeiosPeer $ "leiosNotificationsBlock!: " ++ Leios.prettyEbId ebId
            StrictSTM.atomically $ do
                x <- StrictSTM.readTVar var
                let !x' =
                        Map.insertWith
                            (<>)
                            (ebIdSlot ebId)
                            (Seq.singleton (LeiosOfferBlock ebId ebBytesSize))
                            x
                StrictSTM.writeTVar var x'
    traceWith tracer $ MkTraceLeiosPeer $ "[done] MsgLeiosBlock " <> Leios.prettyLeiosPoint p

sql_insert_ebBody :: String
sql_insert_ebBody =
    "INSERT INTO ebTxs (ebId, txOffset, txHashBytes, txBytesSize, txBytes) VALUES (?, ?, ?, ?, NULL)\n\
    \"

-----

delIf :: (a -> Bool) -> a -> Maybe a
delIf predicate x = if predicate x then Nothing else Just x

-----

msgLeiosBlockTxs ::
  (
    Ord pid
  ,
    IOLike m
  )
 =>
    Tracer m TraceLeiosKernel
 ->
    Tracer m TraceLeiosPeer
 ->
    (
      MVar m ()
    ,
      MVar m LeiosEbBodies
    ,
      MVar m (LeiosOutstanding pid)
    ,
      MVar m ()
    ,
      MVar m (Map (PeerId pid) (StrictTVar m (Map SlotNo (Seq LeiosNotification))))
    )
 ->
    LeiosDb stmt m
 ->
    PeerId pid
 ->
    LeiosBlockTxsRequest
 ->
    V.Vector LeiosTx
 ->
    m ()
msgLeiosBlockTxs ktracer tracer (writeLock, ebBodiesVar, outstandingVar, readyVar, notificationVars) db peerId req txs = do
    traceWith tracer $ MkTraceLeiosPeer $ "[start] " ++ Leios.prettyLeiosBlockTxsRequest req
    -- validate it
    let MkLeiosBlockTxsRequest p bitmaps txHashes = req
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
                    V.toList
                  $ V.findIndices id
                  $ V.zipWith (/=) txHashes txHashes'
            error $ "MsgLeiosBlockTxs hash mismatches: " ++ show mismatches
    ebBodies <- MVar.readMVar ebBodiesVar
    ebId <- do
        let (x, mbLeiosEbBodies') = ebIdFromPoint p ebBodies
        case mbLeiosEbBodies' of
            Just _  -> error "Unrecognized Leios point"
            Nothing -> pure x
    let nextOffset = \case
            [] -> Nothing
            (idx, bitmap) : k -> case popLeftmostOffset bitmap of
                Nothing           -> nextOffset k
                Just (i, bitmap') ->
                    Just (64 * fromIntegral idx + i, (idx, bitmap') : k)
        offsets = unfoldr nextOffset bitmaps
    -- ingest
    MVar.withMVar writeLock $ \() -> do
        dbWithBEGIN db $ dbWithPrepare db (fromString sql_insert_txCache) $ \stmtTxCache -> dbWithPrepare db (fromString sql_update_ebTx) $ \stmtEbTxs -> do
            dbBindInt64 db stmtEbTxs 2 (Leios.fromIntegralEbId ebId)
            forM_ (zip offsets $ V.toList $ txHashes `V.zip` txBytess) $ \(txOffset, (txHash, txBytes)) -> do
                -- INTO ebTxs
                dbBindInt64 db stmtEbTxs 3 $ fromIntegral txOffset
                dbBindBlob  db stmtEbTxs 1 $ txBytes
                dbStep1     db stmtEbTxs
                dbReset     db stmtEbTxs
                -- INTO txCache
                dbBindBlob  db stmtTxCache 1 $ (let MkTxHash bytes = txHash in bytes)
                dbBindBlob  db stmtTxCache 2 $ txBytes
                dbBindInt64 db stmtTxCache 3 $ fromIntegral $ BS.length txBytes
                dbStep1     db stmtTxCache
                dbReset     db stmtTxCache
    -- update NodeKernel state
    newNotifications <- MVar.modifyMVar outstandingVar $ \outstanding -> do
        let (requestedTxPeers', cachedTxs', txOffsetss', txsBytesSize) =
                (\f -> V.foldl
                    f
                    (
                      Leios.requestedTxPeers outstanding
                    ,
                      Leios.cachedTxs outstanding
                    ,
                      Leios.txOffsetss outstanding
                    ,
                      0
                    )
                    (txHashes `V.zip` txBytess)
                )
              $ \(!accReqs, !accCache, !accOffsetss, !accSz) (txHash, txBytes) -> id
              $ (
                  Map.update (delIf Set.null . Set.delete peerId) txHash accReqs
                ,
                  Map.insert txHash (fromIntegral (BS.length txBytes)) accCache
                ,
                  Map.update (delIf Map.null . Map.delete ebId) txHash accOffsetss
                ,
                  accSz + BS.length txBytes
                )
        let offsetsSet = IntSet.fromList offsets
            checkBeat :: forall a. Map EbId (IntMap a) -> IntMap a
            checkBeat x =
                (`IntMap.restrictKeys` offsetsSet)
              $ Map.findWithDefault
                    IntMap.empty
                    ebId
                    x
            -- the requests that this MsgLeiosBlockTxs was the first to resolve
            beatOtherPeers = checkBeat $ Leios.missingEbTxs outstanding
            -- the currently scheduled 'toCopy' operations that this
            -- MsgLeiosBlockTxs just won the race against
            beatToCopy = checkBeat $ Leios.toCopy outstanding
        let !outstanding' = outstanding {
                Leios.cachedTxs = cachedTxs'
              ,
                Leios.missingEbTxs =
                    Map.update
                        (delIf IntMap.null . (`IntMap.withoutKeys` offsetsSet))
                        ebId
                        (Leios.missingEbTxs outstanding)
              ,
                Leios.txOffsetss = txOffsetss'
              ,
                Leios.blockingPerEb =
                    if IntMap.null beatOtherPeers && IntMap.null beatToCopy then Leios.blockingPerEb outstanding else
                    Map.alter
                        (\case
                            Nothing -> Nothing
                            Just x -> delIf (==0) $ x - IntMap.size beatOtherPeers - IntMap.size beatToCopy
                        )
                        ebId
                        (Leios.blockingPerEb outstanding)
              ,
                Leios.requestedBytesSize =
                    Leios.requestedBytesSize outstanding - fromIntegral txsBytesSize
              ,
                Leios.requestedBytesSizePerPeer =
                    Map.alter
                        (\case
                            Nothing -> error "impossible!"
                            Just x -> delIf (==0) $ x - fromIntegral txsBytesSize
                        )
                        peerId
                        (Leios.requestedBytesSizePerPeer outstanding)
              ,
                Leios.requestedTxPeers = requestedTxPeers'
              ,
                Leios.toCopy =
                    if IntMap.null beatToCopy then Leios.toCopy outstanding else
                    Map.alter
                        (\case
                            Nothing -> Nothing
                            Just x ->
                                delIf IntMap.null
                              $ x `IntMap.difference` beatToCopy
                        )
                        ebId
                        (Leios.toCopy outstanding)
              ,
                Leios.toCopyBytesSize =
                    Leios.toCopyBytesSize outstanding - sum beatToCopy
              ,
                Leios.toCopyCount =
                    Leios.toCopyCount outstanding - IntMap.size beatToCopy
              }
        let newNotifications =
               Map.keys
             $ Leios.blockingPerEb outstanding `Map.difference` Leios.blockingPerEb outstanding'
        pure (outstanding', newNotifications)
    void $ MVar.tryPutMVar readyVar ()
    when (not $ null newNotifications) $ do
        let notifications =
                Map.fromList
              $ [ (ebIdSlot x, Seq.singleton (LeiosOfferBlockTxs x))
                | x <- newNotifications
                ]
        forM_ newNotifications $ \ebId' -> do
            case ebIdToPoint ebId' ebBodies of
                Nothing -> error $ "Unrecognized Leios EbId: " ++ Leios.prettyEbId ebId'
                Just p' -> traceWith ktracer $ TraceLeiosBlockTxsAcquired p'
        vars <- MVar.readMVar notificationVars
        forM_ vars $ \var -> do
            traceWith tracer $ MkTraceLeiosPeer $ "leiosNotificationsBlockTxs!: " ++ unwords (map Leios.prettyEbId newNotifications)
            StrictSTM.atomically $ do
                x <- StrictSTM.readTVar var
                let !x' = Map.unionWith (<>) x notifications
                StrictSTM.writeTVar var x'
    traceWith tracer $ MkTraceLeiosPeer $ "[done] " ++ Leios.prettyLeiosBlockTxsRequest req

sql_update_ebTx :: String
sql_update_ebTx =
    "UPDATE ebTxs\n\
    \SET txBytes = ?\n\
    \WHERE ebId = ? AND txOffset = ? AND txBytes IS NULL\n\
    \"

sql_insert_txCache :: String
sql_insert_txCache =
    "INSERT OR IGNORE INTO txCache (txHashBytes, txBytes, txBytesSize, expiryUnixEpoch) VALUES (?, ?, ?, -1)\n\
    \"

-----

doCacheCopy ::
    IOLike m
 =>
    Tracer m TraceLeiosKernel
 ->
    LeiosDb stmt m
 ->
    (
      MVar m ()
    ,
      MVar m (LeiosOutstanding pid)
    ,
      MVar m (Map (PeerId pid) (StrictTVar m (Map SlotNo (Seq LeiosNotification))))
    )
 ->
    BytesSize
 ->
    m Bool
doCacheCopy tracer db (writeLock, outstandingVar, notificationVars) bytesSize = do
    copied <- do
        outstanding <- MVar.readMVar outstandingVar
        MVar.withMVar writeLock $ \() -> do
            dbWithBEGIN db $ dbWithPrepare db (fromString sql_copy_from_txCache) $ \stmt -> do
                go1 stmt Map.empty 0 (Leios.toCopy outstanding)
    (moreTodo, newNotifications) <- MVar.modifyMVar outstandingVar $ \outstanding -> do
        let _ = copied :: Map EbId IntSet
        let usefulCopied =
                -- @copied@ might contain elements that were already accounted
                -- for by a @MsgLeiosBlockTxs@ that won the race. This
                -- intersection discards those.
                Map.intersectionWith
                    IntMap.restrictKeys
                    (Leios.toCopy outstanding)
                    copied
        let !outstanding' = outstanding {
                Leios.blockingPerEb =
                    Map.differenceWithKey
                        (\_ebId count copiedEbId ->
                            delIf (==0) $ count - IntMap.size copiedEbId
                        )
                        (Leios.blockingPerEb outstanding)
                        usefulCopied
              ,
                Leios.toCopy =
                    Map.differenceWithKey
                        (\_ebId toCopy copiedEbId ->
                            delIf IntMap.null $ toCopy `IntMap.difference` copiedEbId
                        )
                        (Leios.toCopy outstanding)
                        usefulCopied
              ,
                Leios.toCopyBytesSize =
                    Leios.toCopyBytesSize outstanding - sum (Map.map sum usefulCopied)
              ,
                Leios.toCopyCount =
                    Leios.toCopyCount outstanding - sum (Map.map IntMap.size usefulCopied)
              }
        let newNotifications =
               Map.keys
             $ Leios.blockingPerEb outstanding `Map.difference` Leios.blockingPerEb outstanding'
        pure (outstanding', (0 /= Leios.toCopyCount outstanding', newNotifications))
    when (not $ null newNotifications) $ do
        let notifications =
                Map.fromList
              $ [ (ebIdSlot x, Seq.singleton (LeiosOfferBlockTxs x))
                | x <- newNotifications
                ]
        traceWith tracer $ MkTraceLeiosKernel $ "leiosNotificationsCopy: " ++ unwords (map Leios.prettyEbId newNotifications)
        vars <- MVar.readMVar notificationVars
        forM_ vars $ \var -> do
            traceWith tracer $ MkTraceLeiosKernel $ "leiosNotificationsCopy!: " ++ unwords (map Leios.prettyEbId newNotifications)
            StrictSTM.atomically $ do
                x <- StrictSTM.readTVar var
                let !x' = Map.unionWith (<>) x notifications
                StrictSTM.writeTVar var x'
    pure moreTodo
  where
    go1 stmt !accCopied !accBytesSize !acc
      | accBytesSize < bytesSize
      , Just ((ebId, txs), acc') <- Map.maxViewWithKey acc
      = go2 stmt accCopied accBytesSize acc' ebId IntSet.empty txs

      | otherwise
      = pure accCopied

    go2 stmt !accCopied !accBytesSize !acc ebId !accCopiedEbId txs
      | Just ((txOffset, txBytesSize), txs') <- IntMap.minViewWithKey txs
      = if accBytesSize + txBytesSize > bytesSize then pure accCopied' else do
            dbBindInt64 db stmt 1 (Leios.fromIntegralEbId ebId)
            dbBindInt64 db stmt 2 (fromIntegral txOffset)
            dbStep1     db stmt
            dbReset     db stmt
            go2
                stmt
                accCopied
                (accBytesSize + txBytesSize)
                acc
                ebId
                (IntSet.insert txOffset accCopiedEbId)
                txs'
      | otherwise
      = go1 stmt accCopied' accBytesSize acc
      where
        accCopied' = Map.insertWith IntSet.union ebId accCopiedEbId accCopied

sql_copy_from_txCache :: String
sql_copy_from_txCache =
    "UPDATE ebTxs\n\
    \SET txBytes = (SELECT txBytes FROM txCache WHERE txCache.txHashBytes = ebTxs.txHashBytes)\n\
    \WHERE ebId = ? AND txOffset = ? AND txBytes IS NULL\n\
    \"

-----

nextLeiosNotification ::
  (
    MonadMVar m
  ,
    MonadSTM m
  )
 =>
    Tracer m TraceLeiosPeer
 ->
    (MVar m LeiosEbBodies, StrictTVar m (Map SlotNo (Seq LeiosNotification)))
 ->
    m (LN.Message (LN.LeiosNotify LeiosPoint announcement) LN.StBusy LN.StIdle)
nextLeiosNotification _tracer (ebBodiesVar, var) = do
    notification <- StrictSTM.atomically $ StrictSTM.readTVar var >>= go1
    ebBodies <- MVar.readMVar ebBodiesVar
    let f ebId = case ebIdToPoint ebId ebBodies of
            Nothing -> error "impossible!"
            Just x  -> x
    pure $ case notification of
        LeiosOfferBlock ebId ebBytesSize ->
            LN.MsgLeiosBlockOffer (f ebId) ebBytesSize
        LeiosOfferBlockTxs ebId ->
            LN.MsgLeiosBlockTxsOffer (f ebId)
  where
    go1 = go2 . Map.maxViewWithKey
    go2 = \case
        Nothing -> StrictSTM.retry
        Just ((_slotNo, Seq.Empty), x') -> go1 x'
        Just ((slotNo, notification Seq.:<| notifications), x') -> do
            StrictSTM.writeTVar var $
                if Seq.null notifications then x' else
                Map.insert slotNo notifications x'
            pure notification
