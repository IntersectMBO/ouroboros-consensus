{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LeiosDemoLogic (module LeiosDemoLogic) where

import           Cardano.Slotting.Slot (SlotNo (..))
import           Control.Concurrent.Class.MonadMVar (MVar, MonadMVar)
import qualified Control.Concurrent.Class.MonadMVar as MVar
import           Control.Concurrent.Class.MonadSTM.Strict (StrictTVar)
import qualified Control.Concurrent.Class.MonadSTM.Strict as StrictSTM
import           Control.Monad (foldM, when)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Bits as Bits
import           Data.DList (DList)
import qualified Data.DList as DList
import           Data.Foldable (forM_)
import           Data.Functor ((<&>))
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.List (unfoldr)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (fromString)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Data.Word (Word16, Word64)
import qualified Database.SQLite3.Direct as DB
import           Debug.Trace (traceM)
import qualified LeiosDemoOnlyTestFetch as LF
import           LeiosDemoTypes (BytesSize, EbHash (..), EbId (..), LeiosEbBodies, LeiosOutstanding, LeiosPoint (..), LeiosDb (..), LeiosEb (..), LeiosFetchStaticEnv, LeiosTx (..), PeerId (..), TxHash (..))
import           LeiosDemoTypes (LeiosBlockRequest (..), LeiosBlockTxsRequest (..), LeiosFetchRequest (..))
import qualified LeiosDemoTypes as Leios

ebIdSlot :: EbId -> SlotNo
ebIdSlot (MkEbId y) =
    SlotNo (fromIntegral (y - minBound :: Int) `Bits.unsafeShiftR` 20 :: Word64)

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
            Just y -> (y, Nothing)
            Nothing -> gen $ MkEbId $ zero + (2^(20 :: Int) - 1) - Map.size m
        Nothing -> gen $ MkEbId $ zero + (2^(20 :: Int) - 1)
  where
    MkLeiosPoint ebSlot ebHash = p
    SlotNo wslot = ebSlot
    islot = fromIntegral (wslot :: Word64)

    zero = fromIntegral (wslot `Bits.unsafeShiftL` 20) + minBound :: Int

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

loadEbBodies :: Monad m => LeiosDb stmt m -> m LeiosEbBodies
loadEbBodies db = do
    dbExec db (fromString "BEGIN")
    stmt <- dbPrepare db (fromString sql_scan_ebId)
    let loop !ps !qs =
            dbStep db stmt >>= \case
                DB.Done -> do
                    dbFinalize db stmt
                    pure (ps, qs)
                DB.Row -> do
                    ebSlot <- fromIntegral <$> dbColumnInt64 db stmt 0
                    ebHash <- MkEbHash <$> dbColumnBlob db stmt 1
                    ebId <- fromIntegral <$> dbColumnInt64 db stmt 2
                    loop
                        (IntMap.insertWith Map.union ebSlot (Map.singleton ebHash (MkEbId ebId)) ps)
                        (IntMap.insert ebId ebHash qs)
    (ps, qs) <- loop IntMap.empty IntMap.empty
    dbExec db (fromString "COMMIT")
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
    leiosDb :: !(LeiosDb stmt m)
  , leiosEbBuffer :: !(MV.MVector (PrimState m) (TxHash, BytesSize))
  , leiosEbTxsBuffer :: !(MV.MVector (PrimState m) LeiosTx)
  , readLeiosEbBodies :: !(m LeiosEbBodies)
  }

newLeiosFetchContext ::
    PrimMonad m
 =>
    LeiosDb stmt m
 ->
    m LeiosEbBodies
 ->
    m (LeiosFetchContext stmt m)
newLeiosFetchContext leiosDb readLeiosEbBodies = do
    -- each LeiosFetch server calls this when it initializes
    leiosEbBuffer <- MV.new Leios.maxEbItems
    leiosEbTxsBuffer <- MV.new Leios.maxEbItems
    pure MkLeiosFetchContext { leiosDb, leiosEbBuffer, leiosEbTxsBuffer, readLeiosEbBodies}

-----

leiosFetchHandler ::
    PrimMonad m
 =>
    LeiosFetchContext stmt m
 ->
    LF.LeiosFetchRequestHandler LeiosPoint LeiosEb LeiosTx m
leiosFetchHandler leiosContext = LF.MkLeiosFetchRequestHandler $ \case
    LF.MsgLeiosBlockRequest p -> do
        traceM $ "MsgLeiosBlockRequest " <> Leios.prettyLeiosPoint p
        LF.MsgLeiosBlock <$> msgLeiosBlockRequest leiosContext p
    LF.MsgLeiosBlockTxsRequest p bitmaps -> do
        traceM $ "MsgLeiosBlockTxsRequest " <> Leios.prettyLeiosPoint p
        LF.MsgLeiosBlockTxs <$> msgLeiosBlockTxsRequest leiosContext p bitmaps

msgLeiosBlockRequest :: PrimMonad m => LeiosFetchContext stmt m -> LeiosPoint -> m LeiosEb
msgLeiosBlockRequest leiosContext p = do
    let MkLeiosFetchContext {leiosDb = db, leiosEbBuffer = buf, readLeiosEbBodies} = leiosContext
    (ebId, mbLeiosEbBodies') <- readLeiosEbBodies <&> ebIdFromPoint p
    case mbLeiosEbBodies' of
        Nothing -> pure ()
        Just _ -> error "Unrecognized Leios point"
    -- get the EB items
    dbExec db (fromString "BEGIN")
    stmt <- dbPrepare db (fromString sql_lookup_ebBodies)
    dbBindInt64 db stmt 1 (Leios.fromIntegralEbId ebId)
    let loop !i =
            dbStep db stmt >>= \case
                DB.Done -> do
                    dbFinalize db stmt
                    pure i
                DB.Row -> do
                    txHashBytes <- dbColumnBlob db stmt 0
                    txBytesSize <- fromIntegral <$> dbColumnInt64 db stmt 1
                    MV.write buf i (MkTxHash txHashBytes, txBytesSize)
                    loop (i+1)
    n <- loop 0
    dbExec db (fromString "COMMIT")
    v <- V.freeze $ MV.slice 0 n buf
    pure $ MkLeiosEb v

sql_lookup_ebBodies :: String
sql_lookup_ebBodies =
    "SELECT txHashBytes, txBytesSize FROM ebTxs\n\
    \WHERE ebId = ?\n\
    \ORDER BY txOffset ASC\n\
    \"

msgLeiosBlockTxsRequest ::
    PrimMonad m
 =>
    LeiosFetchContext stmt m
 ->
    LeiosPoint
 ->
    [(Word16, Word64)]
 ->
    m (V.Vector LeiosTx)
msgLeiosBlockTxsRequest leiosContext p bitmaps = do
    let MkLeiosFetchContext {leiosDb = db, leiosEbTxsBuffer = buf, readLeiosEbBodies} = leiosContext
    (ebId, mbLeiosEbBodies') <- readLeiosEbBodies <&> ebIdFromPoint p
    case mbLeiosEbBodies' of
        Nothing -> pure ()
        Just _ -> error "Unrecognized Leios point"
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
    -- fill in-memory table
    dbExec db (fromString sql_attach_memTxPoints)
    dbExec db (fromString "BEGIN")
    do
        stmt <- dbPrepare db (fromString sql_insert_memTxPoints)
        dbBindInt64 db stmt 1 (Leios.fromIntegralEbId ebId)
        forM_ txOffsets $ \txOffset -> do
            dbBindInt64 db stmt 2 (fromIntegral txOffset)
            dbStep1 db stmt
            dbReset db stmt
        dbFinalize db stmt
    -- get txBytess
    stmt <- dbPrepare db (fromString sql_retrieve_from_ebTxs)
    n <- (\f -> foldM f 0 txOffsets) $ \i txOffset -> do
        dbStep db stmt >>= \case
            DB.Done -> do
              dbFinalize db stmt
              pure i
            DB.Row -> do
                txOffset' <- dbColumnInt64 db stmt 0
                txBytes <- dbColumnBlob db stmt 1
                when (fromIntegral txOffset /= txOffset') $ do
                    error $ "Missing offset " ++ show (txOffset, txOffset')
                MV.write buf i (MkLeiosTx txBytes)
                pure $! (i + 1)
    dbExec db (fromString "COMMIT")
    dbExec db (fromString sql_detach_memTxPoints)
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

sql_attach_memTxPoints :: String
sql_attach_memTxPoints =
    "ATTACH DATABASE ':memory:' AS mem;\n\
    \\n\
    \CREATE TABLE mem.txPoints (\n\
    \    ebId INTEGER NOT NULL\n\
    \  ,\n\
    \    txOffset INTEGER NOT NULL\n\
    \  ,\n\
    \    PRIMARY KEY (ebId ASC, txOffset ASC)\n\
    \  ) WITHOUT ROWID;\n\
    \"

sql_detach_memTxPoints :: String
sql_detach_memTxPoints =
    -- NB :memory: databases are discarded when detached
    "DETACH DATABASE mem;\n\
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
        (Map (PeerId pid) (Map SlotNo (DList (TxHash, BytesSize, Map EbId Int), DList EbId)))

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

          | not $ Set.member txHash (Leios.missingTxBodies acc)   -- we already have it
         -> go1 acc accNew targets

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
                        Leios.toCopy = Map.insertWith IntMap.union ebId (IntMap.singleton txOffset txBytesSize) (Leios.toCopy acc)
                      ,
                        Leios.toCopyBytesSize = Leios.toCopyBytesSize acc + txBytesSize
                      ,
                        Leios.toCopyCount = Leios.toCopyCount acc + 1
                      }
            in go1 acc' accNew targets

          | otherwise
         -> let !txOffsets = case Map.lookup txHash (Leios.txOffsetss acc) of
                    Nothing -> error "impossible!"
                    Just x -> x
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
                   (Map.singleton (ebIdSlot ebId) (DList.empty, DList.singleton ebId))
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
                        Nothing -> error "impossible!"
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
            (\ebId -> case ebIdToPoint ebId ebBodies of
                Nothing -> error "impossible!"
                Just p -> LeiosBlockRequest $ MkLeiosBlockRequest p
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
                      Just x -> x
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
        (txOffset, (txHash, txBytesSize)):txs

          | Leios.maxRequestBytesSize env < accTxBytesSize'
         -> flush Seq.:<| goEb p 0 IntMap.empty 0 DList.empty txs

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

nextLeiosFetchClientCommand :: forall eb tx m.
    StrictSTM.MonadSTM m
 =>
    StrictSTM.STM m Bool
 ->
    StrictTVar m (Seq LeiosFetchRequest)
 ->
    m (Either
        (m  (Either () (LF.SomeLeiosFetchJob LeiosPoint eb tx m)))
            (Either () (LF.SomeLeiosFetchJob LeiosPoint eb tx m))
      )
nextLeiosFetchClientCommand stopSTM reqsVar = do
    f (pure Nothing) (pure . Just) >>= \case
        Just x -> pure $ Right x
        Nothing -> pure $ Left $ f StrictSTM.retry pure
  where
    f ::
        StrictSTM.STM m r
     ->
        (Either () (LF.SomeLeiosFetchJob LeiosPoint eb tx m) -> StrictSTM.STM m r)
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
        LeiosBlockRequest (MkLeiosBlockRequest p) ->
            LF.MkSomeLeiosFetchJob
                (LF.MsgLeiosBlockRequest p)
                (pure $ \_ -> pure ())
        LeiosBlockTxsRequest (MkLeiosBlockTxsRequest p bitmaps _txHashes) ->
            LF.MkSomeLeiosFetchJob
                (LF.MsgLeiosBlockTxsRequest p bitmaps)
                (pure $  \_ -> pure ())
