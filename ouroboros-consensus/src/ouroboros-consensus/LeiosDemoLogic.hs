{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module LeiosDemoLogic (module LeiosDemoLogic) where

import           Cardano.Slotting.Slot (SlotNo (..))
import           Control.Concurrent.Class.MonadMVar (MVar, MonadMVar)
import qualified Control.Concurrent.Class.MonadMVar as MVar
import           Control.Monad (foldM, when)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Bits as Bits
import           Data.Foldable (forM_)
import           Data.Functor ((<&>))
import qualified Data.IntMap as IntMap
import           Data.List (unfoldr)
import qualified Data.Map as Map
import           Data.String (fromString)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Data.Word (Word16, Word64)
import qualified Database.SQLite3.Direct as DB
import           LeiosDemoTypes (BytesSize, EbHash (..), EbId (..), LeiosEbBodies, LeiosPoint (..), LeiosDb (..), LeiosEb (..), LeiosTx (..), TxHash (..))
import qualified LeiosDemoTypes as Leios

import qualified LeiosDemoOnlyTestFetch as LF

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
  , leiosEbBodies :: !LeiosEbBodies
  , leiosEbBuffer :: !(MV.MVector (PrimState m) (TxHash, BytesSize))
  , leiosEbTxsBuffer :: !(MV.MVector (PrimState m) LeiosTx)
  }

leiosFetchHandler ::
    PrimMonad m
 =>
    LeiosFetchContext stmt m
 ->
    LF.LeiosFetchRequestHandler LeiosPoint LeiosEb LeiosTx m
leiosFetchHandler leiosContext = LF.MkLeiosFetchRequestHandler $ \case
    LF.MsgLeiosBlockRequest p ->
        LF.MsgLeiosBlock <$> msgLeiosBlockRequest leiosContext p
    LF.MsgLeiosBlockTxsRequest p bitmaps ->
        LF.MsgLeiosBlockTxs <$> msgLeiosBlockTxsRequest leiosContext p bitmaps

msgLeiosBlockRequest :: PrimMonad m => LeiosFetchContext stmt m -> LeiosPoint -> m LeiosEb
msgLeiosBlockRequest leiosContext p = do
    let MkLeiosFetchContext {leiosDb = db, leiosEbBodies, leiosEbBuffer = buf} = leiosContext
    let ebId = fst $ ebIdFromPoint p leiosEbBodies
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
    let MkLeiosFetchContext {leiosDb = db, leiosEbBodies, leiosEbTxsBuffer = buf} = leiosContext
    let ebId = fst $ ebIdFromPoint p leiosEbBodies
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
