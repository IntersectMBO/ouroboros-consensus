{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import           Cardano.Binary (serialize')
import qualified Cardano.Crypto.Hash as Hash
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import           Control.Monad (foldM, when)
import qualified Data.Aeson as JSON
import qualified Data.Bits as Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (forM_)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import           Data.List (intercalate, isSuffixOf, unfoldr)
import           Data.String (fromString)
import qualified Data.Vector as V
import           Data.Word (Word8, Word16, Word32, Word64)
import qualified Database.SQLite3.Direct as DB
import           GHC.Generics (Generic)
import qualified Numeric
import           System.Directory (doesFileExist)
import           System.Environment (getArgs)
import           System.Exit (die)
import qualified System.Random as R
import qualified System.Random.Stateful as R
import           Text.Read (readMaybe)

main :: IO ()
main = getArgs >>= \case
    ["generate", dbPath, manifestPath]
      | ".db" `isSuffixOf` dbPath
      , ".json" `isSuffixOf` manifestPath
      -> do
        doesFileExist dbPath >>= \case
            True -> die "database path must not exist"
            False -> pure ()
        manifest <- fmap JSON.eitherDecode (BSL.readFile manifestPath) >>= \case
            Left err -> die err
            Right x -> pure x
        db <- withDieMsg $ DB.open (fromString dbPath)
        prng0 <- R.initStdGen
        generateDb prng0 db manifest
    ["MsgLeiosBlockRequest", dbPath, ebIdStr]
      | ".db" `isSuffixOf` dbPath
      , Just ebId <- readMaybe ebIdStr
      -> do
        db <- withDieMsg $ DB.open (fromString dbPath)
        msgLeiosBlockRequest db ebId
    ["MsgLeiosBlock", dbPath, ebIdStr, ebSlotStr, ebPath]
      | ".db" `isSuffixOf` dbPath
      , ".bin" `isSuffixOf` ebPath
      , Just ebId <- readMaybe ebIdStr
      , Just ebSlot <- readMaybe ebSlotStr
      -> do
        db <- withDieMsg $ DB.open (fromString dbPath)
        msgLeiosBlock db ebId ebSlot ebPath
    "MsgLeiosBlockTxsRequest" : dbPath : ebIdStr : bitmapChunkStrs
      | ".db" `isSuffixOf` dbPath
      , Just ebId <- readMaybe ebIdStr
      , Just bitmaps <- parseBitmaps bitmapChunkStrs
      -> do
        db <- withDieMsg $ DB.open (fromString dbPath)
        msgLeiosBlockTxsRequest db ebId bitmaps
    "MsgLeiosBlockTxs" : dbPath : ebIdStr : ebTxsPath : bitmapChunkStrs
      | ".db" `isSuffixOf` dbPath
      , ".bin" `isSuffixOf` ebTxsPath
      , Just ebId <- readMaybe ebIdStr
      , Just bitmaps <- parseBitmaps bitmapChunkStrs
      -> do
        db <- withDieMsg $ DB.open (fromString dbPath)
        msgLeiosBlockTxs db ebId ebTxsPath bitmaps
    "fetch-decision" : dbPath : ebIdStrs
      | ".db" `isSuffixOf` dbPath
      , Just ebIds <- sequence $ map readMaybe ebIdStrs
      , not (null ebIds)
      -> do
        db <- withDieMsg $ DB.open (fromString dbPath)
        fetchDecision db (IntSet.fromList ebIds)
    ["hash-txs", ebTxsPath]
      | ".bin" `isSuffixOf` ebTxsPath
      -> do
        hashTxs ebTxsPath
    _ -> die "Either $0 generate myDatabase.db myManifest.json\n\
             \    OR $0 MsgLeiosBlockRequest myDatabase.db ebId\n\
             \    OR $0 MsgLeiosBlock myDatabase.db ebId myEb.bin\n\
             \    OR $0 MsgLeiosBlockTxsRequest myDatabase.db ebId index16:bitmap64 index16:bitmap64 index16:bitmap64 ...\n\
             \    OR $0 MsgLeiosBlockTxs myDatabase.db ebId myEbTxs.bin index16:bitmap64 index16:bitmap64 index16:bitmap64 ...\n\
             \    OR $0 fetch-decision myDatabase.db ebId ebId ebId ...\n\
             \    OR $0 hash-txs myEbTxs.bin\n\
             \"

parseBitmaps :: [String] -> Maybe [(Word16, Word64)]
parseBitmaps =
    go []
  where
    go acc = \case
        [] -> Just (reverse acc)
        bitmapChunkStr : bitmapChunkStrs
          | (idxStr, _:bitmapStr) <- break (==':') bitmapChunkStr
          , Just idx <- readMaybe idxStr
          , Just bitmap <- readMaybe bitmapStr
          -> go ((idx, bitmap) : acc) bitmapChunkStrs
        _ -> Nothing

data EbRecipe = EbRecipe {
    slotNo :: Word64
  ,
    txByteSizes :: V.Vector Word16
  }
  deriving (Generic, Show)

-- | defaults to @GHC.Generics@
instance JSON.FromJSON EbRecipe where {}

-----

type HASH = Hash.Blake2b_256

generateDb :: R.RandomGen g => g -> DB.Database -> [EbRecipe] -> IO ()
generateDb prng0 db ebRecipes = do
    gref <- R.newIOGenM prng0
    -- init db
    withDieMsg $ DB.exec db (fromString sql_schema)
    stmt_write_ebId <- withDieJust $ DB.prepare db (fromString sql_insert_ebId)
    stmt_write_ebClosure <- withDieJust $ DB.prepare db (fromString sql_insert_ebClosure)
    -- loop over EBs (one SQL transaction each, to be gentle)
    forM_ ([(0 :: Word16) ..] `zip` ebRecipes) $ \(ebId, ebRecipe) -> do
        -- generate txs, so we have their hashes
        txs <- V.forM (txByteSizes ebRecipe) $ \txByteSize -> do
            -- generate a random bytestring whose CBOR encoding has the expected length
            --
            -- In the actual implementation, the values themselves will be
            -- valid CBOR. It's useful to maintain that invariant even for the
            -- otherwise-opaque random data within this prototype/demo.
            when (txByteSize < 55) $ die "Tx cannot be smaller than 55 bytes"
            let overhead   -- one for the initial byte, plus 1 2 4 or 8 for the length argument
                  | txByteSize < fromIntegral (maxBound :: Word8) = 2
                  | txByteSize <              (maxBound :: Word16) = 3
                  | txByteSize < fromIntegral (maxBound :: Word32) = 5
                  | otherwise = 9
            txBytes <- id
              $ fmap (serialize' . CBOR.encodeBytes)
              $ R.uniformByteStringM (fromIntegral txByteSize - overhead) gref
            pure (txBytes, Hash.hashWith id txBytes :: Hash.Hash HASH ByteString)
        let ebSlot = slotNo ebRecipe
        let ebHash :: Hash.Hash HASH ByteString
            ebHash =
                Hash.castHash
              $ Hash.hashWithSerialiser
                    (encodeEB (fromIntegral . BS.length) Hash.hashToBytes)
                    txs
        withDieMsg $ DB.exec db (fromString "BEGIN")
        withDie $ DB.bindInt64 stmt_write_ebId   3 (fromIntegral ebId)
        withDie $ DB.bindInt64 stmt_write_ebClosure 1 (fromIntegral ebId)
        -- INSERT INTO ebPoints
        withDie $ DB.bindInt64    stmt_write_ebId 1 (fromIntegral ebSlot)
        withDie $ DB.bindBlob     stmt_write_ebId 2 (Hash.hashToBytes ebHash)
        withDieDone $ DB.stepNoCB stmt_write_ebId
        withDie $ DB.reset        stmt_write_ebId
        -- loop over txs
        V.iforM_ txs $ \txOffset (txBytes, txHash) -> do
            -- INSERT INTO ebTxs
            withDie $ DB.bindInt64    stmt_write_ebClosure 2 (fromIntegral txOffset)
            withDie $ DB.bindBlob     stmt_write_ebClosure 3 (Hash.hashToBytes txHash)
            withDie $ DB.bindInt64    stmt_write_ebClosure 4 (fromIntegral (BS.length txBytes))
            withDie $ DB.bindBlob     stmt_write_ebClosure 5 txBytes
            withDieDone $ DB.stepNoCB stmt_write_ebClosure
            withDie $ DB.reset        stmt_write_ebClosure
        -- finalize each EB
        withDieMsg $ DB.exec db (fromString "COMMIT")
    -- finalize db
    withDieMsg $ DB.exec db (fromString sql_index_schema)

-----

sql_schema :: String
sql_schema =
    "CREATE TABLE txCache (\n\
    \    txHashBytes BLOB NOT NULL PRIMARY KEY   -- raw bytes\n\
    \  ,\n\
    \    txBytes BLOB NOT NULL   -- valid CBOR\n\
    \  ,\n\
    \    expiryUnixEpoch INTEGER NOT NULL\n\
    \  ) WITHOUT ROWID;\n\
    \\n\
    \CREATE TABLE ebPoints (\n\
    \    ebSlot INTEGER NOT NULL\n\
    \  ,\n\
    \    ebHash BLOB NOT NULL\n\
    \  ,\n\
    \    ebId INTEGER NOT NULL\n\
    \  ,\n\
    \    PRIMARY KEY (ebSlot, ebHash)\n\
    \  ) WITHOUT ROWID;\n\
    \\n\
    \CREATE TABLE ebTxs (\n\
    \    ebId INTEGER NOT NULL   -- foreign key ebPoints.ebId\n\
    \  ,\n\
    \    txOffset INTEGER NOT NULL\n\
    \  ,\n\
    \    txHashBytes BLOB NOT NULL   -- raw bytes\n\
    \  ,\n\
    \    txByteSize INTEGER NOT NULL\n\
    \  ,\n\
    \    txBytes BLOB   -- valid CBOR\n\
    \  ,\n\
    \    PRIMARY KEY (ebId, txOffset)\n\
    \  ) WITHOUT ROWID;\n\
    \"

sql_index_schema :: String
sql_index_schema =
    "CREATE INDEX ebPointsExpiry\n\
    \    ON ebPoints (ebSlot, ebId);   -- Helps with the eviction policy of the EbStore.\n\
    \\n\
    \CREATE INDEX txCacheExpiry\n\
    \    ON txCache (expiryUnixEpoch, txHashBytes);   -- Helps with the eviction policy of the TxCache.\n\
    \\n\
    \CREATE INDEX missingEbTxs\n\
    \    ON ebTxs (ebId, txOffset)\n\
    \    WHERE txBytes IS NULL;   -- Helps with fetch logic decisions.\n\
    \\n\
    \CREATE INDEX acquiredEbTxs\n\
    \    ON ebTxs (ebId, txOffset)\n\
    \    WHERE txBytes IS NOT NULL;   -- Helps with fetch logic decisions.\n\
    \"

sql_insert_ebId :: String
sql_insert_ebId =
    "INSERT INTO ebPoints (ebSlot, ebHash, ebId) VALUES (?, ?, ?)\n\
    \"

sql_insert_ebClosure :: String
sql_insert_ebClosure =
    "INSERT INTO ebTxs (ebId, txOffset, txHashBytes, txByteSize, txBytes) VALUES (?, ?, ?, ?, ?)\n\
    \"

-----

withDiePoly :: Show b => (e -> b) -> IO (Either e a) -> IO a
withDiePoly f io =
    io >>= \case
        Left e -> die $ show $ f e
        Right x -> pure x

withDieMsg :: IO (Either (DB.Error, DB.Utf8) a) -> IO a
withDieMsg = withDiePoly snd

withDie :: IO (Either DB.Error a) -> IO a
withDie = withDiePoly id

withDieDone :: IO (Either DB.Error DB.StepResult) -> IO ()
withDieDone io =
    withDie io >>= \case
        DB.Row -> die "impossible!"
        DB.Done -> pure ()

withDieJust :: IO (Either DB.Error (Maybe a)) -> IO a
withDieJust io =
    withDie io >>= \case
        Nothing -> die "impossible!"
        Just x -> pure x

-----

encodeEbPair :: (b -> Word16) -> (h -> ByteString) -> (b, h) -> CBOR.Encoding
encodeEbPair bytesToLen hashToBytes (txBytes, txHash) =
    CBOR.encodeBytes (hashToBytes txHash)
 <> CBOR.encodeWord16 (bytesToLen txBytes)

encodeEB :: Foldable f => (b -> Word16) -> (h -> ByteString) -> f (b, h) -> CBOR.Encoding
encodeEB bytesToLen hashToBytes ebPairs =
    CBOR.encodeMapLenIndef
 <> foldr
        (\x r -> encodeEbPair bytesToLen hashToBytes x <> r)
        CBOR.encodeBreak
        ebPairs

decodeEbPair :: CBOR.Decoder s (ByteString, Word16)
decodeEbPair =
    (,) <$> CBOR.decodeBytes <*> CBOR.decodeWord16

-- | The logic in this module instead does this decoding incrementally
_decodeEB :: CBOR.Decoder s (X (ByteString, Word16))
_decodeEB =
    CBOR.decodeMapLenIndef
 *> CBOR.decodeSequenceLenIndef
       pushX
       emptyX
       id
       decodeEbPair

-----

-- | helper for msgLeiosBlockRequest and msgLeiosBlockTxsRequest
--
-- The @[a]@ is less than 1024 long.
--
-- Each 'V.Vector' is exactly 1024 long.
--
-- TODO those functions could instead generate the CBOR incrementally, but will
-- the patched node be able to do that?
data X a = X !Word16 [a] [V.Vector a]
  deriving (Functor, Foldable)

emptyX :: X a
emptyX = X 0 [] []

pushX :: X a -> a -> X a
pushX (X n xs vs) x =
    if n < 1024 then X (n+1) (x : xs) vs else
    X 1 [x] (V.fromList xs : vs)

msgLeiosBlockRequest :: DB.Database -> Int -> IO ()
msgLeiosBlockRequest db ebId = do
    -- get the EB items
    stmt <- withDieJust $ DB.prepare db (fromString sql_lookup_ebBodies_DESC)
    withDie $ DB.bindInt64 stmt 1 (fromIntegral ebId)
    let loop !acc =
            withDie (DB.stepNoCB stmt) >>= \case
                DB.Done -> pure acc
                DB.Row -> do
                    -- TODO use a sink buffer to avoid polluting the heap with these temporary copies?
                    txHashBytes <- DB.columnBlob stmt 0
                    txByteSize <- DB.columnInt64 stmt 1
                    loop $ pushX acc (txByteSize, txHashBytes)
    acc <- loop emptyX
    -- combine the EB items
    BS.putStr
      $ BS16.encode
      $ serialize'
      $ encodeEB fromIntegral id acc
    putStrLn ""

-- | It's DESCending because the accumulator within the 'msgLeiosBlockRequest'
-- logic naturally reverses it
sql_lookup_ebBodies_DESC :: String
sql_lookup_ebBodies_DESC =
    "SELECT txHashBytes, txByteSize FROM ebTxs\n\
    \WHERE ebId = ?\n\
    \ORDER BY txOffset DESC\n\
    \"

msgLeiosBlockTxsRequest :: DB.Database -> Int -> [(Word16, Word64)] -> IO ()
msgLeiosBlockTxsRequest db ebId bitmaps = do
    do
        let idxs = map fst bitmaps
        let idxLimit = maxEbItems `div` 64
        when (any (== 0) $ map snd bitmaps) $ do
            die "A bitmap is zero"
        when (flip any idxs (> fromIntegral idxLimit)) $ do
            die $ "An offset exceeds the theoretical limit " <> show idxLimit
        when (not $ and $ zipWith (<) idxs (tail idxs)) $ do
            die "Offsets not strictly ascending"
    let numOffsets = sum $ map (Bits.popCount . snd) bitmaps
    let nextOffsetDESC = \case
            [] -> Nothing
            (idx, bitmap) : k -> case popRightmostOffset bitmap of
                Nothing           -> nextOffsetDESC k
                Just (i, bitmap') ->
                    Just (64 * fromIntegral idx + i, (idx, bitmap') : k)
        offsets = unfoldr nextOffsetDESC (reverse bitmaps)
    -- get the txs, at most 'maxBatchSize' at a time
    --
    -- TODO Better workaround for requests of many txs?
    stmt_lookup_ebClosuresMAIN <- withDieJust $ DB.prepare db $ fromString $ sql_lookup_ebClosures_DESC (maxBatchSize `min` numOffsets)
    withDie $ DB.bindInt64 stmt_lookup_ebClosuresMAIN 1 (fromIntegral ebId)
    withDieMsg $ DB.exec db (fromString "BEGIN")
    acc <- (\f -> foldM f emptyX (batches offsets)) $ \acc batch -> do
        stmt <-
          if numOffsets <= maxBatchSize || length batch == maxBatchSize then pure stmt_lookup_ebClosuresMAIN else do
            -- this can only be reached for the last batch
            withDie $ DB.finalize stmt_lookup_ebClosuresMAIN
            stmt_lookup_ebClosuresTIDY <- withDieJust $ DB.prepare db $ fromString $ sql_lookup_ebClosures_DESC (numOffsets `mod` maxBatchSize)
            withDie $ DB.bindInt64 stmt_lookup_ebClosuresTIDY 1 (fromIntegral ebId)
            pure stmt_lookup_ebClosuresTIDY
        forM_ ([(2 :: DB.ParamIndex) ..] `zip` batch) $ \(i, offset) -> do
            withDie $ DB.bindInt64 stmt i (fromIntegral offset)
        acc' <- (\f -> foldM f acc batch) $ \acc' offset -> do
            withDie (DB.stepNoCB stmt) >>= \case
                DB.Done -> die $ "No rows starting at offset: " ++ show offset
                DB.Row -> do
                    -- TODO use a sink buffer to avoid polluting the heap with these temporary copies?
                    txOffset <- DB.columnInt64 stmt 0
                    txBytes <- DB.columnBlob stmt 1
                    when (txOffset /= fromIntegral offset) $ die $ "Missing offset: " <> show offset
                    pure $ pushX acc' txBytes
        withDie $ DB.reset stmt
        pure acc'
    withDieMsg $ DB.exec db (fromString "COMMIT")
    -- combine the txs
    BS.putStr
      $ BS16.encode
      $ serialize'
      $ CBOR.encodeListLenIndef <> foldr (\bs r -> CBOR.encodePreEncoded bs <> r) CBOR.encodeBreak acc
    putStrLn ""

maxEbBodyByteSize :: Int
maxEbBodyByteSize = 500000

minEbItemByteSize :: Int
minEbItemByteSize = (1 + 32 + 1) + (1 + 1)

maxEbItems :: Int
maxEbItems = (negate 1 + maxEbBodyByteSize - 1) `div` minEbItemByteSize

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

{- | For example
@
  print $ unfoldr popRightmostOffset 0
  print $ unfoldr popRightmostOffset 1
  print $ unfoldr popRightmostOffset (2^(34 :: Int))
  print $ unfoldr popRightmostOffset (2^(63 :: Int) + 2^(62 :: Int) + 8)
  []
  [63]
  [29]
  [60,1,0]
@
-}
popRightmostOffset :: Word64 -> Maybe (Int, Word64)
{-# INLINE popRightmostOffset #-}
popRightmostOffset = \case
    0 -> Nothing
    w -> let zs = Bits.countTrailingZeros w
         in
         Just (63 - zs, Bits.clearBit w zs)

-- | Never request more than this many txs simultaneously
--
-- TODO confirm this prevents the query string from exceeding SQLite's size
-- limits, even if the largest possible txOffsets are being requested.
maxBatchSize :: Int
maxBatchSize = 1024

batches :: [a] -> [[a]]
batches xs = if null xs then [] else take maxBatchSize xs : batches (drop maxBatchSize xs)

-- | It's DESCending because the accumulator within the
-- 'msgLeiosBlockTxsRequest' logic naturally reverses it
sql_lookup_ebClosures_DESC :: Int -> String
sql_lookup_ebClosures_DESC n =
    "SELECT txOffset, txBytes FROM ebTxs\n\
    \WHERE ebId = ? AND txBytes IS NOT NULL AND txOffset IN (" ++ hooks ++ ")\n\
    \ORDER BY txOffset DESC\n\
    \"
  where
    hooks = intercalate ", " (replicate n "?")

-----

-- | PREREQ: the file is the CBOR encoding (binary, not hex) of the payload of a MsgLeiosBlock
--
-- PREREQ: No row in ebTxs already has this ebId.
msgLeiosBlock :: DB.Database -> Int -> Word64 -> FilePath -> IO ()
msgLeiosBlock db ebId ebSlot ebPath = do
    ebBytes <- BS.readFile ebPath
    let ebHash :: Hash.Hash HASH ByteString
        ebHash = Hash.castHash $ Hash.hashWith id ebBytes
    stmt_write_ebIds <- withDieJust $ DB.prepare db (fromString sql_insert_ebId)
    stmt_write_ebBodies <- withDieJust $ DB.prepare db (fromString sql_insert_ebBody)
    withDieMsg $ DB.exec db (fromString "BEGIN")
    -- INSERT INTO ebPoints
    withDie $ DB.bindInt64    stmt_write_ebIds 1 (fromIntegral ebSlot)
    withDie $ DB.bindBlob     stmt_write_ebIds 2 (Hash.hashToBytes ebHash)
    withDie $ DB.bindInt64    stmt_write_ebIds 3 (fromIntegral ebId)
    withDieDone $ DB.stepNoCB stmt_write_ebIds
    withDie $ DB.reset        stmt_write_ebIds
    -- decode incrementally and simultaneously INSERT INTO ebTxs
    withDie $ DB.bindInt64 stmt_write_ebBodies 1 (fromIntegral ebId)
    let decodeBreakOrEbPair = do
            stop <- CBOR.decodeBreakOr
            if stop then pure Nothing else Just <$> decodeEbPair
    let go1 txOffset bytes = do
            (bytes', next) <- withDiePoly id $ pure $ CBOR.deserialiseFromBytes decodeBreakOrEbPair bytes
            go2 txOffset bytes' next
        go2 txOffset bytes = \case
            Just (txHashBytes, txByteSize) -> do
                withDie $ DB.bindInt64    stmt_write_ebBodies 2 txOffset
                withDie $ DB.bindBlob     stmt_write_ebBodies 3 txHashBytes
                withDie $ DB.bindInt64    stmt_write_ebBodies 4 (fromIntegral txByteSize)
                withDieDone $ DB.stepNoCB stmt_write_ebBodies
                withDie $ DB.reset        stmt_write_ebBodies
                go1 (txOffset + 1) bytes
            Nothing
              | not (BSL.null bytes) -> die "Incomplete EB decode"
              | otherwise -> pure ()
    (ebBytes2, ()) <- withDiePoly id $ pure $ CBOR.deserialiseFromBytes CBOR.decodeMapLenIndef $ BSL.fromStrict ebBytes
    go1 0 ebBytes2
    -- finalize the EB
    withDieMsg $ DB.exec db (fromString "COMMIT")

sql_insert_ebBody :: String
sql_insert_ebBody =
    "INSERT INTO ebTxs (ebId, txOffset, txHashBytes, txByteSize, txBytes) VALUES (?, ?, ?, ?, NULL)\n\
    \"

-- | PREREQ: the file is the CBOR encoding (binary, not hex) of the payload of a MsgLeiosBlockTxs
msgLeiosBlockTxs :: DB.Database -> Int -> FilePath -> [(Word16, Word64)] -> IO ()
msgLeiosBlockTxs db ebId ebTxsPath bitmaps = do
    ebTxsBytes <- BSL.readFile ebTxsPath
    stmt <- withDieJust $ DB.prepare db (fromString sql_insert_ebTx)
    withDie $ DB.bindInt64 stmt 2 (fromIntegral ebId)
    withDieMsg $ DB.exec db (fromString "BEGIN")
    -- decode incrementally and simultaneously UPDATE ebTxs
    --
    -- TODO also INSERT INTO TxCache
    let decodeBreakOrTx = do
            stop <- CBOR.decodeBreakOr
            if stop then pure Nothing else Just <$> CBOR.decodeBytes
    let go1 offsets bytes = do
            (bytes', next) <- withDiePoly id $ pure $ CBOR.deserialiseFromBytes decodeBreakOrTx bytes
            go2 offsets bytes' next
        go2 offsets bytes = \case
            Just txBytes -> case offsets of
                [] -> die "Too many txs"
                txOffset:offsets' -> do
                    withDie $ DB.bindInt64    stmt 3 $ fromIntegral txOffset
                    withDie $ DB.bindBlob     stmt 1 $ serialize' $ CBOR.encodeBytes txBytes
                    withDieDone $ DB.stepNoCB stmt
                    withDie $ DB.reset        stmt
                    go1 offsets' bytes
            Nothing
              | not (BSL.null bytes) -> die "Incomplete EB txs decode"
              | txOffset:_ <- offsets -> die $ "Too few EB txs; next is " <> show txOffset
              | otherwise -> pure ()
    let nextOffset = \case
            [] -> Nothing
            (idx, bitmap) : k -> case popLeftmostOffset bitmap of
                Nothing           -> nextOffset k
                Just (i, bitmap') ->
                    Just (64 * fromIntegral idx + i, (idx, bitmap') : k)
        offsets = unfoldr nextOffset bitmaps
    (ebTxsBytes2, ()) <- withDiePoly id $ pure $ CBOR.deserialiseFromBytes CBOR.decodeListLenIndef ebTxsBytes
    go1 offsets ebTxsBytes2
    -- finalize the EB
    withDieMsg $ DB.exec db (fromString "COMMIT")

sql_insert_ebTx :: String
sql_insert_ebTx =
    "UPDATE ebTxs\n\
    \SET txBytes = ?\n\
    \WHERE ebId = ? AND txOffset = ? AND txBytes IS NULL\n\
    \"

-----

_maxTxOffsetBitWidth :: Int
_maxTxOffsetBitWidth = ceiling $ log (fromIntegral maxEbItems :: Double) / log 2

maxRequestsPerIteration :: Int
maxRequestsPerIteration = 10

maxByteSizePerRequest :: Int
maxByteSizePerRequest = 500000

fetchDecision :: DB.Database -> IntSet.IntSet -> IO ()
fetchDecision db ebIds = do
    stmt <- withDieJust $ DB.prepare db $ fromString $ sql_next_fetch (IntSet.size ebIds)
    forM_ ([(1 :: DB.ParamIndex) ..] `zip` IntSet.toDescList ebIds) $ \(i, p) -> do
        withDie $ DB.bindInt64 stmt i (fromIntegral p)
    let loopLimit = maxRequestsPerIteration * maxByteSizePerRequest
        loop !accReqs !accByteSize =
            if accByteSize >= loopLimit then pure accReqs else
            withDie (DB.stepNoCB stmt) >>= \case
                DB.Done -> pure accReqs
                DB.Row -> do
                    ebId <- fromIntegral <$> DB.columnInt64 stmt 0
                    txOffset <- fromIntegral <$> DB.columnInt64 stmt 1
                    txHash <- DB.columnBlob stmt 2
                    txByteSize <- fromIntegral <$> DB.columnInt64 stmt 3
                    loop
                        (IntMap.insertWith
                            IntMap.union
                            ebId
                            (IntMap.singleton txOffset txHash)
                            accReqs
                        )
                        (accByteSize + txByteSize)
    reqs <- loop IntMap.empty 0
    forM_ (IntMap.assocs reqs) $ \(ebId, m) -> do
        let sho idx bitmap k =
                if (0 :: Word64) == bitmap then k else
                (show idx ++ ":0x" ++ Numeric.showHex bitmap "") : k
            go idx bitmap = \case
                [] -> sho idx bitmap []
                txOffset:txOffsets ->
                    let (q, r) = txOffset `quotRem` 64
                    in
                    if q == idx
                    then go idx (Bits.setBit bitmap (63 - r)) txOffsets else
                      (if 0 /= bitmap then sho idx bitmap else id)
                    $ go q (Bits.bit (63 - r)) txOffsets
        putStrLn
          $ unwords
          $ "bitmaps" : show ebId : go 0 (0x0 :: Word64) (IntMap.keys m)
        putStrLn
          $ unwords
          $ "hashes" : show ebId : map (BS8.unpack . BS16.encode) (IntMap.elems m)

-- | Arbitrarily limited to 2000; about 2000 average txs are in the ball park
-- of one megabyte.
--
-- If a prefix of the 2000 txs are large, the fetch logic can ignore the rest.
--
-- If all 2000 are still much less than a megabyte, then a) the EB is
-- suspicious and b) the fetch logic can advance the query (TODO require
-- parameterizing this query string with an OFFSET).
sql_next_fetch :: Int -> String
sql_next_fetch n =
    "SELECT ebId, txOffset, txHashBytes, txByteSize FROM ebTxs\n\
    \WHERE txBytes IS NULL AND ebId IN (" ++ hooks ++ ")\n\
    \ORDER BY ebId DESC, txOffset ASC\n\
    \LIMIT 2000\n\
    \"
  where
    hooks = intercalate ", " (replicate n "?")

-----

-- | PREREQ: the file is the CBOR encoding (binary, not hex) of the payload of a MsgLeiosBlockTxs
hashTxs :: FilePath -> IO ()
hashTxs ebTxsPath = do
    ebTxsBytes <- BSL.readFile ebTxsPath
    let decodeBreakOrTx = do
            stop <- CBOR.decodeBreakOr
            if stop then pure Nothing else Just <$> CBOR.decodeBytes
    let go1 bytes = do
            (bytes', next) <- withDiePoly id $ pure $ CBOR.deserialiseFromBytes decodeBreakOrTx bytes
            go2 bytes bytes' next
        go2 prevBytes bytes = \case
            Just _txBytes -> do
                let len = BSL.length prevBytes - BSL.length bytes
                    txHash :: Hash.Hash HASH ByteString
                    txHash = Hash.hashWith id $ BSL.toStrict $ BSL.take len prevBytes
                putStrLn $ BS8.unpack $ BS16.encode $ Hash.hashToBytes txHash
                go1 bytes
            Nothing ->
                when (not $ BSL.null bytes) $ do
                    die "Incomplete EB txs decode"
    (ebTxsBytes2, ()) <- withDiePoly id $ pure $ CBOR.deserialiseFromBytes CBOR.decodeListLenIndef ebTxsBytes
    go1 ebTxsBytes2
