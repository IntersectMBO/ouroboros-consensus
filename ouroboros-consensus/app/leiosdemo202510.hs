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
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (forM_)
import           Data.List (intercalate, isSuffixOf, unfoldr)
import           Data.String (fromString)
import qualified Data.Vector as V
import           Data.Word (Word8, Word16, Word32, Word64)
import qualified Database.SQLite3.Direct as DB
import           GHC.Generics (Generic)
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
    ["MsgLeiosBlockRequest", dbPath, ebPointStr]
      | ".db" `isSuffixOf` dbPath
      , Just ebPoint <- readMaybe ebPointStr
      -> do
        db <- withDieMsg $ DB.open (fromString dbPath)
        msgLeiosBlockRequest db ebPoint
    ["MsgLeiosBlock", dbPath, ebPointStr, ebSlotStr, ebPath]
      | ".db" `isSuffixOf` dbPath
      , ".bin" `isSuffixOf` ebPath
      , Just ebPoint <- readMaybe ebPointStr
      , Just ebSlot <- readMaybe ebSlotStr
      -> do
        db <- withDieMsg $ DB.open (fromString dbPath)
        msgLeiosBlock db ebPoint ebSlot ebPath
    "MsgLeiosBlockTxsRequest" : dbPath : ebPointStr : bitmapChunkStrs
      | ".db" `isSuffixOf` dbPath
      , Just ebPoint <- readMaybe ebPointStr
      , Just bitmaps <- parseBitmaps bitmapChunkStrs
      -> do
        db <- withDieMsg $ DB.open (fromString dbPath)
        msgLeiosBlockTxsRequest db ebPoint bitmaps
    "MsgLeiosBlockTxs" : dbPath : ebPointStr : ebTxsPath : bitmapChunkStrs
      | ".db" `isSuffixOf` dbPath
      , ".bin" `isSuffixOf` ebTxsPath
      , Just ebPoint <- readMaybe ebPointStr
      , Just bitmaps <- parseBitmaps bitmapChunkStrs
      -> do
        db <- withDieMsg $ DB.open (fromString dbPath)
        msgLeiosBlockTxs db ebPoint ebTxsPath bitmaps
    _ -> die "Either $0 generate myDatabase.db myManifest.json\n\
             \    OR $0 MsgLeiosBlockRequest myDatabase.db ebPoint(int)\n\
             \    OR $0 MsgLeiosBlock myDatabase.db ebPoint(int) myEb.bin\n\
             \    OR $0 MsgLeiosBlockTxsRequest myDatabase.db ebPoint(int) index16:bitmap64 index16:bitmap64 index16:bitmap64 ...\n\
             \    OR $0 MsgLeiosBlockTxs myDatabase.db ebPoint(int) myEbTxs.bin index16:bitmap64 index16:bitmap64 index16:bitmap64 ...\n\
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
    stmt_write_ebPoint <- withDieJust $ DB.prepare db (fromString sql_insert_ebPoint)
    stmt_write_ebClosure <- withDieJust $ DB.prepare db (fromString sql_insert_ebClosure)
    -- loop over EBs (one SQL transaction each, to be gentle)
    forM_ ([(0 :: Word16) ..] `zip` ebRecipes) $ \(ebPoint, ebRecipe) -> do
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
        withDie $ DB.bindInt64 stmt_write_ebPoint   3 (fromIntegral ebPoint)
        withDie $ DB.bindInt64 stmt_write_ebClosure 1 (fromIntegral ebPoint)
        -- INSERT INTO ebPoints
        withDie $ DB.bindInt64    stmt_write_ebPoint 1 (fromIntegral ebSlot)
        withDie $ DB.bindBlob     stmt_write_ebPoint 2 (Hash.hashToBytes ebHash)
        withDieDone $ DB.stepNoCB stmt_write_ebPoint
        withDie $ DB.reset        stmt_write_ebPoint
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
    \    id INTEGER NOT NULL\n\
    \  ,\n\
    \    PRIMARY KEY (ebSlot, ebHash)\n\
    \  ) WITHOUT ROWID;\n\
    \\n\
    \CREATE TABLE ebTxs (\n\
    \    ebPoint INTEGER NOT NULL   -- foreign key ebPoints.id\n\
    \  ,\n\
    \    txOffset INTEGER NOT NULL\n\
    \  ,\n\
    \    txHashBytes BLOB NOT NULL   -- raw bytes\n\
    \  ,\n\
    \    txByteSize INTEGER NOT NULL\n\
    \  ,\n\
    \    txBytes BLOB   -- valid CBOR\n\
    \  ,\n\
    \    PRIMARY KEY (ebPoint, txOffset)\n\
    \  ) WITHOUT ROWID;\n\
    \"

sql_index_schema :: String
sql_index_schema =
    "CREATE INDEX ebPointsExpiry\n\
    \    ON ebPoints (ebSlot, id);   -- Helps with the eviction policy of the EbStore.\n\
    \\n\
    \CREATE INDEX txCacheExpiry\n\
    \    ON txCache (expiryUnixEpoch, txHashBytes);   -- Helps with the eviction policy of the TxCache.\n\
    \\n\
    \CREATE INDEX missingEbTxs\n\
    \    ON ebTxs (ebPoint, txOffset)\n\
    \    WHERE txBytes IS NULL;   -- Helps with fetch logic decisions.\n\
    \\n\
    \CREATE INDEX acquiredEbTxs\n\
    \    ON ebTxs (ebPoint, txOffset)\n\
    \    WHERE txBytes IS NOT NULL;   -- Helps with fetch logic decisions.\n\
    \"

sql_insert_ebPoint :: String
sql_insert_ebPoint =
    "INSERT INTO ebPoints (ebSlot, ebHash, id) VALUES (?, ?, ?)\n\
    \"

sql_insert_ebClosure :: String
sql_insert_ebClosure =
    "INSERT INTO ebTxs (ebPoint, txOffset, txHashBytes, txByteSize, txBytes) VALUES (?, ?, ?, ?, ?)\n\
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

_decodeEB :: CBOR.Decoder s (X (ByteString, Word16))
_decodeEB =
    CBOR.decodeMapLenIndef
 *> CBOR.decodeSequenceLenIndef
       pushX
       emptyX
       id
       decodeEbPair

-----

-- | helper for msgLeiosBlockRequest
--
-- The @[a]@ is less than 1024 long.
--
-- Each 'V.Vector' is exactly 1024 long.
data X a = X !Word16 [a] [V.Vector a]
  deriving (Functor, Foldable)

emptyX :: X a
emptyX = X 0 [] []

pushX :: X a -> a -> X a
pushX (X n xs vs) x =
    if n < 1024 then X (n+1) (x : xs) vs else
    X 1 [x] (V.fromList xs : vs)

msgLeiosBlockRequest :: DB.Database -> Int -> IO ()
msgLeiosBlockRequest db ebPoint = do
    -- get the EB items
    stmt_lookup_ebBodies <- withDieJust $ DB.prepare db (fromString sql_lookup_ebBodies_DESC)
    withDie $ DB.bindInt64 stmt_lookup_ebBodies 1 (fromIntegral ebPoint)
    let loop !acc = do
            withDie (DB.stepNoCB stmt_lookup_ebBodies) >>= \case
                DB.Done -> pure acc
                DB.Row -> do
                    -- TODO use a sink buffer to avoid polluting the heap with these temporary copies?
                    txHashBytes <- DB.columnBlob stmt_lookup_ebBodies 0
                    txByteSize <- DB.columnInt64 stmt_lookup_ebBodies 1
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
    \WHERE ebPoint = ?\n\
    \ORDER BY txOffset DESC\n\
    \"

msgLeiosBlockTxsRequest :: DB.Database -> Int -> [(Word16, Word64)] -> IO ()
msgLeiosBlockTxsRequest db ebPoint bitmaps = do
    do
        let idxs = map fst bitmaps
        let maxEbByteSize = 12500000 :: Int
            minTxByteSize = 55
            idxLimit = (maxEbByteSize `div` minTxByteSize) `div` 64
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
    withDie $ DB.bindInt64 stmt_lookup_ebClosuresMAIN 1 (fromIntegral ebPoint)
    withDieMsg $ DB.exec db (fromString "BEGIN")
    acc <- (\f -> foldM f emptyX (batches offsets)) $ \acc batch -> do
        stmt <-
          if numOffsets <= maxBatchSize || length batch == maxBatchSize then pure stmt_lookup_ebClosuresMAIN else do
            -- this can only be reached for the last batch
            withDie $ DB.finalize stmt_lookup_ebClosuresMAIN
            stmt_lookup_ebClosuresTIDY <- withDieJust $ DB.prepare db $ fromString $ sql_lookup_ebClosures_DESC (numOffsets `mod` maxBatchSize)
            withDie $ DB.bindInt64 stmt_lookup_ebClosuresTIDY 1 (fromIntegral ebPoint)
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
-- TODO confirm this prevents the query string from exceeding its size limits,
-- even if the largest txOffsets are being requested.
maxBatchSize :: Int
maxBatchSize = 1024

batches :: [a] -> [[a]]
batches xs = if null xs then [] else take maxBatchSize xs : batches (drop maxBatchSize xs)

-- | It's DESCending because the accumulator within the
-- 'msgLeiosBlockTxsRequest' logic naturally reverses it
sql_lookup_ebClosures_DESC :: Int -> String
sql_lookup_ebClosures_DESC n =
    "SELECT txOffset, txBytes FROM ebTxs\n\
    \WHERE ebPoint = ? AND txBytes IS NOT NULL AND txOffset IN (" ++ hooks ++ ")\n\
    \ORDER BY txOffset DESC\n\
    \"
  where
    hooks = intercalate ", " (replicate n "?")

-----

-- | PREREQ: No row in ebTxs already has this ebPoint.
msgLeiosBlock :: DB.Database -> Int -> Word64 -> FilePath -> IO ()
msgLeiosBlock db ebPoint ebSlot ebPath = do
    ebBytes <- BS.readFile ebPath
    let ebHash :: Hash.Hash HASH ByteString
        ebHash = Hash.castHash $ Hash.hashWith id ebBytes
    stmt_write_ebPoints <- withDieJust $ DB.prepare db (fromString sql_insert_ebPoint)
    stmt_write_ebBodies <- withDieJust $ DB.prepare db (fromString sql_insert_ebBody)
    withDieMsg $ DB.exec db (fromString "BEGIN")
    -- INSERT INTO ebPoints
    withDie $ DB.bindInt64    stmt_write_ebPoints 1 (fromIntegral ebSlot)
    withDie $ DB.bindBlob     stmt_write_ebPoints 2 (Hash.hashToBytes ebHash)
    withDie $ DB.bindInt64    stmt_write_ebPoints 3 (fromIntegral ebPoint)
    withDieDone $ DB.stepNoCB stmt_write_ebPoints
    withDie $ DB.reset        stmt_write_ebPoints
    -- decode incrementally and simultaneously INSERT INTO ebTxs
    withDie $ DB.bindInt64 stmt_write_ebBodies 1 (fromIntegral ebPoint)
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
    "INSERT INTO ebTxs (ebPoint, txOffset, txHashBytes, txByteSize, txBytes) VALUES (?, ?, ?, ?, NULL)\n\
    \"

msgLeiosBlockTxs :: DB.Database -> Int -> FilePath -> [(Word16, Word64)] -> IO ()
msgLeiosBlockTxs db ebPoint ebTxsPath bitmaps = do
    ebTxsBytes <- BSL.readFile ebTxsPath
    stmt_write_ebTx <- withDieJust $ DB.prepare db (fromString sql_insert_ebTx)
    withDie $ DB.bindInt64 stmt_write_ebTx 2 (fromIntegral ebPoint)
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
                    withDie $ DB.bindInt64    stmt_write_ebTx 3 $ fromIntegral txOffset
                    withDie $ DB.bindBlob     stmt_write_ebTx 1 $ serialize' $ CBOR.encodeBytes txBytes
                    withDieDone $ DB.stepNoCB stmt_write_ebTx
                    withDie $ DB.reset        stmt_write_ebTx
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
    \WHERE ebPoint = ? AND txOffset = ? AND txBytes IS NULL\n\
    \"
