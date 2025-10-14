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
import qualified Codec.CBOR.Encoding as CBOR
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
    ["MsgLeiosBlockRequest", dbPath, ebSlotStr, ebHashStr]
      | ".db" `isSuffixOf` dbPath
      , Just ebSlot <- readMaybe ebSlotStr
      , Right ebHash <- BS16.decode (fromString ebHashStr :: ByteString)
      -> do
        db <- withDieMsg $ DB.open (fromString dbPath)
        msgLeiosBlockRequest db ebSlot ebHash
    "MsgLeiosBlockTxsRequest" : dbPath : ebSlotStr : ebHashStr : bitmapChunkStrs
      | ".db" `isSuffixOf` dbPath
      , Just ebSlot <- readMaybe ebSlotStr
      , Right ebHash <- BS16.decode (fromString ebHashStr :: ByteString)
      , Just bitmaps <- parseBitmaps bitmapChunkStrs
      -> do
        db <- withDieMsg $ DB.open (fromString dbPath)
        msgLeiosBlockTxsRequest db ebSlot ebHash bitmaps
    _ -> die "Either $0 generate myDatabase.db myManifest.json\n\
             \    OR $0 MsgLeiosBlockRequest myDatabase.db ebSlot ebHash(hex)\n\
             \    OR $0 MsgLeiosBlockTxsRequest myDatabase.db ebSlot ebHash(hex) index16:bitmap64 index16:bitmap64 index16:bitmap64 ...\n\
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
    stmt_insert_ebPoints <- withDieJust $ DB.prepare db (fromString sql_insert_ebPoints)
    stmt_insert_ebBodies <- withDieJust $ DB.prepare db (fromString sql_insert_ebBodies)
    stmt_insert_ebClosures <- withDieJust $ DB.prepare db (fromString sql_insert_ebClosures)
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
            txBytes <- R.uniformByteStringM (fromIntegral txByteSize - overhead) gref
            let txCborBytes = serialize' $ CBOR.encodeBytes txBytes
            pure (txCborBytes, Hash.hashWith id txCborBytes :: Hash.Hash HASH ByteString)
        let ebSlot = slotNo ebRecipe
        let ebHash :: Hash.Hash HASH ByteString
            ebHash =
                Hash.castHash
              $ Hash.hashWithSerialiser
                    (encodeEB (fromIntegral . BS.length) Hash.hashToBytes)
                    txs
        withDieMsg $ DB.exec db (fromString "BEGIN")
        withDie $ DB.bindInt64 stmt_insert_ebPoints   3 (fromIntegral ebPoint)
        withDie $ DB.bindInt64 stmt_insert_ebBodies   1 (fromIntegral ebPoint)
        withDie $ DB.bindInt64 stmt_insert_ebClosures 1 (fromIntegral ebPoint)
        -- INSERT INTO ebPoints
        withDie $ DB.bindInt64    stmt_insert_ebPoints 1 (fromIntegral ebSlot)
        withDie $ DB.bindBlob     stmt_insert_ebPoints 2 (Hash.hashToBytes ebHash)
        withDieDone $ DB.stepNoCB stmt_insert_ebPoints
        withDie $ DB.reset        stmt_insert_ebPoints
        -- loop over txs
        V.iforM_ txs $ \txOffset (txCborBytes, txHash) -> do
            withDie $ DB.bindInt64 stmt_insert_ebBodies   2 (fromIntegral txOffset)
            withDie $ DB.bindInt64 stmt_insert_ebClosures 2 (fromIntegral txOffset)
            -- INSERT INTO ebBodies
            withDie $ DB.bindBlob     stmt_insert_ebBodies 3 (Hash.hashToBytes txHash)
            withDie $ DB.bindInt64    stmt_insert_ebBodies 4 (fromIntegral (BS.length txCborBytes))
            withDieDone $ DB.stepNoCB stmt_insert_ebBodies
            withDie $ DB.reset        stmt_insert_ebBodies
            -- INSERT INTO ebClosures
            withDie $ DB.bindBlob     stmt_insert_ebClosures 3 txCborBytes
            withDieDone $ DB.stepNoCB stmt_insert_ebClosures
            withDie $ DB.reset        stmt_insert_ebClosures
        -- finalize each EB
        withDieMsg $ DB.exec db (fromString "COMMIT")
    -- finalize db
    withDieMsg $ DB.exec db (fromString sql_index_schema)

-----

sql_schema :: String
sql_schema =
    "CREATE TABLE txCache (\n\
    \    txHash BLOB NOT NULL PRIMARY KEY   -- raw bytes\n\
    \  ,\n\
    \    txCborBytes BLOB NOT NULL   -- in CBOR\n\
    \  ,\n\
    \    expiryUnixEpoch INTEGER NOT NULL\n\
    \  ) WITHOUT ROWID;\n\
    \\n\
    \CREATE TABLE ebPoints (\n\
    \    ebSlot INTEGER NOT NULL\n\
    \  ,\n\
    \    ebHash BLOB NOT NULL\n\
    \  ,\n\
    \    id INTEGER NOT NULL UNIQUE\n\
    \  ,\n\
    \    PRIMARY KEY (ebSlot, ebHash)\n\
    \  ) WITHOUT ROWID;\n\
    \\n\
    \CREATE TABLE ebBodies (\n\
    \    ebPoint INTEGER NOT NULL   -- foreign key ebPoints.id\n\
    \  ,\n\
    \    txOffset INTEGER NOT NULL\n\
    \  ,\n\
    \    txHash BLOB NOT NULL   -- raw bytes\n\
    \  ,\n\
    \    txSizeInBytes INTEGER NOT NULL\n\
    \  ,\n\
    \    missing BOOLEAN NOT NULL\n\
    \  ,\n\
    \    PRIMARY KEY (ebPoint, txOffset)\n\
    \  ) WITHOUT ROWID;\n\
    \\n\
    \CREATE TABLE ebClosures (\n\
    \    ebPoint INTEGER NOT NULL   -- foreign key ebPoints.id\n\
    \  ,\n\
    \    txOffset INTEGER NOT NULL\n\
    \  ,\n\
    \    txCborBytes BLOB NOT NULL   -- in CBOR\n\
    \  ,\n\
    \    PRIMARY KEY (ebPoint, txOffset)\n\
    \  ) WITHOUT ROWID;\n\
    \"

sql_index_schema :: String
sql_index_schema =
    "-- Helps with the eviction policy of the EbStore.\n\
    \CREATE INDEX ebPointsExpiry\n\
    \    ON ebPoints (ebSlot, id);\n\
    \\n\
    \-- Helps with the eviction policy of the TxCache.\n\
    \CREATE INDEX txCacheExpiry\n\
    \    ON txCache (expiryUnixEpoch, txHash);\n\
    \\n\
    \-- Helps with the eviction policy of the fetch logic's todo list.\n\
    \CREATE INDEX missingEbTxs\n\
    \    ON ebBodies (ebPoint, txOffset)\n\
    \    WHERE missing = TRUE;\n\
    \"

sql_insert_ebPoints :: String
sql_insert_ebPoints =
    "INSERT INTO ebPoints (ebSlot, ebHash, id) VALUES (?, ?, ?)\n\
    \"

sql_insert_ebBodies :: String
sql_insert_ebBodies =
    "INSERT INTO ebBodies (ebPoint, txOffset, txHash, txSizeInBytes, missing) VALUES (?, ?, ?, ?, FALSE)\n\
    \"

sql_insert_ebClosures :: String
sql_insert_ebClosures =
    "INSERT INTO ebClosures (ebPoint, txOffset, txCborBytes) VALUES (?, ?, ?)\n\
    \"

sql_lookup_ebBodies :: String
sql_lookup_ebBodies =
    "SELECT ebBodies.txHash, ebBodies.txSizeInBytes FROM ebBodies\n\
    \INNER JOIN ebPoints ON ebBodies.ebPoint = ebPoints.id\n\
    \WHERE ebPoints.ebSlot = ? AND ebPoints.ebHash = ?\n\
    \ORDER BY ebBodies.txOffset\n\
    \"

sql_lookup_ebClosures :: Int -> String
sql_lookup_ebClosures n =
    "SELECT ebClosures.txOffset, ebClosures.txCborBytes FROM ebClosures\n\
    \INNER JOIN ebPoints ON ebClosures.ebPoint = ebPoints.id\n\
    \WHERE ebPoints.ebSlot = ? AND ebPoints.ebHash = ? AND ebClosures.txOffset IN (" ++ hooks ++ ")\n\
    \ORDER BY ebClosures.txOffset\n\
    \"
  where
    hooks = intercalate ", " (replicate n "?")

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

encodeEbItem :: (b -> Word16) -> (h -> ByteString) -> (b, h) -> CBOR.Encoding
encodeEbItem bytesToLen hashToBytes (txCborBytes, txHash) =
    CBOR.encodeListLen 2
 <> CBOR.encodeBytes (hashToBytes txHash)
 <> CBOR.encodeWord16 (bytesToLen txCborBytes)

encodeEB :: Foldable f => (b -> Word16) -> (h -> ByteString) -> f (b, h) -> CBOR.Encoding
encodeEB bytesToLen hashToBytes ebItems =
    CBOR.encodeListLenIndef
 <> foldr
        (\x r -> encodeEbItem bytesToLen hashToBytes x <> r)
        CBOR.encodeBreak
        ebItems

-----

-- | helper for msgLeiosBlockRequest
--
-- The @[a]@ is less than 1024 long.
--
-- Each 'V.Vector' is exactly 1024.
data X a = X [V.Vector a] !Word16 [a]

emptyX :: X a
emptyX = X [] 0 []

pushX :: a -> X a -> X a
pushX x (X vs n xs) =
    if n < 1024 then X vs (n+1) (x : xs) else
    X (V.fromList (reverse xs) : vs) 1 [x]

-- | helper for msgLeiosBlockRequest
newtype Y a = Y [V.Vector a]
  deriving (Functor, Foldable)

finalizeX :: X a -> Y a
finalizeX (X vs _n xs) = Y $ reverse $ V.fromList (reverse xs) : vs

msgLeiosBlockRequest :: DB.Database -> Word64 -> ByteString -> IO ()
msgLeiosBlockRequest db ebSlot ebHash = do
    -- get the EB items
    stmt_lookup_ebBodies <- withDieJust $ DB.prepare db (fromString sql_lookup_ebBodies)
    withDie $ DB.bindInt64 stmt_lookup_ebBodies 1 (fromIntegral ebSlot)
    withDie $ DB.bindBlob  stmt_lookup_ebBodies 2 ebHash
    let loop !acc = do
            withDie (DB.stepNoCB stmt_lookup_ebBodies) >>= \case
                DB.Done -> pure $ finalizeX acc
                DB.Row -> do
                    txHash <- DB.columnBlob stmt_lookup_ebBodies 0
                    txSizeInBytes <- DB.columnInt64 stmt_lookup_ebBodies 1
                    loop $ pushX (txSizeInBytes, txHash) acc
    y <- loop emptyX
    -- combine the EB items
    BS.putStr
      $ BS16.encode
      $ serialize'
      $ encodeEB fromIntegral id y
    putStrLn ""

msgLeiosBlockTxsRequest :: DB.Database -> Word64 -> ByteString -> [(Word16, Word64)] -> IO ()
msgLeiosBlockTxsRequest db ebSlot ebHash bitmaps = do
    when (not $ let idxs = map fst bitmaps in and $ zipWith (<) idxs (tail idxs)) $ do
        die "Offsets not strictly ascending"
    when (1000 < sum (map (Bits.popCount . snd) bitmaps)) $ do
        -- TODO insert into temp table and join?
        die "Too many offsets in one request"
    let nextOffset = \case
            [] -> Nothing
            (idx, bitmap) : k -> case popOffset bitmap of
                Nothing           -> nextOffset k
                Just (i, bitmap') ->
                    Just (64 * fromIntegral idx + i, (idx, bitmap') : k)
        offsets = unfoldr nextOffset bitmaps
    -- get the txs
    stmt_lookup_ebClosures <- withDieJust $ DB.prepare db $ fromString $ sql_lookup_ebClosures (length offsets)
    withDie $ DB.bindInt64 stmt_lookup_ebClosures 1 (fromIntegral ebSlot)
    withDie $ DB.bindBlob  stmt_lookup_ebClosures 2 ebHash
    forM_ ([(3 :: DB.ParamIndex) ..] `zip` offsets) $ \(i, offset) -> do
        withDie $ DB.bindInt64 stmt_lookup_ebClosures i (fromIntegral offset)
    acc <- (\f -> foldM f [] offsets) $ \acc offset -> do
        withDie (DB.stepNoCB stmt_lookup_ebClosures) >>= \case
            DB.Done -> die $ "No rows starting at offset: " ++ show offset
            DB.Row -> do
                txOffset <- DB.columnInt64 stmt_lookup_ebClosures 0
                txCborBytes <- DB.columnBlob stmt_lookup_ebClosures 1
                when (txOffset /= fromIntegral offset) $ die $ "Missing offset: " <> show offset
                pure (txCborBytes : acc)
    -- combine the txs
    BS.putStr
      $ BS16.encode
      $ serialize'
      $ CBOR.encodeListLenIndef <> foldr (\bs r -> CBOR.encodePreEncoded bs <> r) CBOR.encodeBreak (reverse acc)
    putStrLn ""

{- | For example
@
  print $ unfoldr popOffset 0
  print $ unfoldr popOffset 1
  print $ unfoldr popOffset (2^(34 :: Int))
  print $ unfoldr popOffset (2^(63 :: Int) + 2^(62 :: Int) + 8)
  []
  [63]
  [29]
  [0,1,60]
@
-}
popOffset :: Word64 -> Maybe (Int, Word64)
{-# INLINE popOffset #-}
popOffset = \case
    0 -> Nothing
    w -> let zs = Bits.countLeadingZeros w
         in
         Just (zs, Bits.clearBit w (63 - zs))
