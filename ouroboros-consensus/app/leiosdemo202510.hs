{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main (module Main) where

import           Cardano.Binary (serialize')
import qualified Cardano.Crypto.Hash as Hash
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import           Control.Applicative ((<|>))
import           Control.Monad (foldM, when)
import qualified Data.Aeson as JSON
import qualified Data.Bits as Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import           Data.DList (DList)
import qualified Data.DList as DList
import           Data.Foldable (forM_)
import qualified Data.Foldable as Foldable
import           Data.Functor.Contravariant ((>$<))
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)
import           Data.List (isSuffixOf, unfoldr)
import           Data.Map (Map)
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (decodeUtf8', encodeUtf8)
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
main = flip asTypeOf main2 $ do
  main2

main2 :: IO ()
main2 = getArgs >>= \case
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
    "ebId-to-point" : dbPath : ebIdStrs
      | ".db" `isSuffixOf` dbPath
      , not (null ebIdStrs)
      , Just ebIds <- map MkEbId <$> traverse readMaybe ebIdStrs
      -> do
        dynEnv <- reopenDb dbPath >>= loadLeiosFetchDynEnvHelper False
        forM_ ebIds $ \ebId -> do
            case ebIdToPoint ebId dynEnv of
                Nothing -> die $ "Unknown EbId: " <> prettyEbId ebId
                Just (ebSlot, ebHash) -> do
                    putStrLn $ unwords [show ebSlot, prettyHashBytes (MkHashBytes ebHash)]
    ["MsgLeiosBlockOffer", dbPath, lfstPath, peerIdStr, ebSlotStr, ebHashStr, ebBytesSizeStr]
      | ".db" `isSuffixOf` dbPath
      , ".lfst" `isSuffixOf` lfstPath
      , not (null peerIdStr)
      , Just ebSlot <- readMaybe ebSlotStr
      , Right ebHash <- BS16.decode $ BS8.pack ebHashStr
      , Just ebBytesSize <- readMaybe ebBytesSizeStr
      -> do
        (db, acc) <- openEvenIfMissing dbPath lfstPath
        ebId <- ebIdFromPoint' db ebSlot ebHash
        acc' <- msgLeiosBlockOffer acc (MkPeerId peerIdStr) ebId ebBytesSize
        JSON.encodeFile lfstPath acc'
    ["MsgLeiosBlockRequest", dbPath, ebSlotStr, ebHashStr]
      | ".db" `isSuffixOf` dbPath
      , Just ebSlot <- readMaybe ebSlotStr
      , Right ebHash <- BS16.decode $ BS8.pack ebHashStr
      -> do
        db <- reopenDb dbPath
        ebId <- ebIdFromPoint' db ebSlot ebHash
        msgLeiosBlockRequest db ebId
    ["MsgLeiosBlock", dbPath, lfstPath, peerIdStr, ebPath]
      | ".db" `isSuffixOf` dbPath
      , ".lfst" `isSuffixOf` lfstPath
      , not (null peerIdStr)
      , ".bin" `isSuffixOf` ebPath
      -> do
        db <- reopenDb dbPath
        acc <- withDiePoly id $ JSON.eitherDecodeFileStrict lfstPath
        acc' <- msgLeiosBlock db acc (MkPeerId peerIdStr) ebPath
        JSON.encodeFile lfstPath acc'
    ["MsgLeiosBlockTxsOffer", dbPath, lfstPath, peerIdStr, ebSlotStr, ebHashStr]
      | ".db" `isSuffixOf` dbPath
      , ".lfst" `isSuffixOf` lfstPath
      , not (null peerIdStr)
      , Just ebSlot <- readMaybe ebSlotStr
      , Right ebHash <- BS16.decode $ BS8.pack ebHashStr
      -> do
        (db, acc) <- openEvenIfMissing dbPath lfstPath
        ebId <- ebIdFromPoint' db ebSlot ebHash
        acc' <- msgLeiosBlockTxsOffer acc (MkPeerId peerIdStr) ebId
        JSON.encodeFile lfstPath acc'
    "MsgLeiosBlockTxsRequest" : dbPath : ebSlotStr : ebHashStr : bitmapChunkStrs
      | ".db" `isSuffixOf` dbPath
      , Just ebSlot <- readMaybe ebSlotStr
      , Right ebHash <- BS16.decode $ BS8.pack ebHashStr
      , Just bitmaps <- parseBitmaps bitmapChunkStrs
      -> do
        db <- reopenDb dbPath
        ebId <- ebIdFromPoint' db ebSlot ebHash
        msgLeiosBlockTxsRequest db ebId bitmaps
    ["MsgLeiosBlockTxs", dbPath, lfstPath, peerIdStr, ebTxsPath]
      | ".db" `isSuffixOf` dbPath
      , ".bin" `isSuffixOf` ebTxsPath
      , not (null peerIdStr)
      -> do
        db <- reopenDb dbPath
        acc <- withDiePoly id $ JSON.eitherDecodeFileStrict lfstPath
        acc' <- msgLeiosBlockTxs db acc (MkPeerId peerIdStr) ebTxsPath
        JSON.encodeFile lfstPath acc'
    ["fetch-logic-iteration", dbPath, lfstPath]
      | ".db" `isSuffixOf` dbPath
      , ".lfst" `isSuffixOf` lfstPath
      -> do
        db <- reopenDb dbPath
        acc <- withDiePoly id $ JSON.eitherDecodeFileStrict lfstPath
        acc' <- fetchDecision2 db acc
        JSON.encodeFile lfstPath acc'
    ["hash-txs", ebTxsPath]
      | ".bin" `isSuffixOf` ebTxsPath
      -> do
        hashTxs ebTxsPath
    ["cache-copy", dbPath, lfstPath, bytesSizeStr]
      | ".db" `isSuffixOf` dbPath
      , ".lfst" `isSuffixOf` lfstPath
      , Just bytesSize <- readMaybe bytesSizeStr
      , 0 < bytesSize
      -> do
        db <- reopenDb dbPath
        acc <- withDiePoly id $ JSON.eitherDecodeFileStrict lfstPath
        acc' <- doCacheCopy db acc bytesSize
        JSON.encodeFile lfstPath acc'
    _ ->
        die "Either $0 generate my.db myManifest.json\n\
            \    OR $0 ebId-to-point my.db ebId ebId ebId...\n\
            \    OR $0 MsgLeiosBlockOffer my.db my.lfst peerId ebSlot ebHash(hex) ebBytesSize\n\
            \    OR $0 MsgLeiosBlockRequest my.db ebSlot ebHash(hex)\n\
            \    OR $0 MsgLeiosBlock my.db my.lfst myEb.bin\n\
            \    OR $0 MsgLeiosBlockTxsOffer my.db my.lfst peerId ebSlot ebHash(hex)\n\
            \    OR $0 MsgLeiosBlockTxsRequest my.db ebSlot ebHash(hex) index16:bitmap64 index16:bitmap64 index16:bitmap64 ...\n\
            \    OR $0 MsgLeiosBlockTxs my.db my.lfst peerId myEbTxs.bin\n\
            \    OR $0 fetch-logic-iteration my.db my.lfst\n\
            \    OR $0 hash-txs myEbTxs.bin\n\
            \    OR $0 cache-copy my.db my.lfst bytesSize(positive)\n\
            \"

{- TODO

- all kinds of evictions?

- disconnects?

-}

reopenDb :: FilePath -> IO DB.Database
reopenDb dbPath = do
    doesFileExist dbPath >>= \case
        True -> withDieMsg $ DB.open (fromString dbPath)
        False -> die $ "No such file: " ++ dbPath

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

prettyBitmap :: (Word16, Word64) -> String
prettyBitmap (idx, bitmap) =
    show idx ++ ":0x" ++ Numeric.showHex bitmap ""

data EbRecipe = EbRecipe {
    ebRecipeNickname :: Maybe String
  ,
    ebRecipeElems :: V.Vector EbRecipeElem
  ,
    ebRecipeSlotNo :: Word64
  }
  deriving (Show)

instance JSON.FromJSON EbRecipe where
    parseJSON = JSON.withObject "EbRecipe" $ \v -> EbRecipe
        <$> v JSON..:? (fromString "nickname")
        <*> v JSON..: (fromString "txRecipes")
        <*> v JSON..: (fromString "slotNo")

data EbRecipeElem =
    EbRecipeTxBytesSize Word16
  |
    -- | Binder occurrence, inclusive start, and exclusive stop
    EbRecipeShare String Int (Maybe Int)
  deriving (Show)

instance JSON.FromJSON EbRecipeElem where
    parseJSON =
        \v -> size v <|> share v
      where
        size v = EbRecipeTxBytesSize <$> JSON.parseJSON @Word16 v
        share = JSON.withObject "EbRecipeElem" $ \v -> EbRecipeShare
          <$> v JSON..: (fromString "share")
          <*> v JSON..: (fromString "startIncl")
          <*> v JSON..:? (fromString "stopExcl")

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
    (_dynEnv', sigma) <- (\f -> foldM f (emptyLeiosFetchDynEnv, Map.empty) ebRecipes) $ \(dynEnv, sigma) ebRecipe -> do
        -- generate txs, so we have their hashes
        let finishX (n, x) = V.fromListN n $ Foldable.toList $ revX x   -- TODO in ST with mut vector
        txs <- fmap finishX $ (\f -> V.foldM f (0, emptyX) (ebRecipeElems ebRecipe)) $ \(accN, accX) -> \case
            EbRecipeShare occ startIncl mbStopExcl -> do
                (srcEbId, ebTxsCount) <- case Map.lookup occ sigma of
                    Nothing -> die $ "Could not find EB binder: " ++ occ
                    Just x -> pure x
                let stopExcl = fromMaybe ebTxsCount mbStopExcl
                    len = stopExcl - startIncl
                when (len < 0) $ die $ "Non-positive share length: " ++ show (occ, startIncl, mbStopExcl, stopExcl, len)
                -- SELECT the referenced txs
                stmt <- withDieJust $ DB.prepare db (fromString sql_share_ebClosures_ASC)
                withDie $ DB.bindInt64 stmt 1 (fromIntegralEbId srcEbId)
                withDie $ DB.bindInt64 stmt 2 (fromIntegral startIncl)
                withDie $ DB.bindInt64 stmt 3 (fromIntegral len)
                let loop i !accX' =
                        withDie (DB.stepNoCB stmt) >>= \case
                            DB.Done -> do
                                when (i /= fromIntegral stopExcl) $ do
                                    die $ "Ran out of txs for share" ++ show (occ, startIncl, mbStopExcl, i)
                                pure accX'
                            DB.Row -> do
                                txOffset <- DB.columnInt64 stmt 0
                                txHashBytes <- DB.columnBlob stmt 1
                                txBytes <- DB.columnBlob stmt 2
                                when (txOffset /= i) $ do
                                    die $ "Unexpected share txOffset" ++ show (occ, startIncl, mbStopExcl, txOffset, i)
                                loop (i + 1) $ pushX accX' (txBytes, MkHashBytes txHashBytes)
                accX' <- loop (fromIntegral startIncl) accX
                pure (accN + len, accX')
            EbRecipeTxBytesSize txBytesSize -> do
                -- generate a random bytestring whose CBOR encoding has the expected length
                --
                -- In the actual implementation, the values themselves will be
                -- valid CBOR. It's useful to maintain that invariant even for
                -- the otherwise-opaque random data within this prototype/demo.
                when (txBytesSize < 55) $ die "Tx cannot be smaller than 55 bytes"
                when (txBytesSize > 2^(14::Int)) $ die "Tx cannot be be larger than 2^14 bytes"
                let overhead   -- one for the initial byte, plus 1 2 4 or 8 for the length argument
                      | txBytesSize < fromIntegral (maxBound :: Word8) = 2
                      | txBytesSize <              (maxBound :: Word16) = 3
                      | txBytesSize < fromIntegral (maxBound :: Word32) = 5
                      | otherwise = 9
                txBytes <- id
                  $ fmap (serialize' . CBOR.encodeBytes)
                  $ R.uniformByteStringM (fromIntegral txBytesSize - overhead) gref
                let txHash = Hash.hashWith id txBytes :: Hash.Hash HASH ByteString
                pure (accN + 1, accX `pushX` (txBytes, MkHashBytes $ Hash.hashToBytes txHash))
        let ebSlot = ebRecipeSlotNo ebRecipe
        let ebHash :: Hash.Hash HASH ByteString
            ebHash =
                Hash.castHash
              $ Hash.hashWithSerialiser
                    (encodeEB (V.length txs) (fromIntegral . BS.length) (\(MkHashBytes x) -> x))
                    txs
        let (ebId, mbDynEnv') = ebIdFromPoint ebSlot (Hash.hashToBytes ebHash) dynEnv
        withDieMsg $ DB.exec db (fromString "BEGIN")
        withDie $ DB.bindInt64 stmt_write_ebId      3 (fromIntegralEbId ebId)
        withDie $ DB.bindInt64 stmt_write_ebClosure 1 (fromIntegralEbId ebId)
        -- INSERT INTO ebPoints
        withDie $ DB.bindInt64    stmt_write_ebId 1 (fromIntegral ebSlot)
        withDie $ DB.bindBlob     stmt_write_ebId 2 (Hash.hashToBytes ebHash)
        withDieDone $ DB.stepNoCB stmt_write_ebId
        withDie $ DB.reset        stmt_write_ebId
        -- loop over txs
        V.iforM_ txs $ \txOffset (txBytes, txHash) -> do
            -- INSERT INTO ebTxs
            withDie $ DB.bindInt64    stmt_write_ebClosure 2 (fromIntegral txOffset)
            withDie $ DB.bindBlob     stmt_write_ebClosure 3 (let MkHashBytes x = txHash in x)
            withDie $ DB.bindInt64    stmt_write_ebClosure 4 (fromIntegral (BS.length txBytes))
            withDie $ DB.bindBlob     stmt_write_ebClosure 5 txBytes
            withDieDone $ DB.stepNoCB stmt_write_ebClosure
            withDie $ DB.reset        stmt_write_ebClosure
        -- finalize each EB
        withDieMsg $ DB.exec db (fromString "COMMIT")
        pure (fromMaybe dynEnv mbDynEnv', maybe id (\bndr -> Map.insert bndr (ebId, V.length txs)) (ebRecipeNickname ebRecipe) sigma)
    -- finalize db
    withDieMsg $ DB.exec db (fromString sql_index_schema)
    forM_ (Map.toList sigma) $ \(nickname, (ebId, _count)) -> do
        putStrLn $ unwords [nickname, prettyEbId ebId]

-----

sql_schema :: String
sql_schema =
    "CREATE TABLE txCache (\n\
    \    txHashBytes BLOB NOT NULL PRIMARY KEY   -- raw bytes\n\
    \  ,\n\
    \    txBytes BLOB NOT NULL   -- valid CBOR\n\
    \  ,\n\
    \    txBytesSize INTEGER NOT NULL\n\
    \  ,\n\
    \    expiryUnixEpoch INTEGER NOT NULL\n\
    \  ) WITHOUT ROWID;\n\
    \\n\
    \CREATE TABLE ebPoints (\n\
    \    ebSlot INTEGER NOT NULL\n\
    \  ,\n\
    \    ebHashBytes BLOB NOT NULL\n\
    \  ,\n\
    \    ebId INTEGER NOT NULL\n\
    \  ,\n\
    \    PRIMARY KEY (ebSlot, ebHashBytes)\n\
    \  ) WITHOUT ROWID;\n\
    \\n\
    \CREATE TABLE ebTxs (\n\
    \    ebId INTEGER NOT NULL   -- foreign key ebPoints.ebId\n\
    \  ,\n\
    \    txOffset INTEGER NOT NULL\n\
    \  ,\n\
    \    txHashBytes BLOB NOT NULL   -- raw bytes\n\
    \  ,\n\
    \    txBytesSize INTEGER NOT NULL\n\
    \  ,\n\
    \    txBytes BLOB   -- valid CBOR\n\
    \  ,\n\
    \    PRIMARY KEY (ebId, txOffset)\n\
    \  ) WITHOUT ROWID;\n\
    \"

sql_index_schema :: String
sql_index_schema =
    "CREATE INDEX ebPointsExpiry\n\
    \    ON ebPoints (ebSlot ASC, ebId ASC);   -- Helps with the eviction policy of the EbStore.\n\
    \\n\
    \CREATE INDEX txCacheExpiry\n\
    \    ON txCache (expiryUnixEpoch ASC, txHashBytes);   -- Helps with the eviction policy of the TxCache.\n\
    \\n\
    \CREATE INDEX missingEbTxs\n\
    \    ON ebTxs (ebId DESC, txOffset ASC)\n\
    \    WHERE txBytes IS NULL;   -- Helps with fetch logic decisions.\n\
    \\n\
    \CREATE INDEX acquiredEbTxs\n\
    \    ON ebTxs (ebId DESC, txOffset ASC)\n\
    \    WHERE txBytes IS NOT NULL;   -- Helps with fetch logic decisions.\n\
    \"

sql_insert_ebId :: String
sql_insert_ebId =
    "INSERT INTO ebPoints (ebSlot, ebHashBytes, ebId) VALUES (?, ?, ?)\n\
    \"

sql_insert_ebClosure :: String
sql_insert_ebClosure =
    "INSERT INTO ebTxs (ebId, txOffset, txHashBytes, txBytesSize, txBytes) VALUES (?, ?, ?, ?, ?)\n\
    \"

sql_share_ebClosures_ASC :: String
sql_share_ebClosures_ASC =
    "SELECT txOffset, txHashBytes, txBytes FROM ebTxs\n\
    \WHERE ebId = ? AND txOffset >= ?\n\
    \ORDER BY txOffset ASC\n\
    \LIMIT ?\n\
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

encodeEB :: Foldable f => Int -> (b -> Word16) -> (h -> ByteString) -> f (b, h) -> CBOR.Encoding
encodeEB n bytesToLen hashToBytes ebPairs =
    CBOR.encodeMapLen (fromIntegral n)
 <> foldMap (encodeEbPair bytesToLen hashToBytes) ebPairs

decodeEbPair :: CBOR.Decoder s (ByteString, Word16)
decodeEbPair =
    (,) <$> CBOR.decodeBytes <*> CBOR.decodeWord16

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

revX :: X a -> X a
revX (X n xs vs) = X n (reverse xs) (reverse (map V.reverse vs))

msgLeiosBlockRequest :: DB.Database -> EbId -> IO ()
msgLeiosBlockRequest db ebId = do
    -- get the EB items
    stmt <- withDieJust $ DB.prepare db (fromString sql_lookup_ebBodies_DESC)
    withDie $ DB.bindInt64 stmt 1 (fromIntegralEbId ebId)
    let loop !accN !acc =
            withDie (DB.stepNoCB stmt) >>= \case
                DB.Done -> pure (accN, acc)
                DB.Row -> do
                    -- TODO use a sink buffer to avoid polluting the heap with these temporary copies?
                    txOffset <- DB.columnInt64 stmt 0
                    txHashBytes <- DB.columnBlob stmt 1
                    txBytesSize <- DB.columnInt64 stmt 2
                    loop
                        (if 0 == accN then fromIntegral (txOffset + 1) else accN)
                        (pushX acc (txBytesSize, txHashBytes))
    (n, acc) <- loop 0 emptyX
    -- combine the EB items
    BS.putStr
      $ BS16.encode
      $ serialize'
      $ encodeEB n fromIntegral id acc
    putStrLn ""

-- | It's DESCending because the accumulator within the 'msgLeiosBlockRequest'
-- logic naturally reverses it
sql_lookup_ebBodies_DESC :: String
sql_lookup_ebBodies_DESC =
    "SELECT txOffset, txHashBytes, txBytesSize FROM ebTxs\n\
    \WHERE ebId = ?\n\
    \ORDER BY txOffset DESC\n\
    \"

msgLeiosBlockTxsRequest :: DB.Database -> EbId -> [(Word16, Word64)] -> IO ()
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
    let nextOffsetDESC = \case
            [] -> Nothing
            (idx, bitmap) : k -> case popRightmostOffset bitmap of
                Nothing           -> nextOffsetDESC k
                Just (i, bitmap') ->
                    Just (64 * fromIntegral idx + i, (idx, bitmap') : k)
        txOffsets = unfoldr nextOffsetDESC (reverse bitmaps)
    -- fill in-memory table
    withDieMsg $ DB.exec db (fromString sql_attach_memTxPoints)
    withDieMsg $ DB.exec db (fromString "BEGIN")
    do
        stmt <- withDieJust $ DB.prepare db (fromString sql_insert_memTxPoints)
        withDie $ DB.bindInt64 stmt 1 (fromIntegralEbId ebId)
        forM_ txOffsets $ \txOffset -> do
            withDie $ DB.bindInt64 stmt 2 (fromIntegral txOffset)
            withDieDone $ DB.stepNoCB stmt
            withDie $ DB.reset stmt
        withDie $ DB.finalize stmt
    -- get txBytess
    stmt <- withDieJust $ DB.prepare db (fromString sql_retrieve_from_ebTxs)
    acc <- (\f -> foldM f emptyX txOffsets) $ \acc txOffset -> do
        withDie (DB.stepNoCB stmt) >>= \case
            DB.Done -> pure acc
            DB.Row -> do
                txOffset' <- DB.columnInt64 stmt 0
                txBytes <- DB.columnBlob stmt 1
                when (fromIntegral txOffset /= txOffset') $ do
                    die $ "Missing offset " ++ show (txOffset, txOffset')
                pure $ pushX acc txBytes
    withDie $ DB.finalize stmt
    withDieMsg $ DB.exec db (fromString "COMMIT")
    withDieMsg $ DB.exec db (fromString sql_detach_memTxPoints)
    -- combine the txs
    BS.putStr
      $ BS16.encode
      $ serialize'
      $ CBOR.encodeListLenIndef <> foldr (\bs r -> CBOR.encodePreEncoded bs <> r) CBOR.encodeBreak acc
    putStrLn ""

maxEbBodyBytesSize :: BytesSize
maxEbBodyBytesSize = 500000

minEbItemBytesSize :: BytesSize
minEbItemBytesSize = (1 + 32 + 1) + (1 + 1)

maxEbItems :: Int
maxEbItems = (negate 1 + maxEbBodyBytesSize - 1) `div` minEbItemBytesSize

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

-- | It's DESCending because the accumulator within the
-- 'msgLeiosBlockTxsRequest' logic naturally reverses it
sql_retrieve_from_ebTxs :: String
sql_retrieve_from_ebTxs =
    "SELECT x.txOffset, x.txBytes\n\
    \FROM ebTxs as x\n\
    \INNER JOIN mem.txPoints ON x.ebId = mem.txPoints.ebId AND x.txOffset = mem.txPoints.txOffset\n\
    \WHERE x.txBytes IS NOT NULL\n\
    \ORDER BY x.txOffset DESC\n\
    \"

-----

-- | PREREQ: the file is the CBOR encoding (binary, not hex) of the payload of a MsgLeiosBlock
--
-- PREREQ: No row in ebTxs already has this ebId.
msgLeiosBlock :: DB.Database -> LeiosFetchState -> PeerId -> FilePath -> IO LeiosFetchState
msgLeiosBlock db lfst0 peerId ebPath = do
    (MkLeiosBlockRequest ebSlot ebHash, lfst1) <-
        case Map.lookup peerId (requestedPerPeer lfst0) of
            Just (LeiosBlockRequest req:reqs) -> pure $ (,) req $ lfst0 {
                requestedPerPeer =
                    if null reqs then Map.delete peerId (requestedPerPeer lfst0) else
                    Map.insert peerId reqs (requestedPerPeer lfst0)
              }
            _ -> die "Not expecting MsgLeiosBlock"
    ebBytes <- BS.readFile ebPath
    let ebBytesSize = BS.length ebBytes
    let ebHash' :: Hash.Hash HASH ByteString
        ebHash' = Hash.castHash $ Hash.hashWith id ebBytes
        ebHash'' = MkHashBytes $ Hash.hashToBytes ebHash'
    when (ebHash /= ebHash'') $ do
        die $ "MsgLeiosBlock hash mismatch: " <> show (ebHash, ebHash'')
    ebId <- ebIdFromPoint' db ebSlot (let MkHashBytes x = ebHash in x)
    stmt <- withDieJust $ DB.prepare db (fromString sql_insert_ebBody)
    withDieMsg $ DB.exec db (fromString "BEGIN")
    -- decode incrementally and simultaneously INSERT INTO ebTxs
    withDie $ DB.bindInt64 stmt 1 (fromIntegralEbId ebId)
    (ebBytes2, n) <- withDiePoly id $ pure $ CBOR.deserialiseFromBytes CBOR.decodeMapLen $ BSL.fromStrict ebBytes
    let go1 txOffset bytes = if fromIntegral n == txOffset then pure () else do
            (bytes', next) <- withDiePoly id $ pure $ CBOR.deserialiseFromBytes decodeEbPair bytes
            go2 txOffset bytes' next
        go2 txOffset bytes (txHashBytes, txBytesSize) = do
            withDie $ DB.bindInt64    stmt 2 txOffset
            withDie $ DB.bindBlob     stmt 3 txHashBytes
            withDie $ DB.bindInt64    stmt 4 (fromIntegral txBytesSize)
            withDieDone $ DB.stepNoCB stmt
            withDie $ DB.reset        stmt
            go1 (txOffset + 1) bytes
    go1 0 ebBytes2
    -- finalize the EB
    withDieMsg $ DB.exec db (fromString "COMMIT")
    pure lfst1 {
        acquiredEbBodies = Set.insert ebId (acquiredEbBodies lfst1)
      ,
        missingEbBodies = Map.delete ebId (missingEbBodies lfst1)
      ,
        requestedBytesSize = requestedBytesSize lfst1 - ebBytesSize
      ,
        requestedBytesSizePerPeer =
            Map.alter
                (\case
                    Nothing -> error "impossible!"
                    Just x -> delIfZero $ x - ebBytesSize
                )
                peerId
                (requestedBytesSizePerPeer lfst1)
      ,
        requestedEbPeers =
            Map.update (delIfNull . Set.delete peerId) ebId (requestedEbPeers lfst1)
      }

sql_insert_ebBody :: String
sql_insert_ebBody =
    "INSERT INTO ebTxs (ebId, txOffset, txHashBytes, txBytesSize, txBytes) VALUES (?, ?, ?, ?, NULL)\n\
    \"

-- | PREREQ: the file is the CBOR encoding (binary, not hex) of the payload of a MsgLeiosBlockTxs
msgLeiosBlockTxs :: DB.Database -> LeiosFetchState -> PeerId -> FilePath -> IO LeiosFetchState
msgLeiosBlockTxs db lfst0 peerId ebTxsPath = do
    (MkLeiosBlockTxsRequest ebSlot ebHash bitmaps0 txHashes, lfst1) <-
        case Map.lookup peerId (requestedPerPeer lfst0) of
            Just (LeiosBlockTxsRequest req:reqs) -> pure $ (,) req $ lfst0 {
                requestedPerPeer =
                    if null reqs then Map.delete peerId (requestedPerPeer lfst0) else
                    Map.insert peerId reqs (requestedPerPeer lfst0)
              }
            _ -> die "Not expecting MsgLeiosBlockTxs"
    ebId <- ebIdFromPoint' db ebSlot (let MkHashBytes x = ebHash in x)
    ebTxsBytes <- BSL.readFile ebTxsPath
    stmtTxCache <- withDieJust $ DB.prepare db (fromString sql_insert_txCache)
    stmtEbTxs <- withDieJust $ DB.prepare db (fromString sql_update_ebTx)
    withDie $ DB.bindInt64 stmtEbTxs 2 (fromIntegralEbId ebId)
    withDieMsg $ DB.exec db (fromString "BEGIN")
    -- decode incrementally and simultaneously UPDATE ebTxs and INSERT INTO txCache
    let decodeBreakOrTx = do
            stop <- CBOR.decodeBreakOr
            if stop then pure Nothing else Just <$> CBOR.decodeBytes
    let go1 accRequested accTxBytesSize offsets bytes !i = do
            (bytes', next) <- withDiePoly id $ pure $ CBOR.deserialiseFromBytes decodeBreakOrTx bytes
            let txBytesSize = BSL.length bytes - BSL.length bytes'
                txHash' :: Hash.Hash HASH ByteString
                txHash' = Hash.hashWith id $ BSL.toStrict $ BSL.take txBytesSize bytes
            go2
                accRequested
                accTxBytesSize
                offsets
                bytes'
                i
                (fromIntegral txBytesSize)
                txHash'
                next
        go2 accRequested accTxBytesSize offsets bytes !i txBytesSize txHash' = \case
            Just txBytes -> case (offsets, txHashes V.!? i)  of
                ([], _) -> die "More txs than offsets"
                (_, Nothing) -> die "More offsets than hashes"
                
                (txOffset:offsets', Just txHash)
                  | txHash /= MkHashBytes (Hash.hashToBytes txHash') -> die "Wrong tx hash"
                  | otherwise -> do
                    -- INTO ebTxs
                    withDie $ DB.bindInt64    stmtEbTxs 3 $ fromIntegral txOffset
                    withDie $ DB.bindBlob     stmtEbTxs 1 $ serialize' $ CBOR.encodeBytes txBytes
                    withDieDone $ DB.stepNoCB stmtEbTxs
                    withDie $ DB.reset        stmtEbTxs
                    -- INTO txCache
                    withDie $ DB.bindBlob     stmtTxCache 1 $ Hash.hashToBytes txHash'
                    withDie $ DB.bindBlob     stmtTxCache 2 $ serialize' $ CBOR.encodeBytes txBytes
                    withDie $ DB.bindInt64    stmtTxCache 3 $ fromIntegral txBytesSize
                    withDieDone $ DB.stepNoCB stmtTxCache
                    withDie $ DB.reset        stmtTxCache
                    go1
                        (Map.update (delIfNull . Set.delete peerId) txHash accRequested)
                        (accTxBytesSize + txBytesSize)
                        offsets'
                        bytes
                        (i + 1)
            Nothing
              | not (BSL.null bytes) -> die "Incomplete EB txs decode"
              | txOffset:_ <- offsets -> die $ "Too few EB txs; next is " <> show txOffset
              | otherwise -> pure (accRequested, accTxBytesSize)
    let nextOffset = \case
            [] -> Nothing
            (idx, bitmap) : k -> case popLeftmostOffset bitmap of
                Nothing           -> nextOffset k
                Just (i, bitmap') ->
                    Just (64 * fromIntegral idx + i, (idx, bitmap') : k)
        offsets0 = unfoldr nextOffset bitmaps0
    (ebTxsBytes2, ()) <- withDiePoly id $ pure $ CBOR.deserialiseFromBytes CBOR.decodeListLenIndef ebTxsBytes
    (requested', txBytesSize) <- go1 (requestedTxPeers lfst1) 0 offsets0 ebTxsBytes2 0
    -- finalize the EB
    withDieMsg $ DB.exec db (fromString "COMMIT")
    pure lfst1 {
        requestedBytesSize = requestedBytesSize lfst1 - txBytesSize
      ,
        requestedBytesSizePerPeer =
            Map.alter
                (\case
                    Nothing -> error "impossible!"
                    Just x -> delIfZero $ x - txBytesSize
                )
                peerId
                (requestedBytesSizePerPeer lfst1)
      ,
        requestedTxPeers = requested'
      }

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

{-
_maxTxOffsetBitWidth :: Int
_maxTxOffsetBitWidth = ceiling $ log (fromIntegral maxEbItems :: Double) / log 2

maxRequestsPerIteration :: Int
maxRequestsPerIteration = 10

maxBytesSizePerRequest :: BytesSize
maxBytesSizePerRequest = 500000

fetchDecision :: DB.Database -> IntSet.IntSet -> IO ()
fetchDecision db ebIds = do
    stmt <- withDieJust $ DB.prepare db $ fromString $ sql_next_fetch (IntSet.size ebIds)
    forM_ ([(1 :: DB.ParamIndex) ..] `zip` IntSet.toDescList ebIds) $ \(i, p) -> do
        withDie $ DB.bindInt64 stmt i (fromIntegral p)
    let loopLimit = maxRequestsPerIteration * maxBytesSizePerRequest
        loop !accReqs !accBytesSize =
            if accBytesSize >= loopLimit then pure accReqs else
            withDie (DB.stepNoCB stmt) >>= \case
                DB.Done -> pure accReqs
                DB.Row -> do
                    ebId <- fromIntegral <$> DB.columnInt64 stmt 0
                    txOffset <- fromIntegral <$> DB.columnInt64 stmt 1
                    txHash <- DB.columnBlob stmt 2
                    txBytesSize <- fromIntegral <$> DB.columnInt64 stmt 3
                    loop
                        (IntMap.insertWith
                            IntMap.union
                            ebId
                            (IntMap.singleton txOffset txHash)
                            accReqs
                        )
                        (accBytesSize + txBytesSize)
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

-- | The SQL query optimizer should use the @missingEbTxs@ INDEX.
sql_next_fetch :: Int -> String
sql_next_fetch n =
    "SELECT ebId, txOffset, txHashBytes, txBytesSize FROM ebTxs\n\
    \WHERE txBytes IS NULL AND ebId IN (" ++ hooks ++ ")\n\
    \ORDER BY ebId DESC, txOffset ASC\n\
    \"
  where
    hooks = intercalate ", " (replicate n "?")
-}

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

-----

newtype PeerId = MkPeerId String
  deriving (Eq, Ord, Show)
  deriving newtype (JSON.FromJSON, JSON.FromJSONKey, JSON.ToJSON, JSON.ToJSONKey)

prettyPeerId :: PeerId -> String
prettyPeerId (MkPeerId x) = x

newtype EbId = MkEbId Int
  deriving (Eq, Ord, Show)
  deriving newtype (JSON.FromJSON, JSON.FromJSONKey, JSON.ToJSON, JSON.ToJSONKey)

prettyEbId :: EbId -> String
prettyEbId (MkEbId y) = show y

fromIntegralEbId :: Integral a => EbId -> a
fromIntegralEbId (MkEbId y) = fromIntegral y

newtype HashBytes = MkHashBytes ByteString
  deriving (Eq, Ord, Show)

prettyHashBytes :: HashBytes -> String
prettyHashBytes (MkHashBytes bytes) = BS8.unpack $ BS16.encode bytes

hashBytesToText :: HashBytes -> T.Text
hashBytesToText (MkHashBytes hash) =
    let hex = BS16.encode hash
    in
    case T.decodeUtf8' hex of
        Left err -> error $ "bad HashBytes, " <> show err <> ": " <> BS8.unpack hex
        Right txt -> txt

instance JSON.FromJSON HashBytes where
    parseJSON val = do
        txt <- JSON.parseJSON val
        case BS16.decode $ T.encodeUtf8 txt of
            Left s -> error $ "bad HashBytes, " <> s <> ": " <> T.unpack txt
            Right b -> pure $ MkHashBytes b

instance JSON.FromJSONKey HashBytes where
    fromJSONKey =
        JSON.FromJSONKeyTextParser $ \txt ->
        case BS16.decode $ T.encodeUtf8 txt of
            Left s -> error $ "bad HashBytes, " <> s <> ": " <> T.unpack txt
            Right b -> pure $ MkHashBytes b

instance JSON.ToJSON HashBytes where
    toJSON = JSON.toJSON . hashBytesToText
    toEncoding = JSON.toEncoding . hashBytesToText

instance JSON.ToJSONKey HashBytes where
    toJSONKey = hashBytesToText >$< JSON.toJSONKey

data LeiosFetchState = MkLeiosFetchState {
    -- | Which EBs each peer has offered the body of
    --
    -- TODO reverse index for when EBs age out?
    offeredEbs :: Map PeerId (Set EbId)
  ,
    -- | Which EBs each peer has offered the closure of
    --
    -- INVARIANT: all EBs from some peer exactly agree about the claimed size
    -- of any txs they share (TODO enforce)
    --
    -- TODO reverse index for when EBs age out?
    offeredEbTxs :: Map PeerId (Set EbId)
  ,
    -- | EBs whose bodies have been received
    acquiredEbBodies :: Set EbId
  ,
    -- | The size of each EB whose body has been offered but never received
    --
    -- TODO double-check it won't actually be possible for peers to list
    -- different sizes for the same EB, since 'EbId' will eventually be the
    -- header hash, not the hash of the EB body? (It's the announcement that
    -- specifies the EB body's hash.)
    missingEbBodies :: Map EbId BytesSize
  ,
    -- | Which requests have been sent to this peer
    --
    -- (The fetch logic will not update this when it decides on some requests.
    -- The LeiosFetch mini protocol clients update this when they actually send
    -- those requests.)
    requestedPerPeer :: Map PeerId [LeiosFetchRequest]
  ,
    -- | Which peers have outstanding requests for which EB bodies
    --
    -- INVARIANT: no empty sets
    requestedEbPeers :: Map EbId (Set PeerId)
  ,
    -- | Which peers have outstanding requests for which txs
    --
    -- INVARIANT: no empty sets
    --
    -- TODO may need to also store priority here
    requestedTxPeers :: Map TxHash (Set PeerId)
  ,
    -- | Outstanding requested bytes for each peer
    --
    -- INVARIANT: @Map.all (<= maxRequestedBytesSizePerPeer)@
    requestedBytesSizePerPeer :: Map PeerId BytesSize
  ,
    -- | Sum of 'requestedBytesSizePerPeer'
    --
    -- INVARIANT: @<= maxRequestedBytesSize@
    requestedBytesSize :: !BytesSize
  ,
    -- | The 'EbId', offsets, and sizes of txs that need to be copied from the
    -- TxCache to the EbStore
    toCopy :: Map EbId (IntMap BytesSize)
  ,
    -- | INVARIANT: @sum $ fmap sum 'toCopy'@
    toCopyBytesSize :: !BytesSize
  ,
    -- | INVARIANT: @sum $ fmap IntMap.size 'toCopy'@
    toCopyCount :: !Int
  }
  deriving (Generic)

-- | defaults to @GHC.Generics@
instance JSON.FromJSON LeiosFetchState where {}

-- | defaults to @GHC.Generics@
instance JSON.ToJSON LeiosFetchState where {}

emptyLeiosFetchState :: LeiosFetchState
emptyLeiosFetchState =
    MkLeiosFetchState
        Map.empty
        Map.empty
        Set.empty
        Map.empty
        Map.empty
        Map.empty
        Map.empty
        Map.empty
        0
        Map.empty
        0
        0

ebIdSlot :: EbId -> Word64
ebIdSlot (MkEbId y) =
    fromIntegral (y - minBound :: Int) `Bits.unsafeShiftR` 20 :: Word64

ebIdToPoint :: EbId -> LeiosFetchDynamicEnv -> Maybe (Word64, ByteString)
ebIdToPoint (MkEbId y) x =
    f <$> IntMap.lookup y (ebPointsInverse x)
  where
    f (MkHashBytes z) = (ebIdSlot (MkEbId y), z)

ebIdFromPoint :: Word64 -> ByteString -> LeiosFetchDynamicEnv -> (EbId, Maybe LeiosFetchDynamicEnv)
ebIdFromPoint ebSlot ebHash x =
    case IntMap.lookup (fromIntegral ebSlot) (ebPoints x) of
        Just m -> case Map.lookup hashBytes m of
            Just y -> (y, Nothing)
            Nothing -> gen $ MkEbId $ zero + (2^(20 :: Int) - 1) - Map.size m
        Nothing -> gen $ MkEbId $ zero + (2^(20 :: Int) - 1)
  where
    hashBytes = MkHashBytes ebHash

    zero = fromIntegral ((ebSlot `Bits.unsafeShiftL` 20) :: Word64) + minBound :: Int

    gen y =
        (,) y
      $ Just
      $ x { ebPoints = IntMap.insertWith Map.union (fromIntegral ebSlot) (Map.singleton hashBytes y) (ebPoints x)
          , ebPointsInverse = let MkEbId z = y in IntMap.insert z hashBytes (ebPointsInverse x)
          }

ebIdFromPoint' :: DB.Database -> Word64 -> ByteString -> IO EbId
ebIdFromPoint' db ebSlot ebHash = do
    dynEnv <- loadLeiosFetchDynEnvHelper False db
    let (ebId, mbDynEnv') = ebIdFromPoint ebSlot ebHash dynEnv
    case mbDynEnv' of
        Nothing -> pure ()
        Just{}  -> do
            -- INSERT INTO ebPoints
            stmt_write_ebIds <- withDieJust $ DB.prepare db (fromString sql_insert_ebId)
            withDie $ DB.bindInt64    stmt_write_ebIds 1 (fromIntegral ebSlot)
            withDie $ DB.bindBlob     stmt_write_ebIds 2 ebHash
            withDie $ DB.bindInt64    stmt_write_ebIds 3 (fromIntegralEbId ebId)
            withDieDone $ DB.stepNoCB stmt_write_ebIds
    pure ebId

-----

type BytesSize = Int

type TxHash = HashBytes

-- TODO which of these limits are allowed to be exceeded by at most one
-- request?
data LeiosFetchStaticEnv = MkLeiosFetchStaticEnv {
    -- | At most this many outstanding bytes requested from all peers together
    maxRequestedBytesSize :: BytesSize
  ,
    -- | At most this many outstanding bytes requested from each peer
    maxRequestedBytesSizePerPeer :: BytesSize
  ,
    -- | At most this many outstanding bytes per request
    maxRequestBytesSize :: BytesSize
  ,
    -- | At most this many outstanding requests for each EB body
    maxRequestsPerEb :: Int
  ,
    -- | At most this many outstanding requests for each individual tx
    maxRequestsPerTx :: Int
  ,
    -- | At most this many bytes are scheduled to be copied from the TxCache to the EbStore
    maxToCopyBytesSize :: BytesSize
  ,
    -- | At most this many txs are scheduled to be copied from the TxCache to the EbStore
    maxToCopyCount :: Int
  }

-- TODO these maps are too big to actually have on the heap in the worst-case:
-- 13888 txs per EB, 11000 EBs, 50 upstream peers, 32 bytes per hash
data LeiosFetchDynamicEnv = MkLeiosFetchDynamicEnv {
    -- | The size of every tx in the TxCache
    cachedTxs :: Map TxHash BytesSize
  ,
    -- | All missing txs in the context of EBs worth retrieving the closure for
    missingEbTxs :: Map EbId (IntMap (TxHash, BytesSize))
  ,
    -- | @slot -> hash -> EbId@
    --
    -- This relation is written to and loaded from the 'ebPoints' table on node
    -- shutdown and startup.
    --
    -- INVARIANT: the inner map is non-empty
    --
    -- INVARIANT: strictly ascending 'EbId's, which is ensured as follows.
    --
    -- Assumptions:
    --   o This code base will not survive more than 100 years.
    --   o The EBs' slot schedule will never be more granular than 5000 per second.
    --   o There will be never be more than 2^20 elections in a single EB slot.
    --
    -- Therefore, since 2^(64 - 20) = 2^44 seconds is more than 500,000 years,
    -- the 20 highest-order bits of the slot number will be 0 for the entire
    -- lifetime of this codebase, and so can be repurposed to identify
    -- individual EBs.
    --
    -- PREREQ: As justified above, this codes assumes the slot number will
    -- never exceed 2^44 - 1.
    --
    -- Thus, the first EB with slot S received is assigned 'EbId' @fromIntegral
    -- (S << 20) + 2^20 - 1 + minBound@, the next EB with slot S to arrive is
    -- assigned the predecessor (so the tiebreaker favors first to arrive), and
    -- so on. Note that @(enm () :: [Int]) == map cnv (enm () :: [Word64])@
    -- where @cnv w = fromIntegral (w :: Word64) + minBound :: Int@ and @enm ()
    -- = [minBound, minBound + 1, maxBound - 1, maxBound]@.
    ebPoints :: IntMap {- SlotNo -} (Map HashBytes EbId)
  ,
    -- | Reverse index of 'ebPoints', just the hash
    --
    -- INVARIANT: @(ebPoints IntMap.! (ebIdSlot ebId)) Map.! (ebPointsInverse IntMap.! ebId) = ebId@
    ebPointsInverse :: IntMap {- EbId -} HashBytes
  ,
    -- | Txs listed in received EBs but never themselves received
    missingTxBodies :: Set TxHash
  ,
    -- | Reverse index of 'missingEbTxs'
    --
    -- INVARIANT: @let (ebId, txOffset) = txOffsetss Map.! h in h = fst ((missingEbTxs IntMap.! ebId) IntMap.! txOffset)@
    txOffsetss :: Map TxHash (Map EbId Int)
  }

emptyLeiosFetchDynEnv :: LeiosFetchDynamicEnv
emptyLeiosFetchDynEnv =
    MkLeiosFetchDynamicEnv
        Map.empty
        Map.empty
        IntMap.empty
        IntMap.empty
        Set.empty
        Map.empty

loadLeiosFetchDynEnv :: DB.Database -> IO LeiosFetchDynamicEnv
loadLeiosFetchDynEnv = loadLeiosFetchDynEnvHelper True

loadLeiosFetchDynEnvHelper :: Bool -> DB.Database -> IO LeiosFetchDynamicEnv
loadLeiosFetchDynEnvHelper full db = do
    withDieMsg $ DB.exec db (fromString "BEGIN")
    (ps, qs) <- do
        stmt <- withDieJust $ DB.prepare db (fromString sql_scan_ebId)
        let loop !ps !qs =
                withDie (DB.stepNoCB stmt) >>= \case
                    DB.Done -> pure (ps, qs)
                    DB.Row -> do
                        ebSlot <- fromIntegral <$> DB.columnInt64 stmt 0
                        ebHash <- MkHashBytes <$> DB.columnBlob stmt 1
                        ebId <- fromIntegral <$> DB.columnInt64 stmt 2
                        loop
                            (IntMap.insertWith Map.union ebSlot (Map.singleton ebHash (MkEbId ebId)) ps)
                            (IntMap.insert ebId ebHash qs)
        loop IntMap.empty IntMap.empty
    cached <- if not full then pure Map.empty else do
        stmt <- withDieJust $ DB.prepare db (fromString sql_scan_txCache)
        let loop !cached =
                withDie (DB.stepNoCB stmt) >>= \case
                    DB.Done -> pure cached
                    DB.Row -> do
                        txHashBytes <- MkHashBytes <$> DB.columnBlob stmt 0
                        txBytesSize <- fromIntegral <$> DB.columnInt64 stmt 1
                        loop (Map.insert txHashBytes txBytesSize cached)
        loop Map.empty
    (missing, bodies, offsetss) <- if not full then pure (Set.empty, Map.empty, Map.empty) else do
        stmt <- withDieJust $ DB.prepare db (fromString sql_scan_missingEbTx)
        let loop !missing !bodies !offsetss =
                withDie (DB.stepNoCB stmt) >>= \case
                    DB.Done -> pure (missing, bodies, offsetss)
                    DB.Row -> do
                        ebId <- (MkEbId . fromIntegral) <$> DB.columnInt64 stmt 0
                        txOffset <- fromIntegral <$> DB.columnInt64 stmt 1
                        txHash <- MkHashBytes <$> DB.columnBlob stmt 2
                        txBytesSize <- fromIntegral <$> DB.columnInt64 stmt 3
                        loop
                            (Set.insert txHash missing)
                            (Map.insertWith IntMap.union ebId (IntMap.singleton txOffset (txHash, txBytesSize)) bodies)
                            (Map.insertWith Map.union txHash (Map.singleton ebId txOffset) offsetss)
        loop Set.empty Map.empty Map.empty
    withDieMsg $ DB.exec db (fromString "COMMIT")
    pure MkLeiosFetchDynamicEnv {
        cachedTxs = cached
      ,
        missingEbTxs = bodies
      ,
        ebPoints = ps
      ,
        ebPointsInverse = qs
      ,
        missingTxBodies = missing
      , 
        txOffsetss = offsetss
      }

sql_scan_ebId :: String
sql_scan_ebId =
    "SELECT ebSlot, ebHashBytes, ebId\n\
    \FROM ebPoints\n\
    \ORDER BY ebId ASC\n\
    \"

sql_scan_missingEbTx :: String
sql_scan_missingEbTx =
    "SELECT ebId, txOffset, txHashBytes, txBytesSize\n\
    \FROM ebTxs\n\
    \WHERE txBytes IS NULL\n\
    \ORDER BY ebId DESC, txOffset ASC\n\
    \"

sql_scan_txCache :: String
sql_scan_txCache =
    "SELECT txHashBytes, txBytesSize\n\
    \FROM txCache\n\
    \ORDER BY txHashBytes\n\
    \"

-----

newtype LeiosFetchDecisions =
    MkLeiosFetchDecisions
        (Map PeerId (Map Word64 (DList (TxHash, BytesSize, Map EbId Int), DList EbId)))
  deriving (Show)

emptyLeiosFetchDecisions :: LeiosFetchDecisions
emptyLeiosFetchDecisions = MkLeiosFetchDecisions Map.empty

leiosFetchLogicIteration ::
    LeiosFetchStaticEnv
 ->
    LeiosFetchDynamicEnv
 ->
    LeiosFetchState
 ->
    (LeiosFetchState, LeiosFetchDecisions)
leiosFetchLogicIteration env dynEnv =
    \acc ->
        go1 acc emptyLeiosFetchDecisions
      $ expand
      $ Map.toDescList
      $ Map.map Left (missingEbBodies acc) `Map.union` Map.map Right (missingEbTxs dynEnv)
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
          | let peerIds :: Set PeerId
                peerIds = Map.findWithDefault Set.empty ebId (requestedEbPeers acc)
         -> goEb2 acc accNew targets ebId ebBytesSize peerIds

        Right (ebId, txOffset, txHash) : targets

          | not $ Set.member txHash (missingTxBodies dynEnv)   -- we already have it
         -> go1 acc accNew targets

          | Just _ <- Map.lookup ebId (toCopy acc) >>= IntMap.lookup txOffset
              -- it's already scheduled to be copied from TxCache
         -> go1 acc accNew targets

          | Just txBytesSize <- Map.lookup txHash (cachedTxs dynEnv)   -- it's in the TxCache
         -> let full =
                    toCopyBytesSize acc >= maxToCopyBytesSize env
                 ||
                    toCopyCount acc >= maxToCopyCount env
                acc' =
                    if full then acc else
                    acc {
                        toCopy = Map.insertWith IntMap.union ebId (IntMap.singleton txOffset txBytesSize) (toCopy acc)
                      ,
                        toCopyBytesSize = toCopyBytesSize acc + txBytesSize
                      ,
                        toCopyCount = toCopyCount acc + 1
                      }
            in go1 acc' accNew targets

          | otherwise
         -> let !txOffsets = case Map.lookup txHash (txOffsetss dynEnv) of
                    Nothing -> error "impossible!"
                    Just x -> x
                peerIds :: Set PeerId
                peerIds = Map.findWithDefault Set.empty txHash (requestedTxPeers acc)
            in
            goTx2 acc accNew targets (ebIdSlot ebId) txHash txOffsets peerIds

    goEb2 !acc !accNew targets ebId ebBytesSize peerIds
      | requestedBytesSize acc >= maxRequestedBytesSize env   -- we can't request anything
      = (acc, accNew)

      | Set.size peerIds < maxRequestsPerEb env   -- we would like to request it from an additional peer
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
                requestedEbPeers = Map.insertWith Set.union ebId (Set.singleton peerId) (requestedEbPeers acc)
              ,
                requestedBytesSizePerPeer = Map.insertWith (+) peerId ebBytesSize (requestedBytesSizePerPeer acc)
              ,
                requestedBytesSize = ebBytesSize + requestedBytesSize acc
              }
            peerIds' = Set.insert peerId peerIds
        in
        goEb2 acc' accNew' targets ebId ebBytesSize peerIds'

      | otherwise
      = go1 acc accNew targets

    choosePeerEb :: Set PeerId -> LeiosFetchState -> EbId -> Maybe PeerId
    choosePeerEb peerIds acc ebId =
        foldr (\a _ -> Just a) Nothing
      $ [ peerId
        | (peerId, ebIds) <-
              Map.toList   -- TODO prioritize/shuffle?
            $ (`Map.withoutKeys` peerIds)   -- not already requested from this peer
            $ offeredEbs acc
        , Map.findWithDefault 0 peerId (requestedBytesSizePerPeer acc) <= maxRequestedBytesSizePerPeer env
            -- peer can be sent more requests
        , ebId `Set.member` ebIds   -- peer has offered this EB body
        ]

    goTx2 !acc !accNew targets ebSlot txHash txOffsets peerIds

      | requestedBytesSize acc >= maxRequestedBytesSize env   -- we can't request anything
      = (acc, accNew)

      | Set.size peerIds < maxRequestsPerTx env   -- we would like to request it from an additional peer
          -- TODO if requests list priority, does this limit apply even if the
          -- tx has only been requested at lower priorities?
      , Just (peerId, txOffsets') <- choosePeerTx peerIds acc txOffsets
          -- there's a peer who offered it and we haven't already requested it from them
      = let txBytesSize = case Map.lookupMax txOffsets' of
                Nothing -> error "impossible!"
                Just (ebId, txOffset) -> case Map.lookup ebId (missingEbTxs dynEnv) of
                    Nothing -> error "impossible!"
                    Just v -> snd $ v IntMap.! txOffset
            accNew' =
                MkLeiosFetchDecisions
              $ Map.insertWith
                    (Map.unionWith (<>))
                    peerId
                    (Map.singleton ebSlot (DList.singleton (txHash, txBytesSize, txOffsets'), DList.empty))
                    (let MkLeiosFetchDecisions x = accNew in x)
            acc' = acc {
                requestedTxPeers = Map.insertWith Set.union txHash (Set.singleton peerId) (requestedTxPeers acc)
              ,
                requestedBytesSizePerPeer = Map.insertWith (+) peerId txBytesSize (requestedBytesSizePerPeer acc)
              ,
                requestedBytesSize = txBytesSize + requestedBytesSize acc
              }
            peerIds' = Set.insert peerId peerIds
        in
        goTx2 acc' accNew' targets ebSlot txHash txOffsets peerIds'

      | otherwise
      = go1 acc accNew targets

    choosePeerTx :: Set PeerId -> LeiosFetchState -> Map EbId Int -> Maybe (PeerId, Map EbId Int)
    choosePeerTx peerIds acc txOffsets =
        foldr (\a _ -> Just a) Nothing
      $ [ (peerId, txOffsets')
        | (peerId, ebIds) <-
              Map.toList   -- TODO prioritize/shuffle?
            $ (`Map.withoutKeys` peerIds)   -- not already requested from this peer
            $ offeredEbTxs acc
        , Map.findWithDefault 0 peerId (requestedBytesSizePerPeer acc) <= maxRequestedBytesSizePerPeer env
            -- peer can be sent more requests
        , let txOffsets' = txOffsets `Map.restrictKeys` ebIds
        , not $ Map.null txOffsets'   -- peer has offered an EB closure that includes this tx
        ]

-----

data LeiosFetchRequest =
    LeiosBlockRequest LeiosBlockRequest
  |
    LeiosBlockTxsRequest LeiosBlockTxsRequest
  deriving (Generic, Show)

-- | defaults to @GHC.Generics@
instance JSON.FromJSON LeiosFetchRequest where {}

-- | defaults to @GHC.Generics@
instance JSON.ToJSON LeiosFetchRequest where {}

data LeiosBlockRequest =
    -- | ebSlot, ebHash
    MkLeiosBlockRequest
        !Word64
        !HashBytes
  deriving (Generic, Show)

-- | defaults to @GHC.Generics@
instance JSON.FromJSON LeiosBlockRequest where {}

-- | defaults to @GHC.Generics@
instance JSON.ToJSON LeiosBlockRequest where {}

data LeiosBlockTxsRequest =
    -- | ebSlot, ebHash, bitmaps, txHashes
    --
    -- The hashes aren't sent to the peer, but they are used to validate the
    -- reply when it arrives.
    MkLeiosBlockTxsRequest
        !Word64
        !HashBytes
        [(Word16, Word64)]
        !(V.Vector TxHash)
  deriving (Generic, Show)

-- | defaults to @GHC.Generics@
instance JSON.FromJSON LeiosBlockTxsRequest where {}

-- | defaults to @GHC.Generics@
instance JSON.ToJSON LeiosBlockTxsRequest where {}

packRequests :: LeiosFetchStaticEnv -> LeiosFetchDynamicEnv -> LeiosFetchDecisions -> Map PeerId [LeiosFetchRequest]
packRequests env dynEnv =
    \(MkLeiosFetchDecisions x) -> Map.map goPeer x
  where
    goPeer =
         DList.toList
       . Map.foldlWithKey
            (\acc prio (txs, ebs) -> goPrioTx prio txs <> goPrioEb prio ebs <> acc)
                -- TODO priority within same slot?
            DList.empty

    goPrioEb _prio ebs =
        DList.map
            (\ebId -> case ebIdToPoint ebId dynEnv of
                Nothing -> error "impossible!"
                Just (ebSlot, ebHash) -> LeiosBlockRequest $ MkLeiosBlockRequest ebSlot (MkHashBytes ebHash)
            )
            ebs

    goPrioTx _prio txs =
        Map.foldlWithKey
            (\acc ebId txs' ->
                case ebIdToPoint ebId dynEnv of
                    Nothing -> error "impossible!"
                    Just (ebSlot, ebHash) ->
                        goEb
                            {- prio -}
                            ebSlot ebHash
                            0
                            IntMap.empty
                            0
                            DList.empty
                            (IntMap.toAscList txs')
                     <> acc
            )
            DList.empty
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
        Word64
     ->
        ByteString
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
        DList LeiosFetchRequest
    -- TODO the incoming indexes are ascending, so the IntMap accumulator could
    -- be simplified away
    goEb ebSlot ebHash !accTxBytesSize !accBitmaps !accN !accHashes = \case
        [] -> if 0 < accN then DList.singleton flush else DList.empty
        (txOffset, (txHash, txBytesSize)):txs

          | maxRequestBytesSize env < accTxBytesSize'
         -> flush `DList.cons` goEb ebSlot ebHash 0 IntMap.empty 0 DList.empty txs

          | otherwise
          , let (q, r) = txOffset `divMod` 64
         -> goEb
                ebSlot ebHash
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
                ebSlot
                (MkHashBytes ebHash)
                [ (fromIntegral idx, bitmap) | (idx, bitmap) <- IntMap.toAscList accBitmaps ]
                (V.fromListN accN $ DList.toList accHashes)              

-----

fetchDecision2 :: DB.Database -> LeiosFetchState -> IO LeiosFetchState
fetchDecision2 db acc0 = do
    let million = 10^(6 :: Int)
        millionBase2 = 2^(20 :: Int)
        thousand = 10^(3 :: Int)
        env = MkLeiosFetchStaticEnv {
            maxRequestedBytesSize = 50 * million
          ,
            maxRequestedBytesSizePerPeer = 5 * million
          ,
            maxRequestBytesSize = 500 * thousand
          ,
            maxRequestsPerEb = 2
          ,
            maxRequestsPerTx = 2
          ,
            maxToCopyBytesSize = 100 * millionBase2
          ,
            maxToCopyCount = 100 * thousand
          }
    dynEnv <- loadLeiosFetchDynEnv db
    let (acc1, MkLeiosFetchDecisions decisions) = leiosFetchLogicIteration env dynEnv acc0
    forM_ (Map.toList decisions) $ \(peerId, slots) -> do
        forM_ (Map.toDescList slots) $ \(slot, (_txs, ebs)) -> do
            forM_ ebs $ \ebId -> do
                putStrLn $ unwords ["EB", prettyPeerId peerId, show slot, prettyEbId ebId]
        forM_ (Map.toDescList slots) $ \(slot, (txs, _ebs)) -> do
            forM_ txs $ \(txHash, _txBytesSize, ebIds) -> do
                putStrLn $ unwords $ "TX" : prettyPeerId peerId : show slot : prettyHashBytes txHash : [ prettyEbId ebId ++ "~" ++ show txOffset | (ebId, txOffset) <- Map.toList ebIds ]
    acc2 <- (\f -> foldM f acc1 (Map.toList $ packRequests env dynEnv (MkLeiosFetchDecisions decisions))) $ \acc (peerId, reqs) -> do
        forM_ reqs $ \case
            LeiosBlockRequest (MkLeiosBlockRequest ebSlot ebHash) -> do
                putStrLn $ unwords ["MSG", "MsgLeiosBlockRequest", prettyPeerId peerId, show ebSlot ,prettyHashBytes ebHash]
            LeiosBlockTxsRequest (MkLeiosBlockTxsRequest ebSlot ebHash bitmaps _txHashes) -> do
                putStrLn $ unwords $ "MSG" : "MsgLeiosBlockTxsRequest" : prettyPeerId peerId : show ebSlot : prettyHashBytes ebHash : map prettyBitmap bitmaps
        pure $ acc { requestedPerPeer = Map.insertWith (\new old -> old ++ new) peerId reqs (requestedPerPeer acc) }
    pure acc2

-----

openEvenIfMissing :: FilePath -> FilePath -> IO (DB.Database, LeiosFetchState)
openEvenIfMissing dbPath lfstPath = do
    db <- do
        b <- doesFileExist dbPath
        db <- withDieMsg $ DB.open (fromString dbPath)
        when (not b) $ do
            withDieMsg $ DB.exec db (fromString sql_schema)
            withDieMsg $ DB.exec db (fromString sql_index_schema)
        pure db
    lfst <- doesFileExist lfstPath >>= \case
        True -> withDiePoly id $ JSON.eitherDecodeFileStrict lfstPath
        False -> do
            JSON.encodeFile lfstPath emptyLeiosFetchState
            pure emptyLeiosFetchState
    pure (db, lfst)

msgLeiosBlockOffer :: LeiosFetchState -> PeerId -> EbId -> BytesSize -> IO LeiosFetchState
msgLeiosBlockOffer acc peerId ebId ebBytesSize = do
    pure acc {
        offeredEbs = Map.insertWith Set.union peerId (Set.singleton ebId) (offeredEbs acc)
      ,
        missingEbBodies =
            (   if Set.member ebId (acquiredEbBodies acc) then id else
                Map.insert ebId ebBytesSize
            )
          $ missingEbBodies acc
      }

msgLeiosBlockTxsOffer :: LeiosFetchState -> PeerId -> EbId -> IO LeiosFetchState
msgLeiosBlockTxsOffer acc peerId ebId = do
    pure acc {
        offeredEbs = Map.insertWith Set.union peerId (Set.singleton ebId) (offeredEbs acc)
      ,
        offeredEbTxs = Map.insertWith Set.union peerId (Set.singleton ebId) (offeredEbTxs acc)
      }

-----

delIfNull :: Set a -> Maybe (Set a)
delIfNull x = if Set.null x then Nothing else Just x

delIfZero :: (Eq a, Num a) => a -> Maybe a
delIfZero x = if 0 == x then Nothing else Just x

-----

doCacheCopy :: DB.Database -> LeiosFetchState -> BytesSize -> IO LeiosFetchState
doCacheCopy db lfst bytesSize = do
    withDieMsg $ DB.exec db (fromString sql_attach_memTxPoints)
    withDieMsg $ DB.exec db (fromString "BEGIN")
    stmt <- withDieJust $ DB.prepare db (fromString sql_insert_memTxPoints)
    -- load in-mem table of ebId-txOffset pairs
    lfst' <- go1 stmt 0 0 (toCopy lfst)
    withDie $ DB.finalize stmt
    -- UPDATE JOIN driven by the loaded table
    withDieMsg $ DB.exec db (fromString sql_copy_from_txCache)
    withDieMsg $ DB.exec db (fromString "COMMIT")
    withDieMsg $ DB.exec db (fromString sql_detach_memTxPoints)
    pure lfst'
  where
    go1 stmt !accBytesSize !accCount !acc
      | accBytesSize < bytesSize
      , Just ((ebId, txs), acc') <- Map.maxViewWithKey acc
      = go2 stmt accBytesSize accCount acc' ebId txs

      | otherwise
      = finish accBytesSize accCount acc

    go2 stmt !accBytesSize !accCount !acc ebId txs
      | Just ((txOffset, txBytesSize), txs') <- IntMap.minViewWithKey txs
      = if accBytesSize + txBytesSize > bytesSize then stop else do
            withDie $ DB.bindInt64    stmt 1 (fromIntegralEbId ebId)
            withDie $ DB.bindInt64    stmt 2 (fromIntegral txOffset)
            withDieDone $ DB.stepNoCB stmt
            withDie $ DB.reset        stmt
            go2 stmt (accBytesSize + txBytesSize) (accCount + 1) acc ebId txs'
      | otherwise
      = go1 stmt accBytesSize accCount acc
      where
        stop = finish accBytesSize accCount $ if IntMap.null txs then acc else Map.insert ebId txs acc

    finish accBytesSize accCount acc =
        pure lfst {
            toCopy = acc
          ,
            toCopyBytesSize = toCopyBytesSize lfst - accBytesSize
          ,
            toCopyCount = toCopyCount lfst - accCount
          }

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

sql_copy_from_txCache :: String
sql_copy_from_txCache =
    "UPDATE ebTxs\n\
    \SET txBytes = (SELECT txBytes FROM txCache WHERE txCache.txHashBytes = x.txHashBytes)\n\
    \FROM ebTxs AS x\n\
    \INNER JOIN mem.txPoints ON x.ebId = mem.txPoints.ebId AND x.txOffset = mem.txPoints.txOffset\n\
    \"
