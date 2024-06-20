{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Util.CBOR (
    -- * Decode as FlatTerm
    decodeAsFlatTerm
    -- * HasFS interaction
  , ReadIncrementalErr (..)
  , readIncremental
  , withStreamIncrementalOffsets
    -- * Encoding/decoding containers
  , decodeMaybe
  , decodeWithOrigin
  , encodeMaybe
  , encodeWithOrigin
  ) where

import           Cardano.Binary (decodeMaybe, encodeMaybe)
import           Cardano.Slotting.Slot (WithOrigin (..), withOriginFromMaybe,
                     withOriginToMaybe)
import qualified Codec.CBOR.Decoding as CBOR.D
import qualified Codec.CBOR.Encoding as CBOR.E
import qualified Codec.CBOR.FlatTerm as CBOR.F
import qualified Codec.CBOR.Read as CBOR.R
import           Control.Monad.Except
import           Control.Monad.ST
import qualified Control.Monad.ST.Lazy as ST.Lazy
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Builder.Extra (defaultChunkSize)
import qualified Data.ByteString.Lazy as LBS
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Util.IOLike as U
import qualified Streaming as S
import qualified Streaming.Prelude as S
import           Streaming.Prelude (Of (..), Stream)
import           System.FS.API

{-------------------------------------------------------------------------------
  Decode as FlatTerm
-------------------------------------------------------------------------------}

decodeAsFlatTerm ::
     ByteString
  -> Either CBOR.R.DeserialiseFailure CBOR.F.FlatTerm
decodeAsFlatTerm bs0 =
    ST.Lazy.runST (runExceptT (provideInput bs0))
  where
    provideInput ::
         ByteString
      -> ExceptT CBOR.R.DeserialiseFailure (ST.Lazy.ST s) CBOR.F.FlatTerm
    provideInput bs
      | BS.null bs = return []
      | otherwise      = do
          next <- S.lift $ ST.Lazy.strictToLazyST $ do
              -- This will always be a 'Partial' here because decodeTermToken
              -- always starts by requesting initial input. Only decoders that
              -- fail or return a value without looking at their input can give
              -- a different initial result.
              idc <- CBOR.R.deserialiseIncremental CBOR.F.decodeTermToken
              let k = fromPartial idc
              k (Just bs)
          collectOutput next

      where
        fromPartial ::
             CBOR.R.IDecode s a
          -> Maybe ByteString
          -> ST s (CBOR.R.IDecode s a)
        fromPartial idc = case idc of
            CBOR.R.Partial k -> k
            CBOR.R.Done{}    -> error "fromPartial: expected a Partial decoder"
            CBOR.R.Fail{}    -> error "fromPartial: expected a Partial decoder"

    collectOutput ::
         CBOR.R.IDecode s CBOR.F.TermToken
      -> ExceptT CBOR.R.DeserialiseFailure (ST.Lazy.ST s) CBOR.F.FlatTerm
    collectOutput (CBOR.R.Fail _ _ err) = throwError err
    collectOutput (CBOR.R.Partial    k) = S.lift (ST.Lazy.strictToLazyST (k Nothing)) >>=
                                          collectOutput
    collectOutput (CBOR.R.Done bs' _ x) = do xs <- provideInput bs'
                                             return (x : xs)

{-------------------------------------------------------------------------------
  HasFS interaction
-------------------------------------------------------------------------------}

data ReadIncrementalErr =
    -- | Could not deserialise the data
    ReadFailed CBOR.R.DeserialiseFailure

    -- | Deserialisation was successful, but there was additional data
  | TrailingBytes ByteString
  deriving (Eq, Show)

-- | Read a file incrementally
--
-- NOTE: The 'MonadThrow' constraint is only needed for 'bracket'. This
-- function does not actually throw anything.
--
-- NOTE: This uses a chunk size of roughly 32k. If we use this function to read
-- small things this might not be ideal.
--
-- NOTE: This currently expects the file to contain precisely one value; see also
-- 'withStreamIncrementalOffsets'.
readIncremental :: forall m a. IOLike m
                => SomeHasFS m
                -> CBOR.D.Decoder (U.PrimState m) a
                -> FsPath
                -> m (Either ReadIncrementalErr a)
readIncremental = \(SomeHasFS hasFS) decoder fp -> do
    withFile hasFS fp ReadMode $ \h ->
      go hasFS h =<< U.stToIO (CBOR.R.deserialiseIncremental decoder)
  where
    go :: HasFS m h
       -> Handle h
       -> CBOR.R.IDecode (U.PrimState m) a
       -> m (Either ReadIncrementalErr a)
    go hasFS@HasFS{..} h (CBOR.R.Partial k) = do
        bs   <- hGetSome h (fromIntegral defaultChunkSize)
        dec' <- U.stToIO $ k (checkEmpty bs)
        go hasFS h dec'
    go _ _ (CBOR.R.Done leftover _ a) =
        return $ if BS.null leftover
                   then Right a
                   else Left $ TrailingBytes leftover
    go _ _ (CBOR.R.Fail _ _ err) =
        return $ Left $ ReadFailed err

    checkEmpty :: ByteString -> Maybe ByteString
    checkEmpty bs | BS.null bs = Nothing
                  | otherwise  = Just bs

-- | Read multiple @a@s incrementally from a file in a streaming way.
--
-- Continuation-passing style to ensure proper closure of the file.
--
-- Reads the offset ('Word64') of the start of each @a@, the size ('Word64')
-- of each @a@, and each @a@ itself. When deserialising fails, it passes all
-- already deserialised @a@s, the error, and the offset after which the
-- failure occurred.
--
-- NOTE: f we introduce user-facing streaming API also, the fact that we are
-- using @streaming@ here should not dictate that we should stick with it
-- later; rather, we should revisit this code at that point.
withStreamIncrementalOffsets ::
     forall m h a r. (IOLike m, HasCallStack)
  => HasFS m h
  -> (forall s . CBOR.D.Decoder s (LBS.ByteString -> a))
  -> FsPath
  -> (Stream (Of (Word64, (Word64, a))) m (Maybe (ReadIncrementalErr, Word64)) -> m r)
  -> m r
withStreamIncrementalOffsets hasFS@HasFS{..} decoder fp = \k ->
      withFile hasFS fp ReadMode $ \h -> k $ do
        fileSize <- S.lift $ hGetSize h
        if fileSize == 0 then
          -- If the file is empty, we will immediately get "end of input"
          return Nothing
        else
          S.lift (U.stToIO (CBOR.R.deserialiseIncremental decoder)) >>=
            go h 0 Nothing [] fileSize
  where
    -- TODO stream from HasFS?
    go :: Handle h
       -> Word64                   -- ^ Offset
       -> Maybe ByteString         -- ^ Unconsumed bytes from last time
       -> [ByteString]             -- ^ Chunks pushed for this item (rev order)
       -> Word64                   -- ^ Total file size
       -> CBOR.R.IDecode (U.PrimState m) (LBS.ByteString -> a)
       -> Stream (Of (Word64, (Word64, a))) m (Maybe (ReadIncrementalErr, Word64))
    go h offset mbUnconsumed bss fileSize dec = case dec of
      CBOR.R.Partial k -> do
        -- First use the unconsumed bytes from a previous read before read
        -- some more bytes from the file.
        bs   <- case mbUnconsumed of
          Just unconsumed -> return unconsumed
          Nothing         -> S.lift $ hGetSome h (fromIntegral defaultChunkSize)
        dec' <- S.lift $ U.stToIO $ k (checkEmpty bs)
        go h offset Nothing (bs:bss) fileSize dec'

      CBOR.R.Done leftover size mkA -> do
        let nextOffset = offset + fromIntegral size
            -- We've been keeping track of the bytes pushed into the decoder
            -- for this item so far in bss. Now there's some trailing data to
            -- remove and we can get the whole bytes used for this item. We
            -- supply the bytes to the final decoded value. This is to support
            -- annotating values with their original input bytes.
            aBytes     = case bss of
                []      -> LBS.empty
                bs:bss' -> LBS.fromChunks (reverse (bs' : bss'))
                  where
                    bs' = BS.take (BS.length bs - BS.length leftover) bs
            -- The bang on the @a'@ here allows the used 'Decoder' to force
            -- its computation. For example, the decoder might decode a whole
            -- block and then (maybe through a use of 'fmap') just return its
            -- hash. If we don't force the value it returned here, we're just
            -- putting a thunk that references the whole block in the list
            -- instead of merely the hash.
            !a         = mkA aBytes
        S.yield (offset, (fromIntegral size, a))
        case checkEmpty leftover of
          Nothing
            | nextOffset == fileSize
              -- We're at the end of the file, so stop
            -> return Nothing
          -- Some more bytes, so try to read the next @a@.
          mbLeftover ->
            S.lift (U.stToIO (CBOR.R.deserialiseIncremental decoder)) >>=
            go h nextOffset mbLeftover [] fileSize

      CBOR.R.Fail _ _ err -> return $ Just (ReadFailed err, offset)

    checkEmpty :: ByteString -> Maybe ByteString
    checkEmpty bs | BS.null bs = Nothing
                  | otherwise  = Just bs

{-------------------------------------------------------------------------------
  Encoding/decoding lists
-------------------------------------------------------------------------------}

encodeWithOrigin :: (a -> CBOR.E.Encoding) -> WithOrigin a -> CBOR.E.Encoding
encodeWithOrigin f = encodeMaybe f . withOriginToMaybe

decodeWithOrigin :: CBOR.D.Decoder s a -> CBOR.D.Decoder s (WithOrigin a)
decodeWithOrigin f = withOriginFromMaybe <$> decodeMaybe f
