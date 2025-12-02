{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Util.CRC
  ( CRCError (..)
  , crcOfConcat
  , readCRC
  ) where

import Control.Monad.Class.MonadThrow
import Control.Monad.Except
import Data.Bits
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Char hiding (isHexDigit)
import System.FS.API
import System.FS.API.Lazy
import System.FS.CRC

instance Semigroup CRC where
  (<>) = crcOfConcat

instance Monoid CRC where
  mempty = initCRC

crcOfConcat :: CRC -> CRC -> CRC
crcOfConcat crc1 crc2 =
  computeCRC $
    BSL.toStrict $
      BS.toLazyByteString $
        (BS.word32Dec $ getCRC crc1)
          <> (BS.word32Dec $ getCRC crc2)

data CRCError
  = CRCInvalid
  | CRCNoFile
  deriving (Eq, Show)

readCRC ::
  MonadThrow m =>
  HasFS m h ->
  FsPath ->
  ExceptT CRCError m CRC
readCRC hasFS crcPath = ExceptT $ do
  crcExists <- doesFileExist hasFS crcPath
  if not crcExists
    then pure (Left CRCNoFile)
    else do
      withFile hasFS crcPath ReadMode $ \h -> do
        str <- BSL.toStrict <$> hGetAll hasFS h
        if not (BSC.length str == 8 && BSC.all isHexDigit str)
          then pure (Left CRCInvalid)
          else pure . Right . CRC $ fromIntegral (hexdigitsToInt str)
 where
  -- TODO: remove the functions in the where clause when we start depending on lsm-tree

  isHexDigit :: Char -> Bool
  isHexDigit c =
    (c >= '0' && c <= '9')
      || (c >= 'a' && c <= 'f') -- lower case only

  -- Precondition: BSC.all isHexDigit
  hexdigitsToInt :: BSC.ByteString -> Word
  hexdigitsToInt =
    BSC.foldl' accumdigit 0
   where
    accumdigit :: Word -> Char -> Word
    accumdigit !a !c =
      (a `shiftL` 4) .|. hexdigitToWord c

  -- Precondition: isHexDigit
  hexdigitToWord :: Char -> Word
  hexdigitToWord c
    | let !dec = fromIntegral (ord c - ord '0')
    , dec <= 9 =
        dec
    | let !hex = fromIntegral (ord c - ord 'a' + 10)
    , otherwise =
        hex
