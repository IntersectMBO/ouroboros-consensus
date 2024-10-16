{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module FooBarBz.Encoder (encode) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Base64 as B64
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified FooBarBz.Base32768 as Qntm

encode :: [Double] -> IO ()
encode xs = (T.putStr $ T.toLazyText $ Qntm.encode $ B.toLazyByteString $ serialize xs) `asTypeOf` (BSL.putStr $ T.encodeUtf16LE $ T.toLazyText $ Qntm.encode $ B.toLazyByteString $ serialize xs) `asTypeOf` do
    T.putStr "data:application/octet-stream;base64,"
    T.putStrLn $ B64.encodeBase64 $ B.toLazyByteString $ serialize xs

serialize :: [Double] -> B.Builder
serialize = foldr (\x acc -> B.doubleLE x <> acc) mempty
