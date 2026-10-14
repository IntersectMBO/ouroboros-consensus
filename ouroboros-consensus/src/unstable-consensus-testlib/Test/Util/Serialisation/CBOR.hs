-- | CBOR lists and maps built by hand, for testing that a decoder accepts
-- either the definite-length or the indefinite-length encoding.
module Test.Util.Serialisation.CBOR
  ( definite
  , definiteMap
  , indefinite
  , indefiniteMap
  ) where

import qualified Codec.CBOR.Encoding as CBOR

-- | The items as a definite-length list.
definite :: [CBOR.Encoding] -> CBOR.Encoding
definite xs = CBOR.encodeListLen (fromIntegral $ length xs) <> mconcat xs

-- | The items as an indefinite-length list.
indefinite :: [CBOR.Encoding] -> CBOR.Encoding
indefinite xs = CBOR.encodeListLenIndef <> mconcat xs <> CBOR.encodeBreak

-- | The key-value pairs as a definite-length map.
definiteMap :: [CBOR.Encoding] -> CBOR.Encoding
definiteMap kvs = CBOR.encodeMapLen (fromIntegral $ length kvs) <> mconcat kvs

-- | The key-value pairs as an indefinite-length map.
indefiniteMap :: [CBOR.Encoding] -> CBOR.Encoding
indefiniteMap kvs = CBOR.encodeMapLenIndef <> mconcat kvs <> CBOR.encodeBreak
