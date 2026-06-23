{- cabal:
  build-depends: cborg, bytestring, base
-}

-- | A simple script that unwraps a CBOR term serialized as
-- CBOR-in-CBOR. It gets input from stdin and emits on stdout.
--
-- > cat pre.cbor | cabal run ./scripts/unwrap24serialised.hs > post.cbor
module Main where

import Prelude hiding (interact)
import Data.ByteString
import Codec.CBOR.Term
import Codec.CBOR.Write
import Codec.CBOR.Read
import Data.ByteString.Lazy (fromStrict, toStrict)

main = interact $
    toStrict
  . toLazyByteString
  . encodeTerm
  . (\(Right (_, t)) -> t)
  . deserialiseFromBytes decodeTerm
  . (\(Right (_, TTagged _ (TBytes t))) -> fromStrict t)
  . deserialiseFromBytes decodeTerm
  . fromStrict
