-- | Total 'EbHash' and 'TxHash' constructors for test fixtures, generators and
-- benchmarks.
--
-- Production builds hashes either by hashing (always 32 bytes) or from
-- untrusted bytes through the fallible 'mkEbHash'/'mkTxHash', so it needs no
-- partial constructor and does not get one. Tests, though, write out the bytes
-- themselves and know the length statically; threading 'Maybe' through every
-- fixture buys nothing there.
module Test.Util.LeiosHash
  ( unsafeEbHashFromBytes
  , unsafeTxHashFromBytes
  , genRbHash
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.Leios.Types (EbHash, RbHash (..), TxHash, mkEbHash, mkTxHash)
import Test.QuickCheck (Gen, vector)

-- | 'mkEbHash' for bytes the caller built itself and knows are 32 long.
unsafeEbHashFromBytes :: HasCallStack => ByteString -> EbHash
unsafeEbHashFromBytes bs =
  fromMaybe (error $ "unsafeEbHashFromBytes: " <> show (BS.length bs) <> " bytes, expected 32") $
    mkEbHash bs

-- | 'mkTxHash' for bytes the caller built itself and knows are 32 long.
unsafeTxHashFromBytes :: HasCallStack => ByteString -> TxHash
unsafeTxHashFromBytes bs =
  fromMaybe (error $ "unsafeTxHashFromBytes: " <> show (BS.length bs) <> " bytes, expected 32") $
    mkTxHash bs

-- | Generate a random RbHash (32 random bytes).
-- With 256 bits of randomness, collisions are practically impossible.
genRbHash :: Gen RbHash
genRbHash = MkRbHash . BS.pack <$> vector 32
