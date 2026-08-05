module Main (main) where

import Test.LeiosTxCache.Optimized.MutableHashTable (tests)
import Test.Tasty (defaultMain)

-- | Runs the hash-table property suite against a copy of
-- "LeiosTxCache.Optimized.MutableHashTable" recompiled with
-- @-fcheck-prim-bounds@ (see the @leios-txcache-bounds-checked@ cabal stanza), so
-- an out-of-bounds 'Data.Primitive.MutableByteArray' access fails loudly rather
-- than silently corrupting memory.
main :: IO ()
main = defaultMain tests
