module Main (main) where

import qualified Bench.DiffSeq as DS
import           Test.Tasty.Bench

main :: IO ()
main = defaultMain [bgroup "Bench" [
      DS.benchmarks
    ]]
