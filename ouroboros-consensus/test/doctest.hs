module Main (main) where

main :: IO ()
main = do
  putStrLn "Test Suite Placeholder"
  putStrLn "This test suite only exists to pull in the required dependencies."

  putStrLn "\nTo run doctests:"
  putStrLn "  cabal build all --enable-tests"
  putStrLn "  cabal-docspec"

  putStrLn "\n(No tests actually run from this file.)"
