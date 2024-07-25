module Main (main) where

import           Image.LaTeX.Render ()

main :: IO ()
main = do
    putStrLn "This test-suite exists only to add dependencies"
    putStrLn "To run doctests: "
    putStrLn "    cabal build all --enable-tests"
    putStrLn "    cabal-docspec"
