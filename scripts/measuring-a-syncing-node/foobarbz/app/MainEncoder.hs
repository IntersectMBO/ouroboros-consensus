{-# LANGUAGE LambdaCase #-}

module Main where

import           FooBarBz.Encoder (encode)
import           System.Environment (getArgs)

main :: IO ()
main = getArgs >>= go

go :: [String] -> IO ()
go = \case

    []  -> pure ()
    [_] -> fail "even number of arguments"

    name : path : xs -> do
        putStr "        \""
        putStr name
        putStr "\": \""
        readFile path >>= encode . map read . words
        putStrLn "\","
        go xs
