{-# LANGUAGE LambdaCase #-}

module Main where

import           FooBarBz.Outliers

main :: IO ()
main = do
    s <- getContents
    id
      $ mapM_ pp
      $ outliers (HalfWidth (3600 * 12 * 2))
      $ map toXY $ paired $ map read $ words s

paired :: [a] -> [(a, a)]
paired = \case
    x:y:xys -> (x, y) : paired xys
    [_]     -> error "not paired!"
    []      -> []

toXY :: (Int, Int) -> XY (Int, Int)
toXY (x, y) =
    XY (fromIntegral t) (fromIntegral y) (x, y)
  where
    t = if x < 4492800 then x*20 else 4492800*20 + (x - 4492800)

pp :: XY (Int, Int) -> IO ()
pp (XY _ _ (x, y)) = putStrLn $ show x <> " " <> show y

pp2 :: II -> IO ()
pp2 (II lo hi) = putStrLn $ show (hi - lo + 1)
