{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -funbox-small-strict-fields #-}

module FooBarBz.Outliers where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq

data XY a = XY !Double !Double a

newtype HalfWidth = HalfWidth Double

-- | An inclusive lower and upper bound on input stream indexes.
--
-- Non-descending.
data II = II !Int !Int

-- | Maps each 'XY' to the greatest 'II' that is does not extend outside of the
-- closed interval @[-hw+x,x+hw]@.
--
-- Length preserving. And so stutters if two 'XY' map to the same 'II'.
neighbors :: HalfWidth -> [XY a] -> [II]
neighbors (HalfWidth hw) = \case
    []     -> []
    xy:xys -> go zero zero zero xy xys xy xys xy xys
  where
    zero = 0 :: Int

    go !i !j !k l ls m ms r rs =
        case (stepL, stepR) of
          (Just (l', ls'), Nothing       ) -> go (i+1) j  k    l' ls' m ms r  rs
          (Just (l', ls'), Just (r', rs')) -> go (i+1) j (k+1) l' ls' m ms r' rs'
          (Nothing       , Just (r', rs')) -> go  i    j (k+1) l  ls  m ms r' rs'
          (Nothing       , Nothing       ) ->
              (II i k :)
            $ case ms of
                  []     -> []
                  m':ms' -> go i (j+1) k l ls m' ms' r rs
      where
        prj (XY x _ _) = x

        stepL =
            if prj m <= prj l + abs hw then Nothing else
            case ls of
                l':ls' -> Just (l', ls')
                []     ->
                    -- at the very least, i could be j
                    error "unsorted input!"

        stepR = case rs of
            r':rs' | prj r' < prj m + abs hw -> Just (r', rs')
            _                                -> Nothing

-----

-- | A proportion, within the closed interval @[0,1]@
newtype P = P Double

-- | The y coordinate and index of a 'XY', lexicographically
data Nudged = Nudged !Double !Int
  deriving (Eq, Ord)

data Buffer a = Buffer !(Seq.Seq Nudged) !(Map.Map Nudged (XY a))

ins :: Int -> XY a -> Buffer a -> Buffer a
ins i xy@(XY _ y _) (Buffer keys m) =
    Buffer
        (keys Seq.:|> key)
        (Map.insert key xy m)
  where
    key = Nudged y i

del :: Buffer a -> Buffer a
del (Buffer keys m) = case keys of
    Seq.Empty         -> error "impossible!"
    key Seq.:<| keys' -> Buffer keys' (Map.delete key m)

sortedNeighbors :: HalfWidth -> [XY a] -> [Map.Map Nudged (XY a)]
sortedNeighbors (HalfWidth hw) = \case
    []     -> []
    xy:xys ->
        go
            zero zero xy xys xy xys
            (ins zero xy (Buffer Seq.empty Map.empty))
            (neighbors (HalfWidth hw) (xy:xys))
  where
    zero = 0 :: Int

    go i j l ls r rs buf = \case
        []                -> []
        ii@(II lo hi):iis -> case (keepUp lo i ls, keepUp hi j rs) of
          (Just (l', ls'), Nothing       ) -> go (i+1)  j    l' ls' r  rs  (del buf)                (ii:iis)
          (Just (l', ls'), Just (r', rs')) -> go (i+1) (j+1) l' ls' r' rs' (ins (j+1) r' $ del buf) (ii:iis)
          (Nothing       , Just (r', rs')) -> go  i    (j+1) l  ls  r' rs' (ins (j+1) r' buf)       (ii:iis)
          (Nothing       , Nothing       ) ->
              (let Buffer _keys m = buf in m)
            : go i j l ls r rs buf iis

keepUp :: Int -> Int -> [x] -> Maybe (x, [x])
keepUp target current = case compare target current of
    LT -> error "impossible!"
    EQ -> const Nothing
    GT -> \case
        x:xs -> Just (x, xs)
        []   -> error "impossible!"

-----

-- | The least 'XY' value within the 'neighbors' that is greater than or equal
-- to @p@ of the rest according to 'Nudged' order.
quantile :: P -> Map.Map Nudged (XY a) -> XY a
quantile (P p) m =
    snd $ Map.elemAt k m
  where
    n = Map.size m
    k = max 0 $ min (n - 1) $ ceiling $ p * fromIntegral (n - 1)

-----

outliers :: HalfWidth -> [XY a] -> [XY a]
outliers hw xys =
    [ xy
    | (xy, m) <- xys `zip` sortedNeighbors hw xys
    , let q95 = quantile (P 0.95) m
    , let q90 = quantile (P 0.90) m
    , let iqr = prj q95 - prj q90
    , prj q90 + 22 * iqr < prj xy
    ]
  where
    prj (XY _ y _) = y
