{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE MultiWayIf     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

{-# OPTIONS_GHC -w   #-}

module Main (main) where

import           Test.Tasty

import qualified Test.Test.QuickCheck.Randomized.Coin as Coin

-----

import           Control.Monad (forM_, guard)
import           Data.Function (fix)
import           Data.Kind (Constraint, Type)
import           Data.List (scanl')
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (isJust)
import           Data.Word (Word64)
import           GHC.Float (castDoubleToWord64, castWord64ToDouble)
import           GHC.Real
import           Numeric.SpecFunctions (logBeta)
import           Numeric.Search.Range (searchFromTo)
import           Statistics.Distribution (ContDistr (..))
import           Statistics.Distribution (DiscreteDistr (..))
import           Statistics.Distribution (Distribution (..))
import           Statistics.Distribution.Beta
import           Statistics.Distribution.Binomial
import qualified Test.QuickCheck as QC

import           Data.Scientific

data BayesianRule = BayesianRule
  { failThreshold :: MaxBound Probability
    -- ^ FAIL if Pr[bias good | h,t] <= this.
  , isGoodBias    :: Either Bias (LeftOpenInterval Bias)
  , isOkBias      :: LeftOpenInterval Bias
  , passThreshold :: MaxBound Probability
    -- ^ PASS if Pr[bias not ok | h,t] <= this.
  }

main5 :: IO ()
main5 = do
    let -- failThreshold = MaxBound 1e-6

--        isGoodBias    = LeftOpenInterval 0.0001 0.001
--        isOkBias      = LeftOpenInterval 0.00001 0.002

--        isGoodBias    = LeftOpenInterval 0.249 0.251
        isGoodBias    = LeftOpenInterval 0.24 0.26
        isOkBias      = LeftOpenInterval 0.20 0.35

--        isGoodBias    = LeftOpenInterval 0.045 0.055
--        isOkBias      = LeftOpenInterval 0.04 0.06
--        isOkBias      = LeftOpenInterval 0.04 0.07

--        passThreshold = MaxBound 1e-3
        
    case (isOkBias, isGoodBias) of
      (,)
        (LeftOpenInterval okLo okHi)
        (LeftOpenInterval goodLo goodHi)
        -> putStrLn $ "intervals" <> show (okLo, okHi, goodLo, goodHi) <> ";"

    let
      get (Posterior h _t) = h
      binom n = do
          let
            useful (x1, x2, x3) = (x1 `max` x2 `max` x3) >= 5e-18
            dat =
                filter (useful . snd) $
                flip map (allPosteriors n) $ \posterior ->
                ( get posterior
                , case isGoodBias of
                      LeftOpenInterval lo hi ->
                          ( likelihood lo posterior
                          , likelihood ((lo + hi) / 2) posterior
                          , likelihood hi posterior
                          )
                )

            suffix = "binom" <> show n
            xtitle = "h, number of heads out of " <> show n <> " tosses"
            title = "Pr[h | bias] for bias in isGood_lo, isGood_mid and isGood_hi"
          putStrLn $
            "go3nn(" <>
            "'plot_" <> suffix <> "'" <>
            "," <> show n <>
            "," <> show (map fst dat) <>
            "," <> show (map ((\(lo, _, _) -> lo) . snd) dat) <>
            "," <> show (map ((\(_, mid, _) -> mid) . snd) dat) <>
            "," <> show (map ((\(_, _, hi) -> hi) . snd) dat) <>
            ",'" <> title <> "'" <>
            ",'" <> xtitle <> "'" <>
            ");"

    binom 100
    binom 1000
    binom 10000
    binom 100000

    let
      get (Posterior h _t) = h
      snapshot n = do
          let
            useful (x1, x2) = (x1 `max` x2) >= 5e-18
            dat =
                filter (useful . snd) $
                flip map (allPosteriors n) $ \posterior ->
                ( get posterior
                , ( intervalProbability posterior isOkBias
                  , intervalProbability posterior isGoodBias
                  )
                )

            suffix = "snapshot" <> show n
            xtitle = "h, number of heads out of " <> show n <> " tosses"
            title = "Pr[isGood | h] and Pr[isOk | h]"
          putStrLn $
            "go2(" <>
            "'plot_" <> suffix <> "'" <>
            "," <> show n <>
            "," <> show (map fst dat) <>
            "," <> show (map (fst . snd) dat) <>
            "," <> show (map (snd . snd) dat) <>
            ",'" <> title <> "'" <>
            ",'" <> xtitle <> "'" <>
            ");"

    snapshot 100000
    snapshot 10000
    snapshot 1000
    snapshot 100

    let
      timeline suffix rat isGood isOk = do
          let n = 10000 :: Count
          bs <- QC.generate $ QC.vectorOf (fromIntegral n) (QC.choose (1, denominator rat :: Count))
          let hs = scanl' (\acc b -> if b <= numerator rat then acc + 1 else acc) (0 :: Count) bs
              posteriors = zipWith (\h i -> Posterior h (i - h)) hs [0 ..]
          let
             title =
                 "Experiment: " <>
                 "bias = " <> show (numerator rat) <> ":" <> show (denominator rat) <> ", " <>
                 "Pr[isGood | h(n), n] and Pr[isOk | h(n), n]"
             xtitle = "n, number of tosses so far"
          let
            useful (x1, x2) = (x1 `max` x2) >= 5e-18
            dat =
                filter (useful . snd) $
                zip [0 .. n] $
                [ ( intervalProbability posterior isOk
                  , intervalProbability posterior isGood
                  )
                | posterior <- posteriors
                ]
          putStrLn $
            "go2(" <>
            "'plot_" <> suffix <> "'" <>
            "," <> show n <>
            "," <> show (map fst dat) <>
            "," <> show (map (fst . snd) dat) <>
            "," <> show (map (snd . snd) dat) <>
            ",'" <> title <> "'" <>
            ",'" <> xtitle <> "'" <>
            ");"

    timeline "exp_isOk_Lo" 0.20 isGoodBias isOkBias
    timeline "exp_isGood_Lo" 0.24 isGoodBias isOkBias
    timeline "exp_isGood_JustAboveLo" 0.245 isGoodBias isOkBias
    timeline "exp_isGood_Middle" 0.25 isGoodBias isOkBias
    timeline "exp_isGood_Hi" 0.26 isGoodBias isOkBias
    timeline "exp_isOk_JustBelowHi" (1 % 3) isGoodBias isOkBias
    timeline "exp_isOk_Hi" 0.35 isGoodBias isOkBias
    timeline "exp_Wrong" 0.40 isGoodBias isOkBias
    timeline "exp_MoreWrong" 0.50 isGoodBias isOkBias

    let
      get (Posterior h _t) = h
      snapshot2 n = do
          let
            useful ((x1, x2), (x3, x4)) = (x1 `max` x2 `max` x3 `max` x4) >= 5e-18
            dat =
                filter (useful . snd) $
                flip map (allPosteriors n) $ \posterior ->
                ( get posterior
                , ( ( intervalProbability posterior isOkBias
                    , intervalProbability posterior isGoodBias
                    )
                  , case isGoodBias of
                      LeftOpenInterval lo hi ->
                          ( likelihood lo posterior
                          , likelihood hi posterior
                          )
                  )
                )

            suffix = "snapshot2" <> show n
            xtitle = "h, number of heads out of " <> show n <> " tosses"
            title = "Pr[h | bias] and Pr[bias | h] for bias a boundary of isGood"
          putStrLn $
            "go4(" <>
            "'plot_" <> suffix <> "'" <>
            "," <> show n <>
            "," <> show (map fst dat) <>
            "," <> show (map (fst . fst . snd) dat) <>
            "," <> show (map (snd . fst . snd) dat) <>
            "," <> show (map (fst . snd . snd) dat) <>
            "," <> show (map (snd . snd . snd) dat) <>
            ",'" <> title <> "'" <>
            ",'" <> xtitle <> "'" <>
            ");"

    snapshot2 10000
    snapshot2 1000
    snapshot2 100
    snapshot2 10

main4 :: IO ()
main4 = do
    let rule = BayesianRule
          { failThreshold = MaxBound 1e-6
--          , isGoodBias    = Right $ LeftOpenInterval 0.24 0.26 -- Left 0.25
          , isGoodBias    = Right $ LeftOpenInterval 0.045 0.055 -- Left 0.25
--          , isOkBias      = LeftOpenInterval 0.20 0.35
          , isOkBias      = LeftOpenInterval 0.04 0.06
          , passThreshold = MaxBound 1e-3
          }
    -- TODO this only finds min inconclusive and max inconclusive
    --
    -- however, there can be two inconclusive intervals; I think we should
    -- track both bounds of both intervals
    let inner 0 = error "impossible"
        inner n =
            case NE.nonEmpty $ filter (inconclusive rule) (allPosteriors n) of
              Nothing -> inner (div n 2)
              Just xs ->
                  (n, ClosedInterval (get $ NE.head xs) (get $ NE.last xs))
          where
            get (Posterior h _t) = h
    let go n (ClosedInterval lo hi) =
            (\m -> do print (n, lo, hi, fromIntegral (hi - lo + 1) / fromIntegral (n + 1) :: Double); m) $
            if not $ inc loI || inc hiI || inc loO || inc hiO then pure (n + 1) else
            go (n+1) $
            ClosedInterval
              (loO `o` loI `o` hiI `o` hiO)
              (hiO `o` hiI `o` loI `o` loO)
          where
            infixr `o`
            o h k = if inc h then h else k
            inc h = inconclusive rule (Posterior h (n + 1 - h))
            hiI = hi
            hiO = hi + 1
            loI = lo + 1
            loO = lo

    uncurry go (inner 1024) >>= print

allPosteriors :: Count -> [Posterior]
allPosteriors n = [ Posterior k (n - k) | k <- [0 .. n ] ]

likelihood :: Bias -> Posterior -> Probability
likelihood bias (Posterior h t) =
    probability
      (binomial (fromIntegral $ h + t) bias)
      (fromIntegral h)

inconclusive :: BayesianRule -> Posterior -> Bool
inconclusive bayesianRule post =
    not $ passB bayesianRule post || failB bayesianRule post

conclusive :: BayesianRule -> Posterior -> Bool
conclusive bayesianRule post = not $ inconclusive bayesianRule post

passB :: BayesianRule -> Posterior -> Bool
passB bayesianRule posterior =
    (1 - intervalProbability posterior isOkBias) `satisfies` passThreshold
  where
    BayesianRule
      { isOkBias
      , passThreshold
      } = bayesianRule

failB :: BayesianRule -> Posterior -> Bool
failB bayesianRule posterior =
    case isGoodBias of
      Left p     ->
          case hpdInterval posterior (complementMax failThreshold) of
            Nothing   -> error "wut"
            Just ival -> not $ p `satisfies` ival
      Right ival ->
          intervalProbability posterior ival `satisfies` failThreshold
  where
    BayesianRule
      { failThreshold
      , isGoodBias
      } = bayesianRule

-----

complementMax :: MaxBound Probability -> MinBound Probability
complementMax (MaxBound p) = MinBound (1 - p)

newtype MaxBound a = MaxBound a

instance Predicate MaxBound where
  type PredicateCon MaxBound = Ord
  satisfies a (MaxBound hi) = a <= hi

-----

main3 :: IO ()
main3 = do
    let
      -- An IEEE 754 number denotes: (-1)s × c × b^q, where b is 2 or 10 and c
      -- has a fixed number of digits as a base b numeral. Since very integer
      -- power of 2 is has a finite number of digits in base 10, so does every
      -- Double.
      sci d       = fromRational $ toRational (d :: Double) :: Scientific
      _nextDouble  = castWord64ToDouble . (\w -> w + 1) . castDoubleToWord64
      prevDouble = castWord64ToDouble . (\w -> w - 1) . castDoubleToWord64

      each rat = do
          let
            x = fromRational rat :: Double
            y = prevDouble x
            diff = sci x - sci y
          print rat
          print (sci x)
          print (sci y)
          print diff
          print (compare diff $ 2^^(-53 :: Int))
          print (compare diff $ 2^^(-54 :: Int))
          putStrLn "--------------------"

    each 1
    each 0.5
{-    each (1/3)
    each (toRational $ nextDouble $ 0.25)
    each 0.25
    each 0.1
    each 0.05
    each 0.01
    each 1e-18
    each (toRational $ nextDouble 0)
-}
    print (2^^(-32 :: Int) :: Double)
    print (2^^(-64 :: Int) :: Double)

main2 :: IO ()
main2 = do
  forM_ [ 2 ^ (8 + n :: Int) | n <- [1..8] ] $ \h -> do
    decision <- try 0.25 (LeftOpenInterval 0.23 0.30) h (h * 3)
    putStr $ "  "
    print decision

data Decision = PASS | FAIL | CONTINUE
  deriving Show

try :: Bias -> LeftOpenInterval Bias -> Count -> Count -> IO Decision
try correct (LeftOpenInterval lo hi) h t = do
    let alpha = -15 :: Int
        beta  = -6 :: Int

    putStrLn $ "Conditional on " <> show h <> " heads and " <> show t <> " tails (" <> show (h+t) <> "):"

    let prGood = intervalProbability (Posterior h t) (LeftOpenInterval lo hi)

    putStr $ "  Pr[" <> show lo <> " < bias <= " <> show hi <> "] = "
    print prGood

    case hpdInterval (Posterior h t) (MinBound $ 1 - 10 ^^ alpha) of
      Nothing
        -> do
          putStrLn $ "  could not compute level 10^ " <> show alpha <> " HPD interval"
          error "oh noes"
      Just hpdIval@(LeftOpenInterval lo' hi')
        -> do
          putStrLn $ "  10^" <> show alpha <> " >= Pr[bias <= " <> show lo' <> " || bias > " <> show hi' <> "] (HPD)"

          pure $ if
            | prGood >= 1 - 10 ^^ beta -> PASS
            | not (correct `satisfies` hpdIval) -> FAIL
            | otherwise -> CONTINUE

-----

-- | The posterior of a flat prior conditional on @h@ heads and @t@ tails.
--
-- The support is the possible 'Bias' of the coin. It includes @0@ iff @h=0@.
-- count is zero. It includes @1@ iff @t=0@.
data Posterior = Posterior Count Count
  deriving (Show)

fromFlatPrior :: Posterior -> BetaDistribution
fromFlatPrior (Posterior h t) =
    betaDistr (1 + fromIntegral h) (1 + fromIntegral t)

instance Distribution Posterior where
  cumulative = cumulative . fromFlatPrior

instance ContDistr Posterior where
  density (Posterior 0 t) 0 = exp $ negate $ logBeta 1 (1 + fromIntegral t)
  density (Posterior h 0) 1 = exp $ negate $ logBeta (1 + fromIntegral h) 1
  density post x            = density (fromFlatPrior post) x
  quantile = quantile . fromFlatPrior

calcMode :: Posterior -> Bias
calcMode (Posterior h t) = case (,) h t of
    (,) 0 0 -> 0.5   -- flat; pick one of the infinite modes
    (,) 0 _ -> 0
    (,) _ 0 -> 1
    (,) _ _ -> fromIntegral h / fromIntegral (h + t)

-----

intervalProbability :: Posterior -> LeftOpenInterval Bias -> Probability
intervalProbability post (LeftOpenInterval lo hi) =
    cumulative post hi - cumulative post lo

-----

hpdInterval :: Posterior -> MinBound Probability -> Maybe (LeftOpenInterval Bias)
hpdInterval post p =
    argmax isoDoubleWord (ClosedInterval 0 maxDensity) $ \height -> do
      ival <- widestIntervalAbove post (MinBound height)
      guard $ intervalProbability post ival `satisfies` p
      pure ival
  where
    mode = calcMode post

    -- Arbitrarily inflated hopefully hiding any minor infelicities in the mode
    -- calculation
    maxDensity :: Density
    maxDensity = density post mode * 16

-- The widest interval of positive width that only includes biases at or above
-- the given density.
widestIntervalAbove :: Posterior -> MinBound Density -> Maybe (LeftOpenInterval Bias)
widestIntervalAbove post height = do
    lo <- argmin isoDoubleWord (ClosedInterval 0 mode) $ guardId $
      \lo -> density post lo `satisfies` height
    hi <- argmax isoDoubleWord (ClosedInterval mode 1) $ guardId $
      \hi -> density post hi `satisfies` height
    guard $ lo /= hi
    pure $ LeftOpenInterval lo hi
  where
    mode = calcMode post

-----

guardId :: (a -> Bool) -> a -> Maybe a
guardId predicate x = do
    guard $ predicate x
    pure x

-- | Find the smallest @a@ in the given interval that satisfies the predicate.
argmin :: Iso a Word64 -> ClosedInterval a -> (a -> Maybe b) -> Maybe b
{-# INLINE argmin #-}
argmin (Iso toW frW) (ClosedInterval lo hi) predicate =
    (>>= inj) $ searchFromTo (isJust . inj) wlo whi
  where
    wlo = toW lo
    whi = toW hi

    inj = predicate . frW

-- | Find the largest @a@ in the given interval that satisfies the predicate.
argmax :: Iso a Word64 -> ClosedInterval a -> (a -> Maybe b) -> Maybe b
{-# INLINE argmax #-}
argmax (Iso toW frW) (ClosedInterval lo hi) predicate =
    (>>= inj) $ searchFromTo (isJust . inj) (inv whi) (inv wlo)
  where
    wlo = toW lo
    whi = toW hi

    inv x = whi - x :: Word64

    inj = predicate . frW . inv

-----

type Count = Word64
type Bias = Double
type Probability = Double
type Density = Double

newtype MinBound a = MinBound a
  deriving (Show)

class Predicate f where
  type PredicateCon f :: Type -> Constraint
  satisfies :: PredicateCon f a => a -> f a -> Bool

instance Predicate MinBound where
  type PredicateCon MinBound = Ord
  satisfies a (MinBound lo) = lo <= a

-- | @(lo, hi]@
data LeftOpenInterval a = LeftOpenInterval a a
  deriving (Show)

instance Predicate LeftOpenInterval where
  type PredicateCon LeftOpenInterval = Ord
  satisfies a (LeftOpenInterval lo hi) = lo < a && a <= hi

-- | @[lo, hi]@
data ClosedInterval a = ClosedInterval a a

instance Predicate ClosedInterval where
  type PredicateCon ClosedInterval = Ord
  satisfies a (ClosedInterval lo hi) = lo <= a && a <= hi

data Iso a b = Iso (a -> b) (b -> a)

-- | A non-obvious, very useful property of IEEE 754: consecutive (mundane?)
-- doubles (with the same sign?) are contiguous when cast as words.
isoDoubleWord :: Iso Double Word64
isoDoubleWord = Iso castDoubleToWord64 castWord64ToDouble

-----

main :: IO ()
main = main5 `asTypeOf` main4 `asTypeOf` main3 `asTypeOf` main2 `asTypeOf` defaultMain tests

tests :: TestTree
tests =
  testGroup "Test.QuickCheck.Randomized"
  [ Coin.tests
  ]
