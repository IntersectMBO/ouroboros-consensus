{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | QuickCheck utilities
module Test.Util.QuickCheck
  ( -- * Generic QuickCheck utilities
    checkGenerator
  , checkInvariant
  , checkShrinker

    -- * Comparison functions
  , expectRight
  , ge
  , gt
  , le
  , lt
  , strictlyIncreasing

    -- * Gen variants that allow transformers
  , frequency'
  , oneof'

    -- * Comparing maps
  , isSubmapOfBy

    -- * Improved variants
  , (=:=)

    -- * SOP
  , cshrinkNP
  , shrinkNP

    -- * Convenience
  , collects
  , forAllGenRunShrinkCheck
  , implies

    -- * Typeclass laws
  , prop_lawfulEqAndTotalOrd
  ) where

import Control.Monad.Except (Except, runExcept)
import Control.Monad.Trans (MonadTrans (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.SOP.Constraint
import Data.SOP.Strict
import Ouroboros.Consensus.Util (repeatedly)
import Ouroboros.Consensus.Util.Condense (Condense, condense)
import Test.QuickCheck

{-------------------------------------------------------------------------------
  Generic QuickCheck utilities
-------------------------------------------------------------------------------}

-- | Test the generator
--
-- Uses explicit 'forAll' as we don't want to assume a correct shrinker.
checkGenerator :: (Arbitrary a, Show a) => (a -> Property) -> Property
checkGenerator p = forAll arbitrary $ p

-- | Test the shrinker
checkShrinker :: forall a. (Arbitrary a, Show a) => (a -> Property) -> Property
checkShrinker p =
  -- Starting point, some arbitrary value
  -- Explicit 'forAll': don't shrink when testing the shrinker
  forAll arbitrary go
 where
  go :: a -> Property
  go a =
    if null (shrink a)
      then
        property True
      else
        -- Nested 'forAll': testing that /all/ shrunk values satisfy the
        -- property is too expensive. Since we're not shrinking, nesting
        -- 'forAll' is ok.
        forAll (elements (shrink a)) $ \a' -> p a' .&&. go a'

-- | Check invariant
checkInvariant :: (a -> Except String ()) -> (a -> Property)
checkInvariant f = expectRight () . runExcept . f

-- | Explicit quantification using the “gen-run-shrink-check” pattern.
--
-- Instead of the usual two stages where one generates an input and then checks
-- the property for that input, we rely on three stages: one generates an input,
-- then transforms it into an output, and then checks the output.
--
-- When adding a shrinker to the mix, we can allow it to inspect the output
-- value as well, which increases its expressivity. This makes sense if the
-- “run” phase is particularly expensive.
forAllGenRunShrinkCheck ::
  Testable prop =>
  Gen input ->
  (input -> output) ->
  (input -> output -> [input]) ->
  (input -> output -> prop) ->
  Property
forAllGenRunShrinkCheck gen run shrink_ check =
  forAllBlind gen $ \input ->
    shrinking
      (map run' . uncurry shrink_)
      (run' input)
      (uncurry check)
 where
  run' inp = (inp, run inp)

{-------------------------------------------------------------------------------
  Comparison functions
-------------------------------------------------------------------------------}

infix 4 `lt`
infix 4 `le`
infix 4 `gt`
infix 4 `ge`

-- | Like '<', but prints a counterexample when it fails.
lt :: (Ord a, Show a) => a -> a -> Property
x `lt` y = counterexample (show x ++ " >= " ++ show y) $ x < y

-- | Like '<=', but prints a counterexample when it fails.
le :: (Ord a, Show a) => a -> a -> Property
x `le` y = counterexample (show x ++ " > " ++ show y) $ x <= y

-- | Like '>', but prints a counterexample when it fails.
gt :: (Ord a, Show a) => a -> a -> Property
x `gt` y = counterexample (show x ++ " <= " ++ show y) $ x > y

-- | Like '>=', but prints a counterexample when it fails.
ge :: (Ord a, Show a) => a -> a -> Property
x `ge` y = counterexample (show x ++ " < " ++ show y) $ x >= y

strictlyIncreasing :: forall a. (Show a, Ord a) => [a] -> Property
strictlyIncreasing xs =
  counterexample (show xs) $ go xs
 where
  go :: [a] -> Property
  go [] = property True
  go [_] = property True
  go (x : y : zs) = x `lt` y .&&. go (y : zs)

-- | Check that we have the expected 'Right' value
--
-- @expectRight b ab@ is roughly equivalent to @Right b === ab@, but avoids an
-- equality constraint on @a@.
expectRight :: (Show a, Show b, Eq b) => b -> Either a b -> Property
expectRight b (Right b') = b === b'
expectRight _ (Left a) =
  counterexample ("Unexpected left " ++ show a) $
    False

{-------------------------------------------------------------------------------
  Comparing maps
-------------------------------------------------------------------------------}

isSubmapOfBy ::
  (Ord k, Show k, Show a, Show b) =>
  (a -> b -> Property) -> Map k a -> Map k b -> Property
isSubmapOfBy p l r =
  conjoin
    [ case Map.lookup k r of
        Nothing ->
          counterexample
            ( "key "
                ++ show k
                ++ " with value "
                ++ show a
                ++ " not present in other map"
            )
            $ property False
        Just b ->
          counterexample
            ( "key "
                ++ show k
                ++ " with values "
                ++ show a
                ++ " and "
                ++ show b
                ++ " doesn't satisfy the property"
            )
            $ p a b
    | (k, a) <- Map.toList l
    ]

{-------------------------------------------------------------------------------
  Improved variants
-------------------------------------------------------------------------------}

-- | Like '===', but uses 'Condense' instead of 'Show' when it fails.
infix 4 =:=

(=:=) :: (Eq a, Condense a) => a -> a -> Property
x =:= y =
  counterexample (condense x ++ interpret res ++ condense y) res
 where
  res = x == y
  interpret True = " == "
  interpret False = " /= "

{-------------------------------------------------------------------------------
  SOP
-------------------------------------------------------------------------------}

cshrinkNP ::
  forall proxy c f g xs.
  All c xs =>
  proxy c ->
  (forall a. c a => f a -> g a) -> -- For elements we don't shrink
  (forall a. c a => f a -> [g a]) ->
  NP f xs ->
  [NP g xs]
cshrinkNP p g f = go
 where
  go :: All c xs' => NP f xs' -> [NP g xs']
  go Nil = [] -- Can't shrink the empty list
  go (x :* xs) =
    concat
      [ -- Shrink the head of the list
        [x' :* hcmap p g xs | x' <- f x]
      , -- Or shrink the tail of the list
        [g x :* xs' | xs' <- go xs]
      ]

shrinkNP ::
  (forall a. f a -> g a) -> -- For elements we don't shrink
  (forall a. f a -> [g a]) ->
  NP f xs ->
  [NP g xs]
shrinkNP g f np = npToSListI np $ cshrinkNP (Proxy @Top) g f np

{-------------------------------------------------------------------------------
  Convenience
-------------------------------------------------------------------------------}

collects :: Show a => [a] -> Property -> Property
collects = repeatedly collect

-- | QuickCheck's '==>' 'discard's the test if @p1@ fails; this is sometimes not
-- what we want, for example if we have other properties that do not depend on
-- @p1@ being true.
implies :: Testable prop => Bool -> prop -> Property
implies p1 p2 = not p1 .||. p2

infixr 0 `implies`

{-------------------------------------------------------------------------------
  Typeclass laws
-------------------------------------------------------------------------------}

prop_lawfulEqAndTotalOrd ::
  forall a.
  (Show a, Ord a) =>
  a -> a -> a -> Property
prop_lawfulEqAndTotalOrd a b c =
  conjoin
    [ counterexample "Not total: a <= b || b <= a VIOLATED" $
        a <= b || b <= a
    , counterexample "Not transitive: a <= b && b <= c => a <= c VIOLATED" $
        let antecedent = a <= b && b <= c
         in classify antecedent "Antecedent for transitivity" $
              antecedent `implies` a <= c
    , counterexample "Not reflexive: a <= a VIOLATED" $
        a `le` a
    , counterexample "Not antisymmetric: a <= b && b <= a => a == b VIOLATED" $
        let antecedent = a <= b && b <= a
         in classify antecedent "Antecedent for antisymmetry" $
              antecedent `implies` a == b
    , -- compatibility laws
      counterexample "(a <= b) == (b >= a) VIOLATED" $
        (a <= b) === (b >= a)
    , counterexample "(a < b) == (a <= b && a /= b) VIOLATED" $
        (a < b) === (a <= b && a /= b)
    , counterexample "(a > b) = (b < a) VIOLATED" $
        (a > b) === (b < a)
    , counterexample "(a < b) == (compare a b == LT) VIOLATED" $
        (a < b) === (compare a b == LT)
    , counterexample "(a > b) == (compare a b == GT) VIOLATED" $
        (a > b) === (compare a b == GT)
    , counterexample "(a == b) == (compare a b == EQ) VIOLATED" $
        (a == b) === (compare a b == EQ)
    , counterexample "min a b == if a <= b then a else b VIOLATED" $
        min a b === if a <= b then a else b
    , counterexample "max a b == if a >= b then a else b VIOLATED" $
        max a b === if a >= b then a else b
    ]

{-------------------------------------------------------------------------------
  Generator variants that allow for transformers
-------------------------------------------------------------------------------}

-- | Variant of 'frequency' that allows for transformers of 'Gen'
frequency' :: (MonadTrans t, Monad (t Gen)) => [(Int, t Gen a)] -> t Gen a
frequency' [] = error "frequency' used with empty list"
frequency' xs0 = lift (choose (1, tot)) >>= (`pick` xs0)
 where
  tot = sum (map fst xs0)

  pick n ((k, x) : xs)
    | n <= k = x
    | otherwise = pick (n - k) xs
  pick _ _ = error "pick used with empty list"

oneof' :: (MonadTrans t, Monad (t Gen)) => [t Gen a] -> t Gen a
oneof' [] = error "QuickCheck.oneof used with empty list"
oneof' gs = lift (chooseInt (0, length gs - 1)) >>= (gs !!)
