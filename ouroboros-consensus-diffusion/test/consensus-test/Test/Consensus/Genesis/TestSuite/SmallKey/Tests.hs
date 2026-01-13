{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
-- NOTE: These tests depend on deferring the type errors that would otherwise
-- prevent some of the instances in this module from being derived.
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Test.Consensus.Genesis.TestSuite.SmallKey.Tests (tests) where

import Control.Exception
import Data.List
  ( isInfixOf
  , permutations
  )
import GHC.Generics
import Test.Consensus.Genesis.TestSuite.SmallKey
import Test.Tasty
import Test.Tasty.HUnit

{-------------------------------------------------------------------------------
  Types for testing SmallKey instances
-------------------------------------------------------------------------------}

data Empty
  deriving stock Generic
  deriving SmallKey via Generically Empty

data Unit = Unit
  deriving stock (Eq, Generic)
  deriving SmallKey via Generically Unit

data Single x = S x
  deriving stock (Eq, Generic)
  deriving SmallKey via Generically (Single x)

data Enumeration = A | B | C
  deriving stock (Eq, Bounded, Enum, Generic)
  deriving SmallKey via Generically Enumeration

data Sum x y = L x | R y
  deriving stock (Eq, Generic)
  deriving SmallKey via Generically (Sum x y)

data Product x y = P x y
  deriving stock (Eq, Generic)
  deriving SmallKey via Generically (Product x y)

data Outer = Inner Bool
  deriving stock (Eq, Generic)
  deriving SmallKey via Generically Outer

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | Unit tests for the correct derivation of some simple ADT 'SmallKey'
-- instances and a test asserting failure for each black-listing strategy:
--
-- 1. A type with a product on its generic representation.
-- 2. A type with a explicitly black-listed field.
--
-- NOTE: These tests depend on @-fdefer-type-errors@ to run.
--
-- TODO: Once the use of 'TypeError' is replaced with 'Unsatisfiable' in
-- 'SmallKey' it should be possible to 'assertError' the /actual/ type error
-- instead of the /unreacheable/ runtime error from 'getAllKeys', and write a
-- test case for the forbidden directly recursive types, e.g.
--
-- @
-- data DirectlyRecursive = Nil | More DirectlyRecursive
--   deriving stock (Eq, Ord, Generic)
--   deriving SmallKey via Generically DirectlyRecursive
-- @
--
-- for which the corresponding test would not terminate trying to evaluate
-- 'getAllKeys' otherwise.
-- See TODO [BlackList].
tests :: TestTree
tests =
  testGroup
    "SmallKey"
    [ testGroup
        "generic instantiation of allowed types"
        [ testCase "Empty" $
            assertBool "must return the empty list" $
              getAllKeys @Empty == []
        , testCase "Unit" $
            assertBool "must return a list with its only constructor" $
              getAllKeys @Unit == [Unit]
        , testCase "Single constructor" $
            assertBool "must return a list with its only constructor" $
              getAllKeys @(Single ()) == [S ()]
        , testCase "Enumeration" $
            assertBool "allKeys must be a permutation of the list of all values" $
              elem (getAllKeys @Enumeration) $
                permutations [minBound .. maxBound]
        , testCase "Sum" $
            assertBool "allKeys must be a permutation of the list of all values" $
              elem (getAllKeys @(Sum () Bool)) $
                permutations [L (), R False, R True]
        , testCase "Nested key" $
            assertBool "allKeys must be a permutation of the list of all values" $
              elem (getAllKeys @Outer) $
                permutations [Inner False, Inner True]
        ]
    , testGroup
        "black-listed types"
        [ testCase "Product" $
            assertError "unreachable: product type" $
              getAllKeys @(Product () ())
        , testCase "Unary type with black-listed argument (Int)" $
            assertError "unreachable: NoSmallKey Int" $
              getAllKeys @(Single Int)
        ]
    ]

assertError :: String -> a -> Assertion
assertError expected val = do
  result <- try @ErrorCall (evaluate val)
  case result of
    Left (ErrorCall msg)
      | expected `isInfixOf` msg -> pure ()
      | otherwise -> assertFailure $ "Unexpected error message:\n" <> msg
    Right _ ->
      assertFailure "Expected an error, but computation succeeded"
