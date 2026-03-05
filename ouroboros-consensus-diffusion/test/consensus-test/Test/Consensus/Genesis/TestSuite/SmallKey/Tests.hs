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
-- NOTE: This tests defer the type errors that would other wise prevent
-- the following 'allKeys' instances from being called (or defined).
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Test.Consensus.Genesis.TestSuite.SmallKey.Tests (tests) where

import Control.Exception
import Data.List (isInfixOf, permutations)
import GHC.Generics
import Test.Consensus.Genesis.TestSuite.SmallKey
import Test.Tasty
import Test.Tasty.HUnit

data UnitSumType = L () | R ()
  deriving stock (Eq, Ord, Generic)
  deriving SmallKey via Generically UnitSumType

data UnitProductType = P () ()
  deriving stock (Eq, Ord, Generic)
  deriving SmallKey via Generically UnitProductType

data IntUnaryType = U Int
  deriving stock (Eq, Ord, Generic)
  deriving SmallKey via Generically IntUnaryType

-- | Contains a unit test for the correct derivation of a simple instance and
-- one corresponding to each black-listing strategy:
--
-- 1. A type with a product on its generic representation.
-- 2. A type with a explicitly black-listed field.
--
-- NOTE: These tests depend on @-fdefer-type-errors@ to run.
--
-- TODO: Once the use of 'TypeError' changes to 'Unsatisfiable' in 'SmallKey' it
-- should be possible to 'assertError' the actual type error, and even
-- test a failing instance for
--
-- @
-- data SimpleInfiniteType = Nil | More SimpleInfiniteType
--   deriving stock (Eq, Ord, Generic)
--   deriving SmallKey via Generically SimpleInfiniteType
-- @
--
-- as we could avoid the evaluation of `allKeys` altogether.
-- See TODO [BlackList].
tests :: TestTree
tests =
  testGroup
    "SmallKey"
    [ testCase "A minimal sum type instance" $
        assertBool "allKeys must be a permutation of the list of all values" $
          elem (allKeys @UnitSumType) $
            permutations [L (), R ()]
    , testCase "A minimal product type instance is forbidden" $
        assertError "unreachable: product type" $
          allKeys @UnitProductType
    , testCase "A minimal unary type instance with black-listed Int argument is forbidden" $
        assertError "unreachable: NoSmallKey Int" $
          allKeys @IntUnaryType
    ]

assertError :: String -> a -> Assertion
assertError expected val = do
  result <- try @ErrorCall (evaluate val)
  case result of
    Left (ErrorCall msg)
      | expected `isInfixOf` msg -> pure ()
      | otherwise -> assertFailure $ "Unexpected error message:\n" <> msg
    Right _ ->
      assertFailure "Expected a type error, but computation succeeded"
