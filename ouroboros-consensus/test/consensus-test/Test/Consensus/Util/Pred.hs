{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for self-explaning boolean predicates
module Test.Consensus.Util.Pred (tests) where

import Ouroboros.Consensus.Util.Pred
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Property
  , forAll
  , frequency
  , sized
  , tabulate
  , testProperty
  , (===)
  )
import Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  testGroup
    "Pred"
    [ evidenceUnitTests
    , explanationUnitTests
    , propertyTests
    ]

{-------------------------------------------------------------------------------
  Unit tests
-------------------------------------------------------------------------------}

-- Setup

data Prop = P | Q | R
  deriving stock (Show, Eq)
  deriving Explainable via ShowExplain Prop

p :: Pred Prop -- ~ True
p = P := Bool True

q :: Pred Prop -- ~ False
q = Q := (5 :<=: (3 :: Int))

r :: Pred Prop -- ~ True
r = R := Not (4 :<=: (2 :: Int))

proves :: Pred Prop -> Pred Prop -> TestTree
evidence `proves` predicate =
  testCase
    (explainDeep evidence <> " ⊢ " <> explainShallow predicate <> " => ⊤")
    ( evalPred predicate $ \case
        ETrue evidence' ->
          evidence' @?= evidence
        EFalse evidence' ->
          assertFailure $ "Expected ETrue, but got EFalse: " <> show evidence'
    )

infix 1 `proves`

refutes :: Pred Prop -> Pred Prop -> TestTree
evidence `refutes` predicate =
  testCase
    (explainDeep evidence <> " ⊢ " <> explainShallow predicate)
    ( evalPred predicate $ \case
        EFalse evidence' ->
          evidence' @?= evidence
        ETrue evidence' ->
          assertFailure $ "Expected EFalse, but got ETrue: " <> show evidence'
    )

infix 1 `refutes`

eq_explain :: (Show a, Explainable a) => ExplanationMode -> a -> a -> TestTree
eq_explain mode x y =
  testCase
    ( "explain "
        <> show mode
        <> " "
        <> show x
        <> " == explain "
        <> show mode
        <> " "
        <> show y
    )
    $ assertEqual
      "Expected equal explanations:"
      (explain mode x)
      (explain mode y)

neq_explain :: (Show a, Explainable a) => ExplanationMode -> a -> a -> TestTree
neq_explain mode x y =
  testCase
    ( "explain "
        <> show mode
        <> " "
        <> show x
        <> " /= explain "
        <> show mode
        <> " "
        <> show y
    )
    $ assertNotEqual
      "Expected different explanations:"
      (explain mode x)
      (explain mode y)

-- Surprising this is not already in 'tasty-hunit'
assertNotEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertNotEqual preface expected actual
  | actual /= expected = return ()
  | otherwise = assertFailure msg
 where
  msg =
    (if null preface then "" else preface <> "\n")
      <> "\n but got: "
      <> show actual

-- | Basic tests ensuring that predicates produce the right evidence
evidenceUnitTests :: TestTree
evidenceUnitTests =
  testGroup
    "evidence unit tests"
    [ p `proves` p
    , q `refutes` q
    , r `proves` r
    , Not p `refutes` Not p
    , Not q `proves` Not q
    , Not r `refutes` Not r
    , p :/\: r `proves` p :/\: r
    , q `refutes` p :/\: q
    , q `refutes` p :/\: q :/\: r
    , p `proves` p :\/: q
    , p `proves` q :\/: p
    , p :/\: r `proves` (p :\/: q) :/\: (q :\/: r)
    ]

-- Basic explanation rendering tests.
--
-- NOTE: we can't easily test equality/inequality of explanations without
-- turning these test into an annoying change detector (see [1]) but we
-- still _can_ test some specific case regarding operator precedence and
-- distributivity that should always hold regardless of implementation.
--
-- [1]: https://testing.googleblog.com/2015/01/testing-on-toilet-change-detector-tests.html
explanationUnitTests :: TestTree
explanationUnitTests =
  testGroup
    "explain"
    $ concat
      [ [ eq_explain mode (p :/\: q :\/: r) ((p :/\: q) :\/: r)
        , eq_explain mode (p :\/: q :/\: r) (p :\/: (q :/\: r))
        , neq_explain mode (p :/\: q :\/: r) (p :/\: (q :\/: r))
        , neq_explain mode (p :\/: q :/\: r) ((p :\/: q) :/\: r)
        , neq_explain mode (Not (p :/\: q)) (Not p :/\: q)
        , neq_explain mode (Not (p :\/: q)) (Not p :\/: q)
        , neq_explain mode (P := Bool True :/\: Bool False) ((P := Bool True) :/\: Bool False)
        , neq_explain mode (P := Bool True :\/: Bool False) ((P := Bool True) :\/: Bool False)
        ]
      | mode <- [Shallow, Deep]
      ]

{-------------------------------------------------------------------------------
  QuickCheck properties
-------------------------------------------------------------------------------}

propertyTests :: TestTree
propertyTests =
  adjustQuickCheckTests (* 100) $
    testGroup
      "property tests"
      [ testProperty "prop_evalPred" prop_evalPred
      ]

genPred :: Gen (Pred ())
genPred = sized go
 where
  go 0 =
    Bool <$> arbitrary
  go n =
    frequency
      [ (n, (:=) <$> pure () <*> go (n - 1))
      , (1, Bool <$> arbitrary)
      , (n, Not <$> go (n - 1))
      , (1, (:>:) <$> arbitrary @Int <*> arbitrary @Int)
      , (1, (:<=:) <$> arbitrary @Int <*> arbitrary @Int)
      , (1, (:==:) <$> arbitrary @Int <*> arbitrary @Int)
      , (n, (:/\:) <$> go (n `div` 2) <*> go (n `div` 2))
      , (n, (:/\:) <$> go (n `div` 2) <*> go (n `div` 2))
      ]

predSize :: Pred a -> Int
predSize = \case
  _ := a -> 1 + predSize a
  Bool _ -> 1
  Not a -> 1 + predSize a
  _ :>: _ -> 1
  _ :<=: _ -> 1
  _ :==: _ -> 1
  a :/\: b -> 1 + predSize a + predSize b
  a :\/: b -> 1 + predSize a + predSize b

-- | The overall judgment of 'evalPred' conforms to a trivial evaluator.
prop_evalPred :: Property
prop_evalPred =
  forAll genPred $ \predicate -> do
    let expected = evalDirect predicate
    let actual = evalPred predicate forgetEvidence
    tabulateSize predicate $
      expected === actual
 where
  evalDirect :: Pred () -> Bool
  evalDirect = \case
    _ := a -> evalDirect a
    Bool b -> b
    Not a -> not (evalDirect a)
    a :>: b -> a > b
    a :<=: b -> a <= b
    a :==: b -> a == b
    a :/\: b -> evalDirect a && evalDirect b
    a :\/: b -> evalDirect a || evalDirect b

  tabulateSize :: Pred a -> Property -> Property
  tabulateSize predicate = do
    let bucket = predSize predicate `div` 10
    tabulate
      "Predicate size"
      [show bucket <> "0-" <> show (bucket + 1) <> "0"]
