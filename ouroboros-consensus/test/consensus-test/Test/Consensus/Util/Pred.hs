{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | Tests for self-explaning boolean predcates
module Test.Consensus.Util.Pred (tests) where

import Ouroboros.Consensus.Util.Pred
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Pred"
    [ -- Basic tests ensuring that predicates produce the right evidence.
      testGroup
        "evalPred"
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
    , -- Basic explanation rendering tests.
      --
      -- NOTE: we can't easily test equality/inequality of explanations without
      -- turning these test into an annoying change detector (see [1]) but we
      -- still _can_ test some specific case regarding operator precedence and
      -- distributivity that should always hold regardless of implementation.
      --
      -- [1]: https://testing.googleblog.com/2015/01/testing-on-toilet-change-detector-tests.html
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
    ]

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

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
