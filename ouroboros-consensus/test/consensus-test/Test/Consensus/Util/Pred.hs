{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
    [ testGroup
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

test_evalPred :: String -> Pred Prop -> Either (Evidence Prop) (Evidence Prop) -> TestTree
test_evalPred name predicate expected =
  testCase name $
    case evalPred predicate of
      Left ce -> Left ce @?= expected
      Right w -> Right w @?= expected

proves :: Pred Prop -> Evidence Prop -> TestTree
evidence `proves` predicate =
  test_evalPred
    (explainDeep evidence <> " ⊢ " <> explainShallow predicate <> " => ⊤")
    predicate
    (Right evidence)

infix 1 `proves`

refutes :: Pred Prop -> Evidence Prop -> TestTree
evidence `refutes` predicate =
  test_evalPred
    (explainDeep evidence <> " ⊢ " <> explainShallow predicate <> " => ⊥")
    predicate
    (Left evidence)

infix 1 `refutes`
