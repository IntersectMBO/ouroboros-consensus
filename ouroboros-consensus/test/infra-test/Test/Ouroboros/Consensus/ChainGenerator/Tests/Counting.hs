{-# LANGUAGE DataKinds #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.Counting (tests) where

import Data.Proxy (Proxy (Proxy))
import Test.Ouroboros.Consensus.ChainGenerator.Counting qualified as C
import Test.QuickCheck qualified as QC
import Test.Tasty qualified as TT
import Test.Tasty.QuickCheck qualified as TT

-----

tests :: [TT.TestTree]
tests =
  [ TT.testProperty "prop_withWindow" prop_withWindow
  ]

-----

prop_withWindow :: QC.NonNegative Int -> Int -> Int -> QC.Property
prop_withWindow (QC.NonNegative n) i m =
  case C.withWindow (C.Count n) (C.Lbl :: C.Lbl "test") (C.Count i) (C.Count m) of
    C.SomeWindow Proxy (C.Contains (C.Count i') (C.Count m')) ->
      QC.counterexample (show (i', m')) $
        QC.conjoin
          [ if i < 0
              then QC.counterexample "neg i" $ i' QC.=== 0
              else
                if i > n
                  then QC.counterexample "too large i" $ i' QC.=== n
                  else QC.counterexample "nonneg i" $ i' QC.=== i
          , if m < 0
              then QC.counterexample "neg m" $ m' QC.=== 0
              else QC.counterexample "nonneg m" $ min (n - 1) i' + m' <= n
          ]
