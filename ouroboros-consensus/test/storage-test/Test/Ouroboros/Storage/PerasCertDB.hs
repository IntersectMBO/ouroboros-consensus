{-# LANGUAGE CPP #-}

module Test.Ouroboros.Storage.PerasCertDB (tests) where

import qualified Test.Ouroboros.Storage.PerasCertDB.StateMachine as StateMachine
import Test.Tasty (TestTree, testGroup)

--
-- The list of all PerasCertDB tests
--

tests :: TestTree
tests =
  testGroup
    "PerasCertDB"
    [ StateMachine.tests
    ]
