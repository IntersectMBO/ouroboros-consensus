{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Consensus.Protocol (tests_chainOrder) where

import           Data.Proxy
import           Data.Typeable (Typeable, typeRep)
import           Ouroboros.Consensus.Protocol.Abstract
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.QuickCheck

-- | Test the laws of the 'ChainOrder' class (in particular, that 'Ord' is
-- lawful) /except/ for the high-level "Chain extension precedence" property.
tests_chainOrder ::
     forall a.
     ( ChainOrder a
     , Typeable a
     , Arbitrary a
     , Show a
     , Arbitrary (ChainOrderConfig a)
     , Show (ChainOrderConfig a)
     )
  => Proxy a
  -> TestTree
tests_chainOrder aPrx = testGroup ("ChainOrder " <> show (typeRep aPrx))
    [ testProperty "Eq & Ord" (prop_lawfulEqAndTotalOrd @a)
    , testProperty "Consistency with Ord" $ \cfg (a :: a) b ->
        preferCandidate cfg a b ==> a `lt` b
    ]
