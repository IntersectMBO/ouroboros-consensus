{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
module Test.Consensus.Util.MonadSTM.NormalForm (tests) where

import           Control.Monad.IOSim
import           GHC.Generics
import           NoThunks.Class
import           Ouroboros.Consensus.Util.MonadSTM.NormalForm (MonadSTM,
                     newMVar, updateMVar)
import           Test.Tasty
import           Test.Tasty.QuickCheck

-- Note that all of the tests here are only significant with compiler
-- optimizations turned off! These tests ensure that the invariants are
-- maintained when calling `updateMVar` on consensus' `StrictMVar` values
-- (which are created with a `NoThunks` "this should not contain any unforced
-- thunks" invariant). Because the existence of thunks (and therefore the
-- behaviour of `unsafeNoThunks`) is heavily dependent on compiler
-- optimizations, these tests will *always* pass at -O1 or higher (at least on
-- GHC 8.10 and GHC 9.2).
tests :: TestTree
tests = testGroup "Ouroboros.Consensus.Util.MonadSTM.NormalForm"
  [ testGroup "updateMVar"
    [ testGroup "updateMVar strictness"
      [ testProperty "IO @Integer @String"
          (prop_update_mvar_strictness_io @Integer @String)
      , testProperty "IOSim @Integer @String"
          (prop_update_mvar_strictness_iosim @Integer @String)
      , testProperty "IO @StrictnessTestType @String"
          (prop_update_mvar_strictness_io @StrictnessTestType @String)
      , testProperty "IOSim @StrictnessTestType @String"
          (prop_update_mvar_strictness_iosim @StrictnessTestType @String)
      ]
    ]
  ]

data StrictnessTestType = StrictnessTestType !Int !Bool
  deriving stock (Show, Generic)
  deriving anyclass (Function, NoThunks, CoArbitrary)

instance Arbitrary StrictnessTestType where
  arbitrary = StrictnessTestType <$> arbitrary <*> arbitrary
  shrink (StrictnessTestType a b) = do
    StrictnessTestType <$> shrink a <*> shrink b

prop_update_mvar_strictness_io
  :: forall a b. NoThunks a
  => Fun a (a, b) -> a -> Property
prop_update_mvar_strictness_io f a =
  ioProperty $ updateMVarTest f a

prop_update_mvar_strictness_iosim
  :: forall a b. NoThunks a
  => Fun a (a, b) -> a -> Property
prop_update_mvar_strictness_iosim f a =
  property $ runSimOrThrow $ updateMVarTest f a

updateMVarTest :: (MonadSTM m, NoThunks a) => Fun a (a, b) -> a -> m ()
updateMVarTest (Fun _ f) a = do
  mvar <- newMVar a
  _ <- updateMVar mvar f
  pure ()
