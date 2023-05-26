{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Test.Consensus.Util.MonadSTM.NormalForm (tests) where

import Ouroboros.Consensus.Util.MonadSTM.NormalForm (MonadSTM, newMVar, updateMVar)
import Test.Tasty
import Test.Tasty.QuickCheck
import NoThunks.Class
import Control.Monad.IOSim
import GHC.Generics


-- Note that all of the tests here are only significant with compiler
-- optimizations turned off!
tests :: TestTree
tests = testGroup "Ouroboros.Consensus.Util.MonadSTM.NormalForm"
  [ testGroup "updateMVar"
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
