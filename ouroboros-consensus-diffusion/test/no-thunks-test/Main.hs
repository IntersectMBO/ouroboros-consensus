{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Main (main, tests) where

import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit
import Ouroboros.Consensus.Util.MonadSTM.NormalForm
import Test.Util.TestEnv (defaultMainWithTestEnv, defaultTestEnvConfig)
import Control.Monad.IOSim
import NoThunks.Class

main :: IO ()
main = do
  putStrLn "Make sure optimizations are turned off!"
  defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests = testGroup "no-thunks-test"
  [ testGroup "updateMVarTest"
      [ testCase "IO-Int" $
          updateMVarTest (TestArgs (5 :: Int) (\v -> (v + 1, "10")) "10")
      , testCase "IOSim-Int" $
          runSimTest $ updateMVarTest (TestArgs (5 :: Int) (\v -> (v + 1, "10")) "10")
      , testCase "IO-String" $
          updateMVarTest (TestArgs "test" (\v -> (v <> "ok", "string")) "string")
      , testCase "IOSim-String" $
          runSimTest $ updateMVarTest (TestArgs "test" (\v -> (v <> "ok", "string")) "string")
      ]
  , testGroup "updateMVarTestI"
      [ testCase "IO-Int" updateMVarTestI
      , testCase "IOSim-Int" (runSimTest updateMVarTestI)
      ]
  , testGroup "updateMVarTestII"
      [ testCase "IO-Int" updateMVarTestII
      , testCase "IOSim-Int" (runSimTest updateMVarTestII)
      ]
  ]

runSimTest :: (forall s . IOSim s ()) -> IO ()
runSimTest act =
  case runSim act of
    Left err -> error (show err)
    Right () -> pure ()

data TestArgs a b =
  TestArgs { initial :: a
           , func :: a -> (a, b)
           , expected :: b
           }

updateMVarTest :: (MonadSTM m, NoThunks a, Show b, Eq b)
               => TestArgs a b -> m ()
updateMVarTest TestArgs{..} = do
  mvar <- newMVar initial
  updated <- updateMVar mvar func
  if updated == expected
    then
      pure ()
    else
      error $ "failed assertion, got " <> show updated <> ", expected " <> show expected

updateMVarTestI :: MonadSTM m => m ()
updateMVarTestI = do
  mvar <- newMVar (5 :: Int)
  updated <- updateMVar mvar $ \v -> (v + 1, "5")
  if updated == "5"
    then
      pure ()
    else
      error $ "failed assertion, got " <> show updated <> ", expected 5"

updateMVarTestII :: MonadSTM m => m ()
updateMVarTestII = do
  mvar <- newMVar (5 :: Int)
  void $ updateMVar mvar $ \v -> (v + 1, "5")
