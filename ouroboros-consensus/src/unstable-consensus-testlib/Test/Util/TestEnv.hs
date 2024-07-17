{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-- | A @tasty@ command-line option for enabling nightly tests
module Test.Util.TestEnv (
    TestEnv (..)
  , adjustQuickCheckMaxSize
  , adjustQuickCheckTests
  , askTestEnv
  , defaultMainWithTestEnv
  , defaultTestEnvConfig
  ) where

import           Cardano.Crypto.Init (cryptoInit)
import           Data.Proxy (Proxy (..))
import           Main.Utf8 (withStdTerminalHandles)
import           Options.Applicative (metavar)
import           Test.Tasty
import           Test.Tasty.Ingredients
import           Test.Tasty.Ingredients.Rerun
import           Test.Tasty.Options
import           Test.Tasty.QuickCheck

-- | 'defaultMain' extended with 'iohkTestEnvIngredient' and setting the
-- terminal handles to UTF-8.
defaultMainWithTestEnv :: TestEnvConfig -> TestTree -> IO ()
defaultMainWithTestEnv testConfig testTree = do
    cryptoInit
    withStdTerminalHandles $
      defaultMainWithIngredients
        [rerunningTests (testEnvIngredient : defaultIngredients)]
        ( withTestEnv testConfig testTree )
    where
      testEnvIngredient :: Ingredient
      testEnvIngredient = includingOptions [Option (Proxy :: Proxy TestEnv)]

-- | Set the appropriate options for the test environment
withTestEnv :: TestEnvConfig -> TestTree -> TestTree
withTestEnv TestEnvConfig{..} testTree = askOption $ \case
      Nightly -> localOption (QuickCheckTests nightly) testTree
      CI      -> localOption (QuickCheckTests ci) testTree
      Dev     -> testTree

-- | Query and adjust options for `TestEnv`
askTestEnv :: (TestEnv -> TestTree) -> TestTree
askTestEnv = askOption

-- | Test configurations for test environment
data TestEnvConfig = TestEnvConfig { nightly :: Int, ci :: Int }

-- | Default set of tests for each environment
defaultTestEnvConfig :: TestEnvConfig
defaultTestEnvConfig = TestEnvConfig { nightly = 100000, ci = 10000 }

-- | An 'Option' that indicates the environment in which to run tests.
data TestEnv = Nightly | CI | Dev

safeReadTestEnv :: String -> Maybe TestEnv
safeReadTestEnv "nightly" = Just Nightly
safeReadTestEnv "ci"      = Just CI
safeReadTestEnv "dev"     = Just Dev
safeReadTestEnv _         = Nothing

instance IsOption TestEnv where
  defaultValue = Dev
  parseValue = safeReadTestEnv
  optionName = pure "test-env"
  optionHelp = pure "Enable a test mode. \
      \ The 'dev' env sets the default number of quickcheck tests to 100, \
      \ 'nightly' env sets it to 100_000 quickcheck tests, and \
      \ 'ci' env sets it to 10_000 quickcheck tests. \
      \ Individual tests are adjusted to run a number of tests proportional to the value above depending \
      \ on the time it takes to run them."

  -- Set of choices for test environment
  optionCLParser = mkOptionCLParser $ metavar "nightly|ci|dev"

-- | Locally adjust the number of QuickCheck tests for the given test subtree.
-- Unless the previous number of tests was exactly '0', the result will always
-- be at least '1'. For instance:
--
-- > adjustQuickCheckTests (`div` 10)
--
-- will reduce the default number of tests by 10.
--
-- This matters in particular with tests that take a long time; in that case, we
-- settle for running fewer tests, while still scaling with the different test
-- environments (nightly, ci, dev). This function should almost always be
-- preferred to @localOption (QuickCheckTests ...)@ which sets the number of
-- tests regarless of the test environment.
adjustQuickCheckTests :: (Int -> Int) -> TestTree -> TestTree
adjustQuickCheckTests f =
  adjustOption $ \(QuickCheckTests n) ->
    QuickCheckTests $ if n == 0 then 0 else max 1 (f n)

-- | Locally adjust the maximum size parameter of QuickCheck tests for the given
-- test subtree, similar to 'adjustQuickCheckTests'.
--
-- The size parameter is varied across test runs from 0 to @maxSize - 1@
-- cyclically, influencing the result of generators that make use of it, like
-- those that call 'Test.QuickCheck.sized'.
--
-- The default is 100.
adjustQuickCheckMaxSize :: (Int -> Int) -> TestTree -> TestTree
adjustQuickCheckMaxSize f =
  adjustOption $ \(QuickCheckMaxSize n) ->
    QuickCheckMaxSize $ if n == 0 then 0 else max 1 (f n)
