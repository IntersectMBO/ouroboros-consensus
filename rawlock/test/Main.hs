{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Exception (throw)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadTest
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Except
import           Control.Monad.IOSim
import           Control.RAWLock
import           Data.Either
import           Test.QuickCheck.Gen.Unsafe
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "RAWLock" [
    testProperty "Exception safe" $ conjoin (map prop_exception_safe allCommandCombinations)
  , testProperty "correctness" prop_correctness
  , testProperty "unsafe functions do not deadlock" $ conjoin (map prop_unsafe_actions allCommandCombinations)
  ]

{-------------------------------------------------------------------------------
  Exception safe
-------------------------------------------------------------------------------}

data Action = Read | Incr | Append
  deriving (Show)

allCommandCombinations :: [(Action, Action, Action)]
allCommandCombinations =
  [ (a, b, c) | let cmds = [Read, Incr, Append], a <- cmds, b <- cmds, c <- cmds ]

prop_exception_safe :: (Action, Action, Action) -> Property
prop_exception_safe actions@(a1, a2, a3) = counterexample (show actions) $
    exploreSimTrace id action
      (\_ tr -> counterexample (ppTrace tr) . property . isRight . traceResult False $ tr)
  where
    action :: IOSim s ()
    action = do
      exploreRaces
      l <- new (0 :: Int)
      let c = \case
            Read -> withReadAccess l $ \s -> say (show s)
            Incr -> withWriteAccess l $ \s -> say (show s) >> pure ((), s + 1)
            Append -> withAppendAccess l $ \s -> say (show s) >> pure ((), s + 1)
      t1 <- async $ c a1
      t2 <- async $ c a2
      t3 <- async $ c a3
      async (cancel t1) >>= wait
      (_ :: Either SomeException ()) <- waitCatch t1
      (_ :: Either SomeException ()) <- waitCatch t2
      (_ :: Either SomeException ()) <- waitCatch t3
      pure ()

{-------------------------------------------------------------------------------
  Correctness
-------------------------------------------------------------------------------}

-- | Test the correctness of the RAWLock
--
-- For a generated number of readers, appenders, and writers: spawn a thread
-- for each. Each thread will process a list of 'ThreadDelays'. For each
-- 'ThreadDelays': wait the generated 'beforeLockTime', then lock the RAWLock
-- with the respective @RAWLock.withXAccess@, increment a 'TVar' that stores
-- the number of readers/appenders/writers that have access, hold the lock for
-- a generated 'withLockTime', decrement the 'TVar', and release the lock.
--
-- In a separate thread, we watch for any changes in the three 'TVar's and
-- write each changed 'RAWState' to a trace (in a separate 'TVar').
-- Afterwards, we check the 'RAWState's in the trace for consistency (using
-- 'isConsistent'), e.g., not more than one concurrent appender.
prop_correctness :: TestSetup -> Property
prop_correctness (TestSetup rawDelays) =
    monadicSimWithTrace tabulateBlockeds test
  where
    RAW readerDelays appenderDelays writerDelays = rawDelays

    test :: forall s. PropertyM (IOSim s) ()
    test = do
      rawVars@(RAW varReaders varAppenders varWriters) <- run newRAWVars

      trace <- run $ do
        rawLock  <- new ()
        varTrace <- newTVarIO []

        let traceState :: STM (IOSim s) ()
            traceState = do
              rawState <- readRAWState rawVars
              modifyTVar varTrace (rawState:)

        threads <- mapM (async . (labelThisThread "testThread" >>)) $
          map (runReader   rawLock traceState varReaders)   readerDelays   <>
          map (runAppender rawLock traceState varAppenders) appenderDelays <>
          map (runWriter   rawLock traceState varWriters)   writerDelays

        mapM_ wait threads
        reverse <$> atomically (readTVar varTrace)

      checkRAWTrace trace

    runReader
      :: RAWLock (IOSim s) ()
      -> STM (IOSim s) ()  -- ^ Trace the 'RAWState'
      -> StrictTVar (IOSim s) Int
      -> [ThreadDelays]
      -> IOSim s ()
    runReader rawLock traceState varReaders =
      mapM_ $ \(ThreadDelays before with) -> do
        threadDelay before
        withReadAccess rawLock $ const $ do
          atomically $ modifyTVar varReaders succ *> traceState
          threadDelay with
          atomically $ modifyTVar varReaders pred *> traceState

    runAppender
      :: RAWLock (IOSim s) ()
      -> STM (IOSim s) ()  -- ^ Trace the 'RAWState'
      -> StrictTVar (IOSim s) Int
      -> [ThreadDelays]
      -> IOSim s ()
    runAppender rawLock traceState varAppenders =
      mapM_ $ \(ThreadDelays before with) -> do
        threadDelay before
        withAppendAccess rawLock $ const $ do
          atomically $ modifyTVar varAppenders succ *> traceState
          threadDelay with
          atomically $ modifyTVar varAppenders pred *> traceState
          return ((), ())

    runWriter
      :: RAWLock (IOSim s) ()
      -> STM (IOSim s) ()  -- ^ Trace the 'RAWState'
      -> StrictTVar (IOSim s) Int
      -> [ThreadDelays]
      -> IOSim s ()
    runWriter rawLock traceState varWriters =
      mapM_ $ \(ThreadDelays before with) -> do
        threadDelay before
        withWriteAccess rawLock $ const $ do
          atomically $ modifyTVar varWriters succ *> traceState
          threadDelay with
          atomically $ modifyTVar varWriters pred *> traceState
          return ((), ())

-- | Like 'monadicSim' (which is like 'monadicIO' for the IO simulator), but
-- allows inspecting the trace for labelling purposes.
monadicSimWithTrace ::
     Testable a
  => (forall x. SimTrace x -> Property -> Property)
  -> (forall s. PropertyM (IOSim s) a)
  -> Property
monadicSimWithTrace attachTrace m = property $ do
    tr <- runSimGenWithTrace (monadic' m)
    case traceResult False tr of
      Left failure -> throw failure
      Right prop   -> return $ attachTrace tr prop
  where
    runSimGenWithTrace :: (forall s. Gen (IOSim s a)) -> Gen (SimTrace a)
    runSimGenWithTrace f = do
      Capture eval <- capture
      return $ runSimTrace (eval f)

-- | Tabulate the number of times a thread is blocked.
--
-- The higher this number, the higher the contention. If there's no
-- contention, we're not testing the lock properly.
tabulateBlockeds :: SimTrace a -> Property -> Property
tabulateBlockeds tr =
    tabulate "number of times blocked" [classifyBand (count isBlocked tr)]
  where
    isBlocked (EventTxBlocked {}) = Just ()
    isBlocked _                   = Nothing

    count :: (SimEventType -> Maybe x) -> SimTrace a -> Int
    count p = length . selectTraceEvents (const p)

    classifyBand :: Int -> String
    classifyBand n
      | n < 10
      = "n < 10"
      | n < 100
      = "n < 100"
      | n < 1000
      = "n < 1,000"
      | n < 10_000
      = "1,000 < n < 10,000"
      | n < 100_000
      = "10,000 < n < 100,000"
      | n < 1000_000
      = "100,000 < n < 1,000,000"
      | otherwise
      = "1,000,000 < n"

{-------------------------------------------------------------------------------
  State checking
-------------------------------------------------------------------------------}

-- | Data type reused whenever we need something for all three of them.
data RAW a = RAW
    { readers   :: a
    , appenders :: a
    , writers   :: a
    }
  deriving (Show, Eq, Functor)

type RAWVars m = RAW (StrictTVar m Int)

newRAWVars :: IOSim s (RAWVars (IOSim s))
newRAWVars = RAW <$> newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO 0

type RAWState' = RAW Int

readRAWState :: RAWVars (IOSim s) -> STM (IOSim s) RAWState'
readRAWState RAW { readers, appenders, writers } =
    RAW
      <$> readTVar readers
      <*> readTVar appenders
      <*> readTVar writers

isConsistent :: RAWState' -> Except String ()
isConsistent RAW { readers, appenders, writers }
    | appenders > 1
    = throwError $ show appenders <> " appenders while at most 1 is allowed"
    | writers > 1
    = throwError $ show writers <> " writers while at most 1 is allowed"
    | writers == 1, readers > 0
    = throwError $ "writer concurrent with " <> show readers <> "reader(s)"
    | writers == 1, appenders > 0
    = throwError $ "writer concurrent with an appender"
    | otherwise
    = return ()

type RAWTrace = [RAWState']

checkRAWTrace :: Monad m => RAWTrace -> PropertyM m ()
checkRAWTrace = mapM_ $ \rawState ->
    case runExcept $ isConsistent rawState of
      Left msg -> do
        monitor (counterexample msg)
        Test.QuickCheck.Monadic.assert False
      Right () ->
        return ()

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

newtype TestSetup = TestSetup (RAW [[ThreadDelays]])
  deriving (Show)

instance Arbitrary TestSetup where
  arbitrary = do
    nbReaders   <- choose (0, 3)
    nbAppenders <- choose (0, 3)
    nbWriters   <- choose (0, 3)
    readers   <- vectorOf nbReaders   arbitrary
    appenders <- vectorOf nbAppenders arbitrary
    writers   <- vectorOf nbWriters   arbitrary
    return $ TestSetup RAW { readers, appenders, writers }
  shrink (TestSetup raw@RAW { readers, appenders, writers }) =
    [TestSetup raw { readers   = readers'   } | readers'   <- shrink readers  ] <>
    [TestSetup raw { appenders = appenders' } | appenders' <- shrink appenders] <>
    [TestSetup raw { writers   = writers'   } | writers'   <- shrink writers  ]

data ThreadDelays = ThreadDelays
    { beforeLockTime :: Int
      -- ^ How long the thread should wait before it starts to take the lock
    , withLockTime   :: Int
      -- ^ How long the thread should wait while holding the lock
    }
  deriving (Eq, Show)

instance Arbitrary ThreadDelays where
  arbitrary = do
    beforeLockTime <- choose (0, 1000)
    withLockTime   <- choose (0, 2000)
    return ThreadDelays { beforeLockTime, withLockTime }

{-------------------------------------------------------------------------------
  unsafe functions
-------------------------------------------------------------------------------}

prop_unsafe_actions :: (Action, Action, Action) -> Property
prop_unsafe_actions actions@(a1, a2, a3) = counterexample (show actions) $
  exploreSimTrace id action
      (\_ tr -> counterexample (ppTrace tr) . property . isRight . traceResult False $ tr)
  where
    action :: IOSim s ()
    action = do
      exploreRaces
      l <- new (0 :: Int)
      let c = \case
            Read -> bracket
                      (atomically (unsafeAcquireReadAccess l))
                      (const $ atomically (unsafeReleaseReadAccess l))
                      (say . ("Read: " <>) . show)
            Incr -> generalBracket
                      (unsafeAcquireWriteAccess l)
                      (\orig -> \case
                          ExitCaseSuccess s' -> unsafeReleaseWriteAccess l s'
                          _ -> unsafeReleaseWriteAccess l orig
                      )
                      (\s -> do
                          say ("Incr: " <> show s)
                          pure (s + 1)
                      ) >> pure ()
            Append -> generalBracket
                        (unsafeAcquireAppendAccess l)
                        (\orig -> \case
                            ExitCaseSuccess s' -> unsafeReleaseAppendAccess l s'
                            _ -> unsafeReleaseAppendAccess l orig
                        )
                        (\s -> do
                            say ("Append: " <> show s)
                            pure (s + 1)
                        ) >> pure ()
      t1 <- async $ c a1
      t2 <- async $ c a2
      t3 <- async $ c a3
      async (cancel t1) >>= wait
      (_ :: Either SomeException ()) <- waitCatch t1
      (_ :: Either SomeException ()) <- waitCatch t2
      (_ :: Either SomeException ()) <- waitCatch t3
      pure ()
