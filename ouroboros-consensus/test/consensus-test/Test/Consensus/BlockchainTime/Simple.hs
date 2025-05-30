{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Tests for the computation of blockchain time.
--
-- The @BlockchainTime@ in consensus used to be ubiquitous throughout the code
-- base, but is now only used in one place: when we are checking if we should
-- produce a block. It is a simple abstraction that returns the current slot
-- number, /if it is known/ (it might be unknown of the current ledger state is
-- too far behind the wallclock). In addition to the problem of the current slot
-- being unknown, it must also deal with discontinuities in the system's
-- wallclock: NTP might adjust the clock forward or backward, or, worse, the
-- user might change their wallclock by a large amount. We don't try to deal
-- with all of these cases:
--
-- * if the clock jumps forward (so we "skip slots") this is no problem
-- * if the clock is moved back a small amount so that we are still in the same
--   slot when we expected to be in the next, also okay
-- * if the clock is moved back by more than that, so that the current slot
--   would actually /decrease/, we throw an exception; it's then up to the user
--   (or the wallet) to restart the node.
--
-- Since all our tests run in an IO simulator, we can test this by having the
-- clock behave very erratically. We then compute (in a model) what we expect
-- the behaviour of the @BlockchainTime@ to be given a specific erratic
-- behaviour, and then verify that it matches the model.
module Test.Consensus.BlockchainTime.Simple (tests) where

import Control.Applicative (Alternative (..))
import qualified Control.Concurrent.Class.MonadMVar.Strict as Strict
import qualified Control.Concurrent.Class.MonadSTM.Strict as Strict
import Control.Monad (MonadPlus, when)
import Control.Monad.Base
import qualified Control.Monad.Class.MonadSTM.Internal as LazySTM
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadTime
import qualified Control.Monad.Class.MonadTimer as MonadTimer
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.IOSim
import Control.Monad.Reader (ReaderT (..), lift)
import Control.ResourceRegistry
import Control.Tracer
import Data.Fixed
import qualified Data.Time.Clock as Time
import NoThunks.Class (AllowThunk (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM (withWatcher)
import Ouroboros.Consensus.Util.Time
import Test.QuickCheck hiding (Fixed)
import Test.Tasty hiding (after)
import Test.Tasty.QuickCheck hiding (Fixed)
import Test.Util.Orphans.Arbitrary (genNominalDiffTime50Years)
import Test.Util.Orphans.IOLike ()
import Test.Util.Range
import Test.Util.TestEnv (adjustQuickCheckTests)
import Test.Util.Time

tests :: TestTree
tests =
  testGroup
    "WallClock"
    [ adjustQuickCheckTests (`div` 10) $ testProperty "delayNextSlot" prop_delayNextSlot
    , testProperty "delayClockShift" prop_delayClockShift
    , adjustQuickCheckTests (const 1) $ testProperty "delayNoClockShift" prop_delayNoClockShift
    ]

{-------------------------------------------------------------------------------
  Test for IO
-------------------------------------------------------------------------------}

-- | Parameters for testing 'timeUntilNextSlot' in some actual IO code
data TestDelayIO = TestDelayIO
  { tdioStart' :: Time.NominalDiffTime
  -- ^ System start
  --
  -- Since we don't actually " start " the system in any way, we specify
  -- this as an offset _before_ the start of the test.
  , tdioSlotLen :: SlotLength
  -- ^ SlotNo length
  --
  -- Since this test is run in IO, we will keep the slot length short.
  }
  deriving Show

instance Arbitrary TestDelayIO where
  arbitrary = do
    tdioStart' <- genNominalDiffTime50Years
    tdioSlotLen <- slotLengthFromMillisec <$> choose (100, 1_000)
    return TestDelayIO{..}

-- | Just as a sanity check, also run the tests in IO
--
-- We override the maximum number of tests since there are slow.
--
-- NOTE: If the system is under very heavy load, this test /could/ fail:
-- the slot number after the delay could be later than the one we expect.
-- We don't relax the test because this is highly unlikely, and the stronger
-- test gives us a more useful property. Also see issue #3894.
prop_delayNextSlot :: TestDelayIO -> Property
prop_delayNextSlot TestDelayIO{..} =
  counterexample flakyTestCopy $ ioProperty test
 where
  test :: IO Property
  test = do
    tdioStart <- pickSystemStart
    let time = defaultSystemTime tdioStart nullTracer
    atStart <- fst <$> getWallClockSlot time tdioSlotLen
    nextSlot <- waitUntilNextSlot time tdioSlotLen maxClockRewind atStart
    afterDelay <- fst <$> getWallClockSlot time tdioSlotLen
    -- Ensure the test can't hang forever, fail after two minutes instead
    pure $
      within 120_000_000 $
        conjoin
          [ counterexample "atStart + 1" $ atStart + 1 === afterDelay
          , counterexample "nextSlot" $ nextSlot === afterDelay
          ]

  pickSystemStart :: IO SystemStart
  pickSystemStart = pick <$> getCurrentTime
   where
    pick :: UTCTime -> SystemStart
    pick = SystemStart . Time.addUTCTime (negate tdioStart')

  -- Will only be needed when the system clock rolls back during the execution
  -- of this test, which is rather unlikely.
  maxClockRewind :: NominalDiffTime
  maxClockRewind = secondsToNominalDiffTime 20

{-------------------------------------------------------------------------------
  Test delay using mock time
-------------------------------------------------------------------------------}

-- | Schedule defines the system time as offsets (in seconds) from the start
--
-- We limit the resolution of the offsets to 0.1 seconds to make the tests
-- easier to interpret and shrink (slot length is set to 1 seconds). We allow
-- the clock to go back at most 2 seconds.
newtype Schedule = Schedule {getSchedule :: [Fixed E1]}
  deriving stock Show
  deriving NoThunks via AllowThunk Schedule

-- | Translate an offset in the schedule to a slot
--
-- Assumes slot length is 1.
offsetToSlot :: Fixed E1 -> SlotNo
offsetToSlot = SlotNo . floor

-- | Does a schedule ever go back?
--
-- Used for labelling.
scheduleGoesBack :: Schedule -> Bool
scheduleGoesBack (Schedule []) = False
scheduleGoesBack (Schedule (t : ts)) = go t ts
 where
  go :: Ord a => a -> [a] -> Bool
  go _ [] = False
  go x (y : ys) = y < x || go y ys

-- | How often do two subsequent time entries fall into the same slot?
--
-- Used for labelling.
scheduleCountSkips :: Schedule -> Int
scheduleCountSkips (Schedule []) = 0
scheduleCountSkips (Schedule (t : ts)) = go t ts
 where
  go :: Fixed E1 -> [Fixed E1] -> Int
  go _ [] = 0
  go x (y : ys) = (if offsetToSlot x == offsetToSlot y then 1 else 0) + go y ys

-- | Predict the outcome of a given schedule
--
-- Returns the set of slot numbers that 'BlockchainTime' should report or,
-- if time moved backwards, the @(before, after)@ slot pair where @after@ is
-- more than the @maxClockRewind@ less than @before@.
--
-- NOTE: Assumes the slot length is 1 and max clock rewind is 2 for these sets.
model :: Int -> Schedule -> Either (SlotNo, SlotNo) [SlotNo]
model = \need (Schedule ss) ->
  -- Establish the invariant that the 'Schedule' is never empty
  let ss' = case ss of
        [] -> [0.0]
        _ -> ss
   in runExcept $
        (SlotNo 0 :) <$> go (need - 1) (Schedule ss') (0.0, SlotNo 0)
 where
  -- \| This let's us treat the schedule as an infinite stream of offsets.
  --
  -- INVARIANT: 'Schedule' is never empty
  --
  -- When there is no offset after the current one in the schedule, create
  -- one, exactly one slot length after the current one.
  advanceSchedule :: Schedule -> (Fixed E1, Schedule)
  advanceSchedule (Schedule ss) =
    case ss of
      [] -> error "invariant broken: empty schedule"
      [s] -> (s, Schedule [s + 1.0])
      s : ss' -> (s, Schedule ss')

  go ::
    Int ->
    Schedule ->
    (Fixed E1, SlotNo) ->
    Except (SlotNo, SlotNo) [SlotNo]
  go n ss (prevOffset, prevSlot)
    | n <= 0 =
        return []
    | nextSlot == prevSlot =
        go n ss' (offset, nextSlot)
    | nextSlot > prevSlot =
        (nextSlot :) <$> go (n - 1) ss' (offset, nextSlot)
    -- If time moved back, but not more than 2s, we don't throw an exception
    | prevOffset - offset <= 2 =
        go n ss' (offset, prevSlot)
    -- If time moved back too much, we should see an exception
    | otherwise =
        throwError (prevSlot, nextSlot)
   where
    (offset, ss') = advanceSchedule ss
    nextSlot = offsetToSlot offset

instance Arbitrary Schedule where
  arbitrary =
    -- We only collect 100 samples. Generate a few more, potentially, but also
    -- often generate fewer (which would give us the default behaviour).
    Schedule <$> (go 0 =<< choose (0, 110))
   where
    go :: Fixed E1 -> Int -> Gen [Fixed E1]
    go _ 0 = return []
    go now n = do
      now' <-
        frequency
          [ -- If time goes back too often, most runs end in an exception
            (100, (\delta -> now + fixedFromDeci delta) <$> choose (0, 30))
          , -- Go back a bit without exceeding the max clock rewind
            (10, (\delta -> max 0 (now - fixedFromDeci delta)) <$> choose (0, 2))
          , -- Occassionally just pick an entirely random time
            (1, fixedFromDeci <$> choose (0, 100))
          ]
      (now' :) <$> go now' (n - 1)

    fixedFromDeci :: Integer -> Fixed E1
    fixedFromDeci = MkFixed

  shrink (Schedule s) = Schedule <$> shrinkList shrinkOffset s
   where
    shrinkOffset :: Fixed E1 -> [Fixed E1]
    shrinkOffset (MkFixed o) = MkFixed <$> shrink o

prop_delayClockShift :: Schedule -> Property
prop_delayClockShift schedule =
  counterexample flakyTestCopy $
    tabulate "schedule length" [show $ range (length (getSchedule schedule))] $
      tabulate "schedule goes back" [show $ scheduleGoesBack schedule] $
        tabulate "schedule skips" [show $ range (scheduleCountSkips schedule)] $
          case model numSlots schedule of
            Left (before, after) ->
              case testResult of
                Left (FailureException e) ->
                  checkException before after e
                Left e ->
                  counterexample ("Unexpected simulator failure " ++ show e) $
                    property False
                Right slots' ->
                  counterexample ("Expected exception but got " ++ show slots') $
                    property False
            Right slots ->
              case testResult of
                Left e ->
                  counterexample ("Expected normal termination, but got " ++ show e) $
                    property False
                Right slots' ->
                  slots' === slots
 where
  numSlots :: Int
  numSlots = 100

  testResult :: Either Failure [SlotNo]
  testResult =
    overrideDelay dawnOfTime schedule $
      testOverrideDelay
        (SystemStart dawnOfTime)
        (slotLengthFromSec 1)
        (secondsToNominalDiffTime 2)
        numSlots

  checkException :: SlotNo -> SlotNo -> SomeException -> Property
  checkException before after e
    | Just (ExceptionInLinkedThread _ e') <- fromException e =
        checkException before after e'
    | Just (SystemClockMovedBack before' after') <- fromException e =
        counterexample ("Got expected exception " ++ show e) $
          conjoin
            [ before' === before
            , after' === after
            ]
    | otherwise =
        counterexample ("Unexpected exception: " ++ show e) $
          property False

-- | Just as a sanity check, verify that this works in IO
prop_delayNoClockShift :: Property
prop_delayNoClockShift =
  counterexample flakyTestCopy $
    ioProperty $ do
      now <- getCurrentTime
      slots <-
        originalDelay $
          testOverrideDelay
            (SystemStart now)
            (slotLengthFromMillisec 100)
            (secondsToNominalDiffTime 20)
            5
      pure $ slots === [SlotNo n | n <- [0 .. 4]]

-- | Note that that under load, the returned list could be missing certain slots
-- or contain more slots than requested. This means that tests using this
-- function can fail, also see issue #3894.
testOverrideDelay ::
  forall m.
  (IOLike m, MonadTime m, MonadDelay (OverrideDelay m)) =>
  SystemStart ->
  SlotLength ->
  NominalDiffTime ->
  -- | Number of slots to collect
  Int ->
  OverrideDelay m [SlotNo]
testOverrideDelay systemStart slotLength maxClockRewind numSlots = do
  bracketWithPrivateRegistry
    ( \registry ->
        simpleBlockchainTime
          registry
          (defaultSystemTime systemStart nullTracer)
          slotLength
          maxClockRewind
    )
    (\_btime -> pure ())
    $ \btime -> do
      slotsVar <- uncheckedNewTVarM []
      withWatcher
        "testOverrideDelay"
        ( knownSlotWatcher btime $ \slotNo -> do
            atomically $ modifyTVar slotsVar (slotNo :)
        )
        $ do
          -- Wait to collect the required number of slots
          atomically $ do
            slots <- readTVar slotsVar
            when (length slots < numSlots) $ retry
            return $ reverse slots

{-------------------------------------------------------------------------------
  Test-programmable time
-------------------------------------------------------------------------------}

-- | IO wrapper where we can program the effect of 'threadDelay'
newtype OverrideDelay m a = OverrideDelay
  { unOverrideDelay :: ReaderT (StrictTVar m Schedule) m a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadEventlog
    , MonadThrow
    , MonadCatch
    , MonadEvaluate
    , MonadMask
    , MonadMonotonicTime
    , MonadMonotonicTimeNSec
    , MonadMVar
    , MonadTime
    , MonadThread
    , MonadFork
    , MonadST
    )

instance PrimMonad m => PrimMonad (OverrideDelay m) where
  type PrimState (OverrideDelay m) = PrimState m
  primitive = OverrideDelay . primitive
  {-# INLINE primitive #-}

deriving via
  AllowThunk (OverrideDelay s a)
  instance
    NoThunks (OverrideDelay s a)

deriving via
  AllowThunk (StrictTVar (OverrideDelay s) a)
  instance
    NoThunks (StrictTVar (OverrideDelay s) a)

deriving via
  AllowThunk (StrictTMVar (OverrideDelay s) a)
  instance
    NoThunks (StrictTMVar (OverrideDelay s) a)

deriving via
  AllowThunk (StrictSVar (OverrideDelay s) a)
  instance
    NoThunks (StrictSVar (OverrideDelay s) a)

deriving via
  AllowThunk (StrictMVar (OverrideDelay s) a)
  instance
    NoThunks (StrictMVar (OverrideDelay s) a)

deriving via
  AllowThunk (Strict.StrictTVar (OverrideDelay s) a)
  instance
    NoThunks (Strict.StrictTVar (OverrideDelay s) a)

deriving via
  AllowThunk (Strict.StrictMVar (OverrideDelay s) a)
  instance
    NoThunks (Strict.StrictMVar (OverrideDelay s) a)

instance MonadTimer.MonadDelay (OverrideDelay (IOSim s)) where
  threadDelay d = OverrideDelay $ ReaderT $ \schedule -> do
    -- Do the original delay. This is important, because otherwise this
    -- turns into a busy loop in the simulator
    MonadTimer.threadDelay d
    -- However, the time /after/ the delay will be determined by the
    -- schedule (unless it is empty, in which case the threadDelay behaves
    -- as normal).
    mOverride <- atomically $ stateTVar schedule nextDelay
    case mOverride of
      Nothing -> return ()
      Just t -> setCurrentTime t
   where
    nextDelay :: Schedule -> (Maybe UTCTime, Schedule)
    nextDelay = \case
      Schedule [] -> (Nothing, Schedule [])
      Schedule (t : ts) -> (Just $ offsetToTime t, Schedule ts)

    offsetToTime :: Fixed E1 -> UTCTime
    offsetToTime t = Time.addUTCTime (realToFrac t) dawnOfTime

instance MonadDelay (OverrideDelay (IOSim s)) where
  threadDelay d = OverrideDelay $ ReaderT $ \schedule -> do
    -- Do the original delay. This is important, because otherwise this
    -- turns into a busy loop in the simulator
    threadDelay d
    -- However, the time /after/ the delay will be determined by the
    -- schedule (unless it is empty, in which case the threadDelay behaves
    -- as normal).
    mOverride <- atomically $ stateTVar schedule nextDelay
    case mOverride of
      Nothing -> return ()
      Just t -> setCurrentTime t
   where
    nextDelay :: Schedule -> (Maybe UTCTime, Schedule)
    nextDelay = \case
      Schedule [] -> (Nothing, Schedule [])
      Schedule (t : ts) -> (Just $ offsetToTime t, Schedule ts)

    offsetToTime :: Fixed E1 -> UTCTime
    offsetToTime t = Time.addUTCTime (realToFrac t) dawnOfTime

-- | The IO instance just uses the default delay
instance MonadTimer.MonadDelay (OverrideDelay IO) where
  threadDelay d = OverrideDelay $ ReaderT $ \_schedule -> MonadTimer.threadDelay d

-- | The IO instance just uses the default delay
instance MonadDelay (OverrideDelay IO) where
  threadDelay d = OverrideDelay $ ReaderT $ \_schedule -> threadDelay d

newtype OverrideDelaySTM m a
  = OverrideDelaySTM (ReaderT (StrictTVar m Schedule) (STM m) a)

deriving instance Alternative (STM m) => Alternative (OverrideDelaySTM m)
deriving instance Applicative (STM m) => Applicative (OverrideDelaySTM m)
deriving instance Functor (STM m) => Functor (OverrideDelaySTM m)
deriving instance Monad (STM m) => Monad (OverrideDelaySTM m)
deriving instance MonadPlus (STM m) => MonadPlus (OverrideDelaySTM m)
deriving instance MonadThrow (STM m) => MonadThrow (OverrideDelaySTM m)
deriving instance MonadCatch (STM m) => MonadCatch (OverrideDelaySTM m)

instance MonadSTM m => MonadSTM (OverrideDelay m) where
  type STM (OverrideDelay m) = OverrideDelaySTM m
  atomically (OverrideDelaySTM (ReaderT stm)) =
    OverrideDelay (ReaderT (atomically . stm))

  type TVar (OverrideDelay m) = LazySTM.TVar m
  newTVar = OverrideDelaySTM . lift . LazySTM.newTVar
  readTVar = OverrideDelaySTM . lift . LazySTM.readTVar
  writeTVar v = OverrideDelaySTM . lift . LazySTM.writeTVar v
  retry = OverrideDelaySTM . lift $ retry
  orElse (OverrideDelaySTM (ReaderT stm)) (OverrideDelaySTM (ReaderT stm')) =
    OverrideDelaySTM (ReaderT $ \v -> stm v `orElse` stm' v)
  modifyTVar v = OverrideDelaySTM . lift . LazySTM.modifyTVar v
  stateTVar v = OverrideDelaySTM . lift . LazySTM.stateTVar v
  swapTVar v = OverrideDelaySTM . lift . LazySTM.swapTVar v

  type TMVar (OverrideDelay m) = LazySTM.TMVar m
  newTMVar = OverrideDelaySTM . lift . LazySTM.newTMVar
  newEmptyTMVar = OverrideDelaySTM . lift $ LazySTM.newEmptyTMVar
  takeTMVar = OverrideDelaySTM . lift . LazySTM.takeTMVar
  tryTakeTMVar = OverrideDelaySTM . lift . LazySTM.tryTakeTMVar
  putTMVar v = OverrideDelaySTM . lift . LazySTM.putTMVar v
  tryPutTMVar v = OverrideDelaySTM . lift . LazySTM.tryPutTMVar v
  readTMVar = OverrideDelaySTM . lift . LazySTM.readTMVar
  writeTMVar v = OverrideDelaySTM . lift . LazySTM.writeTMVar v
  tryReadTMVar = OverrideDelaySTM . lift . LazySTM.tryReadTMVar
  swapTMVar v = OverrideDelaySTM . lift . LazySTM.swapTMVar v
  isEmptyTMVar = OverrideDelaySTM . lift . LazySTM.isEmptyTMVar

  type TQueue (OverrideDelay m) = LazySTM.TQueue m
  newTQueue = OverrideDelaySTM . lift $ LazySTM.newTQueue
  readTQueue = OverrideDelaySTM . lift . LazySTM.readTQueue
  tryReadTQueue = OverrideDelaySTM . lift . LazySTM.tryReadTQueue
  peekTQueue = OverrideDelaySTM . lift . LazySTM.peekTQueue
  tryPeekTQueue = OverrideDelaySTM . lift . LazySTM.tryPeekTQueue
  writeTQueue v = OverrideDelaySTM . lift . LazySTM.writeTQueue v
  isEmptyTQueue = OverrideDelaySTM . lift . LazySTM.isEmptyTQueue
  flushTQueue = OverrideDelaySTM . lift . LazySTM.flushTQueue
  unGetTQueue v = OverrideDelaySTM . lift . LazySTM.unGetTQueue v

  type TBQueue (OverrideDelay m) = LazySTM.TBQueue m
  newTBQueue = OverrideDelaySTM . lift . LazySTM.newTBQueue
  readTBQueue = OverrideDelaySTM . lift . LazySTM.readTBQueue
  tryReadTBQueue = OverrideDelaySTM . lift . LazySTM.tryReadTBQueue
  peekTBQueue = OverrideDelaySTM . lift . LazySTM.peekTBQueue
  tryPeekTBQueue = OverrideDelaySTM . lift . LazySTM.tryPeekTBQueue
  writeTBQueue v = OverrideDelaySTM . lift . LazySTM.writeTBQueue v
  lengthTBQueue = OverrideDelaySTM . lift . LazySTM.lengthTBQueue
  isEmptyTBQueue = OverrideDelaySTM . lift . LazySTM.isEmptyTBQueue
  isFullTBQueue = OverrideDelaySTM . lift . LazySTM.isFullTBQueue
  flushTBQueue = OverrideDelaySTM . lift . LazySTM.flushTBQueue
  unGetTBQueue v = OverrideDelaySTM . lift . LazySTM.unGetTBQueue v

  type TArray (OverrideDelay m) = LazySTM.TArray m

  type TSem (OverrideDelay m) = LazySTM.TSem m
  newTSem = OverrideDelaySTM . lift . LazySTM.newTSem
  waitTSem = OverrideDelaySTM . lift . LazySTM.waitTSem
  signalTSem = OverrideDelaySTM . lift . LazySTM.signalTSem
  signalTSemN v = OverrideDelaySTM . lift . LazySTM.signalTSemN v

  type TChan (OverrideDelay m) = LazySTM.TChan m
  newTChan = OverrideDelaySTM . lift $ LazySTM.newTChan
  newBroadcastTChan = OverrideDelaySTM . lift $ LazySTM.newBroadcastTChan
  dupTChan = OverrideDelaySTM . lift . LazySTM.dupTChan
  cloneTChan = OverrideDelaySTM . lift . LazySTM.cloneTChan
  readTChan = OverrideDelaySTM . lift . LazySTM.readTChan
  tryReadTChan = OverrideDelaySTM . lift . LazySTM.tryReadTChan
  peekTChan = OverrideDelaySTM . lift . LazySTM.peekTChan
  tryPeekTChan = OverrideDelaySTM . lift . LazySTM.tryPeekTChan
  writeTChan v = OverrideDelaySTM . lift . LazySTM.writeTChan v
  unGetTChan v = OverrideDelaySTM . lift . LazySTM.unGetTChan v
  isEmptyTChan = OverrideDelaySTM . lift . LazySTM.isEmptyTChan

instance MonadLabelledSTM m => MonadLabelledSTM (OverrideDelay m) where
  labelTVar v = OverrideDelaySTM . lift . LazySTM.labelTVar v
  labelTMVar v = OverrideDelaySTM . lift . LazySTM.labelTMVar v
  labelTQueue v = OverrideDelaySTM . lift . LazySTM.labelTQueue v
  labelTBQueue v = OverrideDelaySTM . lift . LazySTM.labelTBQueue v
  labelTArray v = OverrideDelaySTM . lift . LazySTM.labelTArray v
  labelTSem v = OverrideDelaySTM . lift . LazySTM.labelTSem v
  labelTChan v = OverrideDelaySTM . lift . LazySTM.labelTChan v

  labelTVarIO v = OverrideDelay . lift . LazySTM.labelTVarIO v
  labelTMVarIO v = OverrideDelay . lift . LazySTM.labelTMVarIO v
  labelTQueueIO v = OverrideDelay . lift . LazySTM.labelTQueueIO v
  labelTBQueueIO v = OverrideDelay . lift . LazySTM.labelTBQueueIO v

instance MonadInspectSTM m => MonadInspectSTM (OverrideDelay m) where
  type InspectMonad (OverrideDelay m) = InspectMonad m
  inspectTVar _ = inspectTVar (Proxy :: Proxy m)
  inspectTMVar _ = inspectTMVar (Proxy :: Proxy m)

instance MonadTraceSTM m => MonadTraceSTM (OverrideDelay m) where
  traceTVar _ v = OverrideDelaySTM . lift . LazySTM.traceTVar Proxy v
  traceTMVar _ v = OverrideDelaySTM . lift . LazySTM.traceTMVar Proxy v
  traceTQueue _ v = OverrideDelaySTM . lift . LazySTM.traceTQueue Proxy v
  traceTBQueue _ v = OverrideDelaySTM . lift . LazySTM.traceTBQueue Proxy v
  traceTSem _ v = OverrideDelaySTM . lift . LazySTM.traceTSem Proxy v

newtype OverrideDelayAsync m a = OverrideDelayAsync
  { unOverrideDelayAsync :: Async m a
  }

instance (MonadAsync m, MonadMask m, MonadThrow (STM m)) => MonadAsync (OverrideDelay m) where
  type Async (OverrideDelay m) = OverrideDelayAsync m
  async io = OverrideDelay $
    ReaderT $
      \schedule -> OverrideDelayAsync <$> async (runReaderT (unOverrideDelay io) schedule)
  asyncBound io = OverrideDelay $
    ReaderT $
      \schedule -> OverrideDelayAsync <$> asyncBound (runReaderT (unOverrideDelay io) schedule)
  asyncOn n io = OverrideDelay $
    ReaderT $
      \schedule -> OverrideDelayAsync <$> asyncOn n (runReaderT (unOverrideDelay io) schedule)
  asyncThreadId (OverrideDelayAsync a) = asyncThreadId a
  cancel = OverrideDelay . lift . cancel . unOverrideDelayAsync
  cancelWith a = OverrideDelay . lift . cancelWith (unOverrideDelayAsync a)
  asyncWithUnmask f = OverrideDelay $
    ReaderT $
      \schedule ->
        OverrideDelayAsync
          <$> asyncWithUnmask
            ( \unmask ->
                let unmask' :: forall x. OverrideDelay m x -> OverrideDelay m x
                    unmask' (OverrideDelay (ReaderT m)) =
                      OverrideDelay $ ReaderT $ \schedule' -> unmask (m schedule')
                 in runReaderT (unOverrideDelay $ f unmask') schedule
            )
  asyncOnWithUnmask n f = OverrideDelay $
    ReaderT $
      \schedule ->
        OverrideDelayAsync
          <$> asyncOnWithUnmask
            n
            ( \unmask ->
                let unmask' :: forall x. OverrideDelay m x -> OverrideDelay m x
                    unmask' (OverrideDelay (ReaderT m)) =
                      OverrideDelay $ ReaderT $ \schedule' -> unmask (m schedule')
                 in runReaderT (unOverrideDelay $ f unmask') schedule
            )

  waitCatchSTM = OverrideDelaySTM . lift . waitCatchSTM . unOverrideDelayAsync
  pollSTM = OverrideDelaySTM . lift . pollSTM . unOverrideDelayAsync

instance MonadSay m => MonadSay (OverrideDelay m) where
  say = OverrideDelay . lift . say

instance Monad m => MonadBase (OverrideDelay m) (OverrideDelay m) where
  liftBase = id

instance (IOLike m, MonadDelay (OverrideDelay m)) => IOLike (OverrideDelay m) where
  forgetSignKeyKES = OverrideDelay . lift . forgetSignKeyKES

overrideDelay ::
  UTCTime ->
  Schedule ->
  (forall s. OverrideDelay (IOSim s) a) ->
  Either Failure a
overrideDelay start schedule ma = runSim $ do
  setCurrentTime start
  scheduleVar <- newTVarIO schedule
  runReaderT (unOverrideDelay ma) scheduleVar

originalDelay :: OverrideDelay IO a -> IO a
originalDelay ma = runReaderT (unOverrideDelay ma) (error "schedule unused")

flakyTestCopy :: String
flakyTestCopy =
  "This test may be flaky, and its failure may not be indicative of an actual problem: see https://github.com/IntersectMBO/ouroboros-consensus/issues/567"
