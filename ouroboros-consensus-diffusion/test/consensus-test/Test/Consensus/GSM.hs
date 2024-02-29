{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

module Test.Consensus.GSM (tests) where

import           Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked
import           Control.Monad (replicateM_)
import           Control.Monad.Class.MonadAsync (poll, withAsync)
import           Control.Monad.Class.MonadFork (MonadFork, yield)
import           Control.Monad.Class.MonadSTM
import qualified Control.Monad.Class.MonadTime.SI as SI
import qualified Control.Monad.Class.MonadTimer.SI as SI
import qualified Control.Monad.IOSim as IOSim
import           Control.Tracer (Tracer (Tracer))
import           Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Ouroboros.Consensus.Node.GSM as GSM
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type
                     (LedgerStateJudgement (..))
import           Test.Consensus.GSM.Model
import           Test.Consensus.IOSimQSM.Test.StateMachine.Sequential
                     (runCommands')
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import qualified Test.StateMachine as QSM
import           Test.StateMachine (Concrete)
import qualified Test.StateMachine.Types as QSM
import           Test.Tasty (TestTree)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestEnv (adjustQuickCheckTests)
import           Test.Util.ToExpr ()

-----

tests :: [TestTree]
tests =
    adhoc <> core
  where
    adhoc = [
        testProperty "GSM yield regression" prop_yield_regression
     ]

    core = [
          adjustQuickCheckTests (* 10)
        $ testProperty ("GSM (" <> coreTestName ub <> ")")
        $ prop_sequential ub
      | ub <- Nothing : map Just [minBound .. maxBound :: UpstreamPeer]
      ]

    coreTestName = \case
        Nothing -> "no peers"
        Just ub -> "at most " <> case fromEnum ub of
            0 -> "1 peer"
            n -> show (n + 1) <> " peers"

----- the QSM code under test

-- These definitions are in the exact same order as the QSM tutorial at
-- <https://github.com/stevana/quickcheck-state-machine/blob/3748220bffacb61847e35e3808403f3e7187a7c6/README.md>,
-- except the model definition is in "Test.Consensus.GSM.Model".

semantics ::
     IOLike m
  => Vars m
  -> Command Concrete
  -> m (Response Concrete)
semantics vars cmd = pre $ case cmd of
    Disconnect peer -> do
        atomically $ do
            modifyTVar varCandidates $ Map.delete peer
            modifyTVar varIdlers     $ Set.delete peer
        pure Unit
    ExtendSelection sdel -> do
        atomically $ do
            Selection b s <- readTVar varSelection
            writeTVar varSelection $! Selection (b + 1) (s + sdel)
        pure Unit
    ModifyCandidate peer bdel -> do
        atomically $ do

            modifyTVar varIdlers $ Set.delete peer

            v <- (Map.! peer) <$> readTVar varCandidates
            Candidate b <- readTVar v
            writeTVar v $! Candidate (b + bdel)

        pure Unit
    NewCandidate peer bdel -> do
        atomically $ do
            Selection b _s <- readTVar varSelection
            v <- newTVar $! Candidate (b + bdel)
            modifyTVar varCandidates $ Map.insert peer v
        pure Unit
    ReadGsmState -> do
        fmap ReadThisGsmState $ atomically $ readTVar varGsmState
    ReadMarker -> do
        fmap ReadThisMarker $ atomically $ readTVar varMarker
    StartIdling peer -> do
        atomically $ modifyTVar varIdlers $ Set.insert peer
        pure Unit
    TimePasses dur -> do
        SI.threadDelay (0.1 * fromIntegral dur)
        pure Unit
  where
    Vars
        varSelection
        varCandidates
        varIdlers
        varGsmState
        varMarker
        varEvents
      = vars

    pre m = do
        push varEvents (EvBegin cmd)
        x <- m
        yieldSeveralTimes   -- see Note [Why yield after the command]
        push varEvents EvEnd
        pure x

sm ::
     IOLike m
  => Maybe UpstreamPeer
  -> Vars m
  -> LedgerStateJudgement
  -> QSM.StateMachine Model Command m Response
sm ub vars j = QSM.StateMachine {
    QSM.cleanup = \_model -> pure ()
  ,
    QSM.generator = generator ub
  ,
    QSM.initModel = initModel j
  ,
    QSM.invariant = Nothing
  ,
    QSM.mock = \model cmd -> pure $ mock model cmd
  ,
    QSM.postcondition = postcondition
  ,
    QSM.precondition = precondition
  ,
    QSM.semantics = semantics vars
  ,
    QSM.shrinker = shrinker
  ,
    QSM.transition = transition
  }

prop_sequential ::
     Maybe UpstreamPeer
  -> LedgerStateJudgement
  -> QC.Property
prop_sequential ub j0 =
    QSM.forAllCommands
        commandArbitrarySM
        mbMinimumCommandLen
        (prop_sequential1 j0)
  where
    mbMinimumCommandLen = Just 20   -- just a guess

    -- NB the monad is irrelevant here but is ambiguous, so we merely ascribe a
    -- convenient concrete one
    commandArbitrarySM = sm ub (undefined :: Vars IO) j0

prop_sequential1 ::
     LedgerStateJudgement
  -> QSM.Commands Command Response
  -> QC.Property
prop_sequential1 j0 cmds = runSimQC $ do
    let s0 = case j0 of
          TooOld      -> GSM.PreSyncing
          YoungEnough -> GSM.CaughtUp

    -- these variables are part of the 'GSM.GsmView'
    varSelection  <- newTVarIO (mSelection $ initModel j0)
    varCandidates <- newTVarIO Map.empty
    varIdlers     <- newTVarIO Set.empty
    varGsmState   <- newTVarIO s0
    varMarker     <- newTVarIO (toMarker s0)

    -- this variable is for better 'QC.counterexample' messages
    varEvents <- newRecorder
    let tracer = Tracer $ push varEvents . EvGsm

    let vars =
            Vars
                varSelection
                varCandidates
                varIdlers
                varGsmState
                varMarker
                varEvents

    let executionSM = sm (Just maxBound) vars j0

        -- NB the specific IO type is unused here
        prettySM = sm undefined undefined j0

    let gsm = GSM.realGsmEntryPoints (id, tracer) GSM.GsmView {
            GSM.antiThunderingHerd = Nothing
          ,
            GSM.candidateOverSelection = candidateOverSelection
          ,
            GSM.durationUntilTooOld = Just durationUntilTooOld
          ,
            GSM.equivalent = (==)   -- unsound, but harmless in this test
          ,
            GSM.getChainSyncCandidates = readTVar varCandidates
          ,
            GSM.getChainSyncIdlers = readTVar varIdlers
          ,
            GSM.getCurrentSelection = readTVar varSelection
          ,
            GSM.minCaughtUpDuration = thrashLimit
          ,
            GSM.setCaughtUpPersistentMark = \b ->
                atomically $ do
                    writeTVar varMarker $ if b then Present else Absent
          ,
            GSM.writeGsmState = \x -> atomically $ do
                writeTVar varGsmState x
          ,
            GSM.isHaaSatisfied =
                isHaaSatisfied . Map.keysSet <$> readTVar varCandidates
          }
        gsmEntryPoint = case j0 of
            TooOld      -> GSM.enterPreSyncing gsm
            YoungEnough -> GSM.enterCaughtUp   gsm

    ((hist, model', res), mbExn) <- id
      $ withAsync gsmEntryPoint
      $ \hGSM -> do

          yieldSeveralTimes   -- see Note [Why yield after the command]

          x <- runCommands' (pure executionSM) cmds

          -- notice if the GSM thread raised an exception while processing the
          -- commands
          poll hGSM <&> \case
              Just Right{}    ->
                  error "impossible! GSM terminated"
              Just (Left exn) ->
                  -- we don't simply rethrow it, since we still want to pretty print
                  -- the command sequence
                  (x, Just exn)
              Nothing         ->
                  (x, Nothing)

    let noExn = case mbExn of
            Nothing  -> QC.property ()
            Just exn -> QC.counterexample (show exn) False

    -- effectively add a 'ReadGsmState' to the end of the command list
    lastCheck <- do
        actual <- semantics vars ReadGsmState
        let expected = mock model' ReadGsmState
        pure $ case (actual, expected) of
            (ReadThisGsmState x, ReadThisGsmState y) ->
                QC.counterexample "lastCheck" $ x QC.=== y
            _ ->
                error "impossible! lastCheck response"

    watcherEvents <- dumpEvents varEvents

    pure
      $ QC.monadicIO
      $ QSM.prettyCommands prettySM hist
      $ QC.counterexample
            (unlines
               $ (:) "WATCHER"
               $ map (("    " <>) . show)
               $ watcherEvents
            )
      $ QC.tabulate
            "Notables"
            (case Set.toList $ mNotables model' of
               []       -> ["<none>"]
               notables -> map show notables
            )
      $ QSM.checkCommandNames cmds
      $ noExn QC..&&. lastCheck QC..&&. res QC.=== QSM.Ok

-----

durationUntilTooOld :: Selection -> IOSim.IOSim s GSM.DurationFromNow
durationUntilTooOld sel = do
    let expiryAge = ageLimit `SI.addTime` onset sel
    now <- SI.getMonotonicTime
    pure $ case compare expiryAge now of
        LT -> GSM.Already
        GT -> GSM.After $ realToFrac $ expiryAge `SI.diffTime` now

        -- 'boringDur' cannot prevent this case. In particular, this case
        -- necessarily arises in the GSM itself during a 'TimePasses' that
        -- incurs a so-called /flicker/ event, in which the anti-thrashing
        -- timer expires and yet the node state at that moment still
        -- _immediately_ indicates that it's CaughtUp. For the specific case of
        -- this test suite, the answer here must be 'GSM.Already'.
        EQ -> GSM.Already

-----

-- | Ensure the GSM thread's transactions quiesce
--
-- I'm unsure how many 'yield's are actually necessary, but ten is both small
-- and also seems likely to suffice.
--
-- Despite the crudeness, this seems much more compositional than invasive
-- explicit synchronization.
yieldSeveralTimes :: MonadFork m => m ()
yieldSeveralTimes = replicateM_ 10 yield

{-

Note [Why yield after the command]

For this 'prop_sequential1' repro

@
   YoungEnough

   Command (NewCandidate Amara (B 1)) Unit []
   Command (StartIdling Amara) Unit []
   Command (TimePasses 61) Unit []
   Command (ExtendSelection (S (-4))) Unit []
   Command ReadMarker (ReadThisMarker Absent) []

   (Command ReadGsmState _ [])   -- this last command is baked into the property
@

If we yield after the command, then both GSM flicker writes happen during the
'ExtendSelection'.

If we yield before the command, then both GSM flicker writes happen during the
'ReadMarker'.

If we don't yield, one write happens during the ReadMarker and the other
happens /between/ 'ReadMarker' and 'ReadGsmState'.

It seems most intuitive for the updates to happen "as part of" the
'ExtendSelection', so I'm favoring yielding after.

And since we're yielding after the command, we should also yield before the
first command, for consistency.

-}

-- | Test the example from the Note [Why yield after the command]
--
-- This property fails when 'yieldSeveralTimes' is removed/redefined to @pure
-- ()@.
prop_yield_regression :: QC.Property
prop_yield_regression =
   QC.once
 $ prop_sequential1 YoungEnough
 $ QSM.Commands
     [ QSM.Command (NewCandidate Amara (B 1)) Unit []
     , QSM.Command (StartIdling Amara) Unit []
     , QSM.Command (TimePasses 61) Unit []
     , QSM.Command (ExtendSelection (S (-4))) Unit []
     , QSM.Command ReadMarker (ReadThisMarker Absent) []
     ]

----- trivial event accumulator, useful for debugging test failures

data Ev =
    EvBegin (Command Concrete)
    -- ^ 'semantics' started stimulating the GSM code being tested
  |
    EvEnd
    -- ^ 'semantics' stopped stimulating the GSM code being tested
  |
    EvGsm (GSM.TraceGsmEvent Selection)
    -- ^ the GSM code being tested emitted an event
  deriving (Show)

newtype EvRecorder m = EvRecorder (StrictTVar m [(SI.Time, Ev)])

newRecorder :: IOLike m => m (EvRecorder m)
newRecorder = EvRecorder <$> newTVarIO []

dumpEvents :: IOLike m => EvRecorder m -> m [(SI.Time, Ev)]
dumpEvents (EvRecorder var) = reverse <$> readTVarIO var

push :: IOLike m => EvRecorder m -> Ev -> m ()
push (EvRecorder var) ev = do
    now <- SI.getMonotonicTime
    atomically $ modifyTVar var $ (:) (now, ev)

-----

-- | merely a tidy bundle of arguments
data Vars m = Vars
    (StrictTVar m Selection)
    (StrictTVar m (Map.Map UpstreamPeer (StrictTVar m Candidate)))
    (StrictTVar m (Set.Set UpstreamPeer))
    (StrictTVar m GSM.GsmState)
    (StrictTVar m MarkerState)
    (EvRecorder m)

-----

-- | a straight-forwardtrivial alias
runSimQC :: (forall s. IOSim.IOSim s QC.Property) -> QC.Property
runSimQC m = case IOSim.runSim m of
    Left  failure -> QC.counterexample (show failure) False
    Right prop    -> prop
