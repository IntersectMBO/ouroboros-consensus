{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Tests for the Genesis State Machine using the [quickcheck-dynamic](https://hackage.haskell.org/package/quickcheck-dynamic) library.
--
--   The instance 'QD.StateModel Model' describes the actions that the model supports,
--   and their semantics in terms for the model and the system-under-test.
--
--   We use the [reflection](https://hackage.haskell.org/package/reflection) library to solve the problem of
--   injecting user-supplied or quickcheck-generated values into the instance of QD.StateModel.
--   See the definition of the 'initialState' method, where we access the reflected configuration, and
--   see 'prop_sequential_iosim' for the construction and reification of the configuration.
module Test.Consensus.GSM (tests) where

import Cardano.Network.Types (LedgerStateJudgement (..))
import Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked
import Control.Exception (SomeException (..))
import qualified Control.Monad as Monad
import Control.Monad.Class.MonadAsync
  ( async
  , poll
  , uninterruptibleCancel
  )
import Control.Monad.Class.MonadFork (MonadFork, yield)
import Control.Monad.Class.MonadSTM
import qualified Control.Monad.Class.MonadTime.SI as SI
import qualified Control.Monad.Class.MonadTimer.SI as SI
import qualified Control.Monad.IOSim as IOSim
import Control.Monad.Reader
import Control.Tracer (Tracer (Tracer))
import Data.Functor ((<&>))
import Data.List ((\\))
import qualified Data.Map.Strict as Map
import qualified Data.Reflection as Reflection
import qualified Data.Set as Set
import Data.Time (diffTimeToPicoseconds)
import qualified Data.TreeDiff as TD
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Ouroboros.Consensus.Node.GSM as GSM
import Ouroboros.Consensus.Util.IOLike (IOLike)
import Test.QuickCheck (elements, shrink)
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Gen.Unsafe (Capture (Capture), capture)
import qualified Test.QuickCheck.Monadic as QC
import qualified Test.QuickCheck.StateModel as QD
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Orphans.IOLike ()
import Test.Util.Orphans.ToExpr ()
import Test.Util.TestEnv (adjustQuickCheckTests)
import Test.Util.ToExpr ()
import qualified Text.PrettyPrint as Pretty

tests :: [TestTree]
tests = adhoc <> core
 where
  adhoc =
    [ testProperty "GSM yield regression" prop_yield_regression
    ]
  core =
    [ adjustQuickCheckTests (* 10) $
        testProperty ("GSM (" <> coreTestName upstreamPeerBound <> ")") $
          prop_sequential_iosim upstreamPeerBound
    | upstreamPeerBound <- Nothing : map Just [minBound .. maxBound :: UpstreamPeer]
    ]

  coreTestName = \case
    Nothing -> "no peers"
    Just ub ->
      "at most " <> case fromEnum ub of
        0 -> "1 peer"
        n -> show (n + 1) <> " peers"

-- | Test the example from the Note [Why yield after the command]
--
-- This property fails when 'yieldSeveralTimes' is removed/redefined to @pure
-- ()@.
prop_yield_regression :: QC.Property
prop_yield_regression =
  Reflection.give
    ( StaticParams
        { pUpstreamPeerBound = Nothing
        , pInitialJudgement = YoungEnough
        , pIsHaaSatisfied = const True
        }
    )
    $ QC.once
    $ runIOSimProp
    $ prop_sequential_iosim1 yieldRegressionActions
 where
  yieldRegressionActions :: QD.Actions Model
  yieldRegressionActions =
    (wrapPositiveAction $ NewCandidate Amara (B 1))
      <> (wrapPositiveAction $ StartIdling Amara)
      <> (wrapPositiveAction $ TimePasses 61)
      <> (wrapPositiveAction $ ExtendSelection (S (-4)))
      <> (wrapPositiveAction $ ReadMarker)

prop_sequential_iosim ::
  Maybe UpstreamPeer -> LedgerStateJudgement -> QC.Fun (Set.Set UpstreamPeer) Bool -> QC.Property
prop_sequential_iosim pUpstreamPeerBound pInitialJudgement (QC.Fun _ pIsHaaSatisfied) =
  Reflection.give
    ( StaticParams
        { pUpstreamPeerBound
        , pInitialJudgement
        , pIsHaaSatisfied
        }
    )
    -- NOTE: the actions have to be generated within the application of 'Reflection.give',
    -- as 'generator' relies on reflection for access to the parameters.
    $ QC.property
    $ \actions ->
      runIOSimProp $ prop_sequential_iosim1 actions

setupGsm ::
  (Set.Set UpstreamPeer -> Bool) ->
  SystemStateVars (IOSim.IOSim s) ->
  GSM.GsmEntryPoints (IOSim.IOSim s)
setupGsm isHaaSatisfied vars = do
  let tracer = Tracer $ push varEvents . EvGsm
  GSM.realGsmEntryPoints
    (id, tracer)
    GSM.GsmView
      { GSM.antiThunderingHerd = Nothing
      , GSM.getCandidateOverSelection = pure $ \s (PeerState c _) ->
          candidateOverSelection s c
      , GSM.peerIsIdle = isIdling
      , GSM.durationUntilTooOld = Just durationUntilTooOld
      , GSM.equivalent = (==) -- unsound, but harmless in this test
      , GSM.getPeerStates = traverse readTVar =<< readTVar varStates
      , GSM.getCurrentSelection = readTVar varSelection
      , GSM.minCaughtUpDuration = thrashLimit
      , GSM.setCaughtUpPersistentMark = \b ->
          atomically $ do
            writeTVar varMarker $ if b then Present else Absent
      , GSM.writeGsmState = \x -> atomically $ do
          writeTVar varGsmState x
      , GSM.isHaaSatisfied =
          isHaaSatisfied . Map.keysSet <$> readTVar varStates
      }
 where
  SystemStateVars{varSelection, varStates, varGsmState, varMarker, varEvents} = vars

prop_sequential_iosim1 ::
  forall s.
  Reflection.Given StaticParams =>
  QD.Actions Model ->
  QC.PropertyM (RunMonad (IOSim.IOSim s)) QC.Property
prop_sequential_iosim1 actions = do
  vars@SystemStateVars{varEvents} <- lift ask

  let StaticParams{pInitialJudgement, pIsHaaSatisfied} = Reflection.given

  -- initialise the SUT state using the model params
  lift . lift $ initVars pInitialJudgement vars

  let model = QD.initialState
  let gsm = setupGsm pIsHaaSatisfied vars
      gsmEntryPoint = case pInitialJudgement of
        TooOld -> GSM.enterPreSyncing gsm
        YoungEnough -> GSM.enterCaughtUp gsm

  -- start the GSM
  hGSM <- lift . lift $ async gsmEntryPoint

  lift . lift $ yieldSeveralTimes

  (metadata, mbExn) <- do
    (metadata, _env) <- QD.runActions actions
    (lift . lift $ poll hGSM) <&> \case
      Just Right{} ->
        error "impossible! GSM terminated"
      Just (Left exn) ->
        -- we don't simply rethrow it, since we still want to pretty print
        -- the command sequence
        (metadata, Just exn)
      Nothing ->
        (metadata, Nothing)

  -- stop the GSM
  _ <- lift . lift $ uninterruptibleCancel hGSM

  let modelStateAfterActions = QD.underlyingState metadata

  let noExn = case mbExn of
        Nothing -> QC.property ()
        Just exn -> QC.counterexample (show exn) False

  -- effectively add a 'ReadGsmState' to the end of the command list
  let readFinalStateAction = ReadGsmState
  (finalStateCheck, finalModelState) <-
    takeActionInBoth
      ("Difference after final action " <> show readFinalStateAction)
      modelStateAfterActions
      readFinalStateAction

  watcherEvents <- lift . lift $ dumpEvents varEvents

  pure
    $ QC.counterexample
      ( unlines $
          (:) "WATCHER" $
            map (("    " <>) . show) $
              watcherEvents
      )
    $ QC.counterexample
      ( ("ediff Initial Final " <>)
          . Pretty.render
          . TD.prettyEditExpr
          $ TD.ediff model finalModelState
      )
    $ QC.tabulate
      "Notables"
      ( case Set.toList $ mNotables finalModelState of
          [] -> ["<none>"]
          notables -> map show notables
      )
      noExn
      QC..&&. finalStateCheck

---

-- | The parameters needed by 'Model' and the 'StateModel' instance.
data StaticParams = StaticParams
  { pUpstreamPeerBound :: Maybe UpstreamPeer
  , pInitialJudgement :: LedgerStateJudgement
  , pIsHaaSatisfied :: Set.Set UpstreamPeer -> Bool
  }

-- | The model state
data Model = Model
  { mCandidates :: Map.Map UpstreamPeer Candidate
  , mClock :: SI.Time
  , mIdlers :: Set.Set UpstreamPeer
  , mNotables :: Set.Set Notable
  , mPrev :: WhetherPrevTimePasses
  , mSelection :: Selection
  , mState :: ModelState
  }
  deriving (Show, Generic)
  deriving anyclass TD.ToExpr

-- | Initialise the 'Model' state from 'StaticParams'
initModel :: Reflection.Given StaticParams => Model
initModel =
  Model
    { mCandidates = Map.empty
    , mClock = SI.Time 0
    , mIdlers = idlers
    , mNotables = Set.empty
    , mPrev = WhetherPrevTimePasses True
    , mSelection = initialSelection pInitialJudgement
    , mState = case pInitialJudgement of
        TooOld
          | pIsHaaSatisfied idlers -> ModelSyncing
          | otherwise -> ModelPreSyncing
        YoungEnough -> ModelCaughtUp (SI.Time (-10000))
    }
 where
  idlers = Set.empty

  StaticParams{pIsHaaSatisfied, pInitialJudgement} = Reflection.given

-- | The 'StaticParams' are supplied through reflection because this seems to be
--   the cleanest way to pass static configuration needed by the 'initState' and
--   'arbitraryAction' methods
instance Reflection.Given StaticParams => QD.StateModel Model where
  data Action Model a where
    Disconnect :: UpstreamPeer -> QD.Action Model ()
    -- \^ INVARIANT must be an existing peer
    --
    -- Mocks the necessary ChainSync client behavior.
    ExtendSelection :: S -> QD.Action Model ()
    -- \^ INVARIANT 'selectionIsBehind'
    --
    -- NOTE Harmless to assume it only advances by @'B' 1@ at a time.
    ModifyCandidate :: UpstreamPeer -> B -> QD.Action Model ()
    -- \^ INVARIANT existing peer
    --
    -- Mocks the necessary ChainSync client behavior.
    NewCandidate :: UpstreamPeer -> B -> QD.Action Model ()
    -- \^ INVARIANT new peer
    --
    -- Mocks the necessary ChainSync client behavior.z
    ReadGsmState :: QD.Action Model GSM.GsmState
    ReadMarker :: QD.Action Model MarkerState
    StartIdling :: UpstreamPeer -> QD.Action Model ()
    -- \^ INVARIANT existing peer, not idling
    TimePasses :: Int -> QD.Action Model ()

  -- \^ tenths of a second
  --
  -- INVARIANT positive
  --
  -- INVARIANT does not end /exactly/ on an interesting time point; see
  -- 'boringDur'.
  --
  -- NOTE The generator does not yield consecutive 'TimePasses' commands,
  -- though shrinking might.
  precondition = precondition

  arbitraryAction _ctx model = generator model

  shrinkAction _ctx model = shrinker model

  initialState = initModel

  nextState model action _ = transition model action

deriving instance Show (QD.Action Model a)
deriving instance Eq (QD.Action Model a)

instance QD.HasVariables SI.DiffTime where
  getAllVariables _ = mempty

instance QD.HasVariables (QD.Action Model a) where
  getAllVariables _ = mempty

instance QD.HasVariables Model where
  getAllVariables _ = mempty

--- System Under Test

-- | Trivial event accumulator, useful for debugging test failures
data Ev
  = -- | 'perform' started stimulating the GSM code being tested
    forall a. EvBegin (QD.Action Model a)
  | -- | 'perform' stopped stimulating the GSM code being tested
    EvEnd
  | -- | the GSM code being tested emitted an event
    EvGsm (GSM.TraceGsmEvent Selection)

deriving instance Show Ev

newtype EvRecorder m = EvRecorder (StrictTVar m [(SI.Time, Ev)])

newRecorder :: IOLike m => m (EvRecorder m)
newRecorder = EvRecorder <$> newTVarIO []

dumpEvents :: IOLike m => EvRecorder m -> m [(SI.Time, Ev)]
dumpEvents (EvRecorder var) = reverse <$> readTVarIO var

push :: IOLike m => EvRecorder m -> Ev -> m ()
push (EvRecorder var) ev = do
  now <- SI.getMonotonicTime
  atomically $ modifyTVar var $ (:) (now, ev)

-- | The state of the system under test
data SystemStateVars m
  = SystemStateVars
  { varSelection :: (StrictTVar m Selection)
  , varStates :: (StrictTVar m (Map.Map UpstreamPeer (StrictTVar m PeerState)))
  , varGsmState :: (StrictTVar m GSM.GsmState)
  , varMarker :: (StrictTVar m MarkerState)
  , varEvents :: (EvRecorder m)
  }

setupVars :: IOLike m => LedgerStateJudgement -> m (SystemStateVars m)
setupVars initStateInitialJudgement = do
  let initialGsmState = case initStateInitialJudgement of
        TooOld -> GSM.PreSyncing
        YoungEnough -> GSM.CaughtUp

  -- these variables are part of the 'GSM.GsmView'
  varSelection <- newTVarIO (initialSelection initStateInitialJudgement)
  varStates <- newTVarIO Map.empty
  varGsmState <- newTVarIO initialGsmState
  varMarker <- newTVarIO (toMarker initialGsmState)

  -- this variable is for better 'QC.counterexample' messages
  varEvents <- newRecorder

  pure $
    SystemStateVars{varSelection, varStates, varGsmState, varMarker, varEvents}

-- | Initialise the values in 'SystemStateVars' based on 'StaticParams'
initVars :: IOLike m => LedgerStateJudgement -> SystemStateVars m -> m ()
initVars initialJudgement vars = do
  let SystemStateVars
        { varSelection
        , varStates
        , varGsmState
        , varMarker
        } = vars
  atomically $ do
    let initialGsmState = case initialJudgement of
          TooOld -> GSM.PreSyncing
          YoungEnough -> GSM.CaughtUp
    writeTVar varSelection $ (initialSelection initialJudgement)
    writeTVar varStates $ Map.empty
    writeTVar varGsmState $ initialGsmState
    writeTVar varMarker $ (toMarker initialGsmState)

newtype RunMonad m a = RunMonad {runMonad :: ReaderT (SystemStateVars m) m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (SystemStateVars m))

instance MonadTrans RunMonad where
  lift = RunMonad . lift

instance (IOLike m, Reflection.Given StaticParams) => QD.RunModel Model (RunMonad m) where
  perform _ action _ = do
    SystemStateVars{varSelection, varStates, varGsmState, varMarker, varEvents} <- ask
    let
      pre :: m a -> RunMonad m a
      pre m = lift $ do
        push varEvents (EvBegin action)
        x <- m
        yieldSeveralTimes -- see Note [Why yield after the command]
        push varEvents EvEnd
        pure x

    pre $ case action of
      Disconnect peer -> do
        atomically $ do
          modifyTVar varStates $ Map.delete peer
        pure ()
      ExtendSelection sdel -> do
        atomically $ do
          Selection b s <- readTVar varSelection
          writeTVar varSelection $! Selection (b + 1) (s + sdel)
        pure ()
      ModifyCandidate peer bdel -> do
        atomically $ do
          v <- (Map.! peer) <$> readTVar varStates
          Candidate b <- psCandidate <$> readTVar v
          writeTVar v $! PeerState (Candidate (b + bdel)) (Idling False)

        pure ()
      NewCandidate peer bdel -> do
        atomically $ do
          Selection b _s <- readTVar varSelection
          v <- newTVar $! PeerState (Candidate (b + bdel)) (Idling False)
          modifyTVar varStates $ Map.insert peer v
        pure ()
      ReadGsmState -> do
        atomically $ readTVar varGsmState
      ReadMarker -> do
        atomically $ readTVar varMarker
      StartIdling peer -> atomically $ do
        v <- (Map.! peer) <$> readTVar varStates
        modifyTVar v $ \(PeerState c _) -> PeerState c (Idling True)
        pure ()
      TimePasses dur -> do
        SI.threadDelay (0.1 * fromIntegral dur)
        pure ()

---

runIOSimProp ::
  QC.Testable a => (forall s. QC.PropertyM (RunMonad (IOSim.IOSim s)) a) -> QC.Property
runIOSimProp p =
  QC.property $ runRunMonadIOSimGen (QC.monadic' p)
 where
  -- Borrowed from Hydra
  runRunMonadIOSimGen ::
    forall a.
    QC.Testable a =>
    (forall s. QC.Gen ((RunMonad (IOSim.IOSim s)) a)) ->
    QC.Gen QC.Property
  runRunMonadIOSimGen f = do
    Capture eval <- capture
    let tr = IOSim.runSimTrace (sim f eval)
    return $
      case IOSim.traceResult False tr of
        Right a -> QC.property a
        Left (IOSim.FailureException (SomeException ex)) ->
          QC.counterexample (show ex) False
        Left ex ->
          QC.counterexample (show ex) False

  sim ::
    forall s a.
    (QC.Gen ((RunMonad (IOSim.IOSim s)) a)) ->
    (QC.Gen (RunMonad (IOSim.IOSim s) a) -> (RunMonad (IOSim.IOSim s) a)) ->
    IOSim.IOSim s a
  sim f eval = do
    vars <- setupVars TooOld
    runReaderT (runMonad (eval f)) vars

-- | Execute an action in both the model and the system under test, returning
--   the modified model and a property that checks that the action brought
--   the model and the SUT into the same state.
takeActionInBoth ::
  (IOLike m, Reflection.Given StaticParams) =>
  String ->
  Model ->
  QD.Action Model GSM.GsmState ->
  QC.PropertyM (RunMonad m) (QC.Property, Model)
takeActionInBoth conterexampleMessage model action = do
  -- run the action in the model
  let expected = QD.nextState model action impossibleError
  -- run the action in the real system under test
  actual <- lift $ QD.perform model action impossibleError
  -- check that the states are the same after
  pure
    ( QC.counterexample conterexampleMessage $
        ((toGsmState . mState $ expected) QC.=== actual)
    , expected
    )
 where
  impossibleError = error "Impossible: unused argument"

-- | Wrap an 'Action' into singleton 'Actions'.
--   This function is useful for constructing adhoc test scenarios,
--   see 'prop_yield_regression' for an example.
--
--   Note: this may break some invariant related to Variables,
--         but we do not care about Variables in these tests.
wrapPositiveAction ::
  ( Show (QD.Action state a)
  , Eq (QD.Action state a)
  , Typeable a
  ) =>
  QD.Action state a ->
  QD.Actions state
wrapPositiveAction action =
  QD.Actions [(QD.:=) dummyVar (QD.ActionWithPolarity action QD.PosPolarity)]

-- | See 'boringDurImpl'
boringDur :: Model -> Int -> Bool
boringDur model = boringDurImpl clk sel st
 where
  Model
    { mClock = clk
    , mSelection = sel
    , mState = st
    } = model

addNotableWhen :: Notable -> Bool -> Model -> Model
addNotableWhen n b model =
  if not b
    then model
    else
      model{mNotables = n `Set.insert` mNotables model}

selectionIsBehind :: Model -> Bool
selectionIsBehind model =
  any (\(Candidate b') -> b' > b) cands
 where
  Model
    { mCandidates = cands
    , mSelection = Selection b _s
    } = model

selectionIsNotEarly :: Model -> Bool
selectionIsNotEarly model =
  onset sel <= clk
 where
  Model
    { mClock = clk
    , mSelection = sel
    } = model

initialSelection :: LedgerStateJudgement -> Selection
initialSelection initStateInitialJudgement = Selection 0 s
 where
  s = S $ case initStateInitialJudgement of
    TooOld -> (-11)
    YoungEnough -> 0

---

dummyVar :: QD.Var a
dummyVar = QD.mkVar 0

--- Definitions used in the 'ModelState' instance

precondition ::
  forall a.
  Reflection.Given StaticParams =>
  Model -> QD.Action Model a -> Bool
precondition model = \case
  cmd@ExtendSelection{} ->
    let model' = QD.nextState model cmd dummyVar
     in selectionIsBehind model
          && selectionIsNotEarly model'
          && boringDur model' 0
  Disconnect peer ->
    peer `Map.member` cands
  ModifyCandidate peer _bdel ->
    peer `Map.member` cands
  NewCandidate peer _bdel ->
    peer `Map.notMember` cands
  ReadGsmState ->
    True
  ReadMarker ->
    True
  StartIdling peer ->
    peer `Map.member` cands
      && peer `Set.notMember` idlers
  TimePasses dur ->
    0 < dur
      && boringDur model dur
 where
  Model
    { mCandidates = cands
    , mIdlers = idlers
    } = model

generator ::
  Reflection.Given StaticParams =>
  Model -> QC.Gen (QD.Any (QD.Action Model))
generator model =
  QC.frequency $
    [(,) 5 $ QD.Some . Disconnect <$> QC.elements old | notNull old]
      <> [ (,) 10 $ QD.Some . ExtendSelection <$> QC.elements sdels
         | notNull sdels
         , selectionIsBehind model -- NB harmless to assume this node never mints
         ]
      <> [ (,) 20 $ do
             (peer, bdel) <- QC.elements bdels
             QD.Some . ModifyCandidate peer <$> QC.elements bdel
         | notNull bdels
         ]
      <> [ (,) 100 $
             QD.Some
               <$> ( NewCandidate
                       <$> QC.elements new
                       <*> (B <$> QC.choose (-10, 10))
                   )
         | notNull new
         ]
      <> [(,) 20 $ pure . QD.Some $ ReadGsmState]
      <> [(,) 20 $ pure . QD.Some $ ReadMarker]
      <> [(,) 50 $ QD.Some . StartIdling <$> QC.elements oldNotIdling | notNull oldNotIdling]
      <> [(,) 100 $ QD.Some . TimePasses <$> genTimePassesDur | prev == WhetherPrevTimePasses False]
 where
  Model
    { mCandidates = cands
    , mClock = clk
    , mIdlers = idlers
    , mPrev = prev
    , mSelection = sel
    } = model

  StaticParams{pUpstreamPeerBound = ub} = Reflection.given

  notNull :: [a] -> Bool
  notNull = not . null

  old = Map.keys cands

  new = case ub of
    Nothing -> []
    Just peer -> [minBound .. peer] \\ old

  oldNotIdling = old \\ Set.toList idlers

  genTimePassesDur =
    QC.frequency $
      [(,) 10 $ QC.choose (1, 70)]
        <> [ (,) 1 $ QC.choose (300, 600)
           | case mState model of
               ModelCaughtUp{} -> True
               ModelPreSyncing{} -> False
               ModelSyncing{} -> False
           ]

  -- sdels that would not cause the selection to be in the future
  sdels =
    let Selection b s = sel
     in [ sdel
        | sdel <- map S [-4 .. 10]
        , 0 /= sdel
        , onset (Selection b (s + sdel)) <= clk
        ]

  -- bdels that keep the candidates' lengths near the selection
  bdels =
    let Selection b _s = sel
        lim = 3
     in [ (,)
            peer
            (filter (/= 0) [b + offset - c | offset <- [-lim .. lim]])
        | (peer, Candidate c) <- Map.assocs cands
        ]

shrinker :: Model -> QD.Action Model a -> [QD.Any (QD.Action Model)]
shrinker _model = \case
  Disconnect{} ->
    []
  ExtendSelection sdel ->
    [QD.Some $ ExtendSelection sdel' | sdel' <- shrinkS sdel]
  ModifyCandidate peer bdel ->
    [QD.Some $ ModifyCandidate peer bdel' | bdel' <- shrinkB bdel, bdel' /= 0]
  NewCandidate peer bdel ->
    [QD.Some $ NewCandidate peer bdel' | bdel' <- shrinkB bdel, bdel' /= 0]
  ReadGsmState ->
    []
  ReadMarker ->
    []
  StartIdling{} ->
    []
  TimePasses dur ->
    [QD.Some $ TimePasses dur' | dur' <- QC.shrink dur, 0 < dur']
 where
  shrinkB (B x) = [B x' | x' <- QC.shrink x]
  shrinkS (S x) = [S x' | x' <- QC.shrink x]

transition ::
  forall a.
  Reflection.Given StaticParams =>
  Model -> QD.Action Model a -> Model
transition model cmd =
  fixupModelState cmd $
    case cmd of
      Disconnect peer ->
        model'
          { mCandidates = Map.delete peer cands
          , mIdlers = Set.delete peer idlers
          }
      ExtendSelection sdel ->
        model'{mSelection = Selection (b + 1) (s + sdel)}
      ModifyCandidate peer bdel ->
        model'
          { mCandidates = Map.insertWith plusC peer (Candidate bdel) cands
          , mIdlers = Set.delete peer idlers
          }
      NewCandidate peer bdel ->
        model'{mCandidates = Map.insert peer (Candidate (b + bdel)) cands}
      ReadGsmState ->
        model'
      ReadMarker ->
        model'
      StartIdling peer ->
        model'{mIdlers = Set.insert peer idlers}
      TimePasses dur ->
        addNotableWhen BigDurN (dur > 300) $
          model
            { mClock = SI.addTime (0.1 * fromIntegral dur) clk
            , mPrev = WhetherPrevTimePasses True
            }
 where
  Model
    { mCandidates = cands
    , mClock = clk
    , mIdlers = idlers
    , mSelection = Selection b s
    } = model

  model' :: Model
  model' = model{mPrev = WhetherPrevTimePasses False}

  plusC (Candidate x) (Candidate y) = Candidate (x + y)

-- | Update the 'mState', assuming that's the only stale field in the given
-- 'Model'
fixupModelState ::
  forall a.
  Reflection.Given StaticParams =>
  QD.Action Model a -> Model -> Model
fixupModelState cmd model =
  case st of
    ModelPreSyncing
      | haaSatisfied ->
          avoidTransientState $
            addNotableWhen PreSyncingToSyncingN True $
              model{mState = ModelSyncing}
      | otherwise ->
          model
    ModelSyncing
      | not haaSatisfied ->
          addNotableWhen SyncingToPreSyncingN True $
            model{mState = ModelPreSyncing}
      | caughtUp ->
          -- ASSUMPTION This new state was /NOT/ incurred by the 'TimePasses'
          -- command.
          --
          -- Therefore the current clock is necessarily the correct timestamp
          -- to record.
          addNotableWhen CaughtUpN True $
            model{mState = ModelCaughtUp clk}
      | otherwise ->
          model
    ModelCaughtUp timestamp
      | flicker timestamp ->
          addNotableWhen FlickerN True $
            model{mState = ModelCaughtUp (flickerTimestamp timestamp)}
      | fellBehind timestamp ->
          avoidTransientState $
            addNotableWhen FellBehindN True $
              model{mState = ModelPreSyncing}
      | otherwise ->
          -- NB in this branch, these notables are mutually exclusive
          addNotableWhen TooOldN (expiryAge < clk)
            $ addNotableWhen
              NotThrashingN
              (SI.Time 0 < timestamp && expiryThrashing timestamp < clk)
            $ model
 where
  Model
    { mCandidates = cands
    , mClock = clk
    , mIdlers = idlers
    , mSelection = sel
    , mState = st
    } = model

  StaticParams{pIsHaaSatisfied} = Reflection.given

  haaSatisfied = pIsHaaSatisfied $ Map.keysSet cands
  caughtUp = some && allIdling && all ok cands
  fellBehind timestamp = expiry timestamp < clk -- NB 'boringDur' prevents ==
  flicker timestamp = fellBehind timestamp && caughtUp && haaSatisfied

  some = 0 < Map.size cands

  allIdling = idlers == Map.keysSet cands

  ok cand =
    GSM.WhetherCandidateIsBetter False == candidateOverSelection sel cand

  -- the first time the node would transition to PreSyncing
  expiry timestamp = expiryAge `max` expiryThrashing timestamp
  expiryAge = SI.addTime ageLimit (onset sel)
  expiryThrashing timestamp = SI.addTime thrashLimit timestamp

  -- It's possible for the node to instantly return to CaughtUp, but that
  -- might have happened /during/ the 'TimePasses' command, not only when it
  -- ends.
  --
  -- Therefore the age limit of the selection is the correct timestamp to
  -- record, instead of the current clock (ie when the 'TimePasses' ended).
  --
  -- NOTE Superficially, in the real implementation, the Diffusion Layer
  -- should be discarding all peers when transitioning from CaughtUp to
  -- PreSyncing. However, it would be plausible for an implementation to
  -- retain any bootstrap/ledger peers it happened to have, so the
  -- idiosyncratic behavior of the system under test in this module is not
  -- totally irrelevant.
  --
  -- the /last/ time the node instantaneously visited PreSyncing during the
  -- 'TimePasses' command, assuming it did so at least once
  flickerTimestamp timestamp = case cmd of
    ExtendSelection sdel
      | sdel < 0 ->
          clk
    TimePasses{} ->
      foldl max (expiry timestamp) $
        takeWhile (< clk) $ -- NB 'boringDur' prevents ==
          iterate (SI.addTime thrashLimit) (expiry timestamp)
    _ ->
      error $
        "impossible! flicker but neither "
          <> "negative ExtendSelection nor TimePasses: "
          <> show cmd

  avoidTransientState :: Model -> Model
  avoidTransientState = fixupModelState cmd

-- | A block count
newtype B = B Int
  deriving stock (Eq, Ord, Generic, Read, Show)
  deriving newtype (Enum, Num)
  deriving anyclass TD.ToExpr

-- | A slot count
newtype S = S Int
  deriving stock (Eq, Ord, Generic, Read, Show)
  deriving newtype (Enum, Num)
  deriving anyclass TD.ToExpr

data UpstreamPeer = Amara | Bao | Cait | Dhani | Eric
  deriving stock (Bounded, Enum, Eq, Ord, Generic, Read, Show)
  deriving anyclass (TD.ToExpr, QC.CoArbitrary, QC.Function)

-- | The cumulative growth relative to whatever length the initial selection
-- was and the slot relative to the start of the test (which is assumed to be
-- the exact onset of some slot)
data Selection = Selection !B !S
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass TD.ToExpr

-- | The age of the candidate is irrelevant, only its length matters
newtype Candidate = Candidate B
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass TD.ToExpr

data MarkerState = Present | Absent
  deriving stock (Eq, Ord, Generic, Read, Show)
  deriving anyclass TD.ToExpr

newtype WhetherPrevTimePasses = WhetherPrevTimePasses Bool
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass TD.ToExpr

data ModelState
  = ModelPreSyncing
  | ModelSyncing
  | -- | when the model most recently transitioned to 'GSM.CaughtUp'.
    ModelCaughtUp !SI.Time
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass TD.ToExpr

-- | Interesting events to record /within the model/
--
-- TODO some less superficial ones (eg even just combinations of these)
data Notable
  = -- | there was a "big" 'TimesPasses' command
    BigDurN
  | -- | the node transitioned from Syncing to CaughtUp
    CaughtUpN
  | -- | the node transitioned from CaughtUp to PreSyncing
    FellBehindN
  | -- | the node transition from Syncing to PreSyncing
    SyncingToPreSyncingN
  | -- | the node transition from PreSyncing to Syncing
    PreSyncingToSyncingN
  | -- | the node transitioned from CaughtUp to PreSyncing to Syncing and back
    -- to CaughtUp "instantly"
    FlickerN
  | -- | the anti-thrashing would have allowed 'FellBehindN', but the selection
    -- wasn't old enough
    NotThrashingN
  | -- | the selection was old enough for 'FellBehindN', but the anti-thrashing
    -- prevented it
    TooOldN
  deriving (Eq, Ord, Show)

instance TD.ToExpr Notable where toExpr = TD.defaultExprViaShow

---

newtype Idling = Idling Bool
  deriving (Eq, Ord, Show)

data PeerState = PeerState {psCandidate :: !Candidate, psIdling :: !Idling}
  deriving (Eq, Ord, Show)

isIdling :: PeerState -> Bool
isIdling (PeerState{psIdling = Idling i}) = i

-- | Ensure the GSM thread's transactions quiesce
--
-- I'm unsure how many 'yield's are actually necessary, but ten is both small
-- and also seems likely to suffice.
--
-- Despite the crudeness, this seems much more compositional than invasive
-- explicit synchronization.
yieldSeveralTimes :: MonadFork m => m ()
yieldSeveralTimes = Monad.replicateM_ 10 yield

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

----- orphans

deriving instance Read LedgerStateJudgement

deriving anyclass instance TD.ToExpr LedgerStateJudgement

instance QC.Arbitrary LedgerStateJudgement where
  arbitrary = elements [TooOld, YoungEnough]
  shrink = \case
    TooOld -> [YoungEnough]
    YoungEnough -> []

instance QC.Arbitrary MarkerState where
  arbitrary = elements [Absent, Present]
  shrink = \case
    Absent -> [Present]
    Present -> []

-----

toGsmState :: ModelState -> GSM.GsmState
toGsmState = \case
  ModelPreSyncing -> GSM.PreSyncing
  ModelSyncing -> GSM.Syncing
  ModelCaughtUp{} -> GSM.CaughtUp

toMarker :: GSM.GsmState -> MarkerState
toMarker = \case
  GSM.PreSyncing -> Absent
  GSM.Syncing -> Absent
  GSM.CaughtUp -> Present

-----

onset :: Selection -> SI.Time
onset (Selection _b (S s)) = SI.Time $ fromIntegral s

ageLimit :: Num a => a
ageLimit = 10 -- seconds

thrashLimit :: Num a => a
thrashLimit = 8 -- seconds

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

candidateOverSelection ::
  Selection ->
  Candidate ->
  GSM.CandidateVersusSelection
candidateOverSelection (Selection b _s) (Candidate b') =
  -- TODO this ignores CandidateDoesNotIntersect, which seems harmless, but
  -- I'm not quite sure
  GSM.WhetherCandidateIsBetter (b < b')

-- | Checks that a 'TimePasses' command does not end exactly when a timeout
-- could fire and that a 'ExtendSelection' does not incur a timeout that would
-- fire immediately
--
-- This insulates the test from race conditions that are innocuous in the real
-- world.
boringDurImpl :: SI.Time -> Selection -> ModelState -> Int -> Bool
boringDurImpl clk sel st dur =
  boringSelection && boringState
 where
  -- the first time the node would transition to PreSyncing
  expiry timestamp = expiryAge `max` expiryThrashing timestamp
  expiryAge = SI.addTime ageLimit (onset sel)
  expiryThrashing timestamp = SI.addTime thrashLimit timestamp

  clk' = SI.addTime (0.1 * fromIntegral dur) clk

  boringSelection = clk' /= expiryAge

  boringState = case st of
    ModelPreSyncing -> True
    ModelSyncing -> True
    ModelCaughtUp timestamp ->
      let gap = clk' `SI.diffTime` expiry timestamp
          n =
            mod
              (diffTimeToPicoseconds gap)
              (secondsToPicoseconds thrashLimit)
       in gap < 0 || 0 /= n

  secondsToPicoseconds x = x * 10 ^ (12 :: Int)
