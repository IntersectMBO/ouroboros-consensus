{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Tests for the Genesis State Machine using the quickcheck-dynamic library

module Test.Consensus.GSM.QD (tests) where

import           Data.Maybe (fromMaybe)
import           Control.Exception (SomeException (..))
import           Test.QuickCheck.Gen.Unsafe (Capture (Capture), capture)
import           Control.Monad.Reader
import           Data.List ((\\))
import           Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked
import           Control.Monad.Class.MonadAsync (poll, async)
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
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import           Test.Tasty (TestTree)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestEnv (adjustQuickCheckTests)
import           Test.Util.ToExpr ()
import qualified Test.QuickCheck.StateModel as QD
import           Test.Util.Orphans.ToExpr ()

import Test.Consensus.GSM.Common

data Model = Model {
    mCandidates :: Map.Map UpstreamPeer Candidate
  ,
    mClock      :: SI.Time
  ,
    mIdlers     :: Set.Set UpstreamPeer
  ,
    mNotables   :: Set.Set Notable
  ,
    mPrev       :: WhetherPrevTimePasses
  ,
    mSelection  :: Selection
  ,
    mState      :: ModelState
  ,
  -- these fields are immutable
    mInitialJudgement  :: Maybe LedgerStateJudgement
  ,
    mIsHaaSatisfied    :: Maybe (Opaque (Set.Set UpstreamPeer -> Bool))
  ,
    mUpstreamPeerBound :: Maybe UpstreamPeer
  }
  deriving (Show)

initialSelection :: Maybe LedgerStateJudgement -> Selection
initialSelection initStateInitialJudgement = Selection 0 s
  where s = S $ case initStateInitialJudgement of
                       Nothing          -> (-11)
                       Just TooOld      -> (-11)
                       Just YoungEnough -> 0

deriving instance Show (QD.Action Model a)
deriving instance Eq (QD.Action Model a)

instance QD.HasVariables SI.DiffTime where
  getAllVariables _ = mempty

instance QD.HasVariables (Opaque (Set.Set UpstreamPeer -> Bool)) where
  getAllVariables _ = mempty

instance QD.HasVariables (QD.Action Model a) where
  getAllVariables _ = mempty

instance QD.HasVariables Model where
  getAllVariables _ = mempty

addNotableWhen :: Notable -> Bool -> Model -> Model
addNotableWhen n b model =
    if not b then model else
    model { mNotables = n `Set.insert` mNotables model }

----

precondition :: Model-> QD.Action Model a -> Bool
precondition model = \case
    InitState{} -> False -- this event must never be generated
    cmd@ExtendSelection{} ->
        let model' = QD.nextState model cmd (error "precondition: not Var ()")
         in
            selectionIsBehind model
         &&
            selectionIsNotEarly model'
         &&
            boringDur model' 0
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
     &&
        peer `Set.notMember` idlers
    TimePasses dur ->
        0 < dur
     &&
        boringDur model dur
    where
      Model {
          mCandidates = cands
        ,
          mIdlers = idlers
        } = model

selectionIsBehind :: Model -> Bool
selectionIsBehind model =
    any (\(Candidate b') -> b' > b) cands
  where
    Model {
        mCandidates = cands
      ,
        mSelection = Selection b _s
      } = model

selectionIsNotEarly :: Model -> Bool
selectionIsNotEarly model =
    onset sel <= clk
  where
    Model {
        mClock = clk
      ,
        mSelection = sel
      } = model

generator :: Model -> QC.Gen (QD.Any (QD.Action Model))
generator model = QC.frequency $
    [ (,) 5 $  QD.Some . Disconnect <$> QC.elements old | notNull old ]
 <>
    [ (,) 10 $ QD.Some . ExtendSelection <$> QC.elements sdels
    | notNull sdels
    , selectionIsBehind model   -- NB harmless to assume this node never mints
    ]
 <>
    [ (,) 20 $ do
          (peer, bdel) <- QC.elements bdels
          QD.Some . ModifyCandidate peer <$> QC.elements bdel
    | notNull bdels
    ]
 <>
    [ (,) 100 $ QD.Some <$>
        (    NewCandidate
         <$> QC.elements new
         <*> (B <$> QC.choose (-10, 10)))
    | notNull new
    ]
 <>
    [ (,) 20 $ pure . QD.Some $ ReadGsmState ]
 <>
    [ (,) 20 $ pure . QD.Some $ ReadMarker ]
 <>
    [ (,) 50 $ QD.Some . StartIdling <$> QC.elements oldNotIdling | notNull oldNotIdling ]
 <>
    [ (,) 100 $ QD.Some . TimePasses <$> genTimePassesDur | prev == WhetherPrevTimePasses False ]
  where
    Model {
        mCandidates = cands
      ,
        mClock = clk
      ,
        mIdlers = idlers
      ,
        mPrev = prev
      ,
        mSelection = sel
      ,
        mUpstreamPeerBound = ub
      } = model

    notNull :: [a] -> Bool
    notNull = not . null

    old = Map.keys cands

    new = case ub of
        Nothing   -> []
        Just peer -> [ minBound .. peer ] \\ old

    oldNotIdling = old \\ Set.toList idlers

    genTimePassesDur = QC.frequency $
        [ (,) 10 $ QC.choose (1, 70) ]
     <>
        [ (,) 1 $ QC.choose (300, 600)
        | case mState model of
            ModelCaughtUp{}   -> True
            ModelPreSyncing{} -> False
            ModelSyncing{}    -> False
        ]

    -- sdels that would not cause the selection to be in the future
    sdels =
        let Selection b s = sel
        in
          [ sdel
          | sdel <- map S [-4 .. 10]
          , 0 /= sdel
          , onset (Selection b (s + sdel)) <= clk
          ]

    -- bdels that keep the candidates' lengths near the selection
    bdels =
        let Selection b _s = sel
            lim            = 3
        in
          [ (,)
              peer
              (filter (/= 0) [ b + offset - c | offset <- [-lim .. lim] ])
          | (peer, Candidate c) <- Map.assocs cands
          ]


shrinker :: Model -> QD.Action Model a -> [QD.Any (QD.Action Model)]
shrinker _model = \case
    InitState{} -> []
    Disconnect{} ->
        []
    ExtendSelection sdel ->
        [ QD.Some $ ExtendSelection sdel' | sdel' <- shrinkS sdel ]
    ModifyCandidate peer bdel ->
        [ QD.Some $ ModifyCandidate peer bdel' | bdel' <- shrinkB bdel, bdel' /= 0 ]
    NewCandidate peer bdel ->
        [ QD.Some $ NewCandidate peer bdel' | bdel' <- shrinkB bdel, bdel' /= 0  ]
    ReadGsmState ->
        []
    ReadMarker ->
        []
    StartIdling{} ->
        []
    TimePasses dur ->
        [ QD.Some $ TimePasses dur' | dur' <- QC.shrink dur, 0 < dur' ]
  where
    shrinkB (B x) = [ B x' | x' <- QC.shrink x ]
    shrinkS (S x) = [ S x' | x' <- QC.shrink x ]

transition :: Model -> QD.Action Model a -> Model
transition model cmd =
  let isHaaSatisfied = applyOpaqueFun (fromMaybe (Opaque $ const False) $ mIsHaaSatisfied model)
   in fixupModelState isHaaSatisfied cmd $
       case cmd of
         InitState initUpstreamPeerBound initStateInitialJudgement initStateIsHaaSatisfied ->
           model' {
                     mInitialJudgement = Just initStateInitialJudgement,
                     mIsHaaSatisfied = Just initStateIsHaaSatisfied,
                     mUpstreamPeerBound = initUpstreamPeerBound,
                     mSelection = initialSelection (Just initStateInitialJudgement),
                     mState = case initStateInitialJudgement of
                               TooOld
                                 | applyOpaqueFun initStateIsHaaSatisfied idlers -> ModelSyncing
                                 | otherwise             -> ModelPreSyncing
                               YoungEnough               -> ModelCaughtUp (SI.Time (-10000))
                  }
         Disconnect peer ->
             model' {
                 mCandidates = Map.delete peer cands
               ,
                 mIdlers = Set.delete peer idlers
               }
         ExtendSelection sdel ->
             model' { mSelection = Selection (b + 1) (s + sdel) }
         ModifyCandidate peer bdel ->
             model' {
                 mCandidates = Map.insertWith plusC peer (Candidate bdel) cands
               ,
                 mIdlers = Set.delete peer idlers
               }
         NewCandidate peer bdel ->
             model' { mCandidates = Map.insert peer (Candidate (b + bdel)) cands }
         ReadGsmState ->
             model'
         ReadMarker ->
             model'
         StartIdling peer ->
             model' { mIdlers = Set.insert peer idlers }
         TimePasses dur ->
             addNotableWhen BigDurN (dur > 300)
           $ model {
                 mClock = SI.addTime (0.1 * fromIntegral dur) clk
               ,
                 mPrev = WhetherPrevTimePasses True
               }
  where
    Model {
        mCandidates = cands
      ,
        mClock = clk
      ,
        mIdlers = idlers
      ,
        mSelection = Selection b s
      } = model

    model' = model { mPrev = WhetherPrevTimePasses False }

    plusC (Candidate x) (Candidate y) = Candidate (x + y)

-- | Update the 'mState', assuming that's the only stale field in the given
-- 'Model'
--
fixupModelState :: (Set.Set UpstreamPeer -> Bool) -> QD.Action Model a -> Model -> Model
fixupModelState isHaaSatisfied cmd model =
    case st of
        ModelPreSyncing
          | haaSatisfied ->
            avoidTransientState
          $ addNotableWhen PreSyncingToSyncingN True
          $ model { mState = ModelSyncing }
          | otherwise ->
            model
        ModelSyncing
          | not haaSatisfied ->
            addNotableWhen SyncingToPreSyncingN True
          $ model { mState = ModelPreSyncing }
          | caughtUp ->
            -- ASSUMPTION This new state was /NOT/ incurred by the 'TimePasses'
            -- command.
            --
            -- Therefore the current clock is necessarily the correct timestamp
            -- to record.
            addNotableWhen CaughtUpN True
          $ model { mState = ModelCaughtUp clk }
          | otherwise ->
            model
        ModelCaughtUp timestamp
          | flicker timestamp ->
            addNotableWhen FlickerN    True
          $ model { mState = ModelCaughtUp (flickerTimestamp timestamp) }
          | fellBehind timestamp ->
            avoidTransientState
          $ addNotableWhen FellBehindN True
          $ model { mState = ModelPreSyncing }
          | otherwise ->
            -- NB in this branch, these notables are mutually exclusive
            addNotableWhen TooOldN (expiryAge < clk)
          $ addNotableWhen
                NotThrashingN
                (SI.Time 0 < timestamp && expiryThrashing timestamp < clk)
          $ model
  where
    Model {
        mCandidates = cands
      ,
        mClock = clk
      ,
        mIdlers = idlers
      ,
        mSelection = sel
      ,
        mState = st
      } = model

    haaSatisfied         = isHaaSatisfied $ Map.keysSet cands
    caughtUp             = some && allIdling && all ok cands
    fellBehind timestamp = expiry timestamp < clk   -- NB 'boringDur' prevents ==

    flicker timestamp = fellBehind timestamp && caughtUp && haaSatisfied

    some = 0 < Map.size cands

    allIdling = idlers == Map.keysSet cands

    ok cand =
        GSM.WhetherCandidateIsBetter False == candidateOverSelection sel cand

    -- the first time the node would transition to PreSyncing
    expiry          timestamp = expiryAge `max` expiryThrashing timestamp
    expiryAge                 = SI.addTime ageLimit (onset sel)
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
        ExtendSelection sdel | sdel < 0 ->
          clk
        TimePasses{} ->
            foldl max (expiry timestamp)
          $ takeWhile (< clk)   -- NB 'boringDur' prevents ==
          $ iterate (SI.addTime thrashLimit) (expiry timestamp)
        _ ->
            error
          $     "impossible! flicker but neither "
             <>
                "negative ExtendSelection nor TimePasses: "
             <>
                show cmd

    avoidTransientState = fixupModelState isHaaSatisfied cmd

instance Eq (Opaque (Set.Set UpstreamPeer -> Bool)) where
  _ == _ = False

instance QD.StateModel Model where
  data Action Model a where
    Disconnect :: UpstreamPeer -> QD.Action Model ()
    ExtendSelection :: S -> QD.Action Model ()
    ModifyCandidate :: UpstreamPeer -> B -> QD.Action Model ()
    NewCandidate :: UpstreamPeer -> B -> QD.Action Model ()
    ReadGsmState :: QD.Action Model GSM.GsmState
    ReadMarker :: QD.Action Model MarkerState
    StartIdling :: UpstreamPeer -> QD.Action Model ()
    TimePasses :: Int -> QD.Action Model ()

    -- the InitState MUST be the first, and only the first, action in any sequence.
    -- the semantics of InitState must agree in the model and the SUT
    InitState :: Maybe UpstreamPeer -> LedgerStateJudgement -> Opaque (Set.Set UpstreamPeer -> Bool) -> QD.Action Model GSM.GsmState

  precondition = precondition

  arbitraryAction _ctx model = generator model

  shrinkAction _ctx model = shrinker model

  initialState = initModel

  nextState model action _ = transition model action

initModel :: Model
initModel =
    Model {
      mCandidates = Map.empty
    ,
      mClock = SI.Time 0
    ,
      mIdlers = idlers
    ,
      mNotables = Set.empty
    ,
      mPrev = WhetherPrevTimePasses True
    ,
      mSelection = initialSelection Nothing
    ,
      mState = ModelPreSyncing
    ,
      mInitialJudgement = Nothing
    ,
      mIsHaaSatisfied = Nothing
    ,
      mUpstreamPeerBound = Nothing
    }
    where
      idlers = Set.empty

--- System Under Test

----- trivial event accumulator, useful for debugging test failures

data Ev =
    forall a. EvBegin (QD.Action Model a)
    -- ^ 'semantics' started stimulating the GSM code being tested
  |
    EvEnd
    -- ^ 'semantics' stopped stimulating the GSM code being tested
  |
    EvGsm (GSM.TraceGsmEvent Selection)
    -- ^ the GSM code being tested emitted an event

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

-- | merely a tidy bundle of arguments
data Vars m = Vars
    (StrictTVar m Selection)
    (StrictTVar m (Map.Map UpstreamPeer (StrictTVar m PeerState)))
    (StrictTVar m GSM.GsmState)
    (StrictTVar m MarkerState)
    (EvRecorder m)

newtype RunMonad m a = RunMonad {runMonad :: ReaderT (Vars m) m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (Vars m))

instance MonadTrans RunMonad where
  lift = RunMonad . lift

instance IOLike m => QD.RunModel Model (RunMonad m) where
  -- TODO: make the reader over vars into a newtype to remove lifts
  perform :: Model -> QD.Action Model a -> QD.LookUp -> RunMonad m a
  perform _ action _ = do --    pre $
      Vars
          varSelection
          varStates
          varGsmState
          varMarker
          varEvents <- ask
      let
        pre :: m a -> RunMonad m a
        pre m = lift $ do
            push varEvents (EvBegin action)
            x <- m
            yieldSeveralTimes   -- see Note [Why yield after the command]
            push varEvents EvEnd
            pure x

      pre $ case action of
        InitState _upstreamPeerBound initStateInitialJudgement _initStateIsHaaSatisfied -> do
          let initialGsmState = case initStateInitialJudgement of
                                  TooOld      -> GSM.PreSyncing
                                  YoungEnough -> GSM.CaughtUp
          atomically $ do
            writeTVar varSelection $ initialSelection (Just initStateInitialJudgement)
            writeTVar varGsmState $ initialGsmState
          pure initialGsmState
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
            modifyTVar v $ \ (PeerState c _) -> PeerState c (Idling True)
            pure ()
        TimePasses dur -> do
            SI.threadDelay (0.1 * fromIntegral dur)
            pure ()

---

tests :: [TestTree]
tests =    adhoc <> core
  where
    adhoc = [testProperty "GSM yield regression" prop_yield_regression]
    core = [
          adjustQuickCheckTests (* 10)
        $ testProperty ("GSM (" <> coreTestName ub <> ")")
        $ prop_sequential_iosim ub
      | ub <- Nothing : map Just [minBound .. maxBound :: UpstreamPeer]
      ]

    coreTestName = \case
        Nothing -> "no peers"
        Just ub -> "at most " <> case fromEnum ub of
            0 -> "1 peer"
            n -> show (n + 1) <> " peers"

runIOSimProp :: QC.Testable a => (forall s. QC.PropertyM (RunMonad (IOSim.IOSim s)) a) -> QC.Property
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
    let tr = IOSim.runSimTrace (sim eval)
    return $
        case IOSim.traceResult False tr of
          Right a -> QC.property a
          Left (IOSim.FailureException (SomeException ex)) ->
            QC.counterexample (show ex) False
          Left ex ->
            QC.counterexample (show ex) False
   where
    sim ::
      forall s.
      (QC.Gen (RunMonad (IOSim.IOSim s) a) -> (RunMonad (IOSim.IOSim s) a)) ->
      IOSim.IOSim s a
    sim eval = do
      vars <- setupVars TooOld
      runReaderT (runMonad (eval f)) vars

setupVars :: IOLike m => LedgerStateJudgement -> m (Vars m)
setupVars initStateInitialJudgement = do
      let initialGsmState = case initStateInitialJudgement of
                                  TooOld      -> GSM.PreSyncing
                                  YoungEnough -> GSM.CaughtUp

      -- these variables are part of the 'GSM.GsmView'
      varSelection  <- newTVarIO (initialSelection . Just $ initStateInitialJudgement)
      varStates     <- newTVarIO Map.empty
      varGsmState   <- newTVarIO initialGsmState
      varMarker     <- newTVarIO (toMarker initialGsmState)

      -- this variable is for better 'QC.counterexample' messages
      varEvents <- newRecorder
      -- let tracer = Tracer $ push varEvents . EvGsm

      pure $
        Vars varSelection
             varStates
             varGsmState
             varMarker
             varEvents

-- | Execute an action in both the model and the system under test, returning
--   the new model and a property that checks that the action brought the model and the SUT into the same state
takeActionInBoth :: IOLike m => Model -> QD.Action Model GSM.GsmState -> QC.PropertyM (RunMonad m) (QC.Property, Model)
takeActionInBoth model action = do
    actual <- lift $ QD.perform model action undefined
    let expected = QD.nextState model action undefined
    pure (QC.counterexample ("Lockstep " <> show action) $
          ((toGsmState . mState $ expected) QC.=== actual), expected)

-- | Test the example from the Note [Why yield after the command]
--
-- This property fails when 'yieldSeveralTimes' is removed/redefined to @pure
-- ()@.
prop_yield_regression :: QC.Fun (Set.Set UpstreamPeer) Bool -> QC.Property
prop_yield_regression f =
   QC.once
 $ runIOSimProp
 $ prop_sequential_iosim1 Nothing YoungEnough f yieldRegressionActions

 where yieldRegressionActions :: QD.Actions Model
       yieldRegressionActions =
         (QD.Actions $
           [ (QD.:=) (QD.mkVar 0) . (flip QD.ActionWithPolarity) QD.PosPolarity $ NewCandidate Amara (B 1)
           , (QD.:=) (QD.mkVar 0) . (flip QD.ActionWithPolarity) QD.PosPolarity $ StartIdling Amara
           , (QD.:=) (QD.mkVar 0) . (flip QD.ActionWithPolarity) QD.PosPolarity $ TimePasses 61
           , (QD.:=) (QD.mkVar 0) . (flip QD.ActionWithPolarity) QD.PosPolarity $ ExtendSelection (S (-4))
           ])
         <> (QD.Actions [(QD.:=) (QD.mkVar 0) . (flip QD.ActionWithPolarity) QD.PosPolarity $ ReadMarker])

prop_sequential_iosim ::
  Maybe UpstreamPeer ->
  LedgerStateJudgement ->
  QC.Fun (Set.Set UpstreamPeer) Bool ->
  QD.Actions Model ->
  QC.Property
prop_sequential_iosim upstreamPeerBound initialJudgement isHaaSatisfied cmds =
  runIOSimProp $ prop_sequential_iosim1 upstreamPeerBound initialJudgement isHaaSatisfied cmds

prop_sequential_iosim1 ::
  Maybe UpstreamPeer ->
  LedgerStateJudgement ->
  QC.Fun (Set.Set UpstreamPeer) Bool ->
  QD.Actions Model ->
  QC.PropertyM (RunMonad (IOSim.IOSim s)) QC.Property
prop_sequential_iosim1 upstreamPeerBound initialJudgement (QC.Fn isHaaSatisfied) cmds = do
  Vars
    varSelection
    varStates
    varGsmState
    varMarker
    varEvents <- lift ask

  let model = QD.initialState @Model
  let tracer = Tracer $ push varEvents . EvGsm
  let gsm = GSM.realGsmEntryPoints (id, tracer) GSM.GsmView {
            GSM.antiThunderingHerd = Nothing
          ,
            GSM.candidateOverSelection = \ s (PeerState c _) -> candidateOverSelection s c
          ,
            GSM.peerIsIdle = isIdling
          ,
            GSM.durationUntilTooOld = Just durationUntilTooOld
          ,
            GSM.equivalent = (==)   -- unsound, but harmless in this test
          ,
            GSM.getChainSyncStates = readTVar varStates
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
                isHaaSatisfied . Map.keysSet <$> readTVar varStates
          }
      gsmEntryPoint = case initialJudgement of
            TooOld      -> GSM.enterPreSyncing gsm
            YoungEnough -> GSM.enterCaughtUp   gsm

  lift . lift $ yieldSeveralTimes

  -- effectively add a 'InitState' to as the first command
  (initStateCheck, modelAfterInitState) <- takeActionInBoth model $
    InitState upstreamPeerBound initialJudgement (Opaque isHaaSatisfied)

  -- TODO: figure out how to do withAsync here
  hGSM <- lift . lift $ async gsmEntryPoint

  (metadata, mbExn)  <- do
        -- we cannot use the stock QD.runActions here, as it will use QD.initialState and thus throw away the
        -- modifications introduced by the InitState action we've just ran
        -- TODO: is there a cleaner way to inject InitState as the first action into the sequence,
        -- while still keeping the above check?
        (metadata, _env) <- QD.runActionsFrom (QD.Metadata mempty modelAfterInitState) cmds
        -- this does not work as tActions cannot be cons'd
        -- (metadata, _env) <- QD.runActions (initStateAction : cmds)
        (lift . lift $ poll hGSM) <&> \case
            Just Right{}    ->
                error "impossible! GSM terminated"
            Just (Left exn) ->
                -- we don't simply rethrow it, since we still want to pretty print
                -- the command sequence
                (metadata, Just exn)
            Nothing         ->
                (metadata, Nothing)
  -- a good way to make this test fail it to wait for the GSM and cause a deadlock
  -- _ <- lift . lift $ wait hGSM

  let modelStateAfterActions = QD.underlyingState metadata

  let noExn = case mbExn of
          Nothing  -> QC.property ()
          Just exn -> QC.counterexample (show exn) False

  -- effectively add a 'ReadGsmState' to the end of the command list
  (finalStateCheck, finalModelState) <- takeActionInBoth modelStateAfterActions ReadGsmState

  watcherEvents <- lift . lift $ dumpEvents varEvents

  pure $ QC.counterexample
            (unlines
               $ (:) "WATCHER"
               $ map (("    " <>) . show)
               $ watcherEvents
            )
       $ QC.counterexample ("Initial " <> show modelAfterInitState)
       $ QC.counterexample ("Final " <> show finalModelState)
       $ QC.tabulate
             "Notables"
             (case Set.toList $ mNotables finalModelState of
                []       -> ["<none>"]
                notables -> map show notables
             )
       $ initStateCheck QC..&&. noExn QC..&&. finalStateCheck

-- | See 'boringDurImpl'
boringDur :: Model -> Int -> Bool
boringDur model = boringDurImpl clk sel st
  where
    Model {
        mClock = clk
      ,
        mSelection = sel
      ,
        mState = st
      } = model

---

-- | The 'Opaque' type exists to give a 'Show' instance to functions
newtype Opaque a = Opaque a

instance Show (Opaque a) where
  show _ = "<opaque>"

applyOpaqueFun :: Opaque (a -> b) -> a -> b
applyOpaqueFun (Opaque f) = f
