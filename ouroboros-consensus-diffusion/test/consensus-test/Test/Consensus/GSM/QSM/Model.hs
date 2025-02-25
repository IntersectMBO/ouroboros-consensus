{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | The definition of the GSM QSM model and its auxiliaries

module Test.Consensus.GSM.QSM.Model (module Test.Consensus.GSM.QSM.Model) where

import qualified Control.Monad.Class.MonadTime.SI as SI
import           Data.Kind (Type)
import           Data.List ((\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.TreeDiff as TD
import           GHC.Generics (Generic, Generic1)
import qualified Ouroboros.Consensus.Node.GSM as GSM
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type
                     (LedgerStateJudgement (..))
import qualified Test.QuickCheck as QC
import           Test.QuickCheck (choose, elements, shrink)
import qualified Test.StateMachine as QSM
import           Test.StateMachine (Concrete, Symbolic)
import qualified Test.StateMachine.Types.Rank2 as QSM
import           Test.Util.Orphans.ToExpr ()

import Test.Consensus.GSM.Common

----- the QSM model

-- These definitions are in the exact same order as the QSM tutorial at
-- <https://github.com/stevana/quickcheck-state-machine/blob/3748220bffacb61847e35e3808403f3e7187a7c6/README.md>,
-- except @semantics@ and the property itself are defined in
-- "Test.Consensus.GSM".

-- | TODO restarts (or maybe just killing the GSM thread)
type Command :: (Type -> Type) -> Type
data Command r =
    Disconnect UpstreamPeer
    -- ^ INVARIANT must be an existing peer
    --
    -- Mocks the necessary ChainSync client behavior.
  |
    ExtendSelection S
    -- ^ INVARIANT 'selectionIsBehind'
    --
    -- NOTE Harmless to assume it only advances by @'B' 1@ at a time.
  |
    ModifyCandidate UpstreamPeer B
    -- ^ INVARIANT existing peer
    --
    -- Mocks the necessary ChainSync client behavior.
  |
    NewCandidate UpstreamPeer B
    -- ^ INVARIANT new peer
    --
    -- Mocks the necessary ChainSync client behavior.
  |
    ReadGsmState
  |
    ReadMarker
  |
    StartIdling UpstreamPeer
    -- ^ INVARIANT existing peer, not idling
  |
    TimePasses Int
    -- ^ tenths of a second
    --
    -- INVARIANT positive
    --
    -- INVARIANT does not end /exactly/ on an interesting time point; see
    -- 'boringDur'.
    --
    -- NOTE The generator does not yield consecutive 'TimePasses' commands,
    -- though shrinking might.
    --

    -- DRAFT NOTE An earlier design attempted to prevent TimePasses from ever landing
    -- exactly on an interesting moment, ie when some timeout expires. Notably:
    -- when a slot becomes too old or when the anti-thrashing limit expires.
    -- This is feasible and its complexity is tolerable for the generators.
    -- However, shrinking------wait, doesn't precondition guard against it?
  deriving stock    (Generic1, Read, Show)
  deriving anyclass (QSM.CommandNames, QSM.Foldable, QSM.Functor, QSM.Traversable)

type Response :: (Type -> Type) -> Type
data Response r =
    ReadThisGsmState GSM.GsmState
  |
    ReadThisMarker MarkerState
  |
    Unit
  deriving stock    (Generic1, Read, Show)
  deriving anyclass (QSM.Foldable, QSM.Functor, QSM.Traversable)

type Model :: (Type -> Type) -> Type
data Model r = Model {
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
  }
  deriving (Generic, Show)
  deriving anyclass (TD.ToExpr)

addNotableWhen :: Notable -> Bool -> Model r -> Model r
addNotableWhen n b model =
    if not b then model else
    model { mNotables = n `Set.insert` mNotables model }

initModel :: Context -> Model r
initModel ctx = Model {
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
    mSelection = Selection 0 s
  ,
    mState = case j of
        TooOld
          | isHaaSatisfied idlers -> ModelSyncing
          | otherwise             -> ModelPreSyncing
        YoungEnough               -> ModelCaughtUp (SI.Time (-10000))
  }
  where
    Context {
        cInitialJudgement = j
      ,
        cIsHaaSatisfied = isHaaSatisfied
      } = ctx

    idlers = Set.empty

    s = S $ case j of
        TooOld      -> (-11)
        YoungEnough -> 0

-- The extra expressivity of 'QSM.Logic' beyond 'Bool' will not be useful in
-- this test module as-is, since we only run commands (via 'runCommands'') that
-- the library generated, which ensures the preconditions. On the other hand,
-- if you're debugging a failure by manually altering commands, then these
-- annotations may be helpful.
precondition :: Context -> Model Symbolic -> Command Symbolic -> QSM.Logic
precondition ctx model = pre $ \case
    cmd@ExtendSelection{} ->
        let model' = transition ctx model cmd Unit
        in
            "syncing node got ahead" `atom` selectionIsBehind model
         QSM..&&
            "early selection" `atom` selectionIsNotEarly model'
         QSM..&&
            boringDur model' 0
    Disconnect peer ->
        "double disconnect" `atom` (peer `Map.member` cands)
    ModifyCandidate peer _bdel ->
        "modify after disconnect" `atom` (peer `Map.member` cands)
    NewCandidate peer _bdel ->
        "double connect" `atom` (peer `Map.notMember` cands)
    ReadGsmState ->
        QSM.Top
    ReadMarker ->
        QSM.Top
    StartIdling peer ->
        "idle after disconnect" `atom` (peer `Map.member` cands)
     QSM..&&
        "double idle" `atom` (peer `Set.notMember` idlers)
    TimePasses dur ->
        "non-positive duration" `atom` (0 < dur)
     QSM..&&
        boringDur model dur
  where
    Model {
        mCandidates = cands
      ,
        mIdlers = idlers
      } = model

    pre f cmd = f cmd QSM..// show cmd

transition :: Context -> Model r -> Command r -> Response r -> Model r
transition ctx model cmd resp = fixupModelState ctx cmd $ case (cmd, resp) of
    (Disconnect peer, Unit) ->
        model' {
            mCandidates = Map.delete peer cands
          ,
            mIdlers = Set.delete peer idlers
          }
    (ExtendSelection sdel, Unit) ->
        model' { mSelection = Selection (b + 1) (s + sdel) }
    (ModifyCandidate peer bdel, Unit) ->
        model' {
            mCandidates = Map.insertWith plusC peer (Candidate bdel) cands
          ,
            mIdlers = Set.delete peer idlers
          }
    (NewCandidate peer bdel, Unit) ->
        model' { mCandidates = Map.insert peer (Candidate (b + bdel)) cands }
    (ReadGsmState, ReadThisGsmState{}) ->
        model'
    (ReadMarker, ReadThisMarker{}) ->
        model'
    (StartIdling peer, Unit) ->
        model' { mIdlers = Set.insert peer idlers }
    (TimePasses dur, Unit) ->
        addNotableWhen BigDurN (dur > 300)
      $ model {
            mClock = SI.addTime (0.1 * fromIntegral dur) clk
          ,
            mPrev = WhetherPrevTimePasses True
          }
    o -> error $ "impossible response: " <> show o
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
fixupModelState :: Context -> Command r -> Model r -> Model r
fixupModelState ctx cmd model =
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

    Context {
        cIsHaaSatisfied = isHaaSatisfied
      } = ctx

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

    avoidTransientState = fixupModelState ctx cmd

postcondition ::
     Model Concrete
  -> Command Concrete
  -> Response Concrete
  -> QSM.Logic
postcondition model _cmd = \case
    ReadThisGsmState s' ->
        s' QSM..== s
    ReadThisMarker m' ->
        m' QSM..== toMarker s
    Unit ->
        QSM.Top
  where
    s = toGsmState $ mState model

generator ::
     Maybe UpstreamPeer
  -> Model Symbolic
  -> Maybe (QC.Gen (Command Symbolic))
generator ub model = Just $ QC.frequency $
    [ (,) 5 $ Disconnect <$> elements old | notNull old ]
 <>
    [ (,) 10 $ ExtendSelection <$> elements sdels
    | notNull sdels
    , selectionIsBehind model   -- NB harmless to assume this node never mints
    ]
 <>
    [ (,) 20 $ do
          (peer, bdel) <- elements bdels
          ModifyCandidate peer <$> elements bdel
    | notNull bdels
    ]
 <>
    [ (,) 100 $
            NewCandidate
        <$> elements new
        <*> (B <$> choose (-10, 10))
    | notNull new
    ]
 <>
    [ (,) 20 $ pure ReadGsmState ]
 <>
    [ (,) 20 $ pure ReadMarker ]
 <>
    [ (,) 50 $ StartIdling <$> elements oldNotIdling | notNull oldNotIdling ]
 <>
    [ (,) 100 $ TimePasses <$> genTimePassesDur | prev == WhetherPrevTimePasses False ]
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
      } = model

    notNull :: [a] -> Bool
    notNull = not . null

    old = Map.keys cands

    new = case ub of
        Nothing   -> []
        Just peer -> [ minBound .. peer ] \\ old

    oldNotIdling = old \\ Set.toList idlers

    genTimePassesDur = QC.frequency $
        [ (,) 10 $ choose (1, 70) ]
     <>
        [ (,) 1 $ choose (300, 600)
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


shrinker :: Model Symbolic -> Command Symbolic -> [Command Symbolic]
shrinker _model = \case
    Disconnect{} ->
        []
    ExtendSelection sdel ->
        [ ExtendSelection sdel' | sdel' <- shrinkS sdel ]
    ModifyCandidate peer bdel ->
        [ ModifyCandidate peer bdel' | bdel' <- shrinkB bdel, bdel' /= 0 ]
    NewCandidate peer bdel ->
        [ NewCandidate peer bdel' | bdel' <- shrinkB bdel, bdel' /= 0  ]
    ReadGsmState ->
        []
    ReadMarker ->
        []
    StartIdling{} ->
        []
    TimePasses dur ->
        [ TimePasses dur' | dur' <- shrink dur, 0 < dur' ]
  where
    shrinkB (B x) = [ B x' | x' <- shrink x ]
    shrinkS (S x) = [ S x' | x' <- shrink x ]

mock :: Model r -> Command r -> Response r
mock model = \case
    Disconnect{} ->
        Unit
    ExtendSelection{} ->
        Unit
    ModifyCandidate{} ->
        Unit
    NewCandidate{} ->
        Unit
    ReadGsmState ->
        ReadThisGsmState s
    ReadMarker ->
        ReadThisMarker $ toMarker s
    StartIdling{} ->
        Unit
    TimePasses{} ->
        Unit
  where
    s = toGsmState $ mState model

-----

data Context = Context {
    cInitialJudgement :: LedgerStateJudgement
  ,
    cIsHaaSatisfied   :: Set.Set UpstreamPeer -> Bool
  }

atom :: String -> Bool -> QSM.Logic
atom s b = QSM.Boolean b QSM..// s

selectionIsBehind :: Model r -> Bool
selectionIsBehind model =
    any (\(Candidate b') -> b' > b) cands
  where
    Model {
        mCandidates = cands
      ,
        mSelection = Selection b _s
      } = model

selectionIsNotEarly :: Model r -> Bool
selectionIsNotEarly model =
    onset sel <= clk
  where
    Model {
        mClock = clk
      ,
        mSelection = sel
      } = model

-- | See 'boringDurImpl'
boringDur :: Model r -> Int -> QSM.Logic
boringDur model = QSM.Boolean . boringDurImpl clk sel st
  where
    Model {
        mClock = clk
      ,
        mSelection = sel
      ,
        mState = st
      } = model
