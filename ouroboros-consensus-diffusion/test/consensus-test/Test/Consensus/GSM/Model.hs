{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | The definition of the GSM QSM model and its auxiliaries

module Test.Consensus.GSM.Model (module Test.Consensus.GSM.Model) where

import qualified Control.Monad.Class.MonadTime.SI as SI
import           Data.Kind (Type)
import           Data.List ((\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Time (diffTimeToPicoseconds)
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
    ReadJudgment
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
    ReadThisJudgment LedgerStateJudgement
  |
    ReadThisMarker MarkerState
  |
    Unit
  deriving stock    (Generic1, Read, Show)
  deriving anyclass (QSM.Foldable, QSM.Functor, QSM.Traversable)

type Model :: (Type -> Type) -> Type
data Model r = Model {
    mCandidates :: Map.Map UpstreamPeer PeerState
  ,
    mClock      :: SI.Time
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

initModel :: LedgerStateJudgement -> Model r
initModel j = Model {
    mCandidates = Map.empty
  ,
    mClock = SI.Time 0
  ,
    mNotables = Set.empty
  ,
    mPrev = WhetherPrevTimePasses True
  ,
    mSelection = Selection 0 s
  ,
    mState = case j of
        TooOld      -> ModelTooOld
        YoungEnough -> ModelYoungEnough (SI.Time (-10000))
  }
  where
    s = S $ case j of
        TooOld      -> (-11)
        YoungEnough -> 0

-- The extra expressivity of 'QSM.Logic' beyond 'Bool' will not be useful in
-- this test module as-is, since we only run commands (via 'runCommands'') that
-- the library generated, which ensures the preconditions. On the other hand,
-- if you're debugging a failure by manually altering commands, then these
-- annotations may be helpful.
precondition :: Model Symbolic -> Command Symbolic -> QSM.Logic
precondition model = pre $ \case
    cmd@ExtendSelection{} ->
        let model' = transition model cmd Unit
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
    ReadJudgment ->
        QSM.Top
    ReadMarker ->
        QSM.Top
    StartIdling peer ->
        "idle after disconnect" `atom` (peer `Map.member` cands)
     QSM..&&
        "double idle" `atom` (Idling False == maybe (Idling False) psIdling (cands Map.!? peer))
    TimePasses dur ->
        "non-positive duration" `atom` (0 < dur)
     QSM..&&
        boringDur model dur
  where
    Model {
        mCandidates = cands
      } = model

    pre f cmd = f cmd QSM..// show cmd

transition :: Model r -> Command r -> Response r -> Model r
transition model cmd resp = fixupModelState cmd $ case (cmd, resp) of
    (Disconnect peer, Unit) ->
        model' {
            mCandidates = Map.delete peer cands
          }
    (ExtendSelection sdel, Unit) ->
        model' { mSelection = Selection (b + 1) (s + sdel) }
    (ModifyCandidate peer bdel, Unit) ->
        model' {
            mCandidates = Map.alter (plusC bdel) peer cands
          }
    (NewCandidate peer bdel, Unit) ->
        model' { mCandidates = Map.insert peer (PeerState (Candidate (b + bdel)) (Idling False)) cands }
    (ReadJudgment, ReadThisJudgment{}) ->
        model'
    (ReadMarker, ReadThisMarker{}) ->
        model'
    (StartIdling peer, Unit) ->
        model' { mCandidates = Map.adjust (\ (PeerState c _) -> PeerState c (Idling True)) peer cands }
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
        mSelection = Selection b s
      } = model

    model' = model { mPrev = WhetherPrevTimePasses False }

    plusC x (Just (PeerState (Candidate y) i)) = Just (PeerState (Candidate (x + y)) i)
    plusC x Nothing = Just (PeerState (Candidate x) (Idling False))

-- | Update the 'mState', assuming that's the only stale field in the given
-- 'Model'
fixupModelState :: Command r -> Model r -> Model r
fixupModelState cmd model =
    case st of
        ModelTooOld
          | caughtUp ->
            -- ASSUMPTION This new state was /NOT/ incurred by the 'TimePasses'
            -- command.
            --
            -- Therefore the current clock is necessarily the correct timestamp
            -- to record.
            addNotableWhen CaughtUpN True
          $ model { mState = ModelYoungEnough clk }
          | otherwise ->
            model
        ModelYoungEnough timestamp
          | flicker timestamp ->
            addNotableWhen FlickerN    True
          $ model { mState = ModelYoungEnough (flickerTimestamp timestamp) }
          | fellBehind timestamp ->
            addNotableWhen FellBehindN True
          $ model { mState = ModelTooOld }
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
        mSelection = sel
      ,
        mState = st
      } = model

    caughtUp             = some && allIdling && all ok cands
    fellBehind timestamp = expiry timestamp < clk   -- NB 'boringDur' prevents ==

    flicker timestamp = fellBehind timestamp && caughtUp

    some = 0 < Map.size cands

    allIdling = all isIdling cands

    ok peer =
        GSM.WhetherCandidateIsBetter False == candidateOverSelection sel peer

    -- the first time the node would transition to OnlyBootstrap
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
    -- OnlyBootstrap. However, it would be plausible for an implementation to
    -- retain any bootstrap peers it happened to have, so the idiosyncratic
    -- behavior of the system under test in this module is not totally
    -- irrelevant.
    --
    -- the /last/ time the node instantaneously visited OnlyBootstrap during
    -- the 'TimePasses' command, assuming it did so at least once
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

postcondition ::
     Model Concrete
  -> Command Concrete
  -> Response Concrete
  -> QSM.Logic
postcondition model _cmd = \case
    ReadThisJudgment j' ->
        j' QSM..== j
    ReadThisMarker m' ->
        m' QSM..== toMarker j
    Unit ->
        QSM.Top
  where
    j = toJudgment $ mState model

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
    [ (,) 20 $ pure ReadJudgment ]
 <>
    [ (,) 20 $ pure ReadMarker ]
 <>
    [ (,) 50 $ StartIdling <$> elements (Map.keys oldNotIdling) | not (Map.null oldNotIdling) ]
 <>
    [ (,) 100 $ TimePasses <$> genTimePassesDur | prev == WhetherPrevTimePasses False ]
  where
    Model {
        mCandidates = cands
      ,
        mClock = clk
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

    oldNotIdling = psCandidate <$> Map.filter (not . isIdling) cands

    genTimePassesDur = QC.frequency $
        [ (,) 10 $ choose (1, 70) ]
     <>
        [ (,) 1 $ choose (300, 600)
        | case mState model of ModelYoungEnough{} -> True; ModelTooOld -> False
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
          | (peer, PeerState (Candidate c) _) <- Map.assocs cands
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
    ReadJudgment ->
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
    ReadJudgment ->
        ReadThisJudgment j
    ReadMarker ->
        ReadThisMarker $ toMarker j
    StartIdling{} ->
        Unit
    TimePasses{} ->
        Unit
  where
    j = toJudgment $ mState model

-----

-- | A block count
newtype B = B Int
  deriving stock    (Eq, Ord, Generic, Read, Show)
  deriving newtype  (Enum, Num)
  deriving anyclass (TD.ToExpr)

-- | A slot count
newtype S = S Int
  deriving stock    (Eq, Ord, Generic, Read, Show)
  deriving newtype  (Enum, Num)
  deriving anyclass (TD.ToExpr)

data UpstreamPeer = Amara | Bao | Cait | Dhani | Eric
  deriving stock    (Bounded, Enum, Eq, Ord, Generic, Read, Show)
  deriving anyclass (TD.ToExpr)

-- | The cumulative growth relative to whatever length the initial selection
-- was and the slot relative to the start of the test (which is assumed to be
-- the exact onset of some slot)
data Selection = Selection !B !S
  deriving stock    (Eq, Ord, Generic, Show)
  deriving anyclass (TD.ToExpr)

-- | The age of the candidate is irrelevant, only its length matters
newtype Candidate = Candidate B
  deriving stock    (Eq, Ord, Generic, Show)
  deriving anyclass (TD.ToExpr)

newtype Idling = Idling Bool
  deriving stock    (Eq, Ord, Generic, Show)
  deriving anyclass (TD.ToExpr)

data PeerState = PeerState { psCandidate :: !Candidate, psIdling :: !Idling }
  deriving stock    (Eq, Ord, Generic, Show)
  deriving anyclass (TD.ToExpr)

data MarkerState = Present | Absent
  deriving stock    (Eq, Ord, Generic, Read, Show)
  deriving anyclass (TD.ToExpr)

newtype WhetherPrevTimePasses = WhetherPrevTimePasses Bool
  deriving stock    (Eq, Ord, Generic, Show)
  deriving anyclass (TD.ToExpr)

data ModelState =
    ModelTooOld
  |
    ModelYoungEnough !SI.Time
    -- ^ when the model most recently transitioned to 'YoungEnough'
  deriving stock    (Eq, Ord, Generic, Show)
  deriving anyclass (TD.ToExpr)

-----

-- | Interesting events to record /within the model/
--
-- TODO some less superficial ones (eg even just combinations of these)
data Notable =
    BigDurN
    -- ^ there was a "big" 'TimesPasses' command
  |
    CaughtUpN
    -- ^ the node transitioned from OnlyBootstrap to CaughtUp
  |
    FellBehindN
    -- ^ the node transitioned from CaughtUp to OnlyBootstrap
  |
    FlickerN
    -- ^ the node transitioned from CaughtUp to OnlyBootstrap and back to
    -- CaughtUp "instantly"
  |
    NotThrashingN
    -- ^ the anti-thrashing would have allowed 'FellBehindN', but the selection
    -- wasn't old enough
  |
    TooOldN
    -- ^ the selection was old enough for 'FellBehindN', but the anti-thrashing
    -- prevented it
  deriving (Eq, Ord, Show)

instance TD.ToExpr Notable where toExpr = TD.defaultExprViaShow

----- orphans

instance TD.ToExpr SI.Time              where toExpr = TD.defaultExprViaShow
instance TD.ToExpr LedgerStateJudgement where toExpr = TD.defaultExprViaShow

deriving instance Read LedgerStateJudgement

instance QC.Arbitrary LedgerStateJudgement where
    arbitrary = elements [TooOld, YoungEnough]
    shrink    = \case
        TooOld      -> [YoungEnough]
        YoungEnough -> []

instance QC.Arbitrary MarkerState where
    arbitrary = elements [Absent, Present]
    shrink    = \case
        Absent  -> [Present]
        Present -> []

-----

candidateOverSelection ::
     Selection
  -> PeerState
  -> GSM.CandidateVersusSelection
candidateOverSelection (Selection b _s) (PeerState (Candidate b') _) =
    -- TODO this ignores CandidateDoesNotIntersect, which seems harmless, but
    -- I'm not quite sure
    GSM.WhetherCandidateIsBetter (b < b')

-----

toJudgment :: ModelState -> LedgerStateJudgement
toJudgment = \case
    ModelTooOld        -> TooOld
    ModelYoungEnough{} -> YoungEnough

toMarker :: LedgerStateJudgement -> MarkerState
toMarker = \case
    TooOld      -> Absent
    YoungEnough -> Present

-----

atom :: String -> Bool -> QSM.Logic
atom s b = QSM.Boolean b QSM..// s

onset :: Selection -> SI.Time
onset (Selection _b (S s)) = SI.Time $ fromIntegral s

ageLimit :: Num a => a
ageLimit = 10   -- seconds

thrashLimit :: Num a => a
thrashLimit = 8   -- seconds

selectionIsBehind :: Model r -> Bool
selectionIsBehind model =
    any (\(PeerState (Candidate b') _) -> b' > b) cands
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

-- | Checks that a 'TimePasses' command does not end exactly when a timeout
-- could fire and that a 'ExtendSelection' does not incur a timeout that would
-- fire immediately
--
-- This insulates the test from race conditions that are innocuous in the real
-- world.
boringDur :: Model r -> Int -> QSM.Logic
boringDur model dur =
    boringSelection QSM..&& boringState
  where
    Model {
        mClock = clk
      ,
        mSelection = sel
      ,
        mState = st
      } = model

    -- the first time the node would transition to OnlyBootstrap
    expiry          timestamp = expiryAge `max` expiryThrashing timestamp
    expiryAge                 = SI.addTime ageLimit (onset sel)
    expiryThrashing timestamp = SI.addTime thrashLimit timestamp

    clk' = SI.addTime (0.1 * fromIntegral dur) clk

    boringSelection = "boringDur selection" `atom` (clk' /= expiryAge)

    boringState = case st of
        ModelTooOld                -> QSM.Top
        ModelYoungEnough timestamp ->
            let gap = clk' `SI.diffTime` expiry timestamp
                n   =
                  mod
                      (diffTimeToPicoseconds gap)
                      (secondsToPicoseconds thrashLimit)
            in
            "boringDur state" `atom` (gap < 0 || 0 /= n)

    secondsToPicoseconds x = x * 10 ^ (12 :: Int)

isIdling :: PeerState -> Bool
isIdling (PeerState {psIdling = Idling i}) = i
