{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.GSM.Common (module Test.Consensus.GSM.Common) where

import qualified Control.Monad.IOSim as IOSim
import           Control.Monad.Class.MonadFork (MonadFork, yield)
import           Control.Monad (replicateM_)
import qualified Control.Monad.Class.MonadTime.SI as SI
import           Data.Time (diffTimeToPicoseconds)
import qualified Data.TreeDiff as TD
import           GHC.Generics (Generic)
import qualified Ouroboros.Consensus.Node.GSM as GSM
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type
                     (LedgerStateJudgement (..))
import qualified Test.QuickCheck as QC
import           Test.QuickCheck (elements, shrink)
import           Test.Util.Orphans.ToExpr ()

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
  deriving anyclass (TD.ToExpr, QC.CoArbitrary, QC.Function)

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

data MarkerState = Present | Absent
  deriving stock    (Eq, Ord, Generic, Read, Show)
  deriving anyclass (TD.ToExpr)

newtype WhetherPrevTimePasses = WhetherPrevTimePasses Bool
  deriving stock    (Eq, Ord, Generic, Show)
  deriving anyclass (TD.ToExpr)

data ModelState =
    ModelPreSyncing
  |
    ModelSyncing
  |
    ModelCaughtUp !SI.Time
    -- ^ when the model most recently transitioned to 'GSM.CaughtUp'.
  deriving stock    (Eq, Ord, Generic, Show)
  deriving anyclass (TD.ToExpr)

-- | Interesting events to record /within the model/
--
-- TODO some less superficial ones (eg even just combinations of these)
data Notable =
    BigDurN
    -- ^ there was a "big" 'TimesPasses' command
  |
    CaughtUpN
    -- ^ the node transitioned from Syncing to CaughtUp
  |
    FellBehindN
    -- ^ the node transitioned from CaughtUp to PreSyncing
  |
    SyncingToPreSyncingN
    -- ^ the node transition from Syncing to PreSyncing
  |
    PreSyncingToSyncingN
    -- ^ the node transition from PreSyncing to Syncing
  |
    FlickerN
    -- ^ the node transitioned from CaughtUp to PreSyncing to Syncing and back
    -- to CaughtUp "instantly"
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

---

newtype Idling = Idling Bool
  deriving (Eq, Ord, Show)

data PeerState = PeerState { psCandidate :: !Candidate, psIdling :: !Idling }
  deriving (Eq, Ord, Show)

isIdling :: PeerState -> Bool
isIdling (PeerState {psIdling = Idling i}) = i

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

----- orphans

deriving instance Read LedgerStateJudgement

deriving anyclass instance TD.ToExpr LedgerStateJudgement

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

toGsmState :: ModelState -> GSM.GsmState
toGsmState = \case
    ModelPreSyncing   -> GSM.PreSyncing
    ModelSyncing      -> GSM.Syncing
    ModelCaughtUp{}   -> GSM.CaughtUp

toMarker :: GSM.GsmState -> MarkerState
toMarker = \case
    GSM.PreSyncing -> Absent
    GSM.Syncing    -> Absent
    GSM.CaughtUp   -> Present

-----

onset :: Selection -> SI.Time
onset (Selection _b (S s)) = SI.Time $ fromIntegral s

ageLimit :: Num a => a
ageLimit = 10   -- seconds

thrashLimit :: Num a => a
thrashLimit = 8   -- seconds

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
     Selection
  -> Candidate
  -> GSM.CandidateVersusSelection
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
    expiry          timestamp = expiryAge `max` expiryThrashing timestamp
    expiryAge                 = SI.addTime ageLimit (onset sel)
    expiryThrashing timestamp = SI.addTime thrashLimit timestamp

    clk' = SI.addTime (0.1 * fromIntegral dur) clk

    boringSelection = clk' /= expiryAge

    boringState = case st of
        ModelPreSyncing         -> True
        ModelSyncing            -> True
        ModelCaughtUp timestamp ->
            let gap = clk' `SI.diffTime` expiry timestamp
                n   =
                  mod
                      (diffTimeToPicoseconds gap)
                      (secondsToPicoseconds thrashLimit)
            in gap < 0 || 0 /= n

    secondsToPicoseconds x = x * 10 ^ (12 :: Int)

-----
