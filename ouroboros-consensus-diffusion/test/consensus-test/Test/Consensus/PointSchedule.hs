{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Data types and generators that convert a 'BlockTree' to a 'PointSchedule'.
--
-- Point schedules can have arbitrary configurations that model different behaviors
-- we want to use for tests.
--
-- Each generator takes a set of 'AnchoredFragment's corresponding to the tested peers'
-- chains, and converts them to a 'PointSchedule' consisting of a sequence of states
-- ('AdvertisedPoints'), each of which is associated with a single peer.
--
-- When a schedule is executed in a test, each tick is processed in order.
-- The peer associated with the current tick is considered "active", which means that
-- its ChainSync server is allowed to continue processing messages, while all the other
-- peers' servers suspend operation by blocking on a concurrency primitive.
-- The state in the current tick determines the actions that the peer is allowed to perform,
-- and once it fulfills the state's criteria, it yields control back to the scheduler,
-- who then activates the next tick's peer.
--
-- /Note/: At the moment this implementation is experimental.
module Test.Consensus.PointSchedule (
    AdvertisedPoints (..)
  , BlockPoint (..)
  , GenesisTest (..)
  , GenesisWindow (..)
  , HeaderPoint (..)
  , NodeState (..)
  , PeerSchedule
  , PointSchedule (..)
  , PointScheduleConfig (..)
  , TestFrag
  , TestFragH
  , Tick (..)
  , TipPoint (..)
  , balanced
  , banalStates
  , blockPointBlock
  , defaultPointScheduleConfig
  , enrichedWith
  , fromSchedulePoints
  , genesisAdvertisedPoints
  , headerPointBlock
  , longRangeAttack
  , peerSchedulesBlocks
  , pointScheduleBlocks
  , pointSchedulePeers
  , prettyGenesisTest
  , prettyPointSchedule
  , stToGen
  , tipPointBlock
  , uniformPoints
  ) where

import           Control.Monad.Class.MonadTime.SI (Time (Time), addTime,
                     diffTime)
import           Control.Monad.ST (ST)
import           Data.Foldable (toList)
import           Data.Functor (($>))
import           Data.List (mapAccumL, partition, scanl', transpose)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import           Data.Time (DiffTime)
import           Data.Word (Word64)
import           Ouroboros.Consensus.Block.Abstract (WithOrigin (..), getHeader)
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam,
                     maxRollbacks)
import           Ouroboros.Consensus.Util.Condense (Condense (condense))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (Empty, (:>)))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Tip (..), tipFromHeader)
import           Ouroboros.Network.Point (WithOrigin (At))
import qualified System.Random.Stateful as Random
import           System.Random.Stateful (STGenM, StatefulGen, runSTGen_)
import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..),
                     prettyBlockTree)
import           Test.Consensus.PointSchedule.Peers (Peer (..), PeerId (..),
                     Peers (..), getPeerIds, mkPeers, peersList)
import           Test.Consensus.PointSchedule.SinglePeer
                     (IsTrunk (IsBranch, IsTrunk), PeerScheduleParams (..),
                     SchedulePoint (..), defaultPeerScheduleParams, mergeOn,
                     peerScheduleFromTipPoints, schedulePointToBlock)
import           Test.Consensus.PointSchedule.SinglePeer.Indices
                     (uniformRMDiffTime)
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Asc,
                     Delta (Delta), ascVal)
import           Test.QuickCheck (Gen, arbitrary)
import           Test.QuickCheck.Random (QCGen)
import           Test.Util.TersePrinting (terseBlock, terseHeader, terseTip,
                     terseWithOrigin)
import           Test.Util.TestBlock (Header, TestBlock, Validity (Valid),
                     testHeader, unsafeTestBlockWithPayload)
import           Text.Printf (printf)

----------------------------------------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------------------------------------

type TestFrag = AnchoredFragment TestBlock

type TestFragH = AnchoredFragment (Header TestBlock)

-- | The current tip that a ChainSync server should advertise to the client in
-- a tick.
newtype TipPoint =
  TipPoint (Tip TestBlock)
  deriving (Eq, Show)

instance Condense TipPoint where
  condense (TipPoint tip) = terseTip tip

-- | Convert a 'TipPoint' to a 'TestBlock'.
tipPointBlock :: TipPoint -> Maybe TestBlock
tipPointBlock (TipPoint TipGenesis) = Nothing
tipPointBlock (TipPoint (Tip slot hash _)) =
  Just $ unsafeTestBlockWithPayload hash slot Valid ()

-- | The latest header that should be sent to the client by the ChainSync server
-- in a tick.
newtype HeaderPoint =
  HeaderPoint (WithOrigin (Header TestBlock))
  deriving (Eq, Show)

instance Condense HeaderPoint where
  condense (HeaderPoint header) = terseWithOrigin terseHeader header

-- | Convert a 'HeaderPoint' to a 'TestBlock'.
headerPointBlock :: HeaderPoint -> Maybe TestBlock
headerPointBlock (HeaderPoint Origin)      = Nothing
headerPointBlock (HeaderPoint (At header)) = Just $ testHeader header

-- | The latest block that should be sent to the client by the BlockFetch server
-- in a tick.
newtype BlockPoint =
  BlockPoint (WithOrigin TestBlock)
  deriving (Eq, Show)

instance Condense BlockPoint where
  condense (BlockPoint block) = terseWithOrigin terseBlock block

-- | Convert a 'BlockPoint' to a 'Point'.
blockPointBlock :: BlockPoint -> Maybe TestBlock
blockPointBlock (BlockPoint Origin)     = Nothing
blockPointBlock (BlockPoint (At block)) = Just block

-- | The set of parameters that define the state that a peer should reach when it receives control
-- by the scheduler in a single tick.
--
-- REVIEW: I find this rather poorly named. If it is really what is advertised
-- then isn't it weird to have the fragment in it? If it is the whole internal
-- state of the (online) node, then maybe we can call it that?
data AdvertisedPoints =
  AdvertisedPoints {
    tip    :: TipPoint,
    header :: HeaderPoint,
    block  :: BlockPoint
  }
  deriving (Eq, Show)

instance Condense AdvertisedPoints where
  condense AdvertisedPoints {tip, header, block} =
    "TP " ++ condense tip ++
    " | HP " ++ condense header ++
    " | BP " ++ condense block

genesisAdvertisedPoints :: AdvertisedPoints
genesisAdvertisedPoints =
  AdvertisedPoints {
    tip = TipPoint TipGenesis,
    header = HeaderPoint Origin,
    block = BlockPoint Origin
  }

-- | The state of a peer in a single tick.
--
-- At the moment, this is only used to encode the fact that a peer does not have a current state
-- before it has been active for the first time.
--
-- REVIEW: Is that necessary/useful?
data NodeState =
  -- | The peer is online and advertises the given points.
  NodeOnline AdvertisedPoints
  |
  -- | The peer should not respond to messages.
  NodeOffline
  deriving (Eq, Show)

instance Condense NodeState where
  condense = \case
    NodeOnline points -> condense points
    NodeOffline -> "*chrrrk* <signal lost>"

-- | A tick is an entry in a 'PointSchedule', containing the peer that is
-- going to change state.
data Tick =
  Tick {
    active   :: Peer NodeState,
    -- | The duration of this tick, for the scheduler to pass to @threadDelay@.
    duration :: DiffTime,
    number   :: Word
  }
  deriving (Eq, Show)

instance Condense Tick where
  condense Tick {active, duration, number} =
    show number ++ ": " ++ condense active ++ " | " ++ showDT duration
    where
      showDT t = printf "%.6f" (realToFrac t :: Double)

tickDefault :: PointScheduleConfig -> Word -> Peer NodeState -> Tick
tickDefault PointScheduleConfig {pscTickDuration} number active =
  Tick {active, duration = pscTickDuration, number}

tickDefaults :: PointScheduleConfig -> [Peer NodeState] -> [Tick]
tickDefaults psc states =
  uncurry (tickDefault psc) <$> zip [0 ..] states

-- | A point schedule is a series of states for a set of peers.
--
-- Each state defines which parts of the peer's chain are supposed to be served in the
-- given tick.
-- Each tick gives agency to only a single peer, which should process messages regularly
-- until the given state is reached, while the other peers block.
data PointSchedule =
  PointSchedule
    { ticks   :: NonEmpty Tick
    , peerIds :: NonEmpty PeerId -- ^ The peer ids that are involved in this point schedule.
                                 -- Ticks can only refer to these peers.
    }
  deriving (Eq, Show)

instance Condense PointSchedule where
  condense (PointSchedule ticks _) = unlines (condense <$> toList ticks)

prettyPointSchedule :: PointSchedule -> [String]
prettyPointSchedule PointSchedule{ticks} =
  "PointSchedule:" : (("  " ++) <$> (condense <$> toList ticks))

-- | Parameters that are significant for components outside of generators, like the peer
-- simulator.
data PointScheduleConfig =
  PointScheduleConfig {
    -- | Duration of a tick, for timeouts in the scheduler.
    pscTickDuration :: DiffTime
  }
  deriving (Eq, Show)

defaultPointScheduleConfig :: PointScheduleConfig
defaultPointScheduleConfig =
  PointScheduleConfig {pscTickDuration = 0.1}

----------------------------------------------------------------------------------------------------
-- Accessors
----------------------------------------------------------------------------------------------------

-- | Get the names of the peers involved in this point schedule.
-- This is the main motivation for requiring the point schedule to be
-- nonempty, so we don't have to carry around another value for the
-- 'PeerId's.
pointSchedulePeers :: PointSchedule -> NonEmpty PeerId
pointSchedulePeers = peerIds

-- | List of all blocks appearing in the schedule as tip point, header point or
-- block point.
pointScheduleBlocks :: PointSchedule -> [TestBlock]
pointScheduleBlocks PointSchedule{ticks} =
  catMaybes $ concatMap
    (\Tick{active=Peer{value}} -> case value of
         NodeOffline -> []
         NodeOnline (AdvertisedPoints{tip, header, block}) ->
           [tipPointBlock tip, headerPointBlock header, blockPointBlock block])
    ticks

----------------------------------------------------------------------------------------------------
-- Conversion to 'PointSchedule'
----------------------------------------------------------------------------------------------------

-- | Create a point schedule from a list of ticks
pointSchedule :: [Tick] -> NonEmpty PeerId -> PointSchedule
pointSchedule [] _nePeerIds = error "pointSchedule: no ticks"
pointSchedule ticks nePeerIds = PointSchedule (NonEmpty.fromList ticks) nePeerIds

-- | Convert a @SinglePeer@ schedule to a 'NodeState' schedule.
--
-- Accumulates the new points in each tick into the previous state, starting with a set of all
-- 'Origin' points.
--
-- Also shifts all tick start times so that the first tip point is announced at the very beginning
-- of the test, keeping the relative delays of the schedule intact.
-- This is a preliminary measure to make the long range attack test work, since that relies on the
-- honest node sending headers later than the adversary, which is not possible if the adversary's
-- first tip point is delayed by 20 or more seconds due to being in a later slot.
--
-- Finally, drops the first state, since all points being 'Origin' (in particular the tip) has no
-- useful effects in the simulator, but it could set the tip in the GDD governor to 'Origin', which
-- causes slow nodes to be disconnected right away.
peerStates :: Peer PeerSchedule -> [(Time, Peer NodeState)]
peerStates Peer {name, value = schedulePoints} =
  drop 1 (zip (Time 0 : (map shiftTime times)) (Peer name . NodeOnline <$> scanl' modPoint genesisAdvertisedPoints points))
  where
    shiftTime :: Time -> Time
    shiftTime t = addTime (- firstTipOffset) t

    firstTipOffset :: DiffTime
    firstTipOffset = case times of [] -> 0; (Time dt : _) -> dt

    modPoint z = \case
      ScheduleTipPoint tip -> z {tip = TipPoint (tipFromHeader tip)}
      ScheduleHeaderPoint h -> z {header = HeaderPoint (At (getHeader h))}
      ScheduleBlockPoint b -> z {block = BlockPoint (At b)}

    (times, points) = unzip schedulePoints

type PeerSchedule = [(Time, SchedulePoint)]

-- | Convert a set of @SinglePeer@ schedules to a 'PointSchedule'.
--
-- Call 'peerStates' for each peer, then merge all of them sorted by tick start times, then convert
-- start times to relative tick durations.
fromSchedulePoints :: Peers PeerSchedule -> PointSchedule
fromSchedulePoints peers = do
  pointSchedule (zipWith3 Tick states durations [0 ..]) peerIds
  where
    peerIds = getPeerIds peers

    durations = snd (mapAccumL (\ prev start -> (start, diffTime start prev)) (Time 0) (drop 1 starts)) ++ [0.1]

    (starts, states) = unzip $ foldr (mergeOn fst) [] (peerStates <$> toList (peersList peers))

-- | List of all blocks appearing in the schedule.
peerScheduleBlocks :: PeerSchedule -> [TestBlock]
peerScheduleBlocks = map (schedulePointToBlock . snd)

-- | List of all blocks appearing in the schedules.
peerSchedulesBlocks :: Peers PeerSchedule -> [TestBlock]
peerSchedulesBlocks = concatMap (peerScheduleBlocks . value) . toList . peersList

----------------------------------------------------------------------------------------------------
-- Schedule generators
----------------------------------------------------------------------------------------------------

-- | Create a peer schedule by serving one header in each tick.
banalStates :: TestFrag -> [NodeState]
banalStates (Empty _) = []
banalStates frag@(_ :> tipBlock) =
  spin [] frag
  where
    spin z (Empty _) = z
    spin z (pre :> block) =
      let header = HeaderPoint $ At (getHeader block)
       in spin
            (NodeOnline AdvertisedPoints {tip, header, block = BlockPoint (At block)} : z)
            pre
    tip = TipPoint $ tipFromHeader tipBlock

-- | Generate a point schedule from a set of peer schedules by taking one element from each peer in
-- turn.
--
-- Implemented by concatenating the peers' schedules and transposing the result.
--
-- REVIEW: I see the point of this point schedule as an exercice to manipulate
-- them but I otherwise find it rather useless.
balanced ::
  PointScheduleConfig ->
  Peers [NodeState] ->
  PointSchedule
balanced config states =
  pointSchedule (tickDefaults config activeSeq) (getPeerIds states)
  where
    -- Sequence containing the first state of all the nodes in order, then the
    -- second in order, etc.
    activeSeq = concat $ transpose $ sequenceA (honest states) : (sequenceA <$> Map.elems (others states))

-- | Produce a schedule similar to @Frequencies (Peers 1 [10])@, using the new @SinglePeer@
-- generator.
--
-- We hardcode the two schedules to use the latest block as the initial tip point.
-- The honest peer gets a substantially larger (and disconnected) delay interval to ensure
-- that k+1 blocks are sent fast enough to trigger selection of a fork.
longRangeAttack ::
  StatefulGen g m =>
  BlockTree TestBlock ->
  g ->
  m (Peers PeerSchedule)
longRangeAttack BlockTree {btTrunk, btBranches = [branch]} g = do
  honest <- peerScheduleFromTipPoints g honParams [(IsTrunk, [AF.length btTrunk - 1])] btTrunk []
  adv <- peerScheduleFromTipPoints g advParams [(IsBranch, [AF.length (btbFull branch) - 1])] btTrunk [btbFull branch]
  pure (mkPeers honest [adv])
  where
    honParams = defaultPeerScheduleParams {pspHeaderDelayInterval = (0.3, 0.4)}
    advParams = defaultPeerScheduleParams {pspTipDelayInterval = (0, 0.1)}

longRangeAttack _ _ =
  error "longRangeAttack can only deal with single adversary"

-- | Generate a schedule in which the trunk and branches are served by one peer each, using
-- a single tip point, without specifically assigned delay intervals like in
-- 'newLongRangeAttack'.
--
-- Include rollbacks in a percentage of adversaries, in which case that peer uses two branchs.
--
uniformPoints ::
  StatefulGen g m =>
  BlockTree TestBlock ->
  g ->
  m (Peers PeerSchedule)
uniformPoints BlockTree {btTrunk, btBranches} g = do
  honestTip0 <- firstTip btTrunk
  honest <- mkSchedule [(IsTrunk, [honestTip0 .. AF.length btTrunk - 1])] []
  advs <- takeBranches btBranches
  pure (mkPeers honest advs)
  where
    takeBranches = \case
        [] -> pure []
        [b] -> pure <$> withoutRollback b
        b1 : b2 : branches -> do
          a <- Random.uniformDouble01M g
          if a < rollbackProb
          then do
            this <- withRollback b1 b2
            rest <- takeBranches branches
            pure (this : rest)
          else do
            this <- withoutRollback b1
            rest <- takeBranches (b2 : branches)
            pure (this : rest)

    withoutRollback branch = do
      tips <- mkTips branch
      mkSchedule tips [btbSuffix branch]

    withRollback b1 b2 = do
      firstTips <- mkTips b1
      let secondTips = [AF.length (btbSuffix b2) - 1]
      mkSchedule (firstTips ++ [(IsBranch, secondTips)]) [btbSuffix b1, btbSuffix b2]

    mkSchedule tips branches = do
      params <- mkParams
      peerScheduleFromTipPoints g params tips btTrunk branches

    mkTips branch = do
      tip0 <- firstTip (btbFull branch)
      let (pre, post) = partition (< firstSuffixBlock) [tip0 .. lastBlock]
      pure ((if null pre then [] else [(IsTrunk, pre)]) ++ [(IsBranch, (shift <$> post))])
      where
        shift i = i - firstSuffixBlock
        firstSuffixBlock = lastBlock - AF.length (btbSuffix branch) + 1
        lastBlock = AF.length full - 1
        full = btbFull branch

    firstTip frag = pure (AF.length frag - 1)

    mkParams = do
      tipL <- uniformRMDiffTime (0, 0.5) g
      tipU <- uniformRMDiffTime (1, 2) g
      headerL <- uniformRMDiffTime (0.018, 0.03) g
      headerU <- uniformRMDiffTime (0.021, 0.04) g
      pure defaultPeerScheduleParams {pspTipDelayInterval = (tipL, tipU), pspHeaderDelayInterval = (headerL, headerU)}

    rollbackProb = 0.2

newtype GenesisWindow = GenesisWindow { unGenesisWindow :: Word64 }
  deriving (Show)

-- | All the data used by point schedule tests.
data GenesisTest schedule = GenesisTest {
  gtHonestAsc     :: Asc,
  gtSecurityParam :: SecurityParam,
  gtGenesisWindow :: GenesisWindow,
  gtDelay         :: Delta,
  gtBlockTree     :: BlockTree TestBlock,
  gtSchedule      :: schedule
  }

prettyGenesisTest :: GenesisTest schedule -> [String]
prettyGenesisTest GenesisTest{gtHonestAsc, gtSecurityParam, gtGenesisWindow, gtDelay = Delta delta, gtBlockTree} =
  [ "GenesisTest:"
  , "  gtHonestAsc: " ++ show (ascVal gtHonestAsc)
  , "  gtSecurityParam: " ++ show (maxRollbacks gtSecurityParam)
  , "  gtGenesisWindow: " ++ show (unGenesisWindow gtGenesisWindow)
  , "  gtDelay: " ++ show delta
  , "  gtBlockTree:"
  ] ++ (("    " ++) <$> prettyBlockTree gtBlockTree)

instance Functor GenesisTest where
  fmap f gt@GenesisTest{gtSchedule} = gt {gtSchedule = f gtSchedule}

enrichedWith :: (Functor f, Monad m) => m (f a) -> (f a -> m b) -> m (f b)
enrichedWith mfa convert = mfa >>= \fa -> (fa $>) <$> convert fa

-- | Wrap a 'ST' generator in 'Gen'.
stToGen ::
  (forall s . STGenM QCGen s -> ST s a) ->
  Gen a
stToGen gen = do
  seed :: QCGen <- arbitrary
  pure (runSTGen_ seed gen)
