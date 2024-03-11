{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

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
  , Peer (..)
  , PeerId (..)
  , Peers (..)
  , PointSchedule (..)
  , PointScheduleConfig (..)
  , ScheduleType (..)
  , TestFrag
  , TestFragH
  , Tick (..)
  , TipPoint (..)
  , balanced
  , banalStates
  , defaultPointScheduleConfig
  , fromSchedulePoints
  , genSchedule
  , mkPeers
  , onlyHonestWithMintingPointSchedule
  , peersOnlyHonest
  , pointSchedulePeers
  ) where

import           Data.Foldable (toList)
import           Data.Hashable (Hashable)
import           Data.List (mapAccumL, scanl', sortOn, transpose)
import           Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.String (IsString (fromString))
import           Data.Time (DiffTime)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block.Abstract (HasHeader,
                     WithOrigin (Origin), getHeader)
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam)
import           Ouroboros.Consensus.Util.Condense (Condense (condense))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (Empty, (:>)), anchorFromBlock)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (SlotNo, Tip (Tip, TipGenesis),
                     blockNo, blockSlot, getTipSlotNo, tipFromHeader)
import           Ouroboros.Network.Point (WithOrigin (At))
import           System.Random.Stateful (StatefulGen)
import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..))
import           Test.Consensus.PointSchedule.SinglePeer
                     (IsTrunk (IsBranch, IsTrunk), PeerScheduleParams (..),
                     SchedulePoint (..), defaultPeerScheduleParams, mergeOn,
                     peerScheduleFromTipPoints)
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Asc)
import           Test.Util.TestBlock (Header (TestHeader), TestBlock)

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
  condense (TipPoint TipGenesis) = "G"
  condense (TipPoint (Tip slot _ bno)) =
      "B:" <> condense bno <> ",S:" <> condense slot

-- | The latest header that should be sent to the client by the ChainSync server
-- in a tick.
newtype HeaderPoint =
  HeaderPoint (WithOrigin (Header TestBlock))
  deriving (Eq, Show)

instance Condense HeaderPoint where
  condense = \case
    HeaderPoint (At (TestHeader b)) ->
      "B:" <> condense (blockNo b) <> ",S:" <> condense (blockSlot b)
    HeaderPoint Origin ->
      "G"

-- | The latest block that should be sent to the client by the BlockFetch server
-- in a tick.
newtype BlockPoint =
  BlockPoint (WithOrigin TestBlock)
  deriving (Eq, Show)

instance Condense BlockPoint where
  condense = \case
    BlockPoint (At b) ->
      "B:" <> condense (blockNo b) <> ",S:" <> condense (blockSlot b)
    BlockPoint Origin ->
      "G"

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

-- | Identifier used to index maps and specify which peer is active during a tick.
data PeerId =
  HonestPeer
  |
  PeerId String
  deriving (Eq, Generic, Show, Ord)

instance IsString PeerId where
  fromString "honest" = HonestPeer
  fromString i        = PeerId i

instance Condense PeerId where
  condense = \case
    HonestPeer -> "honest"
    PeerId name -> name

instance Hashable PeerId

-- | General-purpose functor associated with a peer.
data Peer a =
  Peer {
    name  :: PeerId,
    value :: a
  }
  deriving (Eq, Show)

instance Functor Peer where
  fmap f Peer {name, value} = Peer {name, value = f value}

instance Foldable Peer where
  foldr step z (Peer _ a) = step a z

instance Traversable Peer where
  sequenceA (Peer name fa) =
    Peer name <$> fa

instance Condense a => Condense (Peer a) where
  condense Peer {name, value} = condense name ++ ": " ++ condense value

-- | General-purpose functor for a set of peers.
--
-- REVIEW: There is a duplicate entry for the honest peer, here. We should
-- probably either have only the 'Map' or have the keys of the map be 'String'?
--
-- Alternatively, we could just have 'newtype PeerId = PeerId String' with an
-- alias for 'HonestPeer = PeerId "honest"'?
data Peers a =
  Peers {
    honest :: Peer a,
    others :: Map PeerId (Peer a)
  }
  deriving (Eq, Show)

instance Functor Peers where
  fmap f Peers {honest, others} = Peers {honest = f <$> honest, others = fmap f <$> others}

-- | A tick is an entry in a 'PointSchedule', containing the peer that is
-- going to change state.
data Tick =
  Tick {
    active   :: Peer NodeState,
    -- | The duration of this tick, for the scheduler to pass to @threadDelay@.
    duration :: DiffTime
  }
  deriving (Eq, Show)

instance Condense Tick where
  condense Tick {active, duration} = condense active ++ " | " ++ show duration

tickDefault :: PointScheduleConfig -> Peer NodeState -> Tick
tickDefault PointScheduleConfig {pscTickDuration} active =
  Tick {active, duration = pscTickDuration}

-- | A set of peers with only one honest peer carrying the given value.
peersOnlyHonest :: a -> Peers a
peersOnlyHonest value =
  Peers {
    honest = Peer {name = HonestPeer, value},
    others = Map.empty
    }

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

-- | Extract all 'PeerId's.
getPeerIds :: Peers a -> NonEmpty PeerId
getPeerIds peers = HonestPeer :| Map.keys (others peers)

-- | Extract the trunk and all the branches from the 'BlockTree' and store them in
-- an honest 'Peer' and several adversarial ones, respectively.
blockTreePeers :: BlockTree TestBlock -> Peers TestFrag
blockTreePeers BlockTree {btTrunk, btBranches} =
  Peers {
    honest = Peer HonestPeer btTrunk,
    others = Map.fromList (branches btBranches)
  }
  where
    branches = \case
      [b] -> [peer "adversary" b]
      bs -> uncurry branch <$> zip [1 :: Int ..] bs

    branch num =
      peer (PeerId ("adversary " <> show num))

    peer pid BlockTreeBranch {btbFull} = (pid, Peer pid btbFull)

-- | Get the names of the peers involved in this point schedule.
-- This is the main motivation for requiring the point schedule to be
-- nonempty, so we don't have to carry around another value for the
-- 'PeerId's.
pointSchedulePeers :: PointSchedule -> NonEmpty PeerId
pointSchedulePeers = peerIds

-- | Convert 'Peers' to a list of 'Peer'.
peersList :: Peers a -> NonEmpty (Peer a)
peersList Peers {honest, others} =
  honest :| Map.elems others

-- | Construct 'Peers' from values, adding adversary names based on the default schema.
-- A single adversary gets the ID @adversary@, multiple get enumerated as @adversary N@.
mkPeers :: a -> [a] -> Peers a
mkPeers h as =
  Peers (Peer HonestPeer h) (Map.fromList (mkPeer <$> advs as))
  where
    mkPeer (pid, a) = (pid, Peer pid a)
    advs [a] = [("adversary", a)]
    advs _   = zip enumAdvs as
    enumAdvs = (\ n -> PeerId ("adversary " ++ show n)) <$> [1 :: Int ..]

----------------------------------------------------------------------------------------------------
-- Conversion to 'PointSchedule'
----------------------------------------------------------------------------------------------------

-- | Ensure that a 'PointSchedule' isn't empty.
pointSchedule :: [Tick] -> NonEmpty PeerId -> Maybe PointSchedule
pointSchedule ticks nePeerIds = (`PointSchedule` nePeerIds) <$> nonEmpty ticks

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
peerStates :: PeerId -> [(DiffTime, SchedulePoint)] -> [(DiffTime, Peer NodeState)]
peerStates peerId pts =
  zip (0 : (shiftTime <$> times)) (Peer peerId . NodeOnline <$> scanl' modPoint zero points)
  where
    shiftTime t = t - firstTipOffset

    modPoint z = \case
      ScheduleTipPoint tip -> z {tip = TipPoint (tipFromHeader tip)}
      ScheduleHeaderPoint h -> z {header = HeaderPoint (At (getHeader h))}
      ScheduleBlockPoint b -> z {block = BlockPoint (At b)}

    zero = AdvertisedPoints {
      tip = TipPoint TipGenesis,
      header = HeaderPoint Origin,
      block = BlockPoint Origin
    }

    firstTipOffset = fromMaybe 0 (listToMaybe times)

    (times, points) = unzip pts

-- | Convert a set of @SinglePeer@ schedules to a 'PointSchedule'.
--
-- Call 'peerStates' for each peer, then merge all of them sorted by tick start times, then convert
-- start times to relative tick durations.
fromSchedulePoints :: Map PeerId [(DiffTime, SchedulePoint)] -> Maybe PointSchedule
fromSchedulePoints peers = do
  peerIds <- nonEmpty (Map.keys peers)
  pointSchedule (zipWith Tick states durations) peerIds
  where
    durations = drop 1 (snd (mapAccumL (\ prev start -> (start, start - prev)) 0 (drop 1 starts))) ++ [0]

    (starts, states) = unzip $ foldr (mergeOn fst) [] [peerStates p sch | (p, sch) <- Map.toList peers]

----------------------------------------------------------------------------------------------------
-- Folding functions
----------------------------------------------------------------------------------------------------

-- | Combine two 'Peers' by creating tuples of the two honest 'Peer's and of each pair
-- of 'others' with the same 'PeerId', dropping any 'Peer' that is present in only one
-- of the 'Map's.
zipPeers :: Peers a -> Peers b -> Peers (a, b)
zipPeers a b =
  Peers {
    honest = Peer HonestPeer (value (honest a), value (honest b)),
    others = Map.intersectionWith zp (others a) (others b)
  }
  where
    zp p1 p2 = Peer (name p1) (value p1, value p2)

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
  Maybe PointSchedule
balanced config states =
  pointSchedule (map (tickDefault config) activeSeq) (getPeerIds states)
  where
    -- Sequence containing the first state of all the nodes in order, then the
    -- second in order, etc.
    activeSeq = concat $ transpose $ sequenceA (honest states) : (sequenceA <$> Map.elems (others states))

-- | Generate a point schedule that serves a single header in each tick for each
-- peer in turn. See 'blockTreePeers' for peers generation.
banalPointSchedule ::
  PointScheduleConfig ->
  BlockTree TestBlock ->
  Maybe PointSchedule
banalPointSchedule config blockTree =
  balanced config (banalStates <$> blockTreePeers blockTree)

-- | Generate a point schedule for the scenario in which adversaries send blocks much faster
-- than the honest node.
--
-- This is intended to test the Limit on Eagerness, which prevents the selection from advancing
-- far enough into a fork that the immutable tip moves into the fork as well (i.e. more than k
-- blocks).
--
-- The LoE is only resolved when all peers with forks at that block have been disconnected from,
-- in particular due to a decision based on the Genesis density criterion.
--
-- This is implemented by initializing each peer's schedule with 'banalStates' (which advances by
-- one block per tick) and assigning interval lengths to each peer tick based on the frequency
-- config in the first argument, then sorting the resulting absolute times.
frequencyPointSchedule ::
  -- | A set of relative frequencies.
  -- If peer A has a value of @2@ and peer B has @6@, peer B will get three turns in the schedule
  -- for each turn of A.
  --
  -- Given @Peers { honest = 1, others = [("A", 2), ("B", 10)] }@, we get a schedule like
  --
  -- @BBBBABBBBBHAB BBBBABBBBBHAB...@
  --
  -- with the intermediate interval representation:
  --
  -- B(1/10) B(2/10) B(3/10) B(4/10) A(1/2) B(5/10) B(6/10) B(7/10) B(8/10) B(9/10) H(1/1) (2/2) B(10/10)
  --
  -- With the order of equal values determined by the @PeerId@s.
  PointScheduleConfig ->
  Peers Int ->
  BlockTree TestBlock ->
  Maybe PointSchedule
frequencyPointSchedule config freqs blockTree =
  pointSchedule (map (tickDefault config . fmap snd) (sortOn (fst . value) catted)) (getPeerIds freqs)
  where
    catted = sequenceA =<< toList (peersList intvals)

    intvals = uncurry mkIntvals <$> zipPeers freqs states

    mkIntvals freq ss = zip (peerIntervals (length ss) freq) ss

    states = banalStates <$> frags

    frags = blockTreePeers blockTree

    peerIntervals :: Int -> Int -> [Double]
    peerIntervals count freq =
      (* intvalLen) <$> [1 .. fromIntegral count]
      where
        intvalLen = 1 / fromIntegral freq

-- | Generate a point schedule that consist of a single tick in which the honest peer advertises
-- its entire chain immediately.
onlyHonestPointSchedule ::
  PointScheduleConfig ->
  BlockTree TestBlock ->
  Maybe PointSchedule
onlyHonestPointSchedule _ BlockTree {btTrunk = Empty _} = Nothing
onlyHonestPointSchedule config BlockTree {btTrunk = _ :> tipBlock} =
  Just $ PointSchedule (pure tick) (HonestPeer :| [])
  where
    tick = tickDefault config honestPeerState
    honestPeerState = Peer HonestPeer (NodeOnline points)
    points = AdvertisedPoints tipPoint headerPoint blockPoint
    tipPoint = TipPoint (tipFromHeader tipBlock)
    headerPoint = HeaderPoint $ At (getHeader tipBlock)
    blockPoint = BlockPoint (At tipBlock)

-- | Generate a point schedule that consist of a single tick in which the honest peer advertises
-- its entire chain as it becomes available.
--
-- No idea what the point of this is.
onlyHonestWithMintingPointSchedule ::
  PointScheduleConfig ->
  SlotNo ->
  Int ->
  TestFrag ->
  Maybe PointSchedule
onlyHonestWithMintingPointSchedule config initialSlotNo _ticksPerSlot fullFragment@(_ :> finalBlock) =
  pointSchedule (map tickAtSlotNo [initialSlotNo .. finalSlotNo]) (HonestPeer :| [])
  where
    -- If we hold a block, we are guaranteed that the slot number cannot be
    -- origin?
    finalSlotNo = case getTipSlotNo $ tipFromHeader finalBlock of
      At s -> s
      _    -> error "unexpected alternative"

    advertisedPointsAtSlotNo :: SlotNo -> AdvertisedPoints
    advertisedPointsAtSlotNo slotNo =
      case fst $ splitFragmentAtSlotNo slotNo fullFragment of
        Empty _ -> error "onlyHonestWithMintingPointSchedule: there should be a block at that slot"
        (_ :> tipBlock) ->
          let tipPoint = TipPoint $ tipFromHeader tipBlock
              headerPoint = HeaderPoint $ At (getHeader tipBlock)
              blockPoint = BlockPoint (At tipBlock)
           in AdvertisedPoints tipPoint headerPoint blockPoint

    tickAtSlotNo :: SlotNo -> Tick
    tickAtSlotNo slotNo =
      let honestPeerState =
            Peer HonestPeer $
              NodeOnline $
              advertisedPointsAtSlotNo slotNo
       in
          tickDefault config honestPeerState
onlyHonestWithMintingPointSchedule _ _initialSlotNo _ticksPerSlot _fullFragment =
    error "unexpected alternative"

-- onlyHonestWithMintingPointSchedule' :: SlotNo -> Int -> TestFrag -> PointSchedule
-- onlyHonestWithMintingPointSchedule' initialSlotNo ticksPerSlot fullFragment =
--   let (availFragment, futureFragment) = splitFragmentAtSlotNo (At initialSlotNo) fullFragment
--       blockSlotNos = map blockSlotNo toOldestFirst futureFragment

-- | Given a slot number and an anchored fragment 'a', splits the fragment into
-- two 'b' and 'c' such that:
--
-- - 'b' is anchored in the same place as 'a' and contains all the blocks of 'a'
--   that have a slot number smaller than (or equal to) the given one.
--
-- - 'c' is anchored at the last block of 'b' and contains all the blocks of 'a'
--   that have a slot number strictly greater than the given one.
splitFragmentAtSlotNo ::
  HasHeader b =>
  SlotNo ->
  AnchoredFragment b ->
  (AnchoredFragment b, AnchoredFragment b)
splitFragmentAtSlotNo slotNo (fragment :> block) =
  if blockSlot block <= slotNo then
    (fragment :> block, Empty $ anchorFromBlock block)
  else
    let (firstPart, secondPart) = splitFragmentAtSlotNo slotNo fragment in
      (firstPart, secondPart :> block)
splitFragmentAtSlotNo _ (Empty anchor) =
  (Empty anchor, Empty anchor)

-- | Produce a schedule similar to @Frequencies (Peers 1 [10])@, using the new @SinglePeer@
-- generator.
--
-- We hardcode the two schedules to use the latest block as the initial tip point.
-- The honest peer gets a substantially larger (and disconnected) delay interval to ensure
-- that k+1 blocks are sent fast enough to trigger selection of a fork.
newLongRangeAttack ::
  StatefulGen g m =>
  g ->
  BlockTree TestBlock ->
  m (Maybe PointSchedule)
newLongRangeAttack g BlockTree {btTrunk, btBranches = [branch]} = do
  honest <- peerScheduleFromTipPoints g honParams [(IsTrunk, [AF.length btTrunk - 1])] btTrunk []
  adv <- peerScheduleFromTipPoints g advParams [(IsBranch, [AF.length (btbFull branch) - 1])] btTrunk [btbFull branch]
  pure (fromSchedulePoints (Map.fromList [(HonestPeer, honest), ("adversary 1", adv)]))
  where
    honParams = defaultPeerScheduleParams {pspHeaderDelayInterval = (0.3, 0.4)}
    advParams = defaultPeerScheduleParams {pspTipDelayInterval = (0, 0.1)}

newLongRangeAttack _ _ =
  pure Nothing

-- | Encodes the different scheduling styles for use with quickcheck generators.
data ScheduleType =
  Frequencies (Peers Int)
  -- ^ A schedule where each peer serves their own chain. The frequency at which
  -- they serve blocks is given as a map from peers to integers.
  |
  Banal
  -- ^ A schedule where each peer serves their own chain, one header at a time.
  -- This is similar to 'Frequencies' with all peers having frequency 1.
  |
  OnlyHonest
  -- ^ A schedule with only an honest node serving the trunk of the tree
  -- immediately in one tick.
  |
  NewLRA
  -- ^ “New long-range attack”. Similar to @Frequencies (Peers 1 [10])@ but
  -- relies on the 'SinglePeer' generator, and therefore contains
  -- randomly-chosen delays between messages.
  deriving (Eq, Show)

newtype GenesisWindow = GenesisWindow { getGenesisWindow :: Word64 }
  deriving (Show)

-- | All the data used by point schedule tests.
data GenesisTest = GenesisTest {
  gtHonestAsc     :: Asc,
  gtSecurityParam :: SecurityParam,
  gtGenesisWindow :: GenesisWindow,
  gtBlockTree     :: BlockTree TestBlock
  }

-- | Create a point schedule from the given block tree.
--
-- The first argument determines the scheduling style.
genSchedule ::
  StatefulGen g m =>
  g ->
  PointScheduleConfig ->
  ScheduleType ->
  BlockTree TestBlock ->
  m (Maybe PointSchedule)
genSchedule g config = \case
  Frequencies fs -> pure . frequencyPointSchedule config fs
  Banal -> pure . banalPointSchedule config
  OnlyHonest -> pure . onlyHonestPointSchedule config
  NewLRA -> newLongRangeAttack g
