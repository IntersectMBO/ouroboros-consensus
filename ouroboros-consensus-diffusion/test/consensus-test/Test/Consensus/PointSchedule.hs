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
-- chains, and converts them to a 'PeerSchedule' consisting of a sequence of states
-- ('AdvertisedPoints'), each of which is associated with a single peer.
--
-- The generated 'PeerSchedule' is transformed into a 'PointSchedule' that adds the current
-- states of the other peers to each entry (as a 'Tick').
--
-- When a schedule is executed in a test, each tick is processed in order.
-- The peer associated with the current tick is considered "active", which means that
-- its ChainSync server is allowed to continue processing messages, while all the other
-- peers' servers suspend operation by blocking on a concurrency primitive.
-- The state in the current tick determines the actions that the peer is allowed to perform,
-- and once it fulfills the state's criteria, it yields control back to the scheduler,
-- who then activates the next tick's peer.
--
-- /Note/: This is a prototype implementation, to be rewritten in a later work package.
module Test.Consensus.PointSchedule (
    AdvertisedPoints (..)
  , BlockPoint (..)
  , GenesisTest (..)
  , GenesisWindow (..)
  , HeaderPoint (..)
  , NodeState (..)
  , Peer (..)
  , PeerId (..)
  , PointSchedule (..)
  , ScheduleType (..)
  , TestFrag
  , TestFragH
  , Tick (..)
  , TipPoint (..)
  , genSchedule
  , onlyHonestWithMintingPointSchedule
  , pointSchedulePeers
  ) where

import           Data.Foldable (toList)
import           Data.Hashable (Hashable)
import           Data.List (mapAccumL, transpose)
import           Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String (IsString (fromString))
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block.Abstract (HasHeader, getHeader)
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam)
import           Ouroboros.Consensus.Util.Condense (Condense (condense))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (Empty, (:>)), anchorFromBlock)
import           Ouroboros.Network.Block (SlotNo, Tip (Tip, TipGenesis),
                     blockNo, blockSlot, getTipSlotNo, tipFromHeader)
import           Ouroboros.Network.Point (WithOrigin (At))
import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..))
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
  condense (TipPoint TipGenesis) = "genesis"
  condense (TipPoint (Tip slot _ bno)) =
      "B:" <> condense bno <> ",S:" <> condense slot

-- | The latest header that should be sent to the client by the ChainSync server
-- in a tick.
newtype HeaderPoint =
  HeaderPoint (Header TestBlock)
  deriving (Eq, Show)

instance Condense HeaderPoint where
  condense (HeaderPoint (TestHeader b)) =
    "B:" <> condense (blockNo b) <> ",S:" <> condense (blockSlot b)

-- | The latest block that should be sent to the client by the BlockFetch server
-- in a tick.
newtype BlockPoint =
  BlockPoint TestBlock
  deriving (Eq, Show)

instance Condense BlockPoint where
  condense (BlockPoint b) =
    "B:" <> condense (blockNo b) <> ",S:" <> condense (blockSlot b)

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
  -- | The peer is online and advertise the given points.
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

-- | Intermediate type that contains the states for only the active peers.
newtype PeerSchedule =
  PeerSchedule [Peer NodeState]
  deriving (Eq, Show)

-- | A tick is an entry in a 'PointSchedule', containing the node states for all peers
-- as well as a designated peer that should be processing messages during this tick.
--
-- REVIEW: What is the purpose of having the other peers as well in a
-- 'TickState'?
data Tick =
  Tick {
    active :: Peer NodeState,
    peers  :: Peers NodeState
  }
  deriving (Eq, Show)

instance Condense Tick where
  condense Tick {active} = condense active

-- | A point schedule is a series of states for a set of peers.
--
-- Each state defines which parts of the peer's chain are supposed to be served in the
-- given tick.
-- Each tick gives agency to only a single peer, which should process messages regularly
-- until the given state is reached, while the other peers block.
newtype PointSchedule =
  PointSchedule {ticks :: NonEmpty Tick}
  deriving (Eq, Show)

instance Condense PointSchedule where
  condense (PointSchedule ticks) = unlines (condense <$> toList ticks)

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
pointSchedulePeers PointSchedule{ticks = Tick {peers} :| _} =
  getPeerIds peers

----------------------------------------------------------------------------------------------------
-- Conversion to 'PointSchedule'
----------------------------------------------------------------------------------------------------

-- | Ensure that a 'PointSchedule' isn't empty.
pointSchedule :: [Tick] -> Maybe PointSchedule
pointSchedule ticks = PointSchedule <$> nonEmpty ticks

-- | Create the final 'PointSchedule' from a 'PeerSchedule', which consists of adding the inactive
-- peers' states to each tick.
--
-- - Initialize all peers to 'NodeOffline'.
--
-- - Fold over the list of active peer states in the 'PeerSchedule'.
--
-- - In a fold 'step', update the active peer's state in the accumulator (in @updatePeer@), then
--   emit a 'Tick' with both the active peer's state and the accumulator in it.
--
-- - 'mapAccumL' allows the step function to produce a new accumulator as well as a result list
--   element, so its final result is the accumulator after the last step as well as each step's
--   'Tick' as a new list.
--   We discard the final accumulator and pass the new list of 'Tick's to 'pointSchedule', which
--   ensures that the schedule is nonempty, and returns 'Nothing' otherwise.
peer2Point :: Peers TestFrag -> PeerSchedule -> Maybe PointSchedule
peer2Point ps (PeerSchedule n) =
  pointSchedule (snd (mapAccumL step initial n))
  where

    initial :: Peers NodeState
    initial = NodeOffline <$ ps

    step :: Peers NodeState -> Peer NodeState -> (Peers NodeState, Tick)
    step z active =
      (new, Tick active new)
      where
        new = updatePeer z active

    updatePeer :: Peers a -> Peer a -> Peers a
    updatePeer Peers {honest, others} active =
      case name active of
        HonestPeer -> Peers {honest = active, others}
        name       -> Peers {honest, others = Map.insert name active others}

----------------------------------------------------------------------------------------------------
-- Folding functions
----------------------------------------------------------------------------------------------------

-- | Fold a 'Peers' by applying the second argument to the honest 'Peer' as the initial
-- accumulator and applying the first argument to the accumulator and each 'Peer' in the
-- 'others' 'Map'.
foldHPeers :: (b -> Peer a -> b) -> (Peer a -> b) -> Peers a -> b
foldHPeers adv hon Peers {honest, others} =
  Map.foldl' adv (hon honest) others

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

type PSTrans = PeerId -> TestFrag -> PeerSchedule -> PeerSchedule

-- | Generate a 'PeerSchedule' from a set of fragments and a set of transformations.
--
-- The schedule is initialized by applying the transformation for the honest peer to the honest
-- fragment.
-- Then, it folds over all adversarial peers and applies each peer's transformation to the
-- accumulator and the peer's fragment.
foldGenPeers ::
  Peers TestFrag ->
  Peers PSTrans ->
  PeerSchedule
foldGenPeers frags gen =
  foldHPeers apA apH zp
  where
    zp = zipPeers frags gen

    apH :: Peer (TestFrag, PSTrans) -> PeerSchedule
    apH (Peer i (frag, f)) = f i frag (PeerSchedule [])

    apA z (Peer i (frag, f)) = f i frag z

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
      let header = HeaderPoint $ getHeader block
       in spin
            (NodeOnline AdvertisedPoints {tip, header, block = BlockPoint block} : z)
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
  Peers [NodeState] ->
  Maybe PointSchedule
balanced states =
  pointSchedule (snd (mapAccumL step initial activeSeq))
  where
    step :: Tick -> Peer NodeState -> (Tick, Tick)
    step Tick {peers} active =
      let next = Tick {active, peers = updatePeer peers active}
       in (next, next)

    updatePeer :: Peers a -> Peer a -> Peers a
    updatePeer Peers {honest, others} active =
      case name active of
        HonestPeer -> Peers {honest = active, others}
        name       -> Peers {honest, others = Map.insert name active others}

    -- Sequence containing the first state of all the nodes in order, then the
    -- second in order, etc.
    activeSeq = concat $ transpose $ seqPeer (honest states) : (seqPeer <$> Map.elems (others states))

    seqPeer :: Peer [a] -> [Peer a]
    seqPeer Peer {name, value} =
      Peer name <$> value

    -- Initial state where all the peers are offline.
    initial = Tick {active = initialH, peers = Peers initialH ((NodeOffline <$) <$> others states)}
    initialH = Peer HonestPeer NodeOffline

-- | Generate a point schedule that servers a single header in each tick for each peer in turn.
banalPointSchedule ::
  BlockTree TestBlock ->
  Maybe PointSchedule
banalPointSchedule blockTree =
  balanced (banalStates <$> blockTreePeers blockTree)

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
-- This is implemented by initializing the schedule to contain only the honest chain, then folding
-- over the adversaries and inserting ten ticks before each honest tick.
-- It only makes sense when there's a single adversary – otherwise, each subsequent adversary will
-- be ten times as fast as the previous one.
--
-- It uses 'banalStates' to convert each fragment to a schedule (which advances by one block per
-- tick).
fastAdversaryPointSchedule ::
  BlockTree TestBlock ->
  Maybe PointSchedule
fastAdversaryPointSchedule blockTree =
  peer2Point frags (foldGenPeers frags trans)
  where
    frags = blockTreePeers blockTree

    -- These transformations are applied to the initial empty schedule in turn (first the honest
    -- one, then the adversaries in order of their names.) by 'foldGenPeers'.
    trans :: Peers PSTrans
    trans = Peers {
      honest = Peer HonestPeer (\ _ frag _ -> PeerSchedule (Peer HonestPeer <$> banalStates frag)),
      others = (transOther <$) <$> others frags
    }

    transOther :: PSTrans
    transOther i frag (PeerSchedule z) =
      PeerSchedule (concat (snd (mapAccumL step states z)))
      where
        states :: [NodeState]
        states = banalStates frag

        step :: [NodeState] -> Peer NodeState -> ([NodeState], [Peer NodeState])
        step rest p@(Peer HonestPeer _)
          | let (pre, post) = splitAt 10 rest
          = (post, p : (Peer i <$> pre))
        step rest p = (rest, [p])

-- | Generate a point schedule that consist of a single tick in which the honest peer advertises
-- its entire chain immediately.
onlyHonestPointSchedule :: BlockTree TestBlock -> Maybe PointSchedule
onlyHonestPointSchedule BlockTree {btTrunk = Empty _} = Nothing
onlyHonestPointSchedule BlockTree {btTrunk = _ :> tipBlock} =
  Just $ PointSchedule (pure tick)
  where
    tick = Tick {active = honestPeerState, peers = Peers honestPeerState Map.empty}
    honestPeerState = Peer HonestPeer (NodeOnline points)
    points = AdvertisedPoints tipPoint headerPoint blockPoint
    tipPoint = TipPoint $ tipFromHeader tipBlock
    headerPoint = HeaderPoint $ getHeader tipBlock
    blockPoint = BlockPoint tipBlock

-- | Generate a point schedule that consist of a single tick in which the honest peer advertises
-- its entire chain as it becomes available.
--
-- No idea what the point of this is.
onlyHonestWithMintingPointSchedule :: SlotNo -> Int -> TestFrag -> Maybe PointSchedule
onlyHonestWithMintingPointSchedule initialSlotNo _ticksPerSlot fullFragment@(_ :> finalBlock) =
  pointSchedule (map tickAtSlotNo [initialSlotNo .. finalSlotNo])
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
              headerPoint = HeaderPoint $ getHeader tipBlock
              blockPoint = BlockPoint tipBlock
           in AdvertisedPoints tipPoint headerPoint blockPoint

    tickAtSlotNo :: SlotNo -> Tick
    tickAtSlotNo slotNo =
      let honestPeerState =
            Peer HonestPeer $
              NodeOnline $
              advertisedPointsAtSlotNo slotNo
       in
          Tick {
            active = honestPeerState,
            peers = Peers honestPeerState Map.empty
          }
onlyHonestWithMintingPointSchedule _initialSlotNo _ticksPerSlot _fullFragment =
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

-- | Encodes the different scheduling styles for use with quickcheck generators.
data ScheduleType =
  FastAdversary
  |
  Banal
  |
  OnlyHonest
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
genSchedule :: ScheduleType -> BlockTree TestBlock -> Maybe PointSchedule
genSchedule = \case
  FastAdversary -> fastAdversaryPointSchedule
  Banal -> banalPointSchedule
  OnlyHonest -> onlyHonestPointSchedule
