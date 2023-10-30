{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

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
  , RollbackMode (..)
  , ScheduleType (..)
  , TestFrag
  , TestFragH
  , Tick (..)
  , TipPoint (..)
  , banalStates
  , defaultPointScheduleConfig
  , genSchedule
  , genSchedule'
  , interleaveBalanced
  , mkPeers
  , onlyHonestWithMintingPointSchedule
  , peersOnlyHonest
  , pointSchedulePeers
  ) where

import           Control.Monad (join)
import           Data.Foldable (for_, toList, foldrM)
import           Data.Hashable (Hashable)
import           Data.List (mapAccumL, sortOn, tails, transpose, inits, scanl')
import           Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String (IsString (fromString))
import           Data.Time (DiffTime)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block (blockHash, withOrigin)
import           Ouroboros.Consensus.Block.Abstract (HasHeader, getHeader)
import           Ouroboros.Consensus.Protocol.Abstract
                     (SecurityParam (SecurityParam))
import           Ouroboros.Consensus.Util.Condense (Condense (condense))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (Empty, (:>)), anchorFromBlock,
                     fromOldestFirst, headSlot, toOldestFirst)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (SlotNo, Tip (Tip, TipGenesis),
                     blockNo, blockSlot, getTipSlotNo, tipFromHeader)
import           Ouroboros.Network.Point (WithOrigin (At))
import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..),
                     prettyPrint, addBranch, mkTrunk)
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Asc)
import           Test.Util.TestBlock (Header (TestHeader), TestBlock,
                     TestHash (TestHash), tbSlot, modifyFork, successorBlock, firstBlock)

----------------------------------------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------------------------------------

type TestFrag = AnchoredFragment TestBlock

type TestFragH = AnchoredFragment (Header TestBlock)

data NonEmptyFrag =
  NonEmptyFrag {
    nefTip  :: TestBlock,
    nefFrag :: TestFrag
  }
  deriving (Eq, Show)

instance Condense NonEmptyFrag where
  condense = condense . nefFrag

nonEmptyFragment :: TestFrag -> Maybe NonEmptyFrag
nonEmptyFragment = \case
  Empty _ -> Nothing
  nefFrag@(_ :> nefTip) -> Just NonEmptyFrag {nefFrag, nefTip}

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
newtype PointSchedule =
  PointSchedule {ticks :: NonEmpty Tick}
  deriving (Eq, Show)

instance Condense PointSchedule where
  condense (PointSchedule ticks) = unlines (condense <$> toList ticks)

-- | Parameters that are significant for components outside of generators, like the peer
-- simulator.
data PointScheduleConfig =
  PointScheduleConfig {
    -- | Duration of a tick, for timeouts in the scheduler.
    pscTickDuration :: DiffTime,

    -- | @k@. We all know it
    psSecurityParam :: SecurityParam
  }
  deriving (Eq, Show)

defaultPointScheduleConfig :: SecurityParam -> PointScheduleConfig
defaultPointScheduleConfig psSecurityParam =
  PointScheduleConfig {pscTickDuration = 0.1, psSecurityParam}

----------------------------------------------------------------------------------------------------
-- Accessors
----------------------------------------------------------------------------------------------------

-- | Extract all 'PeerId's.
getPeerIds :: Peers a -> NonEmpty PeerId
getPeerIds peers = HonestPeer :| Map.keys (others peers)

blockTreePeersFull :: BlockTree TestBlock -> Peers (TestFrag, (TestFrag, (TestFrag, TestFrag)))
blockTreePeersFull BlockTree {btTrunk, btBranches} =
  Peers {
    honest = Peer HonestPeer (btTrunk, (btTrunk, (btTrunk, btTrunk))),
    others = Map.fromList (branches btBranches)
  }
  where
    branches = \case
      [b] -> [peer "adversary" b]
      bs -> uncurry branch <$> zip [1 :: Int ..] bs

    branch num =
      peer (PeerId ("adversary " <> show num))

    peer pid BlockTreeBranch {btbPrefix, btbSuffix, btbTrunkSuffix} =
      (pid, Peer pid (btTrunk, (btbPrefix, (btbSuffix, btbTrunkSuffix))))

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
peer2Point :: Peers a -> PeerSchedule -> Maybe PointSchedule
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
-- Folding and traversal functions
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

-- | Apply the first function to the honest peer and the second one to all
-- others.
bimapPeers :: (a -> b) -> (a -> b) -> Peers a -> Peers b
bimapPeers hon oth Peers {honest, others} =
  Peers {honest = hon <$> honest, others = fmap oth <$> others}

----------------------------------------------------------------------------------------------------
-- Schedule generators
----------------------------------------------------------------------------------------------------

stateWithTip :: TestBlock -> TestBlock -> NodeState
stateWithTip tipBlock block =
  NodeOnline AdvertisedPoints {
    tip = TipPoint (tipFromHeader tipBlock),
    header = HeaderPoint (getHeader block),
    block = BlockPoint block
  }

statesWithTip :: TestBlock -> [TestBlock] -> [NodeState]
statesWithTip tipBlock =
  fmap (stateWithTip tipBlock)

-- | Create a peer schedule by serving one header in each tick.
banalStates :: TestFrag -> [NodeState]
banalStates (Empty _)            = []
banalStates frag@(_ :> tipBlock) = statesWithTip tipBlock (toOldestFirst frag)

-- | Generate a point schedule from a set of peer schedules by taking one element from each peer in
-- turn.
--
-- Implemented by concatenating the peers' schedules and transposing the result.
--
-- REVIEW: I see the point of this point schedule as an exercice to manipulate
-- them but I otherwise find it rather useless.
interleaveBalanced ::
  Peers [NodeState] ->
  Maybe PointSchedule
interleaveBalanced states =
  peer2Point states (PeerSchedule activeSeq)
  where
    -- Sequence containing the first state of all the nodes in order, then the
    -- second in order, etc.
    activeSeq = concat $ transpose $ sequenceA (honest states) : (sequenceA <$> Map.elems (others states))

-- | Generate a point schedule that serves a single header in each tick for each
-- peer in turn. See 'blockTreePeers' for peers generation.
banalPointSchedule ::
  BlockTree TestBlock ->
  Maybe PointSchedule
banalPointSchedule blockTree =
  interleaveBalanced (banalStates <$> blockTreePeers blockTree)

-- | Interleave a set of peer schedules with a fixed occurrence frequency for each peer.
--
-- This is implemented by assigning interval lengths to each peer tick based on the frequency
-- config in the first argument, then sorting the resulting absolute times.
interleaveWithFrequencies ::
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
  Peers Int ->
  Peers [NodeState] ->
  Maybe PointSchedule
interleaveWithFrequencies freqs states =
  peer2Point freqs (PeerSchedule (fmap snd <$> sortOn (fst . value) catted))
  where
    catted = sequenceA =<< toList (peersList intvals)

    intvals = uncurry mkIntvals <$> zipPeers freqs states

    mkIntvals freq ss = zip (peerIntervals (length ss) freq) ss

    peerIntervals :: Int -> Int -> [Double]
    peerIntervals count freq =
      (* intvalLen) <$> [1 .. fromIntegral count]
      where
        intvalLen = 1 / fromIntegral freq

-- | Generate a point schedule for the scenario in which some peers send blocks at a different
-- rate than others.
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
  Peers Int ->
  BlockTree TestBlock ->
  Maybe PointSchedule
frequencyPointSchedule freqs blockTree =
  interleaveWithFrequencies freqs states
  where
    states = banalStates <$> blockTreePeers blockTree

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

data RollbackMode =
  RollbackOneBranch
  |
  RollbackRepeatBranch
  deriving (Eq, Show)

-- | Generate a peer schedule that rolls back repeatedly.
--
-- See 'rollbackSpamPointSchedule'.
rollbackSpamPeerSingle ::
  Bool ->
  (TestFrag, (TestFrag, (TestFrag, TestFrag))) ->
  [NodeState]
rollbackSpamPeerSingle bulk (_, (prefix, (fork, suffix))) =
  (prefixStates ++ join (zipWith (++) forkStates suffixStates))
  where
    prefixStates = banalStates prefix
    suffixStates = rollback (banalStates suffix)
    forkStates = rollback (banalStates fork)
    rollback frag | bulk = pure <$> frag
                  | otherwise = tails frag

rollbackSpamPointScheduleSingle ::
  Peers Int ->
  Bool ->
  BlockTree TestBlock ->
  Maybe (PointSchedule, Maybe a)
rollbackSpamPointScheduleSingle freqs bulk blockTree =
  fmap (\ a -> (a, Nothing)) (interleaveWithFrequencies freqs peers)
  where
    peers = bimapPeers (banalStates . fst) (rollbackSpamPeerSingle bulk) (blockTreePeersFull blockTree)

-- | First serve the prefix, advertising the honest tip.
-- Then serve k blocks from the fork, advertising the fork's tip.
-- Then move the fork's anchor one block ahead, rewriting all the hashes and slot numbers.
-- Serve the extra honest block, then serve k blocks off the shifted fork, advertising the new fork tip for both.
-- Repeat until the honest tip is the anchor.
rollbackSpamPeerRepeat ::
  SecurityParam ->
  Bool ->
  Word64 ->
  (NonEmptyFrag, (TestFrag, (NonEmptyFrag, TestFrag))) ->
  (Word64, ([NodeState], [TestFrag]))
rollbackSpamPeerRepeat (SecurityParam k) bulk forkNo0 (honest, (prefix, (fork, suffix))) =
  (newForkNo, (prefixStates ++ allNewForkStates, newBranches))
  where
    allNewForkStates = newForkStates =<< zip suffixBlocks newForks

    newBranches :: [TestFrag]
    newBranches = newBranch <$> zipWith (++) (inits suffixBlocks) newForks

    newBranch f = fromOldestFirst (AF.anchor prefix) (prefixBlocks ++ f)

    (newForkNo, newForks) = mapAccumL newFork forkNo0 (prefixAnchor : (Just <$> toOldestFirst suffix))

    newFork n anchor =
      case forkOffsets of
        (_, slotDiffH) : t ->
          (n + 1, scanl' add (modifyFork (const n) (inc slotDiffH block0)) t)
        [] -> (n, [])
      where
        add z (_, slotDiff) = (inc slotDiff (successorBlock z))
        inc d b = b {tbSlot = slot0 + d}
        (block0, slot0) = case anchor of
          Nothing -> (firstBlock n, 0)
          Just a -> (successorBlock a, tbSlot a)

    prefixAnchor = case prefix of
      Empty _ -> Nothing
      (_ :> b) -> Just b

    forkOffsets =
      forkOffset <$> zip [1 ..] (toOldestFirst (nefFrag fork))

    forkOffset (n, block) =
      (take n (toList (testHash block)), tbSlot block - xSlot)

    xSlot = withOrigin 0 id (headSlot prefix)

    testHash b = let (TestHash h) = blockHash b in h

    prefixStates = blocks (nefTip honest) (AF.takeOldest (AF.length prefix) (nefFrag honest))

    blocks tip bs | bulk, (_ :> h) <- bs = [stateWithTip tip h]
                  | otherwise = statesWithTip tip (toOldestFirst bs)

    newForkStates (extraH, bs) =
      case (reverse bs, reverse bsk) of
        (tip : _, h : _) | bulk -> [stateWithTip tip h]
                         | otherwise -> statesWithTip tip (extraH : bsk)
        _ -> []
      where
        bsk = take (fromIntegral k) bs

    prefixBlocks = toOldestFirst prefix

    suffixBlocks = toOldestFirst suffix

rollbackSpamPointScheduleRepeat ::
  PointScheduleConfig ->
  Peers Int ->
  Bool ->
  BlockTree TestBlock ->
  Maybe (PointSchedule, Maybe (BlockTree TestBlock))
rollbackSpamPointScheduleRepeat PointScheduleConfig {psSecurityParam} freqs bulk blockTree = do
  schedule <- interleaveWithFrequencies freqs (fst <$> peers)
  let newTree = foldrM addBranch (mkTrunk (btTrunk blockTree)) (snd . value =<< Map.elems (others peers))
  pure (schedule, newTree)
  where
    Peers {honest = Peer _ h0, others = o0} = blockTreePeersFull blockTree
    peers = Peers {
      honest = Peer HonestPeer (banalStates (fst h0), []),
      others = Map.fromList (snd (mapAccumL peer 1 (Map.elems o0)))
      }

    peer n (Peer pid (full, (prefix, (fork, suffix)))) =
      case (nonEmptyFragment full, nonEmptyFragment fork) of
        (Just nefFull, Just nefFork) ->
          let (n1, v) = rollbackSpamPeerRepeat psSecurityParam bulk n (nefFull, (prefix, (nefFork, suffix)))
          in (n1, (pid, Peer pid v))
        _ -> (n, (pid, Peer pid ([], [])))

-- | Generate a point schedule in which adversaries roll back repeatedly.
--
-- The adversary first serves the honest prefix, then alternates between
-- the honest and adversarial chain, advancing by one HP each tick.
--
-- If @bulk@ is 'True', the new HP is advertised immediately, otherwise
-- it will step through the entire fragment each time.
rollbackSpamPointSchedule ::
  RollbackMode ->
  PointScheduleConfig ->
  Peers Int ->
  Bool ->
  BlockTree TestBlock ->
  Maybe (PointSchedule, Maybe (BlockTree TestBlock))
rollbackSpamPointSchedule = \case
  RollbackOneBranch -> const rollbackSpamPointScheduleSingle
  RollbackRepeatBranch -> rollbackSpamPointScheduleRepeat

----------------------------------------------------------------------------------------------------
-- Main API
----------------------------------------------------------------------------------------------------

-- | Encodes the different scheduling styles for use with quickcheck generators.
data ScheduleType =
  -- | Serve one point per peer tick, interleaving peer schedules with different
  -- occurrence frequencies.
  Frequencies (Peers Int)
  |
  -- | Serve one point per peer tick, with uniform frequency.
  Banal
  |
  -- | Serve only the honest chain, in a single tick.
  OnlyHonest
  |
  -- | Each adversary advertises @k@ blocks, then rolls back, advances one block, and repeats.
  RollbackSpam
    (Peers Int)
    -- | Whether to advertise the short fragments in a single tick ('True') or individually.
    Bool
    RollbackMode
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
genSchedule' ::
  PointScheduleConfig ->
  ScheduleType ->
  BlockTree TestBlock ->
  Maybe (PointSchedule, Maybe (BlockTree TestBlock))
genSchedule' conf = \case
  Frequencies fs -> sameTree . frequencyPointSchedule fs
  Banal -> sameTree . banalPointSchedule
  OnlyHonest -> sameTree . onlyHonestPointSchedule
  RollbackSpam freqs bulk mode -> rollbackSpamPointSchedule mode conf freqs bulk
  where
    sameTree =
      fmap $ \ ps -> (ps, Nothing)

-- | Create a point schedule from the given block tree.
--
-- The first argument determines the scheduling style.
genSchedule ::
  PointScheduleConfig ->
  ScheduleType ->
  BlockTree TestBlock ->
  Maybe PointSchedule
genSchedule conf t =
  fmap fst . genSchedule' conf t
