{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.PointSchedule (module Test.Ouroboros.Consensus.ChainGenerator.Tests.PointSchedule) where

import           Data.Hashable (Hashable)
import           Data.List (mapAccumL, transpose)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String (IsString (fromString))
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block.Abstract (HasHeader, getHeader)
import           Ouroboros.Consensus.Util.Condense (Condense (condense))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (Empty, (:>)), anchorFromBlock)
import           Ouroboros.Network.Block (SlotNo, Tip (Tip, TipGenesis),
                     blockNo, blockSlot, getTipSlotNo, tipFromHeader)
import           Ouroboros.Network.Point (WithOrigin (At))
import           Test.Util.TestBlock (Header (TestHeader), TestBlock)

type TestFrag = AnchoredFragment TestBlock

type TestFragH = AnchoredFragment (Header TestBlock)

newtype TipPoint =
  TipPoint (Tip TestBlock)
  deriving (Eq, Show)

instance Condense TipPoint where
  condense (TipPoint TipGenesis) = "genesis"
  condense (TipPoint (Tip slot _ bno)) =
      "B:" <> condense bno <> ",S:" <> condense slot

newtype HeaderPoint =
  HeaderPoint (Header TestBlock)
  deriving (Eq, Show)

instance Condense HeaderPoint where
  condense (HeaderPoint (TestHeader b)) =
    "B:" <> condense (blockNo b) <> ",S:" <> condense (blockSlot b)

newtype BlockPoint =
  BlockPoint TestBlock
  deriving (Eq, Show)

instance Condense BlockPoint where
  condense (BlockPoint b) =
    "B:" <> condense (blockNo b) <> ",S:" <> condense (blockSlot b)

-- REVIEW: I find this rather poorly named. If it is really what is advertised
-- then isn't it weird to have the fragment in it? If it is the whole internal
-- state of the (online) node, then maybe we can call it that?
data AdvertisedPoints =
  AdvertisedPoints {
    tip      :: TipPoint,
    header   :: HeaderPoint,
    block    :: BlockPoint,
    fragment :: TestFrag
  }
  deriving (Eq, Show)

instance Condense AdvertisedPoints where
  condense AdvertisedPoints {tip, header, block} =
    "TP " ++ condense tip ++
    " | HP " ++ condense header ++
    " | BP " ++ condense block

-- REVIEW: Is that necessary/useful?
data NodeState =
  NodeOnline AdvertisedPoints
  |
  NodeOffline
  deriving (Eq, Show)

instance Condense NodeState where
  condense = \case
    NodeOnline points -> condense points
    NodeOffline -> "*chrrrk* <signal lost>"

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

foldHPeers :: (b -> Peer a -> b) -> (Peer a -> b) -> Peers a -> b
foldHPeers adv hon Peers {honest, others} =
  Map.foldl' adv (hon honest) others

zipPeers :: Peers a -> Peers b -> Peers (a, b)
zipPeers a b =
  Peers {honest = Peer HonestPeer (value (honest a), value (honest b)), others = Map.intersectionWith zp (others a) (others b)}
  where
    zp p1 p2 = Peer (name p1) (value p1, value p2)

getPeer :: PeerId -> Peers a -> Maybe a
getPeer HonestPeer ps = Just $ value $ honest ps
getPeer pid ps        = value <$> others ps Map.!? pid

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

data PointSchedule =
  PointSchedule { ticks :: [Tick], frags :: Peers TestFrag }
  deriving (Eq, Show)

instance Condense PointSchedule where
  condense (PointSchedule ticks _) = unlines (condense <$> ticks)

banalStates :: TestFrag -> [NodeState]
banalStates (Empty _) = []
banalStates frag@(_ :> tipBlock) =
  spin [] frag
  where
    spin z (Empty _) = z
    spin z fragment@(pre :> block) =
      let header = HeaderPoint $ getHeader block
       in spin
            (NodeOnline AdvertisedPoints {tip, header, block = BlockPoint block, fragment} : z)
            pre
    tip = TipPoint $ tipFromHeader tipBlock

-- REVIEW: I see the point of this point schedule as an exercice to manipulate
-- them but I otherwise find it rather useless.
balanced ::
  Peers TestFrag ->
  Peers [NodeState] ->
  PointSchedule
balanced frags states =
  PointSchedule (snd (mapAccumL step initial activeSeq)) frags
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

newtype PeerSchedule =
  PeerSchedule [Peer NodeState]
  deriving (Eq, Show)

peer2Point :: Peers TestFrag -> PeerSchedule -> PointSchedule
peer2Point ps (PeerSchedule n) =
  PointSchedule (snd (mapAccumL folder initial n)) ps
  where
    initial = NodeOffline <$ ps

    folder :: Peers NodeState -> Peer NodeState -> (Peers NodeState, Tick)
    folder z active =
      (new, Tick active new)
      where
        new = updatePeer z active

    updatePeer :: Peers a -> Peer a -> Peers a
    updatePeer Peers {honest, others} active =
      case name active of
        HonestPeer -> Peers {honest = active, others}
        name       -> Peers {honest, others = Map.insert name active others}

type PSTrans = TestFrag -> PeerSchedule -> PeerSchedule

-- | INVARIANT: same keys in Peers.others
foldGenPeers ::
  Peers TestFrag ->
  Peers PSTrans ->
  PeerSchedule
foldGenPeers frags gen =
  foldHPeers apA apH zp
  where
    zp = zipPeers frags gen
    apH :: Peer (TestFrag, PSTrans) -> PeerSchedule
    apH (Peer _ (frag, f)) = f frag (error "honest fold function thing")
    apA z (Peer _ (frag, f)) = f frag z

banalPointSchedule ::
  Peers TestFrag ->
  PointSchedule
banalPointSchedule frags =
  balanced frags (banalStates <$> frags)

fastAdversarySchedule ::
  Peers TestFrag ->
  PointSchedule
fastAdversarySchedule frags =
  peer2Point frags (foldGenPeers frags trans)
  where
    trans :: Peers PSTrans
    trans = Peers {
      honest = Peer HonestPeer (\ frag _ -> PeerSchedule (Peer HonestPeer <$> banalStates frag)),
      others = transOther <$> others frags
    }
    transOther :: Peer TestFrag -> Peer PSTrans
    transOther (Peer i _) =
      Peer i tr
      where
        tr frag (PeerSchedule z) =
          PeerSchedule (concat (snd (mapAccumL f states z)))
          where
            states :: [NodeState]
            states = banalStates frag
            f :: [NodeState] -> Peer NodeState -> ([NodeState], [Peer NodeState])
            f rest p@(Peer HonestPeer _)
              | let (pre, post) = splitAt 10 rest
              = (post, p : (Peer i <$> pre))
            f rest p = (rest, [p])

-- Schedule with only the honest peer advertising their whole fragment immediately.
onlyHonestPointSchedule :: TestFrag -> PointSchedule
onlyHonestPointSchedule (Empty _) = PointSchedule [] undefined
onlyHonestPointSchedule fragment@(_ :> tipBlock) =
  let honestPeerState =
        Peer HonestPeer $
          NodeOnline $
          AdvertisedPoints tipPoint headerPoint blockPoint fragment
  in
  PointSchedule [
    Tick {
      active = honestPeerState,
      peers = Peers honestPeerState Map.empty
    }
  ] undefined
  where
    tipPoint = TipPoint $ tipFromHeader tipBlock
    headerPoint = HeaderPoint $ getHeader tipBlock
    blockPoint = BlockPoint tipBlock

-- Schedule with only the honest peer advertising the honest chain immediately
-- as it becomes available.
onlyHonestWithMintingPointSchedule :: SlotNo -> Int -> TestFrag -> PointSchedule
onlyHonestWithMintingPointSchedule initialSlotNo _ticksPerSlot fullFragment@(_ :> finalBlock) =
  PointSchedule (map tickAtSlotNo [initialSlotNo .. finalSlotNo]) undefined
  where
    -- If we hold a block, we are guaranteed that the slot number cannot be
    -- origin?
    finalSlotNo = case getTipSlotNo $ tipFromHeader finalBlock of
      At s -> s
      _    -> error "unexpected alternative"

    advertisedPointsAtSlotNo :: SlotNo -> AdvertisedPoints
    advertisedPointsAtSlotNo slotNo =
      case fst $ splitFragmentAtSlotNo (At slotNo) fullFragment of
        Empty _ -> undefined
        fragment@(_ :> tipBlock) ->
          let tipPoint = TipPoint $ tipFromHeader tipBlock
              headerPoint = HeaderPoint $ getHeader tipBlock
              blockPoint = BlockPoint tipBlock
           in AdvertisedPoints tipPoint headerPoint blockPoint fragment

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
  WithOrigin SlotNo ->
  AnchoredFragment b ->
  (AnchoredFragment b, AnchoredFragment b)
splitFragmentAtSlotNo slotNo (fragment :> block) =
  if blockSlotNo block <= slotNo then
    (fragment :> block, Empty $ anchorFromBlock block)
  else
    let (firstPart, secondPart) = splitFragmentAtSlotNo slotNo fragment in
      (firstPart, secondPart :> block)
splitFragmentAtSlotNo _ (Empty anchor) =
  (Empty anchor, Empty anchor)

-- | Returns the block number of the given slot.
-- REVIEW: might just be 'blockSlot'?
blockSlotNo :: HasHeader b => b -> WithOrigin SlotNo
blockSlotNo = getTipSlotNo . tipFromHeader
