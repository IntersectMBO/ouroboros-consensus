{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.PointSchedule.Tests.FullTable (tests) where

import           Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import           Control.Monad.Class.MonadTime.SI (DiffTime, Time (Time))
import           Data.Bifunctor (first)
import           Data.Functor ((<&>))
import           Data.List (sortOn)
import           Data.List.NonEmpty (NonEmpty ((:|)), fromList, nonEmpty,
                     toList)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import           Data.Maybe (maybeToList)
import           Data.Time.Clock (picosecondsToDiffTime)
import           Ouroboros.Consensus.Block (HasHeader)
import           Ouroboros.Network.Block (Tip (..), tipFromHeader)
import           Test.Consensus.PointSchedule (AdvertisedPoints (..),
                     BlockPoint (BlockPoint), HeaderPoint (HeaderPoint),
                     NodeState (..), PointSchedule (..), Tick (..),
                     TipPoint (TipPoint))
import           Test.Consensus.PointSchedule.FullTable
                     (FullTablePointSchedule (..), FullTableRow (..),
                     activeFromFullTablePointSchedule,
                     fullTableFromActivePointSchedule)
import           Test.Consensus.PointSchedule.Peers (Peer (..), PeerId (..),
                     Peers (..), peersFromPeerIdList')
import           Test.QuickCheck (Arbitrary (arbitrary, shrink), Gen, Property,
                     chooseInteger, counterexample, elements, forAllShrink,
                     frequency, listOf, listOf1, shrinkList, suchThat)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Util.TestBlock (Header (TestHeader), TestBlock,
                     TestBlockWith, TestHash, Validity (Valid),
                     pattern TestHash, unsafeBlockWithPayload)
import           Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests = testGroup "FullTable" [
    adjustQuickCheckTests (`div` 5) $
    testProperty
      "roundtrip: full-table -> active -> full-table"
      prop_roundtripFromFullTable
    ,
    adjustQuickCheckTests (* 4) $
    testProperty
      "roundtrip: active -> full-table -> active"
      prop_roundtripFromActive
   ]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

prop_roundtripFromFullTable :: Property
prop_roundtripFromFullTable =
  forAllShrink genFullTablePointSchedule shrinkFullTablePointSchedule $ \ps1 ->
    let ps2 = activeFromFullTablePointSchedule ps1
        ps3 = fullTableFromActivePointSchedule ps2
     in counterexample ("Input:  " ++ show ps1) $
        counterexample ("Output: " ++ show ps3) $
        counterexample ("Intermediary: " ++ show ps2) $
        ps1 == ps3

prop_roundtripFromActive :: Property
prop_roundtripFromActive =
  forAllShrink genActivePointSchedule shrinkActivePointSchedule $ \ps1 ->
    let ps2 = fullTableFromActivePointSchedule ps1
        ps3 = activeFromFullTablePointSchedule ps2
     in counterexample ("Input:  " ++ show ps1) $
        counterexample ("Output: " ++ show ps3) $
        counterexample ("Intermediary: " ++ show ps2) $
        ps1 == ps3

--------------------------------------------------------------------------------
-- High-level generation/shrinking of full-table point schedules
--------------------------------------------------------------------------------

-- | Generator for the first row of a full-table point schedule, given a list of
-- peer ids which must include the honest peer. This row always has time 0 and
-- never has all peers offline simultaneously.
genFirstFullTableRowFor :: NonEmpty PeerId -> Gen FullTableRow
genFirstFullTableRowFor peerIds = do
  ftrStates <- genPeersNotAllOfflineFor peerIds
  pure FullTableRow{ftrStates, ftrTime = Time 0}

-- | Generator for a normal row of a full-table point schedule, given a list of
-- peer ids which must include the honest peer.
genFullTableRowFor :: NonEmpty PeerId -> Gen FullTableRow
genFullTableRowFor peerIds = do
  ftrStates <- genPeersFor peerIds arbitrary
  ftrTime <- arbitrary
  pure FullTableRow{ftrStates, ftrTime}

-- | Generator of full-table point schedules. Those schedules contain at least
-- an honest peer, the first row has time 0, and there are no duplicate rows.
genFullTablePointSchedule :: Gen FullTablePointSchedule
genFullTablePointSchedule =
  do
    ftpsPeerIds <- (HonestPeer :|) <$> listOf arbitrary
    row <- genFirstFullTableRowFor ftpsPeerIds
    rows <- sortOn ftrTime <$> listOf (genFullTableRowFor ftpsPeerIds)
    pure $ FullTablePointSchedule{ftpsRows = row :| rows, ftpsPeerIds}
  `suchThat` noDuplicateStates

-- | Shrink of full-table point schedules by trying to remove rows, remove
-- non-honest peers, and by simplifying rows. The first row always has time 0,
-- and there never are duplicate rows.
shrinkFullTablePointSchedule :: FullTablePointSchedule -> [FullTablePointSchedule]
shrinkFullTablePointSchedule ps =
    filter noDuplicateStates $
      shrinkRows ps
      ++ shrinkColumns ps
  where
    -- | Remove as many rows as possible, but not the first one.
    shrinkRows FullTablePointSchedule{ftpsRows = firstRow :| otherRows, ftpsPeerIds} =
      (shrinkList shrinkRow otherRows <&> flip FullTablePointSchedule ftpsPeerIds . (firstRow :|))
      ++ (shrinkRow firstRow <&> flip FullTablePointSchedule ftpsPeerIds . (:| otherRows))

    -- | Remove non-honest peers.
    shrinkColumns FullTablePointSchedule{ftpsRows, ftpsPeerIds} = do
      otherPeers <- shrinkList (const []) $ filter (/= HonestPeer) $ toList ftpsPeerIds
      ftpsRows' <- maybeToList $ nonEmpty $ map (shrinkOtherPeers otherPeers) (toList ftpsRows)
      pure $ FullTablePointSchedule ftpsRows' (HonestPeer :| otherPeers)

    -- | Shrink one row, making it carry more trivial content.
    shrinkRow :: FullTableRow -> [FullTableRow]
    shrinkRow FullTableRow{ftrStates, ftrTime} = do
      ftrStates' <- shrinkPeers ftrStates
      pure $ FullTableRow ftrStates' ftrTime

    -- | Shrink a row, keeping only the given other peers.
    shrinkOtherPeers :: [PeerId] -> FullTableRow -> FullTableRow
    shrinkOtherPeers otherPeers FullTableRow{ftrStates=Peers{honest, others}, ftrTime} =
      let others' = Map.fromList $ map (\peer -> (peer, others ! peer)) otherPeers
          ftrStates' = Peers honest others'
       in FullTableRow ftrStates' ftrTime

-- | Predicate that checks whether the given full-table point schedule contains
-- the same states in consecutive rows and returns @False@ if that is the case.
noDuplicateStates :: FullTablePointSchedule -> Bool
noDuplicateStates = all ((== 1) . length) . NonEmpty.group1 . fmap ftrStates . ftpsRows

--------------------------------------------------------------------------------
-- High-level generation/shrinking of active point schedules
--------------------------------------------------------------------------------

-- | Generator for an active point schedule tick, given a list of peer ids to
-- choose from. The state generated is never 'NodeOffline'.
genActiveTickFor :: NonEmpty PeerId -> Gen Tick
genActiveTickFor peerIds = do
  peerId <- elements (toList peerIds)
  active <- Peer peerId <$> (NodeOnline <$> arbitrary)
  duration <- arbitrary
  pure Tick{active, duration}

-- | Generator of active point schedules. States are never 'NodeOffline'.
genActivePointSchedule :: Gen PointSchedule
genActivePointSchedule = do
  peerIds <- (HonestPeer :|) <$> listOf arbitrary
  ticks' <- fromList <$> listOf1 (genActiveTickFor peerIds)
  -- Always guarantee that the last duration is 1.
  let ticks = mapLast (\Tick{active} -> Tick{active, duration = 1}) ticks'
  pure $ PointSchedule{ticks, peerIds}

-- | Shrink of active point schedules by trying to remove ticks, and by
-- simplifying rows.
shrinkActivePointSchedule :: PointSchedule -> [PointSchedule]
shrinkActivePointSchedule = shrinkTicks
  where
    -- | Remove as many ticks as possible, but not the last one.
    shrinkTicks PointSchedule{ticks=allTicks, peerIds} =
      let (ticks, tick) = extractLast allTicks
      in (shrinkList shrinkTick ticks <&> flip PointSchedule peerIds . (|: tick))
         ++ (shrinkTick tick <&> flip PointSchedule peerIds . (ticks |:))

    -- | Shrink one tick, making it carry more trivial content.
    shrinkTick :: Tick -> [Tick]
    shrinkTick Tick{active, duration} = do
      active' <- shrink active
      pure $ Tick active' duration

--------------------------------------------------------------------------------
-- Orphan 'Arbitrary' for all the point schedule components
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    pure (x :| xs)

instance Arbitrary SlotNo where
  arbitrary = SlotNo <$> arbitrary

instance Arbitrary TestHash where
  arbitrary = TestHash <$> arbitrary

instance Arbitrary a => Arbitrary (TestBlockWith a) where
  arbitrary = do
    hash <- arbitrary
    slot <- arbitrary
    payload <- arbitrary
    pure $ unsafeBlockWithPayload hash slot Valid payload

instance Arbitrary (Header TestBlock) where
  arbitrary = TestHeader <$> arbitrary

instance (Arbitrary blk, HasHeader blk) => Arbitrary (Tip blk) where
  arbitrary =
    frequency [
      (1, pure TipGenesis),
      (99, tipFromHeader <$> arbitrary)
      ]
  shrink TipGenesis = []
  shrink (Tip {})   = [TipGenesis]

instance Arbitrary a => Arbitrary (WithOrigin a) where
  arbitrary =
    frequency [
      (1, pure Origin),
      (99, At <$> arbitrary)
      ]
  shrink Origin = []
  shrink (At a) = Origin : (At <$> shrink a)

instance Arbitrary TipPoint where
  arbitrary = TipPoint <$> arbitrary
  shrink (TipPoint tip) = TipPoint <$> shrink tip

instance Arbitrary HeaderPoint where
  arbitrary = HeaderPoint <$> arbitrary
  shrink (HeaderPoint header) = HeaderPoint <$> shrink header

instance Arbitrary BlockPoint where
  arbitrary = BlockPoint <$> arbitrary
  shrink (BlockPoint block) = BlockPoint <$> shrink block

instance Arbitrary AdvertisedPoints where
  arbitrary = do
    tip <- arbitrary
    header <- arbitrary
    block <- arbitrary
    pure $ AdvertisedPoints {tip, header, block}
  shrink AdvertisedPoints{tip, header, block} =
    ((\tip' -> AdvertisedPoints tip' header block) <$> shrink tip)
    ++ ((\header' -> AdvertisedPoints tip header' block) <$> shrink header)
    ++ ((\block' -> AdvertisedPoints tip header block') <$> shrink block)

instance Arbitrary NodeState where
  arbitrary =
    frequency [
      (1, pure NodeOffline),
      (19, NodeOnline <$> arbitrary)
      ]
  shrink NodeOffline         = []
  shrink (NodeOnline points) = NodeOnline <$> shrink points

-- | WARNING: Only non-honest peer ids.
instance Arbitrary PeerId where
  arbitrary = PeerId <$> arbitrary

-- | WARNING: Only non-honest peers.
instance Arbitrary a => Arbitrary (Peer a) where
  arbitrary = do
    peerId <- arbitrary
    Peer peerId <$> arbitrary
  shrink (Peer pid value) = Peer pid <$> shrink value

genPeersFor :: NonEmpty PeerId -> Gen a -> Gen (Peers a)
genPeersFor peerIds g = traverse (const g) (peersFromPeerIdList' peerIds)

genPeersNotAllOfflineFor :: NonEmpty PeerId -> Gen (Peers NodeState)
genPeersNotAllOfflineFor peerIds =
    genPeersFor peerIds arbitrary `suchThat` \Peers{honest, others} ->
       not (isOffline honest && all isOffline others)
  where
    isOffline (Peer _ NodeOffline) = True
    isOffline _                    = False

shrinkPeers :: Arbitrary a => Peers a -> [Peers a]
shrinkPeers Peers{honest, others} = do
  honest' <- shrink honest
  others' <- shrink `mapM` others
  pure $  Peers honest' others'

instance Arbitrary DiffTime where
  arbitrary = picosecondsToDiffTime <$> chooseInteger (1_000_000_000, 1_000_000_000_000)

instance Arbitrary Time where
  arbitrary = Time <$> arbitrary

mapFirst :: (a -> a) -> NonEmpty a -> NonEmpty a
mapFirst f (x :| xs) = f x :| xs

mapLast :: (a -> a) -> NonEmpty a -> NonEmpty a
mapLast f = NonEmpty.reverse . mapFirst f . NonEmpty.reverse

extractLast :: NonEmpty a -> ([a], a)
extractLast = go . NonEmpty.toList
  where
    go :: [a] -> ([a], a)
    go []       = error "extractLast"
    go [x]      = ([], x)
    go (x : xs) = first (x :) (go xs)

(|:) :: [a] -> a -> NonEmpty a
(|:) [] z       = z :| []
(|:) (x : xs) z = NonEmpty.cons x (xs |: z)
