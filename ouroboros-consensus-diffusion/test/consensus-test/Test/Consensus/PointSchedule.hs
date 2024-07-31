{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Data types and generators for point schedules.
--
-- Each generator takes a set of 'AnchoredFragment's corresponding to the tested
-- peers' chains, and converts them to a point schedule consisting of a sequence
-- of 'NodeState's, each of which is associated with a single peer.
--
-- When a schedule is executed in a test, each tick is processed in order.
-- The peer associated with the current tick is considered "active", which means that
-- its ChainSync server is allowed to continue processing messages, while all the other
-- peers' servers suspend operation by blocking on a concurrency primitive.
-- The state in the current tick determines the actions that the peer is allowed to perform,
-- and once it fulfills the state's criteria, it yields control back to the scheduler,
-- who then activates the next tick's peer.
module Test.Consensus.PointSchedule (
    BlockFetchTimeout (..)
  , CSJParams (..)
  , DowntimeParams (..)
  , ForecastRange (..)
  , GenesisTest (..)
  , GenesisTestFull
  , GenesisWindow (..)
  , LoPBucketParams (..)
  , PeerSchedule
  , PointSchedule (..)
  , PointsGeneratorParams (..)
  , RunGenesisTestResult (..)
  , enrichedWith
  , ensureScheduleDuration
  , genesisNodeState
  , longRangeAttack
  , peerSchedulesBlocks
  , peerStates
  , peersStates
  , peersStatesRelative
  , prettyGenesisTest
  , prettyPointSchedule
  , stToGen
  , uniformPoints
  ) where

import           Cardano.Slotting.Time (SlotLength)
import           Control.Monad (replicateM)
import           Control.Monad.Class.MonadTime.SI (Time (Time), addTime,
                     diffTime)
import           Control.Monad.ST (ST)
import           Data.Bifunctor (first)
import           Data.Functor (($>))
import           Data.List (mapAccumL, partition, scanl')
import           Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import           Data.Time (DiffTime)
import           Data.Word (Word64)
import           Ouroboros.Consensus.Block.Abstract (withOriginToMaybe)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (GenesisWindow (..))
import           Ouroboros.Consensus.Network.NodeToNode (ChainSyncTimeout (..))
import           Ouroboros.Consensus.Protocol.Abstract
                     (SecurityParam (SecurityParam), maxRollbacks)
import           Ouroboros.Consensus.Util.Condense (CondenseList (..),
                     PaddingDirection (..), condenseListWithPadding)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (SlotNo (..), blockSlot)
import           Ouroboros.Network.Point (withOrigin)
import qualified System.Random.Stateful as Random
import           System.Random.Stateful (STGenM, StatefulGen, runSTGen_)
import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..),
                     allFragments, prettyBlockTree)
import           Test.Consensus.PeerSimulator.StateView (StateView)
import           Test.Consensus.PointSchedule.NodeState (NodeState (..),
                     genesisNodeState)
import           Test.Consensus.PointSchedule.Peers (Peer (..), PeerId,
                     Peers (..), getPeerIds, peers', peersList)
import           Test.Consensus.PointSchedule.SinglePeer
                     (IsTrunk (IsBranch, IsTrunk), PeerScheduleParams (..),
                     SchedulePoint (..), defaultPeerScheduleParams, mergeOn,
                     peerScheduleFromTipPoints, schedulePointToBlock)
import           Test.Consensus.PointSchedule.SinglePeer.Indices
                     (uniformRMDiffTime)
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Delta (Delta))
import           Test.QuickCheck (Gen, arbitrary)
import           Test.QuickCheck.Random (QCGen)
import           Test.Util.TersePrinting (terseFragment)
import           Test.Util.TestBlock (TestBlock)
import           Text.Printf (printf)

prettyPointSchedule ::
  forall blk.
  (CondenseList (NodeState blk)) =>
  PointSchedule blk ->
  [String]
prettyPointSchedule ps@PointSchedule {psStartOrder, psMinEndTime} =
  []
    ++ [ "psSchedule ="
       ]
    ++ ( zipWith3
           ( \number time peerState ->
               "  " ++ number ++ ": " ++ peerState ++ " @ " ++ time
           )
           (condenseListWithPadding PadLeft $ fst <$> numberedPeersStates)
           (showDT . fst . snd <$> numberedPeersStates)
           (condenseList $ (snd . snd) <$> numberedPeersStates)
       )
    ++ [ "psStartOrder = " ++ show psStartOrder,
         "psMinEndTime = " ++ show psMinEndTime
       ]
  where
    numberedPeersStates :: [(Int, (Time, Peer (NodeState blk)))]
    numberedPeersStates = zip [0 ..] (peersStates ps)

    showDT :: Time -> String
    showDT (Time dt) = printf "%.6f" (realToFrac dt :: Double)

----------------------------------------------------------------------------------------------------
-- Conversion to 'PointSchedule'
----------------------------------------------------------------------------------------------------

-- | Convert a @SinglePeer@ schedule to a 'NodeState' schedule.
--
-- Accumulates the new points in each tick into the previous state, starting with a set of all
-- 'Origin' points.
--
-- Finally, drops the first state, since all points being 'Origin' (in particular the tip) has no
-- useful effects in the simulator, but it could set the tip in the GDD governor to 'Origin', which
-- causes slow nodes to be disconnected right away.
--
-- TODO Remove dropping the first state in favor of better GDD logic
peerStates :: Peer (PeerSchedule blk) -> [(Time, Peer (NodeState blk))]
peerStates Peer {name, value = schedulePoints} =
  drop 1 (zip (Time 0 : times) (Peer name <$> scanl' modPoint genesisNodeState points))
  where
    modPoint z = \case
      ScheduleTipPoint nsTip -> z {nsTip}
      ScheduleHeaderPoint nsHeader -> z {nsHeader}
      ScheduleBlockPoint nsBlock -> z {nsBlock}

    (times, points) = unzip schedulePoints

-- | Convert several @SinglePeer@ schedules to a common 'NodeState' schedule.
--
-- The resulting schedule contains all the peers. Items are sorted by time.
peersStates :: PointSchedule blk -> [(Time, Peer (NodeState blk))]
peersStates PointSchedule{psSchedule} =
  foldr (mergeOn fst) [] (peerStates <$> peersList psSchedule)

-- | Same as 'peersStates' but returns the duration of a state instead of the
-- absolute time at which it starts holding.
peersStatesRelative :: PointSchedule blk -> [(DiffTime, Peer (NodeState blk))]
peersStatesRelative peers =
  let (starts, states) = unzip $ peersStates peers
      durations = snd (mapAccumL (\ prev start -> (start, diffTime start prev)) (Time 0) (drop 1 starts)) ++ [0.1]
   in zip durations states

type PeerSchedule blk = [(Time, SchedulePoint blk)]

-- | List of all blocks appearing in the schedule.
peerScheduleBlocks :: (PeerSchedule blk) -> [blk]
peerScheduleBlocks = mapMaybe (withOriginToMaybe . schedulePointToBlock . snd)

data PointSchedule blk = PointSchedule {
    -- | The actual point schedule
    psSchedule   :: Peers (PeerSchedule blk),
    -- | The order in which the peers start and connect to the node under test.
    -- The peers that are absent from 'psSchedule' are ignored; the peers from
    -- 'psSchedule' that are absent of 'psStartOrder' are started in the end in
    -- the order of 'PeerId'.
    psStartOrder :: [PeerId],
    -- | Minimum duration for the simulation of this point schedule.
    -- If no point in the schedule is larger than 'psMinEndTime',
    -- the simulation will still run until this time is reached.
    psMinEndTime :: Time
  }

-- | List of all blocks appearing in the schedules.
peerSchedulesBlocks :: Peers (PeerSchedule blk) -> [blk]
peerSchedulesBlocks = concatMap (peerScheduleBlocks . value) . peersList

----------------------------------------------------------------------------------------------------
-- Schedule generators
----------------------------------------------------------------------------------------------------

-- | Produce a schedule similar to @Frequencies (Peers 1 [10])@, using the new @SinglePeer@
-- generator.
--
-- We hardcode the two schedules to use the latest block as the initial tip point.
-- The honest peer gets a substantially larger (and disconnected) delay interval to ensure
-- that k+1 blocks are sent fast enough to trigger selection of a fork.
longRangeAttack ::
  (StatefulGen g m, AF.HasHeader blk) =>
  BlockTree blk ->
  g ->
  m (PointSchedule blk)
longRangeAttack BlockTree {btTrunk, btBranches = [branch]} g = do
  honest <- peerScheduleFromTipPoints g honParams [(IsTrunk, [AF.length btTrunk - 1])] btTrunk []
  adv <- peerScheduleFromTipPoints g advParams [(IsBranch, [AF.length (btbFull branch) - 1])] btTrunk [btbFull branch]
  pure $ shiftPointSchedule $ PointSchedule {
    psSchedule = peers' [honest] [adv],
    psStartOrder = [],
    psMinEndTime = Time 0
    }
  where
    honParams = defaultPeerScheduleParams {pspHeaderDelayInterval = (0.3, 0.4)}
    advParams = defaultPeerScheduleParams {pspTipDelayInterval = (0, 0.1)}

longRangeAttack _ _ =
  error "longRangeAttack can only deal with single adversary"

data PointsGeneratorParams = PointsGeneratorParams {
  pgpExtraHonestPeers :: Int,
  pgpDowntime         :: DowntimeParams
}

data DowntimeParams = NoDowntime | DowntimeWithSecurityParam SecurityParam

uniformPoints ::
  (StatefulGen g m, AF.HasHeader blk) =>
  PointsGeneratorParams ->
  BlockTree blk ->
  g ->
  m (PointSchedule blk)
uniformPoints PointsGeneratorParams {pgpExtraHonestPeers, pgpDowntime} bt =
  fmap shiftPointSchedule . case pgpDowntime of
    NoDowntime                  ->
      uniformPointsWithExtraHonestPeers pgpExtraHonestPeers bt
    DowntimeWithSecurityParam k ->
      uniformPointsWithExtraHonestPeersAndDowntime pgpExtraHonestPeers k bt

-- | Shifts all tick start times so that the first tip point is announced at
-- the very beginning of the test, keeping the relative delays of the schedule
-- intact.
--
-- This is a measure to make the long range attack test work, since that
-- relies on the honest node sending headers later than the adversary, which
-- is not possible if the adversary's first tip point is delayed by 20 or
-- more seconds due to being in a later slot.
shiftPointSchedule :: PointSchedule blk -> PointSchedule blk
shiftPointSchedule s = s {psSchedule = shiftPeerSchedule <$> psSchedule s}
  where
    shiftPeerSchedule :: PeerSchedule blk -> PeerSchedule blk
    shiftPeerSchedule times = map (first shiftTime) times
      where
        shiftTime :: Time -> Time
        shiftTime t = addTime (- firstTipOffset) t

        firstTipOffset :: DiffTime
        firstTipOffset = case times of [] -> 0; ((Time dt, _) : _) -> dt


-- | Generate a schedule in which the trunk is served by @pgpExtraHonestPeers + 1@ peers,
-- and extra branches are served by one peer each, using a single tip point,
-- without specifically assigned delay intervals like in 'newLongRangeAttack'.
--
-- Include rollbacks in a percentage of adversaries, in which case that peer uses two branchs.
--
uniformPointsWithExtraHonestPeers ::
  forall g m blk.
  (StatefulGen g m, AF.HasHeader blk) =>
  Int ->
  BlockTree blk ->
  g ->
  m (PointSchedule blk)
uniformPointsWithExtraHonestPeers
    extraHonestPeers
    BlockTree {btTrunk, btBranches}
    g
  = do
  honestTip0 <- firstTip btTrunk
  honests <- replicateM (extraHonestPeers + 1) $
    mkSchedule [(IsTrunk, [honestTip0 .. AF.length btTrunk - 1])] []
  advs <- takeBranches btBranches
  let psSchedule = peers' honests advs
  psStartOrder <- shuffle (getPeerIds psSchedule)
  pure $ PointSchedule {psSchedule, psStartOrder, psMinEndTime = Time 0}
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

    -- Inefficient implementation, but sufficient for small lists.
    shuffle :: [a] -> m [a]
    shuffle [] = pure []
    shuffle xs = do
      i <- Random.uniformRM (0, length xs - 1) g
      let x = xs !! i
          xs' = take i xs ++ drop (i+1) xs
      (x :) <$> shuffle xs'

minusClamp :: (Ord a, Num a) => a -> a -> a
minusClamp a b | a <= b = 0
               | otherwise = a - b

zipPadN :: forall a . [[a]] -> [[Maybe a]]
zipPadN =
  spin []
  where
    spin acc as
      | all null as
      = reverse acc
      | let (h, t) = unzip (takeNext <$> as)
      = spin (h : acc) t

    takeNext = \case
      [] -> (Nothing, [])
      h : t -> (Just h, t)

isTip :: SchedulePoint blk -> Bool
isTip = \case
  ScheduleTipPoint _ -> True
  _ -> False

tipTimes :: [(Time, SchedulePoint blk)] -> [Time]
tipTimes =
  fmap fst . filter (isTip . snd)

bumpTips :: [Time] -> [(Time, SchedulePoint blk)] -> [(Time, SchedulePoint blk)]
bumpTips tips =
  snd . mapAccumL step tips
  where
    step (t0 : tn) (_, p)
      | isTip p
      = (tn, (t0, p))
    step ts a = (ts, a)

syncTips :: [[(Time, SchedulePoint blk)]] -> [[(Time, SchedulePoint blk)]] -> ([[(Time, SchedulePoint blk)]], [[(Time, SchedulePoint blk)]])
syncTips honests advs =
  (bump <$> honests, bump <$> advs)
  where
    bump = bumpTips earliestTips
    earliestTips = chooseEarliest <$> zipPadN (tipTimes <$> scheds)
    scheds = honests <> advs
    chooseEarliest times = minimum (fromMaybe (Time 0) <$> times)

-- | This is a variant of 'uniformPointsWithExtraHonestPeers' that uses multiple tip points, used to simulate node downtimes.
-- Ultimately, this should be replaced by a redesign of the peer schedule generator that is aware of node liveness
-- intervals.
--
-- Chooses the first tip points somewhere in the middle of the honest chain:
-- The "pause slot" is half of the honest head slot, or the slot of the kth block, whichever is greater.
-- The last block smaller than the pause slot is then used as the first tip for each branch.
-- The second tip is the last block of each branch.
--
-- Includes rollbacks in some schedules.
uniformPointsWithExtraHonestPeersAndDowntime ::
  forall g m blk.
  (StatefulGen g m, AF.HasHeader blk) =>
  Int ->
  SecurityParam ->
  BlockTree blk ->
  g ->
  m (PointSchedule blk)
uniformPointsWithExtraHonestPeersAndDowntime
    extraHonestPeers
    (SecurityParam k)
    BlockTree {btTrunk, btBranches}
    g
  = do
  let
    kSlot = withOrigin 0 (fromIntegral . unSlotNo) (AF.headSlot (AF.takeOldest (fromIntegral k) btTrunk))
    midSlot = (AF.length btTrunk) `div` 2
    lowerBound = max kSlot midSlot
  pauseSlot <- SlotNo . fromIntegral <$> Random.uniformRM (lowerBound, AF.length btTrunk - 1) g
  honestTip0 <- firstTip pauseSlot btTrunk
  honests <- replicateM (extraHonestPeers + 1) $
    mkSchedule [(IsTrunk, [honestTip0, minusClamp (AF.length btTrunk) 1])] []
  advs <- takeBranches pauseSlot btBranches
  let (honests', advs') = syncTips honests advs
      psSchedule = peers' honests' advs'
  psStartOrder <- shuffle $ getPeerIds psSchedule
  pure $ PointSchedule {psSchedule, psStartOrder, psMinEndTime = Time 0}
  where
    takeBranches pause = \case
        [] -> pure []
        [b] -> pure <$> withoutRollback pause b
        b1 : b2 : branches -> do
          a <- Random.uniformDouble01M g
          if a < rollbackProb
          then do
            this <- withRollback pause b1 b2
            rest <- takeBranches pause branches
            pure (this : rest)
          else do
            this <- withoutRollback pause b1
            rest <- takeBranches pause (b2 : branches)
            pure (this : rest)

    withoutRollback pause branch = do
      tips <- mkTips pause branch
      mkSchedule tips [btbSuffix branch]

    withRollback pause b1 b2 = do
      firstTips <- mkTips pause b1
      let secondTips = [minusClamp (AF.length (btbSuffix b2)) 1]
      mkSchedule (firstTips ++ [(IsBranch, secondTips)]) [btbSuffix b1, btbSuffix b2]

    mkSchedule tips branches = do
      params <- mkParams
      peerScheduleFromTipPoints g params tips btTrunk branches

    mkTips pause branch
      | AF.length full == 0 =
        error "empty branch"
      | otherwise = do
      tip0 <- firstTip pause (btbFull branch)
      let (pre, post) = partition (< firstSuffixBlock) [tip0, fullLen - 1]
      pure ((if null pre then [] else [(IsTrunk, pre)]) ++ [(IsBranch, shift <$> post)])
      where
        shift i = i - firstSuffixBlock
        firstSuffixBlock = fullLen - AF.length (btbSuffix branch)
        fullLen = AF.length full
        full = btbFull branch

    firstTip pause frag = pure (minusClamp (AF.length (AF.dropWhileNewest (\ b -> blockSlot b > pause) frag)) 1)

    mkParams = do
      -- These values appear to be large enough to create pauses of 100 seconds and more.
      tipL <- uniformRMDiffTime (0.5, 1) g
      tipU <- uniformRMDiffTime (1, 2) g
      headerL <- uniformRMDiffTime (0.018, 0.03) g
      headerU <- uniformRMDiffTime (0.021, 0.04) g
      pure defaultPeerScheduleParams {pspTipDelayInterval = (tipL, tipU), pspHeaderDelayInterval = (headerL, headerU)}

    rollbackProb = 0.2

    -- Inefficient implementation, but sufficient for small lists.
    shuffle :: [a] -> m [a]
    shuffle [] = pure []
    shuffle xs = do
      i <- Random.uniformRM (0, length xs - 1) g
      let x = xs !! i
          xs' = take i xs ++ drop (i+1) xs
      (x :) <$> shuffle xs'

newtype ForecastRange = ForecastRange { unForecastRange :: Word64 }
  deriving (Show)

data LoPBucketParams = LoPBucketParams {
  lbpCapacity :: Integer,
  lbpRate     :: Rational
  }

data CSJParams = CSJParams {
    csjpJumpSize :: SlotNo
  }
  deriving Show

-- | Similar to 'ChainSyncTimeout' for BlockFetch. Only the states in which the
-- server has agency are specified. REVIEW: Should it be upstreamed to
-- ouroboros-network-protocols?
data BlockFetchTimeout = BlockFetchTimeout
  { busyTimeout      :: Maybe DiffTime,
    streamingTimeout :: Maybe DiffTime
  }

-- | All the data used by point schedule tests.
data GenesisTest blk schedule = GenesisTest
  { gtSecurityParam      :: SecurityParam,
    gtGenesisWindow      :: GenesisWindow,
    gtForecastRange      :: ForecastRange, -- REVIEW: Do we want to allow infinite forecast ranges?
    gtDelay              :: Delta,
    gtBlockTree          :: BlockTree blk,
    gtChainSyncTimeouts  :: ChainSyncTimeout,
    gtBlockFetchTimeouts :: BlockFetchTimeout,
    gtLoPBucketParams    :: LoPBucketParams,
    gtCSJParams          :: CSJParams,
    gtSlotLength         :: SlotLength,
    -- | The number of extra honest peers we want in the test.
    -- It is stored here for convenience, and because it may affect schedule and block tree generation.
    --
    -- There will be at most one adversarial peer per alternative branch in the block tree
    -- (exactly one per branch if no adversary does a rollback),
    -- and @1 + gtExtraHonestPeers@ honest peers.
    gtExtraHonestPeers   :: Word,
    gtSchedule           :: schedule
  }

type GenesisTestFull blk = GenesisTest blk (PointSchedule blk)

-- | All the data describing the result of a test
data RunGenesisTestResult = RunGenesisTestResult
  { rgtrTrace     :: String,
    rgtrStateView :: StateView TestBlock
  }

prettyGenesisTest :: (schedule -> [String]) -> GenesisTest TestBlock schedule -> [String]
prettyGenesisTest prettySchedule genesisTest =
  [ "GenesisTest:"
  , "  gtSecurityParam: " ++ show (maxRollbacks gtSecurityParam)
  , "  gtGenesisWindow: " ++ show (unGenesisWindow gtGenesisWindow)
  , "  gtForecastRange: " ++ show (unForecastRange gtForecastRange)
  , "  gtDelay: " ++ show delta
  , "  gtSlotLength: " ++ show gtSlotLength
  , "  gtCSJParams: " ++ show gtCSJParams
  , "  gtChainSyncTimeouts: "
  , "    canAwait = " ++ show canAwaitTimeout
  , "    intersect = " ++ show intersectTimeout
  , "    mustReply = " ++ show mustReplyTimeout
  , "    idle = " ++ show idleTimeout
  , "  gtBlockFetchTimeouts: "
  , "    busy = " ++ show busyTimeout
  , "    streaming = " ++ show streamingTimeout
  , "  gtLoPBucketParams: "
  , "    lbpCapacity = " ++ show lbpCapacity ++ " tokens"
  , "    lbpRate = " ++ show lbpRate ++ " ≅ " ++ printf "%.2f" (fromRational lbpRate :: Float) ++ " tokens per second"
  , "  gtBlockTree:"
  ] ++ map (("    " ++) . terseFragment) (allFragments gtBlockTree)
    ++ map ("    " ++) (prettyBlockTree gtBlockTree)
    ++ ["  gtSchedule:"]
    ++ map ("    " ++) (prettySchedule gtSchedule)
  where
    GenesisTest {
        gtSecurityParam
      , gtGenesisWindow
      , gtForecastRange
      , gtDelay = Delta delta
      , gtBlockTree
      , gtChainSyncTimeouts =
          ChainSyncTimeout{canAwaitTimeout, intersectTimeout, mustReplyTimeout, idleTimeout}
      , gtBlockFetchTimeouts = BlockFetchTimeout{busyTimeout, streamingTimeout}
      , gtLoPBucketParams = LoPBucketParams{lbpCapacity, lbpRate}
      , gtSlotLength
      , gtCSJParams
      , gtSchedule
      } = genesisTest

instance Functor (GenesisTest blk) where
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

ensureScheduleDuration :: GenesisTest blk a -> PointSchedule blk -> PointSchedule blk
ensureScheduleDuration gt PointSchedule{psSchedule, psStartOrder, psMinEndTime} =
    PointSchedule
      { psSchedule
      , psStartOrder
      , psMinEndTime = max psMinEndTime (Time endingDelay)
      }
  where
    endingDelay =
     let cst = gtChainSyncTimeouts gt
         bft = gtBlockFetchTimeouts gt
      in 1 + fromIntegral peerCount * maximum (0 : catMaybes
           [ canAwaitTimeout cst
           , intersectTimeout cst
           , busyTimeout bft
           , streamingTimeout bft
           ])
    peerCount = length (peersList psSchedule)
