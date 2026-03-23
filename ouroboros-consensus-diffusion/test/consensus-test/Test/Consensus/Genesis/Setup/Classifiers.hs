{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Consensus.Genesis.Setup.Classifiers
  ( Classifiers (..)
  , ResultClassifiers (..)
  , ScheduleClassifiers (..)
  , classifiers
  , resultClassifiers
  , scheduleClassifiers
  , simpleHash
  ) where

import Cardano.Ledger.BaseTypes (unNonZero)
import Cardano.Slotting.Slot (WithOrigin (..))
import Data.List (sortOn, tails)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Word (Word64)
import Ouroboros.Consensus.Block
  ( ChainHash (..)
  , HeaderHash
  , blockSlot
  , succWithOrigin
  )
import Ouroboros.Consensus.Block.Abstract
  ( SlotNo (SlotNo)
  , withOrigin
  )
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( ChainSyncClientException (DensityTooLow, EmptyBucket)
  )
import Ouroboros.Consensus.Util.IOLike (SomeException, fromException)
import Ouroboros.Network.AnchoredFragment
  ( anchor
  , anchorToSlotNo
  , headSlot
  )
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Driver.Limits
  ( ProtocolLimitFailure (ExceededTimeLimit)
  )
import Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..))
import Test.Consensus.Network.AnchoredFragment.Extras (slotLength)
import Test.Consensus.PeerSimulator.StateView
  ( PeerSimulatorResult (..)
  , StateView (..)
  , pscrToException
  )
import Test.Consensus.PointSchedule
import Test.Consensus.PointSchedule.Peers (PeerId (..), Peers (..))
import Test.Consensus.PointSchedule.SinglePeer (SchedulePoint (..))
import Test.Util.Orphans.IOLike ()
import Test.Util.TestBlock
  ( TestBlock
  , TestHash (TestHash)
  , isAncestorOf
  )

-- | Interesting categories to classify test inputs
data Classifiers
  = Classifiers
  { existsSelectableAdversary :: Bool
  -- ^ There are more than k blocks in at least one alternative chain after the intersection
  , allAdversariesSelectable :: Bool
  -- ^ There are more than k blocks in all alternative chains after the
  -- intersection. Note that this is always guaranteed for the honest chain.
  , allAdversariesForecastable :: Bool
  -- ^ There is always at least one block per sliding forecast window in all
  -- alternative chains. Note that this is always guaranteed for the honest
  -- chain.
  , allAdversariesKPlus1InForecast :: Bool
  -- ^ All adversaries have at least @k+1@ block in the forecast window the
  -- follows their intersection with the trunk. Note that the generator always
  -- enforces that the trunk wins in all _Genesis_ windows after the
  -- intersection. In particular, if @sgen = sfor@, then the trunk will have
  -- at least @k+2@.
  , genesisWindowAfterIntersection :: Bool
  -- ^ There are at least scg slots after the intersection on both the honest
  -- and the alternative chain
  --
  -- Knowing if there is a Genesis window after the intersection is important because
  -- otherwise the Genesis node has no chance to advance the immutable tip past
  -- the Limit on Eagerness.
  , longerThanGenesisWindow :: Bool
  -- ^ The honest chain's slot count is greater than or equal to the Genesis window size.
  }

classifiers :: AF.HasHeader blk => GenesisTest blk schedule -> Classifiers
classifiers GenesisTest{gtBlockTree, gtSecurityParam = SecurityParam k, gtGenesisWindow = GenesisWindow scg} =
  Classifiers
    { existsSelectableAdversary
    , allAdversariesSelectable
    , allAdversariesForecastable
    , allAdversariesKPlus1InForecast
    , genesisWindowAfterIntersection
    , longerThanGenesisWindow
    }
 where
  longerThanGenesisWindow = AF.headSlot goodChain >= At (fromIntegral scg)

  genesisWindowAfterIntersection =
    any fragmentHasGenesis branches

  fragmentHasGenesis btb =
    let
      frag = btbSuffix btb
      SlotNo intersection = withOrigin 0 id (anchorToSlotNo (anchor frag))
     in
      isSelectable btb && slotLength frag > fromIntegral scg && goodTipSlot - intersection > scg

  existsSelectableAdversary =
    any isSelectable branches

  allAdversariesSelectable =
    all isSelectable branches

  isSelectable bt = AF.length (btbSuffix bt) > fromIntegral (unNonZero k)

  allAdversariesForecastable =
    all isForecastable branches

  isForecastable bt =
    -- FIXME: We are using `scg` here but what we really mean is `sfor`.
    -- Distinguish `scg` vs. `sgen` vs. `sfor` and use the latter here.
    -- NOTE: We only care about the difference between slot numbers so it is
    -- not a problem to add @1@ to all of them. However, we do care VERY MUCH
    -- that this list includes the anchor.
    let slotNos =
          (succWithOrigin $ anchorToSlotNo $ anchor $ btbFull bt)
            : (map ((+ 1) . blockSlot) $ AF.toOldestFirst $ btbFull bt)
     in all (\(SlotNo prev, SlotNo next) -> next - prev <= scg) (zip slotNos (drop 1 slotNos))

  allAdversariesKPlus1InForecast =
    all hasKPlus1InForecast branches

  hasKPlus1InForecast BlockTreeBranch{btbSuffix} =
    -- FIXME: We are using `scg` here but what we really mean is `sfor`.
    -- Distinguish `scg` vs. `sgen` vs. `sfor` and use the latter here.
    let forecastSlot = succWithOrigin (anchorToSlotNo $ anchor btbSuffix) + SlotNo scg
        forecastBlocks = AF.takeWhileOldest (\b -> blockSlot b < forecastSlot) btbSuffix
     in AF.length forecastBlocks >= fromIntegral (unNonZero k) + 1

  SlotNo goodTipSlot = withOrigin 0 id (headSlot goodChain)

  branches = btBranches gtBlockTree

  goodChain = btTrunk gtBlockTree

-- | Interesting categories to classify test results
data ResultClassifiers
  = ResultClassifiers
  { adversariesKilledByLoP :: Double
  -- ^ Percentage of adversaries that were killed by receiving an EmptyBucket exception from the LoP
  , adversariesKilledByGDD :: Double
  -- ^ Percentage of adversaries that were disconnected because their fragment was not dense enough
  , adversariesKilledByTimeout :: Double
  -- ^ Percentage of adversaries that were disconnected by network-level timeouts
  , adversariesSurvived :: Double
  -- ^ Percentage of adversaries that weren't killed
  }

-- | Returned when there were no adversaries
nullResultClassifier :: ResultClassifiers
nullResultClassifier = ResultClassifiers 0 0 0 0

resultClassifiers :: GenesisTestFull blk -> RunGenesisTestResult -> ResultClassifiers
resultClassifiers GenesisTest{gtSchedule} RunGenesisTestResult{rgtrStateView} =
  if adversariesCount > 0
    then
      ResultClassifiers
        { adversariesKilledByLoP = 100 * adversariesKilledByLoPC / adversariesCount
        , adversariesKilledByGDD = 100 * adversariesKilledByGDDC / adversariesCount
        , adversariesKilledByTimeout = 100 * adversariesKilledByTimeoutC / adversariesCount
        , adversariesSurvived = 100 * adversariesSurvivedC / adversariesCount
        }
    else nullResultClassifier
 where
  StateView{svPeerSimulatorResults} = rgtrStateView

  adversaries :: [PeerId]
  adversaries = fmap AdversarialPeer $ Map.keys $ adversarialPeers $ psSchedule gtSchedule

  adversariesCount = fromIntegral $ length adversaries

  adversariesExceptions :: [(PeerId, SomeException)]
  adversariesExceptions =
    mapMaybe
      ( \PeerSimulatorResult{psePeerId, pseResult} -> case psePeerId of
          HonestPeer _ -> Nothing
          pid -> (pid,) <$> pscrToException pseResult
      )
      svPeerSimulatorResults

  adversariesSurvivedC =
    fromIntegral $
      length $
        filter
          (\pid -> not $ pid `elem` map fst adversariesExceptions)
          adversaries

  adversariesKilledByLoPC =
    fromIntegral $
      length $
        filter
          (\(_, exn) -> fromException exn == Just EmptyBucket)
          adversariesExceptions

  adversariesKilledByGDDC =
    fromIntegral $
      length $
        filter
          (\(_, exn) -> fromException exn == Just DensityTooLow)
          adversariesExceptions

  adversariesKilledByTimeoutC =
    fromIntegral $
      length $
        filter
          ( \(_, exn) -> case fromException exn of
              Just (ExceededTimeLimit _) -> True
              _ -> False
          )
          adversariesExceptions

data ScheduleClassifiers
  = ScheduleClassifiers
  { adversaryRollback :: Bool
  -- ^ There is an adversary that did a rollback
  , honestRollback :: Bool
  -- ^ The honest peer did a rollback
  , allAdversariesEmpty :: Bool
  -- ^ All adversaries have an empty schedule: the only way to disconnect them are
  -- network timeouts.
  , allAdversariesTrivial :: Bool
  -- ^ All adversaries have trivial schedules: they only have an initial state, and
  -- do nothing afterwards.
  }

scheduleClassifiers :: GenesisTestFull TestBlock -> ScheduleClassifiers
scheduleClassifiers GenesisTest{gtSchedule = schedule} =
  ScheduleClassifiers
    { adversaryRollback
    , honestRollback
    , allAdversariesEmpty
    , allAdversariesTrivial
    }
 where
  hasRollback :: PeerSchedule TestBlock -> Bool
  hasRollback peerSch' =
    any (not . isSorted) [tips, headers, blocks]
   where
    peerSch = sortOn fst peerSch'
    isSorted l = and [x `ancestor` y | (x : y : _) <- tails l]
    ancestor Origin Origin = True
    ancestor Origin (At _) = True
    ancestor (At _) Origin = False
    ancestor (At p1) (At p2) = p1 `isAncestorOf` p2
    tips =
      mapMaybe
        ( \(_, point) -> case point of
            ScheduleTipPoint blk -> Just blk
            _ -> Nothing
        )
        peerSch
    headers =
      mapMaybe
        ( \(_, point) -> case point of
            ScheduleHeaderPoint blk -> Just blk
            _ -> Nothing
        )
        peerSch
    blocks =
      mapMaybe
        ( \(_, point) -> case point of
            ScheduleBlockPoint blk -> Just blk
            _ -> Nothing
        )
        peerSch

  rollbacks :: Peers Bool
  rollbacks = hasRollback <$> psSchedule schedule

  adversaryRollback = any id $ adversarialPeers rollbacks
  honestRollback = any id $ honestPeers rollbacks

  allAdversariesEmpty = all id $ adversarialPeers $ null <$> psSchedule schedule

  isTrivial :: PeerSchedule TestBlock -> Bool
  isTrivial = \case
    [] -> True
    (t0, _) : points -> all ((== t0) . fst) points

  allAdversariesTrivial = all id $ adversarialPeers $ isTrivial <$> psSchedule schedule

simpleHash ::
  HeaderHash block ~ TestHash =>
  ChainHash block ->
  [Word64]
simpleHash = \case
  BlockHash (TestHash h) -> reverse (NonEmpty.toList h)
  GenesisHash -> []
