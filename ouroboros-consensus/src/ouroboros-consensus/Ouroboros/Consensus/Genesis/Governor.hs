{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Genesis.Governor (
    DensityBounds (..)
  , LatestSlot (..)
  , TraceGDDEvent (..)
  , UpdateLoEFrag (..)
  , densityDisconnect
  , runGddAsync
  , sharedCandidatePrefix
  , updateLoEFragGenesis
  , updateLoEFragStall
  , updateLoEFragUnconditional
  ) where

import           Control.Monad (guard)
import           Control.Tracer (Tracer, traceWith)
import           Data.Containers.ListUtils (nubOrd)
import           Data.Foldable (for_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config (TopLevelConfig, configLedger,
                     configSecurityParam)
import           Ouroboros.Consensus.Config.SecurityParam
                     (SecurityParam (SecurityParam))
import           Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (..))
import           Ouroboros.Consensus.HardFork.History.Qry (qryFromExpr,
                     runQuery, slotToGenesisWindow)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState,
                     ledgerState)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (ChainSyncClientHandle (..))
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB,
                     triggerChainSelectionAsync)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Util (eitherToMaybe, whenJust)
import           Ouroboros.Consensus.Util.AnchoredFragment (stripCommonPrefix)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (blockUntilChanged)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

-- | This callback is a hook into ChainSync that is called right before deciding
-- whether a block can be added to the current selection.
--
-- Its purpose is to update the LoE fragment, anchored at the immutable tip and
-- whose tip is the header of the youngest block present in all candidate
-- fragments.
--
-- The callback is applied to the current chain, the current ledger state and
-- an STM action that returns the new LoE fragment.
data UpdateLoEFrag m blk = UpdateLoEFrag {
    updateLoEFrag ::
         AnchoredFragment (Header blk)
      -> ExtLedgerState blk
      -> m (AnchoredFragment (Header blk))
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks)

-- | A dummy version of the LoE that sets the LoE fragment to the current
-- selection.
updateLoEFragUnconditional ::
  MonadSTM m =>
  UpdateLoEFrag m blk
updateLoEFragUnconditional =
  UpdateLoEFrag $ \ curChain _ -> pure curChain

-- | Compute the fragment @loeFrag@ between the immutable tip and the
-- earliest intersection between @curChain@ and any of the @candidates@.
--
-- The immutable tip is the anchor of @curChain@.
--
-- The function also yields the suffixes of the intersection of @loeFrag@ with
-- every candidate fragment.
sharedCandidatePrefix ::
  GetHeader blk =>
  AnchoredFragment (Header blk) ->
  Map peer (AnchoredFragment (Header blk)) ->
  (AnchoredFragment (Header blk), Map peer (AnchoredFragment (Header blk)))
sharedCandidatePrefix curChain candidates =
  stripCommonPrefix (AF.anchor curChain) immutableTipSuffixes
  where
    immutableTip = AF.anchorPoint curChain

    splitAfterImmutableTip frag =
      snd <$> AF.splitAfterPoint frag immutableTip

    immutableTipSuffixes =
      -- If a ChainSync client's candidate forks off before the
      -- immutable tip, then this transaction is currently winning an
      -- innocuous race versus the thread that will fatally raise
      -- 'InvalidIntersection' within that ChainSync client, so it's
      -- sound to pre-emptively discard their candidate from this
      -- 'Map' via 'mapMaybe'.
      Map.mapMaybe splitAfterImmutableTip candidates

-- | This version of the LoE implements part of the intended Genesis approach.
-- The fragment is set to the prefix of all candidates, ranging from the
-- immutable tip to the earliest intersection of all peers.
--
-- Using this will cause ChainSel to stall indefinitely, or until a peer
-- disconnects for unrelated reasons.
-- In the future, the Genesis Density Disconnect Governor variant will extend
-- this with an analysis that will always result in disconnections from peers
-- to ensure the selection can advance.
updateLoEFragStall ::
  MonadSTM m =>
  GetHeader blk =>
  STM m (Map peer (AnchoredFragment (Header blk))) ->
  UpdateLoEFrag m blk
updateLoEFragStall getCandidates =
  UpdateLoEFrag $ \ curChain _ ->
    atomically $ do
      candidates <- getCandidates
      pure (fst (sharedCandidatePrefix curChain candidates))

-- | Run the GDD governor in a background thread, writing the LoE fragment
-- computed by @loEUpdater@ to @varLoEFrag@ whenever the STM action
-- @getTrigger@ changes.
--
-- After writing the fragment, send a message to ChainSel to reprocess all
-- blocks that had previously been postponed by the LoE.
runGddAsync ::
  (Monoid a, Eq a, IOLike m, LedgerSupportsProtocol blk) =>
  UpdateLoEFrag m blk ->
  StrictTVar m (AnchoredFragment (Header blk)) ->
  ChainDB m blk ->
  STM m a ->
  m Void
runGddAsync loEUpdater varLoEFrag chainDb getTrigger =
  spin mempty
  where
    spin oldTrigger = do
      (newTrigger, curChain, curLedger) <- atomically $ do
        (_, newTrigger) <- blockUntilChanged id oldTrigger getTrigger
        curChain <- ChainDB.getCurrentChain chainDb
        curLedger <- ChainDB.getCurrentLedger chainDb
        pure (newTrigger, curChain, curLedger)
      loeFrag <- updateLoEFrag loEUpdater curChain curLedger
      atomically $ writeTVar varLoEFrag loeFrag
      triggerChainSelectionAsync chainDb
      spin newTrigger

-- | Classify the latest known header in relation to the forecast horizon,
-- and provide its slot number.
--
data LatestSlot =
  -- | The candidate fragment is empty and the TVar containing the latest
  -- slot ChainSync has seen does not contain an entry for the current peer.
  NoLatestSlot
  |
  -- | The candidate fragment head is the latest known header.
  LatestSlotCandidate SlotNo
  |
  -- | ChainSync has seen a header after the candidate fragment head, most
  -- likely because it is beyond the forecast horizon.
  LatestSlotForecast SlotNo
  deriving (Show)

data DensityBounds blk =
  DensityBounds {
    fragment        :: AnchoredFragment (Header blk),
    offersMoreThanK :: Bool,
    lowerBound      :: Word64,
    upperBound      :: Word64,
    hasBlockAfter   :: Bool,
    latestSlot      :: LatestSlot
  }

-- | @densityDisconnect cfg immutableLedgerSt candidateSuffixes theirTips loeFrag@
-- yields the list of peers which are known to lose the density comparison with
-- any other peer, when looking at the genesis window after @loeFrag@.
--
-- The peers are taken from the keys of @candidateSuffixes@.
--
-- @candidateSuffixes@ tells for every peer what is the fragment that the peer
-- proposes to use after @loeFrag@.
--
-- @theirTips@ tells for every peer what is the last header that the peer is
-- claiming to have.
--
-- @latestSlots@ tells for every peer which is the slot of the last header that
-- it sent.
--
-- @loeFrag@ is the fragment from the immutable tip to the first intersection
-- with a candidate fragment.
--
densityDisconnect ::
     ( Ord peer
     , LedgerSupportsProtocol blk
     )
  => GenesisWindow
  -> SecurityParam
  -> Map peer (AnchoredFragment (Header blk))
  -> Set peer
  -> Map peer SlotNo
  -> AnchoredFragment (Header blk)
  -> ([peer], Map peer (DensityBounds blk))
densityDisconnect (GenesisWindow sgen) (SecurityParam k) candidateSuffixes caughtUpPeers latestSlots loeFrag =
  (losingPeers, densityBounds)
  where
    densityBounds = Map.fromList $ do
      (peer, fragment) <- Map.toList competingFrags
      -- Skip peers that haven't advertised their tip yet.
      -- They should be disconnected by timeouts instead.
      -- TODO We probably need an alternative approach for this now
      -- that we don't use the tip anymore
      let candidateSuffix = candidateSuffixes Map.! peer

          caughtUp = Set.member peer caughtUpPeers

          latestSlot = case (AF.headSlot candidateSuffix, latestSlots Map.!? peer) of
            (Origin, Nothing) -> NoLatestSlot
            (Origin, Just latest)
              | latest > 0 -> LatestSlotForecast latest
              | otherwise -> NoLatestSlot
            (NotOrigin cand, Nothing) -> LatestSlotCandidate cand
            (NotOrigin cand, Just latest)
              | latest > cand -> LatestSlotForecast latest
              | otherwise -> LatestSlotCandidate cand

          -- If the peer has more headers that it hasn't sent yet, each slot between the latest header we know of and
          -- the end of the Genesis window could contain a block, so the upper bound for the total number of blocks in
          -- the window is the sum of the known blocks and that number of remaining slots.
          -- If the slot of the latest header we know of is _after_ the end of the Genesis window (either because the
          -- candidate fragment extends beyond it or because we are waiting to validate a header beyond the forecast
          -- horizon that we already received), there can be no headers in between and 'potentialSlots' is 0.
          SlotNo potentialSlots
            | caughtUp
            = 0
            | otherwise
            = case latestSlot of
              -- The peer either has not advertised its tip yet or simply has no blocks whatsoever and won't progress
              -- either.
              -- In the latter case, it should be killed by the LoP.
              -- REVIEW: What do we want to do in tests here? Both cases are possible, and we cannot distinguish them
              -- without the tip (even with the tip we have been relying on the invariant that each peer advertises the
              -- tip before any of them sends headers).
              -- NoLatestSlot -> 0
              NoLatestSlot -> SlotNo sgen
              -- 'endOfGenesisWindow' is exclusive, so we have to add 1 to the non-exclusive last slot
              LatestSlotCandidate slot -> intervalLength (slot + 1) endOfGenesisWindow
              -- If the candidate fragment's last slot is smaller than the slot set after receiving a header, we are
              -- stuck at the forecast horizon, which implies that there is a header after the Genesis window.
              LatestSlotForecast _ -> 0

          -- The number of blocks within the Genesis window we know with certainty
          lowerBound = fromIntegral $ AF.length fragment

          upperBound = lowerBound + potentialSlots

          -- The number of blocks we know of on the candidate chain, not limited to the Genesis window.
          totalBlockCount = fromIntegral (AF.length candidateSuffix)

          -- Does the peer have more than k known blocks in _total_ after the intersection?
          -- If not, it is not qualified to compete by density (yet).
          offersMoreThanK = case latestSlot of
            NoLatestSlot          -> False
            LatestSlotCandidate _ -> totalBlockCount > k
            -- If the last slot is not on the candidate chain, we know that there is at least one more block, so we use
            -- '>=' here.
            LatestSlotForecast _  -> totalBlockCount >= k

          -- For tracing: Is there a block after the end of the Genesis window?
          -- Note that 'endOfGenesisWindow' is exclusive, so we use '>='.
          hasBlockAfter = case latestSlot of
            NoLatestSlot             -> False
            LatestSlotCandidate slot -> slot >= endOfGenesisWindow
            LatestSlotForecast slot  -> slot >= endOfGenesisWindow

      pure (peer, DensityBounds {fragment, offersMoreThanK, lowerBound, upperBound, hasBlockAfter, latestSlot})

    losingPeers = nubOrd $ do
      (peer0 , DensityBounds {fragment = frag0, upperBound = ub0}) <- Map.toList densityBounds
      (_peer1, DensityBounds {fragment = frag1, offersMoreThanK, lowerBound = lb1 }) <- Map.toList densityBounds
      -- ensure that the two peer fragments don't share any
      -- headers after the LoE
      guard $ AF.lastPoint frag0 /= AF.lastPoint frag1
      -- peer1 offers more than k blocks
      guard offersMoreThanK
      -- peer1 definitely has higher density than peer0
      guard $ lb1 > ub0
      pure peer0

    loeIntersectionSlot = AF.headSlot loeFrag
    -- exclusive last slot in the Genesis window
    endOfGenesisWindow =
        succWithOrigin loeIntersectionSlot + SlotNo sgen

    dropBeyondGenesisWindow =
      AF.takeWhileOldest ((< endOfGenesisWindow) . blockSlot)

    competingFrags =
      Map.map dropBeyondGenesisWindow candidateSuffixes

    -- Length of an interval with inclusive lower bound @a@ and exclusive upper
    -- bound @b@.
    intervalLength a b
      | a <= b    = b - a
      | otherwise = 0

{- TODO: convert this scribble into a useful explanatory diagram, illustrating the
        density calculation below

            |--------|

    frag1: A - B - C - D - ...            <- exact
            \
    frag2:     E           claimed tip: E <- exact

-}

data TraceGDDEvent peer blk =
  TraceGDDEvent {
    bounds            :: Map peer (DensityBounds blk),
    curChain          :: AnchoredFragment (Header blk),
    candidates        :: Map peer (AnchoredFragment (Header blk)),
    candidateSuffixes :: Map peer (AnchoredFragment (Header blk)),
    losingPeers       :: [peer],
    loeHead           :: AF.Anchor (Header blk),
    sgen              :: GenesisWindow
  }

-- | Update the LoE fragment.
--
-- See 'UpdateLoEFrag' for the definition of LoE fragment.
--
-- Additionally, disconnect the peers that lose density comparisons.
--
-- @getCandidates@ is the callback to obtain the candidate fragments
--
-- @getHandles@ is the callback to get the handles that allow to disconnect
-- from peers.
updateLoEFragGenesis ::
     forall m blk peer.
     ( IOLike m
     , Ord peer
     , LedgerSupportsProtocol blk
     , HasHardForkHistory blk
     )
  => TopLevelConfig blk
  -> Tracer m (TraceGDDEvent peer blk)
  -> STM m (Map peer (AnchoredFragment (Header blk)))
  -> STM m (Map peer (ChainSyncClientHandle m blk))
  -> STM m (Set peer)
  -> UpdateLoEFrag m blk
updateLoEFragGenesis cfg tracer getCandidates getHandles getCaughtUpPeers =
  UpdateLoEFrag $ \ curChain immutableLedgerSt -> do
    (candidates, candidateSuffixes, handles, caughtUpPeers, latestSlots, loeFrag) <- atomically $ do
      candidates <- getCandidates
      handles <- getHandles
      caughtUpPeers <- getCaughtUpPeers
      let
        (loeFrag, candidateSuffixes) =
          sharedCandidatePrefix curChain candidates
      latestSlots <-
        flip Map.traverseWithKey candidateSuffixes $ \peer _ ->
          cschLatestSlot (handles Map.! peer)
      pure (candidates, candidateSuffixes, handles, caughtUpPeers, latestSlots, loeFrag)

    let msgen :: Maybe GenesisWindow
        -- This could also use 'runWithCachedSummary' if deemed desirable.
        msgen = eitherToMaybe $ runQuery qry summary
          where
            -- We use the Genesis window for the first slot /after/ the common
            -- intersection. In particular, when the intersection is the last
            -- slot of an era, we will use the Genesis window of the next era,
            -- as all slots in the Genesis window reside in that next era.
            slot    = succWithOrigin $ AF.headSlot loeFrag
            qry     = qryFromExpr $ slotToGenesisWindow slot
            summary =
              hardForkSummary
                (configLedger cfg)
                -- Due to the cross-chain lemma (Property 17.3 in the Consensus
                -- report) one could also use the ledger state at the tip of our
                -- selection here (in which case this should never return
                -- 'Nothing'), but this is subtle and maybe not desirable.
                --
                -- In any case, the immutable ledger state will also
                -- /eventually/ catch up to the LoE tip, so @msgen@ won't be
                -- 'Nothing' forever.
                (ledgerState immutableLedgerSt)

    whenJust msgen $ \sgen -> do
      let
        (losingPeers, bounds) =
          densityDisconnect sgen (configSecurityParam cfg) candidateSuffixes caughtUpPeers latestSlots loeFrag
        loeHead = AF.headAnchor loeFrag

      traceWith tracer TraceGDDEvent {sgen, curChain, bounds, candidates, candidateSuffixes, losingPeers, loeHead}

      for_ losingPeers $ \peer -> cschKill (handles Map.! peer)

    pure loeFrag
