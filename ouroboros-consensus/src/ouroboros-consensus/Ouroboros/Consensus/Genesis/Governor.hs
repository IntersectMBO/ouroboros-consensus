{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Genesis.Governor (
    DensityBounds (..)
  , TraceGDDEvent (..)
  , densityDisconnect
  , reprocessLoEBlocksOnCandidateChange
  , sharedCandidatePrefix
  , updateLoEFragGenesis
  , updateLoEFragStall
  , updateLoEFragUnconditional
  ) where

import           Control.Monad (guard)
import           Control.Tracer (Tracer, traceWith)
import           Data.Containers.ListUtils (nubOrd)
import           Data.Foldable (for_, toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.Word (Word64)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config (TopLevelConfig, configLedger,
                     configSecurityParam)
import           Ouroboros.Consensus.Config.SecurityParam
                     (SecurityParam (SecurityParam))
import           Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (..))
import           Ouroboros.Consensus.HardFork.History.Qry (qryFromExpr,
                     runQuery, slotToGenesisWindow)
import           Ouroboros.Consensus.Ledger.Extended (ledgerState)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (ChainSyncClientHandle (..))
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB,
                     UpdateLoEFrag (UpdateLoEFrag), reprocessLoEAsync)
import           Ouroboros.Consensus.Util (eitherToMaybe, whenJust)
import           Ouroboros.Consensus.Util.AnchoredFragment (stripCommonPrefix)
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import           Ouroboros.Consensus.Util.MonadSTM.NormalForm
                     (MonadSTM (STM, atomically))
import           Ouroboros.Consensus.Util.STM (blockUntilChanged)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Tip, getTipSlotNo)

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

-- | Background task that wakes up whenever a candidate fragment changes and
-- triggers ChainSel for any block that has been postponed because of the LoE.
reprocessLoEBlocksOnCandidateChange ::
  Ord peer =>
  MonadSTM m =>
  GetHeader blk =>
  ChainDB m blk ->
  STM m (Map peer (AnchoredFragment (Header blk))) ->
  m ()
reprocessLoEBlocksOnCandidateChange chainDb getCandidates =
  spin mempty
  where
    spin prev =
      atomically (blockUntilChanged (Map.map AF.headPoint) prev getCandidates) >>= \ (_, newTips) -> do
        reprocessLoEAsync chainDb
        spin newTips

data DensityBounds blk =
  DensityBounds {
    fragment        :: AnchoredFragment (Header blk),
    offersMoreThanK :: Bool,
    lowerBound      :: Word64,
    upperBound      :: Word64,
    hasBlockAfter   :: Bool,
    lastSlot        :: Either SlotNo SlotNo
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
  -> Map peer (Tip blk)
  -> Map peer SlotNo
  -> AnchoredFragment (Header blk)
  -> ([peer], Map peer (DensityBounds blk))
densityDisconnect (GenesisWindow sgen) (SecurityParam k) candidateSuffixes theirTips latestSlots loeFrag =
  (losingPeers, densityBounds)
  where
    densityBounds = Map.fromList $ do
      (peer, fragment) <- Map.toList competingFrags
      -- Skip peers that haven't advertised their tip yet.
      -- They should be disconnected by timeouts instead.
      theirTip <- toList (theirTips Map.!? peer)
      let candidateSuffix = candidateSuffixes Map.! peer
          lowerBound = fromIntegral $ AF.length fragment
          unresolvedSlotsLB =
            succWithOrigin $ AF.headSlot fragment
          unresolvedSlotsUB =
              endOfGenesisWindow `min` claimedTip
            where
              claimedTip = succWithOrigin $ getTipSlotNo theirTip
          lastSlotCandidate = succWithOrigin (AF.headSlot candidateSuffix)
          maybeForecast = latestSlots Map.!? peer
          lastSlotForecast = maybe 0 succ maybeForecast
          candidateHasBlockAfter =
            -- Note that if
            -- > NotOrigin s = AF.headSlot candidateSuffix
            -- this check is equivalent to
            -- > s >= endOfGenesisWindow
            lastSlotCandidate > endOfGenesisWindow
          forecastHasBlockAfter =
            lastSlotForecast > endOfGenesisWindow
          hasBlockAfterGenesisWindow =
            candidateHasBlockAfter || forecastHasBlockAfter
          upperBound =
              lowerBound
            + if hasBlockAfterGenesisWindow
              then 0
              else unSlotNo (intervalLength unresolvedSlotsLB unresolvedSlotsUB)
          offersMoreThanK
            | candidateHasBlockAfter
            = AF.length candidateSuffix > fromIntegral k
            | otherwise
            = AF.length candidateSuffix >= fromIntegral k
          lastSlot | isJust maybeForecast, lastSlotForecast >= lastSlotCandidate = Left lastSlotForecast
                   | otherwise = Right lastSlotCandidate
      pure (peer, DensityBounds {fragment, offersMoreThanK, lowerBound, upperBound, hasBlockAfter = hasBlockAfterGenesisWindow, lastSlot})

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
  -> UpdateLoEFrag m blk
updateLoEFragGenesis cfg tracer getCandidates getHandles =
  UpdateLoEFrag $ \ curChain immutableLedgerSt -> do
    (candidateSuffixes, handles, theirTips, latestSlots, loeFrag) <- atomically $ do
      candidates <- getCandidates
      handles <- getHandles
      let
        (loeFrag, candidateSuffixes) =
          sharedCandidatePrefix curChain candidates
      theirTips <-
        fmap (Map.mapMaybe id) $ flip Map.traverseWithKey candidateSuffixes $ \peer _ ->
          cschTheirTip (handles Map.! peer)
      latestSlots <-
        flip Map.traverseWithKey candidateSuffixes $ \peer _ ->
          cschLatestSlot (handles Map.! peer)
      pure (candidateSuffixes, handles, theirTips, latestSlots, loeFrag)

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
          densityDisconnect sgen (configSecurityParam cfg) candidateSuffixes theirTips latestSlots loeFrag
        loeHead = AF.headAnchor loeFrag

      traceWith tracer TraceGDDEvent {sgen, bounds, candidateSuffixes, losingPeers, loeHead}

      for_ losingPeers $ \peer -> cschKill (handles Map.! peer)

    pure loeFrag
