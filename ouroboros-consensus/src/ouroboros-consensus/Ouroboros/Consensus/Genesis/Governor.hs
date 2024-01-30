{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Genesis.Governor (
    densityDisconnect
  , sharedCandidatePrefix
  , updateLoEFragGenesis
  , updateLoEFragStall
  , updateLoEFragUnconditional
  ) where

import           Control.Monad (guard)
import           Control.Monad.Except ()
import           Control.Tracer (Tracer, traceWith)
import           Data.Containers.ListUtils (nubOrd)
import           Data.Foldable (for_, toList)
import           Data.List (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)
import           Ouroboros.Consensus.Block (SlotNo (SlotNo, unSlotNo),
                     blockSlot, succWithOrigin)
import           Ouroboros.Consensus.Block.Abstract (GetHeader, Header, Point)
import           Ouroboros.Consensus.Config (TopLevelConfig, configLedger,
                     configSecurityParam)
import           Ouroboros.Consensus.Config.SecurityParam
                     (SecurityParam (SecurityParam))
import           Ouroboros.Consensus.Ledger.Extended (ledgerState)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (GenesisWindow (..), LedgerSupportsProtocol,
                     computeGenesisWindow)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (ChainSyncClientHandle (..))
import           Ouroboros.Consensus.Storage.ChainDB.API
                     (UpdateLoEFrag (UpdateLoEFrag))
import           Ouroboros.Consensus.Util.AnchoredFragment (stripCommonPrefix)
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import           Ouroboros.Consensus.Util.MonadSTM.NormalForm
                     (MonadSTM (STM, atomically))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (Tip, getTipSlotNo)

-- | A dummy version of the LoE that sets the LoE fragment to the current
-- selection.
updateLoEFragUnconditional ::
  MonadSTM m =>
  UpdateLoEFrag m blk
updateLoEFragUnconditional =
  UpdateLoEFrag $ \ curChain _ setLoEFrag -> atomically (setLoEFrag curChain)

-- | Compute the fragment between the immutable tip, as given by the anchor
-- of @curChain@, and the earliest intersection of the @candidates@.
-- This excludes the selection from the set of intersected fragments since we
-- need to be able to select k+1 blocks on a new chain when a fork's peer is
-- killed on which we had selected k blocks, where the selection would
-- otherwise keep the LoE fragment at the killed peer's intersection.
sharedCandidatePrefix ::
  GetHeader blk =>
  SecurityParam ->
  AnchoredFragment (Header blk) ->
  Map peer (AnchoredFragment (Header blk)) ->
  (AnchoredFragment (Header blk), Map peer (AnchoredFragment (Header blk)))
sharedCandidatePrefix (SecurityParam k) curChain candidates =
  (trunc, suffixes)
  where
    trunc | excess > 0 = snd (AF.splitAt excess shared)
          | otherwise = shared

    excess = AF.length shared - fromIntegral k

    (shared, suffixes) = stripCommonPrefix (AF.anchor curChain) immutableTipSuffixes

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
  SecurityParam ->
  STM m (Map peer (AnchoredFragment (Header blk))) ->
  UpdateLoEFrag m blk
updateLoEFragStall k getCandidates =
  UpdateLoEFrag $ \ curChain _ setLoEFrag ->
    atomically $ do
      candidates <- getCandidates
      setLoEFrag (fst (sharedCandidatePrefix k curChain candidates))

showPeers ::
     Show peer
  => Map peer String
  -> String
showPeers = intercalate ", " . fmap (\ (peer, v) -> show peer ++ " -> " ++ v) . Map.toList

densityDisconnect ::
     ( Ord peer
     , Show peer
     , LedgerSupportsProtocol blk
     )
  => GenesisWindow
  -> SecurityParam
  -> Map peer (AnchoredFragment (Header blk))
  -> Map peer (Tip blk)
  -> AnchoredFragment (Header blk)
  -> ([peer], String)
densityDisconnect (GenesisWindow sgen) (SecurityParam k) candidateSuffixes theirTips loeFrag =
  (losingPeers, showPeers (showBounds <$> densityBounds))
  where
    densityBounds = Map.fromList $ do
      (peer, frag) <- Map.toList competingFrags
      theirTip <- toList (theirTips Map.!? peer)
      let candidateSuffix = candidateSuffixes Map.! peer
          lowerBound = fromIntegral $ AF.length frag
          unresolvedSlotsLB =
            succWithOrigin $ AF.headSlot frag
          unresolvedSlotsUB =
              endOfGenesisWindow `min` claimedTip
            where
              claimedTip = succWithOrigin $ getTipSlotNo theirTip
          hasBlockAfterGenesisWindow =
            -- Note that if
            -- > NotOrigin s = AF.headSlot candidateSuffix
            -- this check is equivalent to
            -- > s >= endOfGenesisWindow
              succWithOrigin (AF.headSlot candidateSuffix)
            > endOfGenesisWindow
          upperBound =
              lowerBound
            + if hasBlockAfterGenesisWindow
              then 0
              else unSlotNo (intervalLength unresolvedSlotsLB unresolvedSlotsUB)
          offersMoreThanK = AF.length candidateSuffix > fromIntegral k
      pure (peer, (frag, offersMoreThanK, lowerBound, upperBound))

    losingPeers = nubOrd $ do
      (peer0 , (frag0, _        , _  , ub0)) <- Map.toList densityBounds
      (_peer1, (frag1, moreThanK, lb1, _  )) <- Map.toList densityBounds
      -- ensure that the two peer fragments don't share any
      -- headers after the LoE
      guard $ AF.lastPoint frag0 /= AF.lastPoint frag1
      -- peer1 offers more than k blocks
      guard moreThanK
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

    showBounds :: (AnchoredFragment (Header blk), Bool, Word64, Word64) -> String
    showBounds (_, more, lower, upper) = show lower ++ "/" ++ show upper ++ "[" ++ (if more then "+" else " ") ++ "]"

{- TODO: convert this scribble into a useful explanatory diagram, illustrating the
        density calculation below

            |--------|

    frag1: A - B - C - D - ...            <- exact
            \
    frag2:     E           claimed tip: E <- exact

-}

-- | Run the Genesis disconnection logic once.
--
--   * Maintain and update the LoE.
--
--   * Disconnect from peers with inferior density.
updateLoEFragGenesis ::
     forall m blk peer.
     ( IOLike m
     , Ord peer
     , Show peer
     , LedgerSupportsProtocol blk
     )
  => TopLevelConfig blk
  -> Tracer m String
  -> STM m (Map peer (AnchoredFragment (Header blk)))
  -> STM m (Map peer (ChainSyncClientHandle m blk))
  -> UpdateLoEFrag m blk
updateLoEFragGenesis cfg tracer getCandidates getHandles =
  UpdateLoEFrag $ \ curChain immutableLedgerSt setLoEFrag -> do
    (candidateSuffixes, handles, theirTips, loeFrag) <- atomically $ do
      candidates <- getCandidates
      handles <- getHandles
      let
        (loeFrag, candidateSuffixes) =
          sharedCandidatePrefix (configSecurityParam cfg) curChain candidates
      setLoEFrag loeFrag
      -- Obtaining these before calling setLoEFrag appeared to result in a wrong state,
      -- but maybe that was a fluke.
      theirTips <-
        fmap (Map.mapMaybe id) $ flip Map.traverseWithKey candidateSuffixes $ \peer _ ->
          cschTheirTip (handles Map.! peer)
      pure (candidateSuffixes, handles, theirTips, loeFrag)

    let
      -- TODO: use a forecasted ledger view for the intersection
      -- slot (tip of LoE frag).
      sgen = computeGenesisWindow (configLedger cfg) (ledgerState immutableLedgerSt)
      (losingPeers, boundsDesc) =
        densityDisconnect sgen (configSecurityParam cfg) candidateSuffixes theirTips loeFrag

    trace ("Density bounds: " ++ boundsDesc)
    trace ("New candidate tips: " ++ showPeers (showTip <$> Map.map AF.headPoint candidateSuffixes))
    trace ("Losing peers: " ++ show losingPeers)
    trace ("Setting loeFrag: " ++ show (AF.headAnchor loeFrag))

    for_ losingPeers $ \peer -> do
      trace ("Killing peer: " ++ show peer)
      cschKill (handles Map.! peer)
  where
    trace msg = traceWith tracer ("GDG | " ++ msg)

    showTip :: Point (Header blk) -> String
    showTip = show
