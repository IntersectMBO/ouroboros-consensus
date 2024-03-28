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

-- | Implementation of the GDD governor
--
-- The GDD governor is the component responsible for identifying and
-- disconnecting peers offering sparser chains than the best. This has the
-- effect of unblocking the Limit on Eagerness, since removing disagreeing
-- peers allows the current selection to advance.
--
-- The GDD governor, invoked with 'runGdd', is supposed to run in a background
-- thread. It evaluates candidate chains whenever they change, or whenever a
-- peer claims to have no more headers, or whenever a peer starts sending
-- headers beyond the forecast horizon.
--
-- Whenever GDD disconnects peers, the chain selection is updated.
--
module Ouroboros.Consensus.Genesis.Governor (
    DensityBounds (..)
  , TraceGDDEvent (..)
  , UpdateLoEFrag (..)
  , densityDisconnect
  , runGdd
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
import           Data.Maybe (fromMaybe)
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

-- | An action representing an update to the LoE fragment, that determines which
-- blocks can be selected in the ChainDB. With Ouroboros Genesis, this is
-- implemented via the GDD governor, see 'updateLoEFragGenesis'.
--
-- The callback is applied to the current chain and the current ledger state,
-- and yields the new LoE fragment, which should be anchored in the immutable
-- tip.
data UpdateLoEFrag m blk = UpdateLoEFrag {
    updateLoEFrag ::
         AnchoredFragment (Header blk)
      -> ExtLedgerState blk
      -> m (AnchoredFragment (Header blk))
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks)

-- | A dummy version of the LoE that sets the LoE fragment to the current
-- selection. This can be seen as emulating Praos behavior.
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

-- | A never ending computation that runs the GDD governor whenever
-- the STM action @getTrigger@ changes, writing the LoE fragment
-- computed by @loEUpdater@ to @varLoEFrag@, and then triggering
-- ChainSel to reprocess all blocks that had previously been
-- postponed by the LoE.
runGdd ::
  (Monoid a, Eq a, IOLike m, LedgerSupportsProtocol blk) =>
  UpdateLoEFrag m blk ->
  StrictTVar m (AnchoredFragment (Header blk)) ->
  ChainDB m blk ->
  STM m a ->
  m Void
runGdd loEUpdater varLoEFrag chainDb getTrigger =
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

data DensityBounds blk =
  DensityBounds {
    fragment        :: AnchoredFragment (Header blk),
    offersMoreThanK :: Bool,
    lowerBound      :: Word64,
    upperBound      :: Word64,
    hasBlockAfter   :: Bool,
    latestSlot      :: WithOrigin SlotNo
  }

-- | @densityDisconnect genWin k candidateSuffixes caughtUpPeers latestSlots loeFrag@
-- yields the list of peers which are known to lose the density comparison with
-- any other peer, when looking at the genesis window after @loeFrag@.
--
-- The peers are taken from the keys of @candidateSuffixes@.
--
-- @candidateSuffixes@ tells for every peer what is the fragment that the peer
-- proposes to use after @loeFrag@.
--
-- @caughtUpPeers@ contains peers that sent @MsgAwaitReply@.
--
-- @latestSlots@ tells for every peer which is the slot of the last header that
-- it sent. PRECONDITION: The last sent header should never be later than the
-- the first header after the candidate fragment.
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
  -> Map peer (WithOrigin SlotNo)
  -> AnchoredFragment (Header blk)
  -> ([peer], Map peer (DensityBounds blk))
densityDisconnect (GenesisWindow sgen) (SecurityParam k) candidateSuffixes caughtUpPeers latestSlots loeFrag =
  (losingPeers, densityBounds)
  where
    densityBounds = Map.fromList $ do
      (peer, fragment) <- Map.toList competingFrags
      let candidateSuffix = candidateSuffixes Map.! peer

          caughtUp = Set.member peer caughtUpPeers

          latestSlot = fromMaybe Origin (latestSlots Map.!? peer)

          -- Is there a block after the end of the Genesis window?
          hasBlockAfter =
               max (AF.headSlot candidateSuffix) latestSlot
            >= NotOrigin firstSlotAfterGenesisWindow

          -- If the peer is idling, we assume it has no more headers to send.
          --
          -- If the slot of the latest header we know of is _after_ the end of
          -- the Genesis window (either because the candidate fragment extends
          -- beyond it or because we are waiting to validate a header beyond the
          -- forecast horizon that we already received), there can be no headers
          -- in between and 'potentialSlots' is 0.
          --
          -- If the peer has more headers that it hasn't sent yet, each slot
          -- between the latest header we know of and the end of the Genesis
          -- window could contain a block, so the upper bound for the total
          -- number of blocks in the window is the sum of the known blocks and
          -- that number of remaining slots.
          potentialSlots =
            if caughtUp || hasBlockAfter then 0
            else sgen - totalBlockCount

          -- The number of blocks within the Genesis window we know with certainty
          lowerBound = fromIntegral $ AF.length fragment

          upperBound = lowerBound + potentialSlots

          -- The number of blocks we know to be on the candidate chain after
          -- the intersection, not limited to the Genesis window.
          totalBlockCount = fromIntegral (AF.length candidateSuffix)

          -- Does the peer have more than k known blocks in _total_ after the intersection?
          -- If not, it is not qualified to compete by density (yet).
          offersMoreThanK = totalBlockCount > k

      pure (peer, DensityBounds {fragment, offersMoreThanK, lowerBound, upperBound, hasBlockAfter, latestSlot})

    losingPeers = nubOrd $ do
      (peer0 , DensityBounds {fragment = frag0, upperBound = ub0}) <-
        Map.toList densityBounds
      (_peer1, DensityBounds {fragment = frag1, offersMoreThanK, lowerBound = lb1 }) <-
        Map.toList densityBounds
      -- ensure that the two peer fragments don't share any
      -- headers after the LoE
      guard $ AF.lastPoint frag0 /= AF.lastPoint frag1
      -- peer1 offers more than k blocks
      guard offersMoreThanK
      -- peer1 definitely has higher density than peer0
      guard $ lb1 > ub0
      pure peer0

    loeIntersectionSlot = AF.headSlot loeFrag

    firstSlotAfterGenesisWindow =
        succWithOrigin loeIntersectionSlot + SlotNo sgen

    dropBeyondGenesisWindow =
      AF.takeWhileOldest ((< firstSlotAfterGenesisWindow) . blockSlot)

    competingFrags =
      Map.map dropBeyondGenesisWindow candidateSuffixes

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
-- Disconnecting peers causes chain fragments to be removed, which causes
-- the LoE fragment to be updated over and over until no more peers are
-- disconnected.
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

      for_ losingPeers $ \peer -> cschGDDKill (handles Map.! peer)

    pure loeFrag
