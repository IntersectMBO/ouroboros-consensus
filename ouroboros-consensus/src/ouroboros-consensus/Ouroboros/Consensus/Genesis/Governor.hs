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
-- peers allows the current selection to advance. See
-- 'Ouroboros.Consensus.Storage.ChainDB.API.LoE' for more details.
--
-- The GDD governor, invoked with 'runGDDGovernor', is supposed to run in a background
-- thread. It evaluates candidate chains whenever they change, or whenever a
-- peer claims to have no more headers, or whenever a peer starts sending
-- headers beyond the forecast horizon.
--
-- Whenever GDD disconnects peers, and as a result the youngest header present
-- in all candidate fragments changes, the chain selection is updated.
--
module Ouroboros.Consensus.Genesis.Governor (
    DensityBounds (..)
  , TraceGDDEvent (..)
  , EvaluateGDD (..)
  , densityDisconnect
  , runGDDGovernor
  , sharedCandidatePrefix
  , evaluateGenesisGDD
  , evaluateForgivingGDD
  ) where

import           Control.Monad (guard, when)
import           Control.Tracer (Tracer, traceWith)
import           Data.Containers.ListUtils (nubOrd)
import           Data.Foldable (for_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (maybeToList)
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
                     (ChainSyncClientHandle (..), ChainSyncState (..))
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB,
                     triggerChainSelectionAsync)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Util (eitherToMaybe, whenJust)
import           Ouroboros.Consensus.Util.AnchoredFragment (stripCommonPrefix)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (blockUntilChanged)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

-- | A function to disconnect peers and compute the new LoE fragment.
--
-- The LoE fragment is the fragment anchored at the immutable tip and ending at
-- the LoE tip.
--
-- The callback is applied to the current chain and the current ledger state,
-- and yields the new LoE fragment.
data EvaluateGDD m blk = EvaluateGDD {
    evaluateGDD ::
         AnchoredFragment (Header blk)
      -> ExtLedgerState blk
      -> m (AnchoredFragment (Header blk))
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks)

-- | A dummy version of GDD that doesn't disconnect nodes and returns as LoE
-- fragment the current chain selection. This can be seen as emulating Praos
-- behavior.
evaluateForgivingGDD ::
  MonadSTM m =>
  EvaluateGDD m blk
evaluateForgivingGDD =
  EvaluateGDD $ \ curChain _ -> pure curChain

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

-- | A never ending computation that runs the GDD governor whenever
-- the STM action @getTrigger@ changes, writing the LoE fragment
-- computed by @loEUpdater@ to @varLoEFrag@, and then triggering
-- ChainSel to reprocess all blocks that had previously been
-- postponed by the LoE.
runGDDGovernor ::
  (Monoid a, Eq a, IOLike m, LedgerSupportsProtocol blk) =>
  EvaluateGDD m blk ->
  StrictTVar m (AnchoredFragment (Header blk)) ->
  ChainDB m blk ->
  STM m a ->
  m Void
runGDDGovernor loEUpdater varLoEFrag chainDb getTrigger =
  spin mempty
  where
    spin oldTrigger = do
      (newTrigger, curChain, curLedger) <- atomically $ do
        (_, newTrigger) <- blockUntilChanged id oldTrigger getTrigger
        curChain <- ChainDB.getCurrentChain chainDb
        curLedger <- ChainDB.getCurrentLedger chainDb
        pure (newTrigger, curChain, curLedger)
      loeFrag <- evaluateGDD loEUpdater curChain curLedger
      oldLoEFrag <- atomically $ swapTVar varLoEFrag loeFrag
      -- The chain selection only depends on the LoE tip, so there
      -- is no point in retriggering it if the LoE tip hasn't changed.
      when (AF.headHash oldLoEFrag /= AF.headHash loeFrag) $
        triggerChainSelectionAsync chainDb
      spin newTrigger

data DensityBounds blk =
  DensityBounds {
    clippedFragment :: AnchoredFragment (Header blk),
    offersMoreThanK :: Bool,
    lowerBound      :: Word64,
    upperBound      :: Word64,
    hasBlockAfter   :: Bool,
    latestSlot      :: WithOrigin SlotNo,
    idling          :: Bool
  }

-- | @densityDisconnect genWin k states candidateSuffixes loeFrag@
-- yields the list of peers which are known to lose the density comparison with
-- any other peer, when looking at the genesis window after @loeFrag@.
--
-- The peers are taken from the keys of @candidateSuffixes@.
--
-- @candidateSuffixes@ tells for every peer what is the fragment that the peer
-- proposes to use after @loeFrag@.
--
-- @states@ contains further information for every peer, such as the last
-- ChainSync instruction the peer sent, and whether the peer is idling (i.e. it
-- sent @MsgAwaitReply@).
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
  -> Map peer (ChainSyncState blk)
  -> Map peer (AnchoredFragment (Header blk))
  -> AnchoredFragment (Header blk)
  -> ([peer], Map peer (DensityBounds blk))
densityDisconnect (GenesisWindow sgen) (SecurityParam k) states candidateSuffixes loeFrag =
  (losingPeers, densityBounds)
  where
    densityBounds = Map.fromList $ do
      (peer, clippedFragment) <- Map.toList clippedFrags
      state <- maybeToList (states Map.!? peer)
      -- Skip peers that haven't sent any headers yet.
      -- They should be disconnected by timeouts instead.
      latestSlot <- maybeToList (csLatestSlot state)
      let candidateSuffix = candidateSuffixes Map.! peer

          idling = csIdling state

          -- Is there a block after the end of the Genesis window?
          hasBlockAfter =
               max (AF.headSlot candidateSuffix) latestSlot
            >= NotOrigin firstSlotAfterGenesisWindow

          -- If the slot of the latest header we know of is _after_ the end of
          -- the Genesis window (either because the candidate fragment extends
          -- beyond it or because we are waiting to validate a header beyond the
          -- forecast horizon that we already received), there can be no headers
          -- in between and 'potentialSlots' is 0.
          potentialSlots =
            if hasBlockAfter then 0
            else unknownTrailingSlots

          -- Number of trailing slots in the genesis window that could have
          -- headers which haven't been sent yet
          unknownTrailingSlots = unSlotNo $
            -- cannot underflow as the fragment is clipped to the genesis window
            firstSlotAfterGenesisWindow - succWithOrigin (AF.headSlot clippedFragment)

          -- The number of blocks within the Genesis window we know with certainty
          lowerBound = fromIntegral $ AF.length clippedFragment

          upperBound = lowerBound + potentialSlots

          -- The number of blocks we know to be on the candidate chain after
          -- the intersection, not limited to the Genesis window.
          totalBlockCount = fromIntegral (AF.length candidateSuffix)

          -- Does the peer have more than k known blocks in _total_ after the intersection?
          -- If not, it is not qualified to compete by density (yet).
          offersMoreThanK = totalBlockCount > k

      pure (peer, DensityBounds {clippedFragment, offersMoreThanK, lowerBound, upperBound, hasBlockAfter, latestSlot, idling})

    losingPeers = nubOrd $ Map.toList densityBounds >>= \
      (peer0 , DensityBounds { clippedFragment = frag0
                             , lowerBound = lb0
                             , upperBound = ub0
                             , hasBlockAfter = hasBlockAfter0
                             , idling = idling0
                             }) ->
      -- If the density is 0, the peer should be disconnected. This affects
      -- ChainSync jumping, where genesis windows with no headers prevent jumps
      -- from happening.
      if ub0 == 0 then pure peer0 else do
      (_peer1, DensityBounds {clippedFragment = frag1, offersMoreThanK, lowerBound = lb1 }) <-
        Map.toList densityBounds
      -- Don't disconnect peer0 if it sent no headers after the intersection yet
      -- and it is not idling.
      --
      -- See Note [Chain disagreement]
      --
      -- Note: hasBlockAfter0 is False if frag0 is empty and ub0>0.
      -- But we leave it here as a reminder that we care about it.
      guard $ idling0 || not (AF.null frag0) || hasBlockAfter0
      -- ensure that the two peer fragments don't share any
      -- headers after the LoE
      guard $ AF.lastPoint frag0 /= AF.lastPoint frag1
      -- peer1 offers more than k blocks or peer0 has sent all headers in the
      -- genesis window after the intersection (idling or not)
      guard $ offersMoreThanK || lb0 == ub0
      -- peer1 has the same or better density than peer0
      -- If peer0 is idling, we assume no more headers will be sent.
      --
      -- Having the same density is enough to disconnect peer0, as the honest
      -- chain is expected to have a strictly higher density than all of the
      -- other chains.
      --
      -- This matters to ChainSync jumping, where adversarial dynamo and
      -- objector could offer chains of equal density.
      guard $ lb1 >= (if idling0 then lb0 else ub0)
      pure peer0

    loeIntersectionSlot = AF.headSlot loeFrag

    firstSlotAfterGenesisWindow =
        succWithOrigin loeIntersectionSlot + SlotNo sgen

    dropBeyondGenesisWindow =
      AF.takeWhileOldest ((< firstSlotAfterGenesisWindow) . blockSlot)

    clippedFrags =
      Map.map dropBeyondGenesisWindow candidateSuffixes

-- Note [Chain disagreement]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Imagine two peers serving the following chain:
--
-- > k: 1
-- > sgen: 2
-- >
-- >   0 1 2
-- > G---1-2
--
-- Say peer1 sent no headers yet and peer2 sent 2 headers.
-- The intersection of both is G, the density of peer2's chain is 2,
-- while the upperbound of the density of peer1 is also 2.
--
-- For GDD to disconnect peer1 safely, it is essential that both chains
-- disagree after the intersection.
--
-- To know if the chains will dissagree we defer disconnecting peer1
-- until it declares to have no more headers, or until it sends one header
-- after the intersection. If both chains agree on the next header after
-- the intersection, we don't disconnect peer1 either.

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

-- | Disconnect nodes and recompute the LoE fragment.
--
-- See 'EvaluateGDD' for the definition of LoE fragment.
--
-- Additionally, disconnect the peers that lose density comparisons.
--
-- Disconnecting peers causes candidate fragments to be removed, which causes
-- the GDD governor to reevaluate GDD over and over until no more peers are
-- disconnected.
--
-- @getHandles@ is the callback to get the handles that allow to disconnect
-- from peers.
evaluateGenesisGDD ::
     forall m blk peer.
     ( IOLike m
     , Ord peer
     , LedgerSupportsProtocol blk
     , HasHardForkHistory blk
     )
  => TopLevelConfig blk
  -> Tracer m (TraceGDDEvent peer blk)
  -> STM m (Map peer (ChainSyncClientHandle m blk))
  -> EvaluateGDD m blk
evaluateGenesisGDD cfg tracer getHandles =
  EvaluateGDD $ \ curChain immutableLedgerSt -> do
    (states, candidates, candidateSuffixes, handles, loeFrag) <- atomically $ do
      handles <- getHandles
      states <- traverse (readTVar . cschState) handles
      let
        candidates = csCandidate <$> states
        (loeFrag, candidateSuffixes) =
          sharedCandidatePrefix curChain candidates
      pure (states, candidates, candidateSuffixes, handles, loeFrag)

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
          densityDisconnect sgen (configSecurityParam cfg) states candidateSuffixes loeFrag
        loeHead = AF.headAnchor loeFrag

      traceWith tracer TraceGDDEvent {sgen, curChain, bounds, candidates, candidateSuffixes, losingPeers, loeHead}

      for_ losingPeers $ \peer -> cschGDDKill (handles Map.! peer)

    pure loeFrag
