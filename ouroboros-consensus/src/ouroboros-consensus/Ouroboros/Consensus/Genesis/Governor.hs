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
  , LatestSlot (..)
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

-- | This callback is a hook into ChainSync that is called right before deciding
-- whether a block can be added to the current selection.
--
-- Its purpose is to update the LoE fragment, anchored at the immutable tip and
-- whose tip is the header of the youngest block present in all candidate
-- fragments (their latest common intersection).
--
-- The callback is applied to the current chain and the current ledger state,
-- and yields the new LoE fragment.
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

-- | Indicates whether there is a latest known header in the candidate fragment
-- and provides its slot number.
data LatestSlot =
  NoLatestSlot
  | LatestSlot SlotNo
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
  -> Map peer (ChainSyncState blk)
  -> Map peer (AnchoredFragment (Header blk))
  -> AnchoredFragment (Header blk)
  -> ([peer], Map peer (DensityBounds blk))
densityDisconnect (GenesisWindow sgen) (SecurityParam k) states candidateSuffixes loeFrag =
  (losingPeers, densityBounds)
  where
    densityBounds = Map.fromList $ do
      (peer, fragment) <- Map.toList competingFrags
      state <- maybeToList (states Map.!? peer)
      -- Skip peers that haven't sent any headers yet.
      -- They should be disconnected by timeouts instead.
      latestSlot' <- maybeToList (csLatestSlot state)
      let candidateSuffix = candidateSuffixes Map.! peer

          -- TODO When is the latest slot set to Origin?
          latestSlot = case (AF.headSlot candidateSuffix, latestSlot') of
            (Origin, Origin)                   -> NoLatestSlot
            (Origin, NotOrigin latest)         -> LatestSlot latest
            (NotOrigin cand, Origin)           -> LatestSlot cand
            (NotOrigin cand, NotOrigin latest) -> LatestSlot (max cand latest)

          -- If the peer has more headers that it hasn't sent yet, each slot between the latest header we know of and
          -- the end of the Genesis window could contain a block, so the upper bound for the total number of blocks in
          -- the window is the sum of the known blocks and that number of remaining slots.
          -- If the slot of the latest header we know of is _after_ the end of the Genesis window (either because the
          -- candidate fragment extends beyond it or because we are waiting to validate a header beyond the forecast
          -- horizon that we already received), there can be no headers in between and 'potentialSlots' is 0.
          SlotNo potentialSlots
            | not (csIdling state)
            = 0
            | otherwise
            = case latestSlot of
              -- The peer either has not advertised its tip yet or simply has no blocks whatsoever and won't progress
              -- either.
              -- In the latter case, it should be killed by the LoP.
              NoLatestSlot -> SlotNo sgen
              LatestSlot slot -> intervalLength (slot + 1) firstSlotAfterGenesisWindow

          -- The number of blocks within the Genesis window we know with certainty
          lowerBound = fromIntegral $ AF.length fragment

          upperBound = lowerBound + potentialSlots

          -- The number of blocks we know of on the candidate chain, not limited to the Genesis window.
          totalBlockCount = fromIntegral (AF.length candidateSuffix)

          -- Does the peer have more than k known blocks in _total_ after the intersection?
          -- If not, it is not qualified to compete by density (yet).
          offersMoreThanK = case latestSlot of
            NoLatestSlot -> False
            LatestSlot _ -> totalBlockCount > k

          -- For tracing: Is there a block after the end of the Genesis window?
          hasBlockAfter = case latestSlot of
            NoLatestSlot    -> False
            LatestSlot slot -> slot >= firstSlotAfterGenesisWindow

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

    firstSlotAfterGenesisWindow =
        succWithOrigin loeIntersectionSlot + SlotNo sgen

    dropBeyondGenesisWindow =
      AF.takeWhileOldest ((< firstSlotAfterGenesisWindow) . blockSlot)

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
  -> STM m (Map peer (ChainSyncClientHandle m blk))
  -> UpdateLoEFrag m blk
updateLoEFragGenesis cfg tracer getHandles =
  UpdateLoEFrag $ \ curChain immutableLedgerSt -> do
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

      for_ losingPeers $ \peer -> cschKill (handles Map.! peer)

    pure loeFrag
