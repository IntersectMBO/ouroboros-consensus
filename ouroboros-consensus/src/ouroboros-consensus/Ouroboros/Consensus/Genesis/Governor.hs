{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
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
  , GDDStateView (..)
  , TraceGDDEvent (..)
  , densityDisconnect
  , gddWatcher
  , sharedCandidatePrefix
  ) where

import           Control.Monad (guard, when)
import           Control.Tracer (Tracer, traceWith)
import           Data.Bifunctor (second)
import           Data.Containers.ListUtils (nubOrd)
import           Data.Foldable (for_, toList)
import           Data.Functor.Compose (Compose (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (maybeToList)
import           Data.Maybe.Strict (StrictMaybe)
import           Data.Word (Word64)
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
import           Ouroboros.Consensus.Node.GsmState
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Util (eitherToMaybe, whenJust)
import           Ouroboros.Consensus.Util.AnchoredFragment (stripCommonPrefix)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (Watcher (..))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF

-- | A 'Watcher' that evaluates the GDD rule whenever necessary, writing the LoE
-- fragment to @varLoEFrag@, and then triggering ChainSel to reprocess all
-- blocks that had previously been postponed by the LoE.
--
-- Evaluating the GDD rule might cause peers to be disconnected if they have
-- sparser chains than the best chain.
gddWatcher ::
     forall m blk peer.
     ( IOLike m
     , Ord peer
     , LedgerSupportsProtocol blk
     , HasHardForkHistory blk
     )
  => TopLevelConfig blk
  -> Tracer m (TraceGDDEvent peer blk)
  -> ChainDB m blk
  -> DiffTime -- ^ How often to evaluate GDD. 0 means as soon as possible.
              -- Otherwise, no faster than once every T seconds, where T is
              -- the provided value.
  -> STM m GsmState
  -> STM m (Map peer (ChainSyncClientHandle m blk))
     -- ^ The ChainSync handles. We trigger the GDD whenever our 'GsmState'
     -- changes, and when 'Syncing', whenever any of the candidate fragments
     -- changes. Also, we use this to disconnect from peers with insufficient
     -- densities.
  -> StrictTVar m (AnchoredFragment (Header blk))
     -- ^ The LoE fragment. It starts at a (recent) immutable tip and ends at
     -- the common intersection of the candidate fragments.
  -> Watcher m
       (GsmState, GDDStateView m blk peer)
       (Map peer (StrictMaybe (WithOrigin SlotNo), Bool))
gddWatcher cfg tracer chainDb rateLimit getGsmState getHandles varLoEFrag =
    Watcher {
        wInitial = Nothing
      , wReader  = (,) <$> getGsmState <*> getGDDStateView
      , wFingerprint
      , wNotify
      }
  where
    getGDDStateView :: STM m (GDDStateView m blk peer)
    getGDDStateView = do
        curChain          <- ChainDB.getCurrentChain chainDb
        immutableLedgerSt <- ChainDB.getImmutableLedger chainDb
        handles           <- getHandles
        states            <- traverse (readTVar . cschState) handles
        pure GDDStateView {
            gddCtxCurChain          = curChain
          , gddCtxImmutableLedgerSt = immutableLedgerSt
          , gddCtxKillActions       = Map.map cschGDDKill handles
          , gddCtxStates            = states
          }

    wFingerprint ::
         (GsmState, GDDStateView m blk peer)
      -> Map peer (StrictMaybe (WithOrigin SlotNo), Bool)
    wFingerprint (gsmState, GDDStateView{gddCtxStates}) = case gsmState of
        -- When we are in 'PreSyncing' (HAA not satisfied) or are caught up, we
        -- don't have to run the GDD on changes to the candidate fragments.
        -- (Maybe we want to do it in 'PreSycing'?)
        PreSyncing -> Map.empty
        CaughtUp   -> Map.empty
        -- When syncing, wake up regularly while headers are sent.
        -- Watching csLatestSlot ensures that GDD is woken up when a peer is
        -- sending headers even if they are after the forecast horizon. Note
        -- that there can be some delay between the header being validated and
        -- it becoming visible to GDD. It will be visible only when csLatestSlot
        -- changes again or when csIdling changes, which is guaranteed to happen
        -- eventually.
        Syncing    ->
          Map.map (\css -> (csLatestSlot css, csIdling css)) gddCtxStates

    wNotify :: (GsmState, GDDStateView m blk peer) -> m ()
    wNotify (_gsmState, stateView) = do
        t0 <- getMonotonicTime
        loeFrag <- evaluateGDD cfg tracer stateView
        oldLoEFrag <- atomically $ swapTVar varLoEFrag loeFrag
        -- The chain selection only depends on the LoE tip, so there
        -- is no point in retriggering it if the LoE tip hasn't changed.
        when (AF.headHash oldLoEFrag /= AF.headHash loeFrag) $
          ChainDB.triggerChainSelectionAsync chainDb
        tf <- getMonotonicTime
        -- We limit the rate at which GDD is evaluated, otherwise it would
        -- be called every time a new header is validated.
        threadDelay $ rateLimit - diffTime tf t0

-- | Pure snapshot of the dynamic data the GDD operates on.
data GDDStateView m blk peer = GDDStateView {
    -- | The current chain selection
    gddCtxCurChain          :: AnchoredFragment (Header blk)
    -- | The current ledger state
  , gddCtxImmutableLedgerSt :: ExtLedgerState blk
    -- | Callbacks to disconnect from peers
  , gddCtxKillActions       :: Map peer (m ())
  , gddCtxStates            :: Map peer (ChainSyncState blk)
  }

-- | Disconnect peers that lose density comparisons and recompute the LoE fragment.
--
-- Disconnecting peers causes candidate fragments to be removed, which causes
-- the GDD governor to reevaluate GDD over and over until no more peers are
-- disconnected.
--
-- Yields the new LoE fragment.
--
evaluateGDD ::
     forall m blk peer.
     ( IOLike m
     , Ord peer
     , LedgerSupportsProtocol blk
     , HasHardForkHistory blk
     )
  => TopLevelConfig blk
  -> Tracer m (TraceGDDEvent peer blk)
  -> GDDStateView m blk peer
  -> m (AnchoredFragment (Header blk))
evaluateGDD cfg tracer stateView = do
    let GDDStateView {
            gddCtxCurChain          = curChain
          , gddCtxImmutableLedgerSt = immutableLedgerSt
          , gddCtxKillActions       = killActions
          , gddCtxStates            = states
          } = stateView

        (loeFrag, candidateSuffixes) =
          sharedCandidatePrefix curChain candidates
        candidates = Map.toList (csCandidate <$> states)

        msgen :: Maybe GenesisWindow
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

      for_ losingPeers $ \peer -> killActions Map.! peer

    pure loeFrag

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
  [(peer, AnchoredFragment (Header blk))] ->
  (AnchoredFragment (Header blk), [(peer, AnchoredFragment (Header blk))])
sharedCandidatePrefix curChain candidates =
  second getCompose $
  stripCommonPrefix (AF.anchor curChain) $
  Compose immutableTipSuffixes
  where
    immutableTip = AF.anchorPoint curChain

    splitAfterImmutableTip (peer, frag) =
      case AF.splitAfterPoint frag immutableTip of
        -- When there is no intersection, we assume the candidate fragment is
        -- empty and anchored at the immutable tip.
        -- See Note [CSJ truncates the candidate fragments].
        Nothing          -> (peer, AF.takeOldest 0 curChain)
        Just (_, suffix) -> (peer, suffix)

    immutableTipSuffixes =
      map splitAfterImmutableTip candidates

-- Note [CSJ truncates the candidate fragments]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Before CSJ, only rollback could cause truncation of a candidate fragment.
-- Truncation is a serious business to GDD because the LoE might have allowed
-- the selection to advance, based on the tips of the candidate fragments.
--
-- Truncating a candidate fragment risks moving the LoE back, which could be
-- earlier than the anchor of the latest selection. When rollbacks where the
-- only mechanism to truncate, it was fine to ignore candidate fragments that
-- don't intersect with the current selection. This could only happen if the
-- peer is rolling back more than k blocks, which is dishonest behavior.
--
-- With CSJ, however, the candidate fragments can recede without a rollback.
-- A former objector might be asked to jump back when it becomes a jumper again.
-- The jump point might still be a descendent of the immutable tip. But by the
-- time the jump is accepted, the immutable tip might have advanced, and the
-- candidate fragment of the otherwise honest peer might be ignored by GDD.
--
-- Therefore, at the moment, when there is no intersection with the current
-- selection, the GDD assumes that the candidate fragment is empty and anchored
-- at the immutable tip. It is the job of the ChainSync client to update the
-- candidate fragment so it intersects with the selection or to disconnect the
-- peer if no such fragment can be established.
--

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

deriving stock instance (Show (Header blk), GetHeader blk) => Show (DensityBounds blk)

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
-- @loeFrag@ is the fragment anchored at the immutable tip and ending in the
-- LoE tip.
--
-- ChainSync jumping depends on this function to disconnect either of any two
-- peers that offer different chains and provided a header in the last slot of
-- the genesis window or later. Either of them should be disconnected, even if
-- both of them are serving adversarial chains. See
-- "Ouroboros.Consensus.MiniProtocol.ChainSync.Client.Jumping" for more details.
--
densityDisconnect ::
     ( Ord peer
     , LedgerSupportsProtocol blk
     )
  => GenesisWindow
  -> SecurityParam
  -> Map peer (ChainSyncState blk)
  -> [(peer, AnchoredFragment (Header blk))]
  -> AnchoredFragment (Header blk)
  -> ([peer], [(peer, DensityBounds blk)])
densityDisconnect (GenesisWindow sgen) (SecurityParam k) states candidateSuffixes loeFrag =
  (losingPeers, densityBounds)
  where
    densityBounds = do
      (peer, candidateSuffix) <- candidateSuffixes
      let clippedFragment = dropBeyondGenesisWindow candidateSuffix
      state <- maybeToList (states Map.!? peer)
      -- Skip peers that haven't sent any headers yet.
      -- They should be disconnected by timeouts instead.
      latestSlot <- toList (csLatestSlot state)
      let idling = csIdling state

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

    losingPeers = nubOrd $ densityBounds >>= \
      (peer0 , DensityBounds { clippedFragment = frag0
                             , lowerBound = lb0
                             , upperBound = ub0
                             , hasBlockAfter = hasBlockAfter0
                             , idling = idling0
                             }) -> do
      (_peer1, DensityBounds {clippedFragment = frag1, offersMoreThanK, lowerBound = lb1 }) <-
        densityBounds
      -- Don't disconnect peer0 if it sent no headers after the intersection yet
      -- and it is not idling.
      --
      -- See Note [Chain disagreement]
      --
      guard $ idling0 || not (AF.null frag0) || hasBlockAfter0
      -- ensure that the two peer fragments don't share any
      -- headers after the LoE
      guard $ AF.lastPoint frag0 /= AF.lastPoint frag1
      -- peer1 offers more than k blocks or peer0 has sent all headers in the
      -- genesis window after the intersection (idling or not)
      --
      -- Checking for offersMoreThanK is important to avoid disconnecting
      -- competing honest peers when the syncing node is nearly caught up.
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

      -- We disconnect peer0 if there is at least another peer peer1 with a
      -- chain which is at least as good, and peer0 is either idling or there is
      -- no extension to peer0's chain that can make it better than peer1's, and
      -- peer1's has more than k headers or peer0 has sent all its headers in
      -- the genesis window anchored at the intersection.
      --
      -- A chain is "as good as another" if it has at least as many headers in
      -- the genesis window anchored at the intersection.
      pure peer0

    loeIntersectionSlot = AF.headSlot loeFrag

    firstSlotAfterGenesisWindow =
        succWithOrigin loeIntersectionSlot + SlotNo sgen

    -- This is performance sensitive. We used to call @takeWhileOldest@ here,
    -- which would reconstruct much of the original fragment.
    dropBeyondGenesisWindow =
      AF.dropWhileNewest ((>= firstSlotAfterGenesisWindow) . blockSlot)

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
    bounds            :: [(peer, DensityBounds blk)],
    curChain          :: AnchoredFragment (Header blk),
    candidates        :: [(peer, AnchoredFragment (Header blk))],
    candidateSuffixes :: [(peer, AnchoredFragment (Header blk))],
    losingPeers       :: [peer],
    loeHead           :: AF.Anchor (Header blk),
    sgen              :: GenesisWindow
  }

deriving stock instance
  ( GetHeader blk, Show (Header blk), Show peer
  ) => Show (TraceGDDEvent peer blk)
