{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | CertRB staging area.
--
-- An in-memory buffer of CertRBs whose announced EB closure isn't
-- locally available yet. The BlockFetch client stages such blocks
-- here instead of handing them to ChainSel (which would crash inside
-- 'resolveLeiosBlock'). 'newLeiosStagingArea' both wraps the
-- 'BlockFetchClientInterface.ChainDbView' so the gate is installed on
-- 'addBlockAsync', and spawns two supporting threads on the supplied
-- 'ResourceRegistry':
--
-- * a /drain/ that watches EB-closure notifications and admits the
--   matching staged block via the unwrapped 'ChainDB.addBlockAsync',
-- * a /garbage collector/ that periodically evicts entries whose
--   closure has become unreachable — no live peer's ChainSync
--   candidate contains the staged CertRB anymore.
--
-- Each staged entry remembers the block itself, the announced EB
-- body's expected on-the-wire size (the fetch logic needs it to
-- validate the response in 'msgLeiosBlock'), the set of peers known
-- — at staging time — to have this block on their ChainSync
-- candidate fragment (so the fetch logic can pretend those peers
-- offered the EB), and one or more 'StrictTMVar's owned by parked
-- BlockFetch client threads. Each parked thread waits on its handle
-- for a 'StagedOutcome' so it can return success ('StagedReleased',
-- drain admitted the block) or failure ('StagedEvicted', GC gave up
-- on the closure).
--
-- The staging area is the source of truth: the fetch logic 'reads'
-- 'stagedSnapshot' each tick and synthesises augmented inputs from
-- it transiently, never writing back here.
module LeiosStagingArea (module LeiosStagingArea) where

import qualified Control.Concurrent.Class.MonadMVar as MVar
import Control.Concurrent.Class.MonadSTM.Strict (readTChan)
import Control.Monad (forever, void)
import qualified Control.Monad.Class.MonadTimer.SI as SI
import Control.ResourceRegistry (ResourceRegistry, forkLinkedThread)
import Control.Tracer (Tracer, traceWith)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import LeiosDemoDb
  ( LeiosDbConnection (..)
  , LeiosDbHandle
  , withLeiosDb
  )
import LeiosDemoDb.Common (LeiosEbNotification (..), subscribeEbNotifications)
import LeiosDemoTypes
  ( BytesSize
  , LeiosPoint
  , PeerId (..)
  , TraceLeiosKernel (..)
  )
import Ouroboros.Consensus.Block
  ( ChainHash (..)
  , GetPrevHash
  , HasHeader
  , Header
  )
import qualified Ouroboros.Consensus.Block as Block
import Ouroboros.Consensus.HeaderValidation (HeaderWithTime (..))
import qualified Ouroboros.Consensus.MiniProtocol.BlockFetch.ClientInterface as BlockFetchClientInterface
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( ChainSyncClientHandle (..)
  , ChainSyncClientHandleCollection (..)
  , ChainSyncState (..)
  )
import Ouroboros.Consensus.Storage.ChainDB.API
  ( AddBlockPromise (..)
  , AddBlockResult (..)
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
import Ouroboros.Consensus.Storage.LedgerDB
  ( ResolveLeiosBlock (..)
  )
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , STM
  , StrictTMVar
  , atomically
  , modifyTVar
  , newEmptyTMVarIO
  , readTVar
  , takeTMVar
  , tryPutTMVar
  , uncheckedNewTVarM
  , writeTVar
  )
import qualified Ouroboros.Network.AnchoredFragment as AF

-- | Why a parked stage caller is being woken up.
data StagedOutcome
  = -- | The drain admitted the block via the unwrapped
    -- 'addBlockAsync'; the caller should report
    -- 'SuccesfullyAddedBlock'.
    StagedReleased
  | -- | GC gave up: no live peer's ChainSync candidate contains the
    -- staged CertRB anymore, so the closure has become unreachable.
    -- The caller should report 'FailedToAddBlock' and let BlockFetch's
    -- usual machinery (re-fetch from a fresh candidate, demote peer)
    -- take over.
    StagedEvicted
  deriving stock (Eq, Show)

-- | A single staged CertRB (the part visible to the fetch logic
-- synth step). Outcome handles are kept separately in the internal
-- map entry.
data StagedCertRB peer blk = MkStagedCertRB
  { stagedBlock :: !blk
  , stagedSize :: !BytesSize
  , stagedPeers :: !(Set.Set peer)
  -- ^ Peers known to have this block on their candidate at staging
  -- time. The fetch logic uses this set as the pool of peers it can
  -- pretend offered the certified EB.
  }

-- | Internal record: a staged 'StagedCertRB' plus the outcome
-- handles of every parked stage caller awaiting this point.
data StagedEntry m peer blk = MkStagedEntry
  { stagedRB :: !(StagedCertRB peer blk)
  , stagedOutcomes :: ![StrictTMVar m StagedOutcome]
  -- ^ One per parked stage caller. Two BlockFetch clients can stage
  -- the same point (e.g. two peers delivering the same CertRB before
  -- 'getIsFetched' deduplicates them); both get woken with the same
  -- outcome when the entry is released or evicted.
  }

-- | Staging area handle exposed to callers. The wrapped
-- 'ChainDbView' (with the gate installed) and the drain / GC threads
-- are created by 'newLeiosStagingArea'.
data LeiosStagingArea m peer blk = LeiosStagingArea
  { stagedSnapshot :: STM m (Map LeiosPoint (StagedCertRB peer blk))
  -- ^ Read-only snapshot of the staging area, consumed by the fetch
  -- logic each tick to synthesise augmented inputs (missing-EB
  -- entries + peer offerings).
  , wrappedChainDbView :: BlockFetchClientInterface.ChainDbView m blk
  -- ^ The 'ChainDbView' to hand to BlockFetch: 'addBlockAsync' is
  -- wrapped with the staging gate, and 'getIsFetched' is widened to
  -- treat staged blocks as fetched so BlockFetch's decision logic
  -- doesn't refetch them.
  }

-- | Create a staging area, install the staging gate on the
-- 'ChainDbView', and spawn the drain and GC threads on the supplied
-- resource registry.
newLeiosStagingArea ::
  forall m blk peer.
  ( IOLike m
  , GetPrevHash blk
  , ResolveLeiosBlock blk
  , Ord peer
  ) =>
  Tracer m TraceLeiosKernel ->
  ResourceRegistry m ->
  LeiosDbHandle m ->
  ChainSyncClientHandleCollection peer m blk ->
  -- | Pinged after a stage so the Leios fetch loop wakes promptly
  -- and synthesises the new entry into its next iteration.
  MVar.MVar m () ->
  -- | Base 'ChainDbView' to wrap. The drain admits released blocks
  -- via this view's 'addBlockAsync' (i.e. the unwrapped one,
  -- bypassing the staging gate); the returned 'wrappedChainDbView'
  -- has the gate installed on 'addBlockAsync' and widens
  -- 'getIsFetched'.
  BlockFetchClientInterface.ChainDbView m blk ->
  -- | GC tick interval. Choose long enough to amortise the per-entry
  -- STM scan; minutes are fine since GC is a safety backstop, not
  -- the common-case release path.
  SI.DiffTime ->
  m (LeiosStagingArea m (PeerId peer) blk)
newLeiosStagingArea
  tracer
  registry
  leiosDb
  varChainSyncHandles
  readyMVar
  defView
  gcInterval = do
    -- XXX: Should use stm-containers
    tv <- uncheckedNewTVarM (Map.empty :: Map LeiosPoint (StagedEntry m (PeerId peer) blk))
    notifications <- subscribeEbNotifications leiosDb
    void $ forkLinkedThread registry "LeiosStagingArea.drain" (drainLoop tv notifications)
    void $ forkLinkedThread registry "LeiosStagingArea.gc" (gcLoop tv)
    pure
      LeiosStagingArea
        { stagedSnapshot = stagedSnapshotSTM tv
        , wrappedChainDbView = wrapView tv
        }
   where
    -- Pure helpers
    mergeEntry new old =
      old
        { stagedRB =
            (stagedRB old)
              { stagedPeers =
                  stagedPeers (stagedRB old)
                    `Set.union` stagedPeers (stagedRB new)
              }
        , stagedOutcomes = stagedOutcomes new ++ stagedOutcomes old
        }

    -- STM primitives over the staging map
    stagedSnapshotSTM tv = Map.map stagedRB <$> readTVar tv

    isStagedBlockSTM tv = do
      staged <- readTVar tv
      let points =
            Set.fromList
              [Block.blockPoint (stagedBlock (stagedRB e)) | e <- Map.elems staged]
      pure (`Set.member` points)

    stage tv point size peers blk outcomeVar =
      modifyTVar tv $
        Map.insertWith
          mergeEntry
          point
          MkStagedEntry
            { stagedRB =
                MkStagedCertRB
                  { stagedBlock = blk
                  , stagedSize = size
                  , stagedPeers = peers
                  }
            , stagedOutcomes = [outcomeVar]
            }

    removeEntry tv point outcome = do
      m <- readTVar tv
      case Map.lookup point m of
        Nothing -> pure Nothing
        Just entry -> do
          writeTVar tv (Map.delete point m)
          mapM_ (\v -> tryPutTMVar v outcome) (stagedOutcomes entry)
          pure (Just (stagedBlock (stagedRB entry)))

    -- BlockFetch view wrapper
    wrapView tv =
      defView
        { BlockFetchClientInterface.addBlockAsync = stagingGate tv
        , BlockFetchClientInterface.getIsFetched = do
            baseFetched <- BlockFetchClientInterface.getIsFetched defView
            staged <- isStagedBlockSTM tv
            pure $ \p -> baseFetched p || staged p
        }

    stagingGate tv punish blk
      | not (blockHasLeiosCert blk) =
          BlockFetchClientInterface.addBlockAsync defView punish blk
      | otherwise = do
          -- Look up the parent header in the ChainSync candidate
          -- fragments. A CertRB delivered via BlockFetch is always
          -- fulfilling some candidate, so the parent (announcing the
          -- certified EB) is on that candidate by construction. We
          -- deliberately don't consult 'getCurrentChain' here: the
          -- locally selected chain is incidental — if the block
          -- extends our chain, the peer that delivered it tracks the
          -- same chain via ChainSync and the parent is in its
          -- candidate fragment too.
          mAnn <- atomically $ do
            candidates <- candidateFragments varChainSyncHandles
            pure $ findParentAnnouncement (Block.blockPrevHash blk) candidates
          case mAnn of
            -- Parent isn't visible on any ChainSync candidate, or
            -- didn't announce. Can't determine the EB; admit and
            -- let ChainSel / apply-time error decide.
            Nothing ->
              BlockFetchClientInterface.addBlockAsync defView punish blk
            Just (point, size) -> do
              mEb <- withLeiosDb leiosDb $ \conn ->
                leiosDbQueryCompletedEbByPoint conn point
              case mEb of
                Just _ ->
                  BlockFetchClientInterface.addBlockAsync defView punish blk
                Nothing -> stageAndPark tv point size blk

    -- \| Park the calling BlockFetch client thread on its own outcome
    -- handle registered with the staging area. The drain fills the
    -- handle with 'StagedReleased' when the closure arrives (and the
    -- block has been admitted to ChainDB via the unwrapped
    -- 'addBlockAsync' on the drain side). The GC fills it with
    -- 'StagedEvicted' when the closure becomes unreachable so we
    -- don't park forever.
    --
    -- We also still block the BlockFetch client here rather than
    -- returning early because that stops the decision module from
    -- re-fetching the same CertRB in a tight loop.
    --
    -- Cost: head-of-line blocking on this peer's BlockFetch pipeline.
    -- The closure fetch runs on a separate LeiosFetch channel of the
    -- same connection, so progress is not deadlocked. CPU cost is
    -- zero — STM block on TMVar.
    stageAndPark tv point size blk = do
      peers <- atomically $ peersThatKnowBlock varChainSyncHandles blk
      traceWith
        tracer
        TraceLeiosCertRBStaged
          { stagedBlockPoint = show (Block.blockPoint blk)
          , stagedEbPoint = point
          , stagedKnownPeers = Set.size peers
          }
      outcomeVar <- newEmptyTMVarIO
      atomically (stage tv point size peers blk outcomeVar)
      -- TODO: Should not be necessary if fetching logic would
      -- retry on the staging area query.
      _ <- MVar.tryPutMVar readyMVar ()
      outcome <- atomically $ takeTMVar outcomeVar
      case outcome of
        StagedReleased ->
          pure
            AddBlockPromise
              { blockWrittenToDisk = pure True
              , blockProcessed =
                  pure $ SuccesfullyAddedBlock (Block.blockPoint blk)
              }
        StagedEvicted -> do
          traceWith
            tracer
            TraceLeiosCertRBEvicted
              { evictedBlockPoint = show (Block.blockPoint blk)
              , evictedEbPoint = point
              }
          pure
            AddBlockPromise
              { blockWrittenToDisk = pure False
              , blockProcessed =
                  pure $ FailedToAddBlock "CertRB closure unreachable; evicted from Leios staging area"
              }

    drainLoop tv notifications =
      forever $
        atomically (readTChan notifications) >>= \case
          -- Only the EB body arrived; the tx closure isn't complete
          -- yet. 'resolveLeiosBlock' queries
          -- 'leiosDbQueryCompletedEbByPoint', which still returns
          -- 'Nothing' here. Releasing now would re-submit the staged
          -- block before its txs land and crash at apply time. Wait
          -- for 'AcquiredEbTxs'.
          AcquiredEb{} -> pure ()
          AcquiredEbTxs point -> do
            mStaged <- atomically $ removeEntry tv point StagedReleased
            case mStaged of
              Nothing -> pure ()
              Just blk -> do
                traceWith tracer TraceLeiosCertRBReleased{releasedEbPoint = point}
                void $
                  BlockFetchClientInterface.addBlockAsync
                    defView
                    InvalidBlockPunishment.noPunishment
                    blk

    gcLoop tv = forever $ do
      SI.threadDelay gcInterval
      snapshot <- atomically (stagedSnapshotSTM tv)
      mapM_ (gcOne tv) (Map.toList snapshot)

    -- \| Re-check inside the same STM transaction as the eviction to
    -- avoid racing the drain: if the drain just released this entry,
    -- the snapshot read by the GC tick is stale; we must verify the
    -- entry still exists at eviction time. The entry is evicted when
    -- no live peer's ChainSync candidate still contains the staged
    -- CertRB (so no peer can serve its closure).
    gcOne tv (point, rb) = do
      mEvicted <- atomically $ do
        peers <- peersThatKnowBlock varChainSyncHandles (stagedBlock rb)
        if Set.null peers
          then removeEntry tv point StagedEvicted
          else pure Nothing
      case mEvicted of
        Nothing -> pure ()
        Just blk ->
          traceWith
            tracer
            TraceLeiosCertRBEvicted
              { evictedBlockPoint = show (Block.blockPoint blk)
              , evictedEbPoint = point
              }

-- | Find the parent header in any ChainSync candidate fragment and
-- read its 'headerLeiosAnnouncement'.
--
-- A CertRB delivered via BlockFetch is by construction fulfilling
-- some candidate, so the parent (whose header carries the EB
-- announcement) lives in at least one of the live candidate
-- fragments. The locally selected chain is intentionally not
-- consulted: a parent that's only on the selected chain but on no
-- candidate would mean the chain has rolled past the point where any
-- peer is currently fetching — in which case the staging gate has
-- nothing useful to decide anyway.
findParentAnnouncement ::
  forall blk peer.
  (HasHeader (Header blk), ResolveLeiosBlock blk) =>
  ChainHash blk ->
  Map.Map peer (AF.AnchoredFragment (HeaderWithTime blk)) ->
  Maybe (LeiosPoint, BytesSize)
findParentAnnouncement prev candidates = case prev of
  GenesisHash -> Nothing
  BlockHash h ->
    find
      (\hdr -> Block.blockHash hdr == h)
      (concatMap (fmap hwtHeader . AF.toNewestFirst) (Map.elems candidates))
      >>= headerLeiosAnnouncement

-- | Snapshot every peer's current ChainSync candidate fragment.
candidateFragments ::
  IOLike m =>
  ChainSyncClientHandleCollection peer m blk ->
  STM m (Map.Map peer (AF.AnchoredFragment (HeaderWithTime blk)))
candidateFragments varChainSyncHandles = do
  handles <- cschcMap varChainSyncHandles
  traverse (fmap csCandidate . readTVar . cschState) handles

-- | Scan all ChainSync candidates for ones whose fragment contains a
-- header with the same hash as @blk@. Those peers' chains have
-- admitted this block, so they almost certainly hold (or can quickly
-- obtain) the certified EB closure.
peersThatKnowBlock ::
  ( IOLike m
  , HasHeader blk
  , HasHeader (Header blk)
  , Ord peer
  ) =>
  ChainSyncClientHandleCollection peer m blk ->
  blk ->
  STM m (Set.Set (PeerId peer))
peersThatKnowBlock varChainSyncHandles blk = do
  handles <- cschcMap varChainSyncHandles
  let hsh = Block.blockHash blk
  fmap (Set.fromList . Map.elems) $
    flip Map.traverseMaybeWithKey handles $ \peer h -> do
      st <- readTVar (cschState h)
      let frag = csCandidate st
      pure $
        if any
          ((== hsh) . Block.blockHash)
          (AF.toOldestFirst frag)
          then Just (MkPeerId peer)
          else Nothing
