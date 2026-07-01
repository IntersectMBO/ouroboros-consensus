-- | CertRB staging area for issue #890.
--
-- An in-memory buffer of CertRBs whose announced EB closure isn't
-- locally available yet. The BlockFetch client stages such blocks here
-- instead of handing them to ChainSel (which would crash inside
-- 'resolveLeiosBlock'); a drain loop ('runStagingAreaDrain') releases
-- them once the corresponding EB closure arrives via LeiosFetch
-- notifications.
--
-- Each staged entry remembers:
--
-- * the block itself,
-- * the announced EB body's expected on-the-wire size (the fetch logic
--   needs it to validate the response in 'msgLeiosBlock'), and
-- * the set of peers known — at staging time — to have this block on
--   their ChainSync candidate fragment, so the fetch logic can pretend
--   those peers offered the EB (Phase 2 emergency fetch).
--
-- The staging area is the source of truth: the fetch logic 'reads'
-- 'stagedSnapshot' each tick and synthesises augmented inputs from it
-- transiently, never writing back here.
--
-- TODO(issue #890, Phase 2 follow-up): garbage-collect staged entries
-- whose peer set is empty AND have aged out (no current candidate
-- contains them anymore). For now, entries live until the drain
-- releases them.
module LeiosStagingArea (module LeiosStagingArea) where

import Control.Concurrent.Class.MonadSTM.Strict
  ( MonadSTM
  , STM
  , StrictTChan
  , atomically
  , modifyTVar
  , newTVar
  , readTChan
  , readTVar
  , writeTVar
  )
import Control.Monad (forever)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import LeiosDemoDb.Common (LeiosEbNotification (..))
import LeiosDemoTypes (BytesSize, LeiosPoint)
import Ouroboros.Consensus.Block (HasHeader, Point, blockPoint)

-- | A single staged CertRB.
data StagedCertRB peer blk = MkStagedCertRB
  { stagedBlock :: !blk
  , stagedSize :: !BytesSize
  , stagedPeers :: !(Set.Set peer)
  -- ^ Peers known to have this block on their candidate at staging
  -- time. The fetch logic uses this set as the pool of peers it can
  -- pretend offered the certified EB.
  }

-- | Staging area for CertRBs awaiting their EB closure.
data LeiosStagingArea m peer blk = LeiosStagingArea
  { stageCertRB ::
      LeiosPoint ->
      BytesSize ->
      Set.Set peer ->
      blk ->
      STM m ()
  -- ^ Stage @blk@ at the certified-EB @LeiosPoint@. If an entry
  -- already exists for the same point, its peer set is unioned with
  -- the new one (later peers can add themselves) and size is preserved.
  , releaseCertRB :: LeiosPoint -> STM m (Maybe blk)
  -- ^ Remove and return the block staged at @point@, if any. Called by
  -- the drain loop once the corresponding EB closure arrives.
  , isStagedBlock :: STM m (Point blk -> Bool)
  -- ^ A predicate over block points that returns 'True' for blocks
  -- currently in the staging area. Used to widen the BlockFetch
  -- client's view of fetched blocks so it doesn't keep refetching.
  , stagedSnapshot :: STM m (Map LeiosPoint (StagedCertRB peer blk))
  -- ^ Read-only snapshot of the staging area, consumed by the fetch
  -- logic each tick to synthesise augmented inputs (missing-EB
  -- entries + peer offerings).
  }

newLeiosStagingArea ::
  (MonadSTM m, HasHeader blk, Ord peer) =>
  m (LeiosStagingArea m peer blk)
newLeiosStagingArea = do
  tvar <- atomically $ newTVar Map.empty
  pure
    LeiosStagingArea
      { stageCertRB = \point size peers blk ->
          modifyTVar tvar $
            Map.insertWith
              ( \new old ->
                  old{stagedPeers = stagedPeers old `Set.union` stagedPeers new}
              )
              point
              MkStagedCertRB
                { stagedBlock = blk
                , stagedSize = size
                , stagedPeers = peers
                }
      , releaseCertRB = \point -> do
          m <- readTVar tvar
          case Map.lookup point m of
            Nothing -> pure Nothing
            Just entry -> do
              writeTVar tvar (Map.delete point m)
              pure (Just (stagedBlock entry))
      , isStagedBlock = do
          staged <- readTVar tvar
          let points =
                Set.fromList
                  [blockPoint (stagedBlock e) | e <- Map.elems staged]
          pure (`Set.member` points)
      , stagedSnapshot = readTVar tvar
      }

-- | Drain loop: every EB-closure notification removes any block staged
-- on that point and hands it to @onReleased@ (typically
-- 'ChainDB.addBlockAsync').
--
-- The caller supplies the notification channel (obtained via
-- 'subscribeEbNotifications') so this module need not depend on the
-- 'LeiosDbHandle' type.
runStagingAreaDrain ::
  MonadSTM m =>
  LeiosStagingArea m peer blk ->
  StrictTChan m LeiosEbNotification ->
  (LeiosPoint -> blk -> m ()) ->
  m Void
runStagingAreaDrain area chan onReleased = forever $ do
  ev <- atomically (readTChan chan)
  case ev of
    AcquiredEb{} ->
      -- Only the EB body arrived; the tx closure isn't complete yet.
      -- 'resolveLeiosBlock' queries 'leiosDbQueryCompletedEbByPoint',
      -- which still returns 'Nothing' here. Releasing now would
      -- re-submit the staged block before its txs land and crash at
      -- apply time. Wait for 'AcquiredEbTxs'.
      pure ()
    AcquiredEbTxs point -> do
      mStaged <- atomically $ releaseCertRB area point
      case mStaged of
        Nothing -> pure ()
        Just blk -> onReleased point blk
