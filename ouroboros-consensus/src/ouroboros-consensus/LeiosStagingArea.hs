-- | CertRB staging area for issue #890.
--
-- A small in-memory buffer of CertRBs whose announced EB closure isn't
-- locally available yet. The BlockFetch client stages such blocks here
-- instead of handing them to ChainSel (which would crash inside
-- 'resolveLeiosBlock'); a drain loop ('runStagingAreaDrain') releases
-- them once the corresponding EB closure arrives via LeiosFetch
-- notifications.
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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import LeiosDemoDb.Common (LeiosEbNotification (..))
import LeiosDemoTypes (LeiosPoint)
import Ouroboros.Consensus.Block (HasHeader, Point, blockPoint)

-- | Staging area for CertRBs awaiting their EB closure.
--
-- The TVar is encapsulated; callers use the record fields to stage,
-- release, or query staged blocks.
data LeiosStagingArea m blk = LeiosStagingArea
  { stageCertRB :: LeiosPoint -> blk -> STM m ()
  -- ^ Stage @blk@ at the missing EB @LeiosPoint@. Replaces any prior
  -- entry for the same point.
  , releaseCertRB :: LeiosPoint -> STM m (Maybe blk)
  -- ^ Remove and return the block staged at @point@, if any. Called by
  -- the drain loop once the corresponding EB closure arrives.
  , isStagedBlock :: STM m (Point blk -> Bool)
  -- ^ A predicate over block points that returns 'True' for blocks
  -- currently in the staging area. Used to widen the BlockFetch
  -- client's view of fetched blocks so it doesn't keep refetching them.
  }

newLeiosStagingArea ::
  (MonadSTM m, HasHeader blk) => m (LeiosStagingArea m blk)
newLeiosStagingArea = do
  tvar <- atomically $ newTVar Map.empty
  pure
    LeiosStagingArea
      { stageCertRB = \point blk ->
          modifyTVar tvar (Map.insert point blk)
      , releaseCertRB = \point -> do
          m <- readTVar tvar
          case Map.lookup point m of
            Nothing -> pure Nothing
            Just blk -> do
              writeTVar tvar (Map.delete point m)
              pure (Just blk)
      , isStagedBlock = do
          staged <- readTVar tvar
          let points =
                Set.fromList [blockPoint b | b <- Map.elems staged]
          pure (`Set.member` points)
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
  LeiosStagingArea m blk ->
  StrictTChan m LeiosEbNotification ->
  (LeiosPoint -> blk -> m ()) ->
  m Void
runStagingAreaDrain area chan onReleased = forever $ do
  ev <- atomically (readTChan chan)
  let point = case ev of
        AcquiredEb p _ -> p
        AcquiredEbTxs p -> p
  mStaged <- atomically $ releaseCertRB area point
  case mStaged of
    Nothing -> pure ()
    Just blk -> onReleased point blk
