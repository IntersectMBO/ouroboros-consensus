{-# LANGUAGE Rank2Types #-}

module LeiosTxCache
  ( LeiosTxCache (..)
  , newPureLeiosTxCache
  ) where

import Cardano.Slotting.Slot (SlotNo)
import qualified Control.Concurrent.Class.MonadMVar as MVar
import Data.Set (Set)
import LeiosDemoTypes (EbHash, RbHash, TxHash)
import LeiosTxCacheIndex (ReferencesTxsByHash)
import qualified LeiosTxCacheIndex as Pure
import Ouroboros.Consensus.Util.IOLike (IOLike)

-- | A monadic tx-cache handle: the pure index operations, each performing its
-- state update in @m@.
data LeiosTxCache m a v b = LeiosTxCache
  { insertAnnouncement :: SlotNo -> RbHash -> EbHash -> m (Set EbHash, Set TxHash)
  -- ^ Insert an announcement; returns the bodies and txs it evicted, if any.
  , insertBody :: EbHash -> b -> m ()
  , withLockedInsertUnappliedTx :: (forall w. w -> (w -> TxHash -> a -> m w) -> m w) -> m ()
  -- ^ Has exclusive write-access
  , withLockedInsertAppliedTx :: (forall w. w -> (w -> TxHash -> v -> m w) -> m w) -> m ()
  -- ^ Has exclusive write-access
  , withLookupTx :: forall r. ((TxHash -> m (Maybe (Either a v))) -> m r) -> m r
  -- ^ Does not not hold the lock
  }

-- | A handle backed by the pure index behind an 'MVar'.
newPureLeiosTxCache ::
  (IOLike m, ReferencesTxsByHash b) =>
  m (LeiosTxCache m a v b)
newPureLeiosTxCache = do
  var <- MVar.newMVar Pure.emptyLeiosTxCacheIndex
  pure
    LeiosTxCache
      { insertAnnouncement = \slot rbh ebh ->
          MVar.modifyMVar var $ \idx ->
            let (idx', evEbs, evTxs) = Pure.insertAnnouncement slot rbh ebh idx
             in pure (idx', (evEbs, evTxs))
      , insertBody = \ebh b ->
          MVar.modifyMVar_ var (pure . Pure.insertBody ebh b)
      , withLockedInsertUnappliedTx = \k ->
          MVar.modifyMVar_ var $ \idx ->
            k idx (\idx' txh a -> pure $! Pure.insertUnappliedTx txh a idx')
      , withLockedInsertAppliedTx = \k ->
          MVar.modifyMVar_ var $ \idx ->
            k idx (\idx' txh v -> pure $! Pure.insertAppliedTx txh v idx')
      , withLookupTx = \k -> do
          idx <- MVar.readMVar var
          k $ \txh -> pure $! Pure.lookupTx txh idx
      }
