{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The mempool keeps two views of its one sequence of txs.
--
-- The main view, its 'InternalState', is the txs on the tip. It serves
-- downstream peers and forging blocks with txs.
--
-- The certifying view anticipates the next block certifying the EB the tip
-- announces: it is the txs on the tip with that EB's closure applied. It
-- exists only while the tip announces an EB whose closure the node has, and
-- it serves forging a certifying block.
--
-- Both views hold the same txs. Adding txs changes the main view, and a
-- thread applies them to the certifying view off the mempool's lock, so
-- admission pays nothing for it. Removing txs changes both under the lock.
-- The mempool's sync, on each adopted block, rebuilds the main view, or makes
-- the certifying view the main one if the block certifies the EB; then it
-- builds the certifying view for the new tip's announcement.
module Ouroboros.Consensus.Mempool.CertifyingView
  ( buildCertifyingView
  , catchUpCertifyingView
  , certifyingViewCatchUp
  , removeFromCertifyingView
  , promoteCertifyingView
  , implGetCertifyingSnapshotFor
  ) where

import Control.Monad (forever, guard)
import Data.Functor ((<&>))
import Data.Maybe (isJust)
import Data.Void (Void)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.Tables.Utils (rawPrependDiffs)
import Ouroboros.Consensus.Leios.Types (EbHash)
import Ouroboros.Consensus.Mempool.API (MempoolSnapshot (..))
import Ouroboros.Consensus.Mempool.Capacity (computeMempoolCapacity)
import Ouroboros.Consensus.Mempool.Impl.Common
import Ouroboros.Consensus.Mempool.TxSeq (TxTicket (..))
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import Ouroboros.Consensus.Storage.LedgerDB.Forker (ReadOnlyForker (..))
import Ouroboros.Consensus.Util.IOLike

-- | The certifying view of the main view's txs, for the EB its tip announces.
-- 'Nothing' if the closure does not apply.
buildCertifyingView ::
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  MempoolEnv m blk ->
  -- | At the main view's tip
  ReadOnlyForker m LedgerState blk ->
  EbHash ->
  -- | The main view
  InternalState blk ->
  m (Maybe (CertifyingView blk))
buildCertifyingView mpEnv frk eb is =
  applyAnnouncedClosure (mpEnvLedger mpEnv) eb frk (isSlotNo is) >>= \case
    Nothing -> pure Nothing
    Just base -> do
      RevalidateTxsResult{newInternalState} <-
        revalidateTxsFor
          frk
          (mpEnvCapacityOverride mpEnv)
          (mpEnvLedgerCfg mpEnv)
          (isSlotNo is)
          base
          (isLastTicketNo is)
          (isRemovalCounter is)
          (TxSeq.toList (isTxs is))
      pure $ Just CertifyingView{cvEb = eb, cvBase = base, cvState = newInternalState}

-- | Apply the txs the main view gained since the certifying view was last
-- brought up to date with it.
catchUpCertifyingView ::
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  MempoolEnv m blk ->
  -- | At the main view's tip
  ReadOnlyForker m LedgerState blk ->
  -- | The main view
  InternalState blk ->
  CertifyingView blk ->
  m (CertifyingView blk)
catchUpCertifyingView mpEnv frk is cv@CertifyingView{cvState}
  | null new = pure cv
  | otherwise = do
      RevalidateTxsResult{newInternalState} <-
        revalidateTxsFor'
          frk
          (mpEnvCapacityOverride mpEnv)
          (mpEnvLedgerCfg mpEnv)
          (isSlotNo cvState)
          (RevalidateTxsResult cvState [])
          (isLastTicketNo is)
          new
      pure cv{cvState = newInternalState}
 where
  new = TxSeq.toList . snd $ TxSeq.splitAfterTicketNo (isTxs is) (isLastTicketNo cvState)

-- | Apply the txs added to the main view to the certifying view, whenever
-- there are some.
certifyingViewCatchUp ::
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  MempoolEnv m blk ->
  m Void
certifyingViewCatchUp mpEnv = forever $ do
  atomically $ lagging mpEnv >>= check . isJust
  implCatchUpCertifyingView mpEnv

-- | Apply the txs added to the main view to the certifying view, off the
-- mempool's lock and with a forker of its own. The result replaces the view
-- only if nothing else changed it meanwhile: a rebuild or a removal wins.
implCatchUpCertifyingView ::
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  MempoolEnv m blk ->
  m ()
implCatchUpCertifyingView mpEnv =
  atomically ((,) <$> lagging mpEnv <*> getCurrentLedgerState (mpEnvLedger mpEnv)) >>= \case
    (Just (is, cv), ledger) ->
      mldViewGetForker ledger >>= \case
        Left _ -> pure ()
        Right frk -> do
          cv' <- catchUpCertifyingView mpEnv frk is cv
          roforkerClose frk
          atomically $
            readTVar (mpEnvCertifyingView mpEnv) >>= \case
              Just current | sameView current cv -> writeTVar (mpEnvCertifyingView mpEnv) (Just cv')
              _ -> pure ()
    (Nothing, _) -> pure ()
 where
  sameView a b =
    cvEb a == cvEb b
      && isTip (cvState a) == isTip (cvState b)
      && isRemovalCounter (cvState a) == isRemovalCounter (cvState b)
      && isLastTicketNo (cvState a) == isLastTicketNo (cvState b)

-- | The main view and its certifying view, if the latter misses txs of the
-- former and the ledger is still at their tip.
lagging ::
  (IOLike m, LedgerSupportsMempool blk) =>
  MempoolEnv m blk ->
  STM m (Maybe (InternalState blk, CertifyingView blk))
lagging mpEnv = do
  is <- readSVarSTM (mpEnvStateVar mpEnv)
  ledger <- getCurrentLedgerState (mpEnvLedger mpEnv)
  readTVar (mpEnvCertifyingView mpEnv) <&> \case
    Just cv
      | isTip (cvState cv) == isTip is
      , castPoint (getTip (mldViewState ledger)) == isTip is
      , isRemovalCounter (cvState cv) == isRemovalCounter is
      , isLastTicketNo (cvState cv) < isLastTicketNo is ->
          Just (is, cv)
    _ -> Nothing

-- | The certifying view of only the txs the main view kept after a removal:
-- revalidated from its base, as the main view is from its tip.
removeFromCertifyingView ::
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  MempoolEnv m blk ->
  -- | At the main view's tip
  ReadOnlyForker m LedgerState blk ->
  -- | The main view after the removal
  InternalState blk ->
  CertifyingView blk ->
  m (CertifyingView blk)
removeFromCertifyingView mpEnv frk is cv@CertifyingView{cvBase} = do
  RevalidateTxsResult{newInternalState} <-
    revalidateTxsFor
      frk
      (mpEnvCapacityOverride mpEnv)
      (mpEnvLedgerCfg mpEnv)
      (isSlotNo is)
      cvBase
      (isLastTicketNo is)
      (isRemovalCounter is)
      (TxSeq.toList (isTxs is))
  pure cv{cvState = newInternalState}

-- | The certifying view as the main view on a new tip that certifies its EB.
--
-- Its diffs are relative to the tip it was built on, which the new tip extends
-- by exactly the closure: relative to the new tip only the txs' own diffs
-- remain, and the values are the ones the new tip's forker reads.
promoteCertifyingView ::
  (IOLike m, LedgerSupportsMempool blk) =>
  MempoolEnv m blk ->
  -- | At the new tip
  ReadOnlyForker m LedgerState blk ->
  SlotNo ->
  -- | The new tip, ticked to the slot
  TickedLedgerState blk DiffMK ->
  CertifyingView blk ->
  m (Maybe (InternalState blk))
promoteCertifyingView mpEnv frk slot ls CertifyingView{cvState = view}
  | inSlotRange slot (isTxSlotRange view <> ledgerSlotRange cfg (isLedgerState view)) = do
      values <- roforkerReadTables frk (isTxKeys view)
      pure $
        Just
          view
            { isTip = castPoint (getTip ls)
            , isSlotNo = slot
            , isLedgerState = isLedgerState view `withLedgerTables` txDiffs
            , isTxValues = values
            , isCapacity = computeMempoolCapacity cfg ls (mpEnvCapacityOverride mpEnv)
            }
  | otherwise = pure Nothing
 where
  cfg = mpEnvLedgerCfg mpEnv
  txDiffs =
    LedgerTables $
      foldl
        rawPrependDiffs
        (DiffMK mempty)
        [getLedgerTables (validatedTxDiffs (txTicketTx t)) | t <- TxSeq.toList (isTxs view)]

-- | See 'Ouroboros.Consensus.Mempool.API.getCertifyingSnapshotFor'.
implGetCertifyingSnapshotFor ::
  (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk)) =>
  MempoolEnv m blk ->
  EbHash ->
  ChainHash blk ->
  SlotNo ->
  STM m (Maybe (MempoolSnapshot blk))
implGetCertifyingSnapshotFor mpEnv eb tipHash slot = do
  mbView <- readTVar (mpEnvCertifyingView mpEnv)
  pure $ do
    CertifyingView{cvEb, cvState = is} <- mbView
    guard $ cvEb == eb && pointHash (isTip is) == tipHash
    -- As for 'getSnapshotFor': ticking within the ranges changes nothing.
    guard $
      isSlotNo is == slot
        || ( slot > isSlotNo is
               && inSlotRange slot (isTxSlotRange is <> ledgerSlotRange (mpEnvLedgerCfg mpEnv) (isLedgerState is))
           )
    pure (snapshotFromIS is){snapshotSlotNo = slot}
