{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Ouroboros.Consensus.MiniProtocol.LocalTxMonitor.Server (localTxMonitorServer) where

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Protocol.LocalTxMonitor.Server
import           Ouroboros.Network.Protocol.LocalTxMonitor.Type
import           Ouroboros.Network.SizeInBytes (getSizeInBytes)

-- | Local transaction monitoring server, for inspecting the mempool.
--
localTxMonitorServer ::
     forall blk m.
     ( MonadSTM m
     , LedgerSupportsMempool blk
     )
  => Mempool m blk
  -> LocalTxMonitorServer (GenTxId blk) (GenTx blk) SlotNo m ()
localTxMonitorServer mempool =
    LocalTxMonitorServer (pure serverStIdle)
  where
    serverStIdle
      :: ServerStIdle (GenTxId blk) (GenTx blk) SlotNo m ()
    serverStIdle =
      ServerStIdle
      { recvMsgDone = do
          pure ()
      , recvMsgAcquire = do
          s <- atomically query
          pure $ serverStAcquiring s
      }

    serverStAcquiring
      :: (TxMeasure blk, MempoolSnapshot blk)
      -> ServerStAcquiring (GenTxId blk) (GenTx blk) SlotNo m ()
    serverStAcquiring s@(_, snapshot) =
      SendMsgAcquired
        (snapshotSlotNo snapshot)
        (serverStAcquired s (snapshotTxs snapshot))

    serverStAcquired
      :: (TxMeasure blk, MempoolSnapshot blk)
      -> [TxTicket (TxMeasure blk) (Validated (GenTx blk))]
      -> ServerStAcquired (GenTxId blk) (GenTx blk) SlotNo m ()
    serverStAcquired s@(capacity, snapshot) txs =
      ServerStAcquired
      { recvMsgNextTx =
          case txs of
            []  ->
              pure $ SendMsgReplyNextTx Nothing (serverStAcquired s [])
            (txTicketTx -> txForgetValidated -> h):q ->
              pure $ SendMsgReplyNextTx (Just h) (serverStAcquired s q)
      , recvMsgHasTx = \txid ->
          pure $ SendMsgReplyHasTx (snapshotHasTx snapshot txid) (serverStAcquired s txs)
      , recvMsgGetSizes = do
          let MempoolSize{msNumTxs,msSize} = snapshotMempoolSize snapshot
          let sizes = MempoolSizeAndCapacity
                { capacityInBytes = getSizeInBytes $ txMeasureBytes snapshot capacity
                , sizeInBytes     = getSizeInBytes $ txMeasureBytes snapshot msSize
                , numberOfTxs     = msNumTxs
                }
          pure $ SendMsgReplyGetSizes sizes (serverStAcquired s txs)
      , recvMsgAwaitAcquire = do
          s' <- atomically $ do
            s'@(_, snapshot') <- query
            s' <$ check (not (snapshot `isSameSnapshot` snapshot'))
          pure $ serverStAcquiring s'
      , recvMsgRelease =
          pure serverStIdle
      }

    -- Are two snapshots equal? (from the perspective of this protocol)
    isSameSnapshot
      :: MempoolSnapshot blk
      -> MempoolSnapshot blk
      -> Bool
    isSameSnapshot a b =
      (txTicketNo <$> snapshotTxs a) == (txTicketNo <$> snapshotTxs b)
      &&
      snapshotSlotNo a == snapshotSlotNo b

    query :: STM m (TxMeasure blk, MempoolSnapshot blk)
    query = do
      capacity <- worstCaseCapacity <$> getCapacity mempool
      snapshot <- getSnapshot mempool
      pure (capacity, snapshot)
