{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Ouroboros.Consensus.MiniProtocol.LocalTxMonitor.Server (localTxMonitorServer) where

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Protocol.LocalTxMonitor.Server
import           Ouroboros.Network.Protocol.LocalTxMonitor.Type

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
          s <- atomically $
                (,)
            <$> (txMeasureByteSize <$> getCapacity mempool)
            <*> getSnapshot mempool
          pure $ serverStAcquiring s
      }

    serverStAcquiring
      :: (ByteSize32, MempoolSnapshot blk)
      -> ServerStAcquiring (GenTxId blk) (GenTx blk) SlotNo m ()
    serverStAcquiring s@(_, snapshot) =
      SendMsgAcquired (snapshotSlotNo snapshot) (serverStAcquired s (snapshotTxs snapshot))

    serverStAcquired
      :: (ByteSize32, MempoolSnapshot blk)
      -> [(Validated (GenTx blk), idx, ByteSize32)]
      -> ServerStAcquired (GenTxId blk) (GenTx blk) SlotNo m ()
    serverStAcquired s@(capacity, snapshot) txs =
      ServerStAcquired
      { recvMsgNextTx =
          case txs of
            []  ->
              pure $ SendMsgReplyNextTx Nothing (serverStAcquired s [])
            (txForgetValidated -> h, _tno, _byteSize):q ->
              pure $ SendMsgReplyNextTx (Just h) (serverStAcquired s q)
      , recvMsgHasTx = \txid ->
          pure $ SendMsgReplyHasTx (snapshotHasTx snapshot txid) (serverStAcquired s txs)
      , recvMsgGetSizes = do
          let MempoolSize{msNumTxs,msNumBytes} = snapshotMempoolSize snapshot
          let sizes = MempoolSizeAndCapacity
                { capacityInBytes = unByteSize32 capacity
                , sizeInBytes     = unByteSize32 msNumBytes
                , numberOfTxs     = msNumTxs
                }
          pure $ SendMsgReplyGetSizes sizes (serverStAcquired s txs)
      , recvMsgAwaitAcquire = do
          s' <- atomically $ do
            s'@(_, snapshot') <-
                  (,)
              <$> (txMeasureByteSize <$> getCapacity mempool)
              <*> getSnapshot mempool
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
      (tno <$> snapshotTxs a) == (tno <$> snapshotTxs b)
      &&
      snapshotSlotNo a == snapshotSlotNo b

    tno (_a, b, _c) = b :: TicketNo
