{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Ouroboros.Consensus.MiniProtocol.LocalTxMonitor.Server (localTxMonitorServer) where

import qualified Data.Measure as Measure
import Data.Word (Word32)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
            <$> getCapacity mempool
            <*> getSnapshot mempool
          pure $ serverStAcquiring s
      }

    serverStAcquiring
      :: (TxMeasure blk, MempoolSnapshot blk)
      -> ServerStAcquiring (GenTxId blk) (GenTx blk) SlotNo m ()
    serverStAcquiring s@(_, snapshot) =
      SendMsgAcquired (snapshotSlotNo snapshot) (serverStAcquired s (snapshotTxs snapshot))

    serverStAcquired
      :: (TxMeasure blk, MempoolSnapshot blk)
      -> [(Validated (GenTx blk), idx, TxMeasure blk)]
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
                { capacityInBytes = fromByteSize $ txMeasureByteSize capacity
                , sizeInBytes     = fromByteSize $ txMeasureByteSize msNumBytes
                , numberOfTxs     = msNumTxs
                }   -- TODO what to do about overflow?
          pure $ SendMsgReplyGetSizes sizes (serverStAcquired s txs)
      , recvMsgGetMeasures = do
          let txsMeasures =
                foldl (\acc (_, _, m) -> Measure.plus acc m) Measure.zero txs
              measures = MempoolMeasures
                { txCount = fromIntegral $ length txs
                , measuresMap =
                    mkMeasuresMap (Proxy :: Proxy blk) txsMeasures capacity
                } -- TODO what to do about overflow?
          pure $ SendMsgReplyGetMeasures measures (serverStAcquired s txs)
      , recvMsgAwaitAcquire = do
          s' <- atomically $ do
            s'@(_, snapshot') <-
                  (,)
              <$> getCapacity mempool
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

mkMeasuresMap :: TxMeasureMetrics (TxMeasure blk)
              => Proxy blk
              -> TxMeasure blk
              -> TxMeasure blk
              -> Map MeasureName (SizeAndCapacity Word32)
mkMeasuresMap Proxy size capacity =
  fmap (fmap fromIntegral) $ -- oof oof ow ouch oo ow
    Map.fromList
      [ (TransactionBytes, SizeAndCapacity (txMeasureMetricTxSizeBytes size) (txMeasureMetricTxSizeBytes capacity))
      , (ExUnitsMemory, SizeAndCapacity (txMeasureMetricExUnitsMemory size) (txMeasureMetricExUnitsMemory capacity))
      , (ExUnitsSteps, SizeAndCapacity (txMeasureMetricExUnitsSteps size) (txMeasureMetricExUnitsSteps capacity))
      , (ReferenceScriptsBytes, SizeAndCapacity (txMeasureMetricRefScriptsSizeBytes size) (txMeasureMetricRefScriptsSizeBytes capacity))
      ]
