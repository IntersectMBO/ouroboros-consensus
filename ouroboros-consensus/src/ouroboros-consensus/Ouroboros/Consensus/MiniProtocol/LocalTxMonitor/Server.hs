{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Ouroboros.Consensus.MiniProtocol.LocalTxMonitor.Server (localTxMonitorServer) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Measure as Measure
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
                { capacityInBytes = unByteSize32 $ txMeasureByteSize capacity
                , sizeInBytes     = unByteSize32 $ txMeasureByteSize msNumBytes
                , numberOfTxs     = msNumTxs
                }
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
              -> Map MeasureName (SizeAndCapacity Integer)
mkMeasuresMap Proxy size capacity =
  Map.fromList
    [ (TransactionBytes, SizeAndCapacity (byteSizeInteger $ txMeasureMetricTxSizeBytes size) (byteSizeInteger $ txMeasureMetricTxSizeBytes capacity))
    , (ExUnitsMemory, SizeAndCapacity (fromIntegral $ txMeasureMetricExUnitsMemory size) (fromIntegral $ txMeasureMetricExUnitsMemory capacity))
    , (ExUnitsSteps, SizeAndCapacity (fromIntegral $ txMeasureMetricExUnitsSteps size) (fromIntegral $ txMeasureMetricExUnitsSteps capacity))
    , (ReferenceScriptsBytes, SizeAndCapacity (byteSizeInteger $ txMeasureMetricRefScriptsSizeBytes size) (byteSizeInteger $ txMeasureMetricRefScriptsSizeBytes capacity))
    ]
  where
    byteSizeInteger :: ByteSize32 -> Integer
    byteSizeInteger = fromIntegral . unByteSize32

pattern TransactionBytes :: MeasureName
pattern TransactionBytes = MeasureName "transaction_bytes"

pattern ExUnitsSteps :: MeasureName
pattern ExUnitsSteps = MeasureName "ex_units_steps"

pattern ExUnitsMemory :: MeasureName
pattern ExUnitsMemory = MeasureName "ex_units_memory"

pattern ReferenceScriptsBytes :: MeasureName
pattern ReferenceScriptsBytes = MeasureName "reference_scripts_bytes"
