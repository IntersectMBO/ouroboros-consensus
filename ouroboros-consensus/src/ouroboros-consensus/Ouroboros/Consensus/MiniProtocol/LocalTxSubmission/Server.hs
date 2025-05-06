{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server (
    localTxSubmissionServer
    -- * Trace events
  , TraceLocalTxSubmissionServerEvent (..)
  ) where

import           Control.Tracer
import           Data.Tuple (Solo (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Protocol.LocalTxSubmission.Server
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type


-- | Local transaction submission server, for adding txs to the 'Mempool'
--
localTxSubmissionServer ::
     MonadSTM m
  => Tracer m (TraceLocalTxSubmissionServerEvent blk)
  -> Mempool m blk
  -> LocalTxSubmissionServer (GenTx blk) (ApplyTxErr blk) m ()
localTxSubmissionServer tracer mempool =
    server
  where
    server = LocalTxSubmissionServer {
      recvMsgSubmitTx = \tx -> do
        traceWith tracer $ TraceReceivedTx tx
        MkSolo addTxRes <- addLocalTxs mempool (MkSolo tx)
        case addTxRes of
          MempoolTxAdded _tx             -> return (SubmitSuccess, server)
          MempoolTxRejected _tx addTxErr -> return (SubmitFail addTxErr, server)

    , recvMsgDone = ()
    }


{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

data TraceLocalTxSubmissionServerEvent blk
  = TraceReceivedTx (GenTx blk)
    -- ^ A transaction was received.

deriving instance Eq   (GenTx blk)
               => Eq   (TraceLocalTxSubmissionServerEvent blk)
deriving instance Show (GenTx blk)
               => Show (TraceLocalTxSubmissionServerEvent blk)
