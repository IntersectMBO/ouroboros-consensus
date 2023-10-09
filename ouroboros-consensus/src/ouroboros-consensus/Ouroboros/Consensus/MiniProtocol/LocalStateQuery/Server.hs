{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server (localStateQueryServer) where


import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query (BlockSupportsLedgerQuery,
                     Query)
import qualified Ouroboros.Consensus.Ledger.Query as Query
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Storage.LedgerDB
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type

localStateQueryServer ::
     forall m blk.
     ( IOLike m
     , BlockSupportsLedgerQuery blk
     , Query.ConfigSupportsNode blk
     , LedgerSupportsProtocol blk
     )
  => ExtLedgerCfg blk
  -> (   Maybe (Point blk)
      -> m (Either (Point blk) (LedgerDBView' m blk))
     )
  -> LocalStateQueryServer blk (Point blk) (Query blk) m ()
localStateQueryServer cfg getView =
    LocalStateQueryServer $ return idle
  where
    idle :: ServerStIdle blk (Point blk) (Query blk) m ()
    idle = ServerStIdle {
          recvMsgAcquire = handleAcquire
        , recvMsgDone    = return ()
        }

    handleAcquire :: Maybe (Point blk)
                  -> m (ServerStAcquiring blk (Point blk) (Query blk) m ())
    handleAcquire mpt = do
        getView mpt >>= \case
          Left immP
            | maybe False ((< pointSlot immP) . pointSlot) mpt
            -> return $ SendMsgFailure AcquireFailurePointTooOld idle
            | otherwise
            -> return $ SendMsgFailure AcquireFailurePointNotOnChain idle
          Right dlv -> return $ SendMsgAcquired $ acquired dlv

    acquired :: LedgerDBView' m blk
             -> ServerStAcquired blk (Point blk) (Query blk) m ()
    acquired dlv = ServerStAcquired {
          recvMsgQuery     = handleQuery dlv
        , recvMsgReAcquire = \mp -> do close; handleAcquire mp
        , recvMsgRelease   =        do close; return idle
        }
      where
        close = closeLedgerDBView dlv

    handleQuery ::
         LedgerDBView' m blk
      -> Query blk result
      -> m (ServerStQuerying blk (Point blk) (Query blk) m () result)
    handleQuery dlv query = do
      result <- Query.answerQuery cfg dlv query
      return $ SendMsgResult result (acquired dlv)
