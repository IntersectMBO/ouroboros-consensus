{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server (localStateQueryServer) where

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip (..))
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (AcquireFailure (..), Target (..))
import qualified Ouroboros.Network.PublicState as Public

localStateQueryServer ::
     forall m addrNTN blk. (IOLike m, BlockSupportsLedgerQuery blk, ConfigSupportsNode blk, HasAnnTip blk)
  => ExtLedgerCfg blk
  -> m (Public.NetworkState addrNTN)
     -- ^ Get public network state
  -> STM m (Point blk)
     -- ^ Get tip point
  -> (Point blk -> STM m (Maybe (ExtLedgerState blk)))
     -- ^ Get a past ledger
  -> STM m (Point blk)
     -- ^ Get the immutable point
  -> LocalStateQueryServer blk (Point blk) (Query blk addrNTN) m ()
localStateQueryServer cfg getNetworkState getTipPoint getPastLedger getImmutablePoint =
    LocalStateQueryServer $ return idle
  where
    idle :: ServerStIdle blk (Point blk) (Query blk addrNTN) m ()
    idle = ServerStIdle {
          recvMsgAcquire = handleAcquire
        , recvMsgDone    = return ()
        }

    handleAcquire :: Target (Point blk)
                  -> m (ServerStAcquiring blk (Point blk) (Query blk addrNTN) m ())
    handleAcquire tpt = do
        (pt, mPastLedger, immutablePoint) <- atomically $ do
          pt <- case tpt of
                  VolatileTip         -> getTipPoint
                  SpecificPoint point -> pure point
                  ImmutableTip        -> getImmutablePoint
          (pt,,) <$> getPastLedger pt <*> getImmutablePoint

        return $ case mPastLedger of
          Just pastLedger
            -> SendMsgAcquired $ acquired pastLedger
          Nothing
            | pointSlot pt < pointSlot immutablePoint
            -> SendMsgFailure AcquireFailurePointTooOld idle
            | otherwise
            -> SendMsgFailure AcquireFailurePointNotOnChain idle

    acquired :: ExtLedgerState blk
             -> ServerStAcquired blk (Point blk) (Query blk addrNTN) m ()
    acquired st = ServerStAcquired {
          recvMsgQuery     = handleQuery st
        , recvMsgReAcquire = handleAcquire
        , recvMsgRelease   = return idle
        }

    handleQuery ::
         ExtLedgerState blk
      -> Query blk addrNTN result
      -> m (ServerStQuerying blk (Point blk) (Query blk addrNTN) m () result)
    handleQuery st query = do
        result <- answerQueryM cfg query st getNetworkState
        return $ SendMsgResult
          result
          (acquired st)
