{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server (localStateQueryServer) where

import Control.Monad (void)
import Control.ResourceRegistry
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Query
  ( BlockSupportsLedgerQuery
  , Query
  )
import qualified Ouroboros.Consensus.Ledger.Query as Query
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol
  )
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Protocol.LocalStateQuery.Server
import Ouroboros.Network.Protocol.LocalStateQuery.Type
  ( AcquireFailure (..)
  , Target (..)
  )

localStateQueryServer ::
  forall m blk.
  ( IOLike m
  , BlockSupportsLedgerQuery blk
  , Query.ConfigSupportsNode blk
  , LedgerSupportsProtocol blk
  ) =>
  ExtLedgerCfg blk ->
  ( Target (Point blk) ->
    m (Either GetForkerError (ResourceKey m, ReadOnlyForker' m blk))
  ) ->
  LocalStateQueryServer blk (Point blk) (Query blk) m ()
localStateQueryServer cfg getView =
  LocalStateQueryServer $ return idle
 where
  idle :: ServerStIdle blk (Point blk) (Query blk) m ()
  idle =
    ServerStIdle
      { recvMsgAcquire = handleAcquire
      , recvMsgDone = return ()
      }

  handleAcquire ::
    Target (Point blk) ->
    m (ServerStAcquiring blk (Point blk) (Query blk) m ())
  handleAcquire mpt = do
    eForker <- getView mpt
    case eForker of
      Right (rk, forker) -> pure $ SendMsgAcquired $ acquired rk forker
      Left e -> do
        pure $ case e of
          PointTooOld{} ->
            SendMsgFailure AcquireFailurePointTooOld idle
          PointNotOnChain ->
            SendMsgFailure AcquireFailurePointNotOnChain idle
          -- LedgerDB still replaying; reuse PointTooOld so clients retry.
          LedgerNotReady ->
            SendMsgFailure AcquireFailurePointTooOld idle

  acquired ::
    ResourceKey m ->
    ReadOnlyForker' m blk ->
    ServerStAcquired blk (Point blk) (Query blk) m ()
  acquired rk forker =
    ServerStAcquired
      { recvMsgQuery = handleQuery rk forker
      , recvMsgReAcquire = \mp -> do close; handleAcquire mp
      , recvMsgRelease = do close; return idle
      }
   where
    close = void $ release rk

  handleQuery ::
    ResourceKey m ->
    ReadOnlyForker' m blk ->
    Query blk result ->
    m (ServerStQuerying blk (Point blk) (Query blk) m () result)
  handleQuery rk forker query = do
    result <- Query.answerQuery cfg forker query
    return $ SendMsgResult result (acquired rk forker)
