{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Running LeiosNotify as a node-to-node mini-protocol.
module Ouroboros.Consensus.Network.LeiosNotify
  ( leiosNotifyMiniProtocolNum
  , leiosNotifyPipelineDepth
  , runLookaheadFixedSenderPeerWithLimits
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer)
import Network.Mux.Timeout (withTimeoutSerial)
import qualified Network.Mux.Types as Mux
import Network.TypedProtocol.Codec (Codec)
import Network.TypedProtocol.Core (Protocol (..))
import Network.TypedProtocol.Driver (runLookaheadFixedSenderPeerWithDriver)
import Network.TypedProtocol.Peer (PeerLookaheadFixedSender)
import Ouroboros.Network.Channel (Channel)
import Ouroboros.Network.Driver.Limits (TraceSendRecv, driverWithLimits)
import Ouroboros.Network.Protocol.Limits
  ( ProtocolSizeLimits
  , ProtocolTimeLimits
  )
import Ouroboros.Network.Util.ShowProxy (ShowProxy)

leiosNotifyMiniProtocolNum :: Mux.MiniProtocolNum
leiosNotifyMiniProtocolNum = Mux.MiniProtocolNum 18

-- | The most LeiosNotify requests a client may leave outstanding.
--
-- TODO justify this bound.
leiosNotifyPipelineDepth :: Int
leiosNotifyPipelineDepth = 1000

-- | Run a lookahead (fixed-sender) peer with the given channel via the given codec.
--
-- The lookahead dual of 'runPipelinedPeerWithLimits': the peer receives ahead
-- and its sends are performed by a parallel thread, hence the 'MonadAsync'
-- constraint.
--
-- TODO: upstream this to ouroboros-network
runLookaheadFixedSenderPeerWithLimits ::
  forall ps (st :: ps) pr failure bytes m a.
  ( MonadAsync m
  , MonadEvaluate m
  , MonadFork m
  , MonadMask m
  , MonadTimer m
  , MonadThrow (STM m)
  , ShowProxy ps
  , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
  , NFData a
  , NFData failure
  , Show failure
  ) =>
  Tracer m (TraceSendRecv ps) ->
  Codec ps failure m bytes ->
  ProtocolSizeLimits ps bytes ->
  ProtocolTimeLimits ps ->
  Channel m bytes ->
  PeerLookaheadFixedSender ps pr st m a ->
  m (a, Maybe bytes)
runLookaheadFixedSenderPeerWithLimits tracer codec slimits tlimits channel peer =
  withTimeoutSerial $ \timeoutFn ->
    let driver = driverWithLimits tracer timeoutFn codec slimits tlimits channel
     in runLookaheadFixedSenderPeerWithDriver driver peer
