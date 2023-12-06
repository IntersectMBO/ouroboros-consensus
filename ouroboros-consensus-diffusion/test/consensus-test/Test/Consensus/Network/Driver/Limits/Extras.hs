{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Test.Consensus.Network.Driver.Limits.Extras (
    chainSyncNoSizeLimits
  , chainSyncNoTimeouts
  , chainSyncTimeouts
  , runConnectedPeersPipelinedWithLimits
  ) where

import           Cardano.Slotting.Time (SlotLength, getSlotLength)
import           Control.Monad.Class.MonadTimer.SI (MonadTimer)
import           Control.Tracer (Contravariant (contramap), Tracer)
import           Data.Time.Clock (secondsToDiffTime)
import qualified Network.TypedProtocol as TP
import qualified Network.TypedProtocol.Codec as TP
import           Ouroboros.Consensus.Util (ShowProxy)
import           Ouroboros.Consensus.Util.IOLike (DiffTime,
                     MonadAsync (concurrently), MonadFork, MonadMask,
                     MonadSTM (STM), MonadThrow)
import           Ouroboros.Network.Channel (Channel)
import           Ouroboros.Network.Driver (TraceSendRecv, runPeer)
import           Ouroboros.Network.Driver.Limits (runPipelinedPeerWithLimits)
import           Ouroboros.Network.Driver.Simple (Role (Client, Server))
import           Ouroboros.Network.Protocol.ChainSync.Codec
                     (ChainSyncTimeout (..), byteLimitsChainSync)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync,
                     ClientHasAgency, ServerHasAgency)
import           Ouroboros.Network.Protocol.Limits (ProtocolSizeLimits (..),
                     ProtocolTimeLimits (..), shortWait)
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Asc, ascVal)
import           Test.Util.Orphans.IOLike ()

-- | Same as 'runConnectedPeersPipelined' except the client peer is ran not with
-- 'runPipelinedPeer' but with 'runPipelinedPeerWithLimits'.
runConnectedPeersPipelinedWithLimits ::
  ( MonadAsync m
  , MonadFork m
  , MonadMask m
  , MonadThrow (STM m)
  , MonadTimer m
  , Show failure
  , forall (st' :: ps). Show (ClientHasAgency st')
  , forall (st' :: ps). Show (ServerHasAgency st')
  , ShowProxy ps
  ) =>
  m (Channel m bytes, Channel m bytes) ->
  Tracer m (Role, TraceSendRecv ps) ->
  TP.Codec ps failure m bytes ->
  ProtocolSizeLimits ps bytes ->
  ProtocolTimeLimits ps ->
  TP.PeerPipelined ps pr st m a ->
  TP.Peer ps (TP.FlipAgency pr) st m b ->
  m (a, b)
runConnectedPeersPipelinedWithLimits createChannels tracer codec sizeLimits timeLimits client server =
    createChannels >>= \(clientChannel, serverChannel) ->
    (fst <$> runPipelinedPeerWithLimits tracerClient codec sizeLimits timeLimits clientChannel client)
      `concurrently`
    (fst <$> runPeer tracerServer codec serverChannel server)
  where
    tracerClient = contramap ((,) Client) tracer
    tracerServer = contramap ((,) Server) tracer

chainSyncNoSizeLimits :: ProtocolSizeLimits (ChainSync header point tip) bytes
chainSyncNoSizeLimits = byteLimitsChainSync (const 0)

chainSyncTimeouts ::
  SlotLength ->
  Asc ->
  ChainSyncTimeout
chainSyncTimeouts t f =
  ChainSyncTimeout{
      canAwaitTimeout
    , intersectTimeout
    , mustReplyTimeout
    , idleTimeout
    }
  where
    canAwaitTimeout :: Maybe DiffTime
    canAwaitTimeout = shortWait -- REVIEW: what is this exactly?

    intersectTimeout :: Maybe DiffTime
    intersectTimeout = shortWait -- REVIEW: what is this exactly?

    -- | The following timeout is derived from the average length of a streak of
    -- empty slots. If the probability of the election of a leader is @f@ and
    -- @Y@ is a probability, then a streak of empty slots will be shorter than
    -- @log (1 - Y) / log (1 - f)@ with probability @Y@. Main net nodes pick a
    -- random value for @Y@ between 99.9% and 99.999%. For our use case, we
    -- choose the tightest bound of 99.9%.
    mustReplyTimeout :: Maybe DiffTime
    mustReplyTimeout = Just $ secondsToDiffTime $ round $
      realToFrac (getSlotLength t)
        * log (1 - 0.999) / log (1 - ascVal f)

    idleTimeout :: Maybe DiffTime
    idleTimeout = Just 3673       -- taken from Ouroboros.Consensus.Node.stdChainSyncTimeout

chainSyncNoTimeouts :: ChainSyncTimeout
chainSyncNoTimeouts =
  ChainSyncTimeout {
      canAwaitTimeout = Nothing
    , intersectTimeout = Nothing
    , mustReplyTimeout = Nothing
    , idleTimeout = Nothing
  }
