-- | This module defines type aliases for the ObjectDiffusion protocol applied
-- to PerasVote diffusion.
module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.PerasVote
  ( TracePerasVoteDiffusionInbound
  , TracePerasVoteDiffusionOutbound
  , PerasVotePoolReader
  , PerasVotePoolWriter
  , PerasVoteDiffusionInboundPipelined
  , PerasVoteDiffusionOutbound
  , PerasVoteDiffusion
  , PerasVoteDiffusionInboundState
  , PerasVoteDiffusionInboundHandle
  , PerasVoteDiffusionInboundHandleCollection
  ) where

import Ouroboros.Consensus.Block
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V1
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V1.State
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Outbound
import Ouroboros.Consensus.Storage.PerasVoteDB.API
import Ouroboros.Network.Protocol.ObjectDiffusion.Inbound (ObjectDiffusionInboundPipelined)
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound (ObjectDiffusionOutbound)
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (ObjectDiffusion)

type TracePerasVoteDiffusionInbound blk =
  TraceObjectDiffusionInbound PerasRoundNo (PerasVote blk)

type TracePerasVoteDiffusionOutbound blk =
  TraceObjectDiffusionOutbound PerasRoundNo (PerasVote blk)

type PerasVotePoolReader blk m =
  ObjectPoolReader PerasRoundNo (PerasVote blk) PerasVoteTicketNo m

type PerasVotePoolWriter blk m =
  ObjectPoolWriter PerasRoundNo (PerasVote blk) m

type PerasVoteDiffusionInboundPipelined blk m a =
  ObjectDiffusionInboundPipelined PerasRoundNo (PerasVote blk) m a

type PerasVoteDiffusionOutbound blk m a =
  ObjectDiffusionOutbound PerasRoundNo (PerasVote blk) m a

type PerasVoteDiffusion blk =
  ObjectDiffusion PerasRoundNo (PerasVote blk)

type PerasVoteDiffusionInboundState blk =
  ObjectDiffusionInboundState blk

type PerasVoteDiffusionInboundHandle m blk =
  ObjectDiffusionInboundHandle m blk

type PerasVoteDiffusionInboundHandleCollection peer m blk =
  ObjectDiffusionInboundHandleCollection peer m blk
