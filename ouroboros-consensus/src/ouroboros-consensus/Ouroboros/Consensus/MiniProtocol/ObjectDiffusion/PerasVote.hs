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
  ) where

import Ouroboros.Consensus.Block
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Outbound
import Ouroboros.Consensus.Peras.Round (PerasRoundNo)
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
