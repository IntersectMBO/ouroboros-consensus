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
import Ouroboros.Consensus.Peras.Types (PerasVoteId)
import Ouroboros.Consensus.Storage.PerasVoteDB.API
import Ouroboros.Network.Protocol.ObjectDiffusion.Inbound (ObjectDiffusionInboundPipelined)
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound (ObjectDiffusionOutbound)
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (ObjectDiffusion)

type TracePerasVoteDiffusionInbound blk =
  TraceObjectDiffusionInbound (PerasVoteId blk) (PerasVote blk)

type TracePerasVoteDiffusionOutbound blk =
  TraceObjectDiffusionOutbound (PerasVoteId blk) (PerasVote blk)

type PerasVotePoolReader blk m =
  ObjectPoolReader (PerasVoteId blk) (PerasVote blk) PerasVoteTicketNo m

type PerasVotePoolWriter blk m =
  ObjectPoolWriter (PerasVoteId blk) (PerasVote blk) m

type PerasVoteDiffusionInboundPipelined blk m a =
  ObjectDiffusionInboundPipelined (PerasVoteId blk) (PerasVote blk) m a

type PerasVoteDiffusionOutbound blk m a =
  ObjectDiffusionOutbound (PerasVoteId blk) (PerasVote blk) m a

type PerasVoteDiffusion blk =
  ObjectDiffusion (PerasVoteId blk) (PerasVote blk)
