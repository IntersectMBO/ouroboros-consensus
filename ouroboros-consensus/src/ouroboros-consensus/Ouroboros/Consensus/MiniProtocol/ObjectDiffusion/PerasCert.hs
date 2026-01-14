-- | This module defines type aliases for the ObjectDiffusion protocol applied
-- to PerasCert diffusion.
module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.PerasCert
  ( TracePerasCertDiffusionInbound
  , TracePerasCertDiffusionOutbound
  , PerasCertPoolReader
  , PerasCertPoolWriter
  , PerasCertDiffusionInboundPipelined
  , PerasCertDiffusionOutbound
  , PerasCertDiffusion
  ) where

import Ouroboros.Consensus.Block
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Outbound
import Ouroboros.Consensus.Peras.Round (PerasRoundNo)
import Ouroboros.Consensus.Storage.PerasCertDB.API
import Ouroboros.Network.Protocol.ObjectDiffusion.Inbound (ObjectDiffusionInboundPipelined)
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound (ObjectDiffusionOutbound)
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (ObjectDiffusion)

type TracePerasCertDiffusionInbound blk =
  TraceObjectDiffusionInbound PerasRoundNo (PerasCert blk)

type TracePerasCertDiffusionOutbound blk =
  TraceObjectDiffusionOutbound PerasRoundNo (PerasCert blk)

type PerasCertPoolReader blk m =
  ObjectPoolReader PerasRoundNo (PerasCert blk) PerasCertTicketNo m

type PerasCertPoolWriter blk m =
  ObjectPoolWriter PerasRoundNo (PerasCert blk) m

type PerasCertDiffusionInboundPipelined blk m a =
  ObjectDiffusionInboundPipelined PerasRoundNo (PerasCert blk) m a

type PerasCertDiffusionOutbound blk m a =
  ObjectDiffusionOutbound PerasRoundNo (PerasCert blk) m a

type PerasCertDiffusion blk =
  ObjectDiffusion PerasRoundNo (PerasCert blk)
