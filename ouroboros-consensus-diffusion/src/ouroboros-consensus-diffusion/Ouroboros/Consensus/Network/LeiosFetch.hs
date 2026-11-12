-- | Running LeiosFetch as a node-to-node mini-protocol.
module Ouroboros.Consensus.Network.LeiosFetch
  ( leiosFetchMiniProtocolNum
  , leiosFetchPipelineDepth
  , maxLeiosFetchIngressBytes
  ) where

import qualified Network.Mux.Types as Mux
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Config as Fetch

leiosFetchMiniProtocolNum :: Mux.MiniProtocolNum
leiosFetchMiniProtocolNum = Mux.MiniProtocolNum 19

-- | Internal physical request bound shared with admission, including zero-byte
-- jobs. Payload credit and body count independently constrain useful work.
leiosFetchPipelineDepth :: Int
leiosFetchPipelineDepth = Fetch.maxOutstandingRequests

-- | Transport ingress ceiling, also used by policy validation including CBOR
-- overhead and trusted body maxima. This is not a per-peer TX credit default.
maxLeiosFetchIngressBytes :: Int
maxLeiosFetchIngressBytes = fromIntegral Fetch.maxIngressBytes
