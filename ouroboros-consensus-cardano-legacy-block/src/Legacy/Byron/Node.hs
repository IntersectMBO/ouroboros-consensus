{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Legacy.Byron.Node () where

import           Cardano.Chain.Byron.API (ApplyMempoolPayloadErr)
import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Data.Coerce (coerce)
import           Ouroboros.Consensus.Legacy.Block (CodecConfig (..),
                     LegacyBlock)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (HasNetworkProtocolVersion (..))
import           Ouroboros.Consensus.Node.Serialisation
                     (SerialiseNodeToClient (..))
import           Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import           Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..),
                     EncodeDisk (..))

{-------------------------------------------------------------------------------
  EncodeDisk & DecodeDisk
-------------------------------------------------------------------------------}

instance EncodeDisk blk (PBftState c)
      => EncodeDisk (LegacyBlock blk) (PBftState c) where
  encodeDisk ::  CodecConfig (LegacyBlock blk) -> PBftState c -> Encoding
  encodeDisk ccfg = coerce $ encodeDisk @blk @(PBftState c) (coerce ccfg)

instance DecodeDisk blk (PBftState c)
      => DecodeDisk (LegacyBlock blk) (PBftState c) where
  decodeDisk :: CodecConfig (LegacyBlock blk) -> forall s. Decoder s (PBftState c)
  decodeDisk ccfg = coerce $ decodeDisk @blk @(PBftState c) (coerce ccfg)

{-------------------------------------------------------------------------------
  SerialiseNodeToClient
-------------------------------------------------------------------------------}

instance SerialiseNodeToClient blk ApplyMempoolPayloadErr
      => SerialiseNodeToClient (LegacyBlock blk) ApplyMempoolPayloadErr where
  encodeNodeToClient ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> ApplyMempoolPayloadErr -> Encoding
  encodeNodeToClient ccfg version = coerce $
      encodeNodeToClient @blk @ApplyMempoolPayloadErr (coerce ccfg) version

  decodeNodeToClient ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> forall s. Decoder s ApplyMempoolPayloadErr
  decodeNodeToClient ccfg version = coerce $
      decodeNodeToClient @blk @ApplyMempoolPayloadErr (coerce ccfg) version
