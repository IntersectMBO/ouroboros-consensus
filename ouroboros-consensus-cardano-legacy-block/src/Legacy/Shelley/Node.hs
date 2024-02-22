{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Legacy.Shelley.Node () where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Data.Coerce (coerce)
import           Ouroboros.Consensus.Legacy.Block (CodecConfig (..),
                     LegacyBlock)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (HasNetworkProtocolVersion (..))
import           Ouroboros.Consensus.Node.Serialisation
                     (SerialiseNodeToClient (..))
import           Ouroboros.Consensus.Protocol.Praos (PraosState)
import           Ouroboros.Consensus.Protocol.TPraos (TPraosState)
import           Ouroboros.Consensus.Shelley.Ledger (ApplyTxError)
import           Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..),
                     EncodeDisk (..))

{-------------------------------------------------------------------------------
  EncodeDisk & DecodeDisk
-------------------------------------------------------------------------------}

instance EncodeDisk blk (TPraosState c)
      => EncodeDisk (LegacyBlock blk) (TPraosState c) where
  encodeDisk :: CodecConfig (LegacyBlock blk) -> TPraosState c -> Encoding
  encodeDisk ccfg = coerce $ encodeDisk @blk @(TPraosState c) (coerce ccfg)

instance DecodeDisk blk (TPraosState c)
      => DecodeDisk (LegacyBlock blk) (TPraosState c) where
  decodeDisk ::
       CodecConfig (LegacyBlock blk)
    -> forall s. Decoder s (TPraosState c)
  decodeDisk ccfg = coerce $ decodeDisk @blk @(TPraosState c) (coerce ccfg)

instance EncodeDisk blk (PraosState c)
      => EncodeDisk (LegacyBlock blk) (PraosState c) where
  encodeDisk :: CodecConfig (LegacyBlock blk) -> PraosState c -> Encoding
  encodeDisk ccfg = coerce $ encodeDisk @blk @(PraosState c) (coerce ccfg)

instance DecodeDisk blk (PraosState c)
      => DecodeDisk (LegacyBlock blk) (PraosState c) where
  decodeDisk ::
       CodecConfig (LegacyBlock blk)
    -> forall s. Decoder s (PraosState c)
  decodeDisk ccfg = coerce $ decodeDisk @blk @(PraosState c) (coerce ccfg)

{-------------------------------------------------------------------------------
  SerialiseNodeToClient
-------------------------------------------------------------------------------}

instance SerialiseNodeToClient blk (ApplyTxError era)
      => SerialiseNodeToClient
           (LegacyBlock blk)
           (ApplyTxError era) where
  encodeNodeToClient ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> ApplyTxError era -> Encoding
  encodeNodeToClient ccfg version = coerce $
      encodeNodeToClient @blk @(ApplyTxError era) (coerce ccfg) version

  decodeNodeToClient ::
       CodecConfig (LegacyBlock blk)
    -> BlockNodeToClientVersion (LegacyBlock blk)
    -> forall s. Decoder s (ApplyTxError era)
  decodeNodeToClient ccfg version = coerce $
      decodeNodeToClient @blk @(ApplyTxError era) (coerce ccfg) version
