{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Serialisation for sending things across the network.
--
-- We separate @NodeToNode@ from @NodeToClient@ to be very explicit about what
-- gets sent where.
--
-- Unlike in "Ouroboros.Consensus.Storage.Serialisation", we don't separate the
-- encoder from the decoder, because the reasons don't apply: we always need
-- both directions and we don't have access to the bytestrings that could be
-- used for the annotations (we use CBOR-in-CBOR in those cases).
module Ouroboros.Consensus.Node.Serialisation
  ( SerialiseBlockQueryResult (..)
  , SerialiseNodeToClient (..)
  , SerialiseNodeToNode (..)
  , SerialiseResult (..)

    -- * Defaults
  , defaultDecodeCBORinCBOR
  , defaultEncodeCBORinCBOR

    -- * Re-exported for convenience
  , Some (..)
  ) where

import qualified Cardano.Binary as KeyHash
import Codec.CBOR.Decoding (Decoder, decodeListLenOf)
import Codec.CBOR.Encoding (Encoding, encodeListLen)
import Codec.Serialise (Serialise (decode, encode))
import Data.Kind
import Data.SOP.BasicFunctors
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsMempool
  ( ApplyTxErr
  , GenTxId
  )
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util (Some (..))
import Ouroboros.Network.Block
  ( Tip
  , decodePoint
  , decodeTip
  , encodePoint
  , encodeTip
  , unwrapCBORinCBOR
  , wrapCBORinCBOR
  )

{-------------------------------------------------------------------------------
  NodeToNode
-------------------------------------------------------------------------------}

-- | Serialise a type @a@ so that it can be sent across network via a
-- node-to-node protocol.
class SerialiseNodeToNode blk a where
  encodeNodeToNode :: CodecConfig blk -> BlockNodeToNodeVersion blk -> a -> Encoding
  decodeNodeToNode :: CodecConfig blk -> BlockNodeToNodeVersion blk -> forall s. Decoder s a

  -- When the config is not needed, we provide a default, unversioned
  -- implementation using 'Serialise'

  default encodeNodeToNode ::
    Serialise a =>
    CodecConfig blk -> BlockNodeToNodeVersion blk -> a -> Encoding
  encodeNodeToNode _ccfg _version = encode

  default decodeNodeToNode ::
    Serialise a =>
    CodecConfig blk -> BlockNodeToNodeVersion blk -> forall s. Decoder s a
  decodeNodeToNode _ccfg _version = decode

{-------------------------------------------------------------------------------
  NodeToClient
-------------------------------------------------------------------------------}

-- | Serialise a type @a@ so that it can be sent across the network via
-- node-to-client protocol.
class SerialiseNodeToClient blk a where
  encodeNodeToClient :: CodecConfig blk -> BlockNodeToClientVersion blk -> a -> Encoding
  decodeNodeToClient :: CodecConfig blk -> BlockNodeToClientVersion blk -> forall s. Decoder s a

  -- When the config is not needed, we provide a default, unversioned
  -- implementation using 'Serialise'

  default encodeNodeToClient ::
    Serialise a =>
    CodecConfig blk -> BlockNodeToClientVersion blk -> a -> Encoding
  encodeNodeToClient _ccfg _version = encode

  default decodeNodeToClient ::
    Serialise a =>
    CodecConfig blk -> BlockNodeToClientVersion blk -> forall s. Decoder s a
  decodeNodeToClient _ccfg _version = decode

{-------------------------------------------------------------------------------
  NodeToClient - SerialiseResult
-------------------------------------------------------------------------------}

-- | How to serialise the @result@ of a query.
--
-- The @LocalStateQuery@ protocol is a node-to-client protocol, hence the
-- 'NodeToClientVersion' argument.
type SerialiseResult :: Type -> (Type -> Type -> Type) -> Constraint
class SerialiseResult blk query where
  encodeResult ::
    forall result.
    CodecConfig blk ->
    BlockNodeToClientVersion blk ->
    query blk result ->
    result ->
    Encoding
  decodeResult ::
    forall result.
    CodecConfig blk ->
    BlockNodeToClientVersion blk ->
    query blk result ->
    forall s.
    Decoder s result

-- | How to serialise the @result@ of a block query.
--
-- The @LocalStateQuery@ protocol is a node-to-client protocol, hence the
-- 'NodeToClientVersion' argument.
type SerialiseBlockQueryResult :: Type -> (Type -> k -> Type -> Type) -> Constraint
class SerialiseBlockQueryResult blk query where
  encodeBlockQueryResult ::
    forall fp result.
    CodecConfig blk ->
    BlockNodeToClientVersion blk ->
    query blk fp result ->
    result ->
    Encoding
  decodeBlockQueryResult ::
    forall fp result.
    CodecConfig blk ->
    BlockNodeToClientVersion blk ->
    query blk fp result ->
    forall s.
    Decoder s result

{-------------------------------------------------------------------------------
  Defaults
-------------------------------------------------------------------------------}

-- | Uses the 'Serialise' instance, but wraps it in CBOR-in-CBOR.
--
-- Use this for the 'SerialiseNodeToNode' and/or 'SerialiseNodeToClient'
-- instance of @blk@ and/or @'Header' blk@, which require CBOR-in-CBOR to be
-- compatible with the corresponding 'Serialised' instance.
defaultEncodeCBORinCBOR :: Serialise a => a -> Encoding
defaultEncodeCBORinCBOR = wrapCBORinCBOR encode

-- | Inverse of 'defaultEncodeCBORinCBOR'
defaultDecodeCBORinCBOR :: Serialise a => Decoder s a
defaultDecodeCBORinCBOR = unwrapCBORinCBOR (const <$> decode)

{-------------------------------------------------------------------------------
  Forwarding instances
-------------------------------------------------------------------------------}

deriving newtype instance
  SerialiseNodeToNode blk blk =>
  SerialiseNodeToNode blk (I blk)

deriving newtype instance
  SerialiseNodeToClient blk blk =>
  SerialiseNodeToClient blk (I blk)

deriving newtype instance
  SerialiseNodeToNode blk (GenTxId blk) =>
  SerialiseNodeToNode blk (WrapGenTxId blk)

instance ConvertRawHash blk => SerialiseNodeToNode blk (Point blk) where
  encodeNodeToNode _ccfg _version = encodePoint $ encodeRawHash (Proxy @blk)
  decodeNodeToNode _ccfg _version = decodePoint $ decodeRawHash (Proxy @blk)

instance ConvertRawHash blk => SerialiseNodeToNode blk (Tip blk) where
  encodeNodeToNode _ccfg _version = encodeTip $ encodeRawHash (Proxy @blk)
  decodeNodeToNode _ccfg _version = decodeTip $ decodeRawHash (Proxy @blk)

instance SerialiseNodeToNode blk PerasRoundNo where
  encodeNodeToNode _ccfg _version = encode
  decodeNodeToNode _ccfg _version = decode

instance ConvertRawHash blk => SerialiseNodeToNode blk (PerasCert blk) where
  -- Consistent with the 'Serialise' instance for 'PerasCert' defined in Ouroboros.Consensus.Block.SupportsPeras
  encodeNodeToNode ccfg version PerasCert{..} =
    encodeListLen 2
      <> encodeNodeToNode ccfg version pcCertRound
      <> encodeNodeToNode ccfg version pcCertBoostedBlock
  decodeNodeToNode ccfg version = do
    decodeListLenOf 2
    pcCertRound <- decodeNodeToNode ccfg version
    pcCertBoostedBlock <- decodeNodeToNode ccfg version
    pure $ PerasCert pcCertRound pcCertBoostedBlock

instance ConvertRawHash blk => SerialiseNodeToNode blk (PerasVote blk) where
  -- Consistent with the 'Serialise' instance for 'PerasVote' defined in Ouroboros.Consensus.Block.SupportsPeras
  encodeNodeToNode ccfg version PerasVote{..} =
    encodeListLen 3
      <> encodeNodeToNode ccfg version pvVoteRound
      <> encodeNodeToNode ccfg version pvVoteBlock
      <> encodeNodeToNode ccfg version pvVoteVoterId
  decodeNodeToNode ccfg version = do
    decodeListLenOf 3
    pvVoteRound <- decodeNodeToNode ccfg version
    pvVoteBlock <- decodeNodeToNode ccfg version
    pvVoteVoterId <- decodeNodeToNode ccfg version
    pure $ PerasVote pvVoteRound pvVoteBlock pvVoteVoterId

instance SerialiseNodeToNode blk PerasVoterId where
  encodeNodeToNode _ccfg _version = KeyHash.toCBOR . unPerasVoterId
  decodeNodeToNode _ccfg _version = PerasVoterId <$> KeyHash.fromCBOR

deriving newtype instance
  SerialiseNodeToClient blk (GenTxId blk) =>
  SerialiseNodeToClient blk (WrapGenTxId blk)

deriving newtype instance
  SerialiseNodeToClient blk (ApplyTxErr blk) =>
  SerialiseNodeToClient blk (WrapApplyTxErr blk)

deriving newtype instance
  SerialiseNodeToClient blk (LedgerConfig blk) =>
  SerialiseNodeToClient blk (WrapLedgerConfig blk)
