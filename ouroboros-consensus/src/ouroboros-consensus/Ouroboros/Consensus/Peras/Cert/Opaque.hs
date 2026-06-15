{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Peras.Cert.Opaque
  ( OpaquePerasCert
  , toOpaquePerasCert
  , fromOpaquePerasCert
  , opaquePerasCertToByteArray
  , opaquePerasCertFromByteArray
  ) where

import Cardano.Binary (Decoder, FromCBOR (..))
import Cardano.Ledger.Binary (ToCBOR (..))
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import Data.Array.Byte (ByteArray)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Lazy as LazyBytesString
import qualified Data.ByteString.Short as ShortByteString
import Data.MemPack.Buffer
  ( byteArrayFromShortByteString
  , byteArrayToShortByteString
  )
import Data.Proxy (Proxy)
import Data.Typeable (Proxy (..), Typeable, eqT, typeRep, (:~:) (..))
import GHC.Base (Any)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.SupportsPeras (BlockSupportsPeras (..))
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1

-- * Opaque Peras certificates to be stored in blocks

-- | Supported versions of Peras certificates that can be stored in blocks.
data OpaquePerasCertVersion = V1
  deriving (Eq, Show, Generic, NoThunks)

instance ToCBOR OpaquePerasCertVersion where
  toCBOR = \case
    V1 -> CBOR.encodeWord8 1

instance FromCBOR OpaquePerasCertVersion where
  fromCBOR = do
    CBOR.decodeWord8 >>= \case
      1 -> pure V1
      version ->
        fail $ "Unknown OpaquePerasCertVersion: " <> show version

-- | Error type for unsupported Peras certificate versions.
newtype OpaquePerasCertError = OpaquePerasCertError String
  deriving (Eq, Show, Generic, NoThunks)

-- | Existential wrapper for Peras certificates to be stored in blocks.
--
-- This allows us to have blocks that contain a potentially different type of
-- Peras certificate than the one used in the current era.
data OpaquePerasCert where
  OpaquePerasCertV1 :: V1.PerasCert Any -> OpaquePerasCert

instance ToCBOR OpaquePerasCert where
  toCBOR = \case
    OpaquePerasCertV1 (cert :: V1.PerasCert blk) ->
      CBOR.encodeListLen 2
        <> toCBOR V1
        <> toCBOR (V1.retagPerasCert @() cert)

instance FromCBOR OpaquePerasCert where
  fromCBOR = do
    CBOR.decodeListLenOf 2
    version <- fromCBOR
    case version of
      V1 -> do
        cert <- fromCBOR @(V1.PerasCert ())
        pure (OpaquePerasCertV1 (V1.retagPerasCert cert))

-- | Create an 'OpaquePerasCert' from a given 'PerasCert'.
toOpaquePerasCert ::
  ( Typeable blk
  , Typeable (PerasCert blk)
  ) =>
  PerasCert blk ->
  Either OpaquePerasCertError OpaquePerasCert
toOpaquePerasCert (cert :: PerasCert blk)
  | Just Refl <- eqT @(PerasCert blk) @(V1.PerasCert blk) =
      Right $
        OpaquePerasCertV1 $
          V1.retagPerasCert cert
  | otherwise =
      Left $
        OpaquePerasCertError $
          "Unsupported PerasCert type for OpaquePerasCert: "
            <> show (typeRep (Proxy @(PerasCert blk)))

-- | Extract a 'PerasCert' from an 'OpaquePerasCert'.
fromOpaquePerasCert ::
  ( Typeable blk
  , Typeable (PerasCert blk)
  ) =>
  Proxy blk ->
  OpaquePerasCert ->
  Either OpaquePerasCertError (PerasCert blk)
fromOpaquePerasCert (Proxy :: Proxy blk) = \case
  OpaquePerasCertV1 cert
    | Just Refl <- eqT @(PerasCert blk) @(V1.PerasCert blk) ->
        Right $
          V1.retagPerasCert cert
    | otherwise ->
        Left $
          OpaquePerasCertError $
            "OpaquePerasCert version does not match expected PerasCert type: "
              <> show (typeRep (Proxy @(PerasCert blk)))

-- | Serialise an opaque Peras certificate into a byte array.
opaquePerasCertToByteArray ::
  OpaquePerasCert ->
  ByteArray
opaquePerasCertToByteArray =
  toByteArray . toCBOR
 where
  toByteArray ::
    CBOR.Encoding ->
    ByteArray
  toByteArray =
    byteArrayFromShortByteString
      . ShortByteString.toShort
      . CBOR.toStrictByteString

-- | Deserialize an opaque Peras certificate from a byte array.
opaquePerasCertFromByteArray ::
  ByteArray ->
  Either OpaquePerasCertError OpaquePerasCert
opaquePerasCertFromByteArray =
  fromByteArray fromCBOR
 where
  fromByteArray ::
    (forall s. Decoder s OpaquePerasCert) ->
    ByteArray ->
    Either OpaquePerasCertError OpaquePerasCert
  fromByteArray decoder =
    handleParseErrors
      . CBOR.deserialiseFromBytes decoder
      . LazyBytesString.fromStrict
      . ShortByteString.fromShort
      . byteArrayToShortByteString

  handleParseErrors ::
    Either CBOR.DeserialiseFailure (ByteString, a) ->
    Either OpaquePerasCertError a
  handleParseErrors = \case
    Left err -> failure err
    Right (trailing, a)
      | not (LazyByteString.null trailing) -> failure "trailing bytes"
      | otherwise -> pure a
   where
    failure err =
      Left $
        OpaquePerasCertError $
          "Failed to deserialize opaque Peras certificate from byte array: "
            <> show err
