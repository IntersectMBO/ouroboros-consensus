{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Peras.Cert.Opaque
  ( OpaquePerasCertError (..)
  , OpaquePerasCert (..)
  , toOpaquePerasCert
  , fromOpaquePerasCert
  ) where

import Cardano.Binary (Decoder, FromCBOR (..))
import Cardano.Ledger.Binary (ToCBOR (..))
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
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.SupportsPeras (BlockSupportsPeras (..))
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1

-- * Opaque Peras certificates to be stored in blocks

-- | Error type for opaque Peras certificate operations.
newtype OpaquePerasCertError = OpaquePerasCertError String
  deriving (Eq, Show, Generic, NoThunks)

-- | Opaque Peras certificates to be stored in blocks.
newtype OpaquePerasCert = OpaquePerasCert {unOpaquePerasCert :: ByteArray}
  deriving (Eq, Show, Generic, NoThunks)

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
        OpaquePerasCert (toByteArray (toCBOR cert))
  | otherwise =
      Left $
        OpaquePerasCertError $
          "Unsupported PerasCert type for OpaquePerasCert: "
            <> show (typeRep (Proxy @(PerasCert blk)))
 where
  toByteArray ::
    CBOR.Encoding ->
    ByteArray
  toByteArray =
    byteArrayFromShortByteString
      . ShortByteString.toShort
      . CBOR.toStrictByteString

-- | Extract a 'PerasCert' from an 'OpaquePerasCert'.
fromOpaquePerasCert ::
  ( Typeable blk
  , Typeable (PerasCert blk)
  ) =>
  Proxy blk ->
  OpaquePerasCert ->
  Either OpaquePerasCertError (PerasCert blk)
fromOpaquePerasCert (Proxy :: Proxy blk) (OpaquePerasCert byteArray)
  | Just Refl <- eqT @(PerasCert blk) @(V1.PerasCert blk) =
      fromByteArray fromCBOR byteArray
  | otherwise =
      Left $
        OpaquePerasCertError $
          "Unsupported PerasCert type for OpaquePerasCert: "
            <> show (typeRep (Proxy @(PerasCert blk)))
 where
  fromByteArray ::
    (forall s. Decoder s (PerasCert blk)) ->
    ByteArray ->
    Either OpaquePerasCertError (PerasCert blk)
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
