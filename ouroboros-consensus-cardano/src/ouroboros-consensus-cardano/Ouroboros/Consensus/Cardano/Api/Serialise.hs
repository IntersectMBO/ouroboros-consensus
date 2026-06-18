{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | A minimal, Consensus-native subset of @cardano-api@'s serialisation
-- machinery, sufficient for the db-tools to read forging credentials and
-- operational certificates from key and certificate files.
--
-- This provides the 'HasTypeProxy' \/ 'AsType' singleton scheme, raw-bytes and
-- CBOR (de)serialisation classes, the 'TextEnvelope' file format reader and the
-- @deriving via@ helpers used by the key types in
-- "Ouroboros.Consensus.Cardano.Api.Keys".
module Ouroboros.Consensus.Cardano.Api.Serialise
  ( -- * Type proxies
    HasTypeProxy (..)
  , AsType (..)

    -- * Hashes
  , Hash

    -- * Raw bytes serialisation
  , SerialiseAsRawBytes (..)
  , serialiseToRawBytesHex
  , serialiseToRawBytesHexText

    -- * CBOR serialisation
  , SerialiseAsCBOR (..)

    -- * Errors
  , Error (..)
  , FileError (..)

    -- * Text envelopes
  , TextEnvelope (..)
  , TextEnvelopeDescr (..)
  , TextEnvelopeError (..)
  , TextEnvelopeType (..)
  , HasTextEnvelope (..)
  , deserialiseFromTextEnvelope
  , readFileTextEnvelope

    -- * @deriving via@ helpers
  , UsingRawBytes (..)
  , UsingRawBytesHex (..)

    -- * Re-exports
  , module Cbor
  , module Proxy
  ) where

import Cardano.Ledger.Binary as Cbor
  ( DecCBOR (..)
  , EncCBOR (..)
  , FromCBOR (..)
  , ToCBOR (..)
  )
import Cardano.Ledger.Binary (DecoderError, fromPlainDecoder)
import qualified Cardano.Ledger.Binary.Plain as CBOR
import Control.Exception (Exception (..), IOException)
import Control.Monad (unless)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Except.Extra
  ( firstExceptT
  , handleIOExceptT
  , hoistEither
  )
import Data.Aeson as Aeson
  ( FromJSON (..)
  , eitherDecodeStrict'
  , withObject
  , (.:)
  )
import Data.Aeson.Types
  ( FromJSONKey
  , ToJSON (..)
  , ToJSONKey
  )
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16 (decode, encode)
import qualified Data.ByteString.Char8 as BSC
import Data.Kind (Type)
import Data.Proxy as Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text as Text (Text)
import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)
import Data.Typeable (Typeable, tyConName, typeRep, typeRepTyCon)

-- ----------------------------------------------------------------------------
-- Type proxies
--

-- DUPLICATE -- adapted from: cardano-api/src/Cardano/Api/HasTypeProxy.hs

class HasTypeProxy t where
  -- | A family of singleton types used in this API to indicate which type to
  -- use where it would otherwise be ambiguous or merely unclear.
  --
  -- Values of this type are passed to deserialisation functions for example.
  data AsType t

  proxyToAsType :: Proxy t -> AsType t

-- ----------------------------------------------------------------------------
-- Hashes
--

-- DUPLICATE -- adapted from: cardano-api/src/Cardano/Api/Hash.hs

data family Hash keyrole :: Type

instance HasTypeProxy a => HasTypeProxy (Hash a) where
  data AsType (Hash a) = AsHash (AsType a)
  proxyToAsType _ = AsHash (proxyToAsType (Proxy :: Proxy a))

-- ----------------------------------------------------------------------------
-- Raw bytes serialisation
--

-- DUPLICATE -- adapted from: cardano-api/src/Cardano/Api/SerialiseRaw.hs

class HasTypeProxy a => SerialiseAsRawBytes a where
  serialiseToRawBytes :: a -> ByteString

  deserialiseFromRawBytes :: AsType a -> ByteString -> Maybe a

serialiseToRawBytesHex :: SerialiseAsRawBytes a => a -> ByteString
serialiseToRawBytesHex = Base16.encode . serialiseToRawBytes

serialiseToRawBytesHexText :: SerialiseAsRawBytes a => a -> Text
serialiseToRawBytesHexText = Text.decodeUtf8 . serialiseToRawBytesHex

-- ----------------------------------------------------------------------------
-- CBOR serialisation
--

-- DUPLICATE -- adapted from: cardano-api/src/Cardano/Api/SerialiseAsCBOR.hs

class HasTypeProxy a => SerialiseAsCBOR a where
  serialiseToCBOR :: a -> ByteString
  deserialiseFromCBOR :: AsType a -> ByteString -> Either CBOR.DecoderError a

  default serialiseToCBOR :: ToCBOR a => a -> ByteString
  serialiseToCBOR = CBOR.serialize'

  default deserialiseFromCBOR ::
    FromCBOR a =>
    AsType a ->
    ByteString ->
    Either CBOR.DecoderError a
  deserialiseFromCBOR _proxy = CBOR.decodeFull'

-- ----------------------------------------------------------------------------
-- Errors
--

-- DUPLICATE -- adapted from: cardano-api/src/Cardano/Api/Error.hs

class Show e => Error e where
  displayError :: e -> String

data FileError e
  = FileError FilePath e
  | FileIOError FilePath IOException
  deriving Show

instance Error e => Error (FileError e) where
  displayError (FileIOError path ioe) =
    path ++ ": " ++ displayException ioe
  displayError (FileError path e) =
    path ++ ": " ++ displayError e

-- ----------------------------------------------------------------------------
-- Text envelopes
--

-- DUPLICATE -- adapted from: cardano-api/src/Cardano/Api/Serialise/TextEnvelope.hs

newtype TextEnvelopeType = TextEnvelopeType String
  deriving (Eq, Show)
  deriving newtype (IsString, Semigroup, FromJSON)

newtype TextEnvelopeDescr = TextEnvelopeDescr String
  deriving (Eq, Show)
  deriving newtype (IsString, Semigroup, FromJSON)

-- | A 'TextEnvelope' is a structured envelope for serialised binary values
-- with an external format with a semi-readable textual format.
--
-- It contains a \"type\" field, e.g. \"PublicKeyByron\" or \"TxSignedShelley\"
-- to indicate the type of the encoded data. This is used as a sanity check
-- and to help readers.
--
-- It also contains a \"title\" field which is free-form, and could be used
-- to indicate the role or purpose to a reader.
data TextEnvelope = TextEnvelope
  { teType :: !TextEnvelopeType
  , teDescription :: !TextEnvelopeDescr
  , teRawCBOR :: !ByteString
  }
  deriving (Eq, Show)

instance HasTypeProxy TextEnvelope where
  data AsType TextEnvelope = AsTextEnvelope
  proxyToAsType _ = AsTextEnvelope

instance FromJSON TextEnvelope where
  parseJSON = withObject "TextEnvelope" $ \v ->
    TextEnvelope
      <$> (v .: "type")
      <*> (v .: "description")
      <*> (parseJSONBase16 =<< v .: "cborHex")
   where
    parseJSONBase16 v =
      either fail return . Base16.decode . Text.encodeUtf8 =<< parseJSON v

-- | The errors that the pure 'TextEnvelope' parsing\/decoding functions can return.
data TextEnvelopeError
  = -- | expected, actual
    TextEnvelopeTypeError ![TextEnvelopeType] !TextEnvelopeType
  | TextEnvelopeDecodeError !DecoderError
  | TextEnvelopeAesonDecodeError !String
  deriving (Eq, Show)

instance Error TextEnvelopeError where
  displayError tee =
    case tee of
      TextEnvelopeTypeError [TextEnvelopeType expType] (TextEnvelopeType actType) ->
        "TextEnvelope type error: "
          <> " Expected: "
          <> expType
          <> " Actual: "
          <> actType
      TextEnvelopeTypeError expTypes (TextEnvelopeType actType) ->
        "TextEnvelope type error: "
          <> " Expected one of: "
          <> mconcat [expType <> ", " | TextEnvelopeType expType <- expTypes]
          <> " Actual: "
          <> actType
      TextEnvelopeAesonDecodeError decErr -> "TextEnvelope aeson decode error: " <> decErr
      TextEnvelopeDecodeError decErr -> "TextEnvelope decode error: " <> show decErr

-- | Check that the \"type\" of the 'TextEnvelope' is as expected.
--
-- For example, one might check that the type is \"TxSignedShelley\".
expectTextEnvelopeOfType :: TextEnvelopeType -> TextEnvelope -> Either TextEnvelopeError ()
expectTextEnvelopeOfType expectedType TextEnvelope{teType = actualType} =
  unless (expectedType == actualType) $
    Left (TextEnvelopeTypeError [expectedType] actualType)

class SerialiseAsCBOR a => HasTextEnvelope a where
  textEnvelopeType :: AsType a -> TextEnvelopeType

deserialiseFromTextEnvelope ::
  HasTextEnvelope a =>
  AsType a ->
  TextEnvelope ->
  Either TextEnvelopeError a
deserialiseFromTextEnvelope ttoken te = do
  expectTextEnvelopeOfType (textEnvelopeType ttoken) te
  first TextEnvelopeDecodeError $
    deserialiseFromCBOR ttoken (teRawCBOR te)

readFileTextEnvelope ::
  HasTextEnvelope a =>
  AsType a ->
  FilePath ->
  IO (Either (FileError TextEnvelopeError) a)
readFileTextEnvelope ttoken path =
  runExceptT $ do
    content <- handleIOExceptT (FileIOError path) $ BS.readFile path
    firstExceptT (FileError path) $ hoistEither $ do
      te <- first TextEnvelopeAesonDecodeError $ Aeson.eitherDecodeStrict' content
      deserialiseFromTextEnvelope ttoken te

-- ----------------------------------------------------------------------------
-- deriving via helpers
--

-- DUPLICATE -- adapted from: cardano-api/src/Cardano/Api/Serialise/Raw.hs (SerialiseUsing)

-- | For use with @deriving via@, to provide 'ToCBOR' and 'FromCBOR' instances,
-- based on the 'SerialiseAsRawBytes' instance. Eg:
--
-- > deriving (ToCBOR, FromCBOR) via (UsingRawBytes Blah)
newtype UsingRawBytes a = UsingRawBytes a

instance (SerialiseAsRawBytes a, Typeable a) => ToCBOR (UsingRawBytes a) where
  toCBOR (UsingRawBytes x) = toCBOR (serialiseToRawBytes x)

instance (SerialiseAsRawBytes a, Typeable a) => FromCBOR (UsingRawBytes a) where
  fromCBOR = do
    bs <- fromCBOR
    case deserialiseFromRawBytes ttoken bs of
      Just x -> return (UsingRawBytes x)
      Nothing -> fail ("cannot deserialise as a " ++ tname)
   where
    ttoken = proxyToAsType (Proxy :: Proxy a)
    tname = (tyConName . typeRepTyCon . typeRep) (Proxy :: Proxy a)

instance (SerialiseAsRawBytes a, Typeable a) => EncCBOR (UsingRawBytes a)

instance (SerialiseAsRawBytes a, Typeable a) => DecCBOR (UsingRawBytes a) where
  decCBOR = fromPlainDecoder fromCBOR

-- | For use with @deriving via@, to provide instances for any\/all of 'Show',
-- 'IsString', 'ToJSON', 'FromJSON', 'ToJSONKey', FromJSONKey' using a hex
-- encoding, based on the 'SerialiseAsRawBytes' instance.
--
-- > deriving (Show, IsString) via (UsingRawBytesHex Blah)
-- > deriving (ToJSON, FromJSON) via (UsingRawBytesHex Blah)
-- > deriving (ToJSONKey, FromJSONKey) via (UsingRawBytesHex Blah)
newtype UsingRawBytesHex a = UsingRawBytesHex a

instance SerialiseAsRawBytes a => Show (UsingRawBytesHex a) where
  show (UsingRawBytesHex x) = show (serialiseToRawBytesHex x)

instance SerialiseAsRawBytes a => IsString (UsingRawBytesHex a) where
  fromString = either error id . deserialiseFromRawBytesBase16 . BSC.pack

instance SerialiseAsRawBytes a => ToJSON (UsingRawBytesHex a) where
  toJSON (UsingRawBytesHex x) = toJSON (serialiseToRawBytesHexText x)

instance (SerialiseAsRawBytes a, Typeable a) => FromJSON (UsingRawBytesHex a) where
  parseJSON =
    Aeson.withText tname $
      either fail pure . deserialiseFromRawBytesBase16 . Text.encodeUtf8
   where
    tname = (tyConName . typeRepTyCon . typeRep) (Proxy :: Proxy a)

instance SerialiseAsRawBytes a => ToJSONKey (UsingRawBytesHex a) where
  toJSONKey =
    Aeson.toJSONKeyText $ \(UsingRawBytesHex x) -> serialiseToRawBytesHexText x

instance (SerialiseAsRawBytes a, Typeable a) => FromJSONKey (UsingRawBytesHex a) where
  fromJSONKey =
    Aeson.FromJSONKeyTextParser $
      either fail pure . deserialiseFromRawBytesBase16 . Text.encodeUtf8

deserialiseFromRawBytesBase16 ::
  SerialiseAsRawBytes a => ByteString -> Either String (UsingRawBytesHex a)
deserialiseFromRawBytesBase16 str =
  case Base16.decode str of
    Right raw -> case deserialiseFromRawBytes ttoken raw of
      Just x -> Right (UsingRawBytesHex x)
      Nothing -> Left ("cannot deserialise " ++ show str)
    Left msg -> Left ("invalid hex " ++ show str ++ ", " ++ msg)
 where
  ttoken = proxyToAsType (Proxy :: Proxy a)
