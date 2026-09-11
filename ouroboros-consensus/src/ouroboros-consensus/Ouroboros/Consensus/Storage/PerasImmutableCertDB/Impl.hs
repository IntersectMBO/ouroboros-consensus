{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | A simplified variant of the ImmutableDB specialised to storing immutable
-- Peras certificates.
--
-- Unlike the ImmutableDB, this database does not use chunking: each certificate
-- is stored in its own file, named after the Peras round number of the
-- certificate (which uniquely identifies it). An in-memory index (keyed by
-- round number) is rebuilt by scanning the directory when the database is
-- opened.
module Ouroboros.Consensus.Storage.PerasImmutableCertDB.Impl
  ( -- * Opening
    PerasImmutableCertDbArgs (..)
  , defaultArgs
  , createDB

    -- * Trace types
  , TraceEvent (..)
  ) where

import Cardano.Binary (fromCBOR, toCBOR)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import Control.Monad (forM, void)
import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Storage.PerasImmutableCertDB.API
import Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..), EncodeDisk (..))
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.IOLike
import System.FS.API.Lazy

{-------------------------------------------------------------------------------
  Database state
-------------------------------------------------------------------------------}

data PerasImmutableCertDbEnv m blk = PerasImmutableCertDbEnv
  { picdbHasFS :: !(SomeHasFS m)
  , picdbCodecConfig :: !(CodecConfig blk)
  , picdbTracer :: !(Tracer m (TraceEvent blk))
  , picdbState :: !(StrictTVar m (Map PerasRoundNo (ValidatedPerasCert blk)))
  -- ^ In-memory index of all certificates on disk, keyed by round number.
  }
  deriving
    NoThunks
    via OnlyCheckWhnfNamed "PerasImmutableCertDbEnv" (PerasImmutableCertDbEnv m blk)

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | A certificate file on disk could not be decoded.
data PerasImmutableCertDbError
  = CorruptPerasImmutableCertFile FsPath String
  deriving stock Show
  deriving anyclass Exception

{-------------------------------------------------------------------------------
  Trace types
-------------------------------------------------------------------------------}

data TraceEvent blk
  = OpenedDB
      Int
      -- ^ Number of certificates found on disk when opening.
  | AddedCert PerasRoundNo AddPerasImmutableCertResult
  deriving stock (Eq, Show, Generic)

{-------------------------------------------------------------------------------
  Creating the database
-------------------------------------------------------------------------------}

type PerasImmutableCertDbArgs ::
  (Type -> Type) -> (Type -> Type) -> Type -> Type
data PerasImmutableCertDbArgs f m blk = PerasImmutableCertDbArgs
  { picdbaCodecConfig :: HKD f (CodecConfig blk)
  , picdbaHasFS :: HKD f (SomeHasFS m)
  , picdbaTracer :: Tracer m (TraceEvent blk)
  }

defaultArgs :: Monad m => Incomplete PerasImmutableCertDbArgs m blk
defaultArgs =
  PerasImmutableCertDbArgs
    { picdbaCodecConfig = noDefault
    , picdbaHasFS = noDefault
    , picdbaTracer = nullTracer
    }

-- | Constraints needed to operate the database for a given block type.
type PerasImmutableCertDbConstraints blk =
  ( StandardHash blk
  , IsPerasCert (PerasCert blk) blk
  , NoThunks (PerasCert blk)
  , EncodeDisk blk (PerasCert blk)
  , DecodeDisk blk (PerasCert blk)
  )

createDB ::
  forall m blk.
  ( IOLike m
  , PerasImmutableCertDbConstraints blk
  ) =>
  Complete PerasImmutableCertDbArgs m blk ->
  m (PerasImmutableCertDB m blk)
createDB
  PerasImmutableCertDbArgs
    { picdbaCodecConfig
    , picdbaHasFS = someHasFS@(SomeHasFS hasFS)
    , picdbaTracer
    } = do
    createDirectoryIfMissing hasFS True (mkFsPath [])
    certs <- readAllCerts hasFS picdbaCodecConfig
    picdbState <-
      newTVarIO $
        Map.fromList [(getPerasCertRound cert, cert) | cert <- certs]
    let env =
          PerasImmutableCertDbEnv
            { picdbHasFS = someHasFS
            , picdbCodecConfig = picdbaCodecConfig
            , picdbTracer = picdbaTracer
            , picdbState
            }
    traceWith picdbaTracer (OpenedDB (length certs))
    pure
      PerasHistCertDB
        { addCert = implAddCert env
        , getPointCerts = implGetCerts env
        }

{-------------------------------------------------------------------------------
  API implementation
-------------------------------------------------------------------------------}

implAddCert ::
  forall m blk.
  ( IOLike m
  , PerasImmutableCertDbConstraints blk
  ) =>
  PerasImmutableCertDbEnv m blk ->
  ValidatedPerasCert blk ->
  m AddPerasImmutableCertResult
implAddCert env cert = do
  present <- atomically $ Map.member roundNo <$> readTVar (picdbState env)
  result <-
    if present
      then pure CertAlreadyInImmutableDB
      else do
        -- ponytail: no lock, assumes a single writer (the ChainDB adds
        -- certificates from one background thread). Add a per-db lock if
        -- concurrent 'addCert' for the same round ever becomes possible.
        writeCertFile env roundNo cert
        atomically $ modifyTVar (picdbState env) (Map.insert roundNo cert)
        pure AddedCertToImmutableDB
  traceWith (picdbTracer env) (AddedCert roundNo result)
  pure result
 where
  roundNo = getPerasCertRound cert

implGetCerts ::
  forall m blk.
  ( IOLike m
  , StandardHash blk
  , IsPerasCert (PerasCert blk) blk
  ) =>
  PerasImmutableCertDbEnv m blk ->
  Point blk ->
  m [ValidatedPerasCert blk]
implGetCerts env pt = atomically $ do
  certs <- readTVar (picdbState env)
  pure [cert | cert <- Map.elems certs, getPerasCertPoint cert == pt]

{-------------------------------------------------------------------------------
  On-disk serialisation
-------------------------------------------------------------------------------}

fsPathCertFile :: PerasRoundNo -> FsPath
fsPathCertFile roundNo = mkFsPath [show (unPerasRoundNo roundNo) <> ".cert"]

encodeCert ::
  EncodeDisk blk (PerasCert blk) =>
  CodecConfig blk ->
  ValidatedPerasCert blk ->
  CBOR.Encoding
encodeCert ccfg (ValidatedPerasCert cert boost) =
  CBOR.encodeListLen 2
    <> encodeDisk ccfg cert
    <> toCBOR boost

decodeCert ::
  DecodeDisk blk (PerasCert blk) =>
  CodecConfig blk ->
  forall s.
  CBOR.Decoder s (ValidatedPerasCert blk)
decodeCert ccfg = do
  CBOR.decodeListLenOf 2
  cert <- decodeDisk ccfg
  boost <- fromCBOR
  pure (ValidatedPerasCert cert boost)

writeCertFile ::
  ( IOLike m
  , EncodeDisk blk (PerasCert blk)
  ) =>
  PerasImmutableCertDbEnv m blk ->
  PerasRoundNo ->
  ValidatedPerasCert blk ->
  m ()
writeCertFile env roundNo cert =
  case picdbHasFS env of
    SomeHasFS hasFS ->
      withFile hasFS path (WriteMode MustBeNew) $ \h ->
        void $ hPutAll hasFS h bytes
 where
  path = fsPathCertFile roundNo
  bytes = CBOR.toLazyByteString $ encodeCert (picdbCodecConfig env) cert

readAllCerts ::
  forall m h blk.
  ( IOLike m
  , DecodeDisk blk (PerasCert blk)
  ) =>
  HasFS m h ->
  CodecConfig blk ->
  m [ValidatedPerasCert blk]
readAllCerts hasFS ccfg = do
  names <- Set.toList <$> listDirectory hasFS (mkFsPath [])
  forM names $ \name -> do
    let path = mkFsPath [name]
    bytes <- withFile hasFS path ReadMode (hGetAll hasFS)
    case CBOR.deserialiseFromBytes (decodeCert ccfg) bytes of
      Right (_leftover, cert) -> pure cert
      Left err -> throwIO $ CorruptPerasImmutableCertFile path (show err)
