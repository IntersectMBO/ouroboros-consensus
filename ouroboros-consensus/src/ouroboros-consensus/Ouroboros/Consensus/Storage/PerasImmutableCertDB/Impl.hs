{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A simplified variant of the ImmutableDB specialised to storing immutable
-- Peras certificates.
--
-- Unlike the ImmutableDB, this database does not use chunking: each certificate
-- is stored in its own file, named after the Peras round number of the
-- certificate (which uniquely identifies it). Certificates themselves are
-- therefore never cached in memory; only the (much smaller) set of certificate
-- round numbers known to be on disk is kept in memory, guarded by a
-- 'StrictSVar', similarly to how the ImmutableDB guards its
-- 'Ouroboros.Consensus.Storage.ImmutableDB.Impl.State.OpenState'.
-- Every database operation goes through this guarded set,
-- which acts as this database's (much simpler, since there is no chunking)
-- equivalent of the ImmutableDB's on-disk indices.
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
import Control.Monad.State.Strict (StateT, get, lift, put)
import Control.ResourceRegistry (WithTempRegistry, allocateTemp, modifyWithTempRegistry)
import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (OnlyCheckWhnfNamed (..))
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
  , picdbKnownRounds :: !(StrictSVar m (Set PerasRoundNo))
  -- ^ The round numbers of all certificates currently stored on
  -- disk. This is the only bit of information about the certificates kept
  -- in memory; the certificates themselves are read back from disk
  -- on demand.
  }
  deriving
    NoThunks
    via OnlyCheckWhnfNamed "PerasImmutableCertDbEnv" (PerasImmutableCertDbEnv m blk)

-- | Shorthand for the monad in which 'implAddCert' safely modifies
-- 'picdbKnownRounds': allocated resources (here, a single certificate file)
-- are automatically cleaned up if they don't end up part of the on-disk state.
type ModifyKnownRounds m = StateT (Set PerasRoundNo) (WithTempRegistry (Set PerasRoundNo) m)

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
  = -- | Number of certificates found on disk when opening.
    OpenedDB
      Int
  | -- | The result of attempting to add a certificate for the given round.
    AddedCert PerasRoundNo AddPerasImmutableCertResult
  deriving stock (Eq, Show, Generic)

{-------------------------------------------------------------------------------
  Creating the database
-------------------------------------------------------------------------------}

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
    -- Validate every certificate file present on disk once, up front, and
    -- keep only the (much cheaper) round numbers around: certificates
    -- themselves are read back from disk on demand, see 'implGetCertsAfter'.
    rounds <- indexCertRounds (Proxy @blk) picdbaCodecConfig hasFS
    picdbKnownRounds <- newSVar rounds
    let env =
          PerasImmutableCertDbEnv
            { picdbHasFS = someHasFS
            , picdbCodecConfig = picdbaCodecConfig
            , picdbTracer = picdbaTracer
            , picdbKnownRounds
            }
    traceWith picdbaTracer (OpenedDB (Set.size rounds))
    pure
      PerasImmutableCertDB
        { addCert = implAddCert env
        , getCertsAfter = implGetCertsAfter env
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
  result <- modifyWithTempRegistry getSt putSt modifyRounds
  traceWith (picdbTracer env) (AddedCert roundNo result)
  pure result
 where
  roundNo = getPerasCertRound cert

  getSt :: m (Set PerasRoundNo)
  getSt = takeSVar (picdbKnownRounds env)

  -- Taking and putting back the 'StrictSVar' makes the whole
  -- check-then-write below atomic wrt concurrent 'addCert' calls, closing
  -- the race that a plain 'StrictTVar' check-then-update can't avoid. On
  -- abort or exception, restore the state as it was before this call; any
  -- certificate file written in the meantime is cleaned up by
  -- 'allocateTemp' (see 'modifyRounds') since it never becomes part of the
  -- committed state.
  putSt :: Set PerasRoundNo -> ExitCase (Set PerasRoundNo) -> m ()
  putSt before ec =
    putSVar (picdbKnownRounds env) $ case ec of
      ExitCaseSuccess after -> after
      _ -> before

  modifyRounds :: ModifyKnownRounds m AddPerasImmutableCertResult
  modifyRounds = do
    rounds <- get
    if Set.member roundNo rounds
      then pure CertAlreadyInImmutableDB
      else do
        lift $
          allocateTemp
            (writeCertFile env roundNo cert)
            (\() -> removeCertFile env roundNo >> pure True)
            (\rounds' () -> Set.member roundNo rounds')
        put (Set.insert roundNo rounds)
        pure AddedCertToImmutableDB

implGetCertsAfter ::
  forall m blk.
  ( IOLike m
  , DecodeDisk blk (PerasCert blk)
  ) =>
  PerasImmutableCertDbEnv m blk ->
  PerasRoundNo ->
  Word64 ->
  m [ValidatedPerasCert blk]
implGetCertsAfter env roundNo maxCerts = do
  -- A possibly slightly stale read is fine: concurrently added certificates
  -- may or may not show up, just as for the ImmutableDB (see
  -- 'getOpenState').
  rounds <- atomically $ readSVarSTM (picdbKnownRounds env)
  let roundsAfter = snd $ Set.split roundNo rounds
  mapM (readCertFile env) (take (fromIntegral maxCerts) (Set.toAscList roundsAfter))

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

-- | Remove the file of a certificate.
--
-- Only used to clean up a certificate file that was written by 'addCert' but
-- never made it into 'picdbKnownRounds' because of an exception.
removeCertFile ::
  PerasImmutableCertDbEnv m blk ->
  PerasRoundNo ->
  m ()
removeCertFile env roundNo =
  case picdbHasFS env of
    SomeHasFS hasFS -> removeFile hasFS (fsPathCertFile roundNo)

-- | Read and decode the certificate of the given round number.
--
-- PRECONDITION: the round number's certificate file exists.
readCertFile ::
  forall m blk.
  ( IOLike m
  , DecodeDisk blk (PerasCert blk)
  ) =>
  PerasImmutableCertDbEnv m blk ->
  PerasRoundNo ->
  m (ValidatedPerasCert blk)
readCertFile env roundNo =
  case picdbHasFS env of
    SomeHasFS hasFS -> readCertFileAt (Proxy @blk) (picdbCodecConfig env) hasFS (fsPathCertFile roundNo)

readCertFileAt ::
  ( IOLike m
  , DecodeDisk blk (PerasCert blk)
  ) =>
  Proxy blk ->
  CodecConfig blk ->
  HasFS m h ->
  FsPath ->
  m (ValidatedPerasCert blk)
readCertFileAt _ ccfg hasFS path = do
  bytes <- withFile hasFS path ReadMode (hGetAll hasFS)
  case CBOR.deserialiseFromBytes (decodeCert ccfg) bytes of
    Right (_leftover, cert) -> pure cert
    -- Corrupt data on disk is treated as unrecoverable,
    -- so error handling is bubbled up.
    Left err -> throwIO $ CorruptPerasImmutableCertFile path (show err)

-- | Read and decode all certificate files in the database directory.
--
-- PRECONDITION: all certificate files are valid.
-- POSTCONDITION: the returned list is finite.
indexCertRounds ::
  forall m h blk.
  ( IOLike m
  , IsPerasCert (PerasCert blk) blk
  , DecodeDisk blk (PerasCert blk)
  ) =>
  Proxy blk ->
  CodecConfig blk ->
  HasFS m h ->
  m (Set PerasRoundNo)
indexCertRounds proxy ccfg hasFS = do
  names <- Set.toAscList <$> listDirectory hasFS (mkFsPath [])
  fmap Set.fromDistinctAscList $ forM names $ \name -> do
    cert <- readCertFileAt proxy ccfg hasFS (mkFsPath [name])
    pure (getPerasCertRound cert)
