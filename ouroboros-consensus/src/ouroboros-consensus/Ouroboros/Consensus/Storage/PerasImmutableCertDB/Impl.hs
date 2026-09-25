{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
--
-- Robustness against on-disk failures (corruption, partial writes, missing
-- files) rests on three mechanisms:
--
-- * Certificates are written atomically (to a temporary file that is then
--   renamed into place, see 'writeCertFile'), so a certificate file that
--   exists is always complete.
--
-- * Each certificate file is self-verifying: it carries a format version and a
--   CRC32 of its payload (see 'encodeCertFileBytes'). Integrity is re-checked
--   whenever a certificate is read back. Certificates are /not/ semantically
--   re-validated here; that already happened before they were written, and the
--   syncing nodes that consume them validate them again themselves.
--
-- * A certificate whose file is missing or corrupt is /quarantined/ rather than
--   crashing the database: it is dropped from the in-memory index and traced,
--   keeping the remaining certificates available (see 'implGetCertsAfter' and
--   'PerasImmutableCertDbValidationPolicy').
module Ouroboros.Consensus.Storage.PerasImmutableCertDB.Impl
  ( -- * Opening
    PerasImmutableCertDbArgs (..)
  , PerasImmutableCertDbValidationPolicy (..)
  , defaultArgs
  , createDB

    -- * Trace types
  , TraceEvent (..)

    -- * Errors
  , CertFileError (..)
  , displayCertFileError
  )
where

import Cardano.Binary
import qualified Codec.CBOR.Read as CBOR
import Control.Monad (filterM, forM, forM_, unless, void)
import Control.Monad.State.Strict (StateT, get, lift, put)
import Control.ResourceRegistry (WithTempRegistry, allocateTemp, modifyWithTempRegistry)
import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import Data.List (isSuffixOf, stripPrefix)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Word (Word32, Word64, Word8)
import GHC.Generics (Generic)
import NoThunks.Class (OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Storage.PerasImmutableCertDB.API
import Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..), EncodeDisk (..))
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.IOLike
import System.FS.API.Lazy
import System.FS.CRC (CRC (..), initCRC, updateCRC)
import Text.Read (readMaybe)

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

-- | Reason why a certificate file on disk could not be read back into a
-- 'ValidatedPerasCert'.
--
-- These are not thrown: a certificate whose file is unreadable is /quarantined/
-- (dropped from the in-memory index and traced via 'QuarantinedCert') rather
-- than bringing down the whole database, so that the remaining, intact
-- certificates stay available to syncing nodes. See 'implGetCertsAfter'.
data CertFileError
  = -- | The file could not be opened or read (e.g. it is missing).
    CertFileReadError FsError
  | -- | The file envelope or the certificate payload could not be decoded.
    CertFileMalformed DecoderError
  | -- | The file uses an on-disk format version we do not understand.
    CertFileUnsupportedVersion Word8
  | -- | The payload's checksum does not match the one stored in the file,
    -- i.e. the file is corrupt (bit rot, a partial write, etc).
    CertFileChecksumMismatch
  deriving stock Show

-- | A short human-readable description of a 'CertFileError', for tracing.
displayCertFileError :: CertFileError -> String
displayCertFileError = \case
  CertFileReadError err -> "Read error: " <> show err
  CertFileMalformed err -> "Malformed: " <> show err
  CertFileUnsupportedVersion v -> "Unsupported on-disk format version " <> show v
  CertFileChecksumMismatch -> "Checksum mismatch"

{-------------------------------------------------------------------------------
  Trace types
-------------------------------------------------------------------------------}

data TraceEvent blk
  = -- | Number of certificates found on disk when opening.
    OpenedDB
      Int
  | -- | The result of attempting to add a certificate for the given round.
    AddedCert PerasRoundNo AddPerasImmutableCertResult
  | -- | A certificate file was found to be unreadable or corrupt and its round
    -- was dropped from the in-memory index. The 'String' describes the reason
    -- (see 'displayCertFileError').
    QuarantinedCert PerasRoundNo String
  deriving stock (Eq, Show, Generic)

{-------------------------------------------------------------------------------
  Creating the database
-------------------------------------------------------------------------------}

data PerasImmutableCertDbArgs f m blk = PerasImmutableCertDbArgs
  { picdbaCodecConfig :: HKD f (CodecConfig blk)
  , picdbaHasFS :: HKD f (SomeHasFS m)
  , picdbaTracer :: Tracer m (TraceEvent blk)
  , picdbaValidationPolicy :: PerasImmutableCertDbValidationPolicy
  -- ^ How thoroughly to check the certificate files on disk when opening.
  -- See 'PerasImmutableCertDbValidationPolicy'.
  }

-- | How much of the on-disk state to validate when opening the database.
--
-- Regardless of the policy, corruption is always detected lazily when a
-- certificate is actually served (see 'implGetCertsAfter'); the policy only
-- controls whether we additionally pay for an eager, up-front integrity sweep.
data PerasImmutableCertDbValidationPolicy
  = -- | Trust the certificate file names to build the index and defer all
    -- integrity checks to read time. Opening is cheap. This is the default.
    ValidateOnRead
  | -- | Additionally read and verify every certificate file when opening,
    -- quarantining any that are unreadable or corrupt. Opening is @O(n)@ in
    -- disk reads, but a corrupt certificate is never advertised to clients.
    ValidateAllOnOpen
  deriving stock (Eq, Show, Generic)

defaultArgs :: Monad m => Incomplete PerasImmutableCertDbArgs m blk
defaultArgs =
  PerasImmutableCertDbArgs
    { picdbaCodecConfig = noDefault
    , picdbaHasFS = noDefault
    , picdbaTracer = nullTracer
    , picdbaValidationPolicy = ValidateOnRead
    }

createDB ::
  forall m blk.
  ( IOLike m
  , EncodeDisk blk (PerasCert blk)
  , DecodeDisk blk (PerasCert blk)
  , IsPerasCert (PerasCert blk) blk
  ) =>
  Complete PerasImmutableCertDbArgs m blk ->
  m (PerasImmutableCertDB m blk)
createDB
  PerasImmutableCertDbArgs
    { picdbaCodecConfig
    , picdbaHasFS = someHasFS@(SomeHasFS hasFS)
    , picdbaTracer
    , picdbaValidationPolicy
    } = do
    createDirectoryIfMissing hasFS True (mkFsPath [])
    -- Remove any leftover temporary files from a certificate write that was
    -- interrupted (e.g. by a crash) before its atomic rename, see
    -- 'writeCertFile'.
    sweepTempCertFiles hasFS
    -- Index the certificate files present on disk by recovering their round
    -- numbers from their file names; the certificates themselves are read back
    -- from disk on demand, see 'implGetCertsAfter'.
    rounds <- case picdbaValidationPolicy of
      ValidateOnRead -> indexCertRounds hasFS
      ValidateAllOnOpen ->
        validateAllCertsOnOpen picdbaTracer picdbaCodecConfig hasFS
          =<< indexCertRounds hasFS
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
  , IsPerasCert (PerasCert blk) blk
  , EncodeDisk blk (PerasCert blk)
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
  -- may or may not show up, just as for the ImmutableDB (see 'getOpenState').
  rounds <- atomically $ readSVarSTM (picdbKnownRounds env)
  let roundsAfter = snd $ Set.split roundNo rounds
      candidates = take (fromIntegral maxCerts) (Set.toAscList roundsAfter)
  -- Read each certificate on demand. A certificate whose file is unreadable or
  -- corrupt is quarantined (traced and dropped from the index) rather than
  -- failing the whole request, so that the remaining certificates stay
  -- available to syncing nodes.
  fmap catMaybes $ forM candidates $ \r ->
    readCertFile env r >>= \case
      Right cert -> pure (Just cert)
      Left err -> Nothing <$ quarantineCert env r err

-- | Drop a certificate's round from the in-memory index and trace why. Used
-- when a certificate file turns out to be unreadable or corrupt; see
-- 'CertFileError'.
quarantineCert ::
  IOLike m =>
  PerasImmutableCertDbEnv m blk ->
  PerasRoundNo ->
  CertFileError ->
  m ()
quarantineCert env roundNo err = do
  updateSVar_ (picdbKnownRounds env) (Set.delete roundNo)
  traceWith (picdbTracer env) (QuarantinedCert roundNo (displayCertFileError err))

{-------------------------------------------------------------------------------
  On-disk serialisation
-------------------------------------------------------------------------------}

-- | The extension shared by all certificate files. A directory entry without
-- this extension is not a certificate file and is ignored when indexing.
certFileExtension :: String
certFileExtension = ".cert"

-- | The name of the file storing the certificate of the given round number.
--
-- The round number is encoded in the file name (and nowhere else), so that it
-- can be recovered without reading the file, see 'certRoundFromFileName'.
certFileName :: PerasRoundNo -> String
certFileName roundNo = show (unPerasRoundNo roundNo) <> certFileExtension

fsPathCertFile :: PerasRoundNo -> FsPath
fsPathCertFile roundNo = mkFsPath [certFileName roundNo]

-- | The suffix appended to a certificate file name while it is being written,
-- before it is atomically renamed into place. See 'writeCertFile'.
certFileTmpSuffix :: String
certFileTmpSuffix = ".tmp"

fsPathCertFileTmp :: PerasRoundNo -> FsPath
fsPathCertFileTmp roundNo = mkFsPath [certFileName roundNo <> certFileTmpSuffix]

-- | Whether a directory entry is a leftover temporary certificate file.
isCertFileTmpName :: String -> Bool
isCertFileTmpName = (certFileTmpSuffix `isSuffixOf`)

-- | The on-disk format version of a certificate file. Bump whenever the
-- envelope produced by 'encodeCertFileBytes' changes.
certFileVersion :: Word8
certFileVersion = 1

-- | Recover the round number of a certificate from its file name, or 'Nothing'
-- if the name is not a well-formed certificate file name. Inverse of
-- 'certFileName'.
certRoundFromFileName :: String -> Maybe PerasRoundNo
certRoundFromFileName name = do
  digits <- stripSuffix certFileExtension name
  PerasRoundNo <$> readMaybe digits
 where
  stripSuffix suffix s = reverse <$> stripPrefix (reverse suffix) (reverse s)

encodeCert ::
  EncodeDisk blk (PerasCert blk) =>
  CodecConfig blk ->
  ValidatedPerasCert blk ->
  Encoding
encodeCert ccfg (ValidatedPerasCert cert boost) =
  encodeListLen 2
    <> encodeDisk ccfg cert
    <> toCBOR boost

decodeCert ::
  DecodeDisk blk (PerasCert blk) =>
  CodecConfig blk ->
  forall s.
  Decoder s (ValidatedPerasCert blk)
decodeCert ccfg = do
  decodeListLenOf 2
  cert <- decodeDisk ccfg
  boost <- fromCBOR
  pure (ValidatedPerasCert cert boost)

-- | Serialise a certificate into the self-verifying, versioned bytes stored on
-- disk: a CBOR 3-element list @[version, crc, payload]@, where @payload@ is the
-- (inline) certificate encoding produced by 'encodeCert' and @crc@ is a CRC32
-- computed over exactly @payload@'s bytes.
--
-- Placing @payload@ /last/ lets us both fold the CRC over it and append it to
-- the already-serialised header in a single streaming pass, so the payload is
-- serialised exactly once. In particular, we avoid the strict copy and the
-- extra serialisation roundtrip that embedding the payload as a nested CBOR
-- byte string would force.
--
-- The CRC lets 'decodeCertFile' detect corruption (including a partial write
-- that somehow slipped past the atomic rename in 'writeCertFile') without
-- having to re-run the certificate's (expensive) semantic validation, which
-- already happened before the certificate was ever written here.
encodeCertFileBytes ::
  EncodeDisk blk (PerasCert blk) =>
  CodecConfig blk ->
  ValidatedPerasCert blk ->
  BSL.ByteString
encodeCertFileBytes ccfg cert =
  header <> payload
 where
  payload :: BSL.ByteString
  payload = serialize $ encodeCert ccfg cert
  header :: BSL.ByteString
  header =
    serialize $
      encodeListLen 3
        <> toCBOR certFileVersion
        <> toCBOR (getCRC (updateCRC payload initCRC))

-- | Decode and integrity-check the bytes produced by 'encodeCertFileBytes',
-- returning the reason on any failure.
decodeCertFile ::
  DecodeDisk blk (PerasCert blk) =>
  CodecConfig blk ->
  BSL.ByteString ->
  Either CertFileError (ValidatedPerasCert blk)
decodeCertFile ccfg fileBytes = do
  -- Decoding only the header leaves the inline payload as the unconsumed
  -- suffix, which is exactly the byte range the CRC was computed over on write.
  (payload, (version, expectedCRC)) <-
    first (CertFileMalformed . asDeserialiseFailure) $
      CBOR.deserialiseFromBytes decodeHeader fileBytes
  unless (version == certFileVersion) $
    Left (CertFileUnsupportedVersion version)
  unless (getCRC (updateCRC payload initCRC) == expectedCRC) $
    Left CertFileChecksumMismatch
  first CertFileMalformed $
    decodeFullDecoder (pack "Immutable Peras Certificate") (decodeCert ccfg) payload
 where
  decodeHeader :: Decoder s (Word8, Word32)
  decodeHeader = do
    decodeListLenOf 3
    version <- fromCBOR
    expectedCRC <- fromCBOR
    pure (version, expectedCRC)
  asDeserialiseFailure =
    DecoderErrorDeserialiseFailure (pack "Immutable Peras Certificate file")

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
    SomeHasFS hasFS -> do
      -- Write to a temporary file first, then rename it into place.
      -- This ensures that a crash mid-write can never leave a partial
      -- file under the committed name: any @<round>.cert@ that exists is
      -- guaranteed to be complete. Leftover @<round>.cert.tmp@ files are
      -- cleaned up on the next open; see 'sweepTempCertFiles'.
      withFile hasFS tmpPath (WriteMode MustBeNew) $ \h ->
        void $ hPutAll hasFS h bytes
      renameFile hasFS tmpPath path
 where
  tmpPath = fsPathCertFileTmp roundNo
  path = fsPathCertFile roundNo
  bytes = encodeCertFileBytes (picdbCodecConfig env) cert

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

-- | Read, integrity-check and decode the certificate of the given round
-- number, returning a 'CertFileError' if the file is missing, unreadable or
-- corrupt (see 'quarantineCert').
readCertFile ::
  forall m blk.
  ( IOLike m
  , DecodeDisk blk (PerasCert blk)
  ) =>
  PerasImmutableCertDbEnv m blk ->
  PerasRoundNo ->
  m (Either CertFileError (ValidatedPerasCert blk))
readCertFile env roundNo =
  case picdbHasFS env of
    SomeHasFS hasFS -> readCertFileAt (picdbCodecConfig env) hasFS (fsPathCertFile roundNo)

readCertFileAt ::
  forall m h blk.
  ( IOLike m
  , DecodeDisk blk (PerasCert blk)
  ) =>
  CodecConfig blk ->
  HasFS m h ->
  FsPath ->
  m (Either CertFileError (ValidatedPerasCert blk))
readCertFileAt ccfg hasFS path = do
  readResult <- try $ withFile hasFS path ReadMode (hGetAll hasFS)
  pure $ case readResult of
    Left (err :: FsError) -> Left (CertFileReadError err)
    Right bytes -> decodeCertFile ccfg bytes

-- | Index the round numbers of all certificate files in the database
-- directory.
--
-- The round number of each certificate is recovered from its file name (see
-- 'certFileName'), so the certificates themselves are not read or decoded here;
-- that happens on demand in 'readCertFile'. Directory entries that are not
-- well-formed certificate file names are ignored.
indexCertRounds ::
  IOLike m =>
  HasFS m h ->
  m (Set PerasRoundNo)
indexCertRounds hasFS = do
  names <- listDirectory hasFS (mkFsPath [])
  pure $ Set.fromList $ mapMaybe certRoundFromFileName $ Set.toList names

-- | Remove any leftover temporary certificate files (see 'writeCertFile') from
-- the database directory. Called once when opening.
sweepTempCertFiles ::
  IOLike m =>
  HasFS m h ->
  m ()
sweepTempCertFiles hasFS = do
  names <- listDirectory hasFS (mkFsPath [])
  forM_ (filter isCertFileTmpName (Set.toList names)) $ \name ->
    removeFile hasFS (mkFsPath [name])

-- | Eagerly read and integrity-check every indexed certificate, returning the
-- subset whose files are intact. Any unreadable or corrupt certificate is
-- traced via 'QuarantinedCert' and dropped, so that it is never advertised to
-- clients. Used by the 'ValidateAllOnOpen' policy.
validateAllCertsOnOpen ::
  ( IOLike m
  , DecodeDisk blk (PerasCert blk)
  ) =>
  Tracer m (TraceEvent blk) ->
  CodecConfig blk ->
  HasFS m h ->
  Set PerasRoundNo ->
  m (Set PerasRoundNo)
validateAllCertsOnOpen tracer ccfg hasFS rounds =
  fmap Set.fromList $ filterM isIntact $ Set.toList rounds
 where
  isIntact roundNo =
    readCertFileAt ccfg hasFS (fsPathCertFile roundNo) >>= \case
      Right _ -> pure True
      Left err -> do
        traceWith tracer (QuarantinedCert roundNo (displayCertFileError err))
        pure False
