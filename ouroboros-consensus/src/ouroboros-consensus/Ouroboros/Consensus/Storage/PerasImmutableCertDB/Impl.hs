{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An on-disk store for immutable (historical) Peras certificates: each
-- certificate is stored in its own file, named after the Peras round number
-- that uniquely identifies it. The certificates themselves are never cached in
-- memory; only the (much smaller) set of round numbers known to be on disk is
-- kept in memory, guarded by a 'StrictSVar' which every database operation
-- goes through.
--
-- = Implementation overview
--
-- The design is modelled on the block ImmutableDB, but is considerably
-- simpler:
--
-- * There is no chunking and there are no on-disk indices. The index is just
--   the in-memory sets of round numbers ('CertDbState'), rebuilt on every
--   open from the certificate file names alone. No file needs to be read, so
--   this is cheap.
--
-- * The index is updated like the ImmutableDB's open state, via
--   'modifyWithTempRegistry' on a 'StrictSVar'. Concurrent adds therefore
--   happen one at a time, and a failed add leaves no trace, so the index and
--   the files on disk never disagree (see 'implAddCert').
--
-- A design goal is to never advertise incomplete, unreadable or corrupt
-- certificates to clients; for this, prevention and handling of on-disk
-- failures (corruption, partial writes, missing files) rests on the following
-- mechanisms:
--
-- * Certificates are written atomically (to a temporary file that is then
--   renamed into place, see 'writeCertFile'), so a certificate file that
--   exists is always complete.
--
-- * Each certificate file is self-verifying: it stores a CRC32 of its payload
--   (see 'encodeCertFileBytes'). Integrity is re-checked whenever a certificate
--   is read back, but they are /not/ semantically re-validated here; that
--   already happened before they were written, and the syncing nodes that
--   consume them are expected to do the validation again themselves.
--
-- * A certificate whose file is missing or corrupt is /quarantined/ rather than
--   crashing the database: its file is atomically renamed in place by
--   appending 'quarantinedSuffix' to its name (an empty marker file with that
--   name is created if the original was missing), its round is moved from the
--   known to the quarantined rounds of the in-memory index, and the event is
--   traced. The remaining certificates stay available to syncing nodes. Only
--   the name of a quarantined file matters here, never its contents.
--   Quarantined rounds survive restarts, since they are re-indexed from the
--   file names on open, and are released from quarantine when a certificate
--   for them is added again.
--
-- * Adding a certificate for a round that is already stored checks the stored
--   file, and overwrites it should it be unreadable or corrupt.
--
-- The 'ValidateAllOnOpen' policy alternatively reads and integrity-checks every
-- certificate file on open.
--
-- = Life cycle of a certificate
--
-- For round 42, the database directory may hold:
--
-- > 42.cert              the certificate        (42 in 'cdsKnownRounds')
-- > 42.cert.quarantined  quarantine marker      (42 in 'cdsQuarantinedRounds')
-- > 42.cert.tmp          interrupted write      (transient, resolved on open)
--
-- Any other entry, including a non-canonical name such as @042.cert@, is
-- ignored. A certificate written "atomically" is written to @42.cert.tmp@,
-- which is then renamed to @42.cert@.
--
-- > addCert 42
-- >  |
-- >  +-- known ------> 42.cert intact? --yes--> CertAlreadyInImmutableDB
-- >  |                  | no
-- >  |                  +--> overwrite 42.cert atomically
-- >  |                       --> ReplacedCorruptCertInImmutableDB
-- >  |
-- >  +-- quarantined -> write 42.cert atomically, then delete 42.cert.quarantined
-- >  |                  --> AddedCertToImmutableDB          (42 released)
-- >  |
-- >  +-- unknown ----> write 42.cert atomically
-- >                     --> AddedCertToImmutableDB
-- >
-- > getCertsAfter (for each known round, ascending; here 42)
-- >  |
-- >  read 42.cert --intact--> served
-- >  | missing, unreadable, malformed or CRC mismatch
-- >  v
-- >  [holding the lock] 42 still known? --no--> skipped (already quarantined
-- >  | yes                                       by a concurrent reader)
-- >  v
-- >  re-read 42.cert --intact--> served (replaced by a concurrent addCert)
-- >  | still broken
-- >  v
-- >  42.cert exists? --yes--> rename 42.cert to 42.cert.quarantined --+
-- >  | no                                                              |
-- >  +--> create empty 42.cert.quarantined ----------------------------+
-- >                                                                    v
-- >                   42 quarantined: not served until a later addCert 42
-- >
-- > openDB
-- >  42.cert.tmp, without 42.cert nor 42.cert.quarantined
-- >      --> rename it to 42.cert.quarantined   (traced as an incomplete write)
-- >  42.cert.tmp, otherwise
-- >      --> delete it
-- >  42.cert and 42.cert.quarantined            (crash while releasing 42)
-- >      --> delete 42.cert.quarantined         (the stored file wins)
-- >  ValidateAllOnOpen, broken 42.cert
-- >      --> quarantine it, as in getCertsAfter
--
-- A crash can therefore only leave behind a @42.cert.tmp@ (interrupted atomic
-- write), or both @42.cert@ and @42.cert.quarantined@ (interrupted release),
-- both of which are resolved on the next open. Quarantining itself is a
-- single 'renameFile' (or file creation), so it is never left half-done.
module Ouroboros.Consensus.Storage.PerasImmutableCertDB.Impl
  ( -- * Opening
    PerasImmutableCertDbArgs (..)
  , PerasImmutableCertDbValidationPolicy (..)
  , defaultArgs
  , openDB

    -- * Trace types
  , TraceEvent (..)

    -- * Errors
  , CertFileError (..)
  , displayCertFileError
  )
where

import Cardano.Binary
import qualified Codec.CBOR.Read as CBOR
import Control.Monad (foldM, forM, forM_, guard, unless, void, when)
import Control.Monad.State.Strict (StateT, get, lift, put)
import Control.ResourceRegistry (WithTempRegistry, allocateTemp, modifyWithTempRegistry)
import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import Data.List (stripPrefix)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import NoThunks.Class (OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Storage.PerasImmutableCertDB.API
import Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..), EncodeDisk (..))
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.IOLike
import System.FS.API.Lazy
import System.FS.CRC (CRC (..), computeCRC)
import Text.Read (readMaybe)

{-------------------------------------------------------------------------------
  Creating the database
-------------------------------------------------------------------------------}

data PerasImmutableCertDbArgs f m blk = PerasImmutableCertDbArgs
  { picdbaCodecConfig :: HKD f (CodecConfig blk)
  , picdbaHasFS :: HKD f (SomeHasFS m)
  , picdbaTracer :: Tracer m (TraceEvent blk)
  , picdbaValidationPolicy :: PerasImmutableCertDbValidationPolicy
  -- ^ How thoroughly to check the certificate files on disk when opening.
  }

-- | How much of the on-disk state to validate when opening the database.
--
-- Regardless of the policy, corruption is always detected lazily when a
-- certificate is actually served; the policy only controls whether we
-- additionally pay for an eager, up-front integrity sweep.
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

openDB ::
  forall m blk.
  ( IOLike m
  , EncodeDisk blk (PerasCert blk)
  , DecodeDisk blk (PerasCert blk)
  , IsPerasCert (PerasCert blk) blk
  ) =>
  Complete PerasImmutableCertDbArgs m blk ->
  m (PerasImmutableCertDB m blk)
openDB
  PerasImmutableCertDbArgs
    { picdbaCodecConfig
    , picdbaHasFS = someHasFS@(SomeHasFS hasFS)
    , picdbaTracer
    , picdbaValidationPolicy
    } = do
    createDirectoryIfMissing hasFS True (mkFsPath rootDir)
    -- Deal with any leftover temporary files from a certificate write that
    -- was interrupted (e.g. by a crash) before its atomic rename, see
    -- 'writeCertFile'.
    recoverTempCertFiles picdbaTracer hasFS
    -- Index the certificate files present on disk by recovering their round
    -- numbers from their file names; the certificates themselves are read back
    -- from disk on demand.
    initialState <- indexCertRounds hasFS >>= dropStaleQuarantineMarkers hasFS
    st <- case picdbaValidationPolicy of
      ValidateOnRead -> pure initialState
      ValidateAllOnOpen ->
        validateAllCertsOnOpen picdbaTracer picdbaCodecConfig hasFS initialState
    picdbState <- newSVar st
    let env =
          PerasImmutableCertDbEnv
            { picdbHasFS = someHasFS
            , picdbCodecConfig = picdbaCodecConfig
            , picdbTracer = picdbaTracer
            , picdbState
            }
    traceWith picdbaTracer $
      OpenedDB (Set.size (cdsKnownRounds st)) (Set.size (cdsQuarantinedRounds st))
    pure
      PerasImmutableCertDB
        { addCert = implAddCert env
        , getCertsAfter = implGetCertsAfter env
        }

{-------------------------------------------------------------------------------
  Opening: recovery and indexing
-------------------------------------------------------------------------------}

-- | Deal with the leftover temporary certificate files in the database
-- directory (see 'writeCertFile'). Called once when opening, before indexing.
--
-- Such a file means that storing a certificate was interrupted. If no
-- certificate file for its round was committed (nor quarantined), that
-- certificate is missing from the database, so the temporary file is renamed
-- to its quarantined name: this makes the round known as quarantined, so that
-- a replacement can be requested for it. Otherwise, the temporary file is just
-- removed. For example, @42.cert.tmp@ is renamed to @42.cert.quarantined@ if
-- neither @42.cert@ nor @42.cert.quarantined@ exist, and removed otherwise.
--
-- Entries that are not well-formed temporary certificate file names (such as
-- @foo.tmp@) are left alone, like any other unrecognised entry.
recoverTempCertFiles ::
  IOLike m =>
  Tracer m (TraceEvent blk) ->
  HasFS m h ->
  m ()
recoverTempCertFiles tracer hasFS = do
  names <- listDirectory hasFS (mkFsPath rootDir)
  forM_ (mapMaybe certRoundFromTmpFileName (Set.toList names)) $ \roundNo ->
    if Set.member (certFileName roundNo) names
      || Set.member (certFileName roundNo <> quarantinedSuffix) names
      then removeFile hasFS (fsPathTmpCertFile roundNo)
      else do
        renameFile hasFS (fsPathTmpCertFile roundNo) (fsPathQuarantinedCertFile roundNo)
        traceQuarantinedCert tracer roundNo CertFileIncompleteWrite

-- | Index the round numbers of all certificate files in the database
-- directory, returning the known and the quarantined rounds, in that order.
--
-- The round number of each certificate is recovered from its file name (see
-- 'certFileName' and 'fsPathQuarantinedCertFile'), so the certificates
-- themselves are not read or decoded here; that happens on demand in
-- 'readCertFile'. Directory entries that are not well-formed certificate file
-- names are ignored.
indexCertRounds ::
  IOLike m =>
  HasFS m h ->
  m (Set PerasRoundNo, Set PerasRoundNo)
indexCertRounds hasFS = do
  names <- Set.toList <$> listDirectory hasFS (mkFsPath rootDir)
  pure
    ( Set.fromList $ mapMaybe certRoundFromFileName names
    , Set.fromList $ mapMaybe certRoundFromQuarantinedFileName names
    )

-- | Build the initial index from the known and the quarantined rounds found
-- by 'indexCertRounds', removing the quarantine marker of every round that is
-- both stored and quarantined.
--
-- Such a round is left behind by a crash while adding a replacement for a
-- quarantined certificate (see 'implAddCert'). The stored file takes
-- precedence: should it be broken, it will be quarantined again.
dropStaleQuarantineMarkers ::
  IOLike m =>
  HasFS m h ->
  (Set PerasRoundNo, Set PerasRoundNo) ->
  m CertDbState
dropStaleQuarantineMarkers hasFS (knownRounds, quarantinedRounds) = do
  forM_ staleQuarantinedRounds $ removeFile hasFS . fsPathQuarantinedCertFile
  pure
    CertDbState
      { cdsKnownRounds = knownRounds
      , cdsQuarantinedRounds = Set.difference quarantinedRounds staleQuarantinedRounds
      }
 where
  staleQuarantinedRounds = Set.intersection knownRounds quarantinedRounds

-- | Eagerly read and integrity-check every known certificate, moving any
-- unreadable or corrupt one to quarantine (tracing it via 'QuarantinedCert'),
-- so that it is never advertised to clients. Used by the 'ValidateAllOnOpen'
-- policy.
validateAllCertsOnOpen ::
  ( IOLike m
  , DecodeDisk blk (PerasCert blk)
  ) =>
  Tracer m (TraceEvent blk) ->
  CodecConfig blk ->
  HasFS m h ->
  CertDbState ->
  m CertDbState
validateAllCertsOnOpen tracer ccfg hasFS st =
  foldM validateCert st (cdsKnownRounds st)
 where
  validateCert st' roundNo =
    readCertFileAt ccfg hasFS (fsPathCertFile roundNo) >>= \case
      Right _ -> pure st'
      Left err -> do
        st'' <- quarantineCert hasFS roundNo st'
        traceQuarantinedCert tracer roundNo err
        pure st''

{-------------------------------------------------------------------------------
  Database state
-------------------------------------------------------------------------------}

data PerasImmutableCertDbEnv m blk = PerasImmutableCertDbEnv
  { picdbHasFS :: !(SomeHasFS m)
  , picdbCodecConfig :: !(CodecConfig blk)
  , picdbTracer :: !(Tracer m (TraceEvent blk))
  , picdbState :: !(StrictSVar m CertDbState)
  -- ^ The in-memory index of the database. This is the only bit of
  -- information about the certificates kept in memory; the certificates
  -- themselves are read back from disk on demand.
  }
  deriving
    NoThunks
    via OnlyCheckWhnfNamed "PerasImmutableCertDbEnv" (PerasImmutableCertDbEnv m blk)

-- | Run a continuation with the (existentially hidden) 'HasFS' of the
-- database.
withHasFS :: PerasImmutableCertDbEnv m blk -> (forall h. HasFS m h -> a) -> a
withHasFS env k = case picdbHasFS env of SomeHasFS hasFS -> k hasFS

-- | The in-memory index of the database. The two sets are disjoint.
data CertDbState = CertDbState
  { cdsKnownRounds :: !(Set PerasRoundNo)
  -- ^ The round numbers of all certificates stored in the database directory
  -- and not (yet) found to be unreadable or corrupt.
  , cdsQuarantinedRounds :: !(Set PerasRoundNo)
  -- ^ The round numbers of all certificates found to be unreadable or corrupt.
  }
  deriving stock Generic
  deriving anyclass NoThunks

-- | Move a round from the known to the quarantined rounds.
quarantineRound :: PerasRoundNo -> CertDbState -> CertDbState
quarantineRound roundNo CertDbState{cdsKnownRounds, cdsQuarantinedRounds} =
  CertDbState
    { cdsKnownRounds = Set.delete roundNo cdsKnownRounds
    , cdsQuarantinedRounds = Set.insert roundNo cdsQuarantinedRounds
    }

{-------------------------------------------------------------------------------
  API implementation
-------------------------------------------------------------------------------}

-- | Shorthand for the monad in which 'implAddCert' safely modifies
-- 'picdbState': allocated resources (here, a single certificate file)
-- are automatically cleaned up if they don't end up part of the on-disk state.
type ModifyCertDbState m = StateT CertDbState (WithTempRegistry CertDbState m)

implAddCert ::
  forall m blk.
  ( IOLike m
  , IsPerasCert (PerasCert blk) blk
  , EncodeDisk blk (PerasCert blk)
  , DecodeDisk blk (PerasCert blk)
  ) =>
  PerasImmutableCertDbEnv m blk ->
  ValidatedPerasCert blk ->
  m AddPerasImmutableCertResult
implAddCert env cert = do
  result <- modifyWithTempRegistry getSt putSt modifyState
  traceWith (picdbTracer env) (AddedCert roundNo result)
  pure result
 where
  roundNo = getPerasCertRound cert

  getSt :: m CertDbState
  getSt = takeSVar (picdbState env)

  -- Holding the 'StrictSVar' makes the check-then-write in 'modifyState'
  -- atomic with respect to concurrent adds. On abort or exception we restore
  -- the previous state, and 'allocateTemp' removes the uncommitted certificate
  -- file.
  putSt :: CertDbState -> ExitCase CertDbState -> m ()
  putSt before ec =
    putSVar (picdbState env) $ case ec of
      ExitCaseSuccess after -> after
      _ -> before

  modifyState :: ModifyCertDbState m AddPerasImmutableCertResult
  modifyState = do
    CertDbState{cdsKnownRounds = known, cdsQuarantinedRounds = quarantined} <- get
    if Set.member roundNo known
      then lift $ lift $ replaceIfBroken
      else do
        lift $
          allocateTemp
            (writeCertFile env roundNo cert)
            (\() -> removeCertFile env roundNo >> pure True)
            (\st' () -> Set.member roundNo (cdsKnownRounds st'))
        put
          CertDbState
            { cdsKnownRounds = Set.insert roundNo known
            , cdsQuarantinedRounds = Set.delete roundNo quarantined
            }
        -- Release the round from quarantine only once the new certificate
        -- file is in place: should this fail, the new file is cleaned up and
        -- the round stays quarantined.
        when (Set.member roundNo quarantined) $
          lift $
            lift $
              removeQuarantinedCertFile env roundNo
        pure AddedCertToImmutableDB

  -- The round is already stored: check the stored file, and overwrite it
  -- should it be unreadable or corrupt. 'writeCertFile' is atomic, so on
  -- failure the old file is left as it was, still matching the index; hence
  -- no 'allocateTemp' here, which would remove it.
  replaceIfBroken :: m AddPerasImmutableCertResult
  replaceIfBroken =
    readCertFile env roundNo >>= \case
      Right _ -> pure CertAlreadyInImmutableDB
      Left _ -> do
        writeCertFile env roundNo cert
        pure ReplacedCorruptCertInImmutableDB

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
  -- A possibly slightly stale read is fine: certificates added concurrently may
  -- or may not show up in this snapshot.
  rounds <- cdsKnownRounds <$> atomically (readSVarSTM (picdbState env))
  let roundsAfter = snd $ Set.split roundNo rounds
      candidates = take (fromIntegral maxCerts) (Set.toAscList roundsAfter)
  -- Read each certificate on demand. A certificate whose file is unreadable or
  -- corrupt is quarantined rather than failing the whole request, so that the
  -- remaining certificates stay available to syncing nodes.
  fmap catMaybes $ forM candidates $ \r ->
    readCertFile env r >>= \case
      Right cert -> pure (Just cert)
      Left _ -> quarantineCertIfBroken env r

{-------------------------------------------------------------------------------
  Quarantine
-------------------------------------------------------------------------------}

-- | Move a certificate to quarantine and trace why, if its file is still
-- unreadable or corrupt (see 'CertFileError') once the 'StrictSVar' is held.
-- Returns the certificate if it turns out to be intact after all.
--
-- The file must be checked again under the 'StrictSVar' because, since it was
-- first found broken, a concurrent 'implAddCert' may have replaced it with an
-- intact one (see 'replaceIfBroken'), or a concurrent reader may have already
-- quarantined it.
quarantineCertIfBroken ::
  ( IOLike m
  , DecodeDisk blk (PerasCert blk)
  ) =>
  PerasImmutableCertDbEnv m blk ->
  PerasRoundNo ->
  m (Maybe (ValidatedPerasCert blk))
quarantineCertIfBroken env roundNo = do
  (mCert, mErr) <- modifySVar (picdbState env) $ \st ->
    if not (Set.member roundNo (cdsKnownRounds st))
      then pure (st, (Nothing, Nothing))
      else
        readCertFile env roundNo >>= \case
          Right cert -> pure (st, (Just cert, Nothing))
          Left err -> withHasFS env $ \hasFS -> do
            st' <- quarantineCert hasFS roundNo st
            pure (st', (Nothing, Just err))
  forM_ mErr $ traceQuarantinedCert (picdbTracer env) roundNo
  pure mCert

-- | Quarantine the certificate of the given round: both its file (see
-- 'quarantineCertFile') and its round in the in-memory index (see
-- 'quarantineRound').
quarantineCert ::
  IOLike m =>
  HasFS m h ->
  PerasRoundNo ->
  CertDbState ->
  m CertDbState
quarantineCert hasFS roundNo st = do
  quarantineCertFile hasFS roundNo
  pure (quarantineRound roundNo st)

-- | Trace that the certificate of the given round was quarantined, and why.
traceQuarantinedCert ::
  Monad m =>
  Tracer m (TraceEvent blk) ->
  PerasRoundNo ->
  CertFileError ->
  m ()
traceQuarantinedCert tracer roundNo err =
  traceWith tracer (QuarantinedCert roundNo (displayCertFileError err))

-- | Quarantine the file of a certificate by atomically renaming it to its
-- quarantined name (see 'fsPathQuarantinedCertFile'). If the file is missing,
-- an empty file is created under the quarantined name instead, so that the
-- round is still known to be quarantined after a restart.
--
-- Since 'renameFile' is atomic, a crash leaves the round either stored or
-- quarantined, never both.
quarantineCertFile ::
  IOLike m =>
  HasFS m h ->
  PerasRoundNo ->
  m ()
quarantineCertFile hasFS roundNo = do
  exists <- doesFileExist hasFS path
  if exists
    then renameFile hasFS path quarantinedPath
    else withFile hasFS quarantinedPath (WriteMode AllowExisting) $ \_ -> pure ()
 where
  path = fsPathCertFile roundNo
  quarantinedPath = fsPathQuarantinedCertFile roundNo

-- | Remove the quarantined file of a certificate, once a new certificate for
-- its round was stored.
removeQuarantinedCertFile ::
  IOLike m =>
  PerasImmutableCertDbEnv m blk ->
  PerasRoundNo ->
  m ()
removeQuarantinedCertFile env roundNo =
  withHasFS env $ \hasFS -> removeFileIfExists hasFS (fsPathQuarantinedCertFile roundNo)

{-------------------------------------------------------------------------------
  On-disk serialisation
-------------------------------------------------------------------------------}

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

-- | Serialise a certificate into the self-verifying bytes stored on disk: a
-- CRC32 of the certificate payload followed by the (inline) certificate
-- encoding produced by 'encodeCert'.
--
-- Placing @payload@ /last/ lets us both fold the CRC over it and append it to
-- the already-serialised CRC in a single streaming pass, so the payload is
-- serialised exactly once.
--
-- The CRC lets 'decodeCertFile' detect corruption (including a partial write
-- that somehow slipped past the atomic rename in 'writeCertFile') without
-- having to re-run the certificate's (expensive) semantic validation, which
-- already happened before the certificate was ever written here. It is also
-- what keeps the format self-guarding: there is no version tag, but a file
-- whose layout does not match this encoding will fail either the CRC check or
-- decoding rather than be served as a valid-but-wrong certificate.
encodeCertFileBytes ::
  EncodeDisk blk (PerasCert blk) =>
  CodecConfig blk ->
  ValidatedPerasCert blk ->
  BSL.ByteString
encodeCertFileBytes ccfg cert =
  crc <> payload
 where
  payload :: BSL.ByteString
  payload = serialize $ encodeCert ccfg cert
  crc :: BSL.ByteString
  crc = serialize $ toCBOR (getCRC (computeCRC payload))

-- | Decode and integrity-check the bytes produced by 'encodeCertFileBytes',
-- returning the reason on any failure.
decodeCertFile ::
  DecodeDisk blk (PerasCert blk) =>
  CodecConfig blk ->
  BSL.ByteString ->
  Either CertFileError (ValidatedPerasCert blk)
decodeCertFile ccfg fileBytes = do
  -- Decoding only the leading CRC leaves the inline payload as the unconsumed
  -- suffix, which is exactly the byte range the CRC was computed over on write.
  (payload, expectedCRC) <-
    first (CertFileMalformed . asDeserialiseFailure) $
      CBOR.deserialiseFromBytes decodeCRC fileBytes
  unless (getCRC (computeCRC payload) == expectedCRC) $
    Left CertFileChecksumMismatch
  first CertFileMalformed $
    decodeFullDecoder (pack "Immutable Peras Certificate") (decodeCert ccfg) payload
 where
  decodeCRC :: Decoder s Word32
  decodeCRC = fromCBOR
  asDeserialiseFailure =
    DecoderErrorDeserialiseFailure (pack "Immutable Peras Certificate file")

{-------------------------------------------------------------------------------
  Certificate file I/O
-------------------------------------------------------------------------------}

-- | Read, integrity-check and decode the certificate of the given round
-- number, returning a 'CertFileError' if the file is missing, unreadable or
-- corrupt.
readCertFile ::
  forall m blk.
  ( IOLike m
  , DecodeDisk blk (PerasCert blk)
  ) =>
  PerasImmutableCertDbEnv m blk ->
  PerasRoundNo ->
  m (Either CertFileError (ValidatedPerasCert blk))
readCertFile env roundNo =
  withHasFS env $ \hasFS ->
    readCertFileAt (picdbCodecConfig env) hasFS (fsPathCertFile roundNo)

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

writeCertFile ::
  ( IOLike m
  , EncodeDisk blk (PerasCert blk)
  ) =>
  PerasImmutableCertDbEnv m blk ->
  PerasRoundNo ->
  ValidatedPerasCert blk ->
  m ()
writeCertFile env roundNo cert =
  withHasFS env $ \hasFS -> do
    -- Write to a temporary file first, then rename it into place.
    -- This ensures that a crash mid-write can never leave a partial
    -- file under the committed name: any @<round>.cert@ that exists is
    -- guaranteed to be complete. Leftover @<round>.cert.tmp@ files are
    -- dealt with on the next open; see 'recoverTempCertFiles'. One may also
    -- be left behind by an earlier failed attempt, so remove it first.
    removeFileIfExists hasFS tmpPath
    withFile hasFS tmpPath (WriteMode MustBeNew) $ \h ->
      void $ hPutAll hasFS h bytes
    renameFile hasFS tmpPath path
 where
  tmpPath = fsPathTmpCertFile roundNo
  path = fsPathCertFile roundNo
  bytes = encodeCertFileBytes (picdbCodecConfig env) cert

-- | Remove the file of a certificate.
--
-- Only used to clean up a certificate file that was written by 'addCert' but
-- never made it into 'picdbState' because of an exception.
removeCertFile ::
  PerasImmutableCertDbEnv m blk ->
  PerasRoundNo ->
  m ()
removeCertFile env roundNo =
  withHasFS env $ \hasFS -> removeFile hasFS (fsPathCertFile roundNo)

{-------------------------------------------------------------------------------
  Database layout
-------------------------------------------------------------------------------}

-- | The database directory, relative to the database's 'HasFS', as a list of
-- path components.
rootDir :: [String]
rootDir = []

-- | The extension shared by all certificate files. A directory entry without
-- this extension is not a certificate file and is ignored when indexing.
certFileExtension :: String
certFileExtension = ".cert"

-- | The suffix appended to the name of a certificate file when it is
-- quarantined (see 'quarantineCertFile'). Keeping quarantined files in the
-- database directory lets them be quarantined with an atomic 'renameFile'.
quarantinedSuffix :: String
quarantinedSuffix = ".quarantined"

-- | The suffix appended to a certificate file name while it is being written,
-- before it is atomically renamed into place. See 'writeCertFile'.
tmpSuffix :: String
tmpSuffix = ".tmp"

-- | The name of the file storing the certificate of the given round number.
--
-- The round number is encoded in the file name (and nowhere else), so that it
-- can be recovered without reading the file, see 'certRoundFromFileName'.
-- E.g. @42.cert@ for round 42.
certFileName :: PerasRoundNo -> String
certFileName roundNo = show (unPerasRoundNo roundNo) <> certFileExtension

-- | The file storing the certificate of the given round, e.g. @42.cert@ for
-- round 42.
fsPathCertFile :: PerasRoundNo -> FsPath
fsPathCertFile roundNo = mkFsPath (rootDir <> [certFileName roundNo])

-- | The quarantined counterpart of 'fsPathCertFile', e.g.
-- @42.cert.quarantined@ for round 42. Only the name of this file matters:
-- its contents (the corrupt bytes, if any) are never read.
fsPathQuarantinedCertFile :: PerasRoundNo -> FsPath
fsPathQuarantinedCertFile roundNo =
  mkFsPath (rootDir <> [certFileName roundNo <> quarantinedSuffix])

-- | The temporary file a certificate is written to before being renamed into
-- place, e.g. @42.cert.tmp@ for round 42.
fsPathTmpCertFile :: PerasRoundNo -> FsPath
fsPathTmpCertFile roundNo = mkFsPath (rootDir <> [certFileName roundNo <> tmpSuffix])

-- | Recover the round number of a certificate from its file name, or 'Nothing'
-- if the name is not a well-formed certificate file name. Inverse of
-- 'certFileName': e.g. @42.cert@ gives round 42.
certRoundFromFileName :: String -> Maybe PerasRoundNo
certRoundFromFileName name = do
  digits <- stripSuffix certFileExtension name
  roundNo <- PerasRoundNo <$> readMaybe digits
  -- Only the canonical name of a round is accepted, as only that name is ever
  -- looked up (see 'fsPathCertFile'). 'readMaybe' alone would also accept, say,
  -- @042.cert@, @ 42.cert@ or @(42).cert@ (and wrap @-1.cert@ around to
  -- 'maxBound'), each of which would index a round whose canonical file does
  -- not exist.
  guard (certFileName roundNo == name)
  pure roundNo

-- | Recover the round number of a certificate from the name of its temporary
-- file, or 'Nothing' if the name is not a well-formed temporary certificate
-- file name. Inverse of the naming in 'fsPathTmpCertFile': e.g. @42.cert.tmp@
-- gives round 42, but @foo.tmp@ and @42.cert.quarantined.tmp@ give 'Nothing'.
certRoundFromTmpFileName :: String -> Maybe PerasRoundNo
certRoundFromTmpFileName name =
  stripSuffix tmpSuffix name >>= certRoundFromFileName

-- | Recover the round number of a certificate from the name of its
-- quarantined file, or 'Nothing' if the name is not a well-formed quarantined
-- certificate file name. Inverse of the naming in 'fsPathQuarantinedCertFile':
-- e.g. @42.cert.quarantined@ gives round 42, but @42.cert.tmp.quarantined@
-- gives 'Nothing' (note that no such name is ever created).
certRoundFromQuarantinedFileName :: String -> Maybe PerasRoundNo
certRoundFromQuarantinedFileName name =
  stripSuffix quarantinedSuffix name >>= certRoundFromFileName

{-------------------------------------------------------------------------------
  Trace types
-------------------------------------------------------------------------------}

data TraceEvent blk
  = -- | Number of certificates and of quarantined certificates found on disk
    -- when opening.
    OpenedDB
      Int
      Int
  | -- | The result of attempting to add a certificate for the given round.
    AddedCert PerasRoundNo AddPerasImmutableCertResult
  | -- | A certificate file was found to be unreadable or corrupt and was moved
    -- to quarantine. The 'String' describes the reason (see
    -- 'displayCertFileError').
    QuarantinedCert PerasRoundNo String
  deriving stock (Eq, Show, Generic)

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Reason why a certificate file on disk could not be read back into a
-- 'ValidatedPerasCert'.
--
-- These are never thrown: a certificate whose file is unreadable is
-- /quarantined/ (renamed to its quarantined name and traced) rather than
-- bringing down the whole database, so the remaining certificates stay
-- available to syncing nodes.
data CertFileError
  = -- | The file could not be opened or read (e.g. it is missing).
    CertFileReadError FsError
  | -- | The leading CRC or the certificate payload could not be decoded.
    CertFileMalformed DecoderError
  | -- | The payload's CRC does not match the one stored in the file,
    -- i.e. the file is corrupt (bit rot, a partial write, etc).
    CertFileChecksumMismatch
  | -- | Only a temporary file was found for the certificate: writing it was
    -- interrupted before it was committed (see 'writeCertFile').
    CertFileIncompleteWrite
  deriving stock Show

-- | A short human-readable description of a 'CertFileError', for tracing.
displayCertFileError :: CertFileError -> String
displayCertFileError = \case
  CertFileReadError err -> "Read error: " <> show err
  CertFileMalformed err -> "Malformed: " <> show err
  CertFileChecksumMismatch -> "CRC mismatch"
  CertFileIncompleteWrite -> "Incomplete write"

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

removeFileIfExists ::
  IOLike m =>
  HasFS m h ->
  FsPath ->
  m ()
removeFileIfExists hasFS path = do
  exists <- doesFileExist hasFS path
  when exists $ removeFile hasFS path

stripSuffix :: String -> String -> Maybe String
stripSuffix suffix s = reverse <$> stripPrefix (reverse suffix) (reverse s)
