{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The Peras certificate database, holding both the /volatile/ certificates
-- (kept in memory for chain selection and voting) and the /historical/
-- certificates (kept on disk only, to be served to syncing nodes).
--
-- = Implementation overview
--
-- The in-memory state is split in two parts:
--
-- * The /volatile state/ ('PerasCertDbVolatileState'), a 'StrictTVar' holding
--   the volatile certificates themselves, used by chain selection.
--
-- * The /immutable state/ ('PerasCertDbImmutableState'), a 'StrictSVar' which
--   only holds the round numbers of the historical certificates stored on
--   disk, and of those found to be unreadable or corrupt (/quarantined/). The
--   historical certificates themselves are read back from disk on demand.
--   Every operation that modifies the database directory holds this
--   'StrictSVar', so they happen one at a time.
--
-- Each certificate is stored in its own file in the database directory, named
-- after the Peras round number that uniquely identifies it. Volatile
-- certificates are also eagerly persisted to disk when they are added (see
-- 'persistCert'), in a file with the 'volatileSuffix', so that they survive a
-- restart: they are loaded back into the volatile state on open.
--
-- The volatile, historical and quarantined rounds are always pairwise disjoint.
-- This relies on there being at most one valid certificate per round; see
-- 'implAddCert' and 'implGarbageCollect'.
--
-- A design goal is to never advertise incomplete, unreadable or corrupt
-- historical certificates to clients; for this, prevention and handling of
-- on-disk failures (corruption, partial writes, missing files) rests on the
-- following mechanisms:
--
-- * Certificates are written atomically (to a temporary file that is then
--   renamed into place, see 'writeCertFileAt'), so a certificate file that
--   exists is always complete.
--
-- * Each certificate file is self-verifying: it stores a CRC32 of its payload
--   (see 'encodeCertFileBytes'). Integrity is re-checked whenever a certificate
--   is read back, but they are /not/ semantically re-validated here; that
--   already happened before they were written, and the syncing nodes that
--   consume them are expected to do the validation again themselves.
--
-- * A historical certificate whose file is missing or corrupt is /quarantined/
--   rather than crashing the database: its file is atomically renamed in place
--   by appending 'quarantinedSuffix' to its name (an empty marker file with
--   that name is created if the original was missing), its round is moved from
--   the historical to the quarantined rounds, and the event is traced. The
--   remaining certificates stay available to syncing nodes. Only the name of a
--   quarantined file matters here, never its contents, but the file is left in
--   place as evidence. Quarantined rounds survive restarts, since they are
--   re-indexed from the file names on open; they are reported by
--   'getMissingRounds'. Adding a certificate for a quarantined round with
--   'addCert' does /not/ repair it ('PerasCertAlreadyInDB').
--
-- The 'CheckAllOnOpen' policy alternatively reads and integrity-checks every
-- historical certificate file on open.
--
-- = Opening and closing
--
-- As with the block ImmutableDB, the database must not be opened more than
-- once at a time, since each handle has its own in-memory state (see the
-- API's documentation). This is not checked here: it relies on the node's
-- database lock file across processes, and on the ChainDB opening it once and
-- closing it before reopening within a process. Closing just marks the handle
-- as closed (no file handles are held open), after which new calls of its
-- operations throw 'ClosedDBError' (see 'closeDB' for calls in progress), so
-- a stale handle fails loudly instead of diverging.
--
-- = Life cycle of a certificate
--
-- For round 42, the database directory may hold:
--
-- > 42.cert.volatile      volatile certificate   (42 in the volatile state)
-- > 42.cert               historical certificate (42 in 'HistoricalRounds')
-- > 42.cert.quarantined   quarantine marker      (42 in 'QuarantinedRounds')
-- > 42.cert.volatile.tmp  interrupted write      (transient, resolved on open)
-- > 42.cert.tmp           interrupted write      (transient, resolved on open)
--
-- Any other entry, including a non-canonical name such as @042.cert@, is
-- ignored. A certificate written "atomically" is first written to the
-- corresponding @.tmp@ file, which is then renamed into place.
--
-- > addCert 42
-- >  |
-- >  +-- 42 volatile, historical or quarantined --> PerasCertAlreadyInDB
-- >  +-- boosted block older than last GC slot  --> PerasCertTooOld
-- >  +-- otherwise --> add 42 to the volatile state --> AddedPerasCertToDB,
-- >                    then (outside STM) write 42.cert.volatile atomically,
-- >                    unless 42 was garbage-collected in the meantime
-- >
-- > garbageCollect isImmutable slot (for each volatile cert older than slot)
-- >  |
-- >  remove 42 from the volatile state, then (outside STM):
-- >  |
-- >  +-- boosted block immutable, 42.cert.volatile exists
-- >  |    --> rename it to 42.cert             (42 becomes historical)
-- >  +-- boosted block immutable, no 42.cert.volatile (not yet persisted)
-- >  |    --> write 42.cert atomically         (42 becomes historical)
-- >  +-- boosted block not immutable (on a fork)
-- >       --> delete 42.cert.volatile, if any
-- >
-- > getHistoricalCertsAfter (for each historical round, ascending; here 42)
-- >  |
-- >  read 42.cert --intact--> served
-- >  | missing, unreadable, malformed or CRC mismatch
-- >  v
-- >  42 still historical? --no--> skipped (already quarantined concurrently)
-- >  | yes
-- >  v
-- >  re-read 42.cert --intact--> served
-- >  | still broken
-- >  v
-- >  42.cert exists? --yes--> rename 42.cert to 42.cert.quarantined ---+
-- >  | no                                                              |
-- >  +--> create empty 42.cert.quarantined ----------------------------+
-- >                                                                    v
-- >                                    42 quarantined: no longer served
-- >
-- > openDB
-- >  42.cert.tmp, without 42.cert nor 42.cert.quarantined
-- >      --> rename it to 42.cert.quarantined   (traced as an incomplete write)
-- >  42.cert.tmp, otherwise
-- >      --> delete it
-- >  42.cert.volatile.tmp
-- >      --> delete it                          (the certificate is lost)
-- >  42.cert and 42.cert.quarantined
-- >      --> delete 42.cert.quarantined         (the stored file wins)
-- >  CheckAllOnOpen, broken 42.cert
-- >      --> quarantine it, as in getHistoricalCertsAfter
-- >  42.cert.volatile, with 42.cert or 42.cert.quarantined
-- >      --> delete 42.cert.volatile
-- >  42.cert.volatile, broken
-- >      --> delete it                          (the certificate is lost)
-- >  42.cert.volatile, otherwise
-- >      --> load it into the volatile state
--
-- Renaming and quarantining are single atomic operations, so a crash can only
-- leave behind the @.tmp@ files of interrupted writes, or both @42.cert@ and
-- @42.cert.quarantined@, all of which are resolved on the next open.
module Ouroboros.Consensus.Storage.PerasCertDB.Impl
  ( -- * Opening
    PerasCertDbArgs (..)
  , PerasCertDbIntegrityCheckPolicy (..)
  , defaultArgs
  , openDB

    -- * Trace types
  , TraceEvent (..)
  , PersistCertResult (..)
  , DiscardVolatileCertReason (..)
  )
where

import Cardano.Binary
import qualified Codec.CBOR.Read as CBOR
import Control.Applicative ((<|>))
import Control.Monad (foldM, forM, forM_, guard, unless, void, when, (>=>))
import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import Data.Kind (Type)
import Data.List (stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Maybe.Strict (StrictMaybe (..), strictMaybeToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime (SystemTime (..), WithArrivalTime (..))
import Ouroboros.Consensus.Peras.Weight (PerasWeightSnapshot, mkPerasWeightSnapshot)
import Ouroboros.Consensus.Storage.PerasCertDB.API
import Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..), EncodeDisk (..))
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.Assert (assertWithMsg)
import Ouroboros.Consensus.Util.CallStack (HasCallStack, prettyCallStack)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.MonadSTM.StrictSVar (newSVarWithInvariant)
import Ouroboros.Consensus.Util.STM
import System.FS.API.Lazy
import System.FS.CRC (CRC (..), computeCRC)
import Text.Read (readMaybe)

{-------------------------------------------------------------------------------
  Database state
-------------------------------------------------------------------------------}

data PerasCertDbEnv m blk = PerasCertDbEnv
  { pcdbTracer :: !(Tracer m (TraceEvent blk))
  , pcdbVolatileState :: !(StrictTVar m (WithFingerprint (PerasCertDbVolatileState blk)))
  -- ^ The volatile state of the certificate database.
  , pcdbHasFS :: !(SomeHasFS m)
  , pcdbCodecConfig :: !(CodecConfig blk)
  , pcdbImmutableState :: !(StrictSVar m PerasCertDbInternalState)
  -- ^ The in-memory index of the historical certificates, if the database is
  -- open. The historical certificates themselves are read back from disk on
  -- demand. Holding this 'StrictSVar' also serialises all modifications of the
  -- database directory.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasCertDbEnv" (PerasCertDbEnv m blk)

data PerasCertDbVolatileState blk = PerasCertDbVolatileState
  { pcdvsCerts :: !(Map PerasRoundNo (WithArrivalTime (ValidatedPerasCert blk)))
  -- ^ The volatile certificates by 'PerasRoundNo'.
  --
  -- INVARIANT: disjoint from the historical and quarantined rounds of the
  -- 'PerasCertDbImmutableState'.
  , pcdvsLatestCertSeen ::
      !(StrictMaybe (WithBoostedBlockStatus (WithArrivalTime (ValidatedPerasCert blk))))
  -- ^ The certificate with the highest round number that has been added to the
  -- db since it has been opened.
  , pcdvsLastGcSlot :: !(WithOrigin SlotNo)
  -- ^ The highest slot passed to 'garbageCollect' since the database has been
  -- opened. Certificates boosting blocks older than this are rejected by
  -- 'addCert' ('PerasCertTooOld').
  }

deriving instance
  Show (PerasCert blk) =>
  Show (PerasCertDbVolatileState blk)

deriving instance
  NoThunks (PerasCert blk) =>
  NoThunks (PerasCertDbVolatileState blk)

deriving instance
  Generic (PerasCertDbVolatileState blk)

-- | Whether the database is open, and if so, its in-memory index of the
-- historical certificates.
data PerasCertDbInternalState
  = CertDbClosed
  | CertDbOpen !PerasCertDbImmutableState
  deriving stock Generic
  deriving anyclass NoThunks

data PerasCertDbImmutableState = PerasCertDbImmutableState
  { pcdisHistoricalRounds :: !HistoricalRounds
  -- ^ The round numbers of all historical certificates stored in the database
  -- directory and not (yet) found to be unreadable or corrupt.
  , pcdisQuarantinedRounds :: !QuarantinedRounds
  -- ^ The round numbers of all historical certificates found to be unreadable
  -- or corrupt.
  }
  deriving stock Generic
  deriving anyclass NoThunks

newtype HistoricalRounds = HistoricalRounds
  { getHistoricalRounds :: Set PerasRoundNo
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

newtype QuarantinedRounds = QuarantinedRounds
  { getQuarantinedRounds :: Set PerasRoundNo
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NoThunks

-- | The 'PerasCertDbInternalState' invariant: historical and quarantined
-- rounds are disjoint sets.
--
-- Their disjointness from the volatile rounds is checked separately, see
-- 'assertVolatileDisjoint'.
disjointInvariant :: PerasCertDbInternalState -> Maybe String
disjointInvariant =
  \case
    CertDbClosed -> Nothing
    CertDbOpen
      ( st@PerasCertDbImmutableState
          { pcdisHistoricalRounds = HistoricalRounds historical
          , pcdisQuarantinedRounds = QuarantinedRounds quarantined
          }
        ) ->
        -- The 'noThunksInvariant' helps ensure the state is free
        -- of unexpected thunks and stays strict. Being an expensive check,
        -- it is gated behind a flag, so it usually does 'Nothing' here.
        disjointInvariantCheck <|> noThunksInvariant st
       where
        disjointInvariantCheck
          | Set.disjoint historical quarantined = Nothing
          | otherwise = Just "Found a redundant quarantined certificate"

-- | The historical and the quarantined rounds.
nonVolatileRounds :: PerasCertDbImmutableState -> Set PerasRoundNo
nonVolatileRounds
  PerasCertDbImmutableState
    { pcdisHistoricalRounds = HistoricalRounds historical
    , pcdisQuarantinedRounds = QuarantinedRounds quarantined
    } = Set.union historical quarantined

-- | Assert (when assertions are enabled) that the given volatile rounds are
-- disjoint from the historical and quarantined ones.
assertVolatileDisjoint ::
  HasCallStack =>
  Set PerasRoundNo ->
  PerasCertDbImmutableState ->
  a ->
  a
assertVolatileDisjoint volatile st =
  assertWithMsg $
    if Set.disjoint volatile (nonVolatileRounds st)
      then Right ()
      else
        Left $
          "PerasCertDB: volatile rounds overlap with historical or quarantined ones: "
            <> show (Set.toList (Set.intersection volatile (nonVolatileRounds st)))

throwClosedDBError :: (MonadThrow m, HasCallStack) => m a
throwClosedDBError = throwIO $ ClosedDBError prettyCallStack

-- | Read the index of the open database, non-blocking, throwing 'ClosedDBError'
-- if it is closed. The index may be slightly stale if it is being modified
-- concurrently.
readOpenStateSTM ::
  (IOLike m, HasCallStack) =>
  PerasCertDbEnv m blk ->
  STM m PerasCertDbImmutableState
readOpenStateSTM env =
  readSVarSTM (pcdbImmutableState env) >>= \case
    CertDbOpen st -> pure st
    CertDbClosed -> throwClosedDBError

-- | Like 'modifySVar' on the index of the open database, throwing
-- 'ClosedDBError' (and leaving it closed) if it is closed.
modifyOpenState ::
  (IOLike m, HasCallStack) =>
  PerasCertDbEnv m blk ->
  (PerasCertDbImmutableState -> m (PerasCertDbImmutableState, a)) ->
  m a
modifyOpenState env f =
  modifySVar (pcdbImmutableState env) $ \case
    CertDbClosed -> throwClosedDBError
    CertDbOpen st -> first CertDbOpen <$> f st

-- | Read the volatile state, throwing 'ClosedDBError' if the database is
-- closed.
readOpenVolatileState ::
  (IOLike m, HasCallStack) =>
  PerasCertDbEnv m blk ->
  STM m (WithFingerprint (PerasCertDbVolatileState blk))
readOpenVolatileState env = do
  _ <- readOpenStateSTM env
  readTVar (pcdbVolatileState env)

-- | Move a round from the historical to the quarantined rounds.
quarantineRound :: PerasRoundNo -> PerasCertDbImmutableState -> PerasCertDbImmutableState
quarantineRound
  roundNo
  PerasCertDbImmutableState
    { pcdisHistoricalRounds = HistoricalRounds historical
    , pcdisQuarantinedRounds = QuarantinedRounds quarantined
    } =
    PerasCertDbImmutableState
      { pcdisHistoricalRounds =
          HistoricalRounds $ Set.delete roundNo historical
      , pcdisQuarantinedRounds =
          QuarantinedRounds $ Set.insert roundNo quarantined
      }

-- | Add a round to the historical rounds.
addHistoricalRound :: PerasRoundNo -> PerasCertDbImmutableState -> PerasCertDbImmutableState
addHistoricalRound roundNo st =
  st
    { pcdisHistoricalRounds =
        HistoricalRounds $ Set.insert roundNo $ getHistoricalRounds $ pcdisHistoricalRounds st
    }

{-------------------------------------------------------------------------------
  Trace types
-------------------------------------------------------------------------------}

data TraceEvent blk
  = AddCert
      PerasRoundNo
      (WithArrivalTime (ValidatedPerasCert blk))
      AddPerasCertResult
  | GarbageCollected SlotNo
  | OpenedDB
      -- | Number of historical certificates
      Int
      -- | Number of quarantined (missing) historical certificates
      Int
      -- | Number of volatile certificates loaded from disk
      Int
  | -- | The result of attempting to persist a volatile certificate for the
    -- given round.
    PersistedCert PerasRoundNo PersistCertResult
  | -- | The volatile certificate of the given round was promoted to a
    -- historical one.
    PromotedCert PerasRoundNo
  | -- | The volatile certificate file of the given round was discarded.
    DiscardedVolatileCert PerasRoundNo DiscardVolatileCertReason
  | -- | A historical certificate file was found to be unreadable or corrupt and
    -- was moved to quarantine. The 'String' describes the reason (see
    -- 'displayCertFileError').
    QuarantinedCert PerasRoundNo String
  | -- | The database was closed.
    ClosedDB

deriving instance
  Show (PerasCert blk) =>
  Show (TraceEvent blk)

deriving instance
  Eq (PerasCert blk) =>
  Eq (TraceEvent blk)

deriving instance
  Generic (TraceEvent blk)

-- | The result of 'persistCert'.
data PersistCertResult
  = -- | The certificate was written to its volatile certificate file.
    PersistedVolatileCert
  | -- | The certificate was garbage-collected before it could be persisted, so
    -- it was not (see 'implGarbageCollect').
    SkippedGarbageCollectedCert
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

-- | Why a volatile certificate (file) was discarded.
data DiscardVolatileCertReason
  = -- | Its boosted block is not part of the immutable chain (it is on a fork)
    -- once garbage-collected.
    BoostedBlockNotImmutable
  | -- | A historical or quarantined certificate already exists for its round.
    RoundAlreadyHistorical
  | -- | Its file is unreadable or corrupt (see 'displayCertFileError').
    CorruptVolatileCertFile String
  | -- | Writing its file was interrupted.
    InterruptedVolatileCertWrite
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

{------------------------------------------------------------------------------
  Creating the database
------------------------------------------------------------------------------}

type PerasCertDbArgs :: (Type -> Type) -> (Type -> Type) -> Type -> Type
data PerasCertDbArgs f m blk = PerasCertDbArgs
  { pcdbaTracer :: Tracer m (TraceEvent blk)
  , pcdbaCodecConfig :: HKD f (CodecConfig blk)
  , pcdbaHasFS :: HKD f (SomeHasFS m)
  -- ^ For on-disk storage.
  , pcdbaSystemTime :: HKD f (SystemTime m)
  -- ^ Used to assign an arrival time to the volatile certificates loaded from
  -- disk when opening.
  , pcdbaIntegrityCheckPolicy :: PerasCertDbIntegrityCheckPolicy
  -- ^ How thoroughly to check the historical certificate files on disk when
  -- opening.
  }

-- | How much of the on-disk state to validate when opening the database.
--
-- Regardless of the policy, corruption is always detected when a
-- certificate is actually served; the policy only controls whether we
-- additionally pay for an eager, up-front integrity sweep.
data PerasCertDbIntegrityCheckPolicy
  = -- | Trust the certificate file names to build the index and defer all
    -- integrity checks to read time. Opening is cheap. This is the default.
    CheckOnRead
  | -- | Additionally read and verify every certificate file when opening,
    -- quarantining any that are unreadable or corrupt. Opening is @O(n)@ in
    -- disk reads, but a corrupt certificate is never advertised to clients.
    CheckAllOnOpen
  deriving stock (Eq, Show, Generic)

defaultArgs :: Monad m => Incomplete PerasCertDbArgs m blk
defaultArgs =
  PerasCertDbArgs
    { pcdbaTracer = nullTracer
    , pcdbaCodecConfig = noDefault
    , pcdbaHasFS = noDefault
    , pcdbaSystemTime = noDefault
    , pcdbaIntegrityCheckPolicy = CheckOnRead
    }

openDB ::
  forall m blk.
  ( IOLike m
  , BlockSupportsPeras blk
  , EncodeDisk blk (PerasCert blk)
  , DecodeDisk blk (PerasCert blk)
  ) =>
  Complete PerasCertDbArgs m blk ->
  m (PerasCertDB m blk)
openDB
  PerasCertDbArgs
    { pcdbaTracer
    , pcdbaCodecConfig
    , pcdbaHasFS = someHasFS@(SomeHasFS hasFS)
    , pcdbaSystemTime
    , pcdbaIntegrityCheckPolicy
    } = do
    createDirectoryIfMissing hasFS True (mkFsPath rootDir)
    -- Deal with any leftover temporary files from a certificate write that
    -- was interrupted (e.g. by a crash) before its atomic rename, see
    -- 'writeCertFileAt'.
    recoverTempCertFiles pcdbaTracer hasFS
    -- Index the historical certificate files present on disk by recovering
    -- their round numbers from their file names; the certificates themselves
    -- are read back from disk on demand.
    initialState <- scanInitialCertDbState hasFS
    st <- case pcdbaIntegrityCheckPolicy of
      CheckOnRead -> pure initialState
      CheckAllOnOpen ->
        scrubCorruptHistoricalCerts pcdbaTracer pcdbaCodecConfig hasFS initialState
    volatileCerts <-
      loadVolatileCerts pcdbaTracer pcdbaCodecConfig pcdbaSystemTime hasFS st
    pcdbImmutableState <-
      assertVolatileDisjoint (Map.keysSet volatileCerts) st $
        newSVarWithInvariant disjointInvariant (CertDbOpen st)
    pcdbVolatileState <-
      newTVarIO $
        WithFingerprint
          PerasCertDbVolatileState
            { pcdvsCerts = volatileCerts
            , pcdvsLatestCertSeen = SNothing
            , pcdvsLastGcSlot = Origin
            }
          (Fingerprint 0)
    let env =
          PerasCertDbEnv
            { pcdbTracer = pcdbaTracer
            , pcdbVolatileState
            , pcdbHasFS = someHasFS
            , pcdbCodecConfig = pcdbaCodecConfig
            , pcdbImmutableState
            }
    traceWith pcdbaTracer $
      OpenedDB
        (Set.size (getHistoricalRounds $ pcdisHistoricalRounds st))
        (Set.size (getQuarantinedRounds $ pcdisQuarantinedRounds st))
        (Map.size volatileCerts)
    pure
      PerasCertDB
        { addCert = implAddCert env
        , getVolatileCertIds = implGetVolatileCertIds env
        , getVolatileCertsAfter = implGetVolatileCertsAfter env
        , getWeightSnapshot = implGetWeightSnapshot env
        , getLatestCertSeen = implGetLatestCertSeen env
        , garbageCollect = implGarbageCollect env
        , getHistoricalCertsAfter = implGetHistoricalCertsAfter env
        , getMissingRounds = implGetMissingRounds env
        , closeDB = implCloseDB env
        }

-- *  Opening: recovery and indexing

-- | Deal with the leftover temporary certificate files in the database
-- directory (see 'writeCertFileAt'). Called once when opening, before indexing.
--
-- A leftover temporary volatile certificate file (e.g. @42.cert.volatile.tmp@)
-- is removed: that certificate is lost, but it can be added again later.
--
-- A leftover temporary historical certificate file means that promoting a
-- certificate (that had not been persisted as volatile) was interrupted. If no
-- historical certificate file for its round was committed (nor quarantined),
-- that certificate is missing from the database, so the temporary file is
-- renamed to its quarantined name: this makes the round known as quarantined.
-- Otherwise, the temporary file is just removed. For example, @42.cert.tmp@ is
-- renamed to @42.cert.quarantined@ if neither @42.cert@ nor
-- @42.cert.quarantined@ exist, and removed otherwise.
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
  forM_ (mapMaybe certRoundFromVolatileTmpFileName (Set.toList names)) $ \roundNo -> do
    removeFile hasFS (fsPathTmpVolatileCertFile roundNo)
    traceWith tracer (DiscardedVolatileCert roundNo InterruptedVolatileCertWrite)
  forM_ (mapMaybe certRoundFromTmpFileName (Set.toList names)) $ \roundNo ->
    if Set.member (certFileName roundNo) names
      || Set.member (certFileName roundNo <> quarantinedSuffix) names
      then removeFile hasFS (fsPathTmpCertFile roundNo)
      else do
        renameFile hasFS (fsPathTmpCertFile roundNo) (fsPathQuarantinedCertFile roundNo)
        traceQuarantinedCert tracer roundNo CertFileIncompleteWrite

-- | Build the initial index from the historical and the quarantined rounds
-- found by scanning the database directory. The resulting state holds the
-- 'disjointInvariant'.
--
-- The round number of each certificate is recovered from its file name (see
-- 'certFileName' and 'fsPathQuarantinedCertFile'), so the certificates
-- themselves are not read or decoded here; that happens on demand in
-- 'readCertFile' or when using `CheckAllOnOpen`. Directory entries that are
-- not well-formed certificate file names are ignored.
--
-- The quarantine marker of every round that is both stored and quarantined
-- is removed. The stored file takes precedence: should it be broken, it will
-- be quarantined again.
scanInitialCertDbState ::
  IOLike m =>
  HasFS m h ->
  m PerasCertDbImmutableState
scanInitialCertDbState hasFS = do
  names <- Set.toList <$> listDirectory hasFS (mkFsPath rootDir)
  let historical = Set.fromList $ mapMaybe certRoundFromFileName names
      quarantined = Set.fromList $ mapMaybe certRoundFromQuarantinedFileName names
      staleQuarantined = Set.intersection historical quarantined
  forM_ staleQuarantined $ removeFile hasFS . fsPathQuarantinedCertFile
  pure
    PerasCertDbImmutableState
      { pcdisHistoricalRounds = HistoricalRounds historical
      , pcdisQuarantinedRounds = QuarantinedRounds $ Set.difference quarantined staleQuarantined
      }

-- | Eagerly read and integrity-check every historical certificate, moving any
-- unreadable or corrupt one to quarantine (tracing it via 'QuarantinedCert'),
-- so that it is never advertised to clients. Used by the 'CheckAllOnOpen'
-- policy.
scrubCorruptHistoricalCerts ::
  ( IOLike m
  , DecodeDisk blk (PerasCert blk)
  ) =>
  Tracer m (TraceEvent blk) ->
  CodecConfig blk ->
  HasFS m h ->
  PerasCertDbImmutableState ->
  m PerasCertDbImmutableState
scrubCorruptHistoricalCerts tracer ccfg hasFS st =
  foldM validateCert st (getHistoricalRounds $ pcdisHistoricalRounds st)
 where
  validateCert st' roundNo =
    readCertFileAt ccfg hasFS (fsPathCertFile roundNo) >>= \case
      Right _ -> pure st'
      Left err -> do
        st'' <- quarantineCert hasFS roundNo st'
        traceQuarantinedCert tracer roundNo err
        pure st''

-- | Load the volatile certificates persisted on disk (see 'persistCert'), to
-- be put in the volatile state.
--
-- A volatile certificate file is deleted (and the event traced) instead of
-- being loaded if its round is already historical or quarantined (a crash
-- happened before a stale volatile file could be deleted), or if it is
-- unreadable or corrupt.
--
-- The arrival time of the loaded certificates is the current time, as their
-- original arrival time is not persisted.
loadVolatileCerts ::
  forall m h blk.
  ( IOLike m
  , IsPerasCert (PerasCert blk) blk
  , DecodeDisk blk (PerasCert blk)
  ) =>
  Tracer m (TraceEvent blk) ->
  CodecConfig blk ->
  SystemTime m ->
  HasFS m h ->
  PerasCertDbImmutableState ->
  m (Map PerasRoundNo (WithArrivalTime (ValidatedPerasCert blk)))
loadVolatileCerts tracer ccfg systemTime hasFS st = do
  names <- Set.toList <$> listDirectory hasFS (mkFsPath rootDir)
  let volatileRounds = mapMaybe certRoundFromVolatileFileName names
  certs <- fmap catMaybes $ forM volatileRounds $ \roundNo ->
    if Set.member roundNo (nonVolatileRounds st)
      then discard roundNo RoundAlreadyHistorical
      else
        readCertFileAt ccfg hasFS (fsPathVolatileCertFile roundNo) >>= \case
          Right cert
            | getPerasCertRound cert == roundNo -> pure (Just (roundNo, cert))
            | otherwise ->
                discard roundNo $
                  CorruptVolatileCertFile $
                    "Round mismatch: found certificate for round "
                      <> show (unPerasRoundNo (getPerasCertRound cert))
          Left err -> discard roundNo (CorruptVolatileCertFile (displayCertFileError err))
  if null certs
    then pure Map.empty
    else do
      -- Only read the current time when there are certificates to load.
      now <- systemTimeCurrent systemTime
      pure $ Map.fromList [(roundNo, WithArrivalTime now cert) | (roundNo, cert) <- certs]
 where
  discard ::
    PerasRoundNo ->
    DiscardVolatileCertReason ->
    m (Maybe (PerasRoundNo, ValidatedPerasCert blk))
  discard roundNo reason = do
    removeFile hasFS (fsPathVolatileCertFile roundNo)
    traceWith tracer (DiscardedVolatileCert roundNo reason)
    pure Nothing

{-------------------------------------------------------------------------------
  API implementation
-------------------------------------------------------------------------------}

-- | See 'addCert'.
--
-- A certificate is only added if its round is neither volatile, historical
-- nor quarantined, and its boosted block is not older than the last garbage
-- collection. The latter check makes sure that a certificate removed from the
-- volatile state by 'implGarbageCollect' (but not yet made historical) can not
-- be re-added, which, together with the assumption that there is at most one
-- valid certificate per round, keeps the volatile rounds disjoint from the
-- historical and quarantined ones.
--
-- The historical and quarantined rounds are read without blocking, so they
-- may be slightly stale while being concurrently modified by
-- 'implGarbageCollect' or 'implGetHistoricalCertsAfter'. That is fine: the
-- former only adds rounds older than the last garbage collection, which are
-- rejected regardless, and the latter only moves rounds from historical to
-- quarantined.
--
-- TODO: we will need to update this method with non-trivial validation logic
-- see https://github.com/tweag/cardano-peras/issues/120
implAddCert ::
  ( IOLike m
  , IsPerasCert (PerasCert blk) blk
  , EncodeDisk blk (PerasCert blk)
  ) =>
  PerasCertDbEnv m blk ->
  WithArrivalTime (ValidatedPerasCert blk) ->
  STM m (m AddPerasCertResult)
implAddCert env@PerasCertDbEnv{pcdbTracer, pcdbVolatileState} cert = do
  immSt <- readOpenStateSTM env
  WithFingerprint pcds fp <- readTVar pcdbVolatileState
  addPerasCertRes <-
    if
      | Map.member roundNo (pcdvsCerts pcds)
          || Set.member roundNo (nonVolatileRounds immSt) ->
          pure PerasCertAlreadyInDB
      | pointSlot (getPerasCertPoint cert) < pcdvsLastGcSlot pcds ->
          pure PerasCertTooOld
      | otherwise -> do
          let pcdsLatestCertSeen' =
                case pcdvsLatestCertSeen pcds of
                  SNothing ->
                    SJust (CertBoostingBlockInVolatileDB cert)
                  SJust prev
                    | roundNo > getPerasCertRound (forgetBoostedBlockStatus prev) ->
                        SJust (CertBoostingBlockInVolatileDB cert)
                    | otherwise ->
                        SJust prev
          writeTVar pcdbVolatileState $
            WithFingerprint
              pcds
                { pcdvsCerts = Map.insert roundNo cert (pcdvsCerts pcds)
                , pcdvsLatestCertSeen = pcdsLatestCertSeen'
                }
              (succ fp)
          pure AddedPerasCertToDB
  pure $ do
    traceWith pcdbTracer (AddCert roundNo cert addPerasCertRes)
    case addPerasCertRes of
      AddedPerasCertToDB -> persistCert env (forgetArrivalTime cert)
      PerasCertAlreadyInDB -> pure ()
      PerasCertTooOld -> pure ()
    pure addPerasCertRes
 where
  roundNo = getPerasCertRound cert

implGetWeightSnapshot ::
  ( IOLike m
  , StandardHash blk
  , IsPerasCert (PerasCert blk) blk
  ) =>
  PerasCertDbEnv m blk ->
  STM m (WithFingerprint (PerasWeightSnapshot blk))
implGetWeightSnapshot env = do
  WithFingerprint pcds fp <- readOpenVolatileState env
  let weights =
        mkPerasWeightSnapshot
          [ (getPerasCertPoint cert, vpcCertBoost (forgetArrivalTime cert))
          | cert <- Map.elems (pcdvsCerts pcds)
          ]
  pure (WithFingerprint weights fp)

implGetVolatileCertIds ::
  IOLike m =>
  PerasCertDbEnv m blk ->
  STM m (Set PerasRoundNo)
implGetVolatileCertIds env =
  Map.keysSet . pcdvsCerts . forgetFingerprint <$> readOpenVolatileState env

implGetVolatileCertsAfter ::
  IOLike m =>
  PerasCertDbEnv m blk ->
  PerasRoundNo ->
  STM m (Map PerasRoundNo (m (WithArrivalTime (ValidatedPerasCert blk))))
implGetVolatileCertsAfter env roundNo = do
  PerasCertDbVolatileState{pcdvsCerts} <-
    forgetFingerprint <$> readOpenVolatileState env
  let strictlyGreater = snd $ Map.split roundNo pcdvsCerts
  pure $ pure <$> strictlyGreater

implGetLatestCertSeen ::
  IOLike m =>
  PerasCertDbEnv m blk ->
  STM m (Maybe (WithBoostedBlockStatus (WithArrivalTime (ValidatedPerasCert blk))))
implGetLatestCertSeen env = do
  PerasCertDbVolatileState{pcdvsLatestCertSeen} <-
    forgetFingerprint <$> readOpenVolatileState env
  pure $ strictMaybeToMaybe pcdvsLatestCertSeen

-- | See 'garbageCollect'.
--
-- The STM transaction removes the garbage-collected certificates from the
-- volatile state, and the resulting 'm' action then deals with each of them
-- on disk, in ascending round order, while holding the 'StrictSVar':
--
-- * If its boosted block is immutable, its volatile certificate file is
--   renamed in place to its historical certificate file. If the volatile file
--   does not exist (yet), e.g. because the 'persistCert' action of 'addCert'
--   has not run yet or was never run, the historical file is written from the
--   in-memory certificate instead; 'persistCert' then skips it.
--
-- * Otherwise, its volatile certificate file is deleted, if it exists.
implGarbageCollect ::
  forall m blk.
  ( IOLike m
  , IsPerasCert (PerasCert blk) blk
  , EncodeDisk blk (PerasCert blk)
  ) =>
  PerasCertDbEnv m blk ->
  (Point blk -> m Bool) ->
  SlotNo ->
  STM m (m ())
implGarbageCollect env@PerasCertDbEnv{pcdbTracer, pcdbVolatileState} isImmutable slotNo = do
  WithFingerprint pcds fp <- readOpenVolatileState env
  let (collected, pcdsCerts') = Map.partition isOld (pcdvsCerts pcds)
  -- No need to update the 'Fingerprint' as we only remove certificates that do
  -- not matter for comparing interesting chains.
  writeTVar pcdbVolatileState $
    WithFingerprint
      PerasCertDbVolatileState
        { pcdvsCerts = pcdsCerts'
        , pcdvsLatestCertSeen =
            updateIfBoostingGarbageCollectedBlock <$> pcdvsLatestCertSeen pcds
        , pcdvsLastGcSlot = max (NotOrigin slotNo) (pcdvsLastGcSlot pcds)
        }
      fp
  pure $ do
    forM_ (Map.toAscList collected) $ \(roundNo, cert) -> do
      immutable <- isImmutable (getPerasCertPoint cert)
      promoteOrDiscardCert env immutable roundNo (forgetArrivalTime cert)
    traceWith pcdbTracer (GarbageCollected slotNo)
 where
  isOld :: IsPerasCert cert blk => cert -> Bool
  isOld cert = pointSlot (getPerasCertPoint cert) < NotOrigin slotNo

  -- Update the latest certificate seen status when its corresponding
  -- boosted block gets garbage collected.
  updateIfBoostingGarbageCollectedBlock cert
    | isOld (forgetBoostedBlockStatus cert) =
        CertBoostingBlockNoLongerInVolatileDB (forgetBoostedBlockStatus cert)
    | otherwise =
        cert

-- | Promote a garbage-collected volatile certificate to a historical one, or
-- discard it, depending on whether its boosted block is immutable. See
-- 'implGarbageCollect'.
promoteOrDiscardCert ::
  forall m blk.
  ( IOLike m
  , HasCallStack
  , EncodeDisk blk (PerasCert blk)
  ) =>
  PerasCertDbEnv m blk ->
  -- | Whether the boosted block is immutable
  Bool ->
  PerasRoundNo ->
  ValidatedPerasCert blk ->
  m ()
promoteOrDiscardCert env immutable roundNo cert = do
  ev <- modifyOpenState env $ \st -> withHasFS env $ \hasFS -> do
    hasVolatileFile <- doesFileExist hasFS volatilePath
    if
      | not immutable -> do
          when hasVolatileFile $ removeFile hasFS volatilePath
          pure (st, DiscardedVolatileCert roundNo BoostedBlockNotImmutable)
      | Set.member roundNo (nonVolatileRounds st) -> do
          -- Can not happen if there is at most one valid certificate per round.
          when hasVolatileFile $ removeFile hasFS volatilePath
          pure (st, DiscardedVolatileCert roundNo RoundAlreadyHistorical)
      | otherwise -> do
          if hasVolatileFile
            then renameFile hasFS volatilePath (fsPathCertFile roundNo)
            else
              writeCertFileAt
                (pcdbCodecConfig env)
                hasFS
                (fsPathTmpCertFile roundNo)
                (fsPathCertFile roundNo)
                cert
          let st' = addHistoricalRound roundNo st
          volatileRounds <-
            Map.keysSet . pcdvsCerts . forgetFingerprint
              <$> readTVarIO (pcdbVolatileState env)
          assertVolatileDisjoint volatileRounds st' $
            pure (st', PromotedCert roundNo)
  traceWith (pcdbTracer env) ev
 where
  volatilePath = fsPathVolatileCertFile roundNo

-- | Persist a certificate just added to the volatile state to its volatile
-- certificate file, while holding the 'StrictSVar'.
--
-- The certificate is not persisted if it was garbage-collected from the
-- volatile state in the meantime: 'implGarbageCollect' then takes care of it
-- on disk. Conversely, should it be garbage-collected after being checked
-- here, 'implGarbageCollect' can only deal with it on disk once it has been
-- persisted, as it needs to hold the 'StrictSVar' to do so.
persistCert ::
  forall m blk.
  ( IOLike m
  , HasCallStack
  , IsPerasCert (PerasCert blk) blk
  , EncodeDisk blk (PerasCert blk)
  ) =>
  PerasCertDbEnv m blk ->
  ValidatedPerasCert blk ->
  m ()
persistCert env cert = do
  result <- modifyOpenState env $ \st -> do
    stillVolatile <-
      Map.member roundNo . pcdvsCerts . forgetFingerprint
        <$> readTVarIO (pcdbVolatileState env)
    if stillVolatile
      then withHasFS env $ \hasFS -> do
        writeCertFileAt
          (pcdbCodecConfig env)
          hasFS
          (fsPathTmpVolatileCertFile roundNo)
          (fsPathVolatileCertFile roundNo)
          cert
        pure (st, PersistedVolatileCert)
      else pure (st, SkippedGarbageCollectedCert)
  traceWith (pcdbTracer env) (PersistedCert roundNo result)
 where
  roundNo = getPerasCertRound cert

implGetHistoricalCertsAfter ::
  forall m blk.
  ( IOLike m
  , HasCallStack
  , DecodeDisk blk (PerasCert blk)
  ) =>
  PerasCertDbEnv m blk ->
  PerasRoundNo ->
  Word64 ->
  m [ValidatedPerasCert blk]
implGetHistoricalCertsAfter env roundNo maxCerts = do
  -- A possibly slightly stale read is fine: certificates promoted concurrently
  -- may or may not show up in this snapshot.
  HistoricalRounds rounds <- pcdisHistoricalRounds <$> atomically (readOpenStateSTM env)
  let roundsAfter = snd $ Set.split roundNo rounds
      candidates = take (fromIntegral maxCerts) (Set.toAscList roundsAfter)
  -- Read each certificate on demand. A certificate whose file is unreadable or
  -- corrupt is quarantined rather than failing the whole request, so that the
  -- remaining certificates stay available to syncing nodes. The file is
  -- re-checked once the 'StrictSVar' is held, to stay consistent with a
  -- concurrent reader (see 'quarantineCertIfStillBroken').
  fmap catMaybes $ forM candidates $ \r ->
    readCertFile env r >>= \case
      Right cert -> pure (Just cert)
      Left _ -> quarantineCertIfStillBroken env r

implGetMissingRounds ::
  IOLike m =>
  PerasCertDbEnv m blk ->
  STM m (Set PerasRoundNo)
implGetMissingRounds env =
  getQuarantinedRounds . pcdisQuarantinedRounds <$> readOpenStateSTM env

implCloseDB :: IOLike m => PerasCertDbEnv m blk -> m ()
implCloseDB env =
  swapSVar (pcdbImmutableState env) CertDbClosed >>= \case
    CertDbClosed -> pure ()
    CertDbOpen _ -> traceWith (pcdbTracer env) ClosedDB

-- | Run a continuation with the (existentially hidden) 'HasFS' of the
-- database.
withHasFS :: PerasCertDbEnv m blk -> (forall h. HasFS m h -> a) -> a
withHasFS env k = case pcdbHasFS env of SomeHasFS hasFS -> k hasFS

{-------------------------------------------------------------------------------
  Quarantine
-------------------------------------------------------------------------------}

-- | Move a historical certificate to quarantine and trace why, if its file is
-- still unreadable or corrupt (see 'CertFileError') once the 'StrictSVar' is
-- held. Returns the certificate if it turns out to be intact after all.
--
-- The file must be checked again under the 'StrictSVar' because, since it was
-- first found broken, a concurrent reader may have already quarantined it.
quarantineCertIfStillBroken ::
  ( IOLike m
  , HasCallStack
  , DecodeDisk blk (PerasCert blk)
  ) =>
  PerasCertDbEnv m blk ->
  PerasRoundNo ->
  m (Maybe (ValidatedPerasCert blk))
quarantineCertIfStillBroken env roundNo = do
  (mCert, mErr) <- modifyOpenState env $
    \st@PerasCertDbImmutableState{pcdisHistoricalRounds = HistoricalRounds historical} ->
      if not $ Set.member roundNo historical
        -- The cert was quarantined by a concurrent reader.
        then pure (st, (Nothing, Nothing))
        else
          readCertFile env roundNo >>= \case
            -- The cert is intact after all (e.g. a transient read error).
            Right cert -> pure (st, (Just cert, Nothing))
            -- Cert is bad indeed.
            Left err -> withHasFS env $ \hasFS -> do
              st' <- quarantineCert hasFS roundNo st
              pure (st', (Nothing, Just err))
  forM_ mErr $ traceQuarantinedCert (pcdbTracer env) roundNo
  pure mCert

-- | Quarantine the historical certificate of the given round: both its file
-- (see 'quarantineCertFile') and its round in the in-memory index (see
-- 'quarantineRound').
quarantineCert ::
  IOLike m =>
  HasFS m h ->
  PerasRoundNo ->
  PerasCertDbImmutableState ->
  m PerasCertDbImmutableState
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

-- | Quarantine the file of a historical certificate by atomically renaming it
-- to its quarantined name (see 'fsPathQuarantinedCertFile'). If the file is
-- missing, an empty file is created under the quarantined name instead, so
-- that the round is still known to be quarantined after a restart.
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

{-------------------------------------------------------------------------------
  On-disk serialisation
-------------------------------------------------------------------------------}

-- | Serialise a certificate into the self-verifying bytes stored on disk: a
-- CRC32 of the certificate payload followed by the (inline) certificate
-- encoding produced by 'encodeDisk'.
--
-- The CRC lets 'decodeCertFile' detect corruption (including a partial write
-- that somehow slipped past the atomic rename in 'writeCertFileAt') without
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
  -- We place the @payload@ /last/ so that we get it as the inconsumed suffix
  -- when decoding the leading CRC.
  crc <> payload
 where
  payload :: BSL.ByteString
  payload = serialize $ encodeDisk ccfg cert
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
    decodeFullDecoder (pack "Immutable Peras Certificate") (decodeDisk ccfg) payload
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
  PerasCertDbEnv m blk ->
  PerasRoundNo ->
  m (Either CertFileError (ValidatedPerasCert blk))
readCertFile env roundNo =
  withHasFS env $ \hasFS ->
    readCertFileAt (pcdbCodecConfig env) hasFS (fsPathCertFile roundNo)

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

-- | Atomically write a certificate to the given file, by first writing it to
-- the given temporary file and then renaming that into place.
--
-- This ensures that a crash mid-write can never leave a partial file under
-- the committed name: any certificate file that exists is guaranteed to be
-- complete. Leftover temporary files are dealt with on the next open; see
-- 'recoverTempCertFiles'. One may also be left behind by an earlier failed
-- attempt, so it is removed first.
writeCertFileAt ::
  ( IOLike m
  , EncodeDisk blk (PerasCert blk)
  ) =>
  CodecConfig blk ->
  HasFS m h ->
  -- | Temporary file
  FsPath ->
  -- | Committed file
  FsPath ->
  ValidatedPerasCert blk ->
  m ()
writeCertFileAt ccfg hasFS tmpPath path cert = do
  removeFileIfExists hasFS tmpPath
  withFile hasFS tmpPath (WriteMode MustBeNew) $ \h ->
    void $ hPutAll hasFS h (encodeCertFileBytes ccfg cert)
  renameFile hasFS tmpPath path

removeFileIfExists ::
  IOLike m =>
  HasFS m h ->
  FsPath ->
  m ()
removeFileIfExists hasFS path = do
  exists <- doesFileExist hasFS path
  when exists $ removeFile hasFS path

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
-- before it is atomically renamed into place. See 'writeCertFileAt'.
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

-- | The suffix appended to the name of a certificate file for a /volatile/
-- certificate (see 'persistCert'). It is removed (by an atomic 'renameFile')
-- when the certificate becomes historical.
volatileSuffix :: String
volatileSuffix = ".volatile"

-- | The file storing the volatile certificate of the given round, e.g.
-- @42.cert.volatile@ for round 42.
fsPathVolatileCertFile :: PerasRoundNo -> FsPath
fsPathVolatileCertFile roundNo =
  mkFsPath (rootDir <> [certFileName roundNo <> volatileSuffix])

-- | The temporary file a volatile certificate is written to before being
-- renamed into place, e.g. @42.cert.volatile.tmp@ for round 42.
fsPathTmpVolatileCertFile :: PerasRoundNo -> FsPath
fsPathTmpVolatileCertFile roundNo =
  mkFsPath (rootDir <> [certFileName roundNo <> volatileSuffix <> tmpSuffix])

-- | Recover the round number of a certificate from the name of its volatile
-- file, e.g. @42.cert.volatile@ gives round 42.
certRoundFromVolatileFileName :: String -> Maybe PerasRoundNo
certRoundFromVolatileFileName =
  stripSuffix volatileSuffix >=> certRoundFromFileName

-- | Recover the round number of a certificate from the name of its temporary
-- volatile file, e.g. @42.cert.volatile.tmp@ gives round 42.
certRoundFromVolatileTmpFileName :: String -> Maybe PerasRoundNo
certRoundFromVolatileTmpFileName =
  stripSuffix tmpSuffix >=> certRoundFromVolatileFileName

stripSuffix :: String -> String -> Maybe String
stripSuffix suffix s = reverse <$> stripPrefix (reverse suffix) (reverse s)

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
    -- interrupted before it was committed (see 'writeCertFileAt').
    CertFileIncompleteWrite
  deriving stock Show

-- | A short human-readable description of a 'CertFileError', for tracing.
displayCertFileError :: CertFileError -> String
displayCertFileError = \case
  CertFileReadError err -> "Read error: " <> show err
  CertFileMalformed err -> "Malformed: " <> show err
  CertFileChecksumMismatch -> "CRC mismatch"
  CertFileIncompleteWrite -> "Incomplete write"
