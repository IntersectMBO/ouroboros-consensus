{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}

module Ouroboros.Consensus.Storage.PerasImmutableCertDB.API
  ( PerasImmutableCertDB (..)
  , AddPerasImmutableCertResult (..)
  , PerasImmutableCertDbError (..)
  ) where

import Data.Set (Set)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Util.CallStack (HasCallStack, PrettyCallStack)
import Ouroboros.Consensus.Util.IOLike (Exception (..), STM)

-- | API for the 'PerasImmutableCertDB', which stores immutable (aka historical)
-- Peras certificates, ie those relevant for syncing nodes only.
--
-- = Concurrency
--
-- Like the block ImmutableDB, the same database must not be opened more than
-- once at a time: each open handle keeps its own in-memory index, which would
-- otherwise diverge from the others. This is not checked by the database
-- itself:
--
-- * Across processes, it is ensured by the node's database lock file (see
--   @Ouroboros.Consensus.Node.DbLock@). The lock file lives at the root of the
--   immutable database path, under which this database's directory
--   (@perasimmutablecert/@) is mounted too, also when the immutable and
--   volatile databases are stored on separate paths.
--
-- * Within a process, it is ensured by the ChainDB opening it exactly once and
--   closing it with 'closeDB' before any reopen.
--
-- A single handle may be used concurrently.
data PerasImmutableCertDB m blk = PerasImmutableCertDB
  { closeDB :: HasCallStack => m ()
  -- ^ Close the database. Idempotent.
  --
  -- Calls of the other operations made after this returns throw
  -- 'ClosedDBError'. Calls already in progress are not interrupted: this
  -- waits for an 'addCert' that is already modifying the database to
  -- complete, while a 'getCertsAfter' in progress may still read certificate
  -- files and return them after the database is closed.
  , addCert :: HasCallStack => ValidatedPerasCert blk -> m AddPerasImmutableCertResult
  -- ^ Add a certificate to the Peras immutable certificate database. If a
  -- certificate for the same round is already stored but its file is
  -- unreadable or corrupt, it is replaced by the given one.
  --
  -- Throws 'ClosedDBError' if the database is closed.
  , getCertsAfter :: HasCallStack => PerasRoundNo -> Word64 -> m [ValidatedPerasCert blk]
  -- ^ @'getCertsAfter' roundNo maxCerts@ gets at most @maxCerts@ immutable
  -- certificates with a round number strictly greater than @roundNo@, in
  -- ascending round number order.
  --
  -- Throws 'ClosedDBError' if the database is closed.
  , getMissingRounds :: HasCallStack => STM m (Set PerasRoundNo)
  -- ^ Get the round numbers of the certificates that were found to be
  -- unreadable or corrupt on disk. These are no longer served by
  -- 'getCertsAfter'; adding a certificate for such a round
  -- (e.g. one fetched anew from a peer) releases it from this set.
  --
  -- Throws 'ClosedDBError' if the database is closed.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasImmutableCertDB" (PerasImmutableCertDB m blk)

data AddPerasImmutableCertResult
  = AddedCertToImmutableDB
  | CertAlreadyInImmutableDB
  | -- | A certificate for the same round was already stored, but its file was
    -- unreadable or corrupt, so it was replaced.
    ReplacedCorruptCertInImmutableDB
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

-- | Errors thrown by the 'PerasImmutableCertDB'.
--
-- Failures of individual certificate files are not thrown but quarantined
-- instead, so these only indicate incorrect usage of the database.
data PerasImmutableCertDbError
  = -- | An operation other than 'closeDB' was performed on a closed database.
    ClosedDBError PrettyCallStack
  deriving stock Show

instance Exception PerasImmutableCertDbError where
  displayException ClosedDBError{} =
    "PerasImmutableCertDB used after being closed"
