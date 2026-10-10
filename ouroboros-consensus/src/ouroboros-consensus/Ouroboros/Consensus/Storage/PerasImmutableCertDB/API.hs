{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Ouroboros.Consensus.Storage.PerasImmutableCertDB.API (PerasImmutableCertDB (..), AddPerasImmutableCertResult (..)) where

import Data.Set (Set)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Util.IOLike (STM)

-- | API for the 'PerasImmutableCertDB', which stores immutable (aka historical)
-- Peras certificates, ie those relevant for syncing nodes only.
data PerasImmutableCertDB m blk = PerasImmutableCertDB
  { addCert :: ValidatedPerasCert blk -> m AddPerasImmutableCertResult
  -- ^ Add a certificate to the Peras immutable certificate database. If a
  -- certificate for the same round is already stored but its file is
  -- unreadable or corrupt, it is replaced by the given one.
  , getCertsAfter :: PerasRoundNo -> Word64 -> m [ValidatedPerasCert blk]
  -- ^ @'getCertsAfter' roundNo maxCerts@ gets at most @maxCerts@ immutable
  -- certificates with a round number strictly greater than @roundNo@, in
  -- ascending round number order.
  , getMissingRounds :: STM m (Set PerasRoundNo)
  -- ^ Get the round numbers of the certificates that were found to be
  -- unreadable or corrupt on disk. These are no longer served by
  -- 'getCertsAfter'; adding a certificate for such a round
  -- (e.g. one fetched anew from a peer) releases it from this set.
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
