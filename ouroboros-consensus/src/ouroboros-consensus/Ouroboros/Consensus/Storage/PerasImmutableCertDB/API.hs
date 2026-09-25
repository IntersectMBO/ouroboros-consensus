{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Ouroboros.Consensus.Storage.PerasImmutableCertDB.API (PerasImmutableCertDB (..), AddPerasImmutableCertResult (..)) where

import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block

-- | API for the 'PerasImmutableCertDB', which stores immutable (aka historical)
-- Peras certificates, ie those relevant for syncing nodes only.
data PerasImmutableCertDB m blk = PerasImmutableCertDB
  { addCert :: ValidatedPerasCert blk -> m AddPerasImmutableCertResult
  -- ^ Add a certificate to the Peras immutable certificate database.
  , getCertsAfter :: PerasRoundNo -> Word64 -> m [ValidatedPerasCert blk]
  -- ^ @'getCertsAfter' roundNo maxCerts@ gets at most @maxCerts@ immutable
  -- certificates with a round number strictly greater than @roundNo@, in
  -- ascending round number order.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasImmutableCertDB" (PerasImmutableCertDB m blk)

data AddPerasImmutableCertResult
  = AddedCertToImmutableDB
  | CertAlreadyInImmutableDB
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks
