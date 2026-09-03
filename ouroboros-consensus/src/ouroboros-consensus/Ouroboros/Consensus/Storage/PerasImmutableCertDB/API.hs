{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Ouroboros.Consensus.Storage.PerasImmutableCertDB.API (PerasImmutableCertDB (..), AddPerasImmutableCertResult (..)) where

import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block

-- | API for the 'PerasImmutableCertDB', which stores immutable (aka historical)
-- Peras certificates, ie those relevant for syncing nodes only.
data PerasImmutableCertDB m blk = PerasHistCertDB
  { addCert :: ValidatedPerasCert blk -> m AddPerasImmutableCertResult
  -- ^ Add a certificate
  , getPointCerts :: Point blk -> m [ValidatedPerasCert blk]
  -- ^ Get all the immutable certificates pointing to a block.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasHistCertDB" (PerasImmutableCertDB m blk)

data AddPerasImmutableCertResult
  = AddedCertToImmutableDB
  | CertAlreadyInImmutableDB
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks
