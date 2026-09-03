{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Ouroboros.Consensus.Storage.PerasHistCertDB.API (PerasHistCertDB (..), AddPerasHistCertResult (..)) where

import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block

-- | API for the 'PerasHistCertDB' database, which stores historical Peras certificates,
-- i.e. those that are no longer relevant for the current chain selection.
data PerasHistCertDB m blk = PerasHistCertDB
  { appendCert :: ValidatedPerasCert blk -> m AddPerasHistCertResult
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasHistCertDB" (PerasHistCertDB m blk)

data AddPerasHistCertResult
  = AddedCertToHistDB
  | CertAlreadyInHistDB
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks
