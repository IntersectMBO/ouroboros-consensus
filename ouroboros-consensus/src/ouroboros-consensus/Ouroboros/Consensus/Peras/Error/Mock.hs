{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Concrete Peras error types for the V1 voting protocol.
--
-- NOTE: this module is meant to be imported qualified.
module Ouroboros.Consensus.Peras.Error.Mock
  ( MockPerasError (..)
  ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.SupportsPeras
  ( IsPerasError (..)
  , PerasVotingCommitteeError
  , VoteWeight
  )
import Ouroboros.Consensus.Peras.Types (PerasConversionError)

-- | Collection of voting-related errors for Peras
data MockPerasError blk
  = PerasVotingCommitteeError
      (PerasVotingCommitteeError blk)
  | PerasVotingConversionError
      PerasConversionError
  | PerasQuorumNotReachedError
      VoteWeight

deriving instance
  Show (PerasVotingCommitteeError blk) =>
  Show (MockPerasError blk)
deriving instance
  Eq (PerasVotingCommitteeError blk) =>
  Eq (MockPerasError blk)
deriving instance
  NoThunks (PerasVotingCommitteeError blk) =>
  NoThunks (MockPerasError blk)
deriving instance
  Generic (MockPerasError blk)
deriving instance
  Typeable (MockPerasError blk)
deriving instance
  (Show (PerasVotingCommitteeError blk), Typeable blk) =>
  Exception (MockPerasError blk)

instance IsPerasError (MockPerasError blk) blk where
  injectVotingCommitteeError = PerasVotingCommitteeError
  injectConversionError = PerasVotingConversionError
  injectQuorumNotReachedError = PerasQuorumNotReachedError
