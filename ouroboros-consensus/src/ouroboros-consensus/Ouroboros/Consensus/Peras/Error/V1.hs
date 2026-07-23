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
module Ouroboros.Consensus.Peras.Error.V1
  ( PerasError (..)
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
import Ouroboros.Consensus.Committee.WFA (WFAError)
import Ouroboros.Consensus.Peras.Voting.Adapter (PerasConversionError)

-- | Collection of voting-related errors for Peras
data PerasError blk
  = PerasVotingWFAError
      WFAError
  | PerasVotingCommitteeError
      (PerasVotingCommitteeError blk)
  | PerasVotingConversionError
      PerasConversionError
  | PerasQuorumNotReachedError
      VoteWeight
  | PerasTemporaryPublicKeyHackError
      String

deriving instance
  Show (PerasVotingCommitteeError blk) =>
  Show (PerasError blk)
deriving instance
  Eq (PerasVotingCommitteeError blk) =>
  Eq (PerasError blk)
deriving instance
  NoThunks (PerasVotingCommitteeError blk) =>
  NoThunks (PerasError blk)
deriving instance
  Generic (PerasError blk)
deriving instance
  ( Typeable blk
  , Show (PerasVotingCommitteeError blk)
  ) =>
  Exception (PerasError blk)

instance IsPerasError (PerasError blk) blk where
  injectVotingCommitteeError = PerasVotingCommitteeError
  injectConversionError = PerasVotingConversionError
  injectQuorumNotReachedError = PerasQuorumNotReachedError
