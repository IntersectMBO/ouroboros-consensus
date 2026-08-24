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
import Ouroboros.Consensus.Committee.Types (VoteWeight)
import Ouroboros.Consensus.Peras.Crypto.Mock
  ( MockPerasCrypto
  , MockPerasVotingCommitteeScheme
  , VotingCommitteeError
  )
import Ouroboros.Consensus.Peras.Voting.Adapter (PerasConversionError)

-- | Collection of voting-related errors for Peras
data MockPerasError blk
  = PerasVotingCommitteeError
      (VotingCommitteeError (MockPerasCrypto blk) (MockPerasVotingCommitteeScheme blk))
  | PerasVotingConversionError
      PerasConversionError
  | PerasQuorumNotReachedError
      VoteWeight
  | InputStakeDistrIsEmpty

deriving instance Show (MockPerasError blk)
deriving instance Eq (MockPerasError blk)
deriving instance NoThunks (MockPerasError blk)
deriving instance Generic (MockPerasError blk)
deriving instance Typeable blk => Exception (MockPerasError blk)
