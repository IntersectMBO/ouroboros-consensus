{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Concrete Peras error types for the V1 voting protocol.
--
-- NOTE: this module is meant to be imported qualified.
module Ouroboros.Consensus.Peras.Error.V1
  ( PerasError (..)
  ) where

import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.SupportsPeras
  ( PerasCommitteeScheme
  , PerasCrypto
  )
import Ouroboros.Consensus.Committee.Class (CryptoSupportsVotingCommittee (..))
import Ouroboros.Consensus.Committee.WFA (WFAError)
import Ouroboros.Consensus.Peras.Types (PerasConversionError)

-- | Collection of voting-related errors for Peras
data PerasError blk
  = PerasVotingWFAError
      WFAError
  | PerasVotingCommitteeError
      (VotingCommitteeError (PerasCrypto blk) (PerasCommitteeScheme blk))
  | PerasVotingConversionError
      PerasConversionError
  | PerasTemporaryPublicKeyHackError
      String

deriving instance
  Show (VotingCommitteeError (PerasCrypto blk) (PerasCommitteeScheme blk)) =>
  Show (PerasError blk)
deriving instance
  Eq (VotingCommitteeError (PerasCrypto blk) (PerasCommitteeScheme blk)) =>
  Eq (PerasError blk)
deriving instance
  NoThunks (VotingCommitteeError (PerasCrypto blk) (PerasCommitteeScheme blk)) =>
  NoThunks (PerasError blk)
deriving instance
  Generic (PerasError blk)
