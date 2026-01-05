{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.PerasVoteDB.API
  ( PerasVoteDB (..)
  , AddPerasVoteResult (..)

    -- * 'PerasVoteSnapshot'
  , PerasVoteSnapshot (..)
  , PerasVoteTicketNo
  , zeroPerasVoteTicketNo
  ) where

import Data.Map (Map)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block.SupportsPeras
  ( PerasCert
  , ValidatedPerasCert (..)
  , ValidatedPerasVote
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime)
import Ouroboros.Consensus.Peras.Round (PerasRoundNo)
import Ouroboros.Consensus.Peras.Vote (PerasVoteId)
import Ouroboros.Consensus.Util.MonadSTM.NormalForm (MonadSTM (STM))

data PerasVoteDB m blk = PerasVoteDB
  { addVote :: WithArrivalTime (ValidatedPerasVote blk) -> m (AddPerasVoteResult blk)
  -- ^ Add a Peras vote to the database. The result indicates whether
  -- the vote was actually added, or if it was already present.
  , getVoteSnapshot :: STM m (PerasVoteSnapshot blk)
  -- ^ Interface to read the known votes, mostly for diffusion
  , getForgedCertForRound :: PerasRoundNo -> STM m (Maybe (ValidatedPerasCert blk))
  -- ^ Get the certificate if quorum was reached for the given round.
  , garbageCollect :: PerasRoundNo -> m ()
  -- ^ Garbage-collect state strictly older than the given slot number.
  , closeDB :: m ()
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasVoteDB" (PerasVoteDB m blk)

data AddPerasVoteResult blk
  = PerasVoteAlreadyInDB
  | AddedPerasVoteButDidntGenerateNewCert
  | AddedPerasVoteAndGeneratedNewCert (ValidatedPerasCert blk)

deriving instance Show (PerasCert blk) => Show (AddPerasVoteResult blk)
deriving instance Eq (PerasCert blk) => Eq (AddPerasVoteResult blk)
deriving instance Ord (PerasCert blk) => Ord (AddPerasVoteResult blk)
deriving instance Generic (AddPerasVoteResult blk)
deriving instance NoThunks (PerasCert blk) => NoThunks (AddPerasVoteResult blk)

data PerasVoteSnapshot blk = PerasVoteSnapshot
  { containsVote :: PerasVoteId blk -> Bool
  , getVotesAfter ::
      PerasVoteTicketNo ->
      Map PerasVoteTicketNo (WithArrivalTime (ValidatedPerasVote blk))
  -- ^ Get votes after the given ticket number (excluded).
  -- The result is a map of ticket numbers to validated votes.
  }

-- | A sequence number, incremented every time we receive a new vote.
newtype PerasVoteTicketNo = PerasVoteTicketNo Word64
  deriving stock Show
  deriving newtype (Eq, Ord, Enum, NoThunks)

zeroPerasVoteTicketNo :: PerasVoteTicketNo
zeroPerasVoteTicketNo = PerasVoteTicketNo 0
