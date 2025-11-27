{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.PerasVoteDB.API
  ( PerasVoteDB (..)
  , AddPerasVoteResult (..)

    -- * 'PerasVoteSnapshot'
  , PerasVoteSnapshot (..)
  , PerasStakeSnapshot (..)
  , PerasVoteTicketNo
  , zeroPerasVoteTicketNo
  , getPerasCertsFromStakeSnapshot
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime)
import Ouroboros.Consensus.Util.MonadSTM.NormalForm
  ( MonadSTM (STM)
  )
import Ouroboros.Consensus.Util.STM (WithFingerprint (..))

data PerasVoteDB m blk = PerasVoteDB
  { addVote :: WithArrivalTime (ValidatedPerasVote blk) -> m (AddPerasVoteResult blk)
  -- ^ Add a Peras vote to the database. The result indicates whether
  -- the vote was actually added, or if it was already present.
  , getStakeSnapshot :: STM m (PerasStakeSnapshot blk)
  -- ^ Return a view of accumulated vote stake per (point, round)
  --
  -- The underlying 'Fingerprint' is updated every time a new vote is added, but it
  -- stays the same when votes are garbage-collected.
  , getVoteSnapshot :: STM m (PerasVoteSnapshot blk)
  -- ^ Interface to read the known votes, mostly for diffusion
  , garbageCollect :: PerasRoundNo -> m ()
  -- ^ Garbage-collect state strictly older than the given slot number.
  , closeDB :: m ()
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasVoteDB" (PerasVoteDB m blk)

data AddPerasVoteResult blk
  = PerasVoteAlreadyInDB
  | AddedPerasVoteButDidntGenerateNewCert
  | AddedPerasVoteAndGeneratedNewCert (ValidatedPerasCert blk)
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

newtype PerasStakeSnapshot blk = PerasStakeSnapshot
  {unPerasStakeSnapshot :: WithFingerprint (Map (PerasVoteTarget blk) (PerasVoteAggregateStatus blk))}
  deriving Generic
  deriving newtype NoThunks

getPerasCertsFromStakeSnapshot ::
  StandardHash blk =>
  PerasStakeSnapshot blk ->
  Set (ValidatedPerasCert blk)
getPerasCertsFromStakeSnapshot (PerasStakeSnapshot mp) =
  Set.fromList $ Map.elems $ Map.mapMaybe pvasMaybeCert (forgetFingerprint mp)

data PerasVoteSnapshot blk = PerasVoteSnapshot
  { containsVote :: IdOf (PerasVote blk) -> Bool
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
