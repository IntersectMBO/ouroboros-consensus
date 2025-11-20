{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime)
import Ouroboros.Consensus.Peras.Weight
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM (WithFingerprint (..))

data PerasVoteDB m blk = PerasVoteDB
  { addVote :: WithArrivalTime (ValidatedPerasVote blk) -> m AddPerasVoteResult
  -- ^ Add a Peras vote to the database. The result indicates whether
  -- the vote was actually added, or if it was already present.
  , getWeightSnapshot :: STM m (WithFingerprint (PerasWeightSnapshot blk))
  -- ^ Return the Peras weights in order compare the current selection against
  -- potential candidate chains, namely the weights for blocks not older than
  -- the current immutable tip. It might contain weights for even older blocks
  -- if they have not yet been garbage-collected.
  --
  -- The 'Fingerprint' is updated every time a new vote is added, but it
  -- stays the same when votes are garbage-collected.
  , getVoteSnapshot :: STM m (PerasVoteSnapshot blk)
  , getLatestVoteSeen :: STM m (Maybe (WithArrivalTime (ValidatedPerasVote blk)))
  -- ^ Get the vote with the highest round number that has been added to
  -- the db since it has been opened. This vote is not affected by garbage
  -- collection, but it's forgotten when the db is closed.
  --
  -- NOTE: having seen a vote is a precondition to start voting in every
  -- round except for the first one (at origin). As a consequence, only caught-up
  -- nodes can actively participate in the Peras protocol for now.
  , garbageCollect :: SlotNo -> m ()
  -- ^ Garbage-collect state older than the given slot number.
  , closeDB :: m ()
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasVoteDB" (PerasVoteDB m blk)

data AddPerasVoteResult = AddedPerasVoteToDB | PerasVoteAlreadyInDB
  deriving stock (Show, Eq)

data PerasVoteSnapshot blk = PerasVoteSnapshot
  { containsVote :: PerasRoundNo -> Bool
  -- ^ Do we have the vote for this round?
  , getVotesAfter ::
      PerasVoteTicketNo ->
      Map PerasVoteTicketNo (WithArrivalTime (ValidatedPerasVote blk))
  -- ^ Get votes after the given ticket number (excluded).
  -- The result is a map of ticket numbers to validated votes.
  }

-- | A sequence number, incremented every time we receive a new vote.
--
-- Note that we will /usually/ receive votes monotonically by round
-- number, so round numbers could /almost/ fulfill the role of ticket numbers.
-- However, in certain edge cases (while catching up, or during cooldowns), this
-- might not be true, such as during syncing or during cooldown periods.
-- Therefore, for robustness, we choose to maintain dedicated ticket numbers
-- separately.
newtype PerasVoteTicketNo = PerasVoteTicketNo Word64
  deriving stock Show
  deriving newtype (Eq, Ord, Enum, NoThunks)

zeroPerasVoteTicketNo :: PerasVoteTicketNo
zeroPerasVoteTicketNo = PerasVoteTicketNo 0
