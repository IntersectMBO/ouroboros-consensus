{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ouroboros.Consensus.Storage.PerasVoteDB.API
  ( PerasVoteDB (..)
  , AddPerasVoteResult (..)
  , TraceEvent (..)

    -- * 'PerasVoteSnapshot'
  , PerasVoteTicketNo
  , zeroPerasVoteTicketNo
  , STMWithTracing (..)
  , getSTMWithoutTraceEvents
  ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime)
import Ouroboros.Consensus.Util.MonadSTM.NormalForm (MonadSTM (..))

{-------------------------------------------------------------------------------
  Tracing utilities to work around STM
-------------------------------------------------------------------------------}

newtype STMWithTracing tevent m a = STMWithTracing {getSTMWithTraceEvents :: STM m ([tevent], a)}

instance MonadSTM m => Functor (STMWithTracing tevent m) where
  fmap f (STMWithTracing x) = STMWithTracing $ fmap (\(events, a) -> (events, f a)) x

instance MonadSTM m => Applicative (STMWithTracing tevent m) where
  pure a = STMWithTracing $ pure ([], a)
  STMWithTracing fs <*> STMWithTracing xs = STMWithTracing $ do
    (events1, f) <- fs
    (events2, x) <- xs
    pure (events1 ++ events2, f x)

instance MonadSTM m => Monad (STMWithTracing tevent m) where
  return = pure
  STMWithTracing xs >>= k = STMWithTracing $ do
    (events1, x) <- xs
    let STMWithTracing ys = k x
    (events2, y) <- ys
    pure (events1 ++ events2, y)

getSTMWithoutTraceEvents :: MonadSTM m => STMWithTracing tevent m a -> STM m a
getSTMWithoutTraceEvents (STMWithTracing xs) = snd <$> xs

{------------------------------------------------------------------------------}

-- TODO: for consistency I made all functions of the API return `STMWithTracing`
-- but for the getters it might make sense to keep them as simple `STM m a`.
-- If that is the case, we can get rid of the two variants for
-- `STMWithTracing` and just keep the `WithTracing` one
data PerasVoteDB m blk = PerasVoteDB
  { atomicallyWithTracing ::
      forall a.
      STMWithTracing (TraceEvent blk) m a ->
      m a
  , addVote ::
      WithArrivalTime (ValidatedPerasVote blk) ->
      STMWithTracing (TraceEvent blk) m (AddPerasVoteResult blk)
  -- ^ Add a Peras vote to the database. The result indicates whether
  -- the vote was actually added, or if it was already present.
  , getVoteIds :: STMWithTracing (TraceEvent blk) m (Set (PerasVoteId blk))
  , getVotesAfter ::
      PerasVoteTicketNo ->
      STMWithTracing (TraceEvent blk) m (Map PerasVoteTicketNo (WithArrivalTime (ValidatedPerasVote blk)))
  , getForgedCertForRound ::
      PerasRoundNo ->
      STMWithTracing (TraceEvent blk) m (Maybe (ValidatedPerasCert blk))
  -- ^ Get the certificate if quorum was reached for the given round.
  , garbageCollect :: PerasRoundNo -> STMWithTracing (TraceEvent blk) m ()
  -- ^ Garbage-collect state strictly older than the given slot number.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "PerasVoteDB" (PerasVoteDB m blk)

-- | A sequence number, incremented every time we receive a new vote.
newtype PerasVoteTicketNo = PerasVoteTicketNo Word64
  deriving stock Show
  deriving newtype (Eq, Ord, Enum, NoThunks)

zeroPerasVoteTicketNo :: PerasVoteTicketNo
zeroPerasVoteTicketNo = PerasVoteTicketNo 0

data AddPerasVoteResult blk
  = PerasVoteAlreadyInDB
  | AddedPerasVoteButDidntGenerateNewCert
  | AddedPerasVoteAndGeneratedNewCert (ValidatedPerasCert blk)
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

data TraceEvent blk
  = OpenedPerasVoteDB
  | AddingPerasVote (PerasVoteTarget blk) (PerasVoteId blk) PerasVoteStake
  | AddedPerasVote (PerasVoteId blk)
  | IgnoredVoteAlreadyInDB (PerasVoteId blk)
  | GeneratedPerasCert (ValidatedPerasCert blk)
  | GarbageCollected PerasRoundNo
  deriving stock (Show, Eq, Generic)
