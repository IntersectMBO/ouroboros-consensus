{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ouroboros.Consensus.Storage.PerasVoteDB.API
  ( PerasVoteDB (..)
  , AddPerasVoteResult (..)
  , TraceEvent (..)

    -- * 'PerasVoteSnapshot'
  , PerasVoteTicketNo
  , zeroPerasVoteTicketNo
  , STMWithTracing
  , withoutTracing
  , withTracing
  , keepTraceEvents
  , forgetTraceEvents
  , atomicallyWithTracing
  ) where

import Control.Tracer (Tracer, traceWith)
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

-- NOTE: the representation using two variants is rather arbitrary, we could
-- use a single variant record, with a 'Maybe (Tracer m tevent)' field
-- (or default to `nullTracer`) instead.
data STMWithTracing tevent m a
  = WithoutTracing !(STM m a)
  | -- | We do not expose the constructors, the the tracer cannot leak
    WithTracing !(Tracer m tevent) !(STM m (a, [tevent]))

keepTraceEvents :: MonadSTM m => STMWithTracing tevent m a -> STM m (a, [tevent])
keepTraceEvents (WithoutTracing stm) = (,[]) <$> stm
keepTraceEvents (WithTracing _ stm) = stm

forgetTraceEvents :: MonadSTM m => STMWithTracing tevent m a -> STM m a
forgetTraceEvents (WithoutTracing stm) = stm
forgetTraceEvents (WithTracing _ stm) = fst <$> stm

withTracing :: MonadSTM m => Tracer m tevent -> STM m (a, [tevent]) -> STMWithTracing tevent m a
withTracing = WithTracing

withoutTracing :: MonadSTM m => STM m a -> STMWithTracing tevent m a
withoutTracing = WithoutTracing

instance MonadSTM m => Functor (STMWithTracing tevent m) where
  fmap f (WithoutTracing stm) = WithoutTracing (fmap f stm)
  fmap f (WithTracing tracer stm) = WithTracing tracer (fmap (\(a, tevents) -> (f a, tevents)) stm)

atomicallyWithTracing :: MonadSTM m => STMWithTracing tevent m a -> m a
atomicallyWithTracing (WithoutTracing stm) = atomically stm
atomicallyWithTracing (WithTracing tracer stm) = do
  (res, traceEvents) <- atomically stm
  mapM_ (traceWith tracer) traceEvents
  pure res

{------------------------------------------------------------------------------}

-- TODO: for consistency I made all functions of the API return `STMWithTracing`
-- but for the getters it might make sense to keep them as simple `STM m a`.
-- If that is the case, we can get rid of the two variants for
-- `STMWithTracing` and just keep the `WithTracing` one
data PerasVoteDB m blk = PerasVoteDB
  { addVote ::
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
  deriving stock (Generic, Show)
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
