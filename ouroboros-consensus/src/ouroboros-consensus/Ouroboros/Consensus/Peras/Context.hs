{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Peras.Context
  ( PerasEpochContextResolver
  , BoundedPerasEpochContext (..)
  , resolveRoundNo
  , PerasEpochContextNotFoundForRound (..)
  , PerasEpochContextResolverHandle (..)
  , verifyPerasVoteInContext
  , verifyPerasCertInContext
  , resolveRoundNoWithHandle
  , constPerasEpochContextResolverHandle
  )
where

import Codec.Serialise (Serialise)
import Control.Applicative (Alternative (..))
import Control.Exception (Exception)
import Control.Monad.Class.MonadSTM (STM)
import Data.Either.Extra (maybeToEither)
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , IsPerasCert (getPerasCertRound)
  , PerasCert
  , PerasRoundNo
  , PerasVote
  , ValidatedPerasCert
  , ValidatedPerasVote
  , getPerasVoteRound
  )
import Ouroboros.Consensus.Storage.Serialisation (DecodeDisk, EncodeDisk (..))
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , MonadSTM
  , MonadThrow
  , NoThunks
  , newTVarIO
  , readTVar
  , throwSTM
  )

data PerasEpochContextResolver blk
  = ConstPerasEpochContextResolver
      (PerasEpochContext blk)
  | BoundedPerasEpochContextResolver
      (BoundedPerasEpochContext blk)
      (Maybe (BoundedPerasEpochContext blk))

deriving instance Show (PerasEpochContext blk) => Show (PerasEpochContextResolver blk)
deriving instance Eq (PerasEpochContext blk) => Eq (PerasEpochContextResolver blk)
deriving instance NoThunks (PerasEpochContext blk) => NoThunks (PerasEpochContextResolver blk)
deriving instance Generic (PerasEpochContextResolver blk)

deriving instance Serialise (PerasEpochContext blk) => Serialise (PerasEpochContextResolver blk)
instance Serialise (PerasEpochContext blk) => EncodeDisk blk (PerasEpochContextResolver blk)
instance Serialise (PerasEpochContext blk) => DecodeDisk blk (PerasEpochContextResolver blk)

data BoundedPerasEpochContext blk
  = BoundedPerasEpochContext
  { startPerasRoundNo :: PerasRoundNo -- inclusive
  , endPerasRoundNo :: PerasRoundNo -- exclusive
  , epochContext :: PerasEpochContext blk
  }

deriving instance Show (PerasEpochContext blk) => Show (BoundedPerasEpochContext blk)
deriving instance Eq (PerasEpochContext blk) => Eq (BoundedPerasEpochContext blk)
deriving instance NoThunks (PerasEpochContext blk) => NoThunks (BoundedPerasEpochContext blk)
deriving instance Generic (BoundedPerasEpochContext blk)

deriving instance Serialise (PerasEpochContext blk) => Serialise (BoundedPerasEpochContext blk)
instance Serialise (PerasEpochContext blk) => EncodeDisk blk (BoundedPerasEpochContext blk)
instance Serialise (PerasEpochContext blk) => DecodeDisk blk (BoundedPerasEpochContext blk)

withinEpochContext ::
  PerasRoundNo ->
  BoundedPerasEpochContext blk ->
  Maybe (PerasEpochContext blk)
withinEpochContext roundNo boundedContext
  | roundNo >= startPerasRoundNo boundedContext
      && roundNo < endPerasRoundNo boundedContext =
      Just $ epochContext boundedContext
  | otherwise =
      Nothing

resolveRoundNo ::
  PerasEpochContextResolver blk ->
  PerasRoundNo ->
  Either PerasEpochContextNotFoundForRound (PerasEpochContext blk)
resolveRoundNo resolver roundNo =
  case resolver of
    ConstPerasEpochContextResolver context ->
      Right context
    BoundedPerasEpochContextResolver currEpochContext mbPrevEpochContext ->
      maybeToEither (PerasEpochContextNotFoundForRound roundNo) $
        withinEpochContext roundNo currEpochContext
          <|> (withinEpochContext roundNo =<< mbPrevEpochContext)

data PerasEpochContextNotFoundForRound = PerasEpochContextNotFoundForRound PerasRoundNo
  deriving (Show, Eq, Generic, NoThunks, Exception)

newtype PerasEpochContextResolverHandle m blk
  = PerasEpochContextResolverHandle (STM m (PerasEpochContextResolver blk))

constPerasEpochContextResolverHandle ::
  ( IOLike m
  , NoThunks (PerasEpochContext blk)
  ) =>
  PerasEpochContext blk -> m (PerasEpochContextResolverHandle m blk)
constPerasEpochContextResolverHandle context = do
  let resolver = ConstPerasEpochContextResolver context
  resolverVar <- newTVarIO resolver
  pure $ PerasEpochContextResolverHandle (readTVar resolverVar)

resolveRoundNoWithHandle ::
  MonadSTM m =>
  PerasEpochContextResolverHandle m blk ->
  PerasRoundNo ->
  STM m (Either PerasEpochContextNotFoundForRound (PerasEpochContext blk))
resolveRoundNoWithHandle (PerasEpochContextResolverHandle resolverHandle) roundNo = do
  resolver <- resolverHandle
  pure $ resolveRoundNo resolver roundNo

verifyPerasVoteInContext ::
  ( MonadSTM m
  , MonadThrow (STM m)
  , BlockSupportsPeras blk
  ) =>
  PerasEpochContextResolverHandle m blk ->
  PerasVote blk ->
  STM m (ValidatedPerasVote blk)
verifyPerasVoteInContext handle vote = do
  let roundNo = getPerasVoteRound vote
  resolveRoundNoWithHandle handle roundNo >>= \case
    Left err -> throwSTM err
    Right context ->
      case verifyPerasVote context vote of
        Left err -> throwSTM err
        Right validatedVote -> pure validatedVote

verifyPerasCertInContext ::
  ( MonadSTM m
  , MonadThrow (STM m)
  , BlockSupportsPeras blk
  ) =>
  PerasEpochContextResolverHandle m blk ->
  PerasCert blk ->
  STM m (ValidatedPerasCert blk)
verifyPerasCertInContext handle cert = do
  let roundNo = getPerasCertRound cert
   in resolveRoundNoWithHandle handle roundNo >>= \case
        Left err -> throwSTM err
        Right context ->
          case verifyPerasCert context cert of
            Left err -> throwSTM err
            Right validatedCert -> pure validatedCert
