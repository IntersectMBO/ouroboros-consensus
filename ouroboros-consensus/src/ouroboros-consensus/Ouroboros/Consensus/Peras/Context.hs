{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Ouroboros.Consensus.Peras.Context
  ( PerasEpochContextResolver
  , PerasEpochContextNotFoundForRound (..)
  , PerasEpochContextResolverHandle (..)
  , verifyPerasVoteInContext
  , verifyPerasCertInContext
  , resolveRoundNoWithHandle
  , constPerasEpochContextResolverHandle
  )
where

import Control.Exception (Exception)
import Control.Monad.Class.MonadSTM (STM)
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
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , MonadSTM
  , MonadThrow
  , NoThunks
  , newTVarIO
  , readTVar
  , throwSTM
  )

-- stored inside PraosState ?
type PerasEpochContextResolver blk =
  PerasRoundNo -> Either PerasEpochContextNotFoundForRound (PerasEpochContext blk)

data PerasEpochContextNotFoundForRound = PerasEpochContextNotFoundForRound PerasRoundNo
  deriving (Show, Eq, Generic, NoThunks, Exception)

newtype PerasEpochContextResolverHandle m blk
  = PerasEpochContextResolverHandle (STM m (PerasEpochContextResolver blk))

resolveRoundNoWithHandle ::
  MonadSTM m =>
  PerasEpochContextResolverHandle m blk ->
  PerasRoundNo ->
  STM m (Either PerasEpochContextNotFoundForRound (PerasEpochContext blk))
resolveRoundNoWithHandle (PerasEpochContextResolverHandle resolverHandle) roundNo = do
  resolver <- resolverHandle
  pure $ resolver roundNo

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
    Right epochContext ->
      case verifyPerasVote epochContext vote of
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
        Right epochContext ->
          case verifyPerasCert epochContext cert of
            Left err -> throwSTM err
            Right validatedCert -> pure validatedCert

constPerasEpochContextResolverHandle ::
  IOLike m => PerasEpochContext blk -> m (PerasEpochContextResolverHandle m blk)
constPerasEpochContextResolverHandle epochContext = do
  let resolver = \_ -> Right epochContext
  resolverVar <- newTVarIO resolver
  pure $ PerasEpochContextResolverHandle (readTVar resolverVar)
