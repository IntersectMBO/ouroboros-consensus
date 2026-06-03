{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ouroboros.Consensus.Peras.Context
  ( LedgerStateHeaderStateSupportsPerasVoting (..)
  , PerasEpochContextResolverHandle (..)
  , PerasEpochContextNotFoundForRound (..)
  , EmptyPerasEpochContextResolver
  , MockPerasEpochContextResolver (..)
  , V1PerasEpochContextResolver (..)
  , BoundedPerasEpochContext (..)
  , emptyPerasEpochContextResolver
  , emptyAbsorbErrorInResolver
  , emptyResolveRoundNo
  , mockPerasEpochContextResolver
  , mockResolveRoundNo
  , mockAbsorbErrorInResolver
  , v1InitPerasEpochContextResolver
  , v1AdvancePerasEpochContextResolver
  , v1ResolveRoundNo
  , v1AbsorbErrorInResolver
  , resolveRoundNoWithHandle
  , verifyPerasVoteInContext
  , verifyPerasCertInContext
  , unsafeBoundedPerasEpochContextWithMinMaxBounds
  , mockPerasEpochContextResolverHandle
  )
where

import Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import Codec.Serialise.Class (Serialise)
import Control.Applicative (Alternative (..))
import Control.Exception (Exception)
import Control.Monad.Class.MonadSTM (STM)
import Data.Bifunctor (Bifunctor (..))
import Data.Either.Extra (maybeToEither)
import Data.Kind (Type)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , DefaultPerasEpochContext (..)
  , IsPerasCert (getPerasCertRound)
  , IsPerasError (injectVotingCommitteeError)
  , PerasCert
  , PerasRoundNo
  , PerasVote
  , PerasVotingCommittee
  , PerasVotingCommitteeInput
  , ValidatedPerasCert
  , ValidatedPerasVote
  , VoidPerasVotingCommitteeScheme
  , defaultPerasParams
  , getPerasVoteRound
  )
import Ouroboros.Consensus.Committee.Class (CryptoSupportsVotingCommittee)
import qualified Ouroboros.Consensus.Committee.Class as Committee
import Ouroboros.Consensus.HeaderValidation (HeaderState)
import Ouroboros.Consensus.Ledger.Abstract (LedgerState)
import Ouroboros.Consensus.Peras.Params (PerasParams)
import qualified Ouroboros.Consensus.Peras.Voting.V1 as V1
import Ouroboros.Consensus.Storage.Serialisation (DecodeDisk, EncodeDisk)
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , MonadSTM
  , MonadThrow
  , NoThunks (..)
  , newTVarIO
  , readTVar
  , throwSTM
  )

-------------------------------------------------------------------------------
-- Peras support
-------------------------------------------------------------------------------

data EmptyPerasEpochContextResolver = EmptyPerasEpochContextResolver
  deriving (Show, Eq, Generic, NoThunks, Serialise)
deriving instance (EncodeDisk blk EmptyPerasEpochContextResolver)
deriving instance (DecodeDisk blk EmptyPerasEpochContextResolver)

data MockPerasEpochContextResolver blk
  = MockPerasEpochContextResolverError !String
  | MockPerasEpochContextResolver !(PerasEpochContext blk)
deriving instance Show (PerasEpochContext blk) => Show (MockPerasEpochContextResolver blk)
deriving instance Eq (PerasEpochContext blk) => Eq (MockPerasEpochContextResolver blk)
deriving instance Generic (MockPerasEpochContextResolver blk)
deriving instance NoThunks (PerasEpochContext blk) => NoThunks (MockPerasEpochContextResolver blk)
deriving instance Serialise (PerasEpochContext blk) => Serialise (MockPerasEpochContextResolver blk)
deriving instance
  Serialise (PerasEpochContext blk) => EncodeDisk blk (MockPerasEpochContextResolver blk)
deriving instance
  Serialise (PerasEpochContext blk) => DecodeDisk blk (MockPerasEpochContextResolver blk)

data V1PerasEpochContextResolver blk
  = V1PerasEpochContextResolverError String
  | V1PerasEpochContextResolver
      !(BoundedPerasEpochContext blk)
      !(StrictMaybe (BoundedPerasEpochContext blk))
deriving instance Show (PerasEpochContext blk) => Show (V1PerasEpochContextResolver blk)
deriving instance Eq (PerasEpochContext blk) => Eq (V1PerasEpochContextResolver blk)
deriving instance Generic (V1PerasEpochContextResolver blk)
deriving instance NoThunks (PerasEpochContext blk) => NoThunks (V1PerasEpochContextResolver blk)
deriving instance Serialise (PerasEpochContext blk) => Serialise (V1PerasEpochContextResolver blk)
deriving instance
  Serialise (PerasEpochContext blk) => EncodeDisk blk (V1PerasEpochContextResolver blk)
deriving instance
  Serialise (PerasEpochContext blk) => DecodeDisk blk (V1PerasEpochContextResolver blk)

data PerasEpochContextNotFoundForRound = PerasEpochContextNotFoundForRound !PerasRoundNo
  deriving (Eq, Show, Generic, NoThunks, Exception)

class
  ( IsPerasError (PerasError blk) blk
  , CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)
  , Show (PerasEpochContextResolver blk)
  , Eq (PerasEpochContextResolver blk)
  , NoThunks (PerasEpochContextResolver blk)
  , Typeable (PerasEpochContextResolver blk)
  , Serialise (PerasEpochContextResolver blk)
  , EncodeDisk blk (PerasEpochContextResolver blk)
  , DecodeDisk blk (PerasEpochContextResolver blk)
  ) =>
  LedgerStateHeaderStateSupportsPerasVoting blk
  where
  type PerasEpochContextResolver blk :: Type
  type PerasEpochContextResolver blk = EmptyPerasEpochContextResolver

  ledgerStateHeaderStateMkPerasVotingCommitteeInput ::
    PerasParams blk ->
    LedgerState blk mk ->
    HeaderState blk ->
    Either
      (PerasError blk)
      (PerasVotingCommitteeInput blk)
  default ledgerStateHeaderStateMkPerasVotingCommitteeInput ::
    PerasVotingCommitteeScheme blk ~ VoidPerasVotingCommitteeScheme =>
    PerasParams blk ->
    LedgerState blk mk ->
    HeaderState blk ->
    Either
      (PerasError blk)
      (PerasVotingCommitteeInput blk)
  ledgerStateHeaderStateMkPerasVotingCommitteeInput _ _ _ =
    error "ledgerStateHeaderStateMkPerasVotingCommitteeInput: not supported for this block"

  ledgerStateHeaderStateMkPerasVotingCommittee ::
    PerasParams blk ->
    LedgerState blk mk ->
    HeaderState blk ->
    Either
      (PerasError blk)
      (PerasVotingCommittee blk)
  ledgerStateHeaderStateMkPerasVotingCommittee perasParams ledgerState headerState = do
    committeeInput <-
      ledgerStateHeaderStateMkPerasVotingCommitteeInput perasParams ledgerState headerState
    bimap injectVotingCommitteeError id $
      Committee.mkVotingCommittee committeeInput

  ledgerStateHeaderStateMkPerasEpochContext ::
    LedgerState blk mk ->
    HeaderState blk ->
    Either
      (PerasError blk)
      (PerasEpochContext blk)
  default ledgerStateHeaderStateMkPerasEpochContext ::
    PerasEpochContext blk ~ DefaultPerasEpochContext blk =>
    LedgerState blk mk ->
    HeaderState blk ->
    Either
      (PerasError blk)
      (PerasEpochContext blk)
  ledgerStateHeaderStateMkPerasEpochContext ledgerState headerState = do
    let dpecParams = defaultPerasParams
    dpecCommittee <- ledgerStateHeaderStateMkPerasVotingCommittee dpecParams ledgerState headerState
    pure $ DefaultPerasEpochContext{dpecParams, dpecCommittee}

  ledgerStateHeaderStateMkPerasEpochContextResolver ::
    LedgerState blk mk ->
    HeaderState blk ->
    PerasEpochContextResolver blk
  default ledgerStateHeaderStateMkPerasEpochContextResolver ::
    PerasEpochContextResolver blk ~ EmptyPerasEpochContextResolver =>
    LedgerState blk mk ->
    HeaderState blk ->
    PerasEpochContextResolver blk
  ledgerStateHeaderStateMkPerasEpochContextResolver _ _ = emptyPerasEpochContextResolver

  resolveRoundNo ::
    PerasEpochContextResolver blk ->
    PerasRoundNo ->
    Either PerasEpochContextNotFoundForRound (PerasEpochContext blk)
  default resolveRoundNo ::
    PerasEpochContextResolver blk ~ EmptyPerasEpochContextResolver =>
    PerasEpochContextResolver blk ->
    PerasRoundNo ->
    Either PerasEpochContextNotFoundForRound (PerasEpochContext blk)
  resolveRoundNo = emptyResolveRoundNo

-- absorbErrorInResolver ::
--   Either (PerasError blk) (PerasEpochContextResolver blk) ->
--   PerasEpochContextResolver blk
-- default absorbErrorInResolver ::
--   PerasEpochContextResolver blk ~ EmptyPerasEpochContextResolver =>
--   Either (PerasError blk) (PerasEpochContextResolver blk) ->
--   PerasEpochContextResolver blk
-- absorbErrorInResolver = emptyAbsorbErrorInResolver

--------------------------------------------------------------------------------
-- Empty resolver
--------------------------------------------------------------------------------

emptyPerasEpochContextResolver ::
  EmptyPerasEpochContextResolver
emptyPerasEpochContextResolver = EmptyPerasEpochContextResolver

emptyResolveRoundNo ::
  PerasEpochContextResolver blk ~ EmptyPerasEpochContextResolver =>
  PerasEpochContextResolver blk ->
  PerasRoundNo ->
  Either PerasEpochContextNotFoundForRound (PerasEpochContext blk)
emptyResolveRoundNo _ roundNo = Left $ PerasEpochContextNotFoundForRound roundNo

emptyAbsorbErrorInResolver ::
  PerasEpochContextResolver blk ~ EmptyPerasEpochContextResolver =>
  Either (PerasError blk) (PerasEpochContextResolver blk) ->
  PerasEpochContextResolver blk
emptyAbsorbErrorInResolver _ = EmptyPerasEpochContextResolver

--------------------------------------------------------------------------------
-- Mock Resolver
--------------------------------------------------------------------------------

mockPerasEpochContextResolver ::
  PerasEpochContext blk -> MockPerasEpochContextResolver blk
mockPerasEpochContextResolver = MockPerasEpochContextResolver

mockResolveRoundNo ::
  PerasEpochContextResolver blk ~ MockPerasEpochContextResolver blk =>
  PerasEpochContextResolver blk ->
  PerasRoundNo ->
  Either PerasEpochContextNotFoundForRound (PerasEpochContext blk)
mockResolveRoundNo resolver roundNo = case resolver of
  MockPerasEpochContextResolverError _err -> Left $ PerasEpochContextNotFoundForRound roundNo
  MockPerasEpochContextResolver context -> Right context

mockAbsorbErrorInResolver ::
  (PerasEpochContextResolver blk ~ MockPerasEpochContextResolver blk, Show (PerasError blk)) =>
  Either (PerasError blk) (PerasEpochContextResolver blk) ->
  PerasEpochContextResolver blk
mockAbsorbErrorInResolver = \case
  Left err -> MockPerasEpochContextResolverError (show err)
  Right resolver -> resolver

--------------------------------------------------------------------------------
-- V1 Resolver
--------------------------------------------------------------------------------

v1InitPerasEpochContextResolver ::
  BoundedPerasEpochContext blk ->
  V1PerasEpochContextResolver blk
v1InitPerasEpochContextResolver currEpochContext =
  V1PerasEpochContextResolver currEpochContext SNothing

v1AdvancePerasEpochContextResolver ::
  PerasVotingCommitteeScheme blk ~ V1.PerasVotingCommitteeScheme =>
  V1PerasEpochContextResolver blk ->
  BoundedPerasEpochContext blk ->
  V1PerasEpochContextResolver blk
v1AdvancePerasEpochContextResolver prev newEpochContext = case prev of
  V1PerasEpochContextResolver prevEpochContextResolver _ ->
    V1PerasEpochContextResolver
      prevEpochContextResolver
      (SJust newEpochContext)
  _ -> V1PerasEpochContextResolver newEpochContext SNothing

v1ResolveRoundNo ::
  PerasEpochContextResolver blk ~ V1PerasEpochContextResolver blk =>
  PerasEpochContextResolver blk ->
  PerasRoundNo ->
  Either PerasEpochContextNotFoundForRound (PerasEpochContext blk)
v1ResolveRoundNo resolver roundNo = case resolver of
  V1PerasEpochContextResolverError _err -> Left $ PerasEpochContextNotFoundForRound roundNo
  V1PerasEpochContextResolver current mbPrev ->
    maybeToEither (PerasEpochContextNotFoundForRound roundNo) $
      withinEpochContext roundNo current
        <|> (withinEpochContext roundNo =<< strictMaybeToMaybe mbPrev)

v1AbsorbErrorInResolver ::
  (PerasEpochContextResolver blk ~ V1PerasEpochContextResolver blk, Show (PerasError blk)) =>
  Either (PerasError blk) (PerasEpochContextResolver blk) ->
  PerasEpochContextResolver blk
v1AbsorbErrorInResolver = \case
  Left err -> V1PerasEpochContextResolverError (show err)
  Right resolver -> resolver

--------------------------------------------------------------------------------
-- Bounded context
--------------------------------------------------------------------------------

data BoundedPerasEpochContext blk
  = BoundedPerasEpochContext
  { startPerasRoundNo :: PerasRoundNo -- inclusive
  , endPerasRoundNo :: PerasRoundNo -- exclusive
  , epochContext :: PerasEpochContext blk
  }
deriving instance Show (PerasEpochContext blk) => Show (BoundedPerasEpochContext blk)
deriving instance Eq (PerasEpochContext blk) => Eq (BoundedPerasEpochContext blk)
deriving instance Generic (BoundedPerasEpochContext blk)
deriving instance NoThunks (PerasEpochContext blk) => NoThunks (BoundedPerasEpochContext blk)
deriving instance Serialise (PerasEpochContext blk) => Serialise (BoundedPerasEpochContext blk)
deriving instance Serialise (PerasEpochContext blk) => EncodeDisk blk (BoundedPerasEpochContext blk)
deriving instance Serialise (PerasEpochContext blk) => DecodeDisk blk (BoundedPerasEpochContext blk)

-- [TODO EPOCH CONTEXT PLUMBING] : remove this guy
unsafeBoundedPerasEpochContextWithMinMaxBounds ::
  PerasEpochContext blk -> BoundedPerasEpochContext blk
unsafeBoundedPerasEpochContextWithMinMaxBounds context =
  BoundedPerasEpochContext
    { startPerasRoundNo = minBound
    , endPerasRoundNo = maxBound
    , epochContext = context
    }

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

newtype PerasEpochContextResolverHandle m blk
  = PerasEpochContextResolverHandle (STM m (PerasEpochContextResolver blk))

mockPerasEpochContextResolverHandle ::
  ( IOLike m
  , NoThunks (PerasEpochContext blk)
  , PerasEpochContextResolver blk ~ MockPerasEpochContextResolver blk
  ) =>
  PerasEpochContext blk -> m (PerasEpochContextResolverHandle m blk)
mockPerasEpochContextResolverHandle context = do
  let resolver = MockPerasEpochContextResolver $ context
  resolverVar <- newTVarIO resolver
  pure $ PerasEpochContextResolverHandle (readTVar resolverVar)

resolveRoundNoWithHandle ::
  (MonadSTM m, LedgerStateHeaderStateSupportsPerasVoting blk) =>
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
  , LedgerStateHeaderStateSupportsPerasVoting blk
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
  , LedgerStateHeaderStateSupportsPerasVoting blk
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
