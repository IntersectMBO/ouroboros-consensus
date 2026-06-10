{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ouroboros.Consensus.Peras.Context
  ( StateSupportsPerasEpochContext (..)
  , PerasEpochContextResolverHandle (..)
  , PerasEpochContextNotFoundForRound (..)
  , EmptyPerasEpochContextResolver
  , MockPerasEpochContextResolver (..)
  , V1PerasEpochContextResolver (..)
  , BoundedPerasEpochContext (..)
  , IsPerasEpochContextResolver (..)
  , resolveRoundNoWithHandle
  , verifyPerasVoteInContext
  , verifyPerasCertInContext
  , mockPerasEpochContextResolverHandle
  , forgePerasVoteIfEligibleInContext
  )
where

import Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import Codec.Serialise.Class (Serialise)
import Control.Applicative (Alternative (..))
import Control.Exception (Exception)
import Control.Monad.Class.MonadSTM (STM)
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Proxy (..))
import Data.Either.Extra (maybeToEither)
import Data.Kind (Type)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block.Abstract (BlockProtocol, Point)
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
  , getPerasVoteRound
  )
import Ouroboros.Consensus.Committee.Class (CryptoSupportsVotingCommittee)
import qualified Ouroboros.Consensus.Committee.Class as Committee
import Ouroboros.Consensus.Committee.Crypto (PrivateKey)
import Ouroboros.Consensus.Committee.Types (PoolId)
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory)
import Ouroboros.Consensus.HeaderValidation (Ticked)
import Ouroboros.Consensus.Ledger.Abstract (LedgerState)
import Ouroboros.Consensus.Ledger.SupportsPeras (ALedgerStateSupportsPeras (..))
import Ouroboros.Consensus.Peras.Time (EpochToPerasRoundInfo (..))
import Ouroboros.Consensus.Protocol.Abstract (AChainDepStateSupportsPeras, ConsensusProtocol (..))
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

data EmptyPerasEpochContextResolver blk = EmptyPerasEpochContextResolverError !String
  deriving (Show, Eq, Generic, NoThunks, Serialise)
deriving instance (EncodeDisk blk (EmptyPerasEpochContextResolver blk))
deriving instance (DecodeDisk blk (EmptyPerasEpochContextResolver blk))

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
  = V1PerasEpochContextResolverError !String
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

data PerasEpochContextNotFoundForRound = PerasEpochContextNotFoundForRound !PerasRoundNo !String
  deriving (Eq, Show, Generic, NoThunks, Exception)

class
  ( HasHardForkHistory blk
  , forall mk. ALedgerStateSupportsPeras (LedgerState blk mk)
  , AChainDepStateSupportsPeras (ChainDepState (BlockProtocol blk))
  , forall mk'. ALedgerStateSupportsPeras (Ticked LedgerState blk mk')
  , AChainDepStateSupportsPeras (Ticked (ChainDepState (BlockProtocol blk)))
  , IsPerasError (PerasError blk) blk
  , Show (PerasError blk)
  , CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)
  , Show (PerasEpochContext blk)
  , Eq (PerasEpochContext blk)
  , NoThunks (PerasEpochContext blk)
  , Typeable (PerasEpochContext blk)
  , Serialise (PerasEpochContext blk)
  , IsPerasEpochContextResolver (PerasEpochContextResolver blk) blk
  , Show (PerasEpochContextResolver blk)
  , Eq (PerasEpochContextResolver blk)
  , NoThunks (PerasEpochContextResolver blk)
  , Typeable (PerasEpochContextResolver blk)
  , Serialise (PerasEpochContextResolver blk)
  , EncodeDisk blk (PerasEpochContextResolver blk)
  , DecodeDisk blk (PerasEpochContextResolver blk)
  ) =>
  StateSupportsPerasEpochContext blk
  where
  type PerasEpochContextResolver blk :: Type
  type PerasEpochContextResolver blk = EmptyPerasEpochContextResolver blk

  mkPerasVotingCommitteeInput ::
    (ALedgerStateSupportsPeras ledger, AChainDepStateSupportsPeras chainDep) =>
    ledger ->
    chainDep ->
    Either
      (PerasError blk)
      (PerasVotingCommitteeInput blk)
  default mkPerasVotingCommitteeInput ::
    ( ALedgerStateSupportsPeras ledger
    , AChainDepStateSupportsPeras chainDep
    , PerasVotingCommitteeScheme blk ~ VoidPerasVotingCommitteeScheme
    ) =>
    ledger ->
    chainDep ->
    Either
      (PerasError blk)
      (PerasVotingCommitteeInput blk)
  mkPerasVotingCommitteeInput _ _ =
    error "mkPerasVotingCommitteeInput: not supported for this block"

  mkPerasVotingCommittee ::
    (ALedgerStateSupportsPeras ledger, AChainDepStateSupportsPeras chainDep) =>
    ledger ->
    chainDep ->
    Either
      (PerasError blk)
      (PerasVotingCommittee blk)
  mkPerasVotingCommittee ledgerState headerState = do
    committeeInput <-
      mkPerasVotingCommitteeInput ledgerState headerState
    first injectVotingCommitteeError $
      Committee.mkVotingCommittee committeeInput

  mkPerasEpochContext ::
    (ALedgerStateSupportsPeras ledger, AChainDepStateSupportsPeras chainDep) =>
    ledger ->
    chainDep ->
    Either
      (PerasError blk)
      (PerasEpochContext blk)
  default mkPerasEpochContext ::
    ( ALedgerStateSupportsPeras ledger
    , AChainDepStateSupportsPeras chainDep
    , PerasEpochContext blk ~ DefaultPerasEpochContext blk
    ) =>
    ledger ->
    chainDep ->
    Either
      (PerasError blk)
      (PerasEpochContext blk)
  mkPerasEpochContext ledgerState headerState = do
    dpecCommittee <- mkPerasVotingCommittee ledgerState headerState
    pure $
      DefaultPerasEpochContext{dpecParams = (getPerasParams (Proxy @blk) ledgerState), dpecCommittee}

  mkBoundedPerasEpochContext ::
    (ALedgerStateSupportsPeras ledger, AChainDepStateSupportsPeras chainDep) =>
    EpochToPerasRoundInfo ->
    ledger ->
    chainDep ->
    Either
      (PerasError blk)
      (BoundedPerasEpochContext blk)
  mkBoundedPerasEpochContext EpochToPerasRoundInfo{etpriEpochStartPerasRound, etpriEpochEndPerasRound} ledgerState headerState = do
    epochContext <- mkPerasEpochContext ledgerState headerState
    pure
      BoundedPerasEpochContext
        { startPerasRoundNo = etpriEpochStartPerasRound
        , endPerasRoundNo = etpriEpochEndPerasRound
        , epochContext = epochContext
        }

--------------------------------------------------------------------------------
-- IsPerasEpochContextResolver
--------------------------------------------------------------------------------

-- | Operations to build and query a 'PerasEpochContextResolver'.
class IsPerasEpochContextResolver resolver blk | resolver -> blk where
  -- | Initialise a resolver from a single bounded epoch context.
  initPerasEpochContextResolverWithBoundedEpochContext ::
    BoundedPerasEpochContext blk -> resolver

  -- | Advance a resolver with a new bounded epoch context.
  advancePerasEpochContextResolverWithBoundedEpochContext ::
    resolver -> BoundedPerasEpochContext blk -> resolver

  -- | Absorb a potential error encountered while building a resolver.
  errorIntoResolver ::
    Show err =>
    err -> resolver

  absorbErrorIntoResolver ::
    Show err =>
    Either err resolver -> resolver
  absorbErrorIntoResolver = either errorIntoResolver id

  -- | Resolve the epoch context valid for a given round.
  resolveRoundNo ::
    resolver ->
    PerasRoundNo ->
    Either PerasEpochContextNotFoundForRound (PerasEpochContext blk)

--------------------------------------------------------------------------------
-- Empty resolver
--------------------------------------------------------------------------------

instance
  BlockSupportsPeras blk =>
  IsPerasEpochContextResolver (EmptyPerasEpochContextResolver blk) blk
  where
  initPerasEpochContextResolverWithBoundedEpochContext _ = EmptyPerasEpochContextResolverError "EmptyPerasEpochContextResolver can never resolve"
  advancePerasEpochContextResolverWithBoundedEpochContext emptyResolver _ = emptyResolver
  errorIntoResolver err = EmptyPerasEpochContextResolverError (show err)
  resolveRoundNo _ roundNo =
    Left $
      PerasEpochContextNotFoundForRound
        roundNo
        "EmptyPerasEpochContextResolver can never resolve any round"

--------------------------------------------------------------------------------
-- Mock Resolver
--------------------------------------------------------------------------------

instance
  BlockSupportsPeras blk =>
  IsPerasEpochContextResolver (MockPerasEpochContextResolver blk) blk
  where
  initPerasEpochContextResolverWithBoundedEpochContext = MockPerasEpochContextResolver . epochContext
  advancePerasEpochContextResolverWithBoundedEpochContext _oldResolver = MockPerasEpochContextResolver . epochContext
  errorIntoResolver = MockPerasEpochContextResolverError . show
  resolveRoundNo resolver roundNo = case resolver of
    MockPerasEpochContextResolverError reason -> Left $ PerasEpochContextNotFoundForRound roundNo reason
    MockPerasEpochContextResolver context -> Right context

--------------------------------------------------------------------------------
-- V1 Resolver
--------------------------------------------------------------------------------

instance
  BlockSupportsPeras blk =>
  IsPerasEpochContextResolver (V1PerasEpochContextResolver blk) blk
  where
  initPerasEpochContextResolverWithBoundedEpochContext currEpochContext =
    V1PerasEpochContextResolver currEpochContext SNothing
  advancePerasEpochContextResolverWithBoundedEpochContext prev newEpochContext = case prev of
    V1PerasEpochContextResolver prevEpochContextResolver _ ->
      V1PerasEpochContextResolver
        prevEpochContextResolver
        (SJust newEpochContext)
    _ -> V1PerasEpochContextResolver newEpochContext SNothing
  errorIntoResolver = V1PerasEpochContextResolverError . show
  resolveRoundNo resolver roundNo = case resolver of
    V1PerasEpochContextResolverError reason -> Left $ PerasEpochContextNotFoundForRound roundNo reason
    V1PerasEpochContextResolver current mbPrev ->
      maybeToEither
        ( PerasEpochContextNotFoundForRound
            roundNo
            "Neither current nor previous epoch context cover the given Peras roundNo"
        )
        $ withinEpochContext roundNo current
          <|> (withinEpochContext roundNo =<< strictMaybeToMaybe mbPrev)

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
  let resolver = MockPerasEpochContextResolver context
  resolverVar <- newTVarIO resolver
  pure $ PerasEpochContextResolverHandle (readTVar resolverVar)

resolveRoundNoWithHandle ::
  (MonadSTM m, StateSupportsPerasEpochContext blk) =>
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
  , StateSupportsPerasEpochContext blk
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
  , StateSupportsPerasEpochContext blk
  ) =>
  PerasEpochContextResolverHandle m blk ->
  PerasCert blk ->
  STM m (ValidatedPerasCert blk)
verifyPerasCertInContext handle cert =
  let roundNo = getPerasCertRound cert
   in resolveRoundNoWithHandle handle roundNo >>= \case
        Left err -> throwSTM err
        Right context ->
          case verifyPerasCert context cert of
            Left err -> throwSTM err
            Right validatedCert -> pure validatedCert

forgePerasVoteIfEligibleInContext ::
  ( MonadSTM m
  , MonadThrow (STM m)
  , BlockSupportsPeras blk
  , StateSupportsPerasEpochContext blk
  ) =>
  PerasEpochContextResolverHandle m blk ->
  PoolId ->
  PrivateKey (PerasCrypto blk) ->
  PerasRoundNo ->
  Point blk ->
  STM m (Maybe (ValidatedPerasVote blk))
forgePerasVoteIfEligibleInContext handle poolId privateKey roundNo point =
  resolveRoundNoWithHandle handle roundNo >>= \case
    Left err -> throwSTM err
    Right context ->
      case forgePerasVoteIfEligible context poolId privateKey roundNo point of
        Left err -> throwSTM err
        Right maybeValidatedVote -> pure maybeValidatedVote
