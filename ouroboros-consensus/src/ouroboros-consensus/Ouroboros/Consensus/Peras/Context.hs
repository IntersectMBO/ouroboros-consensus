{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ouroboros.Consensus.Peras.Context
  ( StateSupportsPerasEpochContext (..)
  , PerasEpochContextResolverHandle (..)
  , PerasEpochContextNotFoundForRound (..)
  , PerasEpochContextResolver (..)
  , initPerasEpochContextResolverWithBoundedEpochContext
  , advancePerasEpochContextResolverWithBoundedEpochContext
  , errorIntoResolver
  , absorbErrorIntoResolver
  , resolveRoundNo
  , BoundedPerasEpochContext (..)
  , resolveRoundNoWithHandle
  , verifyPerasVoteInContext
  , verifyPerasCertInContext
  , mockPerasEpochContextResolverHandle
  , forgePerasVoteIfEligibleInContext
  , mkBoundedPerasEpochContextFromMkPerasVotingCommitteeInput
  )
where

import Codec.Serialise.Class (Serialise)
import Control.Exception (Exception)
import Control.Monad.Class.MonadSTM (STM)
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Proxy (..))
import Data.Either.Extra (maybeToEither)
import Data.Kind (Type)
import Data.SOP.Constraint (All, Top)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block.Abstract (BlockProtocol, Point)
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , IsPerasCert (getPerasCertRound)
  , IsPerasError (injectVotingCommitteeError)
  , PerasCert
  , PerasEpochContext (..)
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
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (HardForkIndices))
import qualified Ouroboros.Consensus.HardFork.History as HF
import Ouroboros.Consensus.HeaderValidation (Ticked)
import Ouroboros.Consensus.Ledger.Abstract (LedgerState)
import Ouroboros.Consensus.Ledger.SupportsPeras (ALedgerStateSupportsPeras (..))
import Ouroboros.Consensus.Peras.Time (EpochToPerasRoundInfo (..), EraIndexed, forgetEraIndex)
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

data PerasEpochContextResolver blk
  = PerasEpochContextResolverError !String
  | PerasEpochContextResolver
      !(HF.PerasEnabled (BoundedPerasEpochContext blk))
      !(HF.PerasEnabled (BoundedPerasEpochContext blk))
deriving instance Show (PerasEpochContext blk) => Show (PerasEpochContextResolver blk)
deriving instance Eq (PerasEpochContext blk) => Eq (PerasEpochContextResolver blk)
deriving instance Generic (PerasEpochContextResolver blk)
deriving instance NoThunks (PerasEpochContext blk) => NoThunks (PerasEpochContextResolver blk)
deriving instance Serialise (PerasEpochContext blk) => Serialise (PerasEpochContextResolver blk)
deriving instance
  Serialise (PerasEpochContext blk) => EncodeDisk blk (PerasEpochContextResolver blk)
deriving instance
  Serialise (PerasEpochContext blk) => DecodeDisk blk (PerasEpochContextResolver blk)

data PerasEpochContextNotFoundForRound = PerasEpochContextNotFoundForRound !PerasRoundNo !String
  deriving (Eq, Show, Generic, NoThunks, Exception)

class
  ( HasHardForkHistory blk
  , All Top (HardForkIndices blk)
  , forall mk. ALedgerStateSupportsPeras (LedgerState blk mk)
  , AChainDepStateSupportsPeras (ChainDepState (BlockProtocol blk))
  , forall mk'. ALedgerStateSupportsPeras (Ticked LedgerState blk mk')
  , AChainDepStateSupportsPeras (Ticked (ChainDepState (BlockProtocol blk)))
  , IsPerasError (PerasError blk) blk
  , Show (PerasError blk)
  , Show (PerasVotingCommittee blk)
  , Eq (PerasVotingCommittee blk)
  , NoThunks (PerasVotingCommittee blk)
  , Typeable (PerasVotingCommittee blk)
  , Serialise (PerasVotingCommittee blk)
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
  type MaybeEraIndexedEpochToPerasRoundInfo blk :: Type
  type MaybeEraIndexedEpochToPerasRoundInfo blk = EpochToPerasRoundInfo

  fromMaybeEraIndexedEpochToPerasRoundInfo ::
    proxy blk -> MaybeEraIndexedEpochToPerasRoundInfo blk -> EpochToPerasRoundInfo
  default fromMaybeEraIndexedEpochToPerasRoundInfo ::
    proxy blk ->
    MaybeEraIndexedEpochToPerasRoundInfo blk ~ EpochToPerasRoundInfo =>
    MaybeEraIndexedEpochToPerasRoundInfo blk -> EpochToPerasRoundInfo
  fromMaybeEraIndexedEpochToPerasRoundInfo _ = id

  toMaybeEraIndexedEpochToPerasRoundInfo ::
    All Top (HardForkIndices blk) =>
    proxy blk -> EraIndexed blk EpochToPerasRoundInfo -> MaybeEraIndexedEpochToPerasRoundInfo blk
  default toMaybeEraIndexedEpochToPerasRoundInfo ::
    All Top (HardForkIndices blk) =>
    proxy blk ->
    MaybeEraIndexedEpochToPerasRoundInfo blk ~ EpochToPerasRoundInfo =>
    EraIndexed blk EpochToPerasRoundInfo -> MaybeEraIndexedEpochToPerasRoundInfo blk
  toMaybeEraIndexedEpochToPerasRoundInfo _ = forgetEraIndex

  mkBoundedPerasEpochContext ::
    (ALedgerStateSupportsPeras ledger, AChainDepStateSupportsPeras chainDep) =>
    MaybeEraIndexedEpochToPerasRoundInfo blk ->
    ledger ->
    chainDep ->
    Either
      (PerasError blk)
      (BoundedPerasEpochContext blk)
  default mkBoundedPerasEpochContext ::
    ( PerasVotingCommitteeScheme blk ~ VoidPerasVotingCommitteeScheme
    , ALedgerStateSupportsPeras ledger
    , AChainDepStateSupportsPeras chainDep
    ) =>
    MaybeEraIndexedEpochToPerasRoundInfo blk ->
    ledger ->
    chainDep ->
    Either
      (PerasError blk)
      (BoundedPerasEpochContext blk)
  mkBoundedPerasEpochContext _ _ _ =
    error
      "mkBoundedPerasEpochContext: should never be called for this block type since it does not support Peras"

-- | This intends to be the normal way of implementing 'mkBoundedPerasEpochContext' in terms of a function that produces a 'PerasVotingCommitteeInput', namely 'V1.mkPerasVotingCommitteeInput' or 'mkMockPerasVotingCommitteeInput'.
mkBoundedPerasEpochContextFromMkPerasVotingCommitteeInput ::
  ( All Top (HardForkIndices blk)
  , ALedgerStateSupportsPeras ledger
  , AChainDepStateSupportsPeras chainDep
  , MaybeEraIndexedEpochToPerasRoundInfo blk ~ EpochToPerasRoundInfo
  , CryptoSupportsVotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)
  , IsPerasError (PerasError blk) blk
  ) =>
  ( (ALedgerStateSupportsPeras ledger, AChainDepStateSupportsPeras chainDep) =>
    ledger ->
    chainDep ->
    Either
      (PerasError blk)
      (PerasVotingCommitteeInput blk)
  ) ->
  MaybeEraIndexedEpochToPerasRoundInfo blk ->
  ledger ->
  chainDep ->
  Either
    (PerasError blk)
    (BoundedPerasEpochContext blk)
mkBoundedPerasEpochContextFromMkPerasVotingCommitteeInput mkPerasVotingCommitteeInput EpochToPerasRoundInfo{etpriEpochStartPerasRound, etpriEpochEndPerasRound} ledgerState headerState = do
  committeeInput <-
    mkPerasVotingCommitteeInput ledgerState headerState
  pecCommittee <-
    first injectVotingCommitteeError $
      Committee.mkVotingCommittee committeeInput
  pure
    BoundedPerasEpochContext
      { startPerasRoundNo = etpriEpochStartPerasRound
      , endPerasRoundNo = etpriEpochEndPerasRound
      , epochContext =
          PerasEpochContext{pecParams = getPerasParams Proxy ledgerState, pecCommittee}
      }

--------------------------------------------------------------------------------
-- Resolver
--------------------------------------------------------------------------------

initPerasEpochContextResolverWithBoundedEpochContext ::
  HF.PerasEnabled (BoundedPerasEpochContext blk) -> PerasEpochContextResolver blk
initPerasEpochContextResolverWithBoundedEpochContext currEpochContext =
  PerasEpochContextResolver currEpochContext HF.NoPerasEnabled

advancePerasEpochContextResolverWithBoundedEpochContext ::
  PerasEpochContextResolver blk ->
  (HF.PerasEnabled (BoundedPerasEpochContext blk)) ->
  PerasEpochContextResolver blk
advancePerasEpochContextResolverWithBoundedEpochContext prev newEpochContext = case prev of
  PerasEpochContextResolver prevEpochContextResolver _ ->
    PerasEpochContextResolver
      prevEpochContextResolver
      newEpochContext
  _ -> PerasEpochContextResolver newEpochContext HF.NoPerasEnabled

errorIntoResolver ::
  Show err => err -> PerasEpochContextResolver blk
errorIntoResolver = PerasEpochContextResolverError . show

absorbErrorIntoResolver ::
  Show err => Either err (PerasEpochContextResolver blk) -> PerasEpochContextResolver blk
absorbErrorIntoResolver = either errorIntoResolver id

resolveRoundNo ::
  PerasEpochContextResolver blk ->
  PerasRoundNo ->
  Either PerasEpochContextNotFoundForRound (PerasEpochContext blk)
resolveRoundNo resolver roundNo = case resolver of
  PerasEpochContextResolverError reason -> Left $ PerasEpochContextNotFoundForRound roundNo reason
  PerasEpochContextResolver current prev ->
    case ( maybeToEither
             "no current epoch context available because Peras isn't enabled"
             (HF.perasEnabledToMaybe current)
             >>= \current' ->
               maybeToEither
                 ( "current epoch context available, but roundNo "
                     ++ show roundNo
                     ++ " not within current epoch context bounds ["
                     ++ show (startPerasRoundNo current')
                     ++ ", "
                     ++ show (endPerasRoundNo current')
                     ++ "["
                 )
                 (withinEpochContext roundNo current')
         , maybeToEither
             "no previous epoch context available because Peras isn't enabled"
             (HF.perasEnabledToMaybe prev)
             >>= \prev' ->
               maybeToEither
                 ( "roundNo "
                     ++ show roundNo
                     ++ " not within previous epoch context bounds ["
                     ++ show (startPerasRoundNo prev')
                     ++ ", "
                     ++ show (endPerasRoundNo prev')
                     ++ "["
                 )
                 (withinEpochContext roundNo prev')
         ) of
      (Right context, _) -> Right context
      (_, Right context) -> Right context
      (Left reason1, Left reason2) -> Left $ PerasEpochContextNotFoundForRound roundNo (reason1 ++ "; " ++ reason2)

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
  ) =>
  PerasEpochContext blk -> m (PerasEpochContextResolverHandle m blk)
mockPerasEpochContextResolverHandle context = do
  let resolver =
        PerasEpochContextResolver
          (HF.PerasEnabled $ BoundedPerasEpochContext minBound maxBound context)
          HF.NoPerasEnabled
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
