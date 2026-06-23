{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Basics
  ( -- * Hard fork protocol, block, and ledger state
    HardForkBlock (..)
  , HardForkProtocol
  , LedgerState (..)

    -- * Config
  , BlockConfig (..)
  , CodecConfig (..)
  , ConsensusConfig (..)
  , HardForkLedgerConfig (..)
  , StorageConfig (..)

    -- ** Functions on config
  , completeConsensusConfig'
  , completeConsensusConfig''
  , completeLedgerConfig'
  , completeLedgerConfig''
  , distribLedgerConfig
  , distribTopLevelConfig

    -- ** Convenience re-exports
  , EpochInfo
  , Except
  ) where

import Cardano.Slotting.EpochInfo
import Data.Bifunctor (bimap)
import Data.Functor.Product (Product (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.SOP (K (..), type (:.:) (..))
import Data.SOP.Constraint
import Data.SOP.Functors
import Data.SOP.Index (himap, injectNS)
import Data.SOP.Match (matchNS)
import Data.SOP.Strict
import Data.Typeable
import Data.Word (Word32)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract
import Ouroboros.Consensus.Block.SupportsPeras
  ( BlockSupportsPeras (..)
  , BoostedBlock
  , IsPerasCert (..)
  , IsPerasError (..)
  , IsPerasVote (..)
  , PerasBoostedBlock
  , PerasCertCompatibleWithVotingCommittee (..)
  , PerasEpochContext (..)
  , PerasRoundNo
  , PerasVoteCollection (..)
  , PerasVoteCollectionWithQuorum (..)
  , PerasVoteCompatibleWithVotingCommittee (..)
  , ValidatedPerasCert (..)
  , ValidatedPerasVote (..)
  , retagPerasParams
  , unsafePerasVoteCollectionWithQuorum
  )
import Ouroboros.Consensus.BlockchainTime (WithArrivalTime (..))
import Ouroboros.Consensus.Committee.Class
  ( CryptoSupportsVotingCommittee (..)
  , UniqueVotesWithSameTarget
  , getRawVotes
  , unsafeUniqueVotesWithSameTarget
  )
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsVoteSigning (..)
  , ElectionId
  , PrivateKey
  , PublicKey
  , VoteCandidate
  )
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork
  ( CanHardFork
  )
import Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
  ( SingleEraBlock
  , proxySingle
  )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import Ouroboros.Consensus.HardFork.Combinator.Lifting (LiftNS (..), LiftNamedNS (..))
import Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import qualified Ouroboros.Consensus.HardFork.Combinator.State.Infra as State
import Ouroboros.Consensus.HardFork.Combinator.State.Instances ()
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.SupportsPeras
  ( ALedgerStateSupportsPeras (..)
  )
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util (ShowProxy)
import Type.Reflection (someTypeRep)
import Unsafe.Coerce (unsafeCoerce)

{-------------------------------------------------------------------------------
  Hard fork protocol, block, and ledger state
-------------------------------------------------------------------------------}

data HardForkProtocol (xs :: [Type])

newtype HardForkBlock xs = HardForkBlock
  { getHardForkBlock :: OneEraBlock xs
  }
  deriving Show

instance Typeable xs => ShowProxy (HardForkBlock xs)

type instance BlockProtocol (HardForkBlock xs) = HardForkProtocol xs
type instance HeaderHash (HardForkBlock xs) = OneEraHash xs

newtype instance LedgerState (HardForkBlock xs) mk = HardForkLedgerState
  { hardForkLedgerStatePerEra :: HardForkState (Flip LedgerState mk) xs
  }

deriving stock instance
  (ShowMK mk, CanHardFork xs) =>
  Show (LedgerState (HardForkBlock xs) mk)
deriving stock instance
  (EqMK mk, CanHardFork xs) =>
  Eq (LedgerState (HardForkBlock xs) mk)
deriving newtype instance
  (NoThunksMK mk, CanHardFork xs) =>
  NoThunks (LedgerState (HardForkBlock xs) mk)

{-------------------------------------------------------------------------------
  Protocol config
-------------------------------------------------------------------------------}

data instance ConsensusConfig (HardForkProtocol xs) = HardForkConsensusConfig
  { hardForkConsensusConfigK :: !(SecurityParam)
  -- ^ The value of @k@ cannot change at hard fork boundaries
  , hardForkConsensusConfigShape :: !(History.Shape xs)
  -- ^ The shape of the hard fork
  --
  -- We require this in the consensus config because consensus might need
  -- access to 'EpochInfo', and in order to compute that, we need the
  -- 'EraParams' of all eras.
  , hardForkConsensusConfigPerEra :: !(PerEraConsensusConfig xs)
  -- ^ Config for each era
  }
  deriving stock Generic
  deriving anyclass NoThunks

{-------------------------------------------------------------------------------
  Block config
-------------------------------------------------------------------------------}

newtype instance BlockConfig (HardForkBlock xs) = HardForkBlockConfig
  { hardForkBlockConfigPerEra :: PerEraBlockConfig xs
  }
  deriving newtype NoThunks

{-------------------------------------------------------------------------------
  Codec config
-------------------------------------------------------------------------------}

newtype instance CodecConfig (HardForkBlock xs) = HardForkCodecConfig
  { hardForkCodecConfigPerEra :: PerEraCodecConfig xs
  }
  deriving newtype NoThunks

{-------------------------------------------------------------------------------
  Storage config
-------------------------------------------------------------------------------}

newtype instance StorageConfig (HardForkBlock xs) = HardForkStorageConfig
  { hardForkStorageConfigPerEra :: PerEraStorageConfig xs
  }
  deriving newtype NoThunks

{-------------------------------------------------------------------------------
  Ledger config
-------------------------------------------------------------------------------}

data HardForkLedgerConfig xs = HardForkLedgerConfig
  { hardForkLedgerConfigShape :: !(History.Shape xs)
  , hardForkLedgerConfigPerEra :: !(PerEraLedgerConfig xs)
  }
  deriving Generic

deriving instance Show (PerEraLedgerConfig xs) => Show (HardForkLedgerConfig xs)
instance CanHardFork xs => NoThunks (HardForkLedgerConfig xs)

type instance LedgerCfg LedgerState (HardForkBlock xs) = HardForkLedgerConfig xs

{-------------------------------------------------------------------------------
  Operations on config
-------------------------------------------------------------------------------}

completeLedgerConfig' ::
  forall blk.
  HasPartialLedgerConfig blk =>
  EpochInfo (Except PastHorizonException) ->
  WrapPartialLedgerConfig blk ->
  LedgerConfig blk
completeLedgerConfig' ei =
  completeLedgerConfig (Proxy @blk) ei
    . unwrapPartialLedgerConfig

completeLedgerConfig'' ::
  forall blk.
  HasPartialLedgerConfig blk =>
  EpochInfo (Except PastHorizonException) ->
  WrapPartialLedgerConfig blk ->
  WrapLedgerConfig blk
completeLedgerConfig'' ei =
  WrapLedgerConfig
    . completeLedgerConfig (Proxy @blk) ei
    . unwrapPartialLedgerConfig

completeConsensusConfig' ::
  forall blk.
  HasPartialConsensusConfig (BlockProtocol blk) =>
  EpochInfo (Except PastHorizonException) ->
  WrapPartialConsensusConfig blk ->
  ConsensusConfig (BlockProtocol blk)
completeConsensusConfig' ei =
  completeConsensusConfig (Proxy @(BlockProtocol blk)) ei
    . unwrapPartialConsensusConfig

completeConsensusConfig'' ::
  forall blk.
  HasPartialConsensusConfig (BlockProtocol blk) =>
  EpochInfo (Except PastHorizonException) ->
  WrapPartialConsensusConfig blk ->
  WrapConsensusConfig blk
completeConsensusConfig'' ei =
  WrapConsensusConfig
    . completeConsensusConfig (Proxy @(BlockProtocol blk)) ei
    . unwrapPartialConsensusConfig

distribLedgerConfig ::
  CanHardFork xs =>
  EpochInfo (Except PastHorizonException) ->
  LedgerConfig (HardForkBlock xs) ->
  NP WrapLedgerConfig xs
distribLedgerConfig ei cfg =
  hcmap
    proxySingle
    (completeLedgerConfig'' ei)
    (getPerEraLedgerConfig $ hardForkLedgerConfigPerEra cfg)

distribTopLevelConfig ::
  All SingleEraBlock xs =>
  EpochInfo (Except PastHorizonException) ->
  TopLevelConfig (HardForkBlock xs) ->
  NP TopLevelConfig xs
distribTopLevelConfig ei tlc =
  hcpure
    proxySingle
    ( fn_5
        ( \cfgConsensus cfgLedger cfgBlock cfgCodec cfgStorage ->
            mkTopLevelConfig
              (completeConsensusConfig' ei cfgConsensus)
              (completeLedgerConfig' ei cfgLedger)
              cfgBlock
              cfgCodec
              cfgStorage
              -- topLevelConfigCheckpoints is only used in validateEnvelope,
              -- where it comes from the TopLevelConfig of the HardForkBlock.
              --
              -- The checkpoints of the underlying blocks are not used.
              emptyCheckpointsMap
        )
    )
    `hap` ( getPerEraConsensusConfig $
              hardForkConsensusConfigPerEra (configConsensus tlc)
          )
    `hap` ( getPerEraLedgerConfig $
              hardForkLedgerConfigPerEra (configLedger tlc)
          )
    `hap` ( getPerEraBlockConfig $
              hardForkBlockConfigPerEra (configBlock tlc)
          )
    `hap` ( getPerEraCodecConfig $
              hardForkCodecConfigPerEra (configCodec tlc)
          )
    `hap` ( getPerEraStorageConfig $
              hardForkStorageConfigPerEra (configStorage tlc)
          )

{-------------------------------------------------------------------------------
  LedgerSupportsPeras
-------------------------------------------------------------------------------}

instance CanHardFork xs => ALedgerStateSupportsPeras (LedgerState (HardForkBlock xs) mk) where
  getPoolDistr =
    hcollapse
      . hcmap proxySingle (K . getPoolDistr . unFlip)
      . State.tip
      . hardForkLedgerStatePerEra

{-------------------------------------------------------------------------------
  BlockSupportsPeras
-------------------------------------------------------------------------------}

-- newtype WrapBoostedBlock blk = WrapBoostedBlock {unwrapBoostedBlock :: BoostedBlock blk}
-- deriving instance Show (BoostedBlock blk) => Show (WrapBoostedBlock blk)
-- deriving instance Eq (BoostedBlock blk) => Eq (WrapBoostedBlock blk)
-- deriving instance Generic (WrapBoostedBlock blk)
-- deriving instance NoThunks (BoostedBlock blk) => NoThunks (WrapBoostedBlock blk)

-- newtype OneEraBoostedBlock xs = OneEraBoostedBlock {getOneEraBoostedBlock :: NS WrapBoostedBlock xs}
-- deriving via LiftNS WrapBoostedBlock xs instance CanHardFork xs => Show (OneEraBoostedBlock xs)
-- deriving via LiftNS WrapBoostedBlock xs instance CanHardFork xs => Eq (OneEraBoostedBlock xs)
-- deriving instance Generic (OneEraBoostedBlock xs)
-- deriving via LiftNamedNS "OneEraBoostedBlock" WrapBoostedBlock xs instance CanHardFork xs => NoThunks (OneEraBoostedBlock xs)

-- case NonEmpty.nonEmpty (map getNS (NonEmpty.toList votes)) of
--   Nothing -> Nothing
--   Just ns ->
--     Just $
--       UniqueVotesWithSameTarget
--         electionId
--         candidate
--         (NonEmpty.fromList ns)
--

-- | Ensure that all elements of a non-empty list of 'NS' values are in the same
-- era, collecting them into a single 'NS' containing a 'NonEmpty'.
ensureSameEraNonEmpty ::
  SListI xs =>
  NonEmpty (NS f xs) ->
  Maybe (NS (NonEmpty :.: f) xs)
ensureSameEraNonEmpty (x :| rest) =
  foldl go (Just $ hmap (Comp . (:| [])) x) rest
 where
  go Nothing _ =
    Nothing
  go (Just acc) ns =
    case matchNS acc ns of
      Left _mismatch ->
        Nothing
      Right nsPair ->
        Just $ hmap (\(Pair (Comp fs) f) -> Comp (fs <> (f :| []))) nsPair

ensureSameEraNonEmptyMap ::
  (Ord k, All Top xs) =>
  NEMap k (NS f xs) ->
  Maybe (NS (NEMap k :.: f) xs)
ensureSameEraNonEmptyMap neMap =
  let keyValPairs =
        (\(k, v) -> hmap (\v' -> Pair (K k) v') v) <$> NEMap.toList neMap
   in case ensureSameEraNonEmpty keyValPairs of
        Nothing -> Nothing
        Just ns ->
          Just $
            hmap
              ( \(Comp neKeyValPairs) ->
                  Comp $ NEMap.fromList $ (\(Pair (K k) v) -> (k, v)) <$> neKeyValPairs
              )
              ns

-- | Ensure that two 'NS' values are in the same era, pairing them together.
-- Returns 'Left ParamsEraMismatch' if they are from different eras.
ensureSameEraPair ::
  (NS f xs, NS g xs) ->
  Maybe (NS (Product f g) xs)
ensureSameEraPair (l, r) =
  case matchNS l r of
    Left _mismatch -> Nothing
    Right nsQueryResAndLedgerView -> Just nsQueryResAndLedgerView

alignNpWithNs ::
  All Top xs =>
  NP f xs ->
  NS g xs ->
  NS (Product f g) xs
alignNpWithNs = hzipWith Pair

newtype WrapUniqueVotesWithSameTarget x
  = WrapUniqueVotesWithSameTarget
  { unwrapUniqueVotesWithSameTarget ::
      UniqueVotesWithSameTarget
        (PerasCrypto x)
        (PerasVotingCommitteeScheme x)
  }

projectHFCUniqueVotesWithSameTarget ::
  All SingleEraBlockWithPeras xs =>
  UniqueVotesWithSameTarget
    (PerasCrypto (HardForkBlock xs))
    (PerasVotingCommitteeScheme (HardForkBlock xs)) ->
  Maybe (NS WrapUniqueVotesWithSameTarget xs)
projectHFCUniqueVotesWithSameTarget uniqueVotesWithSameTarget =
  case ensureSameEraNonEmpty (getOneEraPerasCommitteeVote <$> getRawVotes uniqueVotesWithSameTarget) of
    Nothing ->
      Nothing
    Just ns ->
      Just $
        hcmap
          proxySingleWithPeras
          ( \(Comp votes) ->
              WrapUniqueVotesWithSameTarget
                . unsafeUniqueVotesWithSameTarget
                . fmap unwrapPerasCommitteeVote
                $ votes
          )
          ns

-- NOTE: Here I take the 'PerasEpochContext' as an argument so we can
-- assert that quorum is reached when constructing the 'PerasVoteCollectionWithQuorum' through the unsafe function,
-- even thought we theoretically don't need to check it since we actually just transform an existing, properly built 'PerasVoteCollectionWithQuorum'.
projectHFCPerasVoteCollectionWithQuorum ::
  All SingleEraBlockWithPeras xs =>
  -- | Just needed for quorum checking is assert in the unsafe function call
  PerasEpochContext (HardForkBlock xs) ->
  PerasVoteCollectionWithQuorum (HardForkBlock xs) ->
  Maybe (NS PerasVoteCollectionWithQuorum xs)
projectHFCPerasVoteCollectionWithQuorum hfcContext hfcCollection =
  let votes = pvcVotes $ forgetQuorum hfcCollection
      neMapNs = projectHFCWatValidatedPerasVote <$> votes
   in case ensureSameEraNonEmptyMap neMapNs of
        Nothing -> Nothing
        Just nsNeMap -> case ensureSameEraPair (projectHFCPerasContext hfcContext, nsNeMap) of
          Nothing -> Nothing
          Just nsPair ->
            Just $
              hcmap
                proxySingleWithPeras
                ( \(Pair context (Comp compedValMap)) ->
                    unsafePerasVoteCollectionWithQuorum
                      (pecParams context)
                      ((\(Comp v) -> v) <$> compedValMap)
                )
                nsPair

-- NOTE: this assumes PerasParams are the same for all eras.
--
-- In the future, @pecParams@ will be an @NP@ of @PerasParams@.
projectHFCPerasContext ::
  All Top xs =>
  PerasEpochContext (HardForkBlock xs) ->
  NS PerasEpochContext xs
projectHFCPerasContext PerasEpochContext{pecCommittee, pecParams} =
  hmap
    ( \(WrapPerasVotingCommittee committee) ->
        PerasEpochContext
          { pecCommittee = committee
          , pecParams = retagPerasParams pecParams
          }
    )
    . getOneEraPerasVotingCommittee
    $ pecCommittee

class
  ( BoostedBlock (PerasVote blk) ~ boostedBlock
  , BoostedBlock (PerasCert blk) ~ boostedBlock
  ) =>
  IsBoostedBlock boostedBlock blk
class ElectionId (PerasCrypto blk) ~ electionId => IsElectionId electionId blk
type instance ElectionId (OneEraPerasCrypto xs) = PerasRoundNo
type instance BoostedBlock (OneEraPerasVote xs) = PerasBoostedBlock
type instance BoostedBlock (OneEraPerasCert xs) = PerasBoostedBlock
type instance VoteCandidate (OneEraPerasCrypto xs) = PerasBoostedBlock
type instance PrivateKey (OneEraPerasCrypto xs) = PerEraPerasPrivateKey xs
type instance PublicKey (OneEraPerasCrypto xs) = OneEraPerasPublicKey xs

newtype PerEraPerasPrivateKey xs = PerEraPerasPrivateKey
  { getPerEraPerasPrivateKey :: NP WrapPerasPrivateKey xs
  }

newtype OneEraPerasPublicKey xs = OneEraPerasPublicKey
  { getOneEraPerasPublicKey :: NS WrapPerasPublicKey xs
  }

newtype WrapPerasPublicKey blk = WrapPerasPublicKey
  { unwrapPerasPublicKey :: PublicKey (PerasCrypto blk)
  }

newtype WrapPerasPrivateKey blk = WrapPerasPrivateKey
  { unwrapPerasPrivateKey :: PrivateKey (PerasCrypto blk)
  }

newtype WrapPerasVotingCommittee blk = WrapPerasVotingCommittee
  { unwrapPerasVotingCommittee :: VotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)
  }
newtype WrapPerasVotingCommitteeInput blk = WrapPerasVotingCommitteeInput
  { unwrapPerasVotingCommitteeInput ::
      VotingCommitteeInput (PerasCrypto blk) (PerasVotingCommitteeScheme blk)
  }
newtype WrapPerasVotingCommitteeError blk = WrapPerasVotingCommitteeError
  { unwrapPerasVotingCommitteeError ::
      VotingCommitteeError (PerasCrypto blk) (PerasVotingCommitteeScheme blk)
  }
newtype WrapPerasEligibilityWitness blk = WrapPerasEligibilityWitness
  { unwrapPerasEligibilityWitness ::
      EligibilityWitness (PerasCrypto blk) (PerasVotingCommitteeScheme blk)
  }
newtype WrapPerasCommitteeVote blk = WrapPerasCommitteeVote
  {unwrapPerasCommitteeVote :: Vote (PerasCrypto blk) (PerasVotingCommitteeScheme blk)}
newtype WrapPerasCommitteeCert blk = WrapPerasCommitteeCert
  {unwrapPerasCommitteeCert :: Cert (PerasCrypto blk) (PerasVotingCommitteeScheme blk)}

newtype OneEraPerasVotingCommitteeError xs = OneEraPerasVotingCommitteeError
  { getOneEraPerasVotingCommitteeError :: NS WrapPerasVotingCommitteeError xs
  }

class
  (SingleEraBlock x, IsBoostedBlock PerasBoostedBlock x, IsElectionId PerasRoundNo x) =>
  SingleEraBlockWithPeras x

proxySingleWithPeras :: Proxy SingleEraBlockWithPeras
proxySingleWithPeras = Proxy

-- type InnerF x = Either :.:

newtype EitherF f g x = EitherF {unEitherF :: Either (f x) (g x)}

mkEitherF :: (a -> f x) -> (b -> g x) -> Either a b -> EitherF f g x
mkEitherF f g = EitherF . bimap f g

hcollect ::
  All Top xs =>
  NS (EitherF f g) xs ->
  Either (NS f xs) (NS g xs)
hcollect ns = hcollapse $ himap f ns
 where
  f idx (EitherF (Left fx)) = K $ Left $ injectNS idx fx
  f idx (EitherF (Right gx)) = K $ Right $ injectNS idx gx

newtype WrapPerasVoteSignature blk = WrapPerasVoteSignature
  { unwrapPerasVoteSignature :: VoteSignature (PerasCrypto blk)
  }

newtype WrapPerasVoteVerificationKey blk = WrapPerasVoteVerificationKey
  { unwrapPerasVoteVerificationKey :: VoteVerificationKey (PerasCrypto blk)
  }

newtype WrapPerasVoteSigningKey blk = WrapPerasVoteSigningKey
  { unwrapPerasVoteSigningKey :: VoteSigningKey (PerasCrypto blk)
  }

newtype PerEraPerasVoteSigningKey xs = PerEraPerasVoteSigningKey
  { getPerEraPerasVoteSigningKey :: NP WrapPerasVoteSigningKey xs
  }

newtype OneEraPerasVoteVerificationKey xs = OneEraPerasVoteVerificationKey
  { getOneEraPerasVoteVerificationKey :: NS WrapPerasVoteVerificationKey xs
  }

{-------------------------------------------------------------------------------
  Standard instances for the Peras wrapper types

  'Show', 'Eq', 'NoThunks' and 'Generic' for the per-era 'Wrap*' newtypes, plus
  'Generic' for the 'OneEra*' / 'PerEra*' n-ary wrappers. The 'Wrap*' instances
  are constrained on the corresponding per-era instance, mirroring the wrappers
  in "Ouroboros.Consensus.TypeFamilyWrappers".

  NOTE: 'Show' / 'Eq' / 'NoThunks' cannot be lifted to the 'OneEra*' / 'PerEra*'
  wrappers for the key, signature and committee-error fields: neither
  'CryptoSupportsVoteSigning' nor 'CryptoSupportsVotingCommittee' provide those
  instances as superclasses for their associated types, and 'BlockSupportsPeras'
  only guarantees them for 'PerasVotingCommittee' (not for the other committee
  types). Hence there is nothing to lift via 'LiftNS' / 'LiftNP', and only
  'Generic' (which is structural) is available for those wrappers.
-------------------------------------------------------------------------------}

deriving newtype instance
  Show (UniqueVotesWithSameTarget (PerasCrypto x) (PerasVotingCommitteeScheme x)) =>
  Show (WrapUniqueVotesWithSameTarget x)
deriving newtype instance
  Eq (UniqueVotesWithSameTarget (PerasCrypto x) (PerasVotingCommitteeScheme x)) =>
  Eq (WrapUniqueVotesWithSameTarget x)
deriving newtype instance
  NoThunks (UniqueVotesWithSameTarget (PerasCrypto x) (PerasVotingCommitteeScheme x)) =>
  NoThunks (WrapUniqueVotesWithSameTarget x)
deriving stock instance Generic (WrapUniqueVotesWithSameTarget x)

deriving newtype instance
  Show (PublicKey (PerasCrypto blk)) =>
  Show (WrapPerasPublicKey blk)
deriving newtype instance
  Eq (PublicKey (PerasCrypto blk)) =>
  Eq (WrapPerasPublicKey blk)
deriving newtype instance
  NoThunks (PublicKey (PerasCrypto blk)) =>
  NoThunks (WrapPerasPublicKey blk)
deriving stock instance Generic (WrapPerasPublicKey blk)

deriving newtype instance
  Show (PrivateKey (PerasCrypto blk)) =>
  Show (WrapPerasPrivateKey blk)
deriving newtype instance
  Eq (PrivateKey (PerasCrypto blk)) =>
  Eq (WrapPerasPrivateKey blk)
deriving newtype instance
  NoThunks (PrivateKey (PerasCrypto blk)) =>
  NoThunks (WrapPerasPrivateKey blk)
deriving stock instance Generic (WrapPerasPrivateKey blk)

-- 'Show' / 'Eq' / 'NoThunks' for 'WrapPerasVotingCommittee' are in the
-- BlockSupportsPeras superclass-instances section below.
deriving stock instance Generic (WrapPerasVotingCommittee blk)

deriving newtype instance
  Show (VotingCommitteeInput (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Show (WrapPerasVotingCommitteeInput blk)
deriving newtype instance
  Eq (VotingCommitteeInput (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Eq (WrapPerasVotingCommitteeInput blk)
deriving newtype instance
  NoThunks (VotingCommitteeInput (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  NoThunks (WrapPerasVotingCommitteeInput blk)
deriving stock instance Generic (WrapPerasVotingCommitteeInput blk)

deriving newtype instance
  Show (VotingCommitteeError (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Show (WrapPerasVotingCommitteeError blk)
deriving newtype instance
  Eq (VotingCommitteeError (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Eq (WrapPerasVotingCommitteeError blk)
deriving newtype instance
  NoThunks (VotingCommitteeError (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  NoThunks (WrapPerasVotingCommitteeError blk)
deriving stock instance Generic (WrapPerasVotingCommitteeError blk)

deriving newtype instance
  Show (EligibilityWitness (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Show (WrapPerasEligibilityWitness blk)
deriving newtype instance
  Eq (EligibilityWitness (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Eq (WrapPerasEligibilityWitness blk)
deriving newtype instance
  NoThunks (EligibilityWitness (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  NoThunks (WrapPerasEligibilityWitness blk)
deriving stock instance Generic (WrapPerasEligibilityWitness blk)

deriving newtype instance
  Show (Vote (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Show (WrapPerasCommitteeVote blk)
deriving newtype instance
  Eq (Vote (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Eq (WrapPerasCommitteeVote blk)
deriving newtype instance
  NoThunks (Vote (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  NoThunks (WrapPerasCommitteeVote blk)
deriving stock instance Generic (WrapPerasCommitteeVote blk)

deriving newtype instance
  Show (Cert (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Show (WrapPerasCommitteeCert blk)
deriving newtype instance
  Eq (Cert (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Eq (WrapPerasCommitteeCert blk)
deriving newtype instance
  NoThunks (Cert (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  NoThunks (WrapPerasCommitteeCert blk)
deriving stock instance Generic (WrapPerasCommitteeCert blk)

deriving newtype instance
  Show (VoteSignature (PerasCrypto blk)) =>
  Show (WrapPerasVoteSignature blk)
deriving newtype instance
  Eq (VoteSignature (PerasCrypto blk)) =>
  Eq (WrapPerasVoteSignature blk)
deriving newtype instance
  NoThunks (VoteSignature (PerasCrypto blk)) =>
  NoThunks (WrapPerasVoteSignature blk)
deriving stock instance Generic (WrapPerasVoteSignature blk)

deriving newtype instance
  Show (VoteVerificationKey (PerasCrypto blk)) =>
  Show (WrapPerasVoteVerificationKey blk)
deriving newtype instance
  Eq (VoteVerificationKey (PerasCrypto blk)) =>
  Eq (WrapPerasVoteVerificationKey blk)
deriving newtype instance
  NoThunks (VoteVerificationKey (PerasCrypto blk)) =>
  NoThunks (WrapPerasVoteVerificationKey blk)
deriving stock instance Generic (WrapPerasVoteVerificationKey blk)

deriving newtype instance
  Show (VoteSigningKey (PerasCrypto blk)) =>
  Show (WrapPerasVoteSigningKey blk)
deriving newtype instance
  Eq (VoteSigningKey (PerasCrypto blk)) =>
  Eq (WrapPerasVoteSigningKey blk)
deriving newtype instance
  NoThunks (VoteSigningKey (PerasCrypto blk)) =>
  NoThunks (WrapPerasVoteSigningKey blk)
deriving stock instance Generic (WrapPerasVoteSigningKey blk)

-- 'OneEra*' / 'PerEra*' wrappers: only 'Generic' is available (see NOTE above).
deriving stock instance Generic (PerEraPerasPrivateKey xs)
deriving stock instance Generic (OneEraPerasPublicKey xs)
deriving stock instance Generic (OneEraPerasVotingCommitteeError xs)
deriving stock instance Generic (PerEraPerasVoteSigningKey xs)
deriving stock instance Generic (OneEraPerasVoteVerificationKey xs)

instance
  ( CanHardFork xs
  , -- , All (IsBoostedBlock PerasBoostedBlock) xs
    -- , All (IsElectionId PerasRoundNo) xs
    All SingleEraBlockWithPeras xs
  ) =>
  CryptoSupportsVoteSigning (OneEraPerasCrypto xs)
  where
  type VoteSigningKey (OneEraPerasCrypto xs) = PerEraPerasVoteSigningKey xs

  -- \| Key used for verifying votes
  type VoteVerificationKey (OneEraPerasCrypto xs) = OneEraPerasVoteVerificationKey xs

  -- \| Cryptographic signature of a vote
  newtype VoteSignature (OneEraPerasCrypto xs) = PerEraPerasVoteSignature
    { getPerEraPerasVoteSignature :: NP WrapPerasVoteSignature xs
    }

  getVoteSigningKey _proxy (PerEraPerasPrivateKey privKey) =
    PerEraPerasVoteSigningKey $ hcmap proxySingleWithPeras dispatchKey privKey
   where
    dispatchKey ::
      forall blk. SingleEraBlockWithPeras blk => WrapPerasPrivateKey blk -> WrapPerasVoteSigningKey blk
    dispatchKey = WrapPerasVoteSigningKey . getVoteSigningKey (Proxy @(PerasCrypto blk)) . unwrapPerasPrivateKey

  getVoteVerificationKey _proxy (OneEraPerasPublicKey pubKey) =
    OneEraPerasVoteVerificationKey $ hcmap proxySingleWithPeras dispatchKey pubKey
   where
    dispatchKey ::
      forall blk.
      SingleEraBlockWithPeras blk => WrapPerasPublicKey blk -> WrapPerasVoteVerificationKey blk
    dispatchKey =
      WrapPerasVoteVerificationKey
        . getVoteVerificationKey (Proxy @(PerasCrypto blk))
        . unwrapPerasPublicKey

  signVote signingKey roundNo boostedBlock =
    PerEraPerasVoteSignature
      . hcmap
        proxySingleWithPeras
        (\(WrapPerasVoteSigningKey sk) -> WrapPerasVoteSignature $ signVote sk roundNo boostedBlock)
      . getPerEraPerasVoteSigningKey
      $ signingKey

  verifyVoteSignature (OneEraPerasVoteVerificationKey verKey) roundNo boostedBlock (PerEraPerasVoteSignature voteSignature) =
    let nsSignatureVerKey = alignNpWithNs voteSignature verKey
     in hcollapse
          . hcmap
            proxySingleWithPeras
            ( \(Pair (WrapPerasVoteSignature sig) (WrapPerasVoteVerificationKey vk)) ->
                K $ verifyVoteSignature vk roundNo boostedBlock sig
            )
          $ nsSignatureVerKey

instance
  ( CanHardFork xs
  , -- , All (IsBoostedBlock PerasBoostedBlock) xs
    -- , All (IsElectionId PerasRoundNo) xs
    All SingleEraBlockWithPeras xs
  ) =>
  CryptoSupportsVotingCommittee (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs)
  where
  newtype VotingCommittee (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs) = OneEraPerasVotingCommittee
    { getOneEraPerasVotingCommittee :: NS WrapPerasVotingCommittee xs
    }

  newtype VotingCommitteeInput (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs) = OneEraPerasVotingCommitteeInput
    { getOneEraPerasVotingCommitteeInput :: NS WrapPerasVotingCommitteeInput xs
    }

  data VotingCommitteeError (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs)
    = HardForkVotingCommitteeErrorEraMismatch
    | HardForkVotingCommitteeErrorOneEraVotingCommitteeError (OneEraPerasVotingCommitteeError xs)

  newtype EligibilityWitness (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs) = OneEraPerasEligibilityWitness
    { getOneEraPerasEligibilityWitness :: NS WrapPerasEligibilityWitness xs
    }

  newtype Vote (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs) = OneEraPerasCommitteeVote
    { getOneEraPerasCommitteeVote :: NS WrapPerasCommitteeVote xs
    }

  newtype Cert (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs) = OneEraPerasCommitteeCert
    { getOneEraPerasCommitteeCert :: NS WrapPerasCommitteeCert xs
    }

  voteTarget =
    hcollapse
      . hcmap proxySingleWithPeras (K . voteTarget . unwrapPerasCommitteeVote)
      . getOneEraPerasCommitteeVote

  compareVotesById vote1 vote2 =
    case ensureSameEraPair
      ( getOneEraPerasCommitteeVote vote1
      , getOneEraPerasCommitteeVote vote2
      ) of
      Nothing -> error "compareVotesById: votes are from different eras"
      Just nsPair ->
        hcollapse
          . hcmap proxySingleWithPeras (K . uncurry compareVotesById . unwrapVotePair)
          $ nsPair
   where
    unwrapVotePair (Pair (WrapPerasCommitteeVote v1) (WrapPerasCommitteeVote v2)) = (v1, v2)

  mkVotingCommittee =
    bimap
      (HardForkVotingCommitteeErrorOneEraVotingCommitteeError . OneEraPerasVotingCommitteeError)
      OneEraPerasVotingCommittee
      . hcollect
      . hcmap
        proxySingleWithPeras
        ( \(WrapPerasVotingCommitteeInput input) ->
            mkEitherF
              WrapPerasVotingCommitteeError
              WrapPerasVotingCommittee
              $ mkVotingCommittee input
        )
      . getOneEraPerasVotingCommitteeInput

  checkShouldVote committee poolId privKey electionId =
    let nsPrivKeyCommittee =
          alignNpWithNs
            (getPerEraPerasPrivateKey privKey)
            (getOneEraPerasVotingCommittee committee)
     in bimap
          (HardForkVotingCommitteeErrorOneEraVotingCommitteeError . OneEraPerasVotingCommitteeError)
          (fmap OneEraPerasEligibilityWitness . hsequence')
          . hcollect
          . hcmap
            proxySingleWithPeras
            ( \(Pair (WrapPerasPrivateKey privKey') (WrapPerasVotingCommittee committee')) ->
                mkEitherF
                  WrapPerasVotingCommitteeError
                  (Comp . fmap WrapPerasEligibilityWitness)
                  $ checkShouldVote committee' poolId privKey' electionId
            )
          $ nsPrivKeyCommittee

  forgeVote witness privKey electionId candidate =
    let nsPrivKeyWitness =
          alignNpWithNs
            (getPerEraPerasPrivateKey privKey)
            (getOneEraPerasEligibilityWitness witness)
     in OneEraPerasCommitteeVote
          . hcmap
            proxySingleWithPeras
            ( \(Pair (WrapPerasPrivateKey privKey') (WrapPerasEligibilityWitness witness')) ->
                WrapPerasCommitteeVote $
                  forgeVote witness' privKey' electionId candidate
            )
          $ nsPrivKeyWitness

  verifyVote committee vote =
    case ensureSameEraPair
      ( getOneEraPerasVotingCommittee committee
      , getOneEraPerasCommitteeVote vote
      ) of
      Nothing ->
        Left HardForkVotingCommitteeErrorEraMismatch
      Just nsCommitteeVote ->
        bimap
          (HardForkVotingCommitteeErrorOneEraVotingCommitteeError . OneEraPerasVotingCommitteeError)
          OneEraPerasEligibilityWitness
          . hcollect
          . hcmap
            proxySingleWithPeras
            ( \(Pair (WrapPerasVotingCommittee committee') (WrapPerasCommitteeVote vote')) ->
                mkEitherF
                  WrapPerasVotingCommitteeError
                  WrapPerasEligibilityWitness
                  $ verifyVote committee' vote'
            )
          $ nsCommitteeVote

  eligiblePartyVoteWeight committee witness =
    case ensureSameEraPair
      ( getOneEraPerasVotingCommittee committee
      , getOneEraPerasEligibilityWitness witness
      ) of
      Nothing ->
        error "eligiblePartyVoteWeight: committee and witness are from different eras"
      Just nsCommitteeWitness ->
        hcollapse
          . hcmap
            proxySingleWithPeras
            ( \(Pair (WrapPerasVotingCommittee committee') (WrapPerasEligibilityWitness witness')) ->
                K $ eligiblePartyVoteWeight committee' witness'
            )
          $ nsCommitteeWitness

  forgeCert uniqueVotesWithSameTarget =
    case projectHFCUniqueVotesWithSameTarget uniqueVotesWithSameTarget of
      Nothing ->
        Left HardForkVotingCommitteeErrorEraMismatch
      Just nsVotes ->
        bimap
          (HardForkVotingCommitteeErrorOneEraVotingCommitteeError . OneEraPerasVotingCommitteeError)
          OneEraPerasCommitteeCert
          . hcollect
          . hcmap
            proxySingleWithPeras
            ( \(WrapUniqueVotesWithSameTarget votes) ->
                mkEitherF
                  WrapPerasVotingCommitteeError
                  WrapPerasCommitteeCert
                  $ forgeCert votes
            )
          $ nsVotes

  verifyCert committee cert =
    case ensureSameEraPair
      ( getOneEraPerasVotingCommittee committee
      , getOneEraPerasCommitteeCert cert
      ) of
      Nothing ->
        Left HardForkVotingCommitteeErrorEraMismatch
      Just nsCommitteeCert ->
        bimap
          (HardForkVotingCommitteeErrorOneEraVotingCommitteeError . OneEraPerasVotingCommitteeError)
          (fmap OneEraPerasEligibilityWitness . hsequence')
          . hcollect
          . hcmap
            proxySingleWithPeras
            ( \(Pair (WrapPerasVotingCommittee committee') (WrapPerasCommitteeCert cert')) ->
                mkEitherF
                  WrapPerasVotingCommitteeError
                  (Comp . fmap WrapPerasEligibilityWitness)
                  $ verifyCert committee' cert'
            )
          $ nsCommitteeCert

injectHFCValidatedPerasVote ::
  All Top xs => NS ValidatedPerasVote xs -> ValidatedPerasVote (HardForkBlock xs)
injectHFCValidatedPerasVote ns =
  ValidatedPerasVote
    { vpvVote = OneEraPerasVote (hmap (WrapPerasVote . vpvVote) ns)
    , vpvVoteWeight = hcollapse (hmap (K . vpvVoteWeight) ns)
    }

injectHFCValidatedPerasCert ::
  All Top xs => NS ValidatedPerasCert xs -> ValidatedPerasCert (HardForkBlock xs)
injectHFCValidatedPerasCert ns =
  ValidatedPerasCert
    { vpcCert = OneEraPerasCert (hmap (WrapPerasCert . vpcCert) ns)
    , vpcCertBoost = hcollapse (hmap (K . vpcCertBoost) ns)
    }

projectHFCValidatedPerasVote ::
  All Top xs =>
  ValidatedPerasVote (HardForkBlock xs) ->
  NS ValidatedPerasVote xs
projectHFCValidatedPerasVote ValidatedPerasVote{vpvVote = OneEraPerasVote vote, vpvVoteWeight} =
  hmap (\(WrapPerasVote v) -> ValidatedPerasVote{vpvVote = v, vpvVoteWeight}) vote

projectHFCWatValidatedPerasVote ::
  All Top xs =>
  WithArrivalTime (ValidatedPerasVote (HardForkBlock xs)) ->
  NS (WithArrivalTime :.: ValidatedPerasVote) xs
projectHFCWatValidatedPerasVote (WithArrivalTime arrivalTime validatedVote) =
  hmap (\v -> Comp (WithArrivalTime arrivalTime v)) (projectHFCValidatedPerasVote validatedVote)

{-------------------------------------------------------------------------------
  BlockSupportsPeras superclass instances for the OneEra* Peras types

  These instances are needed by the (large) superclass context of
  'BlockSupportsPeras (HardForkBlock xs)'. They all dispatch to the
  corresponding per-era instances, which are available under
  'SingleEraBlockWithPeras'.
-------------------------------------------------------------------------------}

deriving newtype instance
  Show (VotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Show (WrapPerasVotingCommittee blk)
deriving newtype instance
  Eq (VotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Eq (WrapPerasVotingCommittee blk)
deriving newtype instance
  NoThunks (VotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  NoThunks (WrapPerasVotingCommittee blk)

deriving via
  LiftNS WrapPerasVotingCommittee xs
  instance
    CanHardFork xs =>
    Show (VotingCommittee (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs))
deriving via
  LiftNS WrapPerasVotingCommittee xs
  instance
    CanHardFork xs =>
    Eq (VotingCommittee (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs))
deriving via
  LiftNamedNS "OneEraPerasVotingCommittee" WrapPerasVotingCommittee xs
  instance
    CanHardFork xs =>
    NoThunks (VotingCommittee (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs))

instance
  (CanHardFork xs, All SingleEraBlockWithPeras xs) =>
  IsPerasVote (OneEraPerasVote xs) (HardForkBlock xs)
  where
  getPerasVoteRound =
    hcollapse
      . hcmap proxySingleWithPeras (K . getPerasVoteRound . unwrapPerasVote)
      . getOneEraPerasVote
  getPerasVoteSeatIndex =
    hcollapse
      . hcmap proxySingleWithPeras (K . getPerasVoteSeatIndex . unwrapPerasVote)
      . getOneEraPerasVote
  getPerasVoteBlock =
    hcollapse
      . hcmap proxySingleWithPeras (K . getPerasVoteBlock . unwrapPerasVote)
      . getOneEraPerasVote

instance
  (CanHardFork xs, All SingleEraBlockWithPeras xs) =>
  IsPerasCert (OneEraPerasCert xs) (HardForkBlock xs)
  where
  getPerasCertRound =
    hcollapse
      . hcmap proxySingleWithPeras (K . getPerasCertRound . unwrapPerasCert)
      . getOneEraPerasCert
  getPerasCertBlock =
    hcollapse
      . hcmap proxySingleWithPeras (K . getPerasCertBlock . unwrapPerasCert)
      . getOneEraPerasCert

instance
  (CanHardFork xs, All SingleEraBlockWithPeras xs) =>
  IsPerasError (HardForkPerasError xs) (HardForkBlock xs)
  where
  injectVotingCommitteeError err = case err of
    HardForkVotingCommitteeErrorEraMismatch ->
      HardForkPerasErrorEraMismatch
    HardForkVotingCommitteeErrorOneEraVotingCommitteeError (OneEraPerasVotingCommitteeError ns) ->
      HardForkPerasErrorOneEraPerasError
        . OneEraPerasError
        $ hcmap
          proxySingleWithPeras
          (\(WrapPerasVotingCommitteeError e) -> WrapPerasError (injectVotingCommitteeError e))
          ns

  -- NOTE: in practice this is never produced at the HFC level
  injectConversionError = HardForkPerasErrorConversionError

  -- NOTE: in practice this is never produced at the HFC level
  injectQuorumNotReachedError = HardForkPerasErrorQuorumNotReachedError

instance
  (CanHardFork xs, All SingleEraBlockWithPeras xs) =>
  PerasVoteCompatibleWithVotingCommittee
    (OneEraPerasVote xs)
    (OneEraPerasCrypto xs)
    (OneEraPerasVotingCommitteeScheme xs)
  where
  toPerasVote =
    fmap OneEraPerasVote
      . hsequence'
      . hcmap
        proxySingleWithPeras
        (Comp . fmap WrapPerasVote . toPerasVote . unwrapPerasCommitteeVote)
      . getOneEraPerasCommitteeVote
  fromPerasVote =
    fmap OneEraPerasCommitteeVote
      . hsequence'
      . hcmap
        proxySingleWithPeras
        (Comp . fmap WrapPerasCommitteeVote . fromPerasVote . unwrapPerasVote)
      . getOneEraPerasVote

instance
  (CanHardFork xs, All SingleEraBlockWithPeras xs) =>
  PerasCertCompatibleWithVotingCommittee
    (OneEraPerasCert xs)
    (OneEraPerasCrypto xs)
    (OneEraPerasVotingCommitteeScheme xs)
  where
  toPerasCert =
    fmap OneEraPerasCert
      . hsequence'
      . hcmap
        proxySingleWithPeras
        (Comp . fmap WrapPerasCert . toPerasCert . unwrapPerasCommitteeCert)
      . getOneEraPerasCommitteeCert
  fromPerasCert =
    fmap OneEraPerasCommitteeCert
      . hsequence'
      . hcmap
        proxySingleWithPeras
        (Comp . fmap WrapPerasCommitteeCert . fromPerasCert . unwrapPerasCert)
      . getOneEraPerasCert

-- TODO: we need to change the binary representation of votes and certs to carry
-- era-specific/versionning information, to allow future evolutions
instance
  ( StandardHash (HardForkBlock xs)
  , CanHardFork xs
  , All SingleEraBlockWithPeras xs
  ) =>
  BlockSupportsPeras (HardForkBlock xs)
  where
  type PerasVote (HardForkBlock xs) = OneEraPerasVote xs
  type PerasCert (HardForkBlock xs) = OneEraPerasCert xs
  type PerasError (HardForkBlock xs) = HardForkPerasError xs
  type PerasCrypto (HardForkBlock xs) = OneEraPerasCrypto xs
  type PerasVotingCommitteeScheme (HardForkBlock xs) = OneEraPerasVotingCommitteeScheme xs

  forgePerasVoteIfEligible context poolId privKey roundNo point =
    let
      nsPrivKeyContext =
        alignNpWithNs
          (getPerEraPerasPrivateKey privKey)
          (projectHFCPerasContext context)
     in
      bimap
        (HardForkPerasErrorOneEraPerasError . OneEraPerasError)
        (fmap injectHFCValidatedPerasVote . hsequence')
        . hcollect
        . hcmap
          proxySingleWithPeras
          ( \(Pair (WrapPerasPrivateKey privKey') context') ->
              mkEitherF
                WrapPerasError
                Comp
                $ forgePerasVoteIfEligible
                  context'
                  poolId
                  privKey'
                  roundNo
                  (castPoint point)
          )
        $ nsPrivKeyContext

  verifyPerasVote context vote =
    case ensureSameEraPair (projectHFCPerasContext context, getOneEraPerasVote vote) of
      Nothing ->
        Left HardForkPerasErrorEraMismatch
      Just nsContextVote ->
        bimap
          (HardForkPerasErrorOneEraPerasError . OneEraPerasError)
          injectHFCValidatedPerasVote
          . hcollect
          . hcmap
            proxySingleWithPeras
            ( \(Pair context' (WrapPerasVote vote')) ->
                mkEitherF
                  WrapPerasError
                  id
                  $ verifyPerasVote context' vote'
            )
          $ nsContextVote

  forgePerasCert context collection =
    case projectHFCPerasVoteCollectionWithQuorum context collection of
      Nothing ->
        Left HardForkPerasErrorEraMismatch
      Just nsCollection -> case ensureSameEraPair (projectHFCPerasContext context, nsCollection) of
        Nothing -> Left HardForkPerasErrorEraMismatch
        Just nsContextCollection ->
          bimap
            (HardForkPerasErrorOneEraPerasError . OneEraPerasError)
            injectHFCValidatedPerasCert
            . hcollect
            . hcmap
              proxySingleWithPeras
              ( \(Pair context' collection') ->
                  mkEitherF
                    WrapPerasError
                    id
                    $ forgePerasCert context' collection'
              )
            $ nsContextCollection

  verifyPerasCert context cert =
    case ensureSameEraPair (projectHFCPerasContext context, getOneEraPerasCert cert) of
      Nothing ->
        Left HardForkPerasErrorEraMismatch
      Just nsContextCert ->
        bimap
          (HardForkPerasErrorOneEraPerasError . OneEraPerasError)
          injectHFCValidatedPerasCert
          . hcollect
          . hcmap
            proxySingleWithPeras
            ( \(Pair context' (WrapPerasCert cert')) ->
                mkEitherF
                  WrapPerasError
                  id
                  $ verifyPerasCert context' cert'
            )
          $ nsContextCert

  getPerasCertInBlock = undefined

  -- hcollapse
  --   . hcmap
  --     proxySingle
  --     (K . (unsafeCastPerasCertV1 <=< getPerasCertInBlock) . unI)
  --   . getOneEraBlock
  --   . getHardForkBlock

  readPerasPrivateKeyFromEnv _proxy = undefined

  blockDoesReallySupportsPeras _proxy = True

-- [TODO PERAS CERTS IN BLOCKS] this is a nasty hack
unsafeCastPerasCertV1 ::
  forall x xs.
  ( Typeable xs
  , Typeable (PerasCert x)
  ) =>
  PerasCert x ->
  Maybe (V1.PerasCert (HardForkBlock xs))
unsafeCastPerasCertV1 cert = do
  let xCertRep = someTypeRep (Proxy @(PerasCert x))
  let xsCertRep = someTypeRep (Proxy @(V1.PerasCert (HardForkBlock xs)))
  if typeRepTyCon xCertRep == typeRepTyCon xsCertRep
    then Just (unsafeCoerce cert)
    else Nothing

{-------------------------------------------------------------------------------
  ConvertRawHash
-------------------------------------------------------------------------------}

instance CanHardFork xs => ConvertRawHash (HardForkBlock xs) where
  toShortRawHash _ = getOneEraHash
  fromShortRawHash _ = OneEraHash
  hashSize _ = getSameValue hashSizes
   where
    hashSizes :: NP (K Word32) xs
    hashSizes = hcpure proxySingle hashSizeOne

    hashSizeOne :: forall blk. SingleEraBlock blk => K Word32 blk
    hashSizeOne = K $ hashSize (Proxy @blk)
