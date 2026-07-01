{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
  , projectHFCPerasContext
  , injectHFCPerasEpochContext
  , projectHFCBoundedPerasEpochContext
  , injectHFCBoundedPerasEpochContext
  , extractHFCPerasEpochContextResolver
  , injectHFCPerasEpochContextResolver
  , EitherF (..)
  , mkEitherF
  , hcollect

    -- ** Convenience re-exports
  , EpochInfo
  , Except

    -- * Serialisation of n-ary sums
  , decodeNS
  , encodeNS
  ) where

import Cardano.Binary (enforceSize)
import Cardano.Slotting.EpochInfo
import Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import qualified Codec.Serialise as Serialise
import Codec.Serialise.Class (Serialise)
import Data.Bifunctor (bimap)
import Data.Functor.Product (Product (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.Maybe.Strict (StrictMaybe (..))
import Data.SOP (I (..), K (..), type (:.:) (..))
import Data.SOP.Constraint
import Data.SOP.Functors
import Data.SOP.Index (Index (..), himap, hizipWith, injectNS, nsFromIndex, nsToIndex)
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
  , BoostedBlockCompatibleWithPoint (..)
  , IsPerasCert (..)
  , IsPerasError (..)
  , IsPerasVote (..)
  , PerasCertCompatibleWithVotingCommittee (..)
  , PerasEpochContext (..)
  , PerasRoundNo
  , PerasVoteCollection (..)
  , PerasVoteCollectionWithQuorum (..)
  , PerasVoteCompatibleWithVotingCommittee (..)
  , ValidatedPerasCert (..)
  , ValidatedPerasVote (..)
  , retagPerasParams
  , unsafeAssumeQuorum
  , unsafePerasVoteCollection
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
import Ouroboros.Consensus.Peras.Context
  ( BoundedPerasEpochContext (..)
  , PerasEpochContextResolver (..)
  )
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util (ShowProxy)

{-------------------------------------------------------------------------------
  Serialisation of n-ary sums ('NS')

  Generic CBOR (de)serialisation of an 'NS': a length-2 list holding the 'Word8'
  era index followed by the selected era's payload. Defined here, rather than in
  "Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common" (which re-exports
  them), so that instances sitting upstream of @Common@ -- such as the
  'VotingCommittee' instance below -- can reuse them.
-------------------------------------------------------------------------------}

encodeNS :: SListI xs => NP (f -.-> K Encoding) xs -> NS f xs -> Encoding
encodeNS es ns =
  mconcat
    [ Enc.encodeListLen 2
    , Enc.encodeWord8 $ nsToIndex ns
    , hcollapse $ hzipWith apFn es ns
    ]

decodeNS :: forall xs f s. SListI xs => NP (Decoder s :.: f) xs -> Decoder s (NS f xs)
decodeNS ds = do
  enforceSize "decodeNS" 2
  i <- Dec.decodeWord8
  case nsFromIndex i of
    Nothing -> fail $ "decodeNS: invalid index " ++ show i
    Just ns -> hcollapse $ hizipWith aux ds ns
 where
  aux ::
    Index xs blk ->
    (Decoder s :.: f) blk ->
    K () blk ->
    K (Decoder s (NS f xs)) blk
  aux index (Comp dec) (K ()) = K $ injectNS index <$> dec

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
  All SingleEraBlock xs =>
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
          proxySingle
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
  All SingleEraBlock xs =>
  PerasVoteCollectionWithQuorum (HardForkBlock xs) ->
  Maybe (NS PerasVoteCollectionWithQuorum xs)
projectHFCPerasVoteCollectionWithQuorum hfcCollection =
  let votes = pvcVotes $ forgetQuorum hfcCollection
      neMapNs = projectHFCWatValidatedPerasVote <$> votes
   in case ensureSameEraNonEmptyMap neMapNs of
        Nothing -> Nothing
        Just nsNeMap ->
          Just $
            hcmap
              proxySingle
              ( \(Comp compedValMap) ->
                  unsafeAssumeQuorum $
                    unsafePerasVoteCollection
                      ((\(Comp v) -> v) <$> compedValMap)
              )
              nsNeMap

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

injectHFCPerasEpochContext ::
  All Top xs =>
  NS PerasEpochContext xs ->
  PerasEpochContext (HardForkBlock xs)
injectHFCPerasEpochContext nsContext =
  PerasEpochContext
    { pecCommittee = OneEraPerasVotingCommittee $ hmap (WrapPerasVotingCommittee . pecCommittee) nsContext
    , pecParams = hcollapse $ hmap (K . retagPerasParams . pecParams) nsContext
    }

projectHFCBoundedPerasEpochContext ::
  All Top xs =>
  BoundedPerasEpochContext (HardForkBlock xs) ->
  NS BoundedPerasEpochContext xs
projectHFCBoundedPerasEpochContext BoundedPerasEpochContext{startPerasRoundNo, endPerasRoundNo, epochContext} =
  hmap
    ( \(WrapPerasVotingCommittee committee) ->
        BoundedPerasEpochContext
          { startPerasRoundNo = startPerasRoundNo
          , endPerasRoundNo = endPerasRoundNo
          , epochContext =
              PerasEpochContext
                { pecCommittee = committee
                , pecParams = retagPerasParams $ pecParams epochContext
                }
          }
    )
    . getOneEraPerasVotingCommittee
    $ pecCommittee epochContext

injectHFCBoundedPerasEpochContext ::
  All Top xs =>
  NS BoundedPerasEpochContext xs ->
  BoundedPerasEpochContext (HardForkBlock xs)
injectHFCBoundedPerasEpochContext nsBoundedContext =
  BoundedPerasEpochContext
    { startPerasRoundNo = hcollapse $ hmap (K . startPerasRoundNo) nsBoundedContext
    , endPerasRoundNo = hcollapse $ hmap (K . endPerasRoundNo) nsBoundedContext
    , epochContext = injectHFCPerasEpochContext $ hmap epochContext nsBoundedContext
    }

extractHFCPerasEpochContextResolver ::
  All Top xs =>
  Index xs blk ->
  PerasEpochContextResolver (HardForkBlock xs) ->
  PerasEpochContextResolver blk
extractHFCPerasEpochContextResolver idx = \case
  PerasEpochContextResolverError err ->
    PerasEpochContextResolverError err
  PerasEpochContextResolver currentBoundedContext mbPrevBoundedContext ->
    case ensureSameEraPair
      ( getIndex idx
      , projectHFCBoundedPerasEpochContext currentBoundedContext
      ) of
      Nothing ->
        PerasEpochContextResolverError $
          "projectHFCPerasEpochContextResolver: currentBoundedContext is not in the same era as the supplied index"
      Just nsIdxCurrPair ->
        hcollapse $
          hmap
            ( \(Pair Refl currentBoundedContext') ->
                K . PerasEpochContextResolver currentBoundedContext' $
                  mbPrevBoundedContext >>= \prevBoundedContext ->
                    case ensureSameEraPair
                      ( getIndex idx
                      , projectHFCBoundedPerasEpochContext prevBoundedContext
                      ) of
                      Nothing ->
                        -- The current context is from the right era, but the previous one is from a different one.
                        -- So, instead of erroring out, we just discard the previous context.
                        SNothing
                      Just nsIdxPrevPair ->
                        hcollapse $
                          hmap
                            ( \(Pair Refl prevBoundedContext') ->
                                K $ SJust prevBoundedContext'
                            )
                            nsIdxPrevPair
            )
            nsIdxCurrPair

injectHFCPerasEpochContextResolver ::
  All Top xs =>
  NS PerasEpochContextResolver xs ->
  PerasEpochContextResolver (HardForkBlock xs)
injectHFCPerasEpochContextResolver =
  hcollapse
    . himap
      ( \idx resolver ->
          case resolver of
            PerasEpochContextResolverError err ->
              K $ PerasEpochContextResolverError err
            PerasEpochContextResolver currentBoundedContext mbPrevBoundedContext ->
              let hfcCurrentBoundedContext =
                    injectHFCBoundedPerasEpochContext . injectNS idx $ currentBoundedContext
                  hfcPrevBoundedContext =
                    injectHFCBoundedPerasEpochContext . injectNS idx <$> mbPrevBoundedContext
               in K $ PerasEpochContextResolver hfcCurrentBoundedContext hfcPrevBoundedContext
      )

type instance ElectionId (OneEraPerasCrypto xs) = PerasRoundNo
type instance BoostedBlock (OneEraPerasVote xs) = Point (HardForkBlock xs)
type instance BoostedBlock (OneEraPerasCert xs) = Point (HardForkBlock xs)
type instance VoteCandidate (OneEraPerasCrypto xs) = Point (HardForkBlock xs)
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
deriving stock instance Generic (WrapUniqueVotesWithSameTarget x)
deriving newtype instance
  NoThunks (UniqueVotesWithSameTarget (PerasCrypto x) (PerasVotingCommitteeScheme x)) =>
  NoThunks (WrapUniqueVotesWithSameTarget x)

deriving newtype instance
  Show (PublicKey (PerasCrypto blk)) =>
  Show (WrapPerasPublicKey blk)
deriving newtype instance
  Eq (PublicKey (PerasCrypto blk)) =>
  Eq (WrapPerasPublicKey blk)
deriving stock instance Generic (WrapPerasPublicKey blk)
deriving newtype instance
  NoThunks (PublicKey (PerasCrypto blk)) =>
  NoThunks (WrapPerasPublicKey blk)

deriving newtype instance
  Show (PrivateKey (PerasCrypto blk)) =>
  Show (WrapPerasPrivateKey blk)
deriving newtype instance
  Eq (PrivateKey (PerasCrypto blk)) =>
  Eq (WrapPerasPrivateKey blk)
deriving stock instance Generic (WrapPerasPrivateKey blk)
deriving newtype instance
  NoThunks (PrivateKey (PerasCrypto blk)) =>
  NoThunks (WrapPerasPrivateKey blk)

deriving newtype instance
  Show (VotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Show (WrapPerasVotingCommittee blk)
deriving newtype instance
  Eq (VotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Eq (WrapPerasVotingCommittee blk)
deriving stock instance Generic (WrapPerasVotingCommittee blk)
deriving newtype instance
  NoThunks (VotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  NoThunks (WrapPerasVotingCommittee blk)
deriving newtype instance
  Serialise (VotingCommittee (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Serialise (WrapPerasVotingCommittee blk)

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
deriving stock instance
  Generic (VotingCommittee (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs))
deriving via
  LiftNamedNS "OneEraPerasVotingCommittee" WrapPerasVotingCommittee xs
  instance
    CanHardFork xs =>
    NoThunks (VotingCommittee (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs))

-- | Hand-written rather than @deriving via SerialiseNS@: that derivation needs
-- @All (Compose Serialise WrapPerasVotingCommittee) xs@, which GHC cannot solve
-- from the @CanHardFork xs@ (i.e. @All SingleEraBlock xs@) context for an abstract
-- @xs@. We instead build the per-era codecs with @hcpure proxySingle@ -- each era's
-- @Serialise (WrapPerasVotingCommittee blk)@ is reachable from @SingleEraBlock blk@
-- via its @StateSupportsPerasEpochContext@ superclass -- and feed them to 'encodeNS'
-- and 'decodeNS', producing the same wire format a @SerialiseNS@ derivation would.
instance
  CanHardFork xs =>
  Serialise (VotingCommittee (OneEraPerasCrypto xs) (OneEraPerasVotingCommitteeScheme xs))
  where
  encode (OneEraPerasVotingCommittee ns) =
    encodeNS (hcpure proxySingle (fn (K . Serialise.encode))) ns
  decode =
    OneEraPerasVotingCommittee
      <$> decodeNS (hcpure proxySingle (Comp Serialise.decode))

deriving newtype instance
  Show (VotingCommitteeInput (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Show (WrapPerasVotingCommitteeInput blk)
deriving newtype instance
  Eq (VotingCommitteeInput (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Eq (WrapPerasVotingCommitteeInput blk)
deriving stock instance Generic (WrapPerasVotingCommitteeInput blk)
deriving newtype instance
  NoThunks (VotingCommitteeInput (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  NoThunks (WrapPerasVotingCommitteeInput blk)

deriving newtype instance
  Show (VotingCommitteeError (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Show (WrapPerasVotingCommitteeError blk)
deriving newtype instance
  Eq (VotingCommitteeError (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Eq (WrapPerasVotingCommitteeError blk)
deriving stock instance Generic (WrapPerasVotingCommitteeError blk)
deriving newtype instance
  NoThunks (VotingCommitteeError (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  NoThunks (WrapPerasVotingCommitteeError blk)

deriving newtype instance
  Show (EligibilityWitness (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Show (WrapPerasEligibilityWitness blk)
deriving newtype instance
  Eq (EligibilityWitness (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Eq (WrapPerasEligibilityWitness blk)
deriving stock instance Generic (WrapPerasEligibilityWitness blk)
deriving newtype instance
  NoThunks (EligibilityWitness (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  NoThunks (WrapPerasEligibilityWitness blk)

deriving newtype instance
  Show (Vote (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Show (WrapPerasCommitteeVote blk)
deriving newtype instance
  Eq (Vote (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Eq (WrapPerasCommitteeVote blk)
deriving stock instance Generic (WrapPerasCommitteeVote blk)
deriving newtype instance
  NoThunks (Vote (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  NoThunks (WrapPerasCommitteeVote blk)

deriving newtype instance
  Show (Cert (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Show (WrapPerasCommitteeCert blk)
deriving newtype instance
  Eq (Cert (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  Eq (WrapPerasCommitteeCert blk)
deriving stock instance Generic (WrapPerasCommitteeCert blk)
deriving newtype instance
  NoThunks (Cert (PerasCrypto blk) (PerasVotingCommitteeScheme blk)) =>
  NoThunks (WrapPerasCommitteeCert blk)

deriving newtype instance
  Show (VoteSignature (PerasCrypto blk)) =>
  Show (WrapPerasVoteSignature blk)
deriving newtype instance
  Eq (VoteSignature (PerasCrypto blk)) =>
  Eq (WrapPerasVoteSignature blk)
deriving stock instance Generic (WrapPerasVoteSignature blk)
deriving newtype instance
  NoThunks (VoteSignature (PerasCrypto blk)) =>
  NoThunks (WrapPerasVoteSignature blk)

deriving newtype instance
  Show (VoteVerificationKey (PerasCrypto blk)) =>
  Show (WrapPerasVoteVerificationKey blk)
deriving newtype instance
  Eq (VoteVerificationKey (PerasCrypto blk)) =>
  Eq (WrapPerasVoteVerificationKey blk)
deriving stock instance Generic (WrapPerasVoteVerificationKey blk)
deriving newtype instance
  NoThunks (VoteVerificationKey (PerasCrypto blk)) =>
  NoThunks (WrapPerasVoteVerificationKey blk)

deriving newtype instance
  Show (VoteSigningKey (PerasCrypto blk)) =>
  Show (WrapPerasVoteSigningKey blk)
deriving newtype instance
  Eq (VoteSigningKey (PerasCrypto blk)) =>
  Eq (WrapPerasVoteSigningKey blk)
deriving stock instance Generic (WrapPerasVoteSigningKey blk)
deriving newtype instance
  NoThunks (VoteSigningKey (PerasCrypto blk)) =>
  NoThunks (WrapPerasVoteSigningKey blk)

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
    All SingleEraBlock xs
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
    PerEraPerasVoteSigningKey $ hcmap proxySingle dispatchKey privKey
   where
    dispatchKey ::
      forall blk. SingleEraBlock blk => WrapPerasPrivateKey blk -> WrapPerasVoteSigningKey blk
    dispatchKey = WrapPerasVoteSigningKey . getVoteSigningKey (Proxy @(PerasCrypto blk)) . unwrapPerasPrivateKey

  getVoteVerificationKey _proxy (OneEraPerasPublicKey pubKey) =
    OneEraPerasVoteVerificationKey $ hcmap proxySingle dispatchKey pubKey
   where
    dispatchKey ::
      forall blk.
      SingleEraBlock blk => WrapPerasPublicKey blk -> WrapPerasVoteVerificationKey blk
    dispatchKey =
      WrapPerasVoteVerificationKey
        . getVoteVerificationKey (Proxy @(PerasCrypto blk))
        . unwrapPerasPublicKey

  signVote signingKey roundNo boostedBlock =
    PerEraPerasVoteSignature
      . hcmap proxySingle dispatchSign
      . getPerEraPerasVoteSigningKey
      $ signingKey
   where
    -- The single-era block @blk@ must be pinned explicitly: 'pointToBoostedBlock'
    -- goes through 'BoostedBlockCompatibleWithPoint', which has no functional
    -- dependency, so the era cannot be inferred from the (non-injective)
    -- @BoostedBlock (PerasVote blk)@ result alone.
    dispatchSign ::
      forall blk.
      SingleEraBlock blk =>
      WrapPerasVoteSigningKey blk ->
      WrapPerasVoteSignature blk
    dispatchSign (WrapPerasVoteSigningKey sk) =
      WrapPerasVoteSignature $
        signVote sk roundNo (pointToBoostedBlock (downcastHardForkPoint @blk boostedBlock))

  verifyVoteSignature (OneEraPerasVoteVerificationKey verKey) roundNo boostedBlock (PerEraPerasVoteSignature voteSignature) =
    let nsSignatureVerKey = alignNpWithNs voteSignature verKey
     in hcollapse
          . hcmap proxySingle dispatchVerify
          $ nsSignatureVerKey
   where
    dispatchVerify ::
      forall blk.
      SingleEraBlock blk =>
      Product WrapPerasVoteSignature WrapPerasVoteVerificationKey blk ->
      K (Either String ()) blk
    dispatchVerify (Pair (WrapPerasVoteSignature sig) (WrapPerasVoteVerificationKey vk)) =
      K $
        verifyVoteSignature vk roundNo (pointToBoostedBlock (downcastHardForkPoint @blk boostedBlock)) sig

instance
  ( CanHardFork xs
  , -- , All (IsBoostedBlock PerasBoostedBlock) xs
    -- , All (IsElectionId PerasRoundNo) xs
    All SingleEraBlock xs
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
      . hcmap proxySingle dispatchVoteTarget
      . getOneEraPerasCommitteeVote
   where
    -- The single-era block @blk@ must be pinned explicitly: 'boostedBlockToPoint'
    -- goes through 'BoostedBlockCompatibleWithPoint', which has no functional
    -- dependency, so the era cannot be inferred from the (non-injective)
    -- @BoostedBlock (PerasVote blk)@ argument alone.
    dispatchVoteTarget ::
      forall blk.
      SingleEraBlock blk =>
      WrapPerasCommitteeVote blk ->
      K (PerasRoundNo, Point (HardForkBlock xs)) blk
    dispatchVoteTarget =
      K . fmap (upcastToHardForkPoint @blk . boostedBlockToPoint) . voteTarget . unwrapPerasCommitteeVote

  compareVotesById vote1 vote2 =
    case ensureSameEraPair
      ( getOneEraPerasCommitteeVote vote1
      , getOneEraPerasCommitteeVote vote2
      ) of
      Nothing -> error "compareVotesById: votes are from different eras"
      Just nsPair ->
        hcollapse
          . hcmap proxySingle (K . uncurry compareVotesById . unwrapVotePair)
          $ nsPair
   where
    unwrapVotePair (Pair (WrapPerasCommitteeVote v1) (WrapPerasCommitteeVote v2)) = (v1, v2)

  mkVotingCommittee =
    bimap
      (HardForkVotingCommitteeErrorOneEraVotingCommitteeError . OneEraPerasVotingCommitteeError)
      OneEraPerasVotingCommittee
      . hcollect
      . hcmap
        proxySingle
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
            proxySingle
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
          . hcmap proxySingle dispatchForge
          $ nsPrivKeyWitness
   where
    -- The single-era block @blk@ must be pinned explicitly: 'pointToBoostedBlock'
    -- goes through 'BoostedBlockCompatibleWithPoint', which has no functional
    -- dependency, so the era cannot be inferred from the (non-injective)
    -- @BoostedBlock (PerasVote blk)@ result alone.
    dispatchForge ::
      forall blk.
      SingleEraBlock blk =>
      Product WrapPerasPrivateKey WrapPerasEligibilityWitness blk ->
      WrapPerasCommitteeVote blk
    dispatchForge (Pair (WrapPerasPrivateKey privKey') (WrapPerasEligibilityWitness witness')) =
      WrapPerasCommitteeVote $
        forgeVote witness' privKey' electionId (pointToBoostedBlock (downcastHardForkPoint @blk candidate))

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
            proxySingle
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
            proxySingle
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
            proxySingle
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
            proxySingle
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

instance
  (CanHardFork xs, All SingleEraBlock xs) =>
  IsPerasVote (OneEraPerasVote xs) (HardForkBlock xs)
  where
  getPerasVoteRound =
    hcollapse
      . hcmap proxySingle (K . getPerasVoteRound . unwrapPerasVote)
      . getOneEraPerasVote
  getPerasVoteSeatIndex =
    hcollapse
      . hcmap proxySingle (K . getPerasVoteSeatIndex . unwrapPerasVote)
      . getOneEraPerasVote
  getPerasVoteBlock =
    hcollapse
      . hcmap proxySingle (K . upcastToHardForkPoint . getPerasVotePoint . unwrapPerasVote)
      . getOneEraPerasVote

instance
  (CanHardFork xs, All SingleEraBlock xs) =>
  IsPerasCert (OneEraPerasCert xs) (HardForkBlock xs)
  where
  getPerasCertRound =
    hcollapse
      . hcmap proxySingle (K . getPerasCertRound . unwrapPerasCert)
      . getOneEraPerasCert
  getPerasCertBlock =
    hcollapse
      . hcmap proxySingle (K . upcastToHardForkPoint . getPerasCertPoint . unwrapPerasCert)
      . getOneEraPerasCert

instance
  (CanHardFork xs, All SingleEraBlock xs) =>
  IsPerasError (HardForkPerasError xs) (HardForkBlock xs)
  where
  injectVotingCommitteeError err = case err of
    HardForkVotingCommitteeErrorEraMismatch ->
      HardForkPerasErrorEraMismatch
    HardForkVotingCommitteeErrorOneEraVotingCommitteeError (OneEraPerasVotingCommitteeError ns) ->
      HardForkPerasErrorOneEraPerasError
        . OneEraPerasError
        $ hcmap
          proxySingle
          (\(WrapPerasVotingCommitteeError e) -> WrapPerasError (injectVotingCommitteeError e))
          ns

  -- NOTE: in practice this is never produced at the HFC level
  injectConversionError = HardForkPerasErrorConversionError

  -- NOTE: in practice this is never produced at the HFC level
  injectQuorumNotReachedError = HardForkPerasErrorQuorumNotReachedError

instance
  (CanHardFork xs, All SingleEraBlock xs) =>
  PerasVoteCompatibleWithVotingCommittee
    (OneEraPerasVote xs)
    (OneEraPerasCrypto xs)
    (OneEraPerasVotingCommitteeScheme xs)
  where
  toPerasVote =
    fmap OneEraPerasVote
      . hsequence'
      . hcmap
        proxySingle
        (Comp . fmap WrapPerasVote . toPerasVote . unwrapPerasCommitteeVote)
      . getOneEraPerasCommitteeVote
  fromPerasVote =
    fmap OneEraPerasCommitteeVote
      . hsequence'
      . hcmap
        proxySingle
        (Comp . fmap WrapPerasCommitteeVote . fromPerasVote . unwrapPerasVote)
      . getOneEraPerasVote

instance
  (CanHardFork xs, All SingleEraBlock xs) =>
  PerasCertCompatibleWithVotingCommittee
    (OneEraPerasCert xs)
    (OneEraPerasCrypto xs)
    (OneEraPerasVotingCommitteeScheme xs)
  where
  toPerasCert =
    fmap OneEraPerasCert
      . hsequence'
      . hcmap
        proxySingle
        (Comp . fmap WrapPerasCert . toPerasCert . unwrapPerasCommitteeCert)
      . getOneEraPerasCommitteeCert
  fromPerasCert =
    fmap OneEraPerasCommitteeCert
      . hsequence'
      . hcmap
        proxySingle
        (Comp . fmap WrapPerasCommitteeCert . fromPerasCert . unwrapPerasCert)
      . getOneEraPerasCert

-- | Downcast a 'Point' of the hard fork block to a 'Point' of a single era
-- by decoding the raw hash via 'fromShortRawHash'. Used when delegating
-- operations that take a 'Point' argument to a single-era implementation.
downcastHardForkPoint ::
  forall blk xs.
  SingleEraBlock blk =>
  Point (HardForkBlock xs) ->
  Point blk
downcastHardForkPoint = \case
  GenesisPoint ->
    GenesisPoint
  BlockPoint s (OneEraHash h) ->
    BlockPoint s (fromShortRawHash (Proxy @blk) h)

-- | Upcast a 'Point' from a single era into a 'Point' of the hard fork block
-- by encoding the raw hash via 'toShortRawHash'. Used by accessor instances
-- to return 'Point (HardForkBlock xs)' from single-era point values.
upcastToHardForkPoint ::
  forall blk xs.
  SingleEraBlock blk =>
  Point blk ->
  Point (HardForkBlock xs)
upcastToHardForkPoint = \case
  GenesisPoint ->
    GenesisPoint
  BlockPoint s h ->
    BlockPoint s (OneEraHash (toShortRawHash (Proxy @blk) h))

-- TODO: we need to change the binary representation of votes and certs to carry
-- era-specific/versionning information, to allow future evolutions
instance
  ( StandardHash (HardForkBlock xs)
  , CanHardFork xs
  , All SingleEraBlock xs
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
          proxySingle
          ( \(Pair (WrapPerasPrivateKey privKey') context') ->
              mkEitherF
                WrapPerasError
                Comp
                $ forgePerasVoteIfEligible
                  context'
                  poolId
                  privKey'
                  roundNo
                  (downcastHardForkPoint point)
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
            proxySingle
            ( \(Pair context' (WrapPerasVote vote')) ->
                mkEitherF
                  WrapPerasError
                  id
                  $ verifyPerasVote context' vote'
            )
          $ nsContextVote

  forgePerasCert context collection =
    case projectHFCPerasVoteCollectionWithQuorum collection of
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
              proxySingle
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
            proxySingle
            ( \(Pair context' (WrapPerasCert cert')) ->
                mkEitherF
                  WrapPerasError
                  id
                  $ verifyPerasCert context' cert'
            )
          $ nsContextCert

  getPerasCertInBlock (HardForkBlock (OneEraBlock nsBlock)) =
    fmap OneEraPerasCert
      $ hsequence'
        . hcmap proxySingle (\(I block) -> Comp $ WrapPerasCert <$> getPerasCertInBlock block)
      $ nsBlock

  readPerasPrivateKeyFromEnv _proxy =
    fmap PerEraPerasPrivateKey $
      hsequence' $
        hcpure proxySingle dispatchReadKey
   where
    dispatchReadKey ::
      forall blk.
      SingleEraBlock blk =>
      (Either String :.: WrapPerasPrivateKey) blk
    dispatchReadKey =
      Comp $ WrapPerasPrivateKey <$> readPerasPrivateKeyFromEnv (Proxy @blk)

  blockDoesReallySupportsPeras _proxy = True

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
